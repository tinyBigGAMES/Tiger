{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Linker.MachO;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Common,
  Tiger.Linker;

const
  MH_MAGIC_64  = $FEEDFACF;
  MH_CIGAM_64  = $CFFAEDFE;
  MH_OBJECT    = 1;
  LC_SEGMENT_64 = $19;
  LC_SYMTAB    = 2;
  CPU_TYPE_ARM64 = $0100000C;
  CPU_TYPE_X86_64 = $01000007;

  SEG_TEXT = '__TEXT';
  SEG_DATA = '__DATA';
  SECT_TEXT = '__text';
  SECT_DATA = '__data';
  SECT_CONST = '__const';
  SECT_CSTRING = '__cstring';
  SECT_BSS = '__bss';

  N_EXT  = $01;
  N_TYPE = $0E;
  N_SECT = $0E;
  N_UNDF = 0;
  N_ABS  = $0F;

  AR_SIGNATURE = '!<arch>'#10;
  AR_HEADER_SIZE = 60;

type
  TMachOSection = record
    SectionName: string;
    SegName: string;
    Addr: UInt64;
    Size: UInt64;
    FileOffset: Cardinal;
    Alignment: Cardinal;
    RawData: TBytes;
  end;

  TMachOSymbol = record
    SymbolName: string;
    Value: UInt64;
    SectionIndex: Integer;
    IsExternal: Boolean;
  end;

  TMachOObject = class
  public
    SourcePath: string;
    MemberName: string;
    Sections: TArray<TMachOSection>;
    Symbols: TArray<TMachOSymbol>;
    Selected: Boolean;

    constructor Create();
    function GetSectionIndexByName(const ASegName, ASectName: string): Integer;
  end;

  TTigerMachOLinker = class(TTigerLinker)
  private
    FObjects: TObjectList<TMachOObject>;
    FSymbolIndex: TDictionary<string, TList<Integer>>;
    FMergedText: TMemoryStream;
    FMergedRoData: TMemoryStream;
    FMergedData: TMemoryStream;
    FMergedBSS: Cardinal;
    FTextContribs: TList<TLinkerContribution>;
    FRoDataContribs: TList<TLinkerContribution>;
    FDataContribs: TList<TLinkerContribution>;
    FResolvedSymbols: TDictionary<string, TLinkerResolvedSymbol>;
    FUnresolvedSymbols: TStringList;
    FPendingRelocs: TList<TLinkerPendingReloc>;

    function ParseMachOObject(const AData: TBytes; const ASourcePath: string): TMachOObject;
    procedure ParseARLibrary(const AData: TBytes; const ASourcePath: string);
    function ClassifySection(const ASegName, ASectName: string): TLinkerSectionKind;
    procedure BuildGlobalSymbolIndex();
    procedure SelectObject(const AObjectIndex: Integer; const AWorkList: TList<Integer>);
    procedure MergeSelectedObjects();
    procedure CollectRelocations();
    function AlignUp(const AValue: Cardinal; const AAlignment: Cardinal): Cardinal;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure AddObjectFile(const APath: string); override;
    procedure AddLibraryFile(const APath: string); override;
    procedure AddObjectFromMemory(const AData: TBytes; const AName: string); override;
    procedure Resolve(const ANeededSymbols: TStrings); override;

    function GetMergedText(): TBytes; override;
    function GetMergedTextSize(): Cardinal; override;
    function GetMergedRData(): TBytes; override;
    function GetMergedRDataSize(): Cardinal; override;
    function GetMergedData(): TBytes; override;
    function GetMergedDataSize(): Cardinal; override;

    function GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>; override;
    function GetUnresolvedSymbols(): TStringList; override;
    function GetPendingRelocations(): TList<TLinkerPendingReloc>; override;
    function GetObjectCount(): Integer; override;
    function GetSelectedObjectCount(): Integer; override;
  end;

implementation

function ReadU32(const AData: TBytes; var AOff: Integer): Cardinal;
begin
  if AOff + 4 > Length(AData) then
    Exit(0);
  Result := Cardinal(AData[AOff]) or (Cardinal(AData[AOff+1]) shl 8) or
            (Cardinal(AData[AOff+2]) shl 16) or (Cardinal(AData[AOff+3]) shl 24);
  Inc(AOff, 4);
end;

function ReadU64(const AData: TBytes; var AOff: Integer): UInt64;
begin
  if AOff + 8 > Length(AData) then
    Exit(0);
  Result := UInt64(ReadU32(AData, AOff)) or (UInt64(ReadU32(AData, AOff)) shl 32);
end;

function ReadStr(const AData: TBytes; AOff: Integer; ALen: Integer): string;
var
  LBytes: TBytes;
  I: Integer;
begin
  Result := '';
  if (AOff < 0) or (AOff + ALen > Length(AData)) then
    Exit;
  SetLength(LBytes, ALen);
  for I := 0 to ALen - 1 do
    LBytes[I] := AData[AOff + I];
  Result := TEncoding.ASCII.GetString(LBytes).TrimRight([#0]);
end;

//==============================================================================
// TMachOObject
//==============================================================================

constructor TMachOObject.Create();
begin
  inherited Create();
  Selected := False;
end;

function TMachOObject.GetSectionIndexByName(const ASegName, ASectName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Sections) do
    if (Sections[I].SegName = ASegName) and (Sections[I].SectionName = ASectName) then
      Exit(I);
  Result := -1;
end;

//==============================================================================
// TTigerMachOLinker
//==============================================================================

constructor TTigerMachOLinker.Create();
begin
  inherited Create();
  FObjects := TObjectList<TMachOObject>.Create(True);
  FSymbolIndex := TDictionary<string, TList<Integer>>.Create();
  FMergedText := TMemoryStream.Create();
  FMergedRoData := TMemoryStream.Create();
  FMergedData := TMemoryStream.Create();
  FMergedBSS := 0;
  FTextContribs := TList<TLinkerContribution>.Create();
  FRoDataContribs := TList<TLinkerContribution>.Create();
  FDataContribs := TList<TLinkerContribution>.Create();
  FResolvedSymbols := TDictionary<string, TLinkerResolvedSymbol>.Create();
  FUnresolvedSymbols := TStringList.Create();
  FUnresolvedSymbols.Sorted := True;
  FUnresolvedSymbols.Duplicates := dupIgnore;
  FPendingRelocs := TList<TLinkerPendingReloc>.Create();
end;

destructor TTigerMachOLinker.Destroy();
var
  LList: TList<Integer>;
begin
  for LList in FSymbolIndex.Values do
    LList.Free();
  FSymbolIndex.Free();
  FPendingRelocs.Free();
  FUnresolvedSymbols.Free();
  FResolvedSymbols.Free();
  FDataContribs.Free();
  FRoDataContribs.Free();
  FTextContribs.Free();
  FMergedData.Free();
  FMergedRoData.Free();
  FMergedText.Free();
  FObjects.Free();
  inherited;
end;

function TTigerMachOLinker.AlignUp(const AValue: Cardinal; const AAlignment: Cardinal): Cardinal;
begin
  if AAlignment <= 1 then
    Exit(AValue);
  Result := (AValue + AAlignment - 1) and (not (AAlignment - 1));
end;

function TTigerMachOLinker.ClassifySection(const ASegName, ASectName: string): TLinkerSectionKind;
begin
  if (ASegName = SEG_TEXT) and (ASectName = SECT_TEXT) then
    Exit(lskText);
  if (ASegName = SEG_TEXT) and ((ASectName = SECT_CONST) or (ASectName = SECT_CSTRING)) then
    Exit(lskRData);
  if (ASegName = SEG_DATA) and (ASectName = SECT_DATA) then
    Exit(lskData);
  if (ASegName = SEG_DATA) and (ASectName = SECT_CONST) then
    Exit(lskRData);
  if (ASegName = SEG_DATA) and (ASectName = SECT_BSS) then
    Exit(lskBSS);
  Result := lskOther;
end;

function TTigerMachOLinker.ParseMachOObject(const AData: TBytes; const ASourcePath: string): TMachOObject;
var
  LOff: Integer;
  LMagic: Cardinal;
  LCpuType: Cardinal;
  LFileType: Cardinal;
  LNcmds: Cardinal;
  LSizeOfCmds: Cardinal;
  LCmd: Cardinal;
  LCmdSize: Cardinal;
  LSegName: string;
  LFileOff: UInt64;
  LFileSize: UInt64;
  LNsects: Cardinal;
  LSectName: string;
  LSectSegName: string;
  LSectAddr: UInt64;
  LSectSize: UInt64;
  LSectOffset: Cardinal;
  LSectAlign: Cardinal;
  LSections: TList<TMachOSection>;
  LSec: TMachOSection;
  LSymOff: Cardinal;
  LNsyms: Cardinal;
  LStroff: Cardinal;
  LStrSize: Cardinal;
  LNStrx: Cardinal;
  LNType: Byte;
  LNSect: Byte;
  LNValue: UInt64;
  LSymName: string;
  LSymbols: TList<TMachOSymbol>;
  LSym: TMachOSymbol;
  I: Integer;
  LCmdStart: Integer;
  LObj: TMachOObject;
begin
  Result := nil;
  if Length(AData) < 32 then
  begin
    Status('Mach-O Linker: File too small: %s', [ASourcePath]);
    Exit;
  end;
  LOff := 0;
  LMagic := ReadU32(AData, LOff);
  if (LMagic <> MH_MAGIC_64) and (LMagic <> MH_CIGAM_64) then
  begin
    Status('Mach-O Linker: Not Mach-O 64: %s', [ASourcePath]);
    Exit;
  end;
  LCpuType := ReadU32(AData, LOff);
  ReadU32(AData, LOff);
  LFileType := ReadU32(AData, LOff);
  if LFileType <> MH_OBJECT then
  begin
    Status('Mach-O Linker: Not relocatable object: %s', [ASourcePath]);
    Exit;
  end;
  LNcmds := ReadU32(AData, LOff);
  LSizeOfCmds := ReadU32(AData, LOff);
  Inc(LOff, 8);

  LSections := TList<TMachOSection>.Create();
  LSymbols := TList<TMachOSymbol>.Create();
  try
    LSymOff := 0;
    LNsyms := 0;
    LStroff := 0;
    LStrSize := 0;

    while LOff < 32 + Integer(LSizeOfCmds) do
    begin
      if LOff + 8 > Length(AData) then
        Break;
      LCmdStart := LOff;
      LCmd := ReadU32(AData, LOff);
      LCmdSize := ReadU32(AData, LOff);
      if LCmdSize < 8 then
        Break;

      if LCmd = LC_SEGMENT_64 then
      begin
        LSegName := ReadStr(AData, LOff, 16);
        Inc(LOff, 16);
        ReadU64(AData, LOff);
        ReadU64(AData, LOff);
        LFileOff := ReadU64(AData, LOff);
        LFileSize := ReadU64(AData, LOff);
        Inc(LOff, 8);
        LNsects := ReadU32(AData, LOff);
        Inc(LOff, 4);

        for I := 0 to Integer(LNsects) - 1 do
        begin
          if LOff + 80 > Length(AData) then
            Break;
          LSectName := ReadStr(AData, LOff, 16);
          Inc(LOff, 16);
          LSectSegName := ReadStr(AData, LOff, 16);
          Inc(LOff, 16);
          LSectAddr := ReadU64(AData, LOff);
          LSectSize := ReadU64(AData, LOff);
          LSectOffset := ReadU32(AData, LOff);
          LSectAlign := 1 shl ReadU32(AData, LOff);
          Inc(LOff, 8);
          Inc(LOff, 24);

          LSec.SectionName := LSectName;
          LSec.SegName := LSectSegName;
          LSec.Addr := LSectAddr;
          LSec.Size := LSectSize;
          LSec.FileOffset := LSectOffset;
          LSec.Alignment := LSectAlign;
          if (LSectSize > 0) and (LSectOffset < Cardinal(Length(AData))) and
             (LSectOffset + Cardinal(LSectSize) <= Cardinal(Length(AData))) then
          begin
            SetLength(LSec.RawData, LSectSize);
            Move(AData[LSectOffset], LSec.RawData[0], LSectSize);
          end
          else
            SetLength(LSec.RawData, 0);
          LSections.Add(LSec);
        end;
      end
      else if LCmd = LC_SYMTAB then
      begin
        if LCmdSize >= 24 then
        begin
          LSymOff := ReadU32(AData, LOff);
          LNsyms := ReadU32(AData, LOff);
          LStroff := ReadU32(AData, LOff);
          LStrSize := ReadU32(AData, LOff);
        end;
      end;
      LOff := LCmdStart + Integer(LCmdSize);
    end;

    if (LNsyms > 0) and (LStroff < Cardinal(Length(AData))) and
       (LStroff + LStrSize <= Cardinal(Length(AData))) and
       (LSymOff + LNsyms * 16 <= Cardinal(Length(AData))) then
    begin
      for I := 0 to Integer(LNsyms) - 1 do
      begin
        LOff := LSymOff + I * 16;
        if LOff + 16 > Length(AData) then
          Break;
        LNStrx := ReadU32(AData, LOff);
        LNType := AData[LOff];
        LNSect := AData[LOff + 1];
        LOff := LSymOff + I * 16 + 8;
        LNValue := ReadU64(AData, LOff);

        if LNStrx > 0 then
        begin
          if LStroff + LNStrx < Cardinal(Length(AData)) then
          begin
            LSymName := '';
            LOff := LStroff + LNStrx;
            while (LOff < Length(AData)) and (AData[LOff] <> 0) do
            begin
              LSymName := LSymName + Char(AData[LOff]);
              Inc(LOff);
            end;
          end
          else
            LSymName := '';
        end
        else
          LSymName := '';

        LSym.SymbolName := LSymName;
        LSym.Value := LNValue;
        LSym.SectionIndex := LNSect - 1;
        LSym.IsExternal := (LNType and N_EXT) <> 0;
        if LSymName <> '' then
          LSymbols.Add(LSym);
      end;
    end;

    LObj := TMachOObject.Create();
    LObj.SourcePath := ASourcePath;
    LObj.Sections := LSections.ToArray();
    LObj.Symbols := LSymbols.ToArray();
    Result := LObj;
  finally
    LSections.Free();
    LSymbols.Free();
  end;
end;

procedure TTigerMachOLinker.ParseARLibrary(const AData: TBytes; const ASourcePath: string);
var
  LPos: Integer;
  LSig: AnsiString;
  LHeaderName: AnsiString;
  LHeaderSizeStr: AnsiString;
  LMemberSize: Cardinal;
  LMemberData: TBytes;
  LObj: TMachOObject;
  LIsLinkerMember: Boolean;
  LTrimmedName: string;
  LMagic: Cardinal;
begin
  if Length(AData) < 8 then
  begin
    Status('Mach-O Linker: AR file too small: %s', [ASourcePath]);
    Exit;
  end;
  SetLength(LSig, 8);
  Move(AData[0], LSig[1], 8);
  if string(LSig) <> AR_SIGNATURE then
  begin
    Status('Mach-O Linker: Invalid AR signature: %s', [ASourcePath]);
    Exit;
  end;
  LPos := 8;
  while LPos + AR_HEADER_SIZE <= Length(AData) do
  begin
    if (AData[LPos + 58] <> Ord('`')) or (AData[LPos + 59] <> 10) then
      Break;
    SetLength(LHeaderName, 16);
    Move(AData[LPos], LHeaderName[1], 16);
    LHeaderName := AnsiString(TrimRight(string(LHeaderName)));
    SetLength(LHeaderSizeStr, 10);
    Move(AData[LPos + 48], LHeaderSizeStr[1], 10);
    LMemberSize := StrToIntDef(Trim(string(LHeaderSizeStr)), 0);
    LPos := LPos + AR_HEADER_SIZE;

    LIsLinkerMember := (LHeaderName = '/') or (LHeaderName = '//') or
                       (Copy(string(LHeaderName), 1, 8) = '__.SYMDE');
    if (not LIsLinkerMember) and (LMemberSize >= 32) and
       (LPos + Integer(LMemberSize) <= Length(AData)) then
    begin
      SetLength(LMemberData, LMemberSize);
      Move(AData[LPos], LMemberData[0], LMemberSize);
      LMagic := Cardinal(LMemberData[0]) or (Cardinal(LMemberData[1]) shl 8) or
                (Cardinal(LMemberData[2]) shl 16) or (Cardinal(LMemberData[3]) shl 24);
      if (LMagic = MH_MAGIC_64) or (LMagic = MH_CIGAM_64) then
      begin
        LObj := ParseMachOObject(LMemberData, ASourcePath);
        if LObj <> nil then
        begin
          LTrimmedName := string(LHeaderName);
          if (Length(LTrimmedName) > 0) and (LTrimmedName[Length(LTrimmedName)] = '/') then
            LTrimmedName := Copy(LTrimmedName, 1, Length(LTrimmedName) - 1);
          LObj.MemberName := LTrimmedName;
          FObjects.Add(LObj);
        end;
      end;
    end;
    LPos := LPos + Integer(LMemberSize);
    if (LPos mod 2) <> 0 then
      Inc(LPos);
  end;
  Status('Mach-O Linker: Parsed AR library %s -- %d object members', [ASourcePath, FObjects.Count]);
end;

procedure TTigerMachOLinker.BuildGlobalSymbolIndex();
var
  LI, LJ: Integer;
  LObj: TMachOObject;
  LList: TList<Integer>;
begin
  FSymbolIndex.Clear();
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    for LJ := 0 to High(LObj.Symbols) do
    begin
      if (LObj.Symbols[LJ].SymbolName <> '') and (LObj.Symbols[LJ].SectionIndex >= 0) and
         (LObj.Symbols[LJ].SectionIndex < Length(LObj.Sections)) then
      begin
        if not FSymbolIndex.TryGetValue(LObj.Symbols[LJ].SymbolName, LList) then
        begin
          LList := TList<Integer>.Create();
          FSymbolIndex.Add(LObj.Symbols[LJ].SymbolName, LList);
        end;
        LList.Add(LI);
      end;
    end;
  end;
end;

procedure TTigerMachOLinker.SelectObject(const AObjectIndex: Integer; const AWorkList: TList<Integer>);
var
  LObj: TMachOObject;
begin
  if (AObjectIndex < 0) or (AObjectIndex >= FObjects.Count) then
    Exit;
  LObj := FObjects[AObjectIndex];
  if LObj.Selected then
    Exit;
  LObj.Selected := True;
  AWorkList.Add(AObjectIndex);
end;

procedure TTigerMachOLinker.MergeSelectedObjects();
var
  LI, LJ: Integer;
  LObj: TMachOObject;
  LSec: TMachOSection;
  LKind: TLinkerSectionKind;
  LContrib: TLinkerContribution;
  LAlign: Cardinal;
  LPadding: Cardinal;
  LPadByte: Byte;
  LResolved: TLinkerResolvedSymbol;
  LSym: TMachOSymbol;
begin
  FMergedText.Clear();
  FMergedRoData.Clear();
  FMergedData.Clear();
  FMergedBSS := 0;
  FTextContribs.Clear();
  FRoDataContribs.Clear();
  FDataContribs.Clear();
  FResolvedSymbols.Clear();
  LPadByte := 0;

  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Sections) do
    begin
      LSec := LObj.Sections[LJ];
      if Length(LSec.RawData) = 0 then
        Continue;
      LKind := ClassifySection(LSec.SegName, LSec.SectionName);
      if LKind = lskOther then
        Continue;
      if LSec.Alignment > 1 then
        LAlign := LSec.Alignment
      else
        LAlign := 1;

      case LKind of
        lskText:
        begin
          LContrib.ObjectIndex := LI;
          LContrib.OrigSectionIndex := LJ;
          LContrib.MergedOffset := AlignUp(Cardinal(FMergedText.Size), LAlign);
          LContrib.MergedSize := Length(LSec.RawData);
          LPadding := LContrib.MergedOffset - Cardinal(FMergedText.Size);
          while LPadding > 0 do
          begin
            FMergedText.WriteBuffer(LPadByte, 1);
            Dec(LPadding);
          end;
          FMergedText.WriteBuffer(LSec.RawData[0], Length(LSec.RawData));
          FTextContribs.Add(LContrib);
        end;
        lskRData:
        begin
          LContrib.ObjectIndex := LI;
          LContrib.OrigSectionIndex := LJ;
          LContrib.MergedOffset := AlignUp(Cardinal(FMergedRoData.Size), LAlign);
          LContrib.MergedSize := Length(LSec.RawData);
          LPadding := LContrib.MergedOffset - Cardinal(FMergedRoData.Size);
          while LPadding > 0 do
          begin
            FMergedRoData.WriteBuffer(LPadByte, 1);
            Dec(LPadding);
          end;
          FMergedRoData.WriteBuffer(LSec.RawData[0], Length(LSec.RawData));
          FRoDataContribs.Add(LContrib);
        end;
        lskData:
        begin
          LContrib.ObjectIndex := LI;
          LContrib.OrigSectionIndex := LJ;
          LContrib.MergedOffset := AlignUp(Cardinal(FMergedData.Size), LAlign);
          LContrib.MergedSize := Length(LSec.RawData);
          LPadding := LContrib.MergedOffset - Cardinal(FMergedData.Size);
          while LPadding > 0 do
          begin
            FMergedData.WriteBuffer(LPadByte, 1);
            Dec(LPadding);
          end;
          FMergedData.WriteBuffer(LSec.RawData[0], Length(LSec.RawData));
          FDataContribs.Add(LContrib);
        end;
        lskBSS:
          FMergedBSS := AlignUp(FMergedBSS, LAlign) + Cardinal(LSec.Size);
      else
        ;
      end;
    end;

    for LJ := 0 to High(LObj.Symbols) do
    begin
      LSym := LObj.Symbols[LJ];
      if (LSym.SymbolName <> '') and (LSym.SectionIndex >= 0) and
         (LSym.SectionIndex < Length(LObj.Sections)) and
         (not FResolvedSymbols.ContainsKey(LSym.SymbolName)) then
      begin
        LSec := LObj.Sections[LSym.SectionIndex];
        LKind := ClassifySection(LSec.SegName, LSec.SectionName);
        LResolved.SymbolName := LSym.SymbolName;
        LResolved.SectionKind := LKind;
        LResolved.OffsetInMerged := Cardinal(LSym.Value);
        case LKind of
          lskText:
            for LContrib in FTextContribs do
              if (LContrib.ObjectIndex = LI) and (LContrib.OrigSectionIndex = LSym.SectionIndex) then
              begin
                LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
                Break;
              end;
          lskRData:
            for LContrib in FRoDataContribs do
              if (LContrib.ObjectIndex = LI) and (LContrib.OrigSectionIndex = LSym.SectionIndex) then
              begin
                LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
                Break;
              end;
          lskData:
            for LContrib in FDataContribs do
              if (LContrib.ObjectIndex = LI) and (LContrib.OrigSectionIndex = LSym.SectionIndex) then
              begin
                LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
                Break;
              end;
        else
          LResolved.SectionKind := lskOther;
        end;
        if LResolved.SectionKind <> lskOther then
          FResolvedSymbols.Add(LSym.SymbolName, LResolved);
      end;
    end;
  end;
  Status('Mach-O Linker: Merged sections -- text=%d, rodata=%d, data=%d, bss=%d',
    [FMergedText.Size, FMergedRoData.Size, FMergedData.Size, FMergedBSS]);
end;

procedure TTigerMachOLinker.CollectRelocations();
begin
  FPendingRelocs.Clear();
  FUnresolvedSymbols.Clear();
end;

procedure TTigerMachOLinker.AddObjectFile(const APath: string);
var
  LData: TBytes;
  LObj: TMachOObject;
begin
  if not TFile.Exists(APath) then
  begin
    Status('Mach-O Linker: File not found: %s', [APath]);
    Exit;
  end;
  LData := TFile.ReadAllBytes(APath);
  LObj := ParseMachOObject(LData, APath);
  if LObj <> nil then
    FObjects.Add(LObj)
  else
    Status('Mach-O Linker: Failed to parse: %s', [APath]);
end;

procedure TTigerMachOLinker.AddLibraryFile(const APath: string);
var
  LData: TBytes;
begin
  if not TFile.Exists(APath) then
  begin
    Status('Mach-O Linker: Library not found: %s', [APath]);
    Exit;
  end;
  LData := TFile.ReadAllBytes(APath);
  ParseARLibrary(LData, APath);
end;

procedure TTigerMachOLinker.AddObjectFromMemory(const AData: TBytes; const AName: string);
var
  LObj: TMachOObject;
begin
  LObj := ParseMachOObject(AData, AName);
  if LObj <> nil then
  begin
    LObj.MemberName := AName;
    FObjects.Add(LObj);
  end
  else
    Status('Mach-O Linker: Failed to parse object from memory: %s', [AName]);
end;

procedure TTigerMachOLinker.Resolve(const ANeededSymbols: TStrings);
var
  LWorkList: TList<Integer>;
  LI: Integer;
  LList: TList<Integer>;
  LSymbol: string;
begin
  BuildGlobalSymbolIndex();
  for LI := 0 to FObjects.Count - 1 do
    FObjects[LI].Selected := False;
  LWorkList := TList<Integer>.Create();
  try
    for LI := 0 to ANeededSymbols.Count - 1 do
    begin
      LSymbol := ANeededSymbols[LI];
      if FSymbolIndex.TryGetValue(LSymbol, LList) and (LList.Count > 0) then
        SelectObject(LList[0], LWorkList)
      else
        FUnresolvedSymbols.Add(LSymbol);
    end;
    LI := 0;
    while LI < LWorkList.Count do
    begin
      if LWorkList[LI] >= 0 then
        SelectObject(LWorkList[LI], LWorkList);
      Inc(LI);
    end;
    MergeSelectedObjects();
    CollectRelocations();
  finally
    LWorkList.Free();
  end;
end;

function TTigerMachOLinker.GetMergedText(): TBytes;
begin
  SetLength(Result, FMergedText.Size);
  if FMergedText.Size > 0 then
  begin
    FMergedText.Position := 0;
    FMergedText.ReadBuffer(Result[0], FMergedText.Size);
  end;
end;

function TTigerMachOLinker.GetMergedTextSize(): Cardinal;
begin
  Result := Cardinal(FMergedText.Size);
end;

function TTigerMachOLinker.GetMergedRData(): TBytes;
begin
  SetLength(Result, FMergedRoData.Size);
  if FMergedRoData.Size > 0 then
  begin
    FMergedRoData.Position := 0;
    FMergedRoData.ReadBuffer(Result[0], FMergedRoData.Size);
  end;
end;

function TTigerMachOLinker.GetMergedRDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedRoData.Size);
end;

function TTigerMachOLinker.GetMergedData(): TBytes;
begin
  SetLength(Result, FMergedData.Size);
  if FMergedData.Size > 0 then
  begin
    FMergedData.Position := 0;
    FMergedData.ReadBuffer(Result[0], FMergedData.Size);
  end;
end;

function TTigerMachOLinker.GetMergedDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedData.Size);
end;

function TTigerMachOLinker.GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>;
begin
  Result := FResolvedSymbols;
end;

function TTigerMachOLinker.GetUnresolvedSymbols(): TStringList;
begin
  Result := FUnresolvedSymbols;
end;

function TTigerMachOLinker.GetPendingRelocations(): TList<TLinkerPendingReloc>;
begin
  Result := FPendingRelocs;
end;

function TTigerMachOLinker.GetObjectCount(): Integer;
begin
  Result := FObjects.Count;
end;

function TTigerMachOLinker.GetSelectedObjectCount(): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FObjects.Count - 1 do
    if FObjects[I].Selected then
      Inc(Result);
end;

end.
