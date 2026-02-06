{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Linker;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Common;

const
  //----------------------------------------------------------------------------
  // COFF constants
  //----------------------------------------------------------------------------
  IMAGE_FILE_MACHINE_AMD64 = $8664;

  COFF_FILE_HEADER_SIZE    = 20;
  COFF_SECTION_HEADER_SIZE = 40;
  COFF_RELOCATION_SIZE     = 10;
  COFF_SYMBOL_SIZE         = 18;

  // AMD64 relocation types
  IMAGE_REL_AMD64_ABSOLUTE = $0000;
  IMAGE_REL_AMD64_ADDR64   = $0001;
  IMAGE_REL_AMD64_ADDR32NB = $0003;
  IMAGE_REL_AMD64_REL32    = $0004;
  IMAGE_REL_AMD64_REL32_1  = $0005;
  IMAGE_REL_AMD64_REL32_2  = $0006;
  IMAGE_REL_AMD64_REL32_3  = $0007;
  IMAGE_REL_AMD64_REL32_4  = $0008;
  IMAGE_REL_AMD64_REL32_5  = $0009;
  IMAGE_REL_AMD64_SECTION  = $000A;
  IMAGE_REL_AMD64_SECREL   = $000B;

  // COFF symbol storage classes
  COFF_SYM_CLASS_EXTERNAL = 2;
  COFF_SYM_CLASS_STATIC   = 3;
  COFF_SYM_CLASS_FILE     = 103;

  // Section characteristics flags
  IMAGE_SCN_CNT_CODE               = $00000020;
  IMAGE_SCN_CNT_INITIALIZED_DATA   = $00000040;
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = $00000080;
  IMAGE_SCN_LNK_COMDAT             = $00001000;
  IMAGE_SCN_MEM_EXECUTE            = $20000000;
  IMAGE_SCN_MEM_READ               = $40000000;
  IMAGE_SCN_MEM_WRITE              = $80000000;

  // AR archive constants
  AR_SIGNATURE = '!<arch>'#10;
  AR_HEADER_SIZE = 60;

type
  //----------------------------------------------------------------------------
  // COFF structures (for parsed data)
  //----------------------------------------------------------------------------

  { TCOFFRelocation - A single relocation entry from a COFF section }
  TCOFFRelocation = record
    VirtualAddress: Cardinal;    // Offset within the section where reloc applies
    SymbolTableIndex: Cardinal;  // Index into symbol table
    RelocationType: Word;        // IMAGE_REL_AMD64_xxx
  end;

  { TCOFFSymbol - A parsed COFF symbol table entry }
  TCOFFSymbol = record
    SymbolName: string;          // Resolved name (from short name or string table)
    Value: Cardinal;             // Offset within section (for defined symbols)
    SectionNumber: SmallInt;     // 1-based; 0 = UNDEFINED, -1 = ABSOLUTE, -2 = DEBUG
    SymbolType: Word;
    StorageClass: Byte;
    NumberOfAuxSymbols: Byte;
  end;

  { TCOFFSection - A parsed COFF section with its raw data and relocations }
  TCOFFSection = record
    SectionName: string;
    VirtualSize: Cardinal;
    SizeOfRawData: Cardinal;
    Characteristics: Cardinal;
    RawData: TBytes;
    Relocations: TArray<TCOFFRelocation>;
  end;

  { TCOFFObject - A fully parsed COFF object file }
  TCOFFObject = class
  public
    SourcePath: string;          // File path for diagnostics
    MemberName: string;          // AR member name (if from .lib)
    Sections: TArray<TCOFFSection>;
    Symbols: TArray<TCOFFSymbol>;
    // Set to True once this object has been selected for linking
    Selected: Boolean;

    constructor Create();
    destructor Destroy(); override;

    // Find the section index for a given 1-based section number
    function GetSectionIndex(const ASectionNumber: SmallInt): Integer;
  end;

  //----------------------------------------------------------------------------
  // Merged section output
  //----------------------------------------------------------------------------

  { TLinkerSectionKind - Categories of sections the linker merges }
  TLinkerSectionKind = (
    lskText,
    lskRData,
    lskData,
    lskBSS,
    lskPData,
    lskXData,
    lskOther
  );

  { TLinkerContribution - Tracks where an object's section ended up in merged output }
  TLinkerContribution = record
    ObjectIndex: Integer;          // Index into FObjects
    OrigSectionIndex: Integer;     // Section index within the object
    MergedOffset: Cardinal;        // Offset in the merged section buffer
    MergedSize: Cardinal;          // Size contributed
  end;

  { TLinkerResolvedSymbol - A symbol that was successfully resolved }
  TLinkerResolvedSymbol = record
    SymbolName: string;
    SectionKind: TLinkerSectionKind;
    OffsetInMerged: Cardinal;      // Offset within the merged section
  end;

  //----------------------------------------------------------------------------
  // Pending relocation (needs final PE addresses to resolve)
  //----------------------------------------------------------------------------

  { TLinkerPendingReloc - A relocation that needs PE-level fixup }
  TLinkerPendingReloc = record
    SectionKind: TLinkerSectionKind;  // Which merged section contains the reloc site
    OffsetInMerged: Cardinal;         // Offset within merged section where fixup goes
    RelocationType: Word;             // IMAGE_REL_AMD64_xxx
    TargetSymbol: string;             // The symbol being referenced
    Addend: Int32;                    // Existing value at reloc site (addend)
  end;

  //----------------------------------------------------------------------------
  // TTigerLinker - Static linker: reads .obj/.lib, resolves symbols, merges
  //----------------------------------------------------------------------------

  { TTigerLinker }
  TTigerLinker = class(TTigerStatusObject)
  private
    // All parsed objects (from both .obj and .lib files)
    FObjects: TObjectList<TCOFFObject>;

    // AR symbol index: symbol name → list of object indices that define it
    FSymbolIndex: TDictionary<string, TList<Integer>>;

    // Merged section buffers
    FMergedText: TMemoryStream;
    FMergedRData: TMemoryStream;
    FMergedData: TMemoryStream;
    FMergedBSS: Cardinal;  // BSS is just a size, no data
    FMergedPData: TMemoryStream;
    FMergedXData: TMemoryStream;

    // Contributions tracking
    FTextContribs: TList<TLinkerContribution>;
    FRDataContribs: TList<TLinkerContribution>;
    FDataContribs: TList<TLinkerContribution>;
    FPDataContribs: TList<TLinkerContribution>;
    FXDataContribs: TList<TLinkerContribution>;

    // Resolved symbols: name → resolved info
    FResolvedSymbols: TDictionary<string, TLinkerResolvedSymbol>;

    // Symbols that could not be resolved (will need DLL import)
    FUnresolvedSymbols: TStringList;

    // Pending relocations that need PE addresses
    FPendingRelocs: TList<TLinkerPendingReloc>;

    //--------------------------------------------------------------------------
    // COFF parsing
    //--------------------------------------------------------------------------
    function ParseCOFFObject(const AData: TBytes; const ASourcePath: string): TCOFFObject;
    function ReadCOFFSymbolName(const AData: TBytes; const AOffset: Integer;
      const AStringTableOffset: Integer): string;

    //--------------------------------------------------------------------------
    // AR parsing
    //--------------------------------------------------------------------------
    procedure ParseARLibrary(const AData: TBytes; const ASourcePath: string);

    //--------------------------------------------------------------------------
    // Section classification
    //--------------------------------------------------------------------------
    function ClassifySection(const ASectionName: string;
      const ACharacteristics: Cardinal): TLinkerSectionKind;

    //--------------------------------------------------------------------------
    // Symbol resolution internals
    //--------------------------------------------------------------------------
    procedure BuildGlobalSymbolIndex();
    procedure SelectObject(const AObjectIndex: Integer;
      const AWorkList: TList<Integer>);
    procedure MergeSelectedObjects();
    procedure CollectRelocations();
    function AlignUp(const AValue: Cardinal; const AAlignment: Cardinal): Cardinal;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Input — add files to link
    //--------------------------------------------------------------------------
    procedure AddObjectFile(const APath: string);
    procedure AddLibraryFile(const APath: string);
    procedure AddObjectFromMemory(const AData: TBytes; const AName: string);

    //--------------------------------------------------------------------------
    // Processing — resolve symbols and merge sections
    //--------------------------------------------------------------------------
    procedure Resolve(const ANeededSymbols: TStrings);

    //--------------------------------------------------------------------------
    // Output — merged section data
    //--------------------------------------------------------------------------
    function GetMergedText(): TBytes;
    function GetMergedTextSize(): Cardinal;
    function GetMergedRData(): TBytes;
    function GetMergedRDataSize(): Cardinal;
    function GetMergedData(): TBytes;
    function GetMergedDataSize(): Cardinal;
    function GetMergedPData(): TBytes;
    function GetMergedPDataSize(): Cardinal;
    function GetMergedXData(): TBytes;
    function GetMergedXDataSize(): Cardinal;

    //--------------------------------------------------------------------------
    // Symbol info
    //--------------------------------------------------------------------------
    function GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>;
    function GetUnresolvedSymbols(): TStringList;
    function GetPendingRelocations(): TList<TLinkerPendingReloc>;

    //--------------------------------------------------------------------------
    // Diagnostics
    //--------------------------------------------------------------------------
    function GetObjectCount(): Integer;
    function GetSelectedObjectCount(): Integer;
  end;

implementation

//==============================================================================
// TCOFFObject
//==============================================================================

constructor TCOFFObject.Create();
begin
  inherited Create();
  Sections := nil;
  Symbols := nil;
  Selected := False;
end;

destructor TCOFFObject.Destroy();
begin
  Sections := nil;
  Symbols := nil;
  inherited Destroy();
end;

function TCOFFObject.GetSectionIndex(const ASectionNumber: SmallInt): Integer;
begin
  // COFF section numbers are 1-based
  Result := ASectionNumber - 1;
  if (Result < 0) or (Result >= Length(Sections)) then
    Result := -1;
end;

//==============================================================================
// TTigerLinker
//==============================================================================

constructor TTigerLinker.Create();
begin
  inherited Create();

  FObjects := TObjectList<TCOFFObject>.Create(True);
  FSymbolIndex := TDictionary<string, TList<Integer>>.Create();

  FMergedText := TMemoryStream.Create();
  FMergedRData := TMemoryStream.Create();
  FMergedData := TMemoryStream.Create();
  FMergedBSS := 0;
  FMergedPData := TMemoryStream.Create();
  FMergedXData := TMemoryStream.Create();

  FTextContribs := TList<TLinkerContribution>.Create();
  FRDataContribs := TList<TLinkerContribution>.Create();
  FDataContribs := TList<TLinkerContribution>.Create();
  FPDataContribs := TList<TLinkerContribution>.Create();
  FXDataContribs := TList<TLinkerContribution>.Create();

  FResolvedSymbols := TDictionary<string, TLinkerResolvedSymbol>.Create();
  FUnresolvedSymbols := TStringList.Create();
  FPendingRelocs := TList<TLinkerPendingReloc>.Create();
end;

destructor TTigerLinker.Destroy();
var
  LPair: TPair<string, TList<Integer>>;
begin
  // Free the lists inside the symbol index
  for LPair in FSymbolIndex do
    LPair.Value.Free();
  FSymbolIndex.Free();

  FPendingRelocs.Free();
  FUnresolvedSymbols.Free();
  FResolvedSymbols.Free();

  FXDataContribs.Free();
  FPDataContribs.Free();
  FDataContribs.Free();
  FRDataContribs.Free();
  FTextContribs.Free();

  FMergedXData.Free();
  FMergedPData.Free();
  FMergedData.Free();
  FMergedRData.Free();
  FMergedText.Free();

  FObjects.Free();

  inherited Destroy();
end;

//==============================================================================
// COFF Parsing
//==============================================================================

function TTigerLinker.ReadCOFFSymbolName(const AData: TBytes;
  const AOffset: Integer; const AStringTableOffset: Integer): string;
var
  LFirst4: Cardinal;
  LStringOffset: Cardinal;
  LPos: Integer;
  LAnsi: AnsiString;
  LI: Integer;
  LShortName: AnsiString;
begin
  // COFF symbol name is 8 bytes. If the first 4 bytes are zero, the second
  // 4 bytes are an offset into the string table.
  LFirst4 := PCardinal(@AData[AOffset])^;
  if LFirst4 = 0 then
  begin
    // Long name — offset into string table
    LStringOffset := PCardinal(@AData[AOffset + 4])^;
    LPos := AStringTableOffset + Integer(LStringOffset);
    LAnsi := '';
    while (LPos < Length(AData)) and (AData[LPos] <> 0) do
    begin
      LAnsi := LAnsi + AnsiChar(AData[LPos]);
      Inc(LPos);
    end;
    Result := string(LAnsi);
  end
  else
  begin
    // Short name — up to 8 bytes, null-padded
    SetLength(LShortName, 8);
    Move(AData[AOffset], LShortName[1], 8);
    // Trim trailing nulls
    LI := 8;
    while (LI > 0) and (LShortName[LI] = #0) do
      Dec(LI);
    Result := string(Copy(LShortName, 1, LI));
  end;
end;

function TTigerLinker.ParseCOFFObject(const AData: TBytes;
  const ASourcePath: string): TCOFFObject;
var
  LObj: TCOFFObject;
  LMachine: Word;
  LNumSections: Word;
  LSymTabPtr: Cardinal;
  LNumSymbols: Cardinal;
  LSizeOptHdr: Word;
  LOffset: Integer;
  LStringTableOffset: Integer;
  LI, LJ: Integer;
  LSect: TCOFFSection;
  LSym: TCOFFSymbol;
  LReloc: TCOFFRelocation;
  LSecHdrOffset: Integer;
  LRawDataPtr: Cardinal;
  LRelocPtr: Cardinal;
  LNumRelocs: Word;
  LSectNameBytes: array[0..7] of AnsiChar;
begin
  Result := nil;

  if Length(AData) < COFF_FILE_HEADER_SIZE then
  begin
    Status('Linker: COFF file too small: %s', [ASourcePath]);
    Exit;
  end;

  // Parse COFF file header
  LMachine := PWord(@AData[0])^;
  if LMachine <> IMAGE_FILE_MACHINE_AMD64 then
  begin
    Status('Linker: Not an AMD64 COFF object: %s (machine=$%.4x)', [ASourcePath, LMachine]);
    Exit;
  end;

  LNumSections := PWord(@AData[2])^;
  LSymTabPtr := PCardinal(@AData[8])^;
  LNumSymbols := PCardinal(@AData[12])^;
  LSizeOptHdr := PWord(@AData[16])^;

  // String table follows the symbol table
  LStringTableOffset := Integer(LSymTabPtr) + Integer(LNumSymbols) * COFF_SYMBOL_SIZE;

  LObj := TCOFFObject.Create();
  LObj.SourcePath := ASourcePath;

  // Parse section headers
  SetLength(LObj.Sections, LNumSections);
  for LI := 0 to LNumSections - 1 do
  begin
    LSecHdrOffset := COFF_FILE_HEADER_SIZE + Integer(LSizeOptHdr) +
      (LI * COFF_SECTION_HEADER_SIZE);

    // Section name (8 bytes)
    FillChar(LSectNameBytes, SizeOf(LSectNameBytes), 0);
    Move(AData[LSecHdrOffset], LSectNameBytes, 8);
    // Handle long section names (/offset format)
    if LSectNameBytes[0] = '/' then
    begin
      // Long section name — offset into string table
      LOffset := StrToIntDef(string(AnsiString(PAnsiChar(@LSectNameBytes[1]))), 0);
      LSect.SectionName := '';
      LJ := LStringTableOffset + LOffset;
      while (LJ < Length(AData)) and (AData[LJ] <> 0) do
      begin
        LSect.SectionName := LSect.SectionName + Char(AData[LJ]);
        Inc(LJ);
      end;
    end
    else
    begin
      LJ := 0;
      while (LJ < 8) and (LSectNameBytes[LJ] <> #0) do
        Inc(LJ);
      LSect.SectionName := string(Copy(AnsiString(LSectNameBytes), 1, LJ));
    end;

    LSect.VirtualSize := PCardinal(@AData[LSecHdrOffset + 8])^;
    LSect.SizeOfRawData := PCardinal(@AData[LSecHdrOffset + 16])^;
    LRawDataPtr := PCardinal(@AData[LSecHdrOffset + 20])^;
    LRelocPtr := PCardinal(@AData[LSecHdrOffset + 24])^;
    LNumRelocs := PWord(@AData[LSecHdrOffset + 32])^;
    LSect.Characteristics := PCardinal(@AData[LSecHdrOffset + 36])^;

    // Read raw data
    if (LSect.SizeOfRawData > 0) and (LRawDataPtr > 0) then
    begin
      SetLength(LSect.RawData, LSect.SizeOfRawData);
      Move(AData[LRawDataPtr], LSect.RawData[0], LSect.SizeOfRawData);
    end
    else
      LSect.RawData := nil;

    // Read relocations
    if (LNumRelocs > 0) and (LRelocPtr > 0) then
    begin
      SetLength(LSect.Relocations, LNumRelocs);
      for LJ := 0 to LNumRelocs - 1 do
      begin
        LOffset := Integer(LRelocPtr) + (LJ * COFF_RELOCATION_SIZE);
        LReloc.VirtualAddress := PCardinal(@AData[LOffset])^;
        LReloc.SymbolTableIndex := PCardinal(@AData[LOffset + 4])^;
        LReloc.RelocationType := PWord(@AData[LOffset + 8])^;
        LSect.Relocations[LJ] := LReloc;
      end;
    end
    else
      LSect.Relocations := nil;

    LObj.Sections[LI] := LSect;
  end;

  // Parse symbol table
  if (LSymTabPtr > 0) and (LNumSymbols > 0) then
  begin
    SetLength(LObj.Symbols, LNumSymbols);
    LOffset := Integer(LSymTabPtr);
    LI := 0;
    while LI < Integer(LNumSymbols) do
    begin
      LSym.SymbolName := ReadCOFFSymbolName(AData, LOffset, LStringTableOffset);
      LSym.Value := PCardinal(@AData[LOffset + 8])^;
      LSym.SectionNumber := PSmallInt(@AData[LOffset + 12])^;
      LSym.SymbolType := PWord(@AData[LOffset + 14])^;
      LSym.StorageClass := AData[LOffset + 16];
      LSym.NumberOfAuxSymbols := AData[LOffset + 17];

      LObj.Symbols[LI] := LSym;

      // Skip auxiliary symbol records
      LOffset := LOffset + COFF_SYMBOL_SIZE;
      // Fill skipped aux records with empty entries
      for LJ := 0 to LSym.NumberOfAuxSymbols - 1 do
      begin
        Inc(LI);
        if LI < Integer(LNumSymbols) then
        begin
          LObj.Symbols[LI] := Default(TCOFFSymbol);
          LObj.Symbols[LI].SymbolName := '';
          LObj.Symbols[LI].StorageClass := 0;
        end;
        LOffset := LOffset + COFF_SYMBOL_SIZE;
      end;

      Inc(LI);
    end;
  end;

  Result := LObj;
end;

//==============================================================================
// AR Library Parsing
//==============================================================================

procedure TTigerLinker.ParseARLibrary(const AData: TBytes;
  const ASourcePath: string);
var
  LPos: Integer;
  LSig: AnsiString;
  LHeaderName: AnsiString;
  LHeaderSizeStr: AnsiString;
  LMemberSize: Cardinal;
  LMemberData: TBytes;
  LObj: TCOFFObject;
  LIsLinkerMember: Boolean;
  LTrimmedName: string;
begin
  if Length(AData) < 8 then
  begin
    Status('Linker: AR file too small: %s', [ASourcePath]);
    Exit;
  end;

  // Verify AR signature
  SetLength(LSig, 8);
  Move(AData[0], LSig[1], 8);
  if string(LSig) <> AR_SIGNATURE then
  begin
    Status('Linker: Invalid AR signature: %s', [ASourcePath]);
    Exit;
  end;

  LPos := 8;  // After signature

  while LPos + AR_HEADER_SIZE <= Length(AData) do
  begin
    // Read AR member header (60 bytes)
    // Name:    [0..15]  16 bytes
    // Date:    [16..27] 12 bytes
    // UID:     [28..33] 6 bytes
    // GID:     [34..39] 6 bytes
    // Mode:    [40..47] 8 bytes
    // Size:    [48..57] 10 bytes
    // EndMark: [58..59] "`\n"

    // Verify end marker
    if (AData[LPos + 58] <> Ord('`')) or (AData[LPos + 59] <> 10) then
    begin
      Status('Linker: Invalid AR member header at offset %d: %s', [LPos, ASourcePath]);
      Break;
    end;

    // Read member name
    SetLength(LHeaderName, 16);
    Move(AData[LPos], LHeaderName[1], 16);
    LHeaderName := AnsiString(TrimRight(string(LHeaderName)));

    // Read member size
    SetLength(LHeaderSizeStr, 10);
    Move(AData[LPos + 48], LHeaderSizeStr[1], 10);
    LMemberSize := StrToIntDef(Trim(string(LHeaderSizeStr)), 0);

    LPos := LPos + AR_HEADER_SIZE;

    // Classify this member
    LIsLinkerMember := (LHeaderName = '/') or (LHeaderName = '//');

    if (not LIsLinkerMember) and (LMemberSize >= COFF_FILE_HEADER_SIZE) then
    begin
      // This is an object member — parse it as COFF
      SetLength(LMemberData, LMemberSize);
      if LPos + Integer(LMemberSize) <= Length(AData) then
      begin
        Move(AData[LPos], LMemberData[0], LMemberSize);

        // Check that it looks like a COFF object (machine field)
        if (LMemberSize >= 2) and (PWord(@LMemberData[0])^ = IMAGE_FILE_MACHINE_AMD64) then
        begin
          LObj := ParseCOFFObject(LMemberData, ASourcePath);
          if LObj <> nil then
          begin
            // Clean up member name (remove trailing '/')
            LTrimmedName := string(LHeaderName);
            if (Length(LTrimmedName) > 0) and (LTrimmedName[Length(LTrimmedName)] = '/') then
              LTrimmedName := Copy(LTrimmedName, 1, Length(LTrimmedName) - 1);
            LObj.MemberName := LTrimmedName;
            FObjects.Add(LObj);
          end;
        end;
      end;
    end;

    // Advance past member data, respecting 2-byte alignment
    LPos := LPos + Integer(LMemberSize);
    if (LPos mod 2) <> 0 then
      Inc(LPos);
  end;

  Status('Linker: Parsed AR library %s — %d object members', [ASourcePath, FObjects.Count]);
end;

//==============================================================================
// Section Classification
//==============================================================================

function TTigerLinker.ClassifySection(const ASectionName: string;
  const ACharacteristics: Cardinal): TLinkerSectionKind;
begin
  // Match by name first (most reliable for COFF)
  if ASectionName = '.text' then
    Result := lskText
  else if ASectionName = '.rdata' then
    Result := lskRData
  else if ASectionName = '.data' then
    Result := lskData
  else if ASectionName = '.bss' then
    Result := lskBSS
  else if ASectionName = '.pdata' then
    Result := lskPData
  else if ASectionName = '.xdata' then
    Result := lskXData
  else if (ACharacteristics and IMAGE_SCN_CNT_CODE) <> 0 then
    Result := lskText
  else if (ACharacteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
    Result := lskBSS
  else if (ACharacteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
  begin
    if (ACharacteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
      Result := lskData
    else
      Result := lskRData;
  end
  else
    Result := lskOther;
end;

//==============================================================================
// Symbol Resolution
//==============================================================================

procedure TTigerLinker.BuildGlobalSymbolIndex();
var
  LI, LJ: Integer;
  LObj: TCOFFObject;
  LSym: TCOFFSymbol;
  LList: TList<Integer>;
begin
  // Build an index: symbol name → list of object indices that define it
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    for LJ := 0 to High(LObj.Symbols) do
    begin
      LSym := LObj.Symbols[LJ];

      // Skip aux symbols, section symbols, file symbols
      if (LSym.StorageClass = COFF_SYM_CLASS_FILE) or
         (LSym.SymbolName = '') then
        Continue;

      // Only index symbols that are DEFINED (SectionNumber > 0) and EXTERNAL
      if (LSym.SectionNumber > 0) and (LSym.StorageClass = COFF_SYM_CLASS_EXTERNAL) then
      begin
        if not FSymbolIndex.TryGetValue(LSym.SymbolName, LList) then
        begin
          LList := TList<Integer>.Create();
          FSymbolIndex.Add(LSym.SymbolName, LList);
        end;
        // Avoid duplicate entries for the same object
        if (LList.Count = 0) or (LList[LList.Count - 1] <> LI) then
          LList.Add(LI);
      end;
    end;
  end;
end;

procedure TTigerLinker.SelectObject(const AObjectIndex: Integer;
  const AWorkList: TList<Integer>);
var
  LObj: TCOFFObject;
  LSym: TCOFFSymbol;
  LI: Integer;
  LDefObjList: TList<Integer>;
begin
  LObj := FObjects[AObjectIndex];
  if LObj.Selected then
    Exit;

  LObj.Selected := True;
  Status('Linker: Selected object [%d] %s', [AObjectIndex, LObj.SourcePath]);

  // Find undefined symbols in this object — they create transitive dependencies
  for LI := 0 to High(LObj.Symbols) do
  begin
    LSym := LObj.Symbols[LI];
    if (LSym.SymbolName = '') or (LSym.StorageClass = COFF_SYM_CLASS_FILE) then
      Continue;

    // UNDEFINED external — needs resolution
    if (LSym.SectionNumber = 0) and (LSym.StorageClass = COFF_SYM_CLASS_EXTERNAL) then
    begin
      if FSymbolIndex.TryGetValue(LSym.SymbolName, LDefObjList) then
      begin
        // Pull in the first object that defines this symbol (if not already selected)
        if (LDefObjList.Count > 0) and (not FObjects[LDefObjList[0]].Selected) then
          AWorkList.Add(LDefObjList[0]);
      end;
    end;
  end;
end;

//==============================================================================
// Section Merging
//==============================================================================

function TTigerLinker.AlignUp(const AValue: Cardinal;
  const AAlignment: Cardinal): Cardinal;
var
  LMask: Cardinal;
begin
  if AAlignment <= 1 then
    Result := AValue
  else
  begin
    LMask := AAlignment - 1;
    Result := (AValue + LMask) and (not LMask);
  end;
end;

procedure TTigerLinker.MergeSelectedObjects();
var
  LI, LJ: Integer;
  LObj: TCOFFObject;
  LSect: TCOFFSection;
  LKind: TLinkerSectionKind;
  LContrib: TLinkerContribution;
  LTarget: TMemoryStream;
  LContribList: TList<TLinkerContribution>;
  LAlignment: Cardinal;
  LCurrentSize: Cardinal;
  LAlignedSize: Cardinal;
  LPadding: Cardinal;
  LZeroByte: Byte;
begin
  LZeroByte := 0;

  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Sections) do
    begin
      LSect := LObj.Sections[LJ];
      LKind := ClassifySection(LSect.SectionName, LSect.Characteristics);

      // Skip empty sections, COMDAT that we don't need, linker directives
      if (LSect.SizeOfRawData = 0) or (LKind = lskOther) then
        Continue;

      // Determine target stream and contribution list
      LTarget := nil;
      LContribList := nil;
      LAlignment := 1;

      case LKind of
        lskText:
        begin
          LTarget := FMergedText;
          LContribList := FTextContribs;
          LAlignment := 16;  // Code sections aligned to 16 bytes
        end;
        lskRData:
        begin
          LTarget := FMergedRData;
          LContribList := FRDataContribs;
          LAlignment := 4;
        end;
        lskData:
        begin
          LTarget := FMergedData;
          LContribList := FDataContribs;
          LAlignment := 8;
        end;
        lskBSS:
        begin
          // BSS — just accumulate size, align to 8
          FMergedBSS := AlignUp(FMergedBSS, 8) + LSect.SizeOfRawData;
          Continue;
        end;
        lskPData:
        begin
          LTarget := FMergedPData;
          LContribList := FPDataContribs;
          LAlignment := 4;
        end;
        lskXData:
        begin
          LTarget := FMergedXData;
          LContribList := FXDataContribs;
          LAlignment := 4;
        end;
      end;

      if (LTarget = nil) or (LContribList = nil) then
        Continue;

      // Align the stream position
      LCurrentSize := Cardinal(LTarget.Size);
      LAlignedSize := AlignUp(LCurrentSize, LAlignment);
      LPadding := LAlignedSize - LCurrentSize;
      while LPadding > 0 do
      begin
        LTarget.WriteBuffer(LZeroByte, 1);
        Dec(LPadding);
      end;

      // Record contribution
      LContrib.ObjectIndex := LI;
      LContrib.OrigSectionIndex := LJ;
      LContrib.MergedOffset := Cardinal(LTarget.Size);
      LContrib.MergedSize := LSect.SizeOfRawData;
      LContribList.Add(LContrib);

      // Write section data
      if Length(LSect.RawData) > 0 then
        LTarget.WriteBuffer(LSect.RawData[0], LSect.SizeOfRawData);
    end;
  end;

  Status('Linker: Merged sections -- .text=%d .rdata=%d .data=%d .pdata=%d .xdata=%d bytes',
    [FMergedText.Size, FMergedRData.Size, FMergedData.Size,
     FMergedPData.Size, FMergedXData.Size]);
end;

//==============================================================================
// Relocation Collection
//==============================================================================

procedure TTigerLinker.CollectRelocations();
var
  LI, LJ, LK, LM: Integer;
  LObj: TCOFFObject;
  LSect: TCOFFSection;
  LKind: TLinkerSectionKind;
  LReloc: TCOFFRelocation;
  LSym: TCOFFSymbol;
  LPending: TLinkerPendingReloc;
  LContrib: TLinkerContribution;
  LContribList: TList<TLinkerContribution>;
  LTargetContribList: TList<TLinkerContribution>;
  LFound: Boolean;
  LMergedBase: Cardinal;
  LAddend: Int32;
  LResSym: TLinkerResolvedSymbol;
  LTargetSectIdx: Integer;
begin
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Sections) do
    begin
      LSect := LObj.Sections[LJ];
      if Length(LSect.Relocations) = 0 then
        Continue;

      LKind := ClassifySection(LSect.SectionName, LSect.Characteristics);
      if LKind = lskOther then
        Continue;

      // Find the merged base offset for this section contribution
      case LKind of
        lskText:  LContribList := FTextContribs;
        lskRData: LContribList := FRDataContribs;
        lskData:  LContribList := FDataContribs;
        lskPData: LContribList := FPDataContribs;
        lskXData: LContribList := FXDataContribs;
      else
        Continue;
      end;

      // Find our contribution record
      LFound := False;
      LMergedBase := 0;
      for LK := 0 to LContribList.Count - 1 do
      begin
        LContrib := LContribList[LK];
        if (LContrib.ObjectIndex = LI) and (LContrib.OrigSectionIndex = LJ) then
        begin
          LMergedBase := LContrib.MergedOffset;
          LFound := True;
          Break;
        end;
      end;
      if not LFound then
        Continue;

      // Process each relocation
      for LK := 0 to High(LSect.Relocations) do
      begin
        LReloc := LSect.Relocations[LK];

        // Look up the target symbol
        if Integer(LReloc.SymbolTableIndex) >= Length(LObj.Symbols) then
          Continue;
        LSym := LObj.Symbols[LReloc.SymbolTableIndex];

        // Read the addend (existing value at the relocation site)
        LAddend := 0;
        if (LReloc.VirtualAddress + 4 <= LSect.SizeOfRawData) and
           (Length(LSect.RawData) >= Integer(LReloc.VirtualAddress) + 4) then
          LAddend := PInt32(@LSect.RawData[LReloc.VirtualAddress])^;

        LPending.SectionKind := LKind;
        LPending.OffsetInMerged := LMergedBase + LReloc.VirtualAddress;
        LPending.RelocationType := LReloc.RelocationType;
        LPending.Addend := LAddend;

        // Determine target symbol name
        if (LSym.SectionNumber > 0) and (LSym.StorageClass = COFF_SYM_CLASS_STATIC) then
        begin
          // Section symbol — the target is within the same object's section.
          // We need to resolve this to the merged offset.
          // The symbol's Value is the offset within the original section.
          // We synthesize a unique internal name.
          LPending.TargetSymbol := Format('__sect_%d_%d_%s',
            [LI, LSym.SectionNumber - 1, LObj.Sections[LSym.SectionNumber - 1].SectionName]);

          // Register this as a resolved symbol if not already registered
          if not FResolvedSymbols.ContainsKey(LPending.TargetSymbol) then
          begin
            LResSym.SymbolName := LPending.TargetSymbol;
            LResSym.SectionKind := ClassifySection(
              LObj.Sections[LSym.SectionNumber - 1].SectionName,
              LObj.Sections[LSym.SectionNumber - 1].Characteristics);
            // Find the merged base for the target section
            LResSym.OffsetInMerged := 0;
            case LResSym.SectionKind of
              lskText:  LTargetContribList := FTextContribs;
              lskRData: LTargetContribList := FRDataContribs;
              lskData:  LTargetContribList := FDataContribs;
              lskPData: LTargetContribList := FPDataContribs;
              lskXData: LTargetContribList := FXDataContribs;
            else
              LTargetContribList := nil;
            end;
            if LTargetContribList <> nil then
            begin
              LTargetSectIdx := LSym.SectionNumber - 1;
              for LM := 0 to LTargetContribList.Count - 1 do
              begin
                if (LTargetContribList[LM].ObjectIndex = LI) and
                   (LTargetContribList[LM].OrigSectionIndex = LTargetSectIdx) then
                begin
                  LResSym.OffsetInMerged := LTargetContribList[LM].MergedOffset;
                  Break;
                end;
              end;
            end;
            FResolvedSymbols.Add(LPending.TargetSymbol, LResSym);
          end;
        end
        else
          LPending.TargetSymbol := LSym.SymbolName;

        FPendingRelocs.Add(LPending);
      end;
    end;
  end;

  Status('Linker: Collected %d pending relocations', [FPendingRelocs.Count]);
end;

//==============================================================================
// Public: Input
//==============================================================================

procedure TTigerLinker.AddObjectFile(const APath: string);
var
  LData: TBytes;
  LObj: TCOFFObject;
begin
  if not FileExists(APath) then
  begin
    Status('Linker: Object file not found: %s', [APath]);
    Exit;
  end;

  LData := TFile.ReadAllBytes(APath);
  LObj := ParseCOFFObject(LData, APath);
  if LObj <> nil then
  begin
    FObjects.Add(LObj);
    Status('Linker: Added object file: %s (%d sections, %d symbols)',
      [APath, Length(LObj.Sections), Length(LObj.Symbols)]);
  end;
end;

procedure TTigerLinker.AddLibraryFile(const APath: string);
var
  LData: TBytes;
begin
  if not FileExists(APath) then
  begin
    Status('Linker: Library file not found: %s', [APath]);
    Exit;
  end;

  LData := TFile.ReadAllBytes(APath);
  ParseARLibrary(LData, APath);
end;

procedure TTigerLinker.AddObjectFromMemory(const AData: TBytes;
  const AName: string);
var
  LObj: TCOFFObject;
begin
  LObj := ParseCOFFObject(AData, AName);
  if LObj <> nil then
  begin
    FObjects.Add(LObj);
    Status('Linker: Added in-memory object: %s (%d sections, %d symbols)',
      [AName, Length(LObj.Sections), Length(LObj.Symbols)]);
  end;
end;

//==============================================================================
// Public: Resolve
//==============================================================================

procedure TTigerLinker.Resolve(const ANeededSymbols: TStrings);
var
  LI: Integer;
  LWorkList: TList<Integer>;
  LDefObjList: TList<Integer>;
  LSymName: string;
  LObj: TCOFFObject;
  LSym: TCOFFSymbol;
  LJ: Integer;
  LResSym: TLinkerResolvedSymbol;
  LContribList: TList<TLinkerContribution>;
  LContrib: TLinkerContribution;
  LK: Integer;
  LSectIdx: Integer;
  LFound: Boolean;
begin
  Status('Linker: Resolving %d needed symbols across %d objects',
    [ANeededSymbols.Count, FObjects.Count]);

  // Phase 1: Build the global symbol index
  BuildGlobalSymbolIndex();

  // Phase 2: Select objects that define needed symbols (+ transitive deps)
  LWorkList := TList<Integer>.Create();
  try
    // Seed the work list with objects that define our needed symbols
    for LI := 0 to ANeededSymbols.Count - 1 do
    begin
      LSymName := ANeededSymbols[LI];
      if FSymbolIndex.TryGetValue(LSymName, LDefObjList) then
      begin
        if LDefObjList.Count > 0 then
          LWorkList.Add(LDefObjList[0]);
      end;
    end;

    // Process work list — selecting objects pulls in transitive dependencies
    LI := 0;
    while LI < LWorkList.Count do
    begin
      SelectObject(LWorkList[LI], LWorkList);
      Inc(LI);
    end;
  finally
    LWorkList.Free();
  end;

  // Phase 3: Merge sections from selected objects
  MergeSelectedObjects();

  // Phase 4: Build resolved symbol map
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Symbols) do
    begin
      LSym := LObj.Symbols[LJ];
      if (LSym.SymbolName = '') or (LSym.SectionNumber <= 0) then
        Continue;
      if LSym.StorageClass <> COFF_SYM_CLASS_EXTERNAL then
        Continue;

      // This is a defined external symbol — resolve it
      LSectIdx := LSym.SectionNumber - 1;
      if (LSectIdx < 0) or (LSectIdx >= Length(LObj.Sections)) then
        Continue;

      LResSym.SymbolName := LSym.SymbolName;
      LResSym.SectionKind := ClassifySection(
        LObj.Sections[LSectIdx].SectionName,
        LObj.Sections[LSectIdx].Characteristics);

      // Find the merged base for this section contribution
      case LResSym.SectionKind of
        lskText:  LContribList := FTextContribs;
        lskRData: LContribList := FRDataContribs;
        lskData:  LContribList := FDataContribs;
        lskPData: LContribList := FPDataContribs;
        lskXData: LContribList := FXDataContribs;
      else
        Continue;
      end;

      LFound := False;
      for LK := 0 to LContribList.Count - 1 do
      begin
        LContrib := LContribList[LK];
        if (LContrib.ObjectIndex = LI) and (LContrib.OrigSectionIndex = LSectIdx) then
        begin
          LResSym.OffsetInMerged := LContrib.MergedOffset + LSym.Value;
          LFound := True;
          Break;
        end;
      end;

      if LFound then
      begin
        if not FResolvedSymbols.ContainsKey(LSym.SymbolName) then
          FResolvedSymbols.Add(LSym.SymbolName, LResSym);
      end;
    end;
  end;

  // Phase 5: Collect relocations from selected objects
  CollectRelocations();

  // Phase 6: Identify unresolved symbols
  for LI := 0 to ANeededSymbols.Count - 1 do
  begin
    if not FResolvedSymbols.ContainsKey(ANeededSymbols[LI]) then
      FUnresolvedSymbols.Add(ANeededSymbols[LI]);
  end;

  Status('Linker: Resolved %d symbols, %d unresolved, %d pending relocations',
    [FResolvedSymbols.Count, FUnresolvedSymbols.Count, FPendingRelocs.Count]);
end;

//==============================================================================
// Public: Output Getters
//==============================================================================

function TTigerLinker.GetMergedText(): TBytes;
begin
  SetLength(Result, FMergedText.Size);
  if FMergedText.Size > 0 then
  begin
    FMergedText.Position := 0;
    FMergedText.ReadBuffer(Result[0], FMergedText.Size);
  end;
end;

function TTigerLinker.GetMergedTextSize(): Cardinal;
begin
  Result := Cardinal(FMergedText.Size);
end;

function TTigerLinker.GetMergedRData(): TBytes;
begin
  SetLength(Result, FMergedRData.Size);
  if FMergedRData.Size > 0 then
  begin
    FMergedRData.Position := 0;
    FMergedRData.ReadBuffer(Result[0], FMergedRData.Size);
  end;
end;

function TTigerLinker.GetMergedRDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedRData.Size);
end;

function TTigerLinker.GetMergedData(): TBytes;
begin
  SetLength(Result, FMergedData.Size);
  if FMergedData.Size > 0 then
  begin
    FMergedData.Position := 0;
    FMergedData.ReadBuffer(Result[0], FMergedData.Size);
  end;
end;

function TTigerLinker.GetMergedDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedData.Size);
end;

function TTigerLinker.GetMergedPData(): TBytes;
begin
  SetLength(Result, FMergedPData.Size);
  if FMergedPData.Size > 0 then
  begin
    FMergedPData.Position := 0;
    FMergedPData.ReadBuffer(Result[0], FMergedPData.Size);
  end;
end;

function TTigerLinker.GetMergedPDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedPData.Size);
end;

function TTigerLinker.GetMergedXData(): TBytes;
begin
  SetLength(Result, FMergedXData.Size);
  if FMergedXData.Size > 0 then
  begin
    FMergedXData.Position := 0;
    FMergedXData.ReadBuffer(Result[0], FMergedXData.Size);
  end;
end;

function TTigerLinker.GetMergedXDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedXData.Size);
end;

function TTigerLinker.GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>;
begin
  Result := FResolvedSymbols;
end;

function TTigerLinker.GetUnresolvedSymbols(): TStringList;
begin
  Result := FUnresolvedSymbols;
end;

function TTigerLinker.GetPendingRelocations(): TList<TLinkerPendingReloc>;
begin
  Result := FPendingRelocs;
end;

function TTigerLinker.GetObjectCount(): Integer;
begin
  Result := FObjects.Count;
end;

function TTigerLinker.GetSelectedObjectCount(): Integer;
var
  LI: Integer;
begin
  Result := 0;
  for LI := 0 to FObjects.Count - 1 do
  begin
    if FObjects[LI].Selected then
      Inc(Result);
  end;
end;

end.
