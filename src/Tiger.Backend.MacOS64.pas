{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend.MacOS64;

{$I Tiger.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Math,
  Tiger.Utils,
  Tiger.Utils.Win64,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.Backend.ARM64,
  Tiger.ABI.MacOS64;

type
  { TTigerMacOS64Backend }
  TTigerMacOS64Backend = class(TTigerBackend)
  private
    function GenerateMachO(): TBytes;
    procedure EnsureOutputDir();
  protected
    procedure PreBuild(); override;
  public
    function TargetExe(const APath: string; const ASubsystem: TTigerSubsystem = ssConsole): TTigerBackend; override;
    function BuildToMemory(): TBytes; override;
    function Run(): Cardinal; override;
    procedure Clear(); override;
  end;

implementation

uses
  Tiger.ABI,
  Tiger.Linker,
  Tiger.Linker.MachO;

const
  ERR_BACKEND_NO_ACTIVE_FUNC = 'B001';

  // Mach-O 64-bit constants
  MH_MAGIC_64    = $FEEDFACF;
  CPU_TYPE_ARM64 = $0100000C;
  CPU_SUBTYPE_ARM64_ALL = 0;
  MH_EXECUTE     = 2;
  LC_SEGMENT_64  = $19;
  LC_REQ_DYLD    = $80000000;
  LC_MAIN        = $28 or LC_REQ_DYLD;
  LC_LOAD_DYLINKER = $0E;
  LC_LOAD_DYLIB   = $0C;
  LC_DYLD_INFO_ONLY = $22 or LC_REQ_DYLD;
  LC_SYMTAB       = $02;
  LC_DYSYMTAB     = $0B;

  SEG_TEXT = '__TEXT';
  SEG_DATA = '__DATA';
  SECT_TEXT = '__text';
  SECT_CSTRING = '__cstring';
  SECT_DATA = '__data';
  SECT_GOT = '__got';

  PAGE_SIZE = 4096;
  SEG_TEXT_VADDR = 0;
  SEG_DATA_VADDR = $100000000;  // After 4GB

procedure TTigerMacOS64Backend.EnsureOutputDir();
begin
  if not TPath.HasExtension(FOutputPath) then
    TUtils.CreateDirInPath(FOutputPath + '.macho')
  else
    TUtils.CreateDirInPath(FOutputPath);
end;

function TTigerMacOS64Backend.TargetExe(const APath: string;
  const ASubsystem: TTigerSubsystem): TTigerBackend;
begin
  FOutputPath := ChangeFileExt(APath, '');
  FOutputType := otExe;
  FSubsystem := ASubsystem;
  Result := Self;
end;

procedure TTigerMacOS64Backend.PreBuild();
begin
  EnsureOutputDir();
end;

function TTigerMacOS64Backend.BuildToMemory(): TBytes;
begin
  case FOutputType of
    otExe:
      Result := GenerateMachO();
    otObj, otLib, otDll:
      Result := nil;  // Phase 3/4
  else
    Result := nil;
  end;
end;

function TTigerMacOS64Backend.Run(): Cardinal;
begin
  Result := ERROR_BAD_FORMAT;
  Status('Run not supported for macOS target from Windows. Copy binary to Mac and run there.');
end;

procedure TTigerMacOS64Backend.Clear();
begin
  inherited Clear();
end;

function TTigerMacOS64Backend.GenerateMachO(): TBytes;
var
  LCode: TTigerCodeBuilder;
  LData: TTigerDataBuilder;
  LGlobals: TTigerDataBuilder;
  LImports: TTigerImportBuilder;
  LFuncCount, LFuncIdx, LInstrIdx, LI, LK: Integer;
  LFunc: TTigerFuncInfo;
  LInstr: TTigerInstruction;
  LTextStream, LCStringStream, LDataStream, LGotStream: TMemoryStream;
  LTextBytes, LCStringBytes, LDataBytes, LGotBytes: TBytes;
  LEntryOffset: UInt64;
  LFuncOffsets: TDictionary<Integer, Cardinal>;
  LCallFixups: TList<TPair<Cardinal, Integer>>;
  LImportFixups: TList<TPair<Cardinal, Integer>>;
  LDataFixups: TList<TPair<Cardinal, Integer>>;
  LLabelOffsets: TDictionary<Integer, Cardinal>;
  LJumpFixups: TList<TPair<Cardinal, Integer>>;
  LCondJumpFixups: TList<TPair<Cardinal, Integer>>;
  LOutStream: TMemoryStream;
  LHeaderSize, LLoadCmdSize: Cardinal;
  LSegTextFileOff, LSegTextFileSize, LSegDataFileOff, LSegDataFileSize: Cardinal;
  LTextFileOff, LCStringFileOff, LDataFileOff, LGotFileOff: Cardinal;
  LGotVAddr, LSlotAddr: UInt64;
  LOfs12: Cardinal;
  LFrameSize, LLocalsSize, LMaxCallArgs, LOutgoingArgSpace: Cardinal;
  LStackFrameSize: Cardinal;
  LStaticImportIndices: TList<Integer>;
  LStaticSymbolNames: TStringList;
  LStaticLibPaths: TStringList;
  LStaticCallFixups: TList<TPair<Cardinal, Integer>>;
  LStaticResolved: TDictionary<string, TLinkerResolvedSymbol>;
  LHasStaticImports: Boolean;
  LLinker: TTigerMachOLinker;
  LExternalTextBase: Cardinal;
  LMergedBytes: TBytes;
  LResolvedSym: TLinkerResolvedSymbol;
  LEntry: TTigerImportEntry;
  LLibPath, LResolvedPath, LCandidate: string;
  LJ: Integer;
  LBindStream: TMemoryStream;
  LBindBytes: TBytes;
  LBindOff: Cardinal;
  LVal: UInt64;
  LLabelOffset: Cardinal;
  LAdrpImm: Cardinal;
  LByteVal: Byte;
  LCardVal: Cardinal;
  LByteOffset: Int64;
  LPageIndex: Int64;
  procedure WriteFixedAnsi(const AText: AnsiString; const ASize: Integer);
  var
    LBuf: TBytes;
    LCopyLen: Integer;
  begin
    SetLength(LBuf, ASize);
    FillChar(LBuf[0], ASize, 0);
    LCopyLen := Length(AText);
    if LCopyLen > ASize then
      LCopyLen := ASize;
    if LCopyLen > 0 then
      Move(AText[1], LBuf[0], LCopyLen);
    LOutStream.WriteBuffer(LBuf[0], ASize);
  end;

  procedure WritePaddedCString(const AText: AnsiString; const ASize: Integer);
  var
    LBuf: TBytes;
    LCopyLen: Integer;
  begin
    SetLength(LBuf, ASize);
    FillChar(LBuf[0], ASize, 0);
    LCopyLen := Length(AText);
    if LCopyLen > (ASize - 1) then
      LCopyLen := ASize - 1;
    if LCopyLen > 0 then
      Move(AText[1], LBuf[0], LCopyLen);
    LOutStream.WriteBuffer(LBuf[0], ASize);
  end;

  procedure EmitU32(const AValue: Cardinal);
  var
    V: Cardinal;
  begin
    V := AValue;
    LTextStream.WriteBuffer(V, 4);
  end;

  procedure EmitU64(const AValue: UInt64);
  begin
    LTextStream.WriteBuffer(AValue, 8);
  end;

  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    Result := 16 + AIndex * 8;
  end;

  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LOffset, LK: Integer;
  begin
    LOffset := 16;
    for LK := 0 to High(LFunc.Params) do
      LOffset := LOffset + 8;
    for LK := 0 to AIndex - 1 do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -LOffset;
  end;

  function GetTempOffset(const ATempIndex: Integer): Int32;
  var
    LOffset, LK: Integer;
  begin
    LOffset := 16;
    for LK := 0 to High(LFunc.Params) do
      LOffset := LOffset + 8;
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    LOffset := LOffset + ATempIndex * 8;
    Result := -LOffset;
  end;

  procedure EmitARM64(const AInsn: Cardinal);
  begin
    LTextStream.WriteBuffer(AInsn, 4);
  end;

  procedure EmitMovX(const ADest, AVal: Byte);
  begin
    if (AVal >= 0) and (AVal <= 65535) then
      EmitARM64($D2800000 or (Cardinal(AVal) shl 5) or ADest)
    else
      EmitARM64($D2800000 or (Cardinal(ADest) and 31));
  end;

  procedure EmitMovRegImm64(const ARd: Byte; const AImm: UInt64);
  var
    LImm: UInt64;
    I: Integer;
    LW: Cardinal;
  begin
    LImm := AImm;
    EmitARM64($D2800000 or ((Cardinal(LImm and $FFFF) shl 5) or ARd));
    for I := 1 to 3 do
    begin
      LW := Cardinal((LImm shr (I * 16)) and $FFFF);
      if LW <> 0 then
        EmitARM64($F2800000 or (Cardinal(I) shl 21) or ((LW shl 5) or ARd));
    end;
  end;

  procedure EmitAddImm(const ARd, ARn: Byte; const AImm: Cardinal);
  begin
    if AImm <= 4095 then
      EmitARM64($91000000 or (Cardinal(AImm) shl 10) or (Cardinal(ARn) shl 5) or ARd)
    else
    begin
      EmitMovRegImm64(REG_X16, AImm);
      EmitARM64($8B000000 or (REG_X16 shl 16) or (Cardinal(ARn) shl 5) or ARd);
    end;
  end;

  procedure EmitSubImm(const ARd, ARn: Byte; const AImm: Cardinal);
  begin
    if AImm <= 4095 then
      EmitARM64($D1000000 or (Cardinal(AImm) shl 10) or (Cardinal(ARn) shl 5) or ARd)
    else
    begin
      EmitMovRegImm64(REG_X16, AImm);
      EmitARM64($CB000000 or (REG_X16 shl 16) or (Cardinal(ARn) shl 5) or ARd);
    end;
  end;

  procedure EmitStpPre(const ARt1, ARt2, ARn: Byte; const AImm: Int32);
  var
    LImm7: Cardinal;
  begin
    LImm7 := Byte((Int32(AImm) div 8) and $7F);
    EmitARM64($A9A00000 or (LImm7 shl 15) or (Cardinal(ARn) shl 5) or ARt1 or (Cardinal(ARt2) shl 10));
  end;

  procedure EmitLdpPost(const ARt1, ARt2, ARn: Byte; const AImm: Cardinal);
  var
    LImm7: Cardinal;
  begin
    LImm7 := (AImm div 8) and 127;
    EmitARM64($A8C00000 or (LImm7 shl 15) or (Cardinal(ARn) shl 5) or ARt1 or (Cardinal(ARt2) shl 10));
  end;

  procedure EmitLdrX(const ARt, ARn: Byte; const AOffset: Cardinal);
  begin
    if (AOffset <= 32760) and ((AOffset and 7) = 0) then
      EmitARM64($F9400000 or ((AOffset div 8) shl 10) or (Cardinal(ARn) shl 5) or ARt)
    else
    begin
      EmitMovRegImm64(REG_X16, AOffset);
      EmitARM64($F8606800 or (REG_X16 shl 16) or (Cardinal(ARn) shl 5) or ARt);
    end;
  end;

  procedure EmitStrX(const ARt, ARn: Byte; const AOffset: Cardinal);
  begin
    if (AOffset <= 32760) and ((AOffset and 7) = 0) then
      EmitARM64($F9000000 or ((AOffset div 8) shl 10) or (Cardinal(ARn) shl 5) or ARt)
    else
    begin
      EmitMovRegImm64(REG_X16, AOffset);
      EmitARM64($F8206800 or (REG_X16 shl 16) or (Cardinal(ARn) shl 5) or ARt);
    end;
  end;

  procedure EmitLdurFp(const ARt: Byte; const ADisp: Int32);
  var
    LImm9: Cardinal;
  begin
    LImm9 := Cardinal(Int32(ADisp) and $1FF);
    EmitARM64($F85F0000 or (LImm9 shl 12) or (REG_FP shl 5) or ARt);
  end;

  procedure EmitSturFp(const ADisp: Int32; const ARt: Byte);
  var
    LImm9: Cardinal;
  begin
    LImm9 := Cardinal(Int32(ADisp) and $1FF);
    EmitARM64($F81F0000 or (LImm9 shl 12) or (REG_FP shl 5) or ARt);
  end;

  procedure EmitLdrFp(const ARt: Byte; const ADisp: Int32);
  var
    LOff: Cardinal;
  begin
    if ADisp >= 0 then
    begin
      LOff := Cardinal(ADisp);
      if (LOff <= 32760) and ((LOff and 7) = 0) then
        EmitARM64($F9400000 or ((LOff div 8) shl 10) or (REG_FP shl 5) or ARt)
      else
      begin
        EmitMovRegImm64(REG_X16, LOff);
        EmitARM64($8B2063E0 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($F9400000 or (REG_X16 shl 10) or (REG_X16 shl 5) or ARt);
      end;
    end
    else if ADisp >= -256 then
      EmitLdurFp(ARt, ADisp)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB2063E0 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($F9400000 or (REG_X16 shl 5) or ARt);
    end;
  end;

  procedure EmitStrFp(const ADisp: Int32; const ARt: Byte);
  var
    LOff: Cardinal;
  begin
    if ADisp >= 0 then
    begin
      LOff := Cardinal(ADisp);
      if (LOff <= 32760) and ((LOff and 7) = 0) then
        EmitARM64($F9000000 or ((LOff div 8) shl 10) or (REG_FP shl 5) or ARt)
      else
      begin
        EmitMovRegImm64(REG_X16, LOff);
        EmitARM64($8B2063E0 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($F9000000 or (ARt shl 10) or (REG_X16 shl 5) or ARt);
      end;
    end
    else if ADisp >= -256 then
      EmitSturFp(ADisp, ARt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB2063E0 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($F9000000 or (ARt shl 10) or (REG_X16 shl 5) or ARt);
    end;
  end;

  procedure EmitBL(const AOffset: Int32);
  var
    LImm26: Cardinal;
  begin
    LImm26 := (Cardinal(AOffset) shr 2) and $3FFFFFF;
    EmitARM64($94000000 or LImm26);
  end;

  procedure EmitBLR(const ARn: Byte);
  begin
    EmitARM64($D63F0000 or (Cardinal(ARn) shl 5));
  end;

  procedure EmitRet();
  begin
    EmitARM64($D65F03C0);
  end;

  procedure EmitAdrp(const ARd: Byte; const APage: Int32);
  var
    LImm: Cardinal;
  begin
    LImm := Cardinal(APage) and $1FFFFF;
    EmitARM64($90000000 or ((LImm and $3) shl 29) or ((LImm shr 2) shl 16) or ARd);
  end;

  procedure LoadOperandToReg(const AOp: TTigerOperand; const AReg: Byte);
  begin
    case AOp.Kind of
      okImmediate:
        EmitMovRegImm64(AReg, UInt64(AOp.ImmInt));
      okTemp:
        EmitLdrFp(AReg, GetTempOffset(AOp.TempHandle.Index));
      okLocal:
        if AOp.LocalHandle.IsParam then
          EmitLdrFp(AReg, GetParamOffset(AOp.LocalHandle.Index))
        else
          EmitLdrFp(AReg, GetLocalOffset(AOp.LocalHandle.Index));
      okData:
        begin
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), AOp.DataHandle.Index));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
      okGlobal:
        begin
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), AOp.DataHandle.Index));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
    else
      EmitMovRegImm64(AReg, 0);
    end;
  end;

  procedure StoreTempFromReg(const ATempIndex: Integer; const AReg: Byte);
  begin
    if GetTempOffset(ATempIndex) >= -255 then
      EmitStrFp(GetTempOffset(ATempIndex), AReg)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-GetTempOffset(ATempIndex)));
      EmitARM64($CB3063E0 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitStrX(AReg, REG_X16, 0);
    end;
  end;

begin
  Result := nil;
  LCode := GetCode();
  LData := GetData();
  LGlobals := GetGlobals();
  LImports := GetImports();
  LFuncCount := LCode.GetFuncCount();
  if LFuncCount = 0 then
  begin
    Status('Backend: No functions to emit');
    Exit;
  end;

  LTextStream := TMemoryStream.Create();
  LCStringStream := TMemoryStream.Create();
  LDataStream := TMemoryStream.Create();
  LGotStream := TMemoryStream.Create();
  LFuncOffsets := TDictionary<Integer, Cardinal>.Create();
  LCallFixups := TList<TPair<Cardinal, Integer>>.Create();
  LImportFixups := TList<TPair<Cardinal, Integer>>.Create();
  LDataFixups := TList<TPair<Cardinal, Integer>>.Create();
  LJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LCondJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LLabelOffsets := TDictionary<Integer, Cardinal>.Create();

  LStaticImportIndices := TList<Integer>.Create();
  LStaticSymbolNames := TStringList.Create();
  LStaticLibPaths := TStringList.Create();
  LStaticLibPaths.CaseSensitive := False;
  LStaticLibPaths.Sorted := True;
  LStaticLibPaths.Duplicates := dupIgnore;
  LStaticCallFixups := TList<TPair<Cardinal, Integer>>.Create();
  LHasStaticImports := False;
  LLinker := nil;
  try
    for LI := 0 to LImports.GetCount() - 1 do
    begin
      LEntry := LImports.GetEntryByIndex(LI);
      if LEntry.IsStatic then
      begin
        LStaticImportIndices.Add(LI);
        LStaticSymbolNames.Add(LEntry.FuncName);
        LLibPath := LEntry.DllName;
        if TPath.GetExtension(LLibPath) = '' then
          LLibPath := LLibPath + '.a';
        if not TPath.IsPathRooted(LLibPath) then
        begin
          LResolvedPath := '';
          for LJ := 0 to FLibPaths.Count - 1 do
          begin
            LCandidate := TPath.Combine(FLibPaths[LJ], LLibPath);
            if TFile.Exists(LCandidate) then
            begin
              LResolvedPath := LCandidate;
              Break;
            end;
          end;
          if LResolvedPath = '' then
            LResolvedPath := TPath.Combine(TPath.GetDirectoryName(FOutputPath), LLibPath);
          LLibPath := LResolvedPath;
        end;
        LStaticLibPaths.Add(LLibPath);
        LHasStaticImports := True;
      end;
    end;

    if LHasStaticImports then
    begin
      LLinker := TTigerMachOLinker.Create();
      CopyStatusCallbackTo(LLinker);
      for LI := 0 to LStaticLibPaths.Count - 1 do
        LLinker.AddLibraryFile(LStaticLibPaths[LI]);
      LLinker.Resolve(LStaticSymbolNames);
      LStaticResolved := LLinker.GetResolvedSymbols();
    end;

    for LI := 0 to LImports.GetCount() - 1 do
    begin
      LVal := 0;
      LGotStream.WriteBuffer(LVal, 8);
    end;

    LCStringBytes := LData.GetData();
    if Length(LCStringBytes) > 0 then
      LCStringStream.WriteBuffer(LCStringBytes[0], Length(LCStringBytes));
    LDataBytes := LGlobals.GetData();
    if Length(LDataBytes) > 0 then
      LDataStream.WriteBuffer(LDataBytes[0], Length(LDataBytes));

    for LFuncIdx := 0 to LFuncCount - 1 do
    begin
      LFunc := LCode.GetFunc(LFuncIdx);
      LFuncOffsets.Add(LFuncIdx, Cardinal(LTextStream.Position));

      LLocalsSize := 0;
      for LI := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + Cardinal(LFunc.Locals[LI].LocalSize);
      LMaxCallArgs := 0;
      for LI := 0 to High(LFunc.Instructions) do
        if LFunc.Instructions[LI].Kind in [ikCallImport, ikCall, ikCallIndirect] then
          if Length(LFunc.Instructions[LI].Args) > Integer(LMaxCallArgs) then
            LMaxCallArgs := Length(LFunc.Instructions[LI].Args);
      if LMaxCallArgs > 8 then
        LOutgoingArgSpace := LMaxCallArgs * 8
      else
        LOutgoingArgSpace := 64;
      LStackFrameSize := 16 + Cardinal(Length(LFunc.Params)) * 8 + LLocalsSize + Cardinal(LFunc.TempCount) * 8 + LOutgoingArgSpace;
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

      EmitStpPre(REG_FP, REG_LR, REG_SP, 16);
      EmitARM64($910003E0 or (REG_FP shl 5) or REG_SP);
      if LStackFrameSize > 0 then
        EmitSubImm(REG_SP, REG_SP, LStackFrameSize);

      for LI := 0 to Min(Length(LFunc.Params) - 1, 7) do
        EmitStrFp(GetParamOffset(LI), LI);

      for LInstrIdx := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LInstrIdx];
        case LInstr.Kind of
          ikCallImport:
            begin
              for LK := 0 to Min(Length(LInstr.Args) - 1, 7) do
                LoadOperandToReg(LInstr.Args[LK], LK);
              if LHasStaticImports and (LStaticImportIndices.IndexOf(LInstr.ImportTarget.Index) >= 0) then
              begin
                EmitBL(0);
                LStaticCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position) - 4, LInstr.ImportTarget.Index));
              end
              else
              begin
                LImportFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LInstr.ImportTarget.Index));
                EmitAdrp(REG_X16, 0);
                EmitARM64($F9400000 or (0 shl 10) or (REG_X16 shl 5) or REG_X16);
                EmitBLR(REG_X16);
              end;
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCall:
            begin
              for LK := 0 to Min(Length(LInstr.Args) - 1, 7) do
                LoadOperandToReg(LInstr.Args[LK], LK);
              LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LInstr.FuncTarget.Index));
              EmitBL(0);
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikReturn:
            begin
              if LStackFrameSize > 0 then
                EmitAddImm(REG_SP, REG_SP, LStackFrameSize);
              EmitLdpPost(REG_FP, REG_LR, REG_SP, 16);
              EmitRet();
            end;
          ikReturnValue:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              if LStackFrameSize > 0 then
                EmitAddImm(REG_SP, REG_SP, LStackFrameSize);
              EmitLdpPost(REG_FP, REG_LR, REG_SP, 16);
              EmitRet();
            end;
          ikStore:
            begin
              LoadOperandToReg(LInstr.Op2, REG_X0);
              if LInstr.Op1.LocalHandle.IsParam then
                EmitStrFp(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_X0)
              else
                EmitStrFp(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_X0);
            end;
          ikLoad:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikAdd:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikSub:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikMul:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($9B007C00 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikDiv:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($9BC07C00 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikMod:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($AA0003E0 or (REG_X0 shl 5) or REG_X17);
              EmitARM64($9BC07C00 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              EmitARM64($9B000000 or (REG_X16 shl 16) or (REG_X17 shl 10) or (REG_X0 shl 5) or REG_X17);
              StoreTempFromReg(LInstr.Dest.Index, REG_X17);
            end;
          ikBitAnd:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($8A200000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitOr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($AA200000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitXor:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($CA200000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitNot:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              EmitARM64($AA2003E0 or (REG_X0 shl 16) or (REG_SP shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikShl:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($9AC02000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikShr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($9AC02800 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpEq:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F03E0 or (0 shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpNe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F03E0 or (1 shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpLt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F03E0 or ($0B shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpLe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F03E0 or ($0D shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpGt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F03E0 or ($0C shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpGe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F03E0 or ($0A shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCallIndirect:
            begin
              for LK := 0 to Min(Length(LInstr.Args) - 1, 7) do
                LoadOperandToReg(LInstr.Args[LK], LK);
              LoadOperandToReg(LInstr.Op1, REG_X16);
              EmitARM64($D63F0000 or (REG_X16 shl 5));
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikStorePtr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($F9000000 or (REG_X0 shl 5) or REG_X16);
            end;
          ikLoadPtr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              EmitARM64($F9400000 or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikAddressOf:
            begin
              if LInstr.Op1.LocalHandle.IsParam then
                EmitAddImm(REG_X0, REG_FP, Cardinal(GetParamOffset(LInstr.Op1.LocalHandle.Index)))
              else
                EmitSubImm(REG_X0, REG_FP, Cardinal(-GetLocalOffset(LInstr.Op1.LocalHandle.Index)));
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikLabel:
            if LInstr.LabelTarget.IsValid() then
              LLabelOffsets.AddOrSetValue((LFuncIdx shl 16) or LInstr.LabelTarget.Index, Cardinal(LTextStream.Position));
          ikJump:
            if LInstr.LabelTarget.IsValid() then
            begin
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), (LFuncIdx shl 16) or LInstr.LabelTarget.Index));
              EmitARM64($14000000);
            end;
          ikJumpIf:
            if LInstr.LabelTarget.IsValid() then
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), (LFuncIdx shl 16) or LInstr.LabelTarget.Index));
              EmitARM64($35000000);
            end;
          ikJumpIfNot:
            if LInstr.LabelTarget.IsValid() then
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), (LFuncIdx shl 16) or LInstr.LabelTarget.Index));
              EmitARM64($34000000);
            end;
          ikNop:
            ;
        else
          ;
        end;
      end;
    end;

    SetLength(LTextBytes, LTextStream.Size);
    if LTextStream.Size > 0 then
    begin
      LTextStream.Position := 0;
      LTextStream.ReadBuffer(LTextBytes[0], LTextStream.Size);
    end;

    if LHasStaticImports and (LLinker <> nil) then
    begin
      LExternalTextBase := Length(LTextBytes);
      LMergedBytes := LLinker.GetMergedText();
      if Length(LMergedBytes) > 0 then
      begin
        SetLength(LTextBytes, Length(LTextBytes) + Length(LMergedBytes));
        Move(LMergedBytes[0], LTextBytes[LExternalTextBase], Length(LMergedBytes));
      end;
      for LI := 0 to LStaticCallFixups.Count - 1 do
      begin
        for LJ := 0 to LStaticImportIndices.Count - 1 do
          if LStaticImportIndices[LJ] = LStaticCallFixups[LI].Value then
          begin
            if LStaticResolved.TryGetValue(LStaticSymbolNames[LJ], LResolvedSym) and
               (LResolvedSym.SectionKind = lskText) then
            begin
              if (LStaticCallFixups[LI].Key + 4 <= Cardinal(Length(LTextBytes))) then
              begin
                LInstrIdx := (Int64(LExternalTextBase) + Int64(LResolvedSym.OffsetInMerged) - Int64(LStaticCallFixups[LI].Key) - 4) shr 2;
                if (LInstrIdx >= -33554432) and (LInstrIdx <= 33554431) then
                  PCardinal(@LTextBytes[LStaticCallFixups[LI].Key])^ := $94000000 or (Cardinal(LInstrIdx) and $3FFFFFF);
              end;
            end;
            Break;
          end;
      end;
      Status('Static linker: %d symbols resolved, external .text at offset %d',
        [LStaticResolved.Count, LExternalTextBase]);
    end;

    for LI := 0 to LCallFixups.Count - 1 do
    begin
      LK := LCallFixups[LI].Value;
      if LFuncOffsets.ContainsKey(LK) then
      begin
        if (LCallFixups[LI].Key + 4 <= Cardinal(Length(LTextBytes))) then
        begin
          LInstrIdx := Integer(Int64(LFuncOffsets[LK]) - Int64(LCallFixups[LI].Key));
          LInstrIdx := LInstrIdx shr 2;
          if (LInstrIdx >= -33554432) and (LInstrIdx <= 33554431) then
          begin
            LK := Cardinal(LInstrIdx) and $3FFFFFF;
            PCardinal(@LTextBytes[LCallFixups[LI].Key])^ := $94000000 or LK;
          end;
        end;
      end;
    end;

    for LI := 0 to LJumpFixups.Count - 1 do
    begin
      if LLabelOffsets.TryGetValue(LJumpFixups[LI].Value, LLabelOffset) then
      begin
        if (LJumpFixups[LI].Key + 4 <= Cardinal(Length(LTextBytes))) then
        begin
          LInstrIdx := Integer(Int64(LLabelOffset) - Int64(LJumpFixups[LI].Key));
          LInstrIdx := LInstrIdx shr 2;
          if (LInstrIdx >= -33554432) and (LInstrIdx <= 33554431) then
            PCardinal(@LTextBytes[LJumpFixups[LI].Key])^ := $14000000 or (Cardinal(LInstrIdx) and $3FFFFFF);
        end;
      end;
    end;

    for LI := 0 to LCondJumpFixups.Count - 1 do
    begin
      if LLabelOffsets.TryGetValue(LCondJumpFixups[LI].Value, LLabelOffset) then
      begin
        if (LCondJumpFixups[LI].Key + 4 <= Cardinal(Length(LTextBytes))) then
        begin
          LInstrIdx := Integer(Int64(LLabelOffset) - Int64(LCondJumpFixups[LI].Key));
          LInstrIdx := LInstrIdx shr 2;
          if (LInstrIdx >= -262144) and (LInstrIdx <= 262143) then
            PCardinal(@LTextBytes[LCondJumpFixups[LI].Key])^ := (PCardinal(@LTextBytes[LCondJumpFixups[LI].Key])^ and $FF00001F) or ((Cardinal(LInstrIdx) and $7FFFF) shl 5);
        end;
      end;
    end;

    for LI := 0 to LImportFixups.Count - 1 do
    begin
      LSlotAddr := SEG_DATA_VADDR + Cardinal(Length(LDataBytes)) + Cardinal(LImportFixups[LI].Value) * 8;
      LByteOffset := Int64(LSlotAddr) - Int64(LImportFixups[LI].Key);
      LPageIndex := LByteOffset div 4096;
      LOfs12 := Cardinal((LByteOffset mod 4096 + 4096) mod 4096) div 8;
      if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) and
         (LImportFixups[LI].Key + 8 <= Cardinal(Length(LTextBytes))) then
      begin
        LAdrpImm := Cardinal(Int32(LPageIndex) and $1FFFFF);
        PCardinal(@LTextBytes[LImportFixups[LI].Key])^ := $90000000 or REG_X16 or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
        PCardinal(@LTextBytes[LImportFixups[LI].Key + 4])^ := $F9400000 or (LOfs12 shl 10) or (REG_X16 shl 5) or REG_X16;
      end;
    end;

    LEntryOffset := 0;
    for LFuncIdx := 0 to LFuncCount - 1 do
    begin
      LFunc := LCode.GetFunc(LFuncIdx);
      if LFunc.IsEntryPoint then
      begin
        LEntryOffset := LFuncOffsets[LFuncIdx];
        Break;
      end;
    end;

    SetLength(LGotBytes, LGotStream.Size);
    if LGotStream.Size > 0 then
    begin
      LGotStream.Position := 0;
      LGotStream.ReadBuffer(LGotBytes[0], LGotStream.Size);
    end;

    LBindStream := TMemoryStream.Create();
    try
      for LI := 0 to LImports.GetCount() - 1 do
      begin
        if LHasStaticImports and (LStaticImportIndices.IndexOf(LI) >= 0) then
          Continue;
        LEntry := LImports.GetEntryByIndex(LI);
        LByteVal := $71;
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := $11;
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := $30;
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := 95;
        LBindStream.WriteBuffer(LByteVal, 1);
        for LK := 1 to Length(LEntry.FuncName) do
        begin
          LByteVal := Ord(LEntry.FuncName[LK]);
          LBindStream.WriteBuffer(LByteVal, 1);
        end;
        LByteVal := 0;
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := $41;
        LBindStream.WriteBuffer(LByteVal, 1);
        LVal := Length(LDataBytes) + Cardinal(LI) * 8;
        repeat
          LJ := LVal and $7F;
          LVal := LVal shr 7;
          if LVal <> 0 then
            LJ := LJ or $80;
          LByteVal := Byte(LJ);
          LBindStream.WriteBuffer(LByteVal, 1);
        until LVal = 0;
        LByteVal := $90;
        LBindStream.WriteBuffer(LByteVal, 1);
      end;
      LByteVal := $00;
      LBindStream.WriteBuffer(LByteVal, 1);
      SetLength(LBindBytes, LBindStream.Size);
      if LBindStream.Size > 0 then
      begin
        LBindStream.Position := 0;
        LBindStream.ReadBuffer(LBindBytes[0], LBindStream.Size);
      end;
    finally
      LBindStream.Free();
    end;

    LOutStream := TMemoryStream.Create();
    try
      LHeaderSize := 32;
      LSegTextFileSize := (Length(LTextBytes) + Length(LCStringBytes) + 15) and (not 15);
      LSegDataFileSize := (Length(LDataBytes) + Length(LGotBytes) + 15) and (not 15);
      LLoadCmdSize := 72 + 160 + 72 + 160 + 24 + 40 + 56 + 48;
      // __TEXT must start at a page boundary for the kernel to accept the executable
      LSegTextFileOff := PAGE_SIZE;
      LSegDataFileOff := LSegTextFileOff + LSegTextFileSize;
      LBindOff := LSegDataFileOff + LSegDataFileSize;

      LCardVal := MH_MAGIC_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := CPU_TYPE_ARM64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := CPU_SUBTYPE_ARM64_ALL;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := MH_EXECUTE;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 6;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LLoadCmdSize, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);

      LCardVal := LC_SEGMENT_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 72 + 160;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__TEXT', 16);
      LVal := SEG_TEXT_VADDR;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegTextFileSize;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegTextFileOff;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegTextFileSize;
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := 7;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 5;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 2;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__text', 16);
      WriteFixedAnsi('__TEXT', 16);
      LVal := 0;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LTextBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LSegTextFileOff;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 4;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__cstring', 16);
      WriteFixedAnsi('__TEXT', 16);
      LVal := Length(LTextBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LCStringBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LSegTextFileOff + Length(LTextBytes);
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 4;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);

      LCardVal := LC_SEGMENT_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 72 + 160;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__DATA', 16);
      LVal := SEG_DATA_VADDR;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegDataFileSize;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegDataFileOff;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegDataFileSize;
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := 3;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 3;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 2;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__data', 16);
      WriteFixedAnsi('__DATA', 16);
      LVal := 0;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LDataBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LSegDataFileOff;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__got', 16);
      WriteFixedAnsi('__DATA', 16);
      LVal := Length(LDataBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LGotBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LSegDataFileOff + Length(LDataBytes);
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);

      LCardVal := LC_MAIN;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 24;
      LOutStream.WriteBuffer(LCardVal, 4);
      LVal := LEntryOffset;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := 0;
      LOutStream.WriteBuffer(LVal, 8);

      LCardVal := LC_LOAD_DYLINKER;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 40;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 12;
      LOutStream.WriteBuffer(LCardVal, 4);
      WritePaddedCString('/usr/lib/dyld', 28);

      LCardVal := LC_LOAD_DYLIB;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 56;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 24;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      WritePaddedCString('/usr/lib/libSystem.B.dylib', 32);

      LCardVal := LC_DYLD_INFO_ONLY;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 48;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LBindOff, 4);
      LCardVal := Length(LBindBytes);
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);

      LByteVal := 0;
      while LOutStream.Position < LSegTextFileOff do
        LOutStream.WriteBuffer(LByteVal, 1);

      if Length(LTextBytes) > 0 then
        LOutStream.WriteBuffer(LTextBytes[0], Length(LTextBytes));
      if Length(LCStringBytes) > 0 then
        LOutStream.WriteBuffer(LCStringBytes[0], Length(LCStringBytes));
      LK := LSegTextFileSize - Length(LTextBytes) - Length(LCStringBytes);
      LByteVal := 0;
      for LI := 0 to LK - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);
      if Length(LDataBytes) > 0 then
        LOutStream.WriteBuffer(LDataBytes[0], Length(LDataBytes));
      if Length(LGotBytes) > 0 then
        LOutStream.WriteBuffer(LGotBytes[0], Length(LGotBytes));
      LK := LSegDataFileSize - Length(LDataBytes) - Length(LGotBytes);
      for LI := 0 to LK - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);
      if Length(LBindBytes) > 0 then
        LOutStream.WriteBuffer(LBindBytes[0], Length(LBindBytes));

      SetLength(Result, LOutStream.Size);
      LOutStream.Position := 0;
      LOutStream.ReadBuffer(Result[0], LOutStream.Size);
    finally
      LOutStream.Free();
    end;
  finally
    if LLinker <> nil then
      LLinker.Free();
    LStaticCallFixups.Free();
    LStaticSymbolNames.Free();
    LStaticLibPaths.Free();
    LStaticImportIndices.Free();
    LCondJumpFixups.Free();
    LLabelOffsets.Free();
    LJumpFixups.Free();
    LDataFixups.Free();
    LImportFixups.Free();
    LCallFixups.Free();
    LFuncOffsets.Free();
    LGotStream.Free();
    LDataStream.Free();
    LCStringStream.Free();
    LTextStream.Free();
  end;
end;

end.
