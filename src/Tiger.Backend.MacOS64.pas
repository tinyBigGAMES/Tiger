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
  System.Hash,
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
    function GenerateMachO(const AIsDylib: Boolean): TBytes;
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
  MH_DYLIB       = 6;
  MH_NOUNDEFS    = $00000001;
  MH_DYLDLINK    = $00000004;
  MH_TWOLEVEL    = $00000080;
  MH_PIE         = $00200000;
  LC_SEGMENT_64  = $19;
  LC_REQ_DYLD    = $80000000;
  LC_MAIN        = $28 or LC_REQ_DYLD;
  LC_LOAD_DYLINKER = $0E;
  LC_LOAD_DYLIB   = $0C;
  LC_ID_DYLIB     = $0D;
  LC_DYLD_INFO_ONLY = $22 or LC_REQ_DYLD;
  LC_VERSION_MIN_MACOSX = $24;
  LC_CODE_SIGNATURE = $1D;
  LC_UUID         = $1B;
  LC_SYMTAB       = $02;
  LC_DYSYMTAB     = $0B;
  // Code signature blob (linker-signed ad-hoc)
  CSMAGIC_EMBEDDED_SIGNATURE = $FADE0CC0;
  CSMAGIC_CODEDIRECTORY = $FADE0C02;
  CSSLOT_CODEDIRECTORY = 0;
  CS_CD_FLAG_ADHOC = $2;
  CS_CD_FLAG_LINKER_SIGNED = $20000;
  CS_HASHTYPE_SHA256 = 2;
  CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE = 88;  // version 0x20400 layout (incl. scatter/team/codeLimit64/execSeg)
  CODE_SIGNATURE_IDENTIFIER_SIZE = 1;   // minimal identifier (null byte) required by codesign
  CS_EXECSEG_MAIN_BINARY = $1;
  CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE = 20;
  CODE_SIGNATURE_ALIGN = 16;  // dataoff must be 16-byte aligned for codesign to accept

  SEG_TEXT = '__TEXT';
  SEG_DATA = '__DATA';
  SEG_LINKEDIT = '__LINKEDIT';
  SEG_PAGEZERO = '__PAGEZERO';
  SECT_TEXT = '__text';
  SECT_CSTRING = '__cstring';
  SECT_DATA = '__data';
  SECT_GOT = '__got';

  PAGE_SIZE = 4096;
  SEGMENT_PAGE_SIZE = 16384;  // ARM64 macOS uses 16KB pages; segment fileoff/vmsize must be 16K aligned
  SEG_TEXT_VADDR = $0000000100000000;  // Canonical arm64 executable __TEXT base
  SEG_DATA_VADDR = $0000000100001000;  // Keep same vmaddr-fileoff slide across segments

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
      Result := GenerateMachO(False);
    otDll:
      Result := GenerateMachO(True);
    otObj, otLib:
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

function TTigerMacOS64Backend.GenerateMachO(const AIsDylib: Boolean): TBytes;
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
  LGlobalFixups: TList<TPair<Cardinal, Integer>>;
  LDataPageFixups: TList<Cardinal>;  // One ADRP per function that uses globals; targets __data base
  LLabelOffsets: TDictionary<Integer, Cardinal>;
  LJumpFixups: TList<TPair<Cardinal, Integer>>;
  LCondJumpFixups: TList<TPair<Cardinal, Integer>>;
  LOutStream: TMemoryStream;
  LHeaderSize, LLoadCmdSize: Cardinal;
  LSegTextFileOff, LSegTextFileSize, LSegDataFileOff, LSegDataFileSize, LSegDataSegmentFileSize: Cardinal;
  LSegLinkEditFileOff, LSegLinkEditFileSize: Cardinal;
  LTextSectionFileOff: Cardinal;
  LTextMapSize: Cardinal;
  LSlide: UInt64;
  LTextFileOff, LCStringFileOff, LDataFileOff, LGotFileOff: Cardinal;
  LGotVAddr, LSlotAddr: UInt64;
  LOfs12: Cardinal;
  LFrameSize, LLocalsSize, LMaxCallArgs, LOutgoingArgSpace: Cardinal;
  LIncomingSpillSize: Cardinal;
  LStackFrameSize, LVariadicSize: Cardinal;
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
  LRebaseBytes: TBytes;
  LExportBytes: TBytes;
  LBindOff: Cardinal;
  LExportOff: Cardinal;
  LRebaseOff: Cardinal;
  LCodeLimit: Cardinal;
  LSignatureOffset: Cardinal;
  LSignatureSize: Cardinal;
  LNumPages: Cardinal;
  LPadding: Cardinal;
  LSignatureBytes: TBytes;
  LVal: UInt64;
  LLabelOffset: Cardinal;
  LAdrpImm: Cardinal;
  LByteVal: Byte;
  LCardVal: Cardinal;
  LByteOffset: Int64;
  LPageIndex: Int64;
  LTargetIndex: Integer;
  LTargetReg: Byte;
  LDataHandle: TTigerDataHandle;
  LUUID: TGUID;
  LIdName: AnsiString;
  LCmdCount: Cardinal;
  LDylibNames: TStringList;
  LDylibOrdinals: TDictionary<string, Integer>;
  LDylibName: string;
  LDylibOrdinal: Integer;
  LDylibCmdSizeTotal: Cardinal;
  LDylibCmdSize: Cardinal;
  LNamePaddedSize: Cardinal;

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

  function AlignUp8(const AValue: Cardinal): Cardinal;
  begin
    Result := (AValue + 7) and (not 7);
  end;

  function NormalizeDylibName(const AName: string): string;
  var
    L: string;
  begin
    L := AName;
    if L = '' then
      Exit('/usr/lib/libSystem.B.dylib');

    // Many existing tests use Linux-style libc names on non-Windows platforms.
    // On macOS, map those to libSystem so existing code keeps working.
    if L.Contains('libc.so') or L.Contains('libm.so') then
      Exit('/usr/lib/libSystem.B.dylib');

    Result := L;
  end;

  function AlignUp32(const AValue, AAlignment: Cardinal): Cardinal;
  begin
    if AAlignment <= 1 then
      Exit(AValue);
    Result := (AValue + AAlignment - 1) and (not (AAlignment - 1));
  end;

  procedure PutU32BE(var ABuf: TBytes; AOffset: Cardinal; AValue: Cardinal);
  begin
    ABuf[AOffset] := Byte((AValue shr 24) and $FF);
    ABuf[AOffset + 1] := Byte((AValue shr 16) and $FF);
    ABuf[AOffset + 2] := Byte((AValue shr 8) and $FF);
    ABuf[AOffset + 3] := Byte(AValue and $FF);
  end;

  procedure PutU64BE(var ABuf: TBytes; AOffset: Cardinal; AValue: UInt64);
  begin
    PutU32BE(ABuf, AOffset, Cardinal(AValue shr 32));
    PutU32BE(ABuf, AOffset + 4, Cardinal(AValue and $FFFFFFFF));
  end;

  procedure BuildLinkerSignedSignature(const AData: TBytes; ACodeLimit, ANumPages, ASignatureSize: Cardinal; AExecSegBase, AExecSegLimit: UInt64; AExecSegFlags: Cardinal; var AOutSignature: TBytes);
  var
    LPageStream: TMemoryStream;
    LHashBytes: TBytes;
    LCodeDirSize: Cardinal;
    LHashOffset: Cardinal;
    LOff: Cardinal;
    LPageStart: Cardinal;
    LPageLen: Cardinal;
    LIdx: Integer;
  begin
    SetLength(AOutSignature, ASignatureSize);
    FillChar(AOutSignature[0], ASignatureSize, 0);
    LHashOffset := CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE + CODE_SIGNATURE_IDENTIFIER_SIZE;
    LCodeDirSize := LHashOffset + ANumPages * 32;
    LOff := 0;
    PutU32BE(AOutSignature, LOff, CSMAGIC_EMBEDDED_SIGNATURE);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE + LCodeDirSize);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, 1);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, CSSLOT_CODEDIRECTORY);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, CSMAGIC_CODEDIRECTORY);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, LCodeDirSize);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, $20400);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, CS_CD_FLAG_ADHOC or CS_CD_FLAG_LINKER_SIGNED);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, LHashOffset);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, 0);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, ANumPages);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, ACodeLimit);
    Inc(LOff, 4);
    AOutSignature[LOff] := 32;
    Inc(LOff);
    AOutSignature[LOff] := CS_HASHTYPE_SHA256;
    Inc(LOff);
    AOutSignature[LOff] := 0;
    Inc(LOff);
    AOutSignature[LOff] := 12;
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, 0);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, 0);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, 0);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, 0);
    Inc(LOff, 4);
    PutU64BE(AOutSignature, LOff, UInt64(ACodeLimit));
    Inc(LOff, 8);
    PutU64BE(AOutSignature, LOff, AExecSegBase);
    Inc(LOff, 8);
    PutU64BE(AOutSignature, LOff, AExecSegLimit);
    Inc(LOff, 8);
    PutU64BE(AOutSignature, LOff, UInt64(AExecSegFlags));
    AOutSignature[CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE + CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE] := 0;
    LPageStream := TMemoryStream.Create();
    try
      for LIdx := 0 to ANumPages - 1 do
      begin
        LPageStart := LIdx * PAGE_SIZE;
        if LPageStart >= ACodeLimit then
          Break;
        LPageLen := PAGE_SIZE;
        if LPageStart + LPageLen > ACodeLimit then
          LPageLen := ACodeLimit - LPageStart;
        LPageStream.Clear();
        LPageStream.WriteBuffer(AData[LPageStart], LPageLen);
        LPageStream.Position := 0;
        LHashBytes := THashSHA2.GetHashBytes(LPageStream);
        if Length(LHashBytes) >= 32 then
          Move(LHashBytes[0], AOutSignature[CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE + LHashOffset + Cardinal(LIdx) * 32], 32);
      end;
    finally
      LPageStream.Free();
    end;
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

  // We spill incoming integer/pointer args (x0-x7) to [FP-8], [FP-16], ...
  // Locals/temps must live *below* the spill area to avoid aliasing.
  //
  // IMPORTANT: Even if a function has fewer than 8 params, we still reserve a
  // minimum spill area so that temps and locals can never overlap the top slots
  // (e.g. [FP-8]) used to preserve x0..x7 across calls.
  const
    MIN_PARAM_SPILL_SIZE = 64; // 8 regs * 8 bytes

  function IncomingParamSpillSize(): Int32;
  begin
    // Only spill what we actually have (up to 8 regs), but treat it as a contiguous
    // area under FP so locals/temps never overlap it.
    Result := Int32(Min(Length(LFunc.Params), 8) * 8);
  end;

  function SpillBaseSize(): Int32;
  begin
    Result := IncomingParamSpillSize();
    if Result < MIN_PARAM_SPILL_SIZE then
      Result := MIN_PARAM_SPILL_SIZE;
  end;

  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    // Param 0 at [FP-8], param 1 at [FP-16], ...
    Result := -Int32((AIndex + 1) * 8);
  end;

  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LOffset, LK: Integer;
  begin
    // Locals live below the param spill area.
    LOffset := SpillBaseSize();
    for LK := 0 to AIndex do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -Int32(LOffset);
  end;

  function GetTempOffset(const ATempIndex: Integer): Int32;
  var
    LOffset, LK: Integer;
  begin
    // Temps live below the locals area.
    LOffset := SpillBaseSize();
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    // Temp 0 at [FP-(incoming_spill + locals + 8)], etc.
    LOffset := LOffset + (ATempIndex + 1) * 8;
    Result := -Int32(LOffset);
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
    LImm7 := Cardinal((AImm div 8) and $7F);
    EmitARM64($A9800000 or (LImm7 shl 15) or (Cardinal(ARn) shl 5) or ARt1 or (Cardinal(ARt2) shl 10));
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
    // LDUR Xt, [Xn, #imm9] (unscaled, signed imm9). Base opcode must have imm9=0.
    EmitARM64($F8400000 or (LImm9 shl 12) or (REG_FP shl 5) or ARt);
  end;

  procedure EmitSturFp(const ADisp: Int32; const ARt: Byte);
  var
    LImm9: Cardinal;
  begin
    LImm9 := Cardinal(Int32(ADisp) and $1FF);
    // STUR Xt, [Xn, #imm9] (unscaled, signed imm9). Base opcode must have imm9=0.
    EmitARM64($F8000000 or (LImm9 shl 12) or (REG_FP shl 5) or ARt);
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
        // ADD X16, FP, X16
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        // LDR ARt, [X16, #0]
        EmitARM64($F9400000 or (REG_X16 shl 5) or ARt);
      end;
    end
    else if ADisp >= -256 then
      EmitLdurFp(ARt, ADisp)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      // SUB X16, FP, X16
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      // LDR ARt, [X16, #0]
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
        // ADD X16, FP, X16
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        // STR ARt, [X16, #0]
        EmitARM64($F9000000 or (REG_X16 shl 5) or ARt);
      end;
    end
    else if ADisp >= -256 then
      EmitSturFp(ADisp, ARt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      // SUB X16, FP, X16
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      // STR ARt, [X16, #0]
      EmitARM64($F9000000 or (REG_X16 shl 5) or ARt);
    end;
  end;

  procedure EmitBL(const AOffset: Int32);
  var
    LImm26: Cardinal;
  begin
    // BL encodes a signed 28-bit byte offset (imm26 << 2). Use signed math so
    // backward calls encode correctly (logical shifts break negative offsets).
    LImm26 := Cardinal(AOffset div 4) and $3FFFFFF;
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
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), (AOp.DataHandle.Index shl 8) or AReg));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
      okGlobal:
        begin
          // Emit ADRP to __data base (X16) before each load; all target same page, fixed up by LDataPageFixups.
          // Using one ADRP per load avoids preserving X16 across intervening instructions (ikAdd etc. clobber it).
          LDataPageFixups.Add(Cardinal(LTextStream.Position));
          EmitAdrp(REG_X16, 0);
          LDataHandle.Index := AOp.DataHandle.Index;
          LByteOffset := LGlobals.GetEntry(LDataHandle).Offset;
          if LByteOffset <= 4095 then
            EmitARM64($91000000 or (Cardinal(LByteOffset) shl 10) or (REG_X16 shl 5) or AReg)
          else
          begin
            EmitMovRegImm64(REG_X17, LByteOffset);
            EmitARM64($8B000000 or (REG_X17 shl 16) or (REG_X16 shl 5) or AReg);
          end;
        end;
    else
      EmitMovRegImm64(AReg, 0);
    end;
  end;

  function UlebLen(const AValue: UInt64): Integer;
  var
    V: UInt64;
  begin
    V := AValue;
    Result := 1;
    while V >= $80 do
    begin
      Inc(Result);
      V := V shr 7;
    end;
  end;

  procedure WriteUleb(const AStream: TMemoryStream; const AValue: UInt64);
  var
    V: UInt64;
    B: Byte;
  begin
    V := AValue;
    repeat
      B := Byte(V and $7F);
      V := V shr 7;
      if V <> 0 then
        B := B or $80;
      AStream.WriteBuffer(B, 1);
    until V = 0;
  end;

  function MakeTerminalNode(const AAddr: UInt64): TBytes;
  var
    S: TMemoryStream;
    LTermData: TBytes;
  begin
    S := TMemoryStream.Create();
    try
      // terminalData = flags(uleb=0) + address(uleb)
      WriteUleb(S, 0);
      WriteUleb(S, AAddr);
      SetLength(LTermData, S.Size);
      if S.Size > 0 then
      begin
        S.Position := 0;
        S.ReadBuffer(LTermData[0], S.Size);
      end;
    finally
      S.Free();
    end;

    S := TMemoryStream.Create();
    try
      // node: terminalSize + terminalData + childCount(0)
      WriteUleb(S, UInt64(Length(LTermData)));
      if Length(LTermData) > 0 then
        S.WriteBuffer(LTermData[0], Length(LTermData));
      var LZero: Byte := 0;
      S.WriteBuffer(LZero, 1);
      SetLength(Result, S.Size);
      if S.Size > 0 then
      begin
        S.Position := 0;
        S.ReadBuffer(Result[0], S.Size);
      end;
    finally
      S.Free();
    end;
  end;

  procedure StoreTempFromReg(const ATempIndex: Integer; const AReg: Byte);
  begin
    if GetTempOffset(ATempIndex) >= -255 then
      EmitStrFp(GetTempOffset(ATempIndex), AReg)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-GetTempOffset(ATempIndex)));
      // SUB X16, FP, X16
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
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
  LGlobalFixups := TList<TPair<Cardinal, Integer>>.Create();
  LDataPageFixups := TList<Cardinal>.Create();
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
  LDylibNames := TStringList.Create();
  LDylibNames.CaseSensitive := False;
  LDylibNames.Sorted := False;
  LDylibNames.Duplicates := dupIgnore;
  LDylibOrdinals := TDictionary<string, Integer>.Create();
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

    // Collect dynamic library dependencies (for LC_LOAD_DYLIB and bind ordinals).
    // Always include libSystem on macOS.
    LDylibNames.Add('/usr/lib/libSystem.B.dylib');
    for LI := 0 to LImports.GetCount() - 1 do
    begin
      if LHasStaticImports and (LStaticImportIndices.IndexOf(LI) >= 0) then
        Continue;
      LEntry := LImports.GetEntryByIndex(LI);
      LDylibName := NormalizeDylibName(LEntry.DllName);
      if LDylibNames.IndexOf(LDylibName) < 0 then
        LDylibNames.Add(LDylibName);
    end;
    for LI := 0 to LDylibNames.Count - 1 do
      LDylibOrdinals.AddOrSetValue(LDylibNames[LI], LI + 1); // ordinals are 1-based

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

      // Reserve a fixed spill area under FP for x0-x7, even if unused.
      // This must match the offset scheme in GetParamOffset/GetLocalOffset/GetTempOffset
      // or locals/temps will alias outgoing arg space and/or clobber saved FP/LR.
      LIncomingSpillSize := Cardinal(SpillBaseSize());

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
      // Fixed param spill area (x0-x7) + locals + temps + outgoing arg space.
      LStackFrameSize := LIncomingSpillSize + LLocalsSize + Cardinal(LFunc.TempCount) * 8 + LOutgoingArgSpace;
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

      EmitStpPre(REG_FP, REG_LR, REG_SP, -16);
      EmitARM64($910003E0 or (REG_SP shl 5) or REG_FP);
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
              LEntry := LImports.GetEntryByIndex(LInstr.ImportTarget.Index);
              if LEntry.IsVariadic and (Length(LInstr.Args) > 0) then
              begin
                // macOS ARM64 (Darwin) variadic calling convention (as emitted by clang):
                // - Fixed args are passed in registers as usual.
                // - Variadic args are passed on the stack starting at [SP].
                //
                // For printf(const char* fmt, ...):
                // - fmt goes in x0
                // - all variadic args go at [SP + 0], [SP + 8], ...
                //
                // IMPORTANT: Do NOT adjust SP here. Our stack frame already reserves
                // outgoing arg space (LOutgoingArgSpace) at the bottom of the frame.
                LoadOperandToReg(LInstr.Args[0], REG_X0);
                for LK := 1 to Length(LInstr.Args) - 1 do
                begin
                  LoadOperandToReg(LInstr.Args[LK], REG_X16);
                  EmitStrX(REG_X16, REG_SP, Cardinal((LK - 1) * 8));
                end;
              end
              else
              begin
                // Non-variadic (or no-arg) call: pass first 8 args in x0-x7.
                for LK := 0 to Min(Length(LInstr.Args) - 1, 7) do
                  LoadOperandToReg(LInstr.Args[LK], LK);
                // Overflow args for non-variadic are not currently supported on macOS backend.
              end;
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
              // AND X0, X0, X16
              EmitARM64($8A000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitOr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              // ORR X0, X0, X16
              EmitARM64($AA000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitXor:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              // EOR X0, X0, X16
              EmitARM64($CA000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
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
              // Produce a canonical boolean 0/1 in X0:
              // CSET X0, EQ  ==  CSINC X0, XZR, XZR, NE  (invert condition)
              EmitARM64($9A9F07E0 or ((0 xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpNe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              // CSET X0, NE  ==  CSINC X0, XZR, XZR, EQ
              EmitARM64($9A9F07E0 or ((1 xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpLt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              // CSET X0, LT  ==  CSINC X0, XZR, XZR, GE
              EmitARM64($9A9F07E0 or (($0B xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpLe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              // CSET X0, LE  ==  CSINC X0, XZR, XZR, GT
              EmitARM64($9A9F07E0 or (($0D xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpGt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              // CSET X0, GT  ==  CSINC X0, XZR, XZR, LE
              EmitARM64($9A9F07E0 or (($0C xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpGe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              // CSET X0, GE  ==  CSINC X0, XZR, XZR, LT
              EmitARM64($9A9F07E0 or (($0A xor 1) shl 12) or REG_X0);
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
                EmitSubImm(REG_X0, REG_FP, Cardinal(-GetParamOffset(LInstr.Op1.LocalHandle.Index)))
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
          LInstrIdx := LInstrIdx div 4;  // Signed division; shr 2 is wrong for negative (logical shift)
          if (LInstrIdx >= -33554432) and (LInstrIdx <= 33554431) then
            PCardinal(@LTextBytes[LCallFixups[LI].Key])^ := $94000000 or (Cardinal(LInstrIdx) and $3FFFFFF);
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
          LInstrIdx := LInstrIdx div 4;  // Signed division for backward branches
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
          LInstrIdx := LInstrIdx div 4;  // Signed division for backward branches
          if (LInstrIdx >= -262144) and (LInstrIdx <= 262143) then
            PCardinal(@LTextBytes[LCondJumpFixups[LI].Key])^ := (PCardinal(@LTextBytes[LCondJumpFixups[LI].Key])^ and $FF00001F) or ((Cardinal(LInstrIdx) and $7FFFF) shl 5);
        end;
      end;
    end;

    // -------------------------------------------------------------------------
    // Compute canonical Mach-O layout needed for ADRP-based fixups.
    // Must use the same alignment (SEGMENT_PAGE_SIZE) as the emitted binary
    // or GOT/data addresses patched into code will be wrong and the program
    // will jump to 0 or crash.
    // -------------------------------------------------------------------------
    LSegTextFileOff := 0;
    LTextSectionFileOff := PAGE_SIZE;
    LSegTextFileSize := (Length(LTextBytes) + Length(LCStringBytes) + 15) and (not 15);
    LTextMapSize := AlignUp32(LTextSectionFileOff + LSegTextFileSize, SEGMENT_PAGE_SIZE);
    LSegDataFileOff := LTextMapSize;
    LSlide := SEG_TEXT_VADDR; // since __TEXT.fileoff = 0

    for LI := 0 to LImportFixups.Count - 1 do
    begin
      // GOT lives in __DATA right after globals.
      LSlotAddr := LSlide + UInt64(LSegDataFileOff) + UInt64(Length(LDataBytes)) + UInt64(LImportFixups[LI].Value) * 8;
      LPageIndex := (Int64(LSlotAddr) shr 12) - (Int64(LSlide + UInt64(LTextSectionFileOff) + UInt64(LImportFixups[LI].Key)) shr 12);
      LOfs12 := (LSlotAddr and $FFF) div 8;
      if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) and
         (LImportFixups[LI].Key + 8 <= Cardinal(Length(LTextBytes))) then
      begin
        LAdrpImm := Cardinal(Int32(LPageIndex) and $1FFFFF);
        PCardinal(@LTextBytes[LImportFixups[LI].Key])^ := $90000000 or REG_X16 or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
        PCardinal(@LTextBytes[LImportFixups[LI].Key + 4])^ := $F9400000 or (LOfs12 shl 10) or (REG_X16 shl 5) or REG_X16;
      end;
    end;

    for LI := 0 to LDataFixups.Count - 1 do
    begin
      LTargetIndex := LDataFixups[LI].Value shr 8;
      LTargetReg := Byte(LDataFixups[LI].Value and $FF);
      if (LTargetIndex >= 0) then
      begin
        LDataHandle.Index := LTargetIndex;
        // C-strings are placed after .text in the __TEXT mapping.
        LSlotAddr := LSlide + UInt64(LTextSectionFileOff) + UInt64(Length(LTextBytes)) + UInt64(LData.GetEntry(LDataHandle).Offset);
        LPageIndex := (Int64(LSlotAddr) shr 12) - (Int64(LSlide + UInt64(LTextSectionFileOff) + UInt64(LDataFixups[LI].Key)) shr 12);
        LOfs12 := LSlotAddr and $FFF;
        if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) and
           (LDataFixups[LI].Key + 8 <= Cardinal(Length(LTextBytes))) then
        begin
          LAdrpImm := Cardinal(Int32(LPageIndex) and $1FFFFF);
          PCardinal(@LTextBytes[LDataFixups[LI].Key])^ := $90000000 or LTargetReg or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
          PCardinal(@LTextBytes[LDataFixups[LI].Key + 4])^ := $91000000 or (LOfs12 shl 10) or (Cardinal(LTargetReg) shl 5) or LTargetReg;
        end;
      end;
    end;

    for LI := 0 to LGlobalFixups.Count - 1 do
    begin
      LTargetIndex := LGlobalFixups[LI].Value shr 8;
      LTargetReg := Byte(LGlobalFixups[LI].Value and $FF);
      if (LTargetIndex >= 0) then
      begin
        LDataHandle.Index := LTargetIndex;
        LSlotAddr := LSlide + UInt64(LSegDataFileOff) + UInt64(LGlobals.GetEntry(LDataHandle).Offset);
        LPageIndex := (Int64(LSlotAddr) shr 12) - (Int64(LSlide + UInt64(LTextSectionFileOff) + UInt64(LGlobalFixups[LI].Key)) shr 12);
        LOfs12 := LSlotAddr and $FFF;
        if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) and
           (LGlobalFixups[LI].Key + 8 <= Cardinal(Length(LTextBytes))) then
        begin
          LAdrpImm := Cardinal(Int32(LPageIndex) and $1FFFFF);
          PCardinal(@LTextBytes[LGlobalFixups[LI].Key])^ := $90000000 or LTargetReg or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
          PCardinal(@LTextBytes[LGlobalFixups[LI].Key + 4])^ := $91000000 or (LOfs12 shl 10) or (Cardinal(LTargetReg) shl 5) or LTargetReg;
        end;
      end;
    end;

    for LI := 0 to LDataPageFixups.Count - 1 do
    begin
      LSlotAddr := LSlide + UInt64(LSegDataFileOff);
      LPageIndex := (Int64(LSlotAddr) shr 12) - (Int64(LSlide + UInt64(LTextSectionFileOff) + UInt64(LDataPageFixups[LI])) shr 12);
      if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) and
         (LDataPageFixups[LI] + 4 <= Cardinal(Length(LTextBytes))) then
      begin
        LAdrpImm := Cardinal(Int32(LPageIndex) and $1FFFFF);
        PCardinal(@LTextBytes[LDataPageFixups[LI]])^ := $90000000 or REG_X16 or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
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
        // Bind this symbol against the correct dylib ordinal.
        LDylibName := NormalizeDylibName(LEntry.DllName);
        if not LDylibOrdinals.TryGetValue(LDylibName, LDylibOrdinal) then
          LDylibOrdinal := 1;
        if LDylibOrdinal < 0 then
          LDylibOrdinal := 1;
        if LDylibOrdinal > 15 then
          LDylibOrdinal := 1; // TODO: support SET_DYLIB_ORDINAL_ULEB for larger ordinals
        LByteVal := Byte($10 or (LDylibOrdinal and $F));
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := $40;
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
        LByteVal := $51;
        LBindStream.WriteBuffer(LByteVal, 1);
        // SET_SEGMENT_AND_OFFSET_ULEB: imm = segment index.
        // With __PAGEZERO + __TEXT preceding, __DATA is segment index 2.
        LByteVal := $72;
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

    // Build a minimal export trie for dylib outputs so executables can bind to
    // our exported functions (AddC, mangled C++ exports, etc.).
    LExportBytes := nil;
    if AIsDylib then
    begin
      var LExportSyms: TStringList;
      var LExportAddrs: TDictionary<string, UInt64>;
      var LExportStream: TMemoryStream;

      LExportSyms := TStringList.Create();
      LExportAddrs := TDictionary<string, UInt64>.Create();
      LExportStream := TMemoryStream.Create();
      try
        LExportSyms.CaseSensitive := True;
        LExportSyms.Sorted := True;
        LExportSyms.Duplicates := dupIgnore;

        for LI := 0 to LFuncCount - 1 do
        begin
          LFunc := LCode.GetFunc(LI);
          if LFunc.IsPublic then
          begin
            // Mach-O symbols are underscore-prefixed (e.g. _AddC, __Z6AddCppii)
            var LSym := '_' + LFunc.ExportName;
            // Address in export trie is vm-offset within the image.
            // We always place __text at PAGE_SIZE.
            var LAddr := UInt64(PAGE_SIZE) + UInt64(LFuncOffsets[LI]);
            if LExportSyms.IndexOf(LSym) < 0 then
              LExportSyms.Add(LSym);
            LExportAddrs.AddOrSetValue(LSym, LAddr);
          end;
        end;

        if LExportSyms.Count > 0 then
        begin
          // Root node: no terminal, N children. Each child label is the full symbol name.
          // Child nodes are simple terminals with no children.
          var LChildNodes: array of TBytes;
          SetLength(LChildNodes, LExportSyms.Count);
          for LI := 0 to LExportSyms.Count - 1 do
          begin
            if not LExportAddrs.TryGetValue(LExportSyms[LI], LVal) then
              LVal := 0;
            LChildNodes[LI] := MakeTerminalNode(LVal);
          end;

          // Compute root size and child offsets (iterative to account for uleb sizes).
          var LOffsets: array of Cardinal;
          SetLength(LOffsets, LExportSyms.Count);

          var LRootSize: Cardinal := 0;
          var LPrevRootSize: Cardinal := 0;
          repeat
            LPrevRootSize := LRootSize;
            // terminalSize(0) uleb + childCount byte
            LRootSize := Cardinal(UlebLen(0)) + 1;
            // add each child entry: label cstring + uleb(offset)
            for LI := 0 to LExportSyms.Count - 1 do
            begin
              // compute provisional offset based on current root size + previous child sizes
              var LOff: Cardinal := LRootSize;
              for LK := 0 to LI - 1 do
                Inc(LOff, Cardinal(Length(LChildNodes[LK])));
              LOffsets[LI] := LOff;
              Inc(LRootSize, Cardinal(Length(AnsiString(LExportSyms[LI])) + 1));
              Inc(LRootSize, Cardinal(UlebLen(LOff)));
            end;
          until (LRootSize = LPrevRootSize);

          // Emit root
          WriteUleb(LExportStream, 0);
          var LChildCount: Byte := Byte(LExportSyms.Count);
          LExportStream.WriteBuffer(LChildCount, 1);
          for LI := 0 to LExportSyms.Count - 1 do
          begin
            var LLabel: AnsiString := AnsiString(LExportSyms[LI]);
            if Length(LLabel) > 0 then
              LExportStream.WriteBuffer(LLabel[1], Length(LLabel));
            var LNull: Byte := 0;
            LExportStream.WriteBuffer(LNull, 1);
            WriteUleb(LExportStream, LOffsets[LI]);
          end;

          // Emit child nodes sequentially
          for LI := 0 to High(LChildNodes) do
            if Length(LChildNodes[LI]) > 0 then
              LExportStream.WriteBuffer(LChildNodes[LI][0], Length(LChildNodes[LI]));

          SetLength(LExportBytes, LExportStream.Size);
          if LExportStream.Size > 0 then
          begin
            LExportStream.Position := 0;
            LExportStream.ReadBuffer(LExportBytes[0], LExportStream.Size);
          end;
        end;
      finally
        LExportStream.Free();
        LExportAddrs.Free();
        LExportSyms.Free();
      end;
    end;

    LOutStream := TMemoryStream.Create();
    try
      LHeaderSize := 32;
      LSegTextFileSize := (Length(LTextBytes) + Length(LCStringBytes) + 15) and (not 15);
      LSegDataFileSize := (Length(LDataBytes) + Length(LGotBytes) + 15) and (not 15);
      // Compute load command sizes dynamically based on dylib names.
      // All LC_SEGMENT_64 must come first (dyld/kernel expect contiguous segments).
      LDylibCmdSizeTotal := 0;
      for LI := 0 to LDylibNames.Count - 1 do
      begin
        LNamePaddedSize := AlignUp8(Cardinal(Length(AnsiString(LDylibNames[LI])) + 1));
        LDylibCmdSizeTotal := LDylibCmdSizeTotal + (24 + LNamePaddedSize); // dylib_command
      end;

      if AIsDylib then
      begin
        // LC_ID_DYLIB uses @loader_path/<filename> so the EXE can load from its folder.
        LIdName := AnsiString('@loader_path/' + TPath.GetFileName(FOutputPath));
        LNamePaddedSize := AlignUp8(Cardinal(Length(LIdName) + 1));
        LDylibCmdSize := 24 + LNamePaddedSize;
      end
      else
      begin
        LIdName := '';
        LDylibCmdSize := 24; // LC_MAIN
      end;

      // PAGEZERO(72) + TEXT(72+160) + DATA(72+160) + LINKEDIT(72) = 608
      LLoadCmdSize := 608 + LDylibCmdSize + 16 {LC_VERSION_MIN_MACOSX} + 24 {LC_UUID} +
                      (IfThen(AIsDylib, 0, 32)) {LC_LOAD_DYLINKER for exec only} +
                      LDylibCmdSizeTotal {LC_LOAD_DYLIB*} +
                      48 {LC_DYLD_INFO_ONLY} + 16 {LC_CODE_SIGNATURE};
      // Map Mach header inside __TEXT (canonical layout): __TEXT.fileoff = 0
      LSegTextFileOff := 0;
      // Place code at a page boundary after header/loadcmds.
      LTextSectionFileOff := PAGE_SIZE;
      // Total mapped __TEXT size: 16K aligned so segment layout satisfies ARM64 dyld/kernel.
      LTextMapSize := AlignUp32(LTextSectionFileOff + LSegTextFileSize, SEGMENT_PAGE_SIZE);

      LSegDataFileOff := LTextMapSize;
      LRebaseOff := AlignUp32(LSegDataFileOff + LSegDataFileSize, SEGMENT_PAGE_SIZE);
      LSegDataSegmentFileSize := LRebaseOff - LSegDataFileOff;
      // Minimal rebase opcodes for PIE: rebase one pointer in __DATA *padding* (after __data+__got).
      // REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: byte = (opcode<<4)|segment_imm; only offset ULEB follows (segment in nibble, not separate ULEB).
      LBindStream := TMemoryStream.Create();
      try
        LByteVal := $11;
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := $22;  // opcode 2, segment 2 (__DATA) in low nibble; next is offset ULEB only
        LBindStream.WriteBuffer(LByteVal, 1);
        LVal := UInt64(Length(LDataBytes)) + UInt64(Length(LGotBytes));
        repeat
          LJ := LVal and $7F;
          LVal := LVal shr 7;
          if LVal <> 0 then
            LJ := LJ or $80;
          LByteVal := Byte(LJ);
          LBindStream.WriteBuffer(LByteVal, 1);
        until LVal = 0;
        LByteVal := $51;
        LBindStream.WriteBuffer(LByteVal, 1);
        LByteVal := $00;
        LBindStream.WriteBuffer(LByteVal, 1);
        SetLength(LRebaseBytes, LBindStream.Size);
        if LBindStream.Size > 0 then
        begin
          LBindStream.Position := 0;
          LBindStream.ReadBuffer(LRebaseBytes[0], LBindStream.Size);
        end;
      finally
        LBindStream.Free();
      end;
      LBindOff := LRebaseOff + Cardinal(Length(LRebaseBytes));
      LSegLinkEditFileOff := LRebaseOff;
      // Place export trie (dylib only) immediately after bind opcodes.
      LExportOff := LBindOff + Cardinal(Length(LBindBytes));
      if (AIsDylib) and (Length(LExportBytes) > 0) then
        LCodeLimit := LExportOff + Cardinal(Length(LExportBytes))
      else
      begin
        LExportOff := 0;
        LCodeLimit := LExportOff; // keep compiler quiet
        LCodeLimit := LBindOff + Cardinal(Length(LBindBytes));
      end;
      LSignatureOffset := AlignUp32(LCodeLimit, CODE_SIGNATURE_ALIGN);
      LPadding := LSignatureOffset - LCodeLimit;
      LNumPages := (LSignatureOffset + PAGE_SIZE - 1) div PAGE_SIZE;
      LSignatureSize := CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE + CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE + CODE_SIGNATURE_IDENTIFIER_SIZE + LNumPages * 32;
      LSegLinkEditFileSize := Cardinal(Length(LRebaseBytes)) + Cardinal(Length(LBindBytes)) +
                              Cardinal(Length(LExportBytes)) + LPadding + LSignatureSize;
      LSlide := SEG_TEXT_VADDR - UInt64(LSegTextFileOff);

      LCardVal := MH_MAGIC_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := CPU_TYPE_ARM64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := CPU_SUBTYPE_ARM64_ALL;
      LOutStream.WriteBuffer(LCardVal, 4);
      if AIsDylib then
        LCardVal := MH_DYLIB
      else
        LCardVal := MH_EXECUTE;
      LOutStream.WriteBuffer(LCardVal, 4);
      // ncmds must match emitted load commands (see LLoadCmdSize comment).
      // 4 segments + (LC_MAIN or LC_ID_DYLIB) + LC_VERSION_MIN + LC_UUID +
      // (LC_LOAD_DYLINKER for exec) + N*LC_LOAD_DYLIB + LC_DYLD_INFO + LC_CODE_SIGNATURE
      if AIsDylib then
        LCmdCount := 9 + Cardinal(LDylibNames.Count)
      else
        LCmdCount := 10 + Cardinal(LDylibNames.Count);
      LCardVal := LCmdCount;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LLoadCmdSize, 4);
      if AIsDylib then
        LCardVal := MH_NOUNDEFS or MH_DYLDLINK or MH_TWOLEVEL
      else
        LCardVal := MH_NOUNDEFS or MH_DYLDLINK or MH_TWOLEVEL or MH_PIE;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);

      LCardVal := LC_SEGMENT_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 72;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi(SEG_PAGEZERO, 16);
      LVal := 0;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := SEG_TEXT_VADDR;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := 0;
      LOutStream.WriteBuffer(LVal, 8);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);

      LCardVal := LC_SEGMENT_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 72 + 160;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__TEXT', 16);
      LVal := SEG_TEXT_VADDR;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LTextMapSize;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegTextFileOff;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LTextMapSize;
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := 5;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 5;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 2;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi('__text', 16);
      WriteFixedAnsi('__TEXT', 16);
      LVal := LSlide + UInt64(LTextSectionFileOff);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LTextBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LTextSectionFileOff;
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
      LVal := LSlide + UInt64(LTextSectionFileOff) + UInt64(Length(LTextBytes));
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LCStringBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LTextSectionFileOff + Length(LTextBytes);
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
      LVal := LSlide + UInt64(LSegDataFileOff);
      LOutStream.WriteBuffer(LVal, 8);
      // vmsize 16K-aligned for ARM64 (dyld/kernel segment mapping).
      LVal := AlignUp32(LSegDataSegmentFileSize, SEGMENT_PAGE_SIZE);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegDataFileOff;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegDataSegmentFileSize;
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
      LVal := LSlide + UInt64(LSegDataFileOff);
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
      LVal := LSlide + UInt64(LSegDataFileOff) + UInt64(Length(LDataBytes));
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

      // __LINKEDIT segment immediately after __DATA (all segments contiguous for dyld/kernel).
      LCardVal := LC_SEGMENT_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 72;
      LOutStream.WriteBuffer(LCardVal, 4);
      WriteFixedAnsi(SEG_LINKEDIT, 16);
      LVal := LSlide + UInt64(LSegLinkEditFileOff);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := AlignUp32(LSegLinkEditFileSize, SEGMENT_PAGE_SIZE);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegLinkEditFileOff;
      LOutStream.WriteBuffer(LVal, 8);
      LVal := LSegLinkEditFileSize;
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := 1;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 1;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);

      if AIsDylib then
      begin
        // LC_ID_DYLIB (dylib install name)
        LCardVal := LC_ID_DYLIB;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := LDylibCmdSize;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 24; // name offset
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 0;  // timestamp
        LOutStream.WriteBuffer(LCardVal, 4);
        LOutStream.WriteBuffer(LCardVal, 4); // current_version
        LOutStream.WriteBuffer(LCardVal, 4); // compatibility_version
        WritePaddedCString(LIdName, Integer(LDylibCmdSize - 24));
      end
      else
      begin
        // LC_MAIN
        LCardVal := LC_MAIN;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 24;
        LOutStream.WriteBuffer(LCardVal, 4);
        LVal := UInt64(LTextSectionFileOff) + LEntryOffset;
        LOutStream.WriteBuffer(LVal, 8);
        LVal := 0;
        LOutStream.WriteBuffer(LVal, 8);
      end;

      // LC_VERSION_MIN_MACOSX (match FPC; kernel may handle this better than LC_BUILD_VERSION)
      LCardVal := LC_VERSION_MIN_MACOSX;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 16;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := $000A0900;  // 10.9.0
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := $000A0900;  // sdk 10.9.0
      LOutStream.WriteBuffer(LCardVal, 4);

      // LC_UUID (required/expected by various tools)
      LCardVal := LC_UUID;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 24;
      LOutStream.WriteBuffer(LCardVal, 4);
      CreateGUID(LUUID);
      LOutStream.WriteBuffer(LUUID, SizeOf(LUUID));

      if not AIsDylib then
      begin
        // Executables need LC_LOAD_DYLINKER. Shared libraries do not.
        LCardVal := LC_LOAD_DYLINKER;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 32;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 12;
        LOutStream.WriteBuffer(LCardVal, 4);
        WritePaddedCString('/usr/lib/dyld', 20);
      end;

      // LC_LOAD_DYLIB entries (dependencies). Ordinals are assigned in this order.
      for LI := 0 to LDylibNames.Count - 1 do
      begin
        LCardVal := LC_LOAD_DYLIB;
        LOutStream.WriteBuffer(LCardVal, 4);
        LNamePaddedSize := AlignUp8(Cardinal(Length(AnsiString(LDylibNames[LI])) + 1));
        LCardVal := 24 + LNamePaddedSize;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 24; // name offset
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 0; // timestamp
        LOutStream.WriteBuffer(LCardVal, 4);
        LOutStream.WriteBuffer(LCardVal, 4); // current_version
        LOutStream.WriteBuffer(LCardVal, 4); // compatibility_version
        WritePaddedCString(AnsiString(LDylibNames[LI]), Integer(LNamePaddedSize));
      end;

      LCardVal := LC_DYLD_INFO_ONLY;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 48;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LRebaseOff, 4);
      LCardVal := Length(LRebaseBytes);
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LBindOff, 4);
      LCardVal := Length(LBindBytes);
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 0;
      // weak_bind_off/size
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      // lazy_bind_off/size
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LCardVal, 4);
      // export_off/size
      if AIsDylib and (Length(LExportBytes) > 0) then
      begin
        LOutStream.WriteBuffer(LExportOff, 4);
        LCardVal := Length(LExportBytes);
        LOutStream.WriteBuffer(LCardVal, 4);
      end
      else
      begin
        LOutStream.WriteBuffer(LCardVal, 4);
        LOutStream.WriteBuffer(LCardVal, 4);
      end;

      LCardVal := LC_CODE_SIGNATURE;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := 16;
      LOutStream.WriteBuffer(LCardVal, 4);
      LOutStream.WriteBuffer(LSignatureOffset, 4);
      LOutStream.WriteBuffer(LSignatureSize, 4);

      LByteVal := 0;
      while Cardinal(LOutStream.Position) < LTextSectionFileOff do
        LOutStream.WriteBuffer(LByteVal, 1);

      if Length(LTextBytes) > 0 then
        LOutStream.WriteBuffer(LTextBytes[0], Length(LTextBytes));
      if Length(LCStringBytes) > 0 then
        LOutStream.WriteBuffer(LCStringBytes[0], Length(LCStringBytes));
      LK := LSegTextFileSize - Length(LTextBytes) - Length(LCStringBytes);
      LByteVal := 0;
      for LI := 0 to LK - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);
      while Cardinal(LOutStream.Position) < LSegDataFileOff do
        LOutStream.WriteBuffer(LByteVal, 1);
      if Length(LDataBytes) > 0 then
        LOutStream.WriteBuffer(LDataBytes[0], Length(LDataBytes));
      if Length(LGotBytes) > 0 then
        LOutStream.WriteBuffer(LGotBytes[0], Length(LGotBytes));
      LK := LSegDataFileSize - Length(LDataBytes) - Length(LGotBytes);
      for LI := 0 to LK - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);
      while Cardinal(LOutStream.Position) < LRebaseOff do
        LOutStream.WriteBuffer(LByteVal, 1);
      if Length(LRebaseBytes) > 0 then
        LOutStream.WriteBuffer(LRebaseBytes[0], Length(LRebaseBytes));
      while Cardinal(LOutStream.Position) < LBindOff do
        LOutStream.WriteBuffer(LByteVal, 1);
      if Length(LBindBytes) > 0 then
        LOutStream.WriteBuffer(LBindBytes[0], Length(LBindBytes));
      if Length(LExportBytes) > 0 then
      begin
        while Cardinal(LOutStream.Position) < LExportOff do
          LOutStream.WriteBuffer(LByteVal, 1);
        LOutStream.WriteBuffer(LExportBytes[0], Length(LExportBytes));
      end;
      for LI := 0 to LPadding - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);
      for LI := 0 to LSignatureSize - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);

      SetLength(Result, LOutStream.Size);
      LOutStream.Position := 0;
      LOutStream.ReadBuffer(Result[0], LOutStream.Size);

      BuildLinkerSignedSignature(Result, LSignatureOffset, LNumPages, LSignatureSize, UInt64(LSegTextFileOff), UInt64(LTextMapSize), CS_EXECSEG_MAIN_BINARY, LSignatureBytes);
      if Length(LSignatureBytes) = LSignatureSize then
        Move(LSignatureBytes[0], Result[LSignatureOffset], LSignatureSize);
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
    LDylibOrdinals.Free();
    LDylibNames.Free();
    LCondJumpFixups.Free();
    LLabelOffsets.Free();
    LJumpFixups.Free();
    LGlobalFixups.Free();
    LDataPageFixups.Free();
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
