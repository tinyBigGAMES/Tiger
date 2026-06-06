{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend.LinuxARM64;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.DateUtils,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Utils.Host,
  Tiger.ExitCodes,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.Backend.ARM64,
  Tiger.ABI,
  Tiger.ABI.LinuxARM64,
  Tiger.Linker,
  Tiger.Linker.ELF,
  Tiger.JIT;

type
  { TTigerLinuxARM64Backend }
  TTigerLinuxARM64Backend = class(TTigerBackend)
  private
    function GenerateELF(): TBytes;
    function GenerateELFObj(): TBytes;
    function GenerateArArchive(): TBytes;
    procedure EnsureOutputDir();
  protected
    procedure PreBuild(); override;
  public
    function TargetExe(const APath: string; const ASubsystem: TTigerSubsystem = ssConsole): TTigerBackend; override;
    function BuildToMemory(): TBytes; override;
    function BuildJIT(): TTigerJIT; override;
    function Run(): Cardinal; override;
    procedure Clear(); override;
  end;

implementation

{$Q-}
// AArch64 codegen performs unsigned 32-bit instruction packing and large
// offset math. Testbed Debug enables $Q+; without this, EIntOverflow is raised
// during harmless bit-or/shl expressions like $8B000000 or (REG_X16 shl 16).

//==============================================================================
// TTigerLinuxARM64Backend
//==============================================================================

procedure TTigerLinuxARM64Backend.EnsureOutputDir();
begin
  // Linux executables have no extension, so CreateDirInPath would treat
  // the filename as a directory. Temporarily append .elf to trick it
  // into creating only the parent directories.
  if not TPath.HasExtension(FOutputPath) then
    TUtils.CreateDirInPath(FOutputPath + '.elf')
  else
    TUtils.CreateDirInPath(FOutputPath);
end;

function TTigerLinuxARM64Backend.TargetExe(const APath: string;
  const ASubsystem: TTigerSubsystem): TTigerBackend;
begin
  // Strip any extension -- Linux executables have none by convention
  FOutputPath := ChangeFileExt(APath, '');
  FOutputType := otExe;
  FSubsystem := ASubsystem;
  Result := Self;
end;

procedure TTigerLinuxARM64Backend.PreBuild();
begin
  EnsureOutputDir();
end;

function TTigerLinuxARM64Backend.BuildToMemory(): TBytes;
begin
  case FOutputType of
    otExe:
      Result := GenerateELF();
    otObj:
      Result := GenerateELFObj();
    otLib:
      Result := GenerateArArchive();
    otDll:
      Result := GenerateELF();  // GenerateELF handles both exe and .so
  end;
end;

function TTigerLinuxARM64Backend.BuildJIT(): TTigerJIT;
begin
  {$IFDEF LINUX}
  raise Exception.Create('Linux ARM64 JIT is not yet implemented');
  {$ELSE}
  raise Exception.Create('Linux ARM64 JIT requires a Linux host (mmap/dlopen). Build to file and run on Linux instead.');
  {$ENDIF}
end;

function TTigerLinuxARM64Backend.Run(): Cardinal;
begin
  if FOutputType <> otExe then
    Exit(Tiger_ErrorBadFormat);

  try
    Result := THostUtils.RunElf(FOutputPath, ExtractFilePath(FOutputPath));
  except
    on E: Exception do
    begin
      Status('Run failed: %s', [E.Message]);
      Result := Tiger_ErrorFileNotFound;
    end;
  end;
end;

procedure TTigerLinuxARM64Backend.Clear();
begin
  inherited Clear();
end;

//==============================================================================
// GenerateELF -- Produces a minimal ELF64 executable
//
// Layout: [ELF header 64B] [PHDR 56B] [.rodata] [.data] [.text + _start]
// Single PT_LOAD segment with PF_R|PF_W|PF_X (Phase 1 simplicity).
//==============================================================================

function TTigerLinuxARM64Backend.GenerateELF(): TBytes;
const
  //--------------------------------------------------------------------------
  // ELF64 Constants
  //--------------------------------------------------------------------------
  ELF_MAGIC: array[0..3] of Byte = ($7F, $45, $4C, $46);

  ELFCLASS64    = 2;
  ELFDATA2LSB   = 1;
  EV_CURRENT    = 1;
  ELFOSABI_NONE = 0;

  ET_EXEC       = 2;
  ET_DYN        = 3;   // Shared object
  EM_AARCH64    = 183;

  PT_LOAD       = 1;

  PF_X          = 1;
  PF_W          = 2;
  PF_R          = 4;

  ELF64_EHDR_SIZE = 64;
  ELF64_PHDR_SIZE = 56;

  BASE_VADDR    = $400000;

  // Program header types
  PT_INTERP     = 3;
  PT_DYNAMIC    = 2;
  PT_PHDR       = 6;
  PT_GNU_STACK  = $6474E551;

  // Section header types
  SHT_NULL      = 0;
  SHT_PROGBITS  = 1;
  SHT_STRTAB    = 3;
  SHT_RELA      = 4;
  SHT_HASH      = 5;
  SHT_DYNAMIC   = 6;
  SHT_DYNSYM    = 11;

  // Section header flags
  SHF_WRITE     = 1;
  SHF_ALLOC     = 2;
  SHF_EXECINSTR = 4;
  SHF_INFO_LINK = $40;

  // Dynamic section tags
  DT_NULL       = 0;
  DT_NEEDED     = 1;
  DT_HASH       = 4;
  DT_STRTAB     = 5;
  DT_SYMTAB     = 6;
  DT_STRSZ      = 10;
  DT_SYMENT     = 11;
  DT_PLTGOT     = 3;
  DT_PLTRELSZ   = 2;
  DT_PLTREL     = 20;
  DT_JMPREL     = 23;
  DT_RELA       = 7;
  DT_INIT       = 12;
  DT_SONAME     = 14;
  DT_RUNPATH    = 29;

  // Symbol binding/type
  STB_GLOBAL    = 1;
  STT_FUNC      = 2;

  // Relocation types
  R_AARCH64_JUMP_SLOT = 1026;

  // Sizes
  ELF64_SHDR_SIZE = 64;
  ELF64_SYM_SIZE  = 24;
  ELF64_RELA_SIZE = 24;
  ELF64_DYN_SIZE  = 16;

var
  LRoDataSection: TMemoryStream;
  LDataSection: TMemoryStream;
  LTextSection: TMemoryStream;

  LFunc: TTigerFuncInfo;
  LFuncOffsets: TArray<Cardinal>;
  LMainIndex: Integer;
  LDllMainIndex: Integer;   // DllMain function index for .so _init
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  LInstr: TTigerInstruction;
  LExportName: string;
  LParamTypes: TArray<TTigerValueType>;

  // Fixup lists
  LCallFixups: TList<TPair<Cardinal, Integer>>;       // Code offset -> func index
  LJumpFixups: TList<TPair<Cardinal, Integer>>;       // Code offset -> label index
  LDataFixups: TList<TPair<Cardinal, Integer>>;       // Code offset -> data index (.rodata)
  LGlobalFixups: TList<TPair<Cardinal, Integer>>;     // Code offset -> data index (.data)
  LFuncAddrFixups: TList<TPair<Cardinal, Integer>>;   // Code offset -> func index (LEA)
  LLabelOffsets: TArray<Cardinal>;

  // Stack frame
  LLocalsSize: Cardinal;
  LMaxCallArgs: Integer;
  LOutgoingArgSpace: Cardinal;
  LStackFrameSize: Cardinal;

  // Backpatch temps
  LCodeOffset: Cardinal;
  LDataIndex: Integer;
  LDisp: Int32;

  // ELF assembly
  LRoDataFileOffset: Cardinal;
  LDataFileOffset: Cardinal;
  LTextFileOffset: Cardinal;
  LEntryPointOffset: Cardinal;
  LMainOffset: Cardinal;
  LTextSize: Cardinal;
  LEntryVAddr: UInt64;
  LTotalFileSize: Cardinal;

  LResult: TMemoryStream;
  LDataHandle: TTigerDataHandle;
  LDataEntryRec: TTigerDataEntry;
  LByteOffset: Cardinal;

  // Dynamic linking
  LHasImports: Boolean;
  LImportCount: Integer;
  LIsSharedObject: Boolean;                     // True when building .so
  LHasExports: Boolean;
  LNumExports: Integer;
  LExportFuncs: TList<TPair<Integer, string>>;  // Function index, export name
  LExportDynstrOffsets: TArray<Cardinal>;
  LSoName: string;                              // SONAME for shared objects
  LSoNameDynstrOffset: Cardinal;                // Offset of SONAME in .dynstr
  LRunpathDynstrOffset: Cardinal;               // Offset of RUNPATH in .dynstr
  LPhdrCount: Integer;
  LPhdrTableSize: Cardinal;
  LEntry: TTigerImportEntry;

  // Dynamic linking sections
  LInterpSection: TMemoryStream;
  LHashSection: TMemoryStream;
  LDynsymSection: TMemoryStream;
  LDynstrSection: TMemoryStream;
  LRelaPltSection: TMemoryStream;
  LPltSection: TMemoryStream;
  LGotPltSection: TMemoryStream;
  LDynamicSection: TMemoryStream;
  LShstrtabSection: TMemoryStream;

  // Dynamic linking offsets
  LInterpFileOffset: Cardinal;
  LHashFileOffset: Cardinal;
  LDynsymFileOffset: Cardinal;
  LDynstrFileOffset: Cardinal;
  LRelaPltFileOffset: Cardinal;
  LPltFileOffset: Cardinal;
  LGotPltFileOffset: Cardinal;
  LDynamicFileOffset: Cardinal;
  LShstrtabFileOffset: Cardinal;
  LShdrsFileOffset: Cardinal;
  LSectionCount: Integer;
  LInterpOffset: Integer;   // 1 for executables (has .interp), 0 for shared objects

  // Dynamic linking helpers
  LDynstrPos: Cardinal;
  LLibNames: TStringList;
  LLibDynstrOffsets: TArray<Cardinal>;
  LSymDynstrOffsets: TArray<Cardinal>;
  LPltFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> import index
  LHashVal: Cardinal;
  LNBuckets: Cardinal;
  LBuckets: TArray<Cardinal>;
  LChains: TArray<Cardinal>;
  LPltEntryFileOffset: Cardinal;
  LGotEntryVAddr: UInt64;
  LRipAfterInstr: UInt64;
  LShstrPos: Cardinal;
  LShNameInterp: Cardinal;
  LShNameHash: Cardinal;
  LShNameDynsym: Cardinal;
  LShNameDynstr: Cardinal;
  LShNameRelaPlt: Cardinal;
  LShNameRodata: Cardinal;
  LShNameData: Cardinal;
  LShNamePlt: Cardinal;
  LShNameText: Cardinal;
  LShNameGotPlt: Cardinal;
  LShNameDynamic: Cardinal;
  LShNameShstrtab: Cardinal;
  LShIdxHash: Integer;
  LShIdxDynsym: Integer;
  LShIdxDynstr: Integer;
  LShIdxRelaPlt: Integer;
  LShIdxPlt: Integer;

  // Static linking
  LLinker: TTigerELFLinker;
  LStaticImportIndices: TList<Integer>;
  LDynamicImportIndices: TList<Integer>;  // Original indices of non-static imports
  LStaticSymbolNames: TStringList;
  LStaticLibPaths: TStringList;
  LStaticResolved: TDictionary<string, TLinkerResolvedSymbol>;
  LStaticImportResolved: TDictionary<Integer, Cardinal>;
  LHasStaticImports: Boolean;
  LExternalTextBase: Cardinal;
  LTargetOffset: Cardinal;
  LMergedBytes: TBytes;
  LResolvedSym: TLinkerResolvedSymbol;
  LImportIndex: Integer;
  LOrigImportIndex: Integer;
  LPltSlotIndex: Integer;
  LOrigToPltIndex: TDictionary<Integer, Integer>;  // original import index -> PLT slot
  LStaticFuncName: string;
  LLibPath: string;
  LResolvedPath: string;
  LCandidate: string;

  // Exception handling (Linux64: setjmp/longjmp based)
  LExceptFrameSize: Integer;
  LExceptFrameBaseOffset: Cardinal;
  LTryBeginLabels: TDictionary<Integer, Integer>;   // label index -> scope index
  LExceptLabels: TDictionary<Integer, Integer>;     // label index -> scope index
  LFinallyLabels: TDictionary<Integer, Integer>;    // label index -> scope index
  LEndLabels: TDictionary<Integer, Integer>;        // label index -> scope index
  LHasSEH: Boolean;
  LPushExceptFrameIdx: Integer;
  LPopExceptFrameIdx: Integer;
  LGetExceptFrameIdx: Integer;
  LSigsetjmpIdx: Integer;
  LInitExceptionsIdx: Integer;
  LInitSignalsIdx: Integer;
  LInitCommandLineIdx: Integer;
  LScopeIdx: Integer;
  LFrameOffset: Cardinal;
  LExceptLabelIdx: Integer;
  //LFinallyLabelIdx: Integer;

  // Float arg classification for System V ABI
  LFloatTemps: TDictionary<Integer, Boolean>;
  LIntArgIdx: Integer;
  LXmmArgIdx: Integer;
  LDataPageFixups: TList<Cardinal>;
  LCondJumpFixups: TList<TPair<Cardinal, Integer>>;
  LForwardJumpFixups: TList<TPair<Cardinal, Cardinal>>;
  LPageIndex: Int64;
  LAdrpImm: Cardinal;
  LInsn: Cardinal;
  LTargetReg: Byte;
  LTargetIndex: Integer;
  LInstrIdx: Integer;
  LOfs12: Cardinal;
  LIncomingSpillSize: Cardinal;

  //--------------------------------------------------------------------------
  // Emit helpers (write to LTextSection)
  //--------------------------------------------------------------------------

  procedure EmitARM64(const AInsn: Cardinal);
  begin
    // Always append: patch helpers leave Position mid-stream.
    LTextSection.Position := LTextSection.Size;
    LTextSection.WriteData(AInsn);
  end;

  procedure PatchAdrpAdd(const ACodeOffset: Cardinal; const ATargetVA: UInt64; const AReg: Byte);
  var
    LInsnVA: UInt64;
    LPageIdx: Int64;
    LAdrp: Cardinal;
    LOfs12: Cardinal;
    LInsn: Cardinal;
  begin
    LInsnVA := BASE_VADDR + LTextFileOffset + ACodeOffset;
    LPageIdx := Int64(ATargetVA shr 12) - Int64(LInsnVA shr 12);
    LAdrp := AdrpPageImm21(LPageIdx);
    LOfs12 := Cardinal(ATargetVA and $FFF);
    LInsn := EncodeInsnAdrp(AReg, LAdrp);
    LTextSection.Position := ACodeOffset;
    LTextSection.WriteData(LInsn);
    LInsn := InsnMerge($91000000, [LOfs12 shl 10, RegShl(AReg, 5), AReg]);
    LTextSection.WriteData(LInsn);
  end;

  procedure PatchBL(const ACodeOffset: Cardinal; const ATargetSectionOffset: Cardinal);
  var
    LImm26: Int32;
  begin
    LImm26 := Int32(ATargetSectionOffset) - Int32(ACodeOffset);
    LImm26 := LImm26 div 4;
    LTextSection.Position := ACodeOffset;
    LTextSection.WriteData($94000000 or (Cardinal(LImm26) and $3FFFFFF));
  end;

  procedure PatchCondBranch(const ACodeOffset: Cardinal; const ATargetOffset: Cardinal);
  var
    LImm19: Int32;
    LInsn: Cardinal;
    LMask: Cardinal;
  begin
    if ACodeOffset + 4 > Cardinal(LTextSection.Size) then
      Exit;
    LImm19 := Int32(ATargetOffset) - Int32(ACodeOffset);
    LImm19 := LImm19 div 4;
    LTextSection.Position := ACodeOffset;
    LTextSection.ReadData(LInsn, 4);
    if (LInsn and $FF000000) = $54000000 then
      LMask := $FFF0001F  // B.cond (e.g. ikVaArgAt B.GE)
    else
      LMask := $FF00001F; // CBZ/CBNZ placeholders (ikJumpIf / ikJumpIfNot)
    LTextSection.Position := ACodeOffset;
    LTextSection.WriteData((LInsn and LMask) or ((Cardinal(LImm19) and $7FFFF) shl 5));
  end;

  procedure PatchUncondBranch(const ACodeOffset: Cardinal; const ATargetOffset: Cardinal);
  var
    LImm26: Int32;
  begin
    if ACodeOffset + 4 > Cardinal(LTextSection.Size) then
      Exit;
    LImm26 := Int32(ATargetOffset) - Int32(ACodeOffset);
    LImm26 := LImm26 div 4;
    LTextSection.Position := ACodeOffset;
    LTextSection.WriteData($14000000 or (Cardinal(LImm26) and $3FFFFFF));
  end;

  procedure AlignStream(const AStream: TMemoryStream; const AAlign: Cardinal);
  var
    LPad: Cardinal;
  begin
    if AStream.Size mod AAlign <> 0 then
    begin
      LPad := AAlign - (AStream.Size mod AAlign);
      AStream.Position := AStream.Size;
      while LPad > 0 do
      begin
        AStream.WriteData(Byte(0));
        Dec(LPad);
      end;
    end;
  end;

  procedure WriteU64(const AStream: TMemoryStream; const AValue: UInt64);
  begin
    AStream.WriteBuffer(AValue, SizeOf(AValue));
  end;

  procedure WriteI64(const AStream: TMemoryStream; const AValue: Int64);
  begin
    AStream.WriteBuffer(AValue, SizeOf(AValue));
  end;

  function ElfHash(const AName: string): Cardinal;
  var
    LC: Integer;
    LG: Cardinal;
  begin
    Result := 0;
    for LC := 1 to Length(AName) do
    begin
      Result := (Result shl 4) + Cardinal(Ord(AName[LC]));
      LG := Result and $F0000000;
      if LG <> 0 then
        Result := Result xor (LG shr 24);
      Result := Result and (not LG);
    end;
  end;

  procedure WriteShdr(const AStream: TMemoryStream;
    const ANameIdx: Cardinal; const AType: Cardinal; const AFlags: UInt64;
    const AAddr: UInt64; const AOffset: UInt64; const ASize: UInt64;
    const ALink: Cardinal; const AInfo: Cardinal;
    const AAddrAlign: UInt64; const AEntSize: UInt64);
  var
    L32: Cardinal;
    L64: UInt64;
  begin
    // Write each field with an explicit byte width (Elf64_Shdr = 64 bytes).
    L32 := ANameIdx;      AStream.WriteBuffer(L32, 4);   // sh_name
    L32 := AType;         AStream.WriteBuffer(L32, 4);   // sh_type
    L64 := AFlags;        AStream.WriteBuffer(L64, 8);   // sh_flags
    L64 := AAddr;         AStream.WriteBuffer(L64, 8);   // sh_addr
    L64 := AOffset;       AStream.WriteBuffer(L64, 8);   // sh_offset
    L64 := ASize;         AStream.WriteBuffer(L64, 8);   // sh_size
    L32 := ALink;         AStream.WriteBuffer(L32, 4);   // sh_link
    L32 := AInfo;         AStream.WriteBuffer(L32, 4);   // sh_info
    L64 := AAddrAlign;    AStream.WriteBuffer(L64, 8);   // sh_addralign
    L64 := AEntSize;      AStream.WriteBuffer(L64, 8);   // sh_entsize
  end;

  procedure AddShstrtabName(const AName: AnsiString; out AOffset: Cardinal);
  var
    B: Byte;
  begin
    AOffset := Cardinal(LShstrtabSection.Size);
    if Length(AName) > 0 then
      LShstrtabSection.WriteBuffer(AName[1], Length(AName))
    else
    begin
      B := 0;
      LShstrtabSection.WriteBuffer(B, 1);
    end;
  end;

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
    // Non-variadic: param 0 at [FP-8], param 1 at [FP-16], ...
    // Variadic: [FP-8] = hidden count, param 0 at [FP-16], param 1 at [FP-24], ...
    if LFunc.IsVariadic then
      Result := -Int32((AIndex + 2) * 8)
    else
      Result := -Int32((AIndex + 1) * 8);
  end;

  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LOffset: Int64;
    LK: Integer;
  begin
    // Locals live below the param spill area.
    LOffset := SpillBaseSize();
    for LK := 0 to AIndex do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -Int32(LOffset);
  end;

  function GetTempOffset(const ATempIndex: Integer): Int32;
  var
    LOffset: Int64;
    LK: Integer;
  begin
    // Temps live below the locals area.
    LOffset := SpillBaseSize();
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    // Temp 0 at [FP-(incoming_spill + locals + 8)], etc.
    LOffset := LOffset + (ATempIndex + 1) * 8;
    Result := -Int32(LOffset);
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

  // LDR Dt / STR Dt (64-bit SIMD&FP) for [FP+disp]. Used for float temps/locals.
  procedure EmitLdrFpD(const ADt: Byte; const ADisp: Int32);
  var
    LOff: Cardinal;
  begin
    if ADisp >= 0 then
    begin
      LOff := Cardinal(ADisp);
      if (LOff <= 32760) and ((LOff and 7) = 0) then
        EmitARM64($FD400000 or ((LOff div 8) shl 10) or (REG_FP shl 5) or ADt)
      else
      begin
        EmitMovRegImm64(REG_X16, LOff);
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($FD400000 or (REG_X16 shl 5) or ADt);
      end;
    end
    else if ADisp >= -256 then
      EmitARM64($FC400000 or (Cardinal(Int32(ADisp) and $1FF) shl 12) or (REG_FP shl 5) or ADt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($FD400000 or (REG_X16 shl 5) or ADt);
    end;
  end;

  procedure EmitStrFpD(const ADisp: Int32; const ADt: Byte);
  var
    LOff: Cardinal;
  begin
    if ADisp >= 0 then
    begin
      LOff := Cardinal(ADisp);
      if (LOff <= 32760) and ((LOff and 7) = 0) then
        EmitARM64($FD000000 or ((LOff div 8) shl 10) or (REG_FP shl 5) or ADt)
      else
      begin
        EmitMovRegImm64(REG_X16, LOff);
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($FD000000 or (REG_X16 shl 5) or ADt);
      end;
    end
    else if ADisp >= -256 then
      EmitARM64($FC000000 or (Cardinal(Int32(ADisp) and $1FF) shl 12) or (REG_FP shl 5) or ADt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($FD000000 or (REG_X16 shl 5) or ADt);
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
    EmitARM64(EncodeInsnAdrpLo16(ARd, LImm));
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
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size,
            Integer((Cardinal(AOp.DataHandle.Index) shl 8) or Cardinal(AReg))));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
      okGlobal:
        begin
          // ADRP + ADD to get address of global in .data; both ADRP page and
          // ADD lo12 are patched in STEP 7 (LGlobalFixups) using the global's
          // full virtual address. NOTE: .data is not page-aligned, so the ADD
          // immediate must be (vaddr and $FFF), NOT the offset within .data.
          LGlobalFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size,
            Integer((Cardinal(AOp.DataHandle.Index) shl 8) or Cardinal(AReg))));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
      okFunc:
        begin
          // ADRP + ADD to get address of function in __text; patched in fixup pass.
          LFuncAddrFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size,
            Integer((Cardinal(AOp.FuncHandle.Index) shl 8) or Cardinal(AReg))));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
    else
      EmitMovRegImm64(AReg, 0);
    end;
  end;

  //--------------------------------------------------------------------------
  // Load call argument to register, handling large struct params.
  // AAPCS64: structs > 16 bytes are passed indirectly (by pointer). When the
  // source is a struct local (not a param), the caller must pass its ADDRESS,
  // not the first 8 bytes of its value. A large-struct param already holds a
  // pointer in its slot, so a plain load is correct there.
  //--------------------------------------------------------------------------
  procedure LoadCallArgToReg(const AOp: TTigerOperand; const AReg: Byte;
    const ATargetFuncIndex: Integer; const AArgIndex: Integer);
  var
    LTargetFunc: TTigerFuncInfo;
    LNeedsAddress: Boolean;
  begin
    LNeedsAddress := False;
    if ATargetFuncIndex >= 0 then
    begin
      LTargetFunc := FCode.GetFunc(ATargetFuncIndex);
      if (AArgIndex >= 0) and (AArgIndex < Length(LTargetFunc.Params)) then
        if LTargetFunc.Params[AArgIndex].ParamSize > 16 then
          if (AOp.Kind = okLocal) and (not AOp.LocalHandle.IsParam) then
            LNeedsAddress := True;
    end;

    if LNeedsAddress then
      EmitSubImm(AReg, REG_FP, Cardinal(-GetLocalOffset(AOp.LocalHandle.Index)))
    else
      LoadOperandToReg(AOp, AReg);
  end;

  //--------------------------------------------------------------------------
  // Store call argument to an outgoing stack slot (args beyond x0-x7),
  // with the same large-struct-by-pointer handling as LoadCallArgToReg.
  //--------------------------------------------------------------------------
  procedure StoreCallArgToStack(const AOp: TTigerOperand; const AArgIndex: Integer;
    const ATargetFuncIndex: Integer);
  var
    LTargetFunc: TTigerFuncInfo;
    LNeedsAddress: Boolean;
  begin
    LNeedsAddress := False;
    if ATargetFuncIndex >= 0 then
    begin
      LTargetFunc := FCode.GetFunc(ATargetFuncIndex);
      if (AArgIndex >= 0) and (AArgIndex < Length(LTargetFunc.Params)) then
        if LTargetFunc.Params[AArgIndex].ParamSize > 16 then
          if (AOp.Kind = okLocal) and (not AOp.LocalHandle.IsParam) then
            LNeedsAddress := True;
    end;

    if LNeedsAddress then
      EmitSubImm(REG_X16, REG_FP, Cardinal(-GetLocalOffset(AOp.LocalHandle.Index)))
    else
      LoadOperandToReg(AOp, REG_X16);
    EmitStrX(REG_X16, REG_SP, Cardinal((AArgIndex - LINUXARM64_MAX_REG_ARGS) * 8));
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

  function IsFloatArg(const AOp: TTigerOperand): Boolean;
  begin
    case AOp.Kind of
      okImmediate:
        Result := AOp.ValueType in [vtFloat32, vtFloat64];
      okTemp:
        Result := LFloatTemps.ContainsKey(AOp.TempHandle.Index);
      okLocal:
        begin
          if (AOp.LocalHandle.Index >= 0) and (AOp.LocalHandle.Index < Length(LFunc.Locals)) then
            Result := LFunc.Locals[AOp.LocalHandle.Index].LocalType in [vtFloat32, vtFloat64]
          else
            Result := False;
        end;
    else
      Result := False;
    end;
  end;

  procedure LoadOperandToVReg(const AOp: TTigerOperand; const ADV: Byte);
  var
    LBits: UInt64;
  begin
    case AOp.Kind of
      okImmediate:
        begin
          LBits := UInt64(PInt64(@AOp.ImmFloat)^);
          EmitMovRegImm64(REG_X16, LBits);
          EmitARM64($9E670000 or (REG_X16 shl 5) or ADV);
        end;
      okLocal:
        if AOp.LocalHandle.IsParam then
          EmitLdrFpD(ADV, GetParamOffset(AOp.LocalHandle.Index))
        else
          EmitLdrFpD(ADV, GetLocalOffset(AOp.LocalHandle.Index));
      okTemp:
        EmitLdrFpD(ADV, GetTempOffset(AOp.TempHandle.Index));
    else
      begin
        LoadOperandToReg(AOp, REG_X16);
        EmitARM64($9E670000 or (REG_X16 shl 5) or ADV);
      end;
    end;
  end;

  procedure StoreTempFromVReg(const ATempIndex: Integer; const ADV: Byte);
  begin
    if GetTempOffset(ATempIndex) >= -255 then
      EmitStrFpD(GetTempOffset(ATempIndex), ADV)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-GetTempOffset(ATempIndex)));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($FD000000 or (REG_X16 shl 5) or ADV);
    end;
  end;

begin
  LRoDataSection := TMemoryStream.Create();
  LDataSection := TMemoryStream.Create();
  LTextSection := TMemoryStream.Create();
  LCallFixups := TList<TPair<Cardinal, Integer>>.Create();
  LJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LDataFixups := TList<TPair<Cardinal, Integer>>.Create();
  LGlobalFixups := TList<TPair<Cardinal, Integer>>.Create();
  LFuncAddrFixups := TList<TPair<Cardinal, Integer>>.Create();
  LPltFixups := TList<TPair<Cardinal, Integer>>.Create();
  LInterpSection := TMemoryStream.Create();
  LHashSection := TMemoryStream.Create();
  LDynsymSection := TMemoryStream.Create();
  LDynstrSection := TMemoryStream.Create();
  LRelaPltSection := TMemoryStream.Create();
  LPltSection := TMemoryStream.Create();
  LGotPltSection := TMemoryStream.Create();
  LDynamicSection := TMemoryStream.Create();
  LShstrtabSection := TMemoryStream.Create();
  LLibNames := TStringList.Create();
  LLibNames.CaseSensitive := False;
  LExportFuncs := TList<TPair<Integer, string>>.Create();

  // Static linking initialization
  LStaticImportIndices := TList<Integer>.Create();
  LDynamicImportIndices := TList<Integer>.Create();
  LStaticSymbolNames := TStringList.Create();
  LStaticLibPaths := TStringList.Create();
  LStaticLibPaths.CaseSensitive := False;
  LStaticLibPaths.Sorted := True;
  LStaticLibPaths.Duplicates := dupIgnore;
  LStaticImportResolved := TDictionary<Integer, Cardinal>.Create();
  LOrigToPltIndex := TDictionary<Integer, Integer>.Create();
  LTryBeginLabels := TDictionary<Integer, Integer>.Create();
  LExceptLabels := TDictionary<Integer, Integer>.Create();
  LFinallyLabels := TDictionary<Integer, Integer>.Create();
  LEndLabels := TDictionary<Integer, Integer>.Create();
  LFloatTemps := TDictionary<Integer, Boolean>.Create();
  LDataPageFixups := TList<Cardinal>.Create();
  LCondJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LForwardJumpFixups := TList<TPair<Cardinal, Cardinal>>.Create();
  LLinker := nil;
  LHasStaticImports := False;
  LHasSEH := False;
  LPushExceptFrameIdx := -1;
  LPopExceptFrameIdx := -1;
  LGetExceptFrameIdx := -1;
  LSigsetjmpIdx := -1;
  LInitExceptionsIdx := -1;
  LInitSignalsIdx := -1;
  LInitCommandLineIdx := -1;

  try
    LHasImports := FImports.GetCount() > 0;
    //LImportCount := FImports.GetCount();
    LIsSharedObject := (FOutputType = otDll);

    // For shared objects, extract SONAME from output path
    if LIsSharedObject then
      LSoName := TPath.GetFileName(FOutputPath)
    else
      LSoName := '';
    LSoNameDynstrOffset := 0;
    LRunpathDynstrOffset := 0;

    //------------------------------------------------------------------------
    // Check for exception handling and find runtime function/import indices
    //------------------------------------------------------------------------
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      // Check for exception scopes
      if Length(LFunc.ExceptionScopes) > 0 then
        LHasSEH := True;
      // Find exception runtime function indices by name
      if SameText(LFunc.FuncName, 'Tiger_PushExceptFrame') then
        LPushExceptFrameIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_PopExceptFrame') then
        LPopExceptFrameIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_GetExceptFrame') then
        LGetExceptFrameIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_InitExceptions') then
        LInitExceptionsIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_InitSignals') then
        LInitSignalsIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_InitCommandLine') then
        LInitCommandLineIdx := LI;
    end;

    // Find __sigsetjmp import index
    for LI := 0 to FImports.GetCount() - 1 do
    begin
      LEntry := FImports.GetEntryByIndex(LI);
      if SameText(LEntry.FuncName, '__sigsetjmp') then
      begin
        LSigsetjmpIdx := LI;
        Break;
      end;
    end;

    //------------------------------------------------------------------------
    // Collect public functions for export (same pattern as Win64)
    //------------------------------------------------------------------------
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      if LFunc.IsPublic then
      begin
        // Compute export name based on linkage
        if LFunc.Linkage = plC then
          LExportName := LFunc.FuncName
        else
        begin
          SetLength(LParamTypes, Length(LFunc.Params));
          for LJ := 0 to High(LFunc.Params) do
            LParamTypes[LJ] := LFunc.Params[LJ].ParamType;
          LExportName := TTigerABIMangler.MangleFunctionWithLinkage(
            LFunc.FuncName, LParamTypes, LFunc.Linkage);
        end;
        LExportFuncs.Add(TPair<Integer, string>.Create(LI, LExportName));
      end;
    end;
    LHasExports := LExportFuncs.Count > 0;
    LNumExports := LExportFuncs.Count;

    //========================================================================
    // STEP 1: Build .rodata section (read-only data: strings, constants)
    //========================================================================
    if FData.GetSize() > 0 then
      LRoDataSection.WriteBuffer(FData.GetDataPointer()^, FData.GetSize());
    if LRoDataSection.Size = 0 then
      LRoDataSection.WriteData(Byte(0));
    AlignStream(LRoDataSection, 16);

    //========================================================================
    // STEP 2: Build .data section (writable globals)
    //========================================================================
    if FGlobals.GetSize() > 0 then
      LDataSection.WriteBuffer(FGlobals.GetDataPointer()^, FGlobals.GetSize());
    if LDataSection.Size = 0 then
      LDataSection.WriteData(Byte(0));
    AlignStream(LDataSection, 16);

    //========================================================================
    // STEP 2b: Separate static vs dynamic imports
    //========================================================================
    for LI := 0 to FImports.GetCount() - 1 do
    begin
      LEntry := FImports.GetEntryByIndex(LI);
      if LEntry.IsStatic then
      begin
        // Static import -- resolve lib path from name
        LStaticImportIndices.Add(LI);
        LStaticSymbolNames.Add(LEntry.FuncName);

        // Build lib filename: append .a if no extension
        LLibPath := LEntry.DllName;
        if ExtractFileExt(LLibPath) = '' then
          LLibPath := LLibPath + '.a';

        // Resolve: absolute → use as-is; otherwise search FLibPaths then output dir
        if not TPath.IsPathRooted(LLibPath) then
        begin
          LResolvedPath := '';
          for LJ := 0 to FLibPaths.Count - 1 do
          begin
            LCandidate := TPath.Combine(FLibPaths[LJ], LLibPath);
            if FileExists(LCandidate) then
            begin
              LResolvedPath := LCandidate;
              Break;
            end;
          end;
          // Fallback: relative to output directory
          if LResolvedPath = '' then
            LResolvedPath := TPath.Combine(ExtractFilePath(FOutputPath), LLibPath);
          LLibPath := LResolvedPath;
        end;

        LStaticLibPaths.Add(LLibPath);
        LHasStaticImports := True;
      end
      else
      begin
        // Dynamic import - add to dynamic list
        LDynamicImportIndices.Add(LI);
      end;
    end;

    // Recalculate import count (dynamic imports only)
    LImportCount := LDynamicImportIndices.Count;
    LHasImports := LImportCount > 0;

    // Build mapping from original import index to PLT slot index
    for LI := 0 to LDynamicImportIndices.Count - 1 do
      LOrigToPltIndex.Add(LDynamicImportIndices[LI], LI);

    //========================================================================
    // STEP 2c: Build dynamic linking sections
    // Required when: dynamic imports present OR building shared object
    //========================================================================
    if LHasImports or LIsSharedObject then
    begin
      //--------------------------------------------------------------------
      // .interp -- dynamic linker path (not needed for shared objects)
      //--------------------------------------------------------------------
      if not LIsSharedObject then
      begin
        LDynstrPos := 0; // reuse as temp
        LInterpSection.WriteBuffer(AnsiString('/lib/ld-linux-aarch64.so.1'#0)[1], 28);
      end;

      //--------------------------------------------------------------------
      // .dynstr -- string table (null byte + symbol names + lib names)
      //--------------------------------------------------------------------
      LDynstrSection.WriteData(Byte(0));  // index 0 = empty string
      LDynstrPos := 1;

      // Import symbol names (dynamic imports only)
      SetLength(LSymDynstrOffsets, LImportCount);
      for LI := 0 to LImportCount - 1 do
      begin
        LOrigImportIndex := LDynamicImportIndices[LI];
        LEntry := FImports.GetEntryByIndex(LOrigImportIndex);
        LSymDynstrOffsets[LI] := LDynstrPos;
        LDynstrSection.WriteBuffer(AnsiString(LEntry.FuncName + #0)[1],
          Length(LEntry.FuncName) + 1);
        Inc(LDynstrPos, Cardinal(Length(LEntry.FuncName)) + 1);
      end;

      // Export symbol names
      SetLength(LExportDynstrOffsets, LNumExports);
      for LI := 0 to LNumExports - 1 do
      begin
        LExportDynstrOffsets[LI] := LDynstrPos;
        LDynstrSection.WriteBuffer(AnsiString(LExportFuncs[LI].Value + #0)[1],
          Length(LExportFuncs[LI].Value) + 1);
        Inc(LDynstrPos, Cardinal(Length(LExportFuncs[LI].Value)) + 1);
      end;

      // Library names (deduplicated, dynamic imports only)
      for LI := 0 to LImportCount - 1 do
      begin
        LOrigImportIndex := LDynamicImportIndices[LI];
        LEntry := FImports.GetEntryByIndex(LOrigImportIndex);
        if LLibNames.IndexOf(LEntry.DllName) < 0 then
          LLibNames.Add(LEntry.DllName);
      end;
      SetLength(LLibDynstrOffsets, LLibNames.Count);
      for LI := 0 to LLibNames.Count - 1 do
      begin
        LLibDynstrOffsets[LI] := LDynstrPos;
        LDynstrSection.WriteBuffer(AnsiString(LLibNames[LI] + #0)[1],
          Length(LLibNames[LI]) + 1);
        Inc(LDynstrPos, Cardinal(Length(LLibNames[LI])) + 1);
      end;

      // SONAME for shared objects
      if LIsSharedObject and (LSoName <> '') then
      begin
        LSoNameDynstrOffset := LDynstrPos;
        LDynstrSection.WriteBuffer(AnsiString(LSoName + #0)[1],
          Length(LSoName) + 1);
        Inc(LDynstrPos, Cardinal(Length(LSoName)) + 1);
      end;

      // Add $ORIGIN for RUNPATH (executables need to find .so in same directory)
      if not LIsSharedObject then
      begin
        LRunpathDynstrOffset := LDynstrPos;
        LDynstrSection.WriteBuffer(AnsiString('$ORIGIN' + #0)[1], 8);
        Inc(LDynstrPos, 8);
      end;

      //--------------------------------------------------------------------
      // .dynsym -- symbol table (STN_UNDEF + imports + exports)
      //--------------------------------------------------------------------
      // Entry 0: STN_UNDEF (24 bytes of zeros)
      for LI := 0 to ELF64_SYM_SIZE - 1 do
        LDynsymSection.WriteData(Byte(0));

      // Import entries (st_shndx = 0 = SHN_UNDEF)
      for LI := 0 to LImportCount - 1 do
      begin
        LDynsymSection.WriteData(LSymDynstrOffsets[LI]);         // st_name
        LDynsymSection.WriteData(Byte((STB_GLOBAL shl 4) or STT_FUNC)); // st_info
        LDynsymSection.WriteData(Byte(0));                       // st_other
        LDynsymSection.WriteData(Word(0));                       // st_shndx = SHN_UNDEF
        LDynsymSection.WriteData(UInt64(0));                     // st_value
        LDynsymSection.WriteData(UInt64(0));                     // st_size
      end;

      // Export entries (st_shndx = 9 = .text section, st_value patched later)
      for LI := 0 to LNumExports - 1 do
      begin
        LDynsymSection.WriteData(LExportDynstrOffsets[LI]);      // st_name
        LDynsymSection.WriteData(Byte((STB_GLOBAL shl 4) or STT_FUNC)); // st_info
        LDynsymSection.WriteData(Byte(0));                       // st_other
        LDynsymSection.WriteData(Word(9));                       // st_shndx = .text section
        LDynsymSection.WriteData(UInt64(0));                     // st_value (placeholder)
        LDynsymSection.WriteData(UInt64(0));                     // st_size
      end;

      //--------------------------------------------------------------------
      // .hash -- SysV hash table for symbol lookup
      //--------------------------------------------------------------------
      LNBuckets := Cardinal(LImportCount + LNumExports);
      if LNBuckets = 0 then
        LNBuckets := 1;
      SetLength(LBuckets, LNBuckets);
      SetLength(LChains, 1 + LImportCount + LNumExports);  // chain[0] = STN_UNDEF
      for LI := 0 to High(LBuckets) do
        LBuckets[LI] := 0;
      for LI := 0 to High(LChains) do
        LChains[LI] := 0;

      // Build hash chains for imports: symbol indices are 1..LImportCount
      for LI := 0 to LImportCount - 1 do
      begin
        LEntry := FImports.GetEntryByIndex(LDynamicImportIndices[LI]);
        LHashVal := ElfHash(LEntry.FuncName) mod LNBuckets;
        // Insert at head of bucket chain
        LChains[LI + 1] := LBuckets[LHashVal];
        LBuckets[LHashVal] := Cardinal(LI + 1);
      end;

      // Build hash chains for exports: symbol indices are (LImportCount+1)..(LImportCount+LNumExports)
      for LI := 0 to LNumExports - 1 do
      begin
        LHashVal := ElfHash(LExportFuncs[LI].Value) mod LNBuckets;
        // Insert at head of bucket chain
        LChains[LImportCount + 1 + LI] := LBuckets[LHashVal];
        LBuckets[LHashVal] := Cardinal(LImportCount + 1 + LI);
      end;

      LHashSection.WriteData(LNBuckets);                           // nbucket
      LHashSection.WriteData(Cardinal(1 + LImportCount + LNumExports)); // nchain
      for LI := 0 to High(LBuckets) do
        LHashSection.WriteData(LBuckets[LI]);
      for LI := 0 to High(LChains) do
        LHashSection.WriteData(LChains[LI]);

      //--------------------------------------------------------------------
      // .rela.plt -- relocations (filled after offset calculation)
      // .plt, .got.plt, .dynamic -- also deferred until offsets known
      //--------------------------------------------------------------------
    end;

    //========================================================================
    // STEP 3: Calculate section file offsets
    //========================================================================
    if LHasImports or LIsSharedObject then
    begin
      // Shared objects: 4 headers (no PT_INTERP)
      // Executables with imports: 5 headers (includes PT_INTERP)
      if LIsSharedObject then
        LPhdrCount := 4
      else
        LPhdrCount := 5;
      LPhdrTableSize := Cardinal(LPhdrCount) * ELF64_PHDR_SIZE;

      // .interp only for executables
      if LIsSharedObject then
      begin
        LInterpFileOffset := 0;  // Not used
        LHashFileOffset   := ELF64_EHDR_SIZE + LPhdrTableSize;
      end
      else
      begin
        LInterpFileOffset := ELF64_EHDR_SIZE + LPhdrTableSize;
        LHashFileOffset   := LInterpFileOffset + Cardinal(LInterpSection.Size);
      end;
      // Align .hash to 8
      if LHashFileOffset mod 8 <> 0 then
        LHashFileOffset := LHashFileOffset + (8 - LHashFileOffset mod 8);
      LDynsymFileOffset   := LHashFileOffset + Cardinal(LHashSection.Size);
      // Align .dynsym to 8
      if LDynsymFileOffset mod 8 <> 0 then
        LDynsymFileOffset := LDynsymFileOffset + (8 - LDynsymFileOffset mod 8);
      LDynstrFileOffset   := LDynsymFileOffset + Cardinal(LDynsymSection.Size);
      LRelaPltFileOffset  := LDynstrFileOffset + Cardinal(LDynstrSection.Size);
      // Align .rela.plt to 8
      if LRelaPltFileOffset mod 8 <> 0 then
        LRelaPltFileOffset := LRelaPltFileOffset + (8 - LRelaPltFileOffset mod 8);

      // .rela.plt size = LImportCount * 24
      LRoDataFileOffset := LRelaPltFileOffset + Cardinal(LImportCount) * ELF64_RELA_SIZE;
      // Align .rodata to 16
      if LRoDataFileOffset mod 16 <> 0 then
        LRoDataFileOffset := LRoDataFileOffset + (16 - LRoDataFileOffset mod 16);
      LDataFileOffset     := LRoDataFileOffset + Cardinal(LRoDataSection.Size);
      // Align .data to 16
      if LDataFileOffset mod 16 <> 0 then
        LDataFileOffset := LDataFileOffset + (16 - LDataFileOffset mod 16);

      // PLT: 32 bytes for PLT[0] + 16 bytes per import (AArch64)
      LPltFileOffset := LDataFileOffset + Cardinal(LDataSection.Size);
      // Align .plt to 16
      if LPltFileOffset mod 16 <> 0 then
        LPltFileOffset := LPltFileOffset + (16 - LPltFileOffset mod 16);

      LTextFileOffset := LPltFileOffset + Cardinal(32 + LImportCount * 16);
    end
    else
    begin
      // No imports and not shared object -- original layout: 1 PHDR
      LPhdrCount := 1;
      LPhdrTableSize := ELF64_PHDR_SIZE;
      LRoDataFileOffset := ELF64_EHDR_SIZE + LPhdrTableSize;
      LDataFileOffset := LRoDataFileOffset + Cardinal(LRoDataSection.Size);
      LTextFileOffset := LDataFileOffset + Cardinal(LDataSection.Size);
    end;

    //========================================================================
    // STEP 4: Generate code for each function (AArch64)
    //========================================================================
    SetLength(LFuncOffsets, FCode.GetFuncCount());
    LMainIndex := -1;
    LDllMainIndex := -1;

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LFuncOffsets[LI] := LTextSection.Size;

      if LFunc.IsEntryPoint then
        LMainIndex := LI;
      if LFunc.IsDllEntry then
        LDllMainIndex := LI;

      SetLength(LLabelOffsets, Length(LFunc.Labels));
      for LJ := 0 to High(LLabelOffsets) do
        LLabelOffsets[LJ] := 0;

      // Reserve a fixed spill area under FP for x0-x7, even if unused.
      // This must match the offset scheme in GetParamOffset/GetLocalOffset/GetTempOffset
      // or locals/temps will alias outgoing arg space and/or clobber saved FP/LR.
      LIncomingSpillSize := Cardinal(SpillBaseSize());

      LLocalsSize := 0;
      for LJ := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + Cardinal(LFunc.Locals[LJ].LocalSize);
      LMaxCallArgs := 0;
      for LJ := 0 to High(LFunc.Instructions) do
        if LFunc.Instructions[LJ].Kind in [ikCallImport, ikCall, ikCallIndirect] then
          if Length(LFunc.Instructions[LJ].Args) > Integer(LMaxCallArgs) then
            LMaxCallArgs := Length(LFunc.Instructions[LJ].Args);
      if LMaxCallArgs > 8 then
        LOutgoingArgSpace := Cardinal(LMaxCallArgs) * 8
      else
        LOutgoingArgSpace := 64;
      LExceptFrameSize := Cardinal(Length(LFunc.ExceptionScopes)) * LINUXARM64_EXCEPT_FRAME_SIZE;
      LExceptFrameBaseOffset := LIncomingSpillSize + Cardinal(LLocalsSize) +
        Cardinal(LFunc.TempCount) * 8 + LOutgoingArgSpace;
      if LExceptFrameSize > 0 then
      begin
        LExceptFrameBaseOffset := AlignUp16(LExceptFrameBaseOffset);
        LExceptFrameBaseOffset := LExceptFrameBaseOffset + 8;
      end;
      LStackFrameSize := LExceptFrameBaseOffset + LExceptFrameSize;
      LStackFrameSize := AlignUp16(LStackFrameSize);

      LTryBeginLabels.Clear();
      LExceptLabels.Clear();
      LFinallyLabels.Clear();
      LEndLabels.Clear();
      for LJ := 0 to High(LFunc.ExceptionScopes) do
      begin
        if LFunc.ExceptionScopes[LJ].TryBeginLabel.IsValid() then
          LTryBeginLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].TryBeginLabel.Index, LJ);
        if LFunc.ExceptionScopes[LJ].ExceptLabel.IsValid() then
          LExceptLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].ExceptLabel.Index, LJ);
        if LFunc.ExceptionScopes[LJ].FinallyLabel.IsValid() then
          LFinallyLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].FinallyLabel.Index, LJ);
        if LFunc.ExceptionScopes[LJ].EndLabel.IsValid() then
          LEndLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].EndLabel.Index, LJ);
      end;

      EmitStpPre(REG_FP, REG_LR, REG_SP, -16);
      EmitARM64($910003E0 or (REG_SP shl 5) or REG_FP);
      if LStackFrameSize > 0 then
        EmitSubImm(REG_SP, REG_SP, LStackFrameSize);

      if LFunc.IsVariadic then
      begin
        // For variadic functions: x0 = hidden count, x1-x7 = declared params or varargs
        // Save ALL 8 registers so VaArgAt can access varargs uniformly
        // Layout: [FP-8] = hidden count (x0), [FP-16] = x1, [FP-24] = x2, ..., [FP-72] = x7
        EmitStrFp(-8, REG_X0);   // Hidden count at [FP-8]
        EmitStrFp(-16, REG_X1);  // Position 1 (param 0 or vararg 0)
        EmitStrFp(-24, REG_X2);  // Position 2 (param 1 or vararg 1)
        EmitStrFp(-32, REG_X3);  // Position 3 (param 2 or vararg 2)
        EmitStrFp(-40, REG_X4);  // Position 4 (param 3 or vararg 3)
        EmitStrFp(-48, REG_X5);  // Position 5 (param 4 or vararg 4)
        EmitStrFp(-56, REG_X6);  // Position 6 (param 5 or vararg 5)
        EmitStrFp(-64, REG_X7);  // Position 7 (param 6 or vararg 6)
        // Note: Position 8+ come from caller's stack, no register save needed
      end
      else
      begin
        // Non-variadic: save declared params only
        for LK := 0 to Min(Length(LFunc.Params) - 1, 7) do
          EmitStrFp(GetParamOffset(LK), LK);
      end;

      if LFunc.IsEntryPoint and LHasSEH and (LInitExceptionsIdx >= 0) then
      begin
        LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInitExceptionsIdx));
        EmitBL(0);
      end;
      if LFunc.IsEntryPoint and LHasSEH and (LInitSignalsIdx >= 0) then
      begin
        LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInitSignalsIdx));
        EmitBL(0);
      end;

      LFloatTemps.Clear();
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        if LFunc.Instructions[LJ].Kind in [ikFAdd, ikFSub, ikFMul, ikFDiv, ikFNeg] then
          LFloatTemps.AddOrSetValue(LFunc.Instructions[LJ].Dest.Index, True);
        if (LFunc.Instructions[LJ].Kind = ikLoad) and (LFunc.Instructions[LJ].Op1.Kind = okLocal) then
        begin
          if (LFunc.Instructions[LJ].Op1.LocalHandle.Index >= 0) and
             (LFunc.Instructions[LJ].Op1.LocalHandle.Index < Length(LFunc.Locals)) and
             (LFunc.Locals[LFunc.Instructions[LJ].Op1.LocalHandle.Index].LocalType in [vtFloat32, vtFloat64]) then
            LFloatTemps.AddOrSetValue(LFunc.Instructions[LJ].Dest.Index, True);
        end;
        if (LFunc.Instructions[LJ].Kind = ikLoadPtr) and LFunc.Instructions[LJ].MemIsFloat then
          LFloatTemps.AddOrSetValue(LFunc.Instructions[LJ].Dest.Index, True);
      end;

      for LInstrIdx := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LInstrIdx];
        case LInstr.Kind of
          ikCallImport:
            begin
              for LK := 0 to High(LInstr.Args) do
              begin
                if LK < LINUXARM64_MAX_REG_ARGS then
                  LoadOperandToReg(LInstr.Args[LK], LINUXARM64_ARG_REGS[LK])
                else
                begin
                  LoadOperandToReg(LInstr.Args[LK], REG_X16);
                  EmitStrX(REG_X16, REG_SP, Cardinal((LK - LINUXARM64_MAX_REG_ARGS) * 8));
                end;
              end;
              LPltFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.ImportTarget.Index));
              EmitBL(0);
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCall:
            begin
              // Stack args (beyond x0-x7) first, so loading register args does
              // not clobber x16 used as the stack-store scratch register.
              for LK := LINUXARM64_MAX_REG_ARGS to High(LInstr.Args) do
                StoreCallArgToStack(LInstr.Args[LK], LK, LInstr.FuncTarget.Index);
              for LK := 0 to Min(Length(LInstr.Args) - 1, LINUXARM64_MAX_REG_ARGS - 1) do
                LoadCallArgToReg(LInstr.Args[LK], LINUXARM64_ARG_REGS[LK],
                  LInstr.FuncTarget.Index, LK);
              LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.FuncTarget.Index));
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
              if IsFloatArg(LInstr.Op2) then
              begin
                LoadOperandToVReg(LInstr.Op2, 0);
                if LInstr.Op1.LocalHandle.IsParam then
                  EmitStrFpD(GetParamOffset(LInstr.Op1.LocalHandle.Index), 0)
                else
                  EmitStrFpD(GetLocalOffset(LInstr.Op1.LocalHandle.Index), 0);
              end
              else
              begin
                LoadOperandToReg(LInstr.Op2, REG_X0);
                if LInstr.Op1.LocalHandle.IsParam then
                  EmitStrFp(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_X0)
                else
                  EmitStrFp(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_X0);
              end;
            end;
          ikLoad:
            begin
              if (LInstr.Op1.Kind = okLocal) and (LInstr.Op1.LocalHandle.Index >= 0) and
                 (LInstr.Op1.LocalHandle.Index < Length(LFunc.Locals)) and
                 (LFunc.Locals[LInstr.Op1.LocalHandle.Index].LocalType in [vtFloat32, vtFloat64]) then
              begin
                if LInstr.Op1.LocalHandle.IsParam then
                  EmitLdrFpD(0, GetParamOffset(LInstr.Op1.LocalHandle.Index))
                else
                  EmitLdrFpD(0, GetLocalOffset(LInstr.Op1.LocalHandle.Index));
                StoreTempFromVReg(LInstr.Dest.Index, 0);
              end
              else
              begin
                LoadOperandToReg(LInstr.Op1, REG_X0);
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
              end;
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
          ikFAdd:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E602800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFSub:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E603800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFMul:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E600800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFDiv:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E601800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFNeg:
            begin
              // Negate: 0.0 - value
              LoadOperandToVReg(LInstr.Op1, 0); // Op1 = 0.0
              LoadOperandToVReg(LInstr.Op2, 1); // Op2 = value
              EmitARM64($1E603800 or (1 shl 16) or (0 shl 5) or 0); // FSUB D0, D0, D1
              StoreTempFromVReg(LInstr.Dest.Index, 0);
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
            begin
              LLabelOffsets[LInstr.LabelTarget.Index] := LTextSection.Size;

              if LTryBeginLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                if LPushExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PushExceptFrame not found - exception handling runtime not linked');
                if LSigsetjmpIdx < 0 then
                  raise Exception.Create('sigsetjmp not imported - exception handling runtime not linked');
                LFrameOffset := LExceptFrameBaseOffset + Cardinal(LScopeIdx) * LINUXARM64_EXCEPT_FRAME_SIZE;
                if LFrameOffset <= 4095 then
                  EmitSubImm(REG_X0, REG_FP, LFrameOffset)
                else
                begin
                  EmitMovRegImm64(REG_X16, LFrameOffset);
                  EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X0);
                end;
                LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LPushExceptFrameIdx));
                EmitBL(0);
                if LFrameOffset + 8 <= 4095 then
                  EmitSubImm(REG_X0, REG_FP, LFrameOffset - 8)
                else
                begin
                  EmitMovRegImm64(REG_X16, LFrameOffset - 8);
                  EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X0);
                end;
                EmitMovX(REG_X1, 0);
                LPltFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LSigsetjmpIdx));
                EmitBL(0);
                if LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.Index
                else if LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.Index
                else
                  LExceptLabelIdx := -1;
                if LExceptLabelIdx >= 0 then
                begin
                  LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LExceptLabelIdx));
                  EmitARM64($35000000);
                end;
              end;

              if LEndLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                if LPopExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PopExceptFrame not found - exception handling runtime not linked');
                LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LPopExceptFrameIdx));
                EmitBL(0);
              end;
            end;
          ikJump:
            if LInstr.LabelTarget.IsValid() then
            begin
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.LabelTarget.Index));
              EmitARM64($14000000);
            end;
          ikJumpIf:
            if LInstr.LabelTarget.IsValid() then
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.LabelTarget.Index));
              EmitARM64($35000000);
            end;
          ikJumpIfNot:
            if LInstr.LabelTarget.IsValid() then
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.LabelTarget.Index));
              EmitARM64($34000000);
            end;
          ikNop:
            ;
          ikSyscall:
            begin
              if Length(LInstr.Args) > 0 then LoadOperandToReg(LInstr.Args[0], REG_X0);
              if Length(LInstr.Args) > 1 then LoadOperandToReg(LInstr.Args[1], REG_X1);
              if Length(LInstr.Args) > 2 then LoadOperandToReg(LInstr.Args[2], REG_X2);
              if Length(LInstr.Args) > 3 then LoadOperandToReg(LInstr.Args[3], REG_X3);
              if Length(LInstr.Args) > 4 then LoadOperandToReg(LInstr.Args[4], REG_X4);
              if Length(LInstr.Args) > 5 then LoadOperandToReg(LInstr.Args[5], REG_X5);
              case LInstr.SyscallNr of
                LINUXARM64_SYS_READ_X64:  EmitMovRegImm64(REG_X8, LINUXARM64_SYS_READ);
                LINUXARM64_SYS_WRITE_X64: EmitMovRegImm64(REG_X8, LINUXARM64_SYS_WRITE);
                LINUXARM64_SYS_EXIT_X64:  EmitMovRegImm64(REG_X8, LINUXARM64_SYS_EXIT);
              else
                EmitMovRegImm64(REG_X8, Cardinal(LInstr.SyscallNr));
              end;
              EmitARM64($D4000001);
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikVaCount:
            begin
              // Load hidden vararg count from [FP-8] (always at fixed position for variadic functions)
              if not LFunc.IsVariadic then
                raise Exception.Create('VaCount can only be used in variadic functions');
              EmitLdrFp(REG_X0, -8);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikVaArgAt:
            begin
              // Load vararg at index from stack
              // Layout: [FP-8] = hidden count
              //         [FP-16] = position 1 (param 0 or vararg 0)
              //         [FP-24] = position 2 (param 1 or vararg 1)
              //         ...
              //         [FP-72] = position 7 (param 6 or vararg 6)
              //         Position 8+ come from caller's stack at [FP+16+(pos-8)*8]
              //
              // Calculate actual position = NumParams + 1 + index
              // If position < 8: use [FP - (8 + position * 8)]
              // If position >= 8: use [FP + 16 + (position - 8) * 8]
              
              if not LFunc.IsVariadic then
                raise Exception.Create('VaArgAt can only be used in variadic functions');
              
              // Load index into X0
              LoadOperandToReg(LInstr.Op1, REG_X0);
              
              // Add (NumParams + 1) to get actual position
              // NumParams = Length(LFunc.Params) (declared params, not counting hidden count)
              EmitAddImm(REG_X0, REG_X0, Cardinal(Length(LFunc.Params) + 1));
              
              // Now X0 = actual position
              // Compare with 8 to determine which stack region
              EmitMovRegImm64(REG_X16, 8);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));  // CMP X0, X16
              
              // Save position in X17 for later use
              EmitARM64($AA0003F1 or (REG_X0 shl 5) or REG_X17);  // MOV X17, X0
              
              // Calculate offset for register path: -(8 + pos * 8)
              // X0 = position, compute offset in X16
              EmitARM64($D37DF000 or (REG_X0 shl 5) or REG_X16);  // LSL X16, X0, #3 (pos * 8)
              EmitAddImm(REG_X16, REG_X16, 8);                     // X16 = 8 + pos * 8
              
              // B.GE to stack args path (branch if position >= 8)
              // B.cond encoding: [31:24]=0x54, [23:5]=imm19, [4]=0, [3:0]=cond (GE=0x0A)
              var LBranchOffset := LTextSection.Size;
              EmitARM64($5400000A);  // B.GE (placeholder offset=0, will be patched)
              
              // === Register arg path: [FP - (8 + pos * 8)] ===
              // X16 has (8 + pos * 8), compute [FP - X16] and load
              EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);  // SUB X16, FP, X16
              EmitLdrX(REG_X0, REG_X16, 0);                      // LDR X0, [X16]
              
              // Jump over stack path
              var LSkipOffset := LTextSection.Size;
              EmitARM64($14000000);  // B (placeholder, will be patched)
              
              // === Stack arg path: [FP + 16 + (pos - 8) * 8] ===
              // Position 8 → [FP+16], Position 9 → [FP+24], etc.
              // Restore position from X17
              var LStackPathStart := LTextSection.Size;
              EmitARM64($AA1103E0 or (REG_X17 shl 5) or REG_X0);  // MOV X0, X17
              EmitARM64($D37DF000 or (REG_X0 shl 5) or REG_X0);    // LSL X0, X0, #3 (pos * 8)
              EmitSubImm(REG_X0, REG_X0, 64);                      // X0 = (pos-8)*8
              EmitAddImm(REG_X16, REG_FP, 16);                     // X16 = FP + 16 (base)
              EmitARM64($8B000000 or (REG_X0 shl 16) or (REG_X16 shl 5) or REG_X16);  // ADD X16, X16, X0 → address
              EmitLdrX(REG_X0, REG_X16, 0);                        // LDR X0, [X16]
              
              // Track forward jumps for patching after LTextBytes is created
              var LRegisterPathSize := LStackPathStart - LBranchOffset - 4;  // Size of register path in bytes
              var LStackPathSize := LTextSection.Size - LStackPathStart;  // Size of stack path in bytes
              LForwardJumpFixups.Add(TPair<Cardinal, Cardinal>.Create(LBranchOffset, LStackPathStart));
              LForwardJumpFixups.Add(TPair<Cardinal, Cardinal>.Create(LSkipOffset, LTextSection.Size));
              
              // Store result
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
        else
          ;
        end;
      end;

      if Length(LFunc.Instructions) > 0 then
      begin
        if not (LFunc.Instructions[High(LFunc.Instructions)].Kind in [ikReturn, ikReturnValue]) then
        begin
          if LStackFrameSize > 0 then
            EmitAddImm(REG_SP, REG_SP, LStackFrameSize);
          EmitLdpPost(REG_FP, REG_LR, REG_SP, 16);
          EmitRet();
        end;
      end
      else
      begin
        if LStackFrameSize > 0 then
          EmitAddImm(REG_SP, REG_SP, LStackFrameSize);
        EmitLdpPost(REG_FP, REG_LR, REG_SP, 16);
        EmitRet();
      end;

      for LJ := 0 to LJumpFixups.Count - 1 do
        if LJumpFixups[LJ].Key + 4 <= Cardinal(LTextSection.Size) then
          PatchUncondBranch(LJumpFixups[LJ].Key, LLabelOffsets[LJumpFixups[LJ].Value]);
      for LJ := 0 to LCondJumpFixups.Count - 1 do
        if LCondJumpFixups[LJ].Key + 4 <= Cardinal(LTextSection.Size) then
          PatchCondBranch(LCondJumpFixups[LJ].Key, LLabelOffsets[LCondJumpFixups[LJ].Value]);
      for LJ := 0 to LForwardJumpFixups.Count - 1 do
      begin
        if (LForwardJumpFixups[LJ].Key + 4 > Cardinal(LTextSection.Size)) or
           (LForwardJumpFixups[LJ].Value > Cardinal(LTextSection.Size)) then
          Continue;
        LTextSection.Position := LForwardJumpFixups[LJ].Key;
        LTextSection.ReadData(LInsn, 4);
        if (LInsn and $FF000000) = $54000000 then
          PatchCondBranch(LForwardJumpFixups[LJ].Key, LForwardJumpFixups[LJ].Value)
        else if (LInsn and $FC000000) = $14000000 then
          PatchUncondBranch(LForwardJumpFixups[LJ].Key, LForwardJumpFixups[LJ].Value);
      end;
      LJumpFixups.Clear();
      LCondJumpFixups.Clear();
      LForwardJumpFixups.Clear();

      // Align between functions only; trailing padding must not precede _start.
      if LI < FCode.GetFuncCount() - 1 then
      begin
        LTextSection.Position := LTextSection.Size;
        AlignStream(LTextSection, 16);
      end;
    end;
    // STEP 4b: Static linking - resolve symbols from .a archives
    //========================================================================
    if LHasStaticImports then
    begin
      LLinker := TTigerELFLinker.Create();
      CopyStatusCallbackTo(LLinker);

      // Add library files from imports
      for LI := 0 to LStaticLibPaths.Count - 1 do
        LLinker.AddLibraryFile(LStaticLibPaths[LI]);

      // Resolve needed symbols
      LLinker.Resolve(LStaticSymbolNames);

      // Align .text to 16 bytes before appending external code
      AlignStream(LTextSection, 16);
      LExternalTextBase := Cardinal(LTextSection.Size);

      // Append merged .text from static libraries
      LMergedBytes := LLinker.GetMergedText();
      if Length(LMergedBytes) >= 8 then
      if Length(LMergedBytes) > 0 then
        LTextSection.WriteBuffer(LMergedBytes[0], Length(LMergedBytes));

      // Build static import resolution map: original import index -> offset in LTextSection
      LStaticResolved := LLinker.GetResolvedSymbols();
      for LI := 0 to LStaticImportIndices.Count - 1 do
      begin
        LImportIndex := LStaticImportIndices[LI];
        LStaticFuncName := LStaticSymbolNames[LI];
        if LStaticResolved.TryGetValue(LStaticFuncName, LResolvedSym) then
        begin
          if LResolvedSym.SectionKind = lskText then
            LStaticImportResolved.Add(LImportIndex,
              LExternalTextBase + LResolvedSym.OffsetInMerged);
        end;
      end;

      Status('Static linker: %d/%d symbols resolved, external .text at offset %d',
        [LStaticImportResolved.Count, LStaticImportIndices.Count, LExternalTextBase]);
    end;

    //========================================================================
    // STEP 5: Backpatch cross-function calls (BL)
    for LI := 0 to LCallFixups.Count - 1 do
      PatchBL(LCallFixups[LI].Key, LFuncOffsets[LCallFixups[LI].Value]);
    LTextSection.Position := LTextSection.Size;

    // STEP 6: Backpatch .rodata (ADRP+ADD)
    for LI := 0 to LDataFixups.Count - 1 do
    begin
      LTargetIndex := LDataFixups[LI].Value shr 8;
      LTargetReg := Byte(LDataFixups[LI].Value and $FF);
      LDataHandle.Index := LTargetIndex;
      LDataEntryRec := FData.GetEntry(LDataHandle);
      PatchAdrpAdd(LDataFixups[LI].Key, BASE_VADDR + LRoDataFileOffset + LDataEntryRec.Offset, LTargetReg);
    end;

    // STEP 7: Backpatch .data (ADRP+ADD) and global page bases
    for LI := 0 to LGlobalFixups.Count - 1 do
    begin
      LTargetIndex := LGlobalFixups[LI].Value shr 8;
      LTargetReg := Byte(LGlobalFixups[LI].Value and $FF);
      LDataHandle.Index := LTargetIndex;
      LDataEntryRec := FGlobals.GetEntry(LDataHandle);
      PatchAdrpAdd(LGlobalFixups[LI].Key, BASE_VADDR + LDataFileOffset + LDataEntryRec.Offset, LTargetReg);
    end;
    for LI := 0 to LDataPageFixups.Count - 1 do
    begin
      LPageIndex := Int64((BASE_VADDR + LDataFileOffset) shr 12) -
                    Int64((BASE_VADDR + LTextFileOffset + LDataPageFixups[LI]) shr 12);
      LAdrpImm := AdrpPageImm21(LPageIndex);
      LTextSection.Position := LDataPageFixups[LI];
      LTextSection.WriteData(EncodeInsnAdrp(REG_X16, LAdrpImm));
    end;

    // STEP 8: Backpatch @func addresses
    for LI := 0 to LFuncAddrFixups.Count - 1 do
    begin
      LJ := LFuncAddrFixups[LI].Value shr 8;
      LTargetReg := Byte(LFuncAddrFixups[LI].Value and $FF);
      PatchAdrpAdd(LFuncAddrFixups[LI].Key, BASE_VADDR + LTextFileOffset + LFuncOffsets[LJ], LTargetReg);
    end;

    // STEP 8b: Backpatch PLT/static import BLs
    for LI := 0 to LPltFixups.Count - 1 do
    begin
      if LStaticImportResolved.TryGetValue(LPltFixups[LI].Value, LTargetOffset) then
        PatchBL(LPltFixups[LI].Key, LTargetOffset)
      else if LOrigToPltIndex.TryGetValue(LPltFixups[LI].Value, LPltSlotIndex) then
        PatchBL(LPltFixups[LI].Key, (LPltFileOffset + 32 + Cardinal(LPltSlotIndex * 16)) - LTextFileOffset)
      else
        raise Exception.CreateFmt(
          'Unpatched import call at .text+0x%x (import index %d)',
          [LPltFixups[LI].Key, LPltFixups[LI].Value]);
    end;
    LTextSection.Position := LTextSection.Size;

    // STEP 9: Emit _start entry point stub (executables only)
    // Shared objects don't have _start; DllMain becomes DT_INIT if present
    //========================================================================
    if LIsSharedObject then
    begin
      // No _start for shared objects
      LEntryPointOffset := 0;  // Will use DT_INIT for DllMain
    end
    else
    begin
      LTextSection.Position := LTextSection.Size;
      LEntryPointOffset := LTextSection.Size;

      if LMainIndex >= 0 then
      begin
        LMainOffset := LFuncOffsets[LMainIndex];

        // Initialize command line: pass argc/argv from stack to Tiger_InitCommandLine
        // At _start: [RSP] = argc, [RSP+8..] = argv pointers
        // LDR x0, [sp]; ADD x1, sp, #8
        EmitARM64($F94003E0);
        EmitARM64($910023E1);
        if LInitCommandLineIdx >= 0 then
          EmitBL(Int32(LFuncOffsets[LInitCommandLineIdx]) - Int32(LTextSection.Size));
        if LHasSEH and (LInitExceptionsIdx >= 0) then
          EmitBL(Int32(LFuncOffsets[LInitExceptionsIdx]) - Int32(LTextSection.Size));
        if LHasSEH and (LInitSignalsIdx >= 0) then
          EmitBL(Int32(LFuncOffsets[LInitSignalsIdx]) - Int32(LTextSection.Size));
        EmitBL(Int32(LMainOffset) - Int32(LTextSection.Size));
        // main return value is already in x0 (AAPCS64)
        EmitMovRegImm64(REG_X8, LINUXARM64_SYS_EXIT);
        EmitARM64($D4000001);
      end
      else
      begin
        EmitMovRegImm64(REG_X0, 0);
        EmitMovRegImm64(REG_X8, LINUXARM64_SYS_EXIT);
        EmitARM64($D4000001);
      end;
    end;

    LTextSize := LTextSection.Size;

    //========================================================================
    // STEP 9b: Patch export symbol st_value in .dynsym
    //========================================================================
    if LHasExports then
    begin
      for LI := 0 to LNumExports - 1 do
      begin
        // Export entries start after STN_UNDEF + imports
        // Each entry is 24 bytes, st_value is at offset 8
        LCodeOffset := Cardinal((1 + LImportCount + LI) * ELF64_SYM_SIZE + 8);
        LDynsymSection.Position := LCodeOffset;
        LDynsymSection.WriteData(UInt64(BASE_VADDR + LTextFileOffset +
          LFuncOffsets[LExportFuncs[LI].Key]));
      end;
      LDynsymSection.Position := LDynsymSection.Size;
    end;

    //========================================================================
    // STEP 10: Build deferred dynamic sections & assemble ELF file
    //========================================================================
    LEntryVAddr := BASE_VADDR + LTextFileOffset + LEntryPointOffset;

    if LHasImports or LIsSharedObject then
    begin
      //--------------------------------------------------------------------
      // Calculate post-text offsets
      //--------------------------------------------------------------------
      LGotPltFileOffset := LTextFileOffset + LTextSize;
      // Align .got.plt to 8
      if LGotPltFileOffset mod 8 <> 0 then
        LGotPltFileOffset := LGotPltFileOffset + (8 - LGotPltFileOffset mod 8);

      LDynamicFileOffset := LGotPltFileOffset + Cardinal(3 + LImportCount) * 8;
      // Align .dynamic to 8
      if LDynamicFileOffset mod 8 <> 0 then
        LDynamicFileOffset := LDynamicFileOffset + (8 - LDynamicFileOffset mod 8);

      //--------------------------------------------------------------------
      // Build .rela.plt (one entry per import)
      //--------------------------------------------------------------------
      for LI := 0 to LImportCount - 1 do
      begin
        // r_offset: VA of GOT[3+n]
        WriteU64(LRelaPltSection,
          BASE_VADDR + LGotPltFileOffset + Cardinal(3 + LI) * 8);
        // r_info: ELF64_R_INFO(symbol_index, R_AARCH64_JUMP_SLOT)
        WriteU64(LRelaPltSection,
          UInt64((UInt64(LI + 1) shl 32) or R_AARCH64_JUMP_SLOT));
        // r_addend
        WriteI64(LRelaPltSection, 0);
      end;

      //--------------------------------------------------------------------
      // Build .plt stubs (match GNU ld/LLD AArch64 lazy-binding layout)
      //--------------------------------------------------------------------
      // PLT[0] resolver (32 bytes): stp; adrp/ldr/add/br to GOT[2]; nops
      begin
        LPltSection.WriteData($A9BF7BF0);  // stp x16, x30, [sp, #-16]!
        LPageIndex := Int64((BASE_VADDR + LGotPltFileOffset + 16) shr 12) -
                      Int64((BASE_VADDR + LPltFileOffset + 4) shr 12);
        LAdrpImm := AdrpPageImm21(LPageIndex);
        LOfs12 := Cardinal((BASE_VADDR + LGotPltFileOffset + 16) and $FFF);
        LPltSection.WriteData(EncodeInsnAdrp(REG_X16, LAdrpImm));
        LPltSection.WriteData(InsnMerge($F9400000,
          [((LOfs12 div 8) shl 10), RegShl(REG_X16, 5), REG_X17]));
        LPltSection.WriteData(InsnMerge($91000000,
          [(LOfs12 shl 10), RegShl(REG_X16, 5), REG_X16]));
        LPltSection.WriteData($D61F0220);  // br x17
        LPltSection.WriteData($D503201F);  // nop
        LPltSection.WriteData($D503201F);  // nop
        LPltSection.WriteData($D503201F);  // nop
        // PLT[0] is exactly 8 instructions (32 bytes); all offset math (+32)
        // depends on this. Do NOT add a 4th nop here.
      end;
      for LI := 0 to LImportCount - 1 do
      begin
        LGotEntryVAddr := BASE_VADDR + LGotPltFileOffset + Cardinal(3 + LI) * 8;
        LRipAfterInstr := BASE_VADDR + LPltFileOffset + 32 + UInt64(LI) * 16;
        LPageIndex := Int64(LGotEntryVAddr shr 12) - Int64(LRipAfterInstr shr 12);
        LAdrpImm := AdrpPageImm21(LPageIndex);
        LOfs12 := Cardinal(LGotEntryVAddr and $FFF);
        LPltSection.WriteData(EncodeInsnAdrp(REG_X16, LAdrpImm));
        LPltSection.WriteData(InsnMerge($F9400000,
          [((LOfs12 div 8) shl 10), RegShl(REG_X16, 5), REG_X17]));
        LPltSection.WriteData(InsnMerge($91000000,
          [(LOfs12 shl 10), RegShl(REG_X16, 5), REG_X16]));
        LPltSection.WriteData($D61F0220);
      end;

      //--------------------------------------------------------------------
      // Build .got.plt
      //--------------------------------------------------------------------
      // GOT[0] = VA of .dynamic
      WriteU64(LGotPltSection, BASE_VADDR + LDynamicFileOffset);
      // GOT[1] = 0 (link_map, filled by ld-linux)
      WriteU64(LGotPltSection, 0);
      // GOT[2] = 0 (_dl_runtime_resolve, filled by ld-linux)
      WriteU64(LGotPltSection, 0);
      // GOT[3+n] = VA of PLT[0] (lazy binding jumps to resolver, not self)
      for LI := 0 to LImportCount - 1 do
        WriteU64(LGotPltSection, BASE_VADDR + LPltFileOffset);

      //--------------------------------------------------------------------
      // Build .dynamic
      //--------------------------------------------------------------------
      // DT_NEEDED for each unique library
      for LI := 0 to LLibNames.Count - 1 do
      begin
        LDynamicSection.WriteData(Int64(DT_NEEDED));
        LDynamicSection.WriteData(Int64(LLibDynstrOffsets[LI]));
      end;
      // DT_SONAME (for shared objects)
      if LIsSharedObject and (LSoName <> '') then
      begin
        LDynamicSection.WriteData(Int64(DT_SONAME));
        LDynamicSection.WriteData(Int64(LSoNameDynstrOffset));
      end;
      // DT_INIT (initialization function for shared objects)
      if LIsSharedObject and (LDllMainIndex >= 0) then
      begin
        LDynamicSection.WriteData(Int64(DT_INIT));
        LDynamicSection.WriteData(Int64(BASE_VADDR + LTextFileOffset +
          LFuncOffsets[LDllMainIndex]));
      end;
      // DT_RUNPATH (executables: find .so in same directory)
      if not LIsSharedObject then
      begin
        LDynamicSection.WriteData(Int64(DT_RUNPATH));
        LDynamicSection.WriteData(Int64(LRunpathDynstrOffset));
      end;
      // DT_HASH
      LDynamicSection.WriteData(Int64(DT_HASH));
      LDynamicSection.WriteData(Int64(BASE_VADDR + LHashFileOffset));
      // DT_STRTAB
      LDynamicSection.WriteData(Int64(DT_STRTAB));
      LDynamicSection.WriteData(Int64(BASE_VADDR + LDynstrFileOffset));
      // DT_SYMTAB
      LDynamicSection.WriteData(Int64(DT_SYMTAB));
      LDynamicSection.WriteData(Int64(BASE_VADDR + LDynsymFileOffset));
      // DT_STRSZ
      LDynamicSection.WriteData(Int64(DT_STRSZ));
      LDynamicSection.WriteData(Int64(LDynstrSection.Size));
      // DT_SYMENT
      LDynamicSection.WriteData(Int64(DT_SYMENT));
      LDynamicSection.WriteData(Int64(ELF64_SYM_SIZE));
      // DT_PLTGOT
      LDynamicSection.WriteData(Int64(DT_PLTGOT));
      LDynamicSection.WriteData(Int64(BASE_VADDR + LGotPltFileOffset));
      // DT_PLTRELSZ
      LDynamicSection.WriteData(Int64(DT_PLTRELSZ));
      LDynamicSection.WriteData(Int64(LRelaPltSection.Size));
      // DT_PLTREL
      LDynamicSection.WriteData(Int64(DT_PLTREL));
      LDynamicSection.WriteData(Int64(DT_RELA));
      // DT_JMPREL
      LDynamicSection.WriteData(Int64(DT_JMPREL));
      LDynamicSection.WriteData(Int64(BASE_VADDR + LRelaPltFileOffset));
      // DT_NULL
      LDynamicSection.WriteData(Int64(DT_NULL));
      LDynamicSection.WriteData(Int64(0));

      //--------------------------------------------------------------------
      // Build .shstrtab (section name string table)
      //--------------------------------------------------------------------
      LShstrtabFileOffset := LDynamicFileOffset + Cardinal(LDynamicSection.Size);

      LShstrtabSection.Clear();
      AddShstrtabName('', LShNameInterp);  // sh_name index 0 = empty string at offset 0
      if not LIsSharedObject then
        AddShstrtabName(AnsiString('.interp'#0), LShNameInterp);
      AddShstrtabName(AnsiString('.hash'#0), LShNameHash);
      AddShstrtabName(AnsiString('.dynsym'#0), LShNameDynsym);
      AddShstrtabName(AnsiString('.dynstr'#0), LShNameDynstr);
      AddShstrtabName(AnsiString('.rela.plt'#0), LShNameRelaPlt);
      AddShstrtabName(AnsiString('.rodata'#0), LShNameRodata);
      AddShstrtabName(AnsiString('.data'#0), LShNameData);
      AddShstrtabName(AnsiString('.plt'#0), LShNamePlt);
      AddShstrtabName(AnsiString('.text'#0), LShNameText);
      AddShstrtabName(AnsiString('.got.plt'#0), LShNameGotPlt);
      AddShstrtabName(AnsiString('.dynamic'#0), LShNameDynamic);
      AddShstrtabName(AnsiString('.shstrtab'#0), LShNameShstrtab);

      // Section headers follow .shstrtab
      LShdrsFileOffset := LShstrtabFileOffset + Cardinal(LShstrtabSection.Size);
      // Align to 8
      if LShdrsFileOffset mod 8 <> 0 then
        LShdrsFileOffset := LShdrsFileOffset + (8 - LShdrsFileOffset mod 8);
      if LIsSharedObject then
      begin
        LSectionCount := 12;  // null + 11 sections (no .interp)
        LInterpOffset := 0;
      end
      else
      begin
        LSectionCount := 13;  // null + 12 sections
        LInterpOffset := 1;
      end;

      LTotalFileSize := LShdrsFileOffset + Cardinal(LSectionCount) * ELF64_SHDR_SIZE;
    end
    else
    begin
      // No imports -- simple layout
      LTotalFileSize := LTextFileOffset + LTextSize;
    end;

    LResult := TMemoryStream.Create();
    try
      //----------------------------------------------------------------------
      // ELF64 Header (64 bytes)
      //----------------------------------------------------------------------
      LResult.WriteData(ELF_MAGIC[0]);
      LResult.WriteData(ELF_MAGIC[1]);
      LResult.WriteData(ELF_MAGIC[2]);
      LResult.WriteData(ELF_MAGIC[3]);
      LResult.WriteData(Byte(ELFCLASS64));     // e_ident[4]: class
      LResult.WriteData(Byte(ELFDATA2LSB));    // e_ident[5]: data encoding
      LResult.WriteData(Byte(EV_CURRENT));     // e_ident[6]: version
      LResult.WriteData(Byte(ELFOSABI_NONE));  // e_ident[7]: OS/ABI
      LResult.WriteData(UInt64(0));            // e_ident[8..15]: padding

      if LIsSharedObject then
        LResult.WriteData(Word(ET_DYN))          // e_type (shared object)
      else
        LResult.WriteData(Word(ET_EXEC));        // e_type (executable)
      LResult.WriteData(Word(EM_AARCH64));      // e_machine
      LResult.WriteData(Cardinal(EV_CURRENT)); // e_version
      if LIsSharedObject then
        LResult.WriteData(UInt64(0))             // e_entry (none for .so)
      else
        LResult.WriteData(UInt64(LEntryVAddr));  // e_entry
      LResult.WriteData(UInt64(ELF64_EHDR_SIZE)); // e_phoff
      if LHasImports or LIsSharedObject then
        LResult.WriteData(UInt64(LShdrsFileOffset))  // e_shoff
      else
        LResult.WriteData(UInt64(0));                // e_shoff (no section headers)
      LResult.WriteData(Cardinal(0));          // e_flags
      LResult.WriteData(Word(ELF64_EHDR_SIZE)); // e_ehsize
      LResult.WriteData(Word(ELF64_PHDR_SIZE)); // e_phentsize
      LResult.WriteData(Word(LPhdrCount));     // e_phnum
      if LHasImports or LIsSharedObject then
      begin
        LResult.WriteData(Word(ELF64_SHDR_SIZE)); // e_shentsize
        LResult.WriteData(Word(LSectionCount));    // e_shnum
        LResult.WriteData(Word(LSectionCount - 1)); // e_shstrndx (.shstrtab index)
      end
      else
      begin
        LResult.WriteData(Word(0));              // e_shentsize
        LResult.WriteData(Word(0));              // e_shnum
        LResult.WriteData(Word(0));              // e_shstrndx
      end;

      //----------------------------------------------------------------------
      // Program Headers
      //----------------------------------------------------------------------
      if LHasImports or LIsSharedObject then
      begin
        // PT_PHDR -- program header table itself
        LResult.WriteData(Cardinal(PT_PHDR));
        LResult.WriteData(Cardinal(PF_R));
        LResult.WriteData(UInt64(ELF64_EHDR_SIZE));
        LResult.WriteData(UInt64(BASE_VADDR + ELF64_EHDR_SIZE));
        LResult.WriteData(UInt64(BASE_VADDR + ELF64_EHDR_SIZE));
        LResult.WriteData(UInt64(LPhdrTableSize));
        LResult.WriteData(UInt64(LPhdrTableSize));
        LResult.WriteData(UInt64(8));

        // PT_INTERP (executables only, not shared objects)
        if not LIsSharedObject then
        begin
          LResult.WriteData(Cardinal(PT_INTERP));
          LResult.WriteData(Cardinal(PF_R));
          LResult.WriteData(UInt64(LInterpFileOffset));
          LResult.WriteData(UInt64(BASE_VADDR + LInterpFileOffset));
          LResult.WriteData(UInt64(BASE_VADDR + LInterpFileOffset));
          LResult.WriteData(UInt64(LInterpSection.Size));
          LResult.WriteData(UInt64(LInterpSection.Size));
          LResult.WriteData(UInt64(1));
        end;

        // PT_LOAD -- entire file (RWX)
        LResult.WriteData(Cardinal(PT_LOAD));
        LResult.WriteData(Cardinal(PF_R or PF_W or PF_X));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(BASE_VADDR));
        LResult.WriteData(UInt64(BASE_VADDR));
        LResult.WriteData(UInt64(LTotalFileSize));
        LResult.WriteData(UInt64(LTotalFileSize));
        LResult.WriteData(UInt64($200000));

        // PT_DYNAMIC
        LResult.WriteData(Cardinal(PT_DYNAMIC));
        LResult.WriteData(Cardinal(PF_R or PF_W));
        LResult.WriteData(UInt64(LDynamicFileOffset));
        LResult.WriteData(UInt64(BASE_VADDR + LDynamicFileOffset));
        LResult.WriteData(UInt64(BASE_VADDR + LDynamicFileOffset));
        LResult.WriteData(UInt64(LDynamicSection.Size));
        LResult.WriteData(UInt64(LDynamicSection.Size));
        LResult.WriteData(UInt64(8));

        // PT_GNU_STACK -- non-executable stack
        LResult.WriteData(Cardinal(PT_GNU_STACK));
        LResult.WriteData(Cardinal(PF_R or PF_W));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(16));
      end
      else
      begin
        // Single PT_LOAD (original Phase 1 layout)
        LResult.WriteData(Cardinal(PT_LOAD));
        LResult.WriteData(Cardinal(PF_R or PF_W or PF_X));
        LResult.WriteData(UInt64(0));
        LResult.WriteData(UInt64(BASE_VADDR));
        LResult.WriteData(UInt64(BASE_VADDR));
        LResult.WriteData(UInt64(LTotalFileSize));
        LResult.WriteData(UInt64(LTotalFileSize));
        LResult.WriteData(UInt64($200000));
      end;

      //----------------------------------------------------------------------
      // Section data
      //----------------------------------------------------------------------
      if LHasImports or LIsSharedObject then
      begin
        // .interp (size=0 for shared objects, so writes nothing)
        LResult.WriteBuffer(LInterpSection.Memory^, LInterpSection.Size);
        // Pad to .hash alignment
        while Cardinal(LResult.Size) < LHashFileOffset do
          LResult.WriteData(Byte(0));
        // .hash
        LResult.WriteBuffer(LHashSection.Memory^, LHashSection.Size);
        // Pad to .dynsym alignment
        while Cardinal(LResult.Size) < LDynsymFileOffset do
          LResult.WriteData(Byte(0));
        // .dynsym
        LResult.WriteBuffer(LDynsymSection.Memory^, LDynsymSection.Size);
        // .dynstr
        LResult.WriteBuffer(LDynstrSection.Memory^, LDynstrSection.Size);
        // Pad to .rela.plt alignment
        while Cardinal(LResult.Size) < LRelaPltFileOffset do
          LResult.WriteData(Byte(0));
        // .rela.plt
        LResult.WriteBuffer(LRelaPltSection.Memory^, LRelaPltSection.Size);
        // Pad to .rodata alignment
        while Cardinal(LResult.Size) < LRoDataFileOffset do
          LResult.WriteData(Byte(0));
      end;

      // .rodata (both paths)
      LResult.WriteBuffer(LRoDataSection.Memory^, LRoDataSection.Size);

      if LHasImports or LIsSharedObject then
      begin
        // Pad to .data alignment
        while Cardinal(LResult.Size) < LDataFileOffset do
          LResult.WriteData(Byte(0));
      end;

      // .data (both paths)
      LResult.WriteBuffer(LDataSection.Memory^, LDataSection.Size);

      if LHasImports then
      begin
        // Pad to .plt alignment
        while Cardinal(LResult.Size) < LPltFileOffset do
          LResult.WriteData(Byte(0));
        // .plt
        LResult.WriteBuffer(LPltSection.Memory^, LPltSection.Size);
      end;

      // .text (both paths)
      LResult.WriteBuffer(LTextSection.Memory^, LTextSize);

      if LHasImports or LIsSharedObject then
      begin
        // Pad to .got.plt alignment
        while Cardinal(LResult.Size) < LGotPltFileOffset do
          LResult.WriteData(Byte(0));
        // .got.plt
        LResult.WriteBuffer(LGotPltSection.Memory^, LGotPltSection.Size);
        // .dynamic
        LResult.WriteBuffer(LDynamicSection.Memory^, LDynamicSection.Size);
        // .shstrtab
        LResult.WriteBuffer(LShstrtabSection.Memory^, LShstrtabSection.Size);

        //------------------------------------------------------------------
        // Section Headers
        //------------------------------------------------------------------
        // Pad to alignment
        while Cardinal(LResult.Size) < LShdrsFileOffset do
          LResult.WriteData(Byte(0));

        // Section header indices (must match write order below)
        LShIdxHash := 1 + LInterpOffset;
        LShIdxDynsym := LShIdxHash + 1;
        LShIdxDynstr := LShIdxDynsym + 1;
        LShIdxRelaPlt := LShIdxDynstr + 1;
        LShIdxPlt := LShIdxRelaPlt + 3;  // .rodata, .data, then .plt

        // [0] SHN_UNDEF
        WriteShdr(LResult, 0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);
        // [1] .interp (executables only)
        if not LIsSharedObject then
          WriteShdr(LResult, LShNameInterp, SHT_PROGBITS, SHF_ALLOC,
            BASE_VADDR + LInterpFileOffset, LInterpFileOffset,
            Cardinal(LInterpSection.Size), 0, 0, 1, 0);
        // .hash
        WriteShdr(LResult, LShNameHash, SHT_HASH, SHF_ALLOC,
          BASE_VADDR + LHashFileOffset, LHashFileOffset,
          Cardinal(LHashSection.Size), Cardinal(LShIdxDynsym), 0, 8, 4);
        // .dynsym
        WriteShdr(LResult, LShNameDynsym, SHT_DYNSYM, SHF_ALLOC,
          BASE_VADDR + LDynsymFileOffset, LDynsymFileOffset,
          Cardinal(LDynsymSection.Size), Cardinal(LShIdxDynstr), 1, 8, ELF64_SYM_SIZE);
        // .dynstr
        WriteShdr(LResult, LShNameDynstr, SHT_STRTAB, SHF_ALLOC,
          BASE_VADDR + LDynstrFileOffset, LDynstrFileOffset,
          Cardinal(LDynstrSection.Size), 0, 0, 1, 0);
        // .rela.plt
        WriteShdr(LResult, LShNameRelaPlt, SHT_RELA, SHF_ALLOC or SHF_INFO_LINK,
          BASE_VADDR + LRelaPltFileOffset, LRelaPltFileOffset,
          Cardinal(LRelaPltSection.Size), Cardinal(LShIdxDynsym),
          Cardinal(LShIdxPlt), 8, ELF64_RELA_SIZE);
        // .rodata
        WriteShdr(LResult, LShNameRodata, SHT_PROGBITS, SHF_ALLOC,
          BASE_VADDR + LRoDataFileOffset, LRoDataFileOffset,
          Cardinal(LRoDataSection.Size), 0, 0, 16, 0);
        // .data
        WriteShdr(LResult, LShNameData, SHT_PROGBITS, SHF_ALLOC or SHF_WRITE,
          BASE_VADDR + LDataFileOffset, LDataFileOffset,
          Cardinal(LDataSection.Size), 0, 0, 16, 0);
        // .plt
        WriteShdr(LResult, LShNamePlt, SHT_PROGBITS, SHF_ALLOC or SHF_EXECINSTR,
          BASE_VADDR + LPltFileOffset, LPltFileOffset,
          Cardinal(LPltSection.Size), 0, 0, 16, 16);
        // .text
        WriteShdr(LResult, LShNameText, SHT_PROGBITS, SHF_ALLOC or SHF_EXECINSTR,
          BASE_VADDR + LTextFileOffset, LTextFileOffset,
          LTextSize, 0, 0, 16, 0);
        // .got.plt
        WriteShdr(LResult, LShNameGotPlt, SHT_PROGBITS, SHF_ALLOC or SHF_WRITE,
          BASE_VADDR + LGotPltFileOffset, LGotPltFileOffset,
          Cardinal(LGotPltSection.Size), 0, 0, 8, 8);
        // .dynamic
        WriteShdr(LResult, LShNameDynamic, SHT_DYNAMIC, SHF_ALLOC or SHF_WRITE,
          BASE_VADDR + LDynamicFileOffset, LDynamicFileOffset,
          Cardinal(LDynamicSection.Size), Cardinal(LShIdxDynstr), 0, 8, ELF64_DYN_SIZE);
        // .shstrtab
        WriteShdr(LResult, LShNameShstrtab, SHT_STRTAB, 0,
          0, LShstrtabFileOffset,
          Cardinal(LShstrtabSection.Size), 0, 0, 1, 0);
      end;

      // Copy to result bytes
      SetLength(Result, LResult.Size);
      Move(LResult.Memory^, Result[0], LResult.Size);
    finally
      LResult.Free();
    end;

  finally
    // Static linking cleanup
    if LLinker <> nil then
      LLinker.Free();
    LOrigToPltIndex.Free();
    LTryBeginLabels.Free();
    LExceptLabels.Free();
    LFinallyLabels.Free();
    LEndLabels.Free();
    LFloatTemps.Free();
    LStaticImportResolved.Free();
    LStaticLibPaths.Free();
    LStaticSymbolNames.Free();
    LDynamicImportIndices.Free();
    LStaticImportIndices.Free();

    LExportFuncs.Free();
    LLibNames.Free();
    LShstrtabSection.Free();
    LDynamicSection.Free();
    LGotPltSection.Free();
    LPltSection.Free();
    LRelaPltSection.Free();
    LDynstrSection.Free();
    LDynsymSection.Free();
    LHashSection.Free();
    LInterpSection.Free();
    LPltFixups.Free();
    LFuncAddrFixups.Free();
    LGlobalFixups.Free();
    LDataFixups.Free();
    LJumpFixups.Free();
    LCallFixups.Free();
    LCondJumpFixups.Free();
    LForwardJumpFixups.Free();
    LDataPageFixups.Free();
    LTextSection.Free();
    LDataSection.Free();
    LRoDataSection.Free();
  end;
end;

//==============================================================================
// GenerateELFObj -- Produces an ELF64 relocatable object file (.o)
//==============================================================================

function TTigerLinuxARM64Backend.GenerateELFObj(): TBytes;
const
  // ELF64 Constants
  ELF_MAGIC: array[0..3] of Byte = ($7F, $45, $4C, $46);
  ELFCLASS64    = 2;
  ELFDATA2LSB   = 1;
  ELFOSABI_NONE = 0;
  ET_REL        = 1;  // Relocatable file
  EM_AARCH64    = 183;

  ELF64_EHDR_SIZE = 64;
  ELF64_SHDR_SIZE = 64;
  ELF64_SYM_SIZE  = 24;
  ELF64_RELA_SIZE = 24;

  // Section types
  SHT_NULL     = 0;
  SHT_PROGBITS = 1;
  SHT_SYMTAB   = 2;
  SHT_STRTAB   = 3;
  SHT_RELA     = 4;

  // Section flags
  SHF_WRITE     = $1;
  SHF_ALLOC     = $2;
  SHF_EXECINSTR = $4;
  SHF_INFO_LINK = $40;

  // Symbol binding/type
  STB_LOCAL  = 0;
  STB_GLOBAL = 1;
  STT_NOTYPE  = 0;
  STT_FUNC    = 2;
  STT_SECTION = 3;

  // Special section indices
  SHN_UNDEF = 0;

  // Relocation types
  R_AARCH64_CALL26 = 283;
  R_AARCH64_ADR_PREL_PG_HI21 = 275;
  R_AARCH64_ADD_ABS_LO12_NC = 277;
  R_AARCH64_ABS64 = 257;

type
  TELFReloc = record
    Offset: Cardinal;      // Offset in .text where reloc applies
    SymbolIndex: Cardinal;  // Index into symbol table
    RelocationType: Cardinal;
    Addend: Int32;
  end;

var
  LResult: TMemoryStream;
  LTextSection: TMemoryStream;
  LRoDataSection: TMemoryStream;
  LStrtabData: TMemoryStream;
  LShstrtabData: TMemoryStream;
  LSymtab: TMemoryStream;
  LRelaText: TMemoryStream;

  // Section indices (0-based for our tracking, but ELF uses 1-based section numbers)
  LShNull: Integer;      // 0: null
  LShText: Integer;      // 1: .text
  LShRoData: Integer;    // 2: .rodata
  LShSymtab: Integer;    // 3: .symtab
  LShStrtab: Integer;    // 4: .strtab
  LShShstrtab: Integer;  // 5: .shstrtab
  LShRelaText: Integer;  // 6: .rela.text
  LSectionCount: Integer;

  // String table helpers
  LShstrtabOffsets: array[0..6] of Cardinal;  // Offsets of section names

  // Symbol tracking
  LSymCount: Integer;
  LFirstGlobalSym: Integer;
  LFuncSymIndices: TArray<Integer>;  // Symbol index for each function

  // Relocation tracking
  LTextRelocs: TList<TELFReloc>;
  LReloc: TELFReloc;

  // Code generation
  LI, LJ, LK: Integer;
  LFunc: TTigerFuncInfo;
  LInstr: TTigerInstruction;
  LFuncOffsets: TArray<Cardinal>;
  LFuncEndOffsets: TArray<Cardinal>;
  LLabelOffsets: TArray<Cardinal>;
  LJumpFixups: TList<TPair<Cardinal, Integer>>;

  // Stack frame
  LStackFrameSize: Cardinal;
  LLocalsSize: Cardinal;
  LMaxCallArgs: Integer;
  LOutgoingArgSpace: Cardinal;

  // Temporaries
  LExportName: string;
  LParamTypes: TArray<TTigerValueType>;
  LNameOffset: Cardinal;
  LEntry: TTigerImportEntry;
  LImportSymIndices: TDictionary<Integer, Integer>;

  // Exception handling (Linux64: setjmp/longjmp based)
  LExceptFrameSize: Integer;
  LExceptFrameBaseOffset: Cardinal;
  LTryBeginLabels: TDictionary<Integer, Integer>;   // label index -> scope index
  LExceptLabels: TDictionary<Integer, Integer>;     // label index -> scope index
  LFinallyLabels: TDictionary<Integer, Integer>;    // label index -> scope index
  LEndLabels: TDictionary<Integer, Integer>;        // label index -> scope index
  LHasSEH: Boolean;
  LPushExceptFrameIdx: Integer;
  LPopExceptFrameIdx: Integer;
  LGetExceptFrameIdx: Integer;
  LSigsetjmpIdx: Integer;
  LInitExceptionsIdx: Integer;
  LInitSignalsIdx: Integer;
  LInitCommandLineIdx: Integer;
  LScopeIdx: Integer;
  LFrameOffset: Cardinal;
  LExceptLabelIdx: Integer;

  // Float arg classification
  LFloatTemps: TDictionary<Integer, Boolean>;
  LDataPageFixups: TList<Cardinal>;
  LCondJumpFixups: TList<TPair<Cardinal, Integer>>;
  LForwardJumpFixups: TList<TPair<Cardinal, Cardinal>>;
  LPageIndex: Int64;
  LAdrpImm: Cardinal;
  LInsn: Cardinal;
  LTargetReg: Byte;
  LTargetIndex: Integer;
  LInstrIdx: Integer;
  LDataHandle: TTigerDataHandle;
  LByteOffset: Cardinal;
  LIncomingSpillSize: Cardinal;
  LOfs12: Cardinal;

  // ELF layout
  LTextOffset: Cardinal;
  LTextSize: Cardinal;
  LRoDataOffset: Cardinal;
  LRoDataSize: Cardinal;
  LSymtabOffset: Cardinal;
  LSymtabSize: Cardinal;
  LStrtabOffset: Cardinal;
  LStrtabSize: Cardinal;
  LRelaTextOffset: Cardinal;
  LRelaTextSize: Cardinal;
  LShstrtabOffset: Cardinal;
  LShstrtabSize: Cardinal;
  LShdrsOffset: Cardinal;

  //----------------------------------------------------------------------------
  // Local AArch64 code emission helpers (same as GenerateELF)
  //----------------------------------------------------------------------------
  const
    MIN_PARAM_SPILL_SIZE = 64;
    LShRoDataSymIdx = 2;  // symtab index of .rodata section symbol

  procedure EmitARM64(const AInsn: Cardinal);
  begin
    LTextSection.Position := LTextSection.Size;
    LTextSection.WriteData(AInsn);
  end;

  procedure PatchCondBranch(const ACodeOffset: Cardinal; const ATargetOffset: Cardinal);
  var
    LImm19: Int32;
    LInsn: Cardinal;
    LMask: Cardinal;
  begin
    if ACodeOffset + 4 > Cardinal(LTextSection.Size) then
      Exit;
    LImm19 := Int32(ATargetOffset) - Int32(ACodeOffset);
    LImm19 := LImm19 div 4;
    LTextSection.Position := ACodeOffset;
    LTextSection.ReadData(LInsn, 4);
    if (LInsn and $FF000000) = $54000000 then
      LMask := $FFF0001F
    else
      LMask := $FF00001F;
    LTextSection.Position := ACodeOffset;
    LTextSection.WriteData((LInsn and LMask) or ((Cardinal(LImm19) and $7FFFF) shl 5));
  end;

  procedure PatchUncondBranch(const ACodeOffset: Cardinal; const ATargetOffset: Cardinal);
  var
    LImm26: Int32;
  begin
    if ACodeOffset + 4 > Cardinal(LTextSection.Size) then
      Exit;
    LImm26 := Int32(ATargetOffset) - Int32(ACodeOffset);
    LImm26 := LImm26 div 4;
    LTextSection.Position := ACodeOffset;
    LTextSection.WriteData($14000000 or (Cardinal(LImm26) and $3FFFFFF));
  end;

  function IncomingParamSpillSize(): Int32;
  begin
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
    if LFunc.IsVariadic then
      Result := -Int32((AIndex + 2) * 8)
    else
      Result := -Int32((AIndex + 1) * 8);
  end;

  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LOffset: Int64;
    LK: Integer;
  begin
    LOffset := SpillBaseSize();
    for LK := 0 to AIndex do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -Int32(LOffset);
  end;

  function GetTempOffset(const ATempIndex: Integer): Int32;
  var
    LOffset: Int64;
    LK: Integer;
  begin
    LOffset := SpillBaseSize();
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    LOffset := LOffset + (ATempIndex + 1) * 8;
    Result := -Int32(LOffset);
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
    EmitARM64($F8400000 or (LImm9 shl 12) or (REG_FP shl 5) or ARt);
  end;

  procedure EmitSturFp(const ADisp: Int32; const ARt: Byte);
  var
    LImm9: Cardinal;
  begin
    LImm9 := Cardinal(Int32(ADisp) and $1FF);
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
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($F9400000 or (REG_X16 shl 5) or ARt);
      end;
    end
    else if ADisp >= -256 then
      EmitLdurFp(ARt, ADisp)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
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
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($F9000000 or (REG_X16 shl 5) or ARt);
      end;
    end
    else if ADisp >= -256 then
      EmitSturFp(ADisp, ARt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($F9000000 or (REG_X16 shl 5) or ARt);
    end;
  end;

  procedure EmitLdrFpD(const ADt: Byte; const ADisp: Int32);
  var
    LOff: Cardinal;
  begin
    if ADisp >= 0 then
    begin
      LOff := Cardinal(ADisp);
      if (LOff <= 32760) and ((LOff and 7) = 0) then
        EmitARM64($FD400000 or ((LOff div 8) shl 10) or (REG_FP shl 5) or ADt)
      else
      begin
        EmitMovRegImm64(REG_X16, LOff);
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($FD400000 or (REG_X16 shl 5) or ADt);
      end;
    end
    else if ADisp >= -256 then
      EmitARM64($FC400000 or (Cardinal(Int32(ADisp) and $1FF) shl 12) or (REG_FP shl 5) or ADt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($FD400000 or (REG_X16 shl 5) or ADt);
    end;
  end;

  procedure EmitStrFpD(const ADisp: Int32; const ADt: Byte);
  var
    LOff: Cardinal;
  begin
    if ADisp >= 0 then
    begin
      LOff := Cardinal(ADisp);
      if (LOff <= 32760) and ((LOff and 7) = 0) then
        EmitARM64($FD000000 or ((LOff div 8) shl 10) or (REG_FP shl 5) or ADt)
      else
      begin
        EmitMovRegImm64(REG_X16, LOff);
        EmitARM64($8B000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
        EmitARM64($FD000000 or (REG_X16 shl 5) or ADt);
      end;
    end
    else if ADisp >= -256 then
      EmitARM64($FC000000 or (Cardinal(Int32(ADisp) and $1FF) shl 12) or (REG_FP shl 5) or ADt)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-ADisp));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($FD000000 or (REG_X16 shl 5) or ADt);
    end;
  end;

  procedure EmitBL(const AOffset: Int32);
  var
    LImm26: Cardinal;
  begin
    LImm26 := Cardinal(AOffset div 4) and $3FFFFFF;
    EmitARM64($94000000 or LImm26);
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
    EmitARM64(EncodeInsnAdrpLo16(ARd, LImm));
  end;

  procedure AddCallReloc(const ASymIdx: Integer);
  begin
    LReloc.Offset := Cardinal(LTextSection.Size);
    LReloc.SymbolIndex := ASymIdx;
    LReloc.RelocationType := R_AARCH64_CALL26;
    LReloc.Addend := 0;
    LTextRelocs.Add(LReloc);
  end;

  procedure AddDataReloc(const AOffset: Cardinal; const ASymIdx: Integer;
    const AType: Cardinal; const AAddend: Int32);
  begin
    LReloc.Offset := AOffset;
    LReloc.SymbolIndex := ASymIdx;
    LReloc.RelocationType := AType;
    LReloc.Addend := AAddend;
    LTextRelocs.Add(LReloc);
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
          LDataHandle.Index := AOp.DataHandle.Index;
          LByteOffset := FData.GetEntry(LDataHandle).Offset;
          AddDataReloc(Cardinal(LTextSection.Size), LShRoDataSymIdx,
            R_AARCH64_ADR_PREL_PG_HI21, Int32(LByteOffset));
          EmitAdrp(AReg, 0);
          AddDataReloc(Cardinal(LTextSection.Size), LShRoDataSymIdx,
            R_AARCH64_ADD_ABS_LO12_NC, Int32(LByteOffset));
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
        end;
      okGlobal:
        begin
          LDataPageFixups.Add(LTextSection.Size);
          EmitAdrp(REG_X16, 0);
          LDataHandle.Index := AOp.DataHandle.Index;
          LByteOffset := FGlobals.GetEntry(LDataHandle).Offset;
          if LByteOffset <= 4095 then
            EmitARM64($91000000 or (Cardinal(LByteOffset) shl 10) or (REG_X16 shl 5) or AReg)
          else
          begin
            EmitMovRegImm64(REG_X17, LByteOffset);
            EmitARM64($8B000000 or (REG_X17 shl 16) or (REG_X16 shl 5) or AReg);
          end;
        end;
      okFunc:
        begin
          LK := AOp.FuncHandle.Index;
          AddDataReloc(Cardinal(LTextSection.Size), LFuncSymIndices[LK],
            R_AARCH64_ADR_PREL_PG_HI21, 0);
          EmitAdrp(AReg, 0);
          AddDataReloc(Cardinal(LTextSection.Size), LFuncSymIndices[LK],
            R_AARCH64_ADD_ABS_LO12_NC, 0);
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
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitStrX(AReg, REG_X16, 0);
    end;
  end;

  function IsFloatArg(const AOp: TTigerOperand): Boolean;
  begin
    case AOp.Kind of
      okImmediate:
        Result := AOp.ValueType in [vtFloat32, vtFloat64];
      okTemp:
        Result := LFloatTemps.ContainsKey(AOp.TempHandle.Index);
      okLocal:
        begin
          if (AOp.LocalHandle.Index >= 0) and
             (AOp.LocalHandle.Index < Length(LFunc.Locals)) then
            Result := LFunc.Locals[AOp.LocalHandle.Index].LocalType in [vtFloat32, vtFloat64]
          else
            Result := False;
        end;
    else
      Result := False;
    end;
  end;

  procedure LoadOperandToVReg(const AOp: TTigerOperand; const ADV: Byte);
  var
    LBits: UInt64;
  begin
    case AOp.Kind of
      okImmediate:
        begin
          LBits := UInt64(PInt64(@AOp.ImmFloat)^);
          EmitMovRegImm64(REG_X16, LBits);
          EmitARM64($9E670000 or (REG_X16 shl 5) or ADV);
        end;
      okLocal:
        if AOp.LocalHandle.IsParam then
          EmitLdrFpD(ADV, GetParamOffset(AOp.LocalHandle.Index))
        else
          EmitLdrFpD(ADV, GetLocalOffset(AOp.LocalHandle.Index));
      okTemp:
        EmitLdrFpD(ADV, GetTempOffset(AOp.TempHandle.Index));
    else
      begin
        LoadOperandToReg(AOp, REG_X16);
        EmitARM64($9E670000 or (REG_X16 shl 5) or ADV);
      end;
    end;
  end;

  procedure StoreTempFromVReg(const ATempIndex: Integer; const ADV: Byte);
  begin
    if GetTempOffset(ATempIndex) >= -255 then
      EmitStrFpD(GetTempOffset(ATempIndex), ADV)
    else
    begin
      EmitMovRegImm64(REG_X16, Cardinal(-GetTempOffset(ATempIndex)));
      EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);
      EmitARM64($FD000000 or (REG_X16 shl 5) or ADV);
    end;
  end;

  procedure AddStrtabString(const AStr: string; out AOffset: Cardinal);
  var
    LAnsi: AnsiString;
  begin
    AOffset := Cardinal(LStrtabData.Size);
    LAnsi := AnsiString(AStr);
    if Length(LAnsi) > 0 then
      LStrtabData.WriteBuffer(LAnsi[1], Length(LAnsi));
    LStrtabData.WriteData(Byte(0));
  end;

  procedure AddShstrtabString(const AStr: string; out AOffset: Cardinal);
  var
    LAnsi: AnsiString;
  begin
    AOffset := Cardinal(LShstrtabData.Size);
    LAnsi := AnsiString(AStr);
    if Length(LAnsi) > 0 then
      LShstrtabData.WriteBuffer(LAnsi[1], Length(LAnsi));
    LShstrtabData.WriteData(Byte(0));
  end;

  procedure WriteSymbol(const ANameIdx: Cardinal; const AInfo: Byte;
    const AOther: Byte; const AShndx: Word; const AValue: UInt64;
    const ASize: UInt64);
  begin
    LSymtab.WriteData(ANameIdx);  // st_name (4 bytes)
    LSymtab.WriteData(AInfo);     // st_info (1 byte)
    LSymtab.WriteData(AOther);    // st_other (1 byte)
    LSymtab.WriteData(AShndx);    // st_shndx (2 bytes)
    LSymtab.WriteData(AValue);    // st_value (8 bytes)
    LSymtab.WriteData(ASize);     // st_size (8 bytes)
  end;

  procedure WriteSectionHeader(const AName: Cardinal; const AType: Cardinal;
    const AFlags: UInt64; const AAddr: UInt64; const AOffset: UInt64;
    const ASize: UInt64; const ALink: Cardinal; const AInfo: Cardinal;
    const AAddralign: UInt64; const AEntsize: UInt64);
  begin
    LResult.WriteData(AName);       // sh_name
    LResult.WriteData(AType);       // sh_type
    LResult.WriteData(AFlags);      // sh_flags
    LResult.WriteData(AAddr);       // sh_addr
    LResult.WriteData(AOffset);     // sh_offset
    LResult.WriteData(ASize);       // sh_size
    LResult.WriteData(ALink);       // sh_link
    LResult.WriteData(AInfo);       // sh_info
    LResult.WriteData(AAddralign);  // sh_addralign
    LResult.WriteData(AEntsize);    // sh_entsize
  end;

  procedure WriteRela(const AOffset: UInt64; const ASymIdx: Cardinal;
    const AType: Cardinal; const AAddend: Int64);
  var
    LInfo: UInt64;
  begin
    LInfo := (UInt64(ASymIdx) shl 32) or AType;
    LRelaText.WriteData(AOffset);
    LRelaText.WriteData(LInfo);
    LRelaText.WriteData(AAddend);
  end;

  procedure AlignStream(const AStream: TMemoryStream; const AAlign: Cardinal);
  var
    LPad: Cardinal;
  begin
    if AStream.Size mod AAlign <> 0 then
    begin
      LPad := AAlign - (AStream.Size mod AAlign);
      AStream.Position := AStream.Size;
      while LPad > 0 do
      begin
        AStream.WriteData(Byte(0));
        Dec(LPad);
      end;
    end;
  end;

begin
  Result := nil;

  LResult := TMemoryStream.Create();
  LTextSection := TMemoryStream.Create();
  LRoDataSection := TMemoryStream.Create();
  LStrtabData := TMemoryStream.Create();
  LShstrtabData := TMemoryStream.Create();
  LSymtab := TMemoryStream.Create();
  LRelaText := TMemoryStream.Create();
  LJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LTextRelocs := TList<TELFReloc>.Create();
  LImportSymIndices := TDictionary<Integer, Integer>.Create();
  LTryBeginLabels := TDictionary<Integer, Integer>.Create();
  LExceptLabels := TDictionary<Integer, Integer>.Create();
  LFinallyLabels := TDictionary<Integer, Integer>.Create();
  LEndLabels := TDictionary<Integer, Integer>.Create();
  LFloatTemps := TDictionary<Integer, Boolean>.Create();
  LCondJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LForwardJumpFixups := TList<TPair<Cardinal, Cardinal>>.Create();
  LDataPageFixups := TList<Cardinal>.Create();
  LHasSEH := False;
  LPushExceptFrameIdx := -1;
  LPopExceptFrameIdx := -1;
  LGetExceptFrameIdx := -1;
  LSigsetjmpIdx := -1;
  LInitExceptionsIdx := -1;
  LInitSignalsIdx := -1;
  LInitCommandLineIdx := -1;

  try
    // Section indices
    LShNull := 0;
    LShText := 1;
    LShRoData := 2;
    LShSymtab := 3;
    LShStrtab := 4;
    LShShstrtab := 5;
    LShRelaText := 6;
    LSectionCount := 7;

    //==========================================================================
    // STEP 1: Build section name string table (.shstrtab)
    //==========================================================================
    AddShstrtabString('', LShstrtabOffsets[0]);           // Null section
    AddShstrtabString('.text', LShstrtabOffsets[1]);
    AddShstrtabString('.rodata', LShstrtabOffsets[2]);
    AddShstrtabString('.symtab', LShstrtabOffsets[3]);
    AddShstrtabString('.strtab', LShstrtabOffsets[4]);
    AddShstrtabString('.shstrtab', LShstrtabOffsets[5]);
    AddShstrtabString('.rela.text', LShstrtabOffsets[6]);

    //==========================================================================
    // STEP 2: Build symbol string table (.strtab) and symbol table (.symtab)
    //==========================================================================
    // First byte of strtab must be null
    LStrtabData.WriteData(Byte(0));

    // Symbol 0: null symbol (required)
    WriteSymbol(0, 0, 0, 0, 0, 0);
    LSymCount := 1;

    // Section symbols (for relocations)
    // Symbol 1: .text section
    WriteSymbol(0, (STB_LOCAL shl 4) or STT_SECTION, 0, Word(LShText), 0, 0);
    Inc(LSymCount);

    // Symbol 2: .rodata section
    WriteSymbol(0, (STB_LOCAL shl 4) or STT_SECTION, 0, Word(LShRoData), 0, 0);
    Inc(LSymCount);

    LFirstGlobalSym := LSymCount;  // First global symbol index

    //------------------------------------------------------------------------
    // Check for exception handling and find runtime function/import indices
    //------------------------------------------------------------------------
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      // Check for exception scopes
      if Length(LFunc.ExceptionScopes) > 0 then
        LHasSEH := True;
      // Find exception runtime function indices by name
      if SameText(LFunc.FuncName, 'Tiger_PushExceptFrame') then
        LPushExceptFrameIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_PopExceptFrame') then
        LPopExceptFrameIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_GetExceptFrame') then
        LGetExceptFrameIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_InitExceptions') then
        LInitExceptionsIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_InitSignals') then
        LInitSignalsIdx := LI
      else if SameText(LFunc.FuncName, 'Tiger_InitCommandLine') then
        LInitCommandLineIdx := LI;
    end;

    // Find __sigsetjmp import index
    for LI := 0 to FImports.GetCount() - 1 do
    begin
      LEntry := FImports.GetEntryByIndex(LI);
      if SameText(LEntry.FuncName, '__sigsetjmp') then
      begin
        LSigsetjmpIdx := LI;
        Break;
      end;
    end;

    // Add function symbols
    SetLength(LFuncSymIndices, FCode.GetFuncCount());
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);

      // Compute export name
      if LFunc.Linkage = plC then
        LExportName := LFunc.FuncName
      else
      begin
        SetLength(LParamTypes, Length(LFunc.Params));
        for LJ := 0 to High(LFunc.Params) do
          LParamTypes[LJ] := LFunc.Params[LJ].ParamType;
        LExportName := TTigerABIMangler.MangleFunctionWithLinkage(
          LFunc.FuncName, LParamTypes, LFunc.Linkage);
      end;

      // Add name to strtab
      AddStrtabString(LExportName, LNameOffset);

      // Write symbol - we'll fix up st_value and st_size after code gen
      // For now, just track the symbol index
      LFuncSymIndices[LI] := LSymCount;

      // Global function symbol - value/size will be patched later
      if LFunc.IsPublic then
        WriteSymbol(LNameOffset, (STB_GLOBAL shl 4) or STT_FUNC, 0, Word(LShText), 0, 0)
      else
        WriteSymbol(LNameOffset, (STB_LOCAL shl 4) or STT_FUNC, 0, Word(LShText), 0, 0);

      Inc(LSymCount);
    end;

    //==========================================================================
    // STEP 2b: Build .rodata section
    //==========================================================================
    if FData.GetSize() > 0 then
      LRoDataSection.WriteBuffer(FData.GetDataPointer()^, FData.GetSize());
    if LRoDataSection.Size = 0 then
      LRoDataSection.WriteData(Byte(0));
    AlignStream(LRoDataSection, 16);

    //==========================================================================
    // STEP 3: Generate code for each function
    //==========================================================================
    SetLength(LFuncOffsets, FCode.GetFuncCount());
    SetLength(LFuncEndOffsets, FCode.GetFuncCount());

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LFuncOffsets[LI] := Cardinal(LTextSection.Size);

      LJumpFixups.Clear();
      LCondJumpFixups.Clear();
      LForwardJumpFixups.Clear();
      LDataPageFixups.Clear();

      LIncomingSpillSize := Cardinal(SpillBaseSize());

      LLocalsSize := 0;
      for LJ := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + Cardinal(LFunc.Locals[LJ].LocalSize);

      LExceptFrameSize := Length(LFunc.ExceptionScopes) * LINUXARM64_EXCEPT_FRAME_SIZE;

      LTryBeginLabels.Clear();
      LExceptLabels.Clear();
      LFinallyLabels.Clear();
      LEndLabels.Clear();
      for LJ := 0 to High(LFunc.ExceptionScopes) do
      begin
        if LFunc.ExceptionScopes[LJ].TryBeginLabel.IsValid() then
          LTryBeginLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].TryBeginLabel.Index, LJ);
        if LFunc.ExceptionScopes[LJ].ExceptLabel.IsValid() then
          LExceptLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].ExceptLabel.Index, LJ);
        if LFunc.ExceptionScopes[LJ].FinallyLabel.IsValid() then
          LFinallyLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].FinallyLabel.Index, LJ);
        if LFunc.ExceptionScopes[LJ].EndLabel.IsValid() then
          LEndLabels.AddOrSetValue(LFunc.ExceptionScopes[LJ].EndLabel.Index, LJ);
      end;

      LMaxCallArgs := 0;
      for LJ := 0 to High(LFunc.Instructions) do
        if LFunc.Instructions[LJ].Kind in [ikCallImport, ikCall, ikCallIndirect] then
          if Length(LFunc.Instructions[LJ].Args) > Integer(LMaxCallArgs) then
            LMaxCallArgs := Length(LFunc.Instructions[LJ].Args);

      if LMaxCallArgs > 8 then
        LOutgoingArgSpace := Cardinal(LMaxCallArgs) * 8
      else
        LOutgoingArgSpace := 64;

      LExceptFrameBaseOffset := LIncomingSpillSize + Cardinal(LLocalsSize) +
        Cardinal(LFunc.TempCount) * 8 + LOutgoingArgSpace;
      if LExceptFrameSize > 0 then
      begin
        LExceptFrameBaseOffset := AlignUp16(LExceptFrameBaseOffset);
        LExceptFrameBaseOffset := LExceptFrameBaseOffset + 8;
      end;
      LStackFrameSize := LExceptFrameBaseOffset + Cardinal(LExceptFrameSize);
      LStackFrameSize := AlignUp16(LStackFrameSize);

      EmitStpPre(REG_FP, REG_LR, REG_SP, -16);
      EmitARM64($910003E0 or (REG_SP shl 5) or REG_FP);
      if LStackFrameSize > 0 then
        EmitSubImm(REG_SP, REG_SP, LStackFrameSize);

      if LFunc.IsVariadic then
      begin
        EmitStrFp(-8, REG_X0);
        EmitStrFp(-16, REG_X1);
        EmitStrFp(-24, REG_X2);
        EmitStrFp(-32, REG_X3);
        EmitStrFp(-40, REG_X4);
        EmitStrFp(-48, REG_X5);
        EmitStrFp(-56, REG_X6);
        EmitStrFp(-64, REG_X7);
      end
      else
      begin
        for LK := 0 to Min(Length(LFunc.Params) - 1, 7) do
          EmitStrFp(GetParamOffset(LK), LK);
      end;

      SetLength(LLabelOffsets, Length(LFunc.Labels));
      for LJ := 0 to High(LLabelOffsets) do
        LLabelOffsets[LJ] := 0;

      // Build float temp set for this function
      LFloatTemps.Clear();
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        if LFunc.Instructions[LJ].Kind in [ikFAdd, ikFSub, ikFMul, ikFDiv, ikFNeg] then
          LFloatTemps.AddOrSetValue(LFunc.Instructions[LJ].Dest.Index, True);
        if (LFunc.Instructions[LJ].Kind = ikLoad) and
           (LFunc.Instructions[LJ].Op1.Kind = okLocal) then
        begin
          if (LFunc.Instructions[LJ].Op1.LocalHandle.Index >= 0) and
             (LFunc.Instructions[LJ].Op1.LocalHandle.Index < Length(LFunc.Locals)) and
             (LFunc.Locals[LFunc.Instructions[LJ].Op1.LocalHandle.Index].LocalType in [vtFloat32, vtFloat64]) then
            LFloatTemps.AddOrSetValue(LFunc.Instructions[LJ].Dest.Index, True);
        end;
        // ikLoadPtr from a float-typed memory location produces a float temp
        if (LFunc.Instructions[LJ].Kind = ikLoadPtr) and
           LFunc.Instructions[LJ].MemIsFloat then
          LFloatTemps.AddOrSetValue(LFunc.Instructions[LJ].Dest.Index, True);
      end;

      for LInstrIdx := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LInstrIdx];
        case LInstr.Kind of
          ikCallImport:
            begin
              for LK := 0 to High(LInstr.Args) do
              begin
                if LK < LINUXARM64_MAX_REG_ARGS then
                  LoadOperandToReg(LInstr.Args[LK], LINUXARM64_ARG_REGS[LK])
                else
                begin
                  LoadOperandToReg(LInstr.Args[LK], REG_X16);
                  EmitStrX(REG_X16, REG_SP, Cardinal((LK - LINUXARM64_MAX_REG_ARGS) * 8));
                end;
              end;
              LK := LInstr.ImportTarget.Index;
              if (LK >= 0) and (LK < FImports.GetCount()) then
              begin
                if not LImportSymIndices.ContainsKey(LK) then
                begin
                  LEntry := FImports.GetEntryByIndex(LK);
                  AddStrtabString(LEntry.FuncName, LNameOffset);
                  LImportSymIndices.Add(LK, LSymCount);
                  WriteSymbol(LNameOffset, (STB_GLOBAL shl 4) or STT_NOTYPE, 0, 0, 0, 0);
                  Inc(LSymCount);
                end;
                AddCallReloc(LImportSymIndices[LK]);
              end;
              EmitBL(0);
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCall:
            begin
              for LK := 0 to Min(Length(LInstr.Args) - 1, 7) do
                LoadOperandToReg(LInstr.Args[LK], LK);
              LK := LInstr.FuncTarget.Index;
              if (LK >= 0) and (LK < Length(LFuncSymIndices)) then
                AddCallReloc(LFuncSymIndices[LK]);
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
              if IsFloatArg(LInstr.Op2) then
              begin
                LoadOperandToVReg(LInstr.Op2, 0);
                if LInstr.Op1.LocalHandle.IsParam then
                  EmitStrFpD(GetParamOffset(LInstr.Op1.LocalHandle.Index), 0)
                else
                  EmitStrFpD(GetLocalOffset(LInstr.Op1.LocalHandle.Index), 0);
              end
              else
              begin
                LoadOperandToReg(LInstr.Op2, REG_X0);
                if LInstr.Op1.LocalHandle.IsParam then
                  EmitStrFp(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_X0)
                else
                  EmitStrFp(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_X0);
              end;
            end;
          ikLoad:
            begin
              if (LInstr.Op1.Kind = okLocal) and (LInstr.Op1.LocalHandle.Index >= 0) and
                 (LInstr.Op1.LocalHandle.Index < Length(LFunc.Locals)) and
                 (LFunc.Locals[LInstr.Op1.LocalHandle.Index].LocalType in [vtFloat32, vtFloat64]) then
              begin
                if LInstr.Op1.LocalHandle.IsParam then
                  EmitLdrFpD(0, GetParamOffset(LInstr.Op1.LocalHandle.Index))
                else
                  EmitLdrFpD(0, GetLocalOffset(LInstr.Op1.LocalHandle.Index));
                StoreTempFromVReg(LInstr.Dest.Index, 0);
              end
              else
              begin
                LoadOperandToReg(LInstr.Op1, REG_X0);
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
              end;
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
          ikFAdd:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E602800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFSub:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E603800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFMul:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E600800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFDiv:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E601800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikFNeg:
            begin
              LoadOperandToVReg(LInstr.Op1, 0);
              LoadOperandToVReg(LInstr.Op2, 1);
              EmitARM64($1E603800 or (1 shl 16) or (0 shl 5) or 0);
              StoreTempFromVReg(LInstr.Dest.Index, 0);
            end;
          ikBitAnd:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($8A000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitOr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($AA000000 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikBitXor:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
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
              EmitARM64($9A9F07E0 or ((0 xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpNe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F07E0 or ((1 xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpLt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F07E0 or (($0B xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpLe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F07E0 or (($0D xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpGt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
              EmitARM64($9A9F07E0 or (($0C xor 1) shl 12) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikCmpGe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              EmitARM64($EB00001F or (REG_X16 shl 16) or (REG_X0 shl 5));
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
            begin
              LLabelOffsets[LInstr.LabelTarget.Index] := LTextSection.Size;

              if LTryBeginLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                if LPushExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PushExceptFrame not found - exception handling runtime not linked');
                if LSigsetjmpIdx < 0 then
                  raise Exception.Create('__sigsetjmp not imported - exception handling runtime not linked');
                LFrameOffset := LExceptFrameBaseOffset + Cardinal(LScopeIdx) * LINUXARM64_EXCEPT_FRAME_SIZE;
                if LFrameOffset <= 4095 then
                  EmitSubImm(REG_X0, REG_FP, LFrameOffset)
                else
                begin
                  EmitMovRegImm64(REG_X16, LFrameOffset);
                  EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X0);
                end;
                AddCallReloc(LFuncSymIndices[LPushExceptFrameIdx]);
                EmitBL(0);
                if LFrameOffset + 8 <= 4095 then
                  EmitSubImm(REG_X0, REG_FP, LFrameOffset - 8)
                else
                begin
                  EmitMovRegImm64(REG_X16, LFrameOffset - 8);
                  EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X0);
                end;
                EmitMovX(REG_X1, 0);
                if not LImportSymIndices.ContainsKey(LSigsetjmpIdx) then
                begin
                  LEntry := FImports.GetEntryByIndex(LSigsetjmpIdx);
                  AddStrtabString(LEntry.FuncName, LNameOffset);
                  WriteSymbol(LNameOffset, (STB_GLOBAL shl 4) or STT_NOTYPE, 0, SHN_UNDEF, 0, 0);
                  LImportSymIndices.Add(LSigsetjmpIdx, LSymCount);
                  Inc(LSymCount);
                end;
                AddCallReloc(LImportSymIndices[LSigsetjmpIdx]);
                EmitBL(0);
                if LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.Index
                else if LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.Index
                else
                  LExceptLabelIdx := -1;
                if LExceptLabelIdx >= 0 then
                begin
                  LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LExceptLabelIdx));
                  EmitARM64($35000000);
                end;
              end;

              if LEndLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                if LPopExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PopExceptFrame not found - exception handling runtime not linked');
                AddCallReloc(LFuncSymIndices[LPopExceptFrameIdx]);
                EmitBL(0);
              end;
            end;
          ikJump:
            if LInstr.LabelTarget.IsValid() then
            begin
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.LabelTarget.Index));
              EmitARM64($14000000);
            end;
          ikJumpIf:
            if LInstr.LabelTarget.IsValid() then
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.LabelTarget.Index));
              EmitARM64($35000000);
            end;
          ikJumpIfNot:
            if LInstr.LabelTarget.IsValid() then
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.LabelTarget.Index));
              EmitARM64($34000000);
            end;
          ikNop:
            ;
          ikSyscall:
            begin
              if Length(LInstr.Args) > 0 then LoadOperandToReg(LInstr.Args[0], REG_X0);
              if Length(LInstr.Args) > 1 then LoadOperandToReg(LInstr.Args[1], REG_X1);
              if Length(LInstr.Args) > 2 then LoadOperandToReg(LInstr.Args[2], REG_X2);
              if Length(LInstr.Args) > 3 then LoadOperandToReg(LInstr.Args[3], REG_X3);
              if Length(LInstr.Args) > 4 then LoadOperandToReg(LInstr.Args[4], REG_X4);
              if Length(LInstr.Args) > 5 then LoadOperandToReg(LInstr.Args[5], REG_X5);
              case LInstr.SyscallNr of
                LINUXARM64_SYS_READ_X64:  EmitMovRegImm64(REG_X8, LINUXARM64_SYS_READ);
                LINUXARM64_SYS_WRITE_X64: EmitMovRegImm64(REG_X8, LINUXARM64_SYS_WRITE);
                LINUXARM64_SYS_EXIT_X64:  EmitMovRegImm64(REG_X8, LINUXARM64_SYS_EXIT);
              else
                EmitMovRegImm64(REG_X8, Cardinal(LInstr.SyscallNr));
              end;
              EmitARM64($D4000001);
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
        else
          ;
        end;
      end;

      if Length(LFunc.Instructions) > 0 then
      begin
        if not (LFunc.Instructions[High(LFunc.Instructions)].Kind in [ikReturn, ikReturnValue]) then
        begin
          if LStackFrameSize > 0 then
            EmitAddImm(REG_SP, REG_SP, LStackFrameSize);
          EmitLdpPost(REG_FP, REG_LR, REG_SP, 16);
          EmitRet();
        end;
      end
      else
      begin
        if LStackFrameSize > 0 then
          EmitAddImm(REG_SP, REG_SP, LStackFrameSize);
        EmitLdpPost(REG_FP, REG_LR, REG_SP, 16);
        EmitRet();
      end;

      LFuncEndOffsets[LI] := Cardinal(LTextSection.Size);

      for LJ := 0 to LJumpFixups.Count - 1 do
        if LJumpFixups[LJ].Key + 4 <= Cardinal(LTextSection.Size) then
          PatchUncondBranch(LJumpFixups[LJ].Key, LLabelOffsets[LJumpFixups[LJ].Value]);
      for LJ := 0 to LCondJumpFixups.Count - 1 do
        if LCondJumpFixups[LJ].Key + 4 <= Cardinal(LTextSection.Size) then
          PatchCondBranch(LCondJumpFixups[LJ].Key, LLabelOffsets[LCondJumpFixups[LJ].Value]);
      for LJ := 0 to LForwardJumpFixups.Count - 1 do
      begin
        if (LForwardJumpFixups[LJ].Key + 4 > Cardinal(LTextSection.Size)) or
           (LForwardJumpFixups[LJ].Value > Cardinal(LTextSection.Size)) then
          Continue;
        LTextSection.Position := LForwardJumpFixups[LJ].Key;
        LTextSection.ReadData(LInsn, 4);
        if (LInsn and $FF000000) = $54000000 then
          PatchCondBranch(LForwardJumpFixups[LJ].Key, LForwardJumpFixups[LJ].Value)
        else if (LInsn and $FC000000) = $14000000 then
          PatchUncondBranch(LForwardJumpFixups[LJ].Key, LForwardJumpFixups[LJ].Value);
      end;
      LTextSection.Position := LTextSection.Size;
    end;

    // Patch function symbol values and sizes
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LSymtab.Position := LFuncSymIndices[LI] * ELF64_SYM_SIZE + 8;  // st_value offset
      LSymtab.WriteData(UInt64(LFuncOffsets[LI]));
      LSymtab.WriteData(UInt64(LFuncEndOffsets[LI] - LFuncOffsets[LI]));
    end;
    LSymtab.Position := LSymtab.Size;

    //==========================================================================
    // STEP 4: Build .rela.text section
    //==========================================================================
    for LI := 0 to LTextRelocs.Count - 1 do
    begin
      LReloc := LTextRelocs[LI];
      WriteRela(LReloc.Offset, LReloc.SymbolIndex, LReloc.RelocationType, LReloc.Addend);
    end;

    //==========================================================================
    // STEP 5: Calculate file layout
    //==========================================================================
    LTextSize := Cardinal(LTextSection.Size);
    LRoDataSize := Cardinal(LRoDataSection.Size);
    LSymtabSize := Cardinal(LSymtab.Size);
    LStrtabSize := Cardinal(LStrtabData.Size);
    LRelaTextSize := Cardinal(LRelaText.Size);
    LShstrtabSize := Cardinal(LShstrtabData.Size);

    // Layout: ELF header | .text | .rodata | .symtab | .strtab | .rela.text | .shstrtab | section headers
    LTextOffset := ELF64_EHDR_SIZE;
    LRoDataOffset := LTextOffset + LTextSize;
    LSymtabOffset := LRoDataOffset + LRoDataSize;
    LStrtabOffset := LSymtabOffset + LSymtabSize;
    LRelaTextOffset := LStrtabOffset + LStrtabSize;
    LShstrtabOffset := LRelaTextOffset + LRelaTextSize;
    LShdrsOffset := LShstrtabOffset + LShstrtabSize;

    //==========================================================================
    // STEP 6: Write ELF header
    //==========================================================================
    // e_ident[16]
    LResult.WriteData(ELF_MAGIC[0]);
    LResult.WriteData(ELF_MAGIC[1]);
    LResult.WriteData(ELF_MAGIC[2]);
    LResult.WriteData(ELF_MAGIC[3]);
    LResult.WriteData(Byte(ELFCLASS64));
    LResult.WriteData(Byte(ELFDATA2LSB));
    LResult.WriteData(Byte(1));  // EV_CURRENT
    LResult.WriteData(Byte(ELFOSABI_NONE));
    LResult.WriteData(UInt64(0));  // e_ident padding

    LResult.WriteData(Word(ET_REL));     // e_type
    LResult.WriteData(Word(EM_AARCH64));  // e_machine
    LResult.WriteData(Cardinal(1));      // e_version
    LResult.WriteData(UInt64(0));        // e_entry (none for .o)
    LResult.WriteData(UInt64(0));        // e_phoff (no program headers)
    LResult.WriteData(UInt64(LShdrsOffset));  // e_shoff
    LResult.WriteData(Cardinal(0));      // e_flags
    LResult.WriteData(Word(ELF64_EHDR_SIZE));  // e_ehsize
    LResult.WriteData(Word(0));          // e_phentsize
    LResult.WriteData(Word(0));          // e_phnum
    LResult.WriteData(Word(ELF64_SHDR_SIZE));  // e_shentsize
    LResult.WriteData(Word(LSectionCount));    // e_shnum
    LResult.WriteData(Word(LShShstrtab));       // e_shstrndx

    //==========================================================================
    // STEP 7: Write section data
    //==========================================================================
    // .text
    if LTextSize > 0 then
    begin
      LTextSection.Position := 0;
      LResult.CopyFrom(LTextSection, LTextSize);
    end;

    // .rodata
    if LRoDataSize > 0 then
    begin
      LRoDataSection.Position := 0;
      LResult.CopyFrom(LRoDataSection, LRoDataSize);
    end;

    // .symtab
    if LSymtabSize > 0 then
    begin
      LSymtab.Position := 0;
      LResult.CopyFrom(LSymtab, LSymtabSize);
    end;

    // .strtab
    if LStrtabSize > 0 then
    begin
      LStrtabData.Position := 0;
      LResult.CopyFrom(LStrtabData, LStrtabSize);
    end;

    // .rela.text
    if LRelaTextSize > 0 then
    begin
      LRelaText.Position := 0;
      LResult.CopyFrom(LRelaText, LRelaTextSize);
    end;

    // .shstrtab
    if LShstrtabSize > 0 then
    begin
      LShstrtabData.Position := 0;
      LResult.CopyFrom(LShstrtabData, LShstrtabSize);
    end;

    //==========================================================================
    // STEP 8: Write section headers
    //==========================================================================
    // SH[0]: null
    WriteSectionHeader(0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);

    // SH[1]: .text
    WriteSectionHeader(LShstrtabOffsets[1], SHT_PROGBITS,
      SHF_ALLOC or SHF_EXECINSTR, 0, LTextOffset, LTextSize, 0, 0, 16, 0);

    // SH[2]: .rodata
    WriteSectionHeader(LShstrtabOffsets[2], SHT_PROGBITS,
      SHF_ALLOC, 0, LRoDataOffset, LRoDataSize, 0, 0, 8, 0);

    // SH[3]: .symtab - link=strtab index, info=first global symbol
    WriteSectionHeader(LShstrtabOffsets[3], SHT_SYMTAB, 0, 0,
      LSymtabOffset, LSymtabSize, LShStrtab, LFirstGlobalSym, 8, ELF64_SYM_SIZE);

    // SH[4]: .strtab
    WriteSectionHeader(LShstrtabOffsets[4], SHT_STRTAB, 0, 0,
      LStrtabOffset, LStrtabSize, 0, 0, 1, 0);

    // SH[5]: .shstrtab
    WriteSectionHeader(LShstrtabOffsets[5], SHT_STRTAB, 0, 0,
      LShstrtabOffset, LShstrtabSize, 0, 0, 1, 0);

    // SH[6]: .rela.text - link=symtab, info=.text section index
    WriteSectionHeader(LShstrtabOffsets[6], SHT_RELA, SHF_INFO_LINK, 0,
      LRelaTextOffset, LRelaTextSize, LShSymtab, LShText, 8, ELF64_RELA_SIZE);

    // Return result
    SetLength(Result, LResult.Size);
    LResult.Position := 0;
    LResult.ReadBuffer(Result[0], LResult.Size);

    Status('ELF object generated: %d bytes, %d functions, %d symbols',
      [Length(Result), FCode.GetFuncCount(), LSymCount]);

  finally
    LTextRelocs.Free();
    LImportSymIndices.Free();
    LTryBeginLabels.Free();
    LExceptLabels.Free();
    LFinallyLabels.Free();
    LEndLabels.Free();
    LFloatTemps.Free();
    LDataPageFixups.Free();
    LForwardJumpFixups.Free();
    LCondJumpFixups.Free();
    LJumpFixups.Free();
    LRelaText.Free();
    LSymtab.Free();
    LShstrtabData.Free();
    LStrtabData.Free();
    LRoDataSection.Free();
    LTextSection.Free();
    LResult.Free();
  end;
end;

//==============================================================================
// GenerateArArchive -- Produces a Unix .a static library archive
//==============================================================================

function TTigerLinuxARM64Backend.GenerateArArchive(): TBytes;
var
  LObjData: TBytes;
  LOutput: TMemoryStream;
  LFunc: TTigerFuncInfo;
  LParamTypes: TArray<TTigerValueType>;
  LExportName: string;
  LSymbolNames: TStringList;
  LI, LJ: Integer;

  // First linker member data
  LNumSymbols: Cardinal;
  LMemberOffset: Cardinal;

  // AR header fields
  LMemberName: AnsiString;
  LMemberSize: Cardinal;
  LTimestamp: Cardinal;
  LHeaderStr: AnsiString;

  //----------------------------------------------------------------------------
  // Helper: Write a 60-byte AR member header
  //----------------------------------------------------------------------------
  procedure WriteARHeader(const AStream: TMemoryStream;
    const AName: AnsiString; const ASize: Cardinal; const ATimestamp: Cardinal);
  var
    LHdr: array[0..59] of AnsiChar;
    LSizeStr: AnsiString;
    LTSStr: AnsiString;
    LNamePadded: AnsiString;
  begin
    FillChar(LHdr, SizeOf(LHdr), ' ');

    // Name (16 bytes, padded with spaces, terminated with '/')
    if Length(AName) <= 15 then
    begin
      LNamePadded := AName + '/';
      Move(LNamePadded[1], LHdr[0], Length(LNamePadded));
    end
    else
    begin
      // Long name would need /offset into longnames member (not implemented)
      LNamePadded := AName;
      if Length(LNamePadded) > 15 then
        SetLength(LNamePadded, 15);
      LNamePadded := LNamePadded + '/';
      Move(LNamePadded[1], LHdr[0], Length(LNamePadded));
    end;

    // Timestamp (12 bytes)
    LTSStr := AnsiString(IntToStr(ATimestamp));
    Move(LTSStr[1], LHdr[16], Length(LTSStr));

    // UID (6 bytes) - leave as spaces
    // GID (6 bytes) - leave as spaces

    // Mode (8 bytes) - '100644' for regular file
    Move(AnsiString('100644')[1], LHdr[40], 6);

    // Size (10 bytes)
    LSizeStr := AnsiString(IntToStr(ASize));
    Move(LSizeStr[1], LHdr[48], Length(LSizeStr));

    // End marker
    LHdr[58] := '`';
    LHdr[59] := #10;

    AStream.WriteBuffer(LHdr[0], 60);
  end;

begin
  Result := nil;

  // First, generate the ELF .o content
  LObjData := GenerateELFObj();
  if Length(LObjData) = 0 then
  begin
    Status('Error: ELF object generation failed, cannot create .a');
    Exit;
  end;

  LOutput := TMemoryStream.Create();
  LSymbolNames := TStringList.Create();
  try
    // Collect public symbol names for the archive symbol table
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      if LFunc.IsPublic then
      begin
        if LFunc.Linkage = plC then
          LExportName := LFunc.FuncName
        else
        begin
          SetLength(LParamTypes, Length(LFunc.Params));
          for LJ := 0 to High(LFunc.Params) do
            LParamTypes[LJ] := LFunc.Params[LJ].ParamType;
          LExportName := TTigerABIMangler.MangleFunctionWithLinkage(
            LFunc.FuncName, LParamTypes, LFunc.Linkage);
        end;
        LSymbolNames.Add(LExportName);
      end;
    end;
    LNumSymbols := LSymbolNames.Count;
    LTimestamp := DateTimeToUnix(Now(), False);

    //==========================================================================
    // AR Archive Structure:
    //   Signature (8 bytes): !<arch>\n
    //   First Linker Member (symbol table, "/" member)
    //   Object Member (the ELF .o data)
    //==========================================================================

    // --- Write archive signature ---
    LOutput.WriteBuffer(AnsiString('!<arch>'#10)[1], 8);

    //==========================================================================
    // First Linker Member ("/")
    // Layout (BSD/GNU format):
    //   4 bytes: Number of symbols (big-endian!)
    //   4 bytes × N: Offsets to archive members (big-endian!)
    //   N null-terminated symbol name strings
    //==========================================================================

    // Calculate first linker member data size
    LMemberSize := 4 + (LNumSymbols * 4);  // Count + offsets array
    for LI := 0 to LSymbolNames.Count - 1 do
      LMemberSize := LMemberSize + Cardinal(Length(AnsiString(LSymbolNames[LI])) + 1);

    // The object member starts after: signature + linker header + linker data + padding
    LMemberOffset := 8 + 60 + LMemberSize;
    if (LMemberOffset mod 2) <> 0 then
      Inc(LMemberOffset);  // AR members are 2-byte aligned

    // Write first linker member header
    WriteARHeader(LOutput, '/', LMemberSize, LTimestamp);

    // Write number of symbols (big-endian)
    LOutput.WriteData(Byte((LNumSymbols shr 24) and $FF));
    LOutput.WriteData(Byte((LNumSymbols shr 16) and $FF));
    LOutput.WriteData(Byte((LNumSymbols shr 8) and $FF));
    LOutput.WriteData(Byte(LNumSymbols and $FF));

    // Write member offsets (big-endian) -- all symbols point to the single .o member
    for LI := 0 to LSymbolNames.Count - 1 do
    begin
      LOutput.WriteData(Byte((LMemberOffset shr 24) and $FF));
      LOutput.WriteData(Byte((LMemberOffset shr 16) and $FF));
      LOutput.WriteData(Byte((LMemberOffset shr 8) and $FF));
      LOutput.WriteData(Byte(LMemberOffset and $FF));
    end;

    // Write symbol name strings (null-terminated)
    for LI := 0 to LSymbolNames.Count - 1 do
    begin
      LHeaderStr := AnsiString(LSymbolNames[LI]);
      if Length(LHeaderStr) > 0 then
        LOutput.WriteBuffer(LHeaderStr[1], Length(LHeaderStr));
      LOutput.WriteData(Byte(0));
    end;

    // Pad to 2-byte alignment
    if (LOutput.Size mod 2) <> 0 then
      LOutput.WriteData(Byte(#10));

    //==========================================================================
    // Object Member (the .o file)
    //==========================================================================
    // Compute member name from output path
    LMemberName := AnsiString(ExtractFileName(ChangeFileExt(FOutputPath, '.o')));
    if Length(LMemberName) = 0 then
      LMemberName := 'output.o';

    // Write object member header
    WriteARHeader(LOutput, LMemberName, Length(LObjData), LTimestamp);

    // Write the ELF .o data
    LOutput.WriteBuffer(LObjData[0], Length(LObjData));

    // Pad to 2-byte alignment
    if (LOutput.Size mod 2) <> 0 then
      LOutput.WriteData(Byte(#10));

    // --- Return result ---
    SetLength(Result, LOutput.Size);
    LOutput.Position := 0;
    LOutput.ReadBuffer(Result[0], LOutput.Size);

    Status('AR archive generated: %d bytes, %d public symbols, %d bytes object data',
      [Length(Result), LNumSymbols, Length(LObjData)]);

  finally
    LSymbolNames.Free();
    LOutput.Free();
  end;
end;

end.
