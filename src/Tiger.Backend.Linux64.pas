{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend.Linux64;

{$I Tiger.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.DateUtils,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Utils.Win64,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.Backend.X64,
  Tiger.ABI,
  Tiger.ABI.Linux64,
  Tiger.Linker,
  Tiger.Linker.ELF;

type
  { TTigerLinux64Backend }
  TTigerLinux64Backend = class(TTigerBackend)
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
    function Run(): Cardinal; override;
    procedure Clear(); override;
  end;

implementation

//==============================================================================
// TTigerLinux64Backend
//==============================================================================

procedure TTigerLinux64Backend.EnsureOutputDir();
begin
  // Linux executables have no extension, so CreateDirInPath would treat
  // the filename as a directory. Temporarily append .elf to trick it
  // into creating only the parent directories.
  if not TPath.HasExtension(FOutputPath) then
    TUtils.CreateDirInPath(FOutputPath + '.elf')
  else
    TUtils.CreateDirInPath(FOutputPath);
end;

function TTigerLinux64Backend.TargetExe(const APath: string;
  const ASubsystem: TTigerSubsystem): TTigerBackend;
begin
  // Strip any extension -- Linux executables have none by convention
  FOutputPath := ChangeFileExt(APath, '');
  FOutputType := otExe;
  FSubsystem := ASubsystem;
  Result := Self;
end;

procedure TTigerLinux64Backend.PreBuild();
begin
  EnsureOutputDir();
end;

function TTigerLinux64Backend.BuildToMemory(): TBytes;
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

function TTigerLinux64Backend.Run(): Cardinal;
begin
  if FOutputType <> otExe then
    Exit(ERROR_BAD_FORMAT);

  try
    Result := TWin64Utils.RunElf(FOutputPath, ExtractFilePath(FOutputPath));
  except
    on E: Exception do
    begin
      Status('Run failed: %s', [E.Message]);
      Result := ERROR_FILE_NOT_FOUND;
    end;
  end;
end;

procedure TTigerLinux64Backend.Clear();
begin
  inherited Clear();
end;

//==============================================================================
// GenerateELF -- Produces a minimal ELF64 executable
//
// Layout: [ELF header 64B] [PHDR 56B] [.rodata] [.data] [.text + _start]
// Single PT_LOAD segment with PF_R|PF_W|PF_X (Phase 1 simplicity).
//==============================================================================

function TTigerLinux64Backend.GenerateELF(): TBytes;
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
  EM_X86_64     = 62;

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
  R_X86_64_JUMP_SLOT = 7;

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
  LLocalsSize: Integer;
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
  LExceptFrameBaseOffset: Integer;
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
  LScopeIdx: Integer;
  LFrameOffset: Integer;
  LExceptLabelIdx: Integer;
  LFinallyLabelIdx: Integer;

  //--------------------------------------------------------------------------
  // Emit helpers (write to LTextSection)
  //--------------------------------------------------------------------------
  procedure EmitByte(const AValue: Byte);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitWord(const AValue: Word);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitDWord(const AValue: Cardinal);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitQWord(const AValue: UInt64);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitInt32(const AValue: Int32);
  begin
    LTextSection.WriteData(AValue);
  end;

  //--------------------------------------------------------------------------
  // x86-64 instruction emitters
  //--------------------------------------------------------------------------
  procedure EmitRex(const AW, AR, AX, AB: Boolean);
  var
    LVal: Byte;
  begin
    LVal := $40;
    if AW then LVal := LVal or $08;
    if AR then LVal := LVal or $04;
    if AX then LVal := LVal or $02;
    if AB then LVal := LVal or $01;
    EmitByte(LVal);
  end;

  procedure EmitModRM(const AMod, AReg, ARM: Byte);
  begin
    EmitByte((AMod shl 6) or ((AReg and 7) shl 3) or (ARM and 7));
  end;

  procedure EmitPushReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitByte($41);
    EmitByte($50 + (AReg and 7));
  end;

  procedure EmitPopReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitByte($41);
    EmitByte($58 + (AReg and 7));
  end;

  procedure EmitMovRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($89);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitSubRspImm32(const AImm: Int32);
  begin
    EmitRex(True, False, False, False);
    EmitByte($81);
    EmitModRM(3, 5, REG_RSP);
    EmitInt32(AImm);
  end;

  procedure EmitAddRspImm32(const AImm: Int32);
  begin
    EmitRex(True, False, False, False);
    EmitByte($81);
    EmitModRM(3, 0, REG_RSP);
    EmitInt32(AImm);
  end;

  procedure EmitMovRegImm64(const AReg: Byte; const AImm: UInt64);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($B8 + (AReg and 7));
    EmitQWord(AImm);
  end;

  procedure EmitMovRegImm32(const AReg: Byte; const AImm: Cardinal);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($B8 + (AReg and 7));
    EmitDWord(AImm);
  end;

  procedure EmitCallRel32(const ADisp: Int32);
  begin
    EmitByte($E8);
    EmitInt32(ADisp);
  end;

  procedure EmitRet();
  begin
    EmitByte($C3);
  end;

  procedure EmitMovRbpDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($89);
    EmitModRM(2, AReg, REG_RBP);
    EmitInt32(ADisp);
  end;

  procedure EmitMovRegRbpDisp(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8B);
    EmitModRM(2, AReg, REG_RBP);
    EmitInt32(ADisp);
  end;

  procedure EmitMovRspDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($89);
    EmitModRM(2, AReg and 7, 4);  // rm=100 → SIB follows
    EmitByte($24);                 // SIB: base=RSP, index=none
    EmitInt32(ADisp);
  end;

  procedure EmitXorRegReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, AReg >= 8, False, AReg >= 8);
    EmitByte($31);
    EmitModRM(3, AReg, AReg);
  end;

  procedure EmitLeaRipRel(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8D);
    EmitModRM(0, AReg and 7, 5);  // mod=00, rm=101 → [RIP+disp32]
    EmitInt32(ADisp);
  end;

  procedure EmitLeaRegRbpDisp(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8D);
    EmitModRM(2, AReg and 7, REG_RBP);
    EmitInt32(ADisp);
  end;

  // Arithmetic helpers
  procedure EmitAddRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($01);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitSubRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($29);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitImulRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ADest >= 8, False, ASrc >= 8);
    EmitByte($0F);
    EmitByte($AF);
    EmitModRM(3, ADest, ASrc);
  end;

  procedure EmitCqo();
  begin
    EmitRex(True, False, False, False);
    EmitByte($99);
  end;

  procedure EmitIdivReg(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($F7);
    EmitModRM(3, 7, AReg);
  end;

  procedure EmitAndRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($21);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitOrRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($09);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitXorRegReg64(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($31);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitNotReg(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($F7);
    EmitModRM(3, 2, AReg);
  end;

  procedure EmitShlRegCl(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($D3);
    EmitModRM(3, 4, AReg);
  end;

  procedure EmitShrRegCl(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($D3);
    EmitModRM(3, 5, AReg);
  end;

  procedure EmitRepMovsb();
  begin
    EmitByte($F3);  // REP prefix
    EmitByte($A4);  // MOVSB
  end;

  // Comparison helpers
  procedure EmitCmpRegReg(const AReg1, AReg2: Byte);
  begin
    EmitRex(True, AReg2 >= 8, False, AReg1 >= 8);
    EmitByte($39);
    EmitModRM(3, AReg2, AReg1);
  end;

  procedure EmitSete();
  begin
    EmitByte($0F); EmitByte($94);
    EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetne();
  begin
    EmitByte($0F); EmitByte($95);
    EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetl();
  begin
    EmitByte($0F); EmitByte($9C);
    EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetle();
  begin
    EmitByte($0F); EmitByte($9E);
    EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetg();
  begin
    EmitByte($0F); EmitByte($9F);
    EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetge();
  begin
    EmitByte($0F); EmitByte($9D);
    EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitMovzxEaxAl();
  begin
    EmitByte($0F);
    EmitByte($B6);
    EmitModRM(3, REG_RAX, REG_RAX);
  end;

  // Branch helpers
  procedure EmitJmpRel32(const ADisp: Int32);
  begin
    EmitByte($E9);
    EmitInt32(ADisp);
  end;

  procedure EmitJzRel32(const ADisp: Int32);
  begin
    EmitByte($0F);
    EmitByte($84);
    EmitInt32(ADisp);
  end;

  procedure EmitJnzRel32(const ADisp: Int32);
  begin
    EmitByte($0F);
    EmitByte($85);
    EmitInt32(ADisp);
  end;

  // Variadic function support helpers
  procedure EmitAddRaxImm32(const AValue: Int32);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($05);  // ADD RAX, imm32
    EmitInt32(AValue);
  end;

  procedure EmitShlRaxImm8(const AValue: Byte);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($C1);  // SHL r/m64, imm8
    EmitModRM(3, 4, 0);  // /4 = SHL, RAX
    EmitByte(AValue);
  end;

  procedure EmitNegRax();
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($F7);  // NEG r/m64
    EmitModRM(3, 3, 0);  // /3 = NEG, RAX
  end;

  procedure EmitMovRegRbpPlusRax(const ADest: Byte);
  begin
    EmitRex(True, ADest >= 8, False, False);
    EmitByte($8B);  // MOV r64, r/m64
    EmitByte($44);  // ModR/M: [base + index + disp8]
    EmitByte($05);  // SIB: scale=1, index=RAX, base=RBP
    EmitByte($00);  // disp8 = 0
  end;

  procedure EmitCmpRaxImm32(const AValue: Int32);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($3D);  // CMP RAX, imm32
    EmitInt32(AValue);
  end;

  procedure EmitJgeRel8(const ADisp: Int8);
  begin
    EmitByte($7D);  // JGE rel8
    EmitByte(Byte(ADisp));
  end;

  procedure EmitJmpRel8(const ADisp: Int8);
  begin
    EmitByte($EB);  // JMP rel8
    EmitByte(Byte(ADisp));
  end;

  procedure EmitTestRegReg(const AReg1, AReg2: Byte);
  begin
    EmitRex(True, AReg2 >= 8, False, AReg1 >= 8);
    EmitByte($85);
    EmitModRM(3, AReg2, AReg1);
  end;

  procedure EmitCallReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($FF);
    EmitModRM(3, 2, AReg and 7);
  end;

  // Pointer helpers
  procedure EmitMovRegMemReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ADest >= 8, False, ASrc >= 8);
    EmitByte($8B);
    EmitModRM(0, ADest and 7, ASrc and 7);
  end;

  procedure EmitMovMemRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($89);
    EmitModRM(0, ASrc and 7, ADest and 7);
  end;

  procedure AlignStream(const AStream: TMemoryStream; const AAlign: Cardinal);
  var
    LPad: Cardinal;
  begin
    if AStream.Size mod AAlign <> 0 then
    begin
      LPad := AAlign - (AStream.Size mod AAlign);
      while LPad > 0 do
      begin
        AStream.WriteData(Byte(0));
        Dec(LPad);
      end;
    end;
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
  begin
    AStream.WriteData(ANameIdx);   // sh_name
    AStream.WriteData(AType);      // sh_type
    AStream.WriteData(AFlags);     // sh_flags
    AStream.WriteData(AAddr);      // sh_addr
    AStream.WriteData(AOffset);    // sh_offset
    AStream.WriteData(ASize);      // sh_size
    AStream.WriteData(ALink);      // sh_link
    AStream.WriteData(AInfo);      // sh_info
    AStream.WriteData(AAddrAlign); // sh_addralign
    AStream.WriteData(AEntSize);   // sh_entsize
  end;

  //--------------------------------------------------------------------------
  // Stack frame helpers (System V AMD64 -- no shadow space)
  //
  // Layout from RBP downward:
  //   [RBP-8..RBP-params*8]     saved params
  //   [after params..]           locals
  //   [after locals..]           temps
  //   [RSP..RSP+outgoing-1]     outgoing stack args (for calls with >6 args)
  //--------------------------------------------------------------------------
  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    // Non-variadic: [RBP-8] = param0, [RBP-16] = param1, etc.
    // Variadic: [RBP-8] = hidden count, [RBP-16] = param0, etc.
    if LFunc.IsVariadic then
      Result := -(8 + (AIndex + 1) * 8)
    else
      Result := -(8 + AIndex * 8);
  end;

  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LOffset: Integer;
    LK: Integer;
  begin
    // Locals follow params
    // For variadic functions, always reserve space for all 6 register args
    // (hidden count + up to 5 declared params/varargs)
    if LFunc.IsVariadic then
      LOffset := 8 + 6 * 8  // Always reserve 6 slots for variadic
    else
      LOffset := 8 + Length(LFunc.Params) * 8;
    for LK := 0 to AIndex - 1 do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -LOffset;
  end;

  function GetTempOffset(const AIndex: Integer): Int32;
  var
    LOffset: Integer;
    LK: Integer;
  begin
    // Temps follow params and locals
    // For variadic functions, always reserve space for all 6 register args
    if LFunc.IsVariadic then
      LOffset := 8 + 6 * 8  // Always reserve 6 slots for variadic
    else
      LOffset := 8 + Length(LFunc.Params) * 8;
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    LOffset := LOffset + AIndex * 8;
    Result := -LOffset;
  end;

  procedure StoreTempFromReg(const ATempIndex: Integer; const AReg: Byte);
  begin
    EmitMovRbpDispReg(GetTempOffset(ATempIndex), AReg);
  end;

  //--------------------------------------------------------------------------
  // Operand loading -- handles all TTigerOperandKind values
  //--------------------------------------------------------------------------
  procedure LoadOperandToReg(const AOp: TTigerOperand; const AReg: Byte);
  begin
    case AOp.Kind of
      okImmediate:
        begin
          if (AOp.ImmInt >= 0) and (AOp.ImmInt <= $FFFFFFFF) then
            EmitMovRegImm32(AReg, Cardinal(AOp.ImmInt))
          else
            EmitMovRegImm64(AReg, UInt64(AOp.ImmInt));
        end;

      okData:
        begin
          // LEA reg, [RIP+disp32] -- fixup to .rodata
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(
            LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder
        end;

      okGlobal:
        begin
          // LEA reg, [RIP+disp32] -- fixup to .data
          LGlobalFixups.Add(TPair<Cardinal, Integer>.Create(
            LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder
        end;

      okLocal:
        begin
          if AOp.LocalHandle.IsParam then
            EmitMovRegRbpDisp(AReg, GetParamOffset(AOp.LocalHandle.Index))
          else
            EmitMovRegRbpDisp(AReg, GetLocalOffset(AOp.LocalHandle.Index));
        end;

      okTemp:
        begin
          EmitMovRegRbpDisp(AReg, GetTempOffset(AOp.TempHandle.Index));
        end;

      okFunc:
        begin
          // LEA reg, [RIP+disp32] -- fixup to function address
          LFuncAddrFixups.Add(TPair<Cardinal, Integer>.Create(
            LTextSection.Size, AOp.FuncHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder
        end;
    else
      EmitXorRegReg(AReg);
    end;
  end;

  //--------------------------------------------------------------------------
  // Store operand to stack for outgoing call args (>6 register args)
  //--------------------------------------------------------------------------
  procedure StoreOperandToStackArg(const AOp: TTigerOperand; const AArgIndex: Integer);
  var
    LStackOffset: Int32;
  begin
    // System V: no shadow space, stack args start at RSP+0
    LStackOffset := (AArgIndex - LINUX64_MAX_REG_ARGS) * 8;
    LoadOperandToReg(AOp, REG_RAX);
    EmitMovRspDispReg(LStackOffset, REG_RAX);
  end;

  //--------------------------------------------------------------------------
  // Helper: Load call argument to register, handling large struct params
  // System V ABI: Large structs (>16 bytes) are passed by pointer
  //--------------------------------------------------------------------------
  procedure LoadCallArgToReg(const AOp: TTigerOperand; const AReg: Byte;
    const ATargetFuncIndex: Integer; const AArgIndex: Integer);
  var
    LTargetFunc: TTigerFuncInfo;
    LNeedsAddress: Boolean;
  begin
    LNeedsAddress := False;
    
    // Check if target param expects a large struct (passed by pointer)
    if ATargetFuncIndex >= 0 then
    begin
      LTargetFunc := FCode.GetFunc(ATargetFuncIndex);
      if (AArgIndex >= 0) and (AArgIndex < Length(LTargetFunc.Params)) then
      begin
        // Large struct param (>16 bytes): callee expects a pointer
        if LTargetFunc.Params[AArgIndex].ParamSize > 16 then
        begin
          // Source is a local variable (not a param): pass its address
          if (AOp.Kind = okLocal) and (not AOp.LocalHandle.IsParam) then
            LNeedsAddress := True;
          // Source is a param that was already large: slot contains pointer, MOV is correct
        end;
      end;
    end;
    
    if LNeedsAddress then
    begin
      // LEA reg, [RBP-offset] to get address of local struct
      EmitLeaRegRbpDisp(AReg, GetLocalOffset(AOp.LocalHandle.Index));
    end
    else
      LoadOperandToReg(AOp, AReg);
  end;

  //--------------------------------------------------------------------------
  // Helper: Store call argument to stack slot, handling large struct params
  // System V ABI: Large structs (>16 bytes) are passed by pointer
  //--------------------------------------------------------------------------
  procedure StoreCallArgToStack(const AOp: TTigerOperand; const AArgIndex: Integer;
    const ATargetFuncIndex: Integer);
  var
    LStackOffset: Int32;
    LTargetFunc: TTigerFuncInfo;
    LNeedsAddress: Boolean;
  begin
    LStackOffset := (AArgIndex - LINUX64_MAX_REG_ARGS) * 8;
    LNeedsAddress := False;
    
    // Check if target param expects a large struct (passed by pointer)
    if ATargetFuncIndex >= 0 then
    begin
      LTargetFunc := FCode.GetFunc(ATargetFuncIndex);
      if (AArgIndex >= 0) and (AArgIndex < Length(LTargetFunc.Params)) then
      begin
        // Large struct param (>16 bytes): callee expects a pointer
        if LTargetFunc.Params[AArgIndex].ParamSize > 16 then
        begin
          // Source is a local variable (not a param): pass its address
          if (AOp.Kind = okLocal) and (not AOp.LocalHandle.IsParam) then
            LNeedsAddress := True;
        end;
      end;
    end;
    
    if LNeedsAddress then
    begin
      // LEA RAX, [RBP-offset] to get address of local struct, then store to stack
      EmitLeaRegRbpDisp(REG_RAX, GetLocalOffset(AOp.LocalHandle.Index));
      EmitMovRspDispReg(LStackOffset, REG_RAX);
    end
    else
      StoreOperandToStackArg(AOp, AArgIndex);
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
  LLinker := nil;
  LHasStaticImports := False;
  LHasSEH := False;
  LPushExceptFrameIdx := -1;
  LPopExceptFrameIdx := -1;
  LGetExceptFrameIdx := -1;
  LSigsetjmpIdx := -1;
  LInitExceptionsIdx := -1;
  LInitSignalsIdx := -1;

  try
    LHasImports := FImports.GetCount() > 0;
    LImportCount := FImports.GetCount();
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
        LInitSignalsIdx := LI;
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
        LInterpSection.WriteBuffer(AnsiString('/lib64/ld-linux-x86-64.so.2'#0)[1], 28);
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
        LEntry := FImports.GetEntryByIndex(LI);
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

      // PLT: 16 bytes for PLT[0] + 16 bytes per import
      LPltFileOffset := LDataFileOffset + Cardinal(LDataSection.Size);
      // Align .plt to 16
      if LPltFileOffset mod 16 <> 0 then
        LPltFileOffset := LPltFileOffset + (16 - LPltFileOffset mod 16);

      LTextFileOffset := LPltFileOffset + Cardinal(16 + LImportCount * 16);
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
    // STEP 4: Generate code for each function
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

      // Initialize label offsets
      SetLength(LLabelOffsets, Length(LFunc.Labels));
      for LJ := 0 to High(LLabelOffsets) do
        LLabelOffsets[LJ] := 0;

      //----------------------------------------------------------------------
      // Calculate stack frame (System V: no shadow space)
      //----------------------------------------------------------------------
      LLocalsSize := 0;
      for LJ := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + LFunc.Locals[LJ].LocalSize;

      // Calculate exception frame space (216 bytes per scope)
      LExceptFrameSize := Length(LFunc.ExceptionScopes) * 216;
      // Base offset for exception frames: after params, locals, temps
      // Will be adjusted after final stack size is known

      // Build label lookup tables for exception handling
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
      begin
        if LFunc.Instructions[LJ].Kind in [ikCallImport, ikCall,
                                            ikCallIndirect, ikSyscall] then
        begin
          if Length(LFunc.Instructions[LJ].Args) > LMaxCallArgs then
            LMaxCallArgs := Length(LFunc.Instructions[LJ].Args);
        end;
      end;

      // Stack args only needed for calls with >6 register args
      if LMaxCallArgs > LINUX64_MAX_REG_ARGS then
        LOutgoingArgSpace := Cardinal(LMaxCallArgs - LINUX64_MAX_REG_ARGS) * 8
      else
        LOutgoingArgSpace := 0;

      LStackFrameSize := Cardinal(8) +                          // alignment base
                          Cardinal(Length(LFunc.Params) * 8) +
                          Cardinal(LLocalsSize) +
                          Cardinal(LFunc.TempCount * 8) +
                          LOutgoingArgSpace +
                          Cardinal(LExceptFrameSize);            // exception frames
      // Align to 16 bytes
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

      // Calculate base offset for exception frames (from RBP, negative)
      // Layout: [params][locals][temps][outgoing args][except frames]
      LExceptFrameBaseOffset := Integer(8 + Length(LFunc.Params) * 8 +
                                  LLocalsSize + LFunc.TempCount * 8 +
                                  LOutgoingArgSpace);

      //----------------------------------------------------------------------
      // Function prologue: PUSH RBP; MOV RBP, RSP; SUB RSP, N
      //----------------------------------------------------------------------
      EmitPushReg(REG_RBP);
      EmitMovRegReg(REG_RBP, REG_RSP);
      if LStackFrameSize > 0 then
        EmitSubRspImm32(LStackFrameSize);

      // Save incoming parameters (System V: RDI, RSI, RDX, RCX, R8, R9)
      // For variadic functions: RDI = hidden count, then declared params
      // For large struct returns (>16 bytes): RDI = hidden return ptr, params shift
      if LFunc.IsVariadic then
      begin
        // Save ALL 6 registers for variadic functions
        // RDI = hidden count, RSI/RDX/RCX/R8/R9 could be declared params or varargs
        // We must save them all so VaArgAt can access varargs uniformly
        EmitMovRbpDispReg(-8, REG_RDI);    // Hidden count at position 0
        EmitMovRbpDispReg(-16, REG_RSI);   // Position 1 (param 0 or vararg 0)
        EmitMovRbpDispReg(-24, REG_RDX);   // Position 2 (param 1 or vararg 1)
        EmitMovRbpDispReg(-32, REG_RCX);   // Position 3 (param 2 or vararg 2)
        EmitMovRbpDispReg(-40, REG_R8);    // Position 4 (param 3 or vararg 3)
        EmitMovRbpDispReg(-48, REG_R9);    // Position 5 (param 4 or vararg 4)
        // Note: position 6+ come from caller's stack, no register save needed
      end
      else if LFunc.ReturnSize > 16 then
      begin
        // Large struct return: RDI = hidden return pointer (shifts all params)
        // Save hidden return pointer to [RBP-32]
        EmitMovRbpDispReg(-32, REG_RDI);
        // Params are shifted: RSI=param0, RDX=param1, RCX=param2, R8=param3, R9=param4
        if Length(LFunc.Params) > 0 then
          EmitMovRbpDispReg(GetParamOffset(0), REG_RSI);
        if Length(LFunc.Params) > 1 then
          EmitMovRbpDispReg(GetParamOffset(1), REG_RDX);
        if Length(LFunc.Params) > 2 then
          EmitMovRbpDispReg(GetParamOffset(2), REG_RCX);
        if Length(LFunc.Params) > 3 then
          EmitMovRbpDispReg(GetParamOffset(3), REG_R8);
        if Length(LFunc.Params) > 4 then
          EmitMovRbpDispReg(GetParamOffset(4), REG_R9);
        // Param 5+ come from caller's stack, no register save needed
      end
      else
      begin
        if Length(LFunc.Params) > 0 then
          EmitMovRbpDispReg(GetParamOffset(0), REG_RDI);
        if Length(LFunc.Params) > 1 then
          EmitMovRbpDispReg(GetParamOffset(1), REG_RSI);
        if Length(LFunc.Params) > 2 then
          EmitMovRbpDispReg(GetParamOffset(2), REG_RDX);
        if Length(LFunc.Params) > 3 then
          EmitMovRbpDispReg(GetParamOffset(3), REG_RCX);
        if Length(LFunc.Params) > 4 then
          EmitMovRbpDispReg(GetParamOffset(4), REG_R8);
        if Length(LFunc.Params) > 5 then
          EmitMovRbpDispReg(GetParamOffset(5), REG_R9);
      end;

      //----------------------------------------------------------------------
      // Process each instruction
      //----------------------------------------------------------------------
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LJ];

        case LInstr.Kind of
          ikLabel:
            begin
              if LInstr.LabelTarget.IsValid() then
              begin
                LLabelOffsets[LInstr.LabelTarget.Index] := LTextSection.Size;

                //--------------------------------------------------------------
                // Exception handling: TryBeginLabel setup
                //--------------------------------------------------------------
                if LTryBeginLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
                begin
                  // Validate required functions/imports are available
                  if LPushExceptFrameIdx < 0 then
                    raise Exception.Create('Tiger_PushExceptFrame not found - exception handling runtime not linked');
                  if LSigsetjmpIdx < 0 then
                    raise Exception.Create('__sigsetjmp not imported - exception handling runtime not linked');

                  // Calculate frame offset for this scope
                  LFrameOffset := LExceptFrameBaseOffset + (LScopeIdx * 216);

                  // LEA RDI, [RBP - frame_offset] (frame pointer)
                  EmitLeaRegRbpDisp(REG_RDI, -LFrameOffset);

                  // CALL Tiger_PushExceptFrame
                  LCallFixups.Add(TPair<Cardinal, Integer>.Create(
                    LTextSection.Size + 1, LPushExceptFrameIdx));
                  EmitCallRel32(0);

                  // LEA RDI, [RBP - frame_offset + 8] (jmpbuf = frame + 8)
                  EmitLeaRegRbpDisp(REG_RDI, -(LFrameOffset - 8));

                  // XOR ESI, ESI (savesigs = 0)
                  EmitXorRegReg(REG_RSI);

                  // XOR EAX, EAX (varargs indicator)
                  EmitXorRegReg(REG_RAX);

                  // CALL __sigsetjmp (via PLT)
                  LPltFixups.Add(TPair<Cardinal, Integer>.Create(
                    LTextSection.Size + 1, LSigsetjmpIdx));
                  EmitCallRel32(0);

                  // TEST EAX, EAX
                  EmitTestRegReg(REG_RAX, REG_RAX);

                  // Determine target: except block if present, else finally
                  if LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.IsValid() then
                    LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.Index
                  else if LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.IsValid() then
                    LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.Index
                  else
                    LExceptLabelIdx := -1;

                  // JNZ target_label (jump if exception occurred)
                  if LExceptLabelIdx >= 0 then
                  begin
                    LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
                      LTextSection.Size + 2, LExceptLabelIdx));
                    EmitJnzRel32(0);
                  end;
                end;

                //--------------------------------------------------------------
                // Exception handling: EndLabel cleanup
                //--------------------------------------------------------------
                if LEndLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
                begin
                  // Validate required function is available
                  if LPopExceptFrameIdx < 0 then
                    raise Exception.Create('Tiger_PopExceptFrame not found - exception handling runtime not linked');

                  // CALL Tiger_PopExceptFrame
                  LCallFixups.Add(TPair<Cardinal, Integer>.Create(
                    LTextSection.Size + 1, LPopExceptFrameIdx));
                  EmitCallRel32(0);
                end;
              end;
            end;

          ikNop:
            EmitByte($90);

          ikStore:
            begin
              // Store value (Op2) to local/param (Op1)
              LoadOperandToReg(LInstr.Op2, REG_RAX);
              if LInstr.Op1.LocalHandle.IsParam then
                EmitMovRbpDispReg(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_RAX)
              else
                EmitMovRbpDispReg(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_RAX);
            end;

          ikLoad:
            begin
              // Load operand (Op1) to temp (Dest)
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikAddressOf:
            begin
              // Dest = address of local/param slot (always LEA)
              // Op1 = local handle
              // Note: For by-ref params (structs >16 bytes), the caller should emit
              // a subsequent load instruction to get the actual struct pointer.
              if LInstr.Op1.LocalHandle.IsParam then
                EmitLeaRegRbpDisp(REG_RAX, GetParamOffset(LInstr.Op1.LocalHandle.Index))
              else
                EmitLeaRegRbpDisp(REG_RAX, GetLocalOffset(LInstr.Op1.LocalHandle.Index));
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikLoadPtr:
            begin
              // Load value at pointer: dest = [Op1]
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitMovRegMemReg(REG_RAX, REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikStorePtr:
            begin
              // Store Op2 to address in Op1: [Op1] = Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitMovMemRegReg(REG_RAX, REG_RCX);
            end;

          //------------------------------------------------------------------
          // Arithmetic
          //------------------------------------------------------------------
          ikAdd:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitAddRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikSub:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitSubRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikMul:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitImulRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikDiv:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCqo();
              EmitIdivReg(REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikMod:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCqo();
              EmitIdivReg(REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RDX);
            end;

          ikBitAnd:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitAndRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikBitOr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitOrRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikBitXor:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitXorRegReg64(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikBitNot:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitNotReg(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikShl:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitShlRegCl(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikShr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitShrRegCl(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          //------------------------------------------------------------------
          // Comparisons
          //------------------------------------------------------------------
          ikCmpEq:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSete();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpNe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetne();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpLt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetl();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpLe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetle();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpGt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetg();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpGe:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetge();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          //------------------------------------------------------------------
          // Branches
          //------------------------------------------------------------------
          ikJump:
            begin
              // JMP rel32
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
                LTextSection.Size + 1, LInstr.LabelTarget.Index));
              EmitJmpRel32(0);
            end;

          ikJumpIf:
            begin
              // Jump if condition is true (non-zero) → JNZ
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitTestRegReg(REG_RAX, REG_RAX);
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
                LTextSection.Size + 2, LInstr.LabelTarget.Index));
              EmitJnzRel32(0);
            end;

          ikJumpIfNot:
            begin
              // Jump if condition is false (zero) → JZ
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitTestRegReg(REG_RAX, REG_RAX);
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
                LTextSection.Size + 2, LInstr.LabelTarget.Index));
              EmitJzRel32(0);
            end;

          //------------------------------------------------------------------
          // Calls (System V: RDI, RSI, RDX, RCX, R8, R9)
          //------------------------------------------------------------------
          ikCall:
            begin
              // Store stack args first (>6) with struct handling
              for LK := LINUX64_MAX_REG_ARGS to High(LInstr.Args) do
                StoreCallArgToStack(LInstr.Args[LK], LK, LInstr.FuncTarget.Index);

              // Load register args (with struct handling)
              if Length(LInstr.Args) > 0 then
                LoadCallArgToReg(LInstr.Args[0], REG_RDI, LInstr.FuncTarget.Index, 0);
              if Length(LInstr.Args) > 1 then
                LoadCallArgToReg(LInstr.Args[1], REG_RSI, LInstr.FuncTarget.Index, 1);
              if Length(LInstr.Args) > 2 then
                LoadCallArgToReg(LInstr.Args[2], REG_RDX, LInstr.FuncTarget.Index, 2);
              if Length(LInstr.Args) > 3 then
                LoadCallArgToReg(LInstr.Args[3], REG_RCX, LInstr.FuncTarget.Index, 3);
              if Length(LInstr.Args) > 4 then
                LoadCallArgToReg(LInstr.Args[4], REG_R8, LInstr.FuncTarget.Index, 4);
              if Length(LInstr.Args) > 5 then
                LoadCallArgToReg(LInstr.Args[5], REG_R9, LInstr.FuncTarget.Index, 5);

              // CALL rel32 -- fixup to target function
              LCallFixups.Add(TPair<Cardinal, Integer>.Create(
                LTextSection.Size + 1, LInstr.FuncTarget.Index));
              EmitCallRel32(0);

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCallIndirect:
            begin
              // Store stack args first (>6)
              for LK := LINUX64_MAX_REG_ARGS to High(LInstr.Args) do
                StoreOperandToStackArg(LInstr.Args[LK], LK);

              // Load register args (reverse order to avoid clobbering)
              if Length(LInstr.Args) > 5 then
                LoadOperandToReg(LInstr.Args[5], REG_R9);
              if Length(LInstr.Args) > 4 then
                LoadOperandToReg(LInstr.Args[4], REG_R8);
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_RCX);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_RDX);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RSI);
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RDI);

              // Load function pointer into RAX, then CALL RAX
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitCallReg(REG_RAX);

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCallImport:
            begin
              // System V ABI: first 6 integer args in RDI, RSI, RDX, RCX, R8, R9
              // Store stack args first (>6)
              for LK := LINUX64_MAX_REG_ARGS to High(LInstr.Args) do
                StoreOperandToStackArg(LInstr.Args[LK], LK);

              // Load register args
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RDI);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RSI);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_RDX);
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_RCX);
              if Length(LInstr.Args) > 4 then
                LoadOperandToReg(LInstr.Args[4], REG_R8);
              if Length(LInstr.Args) > 5 then
                LoadOperandToReg(LInstr.Args[5], REG_R9);

              // System V varargs: AL = number of SSE register args (0 for integer-only)
              EmitXorRegReg(REG_RAX);

              // CALL rel32 to PLT entry -- record fixup
              LPltFixups.Add(TPair<Cardinal, Integer>.Create(
                LTextSection.Size + 1, LInstr.ImportTarget.Index));
              EmitCallRel32(0);  // Placeholder

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          //------------------------------------------------------------------
          // Syscall (Linux: RDI, RSI, RDX, R10, R8, R9; nr in RAX)
          // Note: R10 replaces RCX (clobbered by syscall instruction)
          //------------------------------------------------------------------
          ikSyscall:
            begin
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RDI);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RSI);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_RDX);
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_R10);
              if Length(LInstr.Args) > 4 then
                LoadOperandToReg(LInstr.Args[4], REG_R8);
              if Length(LInstr.Args) > 5 then
                LoadOperandToReg(LInstr.Args[5], REG_R9);

              // Load syscall number into RAX
              EmitMovRegImm64(REG_RAX, UInt64(LInstr.SyscallNr));
              // SYSCALL
              EmitByte($0F);
              EmitByte($05);

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          //------------------------------------------------------------------
          // Variadic function support
          //------------------------------------------------------------------
          ikVaCount:
            begin
              // Load hidden vararg count from [RBP-8] (always at fixed position)
              EmitMovRegRbpDisp(REG_RAX, -8);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikVaArgAt:
            begin
              // Load vararg at index from stack
              // Layout: [RBP-8] = hidden count
              //         [RBP-16] = declared param 0 (if any)
              //         ...
              //         [RBP-(8 + (NumParams+1)*8)] = first vararg (index 0)
              // For args >= 6 total position: caller's stack at [RBP+16+pos*8]
              //
              // Calculate actual position = NumParams + 1 + index
              // If position < 6: use [RBP - (8 + position * 8)]
              // If position >= 6: use [RBP + 16 + (position - 6) * 8]
              
              // Load index into RAX
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              
              // Add (NumParams + 1) to get actual position
              // NumParams = Length(LFunc.Params) (declared params, not counting hidden count)
              EmitAddRaxImm32(Length(LFunc.Params) + 1);
              
              // Now RAX = actual position
              // Compare with 6 to determine which stack region
              EmitCmpRaxImm32(6);
              
              // Save position in R10 for later use
              EmitMovRegReg(REG_R10, REG_RAX);
              
              // JGE rel8 to stack args path (jump +N bytes forward)
              // Register arg path code size: ~20 bytes
              EmitJgeRel8(20);
              
              // === Register arg path: [RBP - (8 + pos * 8)] ===
              // RAX still has position
              EmitShlRaxImm8(3);          // RAX = pos * 8
              EmitAddRaxImm32(8);         // RAX = 8 + pos * 8
              EmitNegRax();               // RAX = -(8 + pos * 8)
              EmitMovRegRbpPlusRax(REG_RAX);  // RAX = [RBP + RAX]
              EmitJmpRel8(18);            // Jump over stack arg path (18 bytes)
              
              // === Stack arg path: [RBP + 16 + (pos - 6) * 8] ===
              // Position 6 → [RBP+16], Position 7 → [RBP+24], etc.
              // Equivalent: [RBP + pos * 8 - 32]
              // Restore position from R10
              EmitMovRegReg(REG_RAX, REG_R10);
              EmitShlRaxImm8(3);          // RAX = pos * 8
              EmitAddRaxImm32(-32);       // RAX = pos * 8 - 32
              EmitMovRegRbpPlusRax(REG_RAX);  // RAX = [RBP + RAX]
              
              // Store result
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          //------------------------------------------------------------------
          // Return
          //------------------------------------------------------------------
          ikReturn:
            begin
              EmitMovRegReg(REG_RSP, REG_RBP);
              EmitPopReg(REG_RBP);
              EmitRet();
            end;

          ikReturnValue:
            begin
              if LFunc.ReturnSize > 16 then
              begin
                // Large struct return: copy to hidden return pointer
                // RSI = source address (the local struct)
                if LInstr.Op1.Kind = okLocal then
                begin
                  if LInstr.Op1.LocalHandle.IsParam then
                    EmitLeaRegRbpDisp(REG_RSI, GetParamOffset(LInstr.Op1.LocalHandle.Index))
                  else
                    EmitLeaRegRbpDisp(REG_RSI, GetLocalOffset(LInstr.Op1.LocalHandle.Index));
                end
                else
                  LoadOperandToReg(LInstr.Op1, REG_RSI);  // Fallback: assume it's a pointer
                // RDI = hidden return pointer from [RBP-32]
                EmitMovRegRbpDisp(REG_RDI, -32);
                // RCX = byte count
                EmitMovRegImm32(REG_RCX, Cardinal(LFunc.ReturnSize));
                // Copy bytes
                EmitRepMovsb();
                // Return the hidden pointer in RAX
                EmitMovRegRbpDisp(REG_RAX, -32);
              end
              else
              begin
                // Small return value: load directly into RAX
                LoadOperandToReg(LInstr.Op1, REG_RAX);
              end;
              EmitMovRegReg(REG_RSP, REG_RBP);
              EmitPopReg(REG_RBP);
              EmitRet();
            end;
        end;
      end;

      //----------------------------------------------------------------------
      // Ensure function ends properly (for functions without explicit return)
      //----------------------------------------------------------------------
      if Length(LFunc.Instructions) > 0 then
      begin
        if not (LFunc.Instructions[High(LFunc.Instructions)].Kind in [ikReturn, ikReturnValue]) then
        begin
          // Function doesn't end with explicit return - add epilogue
          EmitMovRegReg(REG_RSP, REG_RBP);
          EmitPopReg(REG_RBP);
          EmitRet();
        end;
      end
      else
      begin
        // Empty function - add epilogue
        EmitMovRegReg(REG_RSP, REG_RBP);
        EmitPopReg(REG_RBP);
        EmitRet();
      end;

      //----------------------------------------------------------------------
      // Backpatch intra-function jump targets
      //----------------------------------------------------------------------
      for LK := 0 to LJumpFixups.Count - 1 do
      begin
        LCodeOffset := LJumpFixups[LK].Key;
        LDataIndex := LJumpFixups[LK].Value;
        LDisp := Int32(LLabelOffsets[LDataIndex]) - Int32(LCodeOffset + 4);
        LTextSection.Position := LCodeOffset;
        LTextSection.WriteData(LDisp);
      end;
      LJumpFixups.Clear();
      LTextSection.Position := LTextSection.Size;

      AlignStream(LTextSection, 16);
    end;

    //========================================================================
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
      AlignStream(LTextSection, 16);

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
    // STEP 5: Backpatch cross-function calls
    //========================================================================
    for LI := 0 to LCallFixups.Count - 1 do
    begin
      LCodeOffset := LCallFixups[LI].Key;
      LDataIndex := LCallFixups[LI].Value;
      LDisp := Int32(LFuncOffsets[LDataIndex]) - Int32(LCodeOffset + 4);
      LTextSection.Position := LCodeOffset;
      LTextSection.WriteData(LDisp);
    end;
    LTextSection.Position := LTextSection.Size;

    //========================================================================
    // STEP 6: Backpatch .rodata references (LEA [RIP+disp32])
    //========================================================================
    for LI := 0 to LDataFixups.Count - 1 do
    begin
      LCodeOffset := LDataFixups[LI].Key;
      LDataIndex := LDataFixups[LI].Value;

      LDataHandle.Index := LDataIndex;
      LDataEntryRec := FData.GetEntry(LDataHandle);

      // LEA r64, [RIP+disp32] is 7 bytes; disp32 is at offset 3
      // Target file offset = LRoDataFileOffset + entry offset
      // Instruction file offset = LTextFileOffset + LCodeOffset
      // RIP after = instruction file offset + 7
      LDisp := Int32(LRoDataFileOffset + LDataEntryRec.Offset) -
               Int32(LTextFileOffset + LCodeOffset + 7);

      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;

    //========================================================================
    // STEP 7: Backpatch .data references (LEA [RIP+disp32])
    //========================================================================
    for LI := 0 to LGlobalFixups.Count - 1 do
    begin
      LCodeOffset := LGlobalFixups[LI].Key;
      LDataIndex := LGlobalFixups[LI].Value;

      LDataHandle.Index := LDataIndex;
      LDataEntryRec := FGlobals.GetEntry(LDataHandle);

      LDisp := Int32(LDataFileOffset + LDataEntryRec.Offset) -
               Int32(LTextFileOffset + LCodeOffset + 7);

      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;

    //========================================================================
    // STEP 8: Backpatch function address references (LEA [RIP+disp32])
    //========================================================================
    for LI := 0 to LFuncAddrFixups.Count - 1 do
    begin
      LCodeOffset := LFuncAddrFixups[LI].Key;
      LDataIndex := LFuncAddrFixups[LI].Value;

      // Target is in .text section
      LDisp := Int32(LFuncOffsets[LDataIndex]) - Int32(LCodeOffset + 7);

      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;
    LTextSection.Position := LTextSection.Size;

    //========================================================================
    // STEP 8b: Backpatch import calls (static or PLT)
    //========================================================================
    for LI := 0 to LPltFixups.Count - 1 do
    begin
      LCodeOffset := LPltFixups[LI].Key;
      LImportIndex := LPltFixups[LI].Value;  // Original import index

      // Check if this is a statically-resolved import
      if LStaticImportResolved.TryGetValue(LImportIndex, LTargetOffset) then
      begin
        // Static import - direct call to merged .text
        // Displacement: target offset - (instruction offset + 4)
        LDisp := Int32(LTargetOffset) - Int32(LCodeOffset + 4);
        LTextSection.Position := LCodeOffset;
        LTextSection.WriteData(LDisp);
      end
      else if LHasImports and LOrigToPltIndex.TryGetValue(LImportIndex, LPltSlotIndex) then
      begin
        // Dynamic import - call to PLT entry
        // PLT entry offset within PLT section: PLT[0]=16 bytes + slot*16
        LPltEntryFileOffset := LPltFileOffset + Cardinal(16 + LPltSlotIndex * 16);
        // Displacement: target - (instruction address + 4)
        LDisp := Int32(LPltEntryFileOffset) - Int32(LTextFileOffset + LCodeOffset + 4);

        LTextSection.Position := LCodeOffset;
        LTextSection.WriteData(LDisp);
      end;
    end;
    LTextSection.Position := LTextSection.Size;

    //========================================================================
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
      LEntryPointOffset := LTextSection.Size;

      if LMainIndex >= 0 then
      begin
        LMainOffset := LFuncOffsets[LMainIndex];

        // Initialize exception handling if needed
        if LHasSEH and (LInitExceptionsIdx >= 0) then
        begin
          LDisp := Int32(LFuncOffsets[LInitExceptionsIdx]) - Int32(LTextSection.Size + 5);
          EmitCallRel32(LDisp);
        end;
        if LHasSEH and (LInitSignalsIdx >= 0) then
        begin
          LDisp := Int32(LFuncOffsets[LInitSignalsIdx]) - Int32(LTextSection.Size + 5);
          EmitCallRel32(LDisp);
        end;

        // call main (rel32)
        LDisp := Int32(LMainOffset) - Int32(LTextSection.Size + 5);
        EmitCallRel32(LDisp);

        // mov rdi, rax (return value → first syscall arg)
        EmitMovRegReg(REG_RDI, REG_RAX);

        // mov rax, 60 (exit syscall)
        EmitMovRegImm64(REG_RAX, 60);

        // syscall
        EmitByte($0F);
        EmitByte($05);
      end
      else
      begin
        // No main -- exit with 0
        EmitXorRegReg(REG_RDI);
        EmitMovRegImm64(REG_RAX, 60);
        EmitByte($0F);
        EmitByte($05);
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
        LRelaPltSection.WriteData(
          UInt64(BASE_VADDR + LGotPltFileOffset + Cardinal(3 + LI) * 8));
        // r_info: ELF64_R_INFO(symbol_index, R_X86_64_JUMP_SLOT)
        LRelaPltSection.WriteData(
          UInt64((UInt64(LI + 1) shl 32) or R_X86_64_JUMP_SLOT));
        // r_addend
        LRelaPltSection.WriteData(Int64(0));
      end;

      //--------------------------------------------------------------------
      // Build .plt stubs
      //--------------------------------------------------------------------
      // PLT[0] -- resolver stub (16 bytes)
      // push QWORD [RIP + disp_to_GOT1]
      LPltSection.WriteData(Byte($FF));
      LPltSection.WriteData(Byte($35));
      LDisp := Int32(LGotPltFileOffset + 8) - Int32(LPltFileOffset + 6);
      LPltSection.WriteData(LDisp);
      // jmp QWORD [RIP + disp_to_GOT2]
      LPltSection.WriteData(Byte($FF));
      LPltSection.WriteData(Byte($25));
      LDisp := Int32(LGotPltFileOffset + 16) - Int32(LPltFileOffset + 12);
      LPltSection.WriteData(LDisp);
      // 4 bytes NOP padding
      LPltSection.WriteData(Byte($0F));
      LPltSection.WriteData(Byte($1F));
      LPltSection.WriteData(Byte($40));
      LPltSection.WriteData(Byte($00));

      // PLT[1..N] -- per-import stubs (16 bytes each)
      for LI := 0 to LImportCount - 1 do
      begin
        // jmp QWORD [RIP + disp_to_GOT_entry]
        LPltSection.WriteData(Byte($FF));
        LPltSection.WriteData(Byte($25));
        LRipAfterInstr := UInt64(LPltFileOffset) + 16 + UInt64(LI) * 16 + 6;
        LGotEntryVAddr := UInt64(LGotPltFileOffset) + 24 + UInt64(LI) * 8;
        LDisp := Int32(Int64(LGotEntryVAddr) - Int64(LRipAfterInstr));
        LPltSection.WriteData(LDisp);

        // push <relocation index>
        LPltSection.WriteData(Byte($68));
        LPltSection.WriteData(Cardinal(LI));

        // jmp PLT[0]
        LPltSection.WriteData(Byte($E9));
        LDisp := -(Int32(16 + LI * 16 + 16));
        LPltSection.WriteData(LDisp);
      end;

      //--------------------------------------------------------------------
      // Build .got.plt
      //--------------------------------------------------------------------
      // GOT[0] = VA of .dynamic
      LGotPltSection.WriteData(UInt64(BASE_VADDR + LDynamicFileOffset));
      // GOT[1] = 0 (link_map, filled by ld-linux)
      LGotPltSection.WriteData(UInt64(0));
      // GOT[2] = 0 (_dl_runtime_resolve, filled by ld-linux)
      LGotPltSection.WriteData(UInt64(0));
      // GOT[3+n] = PLT[n]+6 (points to push instruction for lazy binding)
      for LI := 0 to LImportCount - 1 do
        LGotPltSection.WriteData(
          UInt64(BASE_VADDR + LPltFileOffset + 16 + Cardinal(LI) * 16 + 6));

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

      LShstrtabSection.WriteData(Byte(0));  // index 0 = empty
      LShstrPos := 1;
      // Write section names and track offsets for section headers
      // Order: .interp .hash .dynsym .dynstr .rela.plt .rodata .data .plt .text .got.plt .dynamic .shstrtab
      LShstrtabSection.WriteBuffer(AnsiString('.interp'#0)[1], 8);     // pos 1
      LShstrtabSection.WriteBuffer(AnsiString('.hash'#0)[1], 6);       // pos 9
      LShstrtabSection.WriteBuffer(AnsiString('.dynsym'#0)[1], 8);     // pos 15
      LShstrtabSection.WriteBuffer(AnsiString('.dynstr'#0)[1], 8);     // pos 23
      LShstrtabSection.WriteBuffer(AnsiString('.rela.plt'#0)[1], 10);  // pos 31
      LShstrtabSection.WriteBuffer(AnsiString('.rodata'#0)[1], 8);     // pos 41
      LShstrtabSection.WriteBuffer(AnsiString('.data'#0)[1], 6);       // pos 49
      LShstrtabSection.WriteBuffer(AnsiString('.plt'#0)[1], 5);        // pos 55
      LShstrtabSection.WriteBuffer(AnsiString('.text'#0)[1], 6);       // pos 60
      LShstrtabSection.WriteBuffer(AnsiString('.got.plt'#0)[1], 9);    // pos 66
      LShstrtabSection.WriteBuffer(AnsiString('.dynamic'#0)[1], 9);    // pos 75
      LShstrtabSection.WriteBuffer(AnsiString('.shstrtab'#0)[1], 10);  // pos 84

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
      LResult.WriteData(Word(EM_X86_64));      // e_machine
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
        LResult.WriteData(Word(12 - LInterpOffset)); // e_shstrndx (.shstrtab index)
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

        // [0] SHN_UNDEF
        WriteShdr(LResult, 0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);
        // [1] .interp (executables only)
        if not LIsSharedObject then
          WriteShdr(LResult, 1, SHT_PROGBITS, SHF_ALLOC,
            BASE_VADDR + LInterpFileOffset, LInterpFileOffset,
            Cardinal(LInterpSection.Size), 0, 0, 1, 0);
        // [1 or 2] .hash
        WriteShdr(LResult, 9, SHT_HASH, SHF_ALLOC,
          BASE_VADDR + LHashFileOffset, LHashFileOffset,
          Cardinal(LHashSection.Size), 3 - LInterpOffset, 0, 8, 4);
        // [3] .dynsym
        WriteShdr(LResult, 15, SHT_DYNSYM, SHF_ALLOC,
          BASE_VADDR + LDynsymFileOffset, LDynsymFileOffset,
          Cardinal(LDynsymSection.Size), 4 - LInterpOffset, 1, 8, ELF64_SYM_SIZE);
        // [4] .dynstr
        WriteShdr(LResult, 23, SHT_STRTAB, SHF_ALLOC,
          BASE_VADDR + LDynstrFileOffset, LDynstrFileOffset,
          Cardinal(LDynstrSection.Size), 0, 0, 1, 0);
        // [5] .rela.plt
        WriteShdr(LResult, 31, SHT_RELA, SHF_ALLOC or SHF_INFO_LINK,
          BASE_VADDR + LRelaPltFileOffset, LRelaPltFileOffset,
          Cardinal(LRelaPltSection.Size), 3 - LInterpOffset, 8 - LInterpOffset, 8, ELF64_RELA_SIZE);
        // [6] .rodata
        WriteShdr(LResult, 41, SHT_PROGBITS, SHF_ALLOC,
          BASE_VADDR + LRoDataFileOffset, LRoDataFileOffset,
          Cardinal(LRoDataSection.Size), 0, 0, 16, 0);
        // [7] .data
        WriteShdr(LResult, 49, SHT_PROGBITS, SHF_ALLOC or SHF_WRITE,
          BASE_VADDR + LDataFileOffset, LDataFileOffset,
          Cardinal(LDataSection.Size), 0, 0, 16, 0);
        // [8] .plt
        WriteShdr(LResult, 55, SHT_PROGBITS, SHF_ALLOC or SHF_EXECINSTR,
          BASE_VADDR + LPltFileOffset, LPltFileOffset,
          Cardinal(LPltSection.Size), 0, 0, 16, 16);
        // [9] .text
        WriteShdr(LResult, 60, SHT_PROGBITS, SHF_ALLOC or SHF_EXECINSTR,
          BASE_VADDR + LTextFileOffset, LTextFileOffset,
          LTextSize, 0, 0, 16, 0);
        // [10] .got.plt
        WriteShdr(LResult, 66, SHT_PROGBITS, SHF_ALLOC or SHF_WRITE,
          BASE_VADDR + LGotPltFileOffset, LGotPltFileOffset,
          Cardinal(LGotPltSection.Size), 0, 0, 8, 8);
        // [11] .dynamic
        WriteShdr(LResult, 75, SHT_DYNAMIC, SHF_ALLOC or SHF_WRITE,
          BASE_VADDR + LDynamicFileOffset, LDynamicFileOffset,
          Cardinal(LDynamicSection.Size), 4, 0, 8, ELF64_DYN_SIZE);
        // [12] .shstrtab
        WriteShdr(LResult, 84, SHT_STRTAB, 0,
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
    LTextSection.Free();
    LDataSection.Free();
    LRoDataSection.Free();
  end;
end;

//==============================================================================
// GenerateELFObj -- Produces an ELF64 relocatable object file (.o)
//==============================================================================

function TTigerLinux64Backend.GenerateELFObj(): TBytes;
const
  // ELF64 Constants
  ELF_MAGIC: array[0..3] of Byte = ($7F, $45, $4C, $46);
  ELFCLASS64    = 2;
  ELFDATA2LSB   = 1;
  ELFOSABI_NONE = 0;
  ET_REL        = 1;  // Relocatable file
  EM_X86_64     = $3E;

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
  R_X86_64_PC32  = 2;  // PC-relative 32-bit
  R_X86_64_PLT32 = 4;  // PLT-relative 32-bit

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
  LAllLabelOffsets: TArray<TArray<Cardinal>>;
  LJumpFixups: TList<TPair<Cardinal, Integer>>;

  // Stack frame
  LStackFrameSize: Cardinal;
  LLocalsSize: Integer;
  LMaxCallArgs: Integer;
  LOutgoingArgSpace: Cardinal;

  // Temporaries
  LCodeOffset: Cardinal;
  LDisp: Int32;
  LTargetOffset: Cardinal;
  LExportName: string;
  LParamTypes: TArray<TTigerValueType>;
  LNameOffset: Cardinal;
  LEntry: TTigerImportEntry;
  LImportSymIndices: TDictionary<Integer, Integer>;

  // Exception handling (Linux64: setjmp/longjmp based)
  LExceptFrameSize: Integer;
  LExceptFrameBaseOffset: Integer;
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
  LScopeIdx: Integer;
  LFrameOffset: Integer;
  LExceptLabelIdx: Integer;
  LFuncSymIdx: Integer;
  LImportSymIdx: Integer;

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
  // Local code emission helpers (same as GenerateELF)
  //----------------------------------------------------------------------------
  procedure EmitByte(const AValue: Byte);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitWord(const AValue: Word);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitDWord(const AValue: Cardinal);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitQWord(const AValue: UInt64);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitInt32(const AValue: Int32);
  begin
    LTextSection.WriteData(AValue);
  end;

  procedure EmitRex(const AW, AR, AX, AB: Boolean);
  var
    LRex: Byte;
  begin
    LRex := $40;
    if AW then LRex := LRex or $08;
    if AR then LRex := LRex or $04;
    if AX then LRex := LRex or $02;
    if AB then LRex := LRex or $01;
    EmitByte(LRex);
  end;

  procedure EmitModRM(const AMod, AReg, ARM: Byte);
  begin
    EmitByte((AMod shl 6) or ((AReg and 7) shl 3) or (ARM and 7));
  end;

  procedure EmitSIB(const AScale, AIndex, ABase: Byte);
  begin
    EmitByte((AScale shl 6) or ((AIndex and 7) shl 3) or (ABase and 7));
  end;

  procedure EmitMovRegReg64(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc > 7, False, ADest > 7);
    EmitByte($89);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitMovRegImm64(const AReg: Byte; const AValue: Int64);
  begin
    EmitRex(True, False, False, AReg > 7);
    EmitByte($B8 + (AReg and 7));
    EmitQWord(UInt64(AValue));
  end;

  procedure EmitAddRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc > 7, False, ADest > 7);
    EmitByte($01);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitSubRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc > 7, False, ADest > 7);
    EmitByte($29);
    EmitModRM(3, ASrc, ADest);
  end;

  procedure EmitImulRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ADest > 7, False, ASrc > 7);
    EmitByte($0F); EmitByte($AF);
    EmitModRM(3, ADest, ASrc);
  end;

  procedure EmitMovMemReg(const ABase: Byte; const AOffset: Int32; const ASrc: Byte);
  begin
    EmitRex(True, ASrc > 7, False, ABase > 7);
    EmitByte($89);
    if AOffset = 0 then
    begin
      EmitModRM(0, ASrc, ABase);
      if (ABase and 7) = 4 then EmitSIB(0, 4, 4);
    end
    else if (AOffset >= -128) and (AOffset <= 127) then
    begin
      EmitModRM(1, ASrc, ABase);
      if (ABase and 7) = 4 then EmitSIB(0, 4, 4);
      EmitByte(Byte(AOffset));
    end
    else
    begin
      EmitModRM(2, ASrc, ABase);
      if (ABase and 7) = 4 then EmitSIB(0, 4, 4);
      EmitDWord(Cardinal(AOffset));
    end;
  end;

  procedure EmitMovRegMem(const ADest: Byte; const ABase: Byte; const AOffset: Int32);
  begin
    EmitRex(True, ADest > 7, False, ABase > 7);
    EmitByte($8B);
    if AOffset = 0 then
    begin
      EmitModRM(0, ADest, ABase);
      if (ABase and 7) = 4 then EmitSIB(0, 4, 4);
    end
    else if (AOffset >= -128) and (AOffset <= 127) then
    begin
      EmitModRM(1, ADest, ABase);
      if (ABase and 7) = 4 then EmitSIB(0, 4, 4);
      EmitByte(Byte(AOffset));
    end
    else
    begin
      EmitModRM(2, ADest, ABase);
      if (ABase and 7) = 4 then EmitSIB(0, 4, 4);
      EmitDWord(Cardinal(AOffset));
    end;
  end;

  function GetLocalOffset(const ALocalIndex: Integer): Int32;
  var
    LOffset: Integer;
    LK: Integer;
  begin
    // Locals follow params: [RBP - (8 + params*8 + local_offset)]
    LOffset := 8 + Length(LFunc.Params) * 8;
    for LK := 0 to ALocalIndex - 1 do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -LOffset;
  end;

  function GetTempOffset(const ATempIndex: Integer): Int32;
  var
    LOffset: Integer;
    LK: Integer;
  begin
    // Temps follow params and locals
    LOffset := 8 + Length(LFunc.Params) * 8;
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    LOffset := LOffset + ATempIndex * 8;
    Result := -LOffset;
  end;

  function GetParamReg(const AParamIndex: Integer): Integer;
  begin
    // System V: RDI, RSI, RDX, RCX, R8, R9
    case AParamIndex of
      0: Result := REG_RDI;
      1: Result := REG_RSI;
      2: Result := REG_RDX;
      3: Result := REG_RCX;
      4: Result := REG_R8;
      5: Result := REG_R9;
    else
      Result := -1;  // Stack param
    end;
  end;

  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    // Params saved at [RBP-8], [RBP-16], etc.
    Result := -(8 + AIndex * 8);
  end;

  procedure EmitMovRbpDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    // mov [rbp+disp], reg
    EmitRex(True, AReg > 7, False, False);
    EmitByte($89);
    if (ADisp >= -128) and (ADisp <= 127) then
    begin
      EmitModRM(1, AReg, 5);  // [RBP+disp8]
      EmitByte(Byte(ADisp));
    end
    else
    begin
      EmitModRM(2, AReg, 5);  // [RBP+disp32]
      EmitDWord(Cardinal(ADisp));
    end;
  end;

  procedure LoadOperandToReg(const AOp: TTigerOperand; const AReg: Byte);
  begin
    case AOp.Kind of
      okImmediate:
        EmitMovRegImm64(AReg, AOp.ImmInt);
      okTemp:
        EmitMovRegMem(AReg, REG_RBP, GetTempOffset(AOp.TempHandle.Index));
      okLocal:
      begin
        if AOp.LocalHandle.IsParam then
          // Params were saved to stack in prologue
          EmitMovRegMem(AReg, REG_RBP, GetParamOffset(AOp.LocalHandle.Index))
        else
          EmitMovRegMem(AReg, REG_RBP, GetLocalOffset(AOp.LocalHandle.Index));
      end;
    end;
  end;

  procedure StoreTempFromReg(const ATempIndex: Integer; const AReg: Byte);
  begin
    EmitMovMemReg(REG_RBP, GetTempOffset(ATempIndex), AReg);
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
  LHasSEH := False;
  LPushExceptFrameIdx := -1;
  LPopExceptFrameIdx := -1;
  LGetExceptFrameIdx := -1;
  LSigsetjmpIdx := -1;
  LInitExceptionsIdx := -1;
  LInitSignalsIdx := -1;

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
        LInitSignalsIdx := LI;
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
    // STEP 3: Generate code for each function
    //==========================================================================
    SetLength(LFuncOffsets, FCode.GetFuncCount());
    SetLength(LFuncEndOffsets, FCode.GetFuncCount());
    SetLength(LAllLabelOffsets, FCode.GetFuncCount());

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LFuncOffsets[LI] := Cardinal(LTextSection.Size);

      LJumpFixups.Clear();

      // Compute stack frame
      LLocalsSize := 0;
      for LJ := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + LFunc.Locals[LJ].LocalSize;

      // Calculate exception frame space (216 bytes per scope)
      LExceptFrameSize := Length(LFunc.ExceptionScopes) * 216;

      // Build label lookup tables for exception handling
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
      begin
        LInstr := LFunc.Instructions[LJ];
        if LInstr.Kind in [ikCallImport, ikCall, ikCallIndirect, ikSyscall] then
        begin
          if Length(LInstr.Args) > LMaxCallArgs then
            LMaxCallArgs := Length(LInstr.Args);
        end;
      end;

      // System V: no shadow space, but need 16-byte alignment
      // Stack after call: RSP mod 16 = 8 (return address pushed)
      // After push rbp: RSP mod 16 = 0
      // Locals + outgoing args, align to 16
      if LMaxCallArgs > 6 then
        LOutgoingArgSpace := Cardinal((LMaxCallArgs - 6) * 8)
      else
        LOutgoingArgSpace := 0;

      LStackFrameSize := Cardinal(8) +                          // alignment base
                          Cardinal(Length(LFunc.Params) * 8) +   // saved params
                          Cardinal(LLocalsSize) +
                          Cardinal(LFunc.TempCount * 8) +        // temporaries
                          LOutgoingArgSpace +
                          Cardinal(LExceptFrameSize);            // exception frames
      // Ensure 16-byte alignment after sub rsp
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

      // Calculate base offset for exception frames (from RBP, negative)
      // Layout: [params][locals][temps][outgoing args][except frames]
      LExceptFrameBaseOffset := Integer(8 + Length(LFunc.Params) * 8 +
                                  LLocalsSize + LFunc.TempCount * 8 +
                                  LOutgoingArgSpace);

      // Prologue: push rbp; mov rbp, rsp; sub rsp, N
      EmitByte($55);  // push rbp
      EmitRex(True, False, False, False);
      EmitByte($89); EmitModRM(3, 4, 5);  // mov rbp, rsp
      if LStackFrameSize > 0 then
      begin
        EmitRex(True, False, False, False);
        EmitByte($81); EmitModRM(3, 5, 4);  // sub rsp, imm32
        EmitDWord(LStackFrameSize);
      end;

      // Save register params to stack (System V ABI: RDI, RSI, RDX, RCX, R8, R9)
      if Length(LFunc.Params) > 0 then
        EmitMovRbpDispReg(GetParamOffset(0), REG_RDI);
      if Length(LFunc.Params) > 1 then
        EmitMovRbpDispReg(GetParamOffset(1), REG_RSI);
      if Length(LFunc.Params) > 2 then
        EmitMovRbpDispReg(GetParamOffset(2), REG_RDX);
      if Length(LFunc.Params) > 3 then
        EmitMovRbpDispReg(GetParamOffset(3), REG_RCX);
      if Length(LFunc.Params) > 4 then
        EmitMovRbpDispReg(GetParamOffset(4), REG_R8);
      if Length(LFunc.Params) > 5 then
        EmitMovRbpDispReg(GetParamOffset(5), REG_R9);

      // Initialize label tracking for this function
      SetLength(LLabelOffsets, Length(LFunc.Labels));
      for LJ := 0 to High(LLabelOffsets) do
        LLabelOffsets[LJ] := $FFFFFFFF;

      // Generate instructions
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LJ];
        LCodeOffset := Cardinal(LTextSection.Size);

        case LInstr.Kind of
          ikLabel:
            if LInstr.LabelTarget.IsValid() then
            begin
              LLabelOffsets[LInstr.LabelTarget.Index] := LCodeOffset;

              //--------------------------------------------------------------
              // Exception handling: TryBeginLabel setup
              //--------------------------------------------------------------
              if LTryBeginLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                // Validate required functions/imports are available
                if LPushExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PushExceptFrame not found - exception handling runtime not linked');
                if LSigsetjmpIdx < 0 then
                  raise Exception.Create('__sigsetjmp not imported - exception handling runtime not linked');

                // Calculate frame offset for this scope
                LFrameOffset := LExceptFrameBaseOffset + (LScopeIdx * 216);

                // LEA RDI, [RBP - frame_offset] (frame pointer)
                EmitRex(True, False, False, False);
                EmitByte($8D);
                EmitModRM(2, 7, 5);  // RDI, [RBP+disp32]
                EmitDWord(Cardinal(-LFrameOffset));

                // CALL Tiger_PushExceptFrame - add relocation
                EmitByte($E8);
                LFuncSymIdx := LFuncSymIndices[LPushExceptFrameIdx];
                LReloc.Offset := Cardinal(LTextSection.Size);
                LReloc.SymbolIndex := LFuncSymIdx;
                LReloc.RelocationType := R_X86_64_PLT32;
                LReloc.Addend := -4;
                LTextRelocs.Add(LReloc);
                EmitDWord(0);

                // LEA RDI, [RBP - frame_offset + 8] (jmpbuf = frame + 8)
                EmitRex(True, False, False, False);
                EmitByte($8D);
                EmitModRM(2, 7, 5);
                EmitDWord(Cardinal(-(LFrameOffset - 8)));

                // XOR ESI, ESI (savesigs = 0)
                EmitByte($31);
                EmitModRM(3, 6, 6);

                // XOR EAX, EAX (varargs indicator)
                EmitByte($31);
                EmitModRM(3, 0, 0);

                // CALL __sigsetjmp (import) - add relocation
                EmitByte($E8);
                if not LImportSymIndices.ContainsKey(LSigsetjmpIdx) then
                begin
                  LEntry := FImports.GetEntryByIndex(LSigsetjmpIdx);
                  AddStrtabString(LEntry.FuncName, LNameOffset);
                  WriteSymbol(LNameOffset, (STB_GLOBAL shl 4) or STT_NOTYPE, 0, SHN_UNDEF, 0, 0);
                  LImportSymIndices.Add(LSigsetjmpIdx, LSymCount);
                  Inc(LSymCount);
                end;
                LReloc.Offset := Cardinal(LTextSection.Size);
                LReloc.SymbolIndex := LImportSymIndices[LSigsetjmpIdx];
                LReloc.RelocationType := R_X86_64_PLT32;
                LReloc.Addend := -4;
                LTextRelocs.Add(LReloc);
                EmitDWord(0);

                // TEST EAX, EAX
                EmitByte($85);
                EmitModRM(3, 0, 0);

                // Determine target: except block if present, else finally
                if LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.Index
                else if LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.Index
                else
                  LExceptLabelIdx := -1;

                // JNZ target_label (jump if exception occurred)
                if LExceptLabelIdx >= 0 then
                begin
                  EmitByte($0F);
                  EmitByte($85);
                  LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
                    Cardinal(LTextSection.Size), LExceptLabelIdx));
                  EmitDWord(0);
                end;
              end;

              //--------------------------------------------------------------
              // Exception handling: EndLabel cleanup
              //--------------------------------------------------------------
              if LEndLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                // Validate required function is available
                if LPopExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PopExceptFrame not found - exception handling runtime not linked');

                // CALL Tiger_PopExceptFrame - add relocation
                EmitByte($E8);
                LFuncSymIdx := LFuncSymIndices[LPopExceptFrameIdx];
                LReloc.Offset := Cardinal(LTextSection.Size);
                LReloc.SymbolIndex := LFuncSymIdx;
                LReloc.RelocationType := R_X86_64_PLT32;
                LReloc.Addend := -4;
                LTextRelocs.Add(LReloc);
                EmitDWord(0);
              end;
            end;

          ikReturn:
          begin
            // Epilogue: mov rsp, rbp; pop rbp; ret
            EmitRex(True, False, False, False);
            EmitByte($89); EmitModRM(3, 5, 4);  // mov rsp, rbp
            EmitByte($5D);  // pop rbp
            EmitByte($C3);  // ret
          end;

          ikJump:
          begin
            // jmp rel32 - will fixup
            EmitByte($E9);
            LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
              Cardinal(LTextSection.Size), LInstr.LabelTarget.Index));
            EmitDWord(0);
          end;

          ikJumpIf:
          begin
            // Jump if condition is true (non-zero)
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            // test rax, rax
            EmitRex(True, False, False, False);
            EmitByte($85); EmitModRM(3, 0, 0);
            // jnz rel32
            EmitByte($0F);
            EmitByte($85);
            LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
              Cardinal(LTextSection.Size), LInstr.LabelTarget.Index));
            EmitDWord(0);
          end;

          ikJumpIfNot:
          begin
            // Jump if condition is false (zero)
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            // test rax, rax
            EmitRex(True, False, False, False);
            EmitByte($85); EmitModRM(3, 0, 0);
            // jz rel32
            EmitByte($0F);
            EmitByte($84);
            LJumpFixups.Add(TPair<Cardinal, Integer>.Create(
              Cardinal(LTextSection.Size), LInstr.LabelTarget.Index));
            EmitDWord(0);
          end;

          ikCall:
          begin
            // Load register args (System V ABI: RDI, RSI, RDX, RCX, R8, R9)
            if Length(LInstr.Args) > 0 then
              LoadOperandToReg(LInstr.Args[0], REG_RDI);
            if Length(LInstr.Args) > 1 then
              LoadOperandToReg(LInstr.Args[1], REG_RSI);
            if Length(LInstr.Args) > 2 then
              LoadOperandToReg(LInstr.Args[2], REG_RDX);
            if Length(LInstr.Args) > 3 then
              LoadOperandToReg(LInstr.Args[3], REG_RCX);
            if Length(LInstr.Args) > 4 then
              LoadOperandToReg(LInstr.Args[4], REG_R8);
            if Length(LInstr.Args) > 5 then
              LoadOperandToReg(LInstr.Args[5], REG_R9);

            // Call to another function - add relocation
            // call rel32 with R_X86_64_PLT32 relocation
            EmitByte($E8);

            // Get target function index from instruction
            LK := LInstr.FuncTarget.Index;
            if (LK >= 0) and (LK < Length(LFuncSymIndices)) then
            begin
              LReloc.Offset := Cardinal(LTextSection.Size);
              LReloc.SymbolIndex := LFuncSymIndices[LK];
              LReloc.RelocationType := R_X86_64_PLT32;
              LReloc.Addend := -4;  // Standard addend for call
              LTextRelocs.Add(LReloc);
            end;

            EmitDWord(0);

            // Store result if needed
            if LInstr.Dest.IsValid() then
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCallImport:
          begin
            // Load register args (System V ABI: RDI, RSI, RDX, RCX, R8, R9)
            if Length(LInstr.Args) > 0 then
              LoadOperandToReg(LInstr.Args[0], REG_RDI);
            if Length(LInstr.Args) > 1 then
              LoadOperandToReg(LInstr.Args[1], REG_RSI);
            if Length(LInstr.Args) > 2 then
              LoadOperandToReg(LInstr.Args[2], REG_RDX);
            if Length(LInstr.Args) > 3 then
              LoadOperandToReg(LInstr.Args[3], REG_RCX);
            if Length(LInstr.Args) > 4 then
              LoadOperandToReg(LInstr.Args[4], REG_R8);
            if Length(LInstr.Args) > 5 then
              LoadOperandToReg(LInstr.Args[5], REG_R9);

            // Call to imported function - add relocation
            EmitByte($E8);
            LK := LInstr.ImportTarget.Index;
            if (LK >= 0) and (LK < FImports.GetCount()) then
            begin
              LEntry := FImports.GetEntryByIndex(LK);
              // Add import symbol if not already added
              if not LImportSymIndices.ContainsKey(LK) then
              begin
                AddStrtabString(LEntry.FuncName, LNameOffset);
                LImportSymIndices.Add(LK, LSymCount);
                WriteSymbol(LNameOffset, (STB_GLOBAL shl 4) or STT_NOTYPE, 0, 0, 0, 0);
                Inc(LSymCount);
              end;

              LReloc.Offset := Cardinal(LTextSection.Size);
              LReloc.SymbolIndex := LImportSymIndices[LK];
              LReloc.RelocationType := R_X86_64_PLT32;
              LReloc.Addend := -4;
              LTextRelocs.Add(LReloc);
            end;
            EmitDWord(0);

            // Store result if needed
            if LInstr.Dest.IsValid() then
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikLoad:
          begin
            // Load operand (Op1) to temp (Dest)
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikStore:
          begin
            // Store value (Op2) to local/param (Op1)
            LoadOperandToReg(LInstr.Op2, REG_RAX);
            if LInstr.Op1.LocalHandle.IsParam then
              EmitMovRbpDispReg(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_RAX)
            else
              EmitMovRbpDispReg(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_RAX);
          end;

          ikAdd:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            EmitAddRegReg(REG_RAX, REG_RCX);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikSub:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            EmitSubRegReg(REG_RAX, REG_RCX);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikMul:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            EmitImulRegReg(REG_RAX, REG_RCX);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikDiv:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cqo: sign-extend RAX into RDX:RAX
            EmitRex(True, False, False, False);
            EmitByte($99);
            // idiv rcx
            EmitRex(True, False, False, False);
            EmitByte($F7); EmitModRM(3, 7, 1);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikMod:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cqo
            EmitRex(True, False, False, False);
            EmitByte($99);
            // idiv rcx
            EmitRex(True, False, False, False);
            EmitByte($F7); EmitModRM(3, 7, 1);
            StoreTempFromReg(LInstr.Dest.Index, REG_RDX);  // remainder in RDX
          end;

          ikBitAnd:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // and rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($21); EmitModRM(3, 1, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikBitOr:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // or rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($09); EmitModRM(3, 1, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikBitXor:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // xor rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($31); EmitModRM(3, 1, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikBitNot:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            // not rax
            EmitRex(True, False, False, False);
            EmitByte($F7); EmitModRM(3, 2, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikShl:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // shl rax, cl
            EmitRex(True, False, False, False);
            EmitByte($D3); EmitModRM(3, 4, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikShr:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // sar rax, cl (arithmetic shift)
            EmitRex(True, False, False, False);
            EmitByte($D3); EmitModRM(3, 7, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCmpEq:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cmp rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($39); EmitModRM(3, 1, 0);
            // sete al
            EmitByte($0F); EmitByte($94); EmitByte($C0);
            // movzx eax, al
            EmitByte($0F); EmitByte($B6); EmitByte($C0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCmpNe:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cmp rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($39); EmitModRM(3, 1, 0);
            // setne al
            EmitByte($0F); EmitByte($95); EmitByte($C0);
            // movzx eax, al
            EmitByte($0F); EmitByte($B6); EmitByte($C0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCmpLt:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cmp rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($39); EmitModRM(3, 1, 0);
            // setl al
            EmitByte($0F); EmitByte($9C); EmitByte($C0);
            // movzx eax, al
            EmitByte($0F); EmitByte($B6); EmitByte($C0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCmpLe:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cmp rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($39); EmitModRM(3, 1, 0);
            // setle al
            EmitByte($0F); EmitByte($9E); EmitByte($C0);
            // movzx eax, al
            EmitByte($0F); EmitByte($B6); EmitByte($C0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCmpGt:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cmp rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($39); EmitModRM(3, 1, 0);
            // setg al
            EmitByte($0F); EmitByte($9F); EmitByte($C0);
            // movzx eax, al
            EmitByte($0F); EmitByte($B6); EmitByte($C0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikCmpGe:
          begin
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // cmp rax, rcx
            EmitRex(True, False, False, False);
            EmitByte($39); EmitModRM(3, 1, 0);
            // setge al
            EmitByte($0F); EmitByte($9D); EmitByte($C0);
            // movzx eax, al
            EmitByte($0F); EmitByte($B6); EmitByte($C0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikNop:
            EmitByte($90);

          ikAddressOf:
          begin
            // LEA rax, [rbp+disp]
            if LInstr.Op1.LocalHandle.IsParam then
            begin
              EmitRex(True, False, False, False);
              EmitByte($8D);
              EmitModRM(2, 0, 5);  // [RBP+disp32]
              EmitDWord(Cardinal(GetParamOffset(LInstr.Op1.LocalHandle.Index)));
            end
            else
            begin
              EmitRex(True, False, False, False);
              EmitByte($8D);
              EmitModRM(2, 0, 5);
              EmitDWord(Cardinal(GetLocalOffset(LInstr.Op1.LocalHandle.Index)));
            end;
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikLoadPtr:
          begin
            // Load value at pointer: dest = [Op1]
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            // mov rax, [rax]
            EmitRex(True, False, False, False);
            EmitByte($8B); EmitModRM(0, 0, 0);
            StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikStorePtr:
          begin
            // Store Op2 to address in Op1: [Op1] = Op2
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            LoadOperandToReg(LInstr.Op2, REG_RCX);
            // mov [rax], rcx
            EmitRex(True, False, False, False);
            EmitByte($89); EmitModRM(0, 1, 0);
          end;

          ikSyscall:
          begin
            // Load syscall args (RDI, RSI, RDX, R10, R8, R9)
            if Length(LInstr.Args) > 0 then
              LoadOperandToReg(LInstr.Args[0], REG_RDI);
            if Length(LInstr.Args) > 1 then
              LoadOperandToReg(LInstr.Args[1], REG_RSI);
            if Length(LInstr.Args) > 2 then
              LoadOperandToReg(LInstr.Args[2], REG_RDX);
            if Length(LInstr.Args) > 3 then
              LoadOperandToReg(LInstr.Args[3], REG_R10);
            if Length(LInstr.Args) > 4 then
              LoadOperandToReg(LInstr.Args[4], REG_R8);
            if Length(LInstr.Args) > 5 then
              LoadOperandToReg(LInstr.Args[5], REG_R9);
            // Load syscall number into RAX
            EmitMovRegImm64(REG_RAX, UInt64(LInstr.SyscallNr));
            // syscall
            EmitByte($0F); EmitByte($05);
            if LInstr.Dest.IsValid() then
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
          end;

          ikReturnValue:
          begin
            // Load return value to RAX before epilog
            LoadOperandToReg(LInstr.Op1, REG_RAX);
            // Epilogue: mov rsp, rbp; pop rbp; ret
            EmitRex(True, False, False, False);
            EmitByte($89); EmitModRM(3, 5, 4);  // mov rsp, rbp
            EmitByte($5D);  // pop rbp
            EmitByte($C3);  // ret
          end;

          // Other instructions are handled by the full GenerateELF()
          // For .o/.a generation, we emit minimal stubs
        end;
      end;

      LFuncEndOffsets[LI] := Cardinal(LTextSection.Size);
      LAllLabelOffsets[LI] := Copy(LLabelOffsets);

      // Fixup jumps within this function
      for LJ := 0 to LJumpFixups.Count - 1 do
      begin
        LCodeOffset := LJumpFixups[LJ].Key;
        LK := LJumpFixups[LJ].Value;
        if (LK >= 0) and (LK < Length(LAllLabelOffsets[LI])) then
        begin
          LTargetOffset := LAllLabelOffsets[LI][LK];
          LDisp := Int32(LTargetOffset) - Int32(LCodeOffset + 4);
          LTextSection.Position := LCodeOffset;
          LTextSection.WriteData(LDisp);
          LTextSection.Position := LTextSection.Size;
        end;
      end;
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
    LResult.WriteData(Word(EM_X86_64));  // e_machine
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

function TTigerLinux64Backend.GenerateArArchive(): TBytes;
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
