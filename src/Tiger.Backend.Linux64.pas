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
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Utils.Win64,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.Backend.X64,
  Tiger.ABI.Linux64;

type
  { TTigerLinux64Backend }
  TTigerLinux64Backend = class(TTigerBackend)
  private
    function GenerateELF(): TBytes;
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
  // Strip any extension — Linux executables have none by convention
  FOutputPath := TPath.ChangeExtension(APath, '');
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
  else
    // DLL, Obj, Lib not supported in Phase 1
    SetLength(Result, 0);
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
// GenerateELF — Produces a minimal ELF64 executable
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
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  LInstr: TTigerInstruction;

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
  // Stack frame helpers (System V AMD64 — no shadow space)
  //
  // Layout from RBP downward:
  //   [RBP-8..RBP-params*8]     saved params
  //   [after params..]           locals
  //   [after locals..]           temps
  //   [RSP..RSP+outgoing-1]     outgoing stack args (for calls with >6 args)
  //--------------------------------------------------------------------------
  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    // [RBP-8] = param0, [RBP-16] = param1, etc.
    Result := -(8 + AIndex * 8);
  end;

  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LOffset: Integer;
    LK: Integer;
  begin
    // Locals follow params
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
  // Operand loading — handles all TTigerOperandKind values
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
          // LEA reg, [RIP+disp32] — fixup to .rodata
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(
            LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder
        end;

      okGlobal:
        begin
          // LEA reg, [RIP+disp32] — fixup to .data
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
          // LEA reg, [RIP+disp32] — fixup to function address
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
  try
    LHasImports := FImports.GetCount() > 0;
    LImportCount := FImports.GetCount();
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
    // STEP 2b: Build dynamic linking sections (when imports present)
    //========================================================================
    if LHasImports then
    begin
      //--------------------------------------------------------------------
      // .interp — dynamic linker path
      //--------------------------------------------------------------------
      LDynstrPos := 0; // reuse as temp
      LInterpSection.WriteBuffer(AnsiString('/lib64/ld-linux-x86-64.so.2'#0)[1], 28);

      //--------------------------------------------------------------------
      // .dynstr — string table (null byte + symbol names + lib names)
      //--------------------------------------------------------------------
      LDynstrSection.WriteData(Byte(0));  // index 0 = empty string
      LDynstrPos := 1;

      // Symbol names
      SetLength(LSymDynstrOffsets, LImportCount);
      for LI := 0 to LImportCount - 1 do
      begin
        LEntry := FImports.GetEntryByIndex(LI);
        LSymDynstrOffsets[LI] := LDynstrPos;
        LDynstrSection.WriteBuffer(AnsiString(LEntry.FuncName + #0)[1],
          Length(LEntry.FuncName) + 1);
        Inc(LDynstrPos, Cardinal(Length(LEntry.FuncName)) + 1);
      end;

      // Library names (deduplicated)
      for LI := 0 to LImportCount - 1 do
      begin
        LEntry := FImports.GetEntryByIndex(LI);
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

      //--------------------------------------------------------------------
      // .dynsym — symbol table (STN_UNDEF + one per import)
      //--------------------------------------------------------------------
      // Entry 0: STN_UNDEF (24 bytes of zeros)
      for LI := 0 to ELF64_SYM_SIZE - 1 do
        LDynsymSection.WriteData(Byte(0));

      for LI := 0 to LImportCount - 1 do
      begin
        LDynsymSection.WriteData(LSymDynstrOffsets[LI]);         // st_name
        LDynsymSection.WriteData(Byte((STB_GLOBAL shl 4) or STT_FUNC)); // st_info
        LDynsymSection.WriteData(Byte(0));                       // st_other
        LDynsymSection.WriteData(Word(0));                       // st_shndx = SHN_UNDEF
        LDynsymSection.WriteData(UInt64(0));                     // st_value
        LDynsymSection.WriteData(UInt64(0));                     // st_size
      end;

      //--------------------------------------------------------------------
      // .hash — SysV hash table for symbol lookup
      //--------------------------------------------------------------------
      LNBuckets := Cardinal(LImportCount);
      if LNBuckets = 0 then
        LNBuckets := 1;
      SetLength(LBuckets, LNBuckets);
      SetLength(LChains, 1 + LImportCount);  // chain[0] = STN_UNDEF
      for LI := 0 to High(LBuckets) do
        LBuckets[LI] := 0;
      for LI := 0 to High(LChains) do
        LChains[LI] := 0;

      // Build hash chains: symbol indices are 1..LImportCount
      for LI := 0 to LImportCount - 1 do
      begin
        LEntry := FImports.GetEntryByIndex(LI);
        LHashVal := ElfHash(LEntry.FuncName) mod LNBuckets;
        // Insert at head of bucket chain
        LChains[LI + 1] := LBuckets[LHashVal];
        LBuckets[LHashVal] := Cardinal(LI + 1);
      end;

      LHashSection.WriteData(LNBuckets);                // nbucket
      LHashSection.WriteData(Cardinal(1 + LImportCount)); // nchain
      for LI := 0 to High(LBuckets) do
        LHashSection.WriteData(LBuckets[LI]);
      for LI := 0 to High(LChains) do
        LHashSection.WriteData(LChains[LI]);

      //--------------------------------------------------------------------
      // .rela.plt — relocations (filled after offset calculation)
      // .plt, .got.plt, .dynamic — also deferred until offsets known
      //--------------------------------------------------------------------
    end;

    //========================================================================
    // STEP 3: Calculate section file offsets
    //========================================================================
    if LHasImports then
    begin
      // 5 program headers: PT_PHDR, PT_INTERP, PT_LOAD, PT_DYNAMIC, PT_GNU_STACK
      LPhdrCount := 5;
      LPhdrTableSize := Cardinal(LPhdrCount) * ELF64_PHDR_SIZE;

      LInterpFileOffset   := ELF64_EHDR_SIZE + LPhdrTableSize;
      LHashFileOffset     := LInterpFileOffset + Cardinal(LInterpSection.Size);
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
      // No imports — original layout: 1 PHDR
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

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LFuncOffsets[LI] := LTextSection.Size;

      if LFunc.IsEntryPoint then
        LMainIndex := LI;

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
                          LOutgoingArgSpace;
      // Align to 16 bytes
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

      //----------------------------------------------------------------------
      // Function prologue: PUSH RBP; MOV RBP, RSP; SUB RSP, N
      //----------------------------------------------------------------------
      EmitPushReg(REG_RBP);
      EmitMovRegReg(REG_RBP, REG_RSP);
      if LStackFrameSize > 0 then
        EmitSubRspImm32(LStackFrameSize);

      // Save incoming parameters (System V: RDI, RSI, RDX, RCX, R8, R9)
      // For large struct returns (>16 bytes): RDI = hidden return ptr, params shift
      if LFunc.ReturnSize > 16 then
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
                LLabelOffsets[LInstr.LabelTarget.Index] := LTextSection.Size;
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

              // CALL rel32 — fixup to target function
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

              // CALL rel32 to PLT entry — record fixup
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
    // STEP 8b: Backpatch import calls (CALL rel32 → PLT entry)
    //========================================================================
    if LHasImports then
    begin
      for LI := 0 to LPltFixups.Count - 1 do
      begin
        LCodeOffset := LPltFixups[LI].Key;
        LDataIndex := LPltFixups[LI].Value;

        // PLT entry offset within PLT section: PLT[0]=16 bytes + index*16
        LPltEntryFileOffset := LPltFileOffset + Cardinal(16 + LDataIndex * 16);
        // Displacement: target - (instruction address + 4)
        // Instruction address in file = LTextFileOffset + LCodeOffset
        LDisp := Int32(LPltEntryFileOffset) - Int32(LTextFileOffset + LCodeOffset + 4);

        LTextSection.Position := LCodeOffset;
        LTextSection.WriteData(LDisp);
      end;
      LTextSection.Position := LTextSection.Size;
    end;

    //========================================================================
    // STEP 9: Emit _start entry point stub
    //========================================================================
    LEntryPointOffset := LTextSection.Size;

    if LMainIndex >= 0 then
    begin
      LMainOffset := LFuncOffsets[LMainIndex];

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
      // No main — exit with 0
      EmitXorRegReg(REG_RDI);
      EmitMovRegImm64(REG_RAX, 60);
      EmitByte($0F);
      EmitByte($05);
    end;

    LTextSize := LTextSection.Size;

    //========================================================================
    // STEP 10: Build deferred dynamic sections & assemble ELF file
    //========================================================================
    LEntryVAddr := BASE_VADDR + LTextFileOffset + LEntryPointOffset;

    if LHasImports then
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
      // PLT[0] — resolver stub (16 bytes)
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

      // PLT[1..N] — per-import stubs (16 bytes each)
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
      LSectionCount := 13;  // null + 12 sections

      LTotalFileSize := LShdrsFileOffset + Cardinal(LSectionCount) * ELF64_SHDR_SIZE;
    end
    else
    begin
      // No imports — simple layout
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

      LResult.WriteData(Word(ET_EXEC));        // e_type
      LResult.WriteData(Word(EM_X86_64));      // e_machine
      LResult.WriteData(Cardinal(EV_CURRENT)); // e_version
      LResult.WriteData(UInt64(LEntryVAddr));  // e_entry
      LResult.WriteData(UInt64(ELF64_EHDR_SIZE)); // e_phoff
      if LHasImports then
        LResult.WriteData(UInt64(LShdrsFileOffset))  // e_shoff
      else
        LResult.WriteData(UInt64(0));                // e_shoff (no section headers)
      LResult.WriteData(Cardinal(0));          // e_flags
      LResult.WriteData(Word(ELF64_EHDR_SIZE)); // e_ehsize
      LResult.WriteData(Word(ELF64_PHDR_SIZE)); // e_phentsize
      LResult.WriteData(Word(LPhdrCount));     // e_phnum
      if LHasImports then
      begin
        LResult.WriteData(Word(ELF64_SHDR_SIZE)); // e_shentsize
        LResult.WriteData(Word(LSectionCount));    // e_shnum
        LResult.WriteData(Word(12));               // e_shstrndx (.shstrtab index)
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
      if LHasImports then
      begin
        // PT_PHDR — program header table itself
        LResult.WriteData(Cardinal(PT_PHDR));
        LResult.WriteData(Cardinal(PF_R));
        LResult.WriteData(UInt64(ELF64_EHDR_SIZE));
        LResult.WriteData(UInt64(BASE_VADDR + ELF64_EHDR_SIZE));
        LResult.WriteData(UInt64(BASE_VADDR + ELF64_EHDR_SIZE));
        LResult.WriteData(UInt64(LPhdrTableSize));
        LResult.WriteData(UInt64(LPhdrTableSize));
        LResult.WriteData(UInt64(8));

        // PT_INTERP
        LResult.WriteData(Cardinal(PT_INTERP));
        LResult.WriteData(Cardinal(PF_R));
        LResult.WriteData(UInt64(LInterpFileOffset));
        LResult.WriteData(UInt64(BASE_VADDR + LInterpFileOffset));
        LResult.WriteData(UInt64(BASE_VADDR + LInterpFileOffset));
        LResult.WriteData(UInt64(LInterpSection.Size));
        LResult.WriteData(UInt64(LInterpSection.Size));
        LResult.WriteData(UInt64(1));

        // PT_LOAD — entire file (RWX)
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

        // PT_GNU_STACK — non-executable stack
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
      if LHasImports then
      begin
        // .interp
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

      if LHasImports then
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

      if LHasImports then
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
        // [1] .interp
        WriteShdr(LResult, 1, SHT_PROGBITS, SHF_ALLOC,
          BASE_VADDR + LInterpFileOffset, LInterpFileOffset,
          Cardinal(LInterpSection.Size), 0, 0, 1, 0);
        // [2] .hash
        WriteShdr(LResult, 9, SHT_HASH, SHF_ALLOC,
          BASE_VADDR + LHashFileOffset, LHashFileOffset,
          Cardinal(LHashSection.Size), 3, 0, 8, 4);
        // [3] .dynsym
        WriteShdr(LResult, 15, SHT_DYNSYM, SHF_ALLOC,
          BASE_VADDR + LDynsymFileOffset, LDynsymFileOffset,
          Cardinal(LDynsymSection.Size), 4, 1, 8, ELF64_SYM_SIZE);
        // [4] .dynstr
        WriteShdr(LResult, 23, SHT_STRTAB, SHF_ALLOC,
          BASE_VADDR + LDynstrFileOffset, LDynstrFileOffset,
          Cardinal(LDynstrSection.Size), 0, 0, 1, 0);
        // [5] .rela.plt
        WriteShdr(LResult, 31, SHT_RELA, SHF_ALLOC or SHF_INFO_LINK,
          BASE_VADDR + LRelaPltFileOffset, LRelaPltFileOffset,
          Cardinal(LRelaPltSection.Size), 3, 8, 8, ELF64_RELA_SIZE);
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

end.
