{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend.Win64;

{$I Tiger.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
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
  Tiger.ABI.Win64;

type
  { TTigerWin64Backend }
  TTigerWin64Backend = class(TTigerBackend)
  private
    function GeneratePE(): TBytes;
    function GenerateCOFF(): TBytes;
    function GenerateLib(): TBytes;

  public
    function BuildToMemory(): TBytes; override;
    function Run(): Cardinal; override;
    procedure Clear(); override;
    function TargetExe(const APath: string; const ASubsystem: TTigerSubsystem = ssConsole): TTigerBackend; override;
  end;

implementation

uses
  Tiger.ABI,
  Tiger.Linker,
  Tiger.Linker.COFF;

const
  // Backend error codes
  ERR_BACKEND_NO_ACTIVE_FUNC = 'B001';

  //============================================================================
  // PE Constants (Internal)
  //============================================================================
  IMAGE_DOS_SIGNATURE = $5A4D;
  IMAGE_NT_SIGNATURE = $00004550;

  IMAGE_FILE_MACHINE_AMD64 = $8664;

  IMAGE_FILE_EXECUTABLE_IMAGE = $0002;
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;
  IMAGE_FILE_DLL = $2000;

  IMAGE_SUBSYSTEM_WINDOWS_GUI = 2;
  IMAGE_SUBSYSTEM_WINDOWS_CUI = 3;

  IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA = $0020;
  IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE = $0040;
  IMAGE_DLLCHARACTERISTICS_NX_COMPAT = $0100;
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;

  IMAGE_SCN_CNT_CODE = $00000020;
  IMAGE_SCN_CNT_INITIALIZED_DATA = $00000040;
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = $00000080;
  IMAGE_SCN_MEM_DISCARDABLE = $02000000;
  IMAGE_SCN_MEM_EXECUTE = $20000000;
  IMAGE_SCN_MEM_READ = $40000000;
  IMAGE_SCN_MEM_WRITE = $80000000;

  IMAGE_DIRECTORY_ENTRY_EXPORT = 0;
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1;
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3;
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5;
  IMAGE_DIRECTORY_ENTRY_IAT = 12;

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

  PE_DEFAULT_IMAGE_BASE: UInt64 = $0000000140000000;
  PE_DEFAULT_SECTION_ALIGNMENT = $1000;
  PE_DEFAULT_FILE_ALIGNMENT = $200;

type
  //============================================================================
  // PE Structures (Internal)
  //============================================================================

  TImageDosHeader = packed record
    e_magic: Word;
    e_cblp: Word;
    e_cp: Word;
    e_crlc: Word;
    e_cparhdr: Word;
    e_minalloc: Word;
    e_maxalloc: Word;
    e_ss: Word;
    e_sp: Word;
    e_csum: Word;
    e_ip: Word;
    e_cs: Word;
    e_lfarlc: Word;
    e_ovno: Word;
    e_res: array[0..3] of Word;
    e_oemid: Word;
    e_oeminfo: Word;
    e_res2: array[0..9] of Word;
    e_lfanew: Cardinal;
  end;

  TImageFileHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: Cardinal;
    PointerToSymbolTable: Cardinal;
    NumberOfSymbols: Cardinal;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;

  TImageDataDirectory = packed record
    VirtualAddress: Cardinal;
    Size: Cardinal;
  end;

  TImageOptionalHeader64 = packed record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Cardinal;
    SizeOfInitializedData: Cardinal;
    SizeOfUninitializedData: Cardinal;
    AddressOfEntryPoint: Cardinal;
    BaseOfCode: Cardinal;
    ImageBase: UInt64;
    SectionAlignment: Cardinal;
    FileAlignment: Cardinal;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: Cardinal;
    SizeOfImage: Cardinal;
    SizeOfHeaders: Cardinal;
    CheckSum: Cardinal;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: UInt64;
    SizeOfStackCommit: UInt64;
    SizeOfHeapReserve: UInt64;
    SizeOfHeapCommit: UInt64;
    LoaderFlags: Cardinal;
    NumberOfRvaAndSizes: Cardinal;
    DataDirectory: array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;

  TImageSectionHeader = packed record
    SectionName: array[0..7] of AnsiChar;
    VirtualSize: Cardinal;
    VirtualAddress: Cardinal;
    SizeOfRawData: Cardinal;
    PointerToRawData: Cardinal;
    PointerToRelocations: Cardinal;
    PointerToLinenumbers: Cardinal;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: Cardinal;
  end;

  TImageImportDescriptor = packed record
    OriginalFirstThunk: Cardinal;
    TimeDateStamp: Cardinal;
    ForwarderChain: Cardinal;
    NameRVA: Cardinal;
    FirstThunk: Cardinal;
  end;

//==============================================================================
// Handle Types Implementation
//==============================================================================



//==============================================================================
// TTigerWin64Backend
//==============================================================================

function TTigerWin64Backend.TargetExe(const APath: string; const ASubsystem: TTigerSubsystem): TTigerBackend;
begin
  inherited;
  FOutputPath := TPath.ChangeExtension(FOutputPath, '.exe');
  Result := Self;
end;

function TTigerWin64Backend.BuildToMemory(): TBytes;
begin
  case FOutputType of
    otExe, otDll:
      Result := GeneratePE();
    otObj:
      Result := GenerateCOFF();
    otLib:
      Result := GenerateLib();
  else
    SetLength(Result, 0);
  end;
end;

function TTigerWin64Backend.Run(): Cardinal;
begin
  if FOutputType <> otExe then
    Exit(ERROR_BAD_FORMAT);

  try
    Result := TWin64Utils.RunExe(FOutputPath, '', ExtractFilePath(FOutputPath));
  except
    on E: Exception do
    begin
      Status('Run failed: %s', [E.Message]);
      Result := ERROR_FILE_NOT_FOUND;
    end;
  end;
end;

procedure TTigerWin64Backend.Clear();
begin
  inherited Clear();
end;

function TTigerWin64Backend.GeneratePE(): TBytes;
const

  DOS_STUB: array[0..63] of Byte = (
    $0E, $1F, $BA, $0E, $00, $B4, $09, $CD, $21, $B8, $01, $4C, $CD, $21,
    $54, $68, $69, $73, $20, $70, $72, $6F, $67, $72, $61, $6D, $20, $63,
    $61, $6E, $6E, $6F, $74, $20, $62, $65, $20, $72, $75, $6E, $20, $69,
    $6E, $20, $44, $4F, $53, $20, $6D, $6F, $64, $65, $2E, $0D, $0D, $0A,
    $24, $00, $00, $00, $00, $00, $00, $00
  );

  // x64 UNWIND_INFO structure (same as TCC)
  // All functions share this since they have identical stack frame setup
  UNWIND_INFO: array[0..7] of Byte = (
    $01,  // Version=1, Flags=0
    $04,  // SizeOfProlog=4 (push rbp; mov rbp,rsp)
    $02,  // CountOfCodes=2
    $05,  // FrameRegister=RBP(5), FrameOffset=0
    $04, $03,  // Offset 4: UWOP_SET_FPREG (mov rbp,rsp)
    $01, $50   // Offset 1: UWOP_PUSH_NONVOL RBP (push rbp)
  );
var
  LOutput: TMemoryStream;
  LTextSection: TMemoryStream;
  LRDataSection: TMemoryStream;
  LDataSection: TMemoryStream;
  LPDataSection: TMemoryStream;
  LIDataSection: TMemoryStream;
  LEdataSection: TMemoryStream;
  LRelocSection: TMemoryStream;

  // Section RVAs and sizes (calculated during layout)
  LHeadersSize: Cardinal;
  LTextRVA, LTextSize, LTextFileSize: Cardinal;
  LRDataRVA, LRDataSize, LRDataFileSize: Cardinal;
  LDataRVA, LDataSize, LDataFileSize: Cardinal;
  LPDataRVA, LPDataSize, LPDataFileSize: Cardinal;
  LIDataRVA, LIDataSize, LIDataFileSize: Cardinal;
  LEdataRVA, LEdataSize, LEdataFileSize: Cardinal;
  LRelocRVA, LRelocSize, LRelocFileSize: Cardinal;
  LSizeOfImage: Cardinal;
  LUnwindInfoRVA: Cardinal;
  LEntryPointRVA: Cardinal;
  LHasExports: Boolean;
  LHasImports: Boolean;
  LHasReloc: Boolean;
  LNumExports: Integer;

  // Export table layout
  LExportFuncs: TList<TPair<Integer, string>>;  // Function index, export name
  LExportDirOffset: Cardinal;
  LExportAddressTableOffset: Cardinal;
  LExportNameTableOffset: Cardinal;
  LExportOrdinalTableOffset: Cardinal;
  LExportNamesOffset: Cardinal;
  LDllNameOffset: Cardinal;
  LExportName: string;

  // Import table layout
  LImportDescriptorsOffset: Cardinal;
  LDllNamesOffset: Cardinal;
  LHintNamesOffset: Cardinal;
  LIATOffset: Cardinal;
  LINTOffset: Cardinal;
  LImportDlls: TStringList;
  LImportsByDll: TObjectList<TStringList>;
  LIATEntries: TList<Cardinal>;  // RVA for each import's IAT slot

  LI, LJ, LK: Integer;
  LFunc: TTigerFuncInfo;
  LParamTypes: TArray<TTigerValueType>;
  LInstr: TTigerInstruction;
  LEntry: TTigerImportEntry;

  // Fixup tracking
  LFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> Import index (for IAT calls)
  LDataFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> Data index (for .rdata)
  LGlobalFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> Global index (for .data)
  LLabelOffsets: TArray<Cardinal>;  // Label index -> code offset (current function)
  LAllLabelOffsets: TArray<TArray<Cardinal>>;  // All functions' label offsets (for SEH)
  LJumpFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> Label index
  LFuncOffsets: TArray<Cardinal>;  // Function index -> code offset
  LFuncUnwindInfoOffsets: TArray<Cardinal>;  // Function index -> UNWIND_INFO offset (for SEH)
  LHasSEH: Boolean;                             // True if any function has exception scopes
  LSEHHandlerImport: TTigerImportHandle;          // Handle to __C_specific_handler import
  LSEHThunkOffset: Cardinal;                    // Offset of SEH dispatcher thunk in .text
  LSEHHandlerFixups: TList<Cardinal>;           // Offsets of ExceptionHandler fields to fixup
  LSEHScopeTableFixups: TList<TPair<Cardinal, Integer>>;  // (offset, count) for SCOPE_TABLE fixups
  LScope: TTigerExceptionScope;                   // Temp for iterating exception scopes
  LScopeCount: Integer;                         // Count of scopes in current function
  LInitExceptionsIndex: Integer;                // Function index of Pax_InitExceptions
  LCallFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> Function index
  LFuncAddrFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> Function index (for LEA to func)

  // Import fixup variables
  LHintNamePos: Cardinal;
  LIATPos: Cardinal;
  LINTPos: Cardinal;
  LDllNamePos: Cardinal;
  LFuncCount: Integer;

  // Code relocation variables
  LCodeOffset: Cardinal;
  LImportIndex: Integer;
  LIATSlotOffset: Cardinal;
  LIATSlotRVA: Cardinal;
  LInstrRVA: Cardinal;
  LRipAfter: Cardinal;
  LDisp: Int32;
  LDataIndex: Integer;
  LDataEntryRec: TTigerDataEntry;
  LTargetRVA: Cardinal;
  LDataHandle: TTigerDataHandle;

  // PE header variables
  LDosHeader: TImageDosHeader;
  LFileHeader: TImageFileHeader;
  LOptHeader: TImageOptionalHeader64;
  LSectionHeader: TImageSectionHeader;
  LSubsystemValue: Word;

  // PE checksum calculation
  LChecksum: Cardinal;
  LChecksumPos: Cardinal;

  // Stack frame
  LStackFrameSize: Cardinal;
  LLocalsSize: Integer;
  LMaxCallArgs: Integer;
  LOutgoingArgSpace: Cardinal;

  // Static linker integration
  LLinker: TTigerCOFFLinker;
  LStaticImportIndices: TList<Integer>;       // Import indices that are static (.lib/.obj)
  LStaticSymbolNames: TStringList;            // Symbol names needed from static libs
  LStaticLibPaths: TStringList;               // Unique .lib paths collected from imports
  LStaticObjPaths: TStringList;               // Unique .obj paths collected from imports
  LStaticResolved: TDictionary<string, TLinkerResolvedSymbol>;  // Resolved symbol map
  LStaticImportResolved: TDictionary<Integer, Cardinal>;  // Import index → offset in merged .text
  LExternalTextBase: Cardinal;                // Where merged external .text starts in LTextSection
  LExternalRDataBase: Cardinal;               // Where merged external .rdata starts in LRDataSection
  LExternalDataBase: Cardinal;                // Where merged external .data starts in LDataSection
  LHasStaticImports: Boolean;
  LMergedBytes: TBytes;
  LPendingRelocs: TList<TLinkerPendingReloc>;
  LPendReloc: TLinkerPendingReloc;
  LSiteRVA: Cardinal;
  LResolvedSym: TLinkerResolvedSymbol;
  LSymTargetRVA: Cardinal;
  LRel32: Int32;
  LLibPath: string;
  LResolvedPath: string;
  LCandidate: string;
  LStaticFuncName: string;

  //----------------------------------------------------------------------------
  // Helper: Emit byte to code section
  //----------------------------------------------------------------------------
  procedure EmitByte(const AValue: Byte);
  begin
    LTextSection.WriteData(AValue);
  end;

  //----------------------------------------------------------------------------
  // Helper: Emit word to code section
  //----------------------------------------------------------------------------
  procedure EmitWord(const AValue: Word);
  begin
    LTextSection.WriteData(AValue);
  end;

  //----------------------------------------------------------------------------
  // Helper: Emit dword to code section
  //----------------------------------------------------------------------------
  procedure EmitDWord(const AValue: Cardinal);
  begin
    LTextSection.WriteData(AValue);
  end;

  //----------------------------------------------------------------------------
  // Helper: Emit qword to code section
  //----------------------------------------------------------------------------
  procedure EmitQWord(const AValue: UInt64);
  begin
    LTextSection.WriteData(AValue);
  end;

  //----------------------------------------------------------------------------
  // Helper: Emit int32 to code section
  //----------------------------------------------------------------------------
  procedure EmitInt32(const AValue: Int32);
  begin
    LTextSection.WriteData(AValue);
  end;

  //----------------------------------------------------------------------------
  // Helper: Emit REX prefix
  //----------------------------------------------------------------------------
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

  //----------------------------------------------------------------------------
  // Helper: Emit ModRM byte
  //----------------------------------------------------------------------------
  procedure EmitModRM(const AMod, AReg, ARM: Byte);
  begin
    EmitByte((AMod shl 6) or ((AReg and $07) shl 3) or (ARM and $07));
  end;

  //----------------------------------------------------------------------------
  // Helper: SUB RSP, imm32
  //----------------------------------------------------------------------------
  procedure EmitSubRspImm32(const AImm: Int32);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($81);                        // SUB r/m64, imm32
    EmitModRM(3, 5, REG_RSP);            // ModRM: reg=5 (SUB), rm=RSP
    EmitInt32(AImm);
  end;

  //----------------------------------------------------------------------------
  // Helper: ADD RSP, imm32
  //----------------------------------------------------------------------------
  procedure EmitAddRspImm32(const AImm: Int32);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($81);                        // ADD r/m64, imm32
    EmitModRM(3, 0, REG_RSP);            // ModRM: reg=0 (ADD), rm=RSP
    EmitInt32(AImm);
  end;

  //----------------------------------------------------------------------------
  // Helper: LEA reg, [RIP+disp32] - Load effective address RIP-relative
  //----------------------------------------------------------------------------
  procedure EmitLeaRipRel(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8D);                        // LEA
    EmitModRM(0, AReg and 7, 5);         // mod=00, rm=101 means [RIP+disp32]
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV reg64, imm64
  //----------------------------------------------------------------------------
  procedure EmitMovRegImm64(const AReg: Byte; const AImm: UInt64);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($B8 + (AReg and 7));        // MOV r64, imm64
    EmitQWord(AImm);
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV reg32, imm32 (zero-extends to 64-bit)
  //----------------------------------------------------------------------------
  procedure EmitMovRegImm32(const AReg: Byte; const AImm: Cardinal);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($B8 + (AReg and 7));        // MOV r32, imm32
    EmitDWord(AImm);
  end;

  //----------------------------------------------------------------------------
  // Helper: XOR reg32, reg32 (zero-extends, shorter than MOV reg, 0)
  //----------------------------------------------------------------------------
  procedure EmitXorRegReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, AReg >= 8, False, AReg >= 8);
    EmitByte($31);                        // XOR r/m32, r32
    EmitModRM(3, AReg and 7, AReg and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: CALL [RIP+disp32] - Indirect call through memory
  //----------------------------------------------------------------------------
  procedure EmitCallIndirectRipRel(const ADisp: Int32);
  begin
    EmitByte($FF);                        // CALL r/m64
    EmitModRM(0, 2, 5);                  // mod=00, reg=2 (CALL), rm=101 (RIP+disp32)
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: CALL reg64 - Indirect call through register
  //----------------------------------------------------------------------------
  procedure EmitCallReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);  // REX.B for R8-R15
    EmitByte($FF);                        // CALL r/m64
    EmitModRM(3, 2, AReg and 7);          // mod=11, reg=2 (CALL), rm=register
  end;

  //----------------------------------------------------------------------------
  // Helper: RET
  //----------------------------------------------------------------------------
  procedure EmitRet();
  begin
    EmitByte($C3);
  end;

  //----------------------------------------------------------------------------
  // Helper: INT3 (breakpoint/padding)
  //----------------------------------------------------------------------------
  procedure EmitInt3();
  begin
    EmitByte($CC);
  end;

  //----------------------------------------------------------------------------
  // Helper: PUSH reg64
  //----------------------------------------------------------------------------
  procedure EmitPushReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($50 + (AReg and 7));
  end;

  //----------------------------------------------------------------------------
  // Helper: POP reg64
  //----------------------------------------------------------------------------
  procedure EmitPopReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($58 + (AReg and 7));
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitMovRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($89);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV reg64, [RBP-disp32] (load from stack)
  //----------------------------------------------------------------------------
  procedure EmitMovRegRbpDisp(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8B);  // MOV r64, r/m64
    EmitModRM(2, AReg and 7, REG_RBP);  // mod=10 (disp32), rm=RBP
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV [RBP-disp32], reg64 (store to stack)
  //----------------------------------------------------------------------------
  procedure EmitMovRbpDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($89);  // MOV r/m64, r64
    EmitModRM(2, AReg and 7, REG_RBP);  // mod=10 (disp32), rm=RBP
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV [RSP+disp32], reg64 (store to stack, RSP-relative)
  // Used for passing stack arguments in Win64 ABI
  //----------------------------------------------------------------------------
  procedure EmitMovRspDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($89);  // MOV r/m64, r64
    EmitModRM(2, AReg and 7, 4);  // mod=10 (disp32), rm=100 (SIB follows)
    EmitByte($24);  // SIB: scale=00, index=100 (none), base=100 (RSP)
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: Align stream to boundary
  //----------------------------------------------------------------------------
  procedure AlignStream(const AStream: TMemoryStream; const AAlign: Cardinal);
  var
    LPad: Cardinal;
  begin
    if AAlign <= 1 then Exit;
    LPad := AStream.Size mod AAlign;
    if LPad > 0 then
    begin
      LPad := AAlign - LPad;
      while LPad > 0 do
      begin
        AStream.WriteData(Byte(0));
        Dec(LPad);
      end;
    end;
  end;

  //----------------------------------------------------------------------------
  // Helper: Align up
  //----------------------------------------------------------------------------
  function AlignUp(const AValue, AAlign: Cardinal): Cardinal;
  begin
    if (AAlign = 0) or ((AValue mod AAlign) = 0) then
      Result := AValue
    else
      Result := AValue + (AAlign - (AValue mod AAlign));
  end;

  //----------------------------------------------------------------------------
  // Helper: Write string to stream (null-terminated)
  //----------------------------------------------------------------------------
  procedure WriteString(const AStream: TMemoryStream; const AStr: AnsiString);
  begin
    if Length(AStr) > 0 then
      AStream.WriteBuffer(AStr[1], Length(AStr));
    AStream.WriteData(Byte(0));
  end;

  //----------------------------------------------------------------------------
  // Helper: Calculate local variable stack offset
  // Layout: params first, then locals
  // Non-variadic: [RBP-40] = param 0, [RBP-48] = param 1, etc.
  // Variadic: [RBP-40] = hidden count, [RBP-48] = param 0, etc.
  // [RBP-(40 + numParams*8)] = local 0, etc.
  //----------------------------------------------------------------------------
  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LK: Integer;
    LOffset: Integer;
  begin
    // Calculate offset by summing sizes of previous locals
    // For variadic functions, always reserve space for all 4 register args
    // (hidden count + up to 3 declared params/varargs)
    if LFunc.IsVariadic then
      LOffset := 40 + 4 * 8  // Always reserve 4 slots for variadic
    else
      LOffset := 40 + Length(LFunc.Params) * 8;
    for LK := 0 to AIndex - 1 do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -LOffset;
  end;

  //----------------------------------------------------------------------------
  // Helper: Calculate parameter stack offset
  // Non-variadic: [RBP-40] = param 0 (RCX), [RBP-48] = param 1 (RDX), etc.
  // Variadic: [RBP-40] = hidden count, [RBP-48] = param 0, etc.
  //----------------------------------------------------------------------------
  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    if LFunc.IsVariadic then
      // Variadic: hidden count at [RBP-40], declared params shifted by 1
      Result := -(40 + (AIndex + 1) * 8)
    else
      Result := -(40 + AIndex * 8);
  end;

  //----------------------------------------------------------------------------
  // Helper: Calculate temp variable stack offset
  // Layout: temps come after params and locals
  //----------------------------------------------------------------------------
  function GetTempOffset(const AIndex: Integer): Int32;
  var
    LK: Integer;
    LOffset: Integer;
  begin
    // Calculate offset by summing all local sizes
    // For variadic functions, always reserve space for all 4 register args
    if LFunc.IsVariadic then
      LOffset := 40 + 4 * 8  // Always reserve 4 slots for variadic
    else
      LOffset := 40 + Length(LFunc.Params) * 8;
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    LOffset := LOffset + AIndex * 8;
    Result := -LOffset;
  end;

  //----------------------------------------------------------------------------
  // Helper: Store register to temp slot
  //----------------------------------------------------------------------------
  procedure StoreTempFromReg(const ATempIndex: Integer; const AReg: Byte);
  begin
    EmitMovRbpDispReg(GetTempOffset(ATempIndex), AReg);
  end;

  //----------------------------------------------------------------------------
  // Helper: ADD reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitAddRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($01);  // ADD r/m64, r64
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: SUB reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitSubRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($29);  // SUB r/m64, r64
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: IMUL reg64, reg64 (signed multiply)
  //----------------------------------------------------------------------------
  procedure EmitImulRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ADest >= 8, False, ASrc >= 8);
    EmitByte($0F);
    EmitByte($AF);  // IMUL r64, r/m64
    EmitModRM(3, ADest and 7, ASrc and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: CQO (sign-extend RAX into RDX:RAX for division)
  //----------------------------------------------------------------------------
  procedure EmitCqo();
  begin
    EmitRex(True, False, False, False);
    EmitByte($99);  // CQO
  end;

  //----------------------------------------------------------------------------
  // Helper: IDIV reg64 (signed divide RDX:RAX by reg, result in RAX, remainder in RDX)
  //----------------------------------------------------------------------------
  procedure EmitIdivReg(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($F7);  // IDIV r/m64
    EmitModRM(3, 7, AReg and 7);  // reg=7 for IDIV
  end;

  //----------------------------------------------------------------------------
  // Helper: AND reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitAndRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($21);  // AND r/m64, r64
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: OR reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitOrRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($09);  // OR r/m64, r64
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: XOR reg64, reg64 (for bitwise XOR, not zeroing)
  //----------------------------------------------------------------------------
  procedure EmitXorRegReg64(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($31);  // XOR r/m64, r64
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: NOT reg64
  //----------------------------------------------------------------------------
  procedure EmitNotReg(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($F7);  // NOT r/m64
    EmitModRM(3, 2, AReg and 7);  // reg=2 for NOT
  end;

  //----------------------------------------------------------------------------
  // Helper: SHL reg64, CL
  //----------------------------------------------------------------------------
  procedure EmitShlRegCl(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($D3);  // SHL r/m64, CL
    EmitModRM(3, 4, AReg and 7);  // reg=4 for SHL
  end;

  //----------------------------------------------------------------------------
  // Helper: SHR reg64, CL
  //----------------------------------------------------------------------------
  procedure EmitShrRegCl(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($D3);  // SHR r/m64, CL
    EmitModRM(3, 5, AReg and 7);  // reg=5 for SHR
  end;

  //----------------------------------------------------------------------------
  // Helper: REP MOVSB - copy RCX bytes from [RSI] to [RDI]
  //----------------------------------------------------------------------------
  procedure EmitRepMovsb();
  begin
    EmitByte($F3);  // REP prefix
    EmitByte($A4);  // MOVSB
  end;

  //----------------------------------------------------------------------------
  // Helper: CMP reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitCmpRegReg(const AReg1, AReg2: Byte);
  begin
    EmitRex(True, AReg2 >= 8, False, AReg1 >= 8);
    EmitByte($39);  // CMP r/m64, r64
    EmitModRM(3, AReg2 and 7, AReg1 and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: SETE AL (set if equal)
  //----------------------------------------------------------------------------
  procedure EmitSete();
  begin
    EmitByte($0F);
    EmitByte($94);  // SETE r/m8
    EmitModRM(3, 0, REG_RAX);  // AL
  end;

  //----------------------------------------------------------------------------
  // Helper: SETNE AL (set if not equal)
  //----------------------------------------------------------------------------
  procedure EmitSetne();
  begin
    EmitByte($0F);
    EmitByte($95);  // SETNE r/m8
    EmitModRM(3, 0, REG_RAX);  // AL
  end;

  //----------------------------------------------------------------------------
  // Helper: SETL AL (set if less, signed)
  //----------------------------------------------------------------------------
  procedure EmitSetl();
  begin
    EmitByte($0F);
    EmitByte($9C);  // SETL r/m8
    EmitModRM(3, 0, REG_RAX);  // AL
  end;

  //----------------------------------------------------------------------------
  // Helper: SETLE AL (set if less or equal, signed)
  //----------------------------------------------------------------------------
  procedure EmitSetle();
  begin
    EmitByte($0F);
    EmitByte($9E);  // SETLE r/m8
    EmitModRM(3, 0, REG_RAX);  // AL
  end;

  //----------------------------------------------------------------------------
  // Helper: SETG AL (set if greater, signed)
  //----------------------------------------------------------------------------
  procedure EmitSetg();
  begin
    EmitByte($0F);
    EmitByte($9F);  // SETG r/m8
    EmitModRM(3, 0, REG_RAX);  // AL
  end;

  //----------------------------------------------------------------------------
  // Helper: SETGE AL (set if greater or equal, signed)
  //----------------------------------------------------------------------------
  procedure EmitSetge();
  begin
    EmitByte($0F);
    EmitByte($9D);  // SETGE r/m8
    EmitModRM(3, 0, REG_RAX);  // AL
  end;

  //----------------------------------------------------------------------------
  // Helper: MOVZX EAX, AL (zero-extend AL to RAX)
  //----------------------------------------------------------------------------
  procedure EmitMovzxEaxAl();
  begin
    EmitByte($0F);
    EmitByte($B6);  // MOVZX r32, r/m8
    EmitModRM(3, REG_RAX, REG_RAX);  // EAX, AL
  end;

  //----------------------------------------------------------------------------
  // Helper: MOVSXD RAX, EAX (sign-extend 32-bit to 64-bit)
  //----------------------------------------------------------------------------
  procedure EmitMovsxdRaxEax();
  begin
    EmitRex(True, False, False, False);  // REX.W for 64-bit destination
    EmitByte($63);  // MOVSXD r64, r/m32
    EmitModRM(3, REG_RAX, REG_RAX);  // RAX, EAX
  end;

  //----------------------------------------------------------------------------
  // Helper: TEST reg64, reg64
  //----------------------------------------------------------------------------
  procedure EmitTestRegReg(const AReg1, AReg2: Byte);
  begin
    EmitRex(True, AReg2 >= 8, False, AReg1 >= 8);
    EmitByte($85);  // TEST r/m64, r64
    EmitModRM(3, AReg2 and 7, AReg1 and 7);
  end;

  //----------------------------------------------------------------------------
  // Helper: JMP rel32 (unconditional jump)
  //----------------------------------------------------------------------------
  procedure EmitJmpRel32(const ADisp: Int32);
  begin
    EmitByte($E9);  // JMP rel32
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: JZ rel32 (jump if zero)
  //----------------------------------------------------------------------------
  procedure EmitJzRel32(const ADisp: Int32);
  begin
    EmitByte($0F);
    EmitByte($84);  // JZ rel32
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: JNZ rel32 (jump if not zero)
  //----------------------------------------------------------------------------
  procedure EmitJnzRel32(const ADisp: Int32);
  begin
    EmitByte($0F);
    EmitByte($85);  // JNZ rel32
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: CALL rel32 (direct call)
  //----------------------------------------------------------------------------
  procedure EmitCallRel32(const ADisp: Int32);
  begin
    EmitByte($E8);  // CALL rel32
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: LEA reg64, [RBP+disp32] (load address of stack location)
  //----------------------------------------------------------------------------
  procedure EmitLeaRegRbpDisp(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8D);  // LEA r64, m
    EmitModRM(2, AReg and 7, REG_RBP);  // mod=10 (disp32), rm=RBP
    EmitInt32(ADisp);
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV reg64, [reg64] (load from pointer)
  //----------------------------------------------------------------------------
  procedure EmitMovRegMemReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ADest >= 8, False, ASrc >= 8);
    EmitByte($8B);  // MOV r64, r/m64
    EmitModRM(0, ADest and 7, ASrc and 7);  // mod=00 (indirect)
  end;

  //----------------------------------------------------------------------------
  // Helper: MOV [reg64], reg64 (store to pointer)
  //----------------------------------------------------------------------------
  procedure EmitMovMemRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($89);  // MOV r/m64, r64
    EmitModRM(0, ASrc and 7, ADest and 7);  // mod=00 (indirect)
  end;

  //----------------------------------------------------------------------------
  // Variadic Helpers
  //----------------------------------------------------------------------------

  // ADD RAX, imm32
  procedure EmitAddRaxImm32(const AValue: Int32);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($05);  // ADD RAX, imm32
    EmitInt32(AValue);
  end;

  // SHL RAX, imm8
  procedure EmitShlRaxImm8(const AValue: Byte);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($C1);  // SHL r/m64, imm8
    EmitModRM(3, 4, 0);  // /4 = SHL, RAX
    EmitByte(AValue);
  end;

  // NEG RAX
  procedure EmitNegRax();
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($F7);  // NEG r/m64
    EmitModRM(3, 3, 0);  // /3 = NEG, RAX
  end;

  // MOV reg64, [RBP + RAX]
  procedure EmitMovRegRbpPlusRax(const ADest: Byte);
  begin
    EmitRex(True, ADest >= 8, False, False);
    EmitByte($8B);  // MOV r64, r/m64
    EmitByte($44);  // ModR/M: [base + index + disp8]
    EmitByte($05);  // SIB: scale=1, index=RAX, base=RBP
    EmitByte($00);  // disp8 = 0
  end;

  // CMP RAX, imm32
  procedure EmitCmpRaxImm32(const AValue: Int32);
  begin
    EmitRex(True, False, False, False);  // REX.W
    EmitByte($3D);  // CMP RAX, imm32
    EmitInt32(AValue);
  end;

  // JGE rel8 (jump if greater or equal, signed)
  procedure EmitJgeRel8(const ADisp: Int8);
  begin
    EmitByte($7D);  // JGE rel8
    EmitByte(Byte(ADisp));
  end;

  // JMP rel8 (short jump)
  procedure EmitJmpRel8(const ADisp: Int8);
  begin
    EmitByte($EB);  // JMP rel8
    EmitByte(Byte(ADisp));
  end;

  //----------------------------------------------------------------------------
  // Helper: Load operand into register for function argument
  //----------------------------------------------------------------------------
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
          // LEA reg, [RIP+offset_to_rdata]
          // Offset will be fixed up later
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder displacement
        end;

      okGlobal:
        begin
          // LEA reg, [RIP+offset_to_data]
          // Offset will be fixed up later (points to writable .data section)
          LGlobalFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder displacement
        end;

      okLocal:
        begin
          // Load from stack: MOV reg, [RBP-offset]
          if AOp.LocalHandle.IsParam then
            EmitMovRegRbpDisp(AReg, GetParamOffset(AOp.LocalHandle.Index))
          else
            EmitMovRegRbpDisp(AReg, GetLocalOffset(AOp.LocalHandle.Index));
        end;

      okTemp:
        begin
          // Load from stack: MOV reg, [RBP-offset]
          EmitMovRegRbpDisp(AReg, GetTempOffset(AOp.TempHandle.Index));
        end;

      okFunc:
        begin
          // LEA reg, [RIP+offset_to_func]
          // Offset will be fixed up later
          LFuncAddrFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, AOp.FuncHandle.Index));
          EmitLeaRipRel(AReg, 0);  // Placeholder displacement
        end;
    else
      EmitXorRegReg(AReg);
    end;
  end;

  //----------------------------------------------------------------------------
  // Helper: Load call argument to register, handling large struct params
  // Win64 ABI: Large structs (>8 bytes) are passed by pointer
  //----------------------------------------------------------------------------
  procedure LoadCallArgToReg(const AOp: TTigerOperand; const AReg: Byte;
    const ATargetFuncIndex: Integer; const AArgIndex: Integer);
  var
    LNeedsAddress: Boolean;
    LLocalInfo: TTigerLocalInfo;
  begin
    LNeedsAddress := False;
    
    // Check if source is a local struct that needs its address passed
    // Win64 ABI: structs >8 bytes are passed by pointer
    if (AOp.Kind = okLocal) and (not AOp.LocalHandle.IsParam) then
    begin
      // Get the local's size info
      LLocalInfo := LFunc.Locals[AOp.LocalHandle.Index];
      // If local is composite (vtVoid) and > 8 bytes, pass address
      if (LLocalInfo.LocalType = vtVoid) and (LLocalInfo.LocalSize > 8) then
        LNeedsAddress := True;
    end;
    
    if LNeedsAddress then
    begin
      // LEA reg, [RBP-offset] to get address of local struct
      EmitLeaRegRbpDisp(AReg, GetLocalOffset(AOp.LocalHandle.Index));
    end
    else
      LoadOperandToReg(AOp, AReg);
  end;

  //----------------------------------------------------------------------------
  // Helper: Store operand to stack argument slot (for args 5+)
  // Win64 ABI: stack args go to [RSP+32], [RSP+40], etc.
  //----------------------------------------------------------------------------
  procedure StoreOperandToStackArg(const AOp: TTigerOperand; const AArgIndex: Integer);
  var
    LStackOffset: Int32;
  begin
    // Calculate stack offset: shadow space (32) + arg index * 8
    LStackOffset := 32 + (AArgIndex - 4) * 8;
    
    case AOp.Kind of
      okImmediate:
        begin
          // Load immediate to RAX, then store to stack
          if (AOp.ImmInt >= 0) and (AOp.ImmInt <= $FFFFFFFF) then
            EmitMovRegImm32(REG_RAX, Cardinal(AOp.ImmInt))
          else
            EmitMovRegImm64(REG_RAX, UInt64(AOp.ImmInt));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okData:
        begin
          // LEA RAX, [RIP+offset], then store to stack
          LDataFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(REG_RAX, 0);  // Placeholder
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okGlobal:
        begin
          // LEA RAX, [RIP+offset to .data], then store to stack
          LGlobalFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, AOp.DataHandle.Index));
          EmitLeaRipRel(REG_RAX, 0);  // Placeholder
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okLocal:
        begin
          // Load local to RAX, then store to stack
          if AOp.LocalHandle.IsParam then
            EmitMovRegRbpDisp(REG_RAX, GetParamOffset(AOp.LocalHandle.Index))
          else
            EmitMovRegRbpDisp(REG_RAX, GetLocalOffset(AOp.LocalHandle.Index));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okTemp:
        begin
          // Load temp to RAX, then store to stack
          EmitMovRegRbpDisp(REG_RAX, GetTempOffset(AOp.TempHandle.Index));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;
    else
      // Zero the slot
      EmitXorRegReg(REG_RAX);
      EmitMovRspDispReg(LStackOffset, REG_RAX);
    end;
  end;

  //----------------------------------------------------------------------------
  // Helper: Store call argument to stack slot, handling large struct params
  // Win64 ABI: Large structs (>8 bytes) are passed by pointer
  //----------------------------------------------------------------------------
  procedure StoreCallArgToStack(const AOp: TTigerOperand; const AArgIndex: Integer;
    const ATargetFuncIndex: Integer);
  var
    LStackOffset: Int32;
    LTargetFunc: TTigerFuncInfo;
    LNeedsAddress: Boolean;
  begin
    LStackOffset := 32 + (AArgIndex - 4) * 8;
    LNeedsAddress := False;
    
    // Check if target param expects a large struct (passed by pointer)
    if ATargetFuncIndex >= 0 then
    begin
      LTargetFunc := FCode.GetFunc(ATargetFuncIndex);
      if (AArgIndex >= 0) and (AArgIndex < Length(LTargetFunc.Params)) then
      begin
        // Large struct param (>8 bytes): callee expects a pointer
        if LTargetFunc.Params[AArgIndex].ParamSize > 8 then
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
  Result := nil;
  LOutput := TMemoryStream.Create();
  LTextSection := TMemoryStream.Create();
  LRDataSection := TMemoryStream.Create();
  LDataSection := TMemoryStream.Create();
  LPDataSection := TMemoryStream.Create();
  LIDataSection := TMemoryStream.Create();
  LEdataSection := TMemoryStream.Create();
  LRelocSection := TMemoryStream.Create();
  LImportDlls := TStringList.Create();
  LImportDlls.CaseSensitive := False;
  LImportDlls.Duplicates := dupIgnore;
  LImportsByDll := TObjectList<TStringList>.Create(True);
  LIATEntries := TList<Cardinal>.Create();
  LFixups := TList<TPair<Cardinal, Integer>>.Create();
  LDataFixups := TList<TPair<Cardinal, Integer>>.Create();
  LGlobalFixups := TList<TPair<Cardinal, Integer>>.Create();
  LJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LCallFixups := TList<TPair<Cardinal, Integer>>.Create();
  LFuncAddrFixups := TList<TPair<Cardinal, Integer>>.Create();
  LExportFuncs := TList<TPair<Integer, string>>.Create();
  LSEHHandlerFixups := TList<Cardinal>.Create();
  LSEHScopeTableFixups := TList<TPair<Cardinal, Integer>>.Create();
  LStaticImportIndices := TList<Integer>.Create();
  LStaticSymbolNames := TStringList.Create();
  LStaticLibPaths := TStringList.Create();
  LStaticLibPaths.CaseSensitive := False;
  LStaticLibPaths.Duplicates := dupIgnore;
  LStaticObjPaths := TStringList.Create();
  LStaticObjPaths.CaseSensitive := False;
  LStaticObjPaths.Duplicates := dupIgnore;
  LStaticImportResolved := TDictionary<Integer, Cardinal>.Create();
  LLinker := nil;
  LHasStaticImports := False;
  LExternalTextBase := 0;
  LExternalRDataBase := 0;
  LExternalDataBase := 0;
  try
    //==========================================================================
    // STEP 1: Build .rdata section (read-only data: strings, constants)
    //==========================================================================
    if FData.GetSize() > 0 then
      LRDataSection.WriteBuffer(FData.GetDataPointer()^, FData.GetSize());
    // Ensure section has at least 1 byte (PE requires non-zero SizeOfRawData)
    if LRDataSection.Size = 0 then
      LRDataSection.WriteData(Byte(0));
    AlignStream(LRDataSection, 16);

    //==========================================================================
    // STEP 1b: Build .data section (writable data: globals)
    //==========================================================================
    if FGlobals.GetSize() > 0 then
      LDataSection.WriteBuffer(FGlobals.GetDataPointer()^, FGlobals.GetSize());
    // Ensure section has at least 1 byte (PE requires non-zero SizeOfRawData)
    if LDataSection.Size = 0 then
      LDataSection.WriteData(Byte(0));
    AlignStream(LDataSection, 16);

    //==========================================================================
    // STEP 1c: Check for SEH and import __C_specific_handler if needed
    //==========================================================================
    // Scan all functions to see if any have exception scopes
    LHasSEH := False;
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      if Length(LFunc.ExceptionScopes) > 0 then
      begin
        LHasSEH := True;
        Break;
      end;
    end;

    // If any function uses SEH, import __C_specific_handler from vcruntime140.dll
    LSEHHandlerImport := TTigerImportHandle.Invalid();
    LInitExceptionsIndex := -1;
    if LHasSEH then
    begin
      LSEHHandlerImport := FImports.Add('vcruntime140.dll', '__C_specific_handler');
      
      // Find Pax_InitExceptions function index for startup call
      for LI := 0 to FCode.GetFuncCount() - 1 do
      begin
        LFunc := FCode.GetFunc(LI);
        if LFunc.FuncName = 'Tiger_InitExceptions' then
        begin
          LInitExceptionsIndex := LI;
          Break;
        end;
      end;
    end;

    //==========================================================================
    // STEP 2: Gather imports by DLL (or static lib)
    //==========================================================================
    for LI := 0 to FImports.GetCount() - 1 do
    begin
      LEntry := FImports.GetEntryByIndex(LI);

      if LEntry.IsStatic then
      begin
        // Static import — resolve lib path from name
        LStaticImportIndices.Add(LI);
        LStaticSymbolNames.Add(LEntry.FuncName);

        // Build lib filename: append .lib if no extension
        LLibPath := LEntry.DllName;
        if ExtractFileExt(LLibPath) = '' then
          LLibPath := LLibPath + '.lib';

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
        // Standard DLL import
        LJ := LImportDlls.IndexOf(LEntry.DllName);
        if LJ < 0 then
        begin
          LJ := LImportDlls.Add(LEntry.DllName);
          LImportsByDll.Add(TStringList.Create());
        end;
        LImportsByDll[LJ].AddObject(LEntry.FuncName, TObject(LI));
      end;
    end;

    //==========================================================================
    // STEP 3: Build .idata section (imports)
    //==========================================================================
    LHasImports := FImports.GetCount() > 0;
    if LHasImports then
    begin
      // Layout:
      // 1. Import descriptors (one per DLL + null terminator)
      // 2. DLL names
      // 3. Hint/Name entries
      // 4. IAT (Import Address Table)
      // 5. INT (Import Name Table) - mirrors IAT

      // Calculate sizes
      LImportDescriptorsOffset := 0;
      // Each descriptor is 20 bytes, plus null terminator
      LDllNamesOffset := (LImportDlls.Count + 1) * 20;

      // Write import descriptors (placeholders - will fixup later)
      for LI := 0 to LImportDlls.Count - 1 do
      begin
        LIDataSection.WriteData(Cardinal(0));  // OriginalFirstThunk (INT)
        LIDataSection.WriteData(Cardinal(0));  // TimeDateStamp
        LIDataSection.WriteData(Cardinal(0));  // ForwarderChain
        LIDataSection.WriteData(Cardinal(0));  // Name
        LIDataSection.WriteData(Cardinal(0));  // FirstThunk (IAT)
      end;
      // Null terminator descriptor
      for LI := 1 to 5 do
        LIDataSection.WriteData(Cardinal(0));

      // Write DLL names
      LDllNamesOffset := LIDataSection.Size;
      for LI := 0 to LImportDlls.Count - 1 do
        WriteString(LIDataSection, AnsiString(LImportDlls[LI]));
      AlignStream(LIDataSection, 2);

      // Write hint/name entries
      LHintNamesOffset := LIDataSection.Size;
      for LI := 0 to LImportDlls.Count - 1 do
      begin
        for LJ := 0 to LImportsByDll[LI].Count - 1 do
        begin
          LIDataSection.WriteData(Word(0));  // Hint
          WriteString(LIDataSection, AnsiString(LImportsByDll[LI][LJ]));
          AlignStream(LIDataSection, 2);
        end;
      end;
      AlignStream(LIDataSection, 8);

      // Write IAT
      LIATOffset := LIDataSection.Size;
      for LI := 0 to LImportDlls.Count - 1 do
      begin
        for LJ := 0 to LImportsByDll[LI].Count - 1 do
        begin
          // Store the IAT entry offset for this import
          LK := Integer(LImportsByDll[LI].Objects[LJ]);  // Original import index
          while LIATEntries.Count <= LK do
            LIATEntries.Add(0);
          LIATEntries[LK] := LIDataSection.Size;
          LIDataSection.WriteData(UInt64(0));  // Placeholder - will point to hint/name
        end;
        LIDataSection.WriteData(UInt64(0));  // Null terminator
      end;

      // Write INT (mirrors IAT structure)
      LINTOffset := LIDataSection.Size;
      for LI := 0 to LImportDlls.Count - 1 do
      begin
        for LJ := 0 to LImportsByDll[LI].Count - 1 do
          LIDataSection.WriteData(UInt64(0));  // Placeholder
        LIDataSection.WriteData(UInt64(0));  // Null terminator
      end;
    end;

    //==========================================================================
    // STEP 4: Generate code for each function
    //==========================================================================
    LEntryPointRVA := 0;

    // Initialize function offsets array
    SetLength(LFuncOffsets, FCode.GetFuncCount());
    SetLength(LAllLabelOffsets, FCode.GetFuncCount());  // For SEH SCOPE_TABLE
    SetLength(LFuncUnwindInfoOffsets, FCode.GetFuncCount());  // Per-function UNWIND_INFO

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);

      // Record function offset
      LFuncOffsets[LI] := LTextSection.Size;

      // Record entry point
      // For EXE: use IsEntryPoint (main)
      // For DLL: use IsDllEntry (DllMain)
      if LFunc.IsEntryPoint and (FOutputType <> otDll) then
        LEntryPointRVA := LTextSection.Size
      else if LFunc.IsDllEntry and (FOutputType = otDll) then
        LEntryPointRVA := LTextSection.Size;

      // Initialize label offsets for this function
      SetLength(LLabelOffsets, Length(LFunc.Labels));
      for LJ := 0 to High(LLabelOffsets) do
        LLabelOffsets[LJ] := 0;

      // Calculate stack frame size:
      // Layout from RBP downward:
      //   [RBP-8..RBP-40]                          reserved base (8 alignment + 32 gap)
      //   [RBP-40-params*8..]                       saved params
      //   [after params..]                          locals
      //   [after locals..]                          temps
      //   [RSP+outgoing..RSP+outgoing+temps*8-1]   temps from RSP perspective
      //   [RSP..RSP+outgoing-1]                     outgoing arg space (shadow + stack args)
      //
      // The outgoing arg space must not overlap with temps. We scan all
      // call instructions to find the maximum argument count and reserve
      // enough space at the bottom of the frame for the callee's shadow
      // space (32 bytes minimum) plus any stack-passed arguments (args 5+).
      LLocalsSize := 0;
      for LJ := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + LFunc.Locals[LJ].LocalSize;

      // Scan for maximum outgoing call argument count
      LMaxCallArgs := 0;
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        if LFunc.Instructions[LJ].Kind in [ikCallImport, ikCall, ikCallIndirect] then
        begin
          if Length(LFunc.Instructions[LJ].Args) > LMaxCallArgs then
            LMaxCallArgs := Length(LFunc.Instructions[LJ].Args);
        end;
      end;
      // Minimum 4 slots (32 bytes shadow) for any function that makes calls
      if LMaxCallArgs > 0 then
      begin
        if LMaxCallArgs < 4 then
          LMaxCallArgs := 4;
      end;
      LOutgoingArgSpace := Cardinal(LMaxCallArgs) * 8;

      if LFunc.IsVariadic then
        LStackFrameSize := 40 + LOutgoingArgSpace + Cardinal(4 * 8) + Cardinal(LLocalsSize) + Cardinal(LFunc.TempCount * 8)  // Always 4 slots for variadic
      else
        LStackFrameSize := 40 + LOutgoingArgSpace + Cardinal(Length(LFunc.Params) * 8) + Cardinal(LLocalsSize) + Cardinal(LFunc.TempCount * 8);
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);  // Round up to multiple of 16

      // Function prologue: PUSH RBP; MOV RBP, RSP; SUB RSP, N
      EmitPushReg(REG_RBP);
      EmitMovRegReg(REG_RBP, REG_RSP);
      if LStackFrameSize > 0 then
        EmitSubRspImm32(LStackFrameSize);

      // Save incoming parameters to stack (Win64: RCX, RDX, R8, R9)
      // For variadic functions: RCX = hidden count, then declared params
      // For large struct returns (>8 bytes): RCX = hidden return ptr, params shift
      if LFunc.IsVariadic then
      begin
        // Save ALL 4 registers for variadic functions
        // RCX = hidden count, RDX/R8/R9 could be declared params or varargs
        // We must save them all so VaArgAt can access varargs uniformly
        EmitMovRbpDispReg(-40, REG_RCX);   // Hidden count at position 0
        EmitMovRbpDispReg(-48, REG_RDX);   // Position 1 (param 0 or vararg 0)
        EmitMovRbpDispReg(-56, REG_R8);    // Position 2 (param 1 or vararg 1)
        EmitMovRbpDispReg(-64, REG_R9);    // Position 3 (param 2 or vararg 2)
        // Note: position 4+ come from caller's stack, no register save needed
      end
      else if LFunc.ReturnSize > 8 then
      begin
        // Large struct return: RCX = hidden return pointer (shifts all params)
        // Save hidden return pointer to [RBP-32]
        EmitMovRbpDispReg(-32, REG_RCX);
        // Params are shifted: RDX=param0, R8=param1, R9=param2, stack=param3+
        if Length(LFunc.Params) > 0 then
          EmitMovRbpDispReg(GetParamOffset(0), REG_RDX);
        if Length(LFunc.Params) > 1 then
          EmitMovRbpDispReg(GetParamOffset(1), REG_R8);
        if Length(LFunc.Params) > 2 then
          EmitMovRbpDispReg(GetParamOffset(2), REG_R9);
        // Param 3+ come from caller's stack, no register save needed
      end
      else
      begin
        if Length(LFunc.Params) > 0 then
          EmitMovRbpDispReg(GetParamOffset(0), REG_RCX);
        if Length(LFunc.Params) > 1 then
          EmitMovRbpDispReg(GetParamOffset(1), REG_RDX);
        if Length(LFunc.Params) > 2 then
          EmitMovRbpDispReg(GetParamOffset(2), REG_R8);
        if Length(LFunc.Params) > 3 then
          EmitMovRbpDispReg(GetParamOffset(3), REG_R9);
      end;

      // For entry point functions using SEH, call Pax_InitExceptions() first
      // This allocates TLS slots for exception context before any try/except blocks
      if LHasSEH and (LInitExceptionsIndex >= 0) then
      begin
        if (LFunc.IsEntryPoint and (FOutputType <> otDll)) or
           (LFunc.IsDllEntry and (FOutputType = otDll)) then
        begin
          // CALL rel32 to Pax_InitExceptions (no arguments)
          // Record fixup: displacement is at offset 1 from current position
          LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 1, LInitExceptionsIndex));
          EmitCallRel32(0);  // Placeholder displacement
        end;
      end;

      // Process each instruction
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LJ];

        case LInstr.Kind of
          ikLabel:
            begin
              // Record label position
              if LInstr.LabelTarget.IsValid() then
                LLabelOffsets[LInstr.LabelTarget.Index] := LTextSection.Size;
            end;

          ikCallImport:
            begin
              // Win64 ABI: First 4 args in RCX, RDX, R8, R9; rest on stack
              // Store stack arguments FIRST (args 5+) using RAX as scratch
              for LK := 4 to High(LInstr.Args) do
                StoreOperandToStackArg(LInstr.Args[LK], LK);
              
              // Then load register arguments
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RCX);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RDX);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_R8);
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_R9);

              // CALL [RIP+offset] - Record fixup
              LFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LInstr.ImportTarget.Index));
              EmitCallIndirectRipRel(0);  // Placeholder

              // Sign-extend 32-bit return values to 64-bit
              if FImports.GetEntryByIndex(LInstr.ImportTarget.Index).ReturnType = vtInt32 then
                EmitMovsxdRaxEax();

              // Store return value to temp if this instruction has a dest
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCall:
            begin
              // Call local function
              // Win64 ABI: First 4 args in RCX, RDX, R8, R9; rest on stack
              // Store stack arguments FIRST (args 5+) using RAX as scratch
              for LK := 4 to High(LInstr.Args) do
                StoreCallArgToStack(LInstr.Args[LK], LK, LInstr.FuncTarget.Index);
              
              // Then load register arguments (with struct handling)
              if Length(LInstr.Args) > 0 then
                LoadCallArgToReg(LInstr.Args[0], REG_RCX, LInstr.FuncTarget.Index, 0);
              if Length(LInstr.Args) > 1 then
                LoadCallArgToReg(LInstr.Args[1], REG_RDX, LInstr.FuncTarget.Index, 1);
              if Length(LInstr.Args) > 2 then
                LoadCallArgToReg(LInstr.Args[2], REG_R8, LInstr.FuncTarget.Index, 2);
              if Length(LInstr.Args) > 3 then
                LoadCallArgToReg(LInstr.Args[3], REG_R9, LInstr.FuncTarget.Index, 3);

              // CALL rel32 - Record fixup
              // CALL rel32 is 5 bytes, displacement is at offset 1
              LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 1, LInstr.FuncTarget.Index));
              EmitCallRel32(0);  // Placeholder

              // Store return value to temp if this instruction has a dest
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCallIndirect:
            begin
              // Call through function pointer
              // Win64 ABI: First 4 args in RCX, RDX, R8, R9; rest on stack
              // Store stack arguments FIRST (args 5+) using RAX as scratch
              for LK := 4 to High(LInstr.Args) do
                StoreOperandToStackArg(LInstr.Args[LK], LK);
              
              // Load register arguments (uses R10/R11 to avoid clobbering)
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_R9);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_R8);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RDX);
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RCX);

              // Load function pointer into RAX
              LoadOperandToReg(LInstr.Op1, REG_RAX);

              // CALL RAX
              EmitCallReg(REG_RAX);

              // Store return value to temp if this instruction has a dest
              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikStore:
            begin
              // Store value to local/param: load value to RAX, then MOV [RBP-offset], RAX
              // Op1 = dest local, Op2 = value
              LoadOperandToReg(LInstr.Op2, REG_RAX);
              if LInstr.Op1.LocalHandle.IsParam then
                EmitMovRbpDispReg(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_RAX)
              else
                EmitMovRbpDispReg(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_RAX);
            end;

          ikLoad:
            begin
              // Load from local to temp: load local to RAX, store to temp
              // Dest = temp, Op1 = local
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikAddressOf:
            begin
              // Dest = address of local/param slot (always LEA)
              // Op1 = local handle
              // Note: For by-ref params (structs >8 bytes), the caller should emit
              // a subsequent load instruction to get the actual struct pointer.
              if LInstr.Op1.LocalHandle.IsParam then
                EmitLeaRegRbpDisp(REG_RAX, GetParamOffset(LInstr.Op1.LocalHandle.Index))
              else
                EmitLeaRegRbpDisp(REG_RAX, GetLocalOffset(LInstr.Op1.LocalHandle.Index));
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikLoadPtr:
            begin
              // Dest = value at pointer Op1
              LoadOperandToReg(LInstr.Op1, REG_RAX);  // Load pointer into RAX
              EmitMovRegMemReg(REG_RAX, REG_RAX);     // Load value from [RAX] into RAX
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikStorePtr:
            begin
              // Store Op2 to address in Op1
              LoadOperandToReg(LInstr.Op1, REG_RAX);  // Load pointer into RAX
              LoadOperandToReg(LInstr.Op2, REG_RCX);  // Load value into RCX
              EmitMovMemRegReg(REG_RAX, REG_RCX);     // Store RCX to [RAX]
            end;

          ikAdd:
            begin
              // Dest = Op1 + Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitAddRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikSub:
            begin
              // Dest = Op1 - Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitSubRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikMul:
            begin
              // Dest = Op1 * Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitImulRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikDiv:
            begin
              // Dest = Op1 / Op2 (signed)
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCqo();  // Sign-extend RAX into RDX:RAX
              EmitIdivReg(REG_RCX);  // RAX = quotient, RDX = remainder
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikMod:
            begin
              // Dest = Op1 mod Op2 (signed remainder)
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCqo();  // Sign-extend RAX into RDX:RAX
              EmitIdivReg(REG_RCX);  // RAX = quotient, RDX = remainder
              StoreTempFromReg(LInstr.Dest.Index, REG_RDX);  // Remainder is in RDX
            end;

          ikBitAnd:
            begin
              // Dest = Op1 and Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitAndRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikBitOr:
            begin
              // Dest = Op1 or Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitOrRegReg(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikBitXor:
            begin
              // Dest = Op1 xor Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitXorRegReg64(REG_RAX, REG_RCX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikBitNot:
            begin
              // Dest = not Op1
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitNotReg(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikShl:
            begin
              // Dest = Op1 shl Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);  // Shift count must be in CL
              EmitShlRegCl(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikShr:
            begin
              // Dest = Op1 shr Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);  // Shift count must be in CL
              EmitShrRegCl(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpEq:
            begin
              // Dest = Op1 = Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSete();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpNe:
            begin
              // Dest = Op1 <> Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetne();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpLt:
            begin
              // Dest = Op1 < Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetl();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpLe:
            begin
              // Dest = Op1 <= Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetle();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpGt:
            begin
              // Dest = Op1 > Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetg();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCmpGe:
            begin
              // Dest = Op1 >= Op2
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitCmpRegReg(REG_RAX, REG_RCX);
              EmitSetge();
              EmitMovzxEaxAl();
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikJump:
            begin
              // Unconditional jump to label
              // Record fixup: code offset -> label index
              // JMP rel32 is 5 bytes, displacement is at offset 1
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 1, LInstr.LabelTarget.Index));
              EmitJmpRel32(0);  // Placeholder
            end;

          ikJumpIf:
            begin
              // Jump if condition is true (non-zero)
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitTestRegReg(REG_RAX, REG_RAX);
              // JNZ rel32 is 6 bytes, displacement is at offset 2
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 2, LInstr.LabelTarget.Index));
              EmitJnzRel32(0);  // Placeholder
            end;

          ikJumpIfNot:
            begin
              // Jump if condition is false (zero)
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitTestRegReg(REG_RAX, REG_RAX);
              // JZ rel32 is 6 bytes, displacement is at offset 2
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 2, LInstr.LabelTarget.Index));
              EmitJzRel32(0);  // Placeholder
            end;

          ikVaCount:
            begin
              // Load hidden vararg count from [RBP-40] (always at fixed position)
              EmitMovRegRbpDisp(REG_RAX, -40);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikVaArgAt:
            begin
              // Load vararg at index from stack
              // Layout: [RBP-40] = hidden count
              //         [RBP-48] = declared param 0 (if any)
              //         ...
              //         [RBP-(40 + (NumParams+1)*8)] = first vararg (index 0)
              // For args >= 4 total position: caller's stack at [RBP+16+pos*8]
              //
              // Calculate actual position = NumParams + 1 + index
              // If position < 4: use [RBP - (40 + position * 8)]
              // If position >= 4: use [RBP + 16 + position * 8]
              
              // Load index into RAX
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              
              // Add (NumParams + 1) to get actual position
              // NumParams = Length(LFunc.Params) (declared params, not counting hidden count)
              EmitAddRaxImm32(Length(LFunc.Params) + 1);
              
              // Now RAX = actual position
              // Compare with 4 to determine which stack region
              EmitCmpRaxImm32(4);
              
              // Save position in R10 for later use
              EmitMovRegReg(REG_R10, REG_RAX);
              
              // JGE rel8 to stack args path (jump +N bytes forward)
              // Register arg path code size: ~20 bytes
              EmitJgeRel8(20);
              
              // === Register arg path: [RBP - (40 + pos * 8)] ===
              // RAX still has position
              EmitShlRaxImm8(3);          // RAX = pos * 8
              EmitAddRaxImm32(40);        // RAX = 40 + pos * 8
              EmitNegRax();               // RAX = -(40 + pos * 8)
              EmitMovRegRbpPlusRax(REG_RAX);  // RAX = [RBP + RAX]
              EmitJmpRel8(18);            // Jump over stack arg path (18 bytes)
              
              // === Stack arg path: [RBP + 16 + pos * 8] ===
              // Position 4 → [RBP+48], Position 5 → [RBP+56], etc.
              // Restore position from R10
              EmitMovRegReg(REG_RAX, REG_R10);
              EmitShlRaxImm8(3);          // RAX = pos * 8
              EmitAddRaxImm32(16);        // RAX = pos * 8 + 16
              EmitMovRegRbpPlusRax(REG_RAX);  // RAX = [RBP + RAX]
              
              // Store result
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikReturn:
            begin
              // Epilogue: MOV RSP, RBP; POP RBP; RET
              EmitMovRegReg(REG_RSP, REG_RBP);
              EmitPopReg(REG_RBP);
              EmitRet();
            end;

          ikReturnValue:
            begin
              if LFunc.ReturnSize > 8 then
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
              // Epilogue: MOV RSP, RBP; POP RBP; RET
              EmitMovRegReg(REG_RSP, REG_RBP);
              EmitPopReg(REG_RBP);
              EmitRet();
            end;

          // TODO: Implement other instructions
        end;
      end;

      // Ensure function ends properly (for functions without explicit return)
      // Check if we need to add an implicit epilogue
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

      // Backpatch jump targets within this function
      for LK := 0 to LJumpFixups.Count - 1 do
      begin
        LCodeOffset := LJumpFixups[LK].Key;
        LDataIndex := LJumpFixups[LK].Value;  // Label index
        // Calculate relative displacement: target - (instruction end)
        // The displacement is relative to the instruction after the jump
        // For JMP rel32: next instruction is at LCodeOffset + 4
        // For Jcc rel32: next instruction is at LCodeOffset + 4
        LDisp := Int32(LLabelOffsets[LDataIndex]) - Int32(LCodeOffset + 4);
        LTextSection.Position := LCodeOffset;
        LTextSection.WriteData(LDisp);
      end;
      LJumpFixups.Clear();
      LTextSection.Position := LTextSection.Size;

      // Save label offsets for SEH SCOPE_TABLE generation
      LAllLabelOffsets[LI] := Copy(LLabelOffsets);

      AlignStream(LTextSection, 16);
    end;

    // Backpatch local function calls (cross-function)
    for LI := 0 to LCallFixups.Count - 1 do
    begin
      LCodeOffset := LCallFixups[LI].Key;
      LDataIndex := LCallFixups[LI].Value;  // Function index
      // Calculate relative displacement: target - (instruction end)
      // CALL rel32: next instruction is at LCodeOffset + 4
      LDisp := Int32(LFuncOffsets[LDataIndex]) - Int32(LCodeOffset + 4);
      LTextSection.Position := LCodeOffset;
      LTextSection.WriteData(LDisp);
    end;
    LTextSection.Position := LTextSection.Size;

    // Ensure we have some code
    if LTextSection.Size = 0 then
    begin
      Status('Error: No code generated');
      Exit;
    end;

    //==========================================================================
    // STEP 4b: Build .edata section (exports) if we have public functions
    //==========================================================================
    // Collect all public functions and compute their export names
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
          // Build param types array for Itanium mangling
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

    if LHasExports then
    begin
      // Export Directory Table (40 bytes)
      // Structure:
      //   DWORD Characteristics (0)
      //   DWORD TimeDateStamp
      //   WORD MajorVersion (0)
      //   WORD MinorVersion (0)
      //   DWORD Name (RVA of DLL name - will be fixed up)
      //   DWORD Base (1)
      //   DWORD NumberOfFunctions
      //   DWORD NumberOfNames
      //   DWORD AddressOfFunctions (RVA - will be fixed up)
      //   DWORD AddressOfNames (RVA - will be fixed up)
      //   DWORD AddressOfNameOrdinals (RVA - will be fixed up)

      LExportDirOffset := LEdataSection.Size;
      LEdataSection.WriteData(Cardinal(0));           // Characteristics
      LEdataSection.WriteData(Cardinal(0));           // TimeDateStamp
      LEdataSection.WriteData(Word(0));               // MajorVersion
      LEdataSection.WriteData(Word(0));               // MinorVersion
      LEdataSection.WriteData(Cardinal(0));           // Name RVA (placeholder)
      LEdataSection.WriteData(Cardinal(1));           // Base ordinal
      LEdataSection.WriteData(Cardinal(LNumExports)); // NumberOfFunctions
      LEdataSection.WriteData(Cardinal(LNumExports)); // NumberOfNames
      LEdataSection.WriteData(Cardinal(0));           // AddressOfFunctions (placeholder)
      LEdataSection.WriteData(Cardinal(0));           // AddressOfNames (placeholder)
      LEdataSection.WriteData(Cardinal(0));           // AddressOfNameOrdinals (placeholder)

      // Export Address Table (EAT) - RVAs to functions
      LExportAddressTableOffset := LEdataSection.Size;
      for LI := 0 to LExportFuncs.Count - 1 do
        LEdataSection.WriteData(Cardinal(0));         // Placeholder for function RVA

      // Name Pointer Table - RVAs to name strings
      LExportNameTableOffset := LEdataSection.Size;
      for LI := 0 to LExportFuncs.Count - 1 do
        LEdataSection.WriteData(Cardinal(0));         // Placeholder for name RVA

      // Ordinal Table - 16-bit ordinals (0-based index into EAT)
      LExportOrdinalTableOffset := LEdataSection.Size;
      for LI := 0 to LExportFuncs.Count - 1 do
        LEdataSection.WriteData(Word(LI));            // Ordinal = index

      // DLL name string
      LDllNameOffset := LEdataSection.Size;
      WriteString(LEdataSection, AnsiString(ExtractFileName(FOutputPath)));
      AlignStream(LEdataSection, 2);

      // Export name strings
      LExportNamesOffset := LEdataSection.Size;
      for LI := 0 to LExportFuncs.Count - 1 do
      begin
        WriteString(LEdataSection, AnsiString(LExportFuncs[LI].Value));
        AlignStream(LEdataSection, 2);
      end;
    end;

    if LEdataSection.Size = 0 then
      LEdataSection.WriteData(Byte(0));  // Minimum size
    AlignStream(LEdataSection, 16);

    //==========================================================================
    // STEP 4c: Generate .pdata section (x64 exception unwind data)
    //==========================================================================
    // Initialize per-function UNWIND_INFO offset tracking
    for LI := 0 to FCode.GetFuncCount() - 1 do
      LFuncUnwindInfoOffsets[LI] := 0;  // Will be set below

    // If any function uses SEH, generate a dispatcher thunk that jumps to
    // __C_specific_handler via IAT. This thunk is shared by all SEH functions.
    LSEHThunkOffset := 0;
    if LHasSEH then
    begin
      AlignStream(LTextSection, 4);
      LSEHThunkOffset := LTextSection.Size;
      // JMP qword ptr [RIP+disp32]: FF 25 xx xx xx xx
      // The disp32 will be fixed up to point to the IAT slot for __C_specific_handler
      LTextSection.WriteData(Byte($FF));
      LTextSection.WriteData(Byte($25));
      LTextSection.WriteData(Cardinal(0));  // Placeholder displacement
      // Add fixup for the thunk's displacement
      LFixups.Add(TPair<Cardinal, Integer>.Create(LSEHThunkOffset, LSEHHandlerImport.Index));
    end;

    // Generate shared basic UNWIND_INFO (for functions without exception scopes)
    AlignStream(LTextSection, 4);
    LUnwindInfoRVA := LTextSection.Size;  // Offset within .text (will add LTextRVA later)
    LTextSection.WriteBuffer(UNWIND_INFO[0], SizeOf(UNWIND_INFO));

    // For each function with exception scopes, generate per-function UNWIND_INFO + SCOPE_TABLE
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LScopeCount := Length(LFunc.ExceptionScopes);

      if LScopeCount = 0 then
      begin
        // No exception scopes - use shared basic UNWIND_INFO
        LFuncUnwindInfoOffsets[LI] := LUnwindInfoRVA;
      end
      else
      begin
        // Has exception scopes - generate per-function UNWIND_INFO with SEH handler
        AlignStream(LTextSection, 4);
        LFuncUnwindInfoOffsets[LI] := LTextSection.Size;

        // UNWIND_INFO with UNW_FLAG_EHANDLER (0x01)
        // Byte 0: Version (bits 0-2) | Flags (bits 3-7)
        //   Version = 1, Flags = UNW_FLAG_EHANDLER (0x01)
        //   Byte 0 = 1 | (1 << 3) = 1 | 8 = 9 = $09
        LTextSection.WriteData(Byte($09));  // Version=1, Flags=UNW_FLAG_EHANDLER
        LTextSection.WriteData(Byte($04));  // SizeOfProlog=4
        LTextSection.WriteData(Byte($02));  // CountOfCodes=2
        LTextSection.WriteData(Byte($05));  // FrameRegister=RBP(5), FrameOffset=0
        // Unwind codes (same as basic UNWIND_INFO)
        LTextSection.WriteData(Byte($04));  // Offset 4
        LTextSection.WriteData(Byte($03));  // UWOP_SET_FPREG
        LTextSection.WriteData(Byte($01));  // Offset 1
        LTextSection.WriteData(Byte($50));  // UWOP_PUSH_NONVOL RBP
        // After unwind codes (already aligned to DWORD), write ExceptionHandler RVA
        // This is an offset within .text pointing to the SEH thunk
        LSEHHandlerFixups.Add(LTextSection.Size);  // Track for fixup
        LTextSection.WriteData(Cardinal(LSEHThunkOffset));  // Will add LTextRVA later

        // SCOPE_TABLE for __C_specific_handler
        // Count: DWORD
        LSEHScopeTableFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, LScopeCount));
        LTextSection.WriteData(Cardinal(LScopeCount));

        // ScopeRecord[Count]: BeginAddress, EndAddress, HandlerAddress, JumpTarget
        // All addresses are offsets within .text (will add LTextRVA later)
        for LJ := 0 to LScopeCount - 1 do
        begin
          LScope := LFunc.ExceptionScopes[LJ];

          // BeginAddress: Start of try block (label offset)
          if (LScope.TryBeginLabel.Index >= 0) and
             (LScope.TryBeginLabel.Index < Length(LAllLabelOffsets[LI])) then
            LTextSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.TryBeginLabel.Index]))
          else
            LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));  // Fallback to function start

          // EndAddress: End of try block = start of handler (one past protected region)
          // Use the handler label (except or finally) as the end of the protected region
          if LScope.ExceptLabel.Index >= 0 then
          begin
            // Except handler marks end of try
            if LScope.ExceptLabel.Index < Length(LAllLabelOffsets[LI]) then
              LTextSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.ExceptLabel.Index]))
            else
              LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else if LScope.FinallyLabel.Index >= 0 then
          begin
            // Finally handler marks end of try
            if LScope.FinallyLabel.Index < Length(LAllLabelOffsets[LI]) then
              LTextSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.FinallyLabel.Index]))
            else
              LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else
            LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));  // Fallback

          // HandlerAddress: 1 for __except filter, 0 for __finally
          // In our model: except handler exists -> 1, finally only -> 0
          if LScope.ExceptLabel.Index >= 0 then
            LTextSection.WriteData(Cardinal(1))   // __except
          else
            LTextSection.WriteData(Cardinal(0));  // __finally

          // JumpTarget: Handler entry point (except or finally label)
          if LScope.ExceptLabel.Index >= 0 then
          begin
            // Jump to except handler
            if LScope.ExceptLabel.Index < Length(LAllLabelOffsets[LI]) then
              LTextSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.ExceptLabel.Index]))
            else
              LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else if LScope.FinallyLabel.Index >= 0 then
          begin
            // Jump to finally handler
            if LScope.FinallyLabel.Index < Length(LAllLabelOffsets[LI]) then
              LTextSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.FinallyLabel.Index]))
            else
              LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else
            LTextSection.WriteData(Cardinal(LFuncOffsets[LI]));  // Fallback
        end;
      end;
    end;

    // Pad .text section to minimum 1024 bytes (Device Guard requirement)
    // Some security policies block executables with .text section < 1024 bytes
    while LTextSection.Size < 1024 do
      LTextSection.WriteData(Byte($90));  // NOP padding

    // Generate RUNTIME_FUNCTION entry for each function (12 bytes each)
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      // BeginAddress (placeholder - will fix after RVA calculation)
      LPDataSection.WriteData(Cardinal(LFuncOffsets[LI]));
      // EndAddress (placeholder - will fix after RVA calculation)
      if LI < FCode.GetFuncCount() - 1 then
        LPDataSection.WriteData(Cardinal(LFuncOffsets[LI + 1]))
      else
        LPDataSection.WriteData(Cardinal(LUnwindInfoRVA));  // End at shared UNWIND_INFO
      // UnwindData - use per-function offset (may be shared or per-function)
      LPDataSection.WriteData(Cardinal(LFuncUnwindInfoOffsets[LI]));
    end;
    AlignStream(LPDataSection, 4);

    //==========================================================================
    // STEP 4c2: Static linker — resolve and merge external code
    //==========================================================================
    if LHasStaticImports then
    begin
      LLinker := TTigerCOFFLinker.Create();
      CopyStatusCallbackTo(LLinker);

      // Add library and object files from imports
      for LI := 0 to LStaticLibPaths.Count - 1 do
        LLinker.AddLibraryFile(LStaticLibPaths[LI]);
      for LI := 0 to LStaticObjPaths.Count - 1 do
        LLinker.AddObjectFile(LStaticObjPaths[LI]);

      // Also add explicitly linked libs/objs via AddLib()/AddObj()
      for LI := 0 to FLinkedLibs.Count - 1 do
        LLinker.AddLibraryFile(FLinkedLibs[LI]);
      for LI := 0 to FLinkedObjs.Count - 1 do
        LLinker.AddObjectFile(FLinkedObjs[LI]);

      // Resolve needed symbols
      LLinker.Resolve(LStaticSymbolNames);

      // Align .text to 16 bytes before appending external code
      AlignStream(LTextSection, 16);
      LExternalTextBase := Cardinal(LTextSection.Size);

      // Append merged .text
      LMergedBytes := LLinker.GetMergedText();
      if Length(LMergedBytes) > 0 then
        LTextSection.WriteBuffer(LMergedBytes[0], Length(LMergedBytes));
      AlignStream(LTextSection, 16);

      // Append merged .rdata
      AlignStream(LRDataSection, 4);
      LExternalRDataBase := Cardinal(LRDataSection.Size);
      LMergedBytes := LLinker.GetMergedRData();
      if Length(LMergedBytes) > 0 then
        LRDataSection.WriteBuffer(LMergedBytes[0], Length(LMergedBytes));
      AlignStream(LRDataSection, 4);

      // Append merged .data
      AlignStream(LDataSection, 8);
      LExternalDataBase := Cardinal(LDataSection.Size);
      LMergedBytes := LLinker.GetMergedData();
      if Length(LMergedBytes) > 0 then
        LDataSection.WriteBuffer(LMergedBytes[0], Length(LMergedBytes));
      AlignStream(LDataSection, 8);

      // Build static import resolution map: import index -> offset in LTextSection
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

      Status('Linker: %d/%d static imports resolved, external .text at offset %d',
        [LStaticImportResolved.Count, LStaticImportIndices.Count, LExternalTextBase]);
    end;

    //==========================================================================
    // STEP 4d: Generate .reloc section (base relocations)
    //==========================================================================
    // EXE: include minimal .reloc for ASLR support
    // DLL: skip .reloc (would need actual relocations which we don't generate)
    LHasReloc := (FOutputType <> otDll);
    if LHasReloc then
    begin
      // Minimal reloc section - just a single empty block
      // IMAGE_BASE_RELOCATION: VirtualAddress (4) + SizeOfBlock (4)
      LRelocSection.WriteData(Cardinal(0));  // VirtualAddress = 0 (page 0)
      LRelocSection.WriteData(Cardinal(8));  // SizeOfBlock = 8 (just header, no entries)
      AlignStream(LRelocSection, 4);
    end;

    //==========================================================================
    // STEP 5: Calculate section layout
    //==========================================================================
    // Headers size (DOS + PE + Optional + Section headers)
    // DOS header = 64, DOS stub = 64, PE sig = 4, File header = 20, Optional = 240
    // Section headers = 40 each
    // Base: 4 sections (.text, .rdata, .data, .pdata)
    // +1 if imports (.idata)
    // +1 if exports (.edata)
    // +1 if reloc (.reloc) - only for EXE, not DLL
    LHeadersSize := 64 + 64 + 4 + 20 + 240 + (40 * 4);
    if LHasImports then
      LHeadersSize := LHeadersSize + 40;
    if LHasExports then
      LHeadersSize := LHeadersSize + 40;
    if LHasReloc then
      LHeadersSize := LHeadersSize + 40;
    LHeadersSize := AlignUp(LHeadersSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .text section
    LTextRVA := AlignUp(LHeadersSize, PE_DEFAULT_SECTION_ALIGNMENT);
    LTextSize := LTextSection.Size;
    LTextFileSize := AlignUp(LTextSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .rdata section
    LRDataRVA := LTextRVA + AlignUp(LTextSize, PE_DEFAULT_SECTION_ALIGNMENT);
    LRDataSize := LRDataSection.Size;
    LRDataFileSize := AlignUp(LRDataSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .data section
    LDataRVA := LRDataRVA + AlignUp(LRDataSize, PE_DEFAULT_SECTION_ALIGNMENT);
    LDataSize := LDataSection.Size;
    LDataFileSize := AlignUp(LDataSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .pdata section (always present)
    LPDataRVA := LDataRVA + AlignUp(LDataSize, PE_DEFAULT_SECTION_ALIGNMENT);
    LPDataSize := LPDataSection.Size;
    LPDataFileSize := AlignUp(LPDataSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .idata section (if imports)
    LIDataRVA := LPDataRVA + AlignUp(LPDataSize, PE_DEFAULT_SECTION_ALIGNMENT);
    LIDataSize := LIDataSection.Size;
    LIDataFileSize := AlignUp(LIDataSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .edata RVA depends on whether we have imports
    if LHasImports then
      LEdataRVA := LIDataRVA + AlignUp(LIDataSize, PE_DEFAULT_SECTION_ALIGNMENT)
    else
      LEdataRVA := LPDataRVA + AlignUp(LPDataSize, PE_DEFAULT_SECTION_ALIGNMENT);
    LEdataSize := LEdataSection.Size;
    LEdataFileSize := AlignUp(LEdataSize, PE_DEFAULT_FILE_ALIGNMENT);

    // .reloc section (only for EXE, at end)
    if LHasReloc then
    begin
      if LHasExports then
        LRelocRVA := LEdataRVA + AlignUp(LEdataSize, PE_DEFAULT_SECTION_ALIGNMENT)
      else if LHasImports then
        LRelocRVA := LIDataRVA + AlignUp(LIDataSize, PE_DEFAULT_SECTION_ALIGNMENT)
      else
        LRelocRVA := LPDataRVA + AlignUp(LPDataSize, PE_DEFAULT_SECTION_ALIGNMENT);
      LRelocSize := LRelocSection.Size;
      LRelocFileSize := AlignUp(LRelocSize, PE_DEFAULT_FILE_ALIGNMENT);
      // SizeOfImage = end of .reloc
      LSizeOfImage := LRelocRVA + AlignUp(LRelocSize, PE_DEFAULT_SECTION_ALIGNMENT);
    end
    else
    begin
      // No .reloc section - SizeOfImage ends at last present section
      LRelocRVA := 0;
      LRelocSize := 0;
      LRelocFileSize := 0;
      if LHasExports then
        LSizeOfImage := LEdataRVA + AlignUp(LEdataSize, PE_DEFAULT_SECTION_ALIGNMENT)
      else if LHasImports then
        LSizeOfImage := LIDataRVA + AlignUp(LIDataSize, PE_DEFAULT_SECTION_ALIGNMENT)
      else
        LSizeOfImage := LPDataRVA + AlignUp(LPDataSize, PE_DEFAULT_SECTION_ALIGNMENT);
    end;

    // Fix entry point RVA
    LEntryPointRVA := LEntryPointRVA + LTextRVA;

    // Fix UNWIND_INFO RVA (shared basic UNWIND_INFO)
    LUnwindInfoRVA := LUnwindInfoRVA + LTextRVA;

    // Fix per-function UNWIND_INFO offsets
    for LI := 0 to FCode.GetFuncCount() - 1 do
      LFuncUnwindInfoOffsets[LI] := LFuncUnwindInfoOffsets[LI] + LTextRVA;

    // Fix SEH thunk offset (if SEH is used)
    if LHasSEH then
      LSEHThunkOffset := LSEHThunkOffset + LTextRVA;

    // Fix ExceptionHandler fields in per-function UNWIND_INFO structures
    // These point to the SEH thunk and need LTextRVA added
    for LI := 0 to LSEHHandlerFixups.Count - 1 do
    begin
      LCodeOffset := LSEHHandlerFixups[LI];
      LTextSection.Position := LCodeOffset;
      LTextSection.ReadData(LTargetRVA, SizeOf(Cardinal));
      LTextSection.Position := LCodeOffset;
      LTextSection.WriteData(Cardinal(LTargetRVA + LTextRVA));
    end;

    // Fix SCOPE_TABLE entries (BeginAddress, EndAddress, JumpTarget need LTextRVA)
    // Each SCOPE_TABLE: Count (4 bytes) + Count * ScopeRecord (16 bytes each)
    // ScopeRecord: BeginAddress (4), EndAddress (4), HandlerAddress (4), JumpTarget (4)
    // Only BeginAddress, EndAddress, and JumpTarget need fixup (HandlerAddress is 0 or 1)
    for LI := 0 to LSEHScopeTableFixups.Count - 1 do
    begin
      LCodeOffset := LSEHScopeTableFixups[LI].Key;  // Offset of Count field
      LScopeCount := LSEHScopeTableFixups[LI].Value;  // Number of scope records
      
      // Skip Count field (4 bytes), iterate through scope records
      for LJ := 0 to LScopeCount - 1 do
      begin
        // BeginAddress at offset + 4 + (j * 16) + 0
        LTextSection.Position := LCodeOffset + 4 + (Cardinal(LJ) * 16) + 0;
        LTextSection.ReadData(LTargetRVA, SizeOf(Cardinal));
        LTextSection.Position := LCodeOffset + 4 + (Cardinal(LJ) * 16) + 0;
        LTextSection.WriteData(Cardinal(LTargetRVA + LTextRVA));
        
        // EndAddress at offset + 4 + (j * 16) + 4
        LTextSection.Position := LCodeOffset + 4 + (Cardinal(LJ) * 16) + 4;
        LTextSection.ReadData(LTargetRVA, SizeOf(Cardinal));
        LTextSection.Position := LCodeOffset + 4 + (Cardinal(LJ) * 16) + 4;
        LTextSection.WriteData(Cardinal(LTargetRVA + LTextRVA));
        
        // HandlerAddress at offset + 4 + (j * 16) + 8 - DO NOT fix (it's 0 or 1)
        
        // JumpTarget at offset + 4 + (j * 16) + 12
        LTextSection.Position := LCodeOffset + 4 + (Cardinal(LJ) * 16) + 12;
        LTextSection.ReadData(LTargetRVA, SizeOf(Cardinal));
        LTextSection.Position := LCodeOffset + 4 + (Cardinal(LJ) * 16) + 12;
        LTextSection.WriteData(Cardinal(LTargetRVA + LTextRVA));
      end;
    end;
    LTextSection.Position := LTextSection.Size;  // Reset position

    // Fix .pdata RUNTIME_FUNCTION entries (add LTextRVA to all RVAs)
    LPDataSection.Position := 0;
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      // Read current values
      LPDataSection.Position := LI * 12;
      // BeginAddress
      LPDataSection.ReadData(LCodeOffset, SizeOf(Cardinal));
      LPDataSection.Position := LI * 12;
      LPDataSection.WriteData(Cardinal(LCodeOffset + LTextRVA));
      // EndAddress
      LPDataSection.ReadData(LCodeOffset, SizeOf(Cardinal));
      LPDataSection.Position := LI * 12 + 4;
      LPDataSection.WriteData(Cardinal(LCodeOffset + LTextRVA));
      // UnwindData - use per-function offset (already includes LTextRVA)
      LPDataSection.Position := LI * 12 + 8;
      LPDataSection.WriteData(Cardinal(LFuncUnwindInfoOffsets[LI]));
    end;

    //==========================================================================
    // STEP 6: Fix up import table RVAs
    //==========================================================================
    if FImports.GetCount() > 0 then
    begin
      // Fix import descriptors
      LIDataSection.Position := 0;
      LIATPos := LIATOffset;
      LINTPos := LINTOffset;
      LDllNamePos := LDllNamesOffset;

      for LI := 0 to LImportDlls.Count - 1 do
      begin
        LFuncCount := LImportsByDll[LI].Count;

        // OriginalFirstThunk (INT RVA)
        LIDataSection.WriteData(Cardinal(LIDataRVA + LINTPos));
        // TimeDateStamp
        LIDataSection.WriteData(Cardinal(0));
        // ForwarderChain
        LIDataSection.WriteData(Cardinal(0));
        // Name RVA
        LIDataSection.WriteData(Cardinal(LIDataRVA + LDllNamePos));
        // FirstThunk (IAT RVA)
        LIDataSection.WriteData(Cardinal(LIDataRVA + LIATPos));

        // Advance positions
        LDllNamePos := LDllNamePos + Cardinal(Length(AnsiString(LImportDlls[LI])) + 1);
        LIATPos := LIATPos + Cardinal((LFuncCount + 1) * 8);
        LINTPos := LINTPos + Cardinal((LFuncCount + 1) * 8);
      end;

      // Fix IAT entries (point to hint/name RVAs)
      LIDataSection.Position := LIATOffset;
      LHintNamePos := LHintNamesOffset;
      for LI := 0 to LImportDlls.Count - 1 do
      begin
        for LJ := 0 to LImportsByDll[LI].Count - 1 do
        begin
          LIDataSection.WriteData(UInt64(LIDataRVA + LHintNamePos));
          // Advance past hint (2) + name + null + alignment
          LHintNamePos := LHintNamePos + 2 + Cardinal(Length(AnsiString(LImportsByDll[LI][LJ])) + 1);
          if (LHintNamePos mod 2) <> 0 then
            Inc(LHintNamePos);
        end;
        LIDataSection.WriteData(UInt64(0));  // Null terminator
      end;

      // Fix INT entries (same as IAT)
      LHintNamePos := LHintNamesOffset;
      for LI := 0 to LImportDlls.Count - 1 do
      begin
        for LJ := 0 to LImportsByDll[LI].Count - 1 do
        begin
          LIDataSection.WriteData(UInt64(LIDataRVA + LHintNamePos));
          LHintNamePos := LHintNamePos + 2 + Cardinal(Length(AnsiString(LImportsByDll[LI][LJ])) + 1);
          if (LHintNamePos mod 2) <> 0 then
            Inc(LHintNamePos);
        end;
        LIDataSection.WriteData(UInt64(0));  // Null terminator
      end;
    end;

    //==========================================================================
    // STEP 7: Fix up code relocations
    //==========================================================================
    // Fix import call sites
    for LI := 0 to LFixups.Count - 1 do
    begin
      LCodeOffset := LFixups[LI].Key;
      LImportIndex := LFixups[LI].Value;

      // Check if this is a statically-resolved import
      if LStaticImportResolved.TryGetValue(LImportIndex, LTargetRVA) then
      begin
        // Rewrite CALL [RIP+disp32] (FF 15 xx xx xx xx, 6 bytes)
        // to CALL rel32 + NOP (E8 xx xx xx xx 90, 6 bytes)
        LSymTargetRVA := LTextRVA + LTargetRVA;
        LInstrRVA := LTextRVA + LCodeOffset;
        LRipAfter := LInstrRVA + 5;  // CALL rel32 is 5 bytes
        LRel32 := Int32(LSymTargetRVA) - Int32(LRipAfter);

        LTextSection.Position := LCodeOffset;
        LTextSection.WriteData(Byte($E8));      // CALL rel32 opcode
        LTextSection.WriteData(LRel32);          // rel32 displacement
        LTextSection.WriteData(Byte($90));       // NOP padding (keeps 6-byte size)
      end
      else
      begin
        // Standard DLL import — CALL [RIP+disp32] pointing to IAT slot
        if LImportIndex >= LIATEntries.Count then
          Continue;
        LIATSlotOffset := LIATEntries[LImportIndex];
        LIATSlotRVA := LIDataRVA + LIATSlotOffset;

        LInstrRVA := LTextRVA + LCodeOffset;
        LRipAfter := LInstrRVA + 6;
        LDisp := Int32(LIATSlotRVA) - Int32(LRipAfter);

        LTextSection.Position := LCodeOffset + 2;
        LTextSection.WriteData(LDisp);
      end;
    end;

    // Fix data references (LEA instructions pointing to .rdata)
    for LI := 0 to LDataFixups.Count - 1 do
    begin
      LCodeOffset := LDataFixups[LI].Key;
      LDataIndex := LDataFixups[LI].Value;

      // Get the actual entry from .rdata
      LDataHandle.Index := LDataIndex;
      LDataEntryRec := FData.GetEntry(LDataHandle);

      LTargetRVA := LRDataRVA + LDataEntryRec.Offset;

      // LEA r64, [RIP+disp32] is 7 bytes: REX 8D mod-reg-rm disp32
      // Displacement is at offset 3
      LInstrRVA := LTextRVA + LCodeOffset;
      LRipAfter := LInstrRVA + 7;
      LDisp := Int32(LTargetRVA) - Int32(LRipAfter);

      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;

    //==========================================================================
    // STEP 7b: Fix up export table RVAs
    //==========================================================================
    if LHasExports then
    begin
      // Fix Export Directory fields
      LEdataSection.Position := LExportDirOffset + 12;  // Offset to Name field
      LEdataSection.WriteData(Cardinal(LEdataRVA + LDllNameOffset));

      LEdataSection.Position := LExportDirOffset + 28;  // AddressOfFunctions
      LEdataSection.WriteData(Cardinal(LEdataRVA + LExportAddressTableOffset));
      LEdataSection.WriteData(Cardinal(LEdataRVA + LExportNameTableOffset));
      LEdataSection.WriteData(Cardinal(LEdataRVA + LExportOrdinalTableOffset));

      // Fix Export Address Table (function RVAs)
      LEdataSection.Position := LExportAddressTableOffset;
      for LI := 0 to LExportFuncs.Count - 1 do
      begin
        LDataIndex := LExportFuncs[LI].Key;  // Function index
        LTargetRVA := LTextRVA + LFuncOffsets[LDataIndex];
        LEdataSection.WriteData(Cardinal(LTargetRVA));
      end;

      // Fix Name Pointer Table (name string RVAs)
      LEdataSection.Position := LExportNameTableOffset;
      LTargetRVA := LEdataRVA + LExportNamesOffset;
      for LI := 0 to LExportFuncs.Count - 1 do
      begin
        LEdataSection.WriteData(Cardinal(LTargetRVA));
        // Advance past name string + null + alignment
        LTargetRVA := LTargetRVA + Cardinal(Length(AnsiString(LExportFuncs[LI].Value)) + 1);
        if (LTargetRVA mod 2) <> 0 then
          Inc(LTargetRVA);
      end;
    end;

    // Fix global references (LEA instructions pointing to .data)
    for LI := 0 to LGlobalFixups.Count - 1 do
    begin
      LCodeOffset := LGlobalFixups[LI].Key;
      LDataIndex := LGlobalFixups[LI].Value;

      // Get the actual entry from .data (globals)
      LDataHandle.Index := LDataIndex;
      LDataEntryRec := FGlobals.GetEntry(LDataHandle);

      LTargetRVA := LDataRVA + LDataEntryRec.Offset;

      // LEA r64, [RIP+disp32] is 7 bytes: REX 8D mod-reg-rm disp32
      // Displacement is at offset 3
      LInstrRVA := LTextRVA + LCodeOffset;
      LRipAfter := LInstrRVA + 7;
      LDisp := Int32(LTargetRVA) - Int32(LRipAfter);

      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;

    // Fix function address references (LEA instructions pointing to functions in .text)
    for LI := 0 to LFuncAddrFixups.Count - 1 do
    begin
      LCodeOffset := LFuncAddrFixups[LI].Key;
      LDataIndex := LFuncAddrFixups[LI].Value;  // Function index

      // Target is the function's code offset within .text
      LTargetRVA := LTextRVA + LFuncOffsets[LDataIndex];

      // LEA r64, [RIP+disp32] is 7 bytes: REX 8D mod-reg-rm disp32
      // Displacement is at offset 3
      LInstrRVA := LTextRVA + LCodeOffset;
      LRipAfter := LInstrRVA + 7;
      LDisp := Int32(LTargetRVA) - Int32(LRipAfter);

      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;

    //==========================================================================
    // STEP 7c: Apply linker pending relocations (external code fixups)
    //==========================================================================
    if LHasStaticImports and (LLinker <> nil) then
    begin
      LPendingRelocs := LLinker.GetPendingRelocations();
      LStaticResolved := LLinker.GetResolvedSymbols();

      for LI := 0 to LPendingRelocs.Count - 1 do
      begin
        LPendReloc := LPendingRelocs[LI];

        // Resolve target symbol
        if not LStaticResolved.TryGetValue(LPendReloc.TargetSymbol, LResolvedSym) then
          Continue;  // Unresolved — skip (would need DLL import handling)

        // Calculate target RVA based on which merged section it resolved to
        case LResolvedSym.SectionKind of
          lskText:  LSymTargetRVA := LTextRVA + LExternalTextBase + LResolvedSym.OffsetInMerged;
          lskRData: LSymTargetRVA := LRDataRVA + LExternalRDataBase + LResolvedSym.OffsetInMerged;
          lskData:  LSymTargetRVA := LDataRVA + LExternalDataBase + LResolvedSym.OffsetInMerged;
        else
          Continue;
        end;

        // Calculate site RVA based on which merged section contains the reloc
        case LPendReloc.SectionKind of
          lskText:  LSiteRVA := LTextRVA + LExternalTextBase + LPendReloc.OffsetInMerged;
          lskRData: LSiteRVA := LRDataRVA + LExternalRDataBase + LPendReloc.OffsetInMerged;
          lskData:  LSiteRVA := LDataRVA + LExternalDataBase + LPendReloc.OffsetInMerged;
        else
          Continue;
        end;

        // Apply relocation based on type
        if (LPendReloc.RelocationType >= IMAGE_REL_AMD64_REL32) and
           (LPendReloc.RelocationType <= IMAGE_REL_AMD64_REL32_5) then
        begin
          // REL32 family: disp = target - (site + 4 + N) + addend
          LRel32 := Int32(LSymTargetRVA) - Int32(LSiteRVA + 4 +
            Cardinal(LPendReloc.RelocationType - IMAGE_REL_AMD64_REL32)) +
            LPendReloc.Addend;

          case LPendReloc.SectionKind of
            lskText:
            begin
              LTextSection.Position := Integer(LExternalTextBase + LPendReloc.OffsetInMerged);
              LTextSection.WriteData(LRel32);
            end;
            lskRData:
            begin
              LRDataSection.Position := Integer(LExternalRDataBase + LPendReloc.OffsetInMerged);
              LRDataSection.WriteData(LRel32);
            end;
            lskData:
            begin
              LDataSection.Position := Integer(LExternalDataBase + LPendReloc.OffsetInMerged);
              LDataSection.WriteData(LRel32);
            end;
          end;
        end
        else if LPendReloc.RelocationType = IMAGE_REL_AMD64_ADDR32NB then
        begin
          // ADDR32NB: image-relative RVA
          LRel32 := Int32(LSymTargetRVA) + LPendReloc.Addend;

          case LPendReloc.SectionKind of
            lskText:
            begin
              LTextSection.Position := Integer(LExternalTextBase + LPendReloc.OffsetInMerged);
              LTextSection.WriteData(LRel32);
            end;
            lskRData:
            begin
              LRDataSection.Position := Integer(LExternalRDataBase + LPendReloc.OffsetInMerged);
              LRDataSection.WriteData(LRel32);
            end;
            lskData:
            begin
              LDataSection.Position := Integer(LExternalDataBase + LPendReloc.OffsetInMerged);
              LDataSection.WriteData(LRel32);
            end;
          end;
        end
        else if LPendReloc.RelocationType = IMAGE_REL_AMD64_ADDR64 then
        begin
          // ADDR64: 64-bit absolute address (ImageBase + RVA)
          case LPendReloc.SectionKind of
            lskText:
            begin
              LTextSection.Position := Integer(LExternalTextBase + LPendReloc.OffsetInMerged);
              LTextSection.WriteData(UInt64($0000000140000000 + LSymTargetRVA));
            end;
          end;
        end;
      end;

      Status('Linker: Applied %d pending relocations', [LPendingRelocs.Count]);
    end;

    //==========================================================================
    // STEP 8: Write PE file
    //==========================================================================

    // DOS Header
    FillChar(LDosHeader, SizeOf(LDosHeader), 0);
    LDosHeader.e_magic := IMAGE_DOS_SIGNATURE;
    LDosHeader.e_cblp := $90;
    LDosHeader.e_cp := $03;
    LDosHeader.e_cparhdr := $04;
    LDosHeader.e_maxalloc := $FFFF;
    LDosHeader.e_sp := $B8;
    LDosHeader.e_lfarlc := $40;
    LDosHeader.e_lfanew := $80;  // PE header at 0x80
    LOutput.WriteBuffer(LDosHeader, SizeOf(LDosHeader));

    // DOS Stub
    LOutput.WriteBuffer(DOS_STUB[0], SizeOf(DOS_STUB));

    // PE Signature
    LOutput.WriteData(Cardinal(IMAGE_NT_SIGNATURE));

    // File Header
    FillChar(LFileHeader, SizeOf(LFileHeader), 0);
    LFileHeader.Machine := IMAGE_FILE_MACHINE_AMD64;
    // Base sections: .text, .rdata, .data, .pdata = 4
    // +1 for .idata if has imports
    // +1 for .edata if has exports
    // +1 for .reloc if has reloc
    LFileHeader.NumberOfSections := 4;
    if LHasImports then
      Inc(LFileHeader.NumberOfSections);
    if LHasExports then
      Inc(LFileHeader.NumberOfSections);
    if LHasReloc then
      Inc(LFileHeader.NumberOfSections);
    LFileHeader.TimeDateStamp := DateTimeToUnix(Now(), False);
    LFileHeader.SizeOfOptionalHeader := SizeOf(TImageOptionalHeader64);
    LFileHeader.Characteristics := IMAGE_FILE_EXECUTABLE_IMAGE or IMAGE_FILE_LARGE_ADDRESS_AWARE;
    if FOutputType = otDll then
      LFileHeader.Characteristics := LFileHeader.Characteristics or IMAGE_FILE_DLL;
    LOutput.WriteBuffer(LFileHeader, SizeOf(LFileHeader));

    // Optional Header
    FillChar(LOptHeader, SizeOf(LOptHeader), 0);
    LOptHeader.Magic := $020B;  // PE32+
    LOptHeader.MajorLinkerVersion := 1;
    LOptHeader.MinorLinkerVersion := 0;
    LOptHeader.SizeOfCode := LTextFileSize;
    LOptHeader.SizeOfInitializedData := LRDataFileSize + LDataFileSize + LPDataFileSize + LIDataFileSize + LEdataFileSize + LRelocFileSize;
    LOptHeader.AddressOfEntryPoint := LEntryPointRVA;
    LOptHeader.BaseOfCode := LTextRVA;
    LOptHeader.ImageBase := PE_DEFAULT_IMAGE_BASE;
    LOptHeader.SectionAlignment := PE_DEFAULT_SECTION_ALIGNMENT;
    LOptHeader.FileAlignment := PE_DEFAULT_FILE_ALIGNMENT;
    LOptHeader.MajorOperatingSystemVersion := 6;
    LOptHeader.MinorOperatingSystemVersion := 0;
    LOptHeader.MajorSubsystemVersion := 6;
    LOptHeader.MinorSubsystemVersion := 0;
    LOptHeader.SizeOfImage := LSizeOfImage;
    LOptHeader.SizeOfHeaders := LHeadersSize;

    if FSubsystem = ssConsole then
      LSubsystemValue := IMAGE_SUBSYSTEM_WINDOWS_CUI
    else
      LSubsystemValue := IMAGE_SUBSYSTEM_WINDOWS_GUI;
    LOptHeader.Subsystem := LSubsystemValue;

    // DllCharacteristics - only set DYNAMIC_BASE/HIGH_ENTROPY if we have .reloc
    if LHasReloc then
      LOptHeader.DllCharacteristics := IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA or
                                       IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE or
                                       IMAGE_DLLCHARACTERISTICS_NX_COMPAT or
                                       IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE
    else
      LOptHeader.DllCharacteristics := IMAGE_DLLCHARACTERISTICS_NX_COMPAT or
                                       IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE;
    LOptHeader.SizeOfStackReserve := $100000;
    LOptHeader.SizeOfStackCommit := $1000;
    LOptHeader.SizeOfHeapReserve := $100000;
    LOptHeader.SizeOfHeapCommit := $1000;
    LOptHeader.NumberOfRvaAndSizes := IMAGE_NUMBEROF_DIRECTORY_ENTRIES;

    // Import directory
    if FImports.GetCount() > 0 then
    begin
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress := LIDataRVA;
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].Size := (LImportDlls.Count + 1) * 20;
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].VirtualAddress := LIDataRVA + LIATOffset;
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].Size := LINTOffset - LIATOffset;
    end;

    // Export directory
    if LHasExports then
    begin
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress := LEdataRVA;
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size := LEdataSize;
    end;

    // Exception directory (.pdata)
    LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXCEPTION].VirtualAddress := LPDataRVA;
    LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXCEPTION].Size := LPDataSize;

    // Base relocation directory (.reloc) - only if present
    if LHasReloc then
    begin
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress := LRelocRVA;
      LOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size := LRelocSize;
    end;

    LOutput.WriteBuffer(LOptHeader, SizeOf(LOptHeader));

    // Section Headers
    // .text
    FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
    Move(AnsiString('.text')[1], LSectionHeader.SectionName[0], 5);
    LSectionHeader.VirtualSize := LTextSize;
    LSectionHeader.VirtualAddress := LTextRVA;
    LSectionHeader.SizeOfRawData := LTextFileSize;
    LSectionHeader.PointerToRawData := LHeadersSize;
    LSectionHeader.Characteristics := IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_MEM_READ;
    LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));

    // .rdata (read-only data: strings, constants)
    FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
    Move(AnsiString('.rdata')[1], LSectionHeader.SectionName[0], 6);
    LSectionHeader.VirtualSize := LRDataSize;
    LSectionHeader.VirtualAddress := LRDataRVA;
    LSectionHeader.SizeOfRawData := LRDataFileSize;
    LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize;
    LSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ;
    LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));

    // .data (writable data: globals)
    FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
    Move(AnsiString('.data')[1], LSectionHeader.SectionName[0], 5);
    LSectionHeader.VirtualSize := LDataSize;
    LSectionHeader.VirtualAddress := LDataRVA;
    LSectionHeader.SizeOfRawData := LDataFileSize;
    LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize + LRDataFileSize;
    LSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
    LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));

    // .pdata (exception unwind data) - always present
    FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
    Move(AnsiString('.pdata')[1], LSectionHeader.SectionName[0], 6);
    LSectionHeader.VirtualSize := LPDataSize;
    LSectionHeader.VirtualAddress := LPDataRVA;
    LSectionHeader.SizeOfRawData := LPDataFileSize;
    LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize + LRDataFileSize + LDataFileSize;
    LSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ;
    LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));

    // .idata (imports) - only if we have imports
    if LHasImports then
    begin
      FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
      Move(AnsiString('.idata')[1], LSectionHeader.SectionName[0], 6);
      LSectionHeader.VirtualSize := LIDataSize;
      LSectionHeader.VirtualAddress := LIDataRVA;
      LSectionHeader.SizeOfRawData := LIDataFileSize;
      LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize + LRDataFileSize + LDataFileSize + LPDataFileSize;
      LSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
      LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));
    end;

    // .edata (exports) - only if we have exports
    if LHasExports then
    begin
      FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
      Move(AnsiString('.edata')[1], LSectionHeader.SectionName[0], 6);
      LSectionHeader.VirtualSize := LEdataSize;
      LSectionHeader.VirtualAddress := LEdataRVA;
      LSectionHeader.SizeOfRawData := LEdataFileSize;
      // PointerToRawData depends on whether .idata exists
      if LHasImports then
        LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize + LRDataFileSize + LDataFileSize + LPDataFileSize + LIDataFileSize
      else
        LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize + LRDataFileSize + LDataFileSize + LPDataFileSize;
      LSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ;
      LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));
    end;

    // .reloc (base relocations) - only if present
    if LHasReloc then
    begin
      FillChar(LSectionHeader, SizeOf(LSectionHeader), 0);
      Move(AnsiString('.reloc')[1], LSectionHeader.SectionName[0], 6);
      LSectionHeader.VirtualSize := LRelocSize;
      LSectionHeader.VirtualAddress := LRelocRVA;
      LSectionHeader.SizeOfRawData := LRelocFileSize;
      // PointerToRawData depends on which optional sections exist
      LSectionHeader.PointerToRawData := LHeadersSize + LTextFileSize + LRDataFileSize + LDataFileSize + LPDataFileSize;
      if LHasImports then
        LSectionHeader.PointerToRawData := LSectionHeader.PointerToRawData + LIDataFileSize;
      if LHasExports then
        LSectionHeader.PointerToRawData := LSectionHeader.PointerToRawData + LEdataFileSize;
      LSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_DISCARDABLE or IMAGE_SCN_MEM_READ;
      LOutput.WriteBuffer(LSectionHeader, SizeOf(LSectionHeader));
    end;

    // Pad to end of headers
    while LOutput.Size < LHeadersSize do
      LOutput.WriteData(Byte(0));

    // Write .text section
    LTextSection.Position := 0;
    LOutput.CopyFrom(LTextSection, LTextSection.Size);
    while (LOutput.Size - LHeadersSize) < LTextFileSize do
      LOutput.WriteData(Byte(0));

    // Write .rdata section
    LRDataSection.Position := 0;
    LOutput.CopyFrom(LRDataSection, LRDataSection.Size);
    while (LOutput.Size - LHeadersSize - LTextFileSize) < LRDataFileSize do
      LOutput.WriteData(Byte(0));

    // Write .data section
    LDataSection.Position := 0;
    LOutput.CopyFrom(LDataSection, LDataSection.Size);
    while (LOutput.Size - LHeadersSize - LTextFileSize - LRDataFileSize) < LDataFileSize do
      LOutput.WriteData(Byte(0));

    // Write .pdata section
    LPDataSection.Position := 0;
    LOutput.CopyFrom(LPDataSection, LPDataSection.Size);
    while (LOutput.Size - LHeadersSize - LTextFileSize - LRDataFileSize - LDataFileSize) < LPDataFileSize do
      LOutput.WriteData(Byte(0));

    // Write .idata section (if we have imports)
    if LHasImports then
    begin
      LIDataSection.Position := 0;
      LOutput.CopyFrom(LIDataSection, LIDataSection.Size);
      while (LOutput.Size - LHeadersSize - LTextFileSize - LRDataFileSize - LDataFileSize - LPDataFileSize) < LIDataFileSize do
        LOutput.WriteData(Byte(0));
    end;

    // Write .edata section (if we have exports)
    if LHasExports then
    begin
      LEdataSection.Position := 0;
      LOutput.CopyFrom(LEdataSection, LEdataSection.Size);
      // Padding calculation depends on whether .idata exists
      if LHasImports then
      begin
        while (LOutput.Size - LHeadersSize - LTextFileSize - LRDataFileSize - LDataFileSize - LPDataFileSize - LIDataFileSize) < LEdataFileSize do
          LOutput.WriteData(Byte(0));
      end
      else
      begin
        while (LOutput.Size - LHeadersSize - LTextFileSize - LRDataFileSize - LDataFileSize - LPDataFileSize) < LEdataFileSize do
          LOutput.WriteData(Byte(0));
      end;
    end;

    // Write .reloc section (if present)
    if LHasReloc then
    begin
      LRelocSection.Position := 0;
      LOutput.CopyFrom(LRelocSection, LRelocSection.Size);
    end;

    // Pad to file alignment
    while (LOutput.Size mod PE_DEFAULT_FILE_ALIGNMENT) <> 0 do
      LOutput.WriteData(Byte(0));

    // Return result
    SetLength(Result, LOutput.Size);
    LOutput.Position := 0;
    LOutput.ReadBuffer(Result[0], LOutput.Size);

    // Calculate and patch PE checksum (same algorithm as TCC)
    // CheckSum position = e_lfanew + 4 (PE sig) + 20 (file header) + 64 (offset in opt header)
    LChecksumPos := PCardinal(@Result[$3C])^ + 4 + 20 + 64;
    LChecksum := 0;
    LI := 0;
    while LI < Length(Result) do
    begin
      // Skip the CheckSum field itself (4 bytes)
      if (Cardinal(LI) >= LChecksumPos) and (Cardinal(LI) < LChecksumPos + 4) then
      begin
        Inc(LI);
        Continue;
      end;
      // Add word (or single byte if at end)
      if LI + 1 < Length(Result) then
        LChecksum := LChecksum + PWord(@Result[LI])^
      else
        LChecksum := LChecksum + Result[LI];
      // Fold to 16 bits
      LChecksum := (LChecksum + (LChecksum shr 16)) and $FFFF;
      Inc(LI, 2);
    end;
    // Add file size
    LChecksum := LChecksum + Cardinal(Length(Result));
    // Patch checksum into result
    PCardinal(@Result[LChecksumPos])^ := LChecksum;

    Status('PE generated: %d bytes, entry point RVA: $%x, checksum: $%x', [Length(Result), LEntryPointRVA, LChecksum]);

  finally
    LLinker.Free();
    LStaticImportResolved.Free();
    LStaticObjPaths.Free();
    LStaticLibPaths.Free();
    LStaticSymbolNames.Free();
    LStaticImportIndices.Free();
    LSEHScopeTableFixups.Free();
    LSEHHandlerFixups.Free();
    LExportFuncs.Free();
    LEdataSection.Free();
    LRelocSection.Free();
    LPDataSection.Free();
    LFuncAddrFixups.Free();
    LCallFixups.Free();
    LJumpFixups.Free();
    LGlobalFixups.Free();
    LDataFixups.Free();
    LFixups.Free();
    LIATEntries.Free();
    LImportsByDll.Free();
    LImportDlls.Free();
    LIDataSection.Free();
    LDataSection.Free();
    LRDataSection.Free();
    LTextSection.Free();
    LOutput.Free();
  end;
end;

// =============================================================================
// GenerateCOFF() and GenerateLib() implementation for Tiger.Backend.pas
// Replaces the stubs at the end of the file (lines 5233-5245)
// =============================================================================


// =============================================================================
// GenerateCOFF() and GenerateLib() implementation for Tiger.Backend.pas
// Replaces the stubs at the end of the file (lines 5233-5245)
// =============================================================================

function TTigerWin64Backend.GenerateCOFF(): TBytes;
const

  // COFF file structure sizes
  COFF_FILE_HEADER_SIZE = 20;
  COFF_SECTION_HEADER_SIZE = 40;
  COFF_RELOCATION_SIZE = 10;
  COFF_SYMBOL_SIZE = 18;

  // COFF relocation types for AMD64
  IMAGE_REL_AMD64_ABSOLUTE = $0000;
  IMAGE_REL_AMD64_ADDR32NB = $0003;  // 32-bit RVA (image-base-relative)
  IMAGE_REL_AMD64_REL32    = $0004;  // 32-bit RIP-relative

  // COFF symbol storage classes
  COFF_SYM_CLASS_EXTERNAL = 2;
  COFF_SYM_CLASS_STATIC   = 3;

  // COFF symbol type for function
  COFF_SYM_DTYPE_FUNCTION = $20;

  // Section characteristics
  COFF_SCN_TEXT  = $60500020;  // CNT_CODE | ALIGN_16BYTES | MEM_EXECUTE | MEM_READ
  COFF_SCN_RDATA = $40300040;  // CNT_INITIALIZED_DATA | ALIGN_4BYTES | MEM_READ
  COFF_SCN_DATA  = $C0300040;  // CNT_INITIALIZED_DATA | ALIGN_4BYTES | MEM_READ | MEM_WRITE
  COFF_SCN_XDATA = $40300040;  // CNT_INITIALIZED_DATA | ALIGN_4BYTES | MEM_READ
  COFF_SCN_PDATA = $40300040;  // CNT_INITIALIZED_DATA | ALIGN_4BYTES | MEM_READ
  COFF_SCN_DRECTVE = $00100A00;  // LNK_INFO | LNK_REMOVE | ALIGN_1BYTES

  // x64 UNWIND_INFO structure (shared by all functions without SEH)
  UNWIND_INFO: array[0..7] of Byte = (
    $01,  // Version=1, Flags=0
    $04,  // SizeOfProlog=4 (push rbp; mov rbp,rsp)
    $02,  // CountOfCodes=2
    $05,  // FrameRegister=RBP(5), FrameOffset=0
    $04, $03,  // Offset 4: UWOP_SET_FPREG (mov rbp,rsp)
    $01, $50   // Offset 1: UWOP_PUSH_NONVOL RBP (push rbp)
  );

type
  // COFF relocation record for tracking during code generation
  TCOFFReloc = record
    VirtualAddress: Cardinal;   // Offset within the section
    SymbolIndex: Cardinal;      // Index into COFF symbol table
    RelocationType: Word;       // IMAGE_REL_AMD64_xxx
  end;

var
  LOutput: TMemoryStream;
  LTextSection: TMemoryStream;
  LRDataSection: TMemoryStream;
  LDataSection: TMemoryStream;
  LXDataSection: TMemoryStream;
  LPDataSection: TMemoryStream;
  LDrectveData: AnsiString;

  // COFF relocation lists (per section)
  LTextRelocs: TList<TCOFFReloc>;
  LPDataRelocs: TList<TCOFFReloc>;
  LXDataRelocs: TList<TCOFFReloc>;

  // Symbol table indices (pre-computed)
  LTextSymIndex: Cardinal;      // .text section symbol index
  LRDataSymIndex: Cardinal;     // .rdata section symbol index
  LDataSymIndex: Cardinal;      // .data section symbol index
  LXDataSymIndex: Cardinal;     // .xdata section symbol index
  LPDataSymIndex: Cardinal;     // .pdata section symbol index
  LFirstFuncSymIndex: Cardinal; // Index of first function symbol
  LFirstImportSymIndex: Cardinal; // Index of first import (extern) symbol
  LSEHHandlerSymIndex: Cardinal;  // Index of __C_specific_handler symbol (if SEH)
  LTotalSymbols: Cardinal;      // Total symbol count

  // Section numbering (1-based for COFF)
  LSectText: SmallInt;
  LSectRData: SmallInt;
  LSectData: SmallInt;
  LSectXData: SmallInt;
  LSectPData: SmallInt;
  LNumSections: Word;

  // Function tracking
  LI, LJ, LK: Integer;
  LFunc: TTigerFuncInfo;
  LParamTypes: TArray<TTigerValueType>;
  LInstr: TTigerInstruction;
  LEntry: TTigerImportEntry;

  // Code generation tracking
  LFuncOffsets: TArray<Cardinal>;
  LFuncEndOffsets: TArray<Cardinal>;
  LLabelOffsets: TArray<Cardinal>;
  LAllLabelOffsets: TArray<TArray<Cardinal>>;
  LJumpFixups: TList<TPair<Cardinal, Integer>>;
  LCallFixups: TList<TPair<Cardinal, Integer>>;
  LFuncAddrFixups: TList<TPair<Cardinal, Integer>>;

  // SEH tracking
  LHasSEH: Boolean;
  LFuncUnwindInfoOffsets: TArray<Cardinal>;  // Per-function offset in .xdata
  LSharedUnwindInfoOffset: Cardinal;         // Shared basic UNWIND_INFO offset in .xdata
  LScope: TTigerExceptionScope;
  LScopeCount: Integer;
  LInitExceptionsIndex: Integer;

  // Stack frame
  LStackFrameSize: Cardinal;
  LLocalsSize: Integer;
  LMaxCallArgs: Integer;
  LOutgoingArgSpace: Cardinal;

  // Temporary variables for fixup resolution
  LCodeOffset: Cardinal;
  LDataIndex: Integer;
  LDisp: Int32;
  LTargetOffset: Cardinal;
  LReloc: TCOFFReloc;

  // Export name computation
  LExportName: string;
  LHasExports: Boolean;
  LHasDrectve: Boolean;

  // COFF assembly variables
  LStringTable: TMemoryStream;
  LSectionDataOffset: Cardinal;
  LSymTabOffset: Cardinal;

  //----------------------------------------------------------------------------
  // Emit helpers (same as GeneratePE, captured over LTextSection)
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
    EmitByte((AMod shl 6) or ((AReg and $07) shl 3) or (ARM and $07));
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

  procedure EmitLeaRipRel(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8D);
    EmitModRM(0, AReg and 7, 5);
    EmitInt32(ADisp);
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

  procedure EmitXorRegReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, AReg >= 8, False, AReg >= 8);
    EmitByte($31);
    EmitModRM(3, AReg and 7, AReg and 7);
  end;

  procedure EmitCallRel32(const ADisp: Int32);
  begin
    EmitByte($E8);
    EmitInt32(ADisp);
  end;

  procedure EmitCallReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($FF);
    EmitModRM(3, 2, AReg and 7);
  end;

  procedure EmitRet();
  begin
    EmitByte($C3);
  end;

  procedure EmitPushReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($50 + (AReg and 7));
  end;

  procedure EmitPopReg(const AReg: Byte);
  begin
    if AReg >= 8 then
      EmitRex(False, False, False, True);
    EmitByte($58 + (AReg and 7));
  end;

  procedure EmitMovRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($89);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  procedure EmitMovRegRbpDisp(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8B);
    EmitModRM(2, AReg and 7, REG_RBP);
    EmitInt32(ADisp);
  end;

  procedure EmitMovRbpDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($89);
    EmitModRM(2, AReg and 7, REG_RBP);
    EmitInt32(ADisp);
  end;

  procedure EmitMovRspDispReg(const ADisp: Int32; const AReg: Byte);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($89);
    EmitModRM(2, AReg and 7, 4);
    EmitByte($24);
    EmitInt32(ADisp);
  end;

  procedure AlignStream(const AStream: TMemoryStream; const AAlign: Cardinal);
  var
    LPad: Cardinal;
  begin
    if AAlign <= 1 then Exit;
    LPad := AStream.Size mod AAlign;
    if LPad > 0 then
    begin
      LPad := AAlign - LPad;
      while LPad > 0 do
      begin
        AStream.WriteData(Byte(0));
        Dec(LPad);
      end;
    end;
  end;

  procedure WriteString(const AStream: TMemoryStream; const AStr: AnsiString);
  begin
    if Length(AStr) > 0 then
      AStream.WriteBuffer(AStr[1], Length(AStr));
    AStream.WriteData(Byte(0));
  end;

  //----------------------------------------------------------------------------
  // Stack layout helpers (same as GeneratePE)
  //----------------------------------------------------------------------------
  function GetLocalOffset(const AIndex: Integer): Int32;
  var
    LK: Integer;
    LOffset: Integer;
  begin
    if LFunc.IsVariadic then
      LOffset := 40 + 4 * 8
    else
      LOffset := 40 + Length(LFunc.Params) * 8;
    for LK := 0 to AIndex - 1 do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    Result := -LOffset;
  end;

  function GetParamOffset(const AIndex: Integer): Int32;
  begin
    if LFunc.IsVariadic then
      Result := -(40 + (AIndex + 1) * 8)
    else
      Result := -(40 + AIndex * 8);
  end;

  function GetTempOffset(const AIndex: Integer): Int32;
  var
    LK: Integer;
    LOffset: Integer;
  begin
    if LFunc.IsVariadic then
      LOffset := 40 + 4 * 8
    else
      LOffset := 40 + Length(LFunc.Params) * 8;
    for LK := 0 to High(LFunc.Locals) do
      LOffset := LOffset + LFunc.Locals[LK].LocalSize;
    LOffset := LOffset + AIndex * 8;
    Result := -LOffset;
  end;

  procedure StoreTempFromReg(const ATempIndex: Integer; const AReg: Byte);
  begin
    EmitMovRbpDispReg(GetTempOffset(ATempIndex), AReg);
  end;

  //----------------------------------------------------------------------------
  // Arithmetic / comparison / branch helpers (same as GeneratePE)
  //----------------------------------------------------------------------------
  procedure EmitAddRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($01);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  procedure EmitSubRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($29);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  procedure EmitImulRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ADest >= 8, False, ASrc >= 8);
    EmitByte($0F);
    EmitByte($AF);
    EmitModRM(3, ADest and 7, ASrc and 7);
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
    EmitModRM(3, 7, AReg and 7);
  end;

  procedure EmitAndRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($21);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  procedure EmitOrRegReg(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($09);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  procedure EmitXorRegReg64(const ADest, ASrc: Byte);
  begin
    EmitRex(True, ASrc >= 8, False, ADest >= 8);
    EmitByte($31);
    EmitModRM(3, ASrc and 7, ADest and 7);
  end;

  procedure EmitNotReg(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($F7);
    EmitModRM(3, 2, AReg and 7);
  end;

  procedure EmitShlRegCl(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($D3);
    EmitModRM(3, 4, AReg and 7);
  end;

  procedure EmitShrRegCl(const AReg: Byte);
  begin
    EmitRex(True, False, False, AReg >= 8);
    EmitByte($D3);
    EmitModRM(3, 5, AReg and 7);
  end;

  procedure EmitRepMovsb();
  begin
    EmitByte($F3);  // REP prefix
    EmitByte($A4);  // MOVSB
  end;

  procedure EmitCmpRegReg(const AReg1, AReg2: Byte);
  begin
    EmitRex(True, AReg2 >= 8, False, AReg1 >= 8);
    EmitByte($39);
    EmitModRM(3, AReg2 and 7, AReg1 and 7);
  end;

  procedure EmitSete();
  begin
    EmitByte($0F); EmitByte($94); EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetne();
  begin
    EmitByte($0F); EmitByte($95); EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetl();
  begin
    EmitByte($0F); EmitByte($9C); EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetle();
  begin
    EmitByte($0F); EmitByte($9E); EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetg();
  begin
    EmitByte($0F); EmitByte($9F); EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitSetge();
  begin
    EmitByte($0F); EmitByte($9D); EmitModRM(3, 0, REG_RAX);
  end;

  procedure EmitMovzxEaxAl();
  begin
    EmitByte($0F); EmitByte($B6); EmitModRM(3, REG_RAX, REG_RAX);
  end;

  procedure EmitMovsxdRaxEax();
  begin
    EmitRex(True, False, False, False);
    EmitByte($63);
    EmitModRM(3, REG_RAX, REG_RAX);
  end;

  procedure EmitTestRegReg(const AReg1, AReg2: Byte);
  begin
    EmitRex(True, AReg2 >= 8, False, AReg1 >= 8);
    EmitByte($85);
    EmitModRM(3, AReg2 and 7, AReg1 and 7);
  end;

  procedure EmitJmpRel32(const ADisp: Int32);
  begin
    EmitByte($E9);
    EmitInt32(ADisp);
  end;

  procedure EmitJzRel32(const ADisp: Int32);
  begin
    EmitByte($0F); EmitByte($84);
    EmitInt32(ADisp);
  end;

  procedure EmitJnzRel32(const ADisp: Int32);
  begin
    EmitByte($0F); EmitByte($85);
    EmitInt32(ADisp);
  end;

  procedure EmitLeaRegRbpDisp(const AReg: Byte; const ADisp: Int32);
  begin
    EmitRex(True, AReg >= 8, False, False);
    EmitByte($8D);
    EmitModRM(2, AReg and 7, REG_RBP);
    EmitInt32(ADisp);
  end;

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

  // Variadic helpers
  procedure EmitAddRaxImm32(const AValue: Int32);
  begin
    EmitRex(True, False, False, False);
    EmitByte($05);
    EmitInt32(AValue);
  end;

  procedure EmitShlRaxImm8(const AValue: Byte);
  begin
    EmitRex(True, False, False, False);
    EmitByte($C1);
    EmitModRM(3, 4, 0);
    EmitByte(AValue);
  end;

  procedure EmitNegRax();
  begin
    EmitRex(True, False, False, False);
    EmitByte($F7);
    EmitModRM(3, 3, 0);
  end;

  procedure EmitMovRegRbpPlusRax(const ADest: Byte);
  begin
    EmitRex(True, ADest >= 8, False, False);
    EmitByte($8B);
    EmitByte($44);
    EmitByte($05);
    EmitByte($00);
  end;

  procedure EmitCmpRaxImm32(const AValue: Int32);
  begin
    EmitRex(True, False, False, False);
    EmitByte($3D);
    EmitInt32(AValue);
  end;

  procedure EmitJgeRel8(const ADisp: Int8);
  begin
    EmitByte($7D);
    EmitByte(Byte(ADisp));
  end;

  procedure EmitJmpRel8(const ADisp: Int8);
  begin
    EmitByte($EB);
    EmitByte(Byte(ADisp));
  end;

  //----------------------------------------------------------------------------
  // COFF-specific: Load operand into register with relocation tracking
  //----------------------------------------------------------------------------
  procedure LoadOperandToReg(const AOp: TTigerOperand; const AReg: Byte);
  var
    LR: TCOFFReloc;
    LDE: TTigerDataEntry;
    LDH: TTigerDataHandle;
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
          // LEA reg, [RIP+disp32] — disp32 = entry offset (addend for linker)
          LDH.Index := AOp.DataHandle.Index;
          LDE := FData.GetEntry(LDH);
          LR.VirtualAddress := LTextSection.Size + 3;  // disp32 is at byte 3 of 7-byte LEA
          LR.SymbolIndex := LRDataSymIndex;
          LR.RelocationType := IMAGE_REL_AMD64_REL32;
          LTextRelocs.Add(LR);
          EmitLeaRipRel(AReg, Int32(LDE.Offset));
        end;

      okGlobal:
        begin
          // LEA reg, [RIP+disp32] — disp32 = entry offset in .data (addend)
          LDH.Index := AOp.DataHandle.Index;
          LDE := FGlobals.GetEntry(LDH);
          LR.VirtualAddress := LTextSection.Size + 3;
          LR.SymbolIndex := LDataSymIndex;
          LR.RelocationType := IMAGE_REL_AMD64_REL32;
          LTextRelocs.Add(LR);
          EmitLeaRipRel(AReg, Int32(LDE.Offset));
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
          // LEA reg, [RIP+disp32] — resolved via backpatch (same section)
          LFuncAddrFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size, AOp.FuncHandle.Index));
          EmitLeaRipRel(AReg, 0);
        end;
    else
      EmitXorRegReg(AReg);
    end;
  end;

  //----------------------------------------------------------------------------
  // Helper: Load call argument to register, handling large struct params
  // Win64 ABI: Large structs (>8 bytes) are passed by pointer
  //----------------------------------------------------------------------------
  procedure LoadCallArgToReg(const AOp: TTigerOperand; const AReg: Byte;
    const ATargetFuncIndex: Integer; const AArgIndex: Integer);
  var
    LNeedsAddress: Boolean;
    LLocalInfo: TTigerLocalInfo;
  begin
    LNeedsAddress := False;
    
    // Check if source is a local struct that needs its address passed
    // Win64 ABI: structs >8 bytes are passed by pointer
    if (AOp.Kind = okLocal) and (not AOp.LocalHandle.IsParam) then
    begin
      // Get the local's size info
      LLocalInfo := LFunc.Locals[AOp.LocalHandle.Index];
      // If local is composite (vtVoid) and > 8 bytes, pass address
      if (LLocalInfo.LocalType = vtVoid) and (LLocalInfo.LocalSize > 8) then
        LNeedsAddress := True;
    end;
    
    if LNeedsAddress then
    begin
      // LEA reg, [RBP-offset] to get address of local struct
      EmitLeaRegRbpDisp(AReg, GetLocalOffset(AOp.LocalHandle.Index));
    end
    else
      LoadOperandToReg(AOp, AReg);
  end;

  //----------------------------------------------------------------------------
  // Store operand to stack argument slot (for args 5+, Win64 ABI)
  //----------------------------------------------------------------------------
  procedure StoreOperandToStackArg(const AOp: TTigerOperand; const AArgIndex: Integer);
  var
    LStackOffset: Int32;
    LR: TCOFFReloc;
    LDE: TTigerDataEntry;
    LDH: TTigerDataHandle;
  begin
    LStackOffset := 32 + (AArgIndex - 4) * 8;

    case AOp.Kind of
      okImmediate:
        begin
          if (AOp.ImmInt >= 0) and (AOp.ImmInt <= $FFFFFFFF) then
            EmitMovRegImm32(REG_RAX, Cardinal(AOp.ImmInt))
          else
            EmitMovRegImm64(REG_RAX, UInt64(AOp.ImmInt));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okData:
        begin
          LDH.Index := AOp.DataHandle.Index;
          LDE := FData.GetEntry(LDH);
          LR.VirtualAddress := LTextSection.Size + 3;
          LR.SymbolIndex := LRDataSymIndex;
          LR.RelocationType := IMAGE_REL_AMD64_REL32;
          LTextRelocs.Add(LR);
          EmitLeaRipRel(REG_RAX, Int32(LDE.Offset));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okGlobal:
        begin
          LDH.Index := AOp.DataHandle.Index;
          LDE := FGlobals.GetEntry(LDH);
          LR.VirtualAddress := LTextSection.Size + 3;
          LR.SymbolIndex := LDataSymIndex;
          LR.RelocationType := IMAGE_REL_AMD64_REL32;
          LTextRelocs.Add(LR);
          EmitLeaRipRel(REG_RAX, Int32(LDE.Offset));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okLocal:
        begin
          if AOp.LocalHandle.IsParam then
            EmitMovRegRbpDisp(REG_RAX, GetParamOffset(AOp.LocalHandle.Index))
          else
            EmitMovRegRbpDisp(REG_RAX, GetLocalOffset(AOp.LocalHandle.Index));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;

      okTemp:
        begin
          EmitMovRegRbpDisp(REG_RAX, GetTempOffset(AOp.TempHandle.Index));
          EmitMovRspDispReg(LStackOffset, REG_RAX);
        end;
    else
      EmitXorRegReg(REG_RAX);
      EmitMovRspDispReg(LStackOffset, REG_RAX);
    end;
  end;

  //----------------------------------------------------------------------------
  // Helper: Store call argument to stack slot, handling large struct params
  // Win64 ABI: Large structs (>8 bytes) are passed by pointer
  //----------------------------------------------------------------------------
  procedure StoreCallArgToStack(const AOp: TTigerOperand; const AArgIndex: Integer;
    const ATargetFuncIndex: Integer);
  var
    LStackOffset: Int32;
    LTargetFunc: TTigerFuncInfo;
    LNeedsAddress: Boolean;
  begin
    LStackOffset := 32 + (AArgIndex - 4) * 8;
    LNeedsAddress := False;
    
    // Check if target param expects a large struct (passed by pointer)
    if ATargetFuncIndex >= 0 then
    begin
      LTargetFunc := FCode.GetFunc(ATargetFuncIndex);
      if (AArgIndex >= 0) and (AArgIndex < Length(LTargetFunc.Params)) then
      begin
        // Large struct param (>8 bytes): callee expects a pointer
        if LTargetFunc.Params[AArgIndex].ParamSize > 8 then
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

  //----------------------------------------------------------------------------
  // Helper: Write COFF symbol name (8 bytes) — short or string table reference
  //----------------------------------------------------------------------------
  procedure WriteSymbolName(const AStream: TMemoryStream;
    const AName: AnsiString; const AStringTable: TMemoryStream);
  var
    LNameBytes: array[0..7] of Byte;
    LStrTabOffset: Cardinal;
  begin
    if Length(AName) <= 8 then
    begin
      // Short name: store directly in 8-byte field, zero-padded
      FillChar(LNameBytes, 8, 0);
      if Length(AName) > 0 then
        Move(AName[1], LNameBytes[0], Length(AName));
      AStream.WriteBuffer(LNameBytes[0], 8);
    end
    else
    begin
      // Long name: store as /offset reference into string table
      // First 4 bytes = 0 (marker for long name), next 4 bytes = offset in string table
      LStrTabOffset := AStringTable.Size + 4;  // +4 for the size field at start of string table
      AStream.WriteData(Cardinal(0));          // Zeroes marker
      AStream.WriteData(Cardinal(LStrTabOffset));
      // Add the name to the string table
      AStringTable.WriteBuffer(AName[1], Length(AName));
      AStringTable.WriteData(Byte(0));  // Null terminator
    end;
  end;

  //----------------------------------------------------------------------------
  // Helper: Write section symbol with auxiliary record (2 symbol entries)
  //----------------------------------------------------------------------------
  procedure WriteSectionSymbol(const AStream: TMemoryStream;
    const AName: AnsiString; const ASectionNumber: SmallInt;
    const ASectionSize: Cardinal; const ANumRelocs: Word;
    const AStringTable: TMemoryStream);
  var
    LAuxBytes: array[0..17] of Byte;
  begin
    // Primary symbol entry (18 bytes)
    WriteSymbolName(AStream, AName, AStringTable);
    AStream.WriteData(Cardinal(0));       // Value = 0 for section symbols
    AStream.WriteData(ASectionNumber);    // SectionNumber (SmallInt)
    AStream.WriteData(Word(0));           // Type = 0
    AStream.WriteData(Byte(COFF_SYM_CLASS_STATIC)); // StorageClass
    AStream.WriteData(Byte(1));           // NumberOfAuxSymbols = 1

    // Auxiliary record for section symbol (18 bytes)
    FillChar(LAuxBytes, SizeOf(LAuxBytes), 0);
    // Bytes 0-3: Length (section raw data size)
    PCardinal(@LAuxBytes[0])^ := ASectionSize;
    // Bytes 4-5: NumberOfRelocations
    PWord(@LAuxBytes[4])^ := ANumRelocs;
    // Bytes 6-7: NumberOfLinenumbers = 0
    // Bytes 8-11: CheckSum = 0
    // Bytes 12-13: Number (associated section) = 0
    // Bytes 14: Selection = 0
    // Bytes 15-17: Unused
    AStream.WriteBuffer(LAuxBytes[0], 18);
  end;

begin
  Result := nil;

  LOutput := TMemoryStream.Create();
  LTextSection := TMemoryStream.Create();
  LRDataSection := TMemoryStream.Create();
  LDataSection := TMemoryStream.Create();
  LXDataSection := TMemoryStream.Create();
  LPDataSection := TMemoryStream.Create();
  LTextRelocs := TList<TCOFFReloc>.Create();
  LPDataRelocs := TList<TCOFFReloc>.Create();
  LXDataRelocs := TList<TCOFFReloc>.Create();
  LJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LCallFixups := TList<TPair<Cardinal, Integer>>.Create();
  LFuncAddrFixups := TList<TPair<Cardinal, Integer>>.Create();
  LStringTable := TMemoryStream.Create();
  try
    //==========================================================================
    // STEP 1: Assign section numbers (1-based for COFF)
    //==========================================================================
    LSectText  := 1;
    LSectRData := 2;
    LSectData  := 3;
    LSectXData := 4;
    LSectPData := 5;
    LNumSections := 5;

    // Check for exports → .drectve section
    LHasExports := False;
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      if LFunc.IsPublic then
      begin
        LHasExports := True;
        Break;
      end;
    end;

    LHasDrectve := LHasExports;
    if LHasDrectve then
      Inc(LNumSections);  // .drectve gets section 6

    //==========================================================================
    // STEP 2: Pre-compute symbol table indices
    //==========================================================================
    // Section symbols: each takes 2 slots (symbol + aux record)
    // .text  = index 0 (aux at 1)
    // .rdata = index 2 (aux at 3)
    // .data  = index 4 (aux at 5)
    // .xdata = index 6 (aux at 7)
    // .pdata = index 8 (aux at 9)
    LTextSymIndex  := 0;
    LRDataSymIndex := 2;
    LDataSymIndex  := 4;
    LXDataSymIndex := 6;
    LPDataSymIndex := 8;
    LFirstFuncSymIndex := 10;  // After 5 section symbols × 2 slots each

    // Function symbols: one per function
    LFirstImportSymIndex := LFirstFuncSymIndex + Cardinal(FCode.GetFuncCount());

    // Check for SEH
    LHasSEH := False;
    LInitExceptionsIndex := -1;
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      if Length(LFunc.ExceptionScopes) > 0 then
      begin
        LHasSEH := True;
        Break;
      end;
    end;

    if LHasSEH then
    begin
      // Find Tiger_InitExceptions function index
      for LI := 0 to FCode.GetFuncCount() - 1 do
      begin
        LFunc := FCode.GetFunc(LI);
        if LFunc.FuncName = 'Tiger_InitExceptions' then
        begin
          LInitExceptionsIndex := LI;
          Break;
        end;
      end;
    end;

    // Import extern symbols: one per import entry
    // If SEH, also add __C_specific_handler as an extern
    LSEHHandlerSymIndex := LFirstImportSymIndex + Cardinal(FImports.GetCount());
    if LHasSEH then
      LTotalSymbols := LSEHHandlerSymIndex + 1
    else
      LTotalSymbols := LFirstImportSymIndex + Cardinal(FImports.GetCount());

    //==========================================================================
    // STEP 3: Build .rdata section (read-only data)
    //==========================================================================
    if FData.GetSize() > 0 then
      LRDataSection.WriteBuffer(FData.GetDataPointer()^, FData.GetSize());
    if LRDataSection.Size = 0 then
      LRDataSection.WriteData(Byte(0));
    AlignStream(LRDataSection, 4);

    //==========================================================================
    // STEP 4: Build .data section (writable globals)
    //==========================================================================
    if FGlobals.GetSize() > 0 then
      LDataSection.WriteBuffer(FGlobals.GetDataPointer()^, FGlobals.GetSize());
    if LDataSection.Size = 0 then
      LDataSection.WriteData(Byte(0));
    AlignStream(LDataSection, 4);

    //==========================================================================
    // STEP 5: Generate x64 code for each function
    //==========================================================================
    SetLength(LFuncOffsets, FCode.GetFuncCount());
    SetLength(LFuncEndOffsets, FCode.GetFuncCount());
    SetLength(LAllLabelOffsets, FCode.GetFuncCount());
    SetLength(LFuncUnwindInfoOffsets, FCode.GetFuncCount());

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);

      // Record function offset in .text
      LFuncOffsets[LI] := LTextSection.Size;

      // Initialize label offsets for this function
      SetLength(LLabelOffsets, Length(LFunc.Labels));
      for LJ := 0 to High(LLabelOffsets) do
        LLabelOffsets[LJ] := 0;

      // Calculate stack frame size (same algorithm as GeneratePE)
      LLocalsSize := 0;
      for LJ := 0 to High(LFunc.Locals) do
        LLocalsSize := LLocalsSize + LFunc.Locals[LJ].LocalSize;

      // Scan for maximum outgoing call argument count
      LMaxCallArgs := 0;
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        if LFunc.Instructions[LJ].Kind in [ikCallImport, ikCall, ikCallIndirect] then
        begin
          if Length(LFunc.Instructions[LJ].Args) > LMaxCallArgs then
            LMaxCallArgs := Length(LFunc.Instructions[LJ].Args);
        end;
      end;
      // Minimum 4 slots (32 bytes shadow) for any function that makes calls
      if LMaxCallArgs > 0 then
      begin
        if LMaxCallArgs < 4 then
          LMaxCallArgs := 4;
      end;
      LOutgoingArgSpace := Cardinal(LMaxCallArgs) * 8;

      if LFunc.IsVariadic then
        LStackFrameSize := 40 + LOutgoingArgSpace + Cardinal(4 * 8) + Cardinal(LLocalsSize) + Cardinal(LFunc.TempCount * 8)
      else
        LStackFrameSize := 40 + LOutgoingArgSpace + Cardinal(Length(LFunc.Params) * 8) + Cardinal(LLocalsSize) + Cardinal(LFunc.TempCount * 8);
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

      // Function prologue: PUSH RBP; MOV RBP, RSP; SUB RSP, N
      EmitPushReg(REG_RBP);
      EmitMovRegReg(REG_RBP, REG_RSP);
      if LStackFrameSize > 0 then
        EmitSubRspImm32(LStackFrameSize);

      // Save incoming parameters to stack (Win64: RCX, RDX, R8, R9)
      // For large struct returns (>8 bytes): RCX = hidden return ptr, params shift
      if LFunc.IsVariadic then
      begin
        EmitMovRbpDispReg(-40, REG_RCX);
        EmitMovRbpDispReg(-48, REG_RDX);
        EmitMovRbpDispReg(-56, REG_R8);
        EmitMovRbpDispReg(-64, REG_R9);
      end
      else if LFunc.ReturnSize > 8 then
      begin
        // Large struct return: RCX = hidden return pointer (shifts all params)
        // Save hidden return pointer to [RBP-32]
        EmitMovRbpDispReg(-32, REG_RCX);
        // Params are shifted: RDX=param0, R8=param1, R9=param2, stack=param3+
        if Length(LFunc.Params) > 0 then
          EmitMovRbpDispReg(GetParamOffset(0), REG_RDX);
        if Length(LFunc.Params) > 1 then
          EmitMovRbpDispReg(GetParamOffset(1), REG_R8);
        if Length(LFunc.Params) > 2 then
          EmitMovRbpDispReg(GetParamOffset(2), REG_R9);
        // Param 3+ come from caller's stack, no register save needed
      end
      else
      begin
        if Length(LFunc.Params) > 0 then
          EmitMovRbpDispReg(GetParamOffset(0), REG_RCX);
        if Length(LFunc.Params) > 1 then
          EmitMovRbpDispReg(GetParamOffset(1), REG_RDX);
        if Length(LFunc.Params) > 2 then
          EmitMovRbpDispReg(GetParamOffset(2), REG_R8);
        if Length(LFunc.Params) > 3 then
          EmitMovRbpDispReg(GetParamOffset(3), REG_R9);
      end;

      // For entry point functions using SEH, call Tiger_InitExceptions first
      if LHasSEH and (LInitExceptionsIndex >= 0) then
      begin
        if (LFunc.IsEntryPoint and (FOutputType <> otDll)) or
           (LFunc.IsDllEntry and (FOutputType = otDll)) then
        begin
          LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 1, LInitExceptionsIndex));
          EmitCallRel32(0);
        end;
      end;

      // Process each instruction
      for LJ := 0 to High(LFunc.Instructions) do
      begin
        LInstr := LFunc.Instructions[LJ];

        case LInstr.Kind of
          ikLabel:
            begin
              if LInstr.LabelTarget.IsValid() then
                LLabelOffsets[LInstr.LabelTarget.Index] := LTextSection.Size;
            end;

          ikCallImport:
            begin
              // Win64 ABI: stack args first (5+), then register args (1-4)
              for LK := 4 to High(LInstr.Args) do
                StoreOperandToStackArg(LInstr.Args[LK], LK);

              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RCX);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RDX);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_R8);
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_R9);

              // COFF: CALL rel32 with relocation to extern import symbol
              // (PE uses CALL [RIP+disp32] through IAT; COFF uses direct CALL rel32)
              LReloc.VirtualAddress := LTextSection.Size + 1;  // disp32 at offset 1 of E8
              LReloc.SymbolIndex := LFirstImportSymIndex + Cardinal(LInstr.ImportTarget.Index);
              LReloc.RelocationType := IMAGE_REL_AMD64_REL32;
              LTextRelocs.Add(LReloc);
              EmitCallRel32(0);  // Placeholder — linker resolves

              // Sign-extend 32-bit return values to 64-bit
              if FImports.GetEntryByIndex(LInstr.ImportTarget.Index).ReturnType = vtInt32 then
                EmitMovsxdRaxEax();

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCall:
            begin
              // Call local function — resolved via backpatch (same .text section)
              for LK := 4 to High(LInstr.Args) do
                StoreCallArgToStack(LInstr.Args[LK], LK, LInstr.FuncTarget.Index);

              // Load register arguments (with struct handling)
              if Length(LInstr.Args) > 0 then
                LoadCallArgToReg(LInstr.Args[0], REG_RCX, LInstr.FuncTarget.Index, 0);
              if Length(LInstr.Args) > 1 then
                LoadCallArgToReg(LInstr.Args[1], REG_RDX, LInstr.FuncTarget.Index, 1);
              if Length(LInstr.Args) > 2 then
                LoadCallArgToReg(LInstr.Args[2], REG_R8, LInstr.FuncTarget.Index, 2);
              if Length(LInstr.Args) > 3 then
                LoadCallArgToReg(LInstr.Args[3], REG_R9, LInstr.FuncTarget.Index, 3);

              // CALL rel32 — displacement patched after all code generated
              LCallFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 1, LInstr.FuncTarget.Index));
              EmitCallRel32(0);

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikCallIndirect:
            begin
              for LK := 4 to High(LInstr.Args) do
                StoreOperandToStackArg(LInstr.Args[LK], LK);

              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_R9);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_R8);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_RDX);
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_RCX);

              // Load function pointer into RAX, then CALL RAX
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitCallReg(REG_RAX);

              if LInstr.Dest.IsValid() then
                StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikStore:
            begin
              LoadOperandToReg(LInstr.Op2, REG_RAX);
              if LInstr.Op1.LocalHandle.IsParam then
                EmitMovRbpDispReg(GetParamOffset(LInstr.Op1.LocalHandle.Index), REG_RAX)
              else
                EmitMovRbpDispReg(GetLocalOffset(LInstr.Op1.LocalHandle.Index), REG_RAX);
            end;

          ikLoad:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikAddressOf:
            begin
              // Dest = address of local/param slot (always LEA)
              // Op1 = local handle
              // Note: For by-ref params (structs >8 bytes), the caller should emit
              // a subsequent load instruction to get the actual struct pointer.
              if LInstr.Op1.LocalHandle.IsParam then
                EmitLeaRegRbpDisp(REG_RAX, GetParamOffset(LInstr.Op1.LocalHandle.Index))
              else
                EmitLeaRegRbpDisp(REG_RAX, GetLocalOffset(LInstr.Op1.LocalHandle.Index));
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikLoadPtr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitMovRegMemReg(REG_RAX, REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikStorePtr:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              LoadOperandToReg(LInstr.Op2, REG_RCX);
              EmitMovMemRegReg(REG_RAX, REG_RCX);
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

          ikJump:
            begin
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 1, LInstr.LabelTarget.Index));
              EmitJmpRel32(0);
            end;

          ikJumpIf:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitTestRegReg(REG_RAX, REG_RAX);
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 2, LInstr.LabelTarget.Index));
              EmitJnzRel32(0);
            end;

          ikJumpIfNot:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitTestRegReg(REG_RAX, REG_RAX);
              LJumpFixups.Add(TPair<Cardinal, Integer>.Create(LTextSection.Size + 2, LInstr.LabelTarget.Index));
              EmitJzRel32(0);
            end;

          ikVaCount:
            begin
              EmitMovRegRbpDisp(REG_RAX, -40);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikVaArgAt:
            begin
              LoadOperandToReg(LInstr.Op1, REG_RAX);
              EmitAddRaxImm32(Length(LFunc.Params) + 1);
              EmitCmpRaxImm32(4);
              EmitMovRegReg(REG_R10, REG_RAX);
              EmitJgeRel8(20);
              // Register arg path
              EmitShlRaxImm8(3);
              EmitAddRaxImm32(40);
              EmitNegRax();
              EmitMovRegRbpPlusRax(REG_RAX);
              EmitJmpRel8(18);
              // Stack arg path
              EmitMovRegReg(REG_RAX, REG_R10);
              EmitShlRaxImm8(3);
              EmitAddRaxImm32(16);
              EmitMovRegRbpPlusRax(REG_RAX);
              StoreTempFromReg(LInstr.Dest.Index, REG_RAX);
            end;

          ikReturn:
            begin
              EmitMovRegReg(REG_RSP, REG_RBP);
              EmitPopReg(REG_RBP);
              EmitRet();
            end;

          ikReturnValue:
            begin
              if LFunc.ReturnSize > 8 then
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

      // Ensure function ends properly (for functions without explicit return)
      // Check if we need to add an implicit epilogue
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

      // Record end of function code (before alignment padding)
      LFuncEndOffsets[LI] := LTextSection.Size;

      // Backpatch jump targets within this function
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

      // Save label offsets for SEH SCOPE_TABLE
      LAllLabelOffsets[LI] := Copy(LLabelOffsets);

      AlignStream(LTextSection, 16);
    end;

    // Backpatch local function calls (cross-function, within .text)
    for LI := 0 to LCallFixups.Count - 1 do
    begin
      LCodeOffset := LCallFixups[LI].Key;       // Offset of disp32 field
      LDataIndex := LCallFixups[LI].Value;       // Target function index
      LDisp := Int32(LFuncOffsets[LDataIndex]) - Int32(LCodeOffset + 4);
      LTextSection.Position := LCodeOffset;
      LTextSection.WriteData(LDisp);
    end;
    LTextSection.Position := LTextSection.Size;

    // Backpatch function address references (LEA to function, within .text)
    for LI := 0 to LFuncAddrFixups.Count - 1 do
    begin
      LCodeOffset := LFuncAddrFixups[LI].Key;   // Offset of LEA instruction start
      LDataIndex := LFuncAddrFixups[LI].Value;   // Target function index
      // LEA disp32 is at offset+3, RIP after = offset+7
      LTargetOffset := LFuncOffsets[LDataIndex];
      LDisp := Int32(LTargetOffset) - Int32(LCodeOffset + 7);
      LTextSection.Position := LCodeOffset + 3;
      LTextSection.WriteData(LDisp);
    end;
    LTextSection.Position := LTextSection.Size;

    if LTextSection.Size = 0 then
    begin
      Status('Error: No code generated');
      Exit;
    end;

    //==========================================================================
    // STEP 6: Generate .xdata section (UNWIND_INFO + SCOPE_TABLE)
    //==========================================================================
    // Write shared basic UNWIND_INFO (for functions without SEH)
    AlignStream(LXDataSection, 4);
    LSharedUnwindInfoOffset := LXDataSection.Size;
    LXDataSection.WriteBuffer(UNWIND_INFO[0], SizeOf(UNWIND_INFO));

    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);
      LScopeCount := Length(LFunc.ExceptionScopes);

      if LScopeCount = 0 then
      begin
        // Use shared basic UNWIND_INFO
        LFuncUnwindInfoOffsets[LI] := LSharedUnwindInfoOffset;
      end
      else
      begin
        // Per-function UNWIND_INFO with UNW_FLAG_EHANDLER
        AlignStream(LXDataSection, 4);
        LFuncUnwindInfoOffsets[LI] := LXDataSection.Size;

        LXDataSection.WriteData(Byte($09));  // Version=1, Flags=UNW_FLAG_EHANDLER
        LXDataSection.WriteData(Byte($04));  // SizeOfProlog=4
        LXDataSection.WriteData(Byte($02));  // CountOfCodes=2
        LXDataSection.WriteData(Byte($05));  // FrameRegister=RBP(5), FrameOffset=0
        LXDataSection.WriteData(Byte($04));  // Unwind code: Offset 4
        LXDataSection.WriteData(Byte($03));  // UWOP_SET_FPREG
        LXDataSection.WriteData(Byte($01));  // Unwind code: Offset 1
        LXDataSection.WriteData(Byte($50));  // UWOP_PUSH_NONVOL RBP

        // ExceptionHandler: ADDR32NB relocation to __C_specific_handler
        LReloc.VirtualAddress := LXDataSection.Size;
        LReloc.SymbolIndex := LSEHHandlerSymIndex;
        LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
        LXDataRelocs.Add(LReloc);
        LXDataSection.WriteData(Cardinal(0));  // Placeholder — linker resolves

        // SCOPE_TABLE: Count + ScopeRecord entries
        LXDataSection.WriteData(Cardinal(LScopeCount));

        for LJ := 0 to LScopeCount - 1 do
        begin
          LScope := LFunc.ExceptionScopes[LJ];

          // BeginAddress: ADDR32NB relocation to .text
          LReloc.VirtualAddress := LXDataSection.Size;
          LReloc.SymbolIndex := LTextSymIndex;
          LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
          LXDataRelocs.Add(LReloc);
          if (LScope.TryBeginLabel.Index >= 0) and
             (LScope.TryBeginLabel.Index < Length(LAllLabelOffsets[LI])) then
            LXDataSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.TryBeginLabel.Index]))
          else
            LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));

          // EndAddress: ADDR32NB relocation to .text
          LReloc.VirtualAddress := LXDataSection.Size;
          LReloc.SymbolIndex := LTextSymIndex;
          LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
          LXDataRelocs.Add(LReloc);
          if LScope.ExceptLabel.Index >= 0 then
          begin
            if LScope.ExceptLabel.Index < Length(LAllLabelOffsets[LI]) then
              LXDataSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.ExceptLabel.Index]))
            else
              LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else if LScope.FinallyLabel.Index >= 0 then
          begin
            if LScope.FinallyLabel.Index < Length(LAllLabelOffsets[LI]) then
              LXDataSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.FinallyLabel.Index]))
            else
              LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else
            LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));

          // HandlerAddress: 1 for __except, 0 for __finally (no relocation)
          if LScope.ExceptLabel.Index >= 0 then
            LXDataSection.WriteData(Cardinal(1))
          else
            LXDataSection.WriteData(Cardinal(0));

          // JumpTarget: ADDR32NB relocation to .text
          LReloc.VirtualAddress := LXDataSection.Size;
          LReloc.SymbolIndex := LTextSymIndex;
          LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
          LXDataRelocs.Add(LReloc);
          if LScope.ExceptLabel.Index >= 0 then
          begin
            if LScope.ExceptLabel.Index < Length(LAllLabelOffsets[LI]) then
              LXDataSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.ExceptLabel.Index]))
            else
              LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else if LScope.FinallyLabel.Index >= 0 then
          begin
            if LScope.FinallyLabel.Index < Length(LAllLabelOffsets[LI]) then
              LXDataSection.WriteData(Cardinal(LAllLabelOffsets[LI][LScope.FinallyLabel.Index]))
            else
              LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));
          end
          else
            LXDataSection.WriteData(Cardinal(LFuncOffsets[LI]));
        end;
      end;
    end;
    AlignStream(LXDataSection, 4);

    //==========================================================================
    // STEP 7: Generate .pdata section (RUNTIME_FUNCTION entries)
    //==========================================================================
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      // BeginAddress: ADDR32NB reloc to .text, addend = func offset
      LReloc.VirtualAddress := LPDataSection.Size;
      LReloc.SymbolIndex := LTextSymIndex;
      LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
      LPDataRelocs.Add(LReloc);
      LPDataSection.WriteData(Cardinal(LFuncOffsets[LI]));

      // EndAddress: ADDR32NB reloc to .text, addend = func end offset
      LReloc.VirtualAddress := LPDataSection.Size;
      LReloc.SymbolIndex := LTextSymIndex;
      LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
      LPDataRelocs.Add(LReloc);
      LPDataSection.WriteData(Cardinal(LFuncEndOffsets[LI]));

      // UnwindData: ADDR32NB reloc to .xdata, addend = unwind info offset
      LReloc.VirtualAddress := LPDataSection.Size;
      LReloc.SymbolIndex := LXDataSymIndex;
      LReloc.RelocationType := IMAGE_REL_AMD64_ADDR32NB;
      LPDataRelocs.Add(LReloc);
      LPDataSection.WriteData(Cardinal(LFuncUnwindInfoOffsets[LI]));
    end;
    AlignStream(LPDataSection, 4);

    //==========================================================================
    // STEP 8: Build .drectve section (linker directives)
    //==========================================================================
    LDrectveData := '';

    // Add /DEFAULTLIB directives for each unique DLL used by imports
    if FImports.GetCount() > 0 then
    begin
      // Collect unique DLL names and generate /DEFAULTLIB directives
      for LI := 0 to FImports.GetCount() - 1 do
      begin
        LEntry := FImports.GetEntryByIndex(LI);
        // Convert DLL name to import lib name (e.g., kernel32.dll → kernel32.lib)
        LExportName := ChangeFileExt(LEntry.DllName, '.lib');
        // Avoid duplicates by checking if already in the string
        if Pos(AnsiString('/DEFAULTLIB:"' + AnsiString(LExportName) + '"'), LDrectveData) = 0 then
        begin
          if Length(LDrectveData) > 0 then
            LDrectveData := LDrectveData + ' ';
          LDrectveData := LDrectveData + AnsiString('/DEFAULTLIB:"' + AnsiString(LExportName) + '"');
        end;
      end;
    end;

    // Add /EXPORT directives for each public function
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
        if Length(LDrectveData) > 0 then
          LDrectveData := LDrectveData + ' ';
        LDrectveData := LDrectveData + AnsiString('/EXPORT:' + AnsiString(LExportName));
      end;
    end;

    //==========================================================================
    // STEP 9: Assemble COFF file
    //==========================================================================
    // Layout:
    //   COFF File Header (20 bytes)
    //   Section Headers (40 bytes each)
    //   Section 1 raw data (.text)
    //   Section 1 relocations
    //   Section 2 raw data (.rdata)
    //   Section 3 raw data (.data)
    //   Section 4 raw data (.xdata)
    //   Section 4 relocations
    //   Section 5 raw data (.pdata)
    //   Section 5 relocations
    //   [Section 6 raw data (.drectve)]
    //   Symbol Table
    //   String Table

    // Calculate file offsets
    LSectionDataOffset := COFF_FILE_HEADER_SIZE + (COFF_SECTION_HEADER_SIZE * LNumSections);

    // --- Write COFF File Header ---
    LOutput.WriteData(Word($8664));           // Machine = IMAGE_FILE_MACHINE_AMD64
    LOutput.WriteData(LNumSections);          // NumberOfSections
    LOutput.WriteData(Cardinal(DateTimeToUnix(Now(), False))); // TimeDateStamp
    // PointerToSymbolTable — we'll fix this up after writing all sections
    LOutput.WriteData(Cardinal(0));           // Placeholder for PointerToSymbolTable
    LOutput.WriteData(Cardinal(LTotalSymbols)); // NumberOfSymbols
    LOutput.WriteData(Word(0));               // SizeOfOptionalHeader = 0 for .obj
    LOutput.WriteData(Word(0));               // Characteristics = 0 for .obj

    // --- Write Section Headers ---
    // We need to calculate raw data offsets incrementally
    // Start after all headers
    // .text section header
    LOutput.WriteBuffer(AnsiString('.text'#0#0#0)[1], 8);  // Name
    LOutput.WriteData(Cardinal(0));                          // VirtualSize (0 for .obj)
    LOutput.WriteData(Cardinal(0));                          // VirtualAddress (0 for .obj)
    LOutput.WriteData(Cardinal(LTextSection.Size));          // SizeOfRawData
    LOutput.WriteData(Cardinal(LSectionDataOffset));         // PointerToRawData
    LOutput.WriteData(Cardinal(LSectionDataOffset + Cardinal(LTextSection.Size))); // PointerToRelocations
    LOutput.WriteData(Cardinal(0));                          // PointerToLinenumbers
    LOutput.WriteData(Word(LTextRelocs.Count));              // NumberOfRelocations
    LOutput.WriteData(Word(0));                              // NumberOfLinenumbers
    LOutput.WriteData(Cardinal(COFF_SCN_TEXT));              // Characteristics
    LSymTabOffset := LSectionDataOffset + Cardinal(LTextSection.Size) +
      Cardinal(LTextRelocs.Count) * COFF_RELOCATION_SIZE;

    // .rdata section header
    LOutput.WriteBuffer(AnsiString('.rdata'#0#0)[1], 8);
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(LRDataSection.Size));
    LOutput.WriteData(Cardinal(LSymTabOffset));              // PointerToRawData
    LOutput.WriteData(Cardinal(0));                          // No relocations for .rdata
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Word(0));
    LOutput.WriteData(Word(0));
    LOutput.WriteData(Cardinal(COFF_SCN_RDATA));
    LSymTabOffset := LSymTabOffset + Cardinal(LRDataSection.Size);

    // .data section header
    LOutput.WriteBuffer(AnsiString('.data'#0#0#0)[1], 8);
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(LDataSection.Size));
    LOutput.WriteData(Cardinal(LSymTabOffset));
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Word(0));
    LOutput.WriteData(Word(0));
    LOutput.WriteData(Cardinal(COFF_SCN_DATA));
    LSymTabOffset := LSymTabOffset + Cardinal(LDataSection.Size);

    // .xdata section header
    LOutput.WriteBuffer(AnsiString('.xdata'#0#0)[1], 8);
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(LXDataSection.Size));
    LOutput.WriteData(Cardinal(LSymTabOffset));
    LOutput.WriteData(Cardinal(LSymTabOffset + Cardinal(LXDataSection.Size))); // PointerToRelocations
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Word(LXDataRelocs.Count));
    LOutput.WriteData(Word(0));
    LOutput.WriteData(Cardinal(COFF_SCN_XDATA));
    LSymTabOffset := LSymTabOffset + Cardinal(LXDataSection.Size) +
      Cardinal(LXDataRelocs.Count) * COFF_RELOCATION_SIZE;

    // .pdata section header
    LOutput.WriteBuffer(AnsiString('.pdata'#0#0)[1], 8);
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Cardinal(LPDataSection.Size));
    LOutput.WriteData(Cardinal(LSymTabOffset));
    LOutput.WriteData(Cardinal(LSymTabOffset + Cardinal(LPDataSection.Size))); // PointerToRelocations
    LOutput.WriteData(Cardinal(0));
    LOutput.WriteData(Word(LPDataRelocs.Count));
    LOutput.WriteData(Word(0));
    LOutput.WriteData(Cardinal(COFF_SCN_PDATA));
    LSymTabOffset := LSymTabOffset + Cardinal(LPDataSection.Size) +
      Cardinal(LPDataRelocs.Count) * COFF_RELOCATION_SIZE;

    // .drectve section header (if needed)
    if LHasDrectve then
    begin
      LOutput.WriteBuffer(AnsiString('.drectve')[1], 8);
      LOutput.WriteData(Cardinal(0));
      LOutput.WriteData(Cardinal(0));
      LOutput.WriteData(Cardinal(Length(LDrectveData)));
      LOutput.WriteData(Cardinal(LSymTabOffset));
      LOutput.WriteData(Cardinal(0));  // No relocations
      LOutput.WriteData(Cardinal(0));
      LOutput.WriteData(Word(0));
      LOutput.WriteData(Word(0));
      LOutput.WriteData(Cardinal(COFF_SCN_DRECTVE));
      LSymTabOffset := LSymTabOffset + Cardinal(Length(LDrectveData));
    end;

    // Fix up PointerToSymbolTable in file header
    LOutput.Position := 8;  // Offset of PointerToSymbolTable in COFF header
    LOutput.WriteData(Cardinal(LSymTabOffset));
    LOutput.Position := LOutput.Size;

    // --- Write Section Raw Data and Relocations ---

    // .text raw data
    LTextSection.Position := 0;
    LOutput.CopyFrom(LTextSection, LTextSection.Size);

    // .text relocations
    for LI := 0 to LTextRelocs.Count - 1 do
    begin
      LReloc := LTextRelocs[LI];
      LOutput.WriteData(Cardinal(LReloc.VirtualAddress));
      LOutput.WriteData(Cardinal(LReloc.SymbolIndex));
      LOutput.WriteData(Word(LReloc.RelocationType));
    end;

    // .rdata raw data
    LRDataSection.Position := 0;
    LOutput.CopyFrom(LRDataSection, LRDataSection.Size);

    // .data raw data
    LDataSection.Position := 0;
    LOutput.CopyFrom(LDataSection, LDataSection.Size);

    // .xdata raw data
    LXDataSection.Position := 0;
    LOutput.CopyFrom(LXDataSection, LXDataSection.Size);

    // .xdata relocations
    for LI := 0 to LXDataRelocs.Count - 1 do
    begin
      LReloc := LXDataRelocs[LI];
      LOutput.WriteData(Cardinal(LReloc.VirtualAddress));
      LOutput.WriteData(Cardinal(LReloc.SymbolIndex));
      LOutput.WriteData(Word(LReloc.RelocationType));
    end;

    // .pdata raw data
    LPDataSection.Position := 0;
    LOutput.CopyFrom(LPDataSection, LPDataSection.Size);

    // .pdata relocations
    for LI := 0 to LPDataRelocs.Count - 1 do
    begin
      LReloc := LPDataRelocs[LI];
      LOutput.WriteData(Cardinal(LReloc.VirtualAddress));
      LOutput.WriteData(Cardinal(LReloc.SymbolIndex));
      LOutput.WriteData(Word(LReloc.RelocationType));
    end;

    // .drectve raw data
    if LHasDrectve and (Length(LDrectveData) > 0) then
      LOutput.WriteBuffer(LDrectveData[1], Length(LDrectveData));

    // --- Write Symbol Table ---
    // Section symbols (each with 1 aux record = 2 symbol entries)

    WriteSectionSymbol(LOutput, '.text', LSectText,
      LTextSection.Size, Word(LTextRelocs.Count), LStringTable);

    WriteSectionSymbol(LOutput, '.rdata', LSectRData,
      LRDataSection.Size, 0, LStringTable);

    WriteSectionSymbol(LOutput, '.data', LSectData,
      LDataSection.Size, 0, LStringTable);

    WriteSectionSymbol(LOutput, '.xdata', LSectXData,
      LXDataSection.Size, Word(LXDataRelocs.Count), LStringTable);

    WriteSectionSymbol(LOutput, '.pdata', LSectPData,
      LPDataSection.Size, Word(LPDataRelocs.Count), LStringTable);

    // Function symbols
    for LI := 0 to FCode.GetFuncCount() - 1 do
    begin
      LFunc := FCode.GetFunc(LI);

      // Compute the symbol name (same mangling as export)
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

      WriteSymbolName(LOutput, AnsiString(LExportName), LStringTable);
      LOutput.WriteData(Cardinal(LFuncOffsets[LI]));   // Value = offset in .text
      LOutput.WriteData(LSectText);                     // SectionNumber = .text
      LOutput.WriteData(Word(COFF_SYM_DTYPE_FUNCTION)); // Type = function
      if LFunc.IsPublic then
        LOutput.WriteData(Byte(COFF_SYM_CLASS_EXTERNAL))
      else
        LOutput.WriteData(Byte(COFF_SYM_CLASS_STATIC));
      LOutput.WriteData(Byte(0));                       // NumberOfAuxSymbols = 0
    end;

    // Import (extern) symbols
    for LI := 0 to FImports.GetCount() - 1 do
    begin
      LEntry := FImports.GetEntryByIndex(LI);
      WriteSymbolName(LOutput, AnsiString(LEntry.FuncName), LStringTable);
      LOutput.WriteData(Cardinal(0));                    // Value = 0 (undefined)
      LOutput.WriteData(SmallInt(0));                    // SectionNumber = 0 (UNDEFINED)
      LOutput.WriteData(Word(COFF_SYM_DTYPE_FUNCTION));  // Type = function
      LOutput.WriteData(Byte(COFF_SYM_CLASS_EXTERNAL));  // StorageClass = EXTERNAL
      LOutput.WriteData(Byte(0));                        // NumberOfAuxSymbols = 0
    end;

    // __C_specific_handler extern symbol (if SEH is used)
    if LHasSEH then
    begin
      WriteSymbolName(LOutput, '__C_specific_handler', LStringTable);
      LOutput.WriteData(Cardinal(0));
      LOutput.WriteData(SmallInt(0));
      LOutput.WriteData(Word(COFF_SYM_DTYPE_FUNCTION));
      LOutput.WriteData(Byte(COFF_SYM_CLASS_EXTERNAL));
      LOutput.WriteData(Byte(0));
    end;

    // --- Write String Table ---
    // First 4 bytes = total size of string table (including this size field)
    LOutput.WriteData(Cardinal(LStringTable.Size + 4));
    if LStringTable.Size > 0 then
    begin
      LStringTable.Position := 0;
      LOutput.CopyFrom(LStringTable, LStringTable.Size);
    end;

    // --- Return result ---
    SetLength(Result, LOutput.Size);
    LOutput.Position := 0;
    LOutput.ReadBuffer(Result[0], LOutput.Size);

    Status('COFF generated: %d bytes, %d functions, %d relocations',
      [Length(Result), FCode.GetFuncCount(),
       LTextRelocs.Count + LPDataRelocs.Count + LXDataRelocs.Count]);

  finally
    LStringTable.Free();
    LFuncAddrFixups.Free();
    LCallFixups.Free();
    LJumpFixups.Free();
    LXDataRelocs.Free();
    LPDataRelocs.Free();
    LTextRelocs.Free();
    LPDataSection.Free();
    LXDataSection.Free();
    LDataSection.Free();
    LRDataSection.Free();
    LTextSection.Free();
    LOutput.Free();
  end;
end;


function TTigerWin64Backend.GenerateLib(): TBytes;
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
      // Long name would need /offset into longnames member (not implemented yet)
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

    // Mode (8 bytes) - '100666' for regular file
    Move(AnsiString('100666')[1], LHdr[40], 6);

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

  // First, generate the COFF .obj content
  LObjData := GenerateCOFF();
  if Length(LObjData) = 0 then
  begin
    Status('Error: COFF generation failed, cannot create .lib');
    Exit;
  end;

  LOutput := TMemoryStream.Create();
  LSymbolNames := TStringList.Create();
  try
    // Collect public symbol names for the linker member
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
    //   First Linker Member (symbol table for link.exe)
    //   Object Member (the COFF .obj data)
    //==========================================================================

    // --- Write archive signature ---
    LOutput.WriteBuffer(AnsiString('!<arch>'#10)[1], 8);

    //==========================================================================
    // First Linker Member ("/")
    // Layout:
    //   4 bytes: Number of symbols (big-endian!)
    //   4 bytes × N: Offsets to archive members (big-endian!)
    //   N null-terminated symbol name strings
    //==========================================================================
    // Calculate the offset where the object member will start:
    //   8 (signature) + 60 (linker member header) + linker_member_size + padding
    //   + 60 (object member header)

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

    // Write member offsets (big-endian) — all symbols point to the single .obj member
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
    // Object Member (the .obj file)
    //==========================================================================
    // Compute member name from output path
    LMemberName := AnsiString(ExtractFileName(ChangeFileExt(FOutputPath, '.obj')));
    if Length(LMemberName) = 0 then
      LMemberName := 'output.obj';

    // Write object member header
    WriteARHeader(LOutput, LMemberName, Length(LObjData), LTimestamp);

    // Write the COFF .obj data
    LOutput.WriteBuffer(LObjData[0], Length(LObjData));

    // Pad to 2-byte alignment
    if (LOutput.Size mod 2) <> 0 then
      LOutput.WriteData(Byte(#10));

    // --- Return result ---
    SetLength(Result, LOutput.Size);
    LOutput.Position := 0;
    LOutput.ReadBuffer(Result[0], LOutput.Size);

    Status('LIB generated: %d bytes, %d public symbols, %d bytes object data',
      [Length(Result), LNumSymbols, Length(LObjData)]);

  finally
    LSymbolNames.Free();
    LOutput.Free();
  end;
end;


end.
