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
    function GenerateMachO(const AFileType: Cardinal; const AIsDylib: Boolean): TBytes;
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
  MH_OBJECT      = 1;
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
  CSMAGIC_REQUIREMENTS = $FADE0C01;
  CSMAGIC_BLOBWRAPPER = $FADE0B01;
  CSSLOT_CODEDIRECTORY = 0;
  CSSLOT_REQUIREMENTS = 2;
  CSSLOT_ALTERNATE_CODEDIRECTORY = $1000;
  CSSLOT_CMS_SIGNATURE = $10000;
  CS_CD_FLAG_ADHOC = $2;
  CS_CD_FLAG_LINKER_SIGNED = $20000;
  CS_HASHTYPE_SHA1 = 1;
  CS_HASHTYPE_SHA256 = 2;
  CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE = 88;  // version 0x20400 layout (incl. scatter/team/codeLimit64/execSeg)
  CODE_SIGNATURE_IDENTIFIER_MIN_SIZE = 1;   // minimal identifier (null byte) required by codesign
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

  // Exception handling: TigerExceptFrame size (Prev 8 + sigjmp_buf 196 + Type 4, aligned to 16)
  MACOS64_EXCEPT_FRAME_SIZE = 208;
  // Set True to disable all SEH emission (inits + try/end) to isolate bus fault; restore False for normal use
  SEH_EMIT_DISABLED = False;

  // Darwin/macOS ARM64 syscall: x16 = number, x0-x5 = args, SVC #0x80, return in x0
  // Map Linux x86_64 syscall numbers to Darwin (BSD) numbers for source compatibility
  LINUX_SYS_READ  = 0;   // Linux x64 read
  LINUX_SYS_WRITE = 1;   // Linux x64 write
  LINUX_SYS_EXIT  = 60;  // Linux x64 exit
  DARWIN_SYS_READ  = 3;  // Darwin read
  DARWIN_SYS_WRITE = 4;  // Darwin write
  DARWIN_SYS_EXIT  = 1;  // Darwin exit

  PAGE_SIZE = 4096;
  SEGMENT_PAGE_SIZE = 16384;  // ARM64 macOS uses 16KB pages; segment fileoff/vmsize must be 16K aligned
  SEG_TEXT_VADDR = $0000000100000000;  // Canonical arm64 executable __TEXT base
  SEG_DATA_VADDR = $0000000100001000;  // Keep same vmaddr-fileoff slide across segments

function Sha1FromUTF8String(const Str : string) : string;
var
  LSHA1 : THashSHA1;
begin
  LSHA1 := THashSHA1.Create;
  LSHA1.Update(TEncoding.UTF8.GetBytes(Str));
  Result := LSHA1.HashAsString;
end;

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
  function MakeArHeaderField(const AValue: AnsiString; const AWidth: Integer): AnsiString;
  begin
    Result := AValue;
    if Length(Result) > AWidth then
      Result := Copy(Result, 1, AWidth);
    while Length(Result) < AWidth do
      Result := Result + ' ';
  end;

  function GenerateArArchiveSingleObject(const AMemberName: AnsiString; const AObjectBytes: TBytes): TBytes;
  var
    S: TMemoryStream;
    LHeader: AnsiString;
    LSizeStr: AnsiString;
    LPad: Byte;
  begin
    Result := nil;
    S := TMemoryStream.Create();
    try
      // Global header
      LHeader := '!<arch>'#10;
      if Length(LHeader) > 0 then
        S.WriteBuffer(LHeader[1], Length(LHeader));

      // Member header (BSD ar)
      // name(16) mtime(12) uid(6) gid(6) mode(8) size(10) "`\n"(2)  = 60 bytes
      // NOTE: Tiger.Linker.MachO only supports simple names; keep <= 16 bytes.
      LSizeStr := AnsiString(IntToStr(Length(AObjectBytes)));
      LHeader :=
        MakeArHeaderField(AMemberName, 16) +
        MakeArHeaderField('0', 12) +
        MakeArHeaderField('0', 6) +
        MakeArHeaderField('0', 6) +
        MakeArHeaderField('100644', 8) +
        MakeArHeaderField(LSizeStr, 10) +
        '`'#10;
      S.WriteBuffer(LHeader[1], Length(LHeader));

      // Member data
      if Length(AObjectBytes) > 0 then
        S.WriteBuffer(AObjectBytes[0], Length(AObjectBytes));
      // Pad to even
      if (S.Size mod 2) <> 0 then
      begin
        LPad := 10;
        S.WriteBuffer(LPad, 1);
      end;

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
begin
  case FOutputType of
    otExe:
      Result := GenerateMachO(MH_EXECUTE, False);
    otDll:
      Result := GenerateMachO(MH_DYLIB, True);
    otObj:
      Result := GenerateMachO(MH_OBJECT, False);
    otLib:
    begin
      // Emit a single-member BSD ar archive containing one Mach-O relocatable object.
      // Keep the member name short (<=16) because Tiger.Linker.MachO ParseARLibrary
      // only supports simple member names.
      var LObj := GenerateMachO(MH_OBJECT, False);
      // NOTE: Don't include a trailing '/' in the filename field; macOS `ar -x`
      // treats it as a path separator and extraction fails.
      Result := GenerateArArchiveSingleObject('tiger.o', LObj);
    end;
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

function TTigerMacOS64Backend.GenerateMachO(const AFileType: Cardinal; const AIsDylib: Boolean): TBytes;
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
  LFuncAddrFixups: TList<TPair<Cardinal, Integer>>;  // Code offset -> (func index shl 8 or reg) for @func
  LLabelOffsets: TDictionary<Integer, Cardinal>;
  LFloatTemps: TDictionary<Integer, Boolean>;
  LJumpFixups: TList<TPair<Cardinal, Integer>>;
  LCondJumpFixups: TList<TPair<Cardinal, Integer>>;
  LForwardJumpFixups: TList<TPair<Cardinal, Cardinal>>;  // (jump_offset, target_offset) for forward jumps
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
  LSymtabBytes, LStrtabBytes: TBytes;
  LBindOff: Cardinal;
  LExportOff, LSymtabOff, LStrtabOff: Cardinal;
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

  LHasSEH: Boolean;
  LPushExceptFrameIdx, LPopExceptFrameIdx, LGetExceptFrameIdx: Integer;
  LInitExceptionsIdx, LInitSignalsIdx: Integer;
  LRaiseCodeIdx: Integer;
  LSigsetjmpIdx: Integer;
  LTryBeginLabels, LExceptLabels, LFinallyLabels, LEndLabels: TDictionary<Integer, Integer>;
  LExceptFrameSize, LExceptFrameBaseOffset: Cardinal;
  LScopeIdx, LExceptLabelIdx: Integer;
  LFrameOffset: Cardinal;
  LGotStartOffset: Cardinal;

  function AlignUp32Local(const AValue: Cardinal; const AAlignment: Cardinal): Cardinal;
  begin
    if AAlignment <= 1 then
      Exit(AValue);
    Result := (AValue + AAlignment - 1) and (not (AAlignment - 1));
  end;

  function BytesToHexLower(const ABytes: TBytes): string;
  const
    HEX: array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
  var
    I: Integer;
  begin
    SetLength(Result, Length(ABytes) * 2);
    for I := 0 to Length(ABytes) - 1 do
    begin
      Result[I * 2 + 1] := HEX[(ABytes[I] shr 4) and $F];
      Result[I * 2 + 2] := HEX[ABytes[I] and $F];
    end;
  end;

  function MachOSymbolName(const AName: string): string;
  begin
    // Mach-O external symbol names are underscore-prefixed on Darwin toolchains.
    if (AName <> '') and (AName[1] <> '_') then
      Result := '_' + AName
    else
      Result := AName;
  end;

  function MachOStaticImportSymbolName(const AEntry: TTigerImportEntry): string;
  begin
    // IMPORTANT:
    // At backend emit time, `TTigerIR.EmitTo` has already applied linkage-based
    // mangling to the import name (plC = plain, plDefault = Itanium). So here we
    // only need to apply Mach-O's leading '_' external symbol convention.
    Result := MachOSymbolName(AEntry.FuncName);
  end;

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

  procedure BuildLinkerSignedSignature(const AData: TBytes; ACodeLimit, ANumPages,
    ASignatureSize: Cardinal; AExecSegBase, AExecSegLimit: UInt64;
    AExecSegFlags: Cardinal; const AIncludeLinkerSignedFlag: Boolean;
    const AIncludeCmsAndRequirements: Boolean; const AIdentifierBytes: TBytes;
    var AOutSignature: TBytes);
  var
    LPageStream: TMemoryStream;
    LHashBytes: TBytes;
    LOff: Cardinal;
    LPageStart: Cardinal;
    LPageLen: Cardinal;
    LIdx: Integer;
    LReqHashBytes: TBytes;

    function HashStreamBytes(const AHashType: Byte; const AStream: TStream): TBytes;
    begin
      if AHashType = CS_HASHTYPE_SHA256 then
        Exit(THashSHA2.GetHashBytes(AStream))
      else
        Exit(THashSHA1.GetHashBytes(AStream));
    end;

    function CodeDirectorySize(const AHashSize: Byte; const AIdentifierSize: Cardinal; const ASpecialSlots, ACodeSlots: Cardinal): Cardinal;
    var
      LHashStartLocal: Cardinal;
    begin
      // Total CodeDirectory size includes:
      // - fixed header (CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE)
      // - identifier bytes (CODE_SIGNATURE_IDENTIFIER_SIZE)
      // - hash slots for special + code (ASpecialSlots + ACodeSlots) * hashSize
      LHashStartLocal := CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE + AIdentifierSize;
      Result := LHashStartLocal + (ASpecialSlots + ACodeSlots) * Cardinal(AHashSize);
    end;

    procedure EmitCodeDirectory(const AStart: Cardinal; const AHashType, AHashSize: Byte;
      const AIdentifierSize: Cardinal;
      const ASpecialSlots, ACodeSlots: Cardinal; const AFlags: Cardinal;
      out AHashOffsetOut, ACodeHashOffsetOut, ACodeDirSizeOut: Cardinal);
    var
      LHashStartLocal: Cardinal;
      LHashOffsetLocal: Cardinal;
      LCodeHashOffsetLocal: Cardinal;
      LCodeDirSizeLocal: Cardinal;
      LWriteOff: Cardinal;
    begin
      // IMPORTANT: In Apple's CodeDirectory format, `hashOffset` points to the start
      // of the *code* slot hashes. Special slot hashes live *before* hashOffset at:
      //   hashOffset - nSpecialSlots * hashSize
      // `hashOffset` is NOT the start of the full hash region.
      LHashStartLocal := CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE + AIdentifierSize;
      LHashOffsetLocal := LHashStartLocal + ASpecialSlots * Cardinal(AHashSize);
      LCodeHashOffsetLocal := LHashOffsetLocal;
      LCodeDirSizeLocal := LHashStartLocal + (ASpecialSlots + ACodeSlots) * Cardinal(AHashSize);

      AHashOffsetOut := LHashOffsetLocal;
      ACodeHashOffsetOut := LCodeHashOffsetLocal;
      ACodeDirSizeOut := LCodeDirSizeLocal;

      LWriteOff := AStart;
      PutU32BE(AOutSignature, LWriteOff, CSMAGIC_CODEDIRECTORY);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, LCodeDirSizeLocal);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, $20400);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, AFlags);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, LHashOffsetLocal);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, ASpecialSlots);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, ACodeSlots);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, ACodeLimit);
      Inc(LWriteOff, 4);
      AOutSignature[LWriteOff] := AHashSize;
      Inc(LWriteOff);
      AOutSignature[LWriteOff] := AHashType;
      Inc(LWriteOff);
      AOutSignature[LWriteOff] := 0;
      Inc(LWriteOff);
      // pageSize is log2(PAGE_SIZE). We use 4KB pages for hashing => 12.
      AOutSignature[LWriteOff] := 12;
      // Advance past pageSize byte to the next u32 field (scatterOffset).
      // DO NOT skip 3 bytes here; the next field starts immediately at offset 40.
      Inc(LWriteOff);
      PutU32BE(AOutSignature, LWriteOff, 0);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, 0);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, 0);
      Inc(LWriteOff, 4);
      PutU32BE(AOutSignature, LWriteOff, 0);
      Inc(LWriteOff, 4);
      PutU64BE(AOutSignature, LWriteOff, UInt64(ACodeLimit));
      Inc(LWriteOff, 8);
      PutU64BE(AOutSignature, LWriteOff, AExecSegBase);
      Inc(LWriteOff, 8);
      PutU64BE(AOutSignature, LWriteOff, AExecSegLimit);
      Inc(LWriteOff, 8);
      PutU64BE(AOutSignature, LWriteOff, UInt64(AExecSegFlags));

      // Identifier bytes (must be consistent across all CodeDirectories)
      if (AIdentifierSize > 0) and (Length(AIdentifierBytes) >= Integer(AIdentifierSize)) then
        Move(AIdentifierBytes[0], AOutSignature[AStart + CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE], AIdentifierSize);
    end;
  begin
    SetLength(AOutSignature, ASignatureSize);
    FillChar(AOutSignature[0], ASignatureSize, 0);

    // SuperBlob header is 12 bytes + N * 8-byte index entries.
    // Our legacy single-blob layout used CODE_SIGNATURE_SUPERBLOB_HEADER_SIZE (=20) which is 12+1*8.
    var LBlobCount: Cardinal;
    var LSuperHdrSize: Cardinal;
    var LReqSize: Cardinal;
    var LCmsSize: Cardinal;
    var LCdSha1Size: Cardinal := 0;
    var LCdSha256Size: Cardinal := 0;
    var LCdSha1Off: Cardinal := 0;
    var LCdSha256Off: Cardinal := 0;
    var LReqOff: Cardinal := 0;
    var LCmsOff: Cardinal := 0;
    var LSuperLen: Cardinal := 0;
    var LSpecialSlots: Cardinal := 0;
    var LHashOffSha1: Cardinal := 0;
    var LCodeHashOffSha1: Cardinal := 0;
    var LHashOffSha256: Cardinal := 0;
    var LCodeHashOffSha256: Cardinal := 0;
    var LFlags: Cardinal;
    var LIdentifierSize: Cardinal;

    LIdentifierSize := Cardinal(Length(AIdentifierBytes));
    if LIdentifierSize < CODE_SIGNATURE_IDENTIFIER_MIN_SIZE then
      LIdentifierSize := CODE_SIGNATURE_IDENTIFIER_MIN_SIZE;

    if AIncludeCmsAndRequirements then
    begin
      // Static-linked binaries: match `codesign -s -` layout with sha1 + sha256 CodeDirectories.
      LBlobCount := 4; // CodeDirectory(sha1) + Requirements + AlternateCodeDirectory(sha256) + CMS wrapper
      LReqSize := 12;
      LCmsSize := 8;
      LSpecialSlots := 2; // Info.plist + Requirements (Info absent -> zero hash)
      LCdSha1Size := CodeDirectorySize(20, LIdentifierSize, LSpecialSlots, ANumPages);
      LCdSha256Size := CodeDirectorySize(32, LIdentifierSize, LSpecialSlots, ANumPages);
    end
    else
    begin
      LBlobCount := 1;
      LReqSize := 0;
      LCmsSize := 0;
      LSpecialSlots := 0;
      LCdSha256Size := CodeDirectorySize(32, LIdentifierSize, 0, ANumPages);
    end;
    LSuperHdrSize := 12 + LBlobCount * 8;
    if AIncludeCmsAndRequirements then
    begin
      LCdSha1Off := LSuperHdrSize;
      LReqOff := LCdSha1Off + LCdSha1Size;
      LCdSha256Off := LReqOff + LReqSize;
      LCmsOff := LCdSha256Off + LCdSha256Size;
      LSuperLen := LCmsOff + LCmsSize;
    end
    else
    begin
      LCdSha256Off := LSuperHdrSize;
      LSuperLen := LSuperHdrSize + LCdSha256Size;
    end;

    LOff := 0;
    PutU32BE(AOutSignature, LOff, CSMAGIC_EMBEDDED_SIGNATURE);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, LSuperLen);
    Inc(LOff, 4);
    PutU32BE(AOutSignature, LOff, LBlobCount);
    Inc(LOff, 4);

    LPageStream := TMemoryStream.Create();
    try
      if AIncludeLinkerSignedFlag then
        LFlags := CS_CD_FLAG_ADHOC or CS_CD_FLAG_LINKER_SIGNED
      else
        LFlags := CS_CD_FLAG_ADHOC;

      if AIncludeCmsAndRequirements then
      begin
        // Index[0] = CodeDirectory (sha1)
        PutU32BE(AOutSignature, LOff, CSSLOT_CODEDIRECTORY);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LCdSha1Off);
        Inc(LOff, 4);
        // Index[1] = Requirements
        PutU32BE(AOutSignature, LOff, CSSLOT_REQUIREMENTS);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LReqOff);
        Inc(LOff, 4);
        // Index[2] = Alternate CodeDirectory (sha256)
        PutU32BE(AOutSignature, LOff, CSSLOT_ALTERNATE_CODEDIRECTORY);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LCdSha256Off);
        Inc(LOff, 4);
        // Index[3] = CMS wrapper
        PutU32BE(AOutSignature, LOff, CSSLOT_CMS_SIGNATURE);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LCmsOff);
        Inc(LOff, 4);

        // Emit CodeDirectories
        EmitCodeDirectory(LCdSha1Off, CS_HASHTYPE_SHA1, 20, LIdentifierSize, LSpecialSlots, ANumPages, LFlags, LHashOffSha1, LCodeHashOffSha1, LCdSha1Size);
        EmitCodeDirectory(LCdSha256Off, CS_HASHTYPE_SHA256, 32, LIdentifierSize, LSpecialSlots, ANumPages, LFlags, LHashOffSha256, LCodeHashOffSha256, LCdSha256Size);

        // Requirements blob (empty superblob)
        LOff := LReqOff;
        PutU32BE(AOutSignature, LOff, CSMAGIC_REQUIREMENTS);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LReqSize);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, 0); // count = 0

        // CMS wrapper blob (empty)
        LOff := LCmsOff;
        PutU32BE(AOutSignature, LOff, CSMAGIC_BLOBWRAPPER);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LCmsSize);

        // Special slot hashes:
        // - slot -2 (Requirements) = hash(requirements blob)
        // - slot -1 (Info.plist) absent -> all zeros (already)
        LPageStream.Clear();
        LPageStream.WriteBuffer(AOutSignature[LReqOff], LReqSize);
        LPageStream.Position := 0;
        LReqHashBytes := HashStreamBytes(CS_HASHTYPE_SHA1, LPageStream);
        if Length(LReqHashBytes) >= 20 then
          // Special slot hashes live at (hashOffset - nSpecial*hashSize)
          Move(LReqHashBytes[0], AOutSignature[LCdSha1Off + (LHashOffSha1 - LSpecialSlots * 20) + 0 * 20], 20);
        LPageStream.Position := 0;
        LReqHashBytes := HashStreamBytes(CS_HASHTYPE_SHA256, LPageStream);
        if Length(LReqHashBytes) >= 32 then
          Move(LReqHashBytes[0], AOutSignature[LCdSha256Off + (LHashOffSha256 - LSpecialSlots * 32) + 0 * 32], 32);

        // Page hashes for sha1
        for LIdx := 0 to ANumPages - 1 do
        begin
          LPageStart := Cardinal(LIdx) * PAGE_SIZE;
          LPageLen := PAGE_SIZE;
          if LPageStart + LPageLen > ACodeLimit then
            LPageLen := ACodeLimit - LPageStart;
          LPageStream.Clear();
          if LPageLen > 0 then
            LPageStream.WriteBuffer(AData[LPageStart], LPageLen);
          LPageStream.Position := 0;
          LHashBytes := HashStreamBytes(CS_HASHTYPE_SHA1, LPageStream);
          if Length(LHashBytes) >= 20 then
            Move(LHashBytes[0], AOutSignature[LCdSha1Off + LCodeHashOffSha1 + Cardinal(LIdx) * 20], 20);
        end;
        // Page hashes for sha256
        for LIdx := 0 to ANumPages - 1 do
        begin
          LPageStart := Cardinal(LIdx) * PAGE_SIZE;
          LPageLen := PAGE_SIZE;
          if LPageStart + LPageLen > ACodeLimit then
            LPageLen := ACodeLimit - LPageStart;
          LPageStream.Clear();
          if LPageLen > 0 then
            LPageStream.WriteBuffer(AData[LPageStart], LPageLen);
          LPageStream.Position := 0;
          LHashBytes := HashStreamBytes(CS_HASHTYPE_SHA256, LPageStream);
          if Length(LHashBytes) >= 32 then
            Move(LHashBytes[0], AOutSignature[LCdSha256Off + LCodeHashOffSha256 + Cardinal(LIdx) * 32], 32);
        end;
      end
      else
      begin
        // Index[0] = CodeDirectory (sha256)
        PutU32BE(AOutSignature, LOff, CSSLOT_CODEDIRECTORY);
        Inc(LOff, 4);
        PutU32BE(AOutSignature, LOff, LCdSha256Off);
        Inc(LOff, 4);

        EmitCodeDirectory(LCdSha256Off, CS_HASHTYPE_SHA256, 32, LIdentifierSize, 0, ANumPages, LFlags, LHashOffSha256, LCodeHashOffSha256, LCdSha256Size);

        for LIdx := 0 to ANumPages - 1 do
        begin
          LPageStart := Cardinal(LIdx) * PAGE_SIZE;
          LPageLen := PAGE_SIZE;
          if LPageStart + LPageLen > ACodeLimit then
            LPageLen := ACodeLimit - LPageStart;
          LPageStream.Clear();
          if LPageLen > 0 then
            LPageStream.WriteBuffer(AData[LPageStart], LPageLen);
          LPageStream.Position := 0;
          LHashBytes := HashStreamBytes(CS_HASHTYPE_SHA256, LPageStream);
          if Length(LHashBytes) >= 32 then
            Move(LHashBytes[0], AOutSignature[LCdSha256Off + LCodeHashOffSha256 + Cardinal(LIdx) * 32], 32);
        end;
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
    // Non-variadic: param 0 at [FP-8], param 1 at [FP-16], ...
    // Variadic: [FP-8] = hidden count, param 0 at [FP-16], param 1 at [FP-24], ...
    if LFunc.IsVariadic then
      Result := -Int32((AIndex + 2) * 8)
    else
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
      okFunc:
        begin
          // ADRP + ADD to get address of function in __text; patched in fixup pass.
          LFuncAddrFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), (AOp.FuncHandle.Index shl 8) or AReg));
          EmitAdrp(AReg, 0);
          EmitARM64($91000000 or (0 shl 10) or (Cardinal(AReg) shl 5) or AReg);
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

  procedure WriteFixedAnsiObj(const LObjStream : TStream; const AText: AnsiString; const ASize: Integer);
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
    LObjStream.WriteBuffer(LBuf[0], ASize);
  end;

begin
  Result := nil;
  LCode := GetCode();
  LData := GetData();
  LGlobals := GetGlobals();
  LImports := GetImports();

  // Generate a UUID once per output image; used by LC_UUID and by the
  // codesign-like identifier we embed for static-linked binaries.
  CreateGUID(LUUID);

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
  LFuncAddrFixups := TList<TPair<Cardinal, Integer>>.Create();
  LJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LCondJumpFixups := TList<TPair<Cardinal, Integer>>.Create();
  LForwardJumpFixups := TList<TPair<Cardinal, Cardinal>>.Create();  // (jump_offset, target_offset) for forward jumps
  LLabelOffsets := TDictionary<Integer, Cardinal>.Create();
  LFloatTemps := TDictionary<Integer, Boolean>.Create();
  LTryBeginLabels := TDictionary<Integer, Integer>.Create();
  LExceptLabels := TDictionary<Integer, Integer>.Create();
  LFinallyLabels := TDictionary<Integer, Integer>.Create();
  LEndLabels := TDictionary<Integer, Integer>.Create();
  LStaticImportIndices := TList<Integer>.Create();
  LStaticSymbolNames := TStringList.Create();
  LStaticLibPaths := TStringList.Create();
  LStaticLibPaths.CaseSensitive := False;
  LStaticLibPaths.Sorted := True;
  LStaticLibPaths.Duplicates := dupIgnore;
  LStaticCallFixups := TList<TPair<Cardinal, Integer>>.Create();
  LHasStaticImports := False;
  LHasSEH := False;
  LPushExceptFrameIdx := -1;
  LPopExceptFrameIdx := -1;
  LGetExceptFrameIdx := -1;
  LInitExceptionsIdx := -1;
  LInitSignalsIdx := -1;
  LRaiseCodeIdx := -1;
  LSigsetjmpIdx := -1;
  LLinker := nil;
  LDylibNames := TStringList.Create();
  LDylibNames.CaseSensitive := False;
  LDylibNames.Sorted := False;
  LDylibNames.Duplicates := dupIgnore;
  LDylibOrdinals := TDictionary<string, Integer>.Create();
  try
    // Exception handling: detect SEH and find runtime/import indices
    for LI := 0 to LFuncCount - 1 do
    begin
      LFunc := LCode.GetFunc(LI);
      if Length(LFunc.ExceptionScopes) > 0 then
        LHasSEH := True;
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
      else if SameText(LFunc.FuncName, 'Tiger_RaiseCode') then
        LRaiseCodeIdx := LI;
    end;
    for LI := 0 to LImports.GetCount() - 1 do
    begin
      LEntry := LImports.GetEntryByIndex(LI);
      if SameText(LEntry.FuncName, 'sigsetjmp') then
      begin
        LSigsetjmpIdx := LI;
        Break;
      end;
    end;

    for LI := 0 to LImports.GetCount() - 1 do
    begin
      LEntry := LImports.GetEntryByIndex(LI);
      if LEntry.IsStatic then
      begin
        LStaticImportIndices.Add(LI);
        LStaticSymbolNames.Add(MachOStaticImportSymbolName(LEntry));
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
      begin
        if not TFile.Exists(LStaticLibPaths[LI]) then
        begin
          Status('Error: Static library not found: %s', [LStaticLibPaths[LI]]);
          if Assigned(FErrors) then
            FErrors.Add(esError, 'B050', 'Static library not found: %s', [LStaticLibPaths[LI]]);
          Result := nil;
          Exit;
        end;
        LLinker.AddLibraryFile(LStaticLibPaths[LI]);
      end;
      LLinker.Resolve(LStaticSymbolNames);
      if LLinker.GetUnresolvedSymbols().Count > 0 then
      begin
        Status('Error: Static linker unresolved symbols: %s', [LLinker.GetUnresolvedSymbols().CommaText]);
        if Assigned(FErrors) then
          FErrors.Add(esError, 'B051', 'Static linker unresolved symbols: %s', [LLinker.GetUnresolvedSymbols().CommaText]);
        Result := nil;
        Exit;
      end;
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
      LExceptFrameSize := Cardinal(Length(LFunc.ExceptionScopes)) * MACOS64_EXCEPT_FRAME_SIZE;
      LExceptFrameBaseOffset := LIncomingSpillSize + LLocalsSize + Cardinal(LFunc.TempCount) * 8 + LOutgoingArgSpace;
      if LExceptFrameSize > 0 then
      begin
        LExceptFrameBaseOffset := (LExceptFrameBaseOffset + 15) and (not 15);
        LExceptFrameBaseOffset := LExceptFrameBaseOffset + 8;
      end;
      LStackFrameSize := LExceptFrameBaseOffset + LExceptFrameSize;
      if (LStackFrameSize mod 16) <> 0 then
        LStackFrameSize := (LStackFrameSize + 15) and (not 15);

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
        for LI := 0 to Min(Length(LFunc.Params) - 1, 7) do
          EmitStrFp(GetParamOffset(LI), LI);
      end;

      if (not SEH_EMIT_DISABLED) and LFunc.IsEntryPoint and LHasSEH and (LInitExceptionsIdx >= 0) then
      begin
        LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LInitExceptionsIdx));
        EmitBL(0);
      end;
      if (not SEH_EMIT_DISABLED) and LFunc.IsEntryPoint and LHasSEH and (LInitSignalsIdx >= 0) then
      begin
        LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LInitSignalsIdx));
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
              if LHasSEH and (LRaiseCodeIdx >= 0) then
              begin
                EmitARM64($F1000000 or (REG_X16 shl 5) or 31);
                EmitARM64($54000061 or ((3 and $7FFFF) shl 5));
                EmitMovX(REG_X0, 42);
                EmitMovX(REG_X1, 0);
                LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LRaiseCodeIdx));
                EmitBL(0);
              end;
              EmitARM64($9BC07C00 or (REG_X16 shl 16) or (REG_X0 shl 5) or REG_X0);
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
          ikMod:
            begin
              LoadOperandToReg(LInstr.Op1, REG_X0);
              LoadOperandToReg(LInstr.Op2, REG_X16);
              if LHasSEH and (LRaiseCodeIdx >= 0) then
              begin
                EmitARM64($F1000000 or (REG_X16 shl 5) or 31);
                EmitARM64($54000061 or ((3 and $7FFFF) shl 5));
                EmitMovX(REG_X0, 42);
                EmitMovX(REG_X1, 0);
                LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LRaiseCodeIdx));
                EmitBL(0);
              end;
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
              LLabelOffsets.AddOrSetValue((LFuncIdx shl 16) or LInstr.LabelTarget.Index, Cardinal(LTextStream.Position));

              if (not SEH_EMIT_DISABLED) and LTryBeginLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                if LPushExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PushExceptFrame not found - exception handling runtime not linked');
                if LSigsetjmpIdx < 0 then
                  raise Exception.Create('sigsetjmp not imported - exception handling runtime not linked');
                LFrameOffset := LExceptFrameBaseOffset + Cardinal(LScopeIdx) * MACOS64_EXCEPT_FRAME_SIZE;
                if LFrameOffset <= 4095 then
                  EmitSubImm(REG_X0, REG_FP, LFrameOffset)
                else
                begin
                  EmitMovRegImm64(REG_X16, LFrameOffset);
                  EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X0);
                end;
                LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LPushExceptFrameIdx));
                EmitBL(0);
                if LFrameOffset + 8 <= 4095 then
                  EmitSubImm(REG_X0, REG_FP, LFrameOffset - 8)
                else
                begin
                  EmitMovRegImm64(REG_X16, LFrameOffset - 8);
                  EmitARM64($CB000000 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X0);
                end;
                EmitMovX(REG_X1, 0);
                if LHasStaticImports and (LStaticImportIndices.IndexOf(LSigsetjmpIdx) >= 0) then
                begin
                  EmitBL(0);
                  LStaticCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position) - 4, LSigsetjmpIdx));
                end
                else
                begin
                  LImportFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LSigsetjmpIdx));
                  EmitAdrp(REG_X16, 0);
                  EmitARM64($F9400000 or (0 shl 10) or (REG_X16 shl 5) or REG_X16);
                  EmitBLR(REG_X16);
                end;
                if LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].ExceptLabel.Index
                else if LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.IsValid() then
                  LExceptLabelIdx := LFunc.ExceptionScopes[LScopeIdx].FinallyLabel.Index
                else
                  LExceptLabelIdx := -1;
                if LExceptLabelIdx >= 0 then
                begin
                  LCondJumpFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), (LFuncIdx shl 16) or LExceptLabelIdx));
                  EmitARM64($35000000);
                end;
              end;

              if (not SEH_EMIT_DISABLED) and LEndLabels.TryGetValue(LInstr.LabelTarget.Index, LScopeIdx) then
              begin
                if LPopExceptFrameIdx < 0 then
                  raise Exception.Create('Tiger_PopExceptFrame not found - exception handling runtime not linked');
                LCallFixups.Add(TPair<Cardinal, Integer>.Create(Cardinal(LTextStream.Position), LPopExceptFrameIdx));
                EmitBL(0);
              end;
            end;
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
          ikSyscall:
            begin
              // macOS ARM64: x16 = syscall number, x0-x5 = args, SVC #0x80, return in x0
              // Map Linux x86_64 syscall numbers to Darwin so Syscall(60, [code]) and Syscall(1, [fd,buf,len]) work
              if Length(LInstr.Args) > 0 then
                LoadOperandToReg(LInstr.Args[0], REG_X0);
              if Length(LInstr.Args) > 1 then
                LoadOperandToReg(LInstr.Args[1], REG_X1);
              if Length(LInstr.Args) > 2 then
                LoadOperandToReg(LInstr.Args[2], REG_X2);
              if Length(LInstr.Args) > 3 then
                LoadOperandToReg(LInstr.Args[3], REG_X3);
              if Length(LInstr.Args) > 4 then
                LoadOperandToReg(LInstr.Args[4], REG_X4);
              if Length(LInstr.Args) > 5 then
                LoadOperandToReg(LInstr.Args[5], REG_X5);
              case LInstr.SyscallNr of
                LINUX_SYS_READ:  EmitMovRegImm64(REG_X16, DARWIN_SYS_READ);
                LINUX_SYS_WRITE: EmitMovRegImm64(REG_X16, DARWIN_SYS_WRITE);
                LINUX_SYS_EXIT:  EmitMovRegImm64(REG_X16, DARWIN_SYS_EXIT);
              else
                EmitMovRegImm64(REG_X16, Cardinal(LInstr.SyscallNr));
              end;
              // SVC #0x80 — encoding: 11010100 000imm16 00001 -> 0xD4001001 for imm=0x80
              EmitARM64($D4001001);
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
              EmitARM64($D37EF400 or (REG_X0 shl 5) or REG_X16);  // LSL X16, X0, #3 (pos * 8)
              EmitAddImm(REG_X16, REG_X16, 8);                     // X16 = 8 + pos * 8
              
              // B.GE to stack args path (branch if position >= 8)
              // B.cond encoding: [31:24]=0x54, [23:20]=cond (GE=0x0A), [19:5]=imm19, [4:0]=0x1E
              var LBranchOffset := Cardinal(LTextStream.Position);
              EmitARM64($540A001E);  // B.GE (placeholder offset=0, will be patched)
              
              // === Register arg path: [FP - (8 + pos * 8)] ===
              // X16 has (8 + pos * 8), compute [FP - X16] and load
              EmitARM64($CB1003F0 or (REG_X16 shl 16) or (REG_FP shl 5) or REG_X16);  // SUB X16, FP, X16
              EmitLdrX(REG_X0, REG_X16, 0);                      // LDR X0, [X16]
              
              // Jump over stack path
              var LSkipOffset := Cardinal(LTextStream.Position);
              EmitARM64($14000000);  // B (placeholder, will be patched)
              
              // === Stack arg path: [FP + 16 + (pos - 8) * 8] ===
              // Position 8 → [FP+16], Position 9 → [FP+24], etc.
              // Restore position from X17
              var LStackPathStart := Cardinal(LTextStream.Position);
              EmitARM64($AA1103E0 or (REG_X17 shl 5) or REG_X0);  // MOV X0, X17
              EmitARM64($D37EF400 or (REG_X0 shl 5) or REG_X0);    // LSL X0, X0, #3 (pos * 8)
              EmitSubImm(REG_X0, REG_X0, 64);                      // X0 = (pos-8)*8
              EmitAddImm(REG_X16, REG_FP, 16);                     // X16 = FP + 16 (base)
              EmitARM64($8B000000 or (REG_X0 shl 16) or (REG_X16 shl 5) or REG_X16);  // ADD X16, X16, X0 → address
              EmitLdrX(REG_X0, REG_X16, 0);                        // LDR X0, [X16]
              
              // Track forward jumps for patching after LTextBytes is created
              var LRegisterPathSize := LStackPathStart - LBranchOffset - 4;  // Size of register path in bytes
              var LStackPathSize := Cardinal(LTextStream.Position) - LStackPathStart;  // Size of stack path in bytes
              LForwardJumpFixups.Add(TPair<Cardinal, Cardinal>.Create(LBranchOffset, LStackPathStart));
              LForwardJumpFixups.Add(TPair<Cardinal, Cardinal>.Create(LSkipOffset, Cardinal(LTextStream.Position)));
              
              // Store result
              StoreTempFromReg(LInstr.Dest.Index, REG_X0);
            end;
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
                // AArch64 BL immediate is relative to the *address of the branch
                // instruction itself* (unlike some 32-bit ARM modes). Do NOT
                // subtract 4 here, or we'll land one instruction early (often
                // the preceding function's RET).
                LInstrIdx := (Int64(LExternalTextBase) + Int64(LResolvedSym.OffsetInMerged) - Int64(LStaticCallFixups[LI].Key)) div 4;
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
      if LCondJumpFixups[LI].Value >= 0 then  // Valid label index
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
    end;

    // Patch forward jumps (for ikVaArgAt)
    for LI := 0 to LForwardJumpFixups.Count - 1 do
    begin
      if (LForwardJumpFixups[LI].Key + 4 <= Cardinal(Length(LTextBytes))) and
         (LForwardJumpFixups[LI].Value <= Cardinal(Length(LTextBytes))) then
      begin
        LInstrIdx := Integer(Int64(LForwardJumpFixups[LI].Value) - Int64(LForwardJumpFixups[LI].Key));
        LInstrIdx := LInstrIdx div 4;  // Convert to instruction offset
        if (LInstrIdx >= -262144) and (LInstrIdx <= 262143) then
        begin
          // Check if it's a conditional branch (B.GE) or unconditional (B)
          var LIn := PCardinal(@LTextBytes[LForwardJumpFixups[LI].Key])^;
          if (LIn and $FF000000) = $54000000 then
            // Conditional branch: preserve condition and opcode bits [31:20] and [4:0], update offset [19:5]
            PCardinal(@LTextBytes[LForwardJumpFixups[LI].Key])^ := (LIn and $FFF0001F) or ((Cardinal(LInstrIdx) and $7FFFF) shl 5)
          else if (LIn and $FC000000) = $14000000 then
            // Unconditional branch: update offset
            PCardinal(@LTextBytes[LForwardJumpFixups[LI].Key])^ := $14000000 or (Cardinal(LInstrIdx) and $3FFFFFF);
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

    LGotStartOffset := (Length(LDataBytes) + 7) and (not 7);
    for LI := 0 to LImportFixups.Count - 1 do
    begin
      // GOT lives in __DATA after globals; align to 8 so LDR Xt,[Xn,#imm] reads correct slot.
      LSlotAddr := LSlide + UInt64(LSegDataFileOff) + UInt64(LGotStartOffset) + UInt64(LImportFixups[LI].Value) * 8;
      LPageIndex := (Int64(LSlotAddr) shr 12) - (Int64(LSlide + UInt64(LTextSectionFileOff) + UInt64(LImportFixups[LI].Key)) shr 12);
      LOfs12 := (LSlotAddr and $FFF) div 8;
      if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) and
         (LImportFixups[LI].Key + 8 <= Cardinal(Length(LTextBytes))) then
      begin
        LAdrpImm := Cardinal((Int32(LPageIndex) + (1 shl 21)) and $1FFFFF);
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
          LAdrpImm := Cardinal((Int32(LPageIndex) + (1 shl 21)) and $1FFFFF);
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
          LAdrpImm := Cardinal((Int32(LPageIndex) + (1 shl 21)) and $1FFFFF);
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
        LAdrpImm := Cardinal((Int32(LPageIndex) + (1 shl 21)) and $1FFFFF);
        PCardinal(@LTextBytes[LDataPageFixups[LI]])^ := $90000000 or REG_X16 or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
      end;
    end;

    for LI := 0 to LFuncAddrFixups.Count - 1 do
    begin
      LByteOffset := LFuncAddrFixups[LI].Key;
      LJ := LFuncAddrFixups[LI].Value;
      LCardVal := Cardinal(LJ and $FF);
      LJ := LJ shr 8;
      if (LJ >= 0) and (LJ < LFuncCount) and (LFuncAddrFixups[LI].Key + 8 <= Cardinal(Length(LTextBytes))) then
      begin
        LSlotAddr := LSlide + UInt64(LTextSectionFileOff) + UInt64(LFuncOffsets[LJ]);
        LPageIndex := (Int64(LSlotAddr) shr 12) - (Int64(LSlide + UInt64(LTextSectionFileOff) + UInt64(LByteOffset)) shr 12);
        LOfs12 := LSlotAddr and $FFF;
        if (LPageIndex >= -1048576) and (LPageIndex <= 1048575) then
        begin
          LAdrpImm := Cardinal((Int32(LPageIndex) + (1 shl 21)) and $1FFFFF);
          PCardinal(@LTextBytes[LByteOffset])^ := $90000000 or LCardVal or ((LAdrpImm and 3) shl 29) or (((LAdrpImm shr 2) and $7FFFF) shl 5);
          PCardinal(@LTextBytes[LByteOffset + 4])^ := $91000000 or (LOfs12 shl 10) or (LCardVal shl 5) or LCardVal;
        end;
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

    //-------------------------------------------------------------------------
    // Relocatable object output (MH_OBJECT) — used for TargetObj and TargetLib.
    // Emits a minimal Mach-O object with __text (+ optional __cstring/__data) and
    // an LC_SYMTAB containing public symbols so Tiger.Linker.MachO can resolve
    // them when linking a static .a.
    //-------------------------------------------------------------------------
    if AFileType = MH_OBJECT then
    begin
      var LObjStream := TMemoryStream.Create();
      var LStrtab := TMemoryStream.Create();
      var LSymtab := TMemoryStream.Create();
      var LExportSyms := TStringList.Create();
      var LExportOffsets := TDictionary<string, UInt64>.Create();
      try
        LExportSyms.CaseSensitive := True;
        LExportSyms.Sorted := True;
        LExportSyms.Duplicates := dupIgnore;

        // Collect public symbols (section-relative offsets within __text)
        for LI := 0 to LFuncCount - 1 do
        begin
          LFunc := LCode.GetFunc(LI);
          if not LFunc.IsPublic then
            Continue;

          var LSymName: string;
          if LFunc.Linkage = plC then
            LSymName := LFunc.FuncName
          else
          begin
            var LParamTypes: TArray<TTigerValueType>;
            SetLength(LParamTypes, Length(LFunc.Params));
            for LK := 0 to High(LFunc.Params) do
              LParamTypes[LK] := LFunc.Params[LK].ParamType;
            LSymName := TTigerABIMangler.MangleFunctionWithLinkage(LFunc.FuncName, LParamTypes, LFunc.Linkage);
          end;
          LSymName := MachOSymbolName(LSymName);
          if LExportSyms.IndexOf(LSymName) < 0 then
            LExportSyms.Add(LSymName);
          LExportOffsets.AddOrSetValue(LSymName, UInt64(LFuncOffsets[LI]));
        end;

        // Build string table (first byte 0)
        LByteVal := 0;
        LStrtab.WriteBuffer(LByteVal, 1);
        var LStrOff: Cardinal := 1;

        // Build symbol table (nlist_64, 16 bytes each)
        // n_type = N_EXT ($01) | N_SECT ($0e) = $0f
        for LI := 0 to LExportSyms.Count - 1 do
        begin
          var LNameA: AnsiString := AnsiString(LExportSyms[LI]);
          var LFuncOff: UInt64 := 0;
          if not LExportOffsets.TryGetValue(LExportSyms[LI], LFuncOff) then
            LFuncOff := 0;

          // n_strx
          LCardVal := LStrOff;
          LSymtab.WriteBuffer(LCardVal, 4);
          // n_type, n_sect
          LByteVal := $0f;
          LSymtab.WriteBuffer(LByteVal, 1);
          LByteVal := 1; // __text is section #1
          LSymtab.WriteBuffer(LByteVal, 1);
          // n_desc
          var LDesc: Word := 0;
          LSymtab.WriteBuffer(LDesc, 2);
          // n_value
          LVal := LFuncOff;
          LSymtab.WriteBuffer(LVal, 8);

          // Append string
          if Length(LNameA) > 0 then
            LStrtab.WriteBuffer(LNameA[1], Length(LNameA));
          LByteVal := 0;
          LStrtab.WriteBuffer(LByteVal, 1);
          Inc(LStrOff, Cardinal(Length(LNameA)) + 1);
        end;

        // Sections to emit
        var LHaveCStr := Length(LCStringBytes) > 0;
        var LHaveData := Length(LDataBytes) > 0;
        var LNumSects: Cardinal := 1 + Ord(LHaveCStr) + Ord(LHaveData);

        // Sizes and offsets
        var LHeaderSizeObj: Cardinal := 32;
        var LSegCmdSize: Cardinal := 72 + 80 * LNumSects;
        var LSymCmdSize: Cardinal := 24;
        var LCmdsSize: Cardinal := LSegCmdSize + LSymCmdSize;
        var LDataStart: Cardinal := LHeaderSizeObj + LCmdsSize;

        var LTextOffObj: Cardinal := AlignUp32Local(LDataStart, 16);
        var LCStrOffObj: Cardinal := AlignUp32Local(LTextOffObj + Cardinal(Length(LTextBytes)), 1);
        var LDataOffObj: Cardinal := AlignUp32Local(LCStrOffObj + Cardinal(Length(LCStringBytes)), 8);
        var LEndOff: Cardinal := LDataOffObj + Cardinal(Length(LDataBytes));

        var LSymOffObj: Cardinal := AlignUp32Local(LEndOff, 8);
        var LStrOffObj: Cardinal := LSymOffObj + Cardinal(LSymtab.Size);

        // mach_header_64
        LObjStream.Clear();
        LCardVal := MH_MAGIC_64;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := CPU_TYPE_ARM64;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := CPU_SUBTYPE_ARM64_ALL;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := MH_OBJECT;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := 2; // ncmds: LC_SEGMENT_64 + LC_SYMTAB
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := LCmdsSize;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := 0; // flags
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := 0; // reserved
        LObjStream.WriteBuffer(LCardVal, 4);

        // LC_SEGMENT_64 (single segment containing all sections)
        LCardVal := LC_SEGMENT_64;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := LSegCmdSize;
        LObjStream.WriteBuffer(LCardVal, 4);
        WriteFixedAnsiObj(LObjStream, AnsiString(SEG_TEXT), 16); // segname
        LVal := 0; LObjStream.WriteBuffer(LVal, 8); // vmaddr
        LVal := 0; LObjStream.WriteBuffer(LVal, 8); // vmsize
        LVal := 0; LObjStream.WriteBuffer(LVal, 8); // fileoff
        LVal := LEndOff; LObjStream.WriteBuffer(LVal, 8); // filesize
        LCardVal := 7; LObjStream.WriteBuffer(LCardVal, 4); // maxprot rwx
        LCardVal := 7; LObjStream.WriteBuffer(LCardVal, 4); // initprot rwx
        LCardVal := LNumSects; LObjStream.WriteBuffer(LCardVal, 4); // nsects
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // flags

        // section_64 records (80 bytes each)
        // __text
        WriteFixedAnsiObj(LObjStream, AnsiString(SECT_TEXT), 16);
        WriteFixedAnsiObj(LObjStream, AnsiString(SEG_TEXT), 16);
        LVal := 0; LObjStream.WriteBuffer(LVal, 8); // addr
        LVal := UInt64(Length(LTextBytes)); LObjStream.WriteBuffer(LVal, 8); // size
        LObjStream.WriteBuffer(LTextOffObj, 4); // offset
        LCardVal := 4; LObjStream.WriteBuffer(LCardVal, 4); // align (2^4 = 16)
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // reloff
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // nreloc
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // flags
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // reserved1
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // reserved2
        LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // reserved3

        if LHaveCStr then
        begin
          WriteFixedAnsiObj(LObjStream, AnsiString(SECT_CSTRING), 16);
          WriteFixedAnsiObj(LObjStream, AnsiString(SEG_TEXT), 16);
          LVal := 0; LObjStream.WriteBuffer(LVal, 8);
          LVal := UInt64(Length(LCStringBytes)); LObjStream.WriteBuffer(LVal, 8);
          LObjStream.WriteBuffer(LCStrOffObj, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4); // align 1
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
        end;

        if LHaveData then
        begin
          WriteFixedAnsiObj(LObjStream, AnsiString(SECT_DATA), 16);
          WriteFixedAnsiObj(LObjStream, AnsiString(SEG_DATA), 16);
          LVal := 0; LObjStream.WriteBuffer(LVal, 8);
          LVal := UInt64(Length(LDataBytes)); LObjStream.WriteBuffer(LVal, 8);
          LObjStream.WriteBuffer(LDataOffObj, 4);
          LCardVal := 3; LObjStream.WriteBuffer(LCardVal, 4); // align 8
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
          LCardVal := 0; LObjStream.WriteBuffer(LCardVal, 4);
        end;

        // LC_SYMTAB
        LCardVal := LC_SYMTAB;
        LObjStream.WriteBuffer(LCardVal, 4);
        LCardVal := LSymCmdSize;
        LObjStream.WriteBuffer(LCardVal, 4);
        LObjStream.WriteBuffer(LSymOffObj, 4);
        LCardVal := LExportSyms.Count;
        LObjStream.WriteBuffer(LCardVal, 4);
        LObjStream.WriteBuffer(LStrOffObj, 4);
        LCardVal := Cardinal(LStrtab.Size);
        LObjStream.WriteBuffer(LCardVal, 4);

        // Pad and write section data
        LByteVal := 0;
        while Cardinal(LObjStream.Position) < LTextOffObj do
          LObjStream.WriteBuffer(LByteVal, 1);
        if Length(LTextBytes) > 0 then
          LObjStream.WriteBuffer(LTextBytes[0], Length(LTextBytes));
        while Cardinal(LObjStream.Position) < LCStrOffObj do
          LObjStream.WriteBuffer(LByteVal, 1);
        if LHaveCStr then
          LObjStream.WriteBuffer(LCStringBytes[0], Length(LCStringBytes));
        while Cardinal(LObjStream.Position) < LDataOffObj do
          LObjStream.WriteBuffer(LByteVal, 1);
        if LHaveData then
          LObjStream.WriteBuffer(LDataBytes[0], Length(LDataBytes));

        while Cardinal(LObjStream.Position) < LSymOffObj do
          LObjStream.WriteBuffer(LByteVal, 1);
        if LSymtab.Size > 0 then
        begin
          LSymtab.Position := 0;
          LObjStream.CopyFrom(LSymtab, LSymtab.Size);
        end;
        while Cardinal(LObjStream.Position) < LStrOffObj do
          LObjStream.WriteBuffer(LByteVal, 1);
        if LStrtab.Size > 0 then
        begin
          LStrtab.Position := 0;
          LObjStream.CopyFrom(LStrtab, LStrtab.Size);
        end;

        SetLength(Result, LObjStream.Size);
        if LObjStream.Size > 0 then
        begin
          LObjStream.Position := 0;
          LObjStream.ReadBuffer(Result[0], LObjStream.Size);
        end;
        Exit;
      finally
        LExportOffsets.Free();
        LExportSyms.Free();
        LSymtab.Free();
        LStrtab.Free();
        LObjStream.Free();
      end;
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
        LVal := LGotStartOffset + Cardinal(LI) * 8;
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

    // Build export trie for any image with public functions (dylib or executable),
    // so executables expose exports (e.g. for nm -gU) and dylibs can be bound to.
    // Also build LC_SYMTAB (symbol + string table) so nm -gU shows exported symbols.
    LExportBytes := nil;
    LSymtabBytes := nil;
    LStrtabBytes := nil;
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

        // Mach-O/macOS: dyld prepends one underscore when resolving bind symbols (see
        // Tiger.Runtime.MacOS64 "dyld adds leading underscore when resolving"). So the
        // EXE's bind stream has "_Z6AddCppii" and dyld looks for "__Z6AddCppii" in the
        // dylib. We must therefore always prepend '_' to the export symbol name so that
        // C → "_AddC" and C++ (Itanium _Z...) → "__Z6AddCppii".
        for LI := 0 to LFuncCount - 1 do
        begin
          LFunc := LCode.GetFunc(LI);
          if LFunc.IsPublic then
          begin
            var LExportSymName: string;
            if LFunc.Linkage = plC then
              LExportSymName := LFunc.FuncName
            else
            begin
              var LParamTypes: TArray<TTigerValueType>;
              SetLength(LParamTypes, Length(LFunc.Params));
              for LK := 0 to High(LFunc.Params) do
                LParamTypes[LK] := LFunc.Params[LK].ParamType;
              LExportSymName := TTigerABIMangler.MangleFunctionWithLinkage(
                LFunc.FuncName, LParamTypes, LFunc.Linkage);
            end;
            var LSym := '_' + LExportSymName;
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
          // Child nodes are simple terminals with no children. Offsets in the trie are
          // relative to the start of the export trie; the first child must start at
          // exactly the root's byte size or dyld can mis-parse (e.g. "re-export ordinal").
          var LChildNodes: array of TBytes;
          SetLength(LChildNodes, LExportSyms.Count);
          for LI := 0 to LExportSyms.Count - 1 do
          begin
            if not LExportAddrs.TryGetValue(LExportSyms[LI], LVal) then
              LVal := 0;
            LChildNodes[LI] := MakeTerminalNode(LVal);
          end;

          var LOffsets: array of Cardinal;
          SetLength(LOffsets, LExportSyms.Count);
          var LRootSize: Cardinal := 0;
          var LPrevRootSize: Cardinal := 0;
          repeat
            LPrevRootSize := LRootSize;
            LRootSize := Cardinal(UlebLen(0)) + 1;
            for LI := 0 to LExportSyms.Count - 1 do
            begin
              var LOff: Cardinal := LRootSize;
              for LK := 0 to LI - 1 do
                Inc(LOff, Cardinal(Length(LChildNodes[LK])));
              LOffsets[LI] := LOff;
              Inc(LRootSize, Cardinal(Length(AnsiString(LExportSyms[LI])) + 1));
              Inc(LRootSize, Cardinal(UlebLen(LOff)));
            end;
          until (LRootSize = LPrevRootSize);

          // Build root into a temp stream so we use its exact size for the first child offset.
          var LRootBuf: TMemoryStream;
          LRootBuf := TMemoryStream.Create();
          try
            WriteUleb(LRootBuf, 0);
            var LChildCount: Byte := Byte(LExportSyms.Count);
            LRootBuf.WriteBuffer(LChildCount, 1);
            for LI := 0 to LExportSyms.Count - 1 do
            begin
              var LLabel: AnsiString := AnsiString(LExportSyms[LI]);
              if Length(LLabel) > 0 then
                LRootBuf.WriteBuffer(LLabel[1], Length(LLabel));
              var LNull: Byte := 0;
              LRootBuf.WriteBuffer(LNull, 1);
              WriteUleb(LRootBuf, LOffsets[LI]);
            end;
            var LRootActualSize: Cardinal := Cardinal(LRootBuf.Position);
            if LRootActualSize <> LOffsets[0] then
            begin
              LOffsets[0] := LRootActualSize;
              for LI := 1 to LExportSyms.Count - 1 do
                LOffsets[LI] := LOffsets[LI - 1] + Cardinal(Length(LChildNodes[LI - 1]));
              LRootBuf.Position := 0;
              LRootBuf.Size := 0;
              WriteUleb(LRootBuf, 0);
              LRootBuf.WriteBuffer(LChildCount, 1);
              for LI := 0 to LExportSyms.Count - 1 do
              begin
                var LLabel: AnsiString := AnsiString(LExportSyms[LI]);
                if Length(LLabel) > 0 then
                  LRootBuf.WriteBuffer(LLabel[1], Length(LLabel));
                var LNull: Byte := 0;
                LRootBuf.WriteBuffer(LNull, 1);
                WriteUleb(LRootBuf, LOffsets[LI]);
              end;
            end;
            LRootBuf.Position := 0;
            if LRootBuf.Size > 0 then
              LExportStream.CopyFrom(LRootBuf, LRootBuf.Size);
          finally
            LRootBuf.Free();
          end;

          for LI := 0 to High(LChildNodes) do
            if Length(LChildNodes[LI]) > 0 then
              LExportStream.WriteBuffer(LChildNodes[LI][0], Length(LChildNodes[LI]));

          SetLength(LExportBytes, LExportStream.Size);
          if LExportStream.Size > 0 then
          begin
            LExportStream.Position := 0;
            LExportStream.ReadBuffer(LExportBytes[0], LExportStream.Size);
          end;

          // Build LC_SYMTAB (symbol table + string table) so nm -gU shows exports.
          // nlist_64: n_strx (4), n_type (1), n_sect (1), n_desc (2), n_value (8) = 16 bytes.
          // n_type = N_EXT ($01) or N_SECT ($0e) = $0f
          var LStrtab: TMemoryStream;
          var LSymtab: TMemoryStream;
          var LStrOff: Cardinal;
          var LNlist: array [0..15] of Byte;
          LStrtab := TMemoryStream.Create();
          LSymtab := TMemoryStream.Create();
          try
            LByteVal := 0;
            LStrtab.WriteBuffer(LByteVal, 1);  // first byte 0 (empty string)
            LStrOff := 1;
            for LI := 0 to LExportSyms.Count - 1 do
            begin
              var LName: AnsiString := AnsiString(LExportSyms[LI]);
              if not LExportAddrs.TryGetValue(LExportSyms[LI], LVal) then
                LVal := 0;
              // nlist_64: n_strx (offset into string table)
              LNlist[0] := Byte(LStrOff);
              LNlist[1] := Byte(LStrOff shr 8);
              LNlist[2] := Byte(LStrOff shr 16);
              LNlist[3] := Byte(LStrOff shr 24);
              LNlist[4] := $0f;   // n_type = N_EXT | N_SECT
              LNlist[5] := 1;    // n_sect = 1 (__text)
              LNlist[6] := 0;    // n_desc
              LNlist[7] := 0;
              LNlist[8] := Byte(LVal);
              LNlist[9] := Byte(LVal shr 8);
              LNlist[10] := Byte(LVal shr 16);
              LNlist[11] := Byte(LVal shr 24);
              LNlist[12] := Byte(LVal shr 40);
              LNlist[13] := Byte(LVal shr 48);
              LNlist[14] := Byte(LVal shr 56);
              LNlist[15] := Byte(LVal shr 56);  // n_value is 64-bit LE; top byte
              LSymtab.WriteBuffer(LNlist[0], 16);
              if Length(LName) > 0 then
                LStrtab.WriteBuffer(LName[1], Length(LName));
              LByteVal := 0;
              LStrtab.WriteBuffer(LByteVal, 1);
              Inc(LStrOff, Cardinal(Length(LName)) + 1);
            end;
            SetLength(LSymtabBytes, LSymtab.Size);
            if LSymtab.Size > 0 then
            begin
              LSymtab.Position := 0;
              LSymtab.ReadBuffer(LSymtabBytes[0], LSymtab.Size);
            end;
            SetLength(LStrtabBytes, LStrtab.Size);
            if LStrtab.Size > 0 then
            begin
              LStrtab.Position := 0;
              LStrtab.ReadBuffer(LStrtabBytes[0], LStrtab.Size);
            end;
          finally
            LStrtab.Free();
            LSymtab.Free();
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
      LSegDataFileSize := (LGotStartOffset + Length(LGotBytes) + 15) and (not 15);
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
                      48 {LC_DYLD_INFO_ONLY} + 16 {LC_CODE_SIGNATURE} +
                      (IfThen(Length(LSymtabBytes) > 0, 24, 0)) {LC_SYMTAB};
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
        LVal := UInt64(LGotStartOffset) + UInt64(Length(LGotBytes));
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
      // Place export trie (when present) immediately after bind opcodes, then symtab and strtab.
      LExportOff := LBindOff + Cardinal(Length(LBindBytes));
      if Length(LExportBytes) > 0 then
      begin
        LSymtabOff := LExportOff + Cardinal(Length(LExportBytes));
        LStrtabOff := LSymtabOff + Cardinal(Length(LSymtabBytes));
        LCodeLimit := LStrtabOff + Cardinal(Length(LStrtabBytes));
      end
      else
      begin
        LExportOff := 0;
        LSymtabOff := 0;
        LStrtabOff := 0;
        LCodeLimit := LBindOff + Cardinal(Length(LBindBytes));
      end;
      LSignatureOffset := AlignUp32(LCodeLimit, CODE_SIGNATURE_ALIGN);
      LPadding := LSignatureOffset - LCodeLimit;
      LNumPages := (LSignatureOffset + PAGE_SIZE - 1) div PAGE_SIZE;
      // Use the same signing structure for all emitted Mach-O images:
      // - 2 CodeDirectories (sha1 + sha256)
      // - empty Requirements blob
      // - empty CMS wrapper blob
      //
      // Also use a codesign-like identifier:
      //   <name>-55554944<uuidhex>
      // where 55554944 is ASCII 'UUID' in hex and <uuidhex> are the LC_UUID bytes.
      var LIdentifierBytes: TBytes;
      var LIdentifierSize: Cardinal;
      var LExeName: string;
      LExeName := ExtractFileName(FOutputPath);
      if LExeName = '' then
        LExeName := 'Tiger';
      var LUUIDBytes: TBytes;
      SetLength(LUUIDBytes, SizeOf(LUUID));
      Move(LUUID, LUUIDBytes[0], SizeOf(LUUID));
      var LIdent: string := LExeName + '-55554944' + BytesToHexLower(LUUIDBytes);
      // NUL-terminated, then pad to even length (matches what codesign tends to do)
      var LIdentAnsi: AnsiString := AnsiString(LIdent);
      SetLength(LIdentifierBytes, Length(LIdentAnsi) + 1);
      if Length(LIdentAnsi) > 0 then
        Move(LIdentAnsi[1], LIdentifierBytes[0], Length(LIdentAnsi));
      LIdentifierBytes[Length(LIdentAnsi)] := 0;
      if (Length(LIdentifierBytes) and 1) <> 0 then
        SetLength(LIdentifierBytes, Length(LIdentifierBytes) + 1);
      LIdentifierSize := Cardinal(Length(LIdentifierBytes));
      // IMPORTANT: LC_CODE_SIGNATURE.datasize must match the embedded SuperBlob length exactly.
      // If datasize is larger, the kernel treats the tail bytes as an "attached" (detached)
      // signature and will reject the image if that trailing region isn't a valid blob.
      const LSpecialSlots: Cardinal = 2;
      var LSuperHdrSize: Cardinal := 12 + 4 * 8;
      var LCodeDirSha1Size: Cardinal := CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE + LIdentifierSize + (LSpecialSlots + LNumPages) * 20;
      var LCodeDirSha256Size: Cardinal := CODE_SIGNATURE_CODEDIRECTORY_HEADER_SIZE + LIdentifierSize + (LSpecialSlots + LNumPages) * 32;
      var LSuperLen: Cardinal := LSuperHdrSize + LCodeDirSha1Size + 12 + LCodeDirSha256Size + 8;
      LSignatureSize := LSuperLen;
      LSegLinkEditFileSize := Cardinal(Length(LRebaseBytes)) + Cardinal(Length(LBindBytes)) +
                              Cardinal(Length(LExportBytes)) + Cardinal(Length(LSymtabBytes)) + Cardinal(Length(LStrtabBytes)) + LPadding + LSignatureSize;
      LSlide := SEG_TEXT_VADDR - UInt64(LSegTextFileOff);

      LCardVal := MH_MAGIC_64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := CPU_TYPE_ARM64;
      LOutStream.WriteBuffer(LCardVal, 4);
      LCardVal := CPU_SUBTYPE_ARM64_ALL;
      LOutStream.WriteBuffer(LCardVal, 4);
      // mach_header_64 filetype
      LCardVal := AFileType;
      LOutStream.WriteBuffer(LCardVal, 4);
      // ncmds must match emitted load commands (see LLoadCmdSize comment).
      // 4 segments + (LC_MAIN or LC_ID_DYLIB) + LC_VERSION_MIN + LC_UUID +
      // (LC_LOAD_DYLINKER for exec) + N*LC_LOAD_DYLIB + LC_DYLD_INFO + [LC_SYMTAB] + LC_CODE_SIGNATURE
      if AIsDylib then
        LCmdCount := 9 + Cardinal(LDylibNames.Count) + (IfThen(Length(LSymtabBytes) > 0, 1, 0))
      else
        LCmdCount := 10 + Cardinal(LDylibNames.Count) + (IfThen(Length(LSymtabBytes) > 0, 1, 0));
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
      LVal := LSlide + UInt64(LSegDataFileOff) + UInt64(LGotStartOffset);
      LOutStream.WriteBuffer(LVal, 8);
      LVal := Length(LGotBytes);
      LOutStream.WriteBuffer(LVal, 8);
      LCardVal := LSegDataFileOff + LGotStartOffset;
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
      if Length(LExportBytes) > 0 then
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

      if Length(LSymtabBytes) > 0 then
      begin
        LCardVal := LC_SYMTAB;
        LOutStream.WriteBuffer(LCardVal, 4);
        LCardVal := 24;
        LOutStream.WriteBuffer(LCardVal, 4);
        LOutStream.WriteBuffer(LSymtabOff, 4);
        LCardVal := Length(LSymtabBytes) div 16;
        LOutStream.WriteBuffer(LCardVal, 4);
        LOutStream.WriteBuffer(LStrtabOff, 4);
        LCardVal := Length(LStrtabBytes);
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
      LK := LGotStartOffset - Length(LDataBytes);
      for LI := 0 to LK - 1 do
        LOutStream.WriteBuffer(LByteVal, 1);
      if Length(LGotBytes) > 0 then
        LOutStream.WriteBuffer(LGotBytes[0], Length(LGotBytes));
      LK := LSegDataFileSize - LGotStartOffset - Length(LGotBytes);
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
      if Length(LSymtabBytes) > 0 then
      begin
        while Cardinal(LOutStream.Position) < LSymtabOff do
          LOutStream.WriteBuffer(LByteVal, 1);
        LOutStream.WriteBuffer(LSymtabBytes[0], Length(LSymtabBytes));
        while Cardinal(LOutStream.Position) < LStrtabOff do
          LOutStream.WriteBuffer(LByteVal, 1);
        LOutStream.WriteBuffer(LStrtabBytes[0], Length(LStrtabBytes));
      end;
      // Avoid "0 to LPadding-1" when LPadding=0: Cardinal underflow makes bound 4294967295 and Inc(LI) overflows
      for LI := 1 to LPadding do
        LOutStream.WriteBuffer(LByteVal, 1);
      for LI := 1 to LSignatureSize do
        LOutStream.WriteBuffer(LByteVal, 1);

      SetLength(Result, LOutStream.Size);
      LOutStream.Position := 0;
      LOutStream.ReadBuffer(Result[0], LOutStream.Size);

      BuildLinkerSignedSignature(
        Result,
        LSignatureOffset,
        LNumPages,
        LSignatureSize,
        UInt64(LSegTextFileOff),
        UInt64(LTextMapSize),
        CS_EXECSEG_MAIN_BINARY,
        False, // no CS_CD_FLAG_LINKER_SIGNED
        True,  // always include Requirements + CMS wrapper blobs
        LIdentifierBytes,
        LSignatureBytes
      );
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
    LForwardJumpFixups.Free();
    LCondJumpFixups.Free();
    LFloatTemps.Free();
    LLabelOffsets.Free();
    LEndLabels.Free();
    LFinallyLabels.Free();
    LExceptLabels.Free();
    LTryBeginLabels.Free();
    LJumpFixups.Free();
    LGlobalFixups.Free();
    LDataPageFixups.Free();
    LFuncAddrFixups.Free();
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
