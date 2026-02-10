{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Linker.ELF;

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
  //----------------------------------------------------------------------------
  // ELF64 constants
  //----------------------------------------------------------------------------

  // ELF identification
  ELF_MAGIC: array[0..3] of Byte = ($7F, $45, $4C, $46);  // 0x7F 'E' 'L' 'F'
  ELFCLASS64    = 2;    // 64-bit objects
  ELFDATA2LSB   = 1;    // Little-endian
  EV_CURRENT    = 1;    // Current ELF version
  ELFOSABI_NONE = 0;    // UNIX System V ABI

  // ELF object file types
  ET_NONE = 0;          // No file type
  ET_REL  = 1;          // Relocatable file (.o)
  ET_EXEC = 2;          // Executable file
  ET_DYN  = 3;          // Shared object file (.so)
  ET_CORE = 4;          // Core file

  // Machine type
  EM_X86_64 = $3E;      // AMD x86-64

  // ELF structure sizes
  ELF64_EHDR_SIZE = 64;
  ELF64_SHDR_SIZE = 64;
  ELF64_SYM_SIZE  = 24;
  ELF64_RELA_SIZE = 24;

  // Section types (sh_type)
  SHT_NULL     = 0;     // Inactive section header
  SHT_PROGBITS = 1;     // Program-defined data
  SHT_SYMTAB   = 2;     // Symbol table
  SHT_STRTAB   = 3;     // String table
  SHT_RELA     = 4;     // Relocation entries with explicit addends
  SHT_HASH     = 5;     // Symbol hash table
  SHT_DYNAMIC  = 6;     // Dynamic linking info
  SHT_NOTE     = 7;     // Notes
  SHT_NOBITS   = 8;     // Uninitialized data (BSS)
  SHT_REL      = 9;     // Relocation entries without addends
  SHT_DYNSYM   = 11;    // Dynamic symbol table

  // Section flags (sh_flags)
  SHF_WRITE     = $1;   // Writable
  SHF_ALLOC     = $2;   // Occupies memory at runtime
  SHF_EXECINSTR = $4;   // Executable
  SHF_MERGE     = $10;  // Might be merged
  SHF_STRINGS   = $20;  // Contains null-terminated strings
  SHF_INFO_LINK = $40;  // sh_info contains SHT index

  // Symbol binding (high nibble of st_info)
  STB_LOCAL  = 0;       // Local symbol
  STB_GLOBAL = 1;       // Global symbol
  STB_WEAK   = 2;       // Weak symbol

  // Symbol type (low nibble of st_info)
  STT_NOTYPE  = 0;      // No type
  STT_OBJECT  = 1;      // Data object
  STT_FUNC    = 2;      // Function
  STT_SECTION = 3;      // Section
  STT_FILE    = 4;      // Source file name

  // Symbol visibility (st_other)
  STV_DEFAULT   = 0;
  STV_INTERNAL  = 1;
  STV_HIDDEN    = 2;
  STV_PROTECTED = 3;

  // Special section indices
  SHN_UNDEF  = 0;       // Undefined symbol
  SHN_ABS    = $FFF1;   // Absolute value
  SHN_COMMON = $FFF2;   // Common symbol

  // x86-64 relocation types (for RELA sections)
  R_X86_64_NONE      = 0;   // No relocation
  R_X86_64_64        = 1;   // Direct 64-bit
  R_X86_64_PC32      = 2;   // PC-relative 32-bit signed
  R_X86_64_GOT32     = 3;   // 32-bit GOT entry
  R_X86_64_PLT32     = 4;   // 32-bit PLT address
  R_X86_64_COPY      = 5;   // Copy symbol at runtime
  R_X86_64_GLOB_DAT  = 6;   // Create GOT entry
  R_X86_64_JUMP_SLOT = 7;   // Create PLT entry
  R_X86_64_RELATIVE  = 8;   // Adjust by program base
  R_X86_64_GOTPCREL  = 9;   // 32-bit signed PC-rel to GOT entry
  R_X86_64_32        = 10;  // Direct 32-bit zero-extended
  R_X86_64_32S       = 11;  // Direct 32-bit sign-extended
  R_X86_64_16        = 12;  // Direct 16-bit zero-extended
  R_X86_64_PC16      = 13;  // 16-bit sign-extended PC-relative
  R_X86_64_8         = 14;  // Direct 8-bit sign-extended
  R_X86_64_PC8       = 15;  // 8-bit sign-extended PC-relative
  R_X86_64_REX_GOTPCRELX = 42;  // Relaxable GOTPCREL

  // AR archive constants (same format as COFF)
  AR_SIGNATURE = '!<arch>'#10;
  AR_HEADER_SIZE = 60;

type
  //----------------------------------------------------------------------------
  // ELF64 structures (for parsed data)
  //----------------------------------------------------------------------------

  { TELFRelocation - A single relocation entry from an ELF RELA section }
  TELFRelocation = record
    Offset: UInt64;           // Location to apply relocation
    SymbolIndex: Cardinal;    // Index into symbol table
    RelocationType: Cardinal; // R_X86_64_xxx
    Addend: Int64;            // Explicit addend
  end;

  { TELFSymbol - A parsed ELF symbol table entry }
  TELFSymbol = record
    SymbolName: string;       // Resolved name from string table
    Value: UInt64;            // Symbol value (offset in section for defined)
    Size: UInt64;             // Symbol size
    Binding: Byte;            // STB_LOCAL, STB_GLOBAL, STB_WEAK
    SymbolType: Byte;         // STT_NOTYPE, STT_FUNC, STT_OBJECT, etc.
    Visibility: Byte;         // STV_DEFAULT, etc.
    SectionIndex: Word;       // Section index (SHN_UNDEF for undefined)
  end;

  { TELFSection - A parsed ELF section with its raw data and relocations }
  TELFSection = record
    SectionName: string;
    SectionType: Cardinal;    // SHT_xxx
    Flags: UInt64;            // SHF_xxx
    VirtualAddr: UInt64;      // Address in memory (0 for relocatable)
    Size: UInt64;
    Link: Cardinal;           // Section header table index link
    Info: Cardinal;           // Extra info
    Alignment: UInt64;
    EntrySize: UInt64;        // Entry size for fixed-size entries
    RawData: TBytes;
    Relocations: TArray<TELFRelocation>;  // From associated .rela section
  end;

  { TELFObject - A fully parsed ELF relocatable object file }
  TELFObject = class
  public
    SourcePath: string;       // File path for diagnostics
    MemberName: string;       // AR member name (if from .a)
    Sections: TArray<TELFSection>;
    Symbols: TArray<TELFSymbol>;
    Selected: Boolean;        // Set when selected for linking

    constructor Create();
    destructor Destroy(); override;

    /// <summary>Find section index by name</summary>
    function GetSectionIndexByName(const ASectionName: string): Integer;
  end;


  //----------------------------------------------------------------------------
  // TTigerELFLinker - ELF static linker implementation
  //----------------------------------------------------------------------------

  { TTigerELFLinker }
  TTigerELFLinker = class(TTigerLinker)
  private
    // All parsed objects (from both .o and .a files)
    FObjects: TObjectList<TELFObject>;

    // AR symbol index: symbol name -> list of object indices that define it
    FSymbolIndex: TDictionary<string, TList<Integer>>;

    // Merged section buffers
    FMergedText: TMemoryStream;
    FMergedRoData: TMemoryStream;
    FMergedData: TMemoryStream;
    FMergedBSS: Cardinal;

    // Contributions tracking
    FTextContribs: TList<TLinkerContribution>;
    FRoDataContribs: TList<TLinkerContribution>;
    FDataContribs: TList<TLinkerContribution>;

    // Resolved symbols: name -> resolved info
    FResolvedSymbols: TDictionary<string, TLinkerResolvedSymbol>;

    // Symbols that could not be resolved (will need dynamic linking)
    FUnresolvedSymbols: TStringList;

    // Pending relocations that need final addresses
    FPendingRelocs: TList<TLinkerPendingReloc>;

    //--------------------------------------------------------------------------
    // ELF parsing
    //--------------------------------------------------------------------------
    function ParseELFObject(const AData: TBytes;
      const ASourcePath: string): TELFObject;
    function ReadELFString(const AStringTable: TBytes;
      const AOffset: Cardinal): string;

    //--------------------------------------------------------------------------
    // AR parsing
    //--------------------------------------------------------------------------
    procedure ParseARLibrary(const AData: TBytes; const ASourcePath: string);

    //--------------------------------------------------------------------------
    // Section classification
    //--------------------------------------------------------------------------
    function ClassifySection(const ASectionName: string;
      const AFlags: UInt64): TLinkerSectionKind;

    //--------------------------------------------------------------------------
    // Symbol resolution internals
    //--------------------------------------------------------------------------
    procedure BuildGlobalSymbolIndex();
    procedure SelectObject(const AObjectIndex: Integer;
      const AWorkList: TList<Integer>);
    procedure MergeSelectedObjects();
    procedure CollectRelocations();
    procedure ApplyInternalRelocations();

    //--------------------------------------------------------------------------
    // Utility
    //--------------------------------------------------------------------------
    function AlignUp(const AValue: Cardinal; const AAlignment: Cardinal): Cardinal;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Input -- add files to link
    //--------------------------------------------------------------------------
    procedure AddObjectFile(const APath: string); override;
    procedure AddLibraryFile(const APath: string); override;
    procedure AddObjectFromMemory(const AData: TBytes;
      const AName: string); override;

    //--------------------------------------------------------------------------
    // Processing -- resolve symbols and merge sections
    //--------------------------------------------------------------------------
    procedure Resolve(const ANeededSymbols: TStrings); override;

    //--------------------------------------------------------------------------
    // Output -- merged section data
    //--------------------------------------------------------------------------
    function GetMergedText(): TBytes; override;
    function GetMergedTextSize(): Cardinal; override;
    function GetMergedRData(): TBytes; override;
    function GetMergedRDataSize(): Cardinal; override;
    function GetMergedData(): TBytes; override;
    function GetMergedDataSize(): Cardinal; override;

    //--------------------------------------------------------------------------
    // Symbol info
    //--------------------------------------------------------------------------
    function GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>; override;
    function GetUnresolvedSymbols(): TStringList; override;
    function GetPendingRelocations(): TList<TLinkerPendingReloc>; override;

    //--------------------------------------------------------------------------
    // Diagnostics
    //--------------------------------------------------------------------------
    function GetObjectCount(): Integer; override;
    function GetSelectedObjectCount(): Integer; override;
  end;

implementation

//==============================================================================
// TELFObject
//==============================================================================

constructor TELFObject.Create();
begin
  inherited Create();
  Selected := False;
end;

destructor TELFObject.Destroy();
begin
  inherited;
end;

function TELFObject.GetSectionIndexByName(const ASectionName: string): Integer;
var
  LI: Integer;
begin
  for LI := 0 to High(Sections) do
  begin
    if Sections[LI].SectionName = ASectionName then
      Exit(LI);
  end;
  Result := -1;
end;

//==============================================================================
// TTigerELFLinker -- Construction / Destruction
//==============================================================================

constructor TTigerELFLinker.Create();
begin
  inherited Create();
  FObjects := TObjectList<TELFObject>.Create(True);
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

destructor TTigerELFLinker.Destroy();
var
  LList: TList<Integer>;
begin
  FPendingRelocs.Free();
  FUnresolvedSymbols.Free();
  FResolvedSymbols.Free();

  FDataContribs.Free();
  FRoDataContribs.Free();
  FTextContribs.Free();

  FMergedData.Free();
  FMergedRoData.Free();
  FMergedText.Free();

  for LList in FSymbolIndex.Values do
    LList.Free();
  FSymbolIndex.Free();

  FObjects.Free();
  inherited;
end;

//==============================================================================
// ELF Parsing
//==============================================================================

function TTigerELFLinker.ReadELFString(const AStringTable: TBytes;
  const AOffset: Cardinal): string;
var
  LI: Cardinal;
  LLen: Cardinal;
  LAnsi: AnsiString;
begin
  if AOffset >= Cardinal(Length(AStringTable)) then
    Exit('');

  // Find null terminator
  LI := AOffset;
  while (LI < Cardinal(Length(AStringTable))) and (AStringTable[LI] <> 0) do
    Inc(LI);

  LLen := LI - AOffset;
  if LLen = 0 then
    Exit('');

  // Read as AnsiString then convert to Unicode
  SetLength(LAnsi, LLen);
  Move(AStringTable[AOffset], LAnsi[1], LLen);
  Result := string(LAnsi);
end;

function TTigerELFLinker.ParseELFObject(const AData: TBytes;
  const ASourcePath: string): TELFObject;
var
  LObj: TELFObject;
  LPos: Integer;
  LI, LJ: Integer;

  // ELF header fields
  LEType: Word;
  LEMachine: Word;
  LEShoff: UInt64;
  LEShentsize: Word;
  LEShnum: Word;
  LEShstrndx: Word;

  // Section header fields
  LShName: Cardinal;
  LShType: Cardinal;
  LShFlags: UInt64;
  LShAddr: UInt64;
  LShOffset: UInt64;
  LShSize: UInt64;
  LShLink: Cardinal;
  LShInfo: Cardinal;
  LShAddralign: UInt64;
  LShEntsize: UInt64;

  // String table for section names
  LShstrtab: TBytes;
  LShstrtabOffset: UInt64;
  LShstrtabSize: UInt64;

  // Symbol table handling
  LSymtabIndex: Integer;
  LStrtabIndex: Integer;
  LStrtab: TBytes;
  LSymCount: Integer;
  LSymOffset: UInt64;

  // Symbol fields
  LStName: Cardinal;
  LStInfo: Byte;
  LStOther: Byte;
  LStShndx: Word;
  LStValue: UInt64;
  LStSize: UInt64;

  // Relocation handling
  LRelaSecName: string;
  LTargetSecIndex: Integer;
  LRelaCount: Integer;
  LRelaOffset: UInt64;
  LROffset: UInt64;
  LRInfo: UInt64;
  LRAddend: Int64;
begin
  Result := nil;

  // Minimum size check: ELF header
  if Length(AData) < ELF64_EHDR_SIZE then
  begin
    Status('ELF: File too small for ELF header: %s', [ASourcePath]);
    Exit;
  end;

  // Verify ELF magic
  if (AData[0] <> $7F) or (AData[1] <> Ord('E')) or
     (AData[2] <> Ord('L')) or (AData[3] <> Ord('F')) then
  begin
    Status('ELF: Invalid ELF magic: %s', [ASourcePath]);
    Exit;
  end;

  // Verify 64-bit, little-endian
  if AData[4] <> ELFCLASS64 then
  begin
    Status('ELF: Not a 64-bit ELF file: %s', [ASourcePath]);
    Exit;
  end;
  if AData[5] <> ELFDATA2LSB then
  begin
    Status('ELF: Not little-endian: %s', [ASourcePath]);
    Exit;
  end;

  // Read ELF header fields
  LEType := PWord(@AData[16])^;
  LEMachine := PWord(@AData[18])^;

  // Verify relocatable object
  if LEType <> ET_REL then
  begin
    Status('ELF: Not a relocatable object (e_type=%d): %s', [LEType, ASourcePath]);
    Exit;
  end;

  // Verify x86-64
  if LEMachine <> EM_X86_64 then
  begin
    Status('ELF: Not x86-64 (e_machine=$%x): %s', [LEMachine, ASourcePath]);
    Exit;
  end;

  // Section header table info
  LEShoff := PUInt64(@AData[40])^;      // e_shoff
  LEShentsize := PWord(@AData[58])^;    // e_shentsize
  LEShnum := PWord(@AData[60])^;        // e_shnum
  LEShstrndx := PWord(@AData[62])^;     // e_shstrndx

  if LEShoff = 0 then
  begin
    Status('ELF: No section header table: %s', [ASourcePath]);
    Exit;
  end;

  if LEShentsize < ELF64_SHDR_SIZE then
  begin
    Status('ELF: Invalid section header size: %s', [ASourcePath]);
    Exit;
  end;

  // Read section header string table first
  if LEShstrndx < LEShnum then
  begin
    LPos := Integer(LEShoff) + (LEShstrndx * LEShentsize);
    LShstrtabOffset := PUInt64(@AData[LPos + 24])^;  // sh_offset
    LShstrtabSize := PUInt64(@AData[LPos + 32])^;    // sh_size
    SetLength(LShstrtab, LShstrtabSize);
    if LShstrtabSize > 0 then
      Move(AData[LShstrtabOffset], LShstrtab[0], LShstrtabSize);
  end
  else
  begin
    SetLength(LShstrtab, 0);
  end;

  // Create object
  LObj := TELFObject.Create();
  LObj.SourcePath := ASourcePath;
  SetLength(LObj.Sections, LEShnum);

  // First pass: read all section headers
  LSymtabIndex := -1;
  LStrtabIndex := -1;

  for LI := 0 to LEShnum - 1 do
  begin
    LPos := Integer(LEShoff) + (LI * LEShentsize);

    LShName := PCardinal(@AData[LPos])^;
    LShType := PCardinal(@AData[LPos + 4])^;
    LShFlags := PUInt64(@AData[LPos + 8])^;
    LShAddr := PUInt64(@AData[LPos + 16])^;
    LShOffset := PUInt64(@AData[LPos + 24])^;
    LShSize := PUInt64(@AData[LPos + 32])^;
    LShLink := PCardinal(@AData[LPos + 40])^;
    LShInfo := PCardinal(@AData[LPos + 44])^;
    LShAddralign := PUInt64(@AData[LPos + 48])^;
    LShEntsize := PUInt64(@AData[LPos + 56])^;

    LObj.Sections[LI].SectionName := ReadELFString(LShstrtab, LShName);
    LObj.Sections[LI].SectionType := LShType;
    LObj.Sections[LI].Flags := LShFlags;
    LObj.Sections[LI].VirtualAddr := LShAddr;
    LObj.Sections[LI].Size := LShSize;
    LObj.Sections[LI].Link := LShLink;
    LObj.Sections[LI].Info := LShInfo;
    LObj.Sections[LI].Alignment := LShAddralign;
    LObj.Sections[LI].EntrySize := LShEntsize;

    // Read raw data for PROGBITS, SYMTAB, STRTAB, RELA sections
    if (LShType <> SHT_NULL) and (LShType <> SHT_NOBITS) and (LShSize > 0) then
    begin
      if LShOffset + LShSize <= UInt64(Length(AData)) then
      begin
        SetLength(LObj.Sections[LI].RawData, LShSize);
        Move(AData[LShOffset], LObj.Sections[LI].RawData[0], LShSize);
      end;
    end;

    // Track symbol table and its string table
    if LShType = SHT_SYMTAB then
    begin
      LSymtabIndex := LI;
      LStrtabIndex := Integer(LShLink);
    end;
  end;

  // Get symbol string table
  if (LStrtabIndex >= 0) and (LStrtabIndex < Length(LObj.Sections)) then
  begin
    LStrtab := LObj.Sections[LStrtabIndex].RawData;
  end
  else
    SetLength(LStrtab, 0);

  // Parse symbol table
  if LSymtabIndex >= 0 then
  begin
    LSymOffset := 0;
    LSymCount := Length(LObj.Sections[LSymtabIndex].RawData) div ELF64_SYM_SIZE;
    SetLength(LObj.Symbols, LSymCount);

    for LI := 0 to LSymCount - 1 do
    begin
      LStName := PCardinal(@LObj.Sections[LSymtabIndex].RawData[LSymOffset])^;
      LStInfo := LObj.Sections[LSymtabIndex].RawData[LSymOffset + 4];
      LStOther := LObj.Sections[LSymtabIndex].RawData[LSymOffset + 5];
      LStShndx := PWord(@LObj.Sections[LSymtabIndex].RawData[LSymOffset + 6])^;
      LStValue := PUInt64(@LObj.Sections[LSymtabIndex].RawData[LSymOffset + 8])^;
      LStSize := PUInt64(@LObj.Sections[LSymtabIndex].RawData[LSymOffset + 16])^;

      LObj.Symbols[LI].SymbolName := ReadELFString(LStrtab, LStName);
      LObj.Symbols[LI].Value := LStValue;
      LObj.Symbols[LI].Size := LStSize;
      LObj.Symbols[LI].Binding := LStInfo shr 4;
      LObj.Symbols[LI].SymbolType := LStInfo and $0F;
      LObj.Symbols[LI].Visibility := LStOther and $03;
      LObj.Symbols[LI].SectionIndex := LStShndx;

      Inc(LSymOffset, ELF64_SYM_SIZE);
    end;
  end;

  // Parse RELA sections and attach to their target sections
  for LI := 0 to High(LObj.Sections) do
  begin
    if LObj.Sections[LI].SectionType = SHT_RELA then
    begin
      // .rela.text -> .text, .rela.rodata -> .rodata, etc.
      LRelaSecName := LObj.Sections[LI].SectionName;
      if Copy(LRelaSecName, 1, 5) = '.rela' then
      begin
        // Find target section (sh_info field contains target section index)
        LTargetSecIndex := Integer(LObj.Sections[LI].Info);
        if (LTargetSecIndex >= 0) and (LTargetSecIndex < Length(LObj.Sections)) then
        begin
          LRelaCount := Length(LObj.Sections[LI].RawData) div ELF64_RELA_SIZE;
          SetLength(LObj.Sections[LTargetSecIndex].Relocations, LRelaCount);

          LRelaOffset := 0;
          for LJ := 0 to LRelaCount - 1 do
          begin
            LROffset := PUInt64(@LObj.Sections[LI].RawData[LRelaOffset])^;
            LRInfo := PUInt64(@LObj.Sections[LI].RawData[LRelaOffset + 8])^;
            LRAddend := PInt64(@LObj.Sections[LI].RawData[LRelaOffset + 16])^;

            LObj.Sections[LTargetSecIndex].Relocations[LJ].Offset := LROffset;
            LObj.Sections[LTargetSecIndex].Relocations[LJ].SymbolIndex := Cardinal(LRInfo shr 32);
            LObj.Sections[LTargetSecIndex].Relocations[LJ].RelocationType := Cardinal(LRInfo and $FFFFFFFF);
            LObj.Sections[LTargetSecIndex].Relocations[LJ].Addend := LRAddend;

            Inc(LRelaOffset, ELF64_RELA_SIZE);
          end;
        end;
      end;
    end;
  end;

  Result := LObj;
end;

//==============================================================================
// AR Archive Parsing
//==============================================================================

procedure TTigerELFLinker.ParseARLibrary(const AData: TBytes;
  const ASourcePath: string);
var
  LPos: Integer;
  LSig: AnsiString;
  LHeaderName: AnsiString;
  LHeaderSizeStr: AnsiString;
  LMemberSize: Cardinal;
  LMemberData: TBytes;
  LObj: TELFObject;
  LIsLinkerMember: Boolean;
  LTrimmedName: string;
begin
  if Length(AData) < 8 then
  begin
    Status('ELF Linker: AR file too small: %s', [ASourcePath]);
    Exit;
  end;

  // Verify AR signature
  SetLength(LSig, 8);
  Move(AData[0], LSig[1], 8);
  if string(LSig) <> AR_SIGNATURE then
  begin
    Status('ELF Linker: Invalid AR signature: %s', [ASourcePath]);
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
      Status('ELF Linker: Invalid AR member header at offset %d: %s',
        [LPos, ASourcePath]);
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
    // "/" = symbol table, "//" = long names table, "__.SYMDEF" = BSD symbol table
    LIsLinkerMember := (LHeaderName = '/') or (LHeaderName = '//') or
                       (Copy(string(LHeaderName), 1, 8) = '__.SYMDE');

    if (not LIsLinkerMember) and (LMemberSize >= ELF64_EHDR_SIZE) then
    begin
      // This is an object member -- parse it as ELF
      SetLength(LMemberData, LMemberSize);
      if LPos + Integer(LMemberSize) <= Length(AData) then
      begin
        Move(AData[LPos], LMemberData[0], LMemberSize);

        // Check that it looks like an ELF object (magic bytes)
        if (LMemberSize >= 4) and (LMemberData[0] = $7F) and
           (LMemberData[1] = Ord('E')) and (LMemberData[2] = Ord('L')) and
           (LMemberData[3] = Ord('F')) then
        begin
          LObj := ParseELFObject(LMemberData, ASourcePath);
          if LObj <> nil then
          begin
            // Clean up member name (remove trailing '/')
            LTrimmedName := string(LHeaderName);
            if (Length(LTrimmedName) > 0) and
               (LTrimmedName[Length(LTrimmedName)] = '/') then
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

  Status('ELF Linker: Parsed AR library %s -- %d object members',
    [ASourcePath, FObjects.Count]);
end;

//==============================================================================
// Section Classification
//==============================================================================

function TTigerELFLinker.ClassifySection(const ASectionName: string;
  const AFlags: UInt64): TLinkerSectionKind;
begin
  // Classify by name first
  if (ASectionName = '.text') or (Copy(ASectionName, 1, 6) = '.text.') then
    Exit(lskText);

  if (ASectionName = '.rodata') or (Copy(ASectionName, 1, 8) = '.rodata.') then
    Exit(lskRData);

  if (ASectionName = '.data') or (Copy(ASectionName, 1, 6) = '.data.') then
    Exit(lskData);

  if (ASectionName = '.bss') or (Copy(ASectionName, 1, 5) = '.bss.') then
    Exit(lskBSS);

  // Fall back to flags
  if (AFlags and SHF_EXECINSTR) <> 0 then
    Exit(lskText);

  if ((AFlags and SHF_ALLOC) <> 0) and ((AFlags and SHF_WRITE) = 0) then
    Exit(lskRData);

  if ((AFlags and SHF_ALLOC) <> 0) and ((AFlags and SHF_WRITE) <> 0) then
    Exit(lskData);

  Result := lskOther;
end;

//==============================================================================
// Symbol Resolution
//==============================================================================

procedure TTigerELFLinker.BuildGlobalSymbolIndex();
var
  LI, LJ: Integer;
  LObj: TELFObject;
  LSym: TELFSymbol;
  LList: TList<Integer>;
  LSymbol: string;
begin
  // Clear existing index
  for LList in FSymbolIndex.Values do
    LList.Free();
  FSymbolIndex.Clear();

  // Build index of global symbols
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    for LJ := 0 to High(LObj.Symbols) do
    begin
      LSym := LObj.Symbols[LJ];

      // Only index defined global/weak symbols
      if (LSym.Binding in [STB_GLOBAL, STB_WEAK]) and
         (LSym.SectionIndex <> SHN_UNDEF) and
         (LSym.SymbolName <> '') then
      begin
        if not FSymbolIndex.TryGetValue(LSym.SymbolName, LList) then
        begin
          LList := TList<Integer>.Create();
          FSymbolIndex.Add(LSym.SymbolName, LList);
        end;
        if not LList.Contains(LI) then
          LList.Add(LI);
      end;
    end;
  end;

  Status('ELF Linker: Built global symbol index -- %d unique symbols',
    [FSymbolIndex.Count]);
end;

procedure TTigerELFLinker.SelectObject(const AObjectIndex: Integer;
  const AWorkList: TList<Integer>);
var
  LObj: TELFObject;
  LSym: TELFSymbol;
  LI: Integer;
  LDefList: TList<Integer>;
begin
  if (AObjectIndex < 0) or (AObjectIndex >= FObjects.Count) then
    Exit;

  LObj := FObjects[AObjectIndex];
  if LObj.Selected then
    Exit;

  LObj.Selected := True;
  Status('ELF Linker: Selected object %d: %s', [AObjectIndex, LObj.SourcePath]);

  // Find undefined symbols in this object and queue their definitions
  for LI := 0 to High(LObj.Symbols) do
  begin
    LSym := LObj.Symbols[LI];
    if (LSym.Binding in [STB_GLOBAL, STB_WEAK]) and
       (LSym.SectionIndex = SHN_UNDEF) and
       (LSym.SymbolName <> '') then
    begin
      // This object needs this symbol -- find who defines it
      if FSymbolIndex.TryGetValue(LSym.SymbolName, LDefList) then
      begin
        // Add first definer to work list (if not already selected)
        if (LDefList.Count > 0) and (not FObjects[LDefList[0]].Selected) then
        begin
          if not AWorkList.Contains(LDefList[0]) then
            AWorkList.Add(LDefList[0]);
        end;
      end;
    end;
  end;
end;

function TTigerELFLinker.AlignUp(const AValue: Cardinal;
  const AAlignment: Cardinal): Cardinal;
begin
  if AAlignment <= 1 then
    Exit(AValue);
  Result := (AValue + AAlignment - 1) and not (AAlignment - 1);
end;

procedure TTigerELFLinker.MergeSelectedObjects();
var
  LI, LJ: Integer;
  LObj: TELFObject;
  LSec: TELFSection;
  LKind: TLinkerSectionKind;
  LContrib: TLinkerContribution;
  LAlign: Cardinal;
  LPadding: Cardinal;
  LPadByte: Byte;
  LResolved: TLinkerResolvedSymbol;
  LSym: TELFSymbol;
begin
  // Clear merged buffers
  FMergedText.Clear();
  FMergedRoData.Clear();
  FMergedData.Clear();
  FMergedBSS := 0;
  FTextContribs.Clear();
  FRoDataContribs.Clear();
  FDataContribs.Clear();
  FResolvedSymbols.Clear();

  LPadByte := 0;

  // Merge sections from selected objects
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Sections) do
    begin
      LSec := LObj.Sections[LJ];

      // Skip non-PROGBITS sections (symtab, strtab, rela, etc.)
      if (LSec.SectionType <> SHT_PROGBITS) and (LSec.SectionType <> SHT_NOBITS) then
        Continue;

      if Length(LSec.RawData) = 0 then
        Continue;

      LKind := ClassifySection(LSec.SectionName, LSec.Flags);

      // Align before appending
      if LSec.Alignment > 1 then
        LAlign := Cardinal(LSec.Alignment)
      else
        LAlign := 1;

      case LKind of
        lskText:
        begin
          LContrib.ObjectIndex := LI;
          LContrib.OrigSectionIndex := LJ;
          LContrib.MergedOffset := AlignUp(Cardinal(FMergedText.Size), LAlign);
          LContrib.MergedSize := Length(LSec.RawData);

          // Pad to alignment
          LPadding := LContrib.MergedOffset - Cardinal(FMergedText.Size);
          while LPadding > 0 do
          begin
            FMergedText.WriteData(LPadByte);
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
            FMergedRoData.WriteData(LPadByte);
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
            FMergedData.WriteData(LPadByte);
            Dec(LPadding);
          end;

          FMergedData.WriteBuffer(LSec.RawData[0], Length(LSec.RawData));
          FDataContribs.Add(LContrib);
        end;

        lskBSS:
        begin
          FMergedBSS := AlignUp(FMergedBSS, LAlign) + Cardinal(LSec.Size);
        end;
      end;
    end;

    // Register resolved symbols from this object
    for LJ := 0 to High(LObj.Symbols) do
    begin
      LSym := LObj.Symbols[LJ];
      if (LSym.Binding in [STB_GLOBAL, STB_WEAK]) and
         (LSym.SectionIndex <> SHN_UNDEF) and
         (LSym.SymbolName <> '') and
         (not FResolvedSymbols.ContainsKey(LSym.SymbolName)) then
      begin
        // Find this symbol's section contribution
        LResolved.SymbolName := LSym.SymbolName;
        LResolved.SectionKind := lskOther;
        LResolved.OffsetInMerged := 0;

        // Look for section in contributions
        if (LSym.SectionIndex > 0) and (LSym.SectionIndex < Cardinal(Length(LObj.Sections))) then
        begin
          LKind := ClassifySection(
            LObj.Sections[LSym.SectionIndex].SectionName,
            LObj.Sections[LSym.SectionIndex].Flags);

          // Find contribution for this section
          case LKind of
            lskText:
              for LContrib in FTextContribs do
              begin
                if (LContrib.ObjectIndex = LI) and
                   (LContrib.OrigSectionIndex = Integer(LSym.SectionIndex)) then
                begin
                  LResolved.SectionKind := lskText;
                  LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
                  Break;
                end;
              end;

            lskRData:
              for LContrib in FRoDataContribs do
              begin
                if (LContrib.ObjectIndex = LI) and
                   (LContrib.OrigSectionIndex = Integer(LSym.SectionIndex)) then
                begin
                  LResolved.SectionKind := lskRData;
                  LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
                  Break;
                end;
              end;

            lskData:
              for LContrib in FDataContribs do
              begin
                if (LContrib.ObjectIndex = LI) and
                   (LContrib.OrigSectionIndex = Integer(LSym.SectionIndex)) then
                begin
                  LResolved.SectionKind := lskData;
                  LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
                  Break;
                end;
              end;
          end;
        end;

        FResolvedSymbols.Add(LSym.SymbolName, LResolved);
      end;
    end;
  end;

  Status('ELF Linker: Merged sections -- .text=%d, .rodata=%d, .data=%d, .bss=%d',
    [FMergedText.Size, FMergedRoData.Size, FMergedData.Size, FMergedBSS]);
end;

procedure TTigerELFLinker.CollectRelocations();
var
  LI, LJ, LK: Integer;
  LObj: TELFObject;
  LSec: TELFSection;
  LRel: TELFRelocation;
  LKind: TLinkerSectionKind;
  LContrib: TLinkerContribution;
  LPending: TLinkerPendingReloc;
  LSym: TELFSymbol;
  LSymName: string;
  LFound: Boolean;
begin
  FPendingRelocs.Clear();
  FUnresolvedSymbols.Clear();

  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Sections) do
    begin
      LSec := LObj.Sections[LJ];
      if Length(LSec.Relocations) = 0 then
        Continue;

      LKind := ClassifySection(LSec.SectionName, LSec.Flags);

      // Find this section's contribution
      LFound := False;
      LContrib.MergedOffset := 0;

      case LKind of
        lskText:
          for LK := 0 to FTextContribs.Count - 1 do
          begin
            if (FTextContribs[LK].ObjectIndex = LI) and
               (FTextContribs[LK].OrigSectionIndex = LJ) then
            begin
              LContrib := FTextContribs[LK];
              LFound := True;
              Break;
            end;
          end;

        lskRData:
          for LK := 0 to FRoDataContribs.Count - 1 do
          begin
            if (FRoDataContribs[LK].ObjectIndex = LI) and
               (FRoDataContribs[LK].OrigSectionIndex = LJ) then
            begin
              LContrib := FRoDataContribs[LK];
              LFound := True;
              Break;
            end;
          end;

        lskData:
          for LK := 0 to FDataContribs.Count - 1 do
          begin
            if (FDataContribs[LK].ObjectIndex = LI) and
               (FDataContribs[LK].OrigSectionIndex = LJ) then
            begin
              LContrib := FDataContribs[LK];
              LFound := True;
              Break;
            end;
          end;
      end;

      if not LFound then
        Continue;

      // Process relocations
      for LRel in LSec.Relocations do
      begin
        // Get symbol name
        if LRel.SymbolIndex < Cardinal(Length(LObj.Symbols)) then
        begin
          LSym := LObj.Symbols[LRel.SymbolIndex];
          LSymName := LSym.SymbolName;
        end
        else
          LSymName := '';

        LPending.SectionKind := LKind;
        LPending.OffsetInMerged := LContrib.MergedOffset + Cardinal(LRel.Offset);
        LPending.RelocationType := Word(LRel.RelocationType);
        LPending.TargetSymbol := LSymName;
        LPending.Addend := Int32(LRel.Addend);

        FPendingRelocs.Add(LPending);

        // Track unresolved symbols
        if (LSymName <> '') and (not FResolvedSymbols.ContainsKey(LSymName)) then
          FUnresolvedSymbols.Add(LSymName);
      end;
    end;
  end;

  Status('ELF Linker: Collected %d relocations, %d unresolved symbols',
    [FPendingRelocs.Count, FUnresolvedSymbols.Count]);
end;

//==============================================================================
// Public Interface
//==============================================================================

procedure TTigerELFLinker.AddObjectFile(const APath: string);
var
  LData: TBytes;
  LObj: TELFObject;
begin
  if not TFile.Exists(APath) then
  begin
    Status('ELF Linker: Object file not found: %s', [APath]);
    Exit;
  end;

  LData := TFile.ReadAllBytes(APath);
  LObj := ParseELFObject(LData, APath);
  if LObj <> nil then
    FObjects.Add(LObj);
end;

procedure TTigerELFLinker.AddLibraryFile(const APath: string);
var
  LData: TBytes;
begin
  if not TFile.Exists(APath) then
  begin
    Status('ELF Linker: Library file not found: %s', [APath]);
    Exit;
  end;

  LData := TFile.ReadAllBytes(APath);

  // Check if it's an AR archive
  if (Length(LData) >= 8) and (LData[0] = Ord('!')) and (LData[1] = Ord('<')) then
    ParseARLibrary(LData, APath)
  else
    Status('ELF Linker: Unknown library format: %s', [APath]);
end;

procedure TTigerELFLinker.AddObjectFromMemory(const AData: TBytes;
  const AName: string);
var
  LObj: TELFObject;
begin
  LObj := ParseELFObject(AData, AName);
  if LObj <> nil then
    FObjects.Add(LObj);
end;

procedure TTigerELFLinker.Resolve(const ANeededSymbols: TStrings);
var
  LI: Integer;
  LWorkList: TList<Integer>;
  LDefList: TList<Integer>;
  LSymbol: string;
  LObjIndex: Integer;
begin
  // First, build the global symbol index
  BuildGlobalSymbolIndex();

  // Create work list of objects to select
  LWorkList := TList<Integer>.Create();
  try
    // Start by selecting objects that define needed symbols
    for LI := 0 to ANeededSymbols.Count - 1 do
    begin
      LSymbol := ANeededSymbols[LI];
      if FSymbolIndex.TryGetValue(LSymbol, LDefList) then
      begin
        if LDefList.Count > 0 then
        begin
          LObjIndex := LDefList[0];
          if not FObjects[LObjIndex].Selected then
          begin
            if not LWorkList.Contains(LObjIndex) then
              LWorkList.Add(LObjIndex);
          end;
        end;
      end
      else
      begin
        // Symbol not defined in any loaded object
        FUnresolvedSymbols.Add(LSymbol);
      end;
    end;

    // Process work list, selecting objects and their dependencies
    while LWorkList.Count > 0 do
    begin
      LObjIndex := LWorkList[0];
      LWorkList.Delete(0);
      SelectObject(LObjIndex, LWorkList);
    end;

  finally
    LWorkList.Free();
  end;

  // Merge selected objects
  MergeSelectedObjects();

  // Collect relocations from merged sections
  CollectRelocations();

  // Apply internal relocations (calls between functions in merged code)
  ApplyInternalRelocations();

  Status('ELF Linker: Resolution complete -- %d objects selected, %d symbols resolved',
    [GetSelectedObjectCount(), FResolvedSymbols.Count]);
end;

procedure TTigerELFLinker.ApplyInternalRelocations();
var
  LReloc: TLinkerPendingReloc;
  LResolved: TLinkerResolvedSymbol;
  LDisp: Int32;
  LApplied: Integer;
  LI, LJ: Integer;
  LObj: TELFObject;
  LSym: TELFSymbol;
  LContrib: TLinkerContribution;
  LKind: TLinkerSectionKind;
  LFound: Boolean;
begin
  LApplied := 0;

  // First, add LOCAL symbols from selected objects to FResolvedSymbols
  for LI := 0 to FObjects.Count - 1 do
  begin
    LObj := FObjects[LI];
    if not LObj.Selected then
      Continue;

    for LJ := 0 to High(LObj.Symbols) do
    begin
      LSym := LObj.Symbols[LJ];
      // Include LOCAL function symbols (not just global)
      if (LSym.Binding = STB_LOCAL) and (LSym.SymbolType = STT_FUNC) and
         (LSym.SectionIndex > 0) and (LSym.SectionIndex < Cardinal(Length(LObj.Sections))) and
         (LSym.SymbolName <> '') and (not FResolvedSymbols.ContainsKey(LSym.SymbolName)) then
      begin
        LKind := ClassifySection(
          LObj.Sections[LSym.SectionIndex].SectionName,
          LObj.Sections[LSym.SectionIndex].Flags);

        if LKind = lskText then
        begin
          // Find contribution
          LFound := False;
          for LContrib in FTextContribs do
          begin
            if (LContrib.ObjectIndex = LI) and
               (LContrib.OrigSectionIndex = Integer(LSym.SectionIndex)) then
            begin
              LResolved.SymbolName := LSym.SymbolName;
              LResolved.SectionKind := lskText;
              LResolved.OffsetInMerged := LContrib.MergedOffset + Cardinal(LSym.Value);
              FResolvedSymbols.Add(LSym.SymbolName, LResolved);
              LFound := True;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;

  for LReloc in FPendingRelocs do
  begin
    // Only handle .text relocations for now
    if LReloc.SectionKind <> lskText then
      Continue;

    // Only handle PC-relative relocations
    if not (LReloc.RelocationType in [R_X86_64_PC32, R_X86_64_PLT32]) then
      Continue;

    // Check if target is resolved within merged code
    if not FResolvedSymbols.TryGetValue(LReloc.TargetSymbol, LResolved) then
      Continue;

    // Target must be in .text
    if LResolved.SectionKind <> lskText then
      Continue;

    // Compute displacement: S + A - P (ELF standard formula)
    LDisp := Int32(LResolved.OffsetInMerged) + LReloc.Addend - Int32(LReloc.OffsetInMerged);

    // Patch the merged .text
    FMergedText.Position := LReloc.OffsetInMerged;
    FMergedText.WriteData(LDisp);

    Inc(LApplied);
  end;

  FMergedText.Position := FMergedText.Size;
  Status('ELF Linker: Applied %d internal relocations', [LApplied]);
end;

function TTigerELFLinker.GetMergedText(): TBytes;
begin
  SetLength(Result, FMergedText.Size);
  if FMergedText.Size > 0 then
  begin
    FMergedText.Position := 0;
    FMergedText.ReadBuffer(Result[0], FMergedText.Size);
  end;
end;

function TTigerELFLinker.GetMergedTextSize(): Cardinal;
begin
  Result := Cardinal(FMergedText.Size);
end;

function TTigerELFLinker.GetMergedRData(): TBytes;
begin
  SetLength(Result, FMergedRoData.Size);
  if FMergedRoData.Size > 0 then
  begin
    FMergedRoData.Position := 0;
    FMergedRoData.ReadBuffer(Result[0], FMergedRoData.Size);
  end;
end;

function TTigerELFLinker.GetMergedRDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedRoData.Size);
end;

function TTigerELFLinker.GetMergedData(): TBytes;
begin
  SetLength(Result, FMergedData.Size);
  if FMergedData.Size > 0 then
  begin
    FMergedData.Position := 0;
    FMergedData.ReadBuffer(Result[0], FMergedData.Size);
  end;
end;

function TTigerELFLinker.GetMergedDataSize(): Cardinal;
begin
  Result := Cardinal(FMergedData.Size);
end;

function TTigerELFLinker.GetResolvedSymbols(): TDictionary<string, TLinkerResolvedSymbol>;
begin
  Result := FResolvedSymbols;
end;

function TTigerELFLinker.GetUnresolvedSymbols(): TStringList;
begin
  Result := FUnresolvedSymbols;
end;

function TTigerELFLinker.GetPendingRelocations(): TList<TLinkerPendingReloc>;
begin
  Result := FPendingRelocs;
end;

function TTigerELFLinker.GetObjectCount(): Integer;
begin
  Result := FObjects.Count;
end;

function TTigerELFLinker.GetSelectedObjectCount(): Integer;
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
