{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Utils.Win64;

{$I Tiger.Defines.inc}

interface

uses
  WinAPI.Windows,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Math,
  Tiger.Utils;


const
  LOAD_LIBRARY_SEARCH_DEFAULT_DIRS   = $00001000;
  LOAD_LIBRARY_SEARCH_USER_DIRS      = $00000400;
  LOAD_LIBRARY_SEARCH_APPLICATION_DIR= $00000200;
  LOAD_LIBRARY_SEARCH_SYSTEM32       = $00000800;

  // ConPTY constants
  PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE = $00020016;
  EXTENDED_STARTUPINFO_PRESENT        = $00080000;

type
  HPCON = THandle;

  PCOORD = ^COORD;
  COORD = record
    X: SmallInt;
    Y: SmallInt;
  end;

  PSTARTUPINFOEXW = ^STARTUPINFOEXW;
  STARTUPINFOEXW = record
    StartupInfo: TStartupInfoW;
    lpAttributeList: Pointer;
  end;

  { TCommandBuilder }
  TCommandBuilder = class(TBaseObject)
  private
    FParams: TStringList;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    
    procedure Clear();
    procedure AddParam(const AParam: string); overload;
    procedure AddParam(const AFlag, AValue: string); overload;
    procedure AddQuotedParam(const AFlag, AValue: string); overload;
    procedure AddQuotedParam(const AValue: string); overload;
    procedure AddFlag(const AFlag: string);
    
    function ToString(): string; reintroduce;
    function GetParamCount(): Integer;
  end;

  { TWin64Utils - Win32/Win64-specific utilities }
  TWin64Utils = class
  private
    class function  EnableVirtualTerminalProcessing(): Boolean; static;
    class procedure InitConsole(); static;
  public
    class function  GetTickCount(): DWORD; static;
    class function  GetTickCount64(): UInt64; static;

    class function  CallI64(AFunction: Pointer; const AArgs: array of const): UInt64; static;
    class function  CallF32(AFunction: Pointer; const AArgs: array of const): Single; static;
    class function  CallF64(AFunction: Pointer; const AArgs: array of const): Double; static;

    class function  HasConsole(): Boolean; static;
    class procedure ClearToEOL(); static;
    class procedure Print(); overload; static;
    class procedure PrintLn(); overload; static;
    class procedure Print(const AText: string); overload; static;
    class procedure Print(const AText: string; const AArgs: array of const); overload; static;
    class procedure PrintLn(const AText: string); overload; static;
    class procedure PrintLn(const AText: string; const AArgs: array of const); overload; static;
    class function  Pause(const AMsg: string=''; const AQuit: string = ''): Boolean; static;

    class procedure ProcessMessages(); static;

    class function  RunExe(const AExe, AParams, AWorkDir: string; const AWait: Boolean = True; const AShowCmd: Word = SW_SHOWNORMAL): Cardinal; static;
    class function  RunElf(const AElf, AWorkDir: string): Cardinal; static;
    class procedure CaptureConsoleOutput(const ATitle: string; const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback); static;
    class procedure CaptureZigConsolePTY(const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback); static;
    class function  CreateProcessWithPipes(const AExe, AParams, AWorkDir: string; out AStdinWrite: THandle; out AStdoutRead: THandle; out AProcessHandle: THandle; out AThreadHandle: THandle): Boolean; static;

    class function  GetVersionInfo(out AVersionInfo: TVersionInfo; const AFilePath: string = ''): Boolean; static;
    class function  GetZigExePath(): string; static;
    class function  GetExePath(): string; static;

    class function  IsValidWin64PE(const AFilePath: string): Boolean; static;
    class procedure UpdateIconResource(const AExeFilePath, AIconFilePath: string); static;
    class procedure UpdateVersionInfoResource(const PEFilePath: string; const AMajor, AMinor, APatch: Word; const AProductName, ADescription, AFilename, ACompanyName, ACopyright: string); static;
    class function  ResourceExist(const AResName: string): Boolean; static;
    class function  AddResManifestFromResource(const aResName: string; const aModuleFile: string; aLanguage: Integer=1033): Boolean; static;

    class procedure SetEnv(const AName: string; const AValue: string); static;
    class function  RunFromIDE(): Boolean; static;
  end;

function AddDllDirectory(NewDirectory: LPCWSTR): Pointer; stdcall; external kernel32 name 'AddDllDirectory';
function RemoveDllDirectory(Cookie: Pointer): BOOL; stdcall; external kernel32 name 'RemoveDllDirectory';
function SetDefaultDllDirectories(DirectoryFlags: DWORD): BOOL; stdcall; external kernel32 name 'SetDefaultDllDirectories';
function GetEnvironmentStringsW(): PWideChar; stdcall; external kernel32 name 'GetEnvironmentStringsW';
function FreeEnvironmentStringsW(lpszEnvironmentBlock: PWideChar): BOOL; stdcall; external kernel32 name 'FreeEnvironmentStringsW';

// ConPTY functions
function CreatePseudoConsole(size: COORD; hInput, hOutput: THandle; dwFlags: DWORD; out phPC: HPCON): HRESULT; stdcall; external kernel32 name 'CreatePseudoConsole';
function ClosePseudoConsole(hPC: HPCON): HRESULT; stdcall; external kernel32 name 'ClosePseudoConsole';
function InitializeProcThreadAttributeList(lpAttributeList: Pointer; dwAttributeCount: DWORD; dwFlags: DWORD; var lpSize: SIZE_T): BOOL; stdcall; external kernel32 name 'InitializeProcThreadAttributeList';
function UpdateProcThreadAttribute(lpAttributeList: Pointer; dwFlags: DWORD; Attribute: DWORD_PTR; lpValue: Pointer; cbSize: SIZE_T; lpPreviousValue: Pointer; lpReturnSize: PSIZE_T): BOOL; stdcall; external kernel32 name 'UpdateProcThreadAttribute';
procedure DeleteProcThreadAttributeList(lpAttributeList: Pointer); stdcall; external kernel32 name 'DeleteProcThreadAttributeList';


implementation

{$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
function ffi_call_win64_i64(AFunction: Pointer; AArgs: PUInt64; AArgCount: Cardinal): UInt64; assembler;
asm
  // Prologue with only RBX saved; compute aligned stack space so that
  // RSP is 16-byte aligned at the CALL site.
  push rbp
  mov  rbp, rsp
  push rbx

  // Volatile locals
  mov  r11, rcx        // AFunction
  mov  r10, rdx        // AArgs
  mov  eax, r8d        // AArgCount -> EAX

  // k = max(0, ArgCount-4)
  mov  ecx, eax
  sub  ecx, 4
  xor  edx, edx
  cmp  ecx, 0
  jle  @no_stack
  mov  edx, ecx
  shl  edx, 3          // edx = k * 8
@no_stack:
  // s = 32 + 8*k ; ensure s ≡ 8 (mod 16) because we've pushed RBX
  lea  ebx, [rdx + 32] // ebx = base space
  test ecx, 1          // if k even, add +8; if k odd, already ≡ 8
  jnz  @have_s
  add  ebx, 8
@have_s:
  sub  rsp, rbx        // allocate

  // Copy stack args (5..N) to [rsp+32]
  mov  ecx, eax
  cmp  ecx, 4
  jle  @load_regs
  sub  ecx, 4
  lea  rsi, [r10 + 32]   // src
  lea  rdi, [rsp + 32]   // dst
  rep  movsq

@load_regs:
  // Dual-load first 4 slots
  test eax, eax
  jz   @do_call
  mov  rcx, [r10]
  movsd xmm0, qword ptr [r10]

  cmp  eax, 1
  jle  @do_call
  mov  rdx, [r10 + 8]
  movsd xmm1, qword ptr [r10 + 8]

  cmp  eax, 2
  jle  @do_call
  mov  r8,  [r10 + 16]
  movsd xmm2, qword ptr [r10 + 16]

  cmp  eax, 3
  jle  @do_call
  mov  r9,  [r10 + 24]
  movsd xmm3, qword ptr [r10 + 24]

@do_call:
  call r11

  // Epilogue
  add  rsp, rbx
  pop  rbx
  pop  rbp
  ret
end;

procedure ffi_call_win64_f32(AFunction: Pointer; AArgs: PUInt64; AArgCount: Cardinal; AResult: PSingle); assembler;
asm
  push rbp
  mov  rbp, rsp
  push rbx

  mov  r11, rcx        // AFunction
  mov  r10, rdx        // AArgs
  mov  eax, r8d        // AArgCount
  mov  r9,  r9         // AResult already in R9 (keep)

  // k = max(0, ArgCount-4)
  mov  ecx, eax
  sub  ecx, 4
  xor  edx, edx
  cmp  ecx, 0
  jle  @no_stack
  mov  edx, ecx
  shl  edx, 3
@no_stack:
  // s = 32 + 8*k ; adjust for RBX push parity
  lea  ebx, [rdx + 32]
  test ecx, 1
  jnz  @have_s
  add  ebx, 8
@have_s:
  sub  rsp, rbx

  // Copy stack args
  mov  ecx, eax
  cmp  ecx, 4
  jle  @load_regs
  sub  ecx, 4
  lea  rsi, [r10 + 32]
  lea  rdi, [rsp + 32]
  rep  movsq

@load_regs:
  test eax, eax
  jz   @do_call
  mov  rcx, [r10]
  // For float params the low 32 bits contain the value; movsd is fine (callee reads low 32)
  movsd xmm0, qword ptr [r10]

  cmp  eax, 1
  jle  @do_call
  mov  rdx, [r10 + 8]
  movsd xmm1, qword ptr [r10 + 8]

  cmp  eax, 2
  jle  @do_call
  mov  r8,  [r10 + 16]
  movsd xmm2, qword ptr [r10 + 16]

  cmp  eax, 3
  jle  @do_call
  mov  r9,  [r10 + 24]
  movsd xmm3, qword ptr [r10 + 24]

@do_call:
  call r11

  // Store float result
  test r9, r9
  jz   @done
  movss dword ptr [r9], xmm0
@done:
  add  rsp, rbx
  pop  rbx
  pop  rbp
  ret
end;

procedure ffi_call_win64_f64(AFunction: Pointer; AArgs: PUInt64; AArgCount: Cardinal; AResult: PDouble); assembler;
asm
  push rbp
  mov  rbp, rsp
  push rbx

  mov  r11, rcx        // AFunction
  mov  r10, rdx        // AArgs
  mov  eax, r8d        // AArgCount
  mov  r9,  r9         // AResult already in R9

  // k = max(0, ArgCount-4)
  mov  ecx, eax
  sub  ecx, 4
  xor  edx, edx
  cmp  ecx, 0
  jle  @no_stack
  mov  edx, ecx
  shl  edx, 3
@no_stack:
  // s = 32 + 8*k ; adjust for RBX push parity
  lea  ebx, [rdx + 32]
  test ecx, 1
  jnz  @have_s
  add  ebx, 8
@have_s:
  sub  rsp, rbx

  // Copy stack args
  mov  ecx, eax
  cmp  ecx, 4
  jle  @load_regs
  sub  ecx, 4
  lea  rsi, [r10 + 32]
  lea  rdi, [rsp + 32]
  rep  movsq

@load_regs:
  test eax, eax
  jz   @do_call
  mov  rcx, [r10]
  movsd xmm0, qword ptr [r10]

  cmp  eax, 1
  jle  @do_call
  mov  rdx, [r10 + 8]
  movsd xmm1, qword ptr [r10 + 8]

  cmp  eax, 2
  jle  @do_call
  mov  r8,  [r10 + 16]
  movsd xmm2, qword ptr [r10 + 16]

  cmp  eax, 3
  jle  @do_call
  mov  r9,  [r10 + 24]
  movsd xmm3, qword ptr [r10 + 24]

@do_call:
  call r11

  // Store double result
  test r9, r9
  jz   @done
  movsd qword ptr [r9], xmm0
@done:
  add  rsp, rbx
  pop  rbx
  pop  rbp
  ret
end;
{$ENDIF}

{ TWin64Utils }

class function TWin64Utils.EnableVirtualTerminalProcessing(): Boolean;
var
  HOut: THandle;
  LMode: DWORD;
begin
  Result := False;

  HOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if HOut = INVALID_HANDLE_VALUE then Exit;
  if not GetConsoleMode(HOut, LMode) then Exit;

  LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if not SetConsoleMode(HOut, LMode) then Exit;

  Result := True;
end;


class procedure TWin64Utils.InitConsole();
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
    EnableVirtualTerminalProcessing();
    SetConsoleCP(CP_UTF8);
    SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
end;

type
  TUInt64Array = array of UInt64;


class function TWin64Utils.CallI64(AFunction: Pointer; const AArgs: array of const): UInt64;
var
  LSlots: TUInt64Array;
  I: Integer;
  L: UInt64;
begin
  SetLength(LSlots, Length(AArgs));
  for I := 0 to High(AArgs) do
  begin
    L := 0;
    case AArgs[I].VType of
      vtInteger:       L := UInt64(Int64(AArgs[I].VInteger));
      vtInt64:         L := UInt64(PInt64(AArgs[I].VInt64)^);
      vtBoolean:       L := Ord(AArgs[I].VBoolean);
      vtPointer:       L := UInt64(NativeUInt(AArgs[I].VPointer));
      vtPChar:         L := UInt64(NativeUInt(AArgs[I].VPChar));
      vtPWideChar:     L := UInt64(NativeUInt(AArgs[I].VPWideChar));
      vtClass:         L := UInt64(NativeUInt(AArgs[I].VClass));
      vtObject:        L := UInt64(NativeUInt(AArgs[I].VObject));
      vtWideChar:      L := UInt64(Ord(AArgs[I].VWideChar));
      vtChar:          L := UInt64(Ord(AArgs[I].VChar));
      vtAnsiString:    L := UInt64(NativeUInt(AArgs[I].VAnsiString));      // pointer to Ansi data
      vtUnicodeString: L := UInt64(NativeUInt(AArgs[I].VUnicodeString));   // pointer to UTF-16 data
      vtExtended:      Move(PExtended(AArgs[I].VExtended)^, L, 8);         // pass as double bits
      vtCurrency:      Move(PCurrency(AArgs[I].VCurrency)^, L, 8);
      vtVariant:       L := UInt64(NativeUInt(AArgs[I].VVariant));         // pointer to Variant
    else
      L := 0;
    end;
    LSlots[I] := L;
  end;

  if Length(LSlots) = 0 then
    Result := ffi_call_win64_i64(AFunction, nil, 0)
  else
    Result := ffi_call_win64_i64(AFunction, @LSlots[0], Length(LSlots));
end;


class function TWin64Utils.CallF32(AFunction: Pointer; const AArgs: array of const): Single;
var
  LSlots: TUInt64Array;
  I: Integer;
  L: UInt64;
  S: Single;
begin
  SetLength(LSlots, Length(AArgs));
  for I := 0 to High(AArgs) do
  begin
    L := 0;
    case AArgs[I].VType of
      vtExtended:      begin S := Single(PExtended(AArgs[I].VExtended)^); Move(S, L, 4); end;
      vtInteger:       L := UInt64(Int64(AArgs[I].VInteger));
      vtInt64:         L := UInt64(PInt64(AArgs[I].VInt64)^);
      vtBoolean:       L := Ord(AArgs[I].VBoolean);
      vtPointer:       L := UInt64(NativeUInt(AArgs[I].VPointer));
      vtPChar:         L := UInt64(NativeUInt(AArgs[I].VPChar));
      vtPWideChar:     L := UInt64(NativeUInt(AArgs[I].VPWideChar));
      vtChar:          L := UInt64(Ord(AArgs[I].VChar));
      vtWideChar:      L := UInt64(Ord(AArgs[I].VWideChar));
      vtAnsiString:    L := UInt64(NativeUInt(AArgs[I].VAnsiString));
      vtUnicodeString: L := UInt64(NativeUInt(AArgs[I].VUnicodeString));
      vtCurrency:      Move(PCurrency(AArgs[I].VCurrency)^, L, 8);
      vtVariant:       L := UInt64(NativeUInt(AArgs[I].VVariant));
    else
      L := 0;
    end;
    LSlots[I] := L;
  end;

  if Length(LSlots) = 0 then
    ffi_call_win64_f32(AFunction, nil, 0, @Result)
  else
    ffi_call_win64_f32(AFunction, @LSlots[0], Length(LSlots), @Result);
end;


class function TWin64Utils.CallF64(AFunction: Pointer; const AArgs: array of const): Double;
var
  LSlots: TUInt64Array;
  I: Integer;
  L: UInt64;
  D: Double;
begin
  SetLength(LSlots, Length(AArgs));
  for I := 0 to High(AArgs) do
  begin
    L := 0;
    case AArgs[I].VType of
      vtExtended:      begin D := Double(PExtended(AArgs[I].VExtended)^); Move(D, L, 8); end;
      vtInteger:       L := UInt64(Int64(AArgs[I].VInteger));
      vtInt64:         L := UInt64(PInt64(AArgs[I].VInt64)^);
      vtBoolean:       L := Ord(AArgs[I].VBoolean);
      vtPointer:       L := UInt64(NativeUInt(AArgs[I].VPointer));
      vtPChar:         L := UInt64(NativeUInt(AArgs[I].VPChar));
      vtPWideChar:     L := UInt64(NativeUInt(AArgs[I].VPWideChar));
      vtChar:          L := UInt64(Ord(AArgs[I].VChar));
      vtWideChar:      L := UInt64(Ord(AArgs[I].VWideChar));
      vtAnsiString:    L := UInt64(NativeUInt(AArgs[I].VAnsiString));
      vtUnicodeString: L := UInt64(NativeUInt(AArgs[I].VUnicodeString));
      vtCurrency:      Move(PCurrency(AArgs[I].VCurrency)^, L, 8);
      vtVariant:       L := UInt64(NativeUInt(AArgs[I].VVariant));
    else
      L := 0;
    end;
    LSlots[I] := L;
  end;

  if Length(LSlots) = 0 then
    ffi_call_win64_f64(AFunction, nil, 0, @Result)
  else
    ffi_call_win64_f64(AFunction, @LSlots[0], Length(LSlots), @Result);
end;


class function TWin64Utils.GetTickCount(): DWORD;
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
  Result := WinApi.Windows.GetTickCount();
  {$ENDIF}
end;


class function TWin64Utils.GetTickCount64(): UInt64;
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
  Result := WinApi.Windows.GetTickCount64();
  {$ENDIF}
end;


class function TWin64Utils.HasConsole(): Boolean;
begin
  {$IF DEFINED(MSWINDOWS) AND DEFINED(CPUX64)}
  Result := Boolean(GetConsoleWindow() <> 0);
  {$ENDIF}
end;


class procedure TWin64Utils.ClearToEOL();
begin
  if not HasConsole() then Exit;
  Write(#27'[0K');
end;

class procedure  TWin64Utils.Print();
begin
  Print('');
end;

class procedure  TWin64Utils.PrintLn();
begin
  PrintLn('');
end;


class procedure TWin64Utils.Print(const AText: string);
begin
  if not HasConsole() then Exit;
  Write(AText);
end;

class procedure TWin64Utils.Print(const AText: string; const AArgs: array of const);
begin
  if not HasConsole() then Exit;
  Write(Format(AText, AArgs));
end;

class procedure TWin64Utils.PrintLn(const AText: string);
begin
  if not HasConsole() then Exit;
  WriteLn(AText);
end;

class procedure  TWin64Utils.PrintLn(const AText: string; const AArgs: array of const);
begin
  if not HasConsole() then Exit;
  WriteLn(Format(AText, AArgs));
end;


class function TWin64Utils.Pause(const AMsg, AQuit: string): Boolean;
var
  LInput: string;
begin
  Result := False;
  PrintLn('');
  if AMsg.IsEmpty then
    Print('Press ENTER to continue...')
  else
    Print(AMsg);
  ReadLn(LInput);
  if not AQuit.IsEmpty then
  begin
    if SameText(LInput, AQuit) then
      Result := True;
  end;
  PrintLn('');
end;


class procedure TWin64Utils.ProcessMessages();
var
  LMsg: TMsg;
begin
  while Integer(PeekMessage(LMsg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;


class function TWin64Utils.RunExe(const AExe, AParams, AWorkDir: string; const AWait: Boolean; const AShowCmd: Word): Cardinal;
var
  LAppPath: string;
  LCmd: UnicodeString;
  LSI: STARTUPINFOW;
  LPI: PROCESS_INFORMATION;
  LExit: DWORD;
  LCreationFlags: DWORD;
  LWorkDirPW: PWideChar;
begin

  if AExe = '' then
    raise Exception.Create('RunExe: Executable path is empty');

  // Resolve the executable path against the workdir if only a filename was provided.
  if TPath.IsPathRooted(AExe) or (Pos('\', AExe) > 0) or (Pos('/', AExe) > 0) then
    LAppPath := AExe
  else if AWorkDir <> '' then
    LAppPath := TPath.Combine(AWorkDir, AExe)
  else
    LAppPath := AExe; // will rely on caller's current dir / PATH

  // Quote the app path and build a mutable command line.
  if AParams <> '' then
    LCmd := '"' + LAppPath + '" ' + AParams
  else
    LCmd := '"' + LAppPath + '"';
  UniqueString(LCmd);

  // Optional: ensure the exe exists when a workdir is provided.
  if (AWorkDir <> '') and (not TFile.Exists(LAppPath)) then
    raise Exception.CreateFmt('RunExe: Executable not found: %s', [LAppPath]);

  ZeroMemory(@LSI, SizeOf(LSI));
  ZeroMemory(@LPI, SizeOf(LPI));
  LSI.cb := SizeOf(LSI);
  LSI.dwFlags := STARTF_USESHOWWINDOW;
  LSI.wShowWindow := AShowCmd;

  if AWorkDir <> '' then
    LWorkDirPW := PWideChar(AWorkDir)
  else
    LWorkDirPW := nil;

  LCreationFlags := CREATE_UNICODE_ENVIRONMENT;

  // IMPORTANT: pass the resolved path in lpApplicationName so Windows won't search using the caller's current directory.
  if not CreateProcessW(
    PWideChar(LAppPath),   // lpApplicationName (explicit module path)
    PWideChar(LCmd),       // lpCommandLine (mutable, includes quoted path + params)
    nil,                   // lpProcessAttributes
    nil,                   // lpThreadAttributes
    False,                 // bInheritHandles
    LCreationFlags,        // dwCreationFlags
    nil,                   // lpEnvironment
    LWorkDirPW,            // lpCurrentDirectory (workdir for the child)
    LSI,                   // lpStartupInfo
    LPI                    // lpProcessInformation
  ) then
    raise Exception.CreateFmt('RunExe: CreateProcess failed (%d) %s', [GetLastError, SysErrorMessage(GetLastError)]);

  try
    if AWait then
    begin
      WaitForSingleObject(LPI.hProcess, INFINITE);
      LExit := 0;
      if GetExitCodeProcess(LPI.hProcess, LExit) then
        Result := LExit
      else
        raise Exception.CreateFmt('RunExe: GetExitCodeProcess failed (%d) %s', [GetLastError, SysErrorMessage(GetLastError)]);
    end
    else
      Result := 0;
  finally
    CloseHandle(LPI.hThread);
    CloseHandle(LPI.hProcess);
  end;
end;

class function TWin64Utils.RunElf(const AElf, AWorkDir: string): Cardinal;
var
  LFullPath: string;
  LWslPath: string;
  LDrive: Char;
  LCmd: UnicodeString;
  LSI: STARTUPINFOW;
  LPI: PROCESS_INFORMATION;
  LExit: DWORD;
begin
  if AElf = '' then
    raise Exception.Create('RunElf: ELF path is empty');

  // Resolve relative paths to absolute (WSL needs a drive letter)
  LFullPath := TPath.GetFullPath(AElf);

  // Convert Windows path to WSL path: C:\foo\bar -> /mnt/c/foo/bar
  if (Length(LFullPath) >= 3) and (LFullPath[2] = ':') and (LFullPath[3] = '\') then
  begin
    LDrive := LowerCase(LFullPath[1])[1];
    LWslPath := '/mnt/' + LDrive + '/' +
      StringReplace(Copy(LFullPath, 4, MaxInt), '\', '/', [rfReplaceAll]);
  end
  else
    raise Exception.CreateFmt('RunElf: Expected absolute Windows path: %s', [LFullPath]);

  //--------------------------------------------------------------------------
  // Step 1: chmod +x via WSL (make the ELF executable)
  //--------------------------------------------------------------------------
  LCmd := 'wsl.exe chmod +x "' + LWslPath + '"';
  UniqueString(LCmd);

  ZeroMemory(@LSI, SizeOf(LSI));
  ZeroMemory(@LPI, SizeOf(LPI));
  LSI.cb := SizeOf(LSI);
  LSI.dwFlags := STARTF_USESHOWWINDOW;
  LSI.wShowWindow := SW_HIDE;

  if not CreateProcessW(
    nil,                   // lpApplicationName
    PWideChar(LCmd),       // lpCommandLine
    nil,                   // lpProcessAttributes
    nil,                   // lpThreadAttributes
    False,                 // bInheritHandles
    CREATE_UNICODE_ENVIRONMENT,
    nil,                   // lpEnvironment
    PWideChar(AWorkDir),   // lpCurrentDirectory
    LSI,                   // lpStartupInfo
    LPI                    // lpProcessInformation
  ) then
    raise Exception.CreateFmt('RunElf: chmod CreateProcess failed (%d) %s',
      [GetLastError, SysErrorMessage(GetLastError)]);

  try
    WaitForSingleObject(LPI.hProcess, INFINITE);
  finally
    CloseHandle(LPI.hThread);
    CloseHandle(LPI.hProcess);
  end;

  //--------------------------------------------------------------------------
  // Step 2: Execute the ELF binary via WSL
  //--------------------------------------------------------------------------
  LCmd := 'wsl.exe "' + LWslPath + '"';
  UniqueString(LCmd);

  ZeroMemory(@LSI, SizeOf(LSI));
  ZeroMemory(@LPI, SizeOf(LPI));
  LSI.cb := SizeOf(LSI);
  LSI.dwFlags := STARTF_USESHOWWINDOW;
  LSI.wShowWindow := SW_HIDE;

  if not CreateProcessW(
    nil,                   // lpApplicationName
    PWideChar(LCmd),       // lpCommandLine
    nil,                   // lpProcessAttributes
    nil,                   // lpThreadAttributes
    False,                 // bInheritHandles
    CREATE_UNICODE_ENVIRONMENT,
    nil,                   // lpEnvironment
    PWideChar(AWorkDir),   // lpCurrentDirectory
    LSI,                   // lpStartupInfo
    LPI                    // lpProcessInformation
  ) then
    raise Exception.CreateFmt('RunElf: execute CreateProcess failed (%d) %s',
      [GetLastError, SysErrorMessage(GetLastError)]);

  try
    WaitForSingleObject(LPI.hProcess, INFINITE);
    LExit := 0;
    if GetExitCodeProcess(LPI.hProcess, LExit) then
      Result := LExit
    else
      raise Exception.CreateFmt('RunElf: GetExitCodeProcess failed (%d) %s',
        [GetLastError, SysErrorMessage(GetLastError)]);
  finally
    CloseHandle(LPI.hThread);
    CloseHandle(LPI.hProcess);
  end;
end;


class procedure TWin64Utils.CaptureConsoleOutput(const ATitle: string; const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback);
const
  //CReadBuffer = 2400;
  CReadBuffer = 1024*2;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
  CmdLine: string;
  LExitCode: DWORD;
  LWorkDirPtr: PChar;
  LLineAccumulator: TStringBuilder;
  LI: Integer;
  LChar: AnsiChar;
  LCurrentLine: string;
begin
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if ATitle.IsEmpty then
        suiStartup.lpTitle := nil
      else
        suiStartup.lpTitle := PChar(ATitle);
      CmdLine := ACommand + ' ' + AParameters;
      if AWorkDir <> '' then
        LWorkDirPtr := PChar(AWorkDir)
      else
        LWorkDirPtr := nil;
      if CreateProcess(nil, PChar(CmdLine), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil, LWorkDirPtr, suiStartup, piProcess) then
        try
          LLineAccumulator := TStringBuilder.Create;
          try
            repeat
              dRunning := WaitForSingleObject(piProcess.hProcess, 100);
              PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
              if (dAvailable > 0) then
                repeat
                  dRead := 0;
                  ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                  pBuffer[dRead] := #0;
                  OemToCharA(pBuffer, dBuffer);
                  
                  // Process character-by-character to find complete lines
                  LI := 0;
                  while LI < Integer(dRead) do
                  begin
                    LChar := dBuffer[LI];
                    
                    if (LChar = #13) or (LChar = #10) then
                    begin
                      // Found line terminator - emit accumulated line if not empty
                      if LLineAccumulator.Length > 0 then
                      begin
                        LCurrentLine := LLineAccumulator.ToString();
                        LLineAccumulator.Clear();
                        
                        if Assigned(ACallback) then
                          ACallback(LCurrentLine, AUserData);
                      end;
                      
                      // Skip paired CR+LF
                      if (LChar = #13) and (LI + 1 < Integer(dRead)) and (dBuffer[LI + 1] = #10) then
                        Inc(LI);
                    end
                    else
                    begin
                      // Accumulate character
                      LLineAccumulator.Append(string(LChar));
                    end;
                    
                    Inc(LI);
                  end;
                until (dRead < CReadBuffer);
              ProcessMessages;
            until (dRunning <> WAIT_TIMEOUT);
            
            // Emit any remaining partial line
            if LLineAccumulator.Length > 0 then
            begin
              LCurrentLine := LLineAccumulator.ToString();
              if Assigned(ACallback) then
                ACallback(LCurrentLine, AUserData);
            end;

            if GetExitCodeProcess(piProcess.hProcess, LExitCode) then
            begin
              AExitCode := LExitCode;
            end;

          finally
            FreeAndNil(LLineAccumulator);
          end;
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
end;


class procedure TWin64Utils.CaptureZigConsolePTY(const ACommand: PChar; const AParameters: PChar; const AWorkDir: string; var AExitCode: DWORD; const AUserData: Pointer; const ACallback: TCaptureConsoleCallback);
const
  CReadBuffer = 4096;
var
  LInputReadSide: THandle;
  LInputWriteSide: THandle;
  LOutputReadSide: THandle;
  LOutputWriteSide: THandle;
  LConsoleSize: COORD;
  LConsoleHandle: THandle;
  LConsoleInfo: TConsoleScreenBufferInfo;
  LPseudoConsole: HPCON;
  LAttrListSize: SIZE_T;
  LAttrList: Pointer;
  LStartupInfoEx: STARTUPINFOEXW;
  LProcessInfo: TProcessInformation;
  LCmdLine: string;
  LWorkDirPtr: PChar;
  LExitCode: DWORD;
  LBuffer: array[0..CReadBuffer-1] of AnsiChar;
  LBytesRead: DWORD;
  LBytesAvailable: DWORD;
  LRunning: DWORD;

begin
  AExitCode := 1;
  LPseudoConsole := 0;
  LAttrList := nil;
  LInputReadSide := 0;
  LInputWriteSide := 0;
  LOutputReadSide := 0;
  LOutputWriteSide := 0;

  // Create pipes for ConPTY
  if not CreatePipe(LInputReadSide, LInputWriteSide, nil, 0) then
    Exit;

  if not CreatePipe(LOutputReadSide, LOutputWriteSide, nil, 0) then
  begin
    CloseHandle(LInputReadSide);
    CloseHandle(LInputWriteSide);
    Exit;
  end;

  try
    // Match PTY size to actual visible window size
    LConsoleSize.X := 120;
    LConsoleSize.Y := 30;
    LConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    if (LConsoleHandle <> INVALID_HANDLE_VALUE) and GetConsoleScreenBufferInfo(LConsoleHandle, LConsoleInfo) then
    begin
      LConsoleSize.X := LConsoleInfo.srWindow.Right - LConsoleInfo.srWindow.Left + 1;
      LConsoleSize.Y := LConsoleInfo.srWindow.Bottom - LConsoleInfo.srWindow.Top + 1;
    end;

    if Failed(CreatePseudoConsole(LConsoleSize, LInputReadSide, LOutputWriteSide, 0, LPseudoConsole)) then
      Exit;

    try
      // Close the handles that were given to the pseudoconsole
      CloseHandle(LInputReadSide);
      LInputReadSide := 0;
      CloseHandle(LOutputWriteSide);
      LOutputWriteSide := 0;

      // Get attribute list size
      LAttrListSize := 0;
      InitializeProcThreadAttributeList(nil, 1, 0, LAttrListSize);

      // Allocate attribute list
      LAttrList := AllocMem(LAttrListSize);
      if not InitializeProcThreadAttributeList(LAttrList, 1, 0, LAttrListSize) then
        Exit;

      try
        // Set pseudoconsole attribute
        if not UpdateProcThreadAttribute(LAttrList, 0, PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
            Pointer(LPseudoConsole), SizeOf(HPCON), nil, nil) then
          Exit;

        // Initialize extended startup info
        FillChar(LStartupInfoEx, SizeOf(LStartupInfoEx), 0);
        LStartupInfoEx.StartupInfo.cb := SizeOf(STARTUPINFOEXW);
        LStartupInfoEx.lpAttributeList := LAttrList;

        // Build command line
        LCmdLine := string(ACommand) + ' ' + string(AParameters);

        if AWorkDir <> '' then
          LWorkDirPtr := PChar(AWorkDir)
        else
          LWorkDirPtr := nil;

        // Create process - pass nil for environment to inherit from parent
        FillChar(LProcessInfo, SizeOf(LProcessInfo), 0);
        if not CreateProcessW(nil, PWideChar(LCmdLine), nil, nil, False,
            EXTENDED_STARTUPINFO_PRESENT,
            nil, LWorkDirPtr, LStartupInfoEx.StartupInfo, LProcessInfo) then
          Exit;

        try
          repeat
            LRunning := WaitForSingleObject(LProcessInfo.hProcess, 50);

            // Read available output
            while True do
            begin
              LBytesAvailable := 0;
              if not PeekNamedPipe(LOutputReadSide, nil, 0, nil, @LBytesAvailable, nil) then
                Break;

              if LBytesAvailable = 0 then
                Break;

              LBytesRead := 0;
              if not ReadFile(LOutputReadSide, LBuffer[0], CReadBuffer - 1, LBytesRead, nil) then
                Break;

              if LBytesRead = 0 then
                Break;

              LBuffer[LBytesRead] := #0;

              // Convert UTF-8 to Unicode and pass raw to callback
              // Let the terminal handle all control codes natively
              if Assigned(ACallback) then
                ACallback(UTF8ToString(PAnsiChar(@LBuffer[0])), AUserData);
            end;

            ProcessMessages();
          until LRunning <> WAIT_TIMEOUT;

          // Small delay to allow final output to be buffered
          Sleep(100);

          // Drain any remaining output after process exits
          repeat
            LBytesAvailable := 0;
            if not PeekNamedPipe(LOutputReadSide, nil, 0, nil, @LBytesAvailable, nil) then
              Break;

            if LBytesAvailable = 0 then
            begin
              // Try one more time after a brief wait
              Sleep(50);
              if not PeekNamedPipe(LOutputReadSide, nil, 0, nil, @LBytesAvailable, nil) then
                Break;
              if LBytesAvailable = 0 then
                Break;
            end;

            LBytesRead := 0;
            if not ReadFile(LOutputReadSide, LBuffer[0], CReadBuffer - 1, LBytesRead, nil) then
              Break;

            if LBytesRead = 0 then
              Break;

            LBuffer[LBytesRead] := #0;

            if Assigned(ACallback) then
              ACallback(UTF8ToString(PAnsiChar(@LBuffer[0])), AUserData);
          until False;

          // Get exit code
          if GetExitCodeProcess(LProcessInfo.hProcess, LExitCode) then
            AExitCode := LExitCode;
        finally
          CloseHandle(LProcessInfo.hProcess);
          CloseHandle(LProcessInfo.hThread);
        end;
      finally
        DeleteProcThreadAttributeList(LAttrList);
      end;
    finally
      ClosePseudoConsole(LPseudoConsole);
    end;
  finally
    if LAttrList <> nil then
      FreeMem(LAttrList);
    if LInputReadSide <> 0 then
      CloseHandle(LInputReadSide);
    if LInputWriteSide <> 0 then
      CloseHandle(LInputWriteSide);
    if LOutputReadSide <> 0 then
      CloseHandle(LOutputReadSide);
    if LOutputWriteSide <> 0 then
      CloseHandle(LOutputWriteSide);
  end;
end;


class function TWin64Utils.CreateProcessWithPipes(const AExe, AParams, AWorkDir: string; out AStdinWrite: THandle; out AStdoutRead: THandle; out AProcessHandle: THandle; out AThreadHandle: THandle): Boolean;
var
  LSA: TSecurityAttributes;
  LStdinReadChild: THandle;
  LStdoutWriteChild: THandle;
  LSI: TStartupInfoW;
  LPI: TProcessInformation;
  LCmdLine: UnicodeString;
  LWorkDirPW: PWideChar;
begin
  Result := False;
  AStdinWrite := INVALID_HANDLE_VALUE;
  AStdoutRead := INVALID_HANDLE_VALUE;
  AProcessHandle := INVALID_HANDLE_VALUE;
  AThreadHandle := INVALID_HANDLE_VALUE;
  LStdinReadChild := INVALID_HANDLE_VALUE;
  LStdoutWriteChild := INVALID_HANDLE_VALUE;

  // Set up security attributes for inheritable handles
  LSA.nLength := SizeOf(TSecurityAttributes);
  LSA.bInheritHandle := True;
  LSA.lpSecurityDescriptor := nil;

  // Create pipe for child's stdin (parent writes, child reads)
  if not CreatePipe(LStdinReadChild, AStdinWrite, @LSA, 0) then
    Exit;

  // Create pipe for child's stdout (child writes, parent reads)
  if not CreatePipe(AStdoutRead, LStdoutWriteChild, @LSA, 0) then
  begin
    CloseHandle(LStdinReadChild);
    CloseHandle(AStdinWrite);
    AStdinWrite := INVALID_HANDLE_VALUE;
    Exit;
  end;

  // Ensure parent-side handles are NOT inherited by the child
  SetHandleInformation(AStdinWrite, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(AStdoutRead, HANDLE_FLAG_INHERIT, 0);

  // Set up startup info with redirected standard handles
  ZeroMemory(@LSI, SizeOf(LSI));
  LSI.cb := SizeOf(LSI);
  LSI.hStdInput := LStdinReadChild;
  LSI.hStdOutput := LStdoutWriteChild;
  LSI.hStdError := LStdoutWriteChild;
  LSI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  LSI.wShowWindow := SW_HIDE;

  // Build command line
  if AParams <> '' then
    LCmdLine := '"' + AExe + '" ' + AParams
  else
    LCmdLine := '"' + AExe + '"';
  UniqueString(LCmdLine);

  if AWorkDir <> '' then
    LWorkDirPW := PWideChar(AWorkDir)
  else
    LWorkDirPW := nil;

  ZeroMemory(@LPI, SizeOf(LPI));

  if not CreateProcessW(
    nil,
    PWideChar(LCmdLine),
    nil,
    nil,
    True,
    CREATE_UNICODE_ENVIRONMENT or CREATE_NO_WINDOW,
    nil,
    LWorkDirPW,
    LSI,
    LPI
  ) then
  begin
    CloseHandle(LStdinReadChild);
    CloseHandle(LStdoutWriteChild);
    CloseHandle(AStdinWrite);
    CloseHandle(AStdoutRead);
    AStdinWrite := INVALID_HANDLE_VALUE;
    AStdoutRead := INVALID_HANDLE_VALUE;
    Exit;
  end;

  // Close child-side pipe handles (child process has its own copies)
  CloseHandle(LStdinReadChild);
  CloseHandle(LStdoutWriteChild);

  AProcessHandle := LPI.hProcess;
  AThreadHandle := LPI.hThread;
  Result := True;
end;


class function TWin64Utils.GetZigExePath(): string;
var
  LBase: string;
begin
  LBase := TPath.GetDirectoryName(ParamStr(0));
  Result := TPath.Combine(
    LBase,
    TPath.Combine('res', TPath.Combine('zig', 'zig.exe'))
  );
end;


class function TWin64Utils.GetExePath(): string;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
end;


class function TWin64Utils.GetVersionInfo(out AVersionInfo: TVersionInfo; const AFilePath: string): Boolean;
var
  LFileName: string;
  LInfoSize: DWORD;
  LHandle: DWORD;
  LBuffer: Pointer;
  LFileInfo: PVSFixedFileInfo;
  LLen: UINT;
begin
  // Initialize output
  AVersionInfo.Major := 0;
  AVersionInfo.Minor := 0;
  AVersionInfo.Patch := 0;
  AVersionInfo.Build := 0;
  AVersionInfo.VersionString := '';

  // Determine which file to query
  if AFilePath = '' then
    LFileName := ParamStr(0)
  else
    LFileName := AFilePath;

  // Get version info size
  LInfoSize := GetFileVersionInfoSize(PChar(LFileName), LHandle);
  if LInfoSize = 0 then
    Exit(False);

  // Allocate buffer and get version info
  GetMem(LBuffer, LInfoSize);
  try
    if not GetFileVersionInfo(PChar(LFileName), LHandle, LInfoSize, LBuffer) then
      Exit(False);

    // Query fixed file info
    if not VerQueryValue(LBuffer, '\', Pointer(LFileInfo), LLen) then
      Exit(False);

    // Extract version components
    AVersionInfo.Major := HiWord(LFileInfo.dwFileVersionMS);
    AVersionInfo.Minor := LoWord(LFileInfo.dwFileVersionMS);
    AVersionInfo.Patch := HiWord(LFileInfo.dwFileVersionLS);
    AVersionInfo.Build := LoWord(LFileInfo.dwFileVersionLS);

    // Format version string (Major.Minor.Patch)
    AVersionInfo.VersionString := Format('%d.%d.%d', [AVersionInfo.Major, AVersionInfo.Minor, AVersionInfo.Patch]);
    
    Result := True;
  finally
    FreeMem(LBuffer);
  end;
end;


class function TWin64Utils.IsValidWin64PE(const AFilePath: string): Boolean;
var
  LFile: TFileStream;
  LDosHeader: TImageDosHeader;
  LPEHeaderOffset: DWORD;
  LPEHeaderSignature: DWORD;
  LFileHeader: TImageFileHeader;
begin
  Result := False;

  if not FileExists(AFilePath) then
    Exit;

  LFile := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // Check if file is large enough for DOS header
    if LFile.Size < SizeOf(TImageDosHeader) then
      Exit;

    // Read DOS header
    LFile.ReadBuffer(LDosHeader, SizeOf(TImageDosHeader));

    // Check DOS signature
    if LDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then // 'MZ'
      Exit;

      // Validate PE header offset
    LPEHeaderOffset := LDosHeader._lfanew;
    if LFile.Size < LPEHeaderOffset + SizeOf(DWORD) + SizeOf(TImageFileHeader) then
      Exit;

    // Seek to the PE header
    LFile.Position := LPEHeaderOffset;

    // Read and validate the PE signature
    LFile.ReadBuffer(LPEHeaderSignature, SizeOf(DWORD));
    if LPEHeaderSignature <> IMAGE_NT_SIGNATURE then // 'PE\0\0'
      Exit;

   // Read the file header
    LFile.ReadBuffer(LFileHeader, SizeOf(TImageFileHeader));

    // Check if it is a 64-bit executable
    if LFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then   Exit;

    // If all checks pass, it's a valid Win64 PE file
    Result := True;
  finally
    LFile.Free;
  end;
end;


class procedure TWin64Utils.UpdateIconResource(const AExeFilePath, AIconFilePath: string);
type
  TIconDir = packed record
    idReserved: Word;  // Reserved, must be 0
    idType: Word;      // Resource type, 1 for icons
    idCount: Word;     // Number of images in the file
  end;
  PIconDir = ^TIconDir;

  TGroupIconDirEntry = packed record
    bWidth: Byte;            // Width of the icon (0 means 256)
    bHeight: Byte;           // Height of the icon (0 means 256)
    bColorCount: Byte;       // Number of colors in the palette (0 if more than 256)
    bReserved: Byte;         // Reserved, must be 0
    wPlanes: Word;           // Color planes
    wBitCount: Word;         // Bits per pixel
    dwBytesInRes: Cardinal;  // Size of the image data
    nID: Word;               // Resource ID of the icon
  end;

  TGroupIconDir = packed record
    idReserved: Word;  // Reserved, must be 0
    idType: Word;      // Resource type, 1 for icons
    idCount: Word;     // Number of images in the file
    Entries: array[0..0] of TGroupIconDirEntry; // Variable-length array
  end;

  TIconResInfo = packed record
    bWidth: Byte;            // Width of the icon (0 means 256)
    bHeight: Byte;           // Height of the icon (0 means 256)
    bColorCount: Byte;       // Number of colors in the palette (0 if more than 256)
    bReserved: Byte;         // Reserved, must be 0
    wPlanes: Word;           // Color planes (should be 1)
    wBitCount: Word;         // Bits per pixel
    dwBytesInRes: Cardinal;  // Size of the image data
    dwImageOffset: Cardinal; // Offset of the image data in the file
  end;
  PIconResInfo = ^TIconResInfo;

var
  LUpdateHandle: THandle;
  LIconStream: TMemoryStream;
  LIconDir: PIconDir;
  LIconGroup: TMemoryStream;
  LIconRes: PByte;
  LIconID: Word;
  I: Integer;
  LGroupEntry: TGroupIconDirEntry;
begin

  if not FileExists(AExeFilePath) then
    raise Exception.Create('The specified executable file does not exist.');

  if not FileExists(AIconFilePath) then
    raise Exception.Create('The specified icon file does not exist.');

  LIconStream := TMemoryStream.Create;
  LIconGroup := TMemoryStream.Create;
  try
    // Load the icon file
    LIconStream.LoadFromFile(AIconFilePath);

    // Read the ICONDIR structure from the icon file
    LIconDir := PIconDir(LIconStream.Memory);
    if LIconDir^.idReserved <> 0 then
      raise Exception.Create('Invalid icon file format.');

    // Begin updating the executable's resources
    LUpdateHandle := BeginUpdateResource(PChar(AExeFilePath), False);
    if LUpdateHandle = 0 then
      raise Exception.Create('Failed to begin resource update.');

    try
      // Process each icon image in the .ico file
      LIconRes := PByte(LIconStream.Memory) + SizeOf(TIconDir);
      for I := 0 to LIconDir^.idCount - 1 do
      begin
        // Assign a unique resource ID for the RT_ICON
        LIconID := I + 1;

        // Add the icon image data as an RT_ICON resource
        if not UpdateResource(LUpdateHandle, RT_ICON, PChar(LIconID), LANG_NEUTRAL,
          Pointer(PByte(LIconStream.Memory) + PIconResInfo(LIconRes)^.dwImageOffset),
          PIconResInfo(LIconRes)^.dwBytesInRes) then
          raise Exception.CreateFmt('Failed to add RT_ICON resource for image %d.', [I]);

        // Move to the next icon entry
        Inc(LIconRes, SizeOf(TIconResInfo));
      end;

      // Create the GROUP_ICON resource
      LIconGroup.Clear;
      LIconGroup.Write(LIconDir^, SizeOf(TIconDir)); // Write ICONDIR header

      LIconRes := PByte(LIconStream.Memory) + SizeOf(TIconDir);
      // Write each GROUP_ICON entry
      for I := 0 to LIconDir^.idCount - 1 do
      begin
        // Populate the GROUP_ICON entry
        LGroupEntry.bWidth := PIconResInfo(LIconRes)^.bWidth;
        LGroupEntry.bHeight := PIconResInfo(LIconRes)^.bHeight;
        LGroupEntry.bColorCount := PIconResInfo(LIconRes)^.bColorCount;
        LGroupEntry.bReserved := 0;
        LGroupEntry.wPlanes := PIconResInfo(LIconRes)^.wPlanes;
        LGroupEntry.wBitCount := PIconResInfo(LIconRes)^.wBitCount;
        LGroupEntry.dwBytesInRes := PIconResInfo(LIconRes)^.dwBytesInRes;
        LGroupEntry.nID := I + 1; // Match resource ID for RT_ICON

        // Write the populated GROUP_ICON entry to the stream
        LIconGroup.Write(LGroupEntry, SizeOf(TGroupIconDirEntry));

        // Move to the next ICONDIRENTRY
        Inc(LIconRes, SizeOf(TIconResInfo));
      end;

      // Add the GROUP_ICON resource to the executable
      if not UpdateResource(LUpdateHandle, RT_GROUP_ICON, 'MAINICON', LANG_NEUTRAL,
        LIconGroup.Memory, LIconGroup.Size) then
        raise Exception.Create('Failed to add RT_GROUP_ICON resource.');

      // Commit the resource updates
      if not EndUpdateResource(LUpdateHandle, False) then
        raise Exception.Create('Failed to commit resource updates.');
    except
      EndUpdateResource(LUpdateHandle, True); // Discard changes on failure
      raise;
    end;
  finally
    LIconStream.Free;
    LIconGroup.Free;
  end;
end;


class procedure TWin64Utils.UpdateVersionInfoResource(const PEFilePath: string; const AMajor, AMinor, APatch: Word; const AProductName, ADescription, AFilename, ACompanyName, ACopyright: string);
type
  { TVSFixedFileInfo }
  TVSFixedFileInfo = packed record
    dwSignature: DWORD;        // e.g. $FEEF04BD
    dwStrucVersion: DWORD;     // e.g. $00010000 for version 1.0
    dwFileVersionMS: DWORD;    // e.g. $00030075 for version 3.75
    dwFileVersionLS: DWORD;    // e.g. $00000031 for version 0.31
    dwProductVersionMS: DWORD; // Same format as dwFileVersionMS
    dwProductVersionLS: DWORD; // Same format as dwFileVersionLS
    dwFileFlagsMask: DWORD;    // = $3F for version "0011 1111"
    dwFileFlags: DWORD;        // e.g. VFF_DEBUG | VFF_PRERELEASE
    dwFileOS: DWORD;           // e.g. VOS_NT_WINDOWS32
    dwFileType: DWORD;         // e.g. VFT_APP
    dwFileSubtype: DWORD;      // e.g. VFT2_UNKNOWN
    dwFileDateMS: DWORD;       // file date
    dwFileDateLS: DWORD;       // file date
  end;

  { TStringPair }
  TStringPair = record
    Key: string;
    Value: string;
  end;

var
  LHandleUpdate: THandle;
  LVersionInfoStream: TMemoryStream;
  LFixedInfo: TVSFixedFileInfo;
  LDataPtr: Pointer;
  LDataSize: Integer;
  LStringFileInfoStart, LStringTableStart, LVarFileInfoStart: Int64;
  LStringPairs: array of TStringPair;
  LVErsion: string;
  LMajor, LMinor,LPatch: Word;
  LVSVersionInfoStart: Int64;
  LPair: TStringPair;
  LStringInfoEnd, LStringStart: Int64;
  LStringEnd, LFinalPos: Int64;
  LTranslationStart: Int64;

  procedure AlignStream(const AStream: TMemoryStream; const AAlignment: Integer);
  var
    LPadding: Integer;
    LPadByte: Byte;
  begin
    LPadding := (AAlignment - (AStream.Position mod AAlignment)) mod AAlignment;
    LPadByte := 0;
    while LPadding > 0 do
    begin
      AStream.WriteBuffer(LPadByte, 1);
      Dec(LPadding);
    end;
  end;

  procedure WriteWideString(const AStream: TMemoryStream; const AText: string);
  var
    LWideText: WideString;
  begin
    LWideText := WideString(AText);
    AStream.WriteBuffer(PWideChar(LWideText)^, (Length(LWideText) + 1) * SizeOf(WideChar));
  end;

  procedure SetFileVersionFromString(const AVersion: string; out AFileVersionMS, AFileVersionLS: DWORD);
  var
    LVersionParts: TArray<string>;
    LMajor, LMinor, LBuild, LRevision: Word;
  begin
    // Split the version string into its components
    LVersionParts := AVersion.Split(['.']);
    if Length(LVersionParts) <> 4 then
      raise Exception.Create('Invalid version string format. Expected "Major.Minor.Build.Revision".');

    // Parse each part into a Word
    LMajor := StrToIntDef(LVersionParts[0], 0);
    LMinor := StrToIntDef(LVersionParts[1], 0);
    LBuild := StrToIntDef(LVersionParts[2], 0);
    LRevision := StrToIntDef(LVersionParts[3], 0);

    // Set the high and low DWORD values
    AFileVersionMS := (DWORD(LMajor) shl 16) or DWORD(LMinor);
    AFileVersionLS := (DWORD(LBuild) shl 16) or DWORD(LRevision);
  end;

begin
  LMajor := EnsureRange(AMajor, 0, MaxWord);
  LMinor := EnsureRange(AMinor, 0, MaxWord);
  LPatch := EnsureRange(APatch, 0, MaxWord);
  LVersion := Format('%d.%d.%d.0', [LMajor, LMinor, LPatch]);

  SetLength(LStringPairs, 8);
  LStringPairs[0].Key := 'CompanyName';
  LStringPairs[0].Value := ACompanyName;
  LStringPairs[1].Key := 'FileDescription';
  LStringPairs[1].Value := ADescription;
  LStringPairs[2].Key := 'FileVersion';
  LStringPairs[2].Value := LVersion;
  LStringPairs[3].Key := 'InternalName';
  LStringPairs[3].Value := ADescription;
  LStringPairs[4].Key := 'LegalCopyright';
  LStringPairs[4].Value := ACopyright;
  LStringPairs[5].Key := 'OriginalFilename';
  LStringPairs[5].Value := AFilename;
  LStringPairs[6].Key := 'ProductName';
  LStringPairs[6].Value := AProductName;
  LStringPairs[7].Key := 'ProductVersion';
  LStringPairs[7].Value := LVersion;

  // Initialize fixed info structure
  FillChar(LFixedInfo, SizeOf(LFixedInfo), 0);
  LFixedInfo.dwSignature := $FEEF04BD;
  LFixedInfo.dwStrucVersion := $00010000;
  LFixedInfo.dwFileVersionMS := $00010000;
  LFixedInfo.dwFileVersionLS := $00000000;
  LFixedInfo.dwProductVersionMS := $00010000;
  LFixedInfo.dwProductVersionLS := $00000000;
  LFixedInfo.dwFileFlagsMask := $3F;
  LFixedInfo.dwFileFlags := 0;
  LFixedInfo.dwFileOS := VOS_NT_WINDOWS32;
  LFixedInfo.dwFileType := VFT_APP;
  LFixedInfo.dwFileSubtype := 0;
  LFixedInfo.dwFileDateMS := 0;
  LFixedInfo.dwFileDateLS := 0;

  // SEt MS and LS for FileVersion and ProductVersion
  SetFileVersionFromString(LVersion, LFixedInfo.dwFileVersionMS, LFixedInfo.dwFileVersionLS);
  SetFileVersionFromString(LVersion, LFixedInfo.dwProductVersionMS, LFixedInfo.dwProductVersionLS);

  LVersionInfoStream := TMemoryStream.Create;
  try
    // VS_VERSION_INFO
    LVSVersionInfoStart := LVersionInfoStream.Position;

    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(SizeOf(TVSFixedFileInfo));  // Value length
    LVersionInfoStream.WriteData<Word>(0);  // Type = 0
    WriteWideString(LVersionInfoStream, 'VS_VERSION_INFO');
    AlignStream(LVersionInfoStream, 4);

    // VS_FIXEDFILEINFO
    LVersionInfoStream.WriteBuffer(LFixedInfo, SizeOf(TVSFixedFileInfo));
    AlignStream(LVersionInfoStream, 4);

    // StringFileInfo
    LStringFileInfoStart := LVersionInfoStream.Position;
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(0);  // Value length = 0
    LVersionInfoStream.WriteData<Word>(1);  // Type = 1
    WriteWideString(LVersionInfoStream, 'StringFileInfo');
    AlignStream(LVersionInfoStream, 4);

    // StringTable
    LStringTableStart := LVersionInfoStream.Position;
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(0);  // Value length = 0
    LVersionInfoStream.WriteData<Word>(1);  // Type = 1
    WriteWideString(LVersionInfoStream, '040904B0'); // Match Delphi's default code page
    AlignStream(LVersionInfoStream, 4);

    // Write string pairs
    for LPair in LStringPairs do
    begin
      LStringStart := LVersionInfoStream.Position;

      LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
      LVersionInfoStream.WriteData<Word>((Length(LPair.Value) + 1) * 2);  // Value length
      LVersionInfoStream.WriteData<Word>(1);  // Type = 1
      WriteWideString(LVersionInfoStream, LPair.Key);
      AlignStream(LVersionInfoStream, 4);
      WriteWideString(LVersionInfoStream, LPair.Value);
      AlignStream(LVersionInfoStream, 4);

      LStringEnd := LVersionInfoStream.Position;
      LVersionInfoStream.Position := LStringStart;
      LVersionInfoStream.WriteData<Word>(LStringEnd - LStringStart);
      LVersionInfoStream.Position := LStringEnd;
    end;

    LStringInfoEnd := LVersionInfoStream.Position;

    // Write StringTable length
    LVersionInfoStream.Position := LStringTableStart;
    LVersionInfoStream.WriteData<Word>(LStringInfoEnd - LStringTableStart);

    // Write StringFileInfo length
    LVersionInfoStream.Position := LStringFileInfoStart;
    LVersionInfoStream.WriteData<Word>(LStringInfoEnd - LStringFileInfoStart);

    // Start VarFileInfo where StringFileInfo ended
    LVarFileInfoStart := LStringInfoEnd;
    LVersionInfoStream.Position := LVarFileInfoStart;

    // VarFileInfo header
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(0);  // Value length = 0
    LVersionInfoStream.WriteData<Word>(1);  // Type = 1 (text)
    WriteWideString(LVersionInfoStream, 'VarFileInfo');
    AlignStream(LVersionInfoStream, 4);

    // Translation value block
    LTranslationStart := LVersionInfoStream.Position;
    LVersionInfoStream.WriteData<Word>(0);  // Length placeholder
    LVersionInfoStream.WriteData<Word>(4);  // Value length = 4 (size of translation value)
    LVersionInfoStream.WriteData<Word>(0);  // Type = 0 (binary)
    WriteWideString(LVersionInfoStream, 'Translation');
    AlignStream(LVersionInfoStream, 4);

    // Write translation value
    LVersionInfoStream.WriteData<Word>($0409);  // Language ID (US English)
    LVersionInfoStream.WriteData<Word>($04B0);  // Unicode code page

    LFinalPos := LVersionInfoStream.Position;

    // Update VarFileInfo block length
    LVersionInfoStream.Position := LVarFileInfoStart;
    LVersionInfoStream.WriteData<Word>(LFinalPos - LVarFileInfoStart);

    // Update translation block length
    LVersionInfoStream.Position := LTranslationStart;
    LVersionInfoStream.WriteData<Word>(LFinalPos - LTranslationStart);

    // Update total version info length
    LVersionInfoStream.Position := LVSVersionInfoStart;
    LVersionInfoStream.WriteData<Word>(LFinalPos);

    LDataPtr := LVersionInfoStream.Memory;
    LDataSize := LVersionInfoStream.Size;

    // Update the resource
    LHandleUpdate := BeginUpdateResource(PChar(PEFilePath), False);
    if LHandleUpdate = 0 then
      RaiseLastOSError;

    try
      if not UpdateResourceW(LHandleUpdate, RT_VERSION, MAKEINTRESOURCE(1),
         MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), LDataPtr, LDataSize) then
        RaiseLastOSError;

      if not EndUpdateResource(LHandleUpdate, False) then
        RaiseLastOSError;
    except
      EndUpdateResource(LHandleUpdate, True);
      raise;
    end;
  finally
    LVersionInfoStream.Free;
  end;
end;


class function  TWin64Utils.ResourceExist(const AResName: string): Boolean;
begin
  Result := Boolean((FindResource(HInstance, PChar(AResName), RT_RCDATA) <> 0));
end;

class function TWin64Utils.AddResManifestFromResource(const aResName: string; const aModuleFile: string; aLanguage: Integer): Boolean;
var
  LHandle: THandle;
  LManifestStream: TResourceStream;
begin
  Result := False;

  if not ResourceExist(aResName) then Exit;
  if not TFile.Exists(aModuleFile) then Exit;

  LManifestStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    LHandle := WinAPI.Windows.BeginUpdateResourceW(System.PWideChar(aModuleFile), LongBool(False));

    if LHandle <> 0 then
    begin
      Result := WinAPI.Windows.UpdateResourceW(LHandle, RT_MANIFEST, CREATEPROCESS_MANIFEST_RESOURCE_ID, aLanguage, LManifestStream.Memory, LManifestStream.Size);
      WinAPI.Windows.EndUpdateResourceW(LHandle, False);
    end;
  finally
    FreeAndNil(LManifestStream);
  end;
end;


class procedure TWin64Utils.SetEnv(const AName: string; const AValue: string);
begin
  SetEnvironmentVariable(PChar(AName), PChar(AValue));
end;


class function TWin64Utils.RunFromIDE(): Boolean;
begin
  Result := TUtils.HasEnv('BDS');
end;


{ TCommandBuilder }

constructor TCommandBuilder.Create();
begin
  inherited;
  
  FParams := TStringList.Create();
  FParams.Delimiter := ' ';
  FParams.StrictDelimiter := True;
end;

destructor TCommandBuilder.Destroy();
begin
  FreeAndNil(FParams);
  
  inherited;
end;

procedure TCommandBuilder.Clear();
begin
  FParams.Clear();
end;

procedure TCommandBuilder.AddParam(const AParam: string);
begin
  if AParam <> '' then
    FParams.Add(AParam);
end;

procedure TCommandBuilder.AddParam(const AFlag, AValue: string);
begin
  if AFlag <> '' then
  begin
    if AValue <> '' then
      FParams.Add(AFlag + AValue)
    else
      FParams.Add(AFlag);
  end
  else if AValue <> '' then
    FParams.Add(AValue);
end;

procedure TCommandBuilder.AddQuotedParam(const AFlag, AValue: string);
begin
  if AValue = '' then
    Exit;
  
  if AFlag <> '' then
    FParams.Add(AFlag + ' "' + AValue + '"')
  else
    FParams.Add('"' + AValue + '"');
end;

procedure TCommandBuilder.AddQuotedParam(const AValue: string);
begin
  AddQuotedParam('', AValue);
end;

procedure TCommandBuilder.AddFlag(const AFlag: string);
begin
  if AFlag <> '' then
    FParams.Add(AFlag);
end;

function TCommandBuilder.ToString(): string;
var
  LI: Integer;
begin
  if FParams.Count = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  // Manually join with spaces to avoid TStringList.DelimitedText auto-quoting
  Result := FParams[0];
  for LI := 1 to FParams.Count - 1 do
    Result := Result + ' ' + FParams[LI];
end;

function TCommandBuilder.GetParamCount(): Integer;
begin
  Result := FParams.Count;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  TWin64Utils.InitConsole();

finalization


end.
