{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Utils.Posix;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Tiger.Utils;

type
  { TPosixUtils - Console and process helpers for Linux / macOS hosts. }
  TPosixUtils = class
  public
    class function  GetTickCount(): Cardinal; static;
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

    class function  RunExe(const AExe, AParams, AWorkDir: string; const AWait: Boolean = True; const AShowCmd: Word = 0): Cardinal; static;
    class function  RunElf(const AElf, AWorkDir: string): Cardinal; static;

    class function  GetVersionInfo(out AVersionInfo: TVersionInfo; const AFilePath: string = ''): Boolean; static;
    class function  GetZigExePath(): string; static;
    class function  GetExePath(): string; static;

    class function  IsValidWin64PE(const AFilePath: string): Boolean; static;

    class procedure SetEnv(const AName: string; const AValue: string); static;
    class function  RunFromIDE(): Boolean; static;
  end;

implementation

{$IFDEF LINUX}
function libc_system(cmd: PAnsiChar): Integer; cdecl; external 'libc.so.6' name 'system';
function libc_setenv(const name: PAnsiChar; const value: PAnsiChar; overwrite: Integer): Integer; cdecl; external 'libc.so.6' name 'setenv';
{$ELSE}
function libc_system(cmd: PAnsiChar): Integer; cdecl; external '/usr/lib/libSystem.dylib' name 'system';
function libc_setenv(const name: PAnsiChar; const value: PAnsiChar; overwrite: Integer): Integer; cdecl; external '/usr/lib/libSystem.dylib' name 'setenv';
{$ENDIF}

function ExitCodeFromWaitStatus(const Status: Integer): Cardinal;
begin
  if Status = -1 then
    raise Exception.Create('system() failed');
  if (Status and $7F) = 0 then
    Result := Cardinal((Status shr 8) and $FF)
  else
    Result := Cardinal(Status and $7F);
end;

function QuoteSh(const S: string): string;
begin
  Result := '''' + StringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
end;

{ TPosixUtils }

class function TPosixUtils.GetTickCount(): Cardinal;
begin
  Result := Cardinal(TThread.GetTickCount64() and $FFFFFFFF);
end;

class function TPosixUtils.GetTickCount64(): UInt64;
begin
  Result := TThread.GetTickCount64();
end;

class function TPosixUtils.CallI64(AFunction: Pointer; const AArgs: array of const): UInt64;
begin
  raise Exception.Create('CallI64 is not implemented for this host (Windows x64 ABI only).');
end;

class function TPosixUtils.CallF32(AFunction: Pointer; const AArgs: array of const): Single;
begin
  raise Exception.Create('CallF32 is not implemented for this host (Windows x64 ABI only).');
end;

class function TPosixUtils.CallF64(AFunction: Pointer; const AArgs: array of const): Double;
begin
  raise Exception.Create('CallF64 is not implemented for this host (Windows x64 ABI only).');
end;

class function TPosixUtils.HasConsole(): Boolean;
begin
  Result := True;
end;

class procedure TPosixUtils.ClearToEOL();
begin
  Write(#27'[0K');
end;

class procedure TPosixUtils.Print();
begin
  Print('');
end;

class procedure TPosixUtils.PrintLn();
begin
  PrintLn('');
end;

class procedure TPosixUtils.Print(const AText: string);
begin
  Write(AText);
end;

class procedure TPosixUtils.Print(const AText: string; const AArgs: array of const);
begin
  Write(Format(AText, AArgs));
end;

class procedure TPosixUtils.PrintLn(const AText: string);
begin
  WriteLn(AText);
end;

class procedure TPosixUtils.PrintLn(const AText: string; const AArgs: array of const);
begin
  WriteLn(Format(AText, AArgs));
end;

class function TPosixUtils.Pause(const AMsg, AQuit: string): Boolean;
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

class procedure TPosixUtils.ProcessMessages();
begin
end;

class function TPosixUtils.RunExe(const AExe, AParams, AWorkDir: string; const AWait: Boolean; const AShowCmd: Word): Cardinal;
var
  LAppPath: string;
  LCmd: AnsiString;
  LStatus: Integer;
begin
  if AExe = '' then
    raise Exception.Create('RunExe: Executable path is empty');

  if TPath.IsPathRooted(AExe) or (Pos(PathDelim, AExe) > 0) then
    LAppPath := AExe
  else if AWorkDir <> '' then
    LAppPath := TPath.Combine(AWorkDir, AExe)
  else
    LAppPath := AExe;

  if (AWorkDir <> '') and (not TFile.Exists(LAppPath)) then
    raise Exception.CreateFmt('RunExe: Executable not found: %s', [LAppPath]);

  if AWorkDir <> '' then
  begin
    if AParams <> '' then
      LCmd := AnsiString(Format('cd %s && %s %s', [QuoteSh(AWorkDir), QuoteSh(LAppPath), AParams]))
    else
      LCmd := AnsiString(Format('cd %s && %s', [QuoteSh(AWorkDir), QuoteSh(LAppPath)]));
  end
  else
  begin
    if AParams <> '' then
      LCmd := AnsiString(Format('%s %s', [QuoteSh(LAppPath), AParams]))
    else
      LCmd := AnsiString(QuoteSh(LAppPath));
  end;

  if not AWait then
    raise Exception.Create('RunExe: asynchronous execution is not implemented on POSIX');

  LStatus := libc_system(PAnsiChar(LCmd));
  Result := ExitCodeFromWaitStatus(LStatus);
end;

class function TPosixUtils.RunElf(const AElf, AWorkDir: string): Cardinal;
var
  LFullPath: string;
  LChmod: AnsiString;
  LRun: AnsiString;
  LStatus: Integer;
begin
  if AElf = '' then
    raise Exception.Create('RunElf: path is empty');

  LFullPath := TPath.GetFullPath(AElf);
  if not TFile.Exists(LFullPath) then
    raise Exception.CreateFmt('RunElf: file not found: %s', [LFullPath]);

  LChmod := AnsiString(Format('chmod +x %s', [QuoteSh(LFullPath)]));
  LStatus := libc_system(PAnsiChar(LChmod));
  ExitCodeFromWaitStatus(LStatus); // ignore chmod exit if already executable

  if AWorkDir <> '' then
    LRun := AnsiString(Format('cd %s && %s', [QuoteSh(AWorkDir), QuoteSh(LFullPath)]))
  else
    LRun := AnsiString(QuoteSh(LFullPath));

  LStatus := libc_system(PAnsiChar(LRun));
  Result := ExitCodeFromWaitStatus(LStatus);
end;

class function TPosixUtils.GetVersionInfo(out AVersionInfo: TVersionInfo; const AFilePath: string): Boolean;
begin
  AVersionInfo.Major := 0;
  AVersionInfo.Minor := 0;
  AVersionInfo.Patch := 0;
  AVersionInfo.Build := 0;
  AVersionInfo.VersionString := '';
  Result := False;
end;

class function TPosixUtils.GetZigExePath(): string;
var
  LBase: string;
begin
  LBase := TPath.GetDirectoryName(ParamStr(0));
  Result := TPath.Combine(
    LBase,
    TPath.Combine('res', TPath.Combine('zig', 'zig'))
  );
end;

class function TPosixUtils.GetExePath(): string;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
end;

class function TPosixUtils.IsValidWin64PE(const AFilePath: string): Boolean;
begin
  Result := False;
end;

class procedure TPosixUtils.SetEnv(const AName: string; const AValue: string);
begin
  libc_setenv(PAnsiChar(AnsiString(AName)), PAnsiChar(AnsiString(AValue)), 1);
end;

class function TPosixUtils.RunFromIDE(): Boolean;
begin
  Result := TUtils.HasEnv('BDS');
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
