{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.JIT.Win64;

{$I Tiger.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Generics.Collections,
  Tiger.JIT;

type
  { TTigerJITWin64 }
  /// <summary>
  ///   Windows x64 implementation of JIT compilation support.
  /// </summary>
  /// <remarks>
  ///   Uses VirtualAlloc for executable memory and LoadLibrary/GetProcAddress
  ///   for dynamic symbol resolution.
  /// </remarks>
  TTigerJITWin64 = class(TTigerJIT)
  private
    FLoadedLibraries: TDictionary<string, HMODULE>;

  protected
    procedure AllocateExecutable(const ASize: NativeUInt); override;
    procedure FreeExecutable(); override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    function ResolveImport(const ALibrary: string; const ASymbol: string): Pointer; override;
  end;

implementation

//==============================================================================
// TTigerJITWin64
//==============================================================================

constructor TTigerJITWin64.Create();
begin
  inherited Create();
  FLoadedLibraries := TDictionary<string, HMODULE>.Create();
end;

destructor TTigerJITWin64.Destroy();
var
  LHandle: HMODULE;
begin
  // Free loaded libraries
  for LHandle in FLoadedLibraries.Values do
    FreeLibrary(LHandle);
  FLoadedLibraries.Free();

  inherited Destroy();
end;

procedure TTigerJITWin64.AllocateExecutable(const ASize: NativeUInt);
begin
  FCodeBase := VirtualAlloc(nil, ASize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  if FCodeBase = nil then
    raise Exception.CreateFmt('Failed to allocate executable memory: %d bytes (error %d)',
      [ASize, GetLastError()]);
  FCodeSize := ASize;
end;

procedure TTigerJITWin64.FreeExecutable();
begin
  if FCodeBase <> nil then
  begin
    VirtualFree(FCodeBase, 0, MEM_RELEASE);
    FCodeBase := nil;
    FCodeSize := 0;
  end;
end;

function TTigerJITWin64.ResolveImport(const ALibrary: string; const ASymbol: string): Pointer;
var
  LHandle: HMODULE;
  LKey: string;
begin
  LKey := ALibrary.ToLower();

  if not FLoadedLibraries.TryGetValue(LKey, LHandle) then
  begin
    LHandle := LoadLibraryW(PWideChar(ALibrary));
    if LHandle = 0 then
      raise Exception.CreateFmt('Failed to load library: %s (error %d)',
        [ALibrary, GetLastError()]);
    FLoadedLibraries.Add(LKey, LHandle);
  end;

  Result := GetProcAddress(LHandle, PAnsiChar(AnsiString(ASymbol)));
  if Result = nil then
    raise Exception.CreateFmt('Failed to find symbol: %s in %s (error %d)',
      [ASymbol, ALibrary, GetLastError()]);
end;

end.
