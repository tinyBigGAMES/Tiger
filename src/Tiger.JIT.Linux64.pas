{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.JIT.Linux64;

{$IFDEF LINUX}
  {$I Tiger.Defines.inc}
{$ENDIF}

interface

{$IFDEF LINUX}
uses
  System.SysUtils,
  System.Generics.Collections,
  Tiger.JIT;

type
  { TTigerJITLinux64 }
  /// <summary>
  ///   Linux x64 implementation of JIT compilation support.
  /// </summary>
  /// <remarks>
  ///   Uses mmap for executable memory and dlopen/dlsym for dynamic symbol resolution.
  /// </remarks>
  TTigerJITLinux64 = class(TTigerJIT)
  private
    FLoadedLibraries: TDictionary<string, NativeUInt>;

  protected
    procedure AllocateExecutable(const ASize: NativeUInt); override;
    procedure FreeExecutable(); override;
    function ResolveImport(const ALibrary: string; const ASymbol: string): Pointer; override;

  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;
{$ENDIF}

implementation

{$IFDEF LINUX}
const
  PROT_READ     = $1;
  PROT_WRITE    = $2;
  PROT_EXEC     = $4;
  MAP_PRIVATE   = $02;
  MAP_ANONYMOUS = $20;
  MAP_FAILED    = Pointer(-1);
  RTLD_NOW      = $2;

function mmap(
  AAddr: Pointer;
  ALength: NativeUInt;
  AProt: Integer;
  AFlags: Integer;
  AFd: Integer;
  AOffset: Int64
): Pointer; cdecl; external 'libc.so.6' name 'mmap';

function munmap(
  AAddr: Pointer;
  ALength: NativeUInt
): Integer; cdecl; external 'libc.so.6' name 'munmap';

function dlopen(
  AFilename: PAnsiChar;
  AFlags: Integer
): NativeUInt; cdecl; external 'libdl.so.2' name 'dlopen';

function dlsym(
  AHandle: NativeUInt;
  ASymbol: PAnsiChar
): Pointer; cdecl; external 'libdl.so.2' name 'dlsym';

function dlclose(
  AHandle: NativeUInt
): Integer; cdecl; external 'libdl.so.2' name 'dlclose';

function dlerror(): PAnsiChar; cdecl; external 'libdl.so.2' name 'dlerror';

//==============================================================================
// TTigerJITLinux64
//==============================================================================

constructor TTigerJITLinux64.Create();
begin
  inherited Create();
  FLoadedLibraries := TDictionary<string, NativeUInt>.Create();
end;

destructor TTigerJITLinux64.Destroy();
var
  LHandle: NativeUInt;
begin
  // Free loaded libraries
  for LHandle in FLoadedLibraries.Values do
    dlclose(LHandle);
  FLoadedLibraries.Free();

  inherited Destroy();
end;

procedure TTigerJITLinux64.AllocateExecutable(const ASize: NativeUInt);
begin
  FCodeBase := mmap(nil, ASize, PROT_READ or PROT_WRITE or PROT_EXEC,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if FCodeBase = MAP_FAILED then
    raise Exception.CreateFmt('Failed to allocate executable memory: %d bytes', [ASize]);
  FCodeSize := ASize;
end;

procedure TTigerJITLinux64.FreeExecutable();
begin
  if (FCodeBase <> nil) and (FCodeBase <> MAP_FAILED) then
  begin
    munmap(FCodeBase, FCodeSize);
    FCodeBase := nil;
    FCodeSize := 0;
  end;
end;

function TTigerJITLinux64.ResolveImport(const ALibrary: string; const ASymbol: string): Pointer;
var
  LHandle: NativeUInt;
  LKey: string;
  LError: PAnsiChar;
begin
  LKey := ALibrary.ToLower();

  if not FLoadedLibraries.TryGetValue(LKey, LHandle) then
  begin
    LHandle := dlopen(PAnsiChar(AnsiString(ALibrary)), RTLD_NOW);
    if LHandle = 0 then
    begin
      LError := dlerror();
      raise Exception.CreateFmt('Failed to load library: %s (%s)',
        [ALibrary, string(AnsiString(LError))]);
    end;
    FLoadedLibraries.Add(LKey, LHandle);
  end;

  Result := dlsym(LHandle, PAnsiChar(AnsiString(ASymbol)));
  if Result = nil then
  begin
    LError := dlerror();
    raise Exception.CreateFmt('Failed to find symbol: %s in %s (%s)',
      [ASymbol, ALibrary, string(AnsiString(LError))]);
  end;
end;
{$ENDIF}

end.
