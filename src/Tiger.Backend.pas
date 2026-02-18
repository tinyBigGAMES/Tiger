{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend;

{$I Tiger.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Utils.Win64,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.JIT;

type
  { TJITCodeGenData - data returned by code generation for JIT compilation }
  TJITCodeGenData = record
    TextSection: TMemoryStream;                      // Code bytes
    FuncOffsets: TArray<Cardinal>;                   // Function index -> code offset
    ImportFixups: TList<TPair<Cardinal, Integer>>;   // Code offset -> Import index
    CallFixups: TList<TPair<Cardinal, Integer>>;     // Code offset -> Func index
    DataFixups: TList<TPair<Cardinal, Integer>>;     // Code offset -> Data index (.rdata)
    GlobalFixups: TList<TPair<Cardinal, Integer>>;   // Code offset -> Global index (.data)
    FuncAddrFixups: TList<TPair<Cardinal, Integer>>; // Code offset -> Func index (LEA)
  end;
  PJITCodeGenData = ^TJITCodeGenData;

  { TTigerBackend }
  TTigerBackend = class(TTigerStatusObject)
  protected
    FOutputPath: string;
    FOutputType: TTigerOutputType;
    FSubsystem: TTigerSubsystem;
    FData: TTigerDataBuilder;
    FGlobals: TTigerDataBuilder;
    FImports: TTigerImportBuilder;
    FExports: TTigerExportBuilder;
    FCode: TTigerCodeBuilder;
    FSSADump: string;
    FDumpIR: Boolean;
    FOptimizationLevel: Integer;
    FLinkedLibs: TStringList;
    FLinkedObjs: TStringList;
    FLibPaths: TStringList;

    procedure PreBuild(); virtual;
    procedure PostBuild(); virtual;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Target Configuration
    //--------------------------------------------------------------------------
    function TargetExe(const APath: string; const ASubsystem: TTigerSubsystem = ssConsole): TTigerBackend; virtual;
    function TargetDll(const APath: string): TTigerBackend;
    function TargetLib(const APath: string): TTigerBackend;
    function TargetObj(const APath: string): TTigerBackend;

    //--------------------------------------------------------------------------
    // Static Linking
    //--------------------------------------------------------------------------
    function AddLib(const APath: string): TTigerBackend;
    function AddObj(const APath: string): TTigerBackend;
    function AddLibPath(const APath: string): TTigerBackend;

    //--------------------------------------------------------------------------
    // Build
    //--------------------------------------------------------------------------
    function Build(): Boolean;
    function BuildToMemory(): TBytes; virtual;
    function BuildJIT(): TTigerJIT; virtual; abstract;
    function Run(): Cardinal; virtual;

    //--------------------------------------------------------------------------
    // Reset
    //--------------------------------------------------------------------------
    procedure Clear(); virtual;

    //--------------------------------------------------------------------------
    // Error Handling (override to propagate to sub-builders)
    //--------------------------------------------------------------------------
    procedure SetErrors(const AErrors: TErrors); override;

    //--------------------------------------------------------------------------
    // Getters/Setters
    //--------------------------------------------------------------------------
    function GetOutputPath(): string;
    function GetOutputType(): TTigerOutputType;
    function GetSubsystem(): TTigerSubsystem;
    function GetData(): TTigerDataBuilder;
    function GetGlobals(): TTigerDataBuilder;
    function GetImports(): TTigerImportBuilder;
    function GetExports(): TTigerExportBuilder;
    function GetCode(): TTigerCodeBuilder;
    function GetSSADump(): string;
    procedure SetSSADump(const AValue: string);
    function GetDumpIR(): Boolean;
    procedure SetDumpIR(const AValue: Boolean);
    function GetOptimizationLevel(): Integer;
    procedure SetOptimizationLevel(const AValue: Integer);
  end;

//------------------------------------------------------------------------------
// Platform-specific code emission helpers
//------------------------------------------------------------------------------

/// <summary>
///   Emits an inline stack probe loop for x86-64 targets. When a function's
///   stack frame exceeds 4096 bytes (one memory page), the OS guard page
///   mechanism requires each page to be touched sequentially before allocation.
///   Without probing, SUB RSP past the guard page causes an access violation
///   (Windows) or segfault (Linux).
/// </summary>
/// <param name="ATextSection">The code stream to emit into.</param>
/// <param name="AFrameSize">The total stack frame size in bytes (must be > 4096).</param>
procedure EmitStackProbe_x64(const ATextSection: TMemoryStream; const AFrameSize: Cardinal);

implementation

const
  // Backend error codes
  ERR_BACKEND_NO_ACTIVE_FUNC = 'B001';


constructor TTigerBackend.Create();
begin
  inherited;

  FData := TTigerDataBuilder.Create();
  FGlobals := TTigerDataBuilder.Create();
  FImports := TTigerImportBuilder.Create();
  FCode := TTigerCodeBuilder.Create();
  FExports := TTigerExportBuilder.Create(FCode);

  FLinkedLibs := TStringList.Create();
  FLinkedObjs := TStringList.Create();
  FLibPaths := TStringList.Create();
  FLibPaths.CaseSensitive := False;
  FLibPaths.Duplicates := dupIgnore;

  FOutputPath := '';
  FOutputType := otExe;
  FSubsystem := ssConsole;
end;

destructor TTigerBackend.Destroy();
begin
  FLibPaths.Free();
  FLinkedObjs.Free();
  FLinkedLibs.Free();
  FExports.Free();
  FCode.Free();
  FImports.Free();
  FGlobals.Free();
  FData.Free();

  inherited;
end;

function TTigerBackend.TargetExe(const APath: string; const ASubsystem: TTigerSubsystem): TTigerBackend;
begin
  FOutputPath := APath;
  FOutputType := otExe;
  FSubsystem := ASubsystem;
  Result := Self;
end;

function TTigerBackend.TargetDll(const APath: string): TTigerBackend;
begin
  FOutputPath := APath;
  FOutputType := otDll;
  FSubsystem := ssConsole;
  Result := Self;
end;

function TTigerBackend.TargetLib(const APath: string): TTigerBackend;
begin
  FOutputPath := APath;
  FOutputType := otLib;
  FSubsystem := ssConsole;
  Result := Self;
end;

function TTigerBackend.TargetObj(const APath: string): TTigerBackend;
begin
  FOutputPath := APath;
  FOutputType := otObj;
  FSubsystem := ssConsole;
  Result := Self;
end;

function TTigerBackend.AddLib(const APath: string): TTigerBackend;
begin
  if FLinkedLibs.IndexOf(APath) < 0 then
    FLinkedLibs.Add(APath);
  Result := Self;
end;

function TTigerBackend.AddObj(const APath: string): TTigerBackend;
begin
  if FLinkedObjs.IndexOf(APath) < 0 then
    FLinkedObjs.Add(APath);
  Result := Self;
end;

function TTigerBackend.AddLibPath(const APath: string): TTigerBackend;
begin
  if FLibPaths.IndexOf(APath) < 0 then
    FLibPaths.Add(APath);
  Result := Self;
end;

procedure TTigerBackend.PreBuild();
begin
  // Default: ensure output directory exists
  TUtils.CreateDirInPath(FOutputPath);
end;

procedure TTigerBackend.PostBuild();
begin
  // Default: no-op — override for signing, permissions, etc.
end;

function TTigerBackend.Build(): Boolean;
var
  LData: TBytes;
  LStream: TFileStream;
begin
  Result := False;

  if FOutputPath = '' then
  begin
    Status('Error: Output path not specified');
    Exit;
  end;

  // Pre-build hook (directory creation, path fixups, etc.)
  PreBuild();

  LData := BuildToMemory();
  if Length(LData) = 0 then
  begin
    Status('Error: Build failed - no output generated');
    Exit;
  end;

  try
    LStream := TFileStream.Create(FOutputPath, fmCreate);
    try
      LStream.WriteBuffer(LData[0], Length(LData));
    finally
      LStream.Free();
    end;
    Result := True;
    Status('Build successful: %s', [FOutputPath.Replace('\', '/')]);

    // Post-build hook (signing, permissions, etc.)
    PostBuild();

  except
    on E: Exception do
      Status('Error writing output: %s', [E.Message]);
  end;
end;

function TTigerBackend.Run(): Cardinal;
begin
  Result := ERROR_BAD_FORMAT;
end;

function TTigerBackend.BuildToMemory(): TBytes;
begin
  SetLength(Result, 0);
end;

procedure TTigerBackend.Clear();
begin
  FData.Clear();
  FGlobals.Clear();
  FImports.Clear();
  FExports.Clear();
  FCode.Clear();
  FLinkedLibs.Clear();
  FLinkedObjs.Clear();
  FLibPaths.Clear();
  FOutputPath := '';
  FOutputType := otExe;
  FSubsystem := ssConsole;
end;

procedure TTigerBackend.SetErrors(const AErrors: TErrors);
begin
  inherited SetErrors(AErrors);
  
  // Propagate to sub-builders
  if Assigned(FCode) then
    FCode.SetErrors(AErrors);
end;

function TTigerBackend.GetOutputPath(): string;
begin
  Result := FOutputPath;
end;

function TTigerBackend.GetOutputType(): TTigerOutputType;
begin
  Result := FOutputType;
end;

function TTigerBackend.GetSubsystem(): TTigerSubsystem;
begin
  Result := FSubsystem;
end;

function TTigerBackend.GetData(): TTigerDataBuilder;
begin
  Result := FData;
end;

function TTigerBackend.GetGlobals(): TTigerDataBuilder;
begin
  Result := FGlobals;
end;

function TTigerBackend.GetImports(): TTigerImportBuilder;
begin
  Result := FImports;
end;

function TTigerBackend.GetExports(): TTigerExportBuilder;
begin
  Result := FExports;
end;

function TTigerBackend.GetCode(): TTigerCodeBuilder;
begin
  Result := FCode;
end;

function TTigerBackend.GetSSADump(): string;
begin
  Result := FSSADump;
end;

procedure TTigerBackend.SetSSADump(const AValue: string);
begin
  FSSADump := AValue;
end;

function TTigerBackend.GetDumpIR(): Boolean;
begin
  Result := FDumpIR;
end;

procedure TTigerBackend.SetDumpIR(const AValue: Boolean);
begin
  FDumpIR := AValue;
end;

function TTigerBackend.GetOptimizationLevel(): Integer;
begin
  Result := FOptimizationLevel;
end;

procedure TTigerBackend.SetOptimizationLevel(const AValue: Integer);
begin
  FOptimizationLevel := AValue;
end;

//==============================================================================
// Platform-specific code emission helpers
//==============================================================================

procedure EmitStackProbe_x64(const ATextSection: TMemoryStream; const AFrameSize: Cardinal);
var
  LProbeLoopTop: Int64;
begin
  // MOV EAX, AFrameSize (B8 imm32) — load total frame size as loop counter
  ATextSection.WriteData(Byte($B8));
  ATextSection.WriteData(Int32(AFrameSize));

  // loop_top: — probe loop touches each 4K page sequentially
  LProbeLoopTop := ATextSection.Size;

  // SUB RSP, 4096 (REX.W 81 /5 imm32) — move RSP down one page
  ATextSection.WriteData(Byte($48));
  ATextSection.WriteData(Byte($81));
  ATextSection.WriteData(Byte((3 shl 6) or (5 shl 3) or 4));  // ModRM: mod=11, reg=5(SUB), rm=RSP
  ATextSection.WriteData(Int32(4096));

  // TEST [RSP], ESP (85 24 24) — touch the page to trigger guard page commit
  ATextSection.WriteData(Byte($85));
  ATextSection.WriteData(Byte($24));
  ATextSection.WriteData(Byte($24));

  // SUB EAX, 4096 (2D imm32) — decrement remaining counter
  ATextSection.WriteData(Byte($2D));
  ATextSection.WriteData(Int32(4096));

  // CMP EAX, 4096 (3D imm32) — check if another full page remains
  ATextSection.WriteData(Byte($3D));
  ATextSection.WriteData(Int32(4096));

  // JAE loop_top (73 rel8) — unsigned: continue while remaining >= 4096
  ATextSection.WriteData(Byte($73));
  ATextSection.WriteData(Byte(LProbeLoopTop - ATextSection.Size - 1));

  // SUB RSP, RAX (REX.W 29 C4) — allocate remaining bytes (< 4096)
  ATextSection.WriteData(Byte($48));
  ATextSection.WriteData(Byte($29));
  ATextSection.WriteData(Byte((3 shl 6) or (0 shl 3) or 4));  // ModRM: mod=11, reg=RAX(0), rm=RSP(4)
end;


end.
