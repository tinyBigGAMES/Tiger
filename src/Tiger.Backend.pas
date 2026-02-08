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
  Tiger.Builders;
type
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


end.
