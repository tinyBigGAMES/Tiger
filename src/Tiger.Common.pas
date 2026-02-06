{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Common;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  Tiger.Utils,
  Tiger.Errors;

const
  TIGER_MAJOR_VERSION = 0;
  TIGER_MINOR_VERSION = 1;
  TIGER_PATCH_VERSION = 0;
  TIGER_VERSION = (TIGER_MAJOR_VERSION * 10000) + (TIGER_MINOR_VERSION * 100) + TIGER_PATCH_VERSION;
  TIGER_VERSION_STR = '0.1.0';

type
  PInt32 = ^Int32;

  { TTigerBuildMode }
  TTigerBuildMode = (
    bmExe,
    bmLib,
    bmDll
  );

  { TTigerOptimizeLevel }
  TTigerOptimizeLevel = (
    olDebug,
    olReleaseSafe,
    olReleaseFast,
    olReleaseSmall
  );

  { TTigerTargetPlatform }
  TTigerTargetPlatform = (
    tpWin64,        // x86_64-windows
    tpLinux64,      // x86_64-linux
    tpMacOS64,      // aarch64-macos (Apple Silicon)
    tpWinARM64,     // aarch64-windows (Windows on ARM)
    tpLinuxARM64    // aarch64-linux (Raspberry Pi, servers)
  );

  { TTigerLinkage - Linkage specification for functions }
  TTigerLinkage = (
    plDefault,      // Itanium C++ ABI mangling
    plC             // C linkage - no mangling
  );

  { TTigerBaseObject }
  TTigerBaseObject = class(TBaseObject)
  protected
    FErrors: TErrors;
  public
    procedure SetErrors(const AErrors: TErrors); virtual;
    function GetErrors(): TErrors;
  end;

  { TTigerStatusObject }
  TTigerStatusObject = class(TTigerBaseObject)
  private
    FStatus: TCallback<TStatusCallback>;
  public
    procedure Status(const AText: string); overload;
    procedure Status(const AText: string; const AArgs: array of const); overload;
    procedure SetStatusCallback(const ACallback: TStatusCallback; const AUserData: Pointer = nil);
    procedure CopyStatusCallbackTo(const ATarget: TTigerStatusObject);
  end;

implementation

{ TTigerBaseObject }

procedure TTigerBaseObject.SetErrors(const AErrors: TErrors);
begin
  FErrors := AErrors;
end;

function TTigerBaseObject.GetErrors(): TErrors;
begin
  Result := FErrors;
end;

{ TTigerStatusObject }

procedure TTigerStatusObject.Status(const AText: string);
begin
  if FStatus.IsAssigned() then
    FStatus.Callback(AText, FStatus.UserData);
end;

procedure TTigerStatusObject.Status(const AText: string; const AArgs: array of const);
begin
  Status(Format(AText, AArgs));
end;

procedure TTigerStatusObject.SetStatusCallback(const ACallback: TStatusCallback; const AUserData: Pointer);
begin
  FStatus.Callback := ACallback;
  FStatus.UserData := AUserData;
end;

procedure TTigerStatusObject.CopyStatusCallbackTo(const ATarget: TTigerStatusObject);
begin
  if ATarget <> nil then
    ATarget.SetStatusCallback(FStatus.Callback, FStatus.UserData);
end;

end.
