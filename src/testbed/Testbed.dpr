{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UTestbed in 'UTestbed.pas',
  Tiger.ABI in '..\Tiger.ABI.pas',
  Tiger.ABI.Win64 in '..\Tiger.ABI.Win64.pas',
  Tiger.Backend in '..\Tiger.Backend.pas',
  Tiger.Backend.Win64 in '..\Tiger.Backend.Win64.pas',
  Tiger.Backend.X64 in '..\Tiger.Backend.X64.pas',
  Tiger.Builders in '..\Tiger.Builders.pas',
  Tiger.Common in '..\Tiger.Common.pas',
  Tiger.Errors in '..\Tiger.Errors.pas',
  Tiger.IR in '..\Tiger.IR.pas',
  Tiger.Linker.COFF in '..\Tiger.Linker.COFF.pas',
  Tiger.Linker in '..\Tiger.Linker.pas',
  Tiger in '..\Tiger.pas',
  Tiger.Resources in '..\Tiger.Resources.pas',
  Tiger.Runtime in '..\Tiger.Runtime.pas',
  Tiger.Runtime.Win64 in '..\Tiger.Runtime.Win64.pas',
  Tiger.SSA in '..\Tiger.SSA.pas',
  Tiger.Types in '..\Tiger.Types.pas',
  Tiger.Utils in '..\Tiger.Utils.pas',
  Tiger.Utils.Win64 in '..\Tiger.Utils.Win64.pas';

begin
  RunTestbed();
end.
