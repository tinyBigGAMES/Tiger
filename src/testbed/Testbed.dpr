program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UTestbed in 'UTestbed.pas',
  Tiger.ABI in '..\Tiger.ABI.pas',
  Tiger.Backend in '..\Tiger.Backend.pas',
  Tiger.Common in '..\Tiger.Common.pas',
  Tiger.Errors in '..\Tiger.Errors.pas',
  Tiger.IR in '..\Tiger.IR.pas',
  Tiger.Linker in '..\Tiger.Linker.pas',
  Tiger in '..\Tiger.pas',
  Tiger.Resources in '..\Tiger.Resources.pas',
  Tiger.Runtime in '..\Tiger.Runtime.pas',
  Tiger.SSA in '..\Tiger.SSA.pas',
  Tiger.Utils in '..\Tiger.Utils.pas';

begin
  RunTestbed();
end.
