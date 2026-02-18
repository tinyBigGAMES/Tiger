{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Runtime;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.IR;

//------------------------------------------------------------------------------
// Compile-time helpers for print/println
// These are called from codegen to emit printf calls with correct arguments
//------------------------------------------------------------------------------
procedure tiger_write(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
procedure tiger_writeln(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);

type
  //============================================================================
  // TTigerRuntime - Base runtime class with virtual methods for platform-
  // specific runtime library injection into the IR.
  //============================================================================

  { TTigerRuntime }
  TTigerRuntime = class
  public
    // System essentials (halt, exit)
    procedure AddSystem(const AIR: TTigerIR; const AOptLevel: Integer); virtual;

    // I/O (write, writeln)
    procedure AddIO(const AIR: TTigerIR); virtual;

    // Memory management (alloc, free, realloc)
    procedure AddMemory(const AIR: TTigerIR; const AOptLevel: Integer); virtual;

    // String management (alloc, free, concat, compare, etc.)
    procedure AddStrings(const AIR: TTigerIR); virtual;

    // String type definitions only (TStringRec, 'string' pointer)
    // Called early in Create() so user code can reference 'string' type
    procedure AddTypes(const AIR: TTigerIR); virtual;

    // Exception handling (raise, get code, etc.)
    procedure AddExceptions(const AIR: TTigerIR); virtual;

    // Command-line arguments (paramcount, paramstr)
    procedure AddCommandLine(const AIR: TTigerIR); virtual;

    // Add all runtime support
    procedure AddAll(const AIR: TTigerIR; const AOptLevel: Integer); virtual;
  end;

implementation

{ TTigerRuntime }

procedure TTigerRuntime.AddSystem(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  // Override in platform-specific subclass
end;

procedure TTigerRuntime.AddIO(const AIR: TTigerIR);
begin
end;

procedure TTigerRuntime.AddMemory(const AIR: TTigerIR; const AOptLevel: Integer);
begin
end;

procedure TTigerRuntime.AddStrings(const AIR: TTigerIR);
begin
end;

procedure TTigerRuntime.AddTypes(const AIR: TTigerIR);
begin
end;

procedure TTigerRuntime.AddExceptions(const AIR: TTigerIR);
begin
end;

procedure TTigerRuntime.AddCommandLine(const AIR: TTigerIR);
begin
end;

procedure TTigerRuntime.AddAll(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  AddIO(AIR);
  AddMemory(AIR, AOptLevel);
  AddStrings(AIR);
  AddExceptions(AIR);
  AddCommandLine(AIR);
  AddSystem(AIR, AOptLevel);
end;

//------------------------------------------------------------------------------
// Compile-time helpers for print/println
//------------------------------------------------------------------------------

procedure tiger_write(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
begin
  if Length(AArgs) > 0 then
    AIR.Call('printf', AArgs);
end;

procedure tiger_writeln(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
begin
  if Length(AArgs) > 0 then
    AIR.Call('printf', AArgs);
  AIR.Call('printf', [AIR.Str(#10)]);
end;

end.
