{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Runtime.Linux64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Types,
  Tiger.Common,
  Tiger.IR,
  Tiger.Runtime;

type
  //============================================================================
  // TTigerLinux64Runtime - Linux x86-64 runtime library injection.
  // Uses raw syscalls instead of DLL imports for Phase 1.
  //============================================================================

  { TTigerLinux64Runtime }
  TTigerLinux64Runtime = class(TTigerRuntime)
  public
    /// <summary>Injects the Tiger_Halt procedure (exit via syscall 60).</summary>
    procedure AddSystem(const AIR: TTigerIR; const AOptLevel: Integer); override;

    /// <summary>Injects console I/O stubs (Tiger_InitConsole as no-op).</summary>
    procedure AddIO(const AIR: TTigerIR); override;

    /// <summary>Memory runtime — stub for Phase 2.</summary>
    procedure AddMemory(const AIR: TTigerIR; const AOptLevel: Integer); override;

    /// <summary>String runtime — stub for Phase 2.</summary>
    procedure AddStrings(const AIR: TTigerIR); override;

    /// <summary>Exception runtime — stub for Phase 2.</summary>
    procedure AddExceptions(const AIR: TTigerIR); override;
  end;

  //----------------------------------------------------------------------------
  // Compile-time helpers for write/writeln (Linux variant)
  //----------------------------------------------------------------------------
  procedure tiger_write_linux(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
  procedure tiger_writeln_linux(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);

implementation

//==============================================================================
// TTigerLinux64Runtime - System
//==============================================================================

procedure TTigerLinux64Runtime.AddSystem(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  //----------------------------------------------------------------------------
  // Tiger_Halt(AExitCode: Int32)
  // Terminates the process via Linux exit syscall (nr 60).
  // No DLL imports needed — syscalls go direct to kernel.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_Halt', vtVoid, False, plC, False)
     .Param('AExitCode', vtInt32);
  AIR.SyscallStmt(60, [AIR.Get('AExitCode')])
  .EndFunc();
end;

//==============================================================================
// TTigerLinux64Runtime - I/O
//==============================================================================

procedure TTigerLinux64Runtime.AddIO(const AIR: TTigerIR);
begin
  //----------------------------------------------------------------------------
  // Tiger_InitConsole()
  // No-op on Linux — console is UTF-8 by default.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_InitConsole', vtVoid, False, plC, False)
     .Return()
  .EndFunc();
end;

//------------------------------------------------------------------------------
// Compile-time helpers for write/writeln (Linux variant)
// Phase 1: raw syscall write(fd=1, buf, len) for string output.
// Formatted printf support comes in Phase 2 with libc linking.
//------------------------------------------------------------------------------

procedure tiger_write_linux(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
begin
  // Phase 1 stub — formatted output requires libc printf (Phase 2)
  // For now, only string literals can be written via syscall 1
  if Length(AArgs) > 0 then
    AIR.SyscallStmt(1, [AIR.Int64(1), AArgs[0], AArgs[1]]);
end;

procedure tiger_writeln_linux(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
begin
  if Length(AArgs) > 0 then
    AIR.SyscallStmt(1, [AIR.Int64(1), AArgs[0], AArgs[1]]);
  // Write newline: write(1, "\n", 1)
  AIR.SyscallStmt(1, [AIR.Int64(1), AIR.Str(#10), AIR.Int64(1)]);
end;

//==============================================================================
// TTigerLinux64Runtime - Stubs (Phase 2+)
//==============================================================================

procedure TTigerLinux64Runtime.AddMemory(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  // Phase 2: mmap/munmap via syscalls or libc malloc/free
end;

procedure TTigerLinux64Runtime.AddStrings(const AIR: TTigerIR);
begin
  // Phase 2: managed string runtime
end;

procedure TTigerLinux64Runtime.AddExceptions(const AIR: TTigerIR);
begin
  // Phase 2: signal-based exception handling
end;

end.
