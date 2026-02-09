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
  // At opt level 0, calls Tiger_ReportLeaks before exit for heap leak detection.
  // No DLL imports needed — syscalls go direct to kernel.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_Halt', vtVoid, False, plC, False)
     .Param('AExitCode', vtInt32);
  if AOptLevel = 0 then
    AIR.Call('Tiger_ReportLeaks', []);
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
  //----------------------------------------------------------------------------
  // Import libc heap functions
  //----------------------------------------------------------------------------
  AIR.Import('libc.so.6', 'malloc', [vtUInt64], vtPointer, False);
  AIR.Import('libc.so.6', 'free', [vtPointer], vtVoid, False);
  AIR.Import('libc.so.6', 'realloc', [vtPointer, vtUInt64], vtPointer, False);
  AIR.Import('libc.so.6', 'calloc', [vtUInt64, vtUInt64], vtPointer, False);
  AIR.Import('libc.so.6', 'malloc_usable_size', [vtPointer], vtUInt64, False);

  //----------------------------------------------------------------------------
  // Debug heap tracking (only when optimization level = 0)
  //----------------------------------------------------------------------------
  if AOptLevel = 0 then
  begin
    AIR.Global('Tiger_AllocCount', vtUInt64);
    AIR.Global('Tiger_FreeCount', vtUInt64);
  end;

  //----------------------------------------------------------------------------
  // Tiger_GetMem(ASize: UInt64): Pointer
  // Allocates ASize bytes using malloc
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LResult', vtPointer);

  AIR.Assign('LResult', AIR.Invoke('malloc', [AIR.Get('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Get('Tiger_AllocCount'), AIR.Int64(1)));

  AIR.Return(AIR.Get('LResult'));
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_FreeMem(APtr: Pointer)
  // Frees memory previously allocated by Tiger_GetMem
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_FreeMem', vtVoid, False, plC, False)
     .Param('APtr', vtPointer);

  AIR.Call('free', [AIR.Get('APtr')]);

  if AOptLevel = 0 then
    AIR.Assign('Tiger_FreeCount', AIR.Add(AIR.Get('Tiger_FreeCount'), AIR.Int64(1)));

  AIR.Return();
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ReAllocMem(APtr: Pointer; ANewSize: UInt64): Pointer
  // Reallocates memory block to new size
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_ReAllocMem', vtPointer, False, plC, False)
     .Param('APtr', vtPointer)
     .Param('ANewSize', vtUInt64)
     .Return(AIR.Invoke('realloc', [AIR.Get('APtr'), AIR.Get('ANewSize')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_AllocMem(ASize: UInt64): Pointer
  // Allocates and zero-initializes memory using calloc
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_AllocMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LResult', vtPointer);

  AIR.Assign('LResult', AIR.Invoke('calloc', [AIR.Int64(1), AIR.Get('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Get('Tiger_AllocCount'), AIR.Int64(1)));

  AIR.Return(AIR.Get('LResult'));
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_MemSize(APtr: Pointer): UInt64
  // Returns the usable size of an allocated memory block (GNU extension)
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_MemSize', vtUInt64, False, plC, False)
     .Param('APtr', vtPointer)
     .Return(AIR.Invoke('malloc_usable_size', [AIR.Get('APtr')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ReportLeaks() - Debug only
  // Prints allocation summary to console
  //----------------------------------------------------------------------------
  if AOptLevel = 0 then
  begin
    AIR.Func('Tiger_ReportLeaks', vtVoid, False, plC, False);
    AIR.Call('printf', [AIR.Str('[Heap] Allocs: %llu, Frees: %llu, Leaked: %lld' + #10),
                        AIR.Get('Tiger_AllocCount'),
                        AIR.Get('Tiger_FreeCount'),
                        AIR.Sub(AIR.Get('Tiger_AllocCount'), AIR.Get('Tiger_FreeCount'))]);
    AIR.Return();
    AIR.EndFunc();
  end;
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
