{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Runtime.Win64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.IR,
  Tiger.Runtime;

type
  //============================================================================
  // TTigerWin64Runtime - Win64-specific runtime library injection.
  // Imports from kernel32.dll, msvcrt.dll, and generates Win64 SEH support.
  //============================================================================

  { TTigerWin64Runtime }
  TTigerWin64Runtime = class(TTigerRuntime)
  public
    // System essentials (ExitProcess, Tiger_Halt)
    procedure AddSystem(const AIR: TTigerIR; const AOptLevel: Integer); override;

    // I/O (tiger_write, tiger_writeln via msvcrt printf)
    procedure AddIO(const AIR: TTigerIR); override;

    // Memory management (Tiger_GetMem, Tiger_FreeMem via HeapAlloc/HeapFree)
    procedure AddMemory(const AIR: TTigerIR; const AOptLevel: Integer); override;

    // String management (Tiger_StrAlloc, Tiger_StrFree, etc.)
    procedure AddStrings(const AIR: TTigerIR); override;
    procedure AddTypes(const AIR: TTigerIR); override;

    // Exception handling (Tiger_Raise, Tiger_GetExceptionCode, etc.)
    procedure AddExceptions(const AIR: TTigerIR); override;
  end;

//------------------------------------------------------------------------------
// Compile-time helpers for write/writeln (called from codegen)
//------------------------------------------------------------------------------
procedure tiger_write(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);
procedure tiger_writeln(const AIR: TTigerIR; const AArgs: array of TTigerIRExpr);

implementation


//==============================================================================
// TTigerRuntime - System Essentials
//==============================================================================

procedure TTigerWin64Runtime.AddSystem(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  //----------------------------------------------------------------------------
  // Import Windows system functions
  //----------------------------------------------------------------------------
  AIR.Import('kernel32.dll', 'ExitProcess', [vtUInt32], vtVoid, False);

  //----------------------------------------------------------------------------
  // Tiger_Halt(AExitCode: Int32)
  // Terminates the process with the specified exit code.
  // At opt level 0, calls Tiger_ReportLeaks before ExitProcess for heap
  // leak detection in debug builds.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_Halt', vtVoid, False, plC, False)
     .Param('AExitCode', vtInt32);
  if AOptLevel = 0 then
    AIR.Call('Tiger_ReportLeaks', []);
  AIR.Call('ExitProcess', [AIR.Get('AExitCode')])
  .EndFunc();
end;

//==============================================================================
// TTigerRuntime - I/O
//==============================================================================

procedure TTigerWin64Runtime.AddIO(const AIR: TTigerIR);
begin
  //----------------------------------------------------------------------------
  // Import C runtime printf (variadic) - used by tiger_write/tiger_writeln helpers
  //----------------------------------------------------------------------------
  AIR.Import('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

  //----------------------------------------------------------------------------
  // Import Windows console functions for UTF-8 support
  //----------------------------------------------------------------------------
  AIR.Import('kernel32.dll', 'SetConsoleOutputCP', [vtUInt32], vtInt32, False);
  AIR.Import('kernel32.dll', 'SetConsoleCP', [vtUInt32], vtInt32, False);

  //----------------------------------------------------------------------------
  // Tiger_InitConsole()
  // Initializes the console for UTF-8 output. Called at program startup.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_InitConsole', vtVoid, False, plC, False)
     // Set console output code page to UTF-8 (65001)
     .Call('SetConsoleOutputCP', [AIR.Int64(65001)])
     // Set console input code page to UTF-8 (65001)
     .Call('SetConsoleCP', [AIR.Int64(65001)])
     .Return()
  .EndFunc();
end;

//------------------------------------------------------------------------------
// Compile-time helpers for write/writeln
// These are called from codegen to emit printf calls
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

//==============================================================================
// TTigerRuntime - Memory Management
//==============================================================================

procedure TTigerWin64Runtime.AddMemory(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  //----------------------------------------------------------------------------
  // Import Windows heap functions
  //----------------------------------------------------------------------------
  AIR.Import('kernel32.dll', 'GetProcessHeap', [], vtPointer, False);
  AIR.Import('kernel32.dll', 'HeapAlloc', [vtPointer, vtUInt32, vtUInt64], vtPointer, False);
  AIR.Import('kernel32.dll', 'HeapFree', [vtPointer, vtUInt32, vtPointer], vtInt32, False);
  AIR.Import('kernel32.dll', 'HeapReAlloc', [vtPointer, vtUInt32, vtPointer, vtUInt64], vtPointer, False);
  AIR.Import('kernel32.dll', 'HeapSize', [vtPointer, vtUInt32, vtPointer], vtUInt64, False);

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
  // Allocates ASize bytes from the process heap
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LHeap', vtPointer)
     .Local('LResult', vtPointer);

  AIR.Assign('LHeap', AIR.Invoke('GetProcessHeap', []));
  AIR.Assign('LResult', AIR.Invoke('HeapAlloc', [AIR.Get('LHeap'), AIR.Int64(0), AIR.Get('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Get('Tiger_AllocCount'), AIR.Int64(1)));

  AIR.Return(AIR.Get('LResult'));
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_FreeMem(APtr: Pointer)
  // Frees memory previously allocated by Tiger_GetMem
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_FreeMem', vtVoid, False, plC, False)
     .Param('APtr', vtPointer)
     .Local('LHeap', vtPointer);

  AIR.Assign('LHeap', AIR.Invoke('GetProcessHeap', []));
  AIR.Call('HeapFree', [AIR.Get('LHeap'), AIR.Int64(0), AIR.Get('APtr')]);

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
     .Local('LHeap', vtPointer)
     .Assign('LHeap', AIR.Invoke('GetProcessHeap', []))
     .Return(AIR.Invoke('HeapReAlloc', [AIR.Get('LHeap'), AIR.Int64(0), AIR.Get('APtr'), AIR.Get('ANewSize')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_AllocMem(ASize: UInt64): Pointer
  // Allocates and zero-initializes memory (uses HEAP_ZERO_MEMORY = 0x08)
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_AllocMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LHeap', vtPointer)
     .Local('LResult', vtPointer);

  AIR.Assign('LHeap', AIR.Invoke('GetProcessHeap', []));
  AIR.Assign('LResult', AIR.Invoke('HeapAlloc', [AIR.Get('LHeap'), AIR.Int64(8), AIR.Get('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Get('Tiger_AllocCount'), AIR.Int64(1)));

  AIR.Return(AIR.Get('LResult'));
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_MemSize(APtr: Pointer): UInt64
  // Returns the size of an allocated memory block
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_MemSize', vtUInt64, False, plC, False)
     .Param('APtr', vtPointer)
     .Local('LHeap', vtPointer)
     .Assign('LHeap', AIR.Invoke('GetProcessHeap', []))
     .Return(AIR.Invoke('HeapSize', [AIR.Get('LHeap'), AIR.Int64(0), AIR.Get('APtr')]))
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

//==============================================================================
// TTigerRuntime - String Management
//==============================================================================
//
// String Layout (TStringRec - 40 bytes):
//   Offset  0: RefCount  (Int64)  - Reference count, -1 = immortal literal
//   Offset  8: Length    (UInt64) - Length in bytes (not including null)
//   Offset 16: Capacity  (UInt64) - Allocated capacity of data buffer
//   Offset 24: Data      (Pointer)- Pointer to UTF-8 bytes (null-terminated)
//   Offset 32: Data16    (Pointer)- Cached UTF-16 conversion (lazily allocated)
//
// A string variable is a pointer to TStringRec (8 bytes).
// nil pointer = empty string.
//
//==============================================================================

procedure TTigerWin64Runtime.AddStrings(const AIR: TTigerIR);
var
  LDeref: TTigerIRExpr;
begin
  //----------------------------------------------------------------------------
  // Import C runtime for memory copy
  //----------------------------------------------------------------------------
  AIR.Import('ntdll.dll', 'memcpy', [vtPointer, vtPointer, vtUInt64], vtPointer, False);
  AIR.Import('ntdll.dll', 'memset', [vtPointer, vtInt32, vtUInt64], vtPointer, False);
  AIR.Import('ntdll.dll', 'memcmp', [vtPointer, vtPointer, vtUInt64], vtInt32, False);
  AIR.Import('kernel32.dll', 'MultiByteToWideChar', [vtUInt32, vtUInt32, vtPointer, vtInt32, vtPointer, vtInt32], vtInt32, False);

  //----------------------------------------------------------------------------
  // Ensure string types are defined (idempotent - may already be defined)
  //----------------------------------------------------------------------------
  AddTypes(AIR);

  //----------------------------------------------------------------------------
  // Tiger_StrAlloc(ACapacity: UInt64): Pointer
  // Allocates a new string with the specified capacity.
  // RefCount = 1, Length = 0, Data buffer allocated and null-terminated.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrAlloc', vtPointer, False, plC, False)
     .Param('ACapacity', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     // Allocate the TStringRec struct (40 bytes)
     .Assign('LRec', AIR.Invoke('Tiger_GetMem', [AIR.Int64(40)]))
     // Allocate data buffer (capacity + 1 for null terminator)
     .Assign('LData', AIR.Invoke('Tiger_GetMem', [AIR.Add(AIR.Get('ACapacity'), AIR.Int64(1))]));

  // Initialize fields
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'RefCount'), AIR.Int64(1));

  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Int64(0));

  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Capacity'), AIR.Get('ACapacity'));

  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Data'), AIR.Get('LData'));

  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Data16'), AIR.Null());

  // Null-terminate the empty string and return
  AIR.Call('memset', [AIR.Get('LData'), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrFree(AStr: Pointer)
  // Frees the string's data buffer and the TStringRec struct.
  // Does nothing if AStr is nil.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrFree', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LData', vtPointer)
     .Local('LData16', vtPointer)
     // if AStr = nil then exit
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return()
     .EndIf();

  // Free UTF-16 cache if present
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LData16', AIR.GetField(LDeref, 'Data16'))
     .&If(AIR.Ne(AIR.Get('LData16'), AIR.Null()))
        .Call('Tiger_FreeMem', [AIR.Get('LData16')])
     .EndIf();

  // Free data buffer
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     .&If(AIR.Ne(AIR.Get('LData'), AIR.Null()))
        .Call('Tiger_FreeMem', [AIR.Get('LData')])
     .EndIf()
     // Free struct
     .Call('Tiger_FreeMem', [AIR.Get('AStr')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrAddRef(AStr: Pointer)
  // Increments the reference count if not nil and not immortal (-1).
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrAddRef', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LRefCount', vtInt64)
     // if AStr = nil then exit
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return()
     .EndIf();

  // Get current refcount
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LRefCount', AIR.GetField(LDeref, 'RefCount'))
     // if RefCount = -1 (immortal) then exit
     .&If(AIR.Eq(AIR.Get('LRefCount'), AIR.Int64(-1)))
        .Return()
     .EndIf();

  // Increment refcount
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'RefCount'), AIR.Add(AIR.Get('LRefCount'), AIR.Int64(1)))
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrRelease(AStr: Pointer)
  // Decrements the reference count. Frees the string if count reaches zero.
  // Does nothing if nil or immortal (-1).
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrRelease', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LRefCount', vtInt64)
     // if AStr = nil then exit
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return()
     .EndIf();

  // Get current refcount
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LRefCount', AIR.GetField(LDeref, 'RefCount'))
     // if RefCount = -1 (immortal) then exit
     .&If(AIR.Eq(AIR.Get('LRefCount'), AIR.Int64(-1)))
        .Return()
     .EndIf()
     // Decrement refcount
     .Assign('LRefCount', AIR.Sub(AIR.Get('LRefCount'), AIR.Int64(1)));

  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'RefCount'), AIR.Get('LRefCount'))
     // if RefCount = 0 then free
     .&If(AIR.Eq(AIR.Get('LRefCount'), AIR.Int64(0)))
        .Call('Tiger_StrFree', [AIR.Get('AStr')])
     .EndIf()
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ReleaseOnDetach(AReason: Int32; AStr: Pointer)
  // Conditionally releases a managed string during DLL unload.
  // Only releases when AReason = 0 (DLL_PROCESS_DETACH).
  // Called by SSA cleanup pass for managed globals in DllMain.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_ReleaseOnDetach', vtVoid, False, plC, False)
     .Param('AReason', vtInt32)
     .Param('AStr', vtPointer)
     // Only release on DLL_PROCESS_DETACH (reason = 0)
     .&If(AIR.Eq(AIR.Get('AReason'), AIR.Int32(0)))
        .Call('Tiger_StrRelease', [AIR.Get('AStr')])
     .EndIf()
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrFromLiteral(AData: Pointer; ALen: UInt64): Pointer
  // Creates a new string from static literal data.
  // Copies the data, sets length, null-terminates.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrFromLiteral', vtPointer, False, plC, False)
     .Param('AData', vtPointer)
     .Param('ALen', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     // Allocate string with capacity = length
     .Assign('LRec', AIR.Invoke('Tiger_StrAlloc', [AIR.Get('ALen')]));

  // Get data pointer
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     // Copy literal data
     .Call('memcpy', [AIR.Get('LData'), AIR.Get('AData'), AIR.Get('ALen')]);

  // Set length
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Get('ALen'))
     // Null terminate (memset at data+len)
     .Call('memset', [AIR.Add(AIR.Get('LData'), AIR.Get('ALen')), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrFromChar(AChar: UInt64): Pointer
  // Creates a new string from a single character.
  // Allocates string, stores char, sets length to 1, null-terminates.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrFromChar', vtPointer, False, plC, False)
     .Param('AChar', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     // Allocate string with capacity = 1
     .Assign('LRec', AIR.Invoke('Tiger_StrAlloc', [AIR.Int64(1)]));

  // Get data pointer
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     // Store char byte at data[0]
     .Call('memset', [AIR.Get('LData'), AIR.Get('AChar'), AIR.Int64(1)]);

  // Set length to 1
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Int64(1))
     // Null terminate at data[1]
     .Call('memset', [AIR.Add(AIR.Get('LData'), AIR.Int64(1)), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrConcat(AStr1, AStr2: Pointer): Pointer
  // Creates a new string that is the concatenation of AStr1 and AStr2.
  // Handles nil strings as empty.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrConcat', vtPointer, False, plC, False)
     .Param('AStr1', vtPointer)
     .Param('AStr2', vtPointer)
     .Local('LLen1', vtUInt64)
     .Local('LLen2', vtUInt64)
     .Local('LTotalLen', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     .Local('LData1', vtPointer)
     .Local('LData2', vtPointer)
     // Get length of first string (0 if nil)
     .&If(AIR.Eq(AIR.Get('AStr1'), AIR.Null()))
        .Assign('LLen1', AIR.Int64(0))
     .&Else();

  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LLen1', AIR.GetField(LDeref, 'Length'))
     .EndIf()
     // Get length of second string (0 if nil)
     .&If(AIR.Eq(AIR.Get('AStr2'), AIR.Null()))
        .Assign('LLen2', AIR.Int64(0))
     .&Else();

  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LLen2', AIR.GetField(LDeref, 'Length'))
     .EndIf()
     // Total length
     .Assign('LTotalLen', AIR.Add(AIR.Get('LLen1'), AIR.Get('LLen2')))
     // Allocate new string
     .Assign('LRec', AIR.Invoke('Tiger_StrAlloc', [AIR.Get('LTotalLen')]));

  // Get data pointer of result
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     // Copy first string if not empty
     .&If(AIR.Gt(AIR.Get('LLen1'), AIR.Int64(0)));

  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LData1', AIR.GetField(LDeref, 'Data'))
        .Call('memcpy', [AIR.Get('LData'), AIR.Get('LData1'), AIR.Get('LLen1')])
     .EndIf()
     // Copy second string if not empty
     .&If(AIR.Gt(AIR.Get('LLen2'), AIR.Int64(0)));

  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LData2', AIR.GetField(LDeref, 'Data'))
        .Call('memcpy', [AIR.Add(AIR.Get('LData'), AIR.Get('LLen1')), AIR.Get('LData2'), AIR.Get('LLen2')])
     .EndIf();

  // Set length and null-terminate
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Get('LTotalLen'))
     .Call('memset', [AIR.Add(AIR.Get('LData'), AIR.Get('LTotalLen')), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrAssign(ADest: Pointer; ASrc: Pointer)
  // Assignment with automatic reference counting.
  // ADest is pointer to a string variable (pointer to pointer to TStringRec).
  // Automatically AddRefs source and releases old dest value.
  // Self-assignment safe: AddRef before Release prevents use-after-free.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrAssign', vtVoid, False, plC, False)
     .Param('ADest', vtPointer)   // Pointer to the string variable
     .Param('ASrc', vtPointer)    // Source string (TStringRec pointer)
     .Local('LOld', vtPointer)
     // Get old value from destination
     .Assign('LOld', AIR.Deref(AIR.Get('ADest'), vtPointer))
     // AddRef the source FIRST (self-assignment safe: refcount 1->2)
     .Call('Tiger_StrAddRef', [AIR.Get('ASrc')])
     // Store new value
     .AssignTo(AIR.Deref(AIR.Get('ADest'), vtPointer), AIR.Get('ASrc'))
     // Release old value LAST (self-assignment safe: refcount 2->1)
     .Call('Tiger_StrRelease', [AIR.Get('LOld')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrLen(AStr: Pointer): UInt64
  // Returns the length of the string in bytes. Returns 0 if nil.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrLen', vtUInt64, False, plC, False)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Int64(0))
     .EndIf();

  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Return(AIR.GetField(LDeref, 'Length'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrCompare(AStr1, AStr2: Pointer): Int64
  // Lexicographic comparison. Returns <0 if AStr1<AStr2, 0 if equal, >0 if AStr1>AStr2.
  // Nil is treated as less than any non-nil string.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrCompare', vtInt64, False, plC, False)
     .Param('AStr1', vtPointer)
     .Param('AStr2', vtPointer)
     .Local('LLen1', vtUInt64)
     .Local('LLen2', vtUInt64)
     .Local('LMinLen', vtUInt64)
     .Local('LData1', vtPointer)
     .Local('LData2', vtPointer)
     .Local('LResult', vtInt32)
     // Check AStr1 nil
     .&If(AIR.Eq(AIR.Get('AStr1'), AIR.Null()))
        // AStr1 is nil - check AStr2
        .&If(AIR.Eq(AIR.Get('AStr2'), AIR.Null()))
           .Return(AIR.Int64(0))   // Both nil -> equal
        .EndIf()
        .Return(AIR.Int64(-1))     // AStr1 nil, AStr2 not nil -> less than
     .EndIf()
     // AStr1 not nil - check AStr2
     .&If(AIR.Eq(AIR.Get('AStr2'), AIR.Null()))
        .Return(AIR.Int64(1))      // AStr1 not nil, AStr2 nil -> greater than
     .EndIf();

  // Get lengths
  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LLen1', AIR.GetField(LDeref, 'Length'));
  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LLen2', AIR.GetField(LDeref, 'Length'));

  // Get data pointers
  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LData1', AIR.GetField(LDeref, 'Data'));
  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LData2', AIR.GetField(LDeref, 'Data'));

  // LMinLen = min(LLen1, LLen2)
  AIR.&If(AIR.Lt(AIR.Get('LLen1'), AIR.Get('LLen2')))
       .Assign('LMinLen', AIR.Get('LLen1'))
     .&Else()
       .Assign('LMinLen', AIR.Get('LLen2'))
     .EndIf();

  // LResult = memcmp(LData1, LData2, LMinLen)
  AIR.Assign('LResult', AIR.Invoke('memcmp', [AIR.Get('LData1'), AIR.Get('LData2'), AIR.Get('LMinLen')]))
     // If memcmp result < 0, return -1
     .&If(AIR.Lt(AIR.Get('LResult'), AIR.Int64(0)))
        .Return(AIR.Int64(-1))
     .EndIf()
     // If memcmp result > 0, return 1
     .&If(AIR.Gt(AIR.Get('LResult'), AIR.Int64(0)))
        .Return(AIR.Int64(1))
     .EndIf()
     // memcmp returned 0, compare by length
     .&If(AIR.Lt(AIR.Get('LLen1'), AIR.Get('LLen2')))
        .Return(AIR.Int64(-1))
     .EndIf()
     .&If(AIR.Gt(AIR.Get('LLen1'), AIR.Get('LLen2')))
        .Return(AIR.Int64(1))
     .EndIf()
     .Return(AIR.Int64(0))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrData(AStr: Pointer): Pointer
  // Returns pointer to the UTF-8 data (for pchar() conversion).
  // Returns nil if string is nil.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrData', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Null())
     .EndIf();

  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Return(AIR.GetField(LDeref, 'Data'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrUtf16(AStr: Pointer): Pointer
  // Returns pointer to cached UTF-16 data. Converts from UTF-8 on first call.
  // Returns nil if string is nil.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrUtf16', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LData16', vtPointer)
     .Local('LData', vtPointer)
     .Local('LLen', vtUInt64)
     .Local('LWideLen', vtInt32)
     .Local('LBuf', vtPointer)
     // if AStr = nil then return nil
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Null())
     .EndIf();

  // Check if already cached
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LData16', AIR.GetField(LDeref, 'Data16'))
     .&If(AIR.Ne(AIR.Get('LData16'), AIR.Null()))
        .Return(AIR.Get('LData16'))
     .EndIf();

  // Get UTF-8 data pointer and length
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'));

  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LLen', AIR.GetField(LDeref, 'Length'));

  // Query required buffer size (CP_UTF8 = 65001)
  AIR.Assign('LWideLen', AIR.Invoke('MultiByteToWideChar',
        [AIR.Int64(65001), AIR.Int64(0), AIR.Get('LData'), AIR.Get('LLen'), AIR.Null(), AIR.Int64(0)]))
     // Allocate buffer: (LWideLen + 1) * 2 bytes for UTF-16 + null terminator
     .Assign('LBuf', AIR.Invoke('Tiger_GetMem',
        [AIR.Mul(AIR.Add(AIR.Get('LWideLen'), AIR.Int64(1)), AIR.Int64(2))]))
     // Convert UTF-8 to UTF-16
     .Call('MultiByteToWideChar',
        [AIR.Int64(65001), AIR.Int64(0), AIR.Get('LData'), AIR.Get('LLen'), AIR.Get('LBuf'), AIR.Get('LWideLen')]);

  // Null-terminate (2 bytes of zero)
  AIR.Call('memset', [AIR.Add(AIR.Get('LBuf'), AIR.Mul(AIR.Get('LWideLen'), AIR.Int64(2))), AIR.Int64(0), AIR.Int64(2)]);

  // Store in cache
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Data16'), AIR.Get('LBuf'))
     .Return(AIR.Get('LBuf'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrLiteralUtf16(AData: Pointer; ALen: UInt64): Pointer
  // Converts UTF-8 literal data to UTF-16. Returns pointer to allocated buffer.
  // Unlike Tiger_StrUtf16, this takes raw data pointer + length (no TTigerStringRec).
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrLiteralUtf16', vtPointer, False, plC, False)
     .Param('AData', vtPointer)
     .Param('ALen', vtUInt64)
     .Local('LWideLen', vtInt32)
     .Local('LBuf', vtPointer)
     // Query required buffer size (CP_UTF8 = 65001)
     .Assign('LWideLen', AIR.Invoke('MultiByteToWideChar',
        [AIR.Int64(65001), AIR.Int64(0), AIR.Get('AData'), AIR.Get('ALen'), AIR.Null(), AIR.Int64(0)]))
     // Allocate buffer: (LWideLen + 1) * 2 bytes
     .Assign('LBuf', AIR.Invoke('Tiger_GetMem',
        [AIR.Mul(AIR.Add(AIR.Get('LWideLen'), AIR.Int64(1)), AIR.Int64(2))]))
     // Convert UTF-8 to UTF-16
     .Call('MultiByteToWideChar',
        [AIR.Int64(65001), AIR.Int64(0), AIR.Get('AData'), AIR.Get('ALen'), AIR.Get('LBuf'), AIR.Get('LWideLen')])
     // Null-terminate (2 bytes of zero)
     .Call('memset', [AIR.Add(AIR.Get('LBuf'), AIR.Mul(AIR.Get('LWideLen'), AIR.Int64(2))), AIR.Int64(0), AIR.Int64(2)])
     .Return(AIR.Get('LBuf'))
  .EndFunc();
end;

//==============================================================================
// TTigerRuntime - String Type Definitions
//==============================================================================
// Defines TStringRec and 'string' pointer type only.
// Called early in TTiger.Create() so user code can reference 'string' type.
// Safe to call multiple times - checks if types already exist.
//==============================================================================

procedure TTigerWin64Runtime.AddTypes(const AIR: TTigerIR);
begin
  // Define TStringRec type
  AIR.DefineRecord('TStringRec')
     .Field('RefCount', vtInt64)
     .Field('Length', vtUInt64)
     .Field('Capacity', vtUInt64)
     .Field('Data', vtPointer)
     .Field('Data16', vtPointer)
  .EndRecord();

  // Register 'string' as a pointer to TStringRec
  AIR.DefinePointer('string', 'TStringRec', False);
end;

//==============================================================================
// TTigerRuntime - Exception Handling
//==============================================================================
//
// Exception Context (stored in TLS):
//   - Code: Int32 (stored as pointer in TLS slot)
//   - Message: Pointer to heap-allocated UTF-8 string
//
// TLS slots are allocated at runtime initialization via Tiger_InitExceptions.
// When raising, the message is copied to heap (runtime owns it).
// When a new exception is raised, old message is freed first.
//
//==============================================================================

procedure TTigerWin64Runtime.AddExceptions(const AIR: TTigerIR);
begin
  //----------------------------------------------------------------------------
  // Import Windows TLS functions
  //----------------------------------------------------------------------------
  AIR.Import('kernel32.dll', 'TlsAlloc', [], vtUInt32, False);
  AIR.Import('kernel32.dll', 'TlsGetValue', [vtUInt32], vtPointer, False);
  AIR.Import('kernel32.dll', 'TlsSetValue', [vtUInt32, vtPointer], vtInt32, False);
  AIR.Import('kernel32.dll', 'RaiseException', [vtUInt32, vtUInt32, vtUInt32, vtPointer], vtVoid, False);
  AIR.Import('ntdll.dll', 'strlen', [vtPointer], vtUInt64, False);
  // memcpy already imported in AddStrings, but import again to be safe
  AIR.Import('ntdll.dll', 'memcpy', [vtPointer, vtPointer, vtUInt64], vtPointer, False);

  //----------------------------------------------------------------------------
  // Global variables for TLS slot indices
  //----------------------------------------------------------------------------
  AIR.Global('Tiger_TlsExcCode', vtUInt32);   // TLS slot for exception code
  AIR.Global('Tiger_TlsExcMsg', vtUInt32);    // TLS slot for exception message

  //----------------------------------------------------------------------------
  // Tiger_InitExceptions()
  // Allocates TLS slots. Must be called at process/thread startup.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_InitExceptions', vtVoid, False, plC, False)
     .Assign('Tiger_TlsExcCode', AIR.Invoke('TlsAlloc', []))
     .Assign('Tiger_TlsExcMsg', AIR.Invoke('TlsAlloc', []))
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_SetException(ACode: Int32; AMsg: Pointer)
  // Internal helper: stores code and copies message to TLS.
  // Frees old message if present.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_SetException', vtVoid, False, plC, False)
     .Param('ACode', vtInt32)
     .Param('AMsg', vtPointer)
     .Local('LOldMsg', vtPointer)
     .Local('LLen', vtUInt64)
     .Local('LNewMsg', vtPointer)
     // Get and free old message if any
     .Assign('LOldMsg', AIR.Invoke('TlsGetValue', [AIR.Get('Tiger_TlsExcMsg')]))
     .&If(AIR.Ne(AIR.Get('LOldMsg'), AIR.Null()))
        .Call('Tiger_FreeMem', [AIR.Get('LOldMsg')])
     .EndIf()
     // Copy new message if not nil
     .&If(AIR.Ne(AIR.Get('AMsg'), AIR.Null()))
        .Assign('LLen', AIR.Invoke('strlen', [AIR.Get('AMsg')]))
        .Assign('LNewMsg', AIR.Invoke('Tiger_GetMem', [AIR.Add(AIR.Get('LLen'), AIR.Int64(1))]))
        .Call('memcpy', [AIR.Get('LNewMsg'), AIR.Get('AMsg'), AIR.Add(AIR.Get('LLen'), AIR.Int64(1))])
        .Call('TlsSetValue', [AIR.Get('Tiger_TlsExcMsg'), AIR.Get('LNewMsg')])
     .&Else()
        .Call('TlsSetValue', [AIR.Get('Tiger_TlsExcMsg'), AIR.Null()])
     .EndIf()
     // Store code (cast to pointer for TLS)
     .Call('TlsSetValue', [AIR.Get('Tiger_TlsExcCode'), AIR.Get('ACode')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_Raise(AMsg: Pointer)
  // Raises exception with default code (1) and message.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_Raise', vtVoid, False, plC, False)
     .Param('AMsg', vtPointer)
     .Call('Tiger_SetException', [AIR.Int64(1), AIR.Get('AMsg')])
     // RaiseException(0xE0505858, 0, 0, nil) - 'PXX' custom exception
     .Call('RaiseException', [AIR.Int64($E0505858), AIR.Int64(0), AIR.Int64(0), AIR.Null()])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_RaiseCode(ACode: Int32; AMsg: Pointer)
  // Raises exception with custom code and message.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_RaiseCode', vtVoid, False, plC, False)
     .Param('ACode', vtInt32)
     .Param('AMsg', vtPointer)
     .Call('Tiger_SetException', [AIR.Get('ACode'), AIR.Get('AMsg')])
     // RaiseException(0xE0505858, 0, 0, nil) - 'PXX' custom exception
     .Call('RaiseException', [AIR.Int64($E0505858), AIR.Int64(0), AIR.Int64(0), AIR.Null()])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptionCode(): Int32
  // Returns the current exception code from TLS.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetExceptionCode', vtInt32, False, plC, False)
     .Return(AIR.Invoke('TlsGetValue', [AIR.Get('Tiger_TlsExcCode')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptionMessage(): Pointer
  // Returns pointer to the current exception message from TLS.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetExceptionMessage', vtPointer, False, plC, False)
     .Return(AIR.Invoke('TlsGetValue', [AIR.Get('Tiger_TlsExcMsg')]))
  .EndFunc();
end;

//==============================================================================
// TTigerRuntime - Add All
//==============================================================================


end.
