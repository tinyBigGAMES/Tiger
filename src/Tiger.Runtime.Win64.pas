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
  AIR.BeginFunc('Tiger_Halt', vtVoid, False, plC, False)
     .Param('AExitCode', vtInt32);
  if AOptLevel = 0 then
    AIR.Call('Tiger_ReportLeaks', []);
  AIR.Call('ExitProcess', [AIR.Var_('AExitCode')])
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
  AIR.BeginFunc('Tiger_InitConsole', vtVoid, False, plC, False)
     // Set console output code page to UTF-8 (65001)
     .Call('SetConsoleOutputCP', [AIR.Int(65001)])
     // Set console input code page to UTF-8 (65001)
     .Call('SetConsoleCP', [AIR.Int(65001)])
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
  AIR.BeginFunc('Tiger_GetMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LHeap', vtPointer)
     .Local('LResult', vtPointer);

  AIR.Assign('LHeap', AIR.CallExpr('GetProcessHeap', []));
  AIR.Assign('LResult', AIR.CallExpr('HeapAlloc', [AIR.Var_('LHeap'), AIR.Int(0), AIR.Var_('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Var_('Tiger_AllocCount'), AIR.Int(1)));

  AIR.Return(AIR.Var_('LResult'));
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_FreeMem(APtr: Pointer)
  // Frees memory previously allocated by Tiger_GetMem
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_FreeMem', vtVoid, False, plC, False)
     .Param('APtr', vtPointer)
     .Local('LHeap', vtPointer);

  AIR.Assign('LHeap', AIR.CallExpr('GetProcessHeap', []));
  AIR.Call('HeapFree', [AIR.Var_('LHeap'), AIR.Int(0), AIR.Var_('APtr')]);

  if AOptLevel = 0 then
    AIR.Assign('Tiger_FreeCount', AIR.Add(AIR.Var_('Tiger_FreeCount'), AIR.Int(1)));

  AIR.Return();
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ReAllocMem(APtr: Pointer; ANewSize: UInt64): Pointer
  // Reallocates memory block to new size
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_ReAllocMem', vtPointer, False, plC, False)
     .Param('APtr', vtPointer)
     .Param('ANewSize', vtUInt64)
     .Local('LHeap', vtPointer)
     .Assign('LHeap', AIR.CallExpr('GetProcessHeap', []))
     .Return(AIR.CallExpr('HeapReAlloc', [AIR.Var_('LHeap'), AIR.Int(0), AIR.Var_('APtr'), AIR.Var_('ANewSize')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_AllocMem(ASize: UInt64): Pointer
  // Allocates and zero-initializes memory (uses HEAP_ZERO_MEMORY = 0x08)
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_AllocMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LHeap', vtPointer)
     .Local('LResult', vtPointer);

  AIR.Assign('LHeap', AIR.CallExpr('GetProcessHeap', []));
  AIR.Assign('LResult', AIR.CallExpr('HeapAlloc', [AIR.Var_('LHeap'), AIR.Int(8), AIR.Var_('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Var_('Tiger_AllocCount'), AIR.Int(1)));

  AIR.Return(AIR.Var_('LResult'));
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_MemSize(APtr: Pointer): UInt64
  // Returns the size of an allocated memory block
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_MemSize', vtUInt64, False, plC, False)
     .Param('APtr', vtPointer)
     .Local('LHeap', vtPointer)
     .Assign('LHeap', AIR.CallExpr('GetProcessHeap', []))
     .Return(AIR.CallExpr('HeapSize', [AIR.Var_('LHeap'), AIR.Int(0), AIR.Var_('APtr')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ReportLeaks() - Debug only
  // Prints allocation summary to console
  //----------------------------------------------------------------------------
  if AOptLevel = 0 then
  begin
    AIR.BeginFunc('Tiger_ReportLeaks', vtVoid, False, plC, False);
    AIR.Call('printf', [AIR.Str('[Heap] Allocs: %llu, Frees: %llu, Leaked: %lld' + #10),
                        AIR.Var_('Tiger_AllocCount'),
                        AIR.Var_('Tiger_FreeCount'),
                        AIR.Sub(AIR.Var_('Tiger_AllocCount'), AIR.Var_('Tiger_FreeCount'))]);
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
  // Define TStringRec type
  //----------------------------------------------------------------------------
  AIR.DefineRecord('TStringRec')
     .Field('RefCount', vtInt64)
     .Field('Length', vtUInt64)
     .Field('Capacity', vtUInt64)
     .Field('Data', vtPointer)
     .Field('Data16', vtPointer)
  .EndRecord();

  // Register 'string' as a pointer to TStringRec
  AIR.DefinePointer('string', 'TStringRec', False);

  //----------------------------------------------------------------------------
  // Tiger_StrAlloc(ACapacity: UInt64): Pointer
  // Allocates a new string with the specified capacity.
  // RefCount = 1, Length = 0, Data buffer allocated and null-terminated.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrAlloc', vtPointer, False, plC, False)
     .Param('ACapacity', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     // Allocate the TStringRec struct (40 bytes)
     .Assign('LRec', AIR.CallExpr('Tiger_GetMem', [AIR.Int(40)]))
     // Allocate data buffer (capacity + 1 for null terminator)
     .Assign('LData', AIR.CallExpr('Tiger_GetMem', [AIR.Add(AIR.Var_('ACapacity'), AIR.Int(1))]));

  // Initialize fields
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'RefCount'), AIR.Int(1));

  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Length'), AIR.Int(0));

  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Capacity'), AIR.Var_('ACapacity'));

  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Data'), AIR.Var_('LData'));

  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Data16'), AIR.Nil_());

  // Null-terminate the empty string and return
  AIR.Call('memset', [AIR.Var_('LData'), AIR.Int(0), AIR.Int(1)])
     .Return(AIR.Var_('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrFree(AStr: Pointer)
  // Frees the string's data buffer and the TStringRec struct.
  // Does nothing if AStr is nil.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrFree', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LData', vtPointer)
     .Local('LData16', vtPointer)
     // if AStr = nil then exit
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr'), AIR.Nil_()))
        .Return()
     .IfEnd();

  // Free UTF-16 cache if present
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LData16', AIR.FieldExpr(LDeref, 'Data16'))
     .IfBegin(AIR.CmpNe(AIR.Var_('LData16'), AIR.Nil_()))
        .Call('Tiger_FreeMem', [AIR.Var_('LData16')])
     .IfEnd();

  // Free data buffer
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LData', AIR.FieldExpr(LDeref, 'Data'))
     .IfBegin(AIR.CmpNe(AIR.Var_('LData'), AIR.Nil_()))
        .Call('Tiger_FreeMem', [AIR.Var_('LData')])
     .IfEnd()
     // Free struct
     .Call('Tiger_FreeMem', [AIR.Var_('AStr')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrAddRef(AStr: Pointer)
  // Increments the reference count if not nil and not immortal (-1).
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrAddRef', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LRefCount', vtInt64)
     // if AStr = nil then exit
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr'), AIR.Nil_()))
        .Return()
     .IfEnd();

  // Get current refcount
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LRefCount', AIR.FieldExpr(LDeref, 'RefCount'))
     // if RefCount = -1 (immortal) then exit
     .IfBegin(AIR.CmpEq(AIR.Var_('LRefCount'), AIR.Int(-1)))
        .Return()
     .IfEnd();

  // Increment refcount
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'RefCount'), AIR.Add(AIR.Var_('LRefCount'), AIR.Int(1)))
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrRelease(AStr: Pointer)
  // Decrements the reference count. Frees the string if count reaches zero.
  // Does nothing if nil or immortal (-1).
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrRelease', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LRefCount', vtInt64)
     // if AStr = nil then exit
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr'), AIR.Nil_()))
        .Return()
     .IfEnd();

  // Get current refcount
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LRefCount', AIR.FieldExpr(LDeref, 'RefCount'))
     // if RefCount = -1 (immortal) then exit
     .IfBegin(AIR.CmpEq(AIR.Var_('LRefCount'), AIR.Int(-1)))
        .Return()
     .IfEnd()
     // Decrement refcount
     .Assign('LRefCount', AIR.Sub(AIR.Var_('LRefCount'), AIR.Int(1)));

  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'RefCount'), AIR.Var_('LRefCount'))
     // if RefCount = 0 then free
     .IfBegin(AIR.CmpEq(AIR.Var_('LRefCount'), AIR.Int(0)))
        .Call('Tiger_StrFree', [AIR.Var_('AStr')])
     .IfEnd()
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrFromLiteral(AData: Pointer; ALen: UInt64): Pointer
  // Creates a new string from static literal data.
  // Copies the data, sets length, null-terminates.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrFromLiteral', vtPointer, False, plC, False)
     .Param('AData', vtPointer)
     .Param('ALen', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     // Allocate string with capacity = length
     .Assign('LRec', AIR.CallExpr('Tiger_StrAlloc', [AIR.Var_('ALen')]));

  // Get data pointer
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.FieldExpr(LDeref, 'Data'))
     // Copy literal data
     .Call('memcpy', [AIR.Var_('LData'), AIR.Var_('AData'), AIR.Var_('ALen')]);

  // Set length
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Length'), AIR.Var_('ALen'))
     // Null terminate (memset at data+len)
     .Call('memset', [AIR.Add(AIR.Var_('LData'), AIR.Var_('ALen')), AIR.Int(0), AIR.Int(1)])
     .Return(AIR.Var_('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrFromChar(AChar: UInt64): Pointer
  // Creates a new string from a single character.
  // Allocates string, stores char, sets length to 1, null-terminates.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrFromChar', vtPointer, False, plC, False)
     .Param('AChar', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     // Allocate string with capacity = 1
     .Assign('LRec', AIR.CallExpr('Tiger_StrAlloc', [AIR.Int(1)]));

  // Get data pointer
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.FieldExpr(LDeref, 'Data'))
     // Store char byte at data[0]
     .Call('memset', [AIR.Var_('LData'), AIR.Var_('AChar'), AIR.Int(1)]);

  // Set length to 1
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Length'), AIR.Int(1))
     // Null terminate at data[1]
     .Call('memset', [AIR.Add(AIR.Var_('LData'), AIR.Int(1)), AIR.Int(0), AIR.Int(1)])
     .Return(AIR.Var_('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrConcat(AStr1, AStr2: Pointer): Pointer
  // Creates a new string that is the concatenation of AStr1 and AStr2.
  // Handles nil strings as empty.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrConcat', vtPointer, False, plC, False)
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
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr1'), AIR.Nil_()))
        .Assign('LLen1', AIR.Int(0))
     .ElseBegin();

  LDeref := AIR.Deref(AIR.Var_('AStr1'), 'TStringRec');
  AIR.Assign('LLen1', AIR.FieldExpr(LDeref, 'Length'))
     .IfEnd()
     // Get length of second string (0 if nil)
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr2'), AIR.Nil_()))
        .Assign('LLen2', AIR.Int(0))
     .ElseBegin();

  LDeref := AIR.Deref(AIR.Var_('AStr2'), 'TStringRec');
  AIR.Assign('LLen2', AIR.FieldExpr(LDeref, 'Length'))
     .IfEnd()
     // Total length
     .Assign('LTotalLen', AIR.Add(AIR.Var_('LLen1'), AIR.Var_('LLen2')))
     // Allocate new string
     .Assign('LRec', AIR.CallExpr('Tiger_StrAlloc', [AIR.Var_('LTotalLen')]));

  // Get data pointer of result
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.FieldExpr(LDeref, 'Data'))
     // Copy first string if not empty
     .IfBegin(AIR.CmpGt(AIR.Var_('LLen1'), AIR.Int(0)));

  LDeref := AIR.Deref(AIR.Var_('AStr1'), 'TStringRec');
  AIR.Assign('LData1', AIR.FieldExpr(LDeref, 'Data'))
        .Call('memcpy', [AIR.Var_('LData'), AIR.Var_('LData1'), AIR.Var_('LLen1')])
     .IfEnd()
     // Copy second string if not empty
     .IfBegin(AIR.CmpGt(AIR.Var_('LLen2'), AIR.Int(0)));

  LDeref := AIR.Deref(AIR.Var_('AStr2'), 'TStringRec');
  AIR.Assign('LData2', AIR.FieldExpr(LDeref, 'Data'))
        .Call('memcpy', [AIR.Add(AIR.Var_('LData'), AIR.Var_('LLen1')), AIR.Var_('LData2'), AIR.Var_('LLen2')])
     .IfEnd();

  // Set length and null-terminate
  LDeref := AIR.Deref(AIR.Var_('LRec'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Length'), AIR.Var_('LTotalLen'))
     .Call('memset', [AIR.Add(AIR.Var_('LData'), AIR.Var_('LTotalLen')), AIR.Int(0), AIR.Int(1)])
     .Return(AIR.Var_('LRec'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrAssign(ADest: Pointer; ASrc: Pointer)
  // Assignment with move semantics.
  // ADest is pointer to a string variable (pointer to pointer to TStringRec).
  // Releases old dest value, stores new value (transfers ownership).
  // NOTE: For variable-to-variable assignment (s2 := s1), caller must AddRef first!
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrAssign', vtVoid, False, plC, False)
     .Param('ADest', vtPointer)   // Pointer to the string variable
     .Param('ASrc', vtPointer)    // Source string (TStringRec pointer)
     .Local('LOld', vtPointer)
     // Get old value from destination
     .Assign('LOld', AIR.Deref(AIR.Var_('ADest'), vtPointer))
     // Release the old value
     .Call('Tiger_StrRelease', [AIR.Var_('LOld')]);

  // Store new value
  AIR.AssignToExpr(AIR.Deref(AIR.Var_('ADest'), vtPointer), AIR.Var_('ASrc'))
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrLen(AStr: Pointer): UInt64
  // Returns the length of the string in bytes. Returns 0 if nil.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrLen', vtUInt64, False, plC, False)
     .Param('AStr', vtPointer)
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr'), AIR.Nil_()))
        .Return(AIR.Int(0))
     .IfEnd();

  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Return(AIR.FieldExpr(LDeref, 'Length'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrCompare(AStr1, AStr2: Pointer): Int64
  // Lexicographic comparison. Returns <0 if AStr1<AStr2, 0 if equal, >0 if AStr1>AStr2.
  // Nil is treated as less than any non-nil string.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrCompare', vtInt64, False, plC, False)
     .Param('AStr1', vtPointer)
     .Param('AStr2', vtPointer)
     .Local('LLen1', vtUInt64)
     .Local('LLen2', vtUInt64)
     .Local('LMinLen', vtUInt64)
     .Local('LData1', vtPointer)
     .Local('LData2', vtPointer)
     .Local('LResult', vtInt32)
     // Check AStr1 nil
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr1'), AIR.Nil_()))
        // AStr1 is nil - check AStr2
        .IfBegin(AIR.CmpEq(AIR.Var_('AStr2'), AIR.Nil_()))
           .Return(AIR.Int(0))   // Both nil -> equal
        .IfEnd()
        .Return(AIR.Int(-1))     // AStr1 nil, AStr2 not nil -> less than
     .IfEnd()
     // AStr1 not nil - check AStr2
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr2'), AIR.Nil_()))
        .Return(AIR.Int(1))      // AStr1 not nil, AStr2 nil -> greater than
     .IfEnd();

  // Get lengths
  LDeref := AIR.Deref(AIR.Var_('AStr1'), 'TStringRec');
  AIR.Assign('LLen1', AIR.FieldExpr(LDeref, 'Length'));
  LDeref := AIR.Deref(AIR.Var_('AStr2'), 'TStringRec');
  AIR.Assign('LLen2', AIR.FieldExpr(LDeref, 'Length'));

  // Get data pointers
  LDeref := AIR.Deref(AIR.Var_('AStr1'), 'TStringRec');
  AIR.Assign('LData1', AIR.FieldExpr(LDeref, 'Data'));
  LDeref := AIR.Deref(AIR.Var_('AStr2'), 'TStringRec');
  AIR.Assign('LData2', AIR.FieldExpr(LDeref, 'Data'));

  // LMinLen = min(LLen1, LLen2)
  AIR.IfBegin(AIR.CmpLt(AIR.Var_('LLen1'), AIR.Var_('LLen2')))
       .Assign('LMinLen', AIR.Var_('LLen1'))
     .ElseBegin()
       .Assign('LMinLen', AIR.Var_('LLen2'))
     .IfEnd();

  // LResult = memcmp(LData1, LData2, LMinLen)
  AIR.Assign('LResult', AIR.CallExpr('memcmp', [AIR.Var_('LData1'), AIR.Var_('LData2'), AIR.Var_('LMinLen')]))
     // If memcmp result < 0, return -1
     .IfBegin(AIR.CmpLt(AIR.Var_('LResult'), AIR.Int(0)))
        .Return(AIR.Int(-1))
     .IfEnd()
     // If memcmp result > 0, return 1
     .IfBegin(AIR.CmpGt(AIR.Var_('LResult'), AIR.Int(0)))
        .Return(AIR.Int(1))
     .IfEnd()
     // memcmp returned 0, compare by length
     .IfBegin(AIR.CmpLt(AIR.Var_('LLen1'), AIR.Var_('LLen2')))
        .Return(AIR.Int(-1))
     .IfEnd()
     .IfBegin(AIR.CmpGt(AIR.Var_('LLen1'), AIR.Var_('LLen2')))
        .Return(AIR.Int(1))
     .IfEnd()
     .Return(AIR.Int(0))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrData(AStr: Pointer): Pointer
  // Returns pointer to the UTF-8 data (for pchar() conversion).
  // Returns nil if string is nil.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrData', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr'), AIR.Nil_()))
        .Return(AIR.Nil_())
     .IfEnd();

  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Return(AIR.FieldExpr(LDeref, 'Data'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrUtf16(AStr: Pointer): Pointer
  // Returns pointer to cached UTF-16 data. Converts from UTF-8 on first call.
  // Returns nil if string is nil.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrUtf16', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LData16', vtPointer)
     .Local('LData', vtPointer)
     .Local('LLen', vtUInt64)
     .Local('LWideLen', vtInt32)
     .Local('LBuf', vtPointer)
     // if AStr = nil then return nil
     .IfBegin(AIR.CmpEq(AIR.Var_('AStr'), AIR.Nil_()))
        .Return(AIR.Nil_())
     .IfEnd();

  // Check if already cached
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LData16', AIR.FieldExpr(LDeref, 'Data16'))
     .IfBegin(AIR.CmpNe(AIR.Var_('LData16'), AIR.Nil_()))
        .Return(AIR.Var_('LData16'))
     .IfEnd();

  // Get UTF-8 data pointer and length
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LData', AIR.FieldExpr(LDeref, 'Data'));

  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.Assign('LLen', AIR.FieldExpr(LDeref, 'Length'));

  // Query required buffer size (CP_UTF8 = 65001)
  AIR.Assign('LWideLen', AIR.CallExpr('MultiByteToWideChar',
        [AIR.Int(65001), AIR.Int(0), AIR.Var_('LData'), AIR.Var_('LLen'), AIR.Nil_(), AIR.Int(0)]))
     // Allocate buffer: (LWideLen + 1) * 2 bytes for UTF-16 + null terminator
     .Assign('LBuf', AIR.CallExpr('Tiger_GetMem',
        [AIR.Mul(AIR.Add(AIR.Var_('LWideLen'), AIR.Int(1)), AIR.Int(2))]))
     // Convert UTF-8 to UTF-16
     .Call('MultiByteToWideChar',
        [AIR.Int(65001), AIR.Int(0), AIR.Var_('LData'), AIR.Var_('LLen'), AIR.Var_('LBuf'), AIR.Var_('LWideLen')]);

  // Null-terminate (2 bytes of zero)
  AIR.Call('memset', [AIR.Add(AIR.Var_('LBuf'), AIR.Mul(AIR.Var_('LWideLen'), AIR.Int(2))), AIR.Int(0), AIR.Int(2)]);

  // Store in cache
  LDeref := AIR.Deref(AIR.Var_('AStr'), 'TStringRec');
  AIR.AssignToExpr(AIR.FieldExpr(LDeref, 'Data16'), AIR.Var_('LBuf'))
     .Return(AIR.Var_('LBuf'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrLiteralUtf16(AData: Pointer; ALen: UInt64): Pointer
  // Converts UTF-8 literal data to UTF-16. Returns pointer to allocated buffer.
  // Unlike Tiger_StrUtf16, this takes raw data pointer + length (no TTigerStringRec).
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_StrLiteralUtf16', vtPointer, False, plC, False)
     .Param('AData', vtPointer)
     .Param('ALen', vtUInt64)
     .Local('LWideLen', vtInt32)
     .Local('LBuf', vtPointer)
     // Query required buffer size (CP_UTF8 = 65001)
     .Assign('LWideLen', AIR.CallExpr('MultiByteToWideChar',
        [AIR.Int(65001), AIR.Int(0), AIR.Var_('AData'), AIR.Var_('ALen'), AIR.Nil_(), AIR.Int(0)]))
     // Allocate buffer: (LWideLen + 1) * 2 bytes
     .Assign('LBuf', AIR.CallExpr('Tiger_GetMem',
        [AIR.Mul(AIR.Add(AIR.Var_('LWideLen'), AIR.Int(1)), AIR.Int(2))]))
     // Convert UTF-8 to UTF-16
     .Call('MultiByteToWideChar',
        [AIR.Int(65001), AIR.Int(0), AIR.Var_('AData'), AIR.Var_('ALen'), AIR.Var_('LBuf'), AIR.Var_('LWideLen')])
     // Null-terminate (2 bytes of zero)
     .Call('memset', [AIR.Add(AIR.Var_('LBuf'), AIR.Mul(AIR.Var_('LWideLen'), AIR.Int(2))), AIR.Int(0), AIR.Int(2)])
     .Return(AIR.Var_('LBuf'))
  .EndFunc();
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
  AIR.BeginFunc('Tiger_InitExceptions', vtVoid, False, plC, False)
     .Assign('Tiger_TlsExcCode', AIR.CallExpr('TlsAlloc', []))
     .Assign('Tiger_TlsExcMsg', AIR.CallExpr('TlsAlloc', []))
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_SetException(ACode: Int32; AMsg: Pointer)
  // Internal helper: stores code and copies message to TLS.
  // Frees old message if present.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_SetException', vtVoid, False, plC, False)
     .Param('ACode', vtInt32)
     .Param('AMsg', vtPointer)
     .Local('LOldMsg', vtPointer)
     .Local('LLen', vtUInt64)
     .Local('LNewMsg', vtPointer)
     // Get and free old message if any
     .Assign('LOldMsg', AIR.CallExpr('TlsGetValue', [AIR.Var_('Tiger_TlsExcMsg')]))
     .IfBegin(AIR.CmpNe(AIR.Var_('LOldMsg'), AIR.Nil_()))
        .Call('Tiger_FreeMem', [AIR.Var_('LOldMsg')])
     .IfEnd()
     // Copy new message if not nil
     .IfBegin(AIR.CmpNe(AIR.Var_('AMsg'), AIR.Nil_()))
        .Assign('LLen', AIR.CallExpr('strlen', [AIR.Var_('AMsg')]))
        .Assign('LNewMsg', AIR.CallExpr('Tiger_GetMem', [AIR.Add(AIR.Var_('LLen'), AIR.Int(1))]))
        .Call('memcpy', [AIR.Var_('LNewMsg'), AIR.Var_('AMsg'), AIR.Add(AIR.Var_('LLen'), AIR.Int(1))])
        .Call('TlsSetValue', [AIR.Var_('Tiger_TlsExcMsg'), AIR.Var_('LNewMsg')])
     .ElseBegin()
        .Call('TlsSetValue', [AIR.Var_('Tiger_TlsExcMsg'), AIR.Nil_()])
     .IfEnd()
     // Store code (cast to pointer for TLS)
     .Call('TlsSetValue', [AIR.Var_('Tiger_TlsExcCode'), AIR.Var_('ACode')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_Raise(AMsg: Pointer)
  // Raises exception with default code (1) and message.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_Raise', vtVoid, False, plC, False)
     .Param('AMsg', vtPointer)
     .Call('Tiger_SetException', [AIR.Int(1), AIR.Var_('AMsg')])
     // RaiseException(0xE0505858, 0, 0, nil) - 'PXX' custom exception
     .Call('RaiseException', [AIR.Int($E0505858), AIR.Int(0), AIR.Int(0), AIR.Nil_()])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_RaiseCode(ACode: Int32; AMsg: Pointer)
  // Raises exception with custom code and message.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_RaiseCode', vtVoid, False, plC, False)
     .Param('ACode', vtInt32)
     .Param('AMsg', vtPointer)
     .Call('Tiger_SetException', [AIR.Var_('ACode'), AIR.Var_('AMsg')])
     // RaiseException(0xE0505858, 0, 0, nil) - 'PXX' custom exception
     .Call('RaiseException', [AIR.Int($E0505858), AIR.Int(0), AIR.Int(0), AIR.Nil_()])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptionCode(): Int32
  // Returns the current exception code from TLS.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_GetExceptionCode', vtInt32, False, plC, False)
     .Return(AIR.CallExpr('TlsGetValue', [AIR.Var_('Tiger_TlsExcCode')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptionMessage(): Pointer
  // Returns pointer to the current exception message from TLS.
  //----------------------------------------------------------------------------
  AIR.BeginFunc('Tiger_GetExceptionMessage', vtPointer, False, plC, False)
     .Return(AIR.CallExpr('TlsGetValue', [AIR.Var_('Tiger_TlsExcMsg')]))
  .EndFunc();
end;

//==============================================================================
// TTigerRuntime - Add All
//==============================================================================


end.
