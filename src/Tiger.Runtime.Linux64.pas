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
    procedure AddTypes(const AIR: TTigerIR); override;

    /// <summary>Exception runtime — stub for Phase 2.</summary>
    procedure AddExceptions(const AIR: TTigerIR); override;

    /// <summary>Command-line arguments — stub for now.</summary>
    procedure AddCommandLine(const AIR: TTigerIR); override;
  end;

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
  // Free command line (no-op on Linux, but keeps API consistent with Win64)
  AIR.Call('Tiger_FreeCommandLine', []);
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
var
  LDeref: TTigerIRExpr;
begin
  //----------------------------------------------------------------------------
  // Import C runtime for memory operations
  //----------------------------------------------------------------------------
  AIR.Import('libc.so.6', 'memcpy', [vtPointer, vtPointer, vtUInt64], vtPointer, False);
  AIR.Import('libc.so.6', 'memset', [vtPointer, vtInt32, vtUInt64], vtPointer, False);
  AIR.Import('libc.so.6', 'memcmp', [vtPointer, vtPointer, vtUInt64], vtInt32, False);

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
  // Conditionally releases a managed string during shared object unload.
  // Only releases when AReason = 0 (equivalent to DLL_PROCESS_DETACH).
  // Called by SSA cleanup pass for managed globals in DllMain.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_ReleaseOnDetach', vtVoid, False, plC, False)
     .Param('AReason', vtInt32)
     .Param('AStr', vtPointer)
     // Only release on unload (reason = 0)
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
  // On Linux, returns the UTF-8 data pointer directly (Linux is UTF-8 native).
  // Returns nil if string is nil.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrUtf16', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Null())
     .EndIf();

  // On Linux, just return the UTF-8 data pointer
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Return(AIR.GetField(LDeref, 'Data'))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_StrLiteralUtf16(AData: Pointer; ALen: UInt64): Pointer
  // On Linux, returns the data pointer directly (no UTF-16 conversion needed).
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_StrLiteralUtf16', vtPointer, False, plC, False)
     .Param('AData', vtPointer)
     .Param('ALen', vtUInt64)
     // On Linux, just return the UTF-8 data pointer as-is
     .Return(AIR.Get('AData'))
  .EndFunc();
end;

//==============================================================================
// TTigerRuntime - String Type Definitions
//==============================================================================
// Defines TStringRec and 'string' pointer type only.
// Called early in TTiger.Create() so user code can reference 'string' type.
// Safe to call multiple times - checks if types already exist.
//==============================================================================

procedure TTigerLinux64Runtime.AddTypes(const AIR: TTigerIR);
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

procedure TTigerLinux64Runtime.AddExceptions(const AIR: TTigerIR);
const
  // Linux signal numbers
  SIGFPE  = 8;   // Floating point exception (includes div by zero)
  SIGSEGV = 11;  // Segmentation fault
  SIGILL  = 4;   // Illegal instruction
  SIGBUS  = 7;   // Bus error

  // Offsets within TigerExceptFrame (216 bytes total)
  FRAME_PREV     = 0;    // PrevFrame: Pointer (8 bytes)
  FRAME_JMPBUF   = 8;    // JmpBuf: sigjmp_buf (200 bytes)
  FRAME_TYPE     = 208;  // FrameType: Int32 (4 bytes)
  FRAME_SIZE     = 216;  // Total frame size (aligned to 8)

  // sigaction structure size (glibc x86-64)
  // sa_handler(8) + sa_mask(128) + sa_flags(4) + padding(4) + sa_restorer(8) = 152
  SIGACTION_SIZE = 152;
begin
  //----------------------------------------------------------------------------
  // Import pthread TLS functions
  //----------------------------------------------------------------------------
  AIR.Import('libc.so.6', 'pthread_key_create', [vtPointer, vtPointer], vtInt32, False);
  AIR.Import('libc.so.6', 'pthread_getspecific', [vtUInt32], vtPointer, False);
  AIR.Import('libc.so.6', 'pthread_setspecific', [vtUInt32, vtPointer], vtInt32, False);

  //----------------------------------------------------------------------------
  // Import signal handling functions
  //----------------------------------------------------------------------------
  AIR.Import('libc.so.6', 'sigaction', [vtInt32, vtPointer, vtPointer], vtInt32, False);
  AIR.Import('libc.so.6', '__sigsetjmp', [vtPointer, vtInt32], vtInt32, False);
  AIR.Import('libc.so.6', 'siglongjmp', [vtPointer, vtInt32], vtVoid, False);

  //----------------------------------------------------------------------------
  // Import string functions (for exception message copying)
  //----------------------------------------------------------------------------
  AIR.Import('libc.so.6', 'strlen', [vtPointer], vtUInt64, False);
  // memcpy already imported in AddStrings

  //----------------------------------------------------------------------------
  // Global variables for TLS slot keys
  //----------------------------------------------------------------------------
  AIR.Global('Tiger_TlsExcChain', vtUInt32);  // TLS key for exception frame chain
  AIR.Global('Tiger_TlsExcCode', vtUInt32);   // TLS key for exception code
  AIR.Global('Tiger_TlsExcMsg', vtUInt32);    // TLS key for exception message

  //----------------------------------------------------------------------------
  // Tiger_InitExceptions()
  // Allocates TLS keys. Called at process startup.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_InitExceptions', vtVoid, False, plC, False)
     // pthread_key_create(&Tiger_TlsExcChain, NULL)
     .Call('pthread_key_create', [AIR.AddrOf('Tiger_TlsExcChain'), AIR.Null()])
     // pthread_key_create(&Tiger_TlsExcCode, NULL)
     .Call('pthread_key_create', [AIR.AddrOf('Tiger_TlsExcCode'), AIR.Null()])
     // pthread_key_create(&Tiger_TlsExcMsg, NULL)
     .Call('pthread_key_create', [AIR.AddrOf('Tiger_TlsExcMsg'), AIR.Null()])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_SignalHandler(ASigNum: Int32; AInfo: Pointer; AContext: Pointer)
  // Signal handler for hardware exceptions. Jumps to current except frame.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_SignalHandler', vtVoid, False, plC, False)
     .Param('ASigNum', vtInt32)
     .Param('AInfo', vtPointer)
     .Param('AContext', vtPointer)
     .Local('LFrame', vtPointer)
     .Local('LJmpBuf', vtPointer)
     // Store signal number as exception code, no message
     .Call('Tiger_SetException', [AIR.Get('ASigNum'), AIR.Null()])
     // Get current exception frame
     .Assign('LFrame', AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcChain')]))
     // Calculate JmpBuf address (frame + 8)
     .Assign('LJmpBuf', AIR.Add(AIR.Get('LFrame'), AIR.Int64(FRAME_JMPBUF)))
     // Jump to recovery point with value 1
     .Call('siglongjmp', [AIR.Get('LJmpBuf'), AIR.Int64(1)])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_InitSignals()
  // Installs signal handlers for hardware exceptions. Called at process startup.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_InitSignals', vtVoid, False, plC, False)
     .Local('LSigAction', vtPointer)
     // Allocate sigaction struct (152 bytes, zeroed)
     .Assign('LSigAction', AIR.Invoke('Tiger_GetMem', [AIR.Int64(SIGACTION_SIZE)]))
     .Call('memset', [AIR.Get('LSigAction'), AIR.Int64(0), AIR.Int64(SIGACTION_SIZE)])
     // Set sa_handler = Tiger_SignalHandler (store function pointer at offset 0)
     .AssignTo(AIR.Deref(AIR.Get('LSigAction'), vtPointer), AIR.FuncAddr('Tiger_SignalHandler'))
     // Install handler for SIGFPE (div by zero)
     .Call('sigaction', [AIR.Int64(SIGFPE), AIR.Get('LSigAction'), AIR.Null()])
     // Install handler for SIGSEGV (access violation)
     .Call('sigaction', [AIR.Int64(SIGSEGV), AIR.Get('LSigAction'), AIR.Null()])
     // Install handler for SIGILL (illegal instruction)
     .Call('sigaction', [AIR.Int64(SIGILL), AIR.Get('LSigAction'), AIR.Null()])
     // Install handler for SIGBUS (bus error)
     .Call('sigaction', [AIR.Int64(SIGBUS), AIR.Get('LSigAction'), AIR.Null()])
     // Free the sigaction struct
     .Call('Tiger_FreeMem', [AIR.Get('LSigAction')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_PushExceptFrame(AFrame: Pointer)
  // Pushes an exception frame onto the TLS chain.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_PushExceptFrame', vtVoid, False, plC, False)
     .Param('AFrame', vtPointer)
     .Local('LPrev', vtPointer)
     // Get current chain head
     .Assign('LPrev', AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcChain')]))
     // Store previous frame pointer at offset 0 of new frame
     .AssignTo(AIR.Deref(AIR.Get('AFrame'), vtPointer), AIR.Get('LPrev'))
     // Set new frame as chain head
     .Call('pthread_setspecific', [AIR.Get('Tiger_TlsExcChain'), AIR.Get('AFrame')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_PopExceptFrame()
  // Pops the current exception frame from the TLS chain.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_PopExceptFrame', vtVoid, False, plC, False)
     .Local('LFrame', vtPointer)
     .Local('LPrev', vtPointer)
     // Get current frame
     .Assign('LFrame', AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcChain')]))
     // Get previous frame from offset 0
     .Assign('LPrev', AIR.Deref(AIR.Get('LFrame')))
     // Set previous as new chain head
     .Call('pthread_setspecific', [AIR.Get('Tiger_TlsExcChain'), AIR.Get('LPrev')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptFrame(): Pointer
  // Returns the current exception frame from TLS.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetExceptFrame', vtPointer, False, plC, False)
     .Return(AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcChain')]))
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
     .Assign('LOldMsg', AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcMsg')]))
     .&If(AIR.Ne(AIR.Get('LOldMsg'), AIR.Null()))
        .Call('Tiger_FreeMem', [AIR.Get('LOldMsg')])
     .EndIf()
     // Copy new message if not nil
     .&If(AIR.Ne(AIR.Get('AMsg'), AIR.Null()))
        .Assign('LLen', AIR.Invoke('strlen', [AIR.Get('AMsg')]))
        .Assign('LNewMsg', AIR.Invoke('Tiger_GetMem', [AIR.Add(AIR.Get('LLen'), AIR.Int64(1))]))
        .Call('memcpy', [AIR.Get('LNewMsg'), AIR.Get('AMsg'), AIR.Add(AIR.Get('LLen'), AIR.Int64(1))])
        .Call('pthread_setspecific', [AIR.Get('Tiger_TlsExcMsg'), AIR.Get('LNewMsg')])
     .&Else()
        .Call('pthread_setspecific', [AIR.Get('Tiger_TlsExcMsg'), AIR.Null()])
     .EndIf()
     // Store code (cast to pointer for TLS)
     .Call('pthread_setspecific', [AIR.Get('Tiger_TlsExcCode'), AIR.Get('ACode')])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_Raise(AMsg: Pointer)
  // Raises exception with default code (1) and message.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_Raise', vtVoid, False, plC, False)
     .Param('AMsg', vtPointer)
     .Local('LFrame', vtPointer)
     .Local('LJmpBuf', vtPointer)
     .Call('Tiger_SetException', [AIR.Int64(1), AIR.Get('AMsg')])
     // Get current exception frame
     .Assign('LFrame', AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcChain')]))
     // Calculate JmpBuf address (frame + 8)
     .Assign('LJmpBuf', AIR.Add(AIR.Get('LFrame'), AIR.Int64(FRAME_JMPBUF)))
     // Jump to recovery point
     .Call('siglongjmp', [AIR.Get('LJmpBuf'), AIR.Int64(1)])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_RaiseCode(ACode: Int32; AMsg: Pointer)
  // Raises exception with custom code and message.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_RaiseCode', vtVoid, False, plC, False)
     .Param('ACode', vtInt32)
     .Param('AMsg', vtPointer)
     .Local('LFrame', vtPointer)
     .Local('LJmpBuf', vtPointer)
     .Call('Tiger_SetException', [AIR.Get('ACode'), AIR.Get('AMsg')])
     // Get current exception frame
     .Assign('LFrame', AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcChain')]))
     // Calculate JmpBuf address (frame + 8)
     .Assign('LJmpBuf', AIR.Add(AIR.Get('LFrame'), AIR.Int64(FRAME_JMPBUF)))
     // Jump to recovery point
     .Call('siglongjmp', [AIR.Get('LJmpBuf'), AIR.Int64(1)])
     .Return()
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptionCode(): Int32
  // Returns the current exception code from TLS.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetExceptionCode', vtInt32, False, plC, False)
     .Return(AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcCode')]))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_GetExceptionMessage(): Pointer
  // Returns pointer to the current exception message from TLS.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_GetExceptionMessage', vtPointer, False, plC, False)
     .Return(AIR.Invoke('pthread_getspecific', [AIR.Get('Tiger_TlsExcMsg')]))
  .EndFunc();
end;

//==============================================================================
// TTigerLinux64Runtime - Command Line
//==============================================================================

procedure TTigerLinux64Runtime.AddCommandLine(const AIR: TTigerIR);
begin
  //----------------------------------------------------------------------------
  // Globals for argc/argv storage
  // On Linux, argv from _start is already an array of UTF-8 C string pointers.
  //----------------------------------------------------------------------------
  AIR.Global('Tiger_Argc', vtInt64);
  AIR.Global('Tiger_Argv', vtPointer);

  //----------------------------------------------------------------------------
  // Tiger_InitCommandLine(AArgc: Int64, AArgv: Pointer)
  // Called from _start with argc/argv from the stack.
  // Simply stores them to globals - no conversion needed on Linux.
  //----------------------------------------------------------------------------
  AIR.Import('libc.so.6', 'realpath', [vtPointer, vtPointer], vtPointer, False);

  AIR.Func('Tiger_InitCommandLine', vtVoid, False, plC, False)
     .Param('AArgc', vtInt64)
     .Param('AArgv', vtPointer)
     .Local('LBuf', vtPointer)
     .Local('LResolved', vtPointer);
  AIR.Assign('Tiger_Argc', AIR.Get('AArgc'));
  AIR.Assign('Tiger_Argv', AIR.Get('AArgv'));
  AIR.Assign('LBuf', AIR.Invoke('Tiger_GetMem', [AIR.Int64(256)]));
  AIR.Assign('LResolved', AIR.Invoke('realpath',
     [AIR.Str('/proc/self/exe'), AIR.Get('LBuf')]));
  // Replace argv[0] with resolved absolute path
  AIR.AssignTo(
     AIR.Deref(AIR.Get('Tiger_Argv'), vtPointer),
     AIR.Get('LResolved'));
  AIR.Return();
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_FreeCommandLine()
  // Frees the heap-allocated absolute path stored in argv[0] by InitCommandLine.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_FreeCommandLine', vtVoid, False, plC, False);
  AIR.Call('Tiger_FreeMem', [AIR.Deref(AIR.Get('Tiger_Argv'), vtPointer)]);
  AIR.Return();
  AIR.EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ParamCount(): Int64
  // Returns argc - 1 (Pascal semantics: excludes program name).
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_ParamCount', vtInt64, False, plC, False)
     .Return(AIR.Sub(AIR.Get('Tiger_Argc'), AIR.Int64(1)))
  .EndFunc();

  //----------------------------------------------------------------------------
  // Tiger_ParamStr(AIndex: Int64): Pointer
  // Returns argv[AIndex] with bounds checking.
  //----------------------------------------------------------------------------
  AIR.Func('Tiger_ParamStr', vtPointer, False, plC, False)
     .Param('AIndex', vtInt64);

  // Bounds check: index < 0
  AIR.&If(AIR.Lt(AIR.Get('AIndex'), AIR.Int64(0)));
  AIR.Return(AIR.Null());
  AIR.EndIf();

  // Bounds check: index >= argc
  AIR.&If(AIR.Ge(AIR.Get('AIndex'), AIR.Get('Tiger_Argc')));
  AIR.Return(AIR.Null());
  AIR.EndIf();

  // Return argv[AIndex] - dereference pointer at Tiger_Argv + AIndex * 8
  AIR.Return(AIR.Deref(
     AIR.Add(AIR.Get('Tiger_Argv'), AIR.Mul(AIR.Get('AIndex'), AIR.Int64(8))),
     vtPointer));
  AIR.EndFunc();
end;

end.
