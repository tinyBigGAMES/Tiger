{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Runtime.MacOS64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Types,
  Tiger.Common,
  Tiger.IR,
  Tiger.Runtime;

type
  //============================================================================
  // TTigerMacOS64Runtime - macOS ARM64 (Apple Silicon) runtime library injection.
  // Uses libSystem.B.dylib for exit, printf, malloc, free, etc.
  //============================================================================

  { TTigerMacOS64Runtime }
  TTigerMacOS64Runtime = class(TTigerRuntime)
  public
    procedure AddSystem(const AIR: TTigerIR; const AOptLevel: Integer); override;
    procedure AddIO(const AIR: TTigerIR); override;
    procedure AddMemory(const AIR: TTigerIR; const AOptLevel: Integer); override;
    procedure AddStrings(const AIR: TTigerIR); override;
    procedure AddTypes(const AIR: TTigerIR); override;
    procedure AddExceptions(const AIR: TTigerIR); override;
  end;

implementation

const
  LIB_SYSTEM = 'libSystem.B.dylib';

//==============================================================================
// TTigerMacOS64Runtime - System
//==============================================================================

procedure TTigerMacOS64Runtime.AddSystem(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  // Use _exit() instead of exit(): _exit() terminates immediately without running
  // atexit handlers. exit() can hang when libSystem's atexit handlers block.
  AIR.Import(LIB_SYSTEM, '_exit', [vtInt32], vtVoid, False);

  AIR.Func('Tiger_Halt', vtVoid, False, plC, False)
     .Param('AExitCode', vtInt32);
  if AOptLevel = 0 then
    AIR.Call('Tiger_ReportLeaks', []);
  AIR.Call('_exit', [AIR.Get('AExitCode')])
  .EndFunc();
end;

//==============================================================================
// TTigerMacOS64Runtime - I/O
//==============================================================================

procedure TTigerMacOS64Runtime.AddIO(const AIR: TTigerIR);
begin
  AIR.Import(LIB_SYSTEM, 'printf', [vtPointer], vtInt32, True);
  // write() - dyld adds leading underscore when resolving to _write in libSystem
  AIR.Import(LIB_SYSTEM, 'write', [vtInt32, vtPointer, vtUInt64], vtInt64, False);

  AIR.Func('Tiger_InitConsole', vtVoid, False, plC, False)
     .Return()
  .EndFunc();
end;

//==============================================================================
// TTigerMacOS64Runtime - Memory
//==============================================================================

procedure TTigerMacOS64Runtime.AddMemory(const AIR: TTigerIR; const AOptLevel: Integer);
begin
  AIR.Import(LIB_SYSTEM, 'malloc', [vtUInt64], vtPointer, False);
  AIR.Import(LIB_SYSTEM, 'free', [vtPointer], vtVoid, False);
  AIR.Import(LIB_SYSTEM, 'realloc', [vtPointer, vtUInt64], vtPointer, False);
  AIR.Import(LIB_SYSTEM, 'calloc', [vtUInt64, vtUInt64], vtPointer, False);

  if AOptLevel = 0 then
  begin
    AIR.Global('Tiger_AllocCount', vtUInt64);
    AIR.Global('Tiger_FreeCount', vtUInt64);
  end;

  AIR.Func('Tiger_GetMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LResult', vtPointer);

  AIR.Assign('LResult', AIR.Invoke('malloc', [AIR.Get('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Get('Tiger_AllocCount'), AIR.Int64(1)));

  AIR.Return(AIR.Get('LResult'));
  AIR.EndFunc();

  AIR.Func('Tiger_FreeMem', vtVoid, False, plC, False)
     .Param('APtr', vtPointer);

  AIR.Call('free', [AIR.Get('APtr')]);

  if AOptLevel = 0 then
    AIR.Assign('Tiger_FreeCount', AIR.Add(AIR.Get('Tiger_FreeCount'), AIR.Int64(1)));

  AIR.Return();
  AIR.EndFunc();

  AIR.Func('Tiger_ReAllocMem', vtPointer, False, plC, False)
     .Param('APtr', vtPointer)
     .Param('ANewSize', vtUInt64)
     .Return(AIR.Invoke('realloc', [AIR.Get('APtr'), AIR.Get('ANewSize')]))
  .EndFunc();

  // Tiger_AllocMem(ASize: UInt64): Pointer - zero-initialized via calloc (same as Linux)
  AIR.Func('Tiger_AllocMem', vtPointer, False, plC, False)
     .Param('ASize', vtUInt64)
     .Local('LResult', vtPointer);

  AIR.Assign('LResult', AIR.Invoke('calloc', [AIR.Int64(1), AIR.Get('ASize')]));

  if AOptLevel = 0 then
    AIR.Assign('Tiger_AllocCount', AIR.Add(AIR.Get('Tiger_AllocCount'), AIR.Int64(1)));

  AIR.Return(AIR.Get('LResult'));
  AIR.EndFunc();

  // Tiger_ReportLeaks: same as Win/Linux - printf with real alloc/free/leak counts.
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
// TTigerMacOS64Runtime - Strings (same layout as Linux/Win: TStringRec, refcount)
//==============================================================================

procedure TTigerMacOS64Runtime.AddStrings(const AIR: TTigerIR);
var
  LDeref: TTigerIRExpr;
begin
  AIR.Import(LIB_SYSTEM, 'memcpy', [vtPointer, vtPointer, vtUInt64], vtPointer, False);
  AIR.Import(LIB_SYSTEM, 'memset', [vtPointer, vtInt32, vtUInt64], vtPointer, False);
  AIR.Import(LIB_SYSTEM, 'memcmp', [vtPointer, vtPointer, vtUInt64], vtInt32, False);

  AddTypes(AIR);

  AIR.Func('Tiger_StrAlloc', vtPointer, False, plC, False)
     .Param('ACapacity', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     .Assign('LRec', AIR.Invoke('Tiger_GetMem', [AIR.Int64(40)]))
     .Assign('LData', AIR.Invoke('Tiger_GetMem', [AIR.Add(AIR.Get('ACapacity'), AIR.Int64(1))]));

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
  AIR.Call('memset', [AIR.Get('LData'), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  AIR.Func('Tiger_StrFree', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LData', vtPointer)
     .Local('LData16', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return()
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LData16', AIR.GetField(LDeref, 'Data16'))
     .&If(AIR.Ne(AIR.Get('LData16'), AIR.Null()))
        .Call('Tiger_FreeMem', [AIR.Get('LData16')])
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     .&If(AIR.Ne(AIR.Get('LData'), AIR.Null()))
        .Call('Tiger_FreeMem', [AIR.Get('LData')])
     .EndIf()
     .Call('Tiger_FreeMem', [AIR.Get('AStr')])
     .Return()
  .EndFunc();

  AIR.Func('Tiger_StrAddRef', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LRefCount', vtInt64)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return()
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LRefCount', AIR.GetField(LDeref, 'RefCount'))
     .&If(AIR.Eq(AIR.Get('LRefCount'), AIR.Int64(-1)))
        .Return()
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'RefCount'), AIR.Add(AIR.Get('LRefCount'), AIR.Int64(1)))
     .Return()
  .EndFunc();

  AIR.Func('Tiger_StrRelease', vtVoid, False, plC, False)
     .Param('AStr', vtPointer)
     .Local('LRefCount', vtInt64)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return()
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Assign('LRefCount', AIR.GetField(LDeref, 'RefCount'))
     .&If(AIR.Eq(AIR.Get('LRefCount'), AIR.Int64(-1)))
        .Return()
     .EndIf()
     .Assign('LRefCount', AIR.Sub(AIR.Get('LRefCount'), AIR.Int64(1)));
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'RefCount'), AIR.Get('LRefCount'))
     .&If(AIR.Eq(AIR.Get('LRefCount'), AIR.Int64(0)))
        .Call('Tiger_StrFree', [AIR.Get('AStr')])
     .EndIf()
     .Return()
  .EndFunc();

  AIR.Func('Tiger_ReleaseOnDetach', vtVoid, False, plC, False)
     .Param('AReason', vtInt32)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AReason'), AIR.Int32(0)))
        .Call('Tiger_StrRelease', [AIR.Get('AStr')])
     .EndIf()
     .Return()
  .EndFunc();

  AIR.Func('Tiger_StrFromLiteral', vtPointer, False, plC, False)
     .Param('AData', vtPointer)
     .Param('ALen', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     .Assign('LRec', AIR.Invoke('Tiger_StrAlloc', [AIR.Get('ALen')]));
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     .Call('memcpy', [AIR.Get('LData'), AIR.Get('AData'), AIR.Get('ALen')]);
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Get('ALen'))
     .Call('memset', [AIR.Add(AIR.Get('LData'), AIR.Get('ALen')), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  AIR.Func('Tiger_StrFromChar', vtPointer, False, plC, False)
     .Param('AChar', vtUInt64)
     .Local('LRec', vtPointer)
     .Local('LData', vtPointer)
     .Assign('LRec', AIR.Invoke('Tiger_StrAlloc', [AIR.Int64(1)]));
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     .Call('memset', [AIR.Get('LData'), AIR.Get('AChar'), AIR.Int64(1)]);
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Int64(1))
     .Call('memset', [AIR.Add(AIR.Get('LData'), AIR.Int64(1)), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

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
     .&If(AIR.Eq(AIR.Get('AStr1'), AIR.Null()))
        .Assign('LLen1', AIR.Int64(0))
     .&Else();
  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LLen1', AIR.GetField(LDeref, 'Length'))
     .EndIf()
     .&If(AIR.Eq(AIR.Get('AStr2'), AIR.Null()))
        .Assign('LLen2', AIR.Int64(0))
     .&Else();
  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LLen2', AIR.GetField(LDeref, 'Length'))
     .EndIf()
     .Assign('LTotalLen', AIR.Add(AIR.Get('LLen1'), AIR.Get('LLen2')))
     .Assign('LRec', AIR.Invoke('Tiger_StrAlloc', [AIR.Get('LTotalLen')]));
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.Assign('LData', AIR.GetField(LDeref, 'Data'))
     .&If(AIR.Gt(AIR.Get('LLen1'), AIR.Int64(0)));
  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LData1', AIR.GetField(LDeref, 'Data'))
        .Call('memcpy', [AIR.Get('LData'), AIR.Get('LData1'), AIR.Get('LLen1')])
     .EndIf()
     .&If(AIR.Gt(AIR.Get('LLen2'), AIR.Int64(0)));
  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LData2', AIR.GetField(LDeref, 'Data'))
        .Call('memcpy', [AIR.Add(AIR.Get('LData'), AIR.Get('LLen1')), AIR.Get('LData2'), AIR.Get('LLen2')])
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('LRec'), 'TStringRec');
  AIR.AssignTo(AIR.GetField(LDeref, 'Length'), AIR.Get('LTotalLen'))
     .Call('memset', [AIR.Add(AIR.Get('LData'), AIR.Get('LTotalLen')), AIR.Int64(0), AIR.Int64(1)])
     .Return(AIR.Get('LRec'))
  .EndFunc();

  AIR.Func('Tiger_StrAssign', vtVoid, False, plC, False)
     .Param('ADest', vtPointer)
     .Param('ASrc', vtPointer)
     .Local('LOld', vtPointer)
     .Assign('LOld', AIR.Deref(AIR.Get('ADest'), vtPointer))
     .Call('Tiger_StrAddRef', [AIR.Get('ASrc')])
     .AssignTo(AIR.Deref(AIR.Get('ADest'), vtPointer), AIR.Get('ASrc'))
     .Call('Tiger_StrRelease', [AIR.Get('LOld')])
     .Return()
  .EndFunc();

  AIR.Func('Tiger_StrLen', vtUInt64, False, plC, False)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Int64(0))
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Return(AIR.GetField(LDeref, 'Length'))
  .EndFunc();

  AIR.Func('Tiger_StrCompare', vtInt64, False, plC, False)
     .Param('AStr1', vtPointer)
     .Param('AStr2', vtPointer)
     .Local('LLen1', vtUInt64)
     .Local('LLen2', vtUInt64)
     .Local('LMinLen', vtUInt64)
     .Local('LData1', vtPointer)
     .Local('LData2', vtPointer)
     .Local('LResult', vtInt32)
     .&If(AIR.Eq(AIR.Get('AStr1'), AIR.Null()))
        .&If(AIR.Eq(AIR.Get('AStr2'), AIR.Null()))
           .Return(AIR.Int64(0))
        .EndIf()
        .Return(AIR.Int64(-1))
     .EndIf()
     .&If(AIR.Eq(AIR.Get('AStr2'), AIR.Null()))
        .Return(AIR.Int64(1))
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LLen1', AIR.GetField(LDeref, 'Length'));
  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LLen2', AIR.GetField(LDeref, 'Length'));
  LDeref := AIR.Deref(AIR.Get('AStr1'), 'TStringRec');
  AIR.Assign('LData1', AIR.GetField(LDeref, 'Data'));
  LDeref := AIR.Deref(AIR.Get('AStr2'), 'TStringRec');
  AIR.Assign('LData2', AIR.GetField(LDeref, 'Data'));
  AIR.&If(AIR.Lt(AIR.Get('LLen1'), AIR.Get('LLen2')))
       .Assign('LMinLen', AIR.Get('LLen1'))
     .&Else()
       .Assign('LMinLen', AIR.Get('LLen2'))
     .EndIf();
  AIR.Assign('LResult', AIR.Invoke('memcmp', [AIR.Get('LData1'), AIR.Get('LData2'), AIR.Get('LMinLen')]))
     .&If(AIR.Lt(AIR.Get('LResult'), AIR.Int64(0)))
        .Return(AIR.Int64(-1))
     .EndIf()
     .&If(AIR.Gt(AIR.Get('LResult'), AIR.Int64(0)))
        .Return(AIR.Int64(1))
     .EndIf()
     .&If(AIR.Lt(AIR.Get('LLen1'), AIR.Get('LLen2')))
        .Return(AIR.Int64(-1))
     .EndIf()
     .&If(AIR.Gt(AIR.Get('LLen1'), AIR.Get('LLen2')))
        .Return(AIR.Int64(1))
     .EndIf()
     .Return(AIR.Int64(0))
  .EndFunc();

  AIR.Func('Tiger_StrData', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Null())
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Return(AIR.GetField(LDeref, 'Data'))
  .EndFunc();

  AIR.Func('Tiger_StrUtf16', vtPointer, False, plC, False)
     .Param('AStr', vtPointer)
     .&If(AIR.Eq(AIR.Get('AStr'), AIR.Null()))
        .Return(AIR.Null())
     .EndIf();
  LDeref := AIR.Deref(AIR.Get('AStr'), 'TStringRec');
  AIR.Return(AIR.GetField(LDeref, 'Data'))
  .EndFunc();
end;

//==============================================================================
// TTigerMacOS64Runtime - Types (TStringRec, string)
//==============================================================================

procedure TTigerMacOS64Runtime.AddTypes(const AIR: TTigerIR);
begin
  AIR.DefineRecord('TStringRec')
     .Field('RefCount', vtInt64)
     .Field('Length', vtUInt64)
     .Field('Capacity', vtUInt64)
     .Field('Data', vtPointer)
     .Field('Data16', vtPointer)
  .EndRecord();

  AIR.DefinePointer('string', 'TStringRec', False);
end;

//==============================================================================
// TTigerMacOS64Runtime - Exceptions (stub for Phase 1)
//==============================================================================

procedure TTigerMacOS64Runtime.AddExceptions(const AIR: TTigerIR);
begin
  // macOS Unwind-based exception support can be added in Phase 5.
end;

end.
