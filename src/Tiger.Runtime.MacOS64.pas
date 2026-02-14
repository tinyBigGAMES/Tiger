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
  AIR.Import(LIB_SYSTEM, 'exit', [vtInt32], vtVoid, False);

  AIR.Func('Tiger_Halt', vtVoid, False, plC, False)
     .Param('AExitCode', vtInt32);
  if AOptLevel = 0 then
    AIR.Call('Tiger_ReportLeaks', []);
  AIR.Call('exit', [AIR.Get('AExitCode')])
  .EndFunc();
end;

//==============================================================================
// TTigerMacOS64Runtime - I/O
//==============================================================================

procedure TTigerMacOS64Runtime.AddIO(const AIR: TTigerIR);
begin
  AIR.Import(LIB_SYSTEM, 'printf', [vtPointer], vtInt32, True);

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
// TTigerMacOS64Runtime - Strings (stub for Phase 1; full impl in Phase 2)
//==============================================================================

procedure TTigerMacOS64Runtime.AddStrings(const AIR: TTigerIR);
begin
  AddTypes(AIR);
  // Full string runtime (Tiger_StrAlloc, Tiger_StrFree, etc.) can be added
  // in Phase 2; for hello world we only need printf and exit.
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
