{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit UTestbed;

interface

procedure RunTestbed();

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Tiger,
  Tiger.Utils,
  Tiger.Utils.Win64;

const
  COutputPath = 'output';

(*==============================================================================
  StatusCallback: Console output callback for TTiger build status messages.
  Passed to SetStatusCallback to receive progress and diagnostic text
  during compilation and linking phases.
==============================================================================*)
procedure StatusCallback(const AText: string; const AUserData: Pointer);
begin
  TWin64Utils.PrintLn(AText);
end;

(*==============================================================================
  ShowErrors: Prints all accumulated errors from a TTiger instance.
  Checks HasErrors first and only prints the error divider and text
  if errors are present. Used as a standard epilogue after each test's
  Build call.
==============================================================================*)
procedure ShowErrors(const ATiger: TTiger);
begin
  if not ATiger.HasErrors() then Exit;
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Errors ---');
  TWin64Utils.PrintLn(ATiger.GetErrorText());
end;

(*==============================================================================
  ProcessBuild: Builds the current Tiger program and prints the result to
  the console. The executable is launched automatically with AAutoRun=True
  and the process exit code is displayed after the run completes. If
  ADumpSSA is True, the SSA intermediate representation is printed
  regardless of success or failure.
==============================================================================*)
procedure ProcessBuild(const ATiger: TTiger; const ADumpSSA: Boolean=False);
var
  LExitCode: Cardinal;
begin
  LExitCode := 0;
  //TWin64Utils.PrintLn(COLOR_CYAN + 'Running...' + COLOR_RESET);
  if ATiger.Build(True, @LExitCode) then
  begin
    //TWin64Utils.PrintLn(COLOR_GREEN + 'Exit code: ' + LExitCode.ToString() + COLOR_RESET);
    TWin64Utils.PrintLn(COLOR_CYAN + 'Build: Success!' + COLOR_RESET);
    if ADumpSSA then
      TWin64Utils.PrintLn(ATiger.GetSSADump());
  end
  else
  begin
    TWin64Utils.PrintLn(COLOR_CYAN + 'Build: Failed!' + COLOR_RESET);
    if ADumpSSA then
      TWin64Utils.PrintLn(ATiger.GetSSADump());
  end;
end;

(*==============================================================================
  SetExeResources: Configures standard version information and an application
  icon for the output executable. Sets a default v0.1.0 version with
  tinyBigGAMES branding and loads the Tiger icon from res/icons/tiger.ico.
  Call this before ProcessBuild so the resources are included in the binary.
==============================================================================*)
procedure SetExeResources(const ATiger: TTiger; const AFilename: string);
begin
  ATiger.SetVersionInfo(0, 1, 0, 'Tiger', 'Tiger Demo', AFilename, 'tinyBigGAMES(tm) LLC', 'Copyright (c) 2026');
  ATiger.AddExeIcon('res/icons/tiger.ico');
  ATiger.AddVersionInfo(True);
end;

(*==============================================================================
  Test01: Hello World (minimal console executable)
  Uses the high-level TTiger API to import printf, emit a
  single main function that prints "Hello, World!", and builds a console
  executable. Validates the basic end-to-end pipeline.
==============================================================================*)
procedure Test01(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test01.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
      .Call('printf', [LTiger.Str('Hello, World!'#10)])
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test01.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test02: IR facade basics (Factorial via while loop)
  Uses the TTiger fluent API to build a factorial(5) program with local
  variables, while loop, arithmetic (Mul/Sub), and printf output.
  Validates: BeginFunc, Local, Assign, WhileBegin/End, Var_, Int, Str, Call.
==============================================================================*)
procedure Test02(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test02.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('n', vtInt64)
       .Local('result', vtInt64)

       .Assign('n', LTiger.Int64(5))
       .Assign('result', LTiger.Int64(1))

       .&While(LTiger.Gt(LTiger.Get('n'), LTiger.Int64(1)))
         .Assign('result', LTiger.Mul(LTiger.Get('result'), LTiger.Get('n')))
         .Assign('n', LTiger.Sub(LTiger.Get('n'), LTiger.Int64(1)))
       .EndWhile()

       .Call('printf', [LTiger.Str('5! = %d'#10), LTiger.Get('result')])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test02.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test03: SSA optimizer passes
  Builds the same program at optimization levels 0, 1, and 2 to verify that
  constant folding (5+3=8), copy propagation (b=a), common subexpression
  elimination (a+2 reused), and dead code elimination (unused variable e)
  all work correctly without changing program output.
==============================================================================*)
procedure Test03(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('a', vtInt64)
       .Local('b', vtInt64)
       .Local('c', vtInt64)
       .Local('d', vtInt64)
       .Local('e', vtInt64)  // Dead - never used

       .Assign('a', LTiger.Add(LTiger.Int64(5), LTiger.Int64(3)))   // Constant fold: 5+3 = 8
       .Assign('b', LTiger.Get('a'))                            // Copy: b = a (copy prop target)
       .Assign('c', LTiger.Add(LTiger.Get('a'), LTiger.Int64(2))) // a + 2
       .Assign('d', LTiger.Add(LTiger.Get('a'), LTiger.Int64(2))) // CSE: same as c, should reuse
       .Assign('e', LTiger.Int64(99))                              // Dead code: e never used

       .Call('printf', [LTiger.Str('a=%d, b=%d, c=%d, d=%d'#10),
             LTiger.Get('a'),
             LTiger.Get('b'),   // Copy prop: should become 'a'
             LTiger.Get('c'),
             LTiger.Get('d')])  // CSE: should use same value as c
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    TWin64Utils.PrintLn('--- Optimization Level 0 ---');
    SetExeResources(LTiger, 'Test03_O0.exe');
    LTiger.SetOptimizationLevel(0);
    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test03_O0.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);

    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Optimization Level 1 ---');
    LTiger.ResetBuild();
    LTiger.SetOptimizationLevel(1);
    SetExeResources(LTiger, 'Test03_O1.exe');
    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test03_O1.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);

    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Optimization Level 2 ---');
    LTiger.ResetBuild();
    SetExeResources(LTiger, 'Test03_O2.exe');
    LTiger.SetOptimizationLevel(2);
    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test03_O2.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test03_2: Same as Test03 but creates a fresh TTiger instance per optimization
  level instead of using ResetBuild. This isolates whether crashes are caused
  by stale state in ResetBuild vs the optimizer itself.
==============================================================================*)
procedure Test03_2(const ADumpSSA: Boolean=False);

  procedure BuildAndRun(const AOptLevel: Integer; const AOutputName: string);
  var
    LTiger: TTiger;
  begin
    LTiger := TTiger.Create();
    try
      LTiger.SetStatusCallback(StatusCallback);
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

      LTiger.Func('main', vtVoid, True)
         .Local('a', vtInt64)
         .Local('b', vtInt64)
         .Local('c', vtInt64)
         .Local('d', vtInt64)
         .Local('e', vtInt64)

         .Assign('a', LTiger.Add(LTiger.Int64(5), LTiger.Int64(3)))
         .Assign('b', LTiger.Get('a'))
         .Assign('c', LTiger.Add(LTiger.Get('a'), LTiger.Int64(2)))
         .Assign('d', LTiger.Add(LTiger.Get('a'), LTiger.Int64(2)))
         .Assign('e', LTiger.Int64(99))

         .Call('printf', [LTiger.Str('a=%d, b=%d, c=%d, d=%d'#10),
               LTiger.Get('a'),
               LTiger.Get('b'),
               LTiger.Get('c'),
               LTiger.Get('d')])
         .Call('Tiger_Halt', [LTiger.Int64(0)])
      .EndFunc();

      SetExeResources(LTiger, AOutputName);
      LTiger.SetOptimizationLevel(AOptLevel);
      LTiger.TargetExe(TPath.Combine(COutputPath, AOutputName), ssConsole);

      ProcessBuild(LTiger, ADumpSSA);
      ShowErrors(LTiger);
    finally
      LTiger.Free();
    end;
  end;

begin
  TWin64Utils.PrintLn('--- Test03_2: Fresh instance per opt level ---');
  TWin64Utils.PrintLn('');

  TWin64Utils.PrintLn('--- Optimization Level 0 ---');
  BuildAndRun(0, 'Test03_2_O0.exe');

  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Optimization Level 1 ---');
  BuildAndRun(1, 'Test03_2_O1.exe');

  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Optimization Level 2 ---');
  BuildAndRun(2, 'Test03_2_O2.exe');
end;

(*==============================================================================
  Test04: Loop optimization with SSA phi nodes
  Runs the factorial(5) while-loop at optimization level 2 to verify that
  phi node insertion and loop-carried variable updates (n, result) survive
  the full optimizer pipeline without corruption.
==============================================================================*)
procedure Test04(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test04.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('n', vtInt64)
       .Local('result', vtInt64)

       .Assign('n', LTiger.Int64(5))
       .Assign('result', LTiger.Int64(1))

       .&While(LTiger.Gt(LTiger.Get('n'), LTiger.Int64(1)))
         .Assign('result', LTiger.Mul(LTiger.Get('result'), LTiger.Get('n')))
         .Assign('n', LTiger.Sub(LTiger.Get('n'), LTiger.Int64(1)))
       .EndWhile()

       .Call('printf', [LTiger.Str('5! = %d'#10), LTiger.Get('result')])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    TWin64Utils.PrintLn('--- Loop Test: Optimization Level 2 ---');
    LTiger.SetOptimizationLevel(2);
    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test04.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test05: Case statement
  Tests CaseBegin/CaseOf/CaseElse/CaseEnd with a day-of-week example.
  Exercises single-value and multi-value CaseOf branches, the else fallback,
  and verifies correct branch selection across three test values (1=Weekday,
  6=Weekend, 9=Unknown).
==============================================================================*)
procedure Test05(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('=== Test05: Case Statement ===');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test05.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    // Test case statement with day of week
    // Expected output:
    //   Day 1: Weekday
    //   Day 6: Weekend
    //   Day 9: Unknown
    LTiger.Func('main', vtVoid, True)
       .Local('day', vtInt64)

       // Test case 1: day = 1 (Monday - weekday)
       .Assign('day', LTiger.Int64(1))
       .Call('printf', [LTiger.Str('Day %d: '#0), LTiger.Get('day')])
       .&Case(LTiger.Get('day'))
         .CaseOf([1, 2, 3, 4, 5])  // Weekdays
           .Call('printf', [LTiger.Str('Weekday'#10)])
         .CaseOf([6, 7])           // Weekend
           .Call('printf', [LTiger.Str('Weekend'#10)])
         .CaseElse()
           .Call('printf', [LTiger.Str('Unknown'#10)])
       .EndCase()

       // Test case 2: day = 6 (Saturday - weekend)
       .Assign('day', LTiger.Int64(6))
       .Call('printf', [LTiger.Str('Day %d: '#0), LTiger.Get('day')])
       .&Case(LTiger.Get('day'))
         .CaseOf([1, 2, 3, 4, 5])
           .Call('printf', [LTiger.Str('Weekday'#10)])
         .CaseOf([6, 7])
           .Call('printf', [LTiger.Str('Weekend'#10)])
         .CaseElse()
           .Call('printf', [LTiger.Str('Unknown'#10)])
       .EndCase()

       // Test case 3: day = 9 (invalid - else branch)
       .Assign('day', LTiger.Int64(9))
       .Call('printf', [LTiger.Str('Day %d: '#0), LTiger.Get('day')])
       .&Case(LTiger.Get('day'))
         .CaseOf([1, 2, 3, 4, 5])
           .Call('printf', [LTiger.Str('Weekday'#10)])
         .CaseOf([6, 7])
           .Call('printf', [LTiger.Str('Weekend'#10)])
         .CaseElse()
           .Call('printf', [LTiger.Str('Unknown'#10)])
       .EndCase()

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test05.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test06: Global variables
  Declares globals (gCounter, gMultiplier), verifies zero-initialization,
  then tests assignment, read-back, arithmetic on globals (add, multiply),
  and cross-global expressions. Validates the .bss section and global
  variable addressing in the generated PE.
==============================================================================*)
procedure Test06(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('=== Test06: Global Variables ===');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test06.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    // Declare global variables
    LTiger.Global('gCounter', vtInt64);
    LTiger.Global('gMultiplier', vtInt64);

    // Expected output:
    //   Initial: counter=0, multiplier=0
    //   After init: counter=10, multiplier=5
    //   After increment: counter=15
    //   Final: counter=75
    LTiger.Func('main', vtVoid, True)
       .Local('temp', vtInt64)

       // Print initial values (should be 0)
       .Call('printf', [LTiger.Str('Initial: counter=%d, multiplier=%d'#10),
             LTiger.Get('gCounter'), LTiger.Get('gMultiplier')])

       // Initialize globals
       .Assign('gCounter', LTiger.Int64(10))
       .Assign('gMultiplier', LTiger.Int64(5))
       .Call('printf', [LTiger.Str('After init: counter=%d, multiplier=%d'#10),
             LTiger.Get('gCounter'), LTiger.Get('gMultiplier')])

       // Increment counter using multiplier
       .Assign('gCounter', LTiger.Add(LTiger.Get('gCounter'), LTiger.Get('gMultiplier')))
       .Call('printf', [LTiger.Str('After increment: counter=%d'#10),
             LTiger.Get('gCounter')])

       // Multiply counter by multiplier
       .Assign('gCounter', LTiger.Mul(LTiger.Get('gCounter'), LTiger.Get('gMultiplier')))
       .Call('printf', [LTiger.Str('Final: counter=%d'#10),
             LTiger.Get('gCounter')])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test06.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test07: Type system (records, arrays, enums, aliases)
  Comprehensive type definition test covering: records (TPoint, TRect),
  packed records (TPackedPoint), records with mixed-size fields (TStudent),
  fixed arrays (TIntArray, TPointArray), dynamic arrays (TDynInts),
  enums with auto and explicit values (TColor, TStatus), type aliases
  to primitives (TMyInt) and composites (TCoord). Queries and prints
  size, alignment, and TypeRef properties for each type. Builds a trivial
  executable to confirm no errors from the type definitions.
==============================================================================*)
procedure Test07(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;
begin
  TWin64Utils.PrintLn('=== Test07: Type System ===');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test07.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    //------------------------------------------------------------------------
    // Define types
    //------------------------------------------------------------------------

    // Record: TPoint (X: int32, Y: int32) - size should be 8, align 4
    LTiger.DefineRecord('TPoint')
         .Field('X', vtInt32)
         .Field('Y', vtInt32)
       .EndRecord();

    // Record: TRect (TopLeft: TPoint, BottomRight: TPoint) - size should be 16
    LTiger.DefineRecord('TRect')
         .Field('TopLeft', 'TPoint')
         .Field('BottomRight', 'TPoint')
       .EndRecord();

    // Packed record: TPackedPoint - size should be 8 (no padding)
    LTiger.DefineRecord('TPackedPoint', True)
         .Field('X', vtInt32)
         .Field('Y', vtInt32)
       .EndRecord();

    // Record with mixed sizes: TStudent
    // name: pointer (8), age: int8 (1), grade: float64 (8)
    // With alignment: 8 + 8(padded age) + 8 = 24
    LTiger.DefineRecord('TStudent')
         .Field('NamePtr', vtPointer)
         .Field('Age', vtUInt8)
         .Field('Grade', vtFloat64)
       .EndRecord();

    // Fixed array: TIntArray = array[0..9] of int32 - size should be 40
    LTiger.DefineArray('TIntArray', vtInt32, 0, 9);

    // Fixed array of records: TPointArray = array[0..4] of TPoint - size 40
    LTiger.DefineArray('TPointArray', 'TPoint', 0, 4);

    // Dynamic array: TDynInts = array of int32
    LTiger.DefineDynArray('TDynInts', vtInt32);

    // Enum: TColor (Red=0, Green=1, Blue=2)
    LTiger.DefineEnum('TColor')
         .EnumValue('Red')
         .EnumValue('Green')
         .EnumValue('Blue')
       .EndEnum();

    // Enum with explicit values: TStatus
    LTiger.DefineEnum('TStatus')
         .EnumValue('OK', 0)
         .EnumValue('Error', -1)
         .EnumValue('Pending', 100)
       .EndEnum();

    // Type alias: TMyInt = int32
    LTiger.DefineAlias('TMyInt', vtInt32);

    // Type alias to record: TCoord = TPoint
    LTiger.DefineAlias('TCoord', 'TPoint');

    //------------------------------------------------------------------------
    // Query and print type information
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Type Information ---');
    TWin64Utils.PrintLn(Format('Total types defined: %d', [LTiger.GetTypeCount()]));
    TWin64Utils.PrintLn('');

    // TPoint
    LTypeIndex := LTiger.FindType('TPoint');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TPoint: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TRect
    LTypeIndex := LTiger.FindType('TRect');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TRect: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TPackedPoint
    LTypeIndex := LTiger.FindType('TPackedPoint');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TPackedPoint: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TStudent
    LTypeIndex := LTiger.FindType('TStudent');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TStudent: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TIntArray
    LTypeIndex := LTiger.FindType('TIntArray');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TIntArray: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TPointArray
    LTypeIndex := LTiger.FindType('TPointArray');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TPointArray: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TDynInts (dynamic array = pointer size)
    LTypeIndex := LTiger.FindType('TDynInts');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TDynInts: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TColor (enum = int32)
    LTypeIndex := LTiger.FindType('TColor');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TColor: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TMyInt (alias to int32)
    LTypeIndex := LTiger.FindType('TMyInt');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TMyInt: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TCoord (alias to TPoint)
    LTypeIndex := LTiger.FindType('TCoord');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TCoord: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // Test TypeRef helper
    LTypeRef := LTiger.TypeRef('TPoint');
    TWin64Utils.PrintLn(Format('TypeRef(TPoint): IsPrimitive=%s, TypeIndex=%d',
      [BoolToStr(LTypeRef.IsPrimitive, True), LTypeRef.TypeIndex]));

    // Test primitive TypeRef
    LTypeRef := TTigerTypeRef.FromPrimitive(vtInt64);
    TWin64Utils.PrintLn(Format('TypeRef(vtInt64): IsPrimitive=%s, size=%d',
      [BoolToStr(LTypeRef.IsPrimitive, True), LTiger.GetTypeSize(LTypeRef)]));

    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Type System Test Complete ---');

    //------------------------------------------------------------------------
    // Build a simple executable to verify no errors
    //------------------------------------------------------------------------
    LTiger.Func('main', vtVoid, True)
       .Call('printf', [LTiger.Str('Type system test passed!'#10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test07.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test08: Full C struct ABI (Phase 4)
  Validates the complete struct layout engine against C ABI expectations:
    4a: Explicit alignment (natural vs forced align(8)/align(16))
    4b: Union types (overlapping fields, size = max member)
    4c: Anonymous unions inside records (TPacket, TMultiUnion)
    4d: Anonymous records inside unions (TSplitValue with Lo/Hi)
    4e: Bit fields (packing, non-corruption across adjacent fields)
    4f: Record inheritance (TPoint2D -> TPoint3D -> TPoint4D, field offsets)
  Also includes runtime tests that build an executable writing/reading
  record fields, union reinterpretation, inherited fields, bit field
  isolation, and explicit alignment via generated machine code.
==============================================================================*)
procedure Test08(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;

begin
  TWin64Utils.PrintLn('=== Test08: Phase 4 - Full C Struct ABI ===');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test08.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    //------------------------------------------------------------------------
    // Phase 4a: Explicit Alignment
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- 4a: Explicit Alignment ---');

    // Normal record (natural alignment = 4)
    LTiger.DefineRecord('TNaturalAlign')
         .Field('A', vtInt32)
         .Field('B', vtInt32)
       .EndRecord();

    // Explicit align(16)
    LTiger.DefineRecord('TAlign16', False, 16)
         .Field('A', vtInt32)
         .Field('B', vtInt32)
       .EndRecord();

    // Explicit align(8) on small record - forces padding
    LTiger.DefineRecord('TAlign8', False, 8)
         .Field('A', vtInt32)  // 4 bytes, natural align=4
       .EndRecord();

    LTypeIndex := LTiger.FindType('TNaturalAlign');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TNaturalAlign: size=%d, align=%d (expected: 8, 4)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TAlign16');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TAlign16: size=%d, align=%d (expected: 16, 16)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TAlign8');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TAlign8: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    TWin64Utils.PrintLn('');

    //------------------------------------------------------------------------
    // Phase 4b: Union Types
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- 4b: Union Types ---');

    LTiger.DefineUnion('TVariant')
         .Field('AsInt', vtInt64)
         .Field('AsFloat', vtFloat64)
         .Field('AsPtr', vtPointer)
       .EndUnion();

    LTiger.DefineUnion('TMixedUnion')
         .Field('AsByte', vtUInt8)
         .Field('AsInt', vtInt32)
         .Field('AsLong', vtInt64)
       .EndUnion();

    LTypeIndex := LTiger.FindType('TVariant');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TVariant: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TMixedUnion');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TMixedUnion: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    TWin64Utils.PrintLn('');

    //------------------------------------------------------------------------
    // Phase 4c: Anonymous Unions in Records
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- 4c: Anonymous Unions in Records ---');

    LTiger.DefineRecord('TPacket')
         .Field('Header', vtUInt32)
         .BeginUnion()
           .Field('IntPayload', vtInt64)
           .Field('FloatPayload', vtFloat64)
         .EndUnion()
         .Field('Checksum', vtUInt32)
       .EndRecord();

    LTiger.DefineRecord('TMultiUnion')
         .Field('Tag1', vtUInt8)
         .BeginUnion()
           .Field('A1', vtInt32)
           .Field('B1', vtFloat32)
         .EndUnion()
         .Field('Tag2', vtUInt8)
         .BeginUnion()
           .Field('A2', vtInt64)
           .Field('B2', vtFloat64)
         .EndUnion()
       .EndRecord();

    LTypeIndex := LTiger.FindType('TPacket');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TPacket: size=%d, align=%d (expected: 24, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TMultiUnion');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TMultiUnion: size=%d, align=%d (expected: 24, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    TWin64Utils.PrintLn('');

    //------------------------------------------------------------------------
    // Phase 4d: Anonymous Records in Unions
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- 4d: Anonymous Records in Unions ---');

    LTiger.DefineUnion('TSplitValue')
         .Field('AsInt64', vtInt64)
         .BeginRecord()
           .Field('Lo', vtInt32)
           .Field('Hi', vtInt32)
         .EndRecord()
         .Field('AsFloat', vtFloat64)
       .EndUnion();

    LTypeIndex := LTiger.FindType('TSplitValue');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TSplitValue: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    TWin64Utils.PrintLn('');

    //------------------------------------------------------------------------
    // Phase 4e: Bit Fields
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- 4e: Bit Fields ---');

    LTiger.DefineRecord('TFlags')
         .BitField('Enabled', vtUInt32, 1)
         .BitField('Priority', vtUInt32, 3)
         .BitField('Mode', vtUInt32, 4)
         .BitField('Reserved', vtUInt32, 24)
       .EndRecord();

    LTiger.DefineRecord('TLargeFlags')
         .BitField('A', vtUInt32, 16)
         .BitField('B', vtUInt32, 16)
         .BitField('C', vtUInt32, 16)
       .EndRecord();

    LTiger.DefineRecord('TMixedFields')
         .Field('Header', vtUInt8)
         .BitField('Flag1', vtUInt8, 1)
         .BitField('Flag2', vtUInt8, 1)
         .BitField('Flag3', vtUInt8, 6)
         .Field('Data', vtInt64)
       .EndRecord();

    LTypeIndex := LTiger.FindType('TFlags');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TFlags: size=%d, align=%d (expected: 4, 4)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TLargeFlags');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TLargeFlags: size=%d, align=%d (expected: 8, 4)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TMixedFields');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TMixedFields: size=%d, align=%d (expected: 16, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    TWin64Utils.PrintLn('');

    //------------------------------------------------------------------------
    // Phase 4f: Record Inheritance
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- 4f: Record Inheritance ---');

    LTiger.DefineRecord('TPoint2D')
         .Field('X', vtInt32)
         .Field('Y', vtInt32)
       .EndRecord();

    LTiger.DefineRecord('TPoint3D', False, 0, 'TPoint2D')
         .Field('Z', vtInt32)
       .EndRecord();

    LTiger.DefineRecord('TPoint4D', False, 0, 'TPoint3D')
         .Field('W', vtInt32)
       .EndRecord();

    LTypeIndex := LTiger.FindType('TPoint2D');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TPoint2D: size=%d, align=%d (expected: 8, 4)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TPoint3D');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TPoint3D: size=%d, align=%d (expected: 12, 4)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TPoint4D');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TPoint4D: size=%d, align=%d (expected: 16, 4)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // Test field offsets with inheritance
    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- FindRecordField Tests (inheritance) ---');

    TWin64Utils.PrintLn('TPoint3D.Z: offset=%d (expected: 8)', [LTiger.GetFieldOffset('TPoint3D', 'Z')]);
    TWin64Utils.PrintLn('TPoint3D.X (inherited): offset=%d (expected: 0)', [LTiger.GetFieldOffset('TPoint3D', 'X')]);
    TWin64Utils.PrintLn('TPoint3D.Y (inherited): offset=%d (expected: 4)', [LTiger.GetFieldOffset('TPoint3D', 'Y')]);
    TWin64Utils.PrintLn('TPoint4D.X (2-level inherit): offset=%d (expected: 0)', [LTiger.GetFieldOffset('TPoint4D', 'X')]);

    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Phase 4 Test Complete ---');

    //------------------------------------------------------------------------
    // Runtime tests: Comprehensive ABI validation
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Runtime ABI Tests ---');

    LTiger.DefineRecord('TTestPoint')
         .Field('X', vtInt64)
         .Field('Y', vtInt64)
       .EndRecord();

    LTiger.DefineUnion('TTestUnion')
         .Field('AsInt', vtInt64)
         .Field('AsUInt', vtUInt64)
       .EndUnion();

    LTiger.DefineRecord('TBasePoint')
         .Field('X', vtInt32)
         .Field('Y', vtInt32)
       .EndRecord();

    LTiger.DefineRecord('TDerivedPoint', False, 0, 'TBasePoint')
         .Field('Z', vtInt32)
       .EndRecord();

    LTiger.DefineRecord('TPacketTest')
         .Field('Header', vtInt32)
         .BeginUnion()
           .Field('IntData', vtInt64)
           .Field('FloatData', vtFloat64)
         .EndUnion()
         .Field('Footer', vtInt32)
       .EndRecord();

    LTiger.DefineRecord('TBitFlags')
         .BitField('Flag0', vtUInt32, 1)
         .BitField('Flag1', vtUInt32, 1)
         .BitField('Value', vtUInt32, 4)
         .BitField('Mode', vtUInt32, 2)
       .EndRecord();

    LTiger.DefineRecord('TAligned16', False, 16)
         .Field('A', vtInt32)
         .Field('B', vtInt64)
       .EndRecord();

    LTiger.Func('main', vtVoid, True)
       .Local('pt', 'TTestPoint')
       .Local('un', 'TTestUnion')
       .Local('dp', 'TDerivedPoint')
       .Local('pk', 'TPacketTest')
       .Local('bf', 'TBitFlags')
       .Local('al', 'TAligned16')

       // Test 1: Simple record
       .Call('printf', [LTiger.Str('=== Test 1: Simple Record ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('pt'), 'X'), LTiger.Int64(42))
       .AssignTo(LTiger.GetField(LTiger.Get('pt'), 'Y'), LTiger.Int64(100))
       .Call('printf', [LTiger.Str('pt.X = %lld (expected 42)' + #10), LTiger.GetField(LTiger.Get('pt'), 'X')])
       .Call('printf', [LTiger.Str('pt.Y = %lld (expected 100)' + #10), LTiger.GetField(LTiger.Get('pt'), 'Y')])

       // Test 2: Union
       .Call('printf', [LTiger.Str('=== Test 2: Union ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('un'), 'AsInt'), LTiger.Int64(-1))
       .Call('printf', [LTiger.Str('un.AsInt = %lld (expected -1)' + #10), LTiger.GetField(LTiger.Get('un'), 'AsInt')])
       .Call('printf', [LTiger.Str('un.AsUInt = %llu (expected 18446744073709551615)' + #10), LTiger.GetField(LTiger.Get('un'), 'AsUInt')])

       // Test 3: Record inheritance
       .Call('printf', [LTiger.Str('=== Test 3: Record Inheritance ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('dp'), 'X'), LTiger.Int64(10))
       .AssignTo(LTiger.GetField(LTiger.Get('dp'), 'Y'), LTiger.Int64(20))
       .AssignTo(LTiger.GetField(LTiger.Get('dp'), 'Z'), LTiger.Int64(30))
       .Call('printf', [LTiger.Str('dp.X = %d (expected 10)' + #10), LTiger.GetField(LTiger.Get('dp'), 'X')])
       .Call('printf', [LTiger.Str('dp.Y = %d (expected 20)' + #10), LTiger.GetField(LTiger.Get('dp'), 'Y')])
       .Call('printf', [LTiger.Str('dp.Z = %d (expected 30)' + #10), LTiger.GetField(LTiger.Get('dp'), 'Z')])

       // Test 4: Record with anonymous union
       .Call('printf', [LTiger.Str('=== Test 4: Anonymous Union in Record ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('pk'), 'Header'), LTiger.Int64($DEAD))
       .AssignTo(LTiger.GetField(LTiger.Get('pk'), 'IntData'), LTiger.Int64($123456789ABC))
       .AssignTo(LTiger.GetField(LTiger.Get('pk'), 'Footer'), LTiger.Int64($BEEF))
       .Call('printf', [LTiger.Str('pk.Header = 0x%X (expected 0xDEAD)' + #10), LTiger.GetField(LTiger.Get('pk'), 'Header')])
       .Call('printf', [LTiger.Str('pk.IntData = 0x%llX (expected 0x123456789ABC)' + #10), LTiger.GetField(LTiger.Get('pk'), 'IntData')])
       .Call('printf', [LTiger.Str('pk.Footer = 0x%X (expected 0xBEEF)' + #10), LTiger.GetField(LTiger.Get('pk'), 'Footer')])

       // Test 5: Bit fields
       .Call('printf', [LTiger.Str('=== Test 5: Bit Fields ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('bf'), 'Flag0'), LTiger.Int64(1))
       .AssignTo(LTiger.GetField(LTiger.Get('bf'), 'Flag1'), LTiger.Int64(0))
       .AssignTo(LTiger.GetField(LTiger.Get('bf'), 'Value'), LTiger.Int64(11))
       .AssignTo(LTiger.GetField(LTiger.Get('bf'), 'Mode'), LTiger.Int64(2))
       .Call('printf', [LTiger.Str('bf.Flag0 = %d (expected 1)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Flag0')])
       .Call('printf', [LTiger.Str('bf.Flag1 = %d (expected 0)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Flag1')])
       .Call('printf', [LTiger.Str('bf.Value = %d (expected 11)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Value')])
       .Call('printf', [LTiger.Str('bf.Mode = %d (expected 2)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Mode')])

       // Verify bit fields don't corrupt each other
       .AssignTo(LTiger.GetField(LTiger.Get('bf'), 'Value'), LTiger.Int64(5))
       .Call('printf', [LTiger.Str('After changing Value to 5:' + #10)])
       .Call('printf', [LTiger.Str('bf.Flag0 = %d (expected 1)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Flag0')])
       .Call('printf', [LTiger.Str('bf.Flag1 = %d (expected 0)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Flag1')])
       .Call('printf', [LTiger.Str('bf.Value = %d (expected 5)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Value')])
       .Call('printf', [LTiger.Str('bf.Mode = %d (expected 2)' + #10), LTiger.GetField(LTiger.Get('bf'), 'Mode')])

       // Test 6: Explicit alignment
       .Call('printf', [LTiger.Str('=== Test 6: Explicit Alignment ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('al'), 'A'), LTiger.Int64(12345))
       .AssignTo(LTiger.GetField(LTiger.Get('al'), 'B'), LTiger.Int64($DEADBEEFCAFE))
       .Call('printf', [LTiger.Str('al.A = %d (expected 12345)' + #10), LTiger.GetField(LTiger.Get('al'), 'A')])
       .Call('printf', [LTiger.Str('al.B = 0x%llX (expected 0xDEADBEEFCAFE)' + #10), LTiger.GetField(LTiger.Get('al'), 'B')])

       .Call('printf', [LTiger.Str('=== All Runtime Tests Complete ===' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test08.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test09: Typed pointers (Phase 5a)
  Tests pointer type definitions (PInt32, PInt64, PPoint, TRawPointer) and
  runtime pointer operations: address-of (@x), dereference (p^), write
  through pointer (p^ := value), address of record fields (@pt.X), and
  address of array elements (@arr[1]). Verifies that modifications through
  pointers are reflected in the original variables.
==============================================================================*)
procedure Test09(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;
begin
  // Test09: Phase 5a - Typed Pointers
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test09: Phase 5a - Typed Pointers');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test09.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    //------------------------------------------------------------------------
    // Type System Tests
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Pointer Type Definitions ---');

    LTiger.DefinePointer('PInt32', vtInt32);
    LTiger.DefinePointer('PInt64', vtInt64);
    LTiger.DefinePointer('TRawPointer');

    LTiger.DefineRecord('TPoint')
         .Field('X', vtInt32)
         .Field('Y', vtInt32)
       .EndRecord();

    LTiger.DefinePointer('PPoint', 'TPoint');
    LTiger.DefineArray('TIntArray', vtInt32, 0, 4);

    LTypeIndex := LTiger.FindType('PInt32');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('PInt32: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('TRawPointer');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TRawPointer: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    LTypeIndex := LTiger.FindType('PPoint');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('PPoint: size=%d, align=%d (expected: 8, 8)',
      [LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    TWin64Utils.PrintLn('');

    //------------------------------------------------------------------------
    // Runtime Tests
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Runtime Pointer Tests ---');

    LTiger.Func('main', vtVoid, True)
       .Local('x', vtInt32)
       .Local('p', vtPointer)
       .Local('y', vtInt32)
       .Local('pt', 'TPoint')
       .Local('px', vtPointer)
       .Local('arr', 'TIntArray')
       .Local('pa', vtPointer)

       // Test 1: Basic pointer operations
       .Call('printf', [LTiger.Str('=== Test 1: Basic Pointer Operations ===' + #10)])
       .Assign('x', LTiger.Int64(42))
       .Call('printf', [LTiger.Str('x = %d (initial value 42)' + #10), LTiger.Get('x')])
       .Assign('p', LTiger.AddrOf('x'))
       .Call('printf', [LTiger.Str('p := @x (took address)' + #10)])
       .Assign('y', LTiger.Deref(LTiger.Get('p')))
       .Call('printf', [LTiger.Str('y := p^ -> y = %d (expected 42)' + #10), LTiger.Get('y')])
       .AssignTo(LTiger.Deref(LTiger.Get('p')), LTiger.Int64(100))
       .Call('printf', [LTiger.Str('p^ := 100' + #10)])
       .Call('printf', [LTiger.Str('x = %d (expected 100, modified via pointer)' + #10), LTiger.Get('x')])

       // Test 2: Address of record field
       .Call('printf', [LTiger.Str('=== Test 2: Address of Record Field ===' + #10)])
       .AssignTo(LTiger.GetField(LTiger.Get('pt'), 'X'), LTiger.Int64(10))
       .AssignTo(LTiger.GetField(LTiger.Get('pt'), 'Y'), LTiger.Int64(20))
       .Call('printf', [LTiger.Str('pt.X = %d, pt.Y = %d (initial)' + #10),
         LTiger.GetField(LTiger.Get('pt'), 'X'),
         LTiger.GetField(LTiger.Get('pt'), 'Y')])
       .Assign('px', LTiger.AddrOfVal(LTiger.GetField(LTiger.Get('pt'), 'X')))
       .Call('printf', [LTiger.Str('px := @pt.X (took address of field)' + #10)])
       .Assign('y', LTiger.Deref(LTiger.Get('px')))
       .Call('printf', [LTiger.Str('y := px^ -> y = %d (expected 10)' + #10), LTiger.Get('y')])
       .AssignTo(LTiger.Deref(LTiger.Get('px')), LTiger.Int64(55))
       .Call('printf', [LTiger.Str('px^ := 55' + #10)])
       .Call('printf', [LTiger.Str('pt.X = %d (expected 55, modified via pointer)' + #10),
         LTiger.GetField(LTiger.Get('pt'), 'X')])
       .Assign('px', LTiger.AddrOfVal(LTiger.GetField(LTiger.Get('pt'), 'Y')))
       .AssignTo(LTiger.Deref(LTiger.Get('px')), LTiger.Int64(77))
       .Call('printf', [LTiger.Str('pt.Y = %d (expected 77, modified via pointer)' + #10),
         LTiger.GetField(LTiger.Get('pt'), 'Y')])

       // Test 3: Address of array element
       .Call('printf', [LTiger.Str('=== Test 3: Address of Array Element ===' + #10)])
       .AssignTo(LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(0)), LTiger.Int64(100))
       .AssignTo(LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(1)), LTiger.Int64(200))
       .AssignTo(LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(2)), LTiger.Int64(300))
       .Call('printf', [LTiger.Str('arr[0]=%d, arr[1]=%d, arr[2]=%d (initial)' + #10),
         LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(0)),
         LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(1)),
         LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(2))])
       .Assign('pa', LTiger.AddrOfVal(LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(1))))
       .Call('printf', [LTiger.Str('pa := @arr[1] (took address of element)' + #10)])
       .Assign('y', LTiger.Deref(LTiger.Get('pa')))
       .Call('printf', [LTiger.Str('y := pa^ -> y = %d (expected 200)' + #10), LTiger.Get('y')])
       .AssignTo(LTiger.Deref(LTiger.Get('pa')), LTiger.Int64(999))
       .Call('printf', [LTiger.Str('pa^ := 999' + #10)])
       .Call('printf', [LTiger.Str('arr[1] = %d (expected 999, modified via pointer)' + #10),
         LTiger.GetIndex(LTiger.Get('arr'), LTiger.Int64(1))])

       .Call('printf', [LTiger.Str('=== All Pointer Tests Complete ===' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test09.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test10: Function pointers (Phase 5b)
  Tests FuncAddr to obtain a function's address, and IndirectCallExpr to
  invoke through a pointer. Covers: basic indirect call, switching the
  pointer between functions, parameterless function pointers, and chained
  indirect calls (result of one fed into another).
==============================================================================*)
procedure Test10(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test10: Function Pointers (Phase 5b)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test10.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('add_func', vtInt32, False)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Add(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.Func('mul_func', vtInt32, False)
       .Param('x', vtInt32)
       .Param('y', vtInt32)
       .Return(LTiger.Mul(LTiger.Get('x'), LTiger.Get('y')))
    .EndFunc();

    LTiger.Func('get_magic', vtInt32, False)
       .Return(LTiger.Int64(42))
    .EndFunc();

    LTiger.Func('main', vtVoid, True)
       .Local('pFunc', vtPointer)
       .Local('result', vtInt32)

       // Test 1: Basic function pointer
       .Call('printf', [LTiger.Str('=== Test 1: Basic Function Pointer ===' + #10)])
       .Assign('pFunc', LTiger.FuncAddr('add_func'))
       .Call('printf', [LTiger.Str('pFunc := @add_func' + #10)])
       .Assign('result', LTiger.InvokeIndirect(LTiger.Get('pFunc'), [LTiger.Int64(10), LTiger.Int64(20)]))
       .Call('printf', [LTiger.Str('result := pFunc(10, 20) -> %d (expected 30)' + #10), LTiger.Get('result')])

       // Test 2: Switch function pointer
       .Call('printf', [LTiger.Str(#10 + '=== Test 2: Switching Function Pointers ===' + #10)])
       .Assign('pFunc', LTiger.FuncAddr('mul_func'))
       .Call('printf', [LTiger.Str('pFunc := @mul_func' + #10)])
       .Assign('result', LTiger.InvokeIndirect(LTiger.Get('pFunc'), [LTiger.Int64(6), LTiger.Int64(7)]))
       .Call('printf', [LTiger.Str('result := pFunc(6, 7) -> %d (expected 42)' + #10), LTiger.Get('result')])

       // Test 3: Parameterless function pointer
       .Call('printf', [LTiger.Str(#10 + '=== Test 3: Parameterless Function Pointer ===' + #10)])
       .Assign('pFunc', LTiger.FuncAddr('get_magic'))
       .Call('printf', [LTiger.Str('pFunc := @get_magic' + #10)])
       .Assign('result', LTiger.InvokeIndirect(LTiger.Get('pFunc'), []))
       .Call('printf', [LTiger.Str('result := pFunc() -> %d (expected 42)' + #10), LTiger.Get('result')])

       // Test 4: Chained indirect calls
       .Call('printf', [LTiger.Str(#10 + '=== Test 4: Chained Indirect Calls ===' + #10)])
       .Assign('pFunc', LTiger.FuncAddr('add_func'))
       .Assign('result', LTiger.InvokeIndirect(LTiger.Get('pFunc'), [LTiger.Int64(5), LTiger.Int64(3)]))
       .Call('printf', [LTiger.Str('add_func(5, 3) = %d (expected 8)' + #10), LTiger.Get('result')])
       .Assign('pFunc', LTiger.FuncAddr('mul_func'))
       .Assign('result', LTiger.InvokeIndirect(LTiger.Get('pFunc'), [LTiger.Get('result'), LTiger.Int64(4)]))
       .Call('printf', [LTiger.Str('mul_func(8, 4) = %d (expected 32)' + #10), LTiger.Get('result')])

       .Call('printf', [LTiger.Str(#10 + '=== All Function Pointer Tests Complete ===' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test10.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test11: Public exports (Phase 5c)
  Defines functions with mixed visibility: MyAdd and MyMul (C linkage,
  exported), MySquare (default linkage, exported), PrivateHelper (C linkage,
  not exported). Verifies all are callable internally and that only the
  public ones appear in the PE export table (checkable via dumpbin).
==============================================================================*)
procedure Test11(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test11: Public Exports (Phase 5c)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test11.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('MyAdd', vtInt32, False, plC, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Add(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.Func('MyMul', vtInt32, False, plC, True)
       .Param('x', vtInt32)
       .Param('y', vtInt32)
       .Return(LTiger.Mul(LTiger.Get('x'), LTiger.Get('y')))
    .EndFunc();

    LTiger.Func('MySquare', vtInt32, False, plDefault, True)
       .Param('n', vtInt32)
       .Return(LTiger.Mul(LTiger.Get('n'), LTiger.Get('n')))
    .EndFunc();

    LTiger.Func('PrivateHelper', vtInt32, False, plC, False)
       .Param('v', vtInt32)
       .Return(LTiger.Add(LTiger.Get('v'), LTiger.Int64(100)))
    .EndFunc();

    LTiger.Func('main', vtVoid, True)
       .Local('result', vtInt32)

       .Call('printf', [LTiger.Str('=== Test11: Public Exports ===' + #10)])
       .Assign('result', LTiger.Invoke('MyAdd', [LTiger.Int64(10), LTiger.Int64(20)]))
       .Call('printf', [LTiger.Str('MyAdd(10, 20) = %d (expected 30)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('MyMul', [LTiger.Int64(6), LTiger.Int64(7)]))
       .Call('printf', [LTiger.Str('MyMul(6, 7) = %d (expected 42)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('MySquare', [LTiger.Int64(9)]))
       .Call('printf', [LTiger.Str('MySquare(9) = %d (expected 81)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('PrivateHelper', [LTiger.Int64(5)]))
       .Call('printf', [LTiger.Str('PrivateHelper(5) = %d (expected 105)' + #10), LTiger.Get('result')])

       .Call('printf', [LTiger.Str(#10 + 'Use "dumpbin /exports Test11.exe" to verify exports:' + #10)])
       .Call('printf', [LTiger.Str('  Expected: MyAdd, MyMul, MySquare' + #10)])
       .Call('printf', [LTiger.Str('  NOT exported: PrivateHelper, main' + #10)])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test11.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test12: Function overloading (Phase 5d)
  Tests BeginOverloadFunc with overloads resolved by parameter types and
  count: Add(int32,int32), Add(int64,int64), Multiply(int32,int32),
  Multiply(int32,int32,int32). Verifies correct dispatch and that each
  overload gets a unique mangled export name.
==============================================================================*)
procedure Test12(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test12: Function Overloading (Phase 5d)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test12.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.OverloadFunc('Add', vtInt32, False, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Add(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.OverloadFunc('Add', vtInt64, False, True)
       .Param('a', vtInt64)
       .Param('b', vtInt64)
       .Return(LTiger.Add(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.OverloadFunc('Multiply', vtInt32, False, True)
       .Param('x', vtInt32)
       .Param('y', vtInt32)
       .Return(LTiger.Mul(LTiger.Get('x'), LTiger.Get('y')))
    .EndFunc();

    LTiger.OverloadFunc('Multiply', vtInt32, False, True)
       .Param('x', vtInt32)
       .Param('y', vtInt32)
       .Param('z', vtInt32)
       .Return(LTiger.Mul(LTiger.Mul(LTiger.Get('x'), LTiger.Get('y')), LTiger.Get('z')))
    .EndFunc();

    LTiger.Func('main', vtVoid, True)
       .Local('r32', vtInt32)
       .Local('r64', vtInt64)

       .Call('printf', [LTiger.Str('=== Test12: Function Overloading ===' + #10)])
       .Assign('r32', LTiger.Invoke('Add', [LTiger.Int32(10), LTiger.Int32(20)]))
       .Call('printf', [LTiger.Str('Add(10, 20) int32 = %d (expected 30)' + #10), LTiger.Get('r32')])
       .Assign('r64', LTiger.Invoke('Add', [LTiger.Int64(100), LTiger.Int64(200)]))
       .Call('printf', [LTiger.Str('Add(100, 200) int64 = %lld (expected 300)' + #10), LTiger.Get('r64')])
       .Assign('r32', LTiger.Invoke('Multiply', [LTiger.Int32(6), LTiger.Int32(7)]))
       .Call('printf', [LTiger.Str('Multiply(6, 7) = %d (expected 42)' + #10), LTiger.Get('r32')])
       .Assign('r32', LTiger.Invoke('Multiply', [LTiger.Int32(2), LTiger.Int32(3), LTiger.Int32(4)]))
       .Call('printf', [LTiger.Str('Multiply(2, 3, 4) = %d (expected 24)' + #10), LTiger.Get('r32')])

       .Call('printf', [LTiger.Str(#10 + 'Use "dumpbin /exports Test12.exe" to verify mangled exports:' + #10)])
       .Call('printf', [LTiger.Str('  Each overload should have a unique mangled name' + #10)])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test12.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test13: DLL generation (Phase 5e)
  Two-part test: first builds Test13.dll with AddC (C linkage exported),
  AddCpp (C++ linkage exported), PrivateHelper (internal only), and a
  DllMain entry point. Then builds Test13.exe that imports AddC and AddCpp
  from the generated DLL and calls them. Validates the full DLL build
  pipeline including export tables, import resolution, and cross-module calls.
==============================================================================*)
procedure Test13(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  //============================================================================
  // Test13: DLL Generation (Phase 5e)
  //============================================================================
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test13: DLL Generation (Phase 5e)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  //----------------------------------------------------------------------------
  // Part 1: Build Test13.dll
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('--- Building Test13.dll ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test13.dll');

    LTiger.Func('PrivateHelper', vtInt32, False, plC, False)
       .Param('x', vtInt32)
       .Param('y', vtInt32)
       .Local('sum', vtInt32)
       .Assign('sum', LTiger.Add(LTiger.Get('x'), LTiger.Get('y')))
       .Assign('sum', LTiger.Add(LTiger.Get('sum'), LTiger.Int64(100)))
       .Return(LTiger.Get('sum'))
    .EndFunc();

    LTiger.Func('AddC', vtInt32, False, plC, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Add(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.Func('AddCpp', vtInt32, False, plDefault, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Invoke('PrivateHelper', [LTiger.Get('a'), LTiger.Get('b')]))
    .EndFunc();

    LTiger.DllMain()
       .Return(LTiger.Int64(1))
    .EndFunc();

    LTiger.TargetDll(TPath.Combine(COutputPath, 'Test13.dll'));

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;

  //----------------------------------------------------------------------------
  // Part 2: Build Test13.exe that imports from Test13.dll
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Building Test13.exe ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test13.exe');
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    LTiger.ImportDll('Test13.dll', 'AddC', [vtInt32, vtInt32], vtInt32, False, plC);
    LTiger.ImportDll('Test13.dll', 'AddCpp', [vtInt32, vtInt32], vtInt32, False, plDefault);

    LTiger.Func('main', vtVoid, True)
       .Local('r1', vtInt32)
       .Local('r2', vtInt32)

       .Call('printf', [LTiger.Str('=== Test13: DLL Generation ===' + #10)])
       .Call('printf', [LTiger.Str(#10)])
       .Assign('r1', LTiger.Invoke('AddC', [LTiger.Int64(10), LTiger.Int64(20)]))
       .Call('printf', [LTiger.Str('AddC(10, 20) = %d (expected 30)' + #10), LTiger.Get('r1')])
       .Assign('r2', LTiger.Invoke('AddCpp', [LTiger.Int64(10), LTiger.Int64(20)]))
       .Call('printf', [LTiger.Str('AddCpp(10, 20) = %d (expected 130, includes +100 from PrivateHelper)' + #10), LTiger.Get('r2')])

       .Call('printf', [LTiger.Str(#10 + 'DLL Import Tests:' + #10)])
       .Call('printf', [LTiger.Str('  - C linkage import (AddC): works if result = 30' + #10)])
       .Call('printf', [LTiger.Str('  - C++ linkage import (AddCpp): works if result = 130' + #10)])
       .Call('printf', [LTiger.Str('  - Private function not exported but callable internally' + #10)])

       .Call('printf', [LTiger.Str(#10 + 'Use "dumpbin /exports Test13.dll" to verify:' + #10)])
       .Call('printf', [LTiger.Str('  Expected exports: AddC, _Z6AddCppii' + #10)])
       .Call('printf', [LTiger.Str('  NOT exported: PrivateHelper, DllMain' + #10)])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test13.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test14: Runtime memory management (Phase 5f)
  Tests the Tiger runtime's Tiger_GetMem/Tiger_FreeMem heap functions.
  Allocates memory, writes/reads through pointers, performs multiple
  independent allocations to verify they don't overlap, and frees all.
  Also includes an unused function to verify DCE removes it at opt level 1.
  Uses Tiger_Halt instead of ExitProcess.
==============================================================================*)
procedure Test14(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test14: Runtime Memory (Phase 5f)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test14.exe');
    //LTiger.SetOptimizationLevel(1);

    // Add an UNUSED function to test DCE removes it
    LTiger.Func('UnusedFunc', vtInt32, False, plC, False)
       .Param('x', vtInt32)
       .Return(LTiger.Get('x'))
    .EndFunc();

    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('ptr', vtPointer)
       .Local('val', vtInt32)

       .Call('printf', [LTiger.Str('=== Test14: Runtime Memory ===' + #10)])
       .Call('printf', [LTiger.Str(#10)])

       // Test 1: Allocate, write, read, free
       .Call('printf', [LTiger.Str('Test 1: Basic GetMem/FreeMem' + #10)])
       .Assign('ptr', LTiger.Invoke('Tiger_GetMem', [LTiger.Int64(8)]))
       .Call('printf', [LTiger.Str('  Allocated 8 bytes at: %p' + #10), LTiger.Get('ptr')])
       .AssignTo(LTiger.Deref(LTiger.Get('ptr')), LTiger.Int64(12345))
       .Call('printf', [LTiger.Str('  Wrote value: 12345' + #10)])
       .Assign('val', LTiger.Deref(LTiger.Get('ptr')))
       .Call('printf', [LTiger.Str('  Read back: %d (expected 12345)' + #10), LTiger.Get('val')])
       .Call('Tiger_FreeMem', [LTiger.Get('ptr')])
       .Call('printf', [LTiger.Str('  Freed memory' + #10)])

       // Test 2: Multiple allocations
       .Call('printf', [LTiger.Str(#10 + 'Test 2: Multiple allocations' + #10)])
       .Local('ptr1', vtPointer)
       .Local('ptr2', vtPointer)
       .Local('ptr3', vtPointer)
       .Assign('ptr1', LTiger.Invoke('Tiger_GetMem', [LTiger.Int64(16)]))
       .Assign('ptr2', LTiger.Invoke('Tiger_GetMem', [LTiger.Int64(32)]))
       .Assign('ptr3', LTiger.Invoke('Tiger_GetMem', [LTiger.Int64(64)]))
       .Call('printf', [LTiger.Str('  ptr1 (16 bytes): %p' + #10), LTiger.Get('ptr1')])
       .Call('printf', [LTiger.Str('  ptr2 (32 bytes): %p' + #10), LTiger.Get('ptr2')])
       .Call('printf', [LTiger.Str('  ptr3 (64 bytes): %p' + #10), LTiger.Get('ptr3')])
       .AssignTo(LTiger.Deref(LTiger.Get('ptr1')), LTiger.Int64(111))
       .AssignTo(LTiger.Deref(LTiger.Get('ptr2')), LTiger.Int64(222))
       .AssignTo(LTiger.Deref(LTiger.Get('ptr3')), LTiger.Int64(333))
       .Call('printf', [LTiger.Str('  *ptr1 = %d (expected 111)' + #10), LTiger.Deref(LTiger.Get('ptr1'))])
       .Call('printf', [LTiger.Str('  *ptr2 = %d (expected 222)' + #10), LTiger.Deref(LTiger.Get('ptr2'))])
       .Call('printf', [LTiger.Str('  *ptr3 = %d (expected 333)' + #10), LTiger.Deref(LTiger.Get('ptr3'))])
       .Call('Tiger_FreeMem', [LTiger.Get('ptr1')])
       .Call('Tiger_FreeMem', [LTiger.Get('ptr2')])
       .Call('Tiger_FreeMem', [LTiger.Get('ptr3')])
       .Call('printf', [LTiger.Str('  All freed' + #10)])

       .Call('printf', [LTiger.Str(#10 + 'Memory tests complete!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test14.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test15: Managed strings (Phase 5g)
  Tests the Tiger runtime's reference-counted string system:
  Tiger_StrFromLiteral (create), Tiger_StrLen (length), Tiger_StrData
  (raw pointer), Tiger_StrConcat (concatenation), Tiger_StrAddRef/Release
  (manual refcount manipulation), and Tiger_StrAssign (refcount-safe
  assignment). Verifies correct string content, lengths, and that
  refcounts increment/decrement as expected.
==============================================================================*)
procedure Test15(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test15: Managed Strings (Phase 5g)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test15.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('s1', vtPointer)
       .Local('s2', vtPointer)
       .Local('s3', vtPointer)
       .Local('len', vtUInt64)
       .Local('data', vtPointer)

       .Call('printf', [LTiger.Str('=== Test15: Managed Strings ===' + #10)])
       .Call('printf', [LTiger.Str(#10)])

       // Test 1: Create string from literal
       .Call('printf', [LTiger.Str('Test 1: Create string from literal' + #10)])
       .Assign('s1', LTiger.Invoke('Tiger_StrFromLiteral', [LTiger.Str('Hello'), LTiger.Int64(5)]))
       .Assign('len', LTiger.Invoke('Tiger_StrLen', [LTiger.Get('s1')]))
       .Assign('data', LTiger.Invoke('Tiger_StrData', [LTiger.Get('s1')]))
       .Call('printf', [LTiger.Str('  s1 = "%s"' + #10), LTiger.Get('data')])
       .Call('printf', [LTiger.Str('  len(s1) = %llu (expected 5)' + #10), LTiger.Get('len')])

       // Test 2: Create another string
       .Call('printf', [LTiger.Str(#10 + 'Test 2: Create another string' + #10)])
       .Assign('s2', LTiger.Invoke('Tiger_StrFromLiteral', [LTiger.Str(' World'), LTiger.Int64(6)]))
       .Assign('len', LTiger.Invoke('Tiger_StrLen', [LTiger.Get('s2')]))
       .Assign('data', LTiger.Invoke('Tiger_StrData', [LTiger.Get('s2')]))
       .Call('printf', [LTiger.Str('  s2 = "%s"' + #10), LTiger.Get('data')])
       .Call('printf', [LTiger.Str('  len(s2) = %llu (expected 6)' + #10), LTiger.Get('len')])

       // Test 3: Concatenate strings
       .Call('printf', [LTiger.Str(#10 + 'Test 3: Concatenate strings' + #10)])
       .Assign('s3', LTiger.Invoke('Tiger_StrConcat', [LTiger.Get('s1'), LTiger.Get('s2')]))
       .Assign('len', LTiger.Invoke('Tiger_StrLen', [LTiger.Get('s3')]))
       .Assign('data', LTiger.Invoke('Tiger_StrData', [LTiger.Get('s3')]))
       .Call('printf', [LTiger.Str('  s3 = s1 + s2 = "%s"' + #10), LTiger.Get('data')])
       .Call('printf', [LTiger.Str('  len(s3) = %llu (expected 11)' + #10), LTiger.Get('len')])

       // Test 4: Reference counting
       .Call('printf', [LTiger.Str(#10 + 'Test 4: Reference counting' + #10)])
       .Call('printf', [LTiger.Str('  s1 refcount before AddRef: %lld' + #10),
          LTiger.GetField(LTiger.Deref(LTiger.Get('s1'), 'TStringRec'), 'RefCount')])
       .Call('Tiger_StrAddRef', [LTiger.Get('s1')])
       .Call('printf', [LTiger.Str('  s1 refcount after AddRef: %lld (expected 2)' + #10),
          LTiger.GetField(LTiger.Deref(LTiger.Get('s1'), 'TStringRec'), 'RefCount')])
       .Call('Tiger_StrRelease', [LTiger.Get('s1')])
       .Call('printf', [LTiger.Str('  s1 refcount after Release: %lld (expected 1)' + #10),
          LTiger.GetField(LTiger.Deref(LTiger.Get('s1'), 'TStringRec'), 'RefCount')])

       // Test 5: StrAssign with refcount
       .Call('printf', [LTiger.Str(#10 + 'Test 5: StrAssign with refcount' + #10)])
       .Local('s4', vtPointer)
       .Assign('s4', LTiger.Null())
       .Call('Tiger_StrAssign', [LTiger.AddrOf('s4'), LTiger.Get('s1')])
       .Call('printf', [LTiger.Str('  After s4 := s1:' + #10)])
       .Call('printf', [LTiger.Str('    s1 refcount: %lld (expected 2)' + #10),
          LTiger.GetField(LTiger.Deref(LTiger.Get('s1'), 'TStringRec'), 'RefCount')])
       .Assign('data', LTiger.Invoke('Tiger_StrData', [LTiger.Get('s4')]))
       .Call('printf', [LTiger.Str('    s4 = "%s"' + #10), LTiger.Get('data')])

       // Cleanup
       .Call('printf', [LTiger.Str(#10 + 'Cleanup: Releasing all strings' + #10)])
       .Call('Tiger_StrRelease', [LTiger.Get('s1')])
       .Call('Tiger_StrRelease', [LTiger.Get('s2')])
       .Call('Tiger_StrRelease', [LTiger.Get('s3')])
       .Call('Tiger_StrRelease', [LTiger.Get('s4')])
       .Call('printf', [LTiger.Str('  All strings released' + #10)])

       .Call('printf', [LTiger.Str(#10 + 'String tests complete!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test15.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test16: ucrtbase/msvcrt printf validation
  Minimal test confirming msvcrt.dll printf works with the Tiger runtime
  (Tiger_Halt). Prints a string, an integer format, and exits. Serves as
  a baseline sanity check for CRT interop.
==============================================================================*)
procedure Test16(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test16: ucrtbase.dll (Universal CRT)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test16.exe');
    LTiger.SetOptimizationLevel(1);

    // msvcrt.dll exports printf directly and works without CRT initialization
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('dummy', vtInt32)

       .Call('printf', [LTiger.Str('=== Test16: msvcrt.dll printf ===' + #10)])
       .Call('printf', [LTiger.Str('Hello from Test16!' + #10)])
       .Call('printf', [LTiger.Str('Integer: %d' + #10), LTiger.Int32(42)])
       .Call('printf', [LTiger.Str('Done!' + #10)])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test16.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test17: Structured exception handling (SEH)
  Tests the Tiger SEH implementation: try/finally (always-execute),
  try/except without exception (except block skipped), try/except with
  Raise_ (software exception caught), and try/except with hardware
  exception (integer division by zero caught). Verifies control flow
  resumes correctly after each block.
==============================================================================*)
procedure Test17(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test17: Exception Handling (SEH)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test17.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('dummy', vtInt32)
       .Local('divisor', vtInt32)
       .Local('result', vtInt32)

       .Call('printf', [LTiger.Str('=== Test17: Exception Handling ===' + #10)])

       // Test 1: try/finally
       .Call('printf', [LTiger.Str('Test 1: try/finally' + #10)])
       .&Try()
          .Call('printf', [LTiger.Str('  Inside try block' + #10)])
       .&Finally()
          .Call('printf', [LTiger.Str('  Inside finally block' + #10)])
       .EndTry()
       .Call('printf', [LTiger.Str('  After try/finally' + #10)])
       .Call('printf', [LTiger.Str('' + #10)])

       // Test 2: try/except without exception
       .Call('printf', [LTiger.Str('Test 2: try/except (no exception)' + #10)])
       .&Try()
          .Call('printf', [LTiger.Str('  Inside try block' + #10)])
       .&Except()
          .Call('printf', [LTiger.Str('  ERROR: Should NOT reach except!' + #10)])
       .EndTry()
       .Call('printf', [LTiger.Str('  After try/except' + #10)])
       .Call('printf', [LTiger.Str('' + #10)])

       // Test 3: try/except WITH exception
       .Call('printf', [LTiger.Str('Test 3: try/except (with raise)' + #10)])
       .&Try()
          .Call('printf', [LTiger.Str('  Inside try block' + #10)])
          .Call('printf', [LTiger.Str('  About to raise...' + #10)])
          .&Raise(LTiger.Str('Test exception!'))
          .Call('printf', [LTiger.Str('  ERROR: Should NOT print after raise!' + #10)])
       .&Except()
          .Call('printf', [LTiger.Str('  Inside except block (caught!)' + #10)])
       .EndTry()
       .Call('printf', [LTiger.Str('  After try/except' + #10)])
       .Call('printf', [LTiger.Str('' + #10)])

       // Test 4: Hardware exception (div by zero)
       .Call('printf', [LTiger.Str('Test 4: try/except (div by zero)' + #10)])
       .Assign('divisor', LTiger.Int64(0))
       .&Try()
          .Call('printf', [LTiger.Str('  Inside try block' + #10)])
          .Call('printf', [LTiger.Str('  About to divide by zero...' + #10)])
          .Assign('result', LTiger.IDiv(LTiger.Int64(100), LTiger.Get('divisor')))
          .Call('printf', [LTiger.Str('  ERROR: Should NOT print after div/0!' + #10)])
       .&Except()
          .Call('printf', [LTiger.Str('  Inside except block (caught div/0!)' + #10)])
       .EndTry()
       .Call('printf', [LTiger.Str('  After try/except' + #10)])
       .Call('printf', [LTiger.Str('' + #10)])

       .Call('printf', [LTiger.Str('All tests passed!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test17.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test18: Set types and operations
  Defines sets of various sizes (TSmallSet 0..7, TMediumSet 0..31,
  TLargeSet 0..63, TOffsetSet 100..163). Tests set literals, empty sets,
  membership (SetIn), union (+), intersection ( * ), difference (-), and
  equality (=). Verifies correct boolean results for each operation.
==============================================================================*)
procedure Test18(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test18: Sets');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test18.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.DefineSet('TSmallSet', 0, 7);
    LTiger.DefineSet('TMediumSet', 0, 31);
    LTiger.DefineSet('TLargeSet', 0, 63);
    LTiger.DefineSet('TOffsetSet', 100, 163);

    LTiger.Func('main', vtVoid, True)
       .Local('s1', 'TSmallSet')
       .Local('s2', 'TSmallSet')
       .Local('s3', 'TSmallSet')
       .Local('result', vtInt32)

       .Call('printf', [LTiger.Str('=== Test18: Sets ===' + #10)])

       // Test 1: Set literal creation
       .Call('printf', [LTiger.Str('Test 1: Set literals' + #10)])
       .Assign('s1', LTiger.SetLit('TSmallSet', [1, 3, 5]))
       .Call('printf', [LTiger.Str('  s1 = {1, 3, 5}' + #10)])

       // Test 2: Empty set
       .Assign('s2', LTiger.EmptySet('TSmallSet'))
       .Call('printf', [LTiger.Str('  s2 = {} (empty)' + #10)])

       // Test 3: Membership test
       .Call('printf', [LTiger.Str('Test 2: Membership (in)' + #10)])
       .&If(LTiger.SetIn(LTiger.Int64(1), LTiger.Get('s1')))
          .Call('printf', [LTiger.Str('  1 in s1: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  1 in s1: false' + #10)])
       .EndIf()
       .&If(LTiger.SetIn(LTiger.Int64(2), LTiger.Get('s1')))
          .Call('printf', [LTiger.Str('  2 in s1: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  2 in s1: false' + #10)])
       .EndIf()

       // Test 4: Set union
       .Call('printf', [LTiger.Str('Test 3: Union (+)' + #10)])
       .Assign('s2', LTiger.SetLit('TSmallSet', [2, 4]))
       .Assign('s3', LTiger.SetUnion(LTiger.Get('s1'), LTiger.Get('s2')))
       .Call('printf', [LTiger.Str('  {1,3,5} + {2,4} = s3' + #10)])
       .&If(LTiger.SetIn(LTiger.Int64(1), LTiger.Get('s3')))
          .Call('printf', [LTiger.Str('  1 in s3: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  1 in s3: false' + #10)])
       .EndIf()
       .&If(LTiger.SetIn(LTiger.Int64(2), LTiger.Get('s3')))
          .Call('printf', [LTiger.Str('  2 in s3: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  2 in s3: false' + #10)])
       .EndIf()

       // Test 5: Set intersection
       .Call('printf', [LTiger.Str('Test 4: Intersection (*)' + #10)])
       .Assign('s1', LTiger.SetLit('TSmallSet', [1, 2, 3]))
       .Assign('s2', LTiger.SetLit('TSmallSet', [2, 3, 4]))
       .Assign('s3', LTiger.SetInter(LTiger.Get('s1'), LTiger.Get('s2')))
       .Call('printf', [LTiger.Str('  {1,2,3} * {2,3,4} = s3' + #10)])
       .&If(LTiger.SetIn(LTiger.Int64(1), LTiger.Get('s3')))
          .Call('printf', [LTiger.Str('  1 in s3: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  1 in s3: false' + #10)])
       .EndIf()
       .&If(LTiger.SetIn(LTiger.Int64(2), LTiger.Get('s3')))
          .Call('printf', [LTiger.Str('  2 in s3: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  2 in s3: false' + #10)])
       .EndIf()

       // Test 6: Set difference
       .Call('printf', [LTiger.Str('Test 5: Difference (-)' + #10)])
       .Assign('s3', LTiger.SetDiff(LTiger.Get('s1'), LTiger.Get('s2')))
       .Call('printf', [LTiger.Str('  {1,2,3} - {2,3,4} = s3' + #10)])
       .&If(LTiger.SetIn(LTiger.Int64(1), LTiger.Get('s3')))
          .Call('printf', [LTiger.Str('  1 in s3: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  1 in s3: false' + #10)])
       .EndIf()
       .&If(LTiger.SetIn(LTiger.Int64(2), LTiger.Get('s3')))
          .Call('printf', [LTiger.Str('  2 in s3: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  2 in s3: false' + #10)])
       .EndIf()

       // Test 7: Set equality
       .Call('printf', [LTiger.Str('Test 6: Equality (=)' + #10)])
       .Assign('s1', LTiger.SetLit('TSmallSet', [1, 2, 3]))
       .Assign('s2', LTiger.SetLit('TSmallSet', [1, 2, 3]))
       .&If(LTiger.SetEq(LTiger.Get('s1'), LTiger.Get('s2')))
          .Call('printf', [LTiger.Str('  {1,2,3} = {1,2,3}: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  {1,2,3} = {1,2,3}: false' + #10)])
       .EndIf()
       .Assign('s2', LTiger.SetLit('TSmallSet', [1, 2]))
       .&If(LTiger.SetEq(LTiger.Get('s1'), LTiger.Get('s2')))
          .Call('printf', [LTiger.Str('  {1,2,3} = {1,2}: true' + #10)])
       .&Else()
          .Call('printf', [LTiger.Str('  {1,2,3} = {1,2}: false' + #10)])
       .EndIf()

       .Call('printf', [LTiger.Str(#10 + 'All tests passed!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test18.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test19: Compile-time intrinsics
  Tests all compile-time and inline intrinsics: SizeOf_ (type sizes),
  AlignOf_ (type alignment), High_/Low_ on arrays, enums, and sets,
  Len_ (array element count), Ord_/Chr_ (identity casts), Succ_/Pred_
  (increment/decrement expressions), and Inc_/Dec_ (in-place variable
  increment/decrement with optional step). All values are verified
  against expected constants at runtime via printf.
==============================================================================*)
procedure Test19(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test19: Compile-Time Intrinsics');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test19.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    // Define test types
    LTiger.DefineRecord('TPoint')
         .Field('X', vtInt32)
         .Field('Y', vtInt32)
       .EndRecord();

    LTiger.DefineRecord('TBigRecord')
         .Field('A', vtInt32)
         .Field('B', vtInt64)
         .Field('C', vtInt32)
       .EndRecord();

    LTiger.DefineArray('TIntArray', vtInt32, 0, 9);
    LTiger.DefineArray('TOffsetArray', vtInt32, 5, 9);

    LTiger.DefineEnum('TColor')
         .EnumValue('Red', 0)
         .EnumValue('Green', 1)
         .EnumValue('Blue', 2)
       .EndEnum();

    LTiger.DefineEnum('TPriority')
         .EnumValue('Low', 10)
         .EnumValue('Medium', 20)
         .EnumValue('High', 30)
       .EndEnum();

    LTiger.DefineSet('TSmallSet', 0, 7);

    LTiger.Func('main', vtVoid, True)
       .Local('result', vtInt64)

       .Call('printf', [LTiger.Str('=== Test19: Compile-Time Intrinsics ===' + #10)])

       // Test SizeOf
       .Call('printf', [LTiger.Str(#10 + 'Test 1: SizeOf' + #10)])
       .Call('printf', [LTiger.Str('  SizeOf(TPoint) = %d (expected: 8)' + #10), LTiger.&SizeOf('TPoint')])
       .Call('printf', [LTiger.Str('  SizeOf(TBigRecord) = %d (expected: 24)' + #10), LTiger.&SizeOf('TBigRecord')])
       .Call('printf', [LTiger.Str('  SizeOf(TIntArray) = %d (expected: 40)' + #10), LTiger.&SizeOf('TIntArray')])

       // Test AlignOf
       .Call('printf', [LTiger.Str(#10 + 'Test 2: AlignOf' + #10)])
       .Call('printf', [LTiger.Str('  AlignOf(TPoint) = %d (expected: 4)' + #10), LTiger.AlignOf('TPoint')])
       .Call('printf', [LTiger.Str('  AlignOf(TBigRecord) = %d (expected: 8)' + #10), LTiger.AlignOf('TBigRecord')])

       // Test High/Low on arrays
       .Call('printf', [LTiger.Str(#10 + 'Test 3: High/Low on arrays' + #10)])
       .Call('printf', [LTiger.Str('  Low(TIntArray) = %d (expected: 0)' + #10), LTiger.Low('TIntArray')])
       .Call('printf', [LTiger.Str('  High(TIntArray) = %d (expected: 9)' + #10), LTiger.High('TIntArray')])
       .Call('printf', [LTiger.Str('  Low(TOffsetArray) = %d (expected: 5)' + #10), LTiger.Low('TOffsetArray')])
       .Call('printf', [LTiger.Str('  High(TOffsetArray) = %d (expected: 9)' + #10), LTiger.High('TOffsetArray')])

       // Test Len on arrays
       .Call('printf', [LTiger.Str(#10 + 'Test 4: Len on arrays' + #10)])
       .Call('printf', [LTiger.Str('  Len(TIntArray) = %d (expected: 10)' + #10), LTiger.Len('TIntArray')])
       .Call('printf', [LTiger.Str('  Len(TOffsetArray) = %d (expected: 5)' + #10), LTiger.Len('TOffsetArray')])

       // Test High/Low on enums
       .Call('printf', [LTiger.Str(#10 + 'Test 5: High/Low on enums' + #10)])
       .Call('printf', [LTiger.Str('  Low(TColor) = %d (expected: 0)' + #10), LTiger.Low('TColor')])
       .Call('printf', [LTiger.Str('  High(TColor) = %d (expected: 2)' + #10), LTiger.High('TColor')])
       .Call('printf', [LTiger.Str('  Low(TPriority) = %d (expected: 10)' + #10), LTiger.Low('TPriority')])
       .Call('printf', [LTiger.Str('  High(TPriority) = %d (expected: 30)' + #10), LTiger.High('TPriority')])

       // Test High/Low on sets
       .Call('printf', [LTiger.Str(#10 + 'Test 6: High/Low on sets' + #10)])
       .Call('printf', [LTiger.Str('  Low(TSmallSet) = %d (expected: 0)' + #10), LTiger.Low('TSmallSet')])
       .Call('printf', [LTiger.Str('  High(TSmallSet) = %d (expected: 7)' + #10), LTiger.High('TSmallSet')])

       // Test 7: Ord/Chr
       .Call('printf', [LTiger.Str(#10 + 'Test 7: Ord/Chr' + #10)])
       .Assign('result', LTiger.Ord(LTiger.Int64(65)))
       .Call('printf', [LTiger.Str('  Ord(65) = %d (expected: 65)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Chr(LTiger.Int64(66)))
       .Call('printf', [LTiger.Str('  Chr(66) = %d (expected: 66)' + #10), LTiger.Get('result')])

       // Test 8: Succ/Pred
       .Call('printf', [LTiger.Str(#10 + 'Test 8: Succ/Pred' + #10)])
       .Assign('result', LTiger.Succ(LTiger.Int64(10)))
       .Call('printf', [LTiger.Str('  Succ(10) = %d (expected: 11)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Pred(LTiger.Int64(10)))
       .Call('printf', [LTiger.Str('  Pred(10) = %d (expected: 9)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Succ(LTiger.Pred(LTiger.Int64(5))))
       .Call('printf', [LTiger.Str('  Succ(Pred(5)) = %d (expected: 5)' + #10), LTiger.Get('result')])

       // Test 9: Inc/Dec
       .Call('printf', [LTiger.Str(#10 + 'Test 9: Inc/Dec' + #10)])
       .Assign('result', LTiger.Int64(10))
       .&Inc('result')
       .Call('printf', [LTiger.Str('  Inc(10) = %d (expected: 11)' + #10), LTiger.Get('result')])
       .&Dec('result')
       .&Dec('result')
       .Call('printf', [LTiger.Str('  Dec(Dec(11)) = %d (expected: 9)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Int64(5))
       .&Inc('result', LTiger.Int64(10))
       .Call('printf', [LTiger.Str('  Inc(5, 10) = %d (expected: 15)' + #10), LTiger.Get('result')])
       .&Dec('result', LTiger.Int64(7))
       .Call('printf', [LTiger.Str('  Dec(15, 7) = %d (expected: 8)' + #10), LTiger.Get('result')])

       .Call('printf', [LTiger.Str(#10 + 'All tests passed!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test19.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test20: Variadic functions
  Tests BeginVariadicFunc with VaCount_ (argument count intrinsic) and
  VaArgAt_ (indexed argument access). Includes: SumAll (pure varargs,
  sums all arguments), SumWithMult (fixed param + varargs), and CountArgs
  (returns VaCount_). Validates 0 to 7 variadic arguments, correct
  count reporting, and correct value retrieval by index.
==============================================================================*)
procedure Test20(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test20: Variadic Functions');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test20.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    // Test 1: Simple variadic - sum all args
    LTiger.VariadicFunc('SumAll', vtInt64)
       .Local('sum', vtInt64)
       .Local('i', vtInt64)
       .Local('count', vtInt64)
       .Assign('sum', LTiger.Int64(0))
       .Assign('count', LTiger.VaCount())
       .Assign('i', LTiger.Int64(0))
       .&While(LTiger.Lt(LTiger.Get('i'), LTiger.Get('count')))
          .Assign('sum', LTiger.Add(LTiger.Get('sum'), LTiger.VaArg(LTiger.Get('i'), vtInt64)))
          .Assign('i', LTiger.Add(LTiger.Get('i'), LTiger.Int64(1)))
       .EndWhile()
       .Return(LTiger.Get('sum'))
    .EndFunc();

    // Test 2: Variadic with fixed param
    LTiger.VariadicFunc('SumWithMult', vtInt64)
       .Param('multiplier', vtInt64)
       .Local('sum', vtInt64)
       .Local('i', vtInt64)
       .Local('count', vtInt64)
       .Assign('sum', LTiger.Int64(0))
       .Assign('count', LTiger.VaCount())
       .Assign('i', LTiger.Int64(0))
       .&While(LTiger.Lt(LTiger.Get('i'), LTiger.Get('count')))
          .Assign('sum', LTiger.Add(LTiger.Get('sum'), LTiger.VaArg(LTiger.Get('i'), vtInt64)))
          .Assign('i', LTiger.Add(LTiger.Get('i'), LTiger.Int64(1)))
       .EndWhile()
       .Return(LTiger.Mul(LTiger.Get('sum'), LTiger.Get('multiplier')))
    .EndFunc();

    // Test 3: Just returns count
    LTiger.VariadicFunc('CountArgs', vtInt64)
       .Return(LTiger.VaCount())
    .EndFunc();

    LTiger.Func('main', vtVoid, True)
       .Local('result', vtInt64)

       .Call('printf', [LTiger.Str('=== Test20: Variadic Functions ===' + #10)])

       .Call('printf', [LTiger.Str(#10 + 'Test 1: SumAll (sum all varargs)' + #10)])
       .Assign('result', LTiger.Invoke('SumAll', []))
       .Call('printf', [LTiger.Str('  SumAll() = %lld (expected: 0)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('SumAll', [LTiger.Int64(10)]))
       .Call('printf', [LTiger.Str('  SumAll(10) = %lld (expected: 10)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('SumAll', [LTiger.Int64(1), LTiger.Int64(2), LTiger.Int64(3)]))
       .Call('printf', [LTiger.Str('  SumAll(1, 2, 3) = %lld (expected: 6)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('SumAll', [LTiger.Int64(10), LTiger.Int64(20), LTiger.Int64(30), LTiger.Int64(40), LTiger.Int64(50)]))
       .Call('printf', [LTiger.Str('  SumAll(10,20,30,40,50) = %lld (expected: 150)' + #10), LTiger.Get('result')])

       .Call('printf', [LTiger.Str(#10 + 'Test 2: SumWithMult (fixed param + varargs)' + #10)])
       .Assign('result', LTiger.Invoke('SumWithMult', [LTiger.Int64(2)]))
       .Call('printf', [LTiger.Str('  SumWithMult(2) = %lld (expected: 0)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('SumWithMult', [LTiger.Int64(2), LTiger.Int64(5)]))
       .Call('printf', [LTiger.Str('  SumWithMult(2, 5) = %lld (expected: 10)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('SumWithMult', [LTiger.Int64(3), LTiger.Int64(1), LTiger.Int64(2), LTiger.Int64(3)]))
       .Call('printf', [LTiger.Str('  SumWithMult(3, 1,2,3) = %lld (expected: 18)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('SumWithMult', [LTiger.Int64(2), LTiger.Int64(10), LTiger.Int64(20), LTiger.Int64(30), LTiger.Int64(40)]))
       .Call('printf', [LTiger.Str('  SumWithMult(2, 10,20,30,40) = %lld (expected: 200)' + #10), LTiger.Get('result')])

       .Call('printf', [LTiger.Str(#10 + 'Test 3: CountArgs (VaCount_ intrinsic)' + #10)])
       .Assign('result', LTiger.Invoke('CountArgs', []))
       .Call('printf', [LTiger.Str('  CountArgs() = %lld (expected: 0)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('CountArgs', [LTiger.Int64(1)]))
       .Call('printf', [LTiger.Str('  CountArgs(1) = %lld (expected: 1)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('CountArgs', [LTiger.Int64(1), LTiger.Int64(2), LTiger.Int64(3), LTiger.Int64(4), LTiger.Int64(5)]))
       .Call('printf', [LTiger.Str('  CountArgs(1,2,3,4,5) = %lld (expected: 5)' + #10), LTiger.Get('result')])
       .Assign('result', LTiger.Invoke('CountArgs', [LTiger.Int64(1), LTiger.Int64(2), LTiger.Int64(3), LTiger.Int64(4), LTiger.Int64(5), LTiger.Int64(6), LTiger.Int64(7)]))
       .Call('printf', [LTiger.Str('  CountArgs(1..7) = %lld (expected: 7)' + #10), LTiger.Get('result')])

       .Call('printf', [LTiger.Str(#10 + 'All variadic tests complete!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test20.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test21: MessageBoxA from user32.dll
  Imports MessageBoxA and displays a modal dialog box. Tests Win32 API
  interop with HWND (uint64), string pointers, and UINT flags. The
  return value (button pressed) is printed to console.
==============================================================================*)
procedure Test21(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test21: MessageBoxA from user32.dll');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test21.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    LTiger.ImportDll('user32.dll', 'MessageBoxA', [vtUInt64, vtPointer, vtPointer, vtUInt32], vtInt32);

    LTiger.Func('main', vtVoid, True)
       .Local('result', vtInt32)

       .Call('printf', [LTiger.Str('Before MessageBoxA call...' + #10)])
       .Assign('result', LTiger.Invoke('MessageBoxA', [
          LTiger.Int64(0),
          LTiger.Str('Hello!'),
          LTiger.Str('Test'),
          LTiger.Int64(0)
       ]))
       .Call('printf', [LTiger.Str('After MessageBoxA, result = %d' + #10), LTiger.Get('result')])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test21.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test22: GetSystemMetrics from user32.dll
  Imports GetSystemMetrics and queries screen width (SM_CXSCREEN=0) and
  height (SM_CYSCREEN=1). Tests static linking to user32.dll alongside
  msvcrt.dll printf output.
==============================================================================*)
procedure Test22(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test22: GetSystemMetrics from user32.dll');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test22.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    LTiger.ImportDll('user32.dll', 'GetSystemMetrics', [vtInt32], vtInt32);

    LTiger.Func('main', vtVoid, True)
       .Local('screenWidth', vtInt32)
       .Local('screenHeight', vtInt32)

       .Call('printf', [LTiger.Str('Calling GetSystemMetrics...' + #10)])
       .Assign('screenWidth', LTiger.Invoke('GetSystemMetrics', [LTiger.Int64(0)]))
       .Call('printf', [LTiger.Str('Screen width = %d' + #10), LTiger.Get('screenWidth')])
       .Assign('screenHeight', LTiger.Invoke('GetSystemMetrics', [LTiger.Int64(1)]))
       .Call('printf', [LTiger.Str('Screen height = %d' + #10), LTiger.Get('screenHeight')])

       .Call('printf', [LTiger.Str('Done!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test22.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test23: Minimal user32.dll (no runtime, no msvcrt)
  Bare-minimum executable importing only kernel32 (ExitProcess) and
  user32 (GetSystemMetrics). No printf, no Tiger runtime. Calls
  GetSystemMetrics and exits with the screen width as the process exit
  code. Tests that the PE import table handles multiple DLLs without
  any CRT or runtime dependency.
==============================================================================*)
procedure Test23(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test23: Minimal user32.dll (no runtime)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test23.exe');
    LTiger.SetOptimizationLevel(1);

    // NO RUNTIME - just bare minimum imports
    LTiger.ImportDll('user32.dll', 'GetSystemMetrics', [vtInt32], vtInt32);

    LTiger.Func('main', vtVoid, True)
       .Local('result', vtInt32)
       .Assign('result', LTiger.Invoke('GetSystemMetrics', [LTiger.Int64(0)]))
       .Call('Tiger_Halt', [LTiger.Get('result')])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test23.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test24: Dynamic DLL loading (LoadLibrary/GetProcAddress)
  Uses kernel32's LoadLibraryA and GetProcAddress to dynamically resolve
  user32.dll!GetSystemMetrics at runtime. Prints the module handle and
  function address. Tests dynamic import resolution as an alternative
  to static PE import table entries.
==============================================================================*)
procedure Test24(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test24: Dynamic user32.dll (LoadLibrary)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test24.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('kernel32.dll', 'LoadLibraryA', [vtPointer], vtPointer);
    LTiger.ImportDll('kernel32.dll', 'GetProcAddress', [vtPointer, vtPointer], vtPointer);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('hUser32', vtPointer)
       .Local('pFunc', vtPointer)
       .Local('result', vtInt32)

       .Call('printf', [LTiger.Str('Loading user32.dll...' + #10)])
       .Assign('hUser32', LTiger.Invoke('LoadLibraryA', [LTiger.Str('user32.dll')]))
       .Call('printf', [LTiger.Str('hUser32 = %p' + #10), LTiger.Get('hUser32')])
       .Assign('pFunc', LTiger.Invoke('GetProcAddress', [LTiger.Get('hUser32'), LTiger.Str('GetSystemMetrics')]))
       .Call('printf', [LTiger.Str('GetSystemMetrics = %p' + #10), LTiger.Get('pFunc')])
       .Call('printf', [LTiger.Str('Success! Exiting with 42' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(42)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test24.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test25: Multi-DLL import ordering (msvcrt + kernel32 + user32)
  Same as Test22 but imports msvcrt first, then kernel32, then user32.
  Validates that import table generation handles three DLLs in a
  different declaration order without address resolution errors.
==============================================================================*)
procedure Test25(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test25: user32 with msvcrt first');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test25.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    LTiger.ImportDll('user32.dll', 'GetSystemMetrics', [vtInt32], vtInt32);

    LTiger.Func('main', vtVoid, True)
       .Local('width', vtInt32)

       .Call('printf', [LTiger.Str('Calling GetSystemMetrics...' + #10)])
       .Assign('width', LTiger.Invoke('GetSystemMetrics', [LTiger.Int64(0)]))
       .Call('printf', [LTiger.Str('Screen width = %d' + #10), LTiger.Get('width')])
       .Call('printf', [LTiger.Str('Success!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test25.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test26: Static linking (.lib -> .exe)
  Two-part test: first builds Test26.lib containing AddC, MulC (C linkage
  exported), AddCpp (C++ linkage exported), and PrivateHelper (internal).
  Then builds Test26.exe that statically links the .lib via ImportLib and
  AddLibPath, calling all three public functions. Validates the COFF .lib
  emitter, symbol resolution, and static linking pipeline.
==============================================================================*)
procedure Test26(const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  //============================================================================
  // Test26: Static Linking (.lib -> .exe)
  //============================================================================
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test26: Static Linking (.lib -> .exe)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  //----------------------------------------------------------------------------
  // Part 1: Build Test26.lib with public functions
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('--- Building Test26.lib ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test26.lib');

    LTiger.Func('PrivateHelper', vtInt32, False, plC, False)
       .Param('x', vtInt32)
       .Param('y', vtInt32)
       .Local('sum', vtInt32)
       .Assign('sum', LTiger.Add(LTiger.Get('x'), LTiger.Get('y')))
       .Assign('sum', LTiger.Add(LTiger.Get('sum'), LTiger.Int64(100)))
       .Return(LTiger.Get('sum'))
    .EndFunc();

    LTiger.Func('AddC', vtInt32, False, plC, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Add(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.Func('MulC', vtInt32, False, plC, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Mul(LTiger.Get('a'), LTiger.Get('b')))
    .EndFunc();

    LTiger.Func('AddCpp', vtInt32, False, plDefault, True)
       .Param('a', vtInt32)
       .Param('b', vtInt32)
       .Return(LTiger.Invoke('PrivateHelper', [LTiger.Get('a'), LTiger.Get('b')]))
    .EndFunc();

    LTiger.TargetLib(TPath.Combine(COutputPath, 'Test26.lib'));

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;

  //----------------------------------------------------------------------------
  // Part 2: Build Test26.exe that statically links Test26.lib
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Building Test26.exe ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create();
  try
    LTiger.SetStatusCallback(StatusCallback);
    SetExeResources(LTiger, 'Test26.exe');
    LTiger.SetOptimizationLevel(1);
    LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);

    LTiger.ImportLib('Test26', 'AddC', [vtInt32, vtInt32], vtInt32, False, plC);
    LTiger.ImportLib('Test26', 'MulC', [vtInt32, vtInt32], vtInt32, False, plC);
    LTiger.ImportLib('Test26', 'AddCpp', [vtInt32, vtInt32], vtInt32, False, plDefault);

    LTiger.Func('main', vtVoid, True)
       .Local('r1', vtInt32)
       .Local('r2', vtInt32)
       .Local('r3', vtInt32)

       .Call('printf', [LTiger.Str('=== Test26: Static Linking ===' + #10)])
       .Call('printf', [LTiger.Str(#10)])
       .Assign('r1', LTiger.Invoke('AddC', [LTiger.Int64(3), LTiger.Int64(4)]))
       .Call('printf', [LTiger.Str('AddC(3, 4) = %d (expect 7)' + #10), LTiger.Get('r1')])
       .Assign('r2', LTiger.Invoke('MulC', [LTiger.Int64(5), LTiger.Int64(6)]))
       .Call('printf', [LTiger.Str('MulC(5, 6) = %d (expect 30)' + #10), LTiger.Get('r2')])
       .Assign('r3', LTiger.Invoke('AddCpp', [LTiger.Int64(10), LTiger.Int64(20)]))
       .Call('printf', [LTiger.Str('AddCpp(10, 20) = %d (expect 130)' + #10), LTiger.Get('r3')])

       .Call('printf', [LTiger.Str(#10 + 'Success!' + #10)])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(COutputPath, 'Test26.exe'), ssConsole);
    LTiger.AddLibPath(COutputPath);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  RunTest: Executes a single test by number, dispatching to the corresponding
  test procedure. If ADumpSSA is True, the SSA intermediate representation
  is printed after the build completes.
==============================================================================*)
procedure RunTest(const ANum: Integer; const ADumpSSA: Boolean=False);
begin
  case ANum of
    01: Test01(ADumpSSA);
    02: Test02(ADumpSSA);
    03: Test03(ADumpSSA);
    04: Test04(ADumpSSA);
    05: Test05(ADumpSSA);
    06: Test06(ADumpSSA);
    07: Test07(ADumpSSA);
    08: Test08(ADumpSSA);
    09: Test09(ADumpSSA);
    10: Test10(ADumpSSA);
    11: Test11(ADumpSSA);
    12: Test12(ADumpSSA);
    13: Test13(ADumpSSA);
    14: Test14(ADumpSSA);
    15: Test15(ADumpSSA);
    16: Test16(ADumpSSA);
    17: Test17(ADumpSSA);
    18: Test18(ADumpSSA);
    19: Test19(ADumpSSA);
    20: Test20(ADumpSSA);
    21: Test21(ADumpSSA);
    22: Test22(ADumpSSA);
    23: Test23(ADumpSSA);
    24: Test24(ADumpSSA);
    25: Test25(ADumpSSA);
    26: Test26(ADumpSSA);
    27: Test03_2(ADumpSSA);
  end;
end;

(*==============================================================================
  RunTestbed: Main entry point for the test harness.
  Calls RunTest with the desired test number, catches and displays any
  unhandled Delphi exceptions, then pauses for console review.
==============================================================================*)
procedure RunTestbed();
begin
  try
    RunTest(1);
  except
    on E: Exception do
    begin
      TWin64Utils.PrintLn('');
      TWin64Utils.PrintLn(COLOR_RED + 'EXCEPTION: ' + E.Message + COLOR_RESET);
    end;
  end;

  if TWin64Utils.RunFromIDE() then
    TWin64Utils.Pause();
end;

end.
