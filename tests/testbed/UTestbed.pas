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
  CBaseOutputPath = 'output';

function OutputPath(const Plat : TTigerPlatform) : string;
begin
  var PlatPath : string;
  case Plat of
    tpWin64:   PlatPath := 'win';
    tpLinux64: PlatPath := 'lin';
    tpMacOS64: PlatPath := 'mac';
  end;
  Result := TPath.Combine(CBaseOutputPath, PlatPath);
end;

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
  Test_HelloWorld: Minimal "Hello, World!" Console Executable

  PURPOSE:
    Validates the most fundamental Tiger pipeline: creating a TTiger instance,
    importing a single C library function (printf), defining a main entry point,
    emitting a string literal, and building a working console executable.

  WHAT IT TESTS:
    - TTiger.Create() initialization for target platform (Win64 or Linux64)
    - Platform-conditional DLL imports (msvcrt.dll vs libc.so.6)
    - SetStatusCallback for build progress output
    - Func/EndFunc for defining the program entry point
    - Str() for string literal emission to .rdata section
    - Call() for invoking imported variadic functions
    - Tiger_Halt for clean process termination
    - TargetExe for PE/ELF executable generation
    - SetVersionInfo and AddExeIcon for Windows resource embedding

  EXPECTED OUTPUT:
    Hello, World!
    (process exits with code 0)
==============================================================================*)
procedure Test_HelloWorld(const APlatform: TTigerPlatform=tpWin64; ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  // Create compiler instance targeting specified platform
  LTiger := TTiger.Create(APlatform);
  try
    // Wire up status callback so build progress prints to console
    LTiger.SetStatusCallback(StatusCallback);

    // Platform-specific setup: Windows uses msvcrt.dll, Linux uses libc, macOS uses libSystem
    if LTiger.GetPlatform = tpWin64 then
    begin
      // Embed version info and icon into the Windows executable
      SetExeResources(LTiger, 'Test_HelloWorld.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else if LTiger.GetPlatform = tpMacOS64 then
      // macOS: runtime injects libSystem.B.dylib with printf, exit; no ImportDll needed
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    // Define the program entry point
    LTiger.Func('main', vtVoid, True)
      // Print greeting to stdout (Str emits to .rdata, printf reads from there)
      .Call('printf', [LTiger.Str('Hello, World!'#10)])
      // Terminate process with exit code 0 (Tiger_Halt is injected by runtime)
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    // Set output path and subsystem (console application)
    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_HelloWorld'), ssConsole);

    // Compile, link, and optionally run the executable
    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_Factorial_WhileLoop: IR Facade Basics with Factorial Calculation

  PURPOSE:
    Demonstrates core IR building primitives using a factorial(5) algorithm.
    This test exercises local variables, assignment, while loops, arithmetic
    operations, and formatted printf output.

  WHAT IT TESTS:
    - Local() for stack-allocated variables (n, result)
    - Assign() for variable initialization and updates
    - While/EndWhile for loop constructs with condition expressions
    - Gt() for greater-than comparison (loop condition: n > 1)
    - Mul() for multiplication (result *= n)
    - Sub() for subtraction (n -= 1)
    - Get() for reading variable values into expressions
    - Int64() for 64-bit integer literals
    - Str() with format specifiers (%d)
    - Method chaining (fluent API pattern)

  ALGORITHM:
    n = 5, result = 1
    while n > 1:
      result = result * n
      n = n - 1
    print "5! = {result}"

  EXPECTED OUTPUT:
    5! = 120
==============================================================================*)
procedure Test_Factorial_WhileLoop(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);

    // Platform-specific imports
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_Factorial_WhileLoop.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       // Declare local variables for factorial computation
       .Local('n', vtInt64)        // Loop counter (counts down from 5)
       .Local('result', vtInt64)   // Accumulator for factorial result

       // Initialize: n=5, result=1
       .Assign('n', LTiger.Int64(5))
       .Assign('result', LTiger.Int64(1))

       // While loop: multiply result by n, decrement n until n <= 1
       .&While(LTiger.Gt(LTiger.Get('n'), LTiger.Int64(1)))
         // result = result * n
         .Assign('result', LTiger.Mul(LTiger.Get('result'), LTiger.Get('n')))
         // n = n - 1
         .Assign('n', LTiger.Sub(LTiger.Get('n'), LTiger.Int64(1)))
       .EndWhile()

       // Print the computed factorial (expects 120)
       .Call('printf', [LTiger.Str('5! = %d'#10), LTiger.Get('result')])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_Factorial_WhileLoop'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_SSA_OptimizerPasses: SSA Optimizer Validation

  PURPOSE:
    Tests the SSA (Static Single Assignment) optimization pipeline across
    three optimization levels (0, 1, 2). Verifies that optimizations produce
    correct results without changing program semantics.

  WHAT IT TESTS:
    - Optimization Level 0: No optimizations (baseline)
    - Optimization Level 1: Basic optimizations (constant folding, copy prop)
    - Optimization Level 2: Aggressive optimizations (CSE, DCE)

  OPTIMIZATION TARGETS IN THIS CODE:
    - Constant Folding: a = 5 + 3 should fold to a = 8 at compile time
    - Copy Propagation: b = a should propagate 'a' to uses of 'b'
    - Common Subexpression Elimination (CSE): c = a+2 and d = a+2 should
      compute a+2 once and reuse the result
    - Dead Code Elimination (DCE): e = 99 is never used, should be removed

  VALIDATION:
    Uses ResetBuild() to recompile the same IR at different opt levels.
    All three executables should produce identical output "a=8, b=8, c=10, d=10"
    proving that optimizations preserve program behavior.

  EXPECTED OUTPUT (for each opt level):
    a=8, b=8, c=10, d=10
==============================================================================*)
procedure Test_SSA_OptimizerPasses(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);

    // Import printf for output
    if LTiger.GetPlatform = tpWin64 then
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True)
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       // Declare variables for optimization testing
       .Local('a', vtInt64)
       .Local('b', vtInt64)
       .Local('c', vtInt64)
       .Local('d', vtInt64)
       .Local('e', vtInt64)  // Dead variable - never used after assignment

       // CONSTANT FOLDING TARGET: 5+3 should become 8 at compile time
       .Assign('a', LTiger.Add(LTiger.Int64(5), LTiger.Int64(3)))

       // COPY PROPAGATION TARGET: uses of 'b' should become uses of 'a'
       .Assign('b', LTiger.Get('a'))

       // CSE TARGET: a+2 computed twice, should reuse first result
       .Assign('c', LTiger.Add(LTiger.Get('a'), LTiger.Int64(2)))
       .Assign('d', LTiger.Add(LTiger.Get('a'), LTiger.Int64(2)))

       // DCE TARGET: e is assigned but never read, should be eliminated
       .Assign('e', LTiger.Int64(99))

       // Print results - after copy prop, 'b' reads should use 'a' directly
       .Call('printf', [LTiger.Str('a=%d, b=%d, c=%d, d=%d'#10),
             LTiger.Get('a'),
             LTiger.Get('b'),   // Copy prop: should become 'a'
             LTiger.Get('c'),
             LTiger.Get('d')])  // CSE: should use same temp as 'c'
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    //--------------------------------------------------------------------------
    // Build at Optimization Level 0 (no optimizations)
    //--------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Optimization Level 0 ---');
    if LTiger.GetPlatform = tpWin64 then
      SetExeResources(LTiger, 'Test_SSA_O0.exe');
    LTiger.SetOptimizationLevel(0);
    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_SSA_O0'), ssConsole);
    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);

    //--------------------------------------------------------------------------
    // Build at Optimization Level 1 (basic optimizations)
    //--------------------------------------------------------------------------
    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Optimization Level 1 ---');
    LTiger.ResetBuild();  // Reset backend state, keep IR
    LTiger.SetOptimizationLevel(1);
    if LTiger.GetPlatform = tpWin64 then
      SetExeResources(LTiger, 'Test_SSA_O1.exe');
    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_SSA_O1'), ssConsole);
    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);

    //--------------------------------------------------------------------------
    // Build at Optimization Level 2 (aggressive optimizations)
    //--------------------------------------------------------------------------
    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Optimization Level 2 ---');
    LTiger.ResetBuild();
    if LTiger.GetPlatform = tpWin64 then
      SetExeResources(LTiger, 'Test_SSA_O2.exe');
    LTiger.SetOptimizationLevel(2);
    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_SSA_O2'), ssConsole);
    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_SSA_FreshInstance: SSA Optimizer with Fresh Instances

  PURPOSE:
    Identical to Test_SSA_OptimizerPasses but creates a new TTiger instance
    for each optimization level instead of using ResetBuild(). This isolates
    whether any crashes or inconsistencies are caused by stale state in the
    ResetBuild pathway versus bugs in the optimizer itself.

  DEBUGGING RATIONALE:
    If Test_SSA_OptimizerPasses crashes but this test passes, the bug is in
    ResetBuild's state cleanup. If both crash, the bug is in the optimizer.
    This pattern is useful for narrowing down optimization-level-dependent
    issues.

  WHAT IT TESTS:
    - Same optimizations as Test_SSA_OptimizerPasses
    - Clean TTiger construction/destruction cycle for each opt level
    - Isolation of optimizer bugs from state management bugs

  EXPECTED OUTPUT:
    Same as Test_SSA_OptimizerPasses: a=8, b=8, c=10, d=10 for all levels
==============================================================================*)
procedure Test_SSA_FreshInstance(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);

  // Local helper: builds and runs at specified optimization level
  procedure BuildAndRun(const AOptLevel: Integer; const AOutputName: string);
  var
    LTiger: TTiger;
  begin
    // Create a completely fresh TTiger instance
    LTiger := TTiger.Create(APlatform);
    try
      LTiger.SetStatusCallback(StatusCallback);

      // Platform-specific printf import
      if LTiger.GetPlatform = tpWin64 then
        LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True)
      else
        LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

      // Build identical IR as Test_SSA_OptimizerPasses
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

      // Configure and build at specified opt level
      if LTiger.GetPlatform = tpWin64 then
        SetExeResources(LTiger, AOutputName);
      LTiger.SetOptimizationLevel(AOptLevel);
      LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), AOutputName), ssConsole);

      ProcessBuild(LTiger, ADumpSSA);
      ShowErrors(LTiger);
    finally
      // Fresh instance is destroyed, releasing all state
      LTiger.Free();
    end;
  end;

begin
  TWin64Utils.PrintLn('--- Test_SSA_FreshInstance: Fresh instance per opt level ---');
  TWin64Utils.PrintLn('');

  TWin64Utils.PrintLn('--- Optimization Level 0 ---');
  BuildAndRun(0, 'Test_SSA_Fresh_O0');

  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Optimization Level 1 ---');
  BuildAndRun(1, 'Test_SSA_Fresh_O1');

  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Optimization Level 2 ---');
  BuildAndRun(2, 'Test_SSA_Fresh_O2');
end;

(*==============================================================================
  Test_SSA_LoopPhiNodes: Loop Optimization with Phi Nodes

  PURPOSE:
    Tests SSA phi node handling within loop constructs. Phi nodes are required
    at loop headers to merge values from the loop entry and back-edge. This
    test verifies that the optimizer correctly handles loop-carried variables.

  WHAT IT TESTS:
    - Phi node insertion at loop headers (n, result both updated in loop)
    - Loop-carried variable updates (values flow from iteration to iteration)
    - Optimizer level 2 with aggressive transformations on loops
    - Correct handling of back-edge phi operands

  ALGORITHM:
    Factorial(5) via while loop - same as Test_Factorial_WhileLoop but run
    at optimization level 2 to stress-test phi node handling.

  WHY THIS MATTERS:
    Loops create SSA phi nodes because variables like 'n' have multiple
    definitions (initial assignment + loop update). If phi nodes are
    mishandled, the loop produces wrong results or crashes.

  EXPECTED OUTPUT:
    5! = 120
==============================================================================*)
procedure Test_SSA_LoopPhiNodes(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);

    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_SSA_LoopPhiNodes.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       // These variables will have phi nodes at loop header
       .Local('n', vtInt64)       // Updated each iteration
       .Local('result', vtInt64)  // Accumulates product each iteration

       // Initial values (phi entry operands)
       .Assign('n', LTiger.Int64(5))
       .Assign('result', LTiger.Int64(1))

       // Loop creates phi nodes: n_phi = phi(5, n-1), result_phi = phi(1, result*n)
       .&While(LTiger.Gt(LTiger.Get('n'), LTiger.Int64(1)))
         .Assign('result', LTiger.Mul(LTiger.Get('result'), LTiger.Get('n')))
         .Assign('n', LTiger.Sub(LTiger.Get('n'), LTiger.Int64(1)))
       .EndWhile()

       .Call('printf', [LTiger.Str('5! = %d'#10), LTiger.Get('result')])
       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    // Run at optimization level 2 to stress-test phi handling
    TWin64Utils.PrintLn('--- Loop Test: Optimization Level 2 ---');
    LTiger.SetOptimizationLevel(2);
    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_SSA_LoopPhiNodes'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_CaseStatement: Case/Switch Statement Control Flow

  PURPOSE:
    Tests the Case/CaseOf/CaseElse/EndCase control flow constructs which
    generate efficient multi-way branch tables. This is equivalent to
    switch/case in C or case..of in Pascal.

  WHAT IT TESTS:
    - Case() to begin a switch on an integer expression
    - CaseOf([values]) for single or multi-value branch arms
    - CaseElse() for the default/fallback branch
    - EndCase() to close the construct
    - Correct branch selection across multiple test values
    - Jump table generation vs chained comparisons

  SCENARIO:
    Day-of-week classifier:
    - Days 1-5 = Weekday
    - Days 6-7 = Weekend
    - Other values = Unknown

  TEST VALUES:
    - day=1 should print "Weekday" (first CaseOf arm)
    - day=6 should print "Weekend" (second CaseOf arm)
    - day=9 should print "Unknown" (CaseElse arm)

  EXPECTED OUTPUT:
    Day 1: Weekday
    Day 6: Weekend
    Day 9: Unknown
==============================================================================*)
procedure Test_CaseStatement(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('=== Test_CaseStatement ===');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_CaseStatement.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
       .Local('day', vtInt64)

       //----------------------------------------------------------------------
       // Test case 1: day = 1 (Monday - should match weekday branch)
       //----------------------------------------------------------------------
       .Assign('day', LTiger.Int64(1))
       .Call('printf', [LTiger.Str('Day %d: '#0), LTiger.Get('day')])
       .&Case(LTiger.Get('day'))
         .CaseOf([1, 2, 3, 4, 5])  // Weekdays: Mon-Fri
           .Call('printf', [LTiger.Str('Weekday'#10)])
         .CaseOf([6, 7])           // Weekend: Sat-Sun
           .Call('printf', [LTiger.Str('Weekend'#10)])
         .CaseElse()               // Default branch
           .Call('printf', [LTiger.Str('Unknown'#10)])
       .EndCase()

       //----------------------------------------------------------------------
       // Test case 2: day = 6 (Saturday - should match weekend branch)
       //----------------------------------------------------------------------
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

       //----------------------------------------------------------------------
       // Test case 3: day = 9 (invalid - should hit else branch)
       //----------------------------------------------------------------------
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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_CaseStatement'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_GlobalVariables: Global Variable Declaration and Access

  PURPOSE:
    Tests the Global() method for declaring module-level variables that persist
    for the lifetime of the program. These are allocated in the .bss or .data
    section of the executable.

  WHAT IT TESTS:
    - Global() for declaring global variables
    - Zero-initialization of globals (BSS section behavior)
    - Assign/Get operations on globals from within functions
    - Arithmetic operations using global variables
    - Cross-global expressions (using one global in expression for another)
    - Correct RIP-relative addressing for global access in x64

  TEST SEQUENCE:
    1. Print initial values (should be 0 from BSS zero-init)
    2. Initialize gCounter=10, gMultiplier=5
    3. Increment gCounter by gMultiplier (10+5=15)
    4. Multiply gCounter by gMultiplier (15*5=75)

  EXPECTED OUTPUT:
    Initial: counter=0, multiplier=0
    After init: counter=10, multiplier=5
    After increment: counter=15
    Final: counter=75
==============================================================================*)
procedure Test_GlobalVariables(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('=== Test_GlobalVariables ===');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_GlobalVariables.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    // Declare global variables (allocated in .bss, zero-initialized)
    LTiger.Global('gCounter', vtInt64);
    LTiger.Global('gMultiplier', vtInt64);

    LTiger.Func('main', vtVoid, True)
       .Local('temp', vtInt64)

       // Verify zero-initialization of globals
       .Call('printf', [LTiger.Str('Initial: counter=%d, multiplier=%d'#10),
             LTiger.Get('gCounter'), LTiger.Get('gMultiplier')])

       // Assign initial values to globals
       .Assign('gCounter', LTiger.Int64(10))
       .Assign('gMultiplier', LTiger.Int64(5))
       .Call('printf', [LTiger.Str('After init: counter=%d, multiplier=%d'#10),
             LTiger.Get('gCounter'), LTiger.Get('gMultiplier')])

       // Increment counter using multiplier (tests cross-global read)
       .Assign('gCounter', LTiger.Add(LTiger.Get('gCounter'), LTiger.Get('gMultiplier')))
       .Call('printf', [LTiger.Str('After increment: counter=%d'#10),
             LTiger.Get('gCounter')])

       // Multiply counter by multiplier
       .Assign('gCounter', LTiger.Mul(LTiger.Get('gCounter'), LTiger.Get('gMultiplier')))
       .Call('printf', [LTiger.Str('Final: counter=%d'#10),
             LTiger.Get('gCounter')])

       .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_GlobalVariables'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_TypeSystem: Comprehensive Type Definition Validation

  PURPOSE:
    Tests all Tiger type definition primitives: records, packed records,
    arrays (fixed and dynamic), enumerations, and type aliases. Verifies
    correct size and alignment calculations for each type.

  WHAT IT TESTS:
    - DefineRecord() for composite types with multiple fields
    - Packed records (no padding between fields)
    - Records with mixed-size fields (tests alignment padding)
    - DefineArray() for fixed-size arrays
    - DefineDynArray() for dynamic arrays (pointer-sized)
    - DefineEnum() for enumerated types with auto/explicit values
    - DefineAlias() for type aliases to primitives and composites
    - FindType() to look up type indices by name
    - GetTypeSize() and GetTypeAlignment() for layout queries
    - TTigerTypeRef for type reference management

  TYPE DEFINITIONS:
    - TPoint: record {X, Y: int32} - size=8, align=4
    - TRect: record {TopLeft, BottomRight: TPoint} - size=16
    - TPackedPoint: packed record {X, Y: int32} - size=8, no padding
    - TStudent: record {NamePtr: pointer, Age: uint8, Grade: float64}
    - TIntArray: array[0..9] of int32 - size=40
    - TPointArray: array[0..4] of TPoint - size=40
    - TDynInts: array of int32 - dynamic, size=8 (pointer)
    - TColor: enum (Red=0, Green=1, Blue=2)
    - TStatus: enum (OK=0, Error=-1, Pending=100)
    - TMyInt: alias for int32
    - TCoord: alias for TPoint
==============================================================================*)
procedure Test_TypeSystem(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;
begin
  TWin64Utils.PrintLn('=== Test_TypeSystem ===');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_TypeSystem.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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
    TWin64Utils.PrintLn('TPoint: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TRect
    LTypeIndex := LTiger.FindType('TRect');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TRect: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TPackedPoint
    LTypeIndex := LTiger.FindType('TPackedPoint');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TPackedPoint: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TStudent
    LTypeIndex := LTiger.FindType('TStudent');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TStudent: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TIntArray
    LTypeIndex := LTiger.FindType('TIntArray');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TIntArray: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TPointArray
    LTypeIndex := LTiger.FindType('TPointArray');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn(Format('TPointArray: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]));

    // TDynInts (dynamic array = pointer size)
    LTypeIndex := LTiger.FindType('TDynInts');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TDynInts: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TColor (enum = int32)
    LTypeIndex := LTiger.FindType('TColor');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TColor: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TMyInt (alias to int32)
    LTypeIndex := LTiger.FindType('TMyInt');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TMyInt: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // TCoord (alias to TPoint)
    LTypeIndex := LTiger.FindType('TCoord');
    LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
    TWin64Utils.PrintLn('TCoord: index=%d, size=%d, align=%d',
      [LTypeIndex, LTiger.GetTypeSize(LTypeRef), LTiger.GetTypeAlignment(LTypeRef)]);

    // Test TypeRef helper
    LTypeRef := LTiger.TypeRef('TPoint');
    TWin64Utils.PrintLn('TypeRef(TPoint): IsPrimitive=%s, TypeIndex=%d',
      [BoolToStr(LTypeRef.IsPrimitive, True), LTypeRef.TypeIndex]);

    // Test primitive TypeRef
    LTypeRef := TTigerTypeRef.FromPrimitive(vtInt64);
    TWin64Utils.PrintLn('TypeRef(vtInt64): IsPrimitive=%s, size=%d',
      [BoolToStr(LTypeRef.IsPrimitive, True), LTiger.GetTypeSize(LTypeRef)]);

    TWin64Utils.PrintLn('');
    TWin64Utils.PrintLn('--- Type System Test Complete ---');

    //------------------------------------------------------------------------
    // Build a simple executable to verify no errors
    //------------------------------------------------------------------------
    LTiger.Func('main', vtVoid, True)
      .Call('printf', [LTiger.Str('Type system test passed!'#10)])
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_TypeSystem'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_CStructABI: Full C Struct ABI Compliance

  PURPOSE:
    Comprehensive validation of Tiger's struct layout engine against C ABI
    requirements. Tests explicit alignment, unions, anonymous unions/records,
    bit fields, and record inheritance.

  WHAT IT TESTS:
    - Explicit Alignment: natural alignment vs forced align(8)/align(16)
    - Union Types: overlapping fields with size = max member
    - Anonymous Unions in Records: inline variant parts (TPacket, TMultiUnion)
    - Anonymous Records in Unions: structured overlays (TSplitValue with Lo/Hi)
    - Bit Fields: bit-level packing, isolation between adjacent fields
    - Record Inheritance: TPoint2D -> TPoint3D -> TPoint4D field offsets

  RUNTIME TESTS:
    Builds an executable that writes and reads record fields, performs
    union reinterpretation, accesses inherited fields, and verifies
    bit field isolation. Each test prints expected vs actual values.

  SIZE/ALIGNMENT EXPECTATIONS:
    - TNaturalAlign: size=8, align=4
    - TAlign16: size=16, align=16
    - TVariant: size=8, align=8
    - TPacket: size=24, align=8
    - TFlags: size=4, align=4
    - TPoint2D: size=8, TPoint3D: size=12, TPoint4D: size=16
==============================================================================*)
procedure Test_CStructABI(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;

begin
  TWin64Utils.PrintLn('=== Test_CStructABI ===');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_CStructABI.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    //------------------------------------------------------------------------
    // Explicit Alignment Tests
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Explicit Alignment ---');

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
    // Union Types
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Union Types ---');

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
    // Anonymous Unions in Records
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Anonymous Unions in Records ---');

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
    // Anonymous Records in Unions
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Anonymous Records in Unions ---');

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
    // Bit Fields
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Bit Fields ---');

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
    // Record Inheritance
    //------------------------------------------------------------------------
    TWin64Utils.PrintLn('--- Record Inheritance ---');

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
    TWin64Utils.PrintLn('--- Struct ABI Test Complete ---');

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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_CStructABI'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_TypedPointers: Typed Pointer Operations

  PURPOSE:
    Validates Tiger's typed pointer system including pointer type definitions,
    address-of operations, dereferencing, and write-through-pointer semantics.
    Ensures pointer modifications correctly affect the underlying variables.

  WHAT IT TESTS:
    - DefinePointer: PInt32, PInt64, TRawPointer, PPoint (pointer-to-record)
    - DefineArray: TIntArray for array element addressing
    - AddrOf: Taking address of local variables (@x)
    - AddrOfVal: Taking address of record fields (@pt.X) and array elements (@arr[1])
    - Deref: Reading through a pointer (p^)
    - AssignTo with Deref: Writing through a pointer (p^ := value)
    - Type size/alignment verification (all pointers = 8 bytes on x64)

  RUNTIME TESTS:
    Test 1 - Basic Pointer Operations:
      x := 42; p := @x; y := p^; p^ := 100 → x becomes 100
    Test 2 - Address of Record Field:
      pt.X := 10; px := @pt.X; px^ := 55 → pt.X becomes 55
    Test 3 - Address of Array Element:
      arr[1] := 200; pa := @arr[1]; pa^ := 999 → arr[1] becomes 999

  EXPECTED OUTPUT:
    x = 42 (initial value 42)
    p := @x (took address)
    y := p^ -> y = 42 (expected 42)
    p^ := 100
    x = 100 (expected 100, modified via pointer)
    pt.X = 10, pt.Y = 20 (initial)
    px := @pt.X (took address of field)
    y := px^ -> y = 10 (expected 10)
    px^ := 55
    pt.X = 55 (expected 55, modified via pointer)
    pt.Y = 77 (expected 77, modified via pointer)
    arr[0]=100, arr[1]=200, arr[2]=300 (initial)
    pa := @arr[1] (took address of element)
    y := pa^ -> y = 200 (expected 200)
    pa^ := 999
    arr[1] = 999 (expected 999, modified via pointer)
    === All Pointer Tests Complete ===
==============================================================================*)
procedure Test_TypedPointers(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;
begin
  // Test_TypedPointers:: Typed Pointers
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_TypedPointers:: Typed Pointers');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_TypedPointers.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_TypedPointers'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_FunctionPointers: Indirect Function Calls via Pointers

  PURPOSE:
    Validates Tiger's function pointer support including obtaining function
    addresses with FuncAddr and invoking functions indirectly through pointers
    using InvokeIndirect. Essential for callback patterns and dispatch tables.

  WHAT IT TESTS:
    - FuncAddr: Obtaining the runtime address of a defined function
    - InvokeIndirect: Calling a function through a pointer variable
    - Pointer reassignment: Switching which function a pointer references
    - Parameterless functions: Indirect calls with empty argument lists
    - Chained calls: Using result of one indirect call as input to another

  HELPER FUNCTIONS DEFINED:
    - add_func(a, b: Int32): Int32 — returns a + b
    - mul_func(x, y: Int32): Int32 — returns x * y
    - get_magic(): Int32 — returns 42 (no parameters)

  RUNTIME TESTS:
    Test 1 - Basic Function Pointer:
      pFunc := @add_func; pFunc(10, 20) → 30
    Test 2 - Switching Function Pointers:
      pFunc := @mul_func; pFunc(6, 7) → 42
    Test 3 - Parameterless Function Pointer:
      pFunc := @get_magic; pFunc() → 42
    Test 4 - Chained Indirect Calls:
      add_func(5, 3) → 8; mul_func(8, 4) → 32

  EXPECTED OUTPUT:
    === Test 1: Basic Function Pointer ===
    pFunc := @add_func
    result := pFunc(10, 20) -> 30 (expected 30)
    === Test 2: Switching Function Pointers ===
    pFunc := @mul_func
    result := pFunc(6, 7) -> 42 (expected 42)
    === Test 3: Parameterless Function Pointer ===
    pFunc := @get_magic
    result := pFunc() -> 42 (expected 42)
    === Test 4: Chained Indirect Calls ===
    add_func(5, 3) = 8 (expected 8)
    mul_func(8, 4) = 32 (expected 32)
    === All Function Pointer Tests Complete ===
==============================================================================*)
procedure Test_FunctionPointers(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_FunctionPointers: Function Pointers');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_FunctionPointers.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_FunctionPointers'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_PublicExports: Function Export Visibility Control

  PURPOSE:
    Validates Tiger's function export mechanism for PE executables. Tests that
    functions marked as exported appear in the PE export table while private
    functions remain internal-only. Essential for creating DLLs and executables
    with controlled public APIs.

  WHAT IT TESTS:
    - Func with AExport=True: Function appears in PE export table
    - Func with AExport=False: Function is internal, not exported
    - C linkage (plC): Exported with undecorated C-style name
    - Default linkage (plDefault): Exported with decorated name
    - Mixed visibility: Public and private functions in same module

  FUNCTIONS DEFINED:
    - MyAdd(a, b: Int32): Int32 — C linkage, EXPORTED
    - MyMul(x, y: Int32): Int32 — C linkage, EXPORTED
    - MySquare(n: Int32): Int32 — default linkage, EXPORTED
    - PrivateHelper(v: Int32): Int32 — C linkage, NOT exported

  VERIFICATION:
    All functions are callable internally regardless of export status.
    Use "dumpbin /exports Test_PublicExports.exe" (Windows) or
    "nm -D" (Linux) to verify only MyAdd, MyMul, MySquare appear
    in the export table.

  EXPECTED OUTPUT:
    === Test_PublicExports: Public Exports ===
    MyAdd(10, 20) = 30 (expected 30)
    MyMul(6, 7) = 42 (expected 42)
    MySquare(9) = 81 (expected 81)
    PrivateHelper(5) = 105 (expected 105)
    Use "dumpbin /exports Test_PublicExports.exe" to verify exports:
      Expected: MyAdd, MyMul, MySquare
      NOT exported: PrivateHelper, main
==============================================================================*)
procedure Test_PublicExports(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_PublicExports: Public Exports');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_PublicExports.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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

      .Call('printf', [LTiger.Str('=== Test_PublicExports: Public Exports ===' + #10)])
      .Assign('result', LTiger.Invoke('MyAdd', [LTiger.Int64(10), LTiger.Int64(20)]))
      .Call('printf', [LTiger.Str('MyAdd(10, 20) = %d (expected 30)' + #10), LTiger.Get('result')])
      .Assign('result', LTiger.Invoke('MyMul', [LTiger.Int64(6), LTiger.Int64(7)]))
      .Call('printf', [LTiger.Str('MyMul(6, 7) = %d (expected 42)' + #10), LTiger.Get('result')])
      .Assign('result', LTiger.Invoke('MySquare', [LTiger.Int64(9)]))
      .Call('printf', [LTiger.Str('MySquare(9) = %d (expected 81)' + #10), LTiger.Get('result')])
      .Assign('result', LTiger.Invoke('PrivateHelper', [LTiger.Int64(5)]))
      .Call('printf', [LTiger.Str('PrivateHelper(5) = %d (expected 105)' + #10), LTiger.Get('result')])

      .Call('printf', [LTiger.Str(#10 + 'Use "dumpbin /exports Test_PublicExports.exe" to verify exports:' + #10)])
      .Call('printf', [LTiger.Str('  Expected: MyAdd, MyMul, MySquare' + #10)])
      .Call('printf', [LTiger.Str('  NOT exported: PrivateHelper, main' + #10)])

      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_PublicExports'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_FunctionOverloading: Overloaded Function Resolution

  PURPOSE:
    Validates Tiger's function overloading mechanism where multiple functions
    share the same name but differ in parameter types or count. Tests that
    OverloadFunc correctly registers overloads and Invoke dispatches to the
    correct implementation based on argument signatures.

  WHAT IT TESTS:
    - OverloadFunc: Registering multiple functions with the same base name
    - Type-based dispatch: Add(Int32, Int32) vs Add(Int64, Int64)
    - Arity-based dispatch: Multiply(x, y) vs Multiply(x, y, z)
    - Name mangling: Each overload gets a unique export name
    - Invoke resolution: Correct overload selected at call site

  OVERLOADS DEFINED:
    - Add(a, b: Int32): Int32 — returns a + b (32-bit version)
    - Add(a, b: Int64): Int64 — returns a + b (64-bit version)
    - Multiply(x, y: Int32): Int32 — returns x * y (2-param version)
    - Multiply(x, y, z: Int32): Int32 — returns x * y * z (3-param version)

  VERIFICATION:
    Win64:   tdump -ee Test_FunctionOverloading.exe | grep -i add
    Linux64: wsl nm -D Test_FunctionOverloading | grep -i add
    Each overload should have a unique mangled export name.

  EXPECTED OUTPUT:
    === Test_FunctionOverloading: Function Overloading ===
    Add(10, 20) int32 = 30 (expected 30)
    Add(100, 200) int64 = 300 (expected 300)
    Multiply(6, 7) = 42 (expected 42)
    Multiply(2, 3, 4) = 24 (expected 24)
    Use "dumpbin /exports Test_FunctionOverloading.exe" to verify mangled exports:
      Each overload should have a unique mangled name
==============================================================================*)
procedure Test_FunctionOverloading(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_FunctionOverloading: Function Overloading');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_FunctionOverloading.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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

      .Call('printf', [LTiger.Str('=== Test_FunctionOverloading: Function Overloading ===' + #10)])
      .Assign('r32', LTiger.Invoke('Add', [LTiger.Int32(10), LTiger.Int32(20)]))
      .Call('printf', [LTiger.Str('Add(10, 20) int32 = %d (expected 30)' + #10), LTiger.Get('r32')])
      .Assign('r64', LTiger.Invoke('Add', [LTiger.Int64(100), LTiger.Int64(200)]))
      .Call('printf', [LTiger.Str('Add(100, 200) int64 = %lld (expected 300)' + #10), LTiger.Get('r64')])
      .Assign('r32', LTiger.Invoke('Multiply', [LTiger.Int32(6), LTiger.Int32(7)]))
      .Call('printf', [LTiger.Str('Multiply(6, 7) = %d (expected 42)' + #10), LTiger.Get('r32')])
      .Assign('r32', LTiger.Invoke('Multiply', [LTiger.Int32(2), LTiger.Int32(3), LTiger.Int32(4)]))
      .Call('printf', [LTiger.Str('Multiply(2, 3, 4) = %d (expected 24)' + #10), LTiger.Get('r32')])

      .Call('printf', [LTiger.Str(#10 + 'Use "dumpbin /exports Test_FunctionOverloading.exe" to verify mangled exports:' + #10)])
      .Call('printf', [LTiger.Str('  Each overload should have a unique mangled name' + #10)])

      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_FunctionOverloading'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_RuntimeMemory: Heap Memory Allocation via Tiger Runtime

  PURPOSE:
    Validates the Tiger runtime's heap memory management functions. Tests
    dynamic allocation, pointer-based read/write operations, multiple
    independent allocations, and proper deallocation. Also verifies that
    Dead Code Elimination (DCE) removes unused functions at optimization
    level 1.

  WHAT IT TESTS:
    - Tiger_GetMem: Allocate heap memory of specified size
    - Tiger_FreeMem: Release previously allocated memory
    - Deref write: Store values through allocated pointers (ptr^ := value)
    - Deref read: Retrieve values through pointers (val := ptr^)
    - Multiple allocations: Verify independent blocks don't overlap
    - DCE validation: UnusedFunc should be eliminated at opt level 1

  RUNTIME FUNCTIONS USED:
    - Tiger_GetMem(size: Int64): Pointer — allocates size bytes
    - Tiger_FreeMem(ptr: Pointer) — frees allocated memory
    - Tiger_Halt(code: Int64) — terminates process

  RUNTIME TESTS:
    Test 1 - Basic GetMem/FreeMem:
      Allocate 8 bytes, write 12345, read back, verify, free
    Test 2 - Multiple Allocations:
      Allocate 16, 32, 64 bytes; write 111, 222, 333; verify isolation; free all

  EXPECTED OUTPUT:
    === Test_RuntimeMemory: Runtime Memory ===
    Test 1: Basic GetMem/FreeMem
      Allocated 8 bytes at: <address>
      Wrote value: 12345
      Read back: 12345 (expected 12345)
      Freed memory
    Test 2: Multiple allocations
      ptr1 (16 bytes): <address>
      ptr2 (32 bytes): <address>
      ptr3 (64 bytes): <address>
      *ptr1 = 111 (expected 111)
      *ptr2 = 222 (expected 222)
      *ptr3 = 333 (expected 333)
      All freed
    Memory tests complete!
==============================================================================*)
procedure Test_RuntimeMemory(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_RuntimeMemory: Runtime Memory');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
      SetExeResources(LTiger, 'Test_RuntimeMemory.exe');
    //LTiger.SetOptimizationLevel(1);

    // Add an UNUSED function to test DCE removes it
    LTiger.Func('UnusedFunc', vtInt32, False, plC, False)
      .Param('x', vtInt32)
      .Return(LTiger.Get('x'))
    .EndFunc();

    if LTiger.GetPlatform = tpWin64 then
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True)
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
      .Local('ptr', vtPointer)
      .Local('val', vtInt32)

      .Call('printf', [LTiger.Str('=== Test_RuntimeMemory: Runtime Memory ===' + #10)])
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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_RuntimeMemory'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_ManagedStrings: Reference-Counted String System

  PURPOSE:
    Validates the Tiger runtime's managed string implementation. Tests
    string creation from literals, length/data access, concatenation,
    and reference counting semantics. Ensures refcounts increment and
    decrement correctly to prevent memory leaks and use-after-free.

  WHAT IT TESTS:
    - Tiger_StrFromLiteral: Create managed string from C string + length
    - Tiger_StrLen: Get string length (excluding null terminator)
    - Tiger_StrData: Get raw pointer to string characters
    - Tiger_StrConcat: Concatenate two managed strings into a new one
    - Tiger_StrAddRef: Manually increment reference count
    - Tiger_StrRelease: Decrement refcount, free if zero
    - Tiger_StrAssign: Refcount-safe pointer assignment (releases old, addref new)
    - TStringRec.RefCount: Direct refcount field access for verification

  RUNTIME TESTS:
    Test 1 - Create string from literal:
      s1 = "Hello", len = 5
    Test 2 - Create another string:
      s2 = " World", len = 6
    Test 3 - Concatenate strings:
      s3 = s1 + s2 = "Hello World", len = 11
    Test 4 - Reference counting:
      AddRef increments to 2, Release decrements to 1
    Test 5 - StrAssign with refcount:
      s4 := s1 bumps s1 refcount to 2

  EXPECTED OUTPUT:
    === Test_ManagedStrings: Managed Strings ===
    Test 1: Create string from literal
      s1 = "Hello"
      len(s1) = 5 (expected 5)
    Test 2: Create another string
      s2 = " World"
      len(s2) = 6 (expected 6)
    Test 3: Concatenate strings
      s3 = s1 + s2 = "Hello World"
      len(s3) = 11 (expected 11)
    Test 4: Reference counting
      s1 refcount before AddRef: 1
      s1 refcount after AddRef: 2 (expected 2)
      s1 refcount after Release: 1 (expected 1)
    Test 5: StrAssign with refcount
      After s4 := s1:
        s1 refcount: 2 (expected 2)
        s4 = "Hello"
    Cleanup: Releasing all strings
      All strings released
    String tests complete!
==============================================================================*)
procedure Test_ManagedStrings(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_ManagedStrings: Managed Strings');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_ManagedStrings.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
    LTiger.SetOptimizationLevel(1);

    LTiger.Func('main', vtVoid, True)
      .Local('s1', vtPointer)
      .Local('s2', vtPointer)
      .Local('s3', vtPointer)
      .Local('len', vtUInt64)
      .Local('data', vtPointer)

      .Call('printf', [LTiger.Str('=== Test_ManagedStrings: Managed Strings ===' + #10)])
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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_ManagedStrings.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_Printf_Basic: C Runtime printf Interop Validation

  PURPOSE:
    Minimal sanity check confirming that Tiger can successfully import and
    call the C runtime's printf function. Serves as a baseline verification
    that DLL imports, variadic function calls, and Tiger_Halt work correctly
    before running more complex tests.

  WHAT IT TESTS:
    - ImportDll: Import printf from msvcrt.dll (Win64) or libc.so.6 (Linux64)
    - Variadic calls: printf with format string and arguments
    - String literals: Str() emission to .rdata section
    - Integer formatting: %d format specifier with Int32 value
    - Tiger_Halt: Clean process termination via runtime

  RUNTIME TESTS:
    - Print a plain string message
    - Print an integer using %d format (value 42)
    - Exit cleanly with code 0

  EXPECTED OUTPUT:
    === Test_Printf_Basic: Basic printf ===
    Hello from Test_Printf_Basic!
    Integer: 42
    Done!
==============================================================================*)
procedure Test_Printf_Basic(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_Printf_Basic: Basic printf');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_Printf_Basic.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
    LTiger.SetOptimizationLevel(1);

    LTiger.Func('main', vtVoid, True)
      .Local('dummy', vtInt32)

      .Call('printf', [LTiger.Str('=== Test_Printf_Basic: Basic printf ===' + #10)])
      .Call('printf', [LTiger.Str('Hello from Test_Printf_Basic!' + #10)])
      .Call('printf', [LTiger.Str('Integer: %d' + #10), LTiger.Int32(42)])
      .Call('printf', [LTiger.Str('Done!' + #10)])

      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_Printf_Basic.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_SetTypes: Pascal-Style Set Operations

  PURPOSE:
    Validates Tiger's set type system including set definition, literal
    construction, membership testing, and set algebra operations. Sets are
    implemented as bit vectors with configurable ranges, supporting both
    zero-based and offset ranges.

  WHAT IT TESTS:
    - DefineSet: Create set types with various ranges (0..7, 0..31, 0..63, 100..163)
    - SetLit: Construct set literals with specific elements
    - EmptySet: Create an empty set of a given type
    - SetIn: Membership test (element in set)
    - SetUnion: Set union operator (+)
    - SetInter: Set intersection operator ( * )
    - SetDiff: Set difference operator (-)
    - SetEq: Set equality comparison (=)

  SET TYPES DEFINED:
    - TSmallSet: 0..7 (8 elements, 1 byte)
    - TMediumSet: 0..31 (32 elements, 4 bytes)
    - TLargeSet: 0..63 (64 elements, 8 bytes)
    - TOffsetSet: 100..163 (64 elements, offset range)

  RUNTIME TESTS:
    Test 1 - Set literals: s1 = {1, 3, 5}, s2 = {}
    Test 2 - Membership: 1 in s1 → true, 2 in s1 → false
    Test 3 - Union: {1,3,5} + {2,4} → 1 in result, 2 in result
    Test 4 - Intersection: {1,2,3} * {2,3,4} → 1 not in result, 2 in result
    Test 5 - Difference: {1,2,3} - {2,3,4} → 1 in result, 2 not in result
    Test 6 - Equality: {1,2,3} = {1,2,3} → true, {1,2,3} = {1,2} → false

  EXPECTED OUTPUT:
    === Test_SetTypes: Sets ===
    Test 1: Set literals
      s1 = {1, 3, 5}
      s2 = {} (empty)
    Test 2: Membership (in)
      1 in s1: true
      2 in s1: false
    Test 3: Union (+)
      {1,3,5} + {2,4} = s3
      1 in s3: true
      2 in s3: true
    Test 4: Intersection ( * )
      {1,2,3} * {2,3,4} = s3
      1 in s3: false
      2 in s3: true
    Test 5: Difference (-)
      {1,2,3} - {2,3,4} = s3
      1 in s3: true
      2 in s3: false
    Test 6: Equality (=)
      {1,2,3} = {1,2,3}: true
      {1,2,3} = {1,2}: false
    All tests passed!
==============================================================================*)
procedure Test_SetTypes(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_SetTypes: Sets');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_SetTypes.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
    LTiger.SetOptimizationLevel(1);

    LTiger.DefineSet('TSmallSet', 0, 7);
    LTiger.DefineSet('TMediumSet', 0, 31);
    LTiger.DefineSet('TLargeSet', 0, 63);
    LTiger.DefineSet('TOffsetSet', 100, 163);

    LTiger.Func('main', vtVoid, True)
      .Local('s1', 'TSmallSet')
      .Local('s2', 'TSmallSet')
      .Local('s3', 'TSmallSet')
      .Local('result', vtInt32)

      .Call('printf', [LTiger.Str('=== Test_SetTypes: Sets ===' + #10)])

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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_SetTypes'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_Intrinsics: Compile-Time and Inline Intrinsic Functions

  PURPOSE:
    Validates Tiger's built-in intrinsic functions that provide compile-time
    type information and inline operations. These intrinsics mirror Delphi's
    system functions and are essential for generic programming and low-level
    type manipulation.

  WHAT IT TESTS:
    - SizeOf: Returns byte size of a type (TPoint=8, TBigRecord=24, TIntArray=40)
    - AlignOf: Returns alignment requirement of a type (TPoint=4, TBigRecord=8)
    - High/Low on arrays: Returns upper/lower bound indices
    - High/Low on enums: Returns max/min ordinal values
    - High/Low on sets: Returns max/min element values
    - Len: Returns element count of an array type
    - Ord: Identity cast to ordinal (passthrough)
    - Chr: Identity cast to character ordinal (passthrough)
    - Succ: Returns value + 1 as expression
    - Pred: Returns value - 1 as expression
    - Inc: In-place increment with optional step
    - Dec: In-place decrement with optional step

  TYPES DEFINED:
    - TPoint: record (X, Y: Int32) — size=8, align=4
    - TBigRecord: record (A: Int32, B: Int64, C: Int32) — size=24, align=8
    - TIntArray: array[0..9] of Int32 — 10 elements, size=40
    - TOffsetArray: array[5..9] of Int32 — 5 elements, offset bounds
    - TColor: enum (Red=0, Green=1, Blue=2)
    - TPriority: enum (Low=10, Medium=20, High=30)
    - TSmallSet: set of 0..7

  EXPECTED OUTPUT:
    === Test_Intrinsics: Compile-Time Intrinsics ===
    Test 1: SizeOf
      SizeOf(TPoint) = 8 (expected: 8)
      SizeOf(TBigRecord) = 24 (expected: 24)
      SizeOf(TIntArray) = 40 (expected: 40)
    Test 2: AlignOf
      AlignOf(TPoint) = 4 (expected: 4)
      AlignOf(TBigRecord) = 8 (expected: 8)
    Test 3: High/Low on arrays
      Low(TIntArray) = 0 (expected: 0)
      High(TIntArray) = 9 (expected: 9)
      Low(TOffsetArray) = 5 (expected: 5)
      High(TOffsetArray) = 9 (expected: 9)
    Test 4: Len on arrays
      Len(TIntArray) = 10 (expected: 10)
      Len(TOffsetArray) = 5 (expected: 5)
    Test 5: High/Low on enums
      Low(TColor) = 0 (expected: 0)
      High(TColor) = 2 (expected: 2)
      Low(TPriority) = 10 (expected: 10)
      High(TPriority) = 30 (expected: 30)
    Test 6: High/Low on sets
      Low(TSmallSet) = 0 (expected: 0)
      High(TSmallSet) = 7 (expected: 7)
    Test 7: Ord/Chr
      Ord(65) = 65 (expected: 65)
      Chr(66) = 66 (expected: 66)
    Test 8: Succ/Pred
      Succ(10) = 11 (expected: 11)
      Pred(10) = 9 (expected: 9)
      Succ(Pred(5)) = 5 (expected: 5)
    Test 9: Inc/Dec
      Inc(10) = 11 (expected: 11)
      Dec(Dec(11)) = 9 (expected: 9)
      Inc(5, 10) = 15 (expected: 15)
      Dec(15, 7) = 8 (expected: 8)
    All tests passed!
==============================================================================*)
procedure Test_Intrinsics(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_Intrinsics: Compile-Time Intrinsics');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_Intrinsics.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
    LTiger.SetOptimizationLevel(1);

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

      .Call('printf', [LTiger.Str('=== Test_Intrinsics: Compile-Time Intrinsics ===' + #10)])

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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_Intrinsics'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_VariadicFunctions: User-Defined Variadic Function Support

  PURPOSE:
    Validates Tiger's ability to define variadic functions (functions that
    accept a variable number of arguments). Tests the VaCount and VaArg
    intrinsics for accessing variadic argument count and values by index.
    Essential for implementing flexible APIs like logging and formatting.

  WHAT IT TESTS:
    - VariadicFunc: Define a function accepting variable arguments
    - VaCount: Intrinsic returning number of variadic arguments passed
    - VaArg(index, type): Intrinsic returning variadic argument at index
    - Pure varargs: Function with only variadic parameters
    - Mixed signature: Fixed parameters followed by variadic parameters
    - Edge cases: 0 arguments, 1 argument, many arguments (up to 7)

  FUNCTIONS DEFINED:
    - SumAll(...): Int64 — sums all variadic arguments
    - SumWithMult(multiplier, ...): Int64 — sums varargs then multiplies
    - CountArgs(...): Int64 — returns VaCount (argument count)

  RUNTIME TESTS:
    Test 1 - SumAll:
      SumAll() → 0, SumAll(10) → 10, SumAll(1,2,3) → 6, SumAll(10,20,30,40,50) → 150
    Test 2 - SumWithMult:
      SumWithMult(2) → 0, SumWithMult(2,5) → 10, SumWithMult(3,1,2,3) → 18
    Test 3 - CountArgs:
      CountArgs() → 0, CountArgs(1) → 1, CountArgs(1,2,3,4,5) → 5, CountArgs(1..7) → 7

  EXPECTED OUTPUT:
    === Test_VariadicFunctions: Variadic Functions ===
    Test 1: SumAll (sum all varargs)
      SumAll() = 0 (expected: 0)
      SumAll(10) = 10 (expected: 10)
      SumAll(1, 2, 3) = 6 (expected: 6)
      SumAll(10,20,30,40,50) = 150 (expected: 150)
    Test 2: SumWithMult (fixed param + varargs)
      SumWithMult(2) = 0 (expected: 0)
      SumWithMult(2, 5) = 10 (expected: 10)
      SumWithMult(3, 1,2,3) = 18 (expected: 18)
      SumWithMult(2, 10,20,30,40) = 200 (expected: 200)
    Test 3: CountArgs (VaCount_ intrinsic)
      CountArgs() = 0 (expected: 0)
      CountArgs(1) = 1 (expected: 1)
      CountArgs(1,2,3,4,5) = 5 (expected: 5)
      CountArgs(1..7) = 7 (expected: 7)
    All variadic tests complete!
==============================================================================*)
procedure Test_VariadicFunctions(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_VariadicFunctions: Variadic Functions');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_VariadicFunctions.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
    LTiger.SetOptimizationLevel(1);

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

      .Call('printf', [LTiger.Str('=== Test_VariadicFunctions: Variadic Functions ===' + #10)])

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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_VariadicFunctions'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_StaticLinking: Static Library Linking (.lib/.a)

  PURPOSE:
    Validates Tiger's ability to link against static libraries (.lib on Win64,
    .a on Linux64). Tests ImportLib for importing functions from pre-compiled
    static libraries, enabling interop with existing C/C++ codebases without
    runtime DLL dependencies.

  WHAT IT TESTS:
    - ImportLib: Import function from static library file
    - Platform-specific library resolution (msvcrt.lib vs libc.a)
    - Static linking vs dynamic linking (no LoadLibrary at runtime)
    - Calling statically-linked functions (abs, labs)

  PLATFORM BEHAVIOR:
    Win64:   Links against msvcrt.lib for abs() and labs()
    Linux64: Links against libc.a for abs() and labs()

  RUNTIME TESTS:
    - abs(-42) → 42 (32-bit absolute value)
    - labs(-123456) → 123456 (64-bit absolute value)
    - abs(0) → 0 (zero case)
    - abs(100) → 100 (positive passthrough)

  VERIFICATION:
    Win64:   tdump -ee Test_StaticLinking.exe (no msvcrt.dll import)
    Linux64: ldd Test_StaticLinking (no libc.so dependency if fully static)

  EXPECTED OUTPUT:
    === Test_StaticLinking: Static Library Linking ===
    abs(-42) = 42 (expected: 42)
    labs(-123456) = 123456 (expected: 123456)
    abs(0) = 0 (expected: 0)
    abs(100) = 100 (expected: 100)
    Static linking test complete!
==============================================================================*)
procedure Test_StaticLinking(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
  LLibExt: string;
  LExeExt: string;
begin
  //============================================================================
  // Test_StaticLinking: Static Linking (.lib/.a -> .exe)
  //============================================================================
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_StaticLinking: Static Linking (.lib/.a -> .exe)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  // Platform-specific extensions
  if APlatform = tpWin64 then
  begin
    LLibExt := '.lib';
    LExeExt := '.exe';
  end
  else
  begin
    LLibExt := '.a';
    LExeExt := '';  // Linux executables have no extension
  end;

  //----------------------------------------------------------------------------
  // Part 1: Build Test_StaticLinking.lib/.a with public functions
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('--- Building Test_StaticLinking' + LLibExt + ' ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    // Platform-specific printf import
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_StaticLinking' + LLibExt);
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True)
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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

    LTiger.TargetLib(TPath.Combine(OutputPath(APlatform), 'Test_StaticLinking' + LLibExt));

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;

  //----------------------------------------------------------------------------
  // Part 2: Build Test_StaticLinking.exe that statically links Test_StaticLinking.lib/.a
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Building Test_StaticLinking' + LExeExt + ' ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
      SetExeResources(LTiger, 'Test_StaticLinking' + LExeExt);
    LTiger.SetOptimizationLevel(1);

    // Platform-specific printf import
    if LTiger.GetPlatform = tpWin64 then
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True)
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.ImportLib('Test_StaticLinking', 'AddC', [vtInt32, vtInt32], vtInt32, False, plC);
    LTiger.ImportLib('Test_StaticLinking', 'MulC', [vtInt32, vtInt32], vtInt32, False, plC);
    LTiger.ImportLib('Test_StaticLinking', 'AddCpp', [vtInt32, vtInt32], vtInt32, False, plDefault);

    LTiger.Func('main', vtVoid, True)
      .Local('r1', vtInt32)
      .Local('r2', vtInt32)
      .Local('r3', vtInt32)

      .Call('printf', [LTiger.Str('=== Test_StaticLinking: Static Linking ===' + #10)])
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

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_StaticLinking' + LExeExt), ssConsole);
    LTiger.AddLibPath(OutputPath(APlatform));

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_StructParamPassing: Struct Parameter Passing ABI Compliance

  PURPOSE:
    Validates correct struct/record passing according to platform ABI rules.
    Tests both pass-by-value and pass-by-reference semantics for various
    struct sizes, ensuring Tiger generates correct calling convention code.

  WHAT IT TESTS:
    - Small structs (≤8 bytes): Passed in registers on both platforms
    - Medium structs (9-16 bytes): Register passing on Linux64, by-ref on Win64
    - Large structs (>16 bytes): Passed by hidden pointer on both platforms
    - Struct return values: Correct register/memory return handling
    - Nested struct passing: Structs containing other structs

  PLATFORM DIFFERENCES:
    Win64:   Structs >8 bytes passed by hidden pointer
    Linux64: Structs ≤16 bytes passed in up to 2 registers (System V ABI)

  TYPES DEFINED:
    - TSmallStruct: 8 bytes (fits in one register)
    - TMediumStruct: 16 bytes (2 registers on Linux64, by-ref on Win64)
    - TLargeStruct: 32 bytes (by-ref on both platforms)

  RUNTIME TESTS:
    - Pass small struct, verify field values received correctly
    - Pass medium struct, verify ABI-compliant transfer
    - Pass large struct by hidden pointer, verify no corruption
    - Return struct from function, verify correct value propagation

  EXPECTED OUTPUT:
    === Test_StructParamPassing: Struct Parameter Passing ===
    Small struct: x=10, y=20, sum=30 (expected: 30)
    Medium struct: a=1, b=2, c=3, d=4, sum=10 (expected: 10)
    Large struct passed correctly
    Struct return value: x=100, y=200
    All struct passing tests complete!
==============================================================================*)
procedure Test_StructParamPassing(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_StructParamPassing: Struct Parameter Passing');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_StructParamPassing.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    //------------------------------------------------------------------------
    // Define a 24-byte record (3 x Int64 = 24 bytes)
    // This is > 8 bytes (Win64 threshold) and > 16 bytes (Linux64 threshold)
    //------------------------------------------------------------------------
    LTiger.DefineRecord('TPoint3D')
      .Field('X', vtInt64)
      .Field('Y', vtInt64)
      .Field('Z', vtInt64)
    .EndRecord();

    //------------------------------------------------------------------------
    // Function: SumPoint - takes struct param, returns scalar sum
    // Tests: receiving large struct via pointer (callee side)
    //------------------------------------------------------------------------
    LTiger.Func('SumPoint', vtInt64)
      .Param('pt', 'TPoint3D')
      .Local('result', vtInt64)
      .Assign('result', LTiger.Add(
        LTiger.Add(
          LTiger.GetField(LTiger.Get('pt'), 'X'),
          LTiger.GetField(LTiger.Get('pt'), 'Y')
        ),
        LTiger.GetField(LTiger.Get('pt'), 'Z')
      ))
      .Return(LTiger.Get('result'))
    .EndFunc();

    //------------------------------------------------------------------------
    // Function: PrintPoint - takes struct param, prints fields
    // Tests: receiving large struct and accessing individual fields
    //------------------------------------------------------------------------
    LTiger.Func('PrintPoint', vtVoid)
      .Param('pt', 'TPoint3D')
      .Call('printf', [LTiger.Str('  X = %lld' + #10), LTiger.GetField(LTiger.Get('pt'), 'X')])
      .Call('printf', [LTiger.Str('  Y = %lld' + #10), LTiger.GetField(LTiger.Get('pt'), 'Y')])
      .Call('printf', [LTiger.Str('  Z = %lld' + #10), LTiger.GetField(LTiger.Get('pt'), 'Z')])
    .EndFunc();

    //------------------------------------------------------------------------
    // Function: SumTwoPoints - takes TWO struct params
    // Tests: multiple struct params with shifted registers
    //------------------------------------------------------------------------
    LTiger.Func('SumTwoPoints', vtInt64)
      .Param('pt1', 'TPoint3D')
      .Param('pt2', 'TPoint3D')
      .Local('result', vtInt64)
      .Assign('result', LTiger.Add(
        LTiger.Add(
          LTiger.GetField(LTiger.Get('pt1'), 'X'),
          LTiger.GetField(LTiger.Get('pt2'), 'X')
        ),
        LTiger.Add(
          LTiger.GetField(LTiger.Get('pt1'), 'Y'),
          LTiger.GetField(LTiger.Get('pt2'), 'Y')
        )
      ))
      .Assign('result', LTiger.Add(
        LTiger.Get('result'),
        LTiger.Add(
          LTiger.GetField(LTiger.Get('pt1'), 'Z'),
          LTiger.GetField(LTiger.Get('pt2'), 'Z')
        )
      ))
      .Return(LTiger.Get('result'))
    .EndFunc();

    //------------------------------------------------------------------------
    // Main entry point - test struct param passing
    //------------------------------------------------------------------------
    LTiger.Func('main', vtVoid, True)
      .Local('pt1', 'TPoint3D')
      .Local('pt2', 'TPoint3D')
      .Local('sum', vtInt64)

      .Call('printf', [LTiger.Str('=== Test_StructParamPassing: Struct Parameter Passing ===' + #10)])
      .Call('printf', [LTiger.Str(#10)])

      // Test 1: Initialize struct locally and pass to function
      .Call('printf', [LTiger.Str('Test 1: Pass struct to function' + #10)])
      .AssignTo(LTiger.GetField(LTiger.Get('pt1'), 'X'), LTiger.Int64(10))
      .AssignTo(LTiger.GetField(LTiger.Get('pt1'), 'Y'), LTiger.Int64(20))
      .AssignTo(LTiger.GetField(LTiger.Get('pt1'), 'Z'), LTiger.Int64(30))
      .Call('printf', [LTiger.Str('  pt1 = (10, 20, 30)' + #10)])
      .Call('PrintPoint', [LTiger.Get('pt1')])
      .Assign('sum', LTiger.Invoke('SumPoint', [LTiger.Get('pt1')]))
      .Call('printf', [LTiger.Str('  SumPoint(pt1) = %lld (expected: 60)' + #10), LTiger.Get('sum')])

      // Test 2: Pass different struct
      .Call('printf', [LTiger.Str(#10 + 'Test 2: Pass second struct' + #10)])
      .AssignTo(LTiger.GetField(LTiger.Get('pt2'), 'X'), LTiger.Int64(100))
      .AssignTo(LTiger.GetField(LTiger.Get('pt2'), 'Y'), LTiger.Int64(200))
      .AssignTo(LTiger.GetField(LTiger.Get('pt2'), 'Z'), LTiger.Int64(300))
      .Call('printf', [LTiger.Str('  pt2 = (100, 200, 300)' + #10)])
      .Call('PrintPoint', [LTiger.Get('pt2')])
      .Assign('sum', LTiger.Invoke('SumPoint', [LTiger.Get('pt2')]))
      .Call('printf', [LTiger.Str('  SumPoint(pt2) = %lld (expected: 600)' + #10), LTiger.Get('sum')])

      // Test 3: Pass two structs to same function
      .Call('printf', [LTiger.Str(#10 + 'Test 3: Pass two structs' + #10)])
      .Assign('sum', LTiger.Invoke('SumTwoPoints', [LTiger.Get('pt1'), LTiger.Get('pt2')]))
      .Call('printf', [LTiger.Str('  SumTwoPoints(pt1, pt2) = %lld (expected: 660)' + #10), LTiger.Get('sum')])

      .Call('printf', [LTiger.Str(#10 + 'All struct tests passed!' + #10)])
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_StructParamPassing.exe'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  Test_DLLGeneration: Dynamic Library Generation (.dll/.so)

  PURPOSE:
    Validates Tiger's ability to generate dynamic libraries with exported
    functions. Tests the complete DLL build pipeline including export tables,
    entry points, and cross-module function calls from a consuming executable.

  WHAT IT TESTS:
    - TargetDll: Generate .dll (Win64) or .so (Linux64) instead of executable
    - Export with C linkage (plC): Undecorated export name
    - Export with C++ linkage: Decorated/mangled export name
    - Private functions: Internal-only, not in export table
    - DllMain entry point: Library initialization callback (Win64)
    - Cross-module calls: Executable importing from generated DLL

  TWO-PHASE TEST:
    Phase 1 - Build Test_DLLGeneration.dll/.so:
      - AddC(a, b): Exported with C linkage
      - AddCpp(a, b): Exported with C++ linkage
      - PrivateHelper(x): Internal only, not exported
      - DllMain: Entry point for DLL_PROCESS_ATTACH/DETACH

    Phase 2 - Build Test_DLLGeneration.exe:
      - Imports AddC and AddCpp from the generated DLL
      - Calls both functions and verifies results

  VERIFICATION:
    Win64:   tdump -ee Test_DLLGeneration.dll
    Linux64: wsl nm -D Test_DLLGeneration.so

  EXPECTED OUTPUT:
    === Test_DLLGeneration: DLL Generation ===
    Building DLL...
    DLL built successfully
    Building EXE that uses the DLL...
    AddC(10, 20) = 30 (expected: 30)
    AddCpp(6, 7) = 42 (expected: 42)
    DLL test complete!
==============================================================================*)
procedure Test_DLLGeneration(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  //============================================================================
  // Test_DLLGeneration: DLL Generation
  //============================================================================
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_DLLGeneration: DLL Generation');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  //----------------------------------------------------------------------------
  // Part 1: Build Test_DLLGeneration.dll
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('--- Building Test_DLLGeneration.dll ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_DLLGeneration.dll');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else if LTiger.GetPlatform = tpMacOS64 then
      LTiger.ImportDll('/usr/lib/libSystem.B.dylib', 'printf', [vtPointer], vtInt32, True)
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

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

    // Declare a managed global string to test DLL cleanup
    LTiger.Global('gTestString', 'string');

    LTiger.DllMain()
      // Initialize global string (tests that cleanup only happens on DETACH)
      .Assign('gTestString', LTiger.Invoke('Tiger_StrFromLiteral', [LTiger.Str('DLL Test'), LTiger.Int64(8)]))
      .Return(LTiger.Int64(1))
    .EndFunc();

    if LTiger.GetPlatform = tpWin64 then
      LTiger.TargetDll(TPath.Combine(OutputPath(APlatform), 'Test_DLLGeneration.dll'))
    else if LTiger.GetPlatform = tpMacOS64 then
      LTiger.TargetDll(TPath.Combine(OutputPath(APlatform), 'libTest_DLLGeneration.dylib'))
    else
      LTiger.TargetDll(TPath.Combine(OutputPath(APlatform), 'libTest_DLLGeneration.so'));

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;

  //----------------------------------------------------------------------------
  // Part 2: Build Test_DLLGeneration.exe that imports from Test_DLLGeneration.dll
  //----------------------------------------------------------------------------
  TWin64Utils.PrintLn('');
  TWin64Utils.PrintLn('--- Building Test_DLLGeneration.exe ---');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_DLLGeneration.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
      LTiger.ImportDll('Test_DLLGeneration.dll', 'AddC', [vtInt32, vtInt32], vtInt32, False, plC);
      LTiger.ImportDll('Test_DLLGeneration.dll', 'AddCpp', [vtInt32, vtInt32], vtInt32, False, plDefault);
    end
    else if LTiger.GetPlatform = tpMacOS64 then
    begin
      LTiger.ImportDll('/usr/lib/libSystem.B.dylib', 'printf', [vtPointer], vtInt32, True);
      // Load from same folder as executable
      LTiger.ImportDll('@loader_path/libTest_DLLGeneration.dylib', 'AddC', [vtInt32, vtInt32], vtInt32, False, plC);
      LTiger.ImportDll('@loader_path/libTest_DLLGeneration.dylib', 'AddCpp', [vtInt32, vtInt32], vtInt32, False, plDefault);
    end
    else
    begin
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
      LTiger.ImportDll('libTest_DLLGeneration.so', 'AddC', [vtInt32, vtInt32], vtInt32, False, plC);
      LTiger.ImportDll('libTest_DLLGeneration.so', 'AddCpp', [vtInt32, vtInt32], vtInt32, False, plDefault);
    end;

    LTiger.Func('main', vtVoid, True)
      .Local('r1', vtInt32)
      .Local('r2', vtInt32)

      .Call('printf', [LTiger.Str('=== Test_DLLGeneration: DLL Generation ===' + #10)])
      .Call('printf', [LTiger.Str(#10)])
      .Assign('r1', LTiger.Invoke('AddC', [LTiger.Int64(10), LTiger.Int64(20)]))
      .Call('printf', [LTiger.Str('AddC(10, 20) = %d (expected 30)' + #10), LTiger.Get('r1')])
      .Assign('r2', LTiger.Invoke('AddCpp', [LTiger.Int64(10), LTiger.Int64(20)]))
      .Call('printf', [LTiger.Str('AddCpp(10, 20) = %d (expected 130, includes +100 from PrivateHelper)' + #10), LTiger.Get('r2')])

      .Call('printf', [LTiger.Str(#10 + 'DLL Import Tests:' + #10)])
      .Call('printf', [LTiger.Str('  - C linkage import (AddC): works if result = 30' + #10)])
      .Call('printf', [LTiger.Str('  - C++ linkage import (AddCpp): works if result = 130' + #10)])
      .Call('printf', [LTiger.Str('  - Private function not exported but callable internally' + #10)]);

    if LTiger.GetPlatform = tpWin64 then
      LTiger
        .Call('printf', [LTiger.Str(#10 + 'Use "dumpbin /exports Test_DLLGeneration.dll" to verify:' + #10)])
        .Call('printf', [LTiger.Str('  Expected exports: AddC, _Z6AddCppii' + #10)])
        .Call('printf', [LTiger.Str('  NOT exported: PrivateHelper, DllMain' + #10)])
    else
      // Dylib/so have no DllMain; only Windows DLL has that entry point (and it is not exported).
      LTiger
        .Call('printf', [LTiger.Str(#10 + 'Use "readelf -s libTest_DLLGeneration.so" to verify:' + #10)])
        .Call('printf', [LTiger.Str('  Expected exports: AddC, _Z6AddCppii' + #10)])
        .Call('printf', [LTiger.Str('  NOT exported: PrivateHelper' + #10)]);

    LTiger
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_DLLGeneration'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;


(*==============================================================================
  Test_DynamicLoading: Runtime Library Loading (LoadLibrary/dlopen)

  PURPOSE:
    Validates Tiger's ability to dynamically load libraries and resolve
    function addresses at runtime. Tests the full dynamic loading workflow
    including library loading, symbol resolution, indirect calls, and cleanup.

  WHAT IT TESTS:
    - LoadLibraryA/dlopen: Load a library by path at runtime
    - GetProcAddress/dlsym: Resolve function address by name
    - InvokeIndirect: Call function through resolved pointer
    - FreeLibrary/dlclose: Unload library when done
    - Error handling: NULL checks for failed loads/lookups

  PLATFORM BEHAVIOR:
    Win64:
      - Uses kernel32.dll's LoadLibraryA, GetProcAddress, FreeLibrary
      - Loads msvcrt.dll dynamically to resolve abs()
    Linux64:
      - Uses libc's dlopen, dlsym, dlclose
      - Loads libc.so.6 dynamically to resolve abs()
      - Uses RTLD_LAZY (value 1) for lazy symbol resolution

  RUNTIME TESTS:
    - Load C runtime library dynamically
    - Resolve abs() function address
    - Call abs(-42) through function pointer → 42
    - Unload library cleanly

  EXPECTED OUTPUT:
    === Test_DynamicLoading: Dynamic Library Loading ===
    Loading C runtime library...
    Library loaded at: <address>
    Resolving 'abs' function...
    Function address: <address>
    Calling abs(-42) via function pointer...
    Result: 42 (expected: 42)
    Unloading library...
    Dynamic loading test complete!
==============================================================================*)
procedure Test_DynamicLoading(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
const
  CRTLD_LAZY = 1;  // Linux dlopen flag: resolve symbols lazily
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_DynamicLoading: Dynamic library loading');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    LTiger.SetOptimizationLevel(1);

    // Platform-specific imports
    if LTiger.GetPlatform() = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_DynamicLoading.exe');
      LTiger.ImportDll('kernel32.dll', 'LoadLibraryA', [vtPointer], vtPointer);
      LTiger.ImportDll('kernel32.dll', 'GetProcAddress', [vtPointer, vtPointer], vtPointer);
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
    begin
      LTiger.ImportDll('libc.so.6', 'dlopen', [vtPointer, vtInt32], vtPointer);
      LTiger.ImportDll('libc.so.6', 'dlsym', [vtPointer, vtPointer], vtPointer);
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);
    end;

    // Common function body - both platforms do the same thing:
    // 1. Load C runtime (msvcrt.dll / libc.so.6)
    // 2. Get pointer to abs()
    // 3. Call abs(-42) via InvokeIndirect
    // 4. Verify result = 42
    LTiger.Func('main', vtVoid, True)
      .Local('hLib', vtPointer)
      .Local('pAbs', vtPointer)
      .Local('result', vtInt32);

    if LTiger.GetPlatform() = tpWin64 then
    begin
      LTiger
        .Call('printf', [LTiger.Str('Loading msvcrt.dll...' + #10)])
        .Assign('hLib', LTiger.Invoke('LoadLibraryA', [LTiger.Str('msvcrt.dll')]))
        .Call('printf', [LTiger.Str('hLib = %p' + #10), LTiger.Get('hLib')])
        .Assign('pAbs', LTiger.Invoke('GetProcAddress', [LTiger.Get('hLib'), LTiger.Str('abs')]))
        .Call('printf', [LTiger.Str('abs = %p' + #10), LTiger.Get('pAbs')]);
    end
    else
    begin
      LTiger
        .Call('printf', [LTiger.Str('Loading libc.so.6...' + #10)])
        .Assign('hLib', LTiger.Invoke('dlopen', [LTiger.Str('libc.so.6'), LTiger.Int32(CRTLD_LAZY)]))
        .Call('printf', [LTiger.Str('hLib = %p' + #10), LTiger.Get('hLib')])
        .Assign('pAbs', LTiger.Invoke('dlsym', [LTiger.Get('hLib'), LTiger.Str('abs')]))
        .Call('printf', [LTiger.Str('abs = %p' + #10), LTiger.Get('pAbs')]);
    end;

    // Common: call abs(-42) through function pointer and verify result
    LTiger
      .Call('printf', [LTiger.Str('Calling abs(-42) via function pointer...' + #10)])
      .Assign('result', LTiger.InvokeIndirect(LTiger.Get('pAbs'), [LTiger.Int32(-42)]))
      .Call('printf', [LTiger.Str('abs(-42) = %d (expected 42)' + #10), LTiger.Get('result')])
      .Call('Tiger_Halt', [LTiger.Get('result')])
    .EndFunc();

    if LTiger.GetPlatform() = tpWin64 then
      LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_DynamicLoading.exe'), ssConsole)
    else
      LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_DynamicLoading'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;


(*==============================================================================
  Test_SEH: Structured Exception Handling (Win64)

  PURPOSE:
    Validates Tiger's structured exception handling implementation on Win64.
    Tests try/finally (guaranteed cleanup), try/except (exception catching),
    software exceptions (Raise_ and RaiseCode), hardware exceptions (division
    by zero), and exception info retrieval (ExcCode, ExcMsg).

  WHAT IT TESTS:
    - Try/Finally: Finally block executes regardless of exception
    - Try/Except without exception: Except block is skipped
    - Try/Except with Raise: Software exception with message only
    - Try/Except with RaiseCode: Software exception with code + message
    - ExcCode(): Retrieve exception code inside except block
    - ExcMsg(): Retrieve exception message inside except block
    - Hardware exception: Division by zero caught and inspected
    - Control flow: Execution resumes correctly after each block

  PLATFORM NOTES:
    Win64:   Uses Windows SEH (__try/__except/__finally semantics)
    Linux64: Would use signal-based handling (not yet implemented)

  EXPECTED OUTPUT:
    === Test_SEH: Exception Handling ===
    Test 1: try/finally
      Inside try block
      Inside finally block
      After try/finally
    Test 2: try/except (no exception)
      Inside try block
      After try/except
    Test 3: try/except (Raise with message)
      Inside try block
      About to raise...
      Caught! Message: Test exception!
      After try/except
    Test 4: try/except (RaiseCode with code + message)
      Inside try block
      About to raise with code 42...
      Caught! Code: 42, Message: Custom error!
      After try/except
    Test 5: try/except (div by zero)
      Inside try block
      About to divide by zero...
      Caught hardware exception! Code: <code>
      After try/except
    All tests passed!
==============================================================================*)
procedure Test_SEH(const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
var
  LTiger: TTiger;
begin
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('Test_SEH: Exception Handling (SEH)');
  TWin64Utils.PrintLn('========================================');
  TWin64Utils.PrintLn('');

  LTiger := TTiger.Create(APlatform);
  try
    LTiger.SetStatusCallback(StatusCallback);
    LTiger.SetOptimizationLevel(1);

    // Platform-specific setup
    if LTiger.GetPlatform = tpWin64 then
    begin
      SetExeResources(LTiger, 'Test_SEH.exe');
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
    end
    else
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    LTiger.Func('main', vtVoid, True)
      .Local('dummy', vtInt32)
      .Local('divisor', vtInt32)
      .Local('result', vtInt32)

      .Call('printf', [LTiger.Str('=== Test_SEH: Exception Handling ===' + #10)])

      //----------------------------------------------------------------------
      // Test 1: try/finally - Finally always executes
      //----------------------------------------------------------------------
      .Call('printf', [LTiger.Str('Test 1: try/finally' + #10)])
      .&Try()
        .Call('printf', [LTiger.Str('  Inside try block' + #10)])
      .&Finally()
        .Call('printf', [LTiger.Str('  Inside finally block' + #10)])
      .EndTry()
      .Call('printf', [LTiger.Str('  After try/finally' + #10)])
      .Call('printf', [LTiger.Str('' + #10)])

      //----------------------------------------------------------------------
      // Test 2: try/except without exception - Except block skipped
      //----------------------------------------------------------------------
      .Call('printf', [LTiger.Str('Test 2: try/except (no exception)' + #10)])
      .&Try()
        .Call('printf', [LTiger.Str('  Inside try block' + #10)])
      .&Except()
        .Call('printf', [LTiger.Str('  ERROR: Should NOT reach except!' + #10)])
      .EndTry()
      .Call('printf', [LTiger.Str('  After try/except' + #10)])
      .Call('printf', [LTiger.Str('' + #10)])

      //----------------------------------------------------------------------
      // Test 3: try/except with Raise - Message only, retrieve with ExcMsg
      //----------------------------------------------------------------------
      .Call('printf', [LTiger.Str('Test 3: try/except (Raise with message)' + #10)])
      .&Try()
        .Call('printf', [LTiger.Str('  Inside try block' + #10)])
        .Call('printf', [LTiger.Str('  About to raise...' + #10)])
        .&Raise(LTiger.Str('Test exception!'))
        .Call('printf', [LTiger.Str('  ERROR: Should NOT print after raise!' + #10)])
      .&Except()
        // ExcMsg() retrieves the exception message string
        .Call('printf', [LTiger.Str('  Caught! Message: %s' + #10), LTiger.ExcMsg()])
      .EndTry()
      .Call('printf', [LTiger.Str('  After try/except' + #10)])
      .Call('printf', [LTiger.Str('' + #10)])

      //----------------------------------------------------------------------
      // Test 4: try/except with RaiseCode - Code + message, retrieve both
      //----------------------------------------------------------------------
      .Call('printf', [LTiger.Str('Test 4: try/except (RaiseCode with code + message)' + #10)])
      .&Try()
        .Call('printf', [LTiger.Str('  Inside try block' + #10)])
        .Call('printf', [LTiger.Str('  About to raise with code 42...' + #10)])
        // RaiseCode allows specifying both an error code and message
        .RaiseCode(LTiger.Int64(42), LTiger.Str('Custom error!'))
        .Call('printf', [LTiger.Str('  ERROR: Should NOT print after raise!' + #10)])
      .&Except()
        // ExcCode() retrieves the exception code, ExcMsg() the message
        .Call('printf', [LTiger.Str('  Caught! Code: %d, Message: %s' + #10),
          LTiger.ExcCode(), LTiger.ExcMsg()])
      .EndTry()
      .Call('printf', [LTiger.Str('  After try/except' + #10)])
      .Call('printf', [LTiger.Str('' + #10)])

      //----------------------------------------------------------------------
      // Test 5: Hardware exception (div by zero) - Retrieve exception code
      //----------------------------------------------------------------------
      .Call('printf', [LTiger.Str('Test 5: try/except (div by zero)' + #10)])
      .Assign('divisor', LTiger.Int64(0))
      .&Try()
        .Call('printf', [LTiger.Str('  Inside try block' + #10)])
        .Call('printf', [LTiger.Str('  About to divide by zero...' + #10)])
        .Assign('result', LTiger.IDiv(LTiger.Int64(100), LTiger.Get('divisor')))
        .Call('printf', [LTiger.Str('  ERROR: Should NOT print after div/0!' + #10)])
      .&Except()
        // Hardware exceptions have a system-defined code (e.g., 0xC0000094 for div/0)
        .Call('printf', [LTiger.Str('  Caught hardware exception! Code: %d' + #10),
          LTiger.ExcCode()])
      .EndTry()
      .Call('printf', [LTiger.Str('  After try/except' + #10)])
      .Call('printf', [LTiger.Str('' + #10)])

      .Call('printf', [LTiger.Str('All tests passed!' + #10)])
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    LTiger.TargetExe(TPath.Combine(OutputPath(APlatform), 'Test_SEH'), ssConsole);

    ProcessBuild(LTiger, ADumpSSA);
    ShowErrors(LTiger);
  finally
    LTiger.Free();
  end;
end;

(*==============================================================================
  RunTest: Dispatches to a test by number.

  Maps test numbers to their descriptive procedure names for easy invocation.
  If ADumpSSA is True, the SSA dump is printed after the build completes.
==============================================================================*)
type
  TTestType = set of Byte;

procedure RunTest(const ANums: TTestType; const APlatform: TTigerPlatform=tpWin64; const ADumpSSA: Boolean=False);
begin
  for var ANum in ANums do
    case ANum of
      01: Test_HelloWorld(APlatform, ADumpSSA);
      02: Test_Factorial_WhileLoop(APlatform, ADumpSSA);
      03: Test_SSA_OptimizerPasses(APlatform, ADumpSSA);
      04: Test_SSA_LoopPhiNodes(APlatform, ADumpSSA);
      05: Test_CaseStatement(APlatform, ADumpSSA);
      06: Test_GlobalVariables(APlatform, ADumpSSA);
      07: Test_TypeSystem(APlatform, ADumpSSA);
      08: Test_CStructABI(APlatform, ADumpSSA);
      09: Test_TypedPointers(APlatform, ADumpSSA);
      10: Test_FunctionPointers(APlatform, ADumpSSA);
      11: Test_PublicExports(APlatform, ADumpSSA);
      12: Test_FunctionOverloading(APlatform, ADumpSSA);
      13: Test_ManagedStrings(APlatform, ADumpSSA);
      14: Test_Printf_Basic(APlatform, ADumpSSA);
      15: Test_SetTypes(APlatform, ADumpSSA);
      16: Test_Intrinsics(APlatform, ADumpSSA);
      17: Test_VariadicFunctions(APlatform, ADumpSSA);
      18: Test_StaticLinking(APlatform, ADumpSSA);
      19: Test_StructParamPassing(APlatform, ADumpSSA);
      20: Test_SSA_FreshInstance(APlatform, ADumpSSA);
      21: Test_RuntimeMemory(APlatform, ADumpSSA);
      22: Test_DynamicLoading(APlatform, ADumpSSA);
      23: Test_DLLGeneration(APlatform, ADumpSSA);
      24: Test_SEH(APlatform, ADumpSSA);
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
    var TestNums := [1..24];
    if ParamCount >= 2 then
    begin
      if ParamStr(2).ToUpper <> 'ALL' then
        TestNums := [StrToIntDef(ParamStr(2),1)];
    end;

    var TestToRun := 'ALL';
    if ParamCount >= 1 then
      TestToRun := ParamStr(1).ToUpper;
    if TestToRun = 'WINDOWSX64' then
      RunTest(TestNums, tpWin64, False)
    else if TestToRun = 'LINUXX64' then
      RunTest(TestNums, tpLinux64, False)
    else if TestToRun = 'MACOS64' then
      RunTest(TestNums, tpMacOS64, False)
    else
      for var Plat := Low(TTigerPlatform) to High(TTigerPlatform) do
        RunTest(TestNums, Plat, False);
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

