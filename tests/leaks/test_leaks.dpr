program test_leaks;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Tiger;

procedure BuildLeakSanityForPlatform(Plat : TTigerPlatform);
var
  LTiger: TTiger;
  LExitCode: Cardinal;
begin
  WriteLn('Building macOS Apple Silicon leak sanity executable...');
  WriteLn('');

  LTiger := TTiger.Create(Plat);
  try
    // Debug: enables Tiger_AllocCount/Tiger_FreeCount globals + leak reporting on halt.
    LTiger.SetOptimizationLevel(0);

    LTiger.SetStatusCallback(
      procedure(const AText: string; const AUserData: Pointer)
      begin
        WriteLn(AText);
      end, nil);

    // Build a program that does one alloc and one free.
    LTiger.Func('main', vtVoid, True)
      .Local('p', vtPointer)
      .Call('printf', [LTiger.Str('Leak sanity test (macOS arm64)'#10)])
      .Assign('p', LTiger.Invoke('Tiger_GetMem', [LTiger.Int64(16)]))
      .Call('Tiger_FreeMem', [LTiger.Get('p')])
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    var sPlatform : string;
    case Plat of
      tpWin64:   sPlatform := 'win';
      tpLinux64: sPlatform := 'lin';
      tpMacOS64: sPlatform := 'mac';
    end;
    LTiger.TargetExe(TPath.Combine('output', 'leak_test_'+sPlatform), ssConsole);

    if LTiger.Build(False, @LExitCode) then
    begin
      WriteLn('');
      WriteLn('========================================');
      WriteLn('Build successful!');
      WriteLn('Output file: output\\tests\\leaks\\leak_sanity_macos');
      WriteLn('');
      WriteLn('Next steps:');
      WriteLn('1. Copy output\\tests\\leaks\\leak_sanity_macos to an Apple Silicon Mac');
      WriteLn('2. In Terminal: chmod +x leak_sanity_macos');
      WriteLn('3. Run: ./leak_sanity_macos');
      WriteLn('Expected leak line: [Heap] Allocs: 1, Frees: 1, Leaked: 0');
      WriteLn('========================================');
    end
    else
    begin
      WriteLn('');
      WriteLn('Build failed!');
      if LTiger.HasErrors() then
        WriteLn(LTiger.GetErrorText());
    end;
  finally
    LTiger.Free();
  end;
end;

begin
  try
    for var plat := Low(TTigerPlatform) to High(TTigerPlatform) do
      BuildLeakSanityForPlatform(plat);
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

