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
  LPlatform : string;
begin
  case Plat of
    tpWin64:   LPlatform := 'Windows x64';
    tpLinux64: LPlatform := 'Linux x64';
    tpMacOS64: LPlatform := 'MacOS ARM64';
  end;
  WriteLn('Building leak sanity executable for '+LPlatform);
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
      WriteLn('Output file: output\leak_test_'+sPlatform);
      WriteLn('');
      WriteLn('Next steps:');
      if Plat in [tpLinux64, tpMacOS64] then
      begin
        WriteLn('1. Copy output\leak_sanity_macos to an Apple Silicon Mac');
        WriteLn('2. In Terminal: chmod +x leak_test_'+sPlatform);
        WriteLn('3. Run: ./leak_test_'+sPlatform);
      end else
      begin
        WriteLn('1. Run: .\leak_test_'+sPlatform+' in a command line window');
      end;
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

