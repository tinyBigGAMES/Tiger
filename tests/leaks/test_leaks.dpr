program test_leaks;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Tiger;

function PlatformLabel(const Plat: TTigerPlatform): string;
begin
  case Plat of
    tpWin64:      Result := 'Windows x64';
    tpLinux64:    Result := 'Linux x64';
    tpMacOS64:    Result := 'MacOS ARM64';
    tpLinuxARM64: Result := 'Linux ARM64';
  else
    Result := 'Unknown platform';
  end;
end;

procedure BuildLeakSanityForPlatform(Plat : TTigerPlatform);
var
  LTiger: TTiger;
  LExitCode: Cardinal;
  LPlatform : string;
  sPlatform: string;
begin
  LPlatform := PlatformLabel(Plat);
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

    if Plat = tpWin64 then
      LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True)
    else if Plat <> tpMacOS64 then
      LTiger.ImportDll('libc.so.6', 'printf', [vtPointer], vtInt32, True);

    // Build a program that does one alloc and one free.
    LTiger.Func('main', vtVoid, True)
      .Local('p', vtPointer)
      .Call('printf', [LTiger.Str('Leak sanity test (' + LPlatform + ')'#10)])
      .Assign('p', LTiger.Invoke('Tiger_GetMem', [LTiger.Int64(16)]))
      .Call('Tiger_FreeMem', [LTiger.Get('p')])
      .Call('Tiger_Halt', [LTiger.Int64(0)])
    .EndFunc();

    case Plat of
      tpWin64:      sPlatform := 'win';
      tpLinux64:    sPlatform := 'lin';
      tpMacOS64:    sPlatform := 'mac';
      tpLinuxARM64: sPlatform := 'linarm';
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
      if Plat = tpWin64 then
        WriteLn('1. Run: .\output\leak_test_'+sPlatform+' in a command line window')
      else if Plat = tpMacOS64 then
      begin
        WriteLn('1. chmod +x output/leak_test_'+sPlatform);
        WriteLn('2. Run: ./output/leak_test_'+sPlatform);
      end
      else if Plat = tpLinuxARM64 then
      begin
        WriteLn('1. Run under linux/arm64 (Docker) or on a Linux ARM64 host');
        WriteLn('2. chmod +x output/leak_test_'+sPlatform+' && ./output/leak_test_'+sPlatform);
      end
      else
      begin
        WriteLn('1. chmod +x output/leak_test_'+sPlatform);
        WriteLn('2. Run: ./output/leak_test_'+sPlatform);
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
    for var plat in [tpWin64, tpLinux64, tpMacOS64, tpLinuxARM64] do
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

