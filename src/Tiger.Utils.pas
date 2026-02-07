{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Utils;

{$I Tiger.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.IOUtils,
  System.AnsiStrings,
  System.Classes,
  System.Math,
  System.Hash;

const
  COLOR_RESET   = #27'[0m';
  COLOR_BOLD    = #27'[1m';
  COLOR_RED     = #27'[31m';
  COLOR_GREEN   = #27'[32m';
  COLOR_YELLOW  = #27'[33m';
  COLOR_BLUE    = #27'[34m';
  COLOR_CYAN    = #27'[36m';
  COLOR_WHITE   = #27'[37m';

type

  { TCallback }
  TCallback<T> = record
    Callback: T;
    UserData: Pointer;
    function IsAssigned(): Boolean;
  end;

  { TCaptureConsoleCallback }
  TCaptureConsoleCallback = reference to procedure(const ALine: string; const AUserData: Pointer);

  { TStatusCallback }
  TStatusCallback = reference to procedure(const AText: string; const AUserData: Pointer);

  { TVersionInfo }
  TVersionInfo = record
    Major: Word;
    Minor: Word;
    Patch: Word;
    Build: Word;
    VersionString: string;
  end;

  { TUtils - Platform-independent utilities }
  TUtils = class
  private class var
    FMarshaller: TMarshaller;
  public
    class procedure FailIf(const Cond: Boolean; const Msg: string; const AArgs: array of const);

    class function  AsUTF8(const AValue: string; ALength: PCardinal=nil): Pointer; static;
    class function  ToAnsi(const AValue: string): AnsiString; static;

    class function  CreateDirInPath(const AFilename: string): Boolean;

    class procedure CopyFilePreservingEncoding(const ASourceFile, ADestFile: string); static;
    class function  DetectFileEncoding(const AFilePath: string): TEncoding; static;
    class function  EnsureBOM(const AText: string): string; static;
    class function  EscapeString(const AText: string): string; static;
    class function  StripAnsi(const AText: string): string; static;
    class function  ExtractAnsiCodes(const AText: string): string; static;

    class function  GetFileSHA256(const APath: string): string; static;
    class function  GetRelativePath(const ABasePath, AFullPath: string): string; static;

    class function  GetEnv(const AName: string): string; static;
    class function  HasEnv(const AName: string): Boolean; static;
  end;

  { TBaseObject }
  TBaseObject = class
  public
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;


implementation

{ TCallback<T> }

function TCallback<T>.IsAssigned(): Boolean;
begin
  Result := PPointer(@Callback)^ <> nil;
end;

{ TUtils }

class procedure TUtils.FailIf(const Cond: Boolean; const Msg: string; const AArgs: array of const);
  begin
    if Cond then
      raise Exception.CreateFmt(Msg, AArgs);
  end;

class function TUtils.AsUTF8(const AValue: string; ALength: PCardinal): Pointer;
begin
  Result := FMarshaller.AsUtf8(AValue).ToPointer;
  if Assigned(ALength) then
    ALength^ := System.AnsiStrings.StrLen(PAnsiChar(Result));

end;

class function TUtils.ToAnsi(const AValue: string): AnsiString;
var
  LBytes: TBytes;
begin
  LBytes := TEncoding.ANSI.GetBytes(AValue);
   if Length(LBytes) = 0 then
    Exit('');
  SetString(Result, PAnsiChar(@LBytes[0]), Length(LBytes));
end;

class function TUtils.CreateDirInPath(const AFilename: string): Boolean;
var
  LPath: string;
begin
  // If AFilename is a directory, use it directly; otherwise, extract its directory part
  if TPath.HasExtension(AFilename) then
    LPath := TPath.GetDirectoryName(AFilename)
  else
    LPath := AFilename;

  if LPath.IsEmpty then
    Exit(False);

  if not TDirectory.Exists(LPath) then
    TDirectory.CreateDirectory(LPath);

  Result := True;
end;


class procedure TUtils.CopyFilePreservingEncoding(const ASourceFile, ADestFile: string);
var
  LSourceBytes: TBytes;
begin
  // Validate source file exists
  if not TFile.Exists(ASourceFile) then
    raise Exception.CreateFmt('CopyFilePreservingEncoding: Source file not found: %s', [ASourceFile]);

  // Ensure destination directory exists
  CreateDirInPath(ADestFile);

  // Read all bytes from source file
  LSourceBytes := TFile.ReadAllBytes(ASourceFile);
  
  // Write bytes to destination - this preserves EVERYTHING including BOM
  TFile.WriteAllBytes(ADestFile, LSourceBytes);
end;


class function TUtils.DetectFileEncoding(const AFilePath: string): TEncoding;
var
  LBytes: TBytes;
  LEncoding: TEncoding;
begin
  // Validate file exists
  if not TFile.Exists(AFilePath) then
    raise Exception.CreateFmt('DetectFileEncoding: File not found: %s', [AFilePath]);

  // Read a sample of bytes (first 4KB should be enough for BOM detection)
  LBytes := TFile.ReadAllBytes(AFilePath);

  if Length(LBytes) = 0 then
    Exit(TEncoding.Default);

  // Let TEncoding detect the encoding from BOM
  LEncoding := nil;
  TEncoding.GetBufferEncoding(LBytes, LEncoding, TEncoding.Default);

  Result := LEncoding;
end;


class function TUtils.EnsureBOM(const AText: string): string;
const
  UTF16_BOM = #$FEFF;
begin
  Result := AText;
  if (Length(Result) = 0) or (Result[1] <> UTF16_BOM) then
    Result := UTF16_BOM + Result;
end;


class function TUtils.EscapeString(const AText: string): string;
var
  LI: Integer;
  LChar: Char;
  LNextChar: Char;
begin
  Result := '';
  LI := 1;
  
  while LI <= Length(AText) do
  begin
    LChar := AText[LI];
    
    case LChar of
      #13: // Carriage return
        begin
          Result := Result + '\r';
          Inc(LI);
        end;
      #10: // Line feed
        begin
          Result := Result + '\n';
          Inc(LI);
        end;
      #9: // Tab
        begin
          Result := Result + '\t';
          Inc(LI);
        end;
      '"': // Quote
        begin
          Result := Result + '\"';
          Inc(LI);
        end;
      '\': // Backslash - requires look-ahead
        begin
          if LI < Length(AText) then
          begin
            LNextChar := AText[LI + 1];
            
            // Preserve valid C++ escape sequences: \x (hex), \n, \r, \t, \", \\
            if CharInSet(LNextChar, ['x', 'n', 'r', 't', '"', '\']) then
            begin
              // Valid C++ escape sequence - preserve the backslash
              Result := Result + '\';
            end
            else
            begin
              // Not a recognized escape - escape the backslash for paths etc.
              Result := Result + '\\';
            end;
          end
          else
          begin
            // Backslash at end of string - escape it
            Result := Result + '\\';
          end;
          Inc(LI);
        end;
    else
      // Regular character - append as-is
      Result := Result + LChar;
      Inc(LI);
    end;
  end;
end;


class function TUtils.StripAnsi(const AText: string): string;
var
  LResult: TStringBuilder;
  LIdx: Integer;
  LLen: Integer;
  LInEscape: Boolean;
begin
  LResult := TStringBuilder.Create();
  try
    LLen := Length(AText);
    LIdx := 1;
    LInEscape := False;
    while LIdx <= LLen do
    begin
      if LInEscape then
      begin
        if CharInSet(AText[LIdx], ['A'..'Z', 'a'..'z', '~']) then
          LInEscape := False;
      end
      else if AText[LIdx] = #27 then
        LInEscape := True
      else
        LResult.Append(AText[LIdx]);
      Inc(LIdx);
    end;
    Result := LResult.ToString();
  finally
    LResult.Free();
  end;
end;


class function TUtils.ExtractAnsiCodes(const AText: string): string;
var
  LResult: TStringBuilder;
  LIdx: Integer;
  LLen: Integer;
  LInEscape: Boolean;
begin
  LResult := TStringBuilder.Create();
  try
    LLen := Length(AText);
    LIdx := 1;
    LInEscape := False;
    while LIdx <= LLen do
    begin
      if LInEscape then
      begin
        LResult.Append(AText[LIdx]);
        if CharInSet(AText[LIdx], ['A'..'Z', 'a'..'z', '~']) then
          LInEscape := False;
      end
      else if AText[LIdx] = #27 then
      begin
        LInEscape := True;
        LResult.Append(AText[LIdx]);
      end;
      Inc(LIdx);
    end;
    Result := LResult.ToString();
  finally
    LResult.Free();
  end;
end;


class function TUtils.GetFileSHA256(const APath: string): string;
begin
  Result := THashSHA2.GetHashStringFromFile(APath).ToLower();
end;


class function TUtils.GetRelativePath(const ABasePath, AFullPath: string): string;
var
  LBasePath: string;
  LFullPath: string;
  LBaseLen: Integer;
begin
  LBasePath := ABasePath.Replace('\', '/');
  LFullPath := AFullPath.Replace('\', '/');

  // Ensure base path ends with /
  if (LBasePath <> '') and not LBasePath.EndsWith('/') then
    LBasePath := LBasePath + '/';

  // If paths share a common prefix, strip it
  if LFullPath.ToLower().StartsWith(LBasePath.ToLower()) then
  begin
    LBaseLen := Length(LBasePath);
    Result := Copy(LFullPath, LBaseLen + 1, Length(LFullPath) - LBaseLen);
  end
  else
    Result := LFullPath;  // Can't make relative, return with forward slashes
end;


class function TUtils.GetEnv(const AName: string): string;
begin
  Result := GetEnvironmentVariable(AName);
end;

class function TUtils.HasEnv(const AName: string): Boolean;
begin
  Result := not GetEnv(AName).IsEmpty();
end;


{ TBaseObject }

constructor TBaseObject.Create();
begin
  inherited;
end;

destructor TBaseObject.Destroy();
begin
  inherited;
end;

end.
