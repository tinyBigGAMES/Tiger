{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Errors;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Resources;

const
  DEFAULT_MAX_ERRORS = 1;

type

  { TErrorSeverity }
  TErrorSeverity = (
    esHint,
    esWarning,
    esError,
    esFatal
  );

  { TSourceRange }
  TSourceRange = record
    Filename: string;
    StartLine: Integer;
    StartColumn: Integer;
    EndLine: Integer;
    EndColumn: Integer;
    StartByteOffset: Integer;
    EndByteOffset: Integer;
    
    procedure Clear();
    function IsEmpty(): Boolean;
    function ToPointString(): string;
    function ToRangeString(): string;
  end;

  { TErrorRelated }
  TErrorRelated = record
    Range: TSourceRange;
    Message: string;
  end;

  { TError }
  TError = record
    Range: TSourceRange;
    Severity: TErrorSeverity;
    Code: string;
    Message: string;
    Related: TArray<TErrorRelated>;

    function GetSeverityString(): string;
    function ToIDEString(): string;
    function ToFullString(): string;
  end;

  { TErrors }
  TErrors = class(TBaseObject)
  private
    FItems: TList<TError>;
    FMaxErrors: Integer;

    function CountErrors(): Integer;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    // Full location with range
    procedure Add(
      const ARange: TSourceRange;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string
    ); overload;

    procedure Add(
      const ARange: TSourceRange;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    // Point location (start = end)
    procedure Add(
      const AFilename: string;
      const ALine: Integer;
      const AColumn: Integer;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string
    ); overload;

    procedure Add(
      const AFilename: string;
      const ALine: Integer;
      const AColumn: Integer;
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    // No location
    procedure Add(
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string
    ); overload;

    procedure Add(
      const ASeverity: TErrorSeverity;
      const ACode: string;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    // Add related info to most recent error
    procedure AddRelated(
      const ARange: TSourceRange;
      const AMessage: string
    ); overload;

    procedure AddRelated(
      const ARange: TSourceRange;
      const AMessage: string;
      const AArgs: array of const
    ); overload;

    function HasHints(): Boolean;
    function HasWarnings(): Boolean;
    function HasErrors(): Boolean;
    function HasFatal(): Boolean;
    function Count(): Integer;
    function ErrorCount(): Integer;
    function WarningCount(): Integer;
    function ReachedMaxErrors(): Boolean;
    procedure Clear();

    function GetItems(): TList<TError>;
    function GetMaxErrors(): Integer;
    procedure SetMaxErrors(const AMaxErrors: Integer);
  end;

implementation

{ TSourceRange }

procedure TSourceRange.Clear();
begin
  Filename := '';
  StartLine := 0;
  StartColumn := 0;
  EndLine := 0;
  EndColumn := 0;
  StartByteOffset := 0;
  EndByteOffset := 0;
end;

function TSourceRange.IsEmpty(): Boolean;
begin
  Result := (StartLine = 0) and (StartColumn = 0);
end;

function TSourceRange.ToPointString(): string;
begin
  if IsEmpty() then
    Result := ''
  else
    Result := Format('%s(%d,%d)', [Filename, StartLine, StartColumn]);
end;

function TSourceRange.ToRangeString(): string;
begin
  if IsEmpty() then
    Result := ''
  else if (StartLine = EndLine) and (StartColumn = EndColumn) then
    Result := Format('%s(%d,%d)', [Filename, StartLine, StartColumn])
  else if StartLine = EndLine then
    Result := Format('%s(%d,%d-%d)', [Filename, StartLine, StartColumn, EndColumn])
  else
    Result := Format('%s(%d,%d)-(%d,%d)', [Filename, StartLine, StartColumn, EndLine, EndColumn]);
end;

{ TError }

function TError.GetSeverityString(): string;
begin
  case Severity of
    esHint:    Result := RSSeverityHint;
    esWarning: Result := RSSeverityWarning;
    esError:   Result := RSSeverityError;
    esFatal:   Result := RSSeverityFatal;
  else
    Result := RSSeverityUnknown;
  end;
end;

function TError.ToIDEString(): string;
begin
  if Range.IsEmpty() then
    Result := Format(RSErrorFormatSimple, [GetSeverityString(), Code, Message])
  else
    Result := Format(RSErrorFormatWithLocation, [Range.ToPointString(), GetSeverityString(), Code, Message]);
end;

function TError.ToFullString(): string;
var
  LBuilder: TStringBuilder;
  LI: Integer;
begin
  LBuilder := TStringBuilder.Create();
  try
    LBuilder.AppendLine(ToIDEString());
    
    for LI := 0 to High(Related) do
    begin
      if Related[LI].Range.IsEmpty() then
        LBuilder.AppendFormat(RSErrorFormatRelatedSimple, [RSSeverityNote, Related[LI].Message])
      else
        LBuilder.AppendFormat(RSErrorFormatRelatedWithLocation, [Related[LI].Range.ToPointString(), RSSeverityNote, Related[LI].Message]);
      LBuilder.AppendLine();
    end;
    
    Result := LBuilder.ToString().TrimRight();
  finally
    LBuilder.Free();
  end;
end;

{ TErrors }

constructor TErrors.Create();
begin
  inherited;

  FItems := TList<TError>.Create();
  FMaxErrors := DEFAULT_MAX_ERRORS;
end;

destructor TErrors.Destroy();
begin
  FItems.Free();

  inherited;
end;

function TErrors.CountErrors(): Integer;
var
  LError: TError;
begin
  Result := 0;
  for LError in FItems do
  begin
    if LError.Severity in [esError, esFatal] then
      Inc(Result);
  end;
end;

procedure TErrors.Add(
  const ARange: TSourceRange;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string
);
var
  LError: TError;
  LRange: TSourceRange;
begin
  // Stop adding errors after limit reached (except fatal)
  if (ASeverity = esError) and (CountErrors() >= FMaxErrors) then
    Exit;

  // Normalize the filename to absolute path with forward slashes
  LRange := ARange;
  if LRange.Filename <> '' then
    LRange.Filename := TPath.GetFullPath(LRange.Filename).Replace('\', '/');

  LError.Range := LRange;
  LError.Severity := ASeverity;
  LError.Code := ACode;
  LError.Message := AMessage;
  SetLength(LError.Related, 0);

  FItems.Add(LError);
end;

procedure TErrors.Add(
  const ARange: TSourceRange;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string;
  const AArgs: array of const
);
begin
  Add(ARange, ASeverity, ACode, Format(AMessage, AArgs));
end;

procedure TErrors.Add(
  const AFilename: string;
  const ALine: Integer;
  const AColumn: Integer;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string
);
var
  LRange: TSourceRange;
begin
  LRange.Filename := AFilename;
  LRange.StartLine := ALine;
  LRange.StartColumn := AColumn;
  LRange.EndLine := ALine;
  LRange.EndColumn := AColumn;
  
  Add(LRange, ASeverity, ACode, AMessage);
end;

procedure TErrors.Add(
  const AFilename: string;
  const ALine: Integer;
  const AColumn: Integer;
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string;
  const AArgs: array of const
);
begin
  Add(AFilename, ALine, AColumn, ASeverity, ACode, Format(AMessage, AArgs));
end;

procedure TErrors.Add(
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string
);
var
  LRange: TSourceRange;
begin
  LRange.Clear();
  Add(LRange, ASeverity, ACode, AMessage);
end;

procedure TErrors.Add(
  const ASeverity: TErrorSeverity;
  const ACode: string;
  const AMessage: string;
  const AArgs: array of const
);
begin
  Add(ASeverity, ACode, Format(AMessage, AArgs));
end;

procedure TErrors.AddRelated(
  const ARange: TSourceRange;
  const AMessage: string
);
var
  LError: TError;
  LRelated: TErrorRelated;
  LLen: Integer;
begin
  if FItems.Count = 0 then
    Exit;
    
  LError := FItems[FItems.Count - 1];
  
  LRelated.Range := ARange;
  LRelated.Message := AMessage;
  
  LLen := Length(LError.Related);
  SetLength(LError.Related, LLen + 1);
  LError.Related[LLen] := LRelated;
  
  FItems[FItems.Count - 1] := LError;
end;

procedure TErrors.AddRelated(
  const ARange: TSourceRange;
  const AMessage: string;
  const AArgs: array of const
);
begin
  AddRelated(ARange, Format(AMessage, AArgs));
end;

function TErrors.HasHints(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity = esHint then
      Exit(True);
  end;
end;

function TErrors.HasWarnings(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity = esWarning then
      Exit(True);
  end;
end;

function TErrors.HasErrors(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity in [esError, esFatal] then
      Exit(True);
  end;
end;

function TErrors.HasFatal(): Boolean;
var
  LError: TError;
begin
  Result := False;
  for LError in FItems do
  begin
    if LError.Severity = esFatal then
      Exit(True);
  end;
end;

function TErrors.Count(): Integer;
begin
  Result := FItems.Count;
end;

function TErrors.ErrorCount(): Integer;
begin
  Result := CountErrors();
end;

function TErrors.WarningCount(): Integer;
var
  LError: TError;
begin
  Result := 0;
  for LError in FItems do
  begin
    if LError.Severity = esWarning then
      Inc(Result);
  end;
end;

function TErrors.ReachedMaxErrors(): Boolean;
begin
  Result := CountErrors() >= FMaxErrors;
end;

procedure TErrors.Clear();
begin
  FItems.Clear();
end;

function TErrors.GetItems(): TList<TError>;
begin
  Result := FItems;
end;

function TErrors.GetMaxErrors(): Integer;
begin
  Result := FMaxErrors;
end;

procedure TErrors.SetMaxErrors(const AMaxErrors: Integer);
begin
  FMaxErrors := AMaxErrors;
end;

end.
