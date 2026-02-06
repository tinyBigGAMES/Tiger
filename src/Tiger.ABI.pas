{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.ABI;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Tiger.Common,
  Tiger.Backend;

type
  //============================================================================
  // TTigerABIMangler - Itanium C++ ABI name mangling implementation
  //============================================================================
  // Reference: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling
  //
  // Mangled names start with _Z followed by an encoding of the name and
  // signature. This implementation supports:
  //   - Simple function names: _Z<length><name><param-types>
  //   - Nested names (classes/namespaces): _ZN<components>E<param-types>
  //   - All primitive types used by Pax
  //   - Pointer types
  //   - Const qualification
  //============================================================================
  TTigerABIMangler = class(TTigerBaseObject)
  private
    class function EncodeName(const AName: string): string;
    class function EncodeNestedName(const AComponents: array of string): string;

  public
    //--------------------------------------------------------------------------
    // Primitive type mangling
    //--------------------------------------------------------------------------
    // Itanium ABI type codes:
    //   v  - void
    //   b  - bool
    //   a  - signed char (int8)
    //   h  - unsigned char (uint8)
    //   s  - short (int16)
    //   t  - unsigned short (uint16)
    //   i  - int (int32)
    //   j  - unsigned int (uint32)
    //   x  - long long (int64)
    //   y  - unsigned long long (uint64)
    //   f  - float (float32)
    //   d  - double (float64)
    //   P  - pointer (followed by pointee type)
    //   K  - const (prefix)
    //   R  - lvalue reference
    //   O  - rvalue reference
    //--------------------------------------------------------------------------
    class function ManglePrimitiveType(const AType: TTigerValueType): string;

    //--------------------------------------------------------------------------
    // Pointer type mangling
    //--------------------------------------------------------------------------
    // P<pointee-type>     - pointer to type
    // PKi                 - pointer to const int
    // Pv                  - pointer to void (untyped pointer)
    //--------------------------------------------------------------------------
    class function ManglePointerType(const APointeeType: TTigerValueType;
      const AIsConst: Boolean = False): string;

    //--------------------------------------------------------------------------
    // Function mangling
    //--------------------------------------------------------------------------
    // _Z<encoded-name><param-types>
    //
    // Examples:
    //   routine add(a: int32; b: int32): int32  →  _Z3addii
    //   routine foo()                           →  _Z3foov
    //   routine bar(x: pointer): pointer        →  _Z3barPv
    //--------------------------------------------------------------------------
    class function MangleFunction(
      const AName: string;
      const AParams: TArray<TTigerValueType>
    ): string;

    //--------------------------------------------------------------------------
    // Nested name mangling (for classes/namespaces)
    //--------------------------------------------------------------------------
    // _ZN<components>E<param-types>
    //
    // Examples:
    //   TPoint.GetX()                →  _ZN6TPoint4GetXEv
    //   Math.Utils.Add(int, int)     →  _ZN4Math5Utils3AddEii
    //--------------------------------------------------------------------------
    class function MangleNestedFunction(
      const AComponents: array of string;
      const AParams: TArray<TTigerValueType>
    ): string;

    //--------------------------------------------------------------------------
    // Full mangling with linkage check
    //--------------------------------------------------------------------------
    // Returns unmangled name if ALinkage = plC, otherwise mangles per Itanium ABI
    //--------------------------------------------------------------------------
    class function MangleFunctionWithLinkage(
      const AName: string;
      const AParams: TArray<TTigerValueType>;
      const ALinkage: TTigerLinkage
    ): string;

    class function MangleNestedFunctionWithLinkage(
      const AComponents: array of string;
      const AParams: TArray<TTigerValueType>;
      const ALinkage: TTigerLinkage
    ): string;

    //--------------------------------------------------------------------------
    // Demangling (for debugging/diagnostics)
    //--------------------------------------------------------------------------
    class function Demangle(const AMangled: string): string;
    class function IsMangled(const AName: string): Boolean;
  end;

implementation

//==============================================================================
// TTigerABIMangler - Private Helpers
//==============================================================================

class function TTigerABIMangler.EncodeName(const AName: string): string;
begin
  // <source-name> ::= <positive length number> <identifier>
  Result := IntToStr(Length(AName)) + AName;
end;

class function TTigerABIMangler.EncodeNestedName(const AComponents: array of string): string;
var
  LI: Integer;
begin
  // <nested-name> ::= N <prefix> <unqualified-name> E
  Result := 'N';
  for LI := Low(AComponents) to High(AComponents) do
    Result := Result + EncodeName(AComponents[LI]);
  Result := Result + 'E';
end;

//==============================================================================
// TTigerABIMangler - Type Mangling
//==============================================================================

class function TTigerABIMangler.ManglePrimitiveType(const AType: TTigerValueType): string;
begin
  case AType of
    vtVoid:    Result := 'v';
    vtInt8:    Result := 'a';   // signed char
    vtInt16:   Result := 's';   // short
    vtInt32:   Result := 'i';   // int
    vtInt64:   Result := 'x';   // long long
    vtUInt8:   Result := 'h';   // unsigned char
    vtUInt16:  Result := 't';   // unsigned short
    vtUInt32:  Result := 'j';   // unsigned int
    vtUInt64:  Result := 'y';   // unsigned long long
    vtFloat32: Result := 'f';   // float
    vtFloat64: Result := 'd';   // double
    vtPointer: Result := 'Pv';  // pointer to void
  else
    Result := 'v';  // Default to void for unknown types
  end;
end;

class function TTigerABIMangler.ManglePointerType(const APointeeType: TTigerValueType;
  const AIsConst: Boolean): string;
begin
  // P<type> for pointer, PK<type> for pointer to const
  if AIsConst then
    Result := 'PK' + ManglePrimitiveType(APointeeType)
  else
    Result := 'P' + ManglePrimitiveType(APointeeType);
end;

//==============================================================================
// TTigerABIMangler - Function Mangling
//==============================================================================

class function TTigerABIMangler.MangleFunction(
  const AName: string;
  const AParams: TArray<TTigerValueType>
): string;
var
  LI: Integer;
begin
  // _Z <encoded-name> <param-types>
  Result := '_Z' + EncodeName(AName);

  if Length(AParams) = 0 then
  begin
    // No parameters: use 'v' for void parameter list
    Result := Result + 'v';
  end
  else
  begin
    // Encode each parameter type
    for LI := 0 to High(AParams) do
      Result := Result + ManglePrimitiveType(AParams[LI]);
  end;
end;

class function TTigerABIMangler.MangleNestedFunction(
  const AComponents: array of string;
  const AParams: TArray<TTigerValueType>
): string;
var
  LI: Integer;
begin
  // _Z <nested-name> <param-types>
  Result := '_Z' + EncodeNestedName(AComponents);

  if Length(AParams) = 0 then
  begin
    Result := Result + 'v';
  end
  else
  begin
    for LI := 0 to High(AParams) do
      Result := Result + ManglePrimitiveType(AParams[LI]);
  end;
end;

class function TTigerABIMangler.MangleFunctionWithLinkage(
  const AName: string;
  const AParams: TArray<TTigerValueType>;
  const ALinkage: TTigerLinkage
): string;
begin
  case ALinkage of
    plC:
      // C linkage: no mangling, return name as-is
      Result := AName;
    plDefault:
      // Default: Itanium mangling
      Result := MangleFunction(AName, AParams);
  else
    Result := MangleFunction(AName, AParams);
  end;
end;

class function TTigerABIMangler.MangleNestedFunctionWithLinkage(
  const AComponents: array of string;
  const AParams: TArray<TTigerValueType>;
  const ALinkage: TTigerLinkage
): string;
begin
  case ALinkage of
    plC:
      begin
        // C linkage: no mangling, just use the last component (function name)
        if Length(AComponents) > 0 then
          Result := AComponents[High(AComponents)]
        else
          Result := '';
      end;
    plDefault:
      Result := MangleNestedFunction(AComponents, AParams);
  else
    Result := MangleNestedFunction(AComponents, AParams);
  end;
end;

//==============================================================================
// TTigerABIMangler - Demangling
//==============================================================================

class function TTigerABIMangler.IsMangled(const AName: string): Boolean;
begin
  Result := (Length(AName) >= 2) and (AName[1] = '_') and (AName[2] = 'Z');
end;

class function TTigerABIMangler.Demangle(const AMangled: string): string;
var
  LPos: Integer;
  LName: string;
  LParams: string;
  LComponents: TList<string>;
  LI: Integer;

  function ParseLength(var APos: Integer): Integer;
  var
    LStart: Integer;
  begin
    LStart := APos;
    while (APos <= Length(AMangled)) and (AMangled[APos] >= '0') and (AMangled[APos] <= '9') do
      Inc(APos);
    if APos = LStart then
      Result := 0
    else
      Result := StrToIntDef(Copy(AMangled, LStart, APos - LStart), 0);
  end;

  function ParseName(var APos: Integer): string;
  var
    LNameLen: Integer;
  begin
    LNameLen := ParseLength(APos);
    if LNameLen > 0 then
    begin
      Result := Copy(AMangled, APos, LNameLen);
      Inc(APos, LNameLen);
    end
    else
      Result := '';
  end;

  function DemangleType(var APos: Integer): string;
  var
    LIsPointer: Boolean;
    LIsConst: Boolean;
  begin
    Result := '';
    if APos > Length(AMangled) then
      Exit;

    LIsPointer := False;
    LIsConst := False;

    // Check for pointer prefix
    if AMangled[APos] = 'P' then
    begin
      LIsPointer := True;
      Inc(APos);
      if (APos <= Length(AMangled)) and (AMangled[APos] = 'K') then
      begin
        LIsConst := True;
        Inc(APos);
      end;
    end;

    if APos > Length(AMangled) then
      Exit;

    case AMangled[APos] of
      'v': Result := 'void';
      'a': Result := 'int8';
      'h': Result := 'uint8';
      's': Result := 'int16';
      't': Result := 'uint16';
      'i': Result := 'int32';
      'j': Result := 'uint32';
      'x': Result := 'int64';
      'y': Result := 'uint64';
      'f': Result := 'float32';
      'd': Result := 'float64';
      'b': Result := 'boolean';
    else
      Result := '?';
    end;
    Inc(APos);

    if LIsPointer then
    begin
      if LIsConst then
        Result := 'pointer to const ' + Result
      else
        Result := 'pointer to ' + Result;
    end;
  end;

begin
  // Check if it's a mangled name
  if not IsMangled(AMangled) then
  begin
    Result := AMangled;  // Return as-is if not mangled
    Exit;
  end;

  LComponents := TList<string>.Create();
  try
    LPos := 3;  // Skip '_Z'

    // Check for nested name
    if (LPos <= Length(AMangled)) and (AMangled[LPos] = 'N') then
    begin
      Inc(LPos);

      // Parse nested components until 'E'
      while (LPos <= Length(AMangled)) and (AMangled[LPos] <> 'E') do
      begin
        LName := ParseName(LPos);
        if LName <> '' then
          LComponents.Add(LName);
      end;

      // Skip 'E'
      if (LPos <= Length(AMangled)) and (AMangled[LPos] = 'E') then
        Inc(LPos);
    end
    else
    begin
      // Simple name
      LName := ParseName(LPos);
      if LName <> '' then
        LComponents.Add(LName);
    end;

    // Build the function name
    if LComponents.Count > 0 then
    begin
      Result := LComponents[0];
      for LI := 1 to LComponents.Count - 1 do
        Result := Result + '.' + LComponents[LI];
    end
    else
      Result := '?';

    // Parse parameter types
    LParams := '';
    while LPos <= Length(AMangled) do
    begin
      LName := DemangleType(LPos);
      if LName = 'void' then
      begin
        // void in parameter list means no parameters
        if LParams = '' then
          Break;
      end;
      if LParams <> '' then
        LParams := LParams + ', ';
      LParams := LParams + LName;
    end;

    Result := Result + '(' + LParams + ')';
  finally
    LComponents.Free();
  end;
end;

end.
