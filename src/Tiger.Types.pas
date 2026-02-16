{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Types;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Tiger.Common;

type
  //============================================================================
  // PUBLIC ENUMERATIONS
  //============================================================================

  { TTigerOutputType }
  TTigerOutputType = (
    otExe,
    otDll,
    otLib,
    otObj
  );

  { TTigerSubsystem }
  TTigerSubsystem = (
    ssConsole,
    ssGui
  );

  { TTigerValueType }
  TTigerValueType = (
    vtVoid,
    vtInt8,
    vtInt16,
    vtInt32,
    vtInt64,
    vtUInt8,
    vtUInt16,
    vtUInt32,
    vtUInt64,
    vtFloat32,
    vtFloat64,
    vtPointer
  );

  //============================================================================
  // HANDLE TYPES (Type-safe wrappers)
  //============================================================================

  { TTigerDataHandle }
  TTigerDataHandle = record
    Index: Integer;
    class function Invalid(): TTigerDataHandle; static;
    function IsValid(): Boolean;
    class operator Equal(const A, B: TTigerDataHandle): Boolean;
    class operator NotEqual(const A, B: TTigerDataHandle): Boolean;
  end;

  { TTigerImportHandle }
  TTigerImportHandle = record
    Index: Integer;
    class function Invalid(): TTigerImportHandle; static;
    function IsValid(): Boolean;
    class operator Equal(const A, B: TTigerImportHandle): Boolean;
    class operator NotEqual(const A, B: TTigerImportHandle): Boolean;
  end;

  { TTigerFuncHandle }
  TTigerFuncHandle = record
    Index: Integer;
    class function Invalid(): TTigerFuncHandle; static;
    function IsValid(): Boolean;
    class operator Equal(const A, B: TTigerFuncHandle): Boolean;
    class operator NotEqual(const A, B: TTigerFuncHandle): Boolean;
  end;

  { TTigerLocalHandle }
  TTigerLocalHandle = record
    Index: Integer;
    IsParam: Boolean;
    class function Invalid(): TTigerLocalHandle; static;
    function IsValid(): Boolean;
    class operator Equal(const A, B: TTigerLocalHandle): Boolean;
    class operator NotEqual(const A, B: TTigerLocalHandle): Boolean;
  end;

  { TTigerLabelHandle }
  TTigerLabelHandle = record
    Index: Integer;
    class function Invalid(): TTigerLabelHandle; static;
    function IsValid(): Boolean;
    class operator Equal(const A, B: TTigerLabelHandle): Boolean;
    class operator NotEqual(const A, B: TTigerLabelHandle): Boolean;
  end;

  { TTigerTempHandle }
  TTigerTempHandle = record
    Index: Integer;
    class function Invalid(): TTigerTempHandle; static;
    function IsValid(): Boolean;
    class operator Equal(const A, B: TTigerTempHandle): Boolean;
    class operator NotEqual(const A, B: TTigerTempHandle): Boolean;
  end;

  //============================================================================
  // OPERAND TYPE
  //============================================================================

  { TTigerOperandKind }
  TTigerOperandKind = (
    okNone,
    okImmediate,
    okData,
    okGlobal,
    okImport,
    okLocal,
    okTemp,
    okFunc
  );

  { TTigerOperand }
  TTigerOperand = record
    Kind: TTigerOperandKind;
    ValueType: TTigerValueType;
    ImmInt: Int64;
    ImmFloat: Double;
    DataHandle: TTigerDataHandle;
    ImportHandle: TTigerImportHandle;
    LocalHandle: TTigerLocalHandle;
    TempHandle: TTigerTempHandle;
    FuncHandle: TTigerFuncHandle;

    class function None(): TTigerOperand; static;
    class function FromImm(const AValue: Int64): TTigerOperand; overload; static;
    class function FromImm(const AValue: Double): TTigerOperand; overload; static;
    class function FromData(const AHandle: TTigerDataHandle): TTigerOperand; static;
    class function FromGlobal(const AHandle: TTigerDataHandle): TTigerOperand; static;
    class function FromImport(const AHandle: TTigerImportHandle): TTigerOperand; static;
    class function FromLocal(const AHandle: TTigerLocalHandle): TTigerOperand; static;
    class function FromTemp(const AHandle: TTigerTempHandle): TTigerOperand; static;
    class function FromFunc(const AHandle: TTigerFuncHandle): TTigerOperand; static;

    class operator Implicit(const AValue: Integer): TTigerOperand;
    class operator Implicit(const AValue: Int64): TTigerOperand;
    class operator Implicit(const AValue: Cardinal): TTigerOperand;
    class operator Implicit(const AValue: UInt64): TTigerOperand;
    class operator Implicit(const AValue: Single): TTigerOperand;
    class operator Implicit(const AValue: Double): TTigerOperand;
    class operator Implicit(const AValue: TTigerDataHandle): TTigerOperand;
    class operator Implicit(const AValue: TTigerImportHandle): TTigerOperand;
    class operator Implicit(const AValue: TTigerLocalHandle): TTigerOperand;
    class operator Implicit(const AValue: TTigerTempHandle): TTigerOperand;
    class operator Implicit(const AValue: TTigerFuncHandle): TTigerOperand;
  end;

  //============================================================================
  // FORWARD DECLARATIONS
  //============================================================================


  //============================================================================
  // INTERNAL DATA STRUCTURES
  //============================================================================

  { TTigerDataEntry }
  TTigerDataEntry = record
    Offset: Cardinal;
    Size: Cardinal;
    DataType: TTigerValueType;
  end;

  { TTigerImportEntry }
  TTigerImportEntry = record
    DllName: string;
    FuncName: string;
    IATOffset: Cardinal;
    ReturnType: TTigerValueType;
    IsStatic: Boolean;
    IsVariadic: Boolean;   // True for printf etc.; on Darwin ARM64, varargs go on stack
  end;

  { TTigerExportEntry }
  TTigerExportEntry = record
    FuncName: string;
    ExportName: string;
    FuncIndex: Integer;
  end;

  { TTigerParamInfo }
  TTigerParamInfo = record
    ParamName: string;
    ParamType: TTigerValueType;
    ParamSize: Integer;       // Size in bytes (for composite types)
    ParamAlignment: Integer;  // Alignment in bytes (for ABI classification)
  end;

  { TTigerLocalInfo }
  TTigerLocalInfo = record
    LocalName: string;
    LocalType: TTigerValueType;
    LocalSize: Integer;      // Size in bytes (for composite types)
    LocalAlignment: Integer;  // Alignment in bytes (for ABI classification)
    StackOffset: Integer;
  end;

  { TTigerLabelInfo }
  TTigerLabelInfo = record
    LabelName: string;
    CodeOffset: Integer;
    IsDefined: Boolean;
  end;

  { TTigerInstrKind }
  TTigerInstrKind = (
    ikNop,
    ikCall,
    ikCallImport,
    ikCallIndirect,
    ikReturn,
    ikReturnValue,
    ikStore,
    ikLoad,
    ikStorePtr,
    ikLoadPtr,
    ikAddressOf,
    ikAdd,
    ikSub,
    ikMul,
    ikDiv,
    ikMod,
    ikBitAnd,
    ikBitOr,
    ikBitXor,
    ikBitNot,
    ikShl,
    ikShr,
    ikCmpEq,
    ikCmpNe,
    ikCmpLt,
    ikCmpLe,
    ikCmpGt,
    ikCmpGe,
    ikJump,
    ikJumpIf,
    ikJumpIfNot,
    ikVaCount,       // dest := VaCount_()
    ikVaArgAt,       // dest := VaArgAt_(index, type)
    ikSyscall,       // syscall(nr, args...) — Linux syscall
    ikLabel
  );

  { TTigerInstruction }
  TTigerInstruction = record
    Kind: TTigerInstrKind;
    Dest: TTigerTempHandle;
    Op1: TTigerOperand;
    Op2: TTigerOperand;
    LabelTarget: TTigerLabelHandle;
    FuncTarget: TTigerFuncHandle;
    ImportTarget: TTigerImportHandle;
    Args: TArray<TTigerOperand>;
    SyscallNr: Integer;             // Linux syscall number (for ikSyscall)
  end;

  { TTigerExceptionScope }
  // Tracks a try/except/finally region for SEH SCOPE_TABLE generation
  TTigerExceptionScope = record
    TryBeginLabel: TTigerLabelHandle;     // Start of try block
    TryEndLabel: TTigerLabelHandle;       // End of try block (before except/finally)
    ExceptLabel: TTigerLabelHandle;       // Start of except handler (-1 if none)
    FinallyLabel: TTigerLabelHandle;      // Start of finally handler (-1 if none)
    EndLabel: TTigerLabelHandle;          // End of entire construct
  end;

  { TTigerFuncInfo }
  TTigerFuncInfo = record
    FuncName: string;
    IsEntryPoint: Boolean;
    IsDllEntry: Boolean;
    IsPublic: Boolean;
    Linkage: TTigerLinkage;
    ExportName: string;           // Name to use in export table (computed from linkage)
    ReturnType: TTigerValueType;
    ReturnSize: Integer;          // Size in bytes (for composite return types)
    ReturnAlignment: Integer;     // Alignment in bytes (for ABI classification)
    Params: TArray<TTigerParamInfo>;
    Locals: TArray<TTigerLocalInfo>;
    Labels: TArray<TTigerLabelInfo>;
    Instructions: TArray<TTigerInstruction>;
    TempCount: Integer;
    ExceptionScopes: TArray<TTigerExceptionScope>;  // SEH regions for this function
    IsVariadic: Boolean;  // True if function accepts variadic arguments
  end;

  //============================================================================
  // TTigerDataBuilder
  //============================================================================


implementation

//==============================================================================

{ TTigerDataHandle }

class function TTigerDataHandle.Invalid(): TTigerDataHandle;
begin
  Result.Index := -1;
end;

function TTigerDataHandle.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerDataHandle.Equal(const A, B: TTigerDataHandle): Boolean;
begin
  Result := A.Index = B.Index;
end;

class operator TTigerDataHandle.NotEqual(const A, B: TTigerDataHandle): Boolean;
begin
  Result := A.Index <> B.Index;
end;

{ TTigerImportHandle }

class function TTigerImportHandle.Invalid(): TTigerImportHandle;
begin
  Result.Index := -1;
end;

function TTigerImportHandle.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerImportHandle.Equal(const A, B: TTigerImportHandle): Boolean;
begin
  Result := A.Index = B.Index;
end;

class operator TTigerImportHandle.NotEqual(const A, B: TTigerImportHandle): Boolean;
begin
  Result := A.Index <> B.Index;
end;

{ TTigerFuncHandle }

class function TTigerFuncHandle.Invalid(): TTigerFuncHandle;
begin
  Result.Index := -1;
end;

function TTigerFuncHandle.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerFuncHandle.Equal(const A, B: TTigerFuncHandle): Boolean;
begin
  Result := A.Index = B.Index;
end;

class operator TTigerFuncHandle.NotEqual(const A, B: TTigerFuncHandle): Boolean;
begin
  Result := A.Index <> B.Index;
end;

{ TTigerLocalHandle }

class function TTigerLocalHandle.Invalid(): TTigerLocalHandle;
begin
  Result.Index := -1;
  Result.IsParam := False;
end;

function TTigerLocalHandle.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerLocalHandle.Equal(const A, B: TTigerLocalHandle): Boolean;
begin
  Result := (A.Index = B.Index) and (A.IsParam = B.IsParam);
end;

class operator TTigerLocalHandle.NotEqual(const A, B: TTigerLocalHandle): Boolean;
begin
  Result := (A.Index <> B.Index) or (A.IsParam <> B.IsParam);
end;

{ TTigerLabelHandle }

class function TTigerLabelHandle.Invalid(): TTigerLabelHandle;
begin
  Result.Index := -1;
end;

function TTigerLabelHandle.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerLabelHandle.Equal(const A, B: TTigerLabelHandle): Boolean;
begin
  Result := A.Index = B.Index;
end;

class operator TTigerLabelHandle.NotEqual(const A, B: TTigerLabelHandle): Boolean;
begin
  Result := A.Index <> B.Index;
end;

{ TTigerTempHandle }

class function TTigerTempHandle.Invalid(): TTigerTempHandle;
begin
  Result.Index := -1;
end;

function TTigerTempHandle.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerTempHandle.Equal(const A, B: TTigerTempHandle): Boolean;
begin
  Result := A.Index = B.Index;
end;

class operator TTigerTempHandle.NotEqual(const A, B: TTigerTempHandle): Boolean;
begin
  Result := A.Index <> B.Index;
end;

//==============================================================================
// TTigerOperand Implementation
//==============================================================================

class function TTigerOperand.None(): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okNone;
end;

class function TTigerOperand.FromImm(const AValue: Int64): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okImmediate;
  Result.ValueType := vtInt64;
  Result.ImmInt := AValue;
end;

class function TTigerOperand.FromImm(const AValue: Double): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okImmediate;
  Result.ValueType := vtFloat64;
  Result.ImmFloat := AValue;
end;

class function TTigerOperand.FromData(const AHandle: TTigerDataHandle): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okData;
  Result.ValueType := vtPointer;
  Result.DataHandle := AHandle;
end;

class function TTigerOperand.FromGlobal(const AHandle: TTigerDataHandle): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okGlobal;
  Result.ValueType := vtPointer;
  Result.DataHandle := AHandle;
end;

class function TTigerOperand.FromImport(const AHandle: TTigerImportHandle): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okImport;
  Result.ValueType := vtPointer;
  Result.ImportHandle := AHandle;
end;

class function TTigerOperand.FromLocal(const AHandle: TTigerLocalHandle): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okLocal;
  Result.LocalHandle := AHandle;
end;

class function TTigerOperand.FromTemp(const AHandle: TTigerTempHandle): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okTemp;
  Result.TempHandle := AHandle;
end;

class function TTigerOperand.FromFunc(const AHandle: TTigerFuncHandle): TTigerOperand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := okFunc;
  Result.ValueType := vtPointer;
  Result.FuncHandle := AHandle;
end;

class operator TTigerOperand.Implicit(const AValue: Integer): TTigerOperand;
begin
  Result := FromImm(Int64(AValue));
  Result.ValueType := vtInt32;
end;

class operator TTigerOperand.Implicit(const AValue: Int64): TTigerOperand;
begin
  Result := FromImm(AValue);
end;

class operator TTigerOperand.Implicit(const AValue: Cardinal): TTigerOperand;
begin
  Result := FromImm(Int64(AValue));
  Result.ValueType := vtUInt32;
end;

class operator TTigerOperand.Implicit(const AValue: UInt64): TTigerOperand;
begin
  Result := FromImm(Int64(AValue));
  Result.ValueType := vtUInt64;
end;

class operator TTigerOperand.Implicit(const AValue: Single): TTigerOperand;
begin
  Result := FromImm(Double(AValue));
  Result.ValueType := vtFloat32;
end;

class operator TTigerOperand.Implicit(const AValue: Double): TTigerOperand;
begin
  Result := FromImm(AValue);
end;

class operator TTigerOperand.Implicit(const AValue: TTigerDataHandle): TTigerOperand;
begin
  Result := FromData(AValue);
end;

class operator TTigerOperand.Implicit(const AValue: TTigerImportHandle): TTigerOperand;
begin
  Result := FromImport(AValue);
end;

class operator TTigerOperand.Implicit(const AValue: TTigerLocalHandle): TTigerOperand;
begin
  Result := FromLocal(AValue);
end;

class operator TTigerOperand.Implicit(const AValue: TTigerTempHandle): TTigerOperand;
begin
  Result := FromTemp(AValue);
end;

class operator TTigerOperand.Implicit(const AValue: TTigerFuncHandle): TTigerOperand;
begin
  Result := FromFunc(AValue);
end;


end.
