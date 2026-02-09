{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Builders;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types;

type
  // Forward declarations
  TTigerCodeBuilder = class;

  { TTigerDataBuilder }
  TTigerDataBuilder = class(TBaseObject)
  private
    FEntries: TList<TTigerDataEntry>;
    FData: TMemoryStream;

  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;

    function AddString(const AValue: string): TTigerDataHandle;
    function AddStringA(const AValue: AnsiString): TTigerDataHandle;
    function AddStringW(const AValue: string): TTigerDataHandle;
    function AddBytes(const AData: TBytes): TTigerDataHandle;
    function AddInt8(const AValue: Int8): TTigerDataHandle;
    function AddInt16(const AValue: Int16): TTigerDataHandle;
    function AddInt32(const AValue: Int32): TTigerDataHandle;
    function AddInt64(const AValue: Int64): TTigerDataHandle;
    function AddUInt8(const AValue: UInt8): TTigerDataHandle;
    function AddUInt16(const AValue: UInt16): TTigerDataHandle;
    function AddUInt32(const AValue: UInt32): TTigerDataHandle;
    function AddUInt64(const AValue: UInt64): TTigerDataHandle;
    function AddFloat32(const AValue: Single): TTigerDataHandle;
    function AddFloat64(const AValue: Double): TTigerDataHandle;
    function AddPointer(const ATarget: TTigerDataHandle): TTigerDataHandle;
    function Reserve(const ASize: Integer; const AAlign: Integer = 8): TTigerDataHandle;

    procedure Align(const AAlignment: Integer);
    procedure Clear();

    function GetEntry(const AHandle: TTigerDataHandle): TTigerDataEntry;
    function GetData(): TBytes;
    function GetDataPointer(): Pointer;
    function GetSize(): Integer;
  end;

  //============================================================================
  // TTigerImportBuilder
  //============================================================================

  { TTigerImportBuilder }
  TTigerImportBuilder = class(TBaseObject)
  private
    FEntries: TList<TTigerImportEntry>;

  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;

    function Add(const ADllName, AFuncName: string): TTigerImportHandle; overload;
    function Add(const ADllName, AFuncName: string; const AReturnType: TTigerValueType): TTigerImportHandle; overload;
    function Add(const ADllName, AFuncName: string; const AReturnType: TTigerValueType;
      const AIsStatic: Boolean): TTigerImportHandle; overload;

    procedure Clear();
    function GetEntry(const AHandle: TTigerImportHandle): TTigerImportEntry;
    function GetCount(): Integer;
    function GetEntryByIndex(const AIndex: Integer): TTigerImportEntry;
  end;

  //============================================================================
  // TTigerExportBuilder
  //============================================================================

  { TTigerExportBuilder }
  TTigerExportBuilder = class(TBaseObject)
  private
    FCodeBuilder: TTigerCodeBuilder;
    FEntries: TList<TTigerExportEntry>;

  public
    constructor Create(const ACodeBuilder: TTigerCodeBuilder); reintroduce;
    destructor Destroy(); override;

    procedure Add(const AFuncHandle: TTigerFuncHandle); overload;
    procedure Add(const AFuncHandle: TTigerFuncHandle; const AExportName: string); overload;

    procedure Clear();
    function GetCount(): Integer;
    function GetEntryByIndex(const AIndex: Integer): TTigerExportEntry;
  end;

  //============================================================================
  // TTigerCodeBuilder (Fluent Interface)
  //============================================================================

  { TTigerCodeBuilder }
  TTigerCodeBuilder = class(TTigerBaseObject)
  private
    FFunctions: TList<TTigerFuncInfo>;
    FCurrentFunc: Integer;

    function GetCurrentFunc(): TTigerFuncInfo;
    procedure SetCurrentFunc(const AFunc: TTigerFuncInfo);
    function AllocTemp(): TTigerTempHandle;
    procedure AddInstr(const AInstr: TTigerInstruction);

  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Function Management
    //--------------------------------------------------------------------------
    function BeginProc(
      const AName: string;
      const AIsEntryPoint: Boolean = False;
      const AIsDllEntry: Boolean = False;
      const AIsPublic: Boolean = False;
      const ALinkage: TTigerLinkage = plDefault
    ): TTigerCodeBuilder;
    function SetReturnType(const AType: TTigerValueType): TTigerCodeBuilder; overload;
    function SetReturnType(const AType: TTigerValueType; const ASize: Integer; const AAlignment: Integer): TTigerCodeBuilder; overload;
    function SetIsVariadic(const AValue: Boolean): TTigerCodeBuilder;
    function EndProc(): TTigerCodeBuilder;

    //--------------------------------------------------------------------------
    // Parameters and Locals
    //--------------------------------------------------------------------------
    function AddParam(const AName: string; const AType: TTigerValueType): TTigerLocalHandle; overload;
    function AddParam(const AName: string; const ASize: Integer; const AAlignment: Integer): TTigerLocalHandle; overload;
    function AddLocal(const AName: string; const AType: TTigerValueType): TTigerLocalHandle; overload;
    function AddLocal(const AName: string; const ASize: Integer): TTigerLocalHandle; overload;
    function AddLocal(const AName: string; const ASize: Integer; const AAlignment: Integer): TTigerLocalHandle; overload;

    //--------------------------------------------------------------------------
    // Labels
    //--------------------------------------------------------------------------
    function DefineLabel(const AName: string = ''): TTigerLabelHandle;
    function MarkLabel(const ALabel: TTigerLabelHandle): TTigerCodeBuilder;

    //--------------------------------------------------------------------------
    // Exception Scopes (for SEH)
    //--------------------------------------------------------------------------
    procedure AddExceptionScope(
      const ATryBegin: TTigerLabelHandle;
      const ATryEnd: TTigerLabelHandle;
      const AExcept: TTigerLabelHandle;
      const AFinally: TTigerLabelHandle;
      const AEnd: TTigerLabelHandle
    );

    //--------------------------------------------------------------------------
    // Function Calls
    //--------------------------------------------------------------------------
    function Call(const AImport: TTigerImportHandle): TTigerCodeBuilder; overload;
    function Call(const AImport: TTigerImportHandle; const AArgs: array of TTigerOperand): TTigerCodeBuilder; overload;
    function Call(const AFunc: TTigerFuncHandle): TTigerCodeBuilder; overload;
    function Call(const AFunc: TTigerFuncHandle; const AArgs: array of TTigerOperand): TTigerCodeBuilder; overload;

    function CallFunc(const AImport: TTigerImportHandle): TTigerTempHandle; overload;
    function CallFunc(const AImport: TTigerImportHandle; const AArgs: array of TTigerOperand): TTigerTempHandle; overload;
    function CallFunc(const AFunc: TTigerFuncHandle): TTigerTempHandle; overload;
    function CallFunc(const AFunc: TTigerFuncHandle; const AArgs: array of TTigerOperand): TTigerTempHandle; overload;

    // Indirect calls (through function pointer)
    function CallIndirect(const AFuncPtr: TTigerOperand): TTigerCodeBuilder; overload;
    function CallIndirect(const AFuncPtr: TTigerOperand; const AArgs: array of TTigerOperand): TTigerCodeBuilder; overload;
    function CallIndirectFunc(const AFuncPtr: TTigerOperand): TTigerTempHandle; overload;
    function CallIndirectFunc(const AFuncPtr: TTigerOperand; const AArgs: array of TTigerOperand): TTigerTempHandle; overload;

    // Function address
    function LoadFuncAddr(const AFuncIndex: Integer): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Return
    //--------------------------------------------------------------------------
    function Return(): TTigerCodeBuilder; overload;
    function Return(const AValue: TTigerOperand): TTigerCodeBuilder; overload;

    //--------------------------------------------------------------------------
    // Arithmetic
    //--------------------------------------------------------------------------
    function OpAdd(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSub(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpMul(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpDiv(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpMod(const ALeft, ARight: TTigerOperand): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Bitwise
    //--------------------------------------------------------------------------
    function OpAnd(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpOr(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpXor(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpNot(const AValue: TTigerOperand): TTigerTempHandle;
    function OpShl(const AValue, ACount: TTigerOperand): TTigerTempHandle;
    function OpShr(const AValue, ACount: TTigerOperand): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Comparison
    //--------------------------------------------------------------------------
    function CmpEq(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function CmpNe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function CmpLt(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function CmpLe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function CmpGt(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function CmpGe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Set Operations
    //--------------------------------------------------------------------------
    function OpSetLiteral(const AElements: array of Integer; const ALowBound: Integer; const AStorageSize: Integer): TTigerTempHandle;
    function OpSetUnion(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSetDiff(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSetInter(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSetIn(const AElement, ASet: TTigerOperand; const ALowBound: Integer): TTigerTempHandle;
    function OpSetEq(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSetNe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSetSubset(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
    function OpSetSuperset(const ALeft, ARight: TTigerOperand): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Variadic Intrinsics
    //--------------------------------------------------------------------------
    function VaCount(): TTigerTempHandle;
    function VaArgAt(const AIndex: TTigerOperand; const AType: TTigerValueType): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Syscall (Linux)
    //--------------------------------------------------------------------------
    function EmitSyscall(const ANr: Integer; const AArgs: array of TTigerOperand): TTigerCodeBuilder;
    function EmitSyscallFunc(const ANr: Integer; const AArgs: array of TTigerOperand): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Memory Operations
    //--------------------------------------------------------------------------
    function Store(const ADest: TTigerLocalHandle; const AValue: TTigerOperand): TTigerCodeBuilder;
    function Load(const ASrc: TTigerLocalHandle): TTigerTempHandle;
    function StorePtr(const APtr, AValue: TTigerOperand): TTigerCodeBuilder;
    function LoadPtr(const APtr: TTigerOperand): TTigerTempHandle;
    function AddressOf(const ALocal: TTigerLocalHandle): TTigerTempHandle;

    //--------------------------------------------------------------------------
    // Control Flow
    //--------------------------------------------------------------------------
    function Jump(const ALabel: TTigerLabelHandle): TTigerCodeBuilder;
    function JumpIf(const ACond: TTigerOperand; const ALabel: TTigerLabelHandle): TTigerCodeBuilder;
    function JumpIfNot(const ACond: TTigerOperand; const ALabel: TTigerLabelHandle): TTigerCodeBuilder;

    //--------------------------------------------------------------------------
    // Query
    //--------------------------------------------------------------------------
    function GetFuncCount(): Integer;
    function GetFunc(const AIndex: Integer): TTigerFuncInfo;
    function GetFuncHandle(const AName: string): TTigerFuncHandle;
    function GetCurrentFuncHandle(): TTigerFuncHandle;

    procedure Clear();
  end;

  //============================================================================
  // TTigerBackend (Main Facade)
  //============================================================================


implementation

const
  ERR_BACKEND_NO_ACTIVE_FUNC = 'B001';

//==============================================================================
// TTigerDataBuilder Implementation
//==============================================================================

constructor TTigerDataBuilder.Create();
begin
  inherited Create();

  FEntries := TList<TTigerDataEntry>.Create();
  FData := TMemoryStream.Create();
end;

destructor TTigerDataBuilder.Destroy();
begin
  FData.Free();
  FEntries.Free();

  inherited;
end;

function TTigerDataBuilder.AddString(const AValue: string): TTigerDataHandle;
begin
  Result := AddStringA(UTF8Encode(AValue));
end;

function TTigerDataBuilder.AddStringA(const AValue: AnsiString): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  LEntry.Offset := FData.Size;
  LEntry.Size := Length(AValue) + 1;
  LEntry.DataType := vtPointer;

  if Length(AValue) > 0 then
    FData.WriteBuffer(AValue[1], Length(AValue));
  FData.WriteData(Byte(0));

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddStringW(const AValue: string): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
  LI: Integer;
  LChar: Word;
begin
  LEntry.Offset := FData.Size;
  LEntry.Size := (Length(AValue) + 1) * 2;
  LEntry.DataType := vtPointer;

  for LI := 1 to Length(AValue) do
  begin
    LChar := Word(AValue[LI]);
    FData.WriteData(LChar);
  end;
  LChar := 0;
  FData.WriteData(LChar);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddBytes(const AData: TBytes): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  LEntry.Offset := FData.Size;
  LEntry.Size := Length(AData);
  LEntry.DataType := vtPointer;

  if Length(AData) > 0 then
    FData.WriteBuffer(AData[0], Length(AData));

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddInt8(const AValue: Int8): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  LEntry.Offset := FData.Size;
  LEntry.Size := 1;
  LEntry.DataType := vtInt8;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddInt16(const AValue: Int16): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(2);
  LEntry.Offset := FData.Size;
  LEntry.Size := 2;
  LEntry.DataType := vtInt16;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddInt32(const AValue: Int32): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(4);
  LEntry.Offset := FData.Size;
  LEntry.Size := 4;
  LEntry.DataType := vtInt32;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddInt64(const AValue: Int64): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(8);
  LEntry.Offset := FData.Size;
  LEntry.Size := 8;
  LEntry.DataType := vtInt64;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddUInt8(const AValue: UInt8): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  LEntry.Offset := FData.Size;
  LEntry.Size := 1;
  LEntry.DataType := vtUInt8;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddUInt16(const AValue: UInt16): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(2);
  LEntry.Offset := FData.Size;
  LEntry.Size := 2;
  LEntry.DataType := vtUInt16;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddUInt32(const AValue: UInt32): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(4);
  LEntry.Offset := FData.Size;
  LEntry.Size := 4;
  LEntry.DataType := vtUInt32;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddUInt64(const AValue: UInt64): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(8);
  LEntry.Offset := FData.Size;
  LEntry.Size := 8;
  LEntry.DataType := vtUInt64;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddFloat32(const AValue: Single): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(4);
  LEntry.Offset := FData.Size;
  LEntry.Size := 4;
  LEntry.DataType := vtFloat32;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddFloat64(const AValue: Double): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
begin
  Align(8);
  LEntry.Offset := FData.Size;
  LEntry.Size := 8;
  LEntry.DataType := vtFloat64;
  FData.WriteData(AValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.AddPointer(const ATarget: TTigerDataHandle): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
  LTargetEntry: TTigerDataEntry;
  LValue: UInt64;
begin
  Align(8);
  LEntry.Offset := FData.Size;
  LEntry.Size := 8;
  LEntry.DataType := vtPointer;

  // Store the offset for now - will be fixed up during build
  LTargetEntry := GetEntry(ATarget);
  LValue := LTargetEntry.Offset;
  FData.WriteData(LValue);

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

function TTigerDataBuilder.Reserve(const ASize: Integer; const AAlign: Integer): TTigerDataHandle;
var
  LEntry: TTigerDataEntry;
  LI: Integer;
begin
  if AAlign > 1 then
    Align(AAlign);

  LEntry.Offset := FData.Size;
  LEntry.Size := ASize;
  LEntry.DataType := vtPointer;

  for LI := 1 to ASize do
    FData.WriteData(Byte(0));

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

procedure TTigerDataBuilder.Align(const AAlignment: Integer);
var
  LPadding: Integer;
  LI: Integer;
begin
  if AAlignment <= 1 then
    Exit;

  LPadding := FData.Size mod AAlignment;
  if LPadding > 0 then
  begin
    LPadding := AAlignment - LPadding;
    for LI := 1 to LPadding do
      FData.WriteData(Byte(0));
  end;
end;

procedure TTigerDataBuilder.Clear();
begin
  FEntries.Clear();
  FData.Clear();
end;

function TTigerDataBuilder.GetEntry(const AHandle: TTigerDataHandle): TTigerDataEntry;
begin
  if (AHandle.Index >= 0) and (AHandle.Index < FEntries.Count) then
    Result := FEntries[AHandle.Index]
  else
  begin
    Result.Offset := 0;
    Result.Size := 0;
    Result.DataType := vtVoid;
  end;
end;

function TTigerDataBuilder.GetData(): TBytes;
begin
  SetLength(Result, FData.Size);
  if FData.Size > 0 then
  begin
    FData.Position := 0;
    FData.ReadBuffer(Result[0], FData.Size);
  end;
end;

function TTigerDataBuilder.GetDataPointer(): Pointer;
begin
  Result := FData.Memory;
end;

function TTigerDataBuilder.GetSize(): Integer;
begin
  Result := FData.Size;
end;

//==============================================================================
// TTigerImportBuilder Implementation
//==============================================================================

constructor TTigerImportBuilder.Create();
begin
  inherited Create();

  FEntries := TList<TTigerImportEntry>.Create();
end;

destructor TTigerImportBuilder.Destroy();
begin
  FEntries.Free();

  inherited;
end;

function TTigerImportBuilder.Add(const ADllName, AFuncName: string): TTigerImportHandle;
begin
  Result := Add(ADllName, AFuncName, vtVoid);
end;

function TTigerImportBuilder.Add(const ADllName, AFuncName: string; const AReturnType: TTigerValueType): TTigerImportHandle;
begin
  Result := Add(ADllName, AFuncName, AReturnType, False);
end;

function TTigerImportBuilder.Add(const ADllName, AFuncName: string;
  const AReturnType: TTigerValueType; const AIsStatic: Boolean): TTigerImportHandle;
var
  LEntry: TTigerImportEntry;
begin
  LEntry.DllName := ADllName;
  LEntry.FuncName := AFuncName;
  LEntry.IATOffset := 0;
  LEntry.ReturnType := AReturnType;
  LEntry.IsStatic := AIsStatic;

  Result.Index := FEntries.Count;
  FEntries.Add(LEntry);
end;

procedure TTigerImportBuilder.Clear();
begin
  FEntries.Clear();
end;

function TTigerImportBuilder.GetEntry(const AHandle: TTigerImportHandle): TTigerImportEntry;
begin
  if (AHandle.Index >= 0) and (AHandle.Index < FEntries.Count) then
    Result := FEntries[AHandle.Index]
  else
  begin
    Result.DllName := '';
    Result.FuncName := '';
    Result.IATOffset := 0;
    Result.ReturnType := vtVoid;
  end;
end;

function TTigerImportBuilder.GetCount(): Integer;
begin
  Result := FEntries.Count;
end;

function TTigerImportBuilder.GetEntryByIndex(const AIndex: Integer): TTigerImportEntry;
begin
  Result := FEntries[AIndex];
end;

//==============================================================================
// TTigerExportBuilder Implementation
//==============================================================================

constructor TTigerExportBuilder.Create(const ACodeBuilder: TTigerCodeBuilder);
begin
  inherited Create();
  FCodeBuilder := ACodeBuilder;

  FEntries := TList<TTigerExportEntry>.Create();
end;

destructor TTigerExportBuilder.Destroy();
begin
  FEntries.Free();

  inherited;
end;

procedure TTigerExportBuilder.Add(const AFuncHandle: TTigerFuncHandle);
var
  LEntry: TTigerExportEntry;
  LFunc: TTigerFuncInfo;
begin
  LFunc := FCodeBuilder.GetFunc(AFuncHandle.Index);
  LEntry.FuncName := LFunc.FuncName;
  LEntry.ExportName := LFunc.FuncName;
  LEntry.FuncIndex := AFuncHandle.Index;
  FEntries.Add(LEntry);
end;

procedure TTigerExportBuilder.Add(const AFuncHandle: TTigerFuncHandle; const AExportName: string);
var
  LEntry: TTigerExportEntry;
  LFunc: TTigerFuncInfo;
begin
  LFunc := FCodeBuilder.GetFunc(AFuncHandle.Index);
  LEntry.FuncName := LFunc.FuncName;
  LEntry.ExportName := AExportName;
  LEntry.FuncIndex := AFuncHandle.Index;
  FEntries.Add(LEntry);
end;

procedure TTigerExportBuilder.Clear();
begin
  FEntries.Clear();
end;

function TTigerExportBuilder.GetCount(): Integer;
begin
  Result := FEntries.Count;
end;

function TTigerExportBuilder.GetEntryByIndex(const AIndex: Integer): TTigerExportEntry;
begin
  Result := FEntries[AIndex];
end;

//==============================================================================
// TTigerCodeBuilder Implementation
//==============================================================================

constructor TTigerCodeBuilder.Create();
begin
  inherited Create();

  FFunctions := TList<TTigerFuncInfo>.Create();
  FCurrentFunc := -1;
end;

destructor TTigerCodeBuilder.Destroy();
begin
  FFunctions.Free();

  inherited;
end;

function TTigerCodeBuilder.GetCurrentFunc(): TTigerFuncInfo;
begin
  if (FCurrentFunc >= 0) and (FCurrentFunc < FFunctions.Count) then
    Result := FFunctions[FCurrentFunc]
  else
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_BACKEND_NO_ACTIVE_FUNC, 'No active function');
    Result := Default(TTigerFuncInfo);
    Exit;
  end;
end;

procedure TTigerCodeBuilder.SetCurrentFunc(const AFunc: TTigerFuncInfo);
begin
  if (FCurrentFunc >= 0) and (FCurrentFunc < FFunctions.Count) then
    FFunctions[FCurrentFunc] := AFunc
  else
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_BACKEND_NO_ACTIVE_FUNC, 'No active function');
    Exit;
  end;
end;

function TTigerCodeBuilder.AllocTemp(): TTigerTempHandle;
var
  LFunc: TTigerFuncInfo;
begin
  LFunc := GetCurrentFunc();
  Result.Index := LFunc.TempCount;
  Inc(LFunc.TempCount);
  SetCurrentFunc(LFunc);
end;

procedure TTigerCodeBuilder.AddInstr(const AInstr: TTigerInstruction);
var
  LFunc: TTigerFuncInfo;
  LLen: Integer;
begin
  LFunc := GetCurrentFunc();
  LLen := Length(LFunc.Instructions);
  SetLength(LFunc.Instructions, LLen + 1);
  LFunc.Instructions[LLen] := AInstr;
  SetCurrentFunc(LFunc);
end;

function TTigerCodeBuilder.BeginProc(
  const AName: string;
  const AIsEntryPoint: Boolean;
  const AIsDllEntry: Boolean;
  const AIsPublic: Boolean;
  const ALinkage: TTigerLinkage
): TTigerCodeBuilder;
var
  LFunc: TTigerFuncInfo;
begin
  FillChar(LFunc, SizeOf(LFunc), 0);
  LFunc.FuncName := AName;
  LFunc.IsEntryPoint := AIsEntryPoint;
  LFunc.IsDllEntry := AIsDllEntry;
  LFunc.IsPublic := AIsPublic;
  LFunc.Linkage := ALinkage;
  LFunc.ReturnType := vtVoid;
  LFunc.TempCount := 0;

  FCurrentFunc := FFunctions.Count;
  FFunctions.Add(LFunc);

  Result := Self;
end;

function TTigerCodeBuilder.SetReturnType(const AType: TTigerValueType): TTigerCodeBuilder;
var
  LFunc: TTigerFuncInfo;
begin
  LFunc := GetCurrentFunc();
  LFunc.ReturnType := AType;
  LFunc.ReturnSize := 0;
  LFunc.ReturnAlignment := 0;
  SetCurrentFunc(LFunc);
  Result := Self;
end;

function TTigerCodeBuilder.SetReturnType(const AType: TTigerValueType;
  const ASize: Integer; const AAlignment: Integer): TTigerCodeBuilder;
var
  LFunc: TTigerFuncInfo;
begin
  LFunc := GetCurrentFunc();
  LFunc.ReturnType := AType;
  LFunc.ReturnSize := ASize;
  LFunc.ReturnAlignment := AAlignment;
  SetCurrentFunc(LFunc);
  Result := Self;
end;

function TTigerCodeBuilder.SetIsVariadic(const AValue: Boolean): TTigerCodeBuilder;
var
  LFunc: TTigerFuncInfo;
begin
  LFunc := GetCurrentFunc();
  LFunc.IsVariadic := AValue;
  SetCurrentFunc(LFunc);
  Result := Self;
end;

function TTigerCodeBuilder.EndProc(): TTigerCodeBuilder;
var
  LFunc: TTigerFuncInfo;
begin
  // Compute ExportName based on linkage if function is public
  LFunc := GetCurrentFunc();
  if LFunc.IsPublic then
  begin
    case LFunc.Linkage of
      plC:
        // C linkage - use raw function name
        LFunc.ExportName := LFunc.FuncName;
      plDefault:
        // Default linkage - for now use raw name, mangling can be added later
        // TODO: Use TTigerABIMangler when circular dependency is resolved
        LFunc.ExportName := LFunc.FuncName;
    end;
    SetCurrentFunc(LFunc);
  end;
  
  FCurrentFunc := -1;
  Result := Self;
end;

function TTigerCodeBuilder.AddParam(const AName: string; const AType: TTigerValueType): TTigerLocalHandle;
var
  LFunc: TTigerFuncInfo;
  LParam: TTigerParamInfo;
  LLen: Integer;
begin
  LFunc := GetCurrentFunc();

  LParam.ParamName := AName;
  LParam.ParamType := AType;

  LLen := Length(LFunc.Params);
  SetLength(LFunc.Params, LLen + 1);
  LFunc.Params[LLen] := LParam;

  SetCurrentFunc(LFunc);

  Result.Index := LLen;
  Result.IsParam := True;
end;

function TTigerCodeBuilder.AddParam(const AName: string; const ASize: Integer; const AAlignment: Integer): TTigerLocalHandle;
var
  LFunc: TTigerFuncInfo;
  LParam: TTigerParamInfo;
  LLen: Integer;
begin
  LFunc := GetCurrentFunc();

  LParam.ParamName := AName;
  LParam.ParamType := vtVoid;  // Composite type - no primitive type
  LParam.ParamSize := ASize;
  LParam.ParamAlignment := AAlignment;

  LLen := Length(LFunc.Params);
  SetLength(LFunc.Params, LLen + 1);
  LFunc.Params[LLen] := LParam;

  SetCurrentFunc(LFunc);

  Result.Index := LLen;
  Result.IsParam := True;
end;

function TTigerCodeBuilder.AddLocal(const AName: string; const AType: TTigerValueType): TTigerLocalHandle;
var
  LFunc: TTigerFuncInfo;
  LLocal: TTigerLocalInfo;
  LLen: Integer;
  LSize: Integer;
begin
  LFunc := GetCurrentFunc();

  // Calculate size based on type
  case AType of
    vtInt8, vtUInt8: LSize := 1;
    vtInt16, vtUInt16: LSize := 2;
    vtInt32, vtUInt32, vtFloat32: LSize := 4;
    vtInt64, vtUInt64, vtFloat64, vtPointer: LSize := 8;
  else
    LSize := 8;  // Default to 8 bytes
  end;

  // Align size to 8 bytes for stack alignment
  LSize := ((LSize + 7) div 8) * 8;

  LLocal.LocalName := AName;
  LLocal.LocalType := AType;
  LLocal.LocalSize := LSize;
  LLocal.LocalAlignment := 8;  // All primitives align to 8 bytes
  LLocal.StackOffset := 0;  // Will be calculated during code generation

  LLen := Length(LFunc.Locals);
  SetLength(LFunc.Locals, LLen + 1);
  LFunc.Locals[LLen] := LLocal;

  SetCurrentFunc(LFunc);

  Result.Index := LLen;
  Result.IsParam := False;
end;

function TTigerCodeBuilder.AddLocal(const AName: string; const ASize: Integer): TTigerLocalHandle;
var
  LFunc: TTigerFuncInfo;
  LLocal: TTigerLocalInfo;
  LLen: Integer;
  LAlignedSize: Integer;
begin
  LFunc := GetCurrentFunc();

  // Align size to 8 bytes for stack alignment
  LAlignedSize := ((ASize + 7) div 8) * 8;

  LLocal.LocalName := AName;
  LLocal.LocalType := vtVoid;  // Composite type
  LLocal.LocalSize := LAlignedSize;
  LLocal.LocalAlignment := 8;  // Default alignment for composite types
  LLocal.StackOffset := 0;  // Will be calculated during code generation

  LLen := Length(LFunc.Locals);
  SetLength(LFunc.Locals, LLen + 1);
  LFunc.Locals[LLen] := LLocal;

  SetCurrentFunc(LFunc);

  Result.Index := LLen;
  Result.IsParam := False;
end;

function TTigerCodeBuilder.AddLocal(const AName: string; const ASize: Integer; const AAlignment: Integer): TTigerLocalHandle;
var
  LFunc: TTigerFuncInfo;
  LLocal: TTigerLocalInfo;
  LLen: Integer;
  LAlignedSize: Integer;
begin
  LFunc := GetCurrentFunc();

  // Align size to 8 bytes for stack alignment
  LAlignedSize := ((ASize + 7) div 8) * 8;

  LLocal.LocalName := AName;
  LLocal.LocalType := vtVoid;  // Composite type
  LLocal.LocalSize := LAlignedSize;
  LLocal.LocalAlignment := AAlignment;
  LLocal.StackOffset := 0;  // Will be calculated during code generation

  LLen := Length(LFunc.Locals);
  SetLength(LFunc.Locals, LLen + 1);
  LFunc.Locals[LLen] := LLocal;

  SetCurrentFunc(LFunc);

  Result.Index := LLen;
  Result.IsParam := False;
end;

function TTigerCodeBuilder.DefineLabel(const AName: string): TTigerLabelHandle;
var
  LFunc: TTigerFuncInfo;
  LLabel: TTigerLabelInfo;
  LLen: Integer;
begin
  LFunc := GetCurrentFunc();

  LLabel.LabelName := AName;
  LLabel.CodeOffset := -1;
  LLabel.IsDefined := False;

  LLen := Length(LFunc.Labels);
  SetLength(LFunc.Labels, LLen + 1);
  LFunc.Labels[LLen] := LLabel;

  SetCurrentFunc(LFunc);

  Result.Index := LLen;
end;

function TTigerCodeBuilder.MarkLabel(const ALabel: TTigerLabelHandle): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikLabel;
  LInstr.LabelTarget := ALabel;
  AddInstr(LInstr);

  Result := Self;
end;

procedure TTigerCodeBuilder.AddExceptionScope(
  const ATryBegin: TTigerLabelHandle;
  const ATryEnd: TTigerLabelHandle;
  const AExcept: TTigerLabelHandle;
  const AFinally: TTigerLabelHandle;
  const AEnd: TTigerLabelHandle
);
var
  LFunc: TTigerFuncInfo;
  LScope: TTigerExceptionScope;
  LLen: Integer;
begin
  if FCurrentFunc < 0 then
    Exit;
  
  LScope.TryBeginLabel := ATryBegin;
  LScope.TryEndLabel := ATryEnd;
  LScope.ExceptLabel := AExcept;
  LScope.FinallyLabel := AFinally;
  LScope.EndLabel := AEnd;
  
  LFunc := FFunctions[FCurrentFunc];
  LLen := Length(LFunc.ExceptionScopes);
  SetLength(LFunc.ExceptionScopes, LLen + 1);
  LFunc.ExceptionScopes[LLen] := LScope;
  FFunctions[FCurrentFunc] := LFunc;
end;

function TTigerCodeBuilder.Call(const AImport: TTigerImportHandle): TTigerCodeBuilder;
begin
  Result := Call(AImport, []);
end;

function TTigerCodeBuilder.Call(const AImport: TTigerImportHandle; const AArgs: array of TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCallImport;
  LInstr.ImportTarget := AImport;
  LInstr.Dest := TTigerTempHandle.Invalid();

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.Call(const AFunc: TTigerFuncHandle): TTigerCodeBuilder;
begin
  Result := Call(AFunc, []);
end;

function TTigerCodeBuilder.Call(const AFunc: TTigerFuncHandle; const AArgs: array of TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCall;
  LInstr.FuncTarget := AFunc;
  LInstr.Dest := TTigerTempHandle.Invalid();

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.CallFunc(const AImport: TTigerImportHandle): TTigerTempHandle;
begin
  Result := CallFunc(AImport, []);
end;

function TTigerCodeBuilder.CallFunc(const AImport: TTigerImportHandle; const AArgs: array of TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCallImport;
  LInstr.ImportTarget := AImport;
  LInstr.Dest := Result;

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CallFunc(const AFunc: TTigerFuncHandle): TTigerTempHandle;
begin
  Result := CallFunc(AFunc, []);
end;

function TTigerCodeBuilder.CallFunc(const AFunc: TTigerFuncHandle; const AArgs: array of TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCall;
  LInstr.FuncTarget := AFunc;
  LInstr.Dest := Result;

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CallIndirect(const AFuncPtr: TTigerOperand): TTigerCodeBuilder;
begin
  Result := CallIndirect(AFuncPtr, []);
end;

function TTigerCodeBuilder.CallIndirect(const AFuncPtr: TTigerOperand; const AArgs: array of TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCallIndirect;
  LInstr.Op1 := AFuncPtr;  // Function pointer operand
  LInstr.Dest := TTigerTempHandle.Invalid();

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.CallIndirectFunc(const AFuncPtr: TTigerOperand): TTigerTempHandle;
begin
  Result := CallIndirectFunc(AFuncPtr, []);
end;

function TTigerCodeBuilder.CallIndirectFunc(const AFuncPtr: TTigerOperand; const AArgs: array of TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCallIndirect;
  LInstr.Op1 := AFuncPtr;  // Function pointer operand
  LInstr.Dest := Result;

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
end;

function TTigerCodeBuilder.LoadFuncAddr(const AFuncIndex: Integer): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
  LFuncHandle: TTigerFuncHandle;
begin
  Result := AllocTemp();

  LFuncHandle.Index := AFuncIndex;

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikLoad;  // Will be resolved as LEA to function address
  LInstr.Dest := Result;
  LInstr.Op1 := TTigerOperand.FromFunc(LFuncHandle);

  AddInstr(LInstr);
end;

function TTigerCodeBuilder.Return(): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikReturn;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.Return(const AValue: TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikReturnValue;
  LInstr.Op1 := AValue;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.OpAdd(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikAdd;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpSub(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikSub;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpMul(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikMul;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpDiv(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikDiv;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpMod(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikMod;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpAnd(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikBitAnd;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpOr(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikBitOr;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpXor(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikBitXor;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpNot(const AValue: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikBitNot;
  LInstr.Dest := Result;
  LInstr.Op1 := AValue;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpShl(const AValue, ACount: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikShl;
  LInstr.Dest := Result;
  LInstr.Op1 := AValue;
  LInstr.Op2 := ACount;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpShr(const AValue, ACount: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikShr;
  LInstr.Dest := Result;
  LInstr.Op1 := AValue;
  LInstr.Op2 := ACount;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CmpEq(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCmpEq;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CmpNe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCmpNe;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CmpLt(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCmpLt;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CmpLe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCmpLe;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CmpGt(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCmpGt;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.CmpGe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikCmpGe;
  LInstr.Dest := Result;
  LInstr.Op1 := ALeft;
  LInstr.Op2 := ARight;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpSetLiteral(const AElements: array of Integer; const ALowBound: Integer; const AStorageSize: Integer): TTigerTempHandle;
var
  LBitPattern: UInt64;
  LI: Integer;
  LInstr: TTigerInstruction;
begin
  // Compute the bit pattern from elements
  LBitPattern := 0;
  for LI := 0 to High(AElements) do
    LBitPattern := LBitPattern or (UInt64(1) shl (AElements[LI] - ALowBound));
  
  // Create temp with the bit pattern as immediate
  Result := AllocTemp();
  
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikBitOr;  // Use OR with 0 to load immediate
  LInstr.Dest := Result;
  LInstr.Op1 := TTigerOperand.FromImm(0);
  LInstr.Op2 := TTigerOperand.FromImm(Int64(LBitPattern));
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.OpSetUnion(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
begin
  // Union is bitwise OR
  Result := OpOr(ALeft, ARight);
end;

function TTigerCodeBuilder.OpSetDiff(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LNotRight: TTigerTempHandle;
begin
  // Difference is A AND (NOT B)
  LNotRight := OpNot(ARight);
  Result := OpAnd(ALeft, TTigerOperand.FromTemp(LNotRight));
end;

function TTigerCodeBuilder.OpSetInter(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
begin
  // Intersection is bitwise AND
  Result := OpAnd(ALeft, ARight);
end;

function TTigerCodeBuilder.OpSetIn(const AElement, ASet: TTigerOperand; const ALowBound: Integer): TTigerTempHandle;
var
  LAdjustedElement: TTigerTempHandle;
  LMask: TTigerTempHandle;
  LAndResult: TTigerTempHandle;
begin
  // Test if element is in set: ((1 << (element - lowbound)) AND set) <> 0
  
  // Adjust element by lowbound if needed
  if ALowBound <> 0 then
    LAdjustedElement := OpSub(AElement, TTigerOperand.FromImm(ALowBound))
  else
  begin
    // Just use element directly - copy to temp
    LAdjustedElement := OpAdd(AElement, TTigerOperand.FromImm(0));
  end;
  
  // Create mask: 1 << adjustedElement
  LMask := OpShl(TTigerOperand.FromImm(1), TTigerOperand.FromTemp(LAdjustedElement));
  
  // AND with set
  LAndResult := OpAnd(TTigerOperand.FromTemp(LMask), ASet);
  
  // Compare with 0 (result is 1 if in set, 0 if not)
  Result := CmpNe(TTigerOperand.FromTemp(LAndResult), TTigerOperand.FromImm(0));
end;

function TTigerCodeBuilder.OpSetEq(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
begin
  // Set equality is direct comparison
  Result := CmpEq(ALeft, ARight);
end;

function TTigerCodeBuilder.OpSetNe(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
begin
  // Set inequality is direct comparison
  Result := CmpNe(ALeft, ARight);
end;

function TTigerCodeBuilder.OpSetSubset(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LAndResult: TTigerTempHandle;
begin
  // A <= B (subset): (A AND B) = A
  LAndResult := OpAnd(ALeft, ARight);
  Result := CmpEq(TTigerOperand.FromTemp(LAndResult), ALeft);
end;

function TTigerCodeBuilder.OpSetSuperset(const ALeft, ARight: TTigerOperand): TTigerTempHandle;
var
  LAndResult: TTigerTempHandle;
begin
  // A >= B (superset): (A AND B) = B
  LAndResult := OpAnd(ALeft, ARight);
  Result := CmpEq(TTigerOperand.FromTemp(LAndResult), ARight);
end;

function TTigerCodeBuilder.VaCount(): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikVaCount;
  LInstr.Dest := Result;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.VaArgAt(const AIndex: TTigerOperand; const AType: TTigerValueType): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikVaArgAt;
  LInstr.Dest := Result;
  LInstr.Op1 := AIndex;                          // Index expression
  LInstr.Op2 := TTigerOperand.FromImm(Ord(AType)); // Type to read as
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.EmitSyscall(const ANr: Integer;
  const AArgs: array of TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikSyscall;
  LInstr.SyscallNr := ANr;
  LInstr.Dest := TTigerTempHandle.Invalid();

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.EmitSyscallFunc(const ANr: Integer;
  const AArgs: array of TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
  LI: Integer;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikSyscall;
  LInstr.SyscallNr := ANr;
  LInstr.Dest := Result;

  SetLength(LInstr.Args, Length(AArgs));
  for LI := 0 to High(AArgs) do
    LInstr.Args[LI] := AArgs[LI];

  AddInstr(LInstr);
end;

function TTigerCodeBuilder.Store(const ADest: TTigerLocalHandle; const AValue: TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikStore;
  LInstr.Op1 := TTigerOperand.FromLocal(ADest);
  LInstr.Op2 := AValue;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.Load(const ASrc: TTigerLocalHandle): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikLoad;
  LInstr.Dest := Result;
  LInstr.Op1 := TTigerOperand.FromLocal(ASrc);
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.StorePtr(const APtr, AValue: TTigerOperand): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikStorePtr;
  LInstr.Op1 := APtr;
  LInstr.Op2 := AValue;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.LoadPtr(const APtr: TTigerOperand): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikLoadPtr;
  LInstr.Dest := Result;
  LInstr.Op1 := APtr;
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.AddressOf(const ALocal: TTigerLocalHandle): TTigerTempHandle;
var
  LInstr: TTigerInstruction;
begin
  Result := AllocTemp();

  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikAddressOf;
  LInstr.Dest := Result;
  LInstr.Op1 := TTigerOperand.FromLocal(ALocal);
  AddInstr(LInstr);
end;

function TTigerCodeBuilder.Jump(const ALabel: TTigerLabelHandle): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikJump;
  LInstr.LabelTarget := ALabel;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.JumpIf(const ACond: TTigerOperand; const ALabel: TTigerLabelHandle): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikJumpIf;
  LInstr.Op1 := ACond;
  LInstr.LabelTarget := ALabel;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.JumpIfNot(const ACond: TTigerOperand; const ALabel: TTigerLabelHandle): TTigerCodeBuilder;
var
  LInstr: TTigerInstruction;
begin
  FillChar(LInstr, SizeOf(LInstr), 0);
  LInstr.Kind := ikJumpIfNot;
  LInstr.Op1 := ACond;
  LInstr.LabelTarget := ALabel;
  AddInstr(LInstr);
  Result := Self;
end;

function TTigerCodeBuilder.GetFuncCount(): Integer;
begin
  Result := FFunctions.Count;
end;

function TTigerCodeBuilder.GetFunc(const AIndex: Integer): TTigerFuncInfo;
begin
  if (AIndex >= 0) and (AIndex < FFunctions.Count) then
    Result := FFunctions[AIndex]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TTigerCodeBuilder.GetFuncHandle(const AName: string): TTigerFuncHandle;
var
  LI: Integer;
begin
  Result := TTigerFuncHandle.Invalid();
  for LI := 0 to FFunctions.Count - 1 do
  begin
    if SameText(FFunctions[LI].FuncName, AName) then
    begin
      Result.Index := LI;
      Exit;
    end;
  end;
end;

function TTigerCodeBuilder.GetCurrentFuncHandle(): TTigerFuncHandle;
begin
  Result.Index := FCurrentFunc;
end;

procedure TTigerCodeBuilder.Clear();
begin
  FFunctions.Clear();
  FCurrentFunc := -1;
end;

//==============================================================================
// TTigerBackend Implementation
//==============================================================================

end.
