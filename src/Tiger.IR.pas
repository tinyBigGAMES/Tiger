{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.IR;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Tiger.Utils,
  Tiger.Common,
  Tiger.Errors,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.ABI;

type
  //============================================================================
  // Forward Declarations
  //============================================================================
  TTigerIR = class;

  //============================================================================
  // TTigerIRExpr - Expression handle (opaque to user)
  //============================================================================
  TTigerIRExpr = record
    Index: Integer;

    class function None(): TTigerIRExpr; static;
    function IsValid(): Boolean;

    class operator Implicit(const AValue: Integer): TTigerIRExpr;
    class operator Implicit(const AValue: Int64): TTigerIRExpr;
    class operator Implicit(const AValue: Double): TTigerIRExpr;
  end;

  //============================================================================
  // TTigerTypeRef - Reference to a type (primitive or composite)
  //============================================================================
  TTigerTypeRef = record
    IsPrimitive: Boolean;
    Primitive: TTigerValueType;    // Valid when IsPrimitive = True
    TypeIndex: Integer;          // Valid when IsPrimitive = False (index into type registry)

    class function FromPrimitive(const AType: TTigerValueType): TTigerTypeRef; static;
    class function FromComposite(const AIndex: Integer): TTigerTypeRef; static;
    class function None(): TTigerTypeRef; static;
    function IsValid(): Boolean;
    function ToString(): string;

    class operator Equal(const A: TTigerTypeRef; const B: TTigerTypeRef): Boolean;
    class operator NotEqual(const A: TTigerTypeRef; const B: TTigerTypeRef): Boolean;
  end;

  //============================================================================
  // TTigerIR - High-level fluent IR builder
  //============================================================================
  TTigerIR = class(TTigerBaseObject)
  public type
    //--------------------------------------------------------------------------
    // Internal: Import info
    //--------------------------------------------------------------------------
    TIRImport = record
      DllName: string;
      FuncName: string;
      ParamTypes: TArray<TTigerValueType>;
      ReturnType: TTigerValueType;
      IsVarArgs: Boolean;
      IsStatic: Boolean;       // True for ImportLib (static linking)
      Linkage: TTigerLinkage;  // plC = raw name, plDefault = Itanium mangled
    end;

    //--------------------------------------------------------------------------
    // Internal: String literal
    //--------------------------------------------------------------------------
    TIRString = record
      Value: string;
      IsWide: Boolean;
    end;

    //--------------------------------------------------------------------------
    // Internal: Global variable
    //--------------------------------------------------------------------------
    TIRGlobal = record
      GlobalName: string;
      GlobalType: TTigerValueType;
      GlobalTypeRef: TTigerTypeRef;  // For composite/managed types (string)
      InitExpr: Integer;  // -1 if uninitialized
    end;

    //--------------------------------------------------------------------------
    // Internal: Variable (param or local)
    //--------------------------------------------------------------------------
    TIRVar = record
      VarName: string;
      VarTypeRef: TTigerTypeRef;  // Supports both primitive and composite types
      IsParam: Boolean;
    end;

    //--------------------------------------------------------------------------
    // Internal: Expression node kinds
    //--------------------------------------------------------------------------
    TIRExprKind = (
      ekNone,
      ekConstInt,
      ekConstFloat,
      ekConstString,
      ekVariable,
      ekBinary,
      ekUnary,
      ekCall,
      ekFieldAccess,       // record.field
      ekArrayIndex,        // array[index]
      ekFuncAddr,          // @funcName
      ekCallIndirect,      // funcPtr(args)
      ekGetExceptionCode,  // getexceptioncode()
      ekGetExceptionMsg,   // getexceptionmessage()
      ekSetLiteral,        // {1, 3, 5..10}
      ekVaCount,           // VaCount()
      ekVaArgAt,           // VaArg(index, type)
      ekSyscall            // Linux syscall(nr, args...)
    );

    //--------------------------------------------------------------------------
    // Internal: Binary/Unary operation kinds
    //--------------------------------------------------------------------------
    TIROpKind = (
      opNone,
      // Arithmetic
      opAdd,
      opSub,
      opMul,
      opDiv,
      opMod,
      opNeg,
      // Bitwise
      opBitAnd,
      opBitOr,
      opBitXor,
      opBitNot,
      opShl,
      opShr,
      // Comparison
      opCmpEq,
      opCmpNe,
      opCmpLt,
      opCmpLe,
      opCmpGt,
      opCmpGe,
      // Logical
      opAnd,
      opOr,
      opNot,
      // Pointer
      opAddrOf,
      opDeref,
      // Set operations
      opSetUnion,     // set1 + set2
      opSetDiff,      // set1 - set2
      opSetInter,     // set1 * set2
      opSetIn,        // element in set
      opSetEq,        // set1 = set2
      opSetNe,        // set1 <> set2
      opSetSubset,    // set1 <= set2
      opSetSuperset   // set1 >= set2
    );

    //--------------------------------------------------------------------------
    // Internal: Expression node
    //--------------------------------------------------------------------------
    TIRExprNode = record
      Kind: TIRExprKind;
      ResultType: TTigerTypeRef;   // Type this expression evaluates to
      ConstInt: Int64;
      ConstFloat: Double;
      StringIndex: Integer;
      VarName: string;
      Op: TIROpKind;
      Left: Integer;
      Right: Integer;
      CallTarget: string;
      CallArgs: TArray<Integer>;
      // For ekFieldAccess
      ObjectExpr: Integer;       // Expression yielding record/object
      FieldName: string;         // Field name to access
      FieldOffset: Integer;      // Pre-computed byte offset
      FieldSize: Integer;        // Pre-computed field size
      BitWidth: Integer;         // 0 = full field, >0 = bit field width
      BitOffset: Integer;        // Bit position within storage unit
      // For ekArrayIndex
      ArrayExpr: Integer;        // Expression yielding array
      IndexExpr: Integer;        // Expression yielding index
      ElementSize: Integer;      // Pre-computed element size
      // For ekFuncAddr
      FuncAddrName: string;      // Name of function to take address of
      // For ekCallIndirect
      IndirectTarget: Integer;   // Expression index of function pointer
      // For ekSetLiteral
      SetTypeIndex: Integer;     // Index into type registry for set type
      SetElements: TArray<Integer>;  // Individual element values (adjusted by LowBound)
      // For ekVaArgAt
      VaArgIndex: Integer;       // Expression index for the arg index
      VaArgType: TTigerValueType;  // Type to read as
      // For ekSyscall
      SyscallNr: Integer;          // Linux syscall number (e.g., 60 = exit)
    end;

    //--------------------------------------------------------------------------
    // Internal: Statement kinds
    //--------------------------------------------------------------------------
    TIRStmtKind = (
      skAssign,
      skCall,
      skReturn,
      skReturnValue,
      skIfBegin,
      skElseBegin,
      skIfEnd,
      skWhileBegin,
      skWhileEnd,
      skForBegin,
      skForEnd,
      skRepeatBegin,
      skRepeatEnd,
      skCaseBegin,
      skCaseOf,
      skCaseElse,
      skCaseEnd,
      skCallIndirect,
      skTryBegin,
      skExceptBegin,
      skFinallyBegin,
      skTryEnd,
      skRaise,
      skRaiseCode,
      skSyscall            // Linux syscall as statement (discard result)
    );

    //--------------------------------------------------------------------------
    // Internal: Statement node
    //--------------------------------------------------------------------------
    TIRStmt = record
      Kind: TIRStmtKind;
      DestVar: string;
      DestExpr: Integer;       // For assigning to field/index expressions (-1 = use DestVar)
      Expr: Integer;
      CallTarget: string;
      CallArgs: TArray<Integer>;
      ForVar: string;
      ForFrom: Integer;
      ForTo: Integer;
      ForDownTo: Boolean;
      CaseValues: TArray<Integer>;  // Raw integer values for CaseOf matching
      IndirectTarget: Integer;        // Expression index for indirect calls
      RaiseMsg: Integer;              // Expression index for raise message
      RaiseCode: Integer;             // Expression index for raise code (skRaiseCode only)
      SyscallNr: Integer;             // Linux syscall number (for skSyscall)
    end;

    //--------------------------------------------------------------------------
    // Internal: Function definition
    //--------------------------------------------------------------------------
    TIRFunc = record
      FuncName: string;
      ReturnType: TTigerValueType;
      IsEntryPoint: Boolean;
      IsDllEntry: Boolean;        // If True, this is the DllMain entry point
      IsPublic: Boolean;          // If True, export this function
      Linkage: TTigerLinkage;
      ParamTypes: TArray<TTigerValueType>;  // For mangling
      Vars: TList<TIRVar>;
      Stmts: TList<TIRStmt>;
      IsVariadic: Boolean;        // If True, function accepts variadic args
    end;

    //--------------------------------------------------------------------------
    // Internal: Control flow block tracking
    //--------------------------------------------------------------------------
    TBlockKind = (
      bkIf,
      bkElse,
      bkWhile,
      bkFor,
      bkRepeat,
      bkCase,
      bkTry,
      bkExcept,
      bkFinally
    );

    TBlockInfo = record
      Kind: TBlockKind;
      LabelStart: TTigerLabelHandle;
      LabelEnd: TTigerLabelHandle;
      LabelElse: TTigerLabelHandle;
      ForVar: string;
      ForTo: Integer;
      ForDownTo: Boolean;
      CaseSelectorExpr: Integer;       // Expression index of case selector
      LabelNextCase: TTigerLabelHandle;  // Label for next CaseOf/CaseElse/CaseEnd
    end;

    //--------------------------------------------------------------------------
    // Internal: Type system - Type kinds
    //--------------------------------------------------------------------------
    TIRTypeKind = (
      tkRecord,
      tkUnion,
      tkFixedArray,
      tkDynArray,
      tkEnum,
      tkAlias,
      tkPointer,
      tkRoutine,
      tkSet
    );

    //--------------------------------------------------------------------------
    // Internal: Record field definition
    //--------------------------------------------------------------------------
    TIRRecordField = record
      FieldName: string;
      FieldType: TTigerTypeRef;
      FieldOffset: Integer;     // Computed during finalization (byte offset)
      BitWidth: Integer;        // 0 = full width, >0 = bit field width
      BitOffset: Integer;       // Bit position within storage unit (0-63)
      OverlayGroup: Integer;    // Records: 0 = normal, >0 = overlay group (anon union)
      AnonRecordGroup: Integer; // Unions: 0 = normal, >0 = sequential group (anon record)
    end;

    //--------------------------------------------------------------------------
    // Internal: Record type definition
    //--------------------------------------------------------------------------
    TIRRecordType = record
      TypeName: string;
      Fields: TArray<TIRRecordField>;
      TotalSize: Integer;    // Computed during finalization
      Alignment: Integer;    // Computed during finalization
      ExplicitAlign: Integer; // 0 = natural, else 1/2/4/8/16
      BaseTypeIndex: Integer; // -1 = no base, else index into FTypes
      IsPacked: Boolean;
      IsFinalized: Boolean;
    end;

    //--------------------------------------------------------------------------
    // Internal: Union type definition
    //--------------------------------------------------------------------------
    TIRUnionType = record
      TypeName: string;
      Fields: TArray<TIRRecordField>;  // Reuse field type, all at offset 0
      TotalSize: Integer;              // Max field size
      Alignment: Integer;              // Max field alignment
      IsFinalized: Boolean;
    end;

    //--------------------------------------------------------------------------
    // Internal: Fixed array type definition
    //--------------------------------------------------------------------------
    TIRFixedArrayType = record
      TypeName: string;
      ElementType: TTigerTypeRef;
      LowBound: Integer;
      HighBound: Integer;
      TotalSize: Integer;    // Computed: (High - Low + 1) * ElementSize
    end;

    //--------------------------------------------------------------------------
    // Internal: Dynamic array type definition
    //--------------------------------------------------------------------------
    TIRDynArrayType = record
      TypeName: string;
      ElementType: TTigerTypeRef;
      // Runtime layout: pointer to record with Length + Data
    end;

    //--------------------------------------------------------------------------
    // Internal: Enum value definition
    //--------------------------------------------------------------------------
    TIREnumValue = record
      ValueName: string;
      OrdinalValue: Int64;
    end;

    //--------------------------------------------------------------------------
    // Internal: Enum type definition
    //--------------------------------------------------------------------------
    TIREnumType = record
      TypeName: string;
      Values: TArray<TIREnumValue>;
      BaseType: TTigerValueType;  // Usually vtInt32
    end;

    //--------------------------------------------------------------------------
    // Internal: Type alias definition
    //--------------------------------------------------------------------------
    TIRAliasType = record
      TypeName: string;
      AliasedType: TTigerTypeRef;
    end;

    //--------------------------------------------------------------------------
    // Internal: Pointer type definition
    //--------------------------------------------------------------------------
    TIRPointerType = record
      TypeName: string;
      PointeeType: TTigerTypeRef;  // Type being pointed to (None = untyped)
      IsConst: Boolean;          // pointer to const T
    end;

    //--------------------------------------------------------------------------
    // Internal: Routine (procedural) type definition
    //--------------------------------------------------------------------------
    TIRRoutineType = record
      TypeName: string;
      ParamTypes: TArray<TTigerTypeRef>;
      ReturnType: TTigerTypeRef;
      Linkage: TTigerLinkage;
      IsVarArgs: Boolean;
    end;

    //--------------------------------------------------------------------------
    // Internal: Set type definition
    //--------------------------------------------------------------------------
    TIRSetType = record
      TypeName: string;
      LowBound: Integer;     // Minimum element value (offset)
      HighBound: Integer;    // Maximum element value
      BaseType: TTigerTypeRef; // For enum-based sets, None for integer ranges
      StorageSize: Integer;  // 1, 2, 4, or 8 bytes based on range span
    end;

    //--------------------------------------------------------------------------
    // Internal: Unified type entry (tagged union)
    //--------------------------------------------------------------------------
    TIRTypeEntry = record
      Kind: TIRTypeKind;
      RecordType: TIRRecordType;
      UnionType: TIRUnionType;
      FixedArrayType: TIRFixedArrayType;
      DynArrayType: TIRDynArrayType;
      EnumType: TIREnumType;
      AliasType: TIRAliasType;
      PointerType: TIRPointerType;
      RoutineType: TIRRoutineType;
      SetType: TIRSetType;
    end;

  private
    FImports: TList<TIRImport>;
    FStrings: TList<TIRString>;
    FGlobals: TList<TIRGlobal>;
    FFunctions: TList<TIRFunc>;
    FExpressions: TList<TIRExprNode>;
    FCurrentFunc: Integer;

    // Runtime snapshot (marks boundary between user code and injected runtime)
    FSnapshotFuncs: Integer;
    FSnapshotImports: Integer;
    FSnapshotStrings: Integer;
    FSnapshotGlobals: Integer;
    FSnapshotExprs: Integer;

    // Type registry
    FTypes: TList<TIRTypeEntry>;
    FBuildingRecordIndex: Integer;  // Index of record being built, -1 if none
    FBuildingUnionIndex: Integer;   // Index of union being built, -1 if none
    FBuildingEnumIndex: Integer;    // Index of enum being built, -1 if none
    FNextEnumOrdinal: Int64;        // Next ordinal value for enum
    FNextOverlayGroup: Integer;     // Counter for overlay groups in records
    FAnonUnionFieldStart: Integer;  // First field index of current anon union, -1 if not in anon union
    FNextAnonRecordGroup: Integer;  // Counter for anon record groups in unions
    FAnonRecordInUnion: Boolean;    // True when building anon record inside union
    FBuildingRoutineIndex: Integer;  // Index of routine type being built, -1 if none

    // Emit-time mappings
    FImportHandles: TArray<TTigerImportHandle>;
    FStringHandles: TArray<TTigerDataHandle>;
    FGlobalHandles: TArray<TTigerDataHandle>;
    FVarHandles: TDictionary<string, TTigerLocalHandle>;
    FBlockStack: TStack<TBlockInfo>;

    function GetCurrentFunc(): TIRFunc;
    function AddExpr(const ANode: TIRExprNode): TTigerIRExpr;
    function MakeBinaryExpr(const ALeft, ARight: TTigerIRExpr; const AOp: TIROpKind): TTigerIRExpr;
    function MakeUnaryExpr(const AValue: TTigerIRExpr; const AOp: TIROpKind): TTigerIRExpr;

    // Emit helpers
    function EmitExpr(const ABackend: TTigerBackend; const AExprIndex: Integer): TTigerOperand;
    function FindImport(const AName: string): Integer;
    function FindLocalFunc(const AName: string): Integer;

    // Type system helpers
    function GetPrimitiveSize(const AType: TTigerValueType): Integer;
    function GetPrimitiveAlignment(const AType: TTigerValueType): Integer;
    procedure FinalizeRecordLayout(const ATypeIndex: Integer);
    procedure FinalizeUnionLayout(const ATypeIndex: Integer);
    function FindVarType(const AVarName: string): TTigerTypeRef;

  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Imports
    //--------------------------------------------------------------------------
    function Import(
      const ADllName: string;
      const AFuncName: string;
      const AParams: array of TTigerValueType;
      const AReturn: TTigerValueType = vtVoid;
      const AVarArgs: Boolean = False;
      const ALinkage: TTigerLinkage = plC
    ): TTigerIR;

    function ImportLib(
      const ALibName: string;
      const AFuncName: string;
      const AParams: array of TTigerValueType;
      const AReturn: TTigerValueType = vtVoid;
      const AVarArgs: Boolean = False;
      const ALinkage: TTigerLinkage = plC
    ): TTigerIR;

    //--------------------------------------------------------------------------
    // Global Variables
    //--------------------------------------------------------------------------
    function Global(const AName: string; const AType: TTigerValueType): TTigerIR; overload;
    function Global(const AName: string; const AType: TTigerValueType; const AInit: TTigerIRExpr): TTigerIR; overload;
    function Global(const AName: string; const ATypeRef: TTigerTypeRef): TTigerIR; overload;
    function Global(const AName: string; const ATypeName: string): TTigerIR; overload;

    //--------------------------------------------------------------------------
    // Type Definitions (fluent)
    //--------------------------------------------------------------------------
    // Record types
    function DefineRecord(const AName: string; const AIsPacked: Boolean = False;
      const AExplicitAlign: Integer = 0; const ABaseTypeName: string = ''): TTigerIR;
    function BeginRecord(): TTigerIR;  // Anonymous record in union
    function Field(const AName: string; const AType: TTigerValueType): TTigerIR; overload;
    function Field(const AName: string; const ATypeName: string): TTigerIR; overload;
    function BitField(const AName: string; const AType: TTigerValueType; const ABitWidth: Integer): TTigerIR;
    function EndRecord(): TTigerIR;

    // Union types
    function DefineUnion(const AName: string): TTigerIR;
    function BeginUnion(): TTigerIR;  // Anonymous union in record
    function EndUnion(): TTigerIR;

    // Array types
    function DefineArray(const AName: string; const AElementType: TTigerValueType;
      const ALowBound: Integer; const AHighBound: Integer): TTigerIR; overload;
    function DefineArray(const AName: string; const AElementTypeName: string;
      const ALowBound: Integer; const AHighBound: Integer): TTigerIR; overload;
    function DefineDynArray(const AName: string; const AElementType: TTigerValueType): TTigerIR; overload;
    function DefineDynArray(const AName: string; const AElementTypeName: string): TTigerIR; overload;

    // Enum types
    function DefineEnum(const AName: string): TTigerIR;
    function EnumValue(const AName: string): TTigerIR; overload;
    function EnumValue(const AName: string; const AOrdinal: Int64): TTigerIR; overload;
    function EndEnum(): TTigerIR;

    // Type aliases
    function DefineAlias(const AName: string; const AType: TTigerValueType): TTigerIR; overload;
    function DefineAlias(const AName: string; const ATypeName: string): TTigerIR; overload;

    // Pointer types
    function DefinePointer(const AName: string): TTigerIR; overload;
    function DefinePointer(const AName: string; const APointeeType: TTigerValueType;
      const AIsConst: Boolean = False): TTigerIR; overload;
    function DefinePointer(const AName: string; const APointeeTypeName: string;
      const AIsConst: Boolean = False): TTigerIR; overload;

    // Routine (procedural) types
    function DefineRoutine(const AName: string; const ALinkage: TTigerLinkage = plDefault): TTigerIR;
    function RoutineParam(const AType: TTigerValueType): TTigerIR; overload;
    function RoutineParam(const ATypeName: string): TTigerIR; overload;
    function RoutineReturns(const AType: TTigerValueType): TTigerIR; overload;
    function RoutineReturns(const ATypeName: string): TTigerIR; overload;
    function RoutineVarArgs(): TTigerIR;
    function EndRoutine(): TTigerIR;

    // Set types
    function DefineSet(const AName: string): TTigerIR; overload;
    function DefineSet(const AName: string; const ALow: Integer; const AHigh: Integer): TTigerIR; overload;
    function DefineSet(const AName: string; const AEnumTypeName: string): TTigerIR; overload;

    // Type queries
    function FindType(const AName: string): Integer;
    function GetTypeSize(const ATypeRef: TTigerTypeRef): Integer;
    function GetTypeAlignment(const ATypeRef: TTigerTypeRef): Integer;
    function TypeRef(const AName: string): TTigerTypeRef;
    function GetTypeCount(): Integer;
    function GetTypeEntry(const AIndex: Integer): TIRTypeEntry;
    function IsStringType(const ATypeRef: TTigerTypeRef): Boolean;

    //--------------------------------------------------------------------------
    // Function Definition (fluent)
    //--------------------------------------------------------------------------
    function Func(
      const AName: string;
      const AReturnType: TTigerValueType = vtVoid;
      const AIsEntryPoint: Boolean = False;
      const ALinkage: TTigerLinkage = plDefault;
      const AIsPublic: Boolean = False
    ): TTigerIR;
    function OverloadFunc(
      const AName: string;
      const AReturnType: TTigerValueType = vtVoid;
      const AIsEntryPoint: Boolean = False;
      const AIsPublic: Boolean = False
    ): TTigerIR;
    function VariadicFunc(
      const AName: string;
      const AReturnType: TTigerValueType = vtVoid;
      const AIsEntryPoint: Boolean = False;
      const AIsPublic: Boolean = False
    ): TTigerIR;
    function DllMain(): TTigerIR;
    function Param(const AName: string; const AType: TTigerValueType): TTigerIR;
    function Local(const AName: string; const AType: TTigerValueType): TTigerIR; overload;
    function Local(const AName: string; const ATypeName: string): TTigerIR; overload;
    function EndFunc(): TTigerIR;

    //--------------------------------------------------------------------------
    // Statements (fluent)
    //--------------------------------------------------------------------------
    function Assign(const ADest: string; const AValue: TTigerIRExpr): TTigerIR;
    function AssignTo(const ADest: TTigerIRExpr; const AValue: TTigerIRExpr): TTigerIR;
    function Call(const AFuncName: string): TTigerIR; overload;
    function Call(const AFuncName: string; const AArgs: array of TTigerIRExpr): TTigerIR; overload;
    function CallAssign(const ADest: string; const AFuncName: string; const AArgs: array of TTigerIRExpr): TTigerIR;
    function Return(): TTigerIR; overload;
    function Return(const AValue: TTigerIRExpr): TTigerIR; overload;

    // Indirect calls (via function pointer)
    function CallIndirect(const AFuncPtr: TTigerIRExpr): TTigerIR; overload;
    function CallIndirect(const AFuncPtr: TTigerIRExpr; const AArgs: array of TTigerIRExpr): TTigerIR; overload;
    function CallIndirectAssign(const ADest: string; const AFuncPtr: TTigerIRExpr;
      const AArgs: array of TTigerIRExpr): TTigerIR;

    //--------------------------------------------------------------------------
    // Control Flow (fluent)
    //--------------------------------------------------------------------------
    function &If(const ACond: TTigerIRExpr): TTigerIR;
    function &Else(): TTigerIR;
    function EndIf(): TTigerIR;

    function &While(const ACond: TTigerIRExpr): TTigerIR;
    function EndWhile(): TTigerIR;

    function &For(const AVar: string; const AFrom: TTigerIRExpr; const ATo: TTigerIRExpr): TTigerIR;
    function ForDownTo(const AVar: string; const AFrom: TTigerIRExpr; const ATo: TTigerIRExpr): TTigerIR;
    function EndFor(): TTigerIR;

    function &Repeat(): TTigerIR;
    function &Until(const ACond: TTigerIRExpr): TTigerIR;

    function &Case(const ASelector: TTigerIRExpr): TTigerIR;
    function CaseOf(const AValues: array of TTigerIRExpr): TTigerIR; overload;
    function CaseOf(const AValues: array of Integer): TTigerIR; overload;
    function CaseElse(): TTigerIR;
    function EndCase(): TTigerIR;

    function &Try(): TTigerIR;
    function &Except(): TTigerIR;
    function &Finally(): TTigerIR;
    function EndTry(): TTigerIR;

    function &Raise(const AMsg: TTigerIRExpr): TTigerIR;
    function RaiseCode(const ACode: TTigerIRExpr; const AMsg: TTigerIRExpr): TTigerIR;

    function &Inc(const AVarName: string): TTigerIR; overload;
    function &Inc(const AVarName: string; const AAmount: TTigerIRExpr): TTigerIR; overload;
    function &Dec(const AVarName: string): TTigerIR; overload;
    function &Dec(const AVarName: string; const AAmount: TTigerIRExpr): TTigerIR; overload;

    //--------------------------------------------------------------------------
    // Expressions - Literals
    //--------------------------------------------------------------------------
    function Str(const AValue: string): TTigerIRExpr;
    function WStr(const AValue: string): TTigerIRExpr;
    function &Int64(const AValue: Int64): TTigerIRExpr;
    function Int32(const AValue: Int32): TTigerIRExpr;
    function Float64(const AValue: Double): TTigerIRExpr;
    function Bool(const AValue: Boolean): TTigerIRExpr;
    function Int8(const AValue: Int8): TTigerIRExpr;
    function Int16(const AValue: Int16): TTigerIRExpr;
    function UInt8(const AValue: UInt8): TTigerIRExpr;
    function UInt16(const AValue: UInt16): TTigerIRExpr;
    function UInt32(const AValue: UInt32): TTigerIRExpr;
    function UInt64(const AValue: UInt64): TTigerIRExpr;
    function Float32(const AValue: Single): TTigerIRExpr;
    function Null(): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Variable Reference
    //--------------------------------------------------------------------------
    function Get(const AName: string): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Arithmetic
    //--------------------------------------------------------------------------
    function Add(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Sub(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Mul(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function IDiv(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function IMod(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Neg(const AValue: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Bitwise
    //--------------------------------------------------------------------------
    function BitAnd(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function BitOr(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function BitXor(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function BitNot(const AValue: TTigerIRExpr): TTigerIRExpr;
    function &Shl(const AValue: TTigerIRExpr; const ACount: TTigerIRExpr): TTigerIRExpr;
    function &Shr(const AValue: TTigerIRExpr; const ACount: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Comparison
    //--------------------------------------------------------------------------
    function Eq(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Ne(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Lt(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Le(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Gt(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function Ge(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Logical
    //--------------------------------------------------------------------------
    function LogAnd(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function LogOr(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function LogNot(const AValue: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Pointers
    //--------------------------------------------------------------------------
    function AddrOf(const AName: string): TTigerIRExpr;
    function AddrOfVal(const AExpr: TTigerIRExpr): TTigerIRExpr;
    function Deref(const APtr: TTigerIRExpr): TTigerIRExpr; overload;
    function Deref(const APtr: TTigerIRExpr; const ATypeName: string): TTigerIRExpr; overload;
    function Deref(const APtr: TTigerIRExpr; const AType: TTigerValueType): TTigerIRExpr; overload;

    //--------------------------------------------------------------------------
    // Expressions - Function Pointers
    //--------------------------------------------------------------------------
    function FuncAddr(const AFuncName: string): TTigerIRExpr;
    function InvokeIndirect(const AFuncPtr: TTigerIRExpr; const AArgs: array of TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Composite Type Access
    //--------------------------------------------------------------------------
    function GetField(const AObject: TTigerIRExpr; const AFieldName: string): TTigerIRExpr;
    function GetIndex(const AArray: TTigerIRExpr; const AIndex: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Function Call
    //--------------------------------------------------------------------------
    function Invoke(const AFuncName: string; const AArgs: array of TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Exception Intrinsics
    //--------------------------------------------------------------------------
    function ExcCode(): TTigerIRExpr;
    function ExcMsg(): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Set Literals
    //--------------------------------------------------------------------------
    function SetLit(const ATypeName: string; const AElements: array of Integer): TTigerIRExpr;
    function SetLitRange(const ATypeName: string; const ALow: Integer; const AHigh: Integer): TTigerIRExpr;
    function EmptySet(const ATypeName: string): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Set Operations
    //--------------------------------------------------------------------------
    function SetUnion(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function SetDiff(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function SetInter(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function SetIn(const AElement: TTigerIRExpr; const ASet: TTigerIRExpr): TTigerIRExpr;
    function SetEq(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function SetNe(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function SetSubset(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
    function SetSuperset(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Compile-Time Intrinsics
    //--------------------------------------------------------------------------
    function &SizeOf(const ATypeName: string): TTigerIRExpr;
    function AlignOf(const ATypeName: string): TTigerIRExpr;
    function High(const ATypeName: string): TTigerIRExpr;
    function Low(const ATypeName: string): TTigerIRExpr;
    function Len(const ATypeName: string): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Runtime Intrinsics
    //--------------------------------------------------------------------------
    function Ord(const AValue: TTigerIRExpr): TTigerIRExpr;
    function Chr(const AValue: TTigerIRExpr): TTigerIRExpr;
    function Succ(const AValue: TTigerIRExpr): TTigerIRExpr;
    function Pred(const AValue: TTigerIRExpr): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Variadic Intrinsics
    //--------------------------------------------------------------------------
    function VaCount(): TTigerIRExpr;
    function VaArg(const AIndex: TTigerIRExpr; const AType: TTigerValueType): TTigerIRExpr;

    //--------------------------------------------------------------------------
    // Expressions - Syscall Intrinsics (Linux)
    //--------------------------------------------------------------------------
    function Syscall(const ANr: Integer; const AArgs: array of TTigerIRExpr): TTigerIRExpr;
    function SyscallStmt(const ANr: Integer; const AArgs: array of TTigerIRExpr): TTigerIR;

    //--------------------------------------------------------------------------
    // Emit to Backend
    //--------------------------------------------------------------------------
    procedure EmitTo(const ABackend: TTigerBackend; const AStatusCallback: TStatusCallback = nil; const AUserData: Pointer = nil);

    //--------------------------------------------------------------------------
    // Reset
    //--------------------------------------------------------------------------
    procedure Clear();

    //--------------------------------------------------------------------------
    // Runtime snapshot (for ResetBuild support)
    //--------------------------------------------------------------------------

    /// <summary>
    ///   Saves the current IR list counts as a snapshot. Everything added after
    ///   this point (typically runtime injection) can be removed by calling
    ///   RestoreSnapshot.
    /// </summary>
    procedure SaveSnapshot();

    /// <summary>
    ///   Truncates all IR lists back to the snapshot point, removing any
    ///   runtime-injected functions, imports, strings, and globals. No-op if
    ///   no snapshot has been saved.
    /// </summary>
    procedure RestoreSnapshot();

    //--------------------------------------------------------------------------
    // Getters for SSA conversion
    //--------------------------------------------------------------------------
    function GetImportCount(): Integer;
    function GetImport(const AIndex: Integer): TIRImport;
    function GetStringCount(): Integer;
    function GetString(const AIndex: Integer): TIRString;
    function GetGlobalCount(): Integer;
    function GetGlobal(const AIndex: Integer): TIRGlobal;
    function FindGlobal(const AName: string): Integer;
    function GetFunctionCount(): Integer;
    function GetFunction(const AIndex: Integer): TIRFunc;
    function GetExpressionCount(): Integer;
    function GetExpression(const AIndex: Integer): TIRExprNode;

    // Field lookup for SSA conversion (handles inheritance)
    function FindRecordField(const ATypeIndex: Integer; const AFieldName: string;
      out AFieldInfo: TIRRecordField): Boolean;
  end;

implementation

uses
  Tiger.SSA;

const
  // IR error codes
  ERR_IR_NO_ACTIVE_FUNCTION = 'I001';
  ERR_IR_UNKNOWN_VARIABLE   = 'I002';
  ERR_IR_UNKNOWN_FUNCTION   = 'I003';
  ERR_IR_UNKNOWN_OPERATION  = 'I004';
  ERR_IR_BLOCK_MISMATCH     = 'I005';
  ERR_IR_TYPE_BUILD         = 'I006';
  ERR_IR_UNKNOWN_TYPE       = 'I007';
  ERR_IR_OVERLOAD_C_LINKAGE = 'I008';
  ERR_IR_VARIADIC_OVERLOAD  = 'I009';

//==============================================================================
// TTigerIRExpr
//==============================================================================

class function TTigerIRExpr.None(): TTigerIRExpr;
begin
  Result.Index := -1;
end;

function TTigerIRExpr.IsValid(): Boolean;
begin
  Result := Index >= 0;
end;

class operator TTigerIRExpr.Implicit(const AValue: Integer): TTigerIRExpr;
begin
  // This creates a placeholder - actual conversion happens in context
  Result.Index := -AValue - 1000000;  // Encode as negative with offset
end;

class operator TTigerIRExpr.Implicit(const AValue: Int64): TTigerIRExpr;
begin
  Result.Index := -Integer(AValue) - 1000000;
end;

class operator TTigerIRExpr.Implicit(const AValue: Double): TTigerIRExpr;
begin
  // For doubles, we can't encode directly - user should use Float64()
  Result.Index := -1;
end;

//==============================================================================
// TTigerTypeRef
//==============================================================================

class function TTigerTypeRef.FromPrimitive(const AType: TTigerValueType): TTigerTypeRef;
begin
  Result := Default(TTigerTypeRef);
  Result.IsPrimitive := True;
  Result.Primitive := AType;
  Result.TypeIndex := -1;
end;

class function TTigerTypeRef.FromComposite(const AIndex: Integer): TTigerTypeRef;
begin
  Result := Default(TTigerTypeRef);
  Result.IsPrimitive := False;
  Result.Primitive := vtVoid;
  Result.TypeIndex := AIndex;
end;

class function TTigerTypeRef.None(): TTigerTypeRef;
begin
  Result := Default(TTigerTypeRef);
  Result.IsPrimitive := True;
  Result.Primitive := vtVoid;
  Result.TypeIndex := -1;
end;

function TTigerTypeRef.IsValid(): Boolean;
begin
  if IsPrimitive then
    Result := Primitive <> vtVoid
  else
    Result := TypeIndex >= 0;
end;

function TTigerTypeRef.ToString(): string;
const
  CPrimitiveNames: array[TTigerValueType] of string = (
    'void', 'int8', 'int16', 'int32', 'int64',
    'uint8', 'uint16', 'uint32', 'uint64',
    'float32', 'float64', 'pointer'
  );
begin
  if IsPrimitive then
    Result := CPrimitiveNames[Primitive]
  else
    Result := Format('type#%d', [TypeIndex]);
end;

class operator TTigerTypeRef.Equal(const A: TTigerTypeRef; const B: TTigerTypeRef): Boolean;
begin
  if A.IsPrimitive <> B.IsPrimitive then
    Exit(False);
  if A.IsPrimitive then
    Result := A.Primitive = B.Primitive
  else
    Result := A.TypeIndex = B.TypeIndex;
end;

class operator TTigerTypeRef.NotEqual(const A: TTigerTypeRef; const B: TTigerTypeRef): Boolean;
begin
  Result := not (A = B);
end;

//==============================================================================
// TTigerIR - Construction/Destruction
//==============================================================================

constructor TTigerIR.Create();
begin
  inherited Create();

  FImports := TList<TIRImport>.Create();
  FStrings := TList<TIRString>.Create();
  FGlobals := TList<TIRGlobal>.Create();
  FFunctions := TList<TIRFunc>.Create();
  FExpressions := TList<TIRExprNode>.Create();
  FTypes := TList<TIRTypeEntry>.Create();
  FVarHandles := TDictionary<string, TTigerLocalHandle>.Create();
  FBlockStack := TStack<TBlockInfo>.Create();
  FCurrentFunc := -1;
  FSnapshotFuncs := -1;
  FSnapshotImports := -1;
  FSnapshotStrings := -1;
  FSnapshotGlobals := -1;
  FSnapshotExprs := -1;
  FBuildingRecordIndex := -1;
  FBuildingUnionIndex := -1;
  FBuildingEnumIndex := -1;
  FNextEnumOrdinal := 0;
  FNextOverlayGroup := 0;
  FAnonUnionFieldStart := -1;
  FNextAnonRecordGroup := 0;
  FAnonRecordInUnion := False;
  FBuildingRoutineIndex := -1;
end;

destructor TTigerIR.Destroy();
var
  LI: Integer;
begin
  for LI := 0 to FFunctions.Count - 1 do
  begin
    FFunctions[LI].Vars.Free();
    FFunctions[LI].Stmts.Free();
  end;

  FBlockStack.Free();
  FVarHandles.Free();
  FTypes.Free();
  FExpressions.Free();
  FFunctions.Free();
  FGlobals.Free();
  FStrings.Free();
  FImports.Free();

  inherited Destroy();
end;

//==============================================================================
// TTigerIR - Private Helpers
//==============================================================================

function TTigerIR.GetCurrentFunc(): TIRFunc;
begin
  if (FCurrentFunc < 0) or (FCurrentFunc >= FFunctions.Count) then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_NO_ACTIVE_FUNCTION, 'No active function');
    Result := Default(TIRFunc);
    Exit;
  end;
  Result := FFunctions[FCurrentFunc];
end;

function TTigerIR.AddExpr(const ANode: TIRExprNode): TTigerIRExpr;
begin
  Result.Index := FExpressions.Count;
  FExpressions.Add(ANode);
end;

function TTigerIR.MakeBinaryExpr(const ALeft, ARight: TTigerIRExpr; const AOp: TIROpKind): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekBinary;
  LNode.Op := AOp;
  LNode.Left := ALeft.Index;
  LNode.Right := ARight.Index;
  Result := AddExpr(LNode);
end;

function TTigerIR.MakeUnaryExpr(const AValue: TTigerIRExpr; const AOp: TIROpKind): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekUnary;
  LNode.Op := AOp;
  LNode.Left := AValue.Index;
  LNode.Right := -1;
  Result := AddExpr(LNode);
end;

function TTigerIR.FindImport(const AName: string): Integer;
var
  LI: Integer;
begin
  for LI := 0 to FImports.Count - 1 do
  begin
    if SameText(FImports[LI].FuncName, AName) then
      Exit(LI);
  end;
  Result := -1;
end;

function TTigerIR.FindGlobal(const AName: string): Integer;
var
  LI: Integer;
begin
  for LI := 0 to FGlobals.Count - 1 do
  begin
    if SameText(FGlobals[LI].GlobalName, AName) then
      Exit(LI);
  end;
  Result := -1;
end;

function TTigerIR.FindLocalFunc(const AName: string): Integer;
var
  LI: Integer;
begin
  for LI := 0 to FFunctions.Count - 1 do
  begin
    if SameText(FFunctions[LI].FuncName, AName) then
      Exit(LI);
  end;
  Result := -1;
end;

//==============================================================================
// TTigerIR - Type System Helpers
//==============================================================================

function TTigerIR.GetPrimitiveSize(const AType: TTigerValueType): Integer;
begin
  case AType of
    vtVoid:    Result := 0;
    vtInt8:    Result := 1;
    vtInt16:   Result := 2;
    vtInt32:   Result := 4;
    vtInt64:   Result := 8;
    vtUInt8:   Result := 1;
    vtUInt16:  Result := 2;
    vtUInt32:  Result := 4;
    vtUInt64:  Result := 8;
    vtFloat32: Result := 4;
    vtFloat64: Result := 8;
    vtPointer: Result := 8;
  else
    Result := 8;
  end;
end;

function TTigerIR.BitField(const AName: string; const AType: TTigerValueType; const ABitWidth: Integer): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LField: TIRRecordField;
  LLen: Integer;
  LTypeSize: Integer;
begin
  Result := Self;

  // BitField only valid in records
  if FBuildingRecordIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'BitField requires an active record');
    Exit;
  end;

  // Validate bit width
  LTypeSize := GetPrimitiveSize(AType) * 8;  // Size in bits
  if (ABitWidth < 1) or (ABitWidth > LTypeSize) then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'BitField width must be 1..%d for this type', [LTypeSize]);
    Exit;
  end;

  LField := Default(TIRRecordField);
  LField.FieldName := AName;
  LField.FieldType := TTigerTypeRef.FromPrimitive(AType);
  LField.FieldOffset := -1;  // Computed during finalization
  LField.BitWidth := ABitWidth;
  LField.BitOffset := -1;  // Computed during finalization
  LField.OverlayGroup := 0;
  LField.AnonRecordGroup := 0;

  // If inside anonymous union, mark as overlay
  if FAnonUnionFieldStart >= 0 then
    LField.OverlayGroup := FNextOverlayGroup;

  LEntry := FTypes[FBuildingRecordIndex];
  LLen := Length(LEntry.RecordType.Fields);
  SetLength(LEntry.RecordType.Fields, LLen + 1);
  LEntry.RecordType.Fields[LLen] := LField;
  FTypes[FBuildingRecordIndex] := LEntry;
end;

function TTigerIR.GetPrimitiveAlignment(const AType: TTigerValueType): Integer;
begin
  // On x64, alignment equals size for primitives (up to 8)
  Result := GetPrimitiveSize(AType);
  if Result > 8 then
    Result := 8;
end;

procedure TTigerIR.FinalizeRecordLayout(const ATypeIndex: Integer);
var
  LEntry: TIRTypeEntry;
  LBaseEntry: TIRTypeEntry;
  LI: Integer;
  LOffset: Integer;
  LFieldSize: Integer;
  LFieldAlign: Integer;
  LMaxAlign: Integer;
  LCurrentOverlay: Integer;
  LOverlayStart: Integer;
  LOverlayMaxSize: Integer;
  // Bit field tracking
  LBitPos: Integer;           // Current bit position within storage unit
  LBitStorageSize: Integer;   // Size of current bit storage unit in bits
  LBitStorageOffset: Integer; // Byte offset of current bit storage unit
begin
  if (ATypeIndex < 0) or (ATypeIndex >= FTypes.Count) then
    Exit;

  LEntry := FTypes[ATypeIndex];
  if LEntry.Kind <> tkRecord then
    Exit;
  if LEntry.RecordType.IsFinalized then
    Exit;

  LOffset := 0;
  LMaxAlign := 1;
  LCurrentOverlay := 0;
  LOverlayStart := 0;
  LOverlayMaxSize := 0;
  LBitPos := 0;
  LBitStorageSize := 0;
  LBitStorageOffset := -1;

  // Handle base type inheritance
  if LEntry.RecordType.BaseTypeIndex >= 0 then
  begin
    // Ensure base is finalized first
    if not FTypes[LEntry.RecordType.BaseTypeIndex].RecordType.IsFinalized then
      FinalizeRecordLayout(LEntry.RecordType.BaseTypeIndex);

    // Get base info (re-read in case it was just finalized)
    LBaseEntry := FTypes[LEntry.RecordType.BaseTypeIndex];

    // Derived starts at base size (properly aligned)
    LOffset := LBaseEntry.RecordType.TotalSize;
    LMaxAlign := LBaseEntry.RecordType.Alignment;
  end;

  // Apply explicit alignment if specified
  if LEntry.RecordType.ExplicitAlign > 0 then
  begin
    if LEntry.RecordType.ExplicitAlign > LMaxAlign then
      LMaxAlign := LEntry.RecordType.ExplicitAlign;
  end;

  for LI := 0 to Length(LEntry.RecordType.Fields) - 1 do
  begin
    // Get field size and alignment
    LFieldSize := GetTypeSize(LEntry.RecordType.Fields[LI].FieldType);
    LFieldAlign := GetTypeAlignment(LEntry.RecordType.Fields[LI].FieldType);

    // Track max alignment
    if LFieldAlign > LMaxAlign then
      LMaxAlign := LFieldAlign;

    // Handle overlay groups (anonymous unions)
    if LEntry.RecordType.Fields[LI].OverlayGroup <> LCurrentOverlay then
    begin
      // Close current bit storage unit if any
      if LBitStorageSize > 0 then
      begin
        LOffset := LBitStorageOffset + (LBitStorageSize div 8);
        LBitStorageSize := 0;
        LBitPos := 0;
        LBitStorageOffset := -1;
      end;

      // Leaving previous overlay group - advance offset by max size
      if LCurrentOverlay > 0 then
        LOffset := LOverlayStart + LOverlayMaxSize;

      // Entering new overlay group
      LCurrentOverlay := LEntry.RecordType.Fields[LI].OverlayGroup;
      if LCurrentOverlay > 0 then
      begin
        // Align for new overlay group
        if not LEntry.RecordType.IsPacked then
        begin
          if (LOffset mod LFieldAlign) <> 0 then
            LOffset := LOffset + (LFieldAlign - (LOffset mod LFieldAlign));
        end;
        LOverlayStart := LOffset;
        LOverlayMaxSize := 0;
      end;
    end;

    // Check if this is a bit field
    if LEntry.RecordType.Fields[LI].BitWidth > 0 then
    begin
      // Bit field processing
      if (LBitStorageSize = 0) or
         (LBitPos + LEntry.RecordType.Fields[LI].BitWidth > LBitStorageSize) or
         (LBitStorageSize <> LFieldSize * 8) then
      begin
        // Need new storage unit: either no current unit, won't fit, or different size
        // Close previous bit storage unit
        if LBitStorageSize > 0 then
          LOffset := LBitStorageOffset + (LBitStorageSize div 8);

        // Align for new storage unit
        if not LEntry.RecordType.IsPacked then
        begin
          if (LOffset mod LFieldAlign) <> 0 then
            LOffset := LOffset + (LFieldAlign - (LOffset mod LFieldAlign));
        end;

        // Start new bit storage unit
        LBitStorageOffset := LOffset;
        LBitStorageSize := LFieldSize * 8;
        LBitPos := 0;
      end;

      // Place bit field
      LEntry.RecordType.Fields[LI].FieldOffset := LBitStorageOffset;
      LEntry.RecordType.Fields[LI].BitOffset := LBitPos;
      LBitPos := LBitPos + LEntry.RecordType.Fields[LI].BitWidth;

      // Track overlay size if in overlay
      if LCurrentOverlay > 0 then
      begin
        if LFieldSize > LOverlayMaxSize then
          LOverlayMaxSize := LFieldSize;
      end;
    end
    else if LCurrentOverlay > 0 then
    begin
      // Close current bit storage unit if any
      if LBitStorageSize > 0 then
      begin
        LOffset := LBitStorageOffset + (LBitStorageSize div 8);
        LBitStorageSize := 0;
        LBitPos := 0;
        LBitStorageOffset := -1;
      end;

      // Inside overlay group - all fields share the same start offset
      LEntry.RecordType.Fields[LI].FieldOffset := LOverlayStart;
      LEntry.RecordType.Fields[LI].BitOffset := 0;
      if LFieldSize > LOverlayMaxSize then
        LOverlayMaxSize := LFieldSize;
    end
    else
    begin
      // Close current bit storage unit if any
      if LBitStorageSize > 0 then
      begin
        LOffset := LBitStorageOffset + (LBitStorageSize div 8);
        LBitStorageSize := 0;
        LBitPos := 0;
        LBitStorageOffset := -1;
      end;

      // Normal field - align and advance
      if not LEntry.RecordType.IsPacked then
      begin
        if (LOffset mod LFieldAlign) <> 0 then
          LOffset := LOffset + (LFieldAlign - (LOffset mod LFieldAlign));
      end;

      LEntry.RecordType.Fields[LI].FieldOffset := LOffset;
      LEntry.RecordType.Fields[LI].BitOffset := 0;
      LOffset := LOffset + LFieldSize;
    end;
  end;

  // Close final bit storage unit if any
  if LBitStorageSize > 0 then
    LOffset := LBitStorageOffset + (LBitStorageSize div 8);

  // Close final overlay group if any
  if LCurrentOverlay > 0 then
    LOffset := LOverlayStart + LOverlayMaxSize;

  // Apply explicit alignment if specified
  if LEntry.RecordType.ExplicitAlign > 0 then
  begin
    if LEntry.RecordType.ExplicitAlign > LMaxAlign then
      LMaxAlign := LEntry.RecordType.ExplicitAlign;
  end;

  // Pad total size to alignment
  if not LEntry.RecordType.IsPacked then
  begin
    if (LOffset mod LMaxAlign) <> 0 then
      LOffset := LOffset + (LMaxAlign - (LOffset mod LMaxAlign));
  end;

  // Ensure alignment is at least explicit alignment
  if LEntry.RecordType.ExplicitAlign > LMaxAlign then
    LMaxAlign := LEntry.RecordType.ExplicitAlign;

  LEntry.RecordType.TotalSize := LOffset;
  LEntry.RecordType.Alignment := LMaxAlign;
  LEntry.RecordType.IsFinalized := True;

  FTypes[ATypeIndex] := LEntry;
end;

procedure TTigerIR.FinalizeUnionLayout(const ATypeIndex: Integer);
var
  LEntry: TIRTypeEntry;
  LI: Integer;
  LFieldSize: Integer;
  LFieldAlign: Integer;
  LMaxSize: Integer;
  LMaxAlign: Integer;
  LCurrentGroup: Integer;
  LGroupOffset: Integer;
  LGroupSize: Integer;
begin
  if (ATypeIndex < 0) or (ATypeIndex >= FTypes.Count) then
    Exit;

  LEntry := FTypes[ATypeIndex];
  if LEntry.Kind <> tkUnion then
    Exit;
  if LEntry.UnionType.IsFinalized then
    Exit;

  LMaxSize := 0;
  LMaxAlign := 1;
  LCurrentGroup := 0;
  LGroupOffset := 0;
  LGroupSize := 0;

  for LI := 0 to Length(LEntry.UnionType.Fields) - 1 do
  begin
    // Get field size and alignment
    LFieldSize := GetTypeSize(LEntry.UnionType.Fields[LI].FieldType);
    LFieldAlign := GetTypeAlignment(LEntry.UnionType.Fields[LI].FieldType);

    // Track max alignment for union overall
    if LFieldAlign > LMaxAlign then
      LMaxAlign := LFieldAlign;

    // Handle anonymous record groups
    if LEntry.UnionType.Fields[LI].AnonRecordGroup <> LCurrentGroup then
    begin
      // Closing previous group - add its size to max
      if LCurrentGroup > 0 then
      begin
        if LGroupSize > LMaxSize then
          LMaxSize := LGroupSize;
      end;

      // Starting new group
      LCurrentGroup := LEntry.UnionType.Fields[LI].AnonRecordGroup;
      LGroupOffset := 0;
      LGroupSize := 0;
    end;

    if LCurrentGroup > 0 then
    begin
      // Inside anonymous record - sequential layout within group
      // Align within group
      if (LGroupOffset mod LFieldAlign) <> 0 then
        LGroupOffset := LGroupOffset + (LFieldAlign - (LGroupOffset mod LFieldAlign));

      LEntry.UnionType.Fields[LI].FieldOffset := LGroupOffset;
      LGroupOffset := LGroupOffset + LFieldSize;
      LGroupSize := LGroupOffset;  // Group size grows
    end
    else
    begin
      // Normal union field - offset 0
      LEntry.UnionType.Fields[LI].FieldOffset := 0;
      if LFieldSize > LMaxSize then
        LMaxSize := LFieldSize;
    end;
  end;

  // Close final group if any
  if LCurrentGroup > 0 then
  begin
    if LGroupSize > LMaxSize then
      LMaxSize := LGroupSize;
  end;

  // Pad total size to alignment
  if (LMaxSize mod LMaxAlign) <> 0 then
    LMaxSize := LMaxSize + (LMaxAlign - (LMaxSize mod LMaxAlign));

  LEntry.UnionType.TotalSize := LMaxSize;
  LEntry.UnionType.Alignment := LMaxAlign;
  LEntry.UnionType.IsFinalized := True;

  FTypes[ATypeIndex] := LEntry;
end;

function TTigerIR.FindVarType(const AVarName: string): TTigerTypeRef;
var
  LFunc: TIRFunc;
  LI: Integer;
begin
  Result := TTigerTypeRef.None();

  if FCurrentFunc < 0 then
    Exit;

  LFunc := FFunctions[FCurrentFunc];
  for LI := 0 to LFunc.Vars.Count - 1 do
  begin
    if SameText(LFunc.Vars[LI].VarName, AVarName) then
    begin
      Result := LFunc.Vars[LI].VarTypeRef;
      Exit;
    end;
  end;
end;

//==============================================================================
// TTigerIR - Type Definitions
//==============================================================================

function TTigerIR.DefineRecord(const AName: string; const AIsPacked: Boolean;
  const AExplicitAlign: Integer; const ABaseTypeName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LBaseIndex: Integer;
begin
  Result := Self;

  if FBuildingRecordIndex >= 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Already building a record type');
    Exit;
  end;

  // Resolve base type if specified
  LBaseIndex := -1;
  if ABaseTypeName <> '' then
  begin
    LBaseIndex := FindType(ABaseTypeName);
    if LBaseIndex < 0 then
    begin
      if Assigned(FErrors) then
        FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown base type: %s', [ABaseTypeName]);
      Exit;
    end;
    // Verify base is a record
    if FTypes[LBaseIndex].Kind <> tkRecord then
    begin
      if Assigned(FErrors) then
        FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Base type must be a record: %s', [ABaseTypeName]);
      Exit;
    end;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkRecord;
  LEntry.RecordType.TypeName := AName;
  LEntry.RecordType.IsPacked := AIsPacked;
  LEntry.RecordType.ExplicitAlign := AExplicitAlign;
  LEntry.RecordType.BaseTypeIndex := LBaseIndex;
  LEntry.RecordType.IsFinalized := False;
  SetLength(LEntry.RecordType.Fields, 0);

  FBuildingRecordIndex := FTypes.Count;
  FTypes.Add(LEntry);
end;

function TTigerIR.Field(const AName: string; const AType: TTigerValueType): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LField: TIRRecordField;
  LLen: Integer;
begin
  Result := Self;

  LField := Default(TIRRecordField);
  LField.FieldName := AName;
  LField.FieldType := TTigerTypeRef.FromPrimitive(AType);
  LField.FieldOffset := -1;  // Computed during finalization
  LField.OverlayGroup := 0;  // Normal field by default
  LField.AnonRecordGroup := 0;  // Normal field by default

  if FBuildingRecordIndex >= 0 then
  begin
    // If inside anonymous union, mark as overlay
    if FAnonUnionFieldStart >= 0 then
      LField.OverlayGroup := FNextOverlayGroup;

    LEntry := FTypes[FBuildingRecordIndex];
    LLen := Length(LEntry.RecordType.Fields);
    SetLength(LEntry.RecordType.Fields, LLen + 1);
    LEntry.RecordType.Fields[LLen] := LField;
    FTypes[FBuildingRecordIndex] := LEntry;
  end
  else if FBuildingUnionIndex >= 0 then
  begin
    // If inside anonymous record, mark with group
    if FAnonRecordInUnion then
      LField.AnonRecordGroup := FNextAnonRecordGroup;

    LEntry := FTypes[FBuildingUnionIndex];
    LLen := Length(LEntry.UnionType.Fields);
    SetLength(LEntry.UnionType.Fields, LLen + 1);
    LEntry.UnionType.Fields[LLen] := LField;
    FTypes[FBuildingUnionIndex] := LEntry;
  end
  else
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a record or union type');
  end;
end;

function TTigerIR.Field(const AName: string; const ATypeName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LField: TIRRecordField;
  LLen: Integer;
  LTypeIndex: Integer;
begin
  Result := Self;

  if (FBuildingRecordIndex < 0) and (FBuildingUnionIndex < 0) then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a record or union type');
    Exit;
  end;

  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [ATypeName]);
    Exit;
  end;

  LField := Default(TIRRecordField);
  LField.FieldName := AName;
  LField.FieldType := TTigerTypeRef.FromComposite(LTypeIndex);
  LField.FieldOffset := -1;
  LField.OverlayGroup := 0;  // Normal field by default
  LField.AnonRecordGroup := 0;  // Normal field by default

  if FBuildingRecordIndex >= 0 then
  begin
    // If inside anonymous union, mark as overlay
    if FAnonUnionFieldStart >= 0 then
      LField.OverlayGroup := FNextOverlayGroup;

    LEntry := FTypes[FBuildingRecordIndex];
    LLen := Length(LEntry.RecordType.Fields);
    SetLength(LEntry.RecordType.Fields, LLen + 1);
    LEntry.RecordType.Fields[LLen] := LField;
    FTypes[FBuildingRecordIndex] := LEntry;
  end
  else
  begin
    // If inside anonymous record, mark with group
    if FAnonRecordInUnion then
      LField.AnonRecordGroup := FNextAnonRecordGroup;

    LEntry := FTypes[FBuildingUnionIndex];
    LLen := Length(LEntry.UnionType.Fields);
    SetLength(LEntry.UnionType.Fields, LLen + 1);
    LEntry.UnionType.Fields[LLen] := LField;
    FTypes[FBuildingUnionIndex] := LEntry;
  end;
end;

function TTigerIR.BeginRecord(): TTigerIR;
begin
  Result := Self;

  // BeginRecord inside a union = anonymous record (sequential fields)
  if FBuildingUnionIndex >= 0 then
  begin
    if FAnonRecordInUnion then
    begin
      if Assigned(FErrors) then
        FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Already inside an anonymous record');
      Exit;
    end;

    // Start new anonymous record group
    System.Inc(FNextAnonRecordGroup);
    FAnonRecordInUnion := True;
  end
  else
  begin
    // Not inside a union - error (use DefineRecord for named records)
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'BeginRecord requires an active union; use DefineRecord for named records');
  end;
end;

function TTigerIR.EndRecord(): TTigerIR;
begin
  Result := Self;

  // Check if we're closing an anonymous record inside a union
  if (FBuildingUnionIndex >= 0) and FAnonRecordInUnion then
  begin
    // Close anonymous record - reset tracking
    FAnonRecordInUnion := False;
    Exit;
  end;

  // Otherwise closing a named record
  if FBuildingRecordIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a record type');
    Exit;
  end;

  FinalizeRecordLayout(FBuildingRecordIndex);
  FBuildingRecordIndex := -1;
end;

function TTigerIR.DefineUnion(const AName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  if FBuildingUnionIndex >= 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Already building a union type');
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkUnion;
  LEntry.UnionType.TypeName := AName;
  LEntry.UnionType.IsFinalized := False;
  SetLength(LEntry.UnionType.Fields, 0);

  FBuildingUnionIndex := FTypes.Count;
  FTypes.Add(LEntry);
end;

function TTigerIR.BeginUnion(): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  // BeginUnion inside a record = anonymous union (overlay fields)
  if FBuildingRecordIndex >= 0 then
  begin
    if FAnonUnionFieldStart >= 0 then
    begin
      if Assigned(FErrors) then
        FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Already inside an anonymous union');
      Exit;
    end;

    // Start new overlay group
    System.Inc(FNextOverlayGroup);
    LEntry := FTypes[FBuildingRecordIndex];
    FAnonUnionFieldStart := Length(LEntry.RecordType.Fields);
  end
  else
  begin
    // Not inside a record - error (use DefineUnion for named unions)
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'BeginUnion requires an active record; use DefineUnion for named unions');
  end;
end;

function TTigerIR.EndUnion(): TTigerIR;
begin
  Result := Self;

  // Check if we're closing an anonymous union inside a record
  if (FBuildingRecordIndex >= 0) and (FAnonUnionFieldStart >= 0) then
  begin
    // Close anonymous union - reset tracking
    FAnonUnionFieldStart := -1;
    Exit;
  end;

  // Otherwise closing a named union
  if FBuildingUnionIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a union type');
    Exit;
  end;

  FinalizeUnionLayout(FBuildingUnionIndex);
  FBuildingUnionIndex := -1;
end;

function TTigerIR.DefineArray(const AName: string; const AElementType: TTigerValueType;
  const ALowBound: Integer; const AHighBound: Integer): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LElementSize: Integer;
begin
  Result := Self;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkFixedArray;
  LEntry.FixedArrayType.TypeName := AName;
  LEntry.FixedArrayType.ElementType := TTigerTypeRef.FromPrimitive(AElementType);
  LEntry.FixedArrayType.LowBound := ALowBound;
  LEntry.FixedArrayType.HighBound := AHighBound;

  LElementSize := GetPrimitiveSize(AElementType);
  LEntry.FixedArrayType.TotalSize := (AHighBound - ALowBound + 1) * LElementSize;

  FTypes.Add(LEntry);
end;

function TTigerIR.DefineArray(const AName: string; const AElementTypeName: string;
  const ALowBound: Integer; const AHighBound: Integer): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LTypeIndex: Integer;
  LElementSize: Integer;
begin
  Result := Self;

  LTypeIndex := FindType(AElementTypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [AElementTypeName]);
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkFixedArray;
  LEntry.FixedArrayType.TypeName := AName;
  LEntry.FixedArrayType.ElementType := TTigerTypeRef.FromComposite(LTypeIndex);
  LEntry.FixedArrayType.LowBound := ALowBound;
  LEntry.FixedArrayType.HighBound := AHighBound;

  LElementSize := GetTypeSize(TTigerTypeRef.FromComposite(LTypeIndex));
  LEntry.FixedArrayType.TotalSize := (AHighBound - ALowBound + 1) * LElementSize;

  FTypes.Add(LEntry);
end;

function TTigerIR.DefineDynArray(const AName: string; const AElementType: TTigerValueType): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkDynArray;
  LEntry.DynArrayType.TypeName := AName;
  LEntry.DynArrayType.ElementType := TTigerTypeRef.FromPrimitive(AElementType);

  FTypes.Add(LEntry);
end;

function TTigerIR.DefineDynArray(const AName: string; const AElementTypeName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LTypeIndex: Integer;
begin
  Result := Self;

  LTypeIndex := FindType(AElementTypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [AElementTypeName]);
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkDynArray;
  LEntry.DynArrayType.TypeName := AName;
  LEntry.DynArrayType.ElementType := TTigerTypeRef.FromComposite(LTypeIndex);

  FTypes.Add(LEntry);
end;

function TTigerIR.DefineEnum(const AName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  if FBuildingEnumIndex >= 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Already building an enum type');
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkEnum;
  LEntry.EnumType.TypeName := AName;
  LEntry.EnumType.BaseType := vtInt32;
  SetLength(LEntry.EnumType.Values, 0);

  FBuildingEnumIndex := FTypes.Count;
  FNextEnumOrdinal := 0;
  FTypes.Add(LEntry);
end;

function TTigerIR.EnumValue(const AName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LValue: TIREnumValue;
  LLen: Integer;
begin
  Result := Self;

  if FBuildingEnumIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building an enum type');
    Exit;
  end;

  LEntry := FTypes[FBuildingEnumIndex];

  LValue := Default(TIREnumValue);
  LValue.ValueName := AName;
  LValue.OrdinalValue := FNextEnumOrdinal;
  System.Inc(FNextEnumOrdinal);

  LLen := Length(LEntry.EnumType.Values);
  SetLength(LEntry.EnumType.Values, LLen + 1);
  LEntry.EnumType.Values[LLen] := LValue;

  FTypes[FBuildingEnumIndex] := LEntry;
end;

function TTigerIR.EnumValue(const AName: string; const AOrdinal: Int64): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LValue: TIREnumValue;
  LLen: Integer;
begin
  Result := Self;

  if FBuildingEnumIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building an enum type');
    Exit;
  end;

  LEntry := FTypes[FBuildingEnumIndex];

  LValue := Default(TIREnumValue);
  LValue.ValueName := AName;
  LValue.OrdinalValue := AOrdinal;
  FNextEnumOrdinal := AOrdinal + 1;

  LLen := Length(LEntry.EnumType.Values);
  SetLength(LEntry.EnumType.Values, LLen + 1);
  LEntry.EnumType.Values[LLen] := LValue;

  FTypes[FBuildingEnumIndex] := LEntry;
end;

function TTigerIR.EndEnum(): TTigerIR;
begin
  Result := Self;

  if FBuildingEnumIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building an enum type');
    Exit;
  end;

  FBuildingEnumIndex := -1;
  FNextEnumOrdinal := 0;
end;

function TTigerIR.DefineAlias(const AName: string; const AType: TTigerValueType): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkAlias;
  LEntry.AliasType.TypeName := AName;
  LEntry.AliasType.AliasedType := TTigerTypeRef.FromPrimitive(AType);

  FTypes.Add(LEntry);
end;

function TTigerIR.DefineAlias(const AName: string; const ATypeName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LTypeIndex: Integer;
begin
  Result := Self;

  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [ATypeName]);
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkAlias;
  LEntry.AliasType.TypeName := AName;
  LEntry.AliasType.AliasedType := TTigerTypeRef.FromComposite(LTypeIndex);

  FTypes.Add(LEntry);
end;

function TTigerIR.DefinePointer(const AName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkPointer;
  LEntry.PointerType.TypeName := AName;
  LEntry.PointerType.PointeeType := TTigerTypeRef.None();
  LEntry.PointerType.IsConst := False;

  FTypes.Add(LEntry);
end;

function TTigerIR.DefinePointer(const AName: string; const APointeeType: TTigerValueType;
  const AIsConst: Boolean): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkPointer;
  LEntry.PointerType.TypeName := AName;
  LEntry.PointerType.PointeeType := TTigerTypeRef.FromPrimitive(APointeeType);
  LEntry.PointerType.IsConst := AIsConst;

  FTypes.Add(LEntry);
end;

function TTigerIR.DefinePointer(const AName: string; const APointeeTypeName: string;
  const AIsConst: Boolean): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LTypeIndex: Integer;
begin
  Result := Self;

  LTypeIndex := FindType(APointeeTypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [APointeeTypeName]);
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkPointer;
  LEntry.PointerType.TypeName := AName;
  LEntry.PointerType.PointeeType := TTigerTypeRef.FromComposite(LTypeIndex);
  LEntry.PointerType.IsConst := AIsConst;

  FTypes.Add(LEntry);
end;

//==============================================================================
// TTigerIR - Routine (Procedural) Type Definition
//==============================================================================

function TTigerIR.DefineRoutine(const AName: string; const ALinkage: TTigerLinkage): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkRoutine;
  LEntry.RoutineType.TypeName := AName;
  LEntry.RoutineType.ReturnType := TTigerTypeRef.FromPrimitive(vtVoid);  // Default return type
  LEntry.RoutineType.Linkage := ALinkage;
  LEntry.RoutineType.IsVarArgs := False;

  FBuildingRoutineIndex := FTypes.Count;
  FTypes.Add(LEntry);
end;

function TTigerIR.RoutineParam(const AType: TTigerValueType): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LLen: Integer;
begin
  Result := Self;

  if FBuildingRoutineIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a routine type');
    Exit;
  end;

  LEntry := FTypes[FBuildingRoutineIndex];
  LLen := Length(LEntry.RoutineType.ParamTypes);
  SetLength(LEntry.RoutineType.ParamTypes, LLen + 1);
  LEntry.RoutineType.ParamTypes[LLen] := TTigerTypeRef.FromPrimitive(AType);
  FTypes[FBuildingRoutineIndex] := LEntry;
end;

function TTigerIR.RoutineParam(const ATypeName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LTypeIndex: Integer;
  LLen: Integer;
begin
  Result := Self;

  if FBuildingRoutineIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a routine type');
    Exit;
  end;

  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [ATypeName]);
    Exit;
  end;

  LEntry := FTypes[FBuildingRoutineIndex];
  LLen := Length(LEntry.RoutineType.ParamTypes);
  SetLength(LEntry.RoutineType.ParamTypes, LLen + 1);
  LEntry.RoutineType.ParamTypes[LLen] := TTigerTypeRef.FromComposite(LTypeIndex);
  FTypes[FBuildingRoutineIndex] := LEntry;
end;

function TTigerIR.RoutineReturns(const AType: TTigerValueType): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  if FBuildingRoutineIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a routine type');
    Exit;
  end;

  LEntry := FTypes[FBuildingRoutineIndex];
  LEntry.RoutineType.ReturnType := TTigerTypeRef.FromPrimitive(AType);
  FTypes[FBuildingRoutineIndex] := LEntry;
end;

function TTigerIR.RoutineReturns(const ATypeName: string): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LTypeIndex: Integer;
begin
  Result := Self;

  if FBuildingRoutineIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a routine type');
    Exit;
  end;

  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [ATypeName]);
    Exit;
  end;

  LEntry := FTypes[FBuildingRoutineIndex];
  LEntry.RoutineType.ReturnType := TTigerTypeRef.FromComposite(LTypeIndex);
  FTypes[FBuildingRoutineIndex] := LEntry;
end;

function TTigerIR.RoutineVarArgs(): TTigerIR;
var
  LEntry: TIRTypeEntry;
begin
  Result := Self;

  if FBuildingRoutineIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a routine type');
    Exit;
  end;

  LEntry := FTypes[FBuildingRoutineIndex];
  LEntry.RoutineType.IsVarArgs := True;
  FTypes[FBuildingRoutineIndex] := LEntry;
end;

function TTigerIR.EndRoutine(): TTigerIR;
begin
  Result := Self;

  if FBuildingRoutineIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Not building a routine type');
    Exit;
  end;

  FBuildingRoutineIndex := -1;
end;

function TTigerIR.DefineSet(const AName: string): TTigerIR;
begin
  // Bare "set" is alias for set of 0..63
  Result := DefineSet(AName, 0, 63);
end;

function TTigerIR.DefineSet(const AName: string; const ALow: Integer; const AHigh: Integer): TTigerIR;
var
  LEntry: TIRTypeEntry;
  LSpan: Integer;
begin
  Result := Self;

  // Validate range
  if AHigh < ALow then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Invalid set range: %d..%d', [ALow, AHigh]));
    Exit;
  end;

  LSpan := AHigh - ALow + 1;
  if LSpan > 64 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Set range too large: %d elements (max 64)', [LSpan]));
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkSet;
  LEntry.SetType.TypeName := AName;
  LEntry.SetType.LowBound := ALow;
  LEntry.SetType.HighBound := AHigh;
  LEntry.SetType.BaseType := TTigerTypeRef.None();

  // Compute storage size based on span
  if LSpan <= 8 then
    LEntry.SetType.StorageSize := 1
  else if LSpan <= 16 then
    LEntry.SetType.StorageSize := 2
  else if LSpan <= 32 then
    LEntry.SetType.StorageSize := 4
  else
    LEntry.SetType.StorageSize := 8;

  FTypes.Add(LEntry);
end;

function TTigerIR.DefineSet(const AName: string; const AEnumTypeName: string): TTigerIR;
var
  LEnumIndex: Integer;
  LEnumEntry: TIRTypeEntry;
  LMinOrd: System.Int64;
  LMaxOrd: System.Int64;
  LI: Integer;
  LEntry: TIRTypeEntry;
  LSpan: Integer;
begin
  Result := Self;

  // Find the enum type
  LEnumIndex := FindType(AEnumTypeName);
  if LEnumIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Unknown enum type: %s', [AEnumTypeName]));
    Exit;
  end;

  LEnumEntry := FTypes[LEnumIndex];
  if LEnumEntry.Kind <> tkEnum then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Type is not an enum: %s', [AEnumTypeName]));
    Exit;
  end;

  // Find min/max ordinal values in the enum
  if Length(LEnumEntry.EnumType.Values) = 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Enum has no values: %s', [AEnumTypeName]));
    Exit;
  end;

  LMinOrd := LEnumEntry.EnumType.Values[0].OrdinalValue;
  LMaxOrd := LMinOrd;
  for LI := 1 to System.High(LEnumEntry.EnumType.Values) do
  begin
    if LEnumEntry.EnumType.Values[LI].OrdinalValue < LMinOrd then
      LMinOrd := LEnumEntry.EnumType.Values[LI].OrdinalValue;
    if LEnumEntry.EnumType.Values[LI].OrdinalValue > LMaxOrd then
      LMaxOrd := LEnumEntry.EnumType.Values[LI].OrdinalValue;
  end;

  LSpan := LMaxOrd - LMinOrd + 1;
  if LSpan > 64 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Enum range too large for set: %d elements (max 64)', [LSpan]));
    Exit;
  end;

  LEntry := Default(TIRTypeEntry);
  LEntry.Kind := tkSet;
  LEntry.SetType.TypeName := AName;
  LEntry.SetType.LowBound := LMinOrd;
  LEntry.SetType.HighBound := LMaxOrd;
  LEntry.SetType.BaseType := TTigerTypeRef.FromComposite(LEnumIndex);

  // Compute storage size based on span
  if LSpan <= 8 then
    LEntry.SetType.StorageSize := 1
  else if LSpan <= 16 then
    LEntry.SetType.StorageSize := 2
  else if LSpan <= 32 then
    LEntry.SetType.StorageSize := 4
  else
    LEntry.SetType.StorageSize := 8;

  FTypes.Add(LEntry);
end;

function TTigerIR.FindType(const AName: string): Integer;
var
  LI: Integer;
  LEntry: TIRTypeEntry;
begin
  for LI := 0 to FTypes.Count - 1 do
  begin
    LEntry := FTypes[LI];
    case LEntry.Kind of
      tkRecord:     if SameText(LEntry.RecordType.TypeName, AName) then Exit(LI);
      tkUnion:      if SameText(LEntry.UnionType.TypeName, AName) then Exit(LI);
      tkFixedArray: if SameText(LEntry.FixedArrayType.TypeName, AName) then Exit(LI);
      tkDynArray:   if SameText(LEntry.DynArrayType.TypeName, AName) then Exit(LI);
      tkEnum:       if SameText(LEntry.EnumType.TypeName, AName) then Exit(LI);
      tkAlias:      if SameText(LEntry.AliasType.TypeName, AName) then Exit(LI);
      tkPointer:    if SameText(LEntry.PointerType.TypeName, AName) then Exit(LI);
      tkRoutine:    if SameText(LEntry.RoutineType.TypeName, AName) then Exit(LI);
      tkSet:        if SameText(LEntry.SetType.TypeName, AName) then Exit(LI);
    end;
  end;
  Result := -1;
end;

function TTigerIR.GetTypeSize(const ATypeRef: TTigerTypeRef): Integer;
var
  LEntry: TIRTypeEntry;
begin
  if ATypeRef.IsPrimitive then
    Exit(GetPrimitiveSize(ATypeRef.Primitive));

  if (ATypeRef.TypeIndex < 0) or (ATypeRef.TypeIndex >= FTypes.Count) then
    Exit(0);

  LEntry := FTypes[ATypeRef.TypeIndex];
  case LEntry.Kind of
    tkRecord:
      begin
        if not LEntry.RecordType.IsFinalized then
          FinalizeRecordLayout(ATypeRef.TypeIndex);
        Result := FTypes[ATypeRef.TypeIndex].RecordType.TotalSize;
      end;
    tkUnion:
      begin
        if not LEntry.UnionType.IsFinalized then
          FinalizeUnionLayout(ATypeRef.TypeIndex);
        Result := FTypes[ATypeRef.TypeIndex].UnionType.TotalSize;
      end;
    tkFixedArray:
      Result := LEntry.FixedArrayType.TotalSize;
    tkDynArray:
      Result := 8;  // Pointer size
    tkEnum:
      Result := GetPrimitiveSize(LEntry.EnumType.BaseType);
    tkAlias:
      Result := GetTypeSize(LEntry.AliasType.AliasedType);
    tkPointer:
      Result := 8;  // Pointer size on Win64
    tkRoutine:
      Result := 8;  // Function pointer size on Win64
    tkSet:
      Result := LEntry.SetType.StorageSize;
  else
    Result := 0;
  end;
end;

function TTigerIR.GetTypeAlignment(const ATypeRef: TTigerTypeRef): Integer;
var
  LEntry: TIRTypeEntry;
begin
  if ATypeRef.IsPrimitive then
    Exit(GetPrimitiveAlignment(ATypeRef.Primitive));

  if (ATypeRef.TypeIndex < 0) or (ATypeRef.TypeIndex >= FTypes.Count) then
    Exit(1);

  LEntry := FTypes[ATypeRef.TypeIndex];
  case LEntry.Kind of
    tkRecord:
      begin
        if not LEntry.RecordType.IsFinalized then
          FinalizeRecordLayout(ATypeRef.TypeIndex);
        Result := FTypes[ATypeRef.TypeIndex].RecordType.Alignment;
      end;
    tkUnion:
      begin
        if not LEntry.UnionType.IsFinalized then
          FinalizeUnionLayout(ATypeRef.TypeIndex);
        Result := FTypes[ATypeRef.TypeIndex].UnionType.Alignment;
      end;
    tkFixedArray:
      Result := GetTypeAlignment(LEntry.FixedArrayType.ElementType);
    tkDynArray:
      Result := 8;  // Pointer alignment
    tkEnum:
      Result := GetPrimitiveAlignment(LEntry.EnumType.BaseType);
    tkAlias:
      Result := GetTypeAlignment(LEntry.AliasType.AliasedType);
    tkPointer:
      Result := 8;  // Pointer alignment on Win64
    tkRoutine:
      Result := 8;  // Function pointer alignment on Win64
    tkSet:
      Result := LEntry.SetType.StorageSize;  // Alignment matches storage size
  else
    Result := 1;
  end;
end;

function TTigerIR.TypeRef(const AName: string): TTigerTypeRef;
var
  LIndex: Integer;
begin
  LIndex := FindType(AName);
  if LIndex >= 0 then
    Result := TTigerTypeRef.FromComposite(LIndex)
  else
    Result := TTigerTypeRef.None();
end;

function TTigerIR.GetTypeCount(): Integer;
begin
  Result := FTypes.Count;
end;

function TTigerIR.GetTypeEntry(const AIndex: Integer): TIRTypeEntry;
begin
  Result := FTypes[AIndex];
end;

function TTigerIR.IsStringType(const ATypeRef: TTigerTypeRef): Boolean;
var
  LEntry: TIRTypeEntry;
  LTypeName: string;
begin
  Result := False;

  // Primitives are never string types
  if ATypeRef.IsPrimitive then
    Exit;

  // Check composite type
  if (ATypeRef.TypeIndex < 0) or (ATypeRef.TypeIndex >= FTypes.Count) then
    Exit;

  LEntry := FTypes[ATypeRef.TypeIndex];

  // Get the type name based on kind
  case LEntry.Kind of
    TIRTypeKind.tkRecord:
      LTypeName := LEntry.RecordType.TypeName;
    TIRTypeKind.tkPointer:
      LTypeName := LEntry.PointerType.TypeName;
    TIRTypeKind.tkAlias:
      begin
        LTypeName := LEntry.AliasType.TypeName;
        // Also check if aliased type is string
        if not SameText(LTypeName, 'string') then
          Result := IsStringType(LEntry.AliasType.AliasedType);
        Exit;
      end;
  else
    Exit;
  end;

  Result := SameText(LTypeName, 'string');
end;

//==============================================================================
// TTigerIR - Import
//==============================================================================

function TTigerIR.Import(
  const ADllName: string;
  const AFuncName: string;
  const AParams: array of TTigerValueType;
  const AReturn: TTigerValueType;
  const AVarArgs: Boolean;
  const ALinkage: TTigerLinkage
): TTigerIR;
var
  LImport: TIRImport;
  LI: Integer;
begin
  Result := Self;

  // Check for duplicate - skip if already imported
  for LI := 0 to FImports.Count - 1 do
  begin
    if SameText(FImports[LI].DllName, ADllName) and
       SameText(FImports[LI].FuncName, AFuncName) then
      Exit;
  end;

  LImport := Default(TIRImport);
  LImport.DllName := ADllName;
  LImport.FuncName := AFuncName;
  LImport.ReturnType := AReturn;
  LImport.IsVarArgs := AVarArgs;
  LImport.Linkage := ALinkage;

  SetLength(LImport.ParamTypes, Length(AParams));
  for LI := 0 to System.High(AParams) do
    LImport.ParamTypes[LI] := AParams[LI];

  FImports.Add(LImport);
end;

function TTigerIR.ImportLib(
  const ALibName: string;
  const AFuncName: string;
  const AParams: array of TTigerValueType;
  const AReturn: TTigerValueType;
  const AVarArgs: Boolean;
  const ALinkage: TTigerLinkage
): TTigerIR;
var
  LImport: TIRImport;
  LI: Integer;
begin
  Result := Self;

  // Check for duplicate - skip if already imported
  for LI := 0 to FImports.Count - 1 do
  begin
    if FImports[LI].IsStatic and
       SameText(FImports[LI].DllName, ALibName) and
       SameText(FImports[LI].FuncName, AFuncName) then
      Exit;
  end;

  LImport := Default(TIRImport);
  LImport.DllName := ALibName;
  LImport.FuncName := AFuncName;
  LImport.ReturnType := AReturn;
  LImport.IsVarArgs := AVarArgs;
  LImport.IsStatic := True;
  LImport.Linkage := ALinkage;

  SetLength(LImport.ParamTypes, Length(AParams));
  for LI := 0 to System.High(AParams) do
    LImport.ParamTypes[LI] := AParams[LI];

  FImports.Add(LImport);
end;

//==============================================================================
// TTigerIR - Global Variables
//==============================================================================

function TTigerIR.Global(const AName: string; const AType: TTigerValueType): TTigerIR;
var
  LGlobal: TIRGlobal;
begin
  LGlobal := Default(TIRGlobal);
  LGlobal.GlobalName := AName;
  LGlobal.GlobalType := AType;
  LGlobal.GlobalTypeRef := TTigerTypeRef.FromPrimitive(AType);
  LGlobal.InitExpr := -1;  // Uninitialized

  FGlobals.Add(LGlobal);
  Result := Self;
end;

function TTigerIR.Global(const AName: string; const AType: TTigerValueType; const AInit: TTigerIRExpr): TTigerIR;
var
  LGlobal: TIRGlobal;
begin
  LGlobal := Default(TIRGlobal);
  LGlobal.GlobalName := AName;
  LGlobal.GlobalType := AType;
  LGlobal.GlobalTypeRef := TTigerTypeRef.FromPrimitive(AType);
  LGlobal.InitExpr := AInit.Index;

  FGlobals.Add(LGlobal);
  Result := Self;
end;

function TTigerIR.Global(const AName: string; const ATypeRef: TTigerTypeRef): TTigerIR;
var
  LGlobal: TIRGlobal;
begin
  LGlobal := Default(TIRGlobal);
  LGlobal.GlobalName := AName;
  LGlobal.GlobalType := vtPointer;  // Managed types are pointers
  LGlobal.GlobalTypeRef := ATypeRef;
  LGlobal.InitExpr := -1;  // Uninitialized

  FGlobals.Add(LGlobal);
  Result := Self;
end;

function TTigerIR.Global(const AName: string; const ATypeName: string): TTigerIR;
var
  LGlobal: TIRGlobal;
  LTypeIndex: Integer;
begin
  Result := Self;

  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [ATypeName]);
    Exit;
  end;

  LGlobal := Default(TIRGlobal);
  LGlobal.GlobalName := AName;
  LGlobal.GlobalType := vtPointer;  // Managed/composite types are pointers
  LGlobal.GlobalTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
  LGlobal.InitExpr := -1;  // Uninitialized

  FGlobals.Add(LGlobal);
end;

//==============================================================================
// TTigerIR - Function Definition
//==============================================================================

function TTigerIR.Func(
  const AName: string;
  const AReturnType: TTigerValueType;
  const AIsEntryPoint: Boolean;
  const ALinkage: TTigerLinkage;
  const AIsPublic: Boolean
): TTigerIR;
var
  LFunc: TIRFunc;
begin
  LFunc := Default(TIRFunc);
  LFunc.FuncName := AName;
  LFunc.ReturnType := AReturnType;
  LFunc.IsEntryPoint := AIsEntryPoint;
  LFunc.IsDllEntry := False;
  LFunc.IsPublic := AIsPublic;
  LFunc.Linkage := ALinkage;
  LFunc.IsVariadic := False;
  LFunc.Vars := TList<TIRVar>.Create();
  LFunc.Stmts := TList<TIRStmt>.Create();

  FCurrentFunc := FFunctions.Count;
  FFunctions.Add(LFunc);
  Result := Self;
end;

function TTigerIR.OverloadFunc(
  const AName: string;
  const AReturnType: TTigerValueType;
  const AIsEntryPoint: Boolean;
  const AIsPublic: Boolean
): TTigerIR;
var
  LFunc: TIRFunc;
  LI: Integer;
begin
  Result := Self;

  // Validate: existing function(s) with same name must not have C linkage
  for LI := 0 to FFunctions.Count - 1 do
  begin
    if SameText(FFunctions[LI].FuncName, AName) then
    begin
      if FFunctions[LI].Linkage = plC then
      begin
        if Assigned(FErrors) then
          FErrors.Add(esError, ERR_IR_OVERLOAD_C_LINKAGE,
            'Cannot overload C-linked function: %s', [AName]);
        Exit;
      end;
    end;
  end;

  // Create function with forced C++ linkage
  LFunc := Default(TIRFunc);
  LFunc.FuncName := AName;
  LFunc.ReturnType := AReturnType;
  LFunc.IsEntryPoint := AIsEntryPoint;
  LFunc.IsDllEntry := False;
  LFunc.IsPublic := AIsPublic;
  LFunc.Linkage := plDefault;  // C++ linkage - forced, no parameter
  LFunc.IsVariadic := False;
  LFunc.Vars := TList<TIRVar>.Create();
  LFunc.Stmts := TList<TIRStmt>.Create();

  FCurrentFunc := FFunctions.Count;
  FFunctions.Add(LFunc);
end;

function TTigerIR.VariadicFunc(
  const AName: string;
  const AReturnType: TTigerValueType;
  const AIsEntryPoint: Boolean;
  const AIsPublic: Boolean
): TTigerIR;
var
  LFunc: TIRFunc;
  LI: Integer;
begin
  Result := Self;

  // Validate: no existing function with same name (variadics cannot be overloaded)
  for LI := 0 to FFunctions.Count - 1 do
  begin
    if SameText(FFunctions[LI].FuncName, AName) then
    begin
      if Assigned(FErrors) then
        FErrors.Add(esError, ERR_IR_VARIADIC_OVERLOAD,
          'Variadic function cannot be overloaded: %s', [AName]);
      Exit;
    end;
  end;

  // Create variadic function
  LFunc := Default(TIRFunc);
  LFunc.FuncName := AName;
  LFunc.ReturnType := AReturnType;
  LFunc.IsEntryPoint := AIsEntryPoint;
  LFunc.IsDllEntry := False;
  LFunc.IsPublic := AIsPublic;
  LFunc.Linkage := plC;  // Variadic uses C-style linkage (no mangling)
  LFunc.IsVariadic := True;
  LFunc.Vars := TList<TIRVar>.Create();
  LFunc.Stmts := TList<TIRStmt>.Create();

  FCurrentFunc := FFunctions.Count;
  FFunctions.Add(LFunc);
end;

function TTigerIR.DllMain(): TTigerIR;
var
  LFunc: TIRFunc;
begin
  // DllMain signature: (HINSTANCE, DWORD, LPVOID) -> BOOL
  // In our types: (vtPointer, vtUInt32, vtPointer) -> vtInt32
  LFunc := Default(TIRFunc);
  LFunc.FuncName := 'DllMain';
  LFunc.ReturnType := vtInt32;  // BOOL
  LFunc.IsEntryPoint := False;  // Not the EXE entry point
  LFunc.IsDllEntry := True;     // This IS the DLL entry point
  LFunc.IsPublic := False;      // Not exported (OS calls it directly)
  LFunc.Linkage := plC;         // C linkage
  LFunc.IsVariadic := False;
  LFunc.Vars := TList<TIRVar>.Create();
  LFunc.Stmts := TList<TIRStmt>.Create();

  FCurrentFunc := FFunctions.Count;
  FFunctions.Add(LFunc);

  // Add standard DllMain parameters
  Param('hinstDLL', vtPointer);   // HINSTANCE
  Param('fdwReason', vtUInt32);   // DWORD
  Param('lpvReserved', vtPointer); // LPVOID

  Result := Self;
end;

function TTigerIR.Param(const AName: string; const AType: TTigerValueType): TTigerIR;
var
  LVar: TIRVar;
  LFunc: TIRFunc;
  LLen: Integer;
begin
  LFunc := GetCurrentFunc();

  LVar := Default(TIRVar);
  LVar.VarName := AName;
  LVar.VarTypeRef := TTigerTypeRef.FromPrimitive(AType);
  LVar.IsParam := True;

  LFunc.Vars.Add(LVar);

  // Track param types for mangling
  LLen := Length(LFunc.ParamTypes);
  SetLength(LFunc.ParamTypes, LLen + 1);
  LFunc.ParamTypes[LLen] := AType;

  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.Local(const AName: string; const AType: TTigerValueType): TTigerIR;
var
  LVar: TIRVar;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LVar := Default(TIRVar);
  LVar.VarName := AName;
  LVar.VarTypeRef := TTigerTypeRef.FromPrimitive(AType);
  LVar.IsParam := False;

  LFunc.Vars.Add(LVar);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.Local(const AName: string; const ATypeName: string): TTigerIR;
var
  LVar: TIRVar;
  LFunc: TIRFunc;
  LTypeIndex: Integer;
begin
  Result := Self;
  LFunc := GetCurrentFunc();

  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Unknown type: %s', [ATypeName]);
    Exit;
  end;

  LVar := Default(TIRVar);
  LVar.VarName := AName;
  LVar.VarTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
  LVar.IsParam := False;

  LFunc.Vars.Add(LVar);
  FFunctions[FCurrentFunc] := LFunc;
end;

function TTigerIR.EndFunc(): TTigerIR;
begin
  FCurrentFunc := -1;
  Result := Self;
end;

//==============================================================================
// TTigerIR - Statements
//==============================================================================

function TTigerIR.Assign(const ADest: string; const AValue: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skAssign;
  LStmt.DestVar := ADest;
  LStmt.DestExpr := -1;  // Not using expression destination
  LStmt.Expr := AValue.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.AssignTo(const ADest: TTigerIRExpr; const AValue: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skAssign;
  LStmt.DestVar := '';
  LStmt.DestExpr := ADest.Index;  // Use expression destination (field/index)
  LStmt.Expr := AValue.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.Call(const AFuncName: string): TTigerIR;
begin
  Result := Call(AFuncName, []);
end;

function TTigerIR.Call(const AFuncName: string; const AArgs: array of TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
  LI: Integer;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCall;
  LStmt.CallTarget := AFuncName;

  SetLength(LStmt.CallArgs, Length(AArgs));
  for LI := 0 to System.High(AArgs) do
    LStmt.CallArgs[LI] := AArgs[LI].Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.CallAssign(const ADest: string; const AFuncName: string; const AArgs: array of TTigerIRExpr): TTigerIR;
var
  LCallExpr: TTigerIRExpr;
begin
  LCallExpr := Invoke(AFuncName, AArgs);
  Result := Assign(ADest, LCallExpr);
end;

function TTigerIR.Return(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skReturn;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.Return(const AValue: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skReturnValue;
  LStmt.Expr := AValue.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.CallIndirect(const AFuncPtr: TTigerIRExpr): TTigerIR;
begin
  Result := CallIndirect(AFuncPtr, []);
end;

function TTigerIR.CallIndirect(const AFuncPtr: TTigerIRExpr; const AArgs: array of TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
  LI: Integer;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCallIndirect;
  LStmt.IndirectTarget := AFuncPtr.Index;

  SetLength(LStmt.CallArgs, Length(AArgs));
  for LI := 0 to System.High(AArgs) do
    LStmt.CallArgs[LI] := AArgs[LI].Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.CallIndirectAssign(const ADest: string; const AFuncPtr: TTigerIRExpr;
  const AArgs: array of TTigerIRExpr): TTigerIR;
var
  LCallExpr: TTigerIRExpr;
begin
  LCallExpr := InvokeIndirect(AFuncPtr, AArgs);
  Result := Assign(ADest, LCallExpr);
end;

//==============================================================================
// TTigerIR - Control Flow
//==============================================================================

function TTigerIR.&If(const ACond: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skIfBegin;
  LStmt.Expr := ACond.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Else(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skElseBegin;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.EndIf(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skIfEnd;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&While(const ACond: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skWhileBegin;
  LStmt.Expr := ACond.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.EndWhile(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skWhileEnd;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&For(const AVar: string; const AFrom: TTigerIRExpr; const ATo: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skForBegin;
  LStmt.ForVar := AVar;
  LStmt.ForFrom := AFrom.Index;
  LStmt.ForTo := ATo.Index;
  LStmt.ForDownTo := False;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.ForDownTo(const AVar: string; const AFrom: TTigerIRExpr; const ATo: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skForBegin;
  LStmt.ForVar := AVar;
  LStmt.ForFrom := AFrom.Index;
  LStmt.ForTo := ATo.Index;
  LStmt.ForDownTo := True;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.EndFor(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skForEnd;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Repeat(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skRepeatBegin;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Until(const ACond: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skRepeatEnd;
  LStmt.Expr := ACond.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

//==============================================================================
// TTigerIR - Case Statement
//==============================================================================

function TTigerIR.&Case(const ASelector: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCaseBegin;
  LStmt.Expr := ASelector.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.CaseOf(const AValues: array of TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
  LI: Integer;
  LExpr: TIRExprNode;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCaseOf;

  // Extract constant values from expressions
  SetLength(LStmt.CaseValues, Length(AValues));
  for LI := 0 to System.High(AValues) do
  begin
    if (AValues[LI].Index >= 0) and (AValues[LI].Index < FExpressions.Count) then
    begin
      LExpr := FExpressions[AValues[LI].Index];
      if LExpr.Kind = ekConstInt then
        LStmt.CaseValues[LI] := LExpr.ConstInt
      else
        LStmt.CaseValues[LI] := 0;  // Non-constant expression - error case
    end
    else
      LStmt.CaseValues[LI] := 0;
  end;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.CaseOf(const AValues: array of Integer): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
  LI: Integer;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCaseOf;

  // Store raw integer values directly (these are case match values, not expression indices)
  SetLength(LStmt.CaseValues, Length(AValues));
  for LI := 0 to System.High(AValues) do
    LStmt.CaseValues[LI] := AValues[LI];

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.CaseElse(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCaseElse;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.EndCase(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skCaseEnd;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Try(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skTryBegin;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Except(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skExceptBegin;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Finally(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skFinallyBegin;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.EndTry(): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skTryEnd;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Raise(const AMsg: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skRaise;
  LStmt.RaiseMsg := AMsg.Index;
  LStmt.RaiseCode := -1;  // No code for simple raise

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.RaiseCode(const ACode: TTigerIRExpr; const AMsg: TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skRaiseCode;
  LStmt.RaiseCode := ACode.Index;
  LStmt.RaiseMsg := AMsg.Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

function TTigerIR.&Inc(const AVarName: string): TTigerIR;
begin
  // Inc(x) = x := x + 1
  Result := Assign(AVarName, Add(Get(AVarName), Int64(1)));
end;

function TTigerIR.&Inc(const AVarName: string; const AAmount: TTigerIRExpr): TTigerIR;
begin
  // Inc(x, n) = x := x + n
  Result := Assign(AVarName, Add(Get(AVarName), AAmount));
end;

function TTigerIR.&Dec(const AVarName: string): TTigerIR;
begin
  // Dec(x) = x := x - 1
  Result := Assign(AVarName, Sub(Get(AVarName), Int64(1)));
end;

function TTigerIR.&Dec(const AVarName: string; const AAmount: TTigerIRExpr): TTigerIR;
begin
  // Dec(x, n) = x := x - n
  Result := Assign(AVarName, Sub(Get(AVarName), AAmount));
end;

//==============================================================================
// TTigerIR - Expressions: Literals
//==============================================================================

function TTigerIR.Str(const AValue: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LStr: TIRString;
begin
  // Add string to table
  LStr := Default(TIRString);
  LStr.Value := AValue;
  LStr.IsWide := False;
  FStrings.Add(LStr);

  // Create expression
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstString;
  LNode.StringIndex := FStrings.Count - 1;
  Result := AddExpr(LNode);
end;

function TTigerIR.WStr(const AValue: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LStr: TIRString;
begin
  // Add wide string to table
  LStr := Default(TIRString);
  LStr.Value := AValue;
  LStr.IsWide := True;
  FStrings.Add(LStr);

  // Create expression
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstString;
  LNode.StringIndex := FStrings.Count - 1;
  Result := AddExpr(LNode);
end;

function TTigerIR.&Int64(const AValue: Int64): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  // Int64 literal
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt64);
  Result := AddExpr(LNode);
end;

function TTigerIR.Int32(const AValue: Int32): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt32);
  Result := AddExpr(LNode);
end;


function TTigerIR.Float64(const AValue: Double): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstFloat;
  LNode.ConstFloat := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtFloat64);
  Result := AddExpr(LNode);
end;

function TTigerIR.Bool(const AValue: Boolean): TTigerIRExpr;
begin
  if AValue then
    Result := Int64(1)
  else
    Result := Int64(0);
end;

function TTigerIR.Int8(const AValue: Int8): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt8);
  Result := AddExpr(LNode);
end;

function TTigerIR.Int16(const AValue: Int16): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt16);
  Result := AddExpr(LNode);
end;

function TTigerIR.UInt8(const AValue: UInt8): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtUInt8);
  Result := AddExpr(LNode);
end;

function TTigerIR.UInt16(const AValue: UInt16): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtUInt16);
  Result := AddExpr(LNode);
end;

function TTigerIR.UInt32(const AValue: UInt32): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtUInt32);
  Result := AddExpr(LNode);
end;

function TTigerIR.UInt64(const AValue: UInt64): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := System.Int64(AValue);
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtUInt64);
  Result := AddExpr(LNode);
end;

function TTigerIR.Float32(const AValue: Single): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstFloat;
  LNode.ConstFloat := AValue;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtFloat32);
  Result := AddExpr(LNode);
end;

function TTigerIR.Null(): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekConstInt;
  LNode.ConstInt := 0;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtPointer);
  Result := AddExpr(LNode);
end;

//==============================================================================
// TTigerIR - Expressions: Variable Reference
//==============================================================================

function TTigerIR.Get(const AName: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekVariable;
  LNode.VarName := AName;
  LNode.ResultType := FindVarType(AName);  // Resolve type at creation time
  Result := AddExpr(LNode);
end;

//==============================================================================
// TTigerIR - Expressions: Arithmetic
//==============================================================================

function TTigerIR.Add(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opAdd);
end;

function TTigerIR.Sub(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSub);
end;

function TTigerIR.Mul(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opMul);
end;

function TTigerIR.IDiv(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opDiv);
end;

function TTigerIR.IMod(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opMod);
end;

function TTigerIR.Neg(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeUnaryExpr(AValue, opNeg);
end;

//==============================================================================
// TTigerIR - Expressions: Bitwise
//==============================================================================

function TTigerIR.BitAnd(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opBitAnd);
end;

function TTigerIR.BitOr(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opBitOr);
end;

function TTigerIR.BitXor(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opBitXor);
end;

function TTigerIR.BitNot(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeUnaryExpr(AValue, opBitNot);
end;

function TTigerIR.&Shl(const AValue: TTigerIRExpr; const ACount: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(AValue, ACount, opShl);
end;

function TTigerIR.&Shr(const AValue: TTigerIRExpr; const ACount: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(AValue, ACount, opShr);
end;

//==============================================================================
// TTigerIR - Expressions: Comparison
//==============================================================================

function TTigerIR.Eq(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opCmpEq);
end;

function TTigerIR.Ne(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opCmpNe);
end;

function TTigerIR.Lt(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opCmpLt);
end;

function TTigerIR.Le(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opCmpLe);
end;

function TTigerIR.Gt(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opCmpGt);
end;

function TTigerIR.Ge(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opCmpGe);
end;

//==============================================================================
// TTigerIR - Expressions: Logical
//==============================================================================

function TTigerIR.LogAnd(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opAnd);
end;

function TTigerIR.LogOr(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opOr);
end;

function TTigerIR.LogNot(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeUnaryExpr(AValue, opNot);
end;

//==============================================================================
// TTigerIR - Expressions: Pointers
//==============================================================================

function TTigerIR.AddrOf(const AName: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekUnary;
  LNode.Op := opAddrOf;
  LNode.VarName := AName;
  LNode.Left := -1;
  Result := AddExpr(LNode);
end;

function TTigerIR.AddrOfVal(const AExpr: TTigerIRExpr): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekUnary;
  LNode.Op := opAddrOf;
  LNode.Left := AExpr.Index;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtPointer);
  Result := AddExpr(LNode);
end;

function TTigerIR.Deref(const APtr: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeUnaryExpr(APtr, opDeref);
end;

function TTigerIR.Deref(const APtr: TTigerIRExpr; const ATypeName: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LTypeIndex: Integer;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekUnary;
  LNode.Op := opDeref;
  LNode.Left := APtr.Index;
  LNode.Right := -1;

  // Resolve the type name to set ResultType
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex >= 0 then
    LNode.ResultType := TTigerTypeRef.FromComposite(LTypeIndex)
  else
    LNode.ResultType := TTigerTypeRef.None();

  Result := AddExpr(LNode);
end;

function TTigerIR.Deref(const APtr: TTigerIRExpr; const AType: TTigerValueType): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekUnary;
  LNode.Op := opDeref;
  LNode.Left := APtr.Index;
  LNode.Right := -1;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(AType);
  Result := AddExpr(LNode);
end;

//==============================================================================
// TTigerIR - Expressions: Function Pointers
//==============================================================================

function TTigerIR.FuncAddr(const AFuncName: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekFuncAddr;
  LNode.FuncAddrName := AFuncName;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtPointer);  // Function pointer is pointer-sized
  Result := AddExpr(LNode);
end;

function TTigerIR.InvokeIndirect(const AFuncPtr: TTigerIRExpr; const AArgs: array of TTigerIRExpr): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LI: Integer;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekCallIndirect;
  LNode.IndirectTarget := AFuncPtr.Index;

  SetLength(LNode.CallArgs, Length(AArgs));
  for LI := 0 to System.High(AArgs) do
    LNode.CallArgs[LI] := AArgs[LI].Index;

  Result := AddExpr(LNode);
end;

//==============================================================================
// TTigerIR - Expressions: Composite Type Access
//==============================================================================

function TTigerIR.GetField(const AObject: TTigerIRExpr; const AFieldName: string): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LObjectNode: TIRExprNode;
  LFieldInfo: TIRRecordField;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekFieldAccess;
  LNode.ObjectExpr := AObject.Index;
  LNode.FieldName := AFieldName;

  // Get object expression's type and resolve field
  if (AObject.Index >= 0) and (AObject.Index < FExpressions.Count) then
  begin
    LObjectNode := FExpressions[AObject.Index];
    if not LObjectNode.ResultType.IsPrimitive and (LObjectNode.ResultType.TypeIndex >= 0) then
    begin
      if FindRecordField(LObjectNode.ResultType.TypeIndex, AFieldName, LFieldInfo) then
      begin
        LNode.FieldOffset := LFieldInfo.FieldOffset;
        LNode.FieldSize := GetTypeSize(LFieldInfo.FieldType);
        LNode.ResultType := LFieldInfo.FieldType;
        LNode.BitWidth := LFieldInfo.BitWidth;
        LNode.BitOffset := LFieldInfo.BitOffset;
      end;
    end;
  end;

  Result := AddExpr(LNode);
end;

function TTigerIR.GetIndex(const AArray: TTigerIRExpr; const AIndex: TTigerIRExpr): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LArrayNode: TIRExprNode;
  LTypeEntry: TIRTypeEntry;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekArrayIndex;
  LNode.ArrayExpr := AArray.Index;
  LNode.IndexExpr := AIndex.Index;

  // Get array expression's element type and size
  if (AArray.Index >= 0) and (AArray.Index < FExpressions.Count) then
  begin
    LArrayNode := FExpressions[AArray.Index];
    if not LArrayNode.ResultType.IsPrimitive and (LArrayNode.ResultType.TypeIndex >= 0) then
    begin
      LTypeEntry := FTypes[LArrayNode.ResultType.TypeIndex];
      if LTypeEntry.Kind = tkFixedArray then
      begin
        LNode.ElementSize := GetTypeSize(LTypeEntry.FixedArrayType.ElementType);
        LNode.ResultType := LTypeEntry.FixedArrayType.ElementType;
      end
      else if LTypeEntry.Kind = tkDynArray then
      begin
        LNode.ElementSize := GetTypeSize(LTypeEntry.DynArrayType.ElementType);
        LNode.ResultType := LTypeEntry.DynArrayType.ElementType;
      end;
    end;
  end;

  Result := AddExpr(LNode);
end;

//==============================================================================
// TTigerIR - Expressions: Function Call
//==============================================================================

function TTigerIR.Invoke(const AFuncName: string; const AArgs: array of TTigerIRExpr): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LI: Integer;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekCall;
  LNode.CallTarget := AFuncName;

  SetLength(LNode.CallArgs, Length(AArgs));
  for LI := 0 to System.High(AArgs) do
    LNode.CallArgs[LI] := AArgs[LI].Index;

  Result := AddExpr(LNode);
end;

function TTigerIR.ExcCode(): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekGetExceptionCode;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt32);
  Result := AddExpr(LNode);
end;

function TTigerIR.ExcMsg(): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekGetExceptionMsg;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtPointer);  // Pointer to string data
  Result := AddExpr(LNode);
end;

function TTigerIR.SetLit(const ATypeName: string; const AElements: array of Integer): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LEntry: TIRTypeEntry;
  LNode: TIRExprNode;
  LI: Integer;
begin
  Result := TTigerIRExpr.None();

  // Find the set type
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, Format('Unknown set type: %s', [ATypeName]));
    Exit;
  end;

  LEntry := FTypes[LTypeIndex];
  if LEntry.Kind <> tkSet then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Type is not a set: %s', [ATypeName]));
    Exit;
  end;

  LNode := Default(TIRExprNode);
  LNode.Kind := ekSetLiteral;
  LNode.ResultType := TTigerTypeRef.FromComposite(LTypeIndex);
  LNode.SetTypeIndex := LTypeIndex;

  // Store elements (they will be adjusted by LowBound during code generation)
  SetLength(LNode.SetElements, Length(AElements));
  for LI := 0 to System.High(AElements) do
    LNode.SetElements[LI] := AElements[LI];

  Result := AddExpr(LNode);
end;

function TTigerIR.SetLitRange(const ATypeName: string; const ALow: Integer; const AHigh: Integer): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LEntry: TIRTypeEntry;
  LNode: TIRExprNode;
  LI: Integer;
  LCount: Integer;
begin
  Result := TTigerIRExpr.None();

  // Find the set type
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, Format('Unknown set type: %s', [ATypeName]));
    Exit;
  end;

  LEntry := FTypes[LTypeIndex];
  if LEntry.Kind <> tkSet then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Type is not a set: %s', [ATypeName]));
    Exit;
  end;

  // Validate range
  if AHigh < ALow then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Invalid set range: %d..%d', [ALow, AHigh]));
    Exit;
  end;

  LNode := Default(TIRExprNode);
  LNode.Kind := ekSetLiteral;
  LNode.ResultType := TTigerTypeRef.FromComposite(LTypeIndex);
  LNode.SetTypeIndex := LTypeIndex;

  // Expand range into individual elements
  LCount := AHigh - ALow + 1;
  SetLength(LNode.SetElements, LCount);
  for LI := 0 to LCount - 1 do
    LNode.SetElements[LI] := ALow + LI;

  Result := AddExpr(LNode);
end;

function TTigerIR.EmptySet(const ATypeName: string): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LEntry: TIRTypeEntry;
  LNode: TIRExprNode;
begin
  Result := TTigerIRExpr.None();

  // Find the set type
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, Format('Unknown set type: %s', [ATypeName]));
    Exit;
  end;

  LEntry := FTypes[LTypeIndex];
  if LEntry.Kind <> tkSet then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, Format('Type is not a set: %s', [ATypeName]));
    Exit;
  end;

  LNode := Default(TIRExprNode);
  LNode.Kind := ekSetLiteral;
  LNode.ResultType := TTigerTypeRef.FromComposite(LTypeIndex);
  LNode.SetTypeIndex := LTypeIndex;
  SetLength(LNode.SetElements, 0);  // Empty set

  Result := AddExpr(LNode);
end;

function TTigerIR.SetUnion(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetUnion);
end;

function TTigerIR.SetDiff(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetDiff);
end;

function TTigerIR.SetInter(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetInter);
end;

function TTigerIR.SetIn(const AElement: TTigerIRExpr; const ASet: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(AElement, ASet, opSetIn);
end;

function TTigerIR.SetEq(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetEq);
end;

function TTigerIR.SetNe(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetNe);
end;

function TTigerIR.SetSubset(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetSubset);
end;

function TTigerIR.SetSuperset(const ALeft: TTigerIRExpr; const ARight: TTigerIRExpr): TTigerIRExpr;
begin
  Result := MakeBinaryExpr(ALeft, ARight, opSetSuperset);
end;

//==============================================================================
// TTigerIR - Expressions: Compile-Time Intrinsics
//==============================================================================

function TTigerIR.&SizeOf(const ATypeName: string): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;
begin
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'SizeOf: Unknown type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
    Exit;
  end;

  LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
  Result := Int64(GetTypeSize(LTypeRef));
end;

function TTigerIR.AlignOf(const ATypeName: string): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LTypeRef: TTigerTypeRef;
begin
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'AlignOf: Unknown type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
    Exit;
  end;

  LTypeRef := TTigerTypeRef.FromComposite(LTypeIndex);
  Result := Int64(GetTypeAlignment(LTypeRef));
end;

function TTigerIR.High(const ATypeName: string): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LEntry: TIRTypeEntry;
begin
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'High: Unknown type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
    Exit;
  end;

  LEntry := FTypes[LTypeIndex];
  case LEntry.Kind of
    tkFixedArray:
      Result := Int64(LEntry.FixedArrayType.HighBound);
    tkEnum:
      begin
        if Length(LEntry.EnumType.Values) > 0 then
          Result := Int64(LEntry.EnumType.Values[System.High(LEntry.EnumType.Values)].OrdinalValue)
        else
          Result := Int64(0);
      end;
    tkSet:
      Result := Int64(LEntry.SetType.HighBound);
  else
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'High: Not applicable to type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
  end;
end;

function TTigerIR.Low(const ATypeName: string): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LEntry: TIRTypeEntry;
begin
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Low: Unknown type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
    Exit;
  end;

  LEntry := FTypes[LTypeIndex];
  case LEntry.Kind of
    tkFixedArray:
      Result := Int64(LEntry.FixedArrayType.LowBound);
    tkEnum:
      begin
        if Length(LEntry.EnumType.Values) > 0 then
          Result := Int64(LEntry.EnumType.Values[0].OrdinalValue)
        else
          Result := Int64(0);
      end;
    tkSet:
      Result := Int64(LEntry.SetType.LowBound);
  else
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Low: Not applicable to type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
  end;
end;

function TTigerIR.Len(const ATypeName: string): TTigerIRExpr;
var
  LTypeIndex: Integer;
  LEntry: TIRTypeEntry;
begin
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
  begin
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_UNKNOWN_TYPE, 'Len: Unknown type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
    Exit;
  end;

  LEntry := FTypes[LTypeIndex];
  case LEntry.Kind of
    tkFixedArray:
      Result := Int64(LEntry.FixedArrayType.HighBound - LEntry.FixedArrayType.LowBound + 1);
  else
    if Assigned(FErrors) then
      FErrors.Add(esError, ERR_IR_TYPE_BUILD, 'Len: Not applicable to type: %s', [ATypeName]);
    Result := TTigerIRExpr.None();
  end;
end;

//==============================================================================
// TTigerIR - Expressions: Runtime Intrinsics
//==============================================================================

function TTigerIR.Ord(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  // Ord is a pass-through - the ordinal value is the integer representation
  // The expression already holds the numeric value
  Result := AValue;
end;

function TTigerIR.Chr(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  // Chr is a pass-through - we just interpret the integer as a character
  // The expression already holds the numeric value
  Result := AValue;
end;

function TTigerIR.Succ(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  // Succ(x) = x + 1
  Result := Add(AValue, Int64(1));
end;

function TTigerIR.Pred(const AValue: TTigerIRExpr): TTigerIRExpr;
begin
  // Pred(x) = x - 1
  Result := Sub(AValue, Int64(1));
end;

//==============================================================================
// TTigerIR - Expressions: Variadic Intrinsics
//==============================================================================

function TTigerIR.VaCount(): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekVaCount;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt32);
  Result := AddExpr(LNode);
end;

function TTigerIR.VaArg(const AIndex: TTigerIRExpr; const AType: TTigerValueType): TTigerIRExpr;
var
  LNode: TIRExprNode;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekVaArgAt;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(AType);
  LNode.VaArgIndex := AIndex.Index;
  LNode.VaArgType := AType;
  Result := AddExpr(LNode);
end;

//==============================================================================
// TTigerIR - Expressions: Syscall Intrinsics (Linux)
//==============================================================================

function TTigerIR.Syscall(const ANr: Integer;
  const AArgs: array of TTigerIRExpr): TTigerIRExpr;
var
  LNode: TIRExprNode;
  LI: Integer;
begin
  LNode := Default(TIRExprNode);
  LNode.Kind := ekSyscall;
  LNode.ResultType := TTigerTypeRef.FromPrimitive(vtInt64);
  LNode.SyscallNr := ANr;

  SetLength(LNode.CallArgs, Length(AArgs));
  for LI := 0 to System.High(AArgs) do
    LNode.CallArgs[LI] := AArgs[LI].Index;

  Result := AddExpr(LNode);
end;

function TTigerIR.SyscallStmt(const ANr: Integer;
  const AArgs: array of TTigerIRExpr): TTigerIR;
var
  LStmt: TIRStmt;
  LFunc: TIRFunc;
  LI: Integer;
begin
  LFunc := GetCurrentFunc();

  LStmt := Default(TIRStmt);
  LStmt.Kind := skSyscall;
  LStmt.SyscallNr := ANr;

  SetLength(LStmt.CallArgs, Length(AArgs));
  for LI := 0 to System.High(AArgs) do
    LStmt.CallArgs[LI] := AArgs[LI].Index;

  LFunc.Stmts.Add(LStmt);
  FFunctions[FCurrentFunc] := LFunc;
  Result := Self;
end;

//==============================================================================
// TTigerIR - Emit Helpers
//==============================================================================

function TTigerIR.EmitExpr(const ABackend: TTigerBackend; const AExprIndex: Integer): TTigerOperand;
var
  LNode: TIRExprNode;
  LLeft: TTigerOperand;
  LRight: TTigerOperand;
  LTemp: TTigerTempHandle;
  LLocalHandle: TTigerLocalHandle;
  LImportIndex: Integer;
  LFuncIndex: Integer;
  LArgs: TArray<TTigerOperand>;
  LI: Integer;
begin
  if AExprIndex < 0 then
  begin
    Result := TTigerOperand.None();
    Exit;
  end;

  LNode := FExpressions[AExprIndex];

  case LNode.Kind of
    ekConstInt:
      Result := TTigerOperand.FromImm(LNode.ConstInt);

    ekConstFloat:
      Result := TTigerOperand.FromImm(LNode.ConstFloat);

    ekConstString:
      Result := TTigerOperand.FromData(FStringHandles[LNode.StringIndex]);

    ekVariable:
      begin
        if not FVarHandles.TryGetValue(LNode.VarName, LLocalHandle) then
        begin
          if Assigned(FErrors) then
            FErrors.Add(esError, ERR_IR_UNKNOWN_VARIABLE, 'Unknown variable: %s', [LNode.VarName]);
          Result := TTigerOperand.None();
          Exit;
        end;
        LTemp := ABackend.GetCode().Load(LLocalHandle);
        Result := TTigerOperand.FromTemp(LTemp);
      end;

    ekBinary:
      begin
        LLeft := EmitExpr(ABackend, LNode.Left);
        LRight := EmitExpr(ABackend, LNode.Right);

        case LNode.Op of
          opAdd:
            LTemp := ABackend.GetCode().OpAdd(LLeft, LRight);
          opSub:
            LTemp := ABackend.GetCode().OpSub(LLeft, LRight);
          opMul:
            LTemp := ABackend.GetCode().OpMul(LLeft, LRight);
          opDiv:
            LTemp := ABackend.GetCode().OpDiv(LLeft, LRight);
          opMod:
            LTemp := ABackend.GetCode().OpMod(LLeft, LRight);
          opBitAnd:
            LTemp := ABackend.GetCode().OpAnd(LLeft, LRight);
          opBitOr:
            LTemp := ABackend.GetCode().OpOr(LLeft, LRight);
          opBitXor:
            LTemp := ABackend.GetCode().OpXor(LLeft, LRight);
          opShl:
            LTemp := ABackend.GetCode().OpShl(LLeft, LRight);
          opShr:
            LTemp := ABackend.GetCode().OpShr(LLeft, LRight);
          opCmpEq:
            LTemp := ABackend.GetCode().CmpEq(LLeft, LRight);
          opCmpNe:
            LTemp := ABackend.GetCode().CmpNe(LLeft, LRight);
          opCmpLt:
            LTemp := ABackend.GetCode().CmpLt(LLeft, LRight);
          opCmpLe:
            LTemp := ABackend.GetCode().CmpLe(LLeft, LRight);
          opCmpGt:
            LTemp := ABackend.GetCode().CmpGt(LLeft, LRight);
          opCmpGe:
            LTemp := ABackend.GetCode().CmpGe(LLeft, LRight);
          opAnd:
            LTemp := ABackend.GetCode().OpAnd(LLeft, LRight);
          opOr:
            LTemp := ABackend.GetCode().OpOr(LLeft, LRight);
        else
          begin
            if Assigned(FErrors) then
              FErrors.Add(esError, ERR_IR_UNKNOWN_OPERATION, 'Unknown binary operation');
            Result := TTigerOperand.None();
            Exit;
          end;
        end;

        Result := TTigerOperand.FromTemp(LTemp);
      end;

    ekUnary:
      begin
        case LNode.Op of
          opAddrOf:
            begin
              if not FVarHandles.TryGetValue(LNode.VarName, LLocalHandle) then
              begin
                if Assigned(FErrors) then
                  FErrors.Add(esError, ERR_IR_UNKNOWN_VARIABLE, 'Unknown variable: %s', [LNode.VarName]);
                Result := TTigerOperand.None();
                Exit;
              end;
              LTemp := ABackend.GetCode().AddressOf(LLocalHandle);
              Result := TTigerOperand.FromTemp(LTemp);
            end;

          opDeref:
            begin
              LLeft := EmitExpr(ABackend, LNode.Left);
              LTemp := ABackend.GetCode().LoadPtr(LLeft);
              Result := TTigerOperand.FromTemp(LTemp);
            end;

          opNeg:
            begin
              LLeft := EmitExpr(ABackend, LNode.Left);
              // Negate: 0 - value
              LTemp := ABackend.GetCode().OpSub(0, LLeft);
              Result := TTigerOperand.FromTemp(LTemp);
            end;

          opBitNot:
            begin
              LLeft := EmitExpr(ABackend, LNode.Left);
              LTemp := ABackend.GetCode().OpNot(LLeft);
              Result := TTigerOperand.FromTemp(LTemp);
            end;

          opNot:
            begin
              LLeft := EmitExpr(ABackend, LNode.Left);
              // Logical not: compare with 0
              LTemp := ABackend.GetCode().CmpEq(LLeft, 0);
              Result := TTigerOperand.FromTemp(LTemp);
            end;

        else
          begin
            if Assigned(FErrors) then
              FErrors.Add(esError, ERR_IR_UNKNOWN_OPERATION, 'Unknown unary operation');
            Result := TTigerOperand.None();
            Exit;
          end;
        end;
      end;

    ekCall:
      begin
        // Evaluate arguments
        SetLength(LArgs, Length(LNode.CallArgs));
        for LI := 0 to System.High(LNode.CallArgs) do
          LArgs[LI] := EmitExpr(ABackend, LNode.CallArgs[LI]);

        // Find target
        LImportIndex := FindImport(LNode.CallTarget);
        if LImportIndex >= 0 then
        begin
          LTemp := ABackend.GetCode().CallFunc(FImportHandles[LImportIndex], LArgs);
          Result := TTigerOperand.FromTemp(LTemp);
        end
        else
        begin
          LFuncIndex := FindLocalFunc(LNode.CallTarget);
          if LFuncIndex >= 0 then
          begin
            LTemp := ABackend.GetCode().CallFunc(ABackend.GetCode().GetFuncHandle(LNode.CallTarget), LArgs);
            Result := TTigerOperand.FromTemp(LTemp);
          end
          else
          begin
            if Assigned(FErrors) then
              FErrors.Add(esError, ERR_IR_UNKNOWN_FUNCTION, 'Unknown function: %s', [LNode.CallTarget]);
            Result := TTigerOperand.None();
            Exit;
          end;
        end;
      end;

  else
    Result := TTigerOperand.None();
  end;
end;

//==============================================================================
// TTigerIR - Emit to Backend
//==============================================================================

procedure TTigerIR.EmitTo(const ABackend: TTigerBackend; const AStatusCallback: TStatusCallback = nil; const AUserData: Pointer = nil);
var
  LI: Integer;
  LSize: Integer;
  LSSA: TTigerSSABuilder;
  LImportName: string;
begin
  // Clear emit-time state
  FVarHandles.Clear();
  FBlockStack.Clear();

  //----------------------------------------------------------------------------
  // Step 1: Emit strings to backend data section
  //----------------------------------------------------------------------------
  SetLength(FStringHandles, FStrings.Count);
  for LI := 0 to FStrings.Count - 1 do
  begin
    if FStrings[LI].IsWide then
      FStringHandles[LI] := ABackend.GetData().AddStringW(FStrings[LI].Value)
    else
      FStringHandles[LI] := ABackend.GetData().AddString(FStrings[LI].Value);
  end;

  //----------------------------------------------------------------------------
  // Step 2: Emit imports to backend
  //----------------------------------------------------------------------------
  SetLength(FImportHandles, FImports.Count);
  for LI := 0 to FImports.Count - 1 do
  begin
    // Compute import name based on linkage
    if FImports[LI].Linkage = plC then
      LImportName := FImports[LI].FuncName
    else
      LImportName := TTigerABIMangler.MangleFunctionWithLinkage(
        FImports[LI].FuncName, FImports[LI].ParamTypes, FImports[LI].Linkage);
    FImportHandles[LI] := ABackend.GetImports().Add(FImports[LI].DllName, LImportName, FImports[LI].ReturnType, FImports[LI].IsStatic);
  end;

  //----------------------------------------------------------------------------
  // Step 3: Emit globals to backend data section
  //----------------------------------------------------------------------------
  SetLength(FGlobalHandles, FGlobals.Count);
  for LI := 0 to FGlobals.Count - 1 do
  begin
    case FGlobals[LI].GlobalType of
      vtInt8, vtUInt8:   LSize := 1;
      vtInt16, vtUInt16: LSize := 2;
      vtInt32, vtUInt32, vtFloat32: LSize := 4;
      vtInt64, vtUInt64, vtFloat64, vtPointer: LSize := 8;
    else
      LSize := 8;
    end;
    FGlobalHandles[LI] := ABackend.GetGlobals().Reserve(LSize);
  end;

  //----------------------------------------------------------------------------
  // Step 4: Build SSA from high-level IR
  //----------------------------------------------------------------------------
  LSSA := TTigerSSABuilder.Create();
  try
    LSSA.SetErrors(FErrors);

    // Set status callback if provided
    if Assigned(AStatusCallback) then
      LSSA.SetStatusCallback(AStatusCallback, AUserData);

    // Convert to SSA form
    LSSA.BuildFrom(Self);

    // Pass handles from backend
    LSSA.SetHandles(FImportHandles, FStringHandles, FGlobalHandles);

    // Run optimization passes (based on level)
    LSSA.Optimize(ABackend.GetOptimizationLevel());

    // Capture SSA dump (after optimization)
    ABackend.SetSSADump(LSSA.DumpSSA());

    // Emit SSA to backend
    LSSA.EmitTo(ABackend);
  finally
    LSSA.Free();
  end;
end;

//==============================================================================
// TTigerIR - Clear
//==============================================================================

procedure TTigerIR.Clear();
var
  LI: Integer;
begin
  for LI := 0 to FFunctions.Count - 1 do
  begin
    FFunctions[LI].Vars.Free();
    FFunctions[LI].Stmts.Free();
  end;

  FImports.Clear();
  FStrings.Clear();
  FGlobals.Clear();
  FFunctions.Clear();
  FExpressions.Clear();
  FTypes.Clear();
  FVarHandles.Clear();
  FBlockStack.Clear();
  FCurrentFunc := -1;
  FBuildingRecordIndex := -1;
  FBuildingUnionIndex := -1;
  FBuildingEnumIndex := -1;
  FNextEnumOrdinal := 0;
  FNextOverlayGroup := 0;
  FAnonUnionFieldStart := -1;
  FNextAnonRecordGroup := 0;
  FAnonRecordInUnion := False;
  FSnapshotFuncs := -1;
  FSnapshotImports := -1;
  FSnapshotStrings := -1;
  FSnapshotGlobals := -1;
  FSnapshotExprs := -1;

  SetLength(FImportHandles, 0);
  SetLength(FStringHandles, 0);
  SetLength(FGlobalHandles, 0);
end;

//==============================================================================
// TTigerIR - Runtime Snapshot
//==============================================================================

procedure TTigerIR.SaveSnapshot();
begin
  FSnapshotFuncs := FFunctions.Count;
  FSnapshotImports := FImports.Count;
  FSnapshotStrings := FStrings.Count;
  FSnapshotGlobals := FGlobals.Count;
  FSnapshotExprs := FExpressions.Count;
end;

procedure TTigerIR.RestoreSnapshot();
var
  LI: Integer;
  LCount: Integer;
begin
  if FSnapshotFuncs < 0 then
    Exit;

  // Free owned sub-objects on functions being removed
  for LI := FFunctions.Count - 1 downto FSnapshotFuncs do
  begin
    FFunctions[LI].Vars.Free();
    FFunctions[LI].Stmts.Free();
  end;

  // Truncate all lists back to snapshot point
  LCount := FFunctions.Count - FSnapshotFuncs;
  if LCount > 0 then
    FFunctions.DeleteRange(FSnapshotFuncs, LCount);

  LCount := FImports.Count - FSnapshotImports;
  if LCount > 0 then
    FImports.DeleteRange(FSnapshotImports, LCount);

  LCount := FStrings.Count - FSnapshotStrings;
  if LCount > 0 then
    FStrings.DeleteRange(FSnapshotStrings, LCount);

  LCount := FGlobals.Count - FSnapshotGlobals;
  if LCount > 0 then
    FGlobals.DeleteRange(FSnapshotGlobals, LCount);

  LCount := FExpressions.Count - FSnapshotExprs;
  if LCount > 0 then
    FExpressions.DeleteRange(FSnapshotExprs, LCount);

  // Reset snapshot
  FSnapshotFuncs := -1;
end;

//==============================================================================
// TTigerIR - Getters for SSA conversion
//==============================================================================

function TTigerIR.GetImportCount(): Integer;
begin
  Result := FImports.Count;
end;

function TTigerIR.GetImport(const AIndex: Integer): TIRImport;
begin
  Result := FImports[AIndex];
end;

function TTigerIR.GetStringCount(): Integer;
begin
  Result := FStrings.Count;
end;

function TTigerIR.GetString(const AIndex: Integer): TIRString;
begin
  Result := FStrings[AIndex];
end;

function TTigerIR.GetGlobalCount(): Integer;
begin
  Result := FGlobals.Count;
end;

function TTigerIR.GetGlobal(const AIndex: Integer): TIRGlobal;
begin
  Result := FGlobals[AIndex];
end;

function TTigerIR.GetFunctionCount(): Integer;
begin
  Result := FFunctions.Count;
end;

function TTigerIR.GetFunction(const AIndex: Integer): TIRFunc;
begin
  Result := FFunctions[AIndex];
end;

function TTigerIR.GetExpressionCount(): Integer;
begin
  Result := FExpressions.Count;
end;

function TTigerIR.GetExpression(const AIndex: Integer): TIRExprNode;
begin
  Result := FExpressions[AIndex];
end;

function TTigerIR.FindRecordField(const ATypeIndex: Integer; const AFieldName: string;
  out AFieldInfo: TIRRecordField): Boolean;
var
  LEntry: TIRTypeEntry;
  LI: Integer;
begin
  Result := False;
  AFieldInfo := Default(TIRRecordField);

  if (ATypeIndex < 0) or (ATypeIndex >= FTypes.Count) then
    Exit;

  LEntry := FTypes[ATypeIndex];
  if LEntry.Kind <> tkRecord then
    Exit;

  // Ensure record is finalized
  if not LEntry.RecordType.IsFinalized then
    FinalizeRecordLayout(ATypeIndex);

  // Re-read after finalization
  LEntry := FTypes[ATypeIndex];

  // Search in this record's own fields first
  for LI := 0 to Length(LEntry.RecordType.Fields) - 1 do
  begin
    if SameText(LEntry.RecordType.Fields[LI].FieldName, AFieldName) then
    begin
      AFieldInfo := LEntry.RecordType.Fields[LI];
      Result := True;
      Exit;
    end;
  end;

  // Not found in own fields - search base type (recursive)
  if LEntry.RecordType.BaseTypeIndex >= 0 then
    Result := FindRecordField(LEntry.RecordType.BaseTypeIndex, AFieldName, AFieldInfo);
end;

end.
