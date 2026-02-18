{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.SSA;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  Tiger.Utils,
  Tiger.Common,
  Tiger.Errors,
  Tiger.Resources,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.IR;

type
  //============================================================================
  // Forward Declarations
  //============================================================================
  TTigerSSABuilder = class;
  TTigerSSAPass = class;
  TTigerSSABlock = class;
  TTigerSSAFunc = class;

  //============================================================================
  // TTigerSSABuildMode - Debug vs Release compilation
  //============================================================================
  TTigerSSABuildMode = (
    bmDebug,      // No optimization, preserve all debug info
    bmRelease     // Full optimization
  );

  //============================================================================
  // TTigerSSAOptions - Configuration for SSA builder
  //============================================================================
  TTigerSSAOptions = record
    BuildMode: TTigerSSABuildMode;
    
    class function Debug(): TTigerSSAOptions; static;
    class function Release(): TTigerSSAOptions; static;
  end;

  //============================================================================
  // TTigerSSAVar - Versioned variable (name_version)
  //============================================================================
  TTigerSSAVar = record
    BaseName: string;
    Version: Integer;
    
    class function Create(const ABaseName: string; const AVersion: Integer): TTigerSSAVar; static;
    class function None(): TTigerSSAVar; static;
    function IsValid(): Boolean;
    function ToString(): string;
    
    class operator Equal(const A: TTigerSSAVar; const B: TTigerSSAVar): Boolean;
    class operator NotEqual(const A: TTigerSSAVar; const B: TTigerSSAVar): Boolean;
  end;

  //============================================================================
  // TTigerSSAOperandKind - Types of operands in SSA instructions
  //============================================================================
  TTigerSSAOperandKind = (
    sokNone,
    sokImmediate,     // Constant value
    sokVar,           // SSA variable reference
    sokData,          // Data section reference (strings, etc.)
    sokGlobal,        // Global variable reference
    sokImport,        // Import function reference
    sokFunc,          // Local function reference
    sokBlock,         // Basic block reference (for jumps)
    sokLocalIndex     // Local variable by index (for direct stack access)
  );

  //============================================================================
  // TTigerSSAOperand - Operand for SSA instructions
  //============================================================================
  TTigerSSAOperand = record
    Kind: TTigerSSAOperandKind;
    ImmInt: Int64;
    ImmFloat: Double;
    Var_: TTigerSSAVar;
    DataIndex: Integer;
    GlobalIndex: Integer;
    ImportIndex: Integer;
    FuncIndex: Integer;
    BlockIndex: Integer;
    LocalIndex: Integer;   // For sokLocalIndex - local variable by index
    FieldName: string;     // For sikFieldAddr - stores field name
    ElementSize: Integer;  // For sikIndexAddr - stores element size
    BitWidth: Integer;     // For bit field operations
    BitOffset: Integer;    // For bit field operations
    
    class function None(): TTigerSSAOperand; static;
    class function FromImm(const AValue: Int64): TTigerSSAOperand; overload; static;
    class function FromImm(const AValue: Double): TTigerSSAOperand; overload; static;
    class function FromVar(const AVar: TTigerSSAVar): TTigerSSAOperand; static;
    class function FromData(const AIndex: Integer): TTigerSSAOperand; static;
    class function FromGlobal(const AIndex: Integer): TTigerSSAOperand; static;
    class function FromImport(const AIndex: Integer): TTigerSSAOperand; static;
    class function FromFunc(const AIndex: Integer): TTigerSSAOperand; static;
    class function FromBlock(const AIndex: Integer): TTigerSSAOperand; static;
    class function FromLocalIndex(const AIndex: Integer): TTigerSSAOperand; static;
    
    function IsValid(): Boolean;
    function ToString(): string;
  end;
  TTigerSSAInstrKind = (
    sikNop,
    
    // Assignment
    sikAssign,        // dest := op1
    
    // Arithmetic
    sikAdd,           // dest := op1 + op2
    sikSub,           // dest := op1 - op2
    sikMul,           // dest := op1 * op2
    sikDiv,           // dest := op1 / op2
    sikMod,           // dest := op1 mod op2
    sikNeg,           // dest := -op1
    // Float arithmetic
    sikFAdd,          // dest := float(op1 + op2)
    sikFSub,          // dest := float(op1 - op2)
    sikFMul,          // dest := float(op1 * op2)
    sikFDiv,          // dest := float(op1 / op2)
    sikFNeg,          // dest := float(-op1)
    
    // Bitwise
    sikBitAnd,        // dest := op1 and op2
    sikBitOr,         // dest := op1 or op2
    sikBitXor,        // dest := op1 xor op2
    sikBitNot,        // dest := not op1
    sikShl,           // dest := op1 shl op2
    sikShr,           // dest := op1 shr op2
    
    // Comparison (result is 0 or 1)
    sikCmpEq,         // dest := op1 = op2
    sikCmpNe,         // dest := op1 <> op2
    sikCmpLt,         // dest := op1 < op2
    sikCmpLe,         // dest := op1 <= op2
    sikCmpGt,         // dest := op1 > op2
    sikCmpGe,         // dest := op1 >= op2
    
    // Memory
    sikLoad,          // dest := mem[op1]
    sikStore,         // mem[op1] := op2
    sikAddressOf,     // dest := @op1
    sikFieldAddr,     // dest := @(op1.field) - field offset in Op2.ImmInt
    sikIndexAddr,     // dest := @(op1[op2]) - element size in CallTarget.ImmInt
    
    // Control flow
    sikJump,          // goto block
    sikJumpIf,        // if op1 then goto block
    sikJumpIfNot,     // if not op1 then goto block
    
    // Phi node (SSA join point)
    sikPhi,           // dest := phi(op1, op2, ...)
    
    // Call
    sikCall,          // call func(args)
    sikCallAssign,    // dest := call func(args)
    
    // Indirect call (via function pointer)
    sikFuncAddr,          // dest := @func
    sikIndirectCall,      // call [op1](args)
    sikIndirectCallAssign,// dest := call [op1](args)
    
    // Return
    sikReturn,        // return
    sikReturnValue,   // return op1
    
    // Set operations
    sikSetLiteral,    // dest := {elements...}
    sikSetUnion,      // dest := op1 + op2
    sikSetDiff,       // dest := op1 - op2
    sikSetInter,      // dest := op1 * op2
    sikSetIn,         // dest := op1 in op2
    sikSetEq,         // dest := op1 = op2
    sikSetNe,         // dest := op1 <> op2
    sikSetSubset,     // dest := op1 <= op2
    sikSetSuperset,   // dest := op1 >= op2
    
    // Variadic intrinsics
    sikVaCount,       // dest := VaCount_()
    sikVaArgAt,       // dest := VaArgAt_(index, type)

    // Syscall intrinsics (Linux)
    sikSyscall,       // syscall(nr, args...) — discard result
    sikSyscallAssign  // dest := syscall(nr, args...) — capture RAX
  );

  //============================================================================
  // TTigerSSAPhiEntry - Single entry in a phi node
  //============================================================================
  TTigerSSAPhiEntry = record
    BlockIndex: Integer;    // Predecessor block
    Var_: TTigerSSAVar;       // Variable from that block
  end;

  //============================================================================
  // TTigerSSAInstr - Single SSA instruction
  //============================================================================
  TTigerSSAInstr = record
    Kind: TTigerSSAInstrKind;
    Dest: TTigerSSAVar;
    Op1: TTigerSSAOperand;
    Op2: TTigerSSAOperand;
    
    // For phi nodes
    PhiEntries: TArray<TTigerSSAPhiEntry>;
    
    // For calls
    CallTarget: TTigerSSAOperand;
    CallArgs: TArray<TTigerSSAOperand>;
    
    // For set literals
    SetElements: TArray<Integer>;  // Element values for sikSetLiteral
    SetTypeIndex: Integer;         // Type index for set operations
    SetLowBound: Integer;          // Low bound of set type
    SetStorageSize: Integer;       // Storage size (1, 2, 4, or 8)
    
    // Source location (for debugging)
    SourceLine: Integer;
    SourceColumn: Integer;
    
    // For sikVaArgAt
    VaArgType: TTigerValueType;  // Type to read vararg as
    // For sikSyscall / sikSyscallAssign
    SyscallNr: Integer;          // Linux syscall number
    // For sized memory access (field loads/stores)
    MemSize: Integer;            // 0=default 8 bytes, 1/2/4/8=explicit size
    MemIsFloat: Boolean;         // True for float32/float64 field access
  end;

  //============================================================================
  // TTigerSSABlock - Basic block containing SSA instructions
  //============================================================================
  TTigerSSABlock = class(TBaseObject)
  private
    FBlockId: Integer;
    FBlockName: string;
    FInstructions: TList<TTigerSSAInstr>;
    FPredecessors: TList<Integer>;
    FSuccessors: TList<Integer>;
    
    // Dominator tree information
    FImmediateDominator: Integer;  // -1 for entry block
    FDominanceFrontier: TList<Integer>;
    FDominatedBlocks: TList<Integer>;  // Blocks immediately dominated by this block
    
    // Variables defined in this block (for phi insertion)
    FDefinedVars: TList<string>;
    
  public
    constructor Create(const ABlockId: Integer; const ABlockName: string); reintroduce;
    destructor Destroy(); override;
    
    procedure AddInstruction(const AInstr: TTigerSSAInstr);
    procedure InsertInstructionAt(const AIndex: Integer; const AInstr: TTigerSSAInstr);
    procedure AddPredecessor(const ABlockId: Integer);
    procedure AddSuccessor(const ABlockId: Integer);
    procedure AddDefinedVar(const AVarName: string);
    
    function GetBlockId(): Integer;
    function GetBlockName(): string;
    function GetInstructionCount(): Integer;
    function GetInstruction(const AIndex: Integer): TTigerSSAInstr;
    procedure SetInstruction(const AIndex: Integer; const AInstr: TTigerSSAInstr);
    function GetPredecessorCount(): Integer;
    function GetPredecessor(const AIndex: Integer): Integer;
    function GetSuccessorCount(): Integer;
    function GetSuccessor(const AIndex: Integer): Integer;
    
    // Dominator tree access
    function GetImmediateDominator(): Integer;
    procedure SetImmediateDominator(const ABlockId: Integer);
    function GetDominanceFrontierCount(): Integer;
    function GetDominanceFrontier(const AIndex: Integer): Integer;
    procedure AddDominanceFrontier(const ABlockId: Integer);
    procedure ClearDominanceFrontier();
    function GetDominatedBlockCount(): Integer;
    function GetDominatedBlock(const AIndex: Integer): Integer;
    procedure AddDominatedBlock(const ABlockId: Integer);
    function GetDefinedVarCount(): Integer;
    function GetDefinedVar(const AIndex: Integer): string;
    function HasDefinedVar(const AVarName: string): Boolean;
    
    procedure Clear();
  end;

  //============================================================================
  // TTigerSSALocalInfo - Local variable metadata
  //============================================================================
  TTigerSSALocalInfo = record
    LocalName: string;
    LocalTypeRef: TTigerTypeRef;  // Supports primitive and composite types
    LocalSize: Integer;         // Size in bytes (for composite types)
    LocalAlignment: Integer;    // Alignment in bytes (for ABI classification)
    IsParam: Boolean;
    IsManaged: Boolean;         // True for string types that need cleanup
  end;

  //============================================================================
  // TTigerSSAExceptionScope - Exception region metadata for Backend SCOPE_TABLE
  //============================================================================
  TTigerSSAExceptionScope = record
    TryBlockIndex: Integer;        // Block where try starts
    TryEndBlockIndex: Integer;     // Block where try body ends (before except/finally)
    ExceptBlockIndex: Integer;     // Block where except handler starts (-1 if none)
    FinallyBlockIndex: Integer;    // Block where finally handler starts (-1 if none)
    EndBlockIndex: Integer;        // Block after entire try construct
  end;

  //============================================================================
  // TTigerSSAFunc - Function containing basic blocks
  //============================================================================
  TTigerSSAFunc = class(TBaseObject)
  private
    FFuncName: string;
    FReturnType: TTigerValueType;
    FReturnSize: Integer;
    FReturnAlignment: Integer;
    FIsEntryPoint: Boolean;
    FIsDllEntry: Boolean;
    FIsPublic: Boolean;
    FLinkage: TTigerLinkage;
    FLocals: TList<TTigerSSALocalInfo>;
    FBlocks: TObjectList<TTigerSSABlock>;
    FCurrentBlockIndex: Integer;
    FNextVarVersion: TDictionary<string, Integer>;
    FExceptionScopes: TList<TTigerSSAExceptionScope>;
    FIsVariadic: Boolean;
    
  public
    constructor Create(const AFuncName: string); reintroduce;
    destructor Destroy(); override;
    
    // Setup
    procedure SetReturnType(const AType: TTigerValueType); overload;
    procedure SetReturnType(const AType: TTigerValueType; const ASize: Integer; const AAlignment: Integer); overload;
    procedure SetIsEntryPoint(const AValue: Boolean);
    procedure SetIsDllEntry(const AValue: Boolean);
    procedure SetIsPublic(const AValue: Boolean);
    procedure SetLinkage(const AValue: TTigerLinkage);
    procedure SetIsVariadic(const AValue: Boolean);
    procedure AddLocal(const AName: string; const ATypeRef: TTigerTypeRef; const ASize: Integer; const AAlignment: Integer; const AIsParam: Boolean; const AIsManaged: Boolean = False);
    
    // Block management
    function CreateBlock(const AName: string): Integer;
    procedure SetCurrentBlock(const ABlockIndex: Integer);
    function GetCurrentBlock(): TTigerSSABlock;
    
    // Variable versioning
    function NewVersion(const ABaseName: string): TTigerSSAVar;
    function CurrentVersion(const ABaseName: string): TTigerSSAVar;
    
    // Exception scopes
    procedure AddExceptionScope(const AScope: TTigerSSAExceptionScope);
    function GetExceptionScopeCount(): Integer;
    function GetExceptionScope(const AIndex: Integer): TTigerSSAExceptionScope;
    
    // Getters
    function GetFuncName(): string;
    function GetReturnType(): TTigerValueType;
    function GetReturnSize(): Integer;
    function GetReturnAlignment(): Integer;
    function GetIsEntryPoint(): Boolean;
    function GetIsDllEntry(): Boolean;
    function GetIsPublic(): Boolean;
    function GetLinkage(): TTigerLinkage;
    function GetIsVariadic(): Boolean;
    function GetLocalCount(): Integer;
    function GetLocal(const AIndex: Integer): TTigerSSALocalInfo;
    function GetBlockCount(): Integer;
    function GetBlock(const AIndex: Integer): TTigerSSABlock;
    
    procedure Clear();
  end;

  //============================================================================
  // TTigerSSAPassKind - Types of optimization passes
  //============================================================================
  TTigerSSAPassKind = (
    spkConstantFolding,
    spkConstantPropagation,
    spkDeadCodeElimination,
    spkCommonSubexprElim,
    spkCopyPropagation,
    spkStringCleanup          // Insert Pax_StrRelease calls before returns
  );

  //============================================================================
  // TTigerSSAPass - Abstract base class for optimization passes
  //============================================================================
  TTigerSSAPass = class abstract(TBaseObject)
  private
    FPassKind: TTigerSSAPassKind;
    FPassName: string;
    FMinLevel: Integer;
    
  public
    constructor Create(const APassKind: TTigerSSAPassKind; const APassName: string; const AMinLevel: Integer = 1); reintroduce;
    
    procedure Run(const AFunc: TTigerSSAFunc); virtual; abstract;
    
    function GetPassKind(): TTigerSSAPassKind;
    function GetPassName(): string;
    function GetMinLevel(): Integer;
  end;

  //============================================================================
  // TTigerSSAConstantFolding - Fold constant expressions
  //============================================================================
  TTigerSSAConstantFolding = class(TTigerSSAPass)
  public
    constructor Create(); reintroduce;
    procedure Run(const AFunc: TTigerSSAFunc); override;
  end;

  //============================================================================
  // TTigerSSAConstantPropagation - Propagate constants through variables
  //============================================================================
  TTigerSSAConstantPropagation = class(TTigerSSAPass)
  public
    constructor Create(); reintroduce;
    procedure Run(const AFunc: TTigerSSAFunc); override;
  end;

  //============================================================================
  // TTigerSSADeadCodeElimination - Remove unused instructions
  //============================================================================
  TTigerSSADeadCodeElimination = class(TTigerSSAPass)
  public
    constructor Create(); reintroduce;
    procedure Run(const AFunc: TTigerSSAFunc); override;
  end;

  //============================================================================
  // TTigerSSACopyPropagation - Replace copies with original values
  //============================================================================
  TTigerSSACopyPropagation = class(TTigerSSAPass)
  public
    constructor Create(); reintroduce;
    procedure Run(const AFunc: TTigerSSAFunc); override;
  end;

  //============================================================================
  // TTigerSSACommonSubexprElim - Eliminate redundant computations
  //============================================================================
  TTigerSSACommonSubexprElim = class(TTigerSSAPass)
  public
    constructor Create(); reintroduce;
    procedure Run(const AFunc: TTigerSSAFunc); override;
  end;

  //============================================================================
  // TTigerSSAStringCleanup - Insert Pax_StrRelease calls before returns
  //============================================================================
  TTigerSSAStringCleanup = class(TTigerSSAPass)
  private
    FSSABuilder: TTigerSSABuilder;
    FIR: TTigerIR;
  public
    constructor Create(const ASSABuilder: TTigerSSABuilder; const AIR: TTigerIR); reintroduce;
    procedure Run(const AFunc: TTigerSSAFunc); override;
  end;

  //============================================================================
  // TTigerSSABuilder - Main SSA builder: converts high-level IR to SSA
  //============================================================================
  TTigerSSABuilder = class(TTigerStatusObject)
  private
    FOptions: TTigerSSAOptions;
    FFunctions: TObjectList<TTigerSSAFunc>;
    FCurrentFuncIndex: Integer;
    FPasses: TObjectList<TTigerSSAPass>;
    
    // Data/Import mappings from source IR
    FStringHandles: TArray<TTigerDataHandle>;
    FGlobalHandles: TArray<TTigerDataHandle>;
    FImportHandles: TArray<TTigerImportHandle>;
    FStrReleaseFuncIdx: Integer;  // Index of Pax_StrRelease function for string cleanup
    FReportLeaksFuncIdx: Integer;  // Index of Pax_ReportLeaks function (debug only)
    FReleaseOnDetachFuncIdx: Integer;  // Index of Tiger_ReleaseOnDetach for DLL cleanup
    FExpressionCache: TDictionary<Integer, TTigerSSAVar>;  // Cache for converted expressions per function
    
    // Internal conversion helpers
    procedure ConvertStatements(
      const AIR: TTigerIR;
      const AFuncIndex: Integer;
      const ASSAFunc: TTigerSSAFunc
    );
    function ConvertExpression(
      const AIR: TTigerIR;
      const AExprIndex: Integer;
      const ASSAFunc: TTigerSSAFunc
    ): TTigerSSAVar;
    function FindImportByName(const AIR: TTigerIR; const AName: string): Integer;
    function FindLocalFuncByName(const AIR: TTigerIR; const AName: string): Integer;
    function FindLocalFuncBySignature(
      const AIR: TTigerIR;
      const AName: string;
      const AArgTypes: TArray<TTigerTypeRef>
    ): Integer;
    function ResolveOperand(
      const AOp: TTigerSSAOperand;
      const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
      const AVarTemps: TDictionary<string, TTigerTempHandle>;
      const AVarOperands: TDictionary<string, TTigerOperand>;
      const ABackend: TTigerBackend
    ): TTigerOperand;
    function StoreSSAVar(
      const ADest: TTigerSSAVar;
      const AValue: TTigerOperand;
      const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
      const AVarTemps: TDictionary<string, TTigerTempHandle>;
      const AVarOperands: TDictionary<string, TTigerOperand>;
      const ABackend: TTigerBackend
    ): TTigerTempHandle;
    procedure EmitCall(
      const AInstr: TTigerSSAInstr;
      const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
      const AVarTemps: TDictionary<string, TTigerTempHandle>;
      const AVarOperands: TDictionary<string, TTigerOperand>;
      const ABackend: TTigerBackend
    );
    function EmitCallWithResult(
      const AInstr: TTigerSSAInstr;
      const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
      const AVarTemps: TDictionary<string, TTigerTempHandle>;
      const AVarOperands: TDictionary<string, TTigerOperand>;
      const ABackend: TTigerBackend
    ): TTigerTempHandle;
    procedure EmitIndirectCall(
      const AInstr: TTigerSSAInstr;
      const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
      const AVarTemps: TDictionary<string, TTigerTempHandle>;
      const AVarOperands: TDictionary<string, TTigerOperand>;
      const ABackend: TTigerBackend
    );
    function EmitIndirectCallWithResult(
      const AInstr: TTigerSSAInstr;
      const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
      const AVarTemps: TDictionary<string, TTigerTempHandle>;
      const AVarOperands: TDictionary<string, TTigerOperand>;
      const ABackend: TTigerBackend
    ): TTigerTempHandle;
    
    // SSA construction algorithms
    procedure ComputeDominators(const AFunc: TTigerSSAFunc);
    procedure ComputeDominanceFrontiers(const AFunc: TTigerSSAFunc);
    procedure InsertPhiNodes(const AFunc: TTigerSSAFunc);
    procedure RenameVariables(const AFunc: TTigerSSAFunc);
    procedure EliminatePhiNodes(const AFunc: TTigerSSAFunc);
    
  public
    constructor Create(); override;
    destructor Destroy(); override;
    
    // Configuration
    procedure SetOptions(const AOptions: TTigerSSAOptions);
    function GetOptions(): TTigerSSAOptions;
    
    // Build from high-level IR
    procedure BuildFrom(const AIR: TTigerIR);
    
    // Set handles from source IR (call after BuildFrom, before EmitTo)
    procedure SetHandles(
      const AImportHandles: TArray<TTigerImportHandle>;
      const AStringHandles: TArray<TTigerDataHandle>;
      const AGlobalHandles: TArray<TTigerDataHandle>
    );
    
    // Optimization
    procedure Optimize(const ALevel: Integer);
    procedure RemoveUnreferencedFunctions();
    procedure AddPass(const APass: TTigerSSAPass);
    
    // Debug
    function DumpSSA(): string;
    procedure ClearPasses();
    
    // Emit to backend
    procedure EmitTo(const ABackend: TTigerBackend);
    
    // Query
    function GetFunctionCount(): Integer;
    function GetFunction(const AIndex: Integer): TTigerSSAFunc;
    function GetStrReleaseFuncIdx(): Integer;
    function GetReportLeaksFuncIdx(): Integer;
    function GetReleaseOnDetachFuncIdx(): Integer;
    
    // Reset
    procedure Clear();
  end;

implementation

const
  ERR_SSA_UNKNOWN_FUNCTION = 'S001';

//==============================================================================
// TTigerSSAOptions
//==============================================================================

class function TTigerSSAOptions.Debug(): TTigerSSAOptions;
begin
  Result.BuildMode := bmDebug;
end;

class function TTigerSSAOptions.Release(): TTigerSSAOptions;
begin
  Result.BuildMode := bmRelease;
end;

//==============================================================================
// TTigerSSAVar
//==============================================================================

class function TTigerSSAVar.Create(const ABaseName: string; const AVersion: Integer): TTigerSSAVar;
begin
  Result.BaseName := ABaseName;
  Result.Version := AVersion;
end;

class function TTigerSSAVar.None(): TTigerSSAVar;
begin
  Result.BaseName := '';
  Result.Version := -1;
end;

function TTigerSSAVar.IsValid(): Boolean;
begin
  Result := (BaseName <> '') and (Version >= 0);
end;

function TTigerSSAVar.ToString(): string;
begin
  if IsValid() then
    Result := Format('%s_%d', [BaseName, Version])
  else
    Result := '<invalid>';
end;

class operator TTigerSSAVar.Equal(const A: TTigerSSAVar; const B: TTigerSSAVar): Boolean;
begin
  Result := (A.BaseName = B.BaseName) and (A.Version = B.Version);
end;

class operator TTigerSSAVar.NotEqual(const A: TTigerSSAVar; const B: TTigerSSAVar): Boolean;
begin
  Result := not (A = B);
end;

//==============================================================================
// TTigerSSAOperand
//==============================================================================

class function TTigerSSAOperand.None(): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokNone;
end;

class function TTigerSSAOperand.FromImm(const AValue: Int64): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokImmediate;
  Result.ImmInt := AValue;
end;

class function TTigerSSAOperand.FromImm(const AValue: Double): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokImmediate;
  Result.ImmFloat := AValue;
end;

class function TTigerSSAOperand.FromVar(const AVar: TTigerSSAVar): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokVar;
  Result.Var_ := AVar;
end;

class function TTigerSSAOperand.FromData(const AIndex: Integer): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokData;
  Result.DataIndex := AIndex;
end;

class function TTigerSSAOperand.FromGlobal(const AIndex: Integer): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokGlobal;
  Result.GlobalIndex := AIndex;
end;

class function TTigerSSAOperand.FromImport(const AIndex: Integer): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokImport;
  Result.ImportIndex := AIndex;
end;

class function TTigerSSAOperand.FromFunc(const AIndex: Integer): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokFunc;
  Result.FuncIndex := AIndex;
end;

class function TTigerSSAOperand.FromBlock(const AIndex: Integer): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokBlock;
  Result.BlockIndex := AIndex;
end;

class function TTigerSSAOperand.FromLocalIndex(const AIndex: Integer): TTigerSSAOperand;
begin
  Result := Default(TTigerSSAOperand);
  Result.Kind := sokLocalIndex;
  Result.LocalIndex := AIndex;
end;

function TTigerSSAOperand.IsValid(): Boolean;
begin
  Result := Kind <> sokNone;
end;

function TTigerSSAOperand.ToString(): string;
begin
  case Kind of
    sokNone: Result := 'none';
    sokImmediate:
    begin
      if ImmFloat <> 0.0 then
        Result := FloatToStr(ImmFloat)
      else
        Result := IntToStr(ImmInt);
    end;
    sokVar: Result := Var_.ToString();
    sokData: Result := Format('data_%d', [DataIndex]);
    sokGlobal: Result := Format('global_%d', [GlobalIndex]);
    sokImport: Result := Format('import_%d', [ImportIndex]);
    sokFunc: Result := Format('func_%d', [FuncIndex]);
    sokBlock: Result := Format('block_%d', [BlockIndex]);
    sokLocalIndex: Result := Format('local_%d', [LocalIndex]);
  else
    Result := '?';
  end;
end;

//==============================================================================
// TTigerSSABlock
//==============================================================================

constructor TTigerSSABlock.Create(const ABlockId: Integer; const ABlockName: string);
begin
  inherited Create();
  
  FBlockId := ABlockId;
  FBlockName := ABlockName;
  FInstructions := TList<TTigerSSAInstr>.Create();
  FPredecessors := TList<Integer>.Create();
  FSuccessors := TList<Integer>.Create();
  FImmediateDominator := -1;
  FDominanceFrontier := TList<Integer>.Create();
  FDominatedBlocks := TList<Integer>.Create();
  FDefinedVars := TList<string>.Create();
end;

destructor TTigerSSABlock.Destroy();
begin
  FDefinedVars.Free();
  FDominatedBlocks.Free();
  FDominanceFrontier.Free();
  FSuccessors.Free();
  FPredecessors.Free();
  FInstructions.Free();
  
  inherited Destroy();
end;

procedure TTigerSSABlock.AddInstruction(const AInstr: TTigerSSAInstr);
begin
  FInstructions.Add(AInstr);
end;

procedure TTigerSSABlock.InsertInstructionAt(const AIndex: Integer; const AInstr: TTigerSSAInstr);
begin
  FInstructions.Insert(AIndex, AInstr);
end;

procedure TTigerSSABlock.AddPredecessor(const ABlockId: Integer);
begin
  if not FPredecessors.Contains(ABlockId) then
    FPredecessors.Add(ABlockId);
end;

procedure TTigerSSABlock.AddSuccessor(const ABlockId: Integer);
begin
  if not FSuccessors.Contains(ABlockId) then
    FSuccessors.Add(ABlockId);
end;

procedure TTigerSSABlock.AddDefinedVar(const AVarName: string);
begin
  if not FDefinedVars.Contains(AVarName) then
    FDefinedVars.Add(AVarName);
end;

function TTigerSSABlock.GetBlockId(): Integer;
begin
  Result := FBlockId;
end;

function TTigerSSABlock.GetBlockName(): string;
begin
  Result := FBlockName;
end;

function TTigerSSABlock.GetInstructionCount(): Integer;
begin
  Result := FInstructions.Count;
end;

function TTigerSSABlock.GetInstruction(const AIndex: Integer): TTigerSSAInstr;
begin
  Result := FInstructions[AIndex];
end;

function TTigerSSABlock.GetPredecessorCount(): Integer;
begin
  Result := FPredecessors.Count;
end;

function TTigerSSABlock.GetPredecessor(const AIndex: Integer): Integer;
begin
  Result := FPredecessors[AIndex];
end;

function TTigerSSABlock.GetSuccessorCount(): Integer;
begin
  Result := FSuccessors.Count;
end;

function TTigerSSABlock.GetSuccessor(const AIndex: Integer): Integer;
begin
  Result := FSuccessors[AIndex];
end;

procedure TTigerSSABlock.SetInstruction(const AIndex: Integer; const AInstr: TTigerSSAInstr);
begin
  FInstructions[AIndex] := AInstr;
end;

function TTigerSSABlock.GetImmediateDominator(): Integer;
begin
  Result := FImmediateDominator;
end;

procedure TTigerSSABlock.SetImmediateDominator(const ABlockId: Integer);
begin
  FImmediateDominator := ABlockId;
end;

function TTigerSSABlock.GetDominanceFrontierCount(): Integer;
begin
  Result := FDominanceFrontier.Count;
end;

function TTigerSSABlock.GetDominanceFrontier(const AIndex: Integer): Integer;
begin
  Result := FDominanceFrontier[AIndex];
end;

procedure TTigerSSABlock.AddDominanceFrontier(const ABlockId: Integer);
begin
  if not FDominanceFrontier.Contains(ABlockId) then
    FDominanceFrontier.Add(ABlockId);
end;

procedure TTigerSSABlock.ClearDominanceFrontier();
begin
  FDominanceFrontier.Clear();
end;

function TTigerSSABlock.GetDominatedBlockCount(): Integer;
begin
  Result := FDominatedBlocks.Count;
end;

function TTigerSSABlock.GetDominatedBlock(const AIndex: Integer): Integer;
begin
  Result := FDominatedBlocks[AIndex];
end;

procedure TTigerSSABlock.AddDominatedBlock(const ABlockId: Integer);
begin
  if not FDominatedBlocks.Contains(ABlockId) then
    FDominatedBlocks.Add(ABlockId);
end;

function TTigerSSABlock.GetDefinedVarCount(): Integer;
begin
  Result := FDefinedVars.Count;
end;

function TTigerSSABlock.GetDefinedVar(const AIndex: Integer): string;
begin
  Result := FDefinedVars[AIndex];
end;

function TTigerSSABlock.HasDefinedVar(const AVarName: string): Boolean;
begin
  Result := FDefinedVars.Contains(AVarName);
end;

procedure TTigerSSABlock.Clear();
begin
  FInstructions.Clear();
  FPredecessors.Clear();
  FSuccessors.Clear();
  FImmediateDominator := -1;
  FDominanceFrontier.Clear();
  FDominatedBlocks.Clear();
  FDefinedVars.Clear();
end;

//==============================================================================
// TTigerSSAFunc
//==============================================================================

constructor TTigerSSAFunc.Create(const AFuncName: string);
begin
  inherited Create();
  
  FFuncName := AFuncName;
  FReturnType := vtVoid;
  FIsEntryPoint := False;
  FIsDllEntry := False;
  FLocals := TList<TTigerSSALocalInfo>.Create();
  FBlocks := TObjectList<TTigerSSABlock>.Create(True);
  FCurrentBlockIndex := -1;
  FNextVarVersion := TDictionary<string, Integer>.Create();
  FExceptionScopes := TList<TTigerSSAExceptionScope>.Create();
end;

destructor TTigerSSAFunc.Destroy();
begin
  FExceptionScopes.Free();
  FNextVarVersion.Free();
  FBlocks.Free();
  FLocals.Free();
  
  inherited Destroy();
end;

procedure TTigerSSAFunc.SetReturnType(const AType: TTigerValueType);
begin
  FReturnType := AType;
  FReturnSize := 0;
  FReturnAlignment := 0;
end;

procedure TTigerSSAFunc.SetReturnType(const AType: TTigerValueType;
  const ASize: Integer; const AAlignment: Integer);
begin
  FReturnType := AType;
  FReturnSize := ASize;
  FReturnAlignment := AAlignment;
end;

procedure TTigerSSAFunc.SetIsEntryPoint(const AValue: Boolean);
begin
  FIsEntryPoint := AValue;
end;

procedure TTigerSSAFunc.SetIsDllEntry(const AValue: Boolean);
begin
  FIsDllEntry := AValue;
end;

procedure TTigerSSAFunc.SetIsPublic(const AValue: Boolean);
begin
  FIsPublic := AValue;
end;

procedure TTigerSSAFunc.SetLinkage(const AValue: TTigerLinkage);
begin
  FLinkage := AValue;
end;

procedure TTigerSSAFunc.SetIsVariadic(const AValue: Boolean);
begin
  FIsVariadic := AValue;
end;

procedure TTigerSSAFunc.AddLocal(const AName: string; const ATypeRef: TTigerTypeRef; const ASize: Integer; const AAlignment: Integer; const AIsParam: Boolean; const AIsManaged: Boolean);
var
  LInfo: TTigerSSALocalInfo;
begin
  LInfo.LocalName := AName;
  LInfo.LocalTypeRef := ATypeRef;
  LInfo.LocalSize := ASize;
  LInfo.LocalAlignment := AAlignment;
  LInfo.IsParam := AIsParam;
  LInfo.IsManaged := AIsManaged;
  FLocals.Add(LInfo);
  
  // Initialize version counter for this variable
  FNextVarVersion.AddOrSetValue(AName, 0);
end;

function TTigerSSAFunc.CreateBlock(const AName: string): Integer;
var
  LBlock: TTigerSSABlock;
begin
  Result := FBlocks.Count;
  LBlock := TTigerSSABlock.Create(Result, AName);
  FBlocks.Add(LBlock);
end;

procedure TTigerSSAFunc.SetCurrentBlock(const ABlockIndex: Integer);
begin
  FCurrentBlockIndex := ABlockIndex;
end;

function TTigerSSAFunc.GetCurrentBlock(): TTigerSSABlock;
begin
  if (FCurrentBlockIndex < 0) or (FCurrentBlockIndex >= FBlocks.Count) then
    Result := nil
  else
    Result := FBlocks[FCurrentBlockIndex];
end;

function TTigerSSAFunc.NewVersion(const ABaseName: string): TTigerSSAVar;
var
  LVersion: Integer;
begin
  if not FNextVarVersion.TryGetValue(ABaseName, LVersion) then
    LVersion := 0;
    
  Result := TTigerSSAVar.Create(ABaseName, LVersion);
  FNextVarVersion.AddOrSetValue(ABaseName, LVersion + 1);
end;

function TTigerSSAFunc.CurrentVersion(const ABaseName: string): TTigerSSAVar;
var
  LVersion: Integer;
begin
  if FNextVarVersion.TryGetValue(ABaseName, LVersion) then
  begin
    if LVersion > 0 then
      Result := TTigerSSAVar.Create(ABaseName, LVersion - 1)
    else
      Result := TTigerSSAVar.None();
  end
  else
    Result := TTigerSSAVar.None();
end;

function TTigerSSAFunc.GetFuncName(): string;
begin
  Result := FFuncName;
end;

function TTigerSSAFunc.GetReturnType(): TTigerValueType;
begin
  Result := FReturnType;
end;

function TTigerSSAFunc.GetReturnSize(): Integer;
begin
  Result := FReturnSize;
end;

function TTigerSSAFunc.GetReturnAlignment(): Integer;
begin
  Result := FReturnAlignment;
end;

function TTigerSSAFunc.GetIsEntryPoint(): Boolean;
begin
  Result := FIsEntryPoint;
end;

function TTigerSSAFunc.GetIsDllEntry(): Boolean;
begin
  Result := FIsDllEntry;
end;

function TTigerSSAFunc.GetIsPublic(): Boolean;
begin
  Result := FIsPublic;
end;

function TTigerSSAFunc.GetLinkage(): TTigerLinkage;
begin
  Result := FLinkage;
end;

function TTigerSSAFunc.GetIsVariadic(): Boolean;
begin
  Result := FIsVariadic;
end;

function TTigerSSAFunc.GetLocalCount(): Integer;
begin
  Result := FLocals.Count;
end;

function TTigerSSAFunc.GetLocal(const AIndex: Integer): TTigerSSALocalInfo;
begin
  Result := FLocals[AIndex];
end;

function TTigerSSAFunc.GetBlockCount(): Integer;
begin
  Result := FBlocks.Count;
end;

function TTigerSSAFunc.GetBlock(const AIndex: Integer): TTigerSSABlock;
begin
  Result := FBlocks[AIndex];
end;

procedure TTigerSSAFunc.AddExceptionScope(const AScope: TTigerSSAExceptionScope);
begin
  FExceptionScopes.Add(AScope);
end;

function TTigerSSAFunc.GetExceptionScopeCount(): Integer;
begin
  Result := FExceptionScopes.Count;
end;

function TTigerSSAFunc.GetExceptionScope(const AIndex: Integer): TTigerSSAExceptionScope;
begin
  Result := FExceptionScopes[AIndex];
end;

procedure TTigerSSAFunc.Clear();
begin
  FLocals.Clear();
  FBlocks.Clear();
  FNextVarVersion.Clear();
  FExceptionScopes.Clear();
  FCurrentBlockIndex := -1;
end;

//==============================================================================
// TTigerSSAPass
//==============================================================================

constructor TTigerSSAPass.Create(const APassKind: TTigerSSAPassKind; const APassName: string; const AMinLevel: Integer = 1);
begin
  inherited Create();
  
  FPassKind := APassKind;
  FPassName := APassName;
  FMinLevel := AMinLevel;
end;

function TTigerSSAPass.GetPassKind(): TTigerSSAPassKind;
begin
  Result := FPassKind;
end;

function TTigerSSAPass.GetPassName(): string;
begin
  Result := FPassName;
end;

function TTigerSSAPass.GetMinLevel(): Integer;
begin
  Result := FMinLevel;
end;

//==============================================================================
// TTigerSSAConstantFolding
//==============================================================================

constructor TTigerSSAConstantFolding.Create();
begin
  inherited Create(spkConstantFolding, 'Constant Folding');
end;

procedure TTigerSSAConstantFolding.Run(const AFunc: TTigerSSAFunc);
var
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LVal1: Int64;
  LVal2: Int64;
  LResult: Int64;
  LChanged: Boolean;
  LF1: Double;
  LF2: Double;
  LFResult: Double;
begin
  // Iterate all blocks and instructions
  for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
  begin
    LBlock := AFunc.GetBlock(LBlockIdx);
    
    for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
    begin
      LInstr := LBlock.GetInstruction(LInstrIdx);
      LChanged := False;
      
      // Check if both operands are immediate constants
      if (LInstr.Op1.Kind = sokImmediate) and (LInstr.Op2.Kind = sokImmediate) then
      begin
        LVal1 := LInstr.Op1.ImmInt;
        LVal2 := LInstr.Op2.ImmInt;
        LResult := 0;
        
        case LInstr.Kind of
          sikAdd:
            begin
              LResult := LVal1 + LVal2;
              LChanged := True;
            end;
          sikSub:
            begin
              LResult := LVal1 - LVal2;
              LChanged := True;
            end;
          sikMul:
            begin
              LResult := LVal1 * LVal2;
              LChanged := True;
            end;
          sikDiv:
            begin
              if LVal2 <> 0 then
              begin
                LResult := LVal1 div LVal2;
                LChanged := True;
              end;
            end;
          sikMod:
            begin
              if LVal2 <> 0 then
              begin
                LResult := LVal1 mod LVal2;
                LChanged := True;
              end;
            end;
          sikBitAnd:
            begin
              LResult := LVal1 and LVal2;
              LChanged := True;
            end;
          sikBitOr:
            begin
              LResult := LVal1 or LVal2;
              LChanged := True;
            end;
          sikBitXor:
            begin
              LResult := LVal1 xor LVal2;
              LChanged := True;
            end;
          sikShl:
            begin
              LResult := LVal1 shl LVal2;
              LChanged := True;
            end;
          sikShr:
            begin
              LResult := LVal1 shr LVal2;
              LChanged := True;
            end;
          sikCmpEq:
            begin
              if LVal1 = LVal2 then LResult := 1 else LResult := 0;
              LChanged := True;
            end;
          sikCmpNe:
            begin
              if LVal1 <> LVal2 then LResult := 1 else LResult := 0;
              LChanged := True;
            end;
          sikCmpLt:
            begin
              if LVal1 < LVal2 then LResult := 1 else LResult := 0;
              LChanged := True;
            end;
          sikCmpLe:
            begin
              if LVal1 <= LVal2 then LResult := 1 else LResult := 0;
              LChanged := True;
            end;
          sikCmpGt:
            begin
              if LVal1 > LVal2 then LResult := 1 else LResult := 0;
              LChanged := True;
            end;
          sikCmpGe:
            begin
              if LVal1 >= LVal2 then LResult := 1 else LResult := 0;
              LChanged := True;
            end;
        end;
        
        // Replace with assignment of constant result
        if LChanged then
        begin
          LInstr.Kind := sikAssign;
          LInstr.Op1 := TTigerSSAOperand.FromImm(LResult);
          LInstr.Op2 := TTigerSSAOperand.None();
          LBlock.SetInstruction(LInstrIdx, LInstr);
        end;
      end
      // Handle unary ops with single immediate operand
      else if (LInstr.Op1.Kind = sokImmediate) and (LInstr.Op2.Kind = sokNone) then
      begin
        LVal1 := LInstr.Op1.ImmInt;
        LResult := 0;
        
        case LInstr.Kind of
          sikNeg:
            begin
              LResult := -LVal1;
              LChanged := True;
            end;
          sikBitNot:
            begin
              LResult := not LVal1;
              LChanged := True;
            end;
        end;
        
        if LChanged then
        begin
          LInstr.Kind := sikAssign;
          LInstr.Op1 := TTigerSSAOperand.FromImm(LResult);
          LBlock.SetInstruction(LInstrIdx, LInstr);
        end;
      end;

      // Float constant folding
      if (not LChanged) and
         (LInstr.Op1.Kind = sokImmediate) and
         (LInstr.Op2.Kind = sokImmediate) and
         (LInstr.Kind in [sikFAdd, sikFSub, sikFMul, sikFDiv, sikFNeg]) then
      begin
        LF1 := LInstr.Op1.ImmFloat;
        LF2 := LInstr.Op2.ImmFloat;
        LFResult := 0.0;

        case LInstr.Kind of
          sikFAdd:
            begin
              LFResult := LF1 + LF2;
              LChanged := True;
            end;
          sikFSub:
            begin
              LFResult := LF1 - LF2;
              LChanged := True;
            end;
          sikFMul:
            begin
              LFResult := LF1 * LF2;
              LChanged := True;
            end;
          sikFDiv:
            begin
              if LF2 <> 0.0 then
              begin
                LFResult := LF1 / LF2;
                LChanged := True;
              end;
            end;
          sikFNeg:
            begin
              LFResult := -LF1;
              LChanged := True;
            end;
        end;

        if LChanged then
        begin
          LInstr.Kind := sikAssign;
          LInstr.Op1 := TTigerSSAOperand.FromImm(LFResult);
          LInstr.Op2 := TTigerSSAOperand.None();
          LBlock.SetInstruction(LInstrIdx, LInstr);
        end;
      end;
    end;
  end;
end;

//==============================================================================
// TTigerSSAConstantPropagation
//==============================================================================

constructor TTigerSSAConstantPropagation.Create();
begin
  inherited Create(spkConstantPropagation, 'Constant Propagation');
end;

procedure TTigerSSAConstantPropagation.Run(const AFunc: TTigerSSAFunc);
var
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LConstants: TDictionary<string, TTigerSSAOperand>;  // Maps var name to constant operand
  LVarKey: string;
  LConstOp: TTigerSSAOperand;
  LChanged: Boolean;
  LArgIdx: Integer;
begin
  LConstants := TDictionary<string, TTigerSSAOperand>.Create();
  try
    // Pass 1: Collect all variables that are assigned constant values
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      
      for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        
        // Check for: var = immediate
        if (LInstr.Kind = sikAssign) and 
           (LInstr.Dest.IsValid()) and 
           (LInstr.Op1.Kind = sokImmediate) then
        begin
          LVarKey := LInstr.Dest.ToString();
          LConstants.AddOrSetValue(LVarKey, LInstr.Op1);
        end;
      end;
    end;
    
    // Pass 2: Replace variable uses with constants where possible
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      
      for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        LChanged := False;
        
        // Replace Op1 if it's a known constant
        if (LInstr.Op1.Kind = sokVar) then
        begin
          LVarKey := LInstr.Op1.Var_.ToString();
          if LConstants.TryGetValue(LVarKey, LConstOp) then
          begin
            LInstr.Op1 := LConstOp;
            LChanged := True;
          end;
        end;
        
        // Replace Op2 if it's a known constant
        if (LInstr.Op2.Kind = sokVar) then
        begin
          LVarKey := LInstr.Op2.Var_.ToString();
          if LConstants.TryGetValue(LVarKey, LConstOp) then
          begin
            LInstr.Op2 := LConstOp;
            LChanged := True;
          end;
        end;
        
        // Replace call arguments if they're known constants
        for LArgIdx := 0 to High(LInstr.CallArgs) do
        begin
          if LInstr.CallArgs[LArgIdx].Kind = sokVar then
          begin
            LVarKey := LInstr.CallArgs[LArgIdx].Var_.ToString();
            if LConstants.TryGetValue(LVarKey, LConstOp) then
            begin
              LInstr.CallArgs[LArgIdx] := LConstOp;
              LChanged := True;
            end;
          end;
        end;
        
        if LChanged then
          LBlock.SetInstruction(LInstrIdx, LInstr);
      end;
    end;
    
  finally
    LConstants.Free();
  end;
end;

//==============================================================================
// TTigerSSADeadCodeElimination
//==============================================================================

constructor TTigerSSADeadCodeElimination.Create();
begin
  inherited Create(spkDeadCodeElimination, 'Dead Code Elimination');
end;

procedure TTigerSSADeadCodeElimination.Run(const AFunc: TTigerSSAFunc);
var
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LUsedVars: TDictionary<string, Boolean>;
  LArgIdx: Integer;
  LPhiIdx: Integer;
  LVarKey: string;
  LToRemove: TList<Integer>;
  LI: Integer;
begin
  LUsedVars := TDictionary<string, Boolean>.Create();
  try
    // Pass 1: Collect all used variables
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      
      for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        
        // Mark Op1 as used if it's a variable
        if LInstr.Op1.Kind = sokVar then
          LUsedVars.AddOrSetValue(LInstr.Op1.Var_.ToString(), True);
        
        // Mark Op2 as used if it's a variable
        if LInstr.Op2.Kind = sokVar then
          LUsedVars.AddOrSetValue(LInstr.Op2.Var_.ToString(), True);
        
        // Mark call arguments as used
        for LArgIdx := 0 to High(LInstr.CallArgs) do
        begin
          if LInstr.CallArgs[LArgIdx].Kind = sokVar then
            LUsedVars.AddOrSetValue(LInstr.CallArgs[LArgIdx].Var_.ToString(), True);
        end;
        
        // Mark phi operands as used
        for LPhiIdx := 0 to High(LInstr.PhiEntries) do
          LUsedVars.AddOrSetValue(LInstr.PhiEntries[LPhiIdx].Var_.ToString(), True);
      end;
    end;
    
    // Pass 2: Remove instructions that define unused variables
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      LToRemove := TList<Integer>.Create();
      try
        for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
        begin
          LInstr := LBlock.GetInstruction(LInstrIdx);
          
          // Skip instructions that don't define a variable
          if not LInstr.Dest.IsValid() then
            Continue;
          
          // Skip calls and syscalls (have side effects)
          if LInstr.Kind in [sikCall, sikCallAssign, sikSyscall, sikSyscallAssign] then
            Continue;
          
          // Skip control flow
          if LInstr.Kind in [sikJump, sikJumpIf, sikJumpIfNot, sikReturn, sikReturnValue] then
            Continue;
          
          // Check if the destination is used
          LVarKey := LInstr.Dest.ToString();
          if not LUsedVars.ContainsKey(LVarKey) then
          begin
            // Mark for removal (can't remove while iterating)
            LToRemove.Add(LInstrIdx);
          end;
        end;
        
        // Remove dead instructions (in reverse order to preserve indices)
        for LI := LToRemove.Count - 1 downto 0 do
        begin
          // Convert to NOP instead of removing to avoid index issues
          LInstr := LBlock.GetInstruction(LToRemove[LI]);
          LInstr.Kind := sikNop;
          LInstr.Dest := TTigerSSAVar.None();
          LInstr.Op1 := TTigerSSAOperand.None();
          LInstr.Op2 := TTigerSSAOperand.None();
          LBlock.SetInstruction(LToRemove[LI], LInstr);
        end;
        
      finally
        LToRemove.Free();
      end;
    end;
    
  finally
    LUsedVars.Free();
  end;
end;

//==============================================================================
// TTigerSSACopyPropagation
//==============================================================================

constructor TTigerSSACopyPropagation.Create();
begin
  inherited Create(spkCopyPropagation, 'Copy Propagation', 2);  // Level 2
end;

procedure TTigerSSACopyPropagation.Run(const AFunc: TTigerSSAFunc);
var
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LCopies: TDictionary<string, TTigerSSAOperand>;  // Maps dest to source
  LVarKey: string;
  LSourceOp: TTigerSSAOperand;
  LChanged: Boolean;
  LArgIdx: Integer;
begin
  LCopies := TDictionary<string, TTigerSSAOperand>.Create();
  try
    // Pass 1: Collect all copy instructions (x = y where y is a var or immediate)
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      
      for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        
        // Check for: dest = var (simple copy)
        if (LInstr.Kind = sikAssign) and 
           (LInstr.Dest.IsValid()) and 
           (LInstr.Op1.Kind = sokVar) then
        begin
          LVarKey := LInstr.Dest.ToString();
          LCopies.AddOrSetValue(LVarKey, LInstr.Op1);
        end;
      end;
    end;
    
    // Pass 2: Replace uses of copied variables with their sources
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      
      for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        LChanged := False;
        
        // Replace Op1 if it's a copied variable
        if (LInstr.Op1.Kind = sokVar) then
        begin
          LVarKey := LInstr.Op1.Var_.ToString();
          if LCopies.TryGetValue(LVarKey, LSourceOp) then
          begin
            LInstr.Op1 := LSourceOp;
            LChanged := True;
          end;
        end;
        
        // Replace Op2 if it's a copied variable
        if (LInstr.Op2.Kind = sokVar) then
        begin
          LVarKey := LInstr.Op2.Var_.ToString();
          if LCopies.TryGetValue(LVarKey, LSourceOp) then
          begin
            LInstr.Op2 := LSourceOp;
            LChanged := True;
          end;
        end;
        
        // Replace call arguments
        for LArgIdx := 0 to High(LInstr.CallArgs) do
        begin
          if LInstr.CallArgs[LArgIdx].Kind = sokVar then
          begin
            LVarKey := LInstr.CallArgs[LArgIdx].Var_.ToString();
            if LCopies.TryGetValue(LVarKey, LSourceOp) then
            begin
              LInstr.CallArgs[LArgIdx] := LSourceOp;
              LChanged := True;
            end;
          end;
        end;
        
        if LChanged then
          LBlock.SetInstruction(LInstrIdx, LInstr);
      end;
    end;
    
  finally
    LCopies.Free();
  end;
end;

//==============================================================================
// TTigerSSACommonSubexprElim
//==============================================================================

constructor TTigerSSACommonSubexprElim.Create();
begin
  inherited Create(spkCommonSubexprElim, 'Common Subexpression Elimination', 2);  // Level 2
end;

procedure TTigerSSACommonSubexprElim.Run(const AFunc: TTigerSSAFunc);
var
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LExprMap: TDictionary<string, TTigerSSAVar>;  // Maps expression key to result var
  LExprKey: string;
  LExistingVar: TTigerSSAVar;
begin
  LExprMap := TDictionary<string, TTigerSSAVar>.Create();
  try
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      
      for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        
        // Only handle binary arithmetic/comparison ops
        if not (LInstr.Kind in [sikAdd, sikSub, sikMul, sikDiv, sikMod,
                                sikFAdd, sikFSub, sikFMul, sikFDiv,
                                sikBitAnd, sikBitOr, sikBitXor, sikShl, sikShr,
                                sikCmpEq, sikCmpNe, sikCmpLt, sikCmpLe, sikCmpGt, sikCmpGe]) then
          Continue;
        
        // Build expression key: "op:operand1:operand2"
        LExprKey := Format('%d:%s:%s', [
          Ord(LInstr.Kind),
          LInstr.Op1.ToString(),
          LInstr.Op2.ToString()
        ]);
        
        // Check if we've seen this expression before
        if LExprMap.TryGetValue(LExprKey, LExistingVar) then
        begin
          // Replace with copy from existing result
          LInstr.Kind := sikAssign;
          LInstr.Op1 := TTigerSSAOperand.FromVar(LExistingVar);
          LInstr.Op2 := TTigerSSAOperand.None();
          LBlock.SetInstruction(LInstrIdx, LInstr);
        end
        else
        begin
          // Record this expression
          LExprMap.Add(LExprKey, LInstr.Dest);
        end;
      end;
    end;
    
  finally
    LExprMap.Free();
  end;
end;

//==============================================================================
// TTigerSSAStringCleanup
//==============================================================================

constructor TTigerSSAStringCleanup.Create(const ASSABuilder: TTigerSSABuilder; const AIR: TTigerIR);
begin
  inherited Create(spkStringCleanup, 'String Cleanup', 0);  // Level 0 = always run
  FSSABuilder := ASSABuilder;
  FIR := AIR;
end;

procedure TTigerSSAStringCleanup.Run(const AFunc: TTigerSSAFunc);
var
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LLocalIdx: Integer;
  LGlobalIdx: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LLocal: TTigerSSALocalInfo;
  LGlobal: TTigerIR.TIRGlobal;
  LManagedLocals: TList<Integer>;  // Indices of managed non-param locals
  LManagedGlobals: TList<Integer>; // Indices of managed globals
  LManagedTemps: TDictionary<string, TTigerSSAVar>;  // Temps from string-returning calls
  LTempLastUse: TDictionary<string, Integer>;  // Temp name -> instruction index of last use
  LReleaseInstr: TTigerSSAInstr;
  LLoadInstr: TTigerSSAInstr;
  LFuncIdx: Integer;
  LTargetFuncIdx: Integer;
  LTargetFunc: TTigerIR.TIRFunc;
  LI: Integer;
  LJ: Integer;
  LIsEntryPoint: Boolean;
  LIsDllEntry: Boolean;
  LIsNoReturnCall: Boolean;
  LReleaseOnDetachFuncIdx: Integer;
  LReasonVar: TTigerSSAVar;
  LParamCount: Integer;
  LTempName: string;
  LTempVar: TTigerSSAVar;
  LUsedVar: string;
  LInsertions: TList<TPair<Integer, TTigerSSAVar>>;
  LPair: TPair<Integer, TTigerSSAVar>;
begin
  // Get function index for Tiger_StrRelease
  LFuncIdx := -1;
  if Assigned(FSSABuilder) then
    LFuncIdx := FSSABuilder.GetStrReleaseFuncIdx();
    
  //----------------------------------------------------------------------------
  // Phase 1: Release managed temps after their last use
  //----------------------------------------------------------------------------
  if (LFuncIdx >= 0) and Assigned(FIR) then
  begin
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      LManagedTemps := TDictionary<string, TTigerSSAVar>.Create();
      LTempLastUse := TDictionary<string, Integer>.Create();
      try
        // First pass: find managed temps and their last use in this block
        for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
        begin
          LInstr := LBlock.GetInstruction(LInstrIdx);
          
          // Check if this is a call that returns a managed string
          if (LInstr.Kind = sikCallAssign) and (LInstr.CallTarget.Kind = sokFunc) then
          begin
            LTargetFuncIdx := LInstr.CallTarget.FuncIndex;
            if (LTargetFuncIdx >= 0) and (LTargetFuncIdx < FIR.GetFunctionCount()) then
            begin
              LTargetFunc := FIR.GetFunction(LTargetFuncIdx);
              // Runtime string functions return vtPtr but are actually managed strings
              if (LTargetFunc.FuncName = 'Tiger_StrFromLiteral') or
                 (LTargetFunc.FuncName = 'Tiger_StrConcat') or
                 (LTargetFunc.FuncName = 'Tiger_StrFromChar') then
              begin
                // This call returns a string - track the dest temp
                LTempName := LInstr.Dest.BaseName + '_' + IntToStr(LInstr.Dest.Version);
                LManagedTemps.AddOrSetValue(LTempName, LInstr.Dest);
              end;
            end;
          end;
          
          // Track uses of managed temps in operands
          if LInstr.Op1.Kind = sokVar then
          begin
            LUsedVar := LInstr.Op1.Var_.BaseName + '_' + IntToStr(LInstr.Op1.Var_.Version);
            if LManagedTemps.ContainsKey(LUsedVar) then
              LTempLastUse.AddOrSetValue(LUsedVar, LInstrIdx);
          end;
          if LInstr.Op2.Kind = sokVar then
          begin
            LUsedVar := LInstr.Op2.Var_.BaseName + '_' + IntToStr(LInstr.Op2.Var_.Version);
            if LManagedTemps.ContainsKey(LUsedVar) then
              LTempLastUse.AddOrSetValue(LUsedVar, LInstrIdx);
          end;
          for LJ := 0 to Length(LInstr.CallArgs) - 1 do
          begin
            if LInstr.CallArgs[LJ].Kind = sokVar then
            begin
              LUsedVar := LInstr.CallArgs[LJ].Var_.BaseName + '_' + IntToStr(LInstr.CallArgs[LJ].Var_.Version);
              if LManagedTemps.ContainsKey(LUsedVar) then
                LTempLastUse.AddOrSetValue(LUsedVar, LInstrIdx);
            end;
          end;
        end;
        
        // Second pass: collect insertions (process in reverse order to preserve indices)
        LInsertions := TList<TPair<Integer, TTigerSSAVar>>.Create();
        try
          for LTempName in LTempLastUse.Keys do
          begin
            if LManagedTemps.TryGetValue(LTempName, LTempVar) then
              LInsertions.Add(TPair<Integer, TTigerSSAVar>.Create(LTempLastUse[LTempName], LTempVar));
          end;
          
          // Sort by instruction index descending (insert from end to start)
          LInsertions.Sort(
            TComparer<TPair<Integer, TTigerSSAVar>>.Construct(
              function(const ALeft, ARight: TPair<Integer, TTigerSSAVar>): Integer
              begin
                Result := ARight.Key - ALeft.Key;
              end
            )
          );
          
          // Insert releases after last use
          for LPair in LInsertions do
          begin
            LReleaseInstr := Default(TTigerSSAInstr);
            LReleaseInstr.Kind := sikCall;
            LReleaseInstr.CallTarget := TTigerSSAOperand.FromFunc(LFuncIdx);
            SetLength(LReleaseInstr.CallArgs, 1);
            LReleaseInstr.CallArgs[0] := TTigerSSAOperand.FromVar(LPair.Value);
            LBlock.InsertInstructionAt(LPair.Key + 1, LReleaseInstr);
          end;
        finally
          LInsertions.Free();
        end;
      finally
        LTempLastUse.Free();
        LManagedTemps.Free();
      end;
    end;
  end;
  
  //----------------------------------------------------------------------------
  // Phase 2: Collect managed locals and globals for cleanup at return
  //----------------------------------------------------------------------------
  LManagedLocals := TList<Integer>.Create();
  LManagedGlobals := TList<Integer>.Create();
  try
    for LLocalIdx := 0 to AFunc.GetLocalCount() - 1 do
    begin
      LLocal := AFunc.GetLocal(LLocalIdx);
      if LLocal.IsManaged and (not LLocal.IsParam) then
        LManagedLocals.Add(LLocalIdx);
    end;
    
    // Check if this is an entry point that needs global cleanup
    LIsEntryPoint := AFunc.GetIsEntryPoint();
    LIsDllEntry := AFunc.GetIsDllEntry();
    
    // Collect managed globals for entry points
    if (LIsEntryPoint or LIsDllEntry) and Assigned(FIR) then
    begin
      for LGlobalIdx := 0 to FIR.GetGlobalCount() - 1 do
      begin
        LGlobal := FIR.GetGlobal(LGlobalIdx);
        if FIR.IsStringType(LGlobal.GlobalTypeRef) then
          LManagedGlobals.Add(LGlobalIdx);
      end;
    end;
    
    // Get function indices for cleanup and reporting
    LFuncIdx := -1;
    if Assigned(FSSABuilder) then
      LFuncIdx := FSSABuilder.GetStrReleaseFuncIdx();
    
    // Check if there's anything to do
    // Entry points may still need to report leaks even with no managed vars
    if (LManagedLocals.Count = 0) and (LManagedGlobals.Count = 0) then
    begin
      // Nothing to clean up - but EXE entry points may still report leaks
      if not (LIsEntryPoint and Assigned(FSSABuilder) and (FSSABuilder.GetReportLeaksFuncIdx() >= 0)) then
        Exit;
    end
    else if LFuncIdx < 0 then
      Exit;  // Have managed vars but no Pax_StrRelease - cannot emit cleanup
    
    // Iterate all blocks to find return instructions
    for LBlockIdx := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LBlockIdx);
      LInstrIdx := 0;
      
      while LInstrIdx < LBlock.GetInstructionCount() do
      begin
        LInstr := LBlock.GetInstruction(LInstrIdx);
        
        // Check for return instructions OR noreturn calls (block ends with call, no successors)
        LIsNoReturnCall := (LInstr.Kind = sikCall) and
                           (LBlock.GetSuccessorCount() = 0) and
                           (LInstrIdx = LBlock.GetInstructionCount() - 1);
        
        if (LInstr.Kind in [sikReturn, sikReturnValue]) or LIsNoReturnCall then
        begin
          // Insert Tiger_StrRelease calls for each managed local BEFORE the return/noreturn call
          for LI := 0 to LManagedLocals.Count - 1 do
          begin
            LLocal := AFunc.GetLocal(LManagedLocals[LI]);
            
            // Load the local value first (bypasses SSA versioning to get actual stack value)
            LLoadInstr := Default(TTigerSSAInstr);
            LLoadInstr.Kind := sikLoad;
            LLoadInstr.Dest := AFunc.NewVersion('_t');
            LLoadInstr.Op1 := TTigerSSAOperand.FromLocalIndex(LManagedLocals[LI]);
            LBlock.InsertInstructionAt(LInstrIdx, LLoadInstr);
            Inc(LInstrIdx);
            
            // Create call to Tiger_StrRelease(loadedValue)
            LReleaseInstr := Default(TTigerSSAInstr);
            LReleaseInstr.Kind := sikCall;
            LReleaseInstr.CallTarget := TTigerSSAOperand.FromFunc(LFuncIdx);
            SetLength(LReleaseInstr.CallArgs, 1);
            LReleaseInstr.CallArgs[0] := TTigerSSAOperand.FromVar(LLoadInstr.Dest);
            
            // Insert before the return instruction
            LBlock.InsertInstructionAt(LInstrIdx, LReleaseInstr);
            Inc(LInstrIdx);
          end;
          
          // For entry points, also release managed globals
          // DLL entry uses Tiger_ReleaseOnDetach to check fdwReason before releasing
          if LIsEntryPoint then
          begin
            // EXE entry: unconditionally release all managed globals
            for LI := 0 to LManagedGlobals.Count - 1 do
            begin
              // Load the global value first
              LLoadInstr := Default(TTigerSSAInstr);
              LLoadInstr.Kind := sikLoad;
              LLoadInstr.Dest := AFunc.NewVersion('_t');
              LLoadInstr.Op1 := TTigerSSAOperand.FromGlobal(LManagedGlobals[LI]);
              LBlock.InsertInstructionAt(LInstrIdx, LLoadInstr);
              Inc(LInstrIdx);
              
              // Create call to Tiger_StrRelease(globalValue)
              LReleaseInstr := Default(TTigerSSAInstr);
              LReleaseInstr.Kind := sikCall;
              LReleaseInstr.CallTarget := TTigerSSAOperand.FromFunc(LFuncIdx);
              SetLength(LReleaseInstr.CallArgs, 1);
              LReleaseInstr.CallArgs[0] := TTigerSSAOperand.FromVar(LLoadInstr.Dest);
              
              LBlock.InsertInstructionAt(LInstrIdx, LReleaseInstr);
              Inc(LInstrIdx);
            end;
            
            // Call Tiger_ReportLeaks after all cleanup (debug mode only)
            // Only for EXE entry points, not DLL entries (DllMain fires on
            // both ATTACH and DETACH, which would produce duplicate reports)
            // Skip if the noreturn call is Tiger_Halt (it already calls ReportLeaks)
            if Assigned(FSSABuilder) and (FSSABuilder.GetReportLeaksFuncIdx() >= 0) and (not LIsNoReturnCall) then
            begin
              LReleaseInstr := Default(TTigerSSAInstr);
              LReleaseInstr.Kind := sikCall;
              LReleaseInstr.CallTarget := TTigerSSAOperand.FromFunc(FSSABuilder.GetReportLeaksFuncIdx());
              SetLength(LReleaseInstr.CallArgs, 0);  // No arguments
              LBlock.InsertInstructionAt(LInstrIdx, LReleaseInstr);
              Inc(LInstrIdx);
            end;
          end
          else if LIsDllEntry then
          begin
            // DLL entry: use Tiger_ReleaseOnDetach which checks fdwReason
            LReleaseOnDetachFuncIdx := -1;
            if Assigned(FSSABuilder) then
              LReleaseOnDetachFuncIdx := FSSABuilder.GetReleaseOnDetachFuncIdx();
            
            if LReleaseOnDetachFuncIdx >= 0 then
            begin
              // Find fdwReason parameter (2nd parameter, index 1)
              // DllMain signature: DllMain(hinstDLL, fdwReason, lpvReserved)
              LParamCount := 0;
              LReasonVar := TTigerSSAVar.None();
              for LI := 0 to AFunc.GetLocalCount() - 1 do
              begin
                LLocal := AFunc.GetLocal(LI);
                if LLocal.IsParam then
                begin
                  if LParamCount = 1 then  // fdwReason is 2nd param (index 1)
                  begin
                    LReasonVar := TTigerSSAVar.Create(LLocal.LocalName, 0);
                    Break;
                  end;
                  Inc(LParamCount);
                end;
              end;
              
              if LReasonVar.IsValid() then
              begin
                for LI := 0 to LManagedGlobals.Count - 1 do
                begin
                  // Load the global value first
                  LLoadInstr := Default(TTigerSSAInstr);
                  LLoadInstr.Kind := sikLoad;
                  LLoadInstr.Dest := AFunc.NewVersion('_t');
                  LLoadInstr.Op1 := TTigerSSAOperand.FromGlobal(LManagedGlobals[LI]);
                  LBlock.InsertInstructionAt(LInstrIdx, LLoadInstr);
                  Inc(LInstrIdx);
                  
                  // Create call to Tiger_ReleaseOnDetach(fdwReason, globalValue)
                  LReleaseInstr := Default(TTigerSSAInstr);
                  LReleaseInstr.Kind := sikCall;
                  LReleaseInstr.CallTarget := TTigerSSAOperand.FromFunc(LReleaseOnDetachFuncIdx);
                  SetLength(LReleaseInstr.CallArgs, 2);
                  LReleaseInstr.CallArgs[0] := TTigerSSAOperand.FromVar(LReasonVar);
                  LReleaseInstr.CallArgs[1] := TTigerSSAOperand.FromVar(LLoadInstr.Dest);
                  
                  LBlock.InsertInstructionAt(LInstrIdx, LReleaseInstr);
                  Inc(LInstrIdx);
                end;
              end;
            end;
          end;
          
          // Skip past the return instruction itself
          Inc(LInstrIdx);
        end
        else
          Inc(LInstrIdx);
      end;
    end;
    
  finally
    LManagedLocals.Free();
    LManagedGlobals.Free();
  end;
end;

//==============================================================================
// TTigerSSABuilder
//==============================================================================

constructor TTigerSSABuilder.Create();
begin
  inherited Create();
  
  FOptions := TTigerSSAOptions.Release();
  FFunctions := TObjectList<TTigerSSAFunc>.Create(True);
  FCurrentFuncIndex := -1;
  FPasses := TObjectList<TTigerSSAPass>.Create(True);
  FExpressionCache := TDictionary<Integer, TTigerSSAVar>.Create();
  
  // Add default optimization passes (order matters!)
  // Level 1: Constant propagation, constant folding, DCE
  // Level 2: + Copy propagation, CSE
  FPasses.Add(TTigerSSAConstantPropagation.Create());   // Level 1
  FPasses.Add(TTigerSSAConstantFolding.Create());       // Level 1
  FPasses.Add(TTigerSSACopyPropagation.Create());       // Level 2
  FPasses.Add(TTigerSSACommonSubexprElim.Create());     // Level 2
  FPasses.Add(TTigerSSACopyPropagation.Create());       // Level 2 - run again to propagate CSE copies
  FPasses.Add(TTigerSSADeadCodeElimination.Create());   // Level 1 (always last)
end;

destructor TTigerSSABuilder.Destroy();
begin
  FExpressionCache.Free();
  FPasses.Free();
  FFunctions.Free();
  
  inherited Destroy();
end;

procedure TTigerSSABuilder.SetOptions(const AOptions: TTigerSSAOptions);
begin
  FOptions := AOptions;
end;

function TTigerSSABuilder.GetOptions(): TTigerSSAOptions;
begin
  Result := FOptions;
end;

procedure TTigerSSABuilder.BuildFrom(const AIR: TTigerIR);
var
  LI: Integer;
  LJ: Integer;
  LFunc: TTigerIR.TIRFunc;
  LSSAFunc: TTigerSSAFunc;
  LVar: TTigerIR.TIRVar;
  LVarSize: Integer;
  LVarAlign: Integer;
  LImport: TTigerIR.TIRImport;
  LStr: TTigerIR.TIRString;
  LStrCleanup: TTigerSSAStringCleanup;
begin
  Clear();
  
  Status('SSA: Building from high-level IR (%d functions)', [AIR.GetFunctionCount()]);
  
  //----------------------------------------------------------------------------
  // Step 1: Store import count for later reference
  //----------------------------------------------------------------------------
  SetLength(FImportHandles, AIR.GetImportCount());
  FStrReleaseFuncIdx := -1;  // Initialize to invalid - will search after function conversion
  FReportLeaksFuncIdx := -1;
  FReleaseOnDetachFuncIdx := -1;
  for LI := 0 to AIR.GetImportCount() - 1 do
  begin
    LImport := AIR.GetImport(LI);
    Status('SSA: Import[%d]: %s.%s', [LI, LImport.DllName, LImport.FuncName]);
  end;
  
  //----------------------------------------------------------------------------
  // Step 2: Store string count for later reference
  //----------------------------------------------------------------------------
  SetLength(FStringHandles, AIR.GetStringCount());
  for LI := 0 to AIR.GetStringCount() - 1 do
  begin
    LStr := AIR.GetString(LI);
    Status('SSA: String[%d]: "%s"', [LI, LStr.Value.Replace(#13, '\r').Replace(#10, '\n')]);
  end;
  
  //----------------------------------------------------------------------------
  // Step 3: Convert each function
  //----------------------------------------------------------------------------
  for LI := 0 to AIR.GetFunctionCount() - 1 do
  begin
    LFunc := AIR.GetFunction(LI);
    Status('SSA: Converting function: %s', [LFunc.FuncName]);
    
    // Create SSA function
    LSSAFunc := TTigerSSAFunc.Create(LFunc.FuncName);
    LSSAFunc.SetReturnType(LFunc.ReturnType, LFunc.ReturnSize, LFunc.ReturnAlignment);
    LSSAFunc.SetIsEntryPoint(LFunc.IsEntryPoint);
    LSSAFunc.SetIsDllEntry(LFunc.IsDllEntry);
    LSSAFunc.SetIsPublic(LFunc.IsPublic);
    LSSAFunc.SetLinkage(LFunc.Linkage);
    LSSAFunc.SetIsVariadic(LFunc.IsVariadic);
    
    // Add params and locals
    for LJ := 0 to LFunc.Vars.Count - 1 do
    begin
      LVar := LFunc.Vars[LJ];
      // Calculate size and alignment for this variable
      if LVar.VarTypeRef.IsPrimitive then
      begin
        LVarSize := 8;   // All primitives use 8 bytes on stack (aligned)
        LVarAlign := 8;  // Primitives align to 8 bytes
      end
      else
      begin
        LVarSize := AIR.GetTypeSize(LVar.VarTypeRef);       // Get composite type size
        LVarAlign := AIR.GetTypeAlignment(LVar.VarTypeRef); // Get composite type alignment
      end;
      LSSAFunc.AddLocal(LVar.VarName, LVar.VarTypeRef, LVarSize, LVarAlign, LVar.IsParam, AIR.IsStringType(LVar.VarTypeRef));
    end;
    
    // Create entry block
    LSSAFunc.CreateBlock('entry');
    LSSAFunc.SetCurrentBlock(0);
    
    // Clear expression cache for this function
    FExpressionCache.Clear();
    
    // Convert statements to SSA
    ConvertStatements(AIR, LI, LSSAFunc);
    
    // SSA construction: compute dominators, insert phi nodes, rename
    ComputeDominators(LSSAFunc);
    ComputeDominanceFrontiers(LSSAFunc);
    InsertPhiNodes(LSSAFunc);
    RenameVariables(LSSAFunc);
    
    // Add function to list
    FFunctions.Add(LSSAFunc);
    
    // Track Pax_StrRelease function index for string cleanup pass
    if LFunc.FuncName = 'Tiger_StrRelease' then
      FStrReleaseFuncIdx := FFunctions.Count - 1;
    
    // Track Pax_ReportLeaks function index (debug mode only)
    if LFunc.FuncName = 'Tiger_ReportLeaks' then
      FReportLeaksFuncIdx := FFunctions.Count - 1;
    
    // Track Tiger_ReleaseOnDetach function index for DLL cleanup
    if LFunc.FuncName = 'Tiger_ReleaseOnDetach' then
      FReleaseOnDetachFuncIdx := FFunctions.Count - 1;
  end;
  
  //----------------------------------------------------------------------------
  // Step 4: Run mandatory string cleanup pass
  //----------------------------------------------------------------------------
  if FStrReleaseFuncIdx >= 0 then
  begin
    Status('SSA: Running string cleanup pass');
    LStrCleanup := TTigerSSAStringCleanup.Create(Self, AIR);
    try
      for LI := 0 to FFunctions.Count - 1 do
        LStrCleanup.Run(FFunctions[LI]);
    finally
      LStrCleanup.Free();
    end;
  end;
  
  Status('SSA: Conversion complete (%d functions)', [FFunctions.Count]);
end;

procedure TTigerSSABuilder.ConvertStatements(
  const AIR: TTigerIR;
  const AFuncIndex: Integer;
  const ASSAFunc: TTigerSSAFunc
);
var
  LFunc: TTigerIR.TIRFunc;
  LStmt: TTigerIR.TIRStmt;
  LI: Integer;
  LInstr: TTigerSSAInstr;
  LDestVar: TTigerSSAVar;
  LExprVar: TTigerSSAVar;
  LCondVar: TTigerSSAVar;
  LArgVar: TTigerSSAVar;
  LLeftVar: TTigerSSAVar;
  LRightVar: TTigerSSAVar;
  LTempVar: TTigerSSAVar;
  LDestExprNode: TTigerIR.TIRExprNode;
  LObjectExpr: TTigerIR.TIRExprNode;
  LBlockStack: TStack<Integer>;  // Stack of block indices for control flow
  LForVarStack: TStack<string>;   // Stack of for-loop variable names
  LForDownToStack: TStack<Boolean>; // Stack of for-loop direction flags
  LForVar: string;
  LIsDownTo: Boolean;
  LThenBlock: Integer;
  LElseBlock: Integer;
  LEndBlock: Integer;
  LLoopBlock: Integer;
  LBodyBlock: Integer;
  LArgIdx: Integer;
  LFuncIdx: Integer;
  LCallArgs: TArray<TTigerSSAOperand>;
  LCurrentBlockId: Integer;
  LHeaderBlock: Integer;
  LPatchIdx: Integer;
  // Exception handling
  LTryBlock: Integer;
  LExceptBlock: Integer;
  LFinallyBlock: Integer;
  LTryEndBlock: Integer;
  LExcScope: TTigerSSAExceptionScope;
  // Variadic call handling
  LTargetFunc: TTigerIR.TIRFunc;
  LFixedParamCount: Integer;
  LVarArgCount: Integer;
  LJ: Integer;
  LImportIdx: Integer;
  // For by-reference parameter detection in field access
  LIsParam: Boolean;
  LLocalSize: Integer;
  LLocalIdx: Integer;
  LLocalInfo: TTigerSSALocalInfo;
begin
  LFunc := AIR.GetFunction(AFuncIndex);
  LBlockStack := TStack<Integer>.Create();
  LForVarStack := TStack<string>.Create();
  LForDownToStack := TStack<Boolean>.Create();
  try
    for LI := 0 to LFunc.Stmts.Count - 1 do
    begin
      LStmt := LFunc.Stmts[LI];
      
      case LStmt.Kind of
        TTigerIR.TIRStmtKind.skAssign:
          begin
            // Convert source expression
            LExprVar := ConvertExpression(AIR, LStmt.Expr, ASSAFunc);
            
            // Check if destination is an expression (field/index access)
            if LStmt.DestExpr >= 0 then
            begin
              // Get destination expression
              LDestExprNode := AIR.GetExpression(LStmt.DestExpr);
              
              if LDestExprNode.Kind = TTigerIR.TIRExprKind.ekFieldAccess then
              begin
                // Field access: compute address and store
                // Get the inner object expression
                LObjectExpr := AIR.GetExpression(LDestExprNode.ObjectExpr);
                
                // For variable expressions with composite types, we need the address
                if (LObjectExpr.Kind = TTigerIR.TIRExprKind.ekVariable) and 
                   (not LObjectExpr.ResultType.IsPrimitive) then
                begin
                  // Check if this is a parameter passed by reference (composite > 8 bytes)
                  // Win64 ABI: Large structs are passed by pointer, so param value IS the address
                  LIsParam := False;
                  LLocalSize := 0;
                  for LLocalIdx := 0 to ASSAFunc.GetLocalCount() - 1 do
                  begin
                    LLocalInfo := ASSAFunc.GetLocal(LLocalIdx);
                    if LLocalInfo.LocalName = LObjectExpr.VarName then
                    begin
                      LIsParam := LLocalInfo.IsParam;
                      LLocalSize := LLocalInfo.LocalSize;
                      Break;
                    end;
                  end;
                  
                  // Emit AddressOf instruction to get local's/param's stack address
                  LLeftVar := ASSAFunc.NewVersion('_t');
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikAddressOf;
                  LInstr.Dest := LLeftVar;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LObjectExpr.VarName));
                  LInstr.Op1.Var_.BaseName := LObjectExpr.VarName;
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  
                  if LIsParam and (LLocalSize > 8) then
                  begin
                    // By-reference parameter: param slot contains pointer to struct
                    // Load the pointer value from the param slot
                    LTempVar := LLeftVar;
                    LLeftVar := ASSAFunc.NewVersion('_t');
                    LInstr := Default(TTigerSSAInstr);
                    LInstr.Kind := sikLoad;
                    LInstr.Dest := LLeftVar;
                    LInstr.Op1 := TTigerSSAOperand.FromVar(LTempVar);
                    ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  end;
                end
                else if (LObjectExpr.Kind = TTigerIR.TIRExprKind.ekUnary) and
                        (LObjectExpr.Op = TTigerIR.TIROpKind.opDeref) then
                begin
                  // For Deref(ptr, TypeName), we want just the pointer value as base.
                  // Don't generate a load - convert only the inner pointer expression.
                  LLeftVar := ConvertExpression(AIR, LObjectExpr.Left, ASSAFunc);
                end
                else
                begin
                  // For other expressions (pointers, etc.), convert normally
                  LLeftVar := ConvertExpression(AIR, LDestExprNode.ObjectExpr, ASSAFunc);
                end;
                
                // Compute field address
                LTempVar := ASSAFunc.NewVersion('_t');
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikFieldAddr;
                LInstr.Dest := LTempVar;
                LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
                LInstr.Op2 := TTigerSSAOperand.FromImm(LDestExprNode.FieldOffset);
                LInstr.Op2.FieldName := LDestExprNode.FieldName;
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                
                // Check if this is a bit field
                if LDestExprNode.BitWidth > 0 then
                begin
                  // Bit field write: read-modify-write
                  // 1. Load current storage unit value
                  LLeftVar := LTempVar;  // Address
                  LRightVar := ASSAFunc.NewVersion('_t');
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikLoad;
                  LInstr.Dest := LRightVar;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  
                  // 2. Clear target bits: current & ~(mask << BitOffset)
                  // mask = (1 << BitWidth) - 1
                  // clearMask = ~(mask << BitOffset)
                  LTempVar := ASSAFunc.NewVersion('_t');
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikBitAnd;
                  LInstr.Dest := LTempVar;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(LRightVar);
                  LInstr.Op2 := TTigerSSAOperand.FromImm(not (((Int64(1) shl LDestExprNode.BitWidth) - 1) shl LDestExprNode.BitOffset));
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  
                  // 3. Mask new value to BitWidth bits
                  LRightVar := ASSAFunc.NewVersion('_t');
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikBitAnd;
                  LInstr.Dest := LRightVar;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(LExprVar);
                  LInstr.Op2 := TTigerSSAOperand.FromImm((Int64(1) shl LDestExprNode.BitWidth) - 1);
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  
                  // 4. Shift new value left by BitOffset
                  if LDestExprNode.BitOffset > 0 then
                  begin
                    LExprVar := LRightVar;
                    LRightVar := ASSAFunc.NewVersion('_t');
                    LInstr := Default(TTigerSSAInstr);
                    LInstr.Kind := sikShl;
                    LInstr.Dest := LRightVar;
                    LInstr.Op1 := TTigerSSAOperand.FromVar(LExprVar);
                    LInstr.Op2 := TTigerSSAOperand.FromImm(LDestExprNode.BitOffset);
                    ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  end;
                  
                  // 5. OR the shifted value into cleared storage
                  LExprVar := ASSAFunc.NewVersion('_t');
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikBitOr;
                  LInstr.Dest := LExprVar;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(LTempVar);
                  LInstr.Op2 := TTigerSSAOperand.FromVar(LRightVar);
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                  
                  // 6. Store the combined value back
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikStore;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);  // Original address
                  LInstr.Op2 := TTigerSSAOperand.FromVar(LExprVar);
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                end
                else
                begin
                  // Normal field: direct store (sized for sub-register fields)
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikStore;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(LTempVar);
                  LInstr.Op2 := TTigerSSAOperand.FromVar(LExprVar);
                  LInstr.MemSize := LDestExprNode.FieldSize;
                  LInstr.MemIsFloat := LDestExprNode.ResultType.IsPrimitive and
                    (LDestExprNode.ResultType.Primitive in [vtFloat32, vtFloat64]);
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                end;
              end
              else if LDestExprNode.Kind = TTigerIR.TIRExprKind.ekArrayIndex then
              begin
                // Array index: compute element address and store
                // Get the array expression node
                LObjectExpr := AIR.GetExpression(LDestExprNode.ArrayExpr);
                
                // For variable expressions with composite types (arrays), we need the address
                if (LObjectExpr.Kind = TTigerIR.TIRExprKind.ekVariable) and 
                   (not LObjectExpr.ResultType.IsPrimitive) then
                begin
                  // Emit AddressOf instruction to get local's stack address
                  LLeftVar := ASSAFunc.NewVersion('_t');
                  LInstr := Default(TTigerSSAInstr);
                  LInstr.Kind := sikAddressOf;
                  LInstr.Dest := LLeftVar;
                  LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LObjectExpr.VarName));
                  LInstr.Op1.Var_.BaseName := LObjectExpr.VarName;
                  ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                end
                else
                begin
                  // For other expressions (pointers, etc.), convert normally
                  LLeftVar := ConvertExpression(AIR, LDestExprNode.ArrayExpr, ASSAFunc);
                end;
                
                LRightVar := ConvertExpression(AIR, LDestExprNode.IndexExpr, ASSAFunc);
                
                // Compute element address
                LTempVar := ASSAFunc.NewVersion('_t');
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikIndexAddr;
                LInstr.Dest := LTempVar;
                LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
                LInstr.Op2 := TTigerSSAOperand.FromVar(LRightVar);
                LInstr.Op2.ElementSize := LDestExprNode.ElementSize;
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
                
                // Store value to element address (sized to element)
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikStore;
                LInstr.Op1 := TTigerSSAOperand.FromVar(LTempVar);
                LInstr.Op2 := TTigerSSAOperand.FromVar(LExprVar);
                LInstr.MemSize := LDestExprNode.ElementSize;
                LInstr.MemIsFloat := LDestExprNode.ResultType.IsPrimitive and
                  (LDestExprNode.ResultType.Primitive in [vtFloat32, vtFloat64]);
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              end
              else if (LDestExprNode.Kind = TTigerIR.TIRExprKind.ekUnary) and 
                      (LDestExprNode.Op = TTigerIR.TIROpKind.opDeref) then
              begin
                // Deref: convert pointer expression and store to it
                LLeftVar := ConvertExpression(AIR, LDestExprNode.Left, ASSAFunc);
                
                // Store value to dereferenced address
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikStore;
                LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
                LInstr.Op2 := TTigerSSAOperand.FromVar(LExprVar);
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              end;
            end
            else
            begin
              // Check if destination is a global variable
              LArgIdx := AIR.FindGlobal(LStmt.DestVar);
              if LArgIdx >= 0 then
              begin
                // Global variable - emit store to global address
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikStore;
                LInstr.Op1 := TTigerSSAOperand.FromGlobal(LArgIdx);
                LInstr.Op2 := TTigerSSAOperand.FromVar(LExprVar);
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              end
              else
              begin
                // Local variable - assign to new version
                LDestVar := ASSAFunc.NewVersion(LStmt.DestVar);
                
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikAssign;
                LInstr.Dest := LDestVar;
                LInstr.Op1 := TTigerSSAOperand.FromVar(LExprVar);
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              end;
            end;
          end;
          
        TTigerIR.TIRStmtKind.skCall:
          begin
            // Convert call arguments
            SetLength(LCallArgs, Length(LStmt.CallArgs));
            for LArgIdx := 0 to High(LStmt.CallArgs) do
            begin
              LArgVar := ConvertExpression(AIR, LStmt.CallArgs[LArgIdx], ASSAFunc);
              LCallArgs[LArgIdx] := TTigerSSAOperand.FromVar(LArgVar);
            end;
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikCall;
            LInstr.CallArgs := LCallArgs;
            
            // Check if this is a local function call first
            LFuncIdx := FindLocalFuncByName(AIR, LStmt.CallTarget);
            if LFuncIdx >= 0 then
            begin
              LInstr.CallTarget := TTigerSSAOperand.FromFunc(LFuncIdx);
              
              // For internal variadic functions, prepend vararg count
              LTargetFunc := AIR.GetFunction(LFuncIdx);
              if LTargetFunc.IsVariadic then
              begin
                // Count fixed params in target function
                LFixedParamCount := 0;
                for LJ := 0 to LTargetFunc.Vars.Count - 1 do
                  if LTargetFunc.Vars[LJ].IsParam then
                    Inc(LFixedParamCount);
                
                // Calculate vararg count
                LVarArgCount := Length(LStmt.CallArgs) - LFixedParamCount;
                if LVarArgCount < 0 then
                  LVarArgCount := 0;
                
                // Prepend count to args array
                SetLength(LCallArgs, Length(LCallArgs) + 1);
                for LArgIdx := High(LCallArgs) downto 1 do
                  LCallArgs[LArgIdx] := LCallArgs[LArgIdx - 1];
                LCallArgs[0] := TTigerSSAOperand.FromImm(LVarArgCount);
                LInstr.CallArgs := LCallArgs;
              end;
            end
            else
            begin
              LImportIdx := FindImportByName(AIR, LStmt.CallTarget);
              if LImportIdx < 0 then
              begin
                if Assigned(FErrors) then
                  FErrors.Add(esError, ERR_SSA_UNKNOWN_FUNCTION, RSSSAUnknownFunction, [LStmt.CallTarget]);
                Exit;
              end;
              LInstr.CallTarget := TTigerSSAOperand.FromImport(LImportIdx);
            end;
            
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
          
        TTigerIR.TIRStmtKind.skCallIndirect:
          begin
            // Convert the function pointer expression
            LExprVar := ConvertExpression(AIR, LStmt.IndirectTarget, ASSAFunc);
            
            // Convert call arguments
            SetLength(LCallArgs, Length(LStmt.CallArgs));
            for LArgIdx := 0 to High(LStmt.CallArgs) do
            begin
              LArgVar := ConvertExpression(AIR, LStmt.CallArgs[LArgIdx], ASSAFunc);
              LCallArgs[LArgIdx] := TTigerSSAOperand.FromVar(LArgVar);
            end;
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikIndirectCall;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LExprVar);  // Function pointer
            LInstr.CallArgs := LCallArgs;
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;

        TTigerIR.TIRStmtKind.skSyscall:
          begin
            // Convert syscall arguments
            SetLength(LCallArgs, Length(LStmt.CallArgs));
            for LArgIdx := 0 to High(LStmt.CallArgs) do
            begin
              LArgVar := ConvertExpression(AIR, LStmt.CallArgs[LArgIdx], ASSAFunc);
              LCallArgs[LArgIdx] := TTigerSSAOperand.FromVar(LArgVar);
            end;

            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikSyscall;
            LInstr.SyscallNr := LStmt.SyscallNr;
            LInstr.CallArgs := LCallArgs;
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
          
        TTigerIR.TIRStmtKind.skReturn:
          begin
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikReturn;
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
          
        TTigerIR.TIRStmtKind.skReturnValue:
          begin
            LExprVar := ConvertExpression(AIR, LStmt.Expr, ASSAFunc);
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikReturnValue;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LExprVar);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
          
        TTigerIR.TIRStmtKind.skIfBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Evaluate condition
            LCondVar := ConvertExpression(AIR, LStmt.Expr, ASSAFunc);
            
            // Create then and end blocks
            LThenBlock := ASSAFunc.CreateBlock('if_then');
            LEndBlock := ASSAFunc.CreateBlock('if_end');
            
            // Add edges: current -> then (the false edge will be added later)
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LThenBlock);
            ASSAFunc.GetBlock(LThenBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump to then block if condition true
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJumpIf;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LCondVar);
            LInstr.Op2 := TTigerSSAOperand.FromBlock(LThenBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Jump to end block if condition false (may be patched to else by skElseBegin)
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Add edge: current -> end (false path, may be updated if else exists)
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // Push condition block id and end block for later
            LBlockStack.Push(LCurrentBlockId);  // Need this to patch the jump if else exists
            LBlockStack.Push(LEndBlock);
            
            // Continue in then block
            ASSAFunc.SetCurrentBlock(LThenBlock);
          end;
          
        TTigerIR.TIRStmtKind.skElseBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop end block and condition block from stack
            LEndBlock := LBlockStack.Pop();
            LHeaderBlock := LBlockStack.Pop();  // This is the condition block
            
            // Jump to end from current (then) block
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Add edge: then -> end
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // Create else block
            LElseBlock := ASSAFunc.CreateBlock('if_else');
            
            // Patch the condition block's Jump instruction to point to else instead of end
            // The Jump is the last instruction in the condition block
            LPatchIdx := ASSAFunc.GetBlock(LHeaderBlock).GetInstructionCount() - 1;
            LInstr := ASSAFunc.GetBlock(LHeaderBlock).GetInstruction(LPatchIdx);
            if LInstr.Kind = sikJump then
            begin
              LInstr.Op1 := TTigerSSAOperand.FromBlock(LElseBlock);
              ASSAFunc.GetBlock(LHeaderBlock).SetInstruction(LPatchIdx, LInstr);
            end;
            
            // Update edges: remove condition -> end, add condition -> else
            // Note: We can't easily remove edges, so we'll have duplicate edges
            // The dominator algorithm handles this, but let's add the correct edge
            ASSAFunc.GetBlock(LHeaderBlock).AddSuccessor(LElseBlock);
            ASSAFunc.GetBlock(LElseBlock).AddPredecessor(LHeaderBlock);
            
            // Push only end block back (marker that else was processed)
            LBlockStack.Push(-1);  // Marker: else exists
            LBlockStack.Push(LEndBlock);
            
            // Continue in else block
            ASSAFunc.SetCurrentBlock(LElseBlock);
          end;
          
        TTigerIR.TIRStmtKind.skIfEnd:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop end block
            LEndBlock := LBlockStack.Pop();
            
            // Pop marker (else was processed)
            LBlockStack.Pop();
            
            // Jump to end block from current
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Add edge: current -> end
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // If LHeaderBlock is -1, else was processed (nothing more to do)
            // If LHeaderBlock >= 0, it's the condition block ID (no else case)
            // In no-else case, the condition->end edge was already added in skIfBegin
            
            // Continue in end block
            ASSAFunc.SetCurrentBlock(LEndBlock);
          end;
          
        TTigerIR.TIRStmtKind.skWhileBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Create header, body and end blocks
            LLoopBlock := ASSAFunc.CreateBlock('while_header');
            LBodyBlock := ASSAFunc.CreateBlock('while_body');
            LEndBlock := ASSAFunc.CreateBlock('while_end');
            
            // Add edge: entry -> header
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LLoopBlock);
            ASSAFunc.GetBlock(LLoopBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump to header from entry
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LLoopBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Switch to header: evaluate condition
            ASSAFunc.SetCurrentBlock(LLoopBlock);
            LCondVar := ConvertExpression(AIR, LStmt.Expr, ASSAFunc);
            
            // Add edges: header -> body (true), header -> end (false)
            ASSAFunc.GetBlock(LLoopBlock).AddSuccessor(LBodyBlock);
            ASSAFunc.GetBlock(LLoopBlock).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LBodyBlock).AddPredecessor(LLoopBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LLoopBlock);
            
            // Jump to body if true
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJumpIf;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LCondVar);
            LInstr.Op2 := TTigerSSAOperand.FromBlock(LBodyBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Jump to end if false
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Push loop header and end for WhileEnd
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(LLoopBlock);
            
            // Continue in body
            ASSAFunc.SetCurrentBlock(LBodyBlock);
          end;
          
        TTigerIR.TIRStmtKind.skWhileEnd:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop header and end blocks
            LLoopBlock := LBlockStack.Pop();
            LEndBlock := LBlockStack.Pop();
            
            // Add edge: body -> header (back edge)
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LLoopBlock);
            ASSAFunc.GetBlock(LLoopBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump back to header
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LLoopBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Continue in end block
            ASSAFunc.SetCurrentBlock(LEndBlock);
          end;
          
        TTigerIR.TIRStmtKind.skForBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Evaluate from and to expressions in current block (before loop)
            LExprVar := ConvertExpression(AIR, LStmt.ForFrom, ASSAFunc);
            LRightVar := ConvertExpression(AIR, LStmt.ForTo, ASSAFunc);
            
            // Assign loop variable = from value
            LDestVar := ASSAFunc.NewVersion(LStmt.ForVar);
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikAssign;
            LInstr.Dest := LDestVar;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LExprVar);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Create header, body, and end blocks
            LLoopBlock := ASSAFunc.CreateBlock('for_header');
            LBodyBlock := ASSAFunc.CreateBlock('for_body');
            LEndBlock := ASSAFunc.CreateBlock('for_end');
            
            // Add edge: current -> header
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LLoopBlock);
            ASSAFunc.GetBlock(LLoopBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump to header
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LLoopBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Switch to header block: emit comparison
            ASSAFunc.SetCurrentBlock(LLoopBlock);
            
            // Compare loop variable with to-value
            LCondVar := ASSAFunc.NewVersion('_t');
            LInstr := Default(TTigerSSAInstr);
            if not LStmt.ForDownTo then
              LInstr.Kind := sikCmpLe   // forVar <= to (ascending)
            else
              LInstr.Kind := sikCmpGe;  // forVar >= to (descending)
            LInstr.Dest := LCondVar;
            LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LStmt.ForVar));
            LInstr.Op2 := TTigerSSAOperand.FromVar(LRightVar);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Add edges: header -> body (true), header -> end (false)
            ASSAFunc.GetBlock(LLoopBlock).AddSuccessor(LBodyBlock);
            ASSAFunc.GetBlock(LLoopBlock).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LBodyBlock).AddPredecessor(LLoopBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LLoopBlock);
            
            // Branch: if condition true -> body, else -> end
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJumpIf;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LCondVar);
            LInstr.Op2 := TTigerSSAOperand.FromBlock(LBodyBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Fall through to end
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Push blocks and loop info for ForEnd
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(LLoopBlock);
            LForVarStack.Push(LStmt.ForVar);
            LForDownToStack.Push(LStmt.ForDownTo);
            
            // Continue in body block
            ASSAFunc.SetCurrentBlock(LBodyBlock);
          end;
          
        TTigerIR.TIRStmtKind.skForEnd:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop loop info
            LLoopBlock := LBlockStack.Pop();
            LEndBlock := LBlockStack.Pop();
            LForVar := LForVarStack.Pop();
            LIsDownTo := LForDownToStack.Pop();
            
            // Increment (or decrement) loop variable
            LTempVar := ASSAFunc.NewVersion('_t');
            LInstr := Default(TTigerSSAInstr);
            if not LIsDownTo then
              LInstr.Kind := sikAdd
            else
              LInstr.Kind := sikSub;
            LInstr.Dest := LTempVar;
            LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LForVar));
            LInstr.Op2 := TTigerSSAOperand.FromImm(Int64(1));
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Assign new version of loop variable
            LDestVar := ASSAFunc.NewVersion(LForVar);
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikAssign;
            LInstr.Dest := LDestVar;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LTempVar);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Add edge: body -> header (back edge)
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LLoopBlock);
            ASSAFunc.GetBlock(LLoopBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump back to header
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LLoopBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Continue in end block
            ASSAFunc.SetCurrentBlock(LEndBlock);
          end;
          
        TTigerIR.TIRStmtKind.skRepeatBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Create body block
            LBodyBlock := ASSAFunc.CreateBlock('repeat_body');
            
            // Add edge: entry -> body
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LBodyBlock);
            ASSAFunc.GetBlock(LBodyBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump to body
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LBodyBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Push body block for RepeatEnd
            LBlockStack.Push(LBodyBlock);
            
            // Continue in body
            ASSAFunc.SetCurrentBlock(LBodyBlock);
          end;
          
        TTigerIR.TIRStmtKind.skRepeatEnd:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop body block
            LBodyBlock := LBlockStack.Pop();
            
            // Evaluate until condition
            LCondVar := ConvertExpression(AIR, LStmt.Expr, ASSAFunc);
            
            // Create end block
            LEndBlock := ASSAFunc.CreateBlock('repeat_end');
            
            // Add edges: current -> body (back edge if false), current -> end (if true)
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LBodyBlock);
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LBodyBlock).AddPredecessor(LCurrentBlockId);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // Jump back to body if condition false (until = exit when true)
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJumpIfNot;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LCondVar);
            LInstr.Op2 := TTigerSSAOperand.FromBlock(LBodyBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Jump to end if condition true
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            // Continue in end block
            ASSAFunc.SetCurrentBlock(LEndBlock);
          end;
          
        TTigerIR.TIRStmtKind.skCaseBegin:
          begin
            // Evaluate selector
            LCondVar := ConvertExpression(AIR, LStmt.Expr, ASSAFunc);
            
            // Create end block
            LEndBlock := ASSAFunc.CreateBlock('case_end');
            
            // Push: end block, selector expression index, marker (-1 = first case)
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(LStmt.Expr);
            LBlockStack.Push(-1);
          end;
          
        TTigerIR.TIRStmtKind.skCaseOf:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop state
            LPatchIdx := LBlockStack.Pop();
            LHeaderBlock := LBlockStack.Pop();
            LEndBlock := LBlockStack.Pop();
            
            // End previous body if not first CaseOf
            if LPatchIdx >= 0 then
            begin
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikJump;
              LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              
              ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
              ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
              
              ASSAFunc.SetCurrentBlock(LPatchIdx);
            end;
            
            // Create body and next test blocks
            LBodyBlock := ASSAFunc.CreateBlock('case_body');
            LThenBlock := ASSAFunc.CreateBlock('case_test');
            
            // Re-evaluate selector
            LCondVar := ConvertExpression(AIR, LHeaderBlock, ASSAFunc);
            
            // For each value: compare and jump to body if match
            for LArgIdx := 0 to High(LStmt.CaseValues) do
            begin
              // CaseValues contains raw integers, not expression indices
              // Create comparison: selector == caseValue
              LDestVar := ASSAFunc.NewVersion('_casecmp');
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikCmpEq;
              LInstr.Dest := LDestVar;
              LInstr.Op1 := TTigerSSAOperand.FromVar(LCondVar);
              LInstr.Op2 := TTigerSSAOperand.FromImm(LStmt.CaseValues[LArgIdx]);
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikJumpIf;
              LInstr.Op1 := TTigerSSAOperand.FromVar(LDestVar);
              LInstr.Op2 := TTigerSSAOperand.FromBlock(LBodyBlock);
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              
              ASSAFunc.GetCurrentBlock().AddSuccessor(LBodyBlock);
              ASSAFunc.GetBlock(LBodyBlock).AddPredecessor(ASSAFunc.GetCurrentBlock().GetBlockId());
            end;
            
            // Fall through to next test
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LThenBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetCurrentBlock().AddSuccessor(LThenBlock);
            ASSAFunc.GetBlock(LThenBlock).AddPredecessor(ASSAFunc.GetCurrentBlock().GetBlockId());
            
            // Push state
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(LHeaderBlock);
            LBlockStack.Push(LThenBlock);
            
            ASSAFunc.SetCurrentBlock(LBodyBlock);
          end;
          
        TTigerIR.TIRStmtKind.skCaseElse:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            LPatchIdx := LBlockStack.Pop();
            LBlockStack.Pop();
            LEndBlock := LBlockStack.Pop();
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            LElseBlock := ASSAFunc.CreateBlock('case_else');
            
            ASSAFunc.SetCurrentBlock(LPatchIdx);
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LElseBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LPatchIdx).AddSuccessor(LElseBlock);
            ASSAFunc.GetBlock(LElseBlock).AddPredecessor(LPatchIdx);
            
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(-2);
            LBlockStack.Push(-2);
            
            ASSAFunc.SetCurrentBlock(LElseBlock);
          end;
          
        TTigerIR.TIRStmtKind.skCaseEnd:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            LPatchIdx := LBlockStack.Pop();
            LHeaderBlock := LBlockStack.Pop();
            LEndBlock := LBlockStack.Pop();
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // If no else, previous test falls through to end
            if (LPatchIdx >= 0) and (LHeaderBlock >= 0) then
            begin
              ASSAFunc.SetCurrentBlock(LPatchIdx);
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikJump;
              LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              
              ASSAFunc.GetBlock(LPatchIdx).AddSuccessor(LEndBlock);
              ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LPatchIdx);
            end;
            
            ASSAFunc.SetCurrentBlock(LEndBlock);
          end;
          
        //----------------------------------------------------------------------
        // Exception Handling Statements
        //----------------------------------------------------------------------
        
        TTigerIR.TIRStmtKind.skTryBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Create blocks: try body, except handler (placeholder), finally (placeholder), end
            LTryBlock := ASSAFunc.CreateBlock('try_body');
            LEndBlock := ASSAFunc.CreateBlock('try_end');
            
            // Jump to try body
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LTryBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LTryBlock);
            ASSAFunc.GetBlock(LTryBlock).AddPredecessor(LCurrentBlockId);
            
            // Push: try_block, end_block, except_block (-1), finally_block (-1)
            LBlockStack.Push(LTryBlock);       // Try body start
            LBlockStack.Push(LEndBlock);       // End block
            LBlockStack.Push(-1);              // Except block (filled by skExceptBegin)
            LBlockStack.Push(-1);              // Finally block (filled by skFinallyBegin)
            
            ASSAFunc.SetCurrentBlock(LTryBlock);
          end;
          
        TTigerIR.TIRStmtKind.skExceptBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop stack to get context
            LFinallyBlock := LBlockStack.Pop();  // Finally block (-1 or set)
            LBlockStack.Pop();                    // Except block (-1)
            LEndBlock := LBlockStack.Pop();       // End block
            LTryBlock := LBlockStack.Pop();       // Try body start
            
            // Create the except block
            LExceptBlock := ASSAFunc.CreateBlock('except_handler');
            
            // Normal path: try body jumps OVER except to end (or finally if present)
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);  // Will be patched if finally exists
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // Add fake edge for CFG connectivity (SEH handles actual dispatch at runtime)
            // Without this edge, except_handler is unreachable and breaks dominator computation
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LExceptBlock);
            ASSAFunc.GetBlock(LExceptBlock).AddPredecessor(LCurrentBlockId);
            
            // Push back with except block set
            LBlockStack.Push(LTryBlock);
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(LExceptBlock);
            LBlockStack.Push(LFinallyBlock);
            LBlockStack.Push(LCurrentBlockId);  // Save try-end block for scope tracking
            
            ASSAFunc.SetCurrentBlock(LExceptBlock);
          end;
          
        TTigerIR.TIRStmtKind.skFinallyBegin:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop stack - might have extra item if except was processed
            if LBlockStack.Count >= 5 then
              LTryEndBlock := LBlockStack.Pop()  // Try-end block from except
            else
              LTryEndBlock := -1;
            
            LBlockStack.Pop();                    // Finally block (-1)
            LExceptBlock := LBlockStack.Pop();    // Except block (may be valid)
            LEndBlock := LBlockStack.Pop();       // End block
            LTryBlock := LBlockStack.Pop();       // Try body start
            
            // Create the finally block
            LFinallyBlock := ASSAFunc.CreateBlock('finally_handler');
            
            // Normal path jumps through finally
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LFinallyBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LFinallyBlock);
            ASSAFunc.GetBlock(LFinallyBlock).AddPredecessor(LCurrentBlockId);
            
            // If except was present, patch its jump to also go through finally
            if (LExceptBlock >= 0) and (LTryEndBlock >= 0) then
            begin
              // Patch the jump in try-end block to go to finally instead of end
              LPatchIdx := ASSAFunc.GetBlock(LTryEndBlock).GetInstructionCount() - 1;
              LInstr := ASSAFunc.GetBlock(LTryEndBlock).GetInstruction(LPatchIdx);
              if LInstr.Kind = sikJump then
              begin
                LInstr.Op1 := TTigerSSAOperand.FromBlock(LFinallyBlock);
                ASSAFunc.GetBlock(LTryEndBlock).SetInstruction(LPatchIdx, LInstr);
                // Update edges
                ASSAFunc.GetBlock(LTryEndBlock).AddSuccessor(LFinallyBlock);
                ASSAFunc.GetBlock(LFinallyBlock).AddPredecessor(LTryEndBlock);
              end;
            end;
            
            // Push back with finally block set
            LBlockStack.Push(LTryBlock);
            LBlockStack.Push(LEndBlock);
            LBlockStack.Push(LExceptBlock);
            LBlockStack.Push(LFinallyBlock);
            if LTryEndBlock >= 0 then
              LBlockStack.Push(LTryEndBlock)
            else
              LBlockStack.Push(LCurrentBlockId);  // Save current block for scope
            
            ASSAFunc.SetCurrentBlock(LFinallyBlock);
          end;
          
        TTigerIR.TIRStmtKind.skTryEnd:
          begin
            LCurrentBlockId := ASSAFunc.GetCurrentBlock().GetBlockId();
            
            // Pop everything
            if LBlockStack.Count >= 5 then
              LTryEndBlock := LBlockStack.Pop()
            else
              LTryEndBlock := -1;
            
            LFinallyBlock := LBlockStack.Pop();
            LExceptBlock := LBlockStack.Pop();
            LEndBlock := LBlockStack.Pop();
            LTryBlock := LBlockStack.Pop();
            
            // Jump to end block
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikJump;
            LInstr.Op1 := TTigerSSAOperand.FromBlock(LEndBlock);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            
            ASSAFunc.GetBlock(LCurrentBlockId).AddSuccessor(LEndBlock);
            ASSAFunc.GetBlock(LEndBlock).AddPredecessor(LCurrentBlockId);
            
            // Record exception scope for backend
            LExcScope.TryBlockIndex := LTryBlock;
            LExcScope.TryEndBlockIndex := LTryEndBlock;
            LExcScope.ExceptBlockIndex := LExceptBlock;
            LExcScope.FinallyBlockIndex := LFinallyBlock;
            LExcScope.EndBlockIndex := LEndBlock;
            ASSAFunc.AddExceptionScope(LExcScope);
            
            ASSAFunc.SetCurrentBlock(LEndBlock);
          end;
          
        TTigerIR.TIRStmtKind.skRaise:
          begin
            // Convert: raiseexception(msg) -> call Pax_Raise(msg)
            LExprVar := ConvertExpression(AIR, LStmt.RaiseMsg, ASSAFunc);
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikCall;
            SetLength(LCallArgs, 1);
            LCallArgs[0] := TTigerSSAOperand.FromVar(LExprVar);
            LInstr.CallArgs := LCallArgs;
            LInstr.CallTarget := TTigerSSAOperand.FromFunc(FindLocalFuncByName(AIR, 'Tiger_Raise'));
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
          
        TTigerIR.TIRStmtKind.skRaiseCode:
          begin
            // Convert: raiseexceptioncode(code, msg) -> call Pax_RaiseCode(code, msg)
            LCondVar := ConvertExpression(AIR, LStmt.RaiseCode, ASSAFunc);
            LExprVar := ConvertExpression(AIR, LStmt.RaiseMsg, ASSAFunc);
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikCall;
            SetLength(LCallArgs, 2);
            LCallArgs[0] := TTigerSSAOperand.FromVar(LCondVar);
            LCallArgs[1] := TTigerSSAOperand.FromVar(LExprVar);
            LInstr.CallArgs := LCallArgs;
            LInstr.CallTarget := TTigerSSAOperand.FromFunc(FindLocalFuncByName(AIR, 'Tiger_RaiseCode'));
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
      end;
    end;
  finally
    LForDownToStack.Free();
    LForVarStack.Free();
    LBlockStack.Free();
  end;
end;

function TTigerSSABuilder.FindImportByName(const AIR: TTigerIR; const AName: string): Integer;
var
  LI: Integer;
  LImport: TTigerIR.TIRImport;
begin
  for LI := 0 to AIR.GetImportCount() - 1 do
  begin
    LImport := AIR.GetImport(LI);
    if SameText(LImport.FuncName, AName) then
      Exit(LI);
  end;
  Result := -1;
end;

function TTigerSSABuilder.FindLocalFuncByName(const AIR: TTigerIR; const AName: string): Integer;
var
  LI: Integer;
  LFunc: TTigerIR.TIRFunc;
begin
  for LI := 0 to AIR.GetFunctionCount() - 1 do
  begin
    LFunc := AIR.GetFunction(LI);
    if SameText(LFunc.FuncName, AName) then
      Exit(LI);
  end;
  Result := -1;
end;

function TTigerSSABuilder.FindLocalFuncBySignature(
  const AIR: TTigerIR;
  const AName: string;
  const AArgTypes: TArray<TTigerTypeRef>
): Integer;
var
  LI: Integer;
  LJ: Integer;
  LFunc: TTigerIR.TIRFunc;
  LFuncParams: TArray<TTigerTypeRef>;
  LParamCount: Integer;
  LMatch: Boolean;
begin
  for LI := 0 to AIR.GetFunctionCount() - 1 do
  begin
    LFunc := AIR.GetFunction(LI);
    if not SameText(LFunc.FuncName, AName) then
      Continue;

    // Extract param types from function
    LParamCount := 0;
    for LJ := 0 to LFunc.Vars.Count - 1 do
    begin
      if LFunc.Vars[LJ].IsParam then
        Inc(LParamCount);
    end;

    // Check param count match
    if LParamCount <> Length(AArgTypes) then
      Continue;

    // Build param type array
    SetLength(LFuncParams, LParamCount);
    LParamCount := 0;
    for LJ := 0 to LFunc.Vars.Count - 1 do
    begin
      if LFunc.Vars[LJ].IsParam then
      begin
        LFuncParams[LParamCount] := LFunc.Vars[LJ].VarTypeRef;
        Inc(LParamCount);
      end;
    end;

    // Compare types
    LMatch := True;
    for LJ := 0 to High(AArgTypes) do
    begin
      if LFuncParams[LJ] <> AArgTypes[LJ] then
      begin
        LMatch := False;
        Break;
      end;
    end;

    if LMatch then
      Exit(LI);
  end;
  Result := -1;
end;

function TTigerSSABuilder.ConvertExpression(
  const AIR: TTigerIR;
  const AExprIndex: Integer;
  const ASSAFunc: TTigerSSAFunc
): TTigerSSAVar;
var
  LExpr: TTigerIR.TIRExprNode;
  LObjectExpr: TTigerIR.TIRExprNode;
  LInnerExpr: TTigerIR.TIRExprNode;
  LArgExpr: TTigerIR.TIRExprNode;
  LInstr: TTigerSSAInstr;
  LLeftVar: TTigerSSAVar;
  LRightVar: TTigerSSAVar;
  LResultVar: TTigerSSAVar;
  LArgVar: TTigerSSAVar;
  LArgIdx: Integer;
  LFuncIndex: Integer;
  LCallArgs: TArray<TTigerSSAOperand>;
  LArgTypes: TArray<TTigerTypeRef>;
  // Variadic call handling
  LTargetFunc: TTigerIR.TIRFunc;
  LFixedParamCount: Integer;
  LVarArgCount: Integer;
  LJ: Integer;
  LImportIdx: Integer;
  // For by-reference parameter detection in field access
  LIsParam: Boolean;
  LLocalSize: Integer;
  LLocalIdx: Integer;
  LLocalInfo: TTigerSSALocalInfo;
  LTempVar: TTigerSSAVar;
begin
  if AExprIndex < 0 then
  begin
    Result := TTigerSSAVar.None();
    Exit;
  end;
  
  // Check cache first - if already converted, return cached result
  if FExpressionCache.TryGetValue(AExprIndex, Result) then
    Exit;
  
  LExpr := AIR.GetExpression(AExprIndex);
  
  case LExpr.Kind of
    TTigerIR.TIRExprKind.ekConstInt:
      begin
        // Create temp variable and assign constant
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikAssign;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromImm(LExpr.ConstInt);
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekConstFloat:
      begin
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikAssign;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromImm(LExpr.ConstFloat);
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekConstString:
      begin
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikAssign;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromData(LExpr.StringIndex);
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekVariable:
      begin
        // Check if it's a global variable first
        LArgIdx := AIR.FindGlobal(LExpr.VarName);
        if LArgIdx >= 0 then
        begin
          // Global variable - emit load from global address
          LResultVar := ASSAFunc.NewVersion('_t');
          
          LInstr := Default(TTigerSSAInstr);
          LInstr.Kind := sikLoad;
          LInstr.Dest := LResultVar;
          LInstr.Op1 := TTigerSSAOperand.FromGlobal(LArgIdx);
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          
          Result := LResultVar;
        end
        else
        begin
          // Local variable - return current version
          Result := ASSAFunc.CurrentVersion(LExpr.VarName);
          
          // If no version exists yet (first use), create version 0
          if not Result.IsValid() then
          begin
            Result := ASSAFunc.NewVersion(LExpr.VarName);
          end;
        end;
      end;
      
    TTigerIR.TIRExprKind.ekBinary:
      begin
        // Convert left and right operands
        LLeftVar := ConvertExpression(AIR, LExpr.Left, ASSAFunc);
        LRightVar := ConvertExpression(AIR, LExpr.Right, ASSAFunc);
        
        // Create result variable
        LResultVar := ASSAFunc.NewVersion('_t');
        
        // Emit binary operation
        LInstr := Default(TTigerSSAInstr);
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
        LInstr.Op2 := TTigerSSAOperand.FromVar(LRightVar);
        
        case LExpr.Op of
          TTigerIR.TIROpKind.opAdd:    LInstr.Kind := sikAdd;
          TTigerIR.TIROpKind.opSub:    LInstr.Kind := sikSub;
          TTigerIR.TIROpKind.opMul:    LInstr.Kind := sikMul;
          TTigerIR.TIROpKind.opDiv:    LInstr.Kind := sikDiv;
          TTigerIR.TIROpKind.opMod:    LInstr.Kind := sikMod;
          TTigerIR.TIROpKind.opFAdd:   LInstr.Kind := sikFAdd;
          TTigerIR.TIROpKind.opFSub:   LInstr.Kind := sikFSub;
          TTigerIR.TIROpKind.opFMul:   LInstr.Kind := sikFMul;
          TTigerIR.TIROpKind.opFDiv:   LInstr.Kind := sikFDiv;
          TTigerIR.TIROpKind.opBitAnd: LInstr.Kind := sikBitAnd;
          TTigerIR.TIROpKind.opBitOr:  LInstr.Kind := sikBitOr;
          TTigerIR.TIROpKind.opBitXor: LInstr.Kind := sikBitXor;
          TTigerIR.TIROpKind.opShl:    LInstr.Kind := sikShl;
          TTigerIR.TIROpKind.opShr:    LInstr.Kind := sikShr;
          TTigerIR.TIROpKind.opCmpEq:  LInstr.Kind := sikCmpEq;
          TTigerIR.TIROpKind.opCmpNe:  LInstr.Kind := sikCmpNe;
          TTigerIR.TIROpKind.opCmpLt:  LInstr.Kind := sikCmpLt;
          TTigerIR.TIROpKind.opCmpLe:  LInstr.Kind := sikCmpLe;
          TTigerIR.TIROpKind.opCmpGt:  LInstr.Kind := sikCmpGt;
          TTigerIR.TIROpKind.opCmpGe:  LInstr.Kind := sikCmpGe;
          TTigerIR.TIROpKind.opAnd:    LInstr.Kind := sikBitAnd;  // Logical and
          TTigerIR.TIROpKind.opOr:     LInstr.Kind := sikBitOr;   // Logical or
          // Set operations
          TTigerIR.TIROpKind.opSetUnion:    LInstr.Kind := sikSetUnion;
          TTigerIR.TIROpKind.opSetDiff:     LInstr.Kind := sikSetDiff;
          TTigerIR.TIROpKind.opSetInter:    LInstr.Kind := sikSetInter;
          TTigerIR.TIROpKind.opSetIn:       LInstr.Kind := sikSetIn;
          TTigerIR.TIROpKind.opSetEq:       LInstr.Kind := sikSetEq;
          TTigerIR.TIROpKind.opSetNe:       LInstr.Kind := sikSetNe;
          TTigerIR.TIROpKind.opSetSubset:   LInstr.Kind := sikSetSubset;
          TTigerIR.TIROpKind.opSetSuperset: LInstr.Kind := sikSetSuperset;
        else
          LInstr.Kind := sikNop;
        end;
        
        // Extract LowBound for SetIn
        if LExpr.Op = TTigerIR.TIROpKind.opSetIn then
        begin
          // Prefer explicit LowBound from IR expression (set by TigerLang codegen)
          if LExpr.SetLowBound <> 0 then
            LInstr.SetLowBound := LExpr.SetLowBound
          else
          begin
            // Fallback: infer from right operand's set type
            var LRightExpr := AIR.GetExpression(LExpr.Right);
            if (not LRightExpr.ResultType.IsPrimitive) and (LRightExpr.ResultType.TypeIndex >= 0) then
            begin
              var LTypeEntry := AIR.GetTypeEntry(LRightExpr.ResultType.TypeIndex);
              if LTypeEntry.Kind = TTigerIR.TIRTypeKind.tkSet then
                LInstr.SetLowBound := LTypeEntry.SetType.LowBound;
            end;
          end;
        end;
        
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekUnary:
      begin
        // Special handling for address-of operations
        if LExpr.Op = TTigerIR.TIROpKind.opAddrOf then
        begin
          LResultVar := ASSAFunc.NewVersion('_t');
          
          if LExpr.Left < 0 then
          begin
            // AddrOf(varname) - take address of named variable
            // Check if it's a global first
            LArgIdx := AIR.FindGlobal(LExpr.VarName);
            if LArgIdx >= 0 then
            begin
              // Global variable - sokGlobal operand IS the address (backend emits LEA)
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikAssign;
              LInstr.Dest := LResultVar;
              LInstr.Op1 := TTigerSSAOperand.FromGlobal(LArgIdx);
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            end
            else
            begin
              // Local variable - use sikAddressOf
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikAddressOf;
              LInstr.Dest := LResultVar;
              LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LExpr.VarName));
              LInstr.Op1.Var_.BaseName := LExpr.VarName;
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            end;
          end
          else
          begin
            // AddrOfExpr(expr) - take address of expression result
            LObjectExpr := AIR.GetExpression(LExpr.Left);
            
            if LObjectExpr.Kind = TTigerIR.TIRExprKind.ekFieldAccess then
            begin
              // Get address of field: compute base address + field offset
              LInnerExpr := AIR.GetExpression(LObjectExpr.ObjectExpr);
              
              if (LInnerExpr.Kind = TTigerIR.TIRExprKind.ekVariable) and 
                 (not LInnerExpr.ResultType.IsPrimitive) then
              begin
                LLeftVar := ASSAFunc.NewVersion('_t');
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikAddressOf;
                LInstr.Dest := LLeftVar;
                LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LInnerExpr.VarName));
                LInstr.Op1.Var_.BaseName := LInnerExpr.VarName;
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              end
              else if (LInnerExpr.Kind = TTigerIR.TIRExprKind.ekUnary) and
                      (LInnerExpr.Op = TTigerIR.TIROpKind.opDeref) then
              begin
                // For Deref(ptr, TypeName), we want just the pointer value as base.
                LLeftVar := ConvertExpression(AIR, LInnerExpr.Left, ASSAFunc);
              end
              else
              begin
                LLeftVar := ConvertExpression(AIR, LObjectExpr.ObjectExpr, ASSAFunc);
              end;
              
              // Compute field address
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikFieldAddr;
              LInstr.Dest := LResultVar;
              LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
              LInstr.Op2 := TTigerSSAOperand.FromImm(LObjectExpr.FieldOffset);
              LInstr.Op2.FieldName := LObjectExpr.FieldName;
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            end
            else if LObjectExpr.Kind = TTigerIR.TIRExprKind.ekArrayIndex then
            begin
              // Get address of array element
              LInnerExpr := AIR.GetExpression(LObjectExpr.ArrayExpr);
              
              if (LInnerExpr.Kind = TTigerIR.TIRExprKind.ekVariable) and 
                 (not LInnerExpr.ResultType.IsPrimitive) then
              begin
                LLeftVar := ASSAFunc.NewVersion('_t');
                LInstr := Default(TTigerSSAInstr);
                LInstr.Kind := sikAddressOf;
                LInstr.Dest := LLeftVar;
                LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LInnerExpr.VarName));
                LInstr.Op1.Var_.BaseName := LInnerExpr.VarName;
                ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
              end
              else
              begin
                LLeftVar := ConvertExpression(AIR, LObjectExpr.ArrayExpr, ASSAFunc);
              end;
              
              LRightVar := ConvertExpression(AIR, LObjectExpr.IndexExpr, ASSAFunc);
              
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikIndexAddr;
              LInstr.Dest := LResultVar;
              LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
              LInstr.Op2 := TTigerSSAOperand.FromVar(LRightVar);
              LInstr.Op2.ElementSize := LObjectExpr.ElementSize;
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            end
            else if LObjectExpr.Kind = TTigerIR.TIRExprKind.ekVariable then
            begin
              // Take address of a variable through expression
              LInstr := Default(TTigerSSAInstr);
              LInstr.Kind := sikAddressOf;
              LInstr.Dest := LResultVar;
              LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LObjectExpr.VarName));
              LInstr.Op1.Var_.BaseName := LObjectExpr.VarName;
              ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
            end;
          end;
          
          Result := LResultVar;
        end
        else
        begin
          // Other unary operations (neg, not, deref)
          LLeftVar := ConvertExpression(AIR, LExpr.Left, ASSAFunc);
          
          LResultVar := ASSAFunc.NewVersion('_t');
          
          LInstr := Default(TTigerSSAInstr);
          LInstr.Dest := LResultVar;
          LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
          
          case LExpr.Op of
            TTigerIR.TIROpKind.opNeg:    LInstr.Kind := sikNeg;
            TTigerIR.TIROpKind.opFNeg:   LInstr.Kind := sikFNeg;
            TTigerIR.TIROpKind.opBitNot: LInstr.Kind := sikBitNot;
            TTigerIR.TIROpKind.opNot:    LInstr.Kind := sikBitNot;
            TTigerIR.TIROpKind.opDeref:  LInstr.Kind := sikLoad;
          else
            LInstr.Kind := sikNop;
          end;
          
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          Result := LResultVar;
        end;
      end;
      
    TTigerIR.TIRExprKind.ekCall:
      begin
        // Build argument types array for overload resolution
        SetLength(LArgTypes, Length(LExpr.CallArgs));
        for LArgIdx := 0 to High(LExpr.CallArgs) do
        begin
          LArgExpr := AIR.GetExpression(LExpr.CallArgs[LArgIdx]);
          LArgTypes[LArgIdx] := LArgExpr.ResultType;
        end;

        // Convert call arguments
        SetLength(LCallArgs, Length(LExpr.CallArgs));
        for LArgIdx := 0 to High(LExpr.CallArgs) do
        begin
          LArgVar := ConvertExpression(AIR, LExpr.CallArgs[LArgIdx], ASSAFunc);
          LCallArgs[LArgIdx] := TTigerSSAOperand.FromVar(LArgVar);
        end;
        
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikCallAssign;
        LInstr.Dest := LResultVar;
        LInstr.CallArgs := LCallArgs;
        
        // Try signature-based lookup first for overload resolution
        LFuncIndex := FindLocalFuncBySignature(AIR, LExpr.CallTarget, LArgTypes);
        if LFuncIndex < 0 then
          // Fall back to name-only lookup (for non-overloaded functions)
          LFuncIndex := FindLocalFuncByName(AIR, LExpr.CallTarget);
        
        if LFuncIndex >= 0 then
        begin
          LInstr.CallTarget := TTigerSSAOperand.FromFunc(LFuncIndex);
          
          // For internal variadic functions, prepend vararg count
          LTargetFunc := AIR.GetFunction(LFuncIndex);
          if LTargetFunc.IsVariadic then
          begin
            // Count fixed params in target function
            LFixedParamCount := 0;
            for LJ := 0 to LTargetFunc.Vars.Count - 1 do
              if LTargetFunc.Vars[LJ].IsParam then
                Inc(LFixedParamCount);
            
            // Calculate vararg count
            LVarArgCount := Length(LExpr.CallArgs) - LFixedParamCount;
            if LVarArgCount < 0 then
              LVarArgCount := 0;
            
            // Prepend count to args array
            SetLength(LCallArgs, Length(LCallArgs) + 1);
            for LArgIdx := High(LCallArgs) downto 1 do
              LCallArgs[LArgIdx] := LCallArgs[LArgIdx - 1];
            LCallArgs[0] := TTigerSSAOperand.FromImm(LVarArgCount);
            LInstr.CallArgs := LCallArgs;
          end;
        end
        else
        begin
          LImportIdx := FindImportByName(AIR, LExpr.CallTarget);
          if LImportIdx < 0 then
          begin
            if Assigned(FErrors) then
              FErrors.Add(esError, ERR_SSA_UNKNOWN_FUNCTION, RSSSAUnknownFunction, [LExpr.CallTarget]);
            Result := TTigerSSAVar.None();
            Exit;
          end;
          LInstr.CallTarget := TTigerSSAOperand.FromImport(LImportIdx);
        end;
        
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekFuncAddr:
      begin
        // Get address of a function
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikFuncAddr;
        LInstr.Dest := LResultVar;
        // Store function name in Op1 - will be resolved to function index at emit time
        LInstr.Op1 := TTigerSSAOperand.FromFunc(FindLocalFuncByName(AIR, LExpr.FuncAddrName));
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekCallIndirect:
      begin
        // Indirect call through function pointer
        // First, evaluate the function pointer expression
        LLeftVar := ConvertExpression(AIR, LExpr.IndirectTarget, ASSAFunc);
        
        // Convert call arguments
        SetLength(LCallArgs, Length(LExpr.CallArgs));
        for LArgIdx := 0 to High(LExpr.CallArgs) do
        begin
          LArgVar := ConvertExpression(AIR, LExpr.CallArgs[LArgIdx], ASSAFunc);
          LCallArgs[LArgIdx] := TTigerSSAOperand.FromVar(LArgVar);
        end;
        
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikIndirectCallAssign;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);  // Function pointer
        LInstr.CallArgs := LCallArgs;
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekFieldAccess:
      begin
        // Get the object expression node
        LObjectExpr := AIR.GetExpression(LExpr.ObjectExpr);
        
        // For variable expressions with composite types, we need the address, not the value
        if (LObjectExpr.Kind = TTigerIR.TIRExprKind.ekVariable) and 
           (not LObjectExpr.ResultType.IsPrimitive) then
        begin
          // Check if this is a parameter passed by reference (composite > 8 bytes)
          // Win64 ABI: Large structs are passed by pointer, so param value IS the address
          LIsParam := False;
          LLocalSize := 0;
          for LLocalIdx := 0 to ASSAFunc.GetLocalCount() - 1 do
          begin
            LLocalInfo := ASSAFunc.GetLocal(LLocalIdx);
            if LLocalInfo.LocalName = LObjectExpr.VarName then
            begin
              LIsParam := LLocalInfo.IsParam;
              LLocalSize := LLocalInfo.LocalSize;
              Break;
            end;
          end;
          
          // Emit AddressOf instruction to get local's/param's stack address
          LLeftVar := ASSAFunc.NewVersion('_t');
          LInstr := Default(TTigerSSAInstr);
          LInstr.Kind := sikAddressOf;
          LInstr.Dest := LLeftVar;
          LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LObjectExpr.VarName));
          LInstr.Op1.Var_.BaseName := LObjectExpr.VarName;  // Store base name for local lookup
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          
          if LIsParam and (LLocalSize > 8) then
          begin
            // By-reference parameter: param slot contains pointer to struct
            // Load the pointer value from the param slot
            LTempVar := LLeftVar;
            LLeftVar := ASSAFunc.NewVersion('_t');
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikLoad;
            LInstr.Dest := LLeftVar;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LTempVar);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
        end
        else if (LObjectExpr.Kind = TTigerIR.TIRExprKind.ekUnary) and
                (LObjectExpr.Op = TTigerIR.TIROpKind.opDeref) then
        begin
          // For Deref(ptr, TypeName), we want just the pointer value as base.
          // Don't generate a load - convert only the inner pointer expression.
          LLeftVar := ConvertExpression(AIR, LObjectExpr.Left, ASSAFunc);
        end
        else
        begin
          // For other expressions (pointers, etc.), convert normally
          LLeftVar := ConvertExpression(AIR, LExpr.ObjectExpr, ASSAFunc);
        end;
        
        // Create temp for field address
        LResultVar := ASSAFunc.NewVersion('_t');
        
        // Emit field address calculation using pre-computed offset
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikFieldAddr;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
        LInstr.Op2 := TTigerSSAOperand.FromImm(LExpr.FieldOffset);  // Use pre-computed offset
        LInstr.Op2.FieldName := LExpr.FieldName;
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        // For composite (embedded struct) fields, the fieldaddr result IS the
        // address we need for chaining further field accesses - do NOT load.
        // Only load for primitive fields to get the actual scalar value.
        if LExpr.ResultType.IsPrimitive then
        begin
          LLeftVar := LResultVar;
          LResultVar := ASSAFunc.NewVersion('_t');
          
          LInstr := Default(TTigerSSAInstr);
          LInstr.Kind := sikLoad;
          LInstr.Dest := LResultVar;
          LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
          LInstr.MemSize := LExpr.FieldSize;
          LInstr.MemIsFloat := LExpr.ResultType.IsPrimitive and
            (LExpr.ResultType.Primitive in [vtFloat32, vtFloat64]);
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        end;
        
        // If bit field, extract the bits
        if LExpr.BitWidth > 0 then
        begin
          // Shift right by BitOffset to move bits to position 0
          if LExpr.BitOffset > 0 then
          begin
            LLeftVar := LResultVar;
            LResultVar := ASSAFunc.NewVersion('_t');
            
            LInstr := Default(TTigerSSAInstr);
            LInstr.Kind := sikShr;
            LInstr.Dest := LResultVar;
            LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
            LInstr.Op2 := TTigerSSAOperand.FromImm(LExpr.BitOffset);
            ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
          end;
          
          // Mask to extract only BitWidth bits
          LLeftVar := LResultVar;
          LResultVar := ASSAFunc.NewVersion('_t');
          
          LInstr := Default(TTigerSSAInstr);
          LInstr.Kind := sikBitAnd;
          LInstr.Dest := LResultVar;
          LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
          LInstr.Op2 := TTigerSSAOperand.FromImm((Int64(1) shl LExpr.BitWidth) - 1);  // Mask with BitWidth bits
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        end;
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekArrayIndex:
      begin
        // Get the array expression node
        LObjectExpr := AIR.GetExpression(LExpr.ArrayExpr);
        
        // For variable expressions with composite types (arrays), we need the address
        if (LObjectExpr.Kind = TTigerIR.TIRExprKind.ekVariable) and 
           (not LObjectExpr.ResultType.IsPrimitive) then
        begin
          // Emit AddressOf instruction to get local's stack address
          LLeftVar := ASSAFunc.NewVersion('_t');
          LInstr := Default(TTigerSSAInstr);
          LInstr.Kind := sikAddressOf;
          LInstr.Dest := LLeftVar;
          LInstr.Op1 := TTigerSSAOperand.FromVar(ASSAFunc.CurrentVersion(LObjectExpr.VarName));
          LInstr.Op1.Var_.BaseName := LObjectExpr.VarName;
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        end
        else
        begin
          // For other expressions (pointers, etc.), convert normally
          LLeftVar := ConvertExpression(AIR, LExpr.ArrayExpr, ASSAFunc);
        end;
        
        // Convert index expression
        LRightVar := ConvertExpression(AIR, LExpr.IndexExpr, ASSAFunc);
        
        // Create temp for element address
        LResultVar := ASSAFunc.NewVersion('_t');
        
        // Emit index address calculation using pre-computed element size
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikIndexAddr;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
        LInstr.Op2 := TTigerSSAOperand.FromVar(LRightVar);
        LInstr.Op2.ElementSize := LExpr.ElementSize;  // Use pre-computed element size
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        // For composite (embedded array/record) elements, the indexaddr result IS
        // the address we need for chaining further indexing — do NOT load.
        // Only load for primitive elements to get the actual scalar value.
        if LExpr.ResultType.IsPrimitive then
        begin
          // Load scalar value from element address
          LLeftVar := LResultVar;
          LResultVar := ASSAFunc.NewVersion('_t');
          
          LInstr := Default(TTigerSSAInstr);
          LInstr.Kind := sikLoad;
          LInstr.Dest := LResultVar;
          LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);
          LInstr.MemSize := LExpr.ElementSize;
          LInstr.MemIsFloat := LExpr.ResultType.Primitive in [vtFloat32, vtFloat64];
          ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        end;
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekGetExceptionCode:
      begin
        // Call Pax_GetExceptionCode() and store result
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikCallAssign;
        LInstr.Dest := LResultVar;
        LInstr.CallTarget := TTigerSSAOperand.FromFunc(FindLocalFuncByName(AIR, 'Tiger_GetExceptionCode'));
        SetLength(LInstr.CallArgs, 0);
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekGetExceptionMsg:
      begin
        // Call Pax_GetExceptionMessage() and store result
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikCallAssign;
        LInstr.Dest := LResultVar;
        LInstr.CallTarget := TTigerSSAOperand.FromFunc(FindLocalFuncByName(AIR, 'Tiger_GetExceptionMessage'));
        SetLength(LInstr.CallArgs, 0);
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekSetLiteral:
      begin
        // Create temp variable for set value
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikSetLiteral;
        LInstr.Dest := LResultVar;
        LInstr.SetTypeIndex := LExpr.SetTypeIndex;
        LInstr.SetElements := Copy(LExpr.SetElements);
        // Store type info for emission phase
        LInstr.SetLowBound := AIR.GetTypeEntry(LExpr.SetTypeIndex).SetType.LowBound;
        LInstr.SetStorageSize := AIR.GetTypeEntry(LExpr.SetTypeIndex).SetType.StorageSize;
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekVaCount:
      begin
        // Get vararg count - backend reads from stack
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikVaCount;
        LInstr.Dest := LResultVar;
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;
      
    TTigerIR.TIRExprKind.ekVaArgAt:
      begin
        // Convert index expression
        LLeftVar := ConvertExpression(AIR, LExpr.VaArgIndex, ASSAFunc);
        
        LResultVar := ASSAFunc.NewVersion('_t');
        
        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikVaArgAt;
        LInstr.Dest := LResultVar;
        LInstr.Op1 := TTigerSSAOperand.FromVar(LLeftVar);  // Index
        LInstr.VaArgType := LExpr.VaArgType;  // Type to read as
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);
        
        Result := LResultVar;
      end;

    TTigerIR.TIRExprKind.ekSyscall:
      begin
        // Convert syscall arguments
        SetLength(LCallArgs, Length(LExpr.CallArgs));
        for LArgIdx := 0 to High(LExpr.CallArgs) do
        begin
          LArgVar := ConvertExpression(AIR, LExpr.CallArgs[LArgIdx], ASSAFunc);
          LCallArgs[LArgIdx] := TTigerSSAOperand.FromVar(LArgVar);
        end;

        LResultVar := ASSAFunc.NewVersion('_t');

        LInstr := Default(TTigerSSAInstr);
        LInstr.Kind := sikSyscallAssign;
        LInstr.Dest := LResultVar;
        LInstr.SyscallNr := LExpr.SyscallNr;
        LInstr.CallArgs := LCallArgs;
        ASSAFunc.GetCurrentBlock().AddInstruction(LInstr);

        Result := LResultVar;
      end;
  else
    Result := TTigerSSAVar.None();
  end;
  
  // Cache the result for reuse
  if Result.IsValid() then
    FExpressionCache.AddOrSetValue(AExprIndex, Result);
end;

procedure TTigerSSABuilder.ComputeDominators(const AFunc: TTigerSSAFunc);
var
  LNumBlocks: Integer;
  LDom: TArray<TList<Integer>>;  // Dom[i] = set of blocks that dominate block i
  LChanged: Boolean;
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  LBlock: TTigerSSABlock;
  LNewDom: TList<Integer>;
  LIntersect: TList<Integer>;
  LFirstPred: Boolean;
begin
  LNumBlocks := AFunc.GetBlockCount();
  if LNumBlocks = 0 then
    Exit;
  
  // Initialize Dom sets
  SetLength(LDom, LNumBlocks);
  for LI := 0 to LNumBlocks - 1 do
  begin
    LDom[LI] := TList<Integer>.Create();
    if LI = 0 then
    begin
      // Entry block dominates only itself
      LDom[LI].Add(0);
    end
    else
    begin
      // All other blocks: initialize with all blocks (will be narrowed)
      for LJ := 0 to LNumBlocks - 1 do
        LDom[LI].Add(LJ);
    end;
  end;
  
  // Iterate until fixed point
  LChanged := True;
  while LChanged do
  begin
    LChanged := False;
    
    for LI := 1 to LNumBlocks - 1 do  // Skip entry block
    begin
      LBlock := AFunc.GetBlock(LI);
      if LBlock.GetPredecessorCount() = 0 then
        Continue;
      
      // Compute intersection of Dom sets of all predecessors
      LNewDom := TList<Integer>.Create();
      try
        LFirstPred := True;
        
        for LJ := 0 to LBlock.GetPredecessorCount() - 1 do
        begin
          if LFirstPred then
          begin
            // Start with first predecessor's dom set
            for LK := 0 to LDom[LBlock.GetPredecessor(LJ)].Count - 1 do
              LNewDom.Add(LDom[LBlock.GetPredecessor(LJ)][LK]);
            LFirstPred := False;
          end
          else
          begin
            // Intersect with this predecessor's dom set
            LIntersect := TList<Integer>.Create();
            try
              for LK := 0 to LNewDom.Count - 1 do
              begin
                if LDom[LBlock.GetPredecessor(LJ)].Contains(LNewDom[LK]) then
                  LIntersect.Add(LNewDom[LK]);
              end;
              LNewDom.Clear();
              for LK := 0 to LIntersect.Count - 1 do
                LNewDom.Add(LIntersect[LK]);
            finally
              LIntersect.Free();
            end;
          end;
        end;
        
        // Add self
        if not LNewDom.Contains(LI) then
          LNewDom.Add(LI);
        
        // Check if changed
        if LNewDom.Count <> LDom[LI].Count then
          LChanged := True
        else
        begin
          for LK := 0 to LNewDom.Count - 1 do
          begin
            if not LDom[LI].Contains(LNewDom[LK]) then
            begin
              LChanged := True;
              Break;
            end;
          end;
        end;
        
        // Update Dom set
        if LChanged then
        begin
          LDom[LI].Clear();
          for LK := 0 to LNewDom.Count - 1 do
            LDom[LI].Add(LNewDom[LK]);
        end;
        
      finally
        LNewDom.Free();
      end;
    end;
  end;
  
  // Compute immediate dominators from Dom sets
  for LI := 0 to LNumBlocks - 1 do
  begin
    LBlock := AFunc.GetBlock(LI);
    
    if LI = 0 then
    begin
      LBlock.SetImmediateDominator(-1);  // Entry has no dominator
    end
    else
    begin
      // Immediate dominator is the dominator closest to this block
      // (dominates this block but doesn't dominate any other dominator)
      for LJ := 0 to LDom[LI].Count - 1 do
      begin
        if LDom[LI][LJ] = LI then
          Continue;  // Skip self
        
        // Check if LDom[LI][LJ] is the immediate dominator
        // It must not dominate any other dominator (except itself)
        LFirstPred := True;  // Reuse as "is immediate" flag
        for LK := 0 to LDom[LI].Count - 1 do
        begin
          if (LDom[LI][LK] <> LI) and (LDom[LI][LK] <> LDom[LI][LJ]) then
          begin
            // Check if LDom[LI][LJ] dominates LDom[LI][LK]
            if LDom[LDom[LI][LK]].Contains(LDom[LI][LJ]) then
            begin
              LFirstPred := False;  // Not immediate
              Break;
            end;
          end;
        end;
        
        if LFirstPred then
        begin
          LBlock.SetImmediateDominator(LDom[LI][LJ]);
          // Add this block to the dominator's children
          AFunc.GetBlock(LDom[LI][LJ]).AddDominatedBlock(LI);
          Break;
        end;
      end;
    end;
  end;
  
  // Clean up
  for LI := 0 to LNumBlocks - 1 do
    LDom[LI].Free();
end;

procedure TTigerSSABuilder.ComputeDominanceFrontiers(const AFunc: TTigerSSAFunc);
var
  LI: Integer;
  LJ: Integer;
  LBlock: TTigerSSABlock;
  LPredIdx: Integer;
  LRunner: Integer;
  LIdom: Integer;
begin
  // Clear existing frontiers
  for LI := 0 to AFunc.GetBlockCount() - 1 do
    AFunc.GetBlock(LI).ClearDominanceFrontier();
  
  // For each block B with multiple predecessors (join point)
  for LI := 0 to AFunc.GetBlockCount() - 1 do
  begin
    LBlock := AFunc.GetBlock(LI);
    
    if LBlock.GetPredecessorCount() < 2 then
      Continue;
    
    // For each predecessor P of B
    for LJ := 0 to LBlock.GetPredecessorCount() - 1 do
    begin
      LPredIdx := LBlock.GetPredecessor(LJ);
      LRunner := LPredIdx;
      
      // Walk up the dominator tree from P to idom(B)
      // Adding B to the dominance frontier of each node along the way
      LIdom := LBlock.GetImmediateDominator();
      
      while (LRunner <> LIdom) and (LRunner >= 0) do
      begin
        AFunc.GetBlock(LRunner).AddDominanceFrontier(LI);
        LRunner := AFunc.GetBlock(LRunner).GetImmediateDominator();
      end;
    end;
  end;
end;

procedure TTigerSSABuilder.EliminatePhiNodes(const AFunc: TTigerSSAFunc);
var
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  LM: Integer;
  LBlock: TTigerSSABlock;
  LPredBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LCopyInstr: TTigerSSAInstr;
  LTermInstr: TTigerSSAInstr;
  LPhiIndices: TList<Integer>;
  LPhiEntry: TTigerSSAPhiEntry;
begin
  // For each block, find phi nodes and convert to copies in predecessors
  for LI := 0 to AFunc.GetBlockCount() - 1 do
  begin
    LBlock := AFunc.GetBlock(LI);
    LPhiIndices := TList<Integer>.Create();
    try
      // Find all phi instructions
      for LJ := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LJ);
        if LInstr.Kind = sikPhi then
          LPhiIndices.Add(LJ)
        else
          Break;  // Phi nodes are at the beginning
      end;
      
      // Process each phi node
      for LJ := LPhiIndices.Count - 1 downto 0 do
      begin
        LInstr := LBlock.GetInstruction(LPhiIndices[LJ]);
        
        // For each phi operand, insert a copy at end of predecessor
        for LK := 0 to High(LInstr.PhiEntries) do
        begin
          LPhiEntry := LInstr.PhiEntries[LK];
          LPredBlock := AFunc.GetBlock(LPhiEntry.BlockIndex);
          
          // Create copy instruction: dest = source
          LCopyInstr := Default(TTigerSSAInstr);
          LCopyInstr.Kind := sikAssign;
          LCopyInstr.Dest := LInstr.Dest;
          LCopyInstr.Op1 := TTigerSSAOperand.FromVar(LPhiEntry.Var_);
          
          // Insert before the terminator (jump instruction)
          // Find terminator position
          for LM := LPredBlock.GetInstructionCount() - 1 downto 0 do
          begin
            LTermInstr := LPredBlock.GetInstruction(LM);
            if LTermInstr.Kind in [sikJump, sikJumpIf, sikJumpIfNot, sikReturn, sikReturnValue] then
            begin
              LPredBlock.InsertInstructionAt(LM, LCopyInstr);
              Break;
            end;
          end;
        end;
        
        // Mark phi as nop (will be skipped during emission)
        LInstr.Kind := sikNop;
        LBlock.SetInstruction(LPhiIndices[LJ], LInstr);
      end;
      
    finally
      LPhiIndices.Free();
    end;
  end;
end;

procedure TTigerSSABuilder.InsertPhiNodes(const AFunc: TTigerSSAFunc);
var
  LVarName: string;
  LDefBlocks: TList<Integer>;       // Blocks where variable is defined
  LPhiBlocks: TDictionary<string, TList<Integer>>; // Blocks with phi for each var
  LWorkList: TList<Integer>;
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LPhiInstr: TTigerSSAInstr;
  LDFBlock: Integer;
  LVarList: TList<string>;
  LBlockPhis: TList<Integer>;
begin
  // Collect all variables that are assigned
  LVarList := TList<string>.Create();
  LPhiBlocks := TDictionary<string, TList<Integer>>.Create();
  try
    // Find all assigned variables and their definition blocks
    for LI := 0 to AFunc.GetBlockCount() - 1 do
    begin
      LBlock := AFunc.GetBlock(LI);
      for LJ := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LJ);
        if (LInstr.Kind = sikAssign) and (LInstr.Dest.BaseName <> '') and 
           (not LInstr.Dest.BaseName.StartsWith('_t')) then
        begin
          LVarName := LInstr.Dest.BaseName;
          if not LVarList.Contains(LVarName) then
            LVarList.Add(LVarName);
          LBlock.AddDefinedVar(LVarName);
        end;
      end;
    end;
    
    // For each variable, compute where phi nodes are needed
    for LI := 0 to LVarList.Count - 1 do
    begin
      LVarName := LVarList[LI];
      LPhiBlocks.Add(LVarName, TList<Integer>.Create());
      
      // Get blocks where this variable is defined
      LDefBlocks := TList<Integer>.Create();
      LWorkList := TList<Integer>.Create();
      try
        for LJ := 0 to AFunc.GetBlockCount() - 1 do
        begin
          if AFunc.GetBlock(LJ).HasDefinedVar(LVarName) then
          begin
            LDefBlocks.Add(LJ);
            LWorkList.Add(LJ);
          end;
        end;
        
        // Iterate: add phi nodes at dominance frontiers
        while LWorkList.Count > 0 do
        begin
          LJ := LWorkList[0];
          LWorkList.Delete(0);
          LBlock := AFunc.GetBlock(LJ);
          
          // For each block in dominance frontier
          for LK := 0 to LBlock.GetDominanceFrontierCount() - 1 do
          begin
            LDFBlock := LBlock.GetDominanceFrontier(LK);
            LBlockPhis := LPhiBlocks[LVarName];
            
            // If we haven't already inserted a phi here
            if not LBlockPhis.Contains(LDFBlock) then
            begin
              LBlockPhis.Add(LDFBlock);
              
              // If this block wasn't already a def block, add to worklist
              if not LDefBlocks.Contains(LDFBlock) then
              begin
                LDefBlocks.Add(LDFBlock);
                LWorkList.Add(LDFBlock);
              end;
            end;
          end;
        end;
      finally
        LWorkList.Free();
        LDefBlocks.Free();
      end;
    end;
    
    // Now actually insert phi instructions at the beginning of blocks
    for LI := 0 to LVarList.Count - 1 do
    begin
      LVarName := LVarList[LI];
      LBlockPhis := LPhiBlocks[LVarName];
      
      for LJ := 0 to LBlockPhis.Count - 1 do
      begin
        LBlock := AFunc.GetBlock(LBlockPhis[LJ]);
        
        // Create phi instruction
        LPhiInstr := Default(TTigerSSAInstr);
        LPhiInstr.Kind := sikPhi;
        LPhiInstr.Dest := TTigerSSAVar.Create(LVarName, 0); // Version assigned during rename
        
        // Pre-allocate phi entries for each predecessor
        SetLength(LPhiInstr.PhiEntries, LBlock.GetPredecessorCount());
        for LK := 0 to LBlock.GetPredecessorCount() - 1 do
        begin
          LPhiInstr.PhiEntries[LK].BlockIndex := LBlock.GetPredecessor(LK);
          LPhiInstr.PhiEntries[LK].Var_ := TTigerSSAVar.Create(LVarName, 0); // Filled during rename
        end;
        
        // Insert at beginning of block
        LBlock.InsertInstructionAt(0, LPhiInstr);
      end;
    end;
    
  finally
    // Clean up phi block lists
    for LVarName in LPhiBlocks.Keys do
      LPhiBlocks[LVarName].Free();
    LPhiBlocks.Free();
    LVarList.Free();
  end;
end;

procedure TTigerSSABuilder.RenameVariables(const AFunc: TTigerSSAFunc);
var
  LVarStacks: TDictionary<string, TStack<Integer>>;
  LVarCounters: TDictionary<string, Integer>;
  
  function GetNewVersion(const AVarName: string): Integer;
  var
    LCounter: Integer;
  begin
    if LVarCounters.TryGetValue(AVarName, LCounter) then
    begin
      Result := LCounter;
      LVarCounters[AVarName] := LCounter + 1;
    end
    else
    begin
      Result := 0;
      LVarCounters.Add(AVarName, 1);
    end;
  end;
  
  function GetCurrentVersion(const AVarName: string): Integer;
  var
    LStack: TStack<Integer>;
  begin
    if LVarStacks.TryGetValue(AVarName, LStack) and (LStack.Count > 0) then
      Result := LStack.Peek()
    else
      Result := 0;
  end;
  
  procedure PushVersion(const AVarName: string; const AVersion: Integer);
  var
    LStack: TStack<Integer>;
  begin
    if not LVarStacks.TryGetValue(AVarName, LStack) then
    begin
      LStack := TStack<Integer>.Create();
      LVarStacks.Add(AVarName, LStack);
    end;
    LStack.Push(AVersion);
  end;
  
  procedure RenameBlock(const ABlockIndex: Integer);
  var
    LBlock: TTigerSSABlock;
    LInstr: TTigerSSAInstr;
    LI: Integer;
    LJ: Integer;
    LK: Integer;
    LNewVersion: Integer;
    LPushedVars: TList<string>;
    LSuccBlock: TTigerSSABlock;
    LSuccInstr: TTigerSSAInstr;
    LPredIdx: Integer;
    LVarName: string;
    LChildIdx: Integer;
  begin
    LBlock := AFunc.GetBlock(ABlockIndex);
    LPushedVars := TList<string>.Create();
    try
      // Process each instruction in the block
      for LI := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LI);
        
        // For phi nodes, only rename the destination
        if LInstr.Kind = sikPhi then
        begin
          LVarName := LInstr.Dest.BaseName;
          LNewVersion := GetNewVersion(LVarName);
          LInstr.Dest := TTigerSSAVar.Create(LVarName, LNewVersion);
          PushVersion(LVarName, LNewVersion);
          LPushedVars.Add(LVarName);
          LBlock.SetInstruction(LI, LInstr);
          Continue;
        end;
        
        // Rename uses (Op1, Op2) - get current version from stack
        // Skip compiler-generated temps (start with '_')
        if (LInstr.Op1.Kind = sokVar) and (not LInstr.Op1.Var_.BaseName.StartsWith('_')) then
        begin
          LVarName := LInstr.Op1.Var_.BaseName;
          LInstr.Op1.Var_ := TTigerSSAVar.Create(LVarName, GetCurrentVersion(LVarName));
        end;
        
        if (LInstr.Op2.Kind = sokVar) and (not LInstr.Op2.Var_.BaseName.StartsWith('_')) then
        begin
          LVarName := LInstr.Op2.Var_.BaseName;
          LInstr.Op2.Var_ := TTigerSSAVar.Create(LVarName, GetCurrentVersion(LVarName));
        end;
        
        // Rename call arguments
        for LJ := 0 to High(LInstr.CallArgs) do
        begin
          if (LInstr.CallArgs[LJ].Kind = sokVar) and 
             (not LInstr.CallArgs[LJ].Var_.BaseName.StartsWith('_')) then
          begin
            LVarName := LInstr.CallArgs[LJ].Var_.BaseName;
            LInstr.CallArgs[LJ].Var_ := TTigerSSAVar.Create(LVarName, GetCurrentVersion(LVarName));
          end;
        end;
        
        // Rename definition (Dest) - create new version
        if (LInstr.Kind = sikAssign) and (LInstr.Dest.BaseName <> '') and
           (not LInstr.Dest.BaseName.StartsWith('_')) then
        begin
          LVarName := LInstr.Dest.BaseName;
          LNewVersion := GetNewVersion(LVarName);
          LInstr.Dest := TTigerSSAVar.Create(LVarName, LNewVersion);
          PushVersion(LVarName, LNewVersion);
          LPushedVars.Add(LVarName);
        end;
        
        LBlock.SetInstruction(LI, LInstr);
      end;
      
      // Fill in phi operands in successor blocks
      for LI := 0 to LBlock.GetSuccessorCount() - 1 do
      begin
        LSuccBlock := AFunc.GetBlock(LBlock.GetSuccessor(LI));
        
        // Find which predecessor index we are
        LPredIdx := -1;
        for LJ := 0 to LSuccBlock.GetPredecessorCount() - 1 do
        begin
          if LSuccBlock.GetPredecessor(LJ) = ABlockIndex then
          begin
            LPredIdx := LJ;
            Break;
          end;
        end;
        
        if LPredIdx < 0 then
          Continue;
        
        // Update phi instructions
        for LJ := 0 to LSuccBlock.GetInstructionCount() - 1 do
        begin
          LSuccInstr := LSuccBlock.GetInstruction(LJ);
          if LSuccInstr.Kind <> sikPhi then
            Break; // Phi nodes are at the beginning
          
          // Find the phi entry for this predecessor
          for LK := 0 to High(LSuccInstr.PhiEntries) do
          begin
            if LSuccInstr.PhiEntries[LK].BlockIndex = ABlockIndex then
            begin
              LVarName := LSuccInstr.Dest.BaseName;
              LSuccInstr.PhiEntries[LK].Var_ := TTigerSSAVar.Create(
                LVarName, GetCurrentVersion(LVarName)
              );
              Break;
            end;
          end;
          
          LSuccBlock.SetInstruction(LJ, LSuccInstr);
        end;
      end;
      
      // Recurse to dominated blocks (children in dominator tree)
      for LI := 0 to LBlock.GetDominatedBlockCount() - 1 do
      begin
        LChildIdx := LBlock.GetDominatedBlock(LI);
        RenameBlock(LChildIdx);
      end;
      
      // Pop versions pushed in this block
      for LI := LPushedVars.Count - 1 downto 0 do
      begin
        LVarName := LPushedVars[LI];
        if LVarStacks.ContainsKey(LVarName) then
          LVarStacks[LVarName].Pop();
      end;
      
    finally
      LPushedVars.Free();
    end;
  end;
  
var
  LVarName: string;
  LI: Integer;
begin
  LVarStacks := TDictionary<string, TStack<Integer>>.Create();
  LVarCounters := TDictionary<string, Integer>.Create();
  try
    // Initialize parameters with version 0 (they come in defined)
    for LI := 0 to AFunc.GetLocalCount() - 1 do
    begin
      if AFunc.GetLocal(LI).IsParam then
      begin
        LVarName := AFunc.GetLocal(LI).LocalName;
        PushVersion(LVarName, 0);
        LVarCounters.Add(LVarName, 1);
      end;
    end;
    
    // Start renaming from entry block (block 0)
    if AFunc.GetBlockCount() > 0 then
      RenameBlock(0);
      
  finally
    // Clean up stacks
    for LVarName in LVarStacks.Keys do
      LVarStacks[LVarName].Free();
    LVarStacks.Free();
    LVarCounters.Free();
  end;
end;

procedure TTigerSSABuilder.SetHandles(
  const AImportHandles: TArray<TTigerImportHandle>;
  const AStringHandles: TArray<TTigerDataHandle>;
  const AGlobalHandles: TArray<TTigerDataHandle>
);
begin
  FImportHandles := AImportHandles;
  FStringHandles := AStringHandles;
  FGlobalHandles := AGlobalHandles;
end;

procedure TTigerSSABuilder.Optimize(const ALevel: Integer);
var
  LI: Integer;
  LJ: Integer;
  LPass: TTigerSSAPass;
  LFunc: TTigerSSAFunc;
  LPassCount: Integer;
begin
  // Skip optimization if level is 0
  if ALevel = 0 then
  begin
    Status('SSA: Optimization skipped (level 0)');
    Exit;
  end;
  
  // Count how many passes will run at this level
  LPassCount := 0;
  for LI := 0 to FPasses.Count - 1 do
  begin
    if FPasses[LI].GetMinLevel() <= ALevel then
      Inc(LPassCount);
  end;
  
  Status('SSA: Running %d optimization passes (level %d)', [LPassCount, ALevel]);
  
  for LI := 0 to FPasses.Count - 1 do
  begin
    LPass := FPasses[LI];
    
    // Skip passes that require higher level
    if LPass.GetMinLevel() > ALevel then
      Continue;
    
    Status('SSA: Running pass: %s', [LPass.GetPassName()]);
    
    for LJ := 0 to FFunctions.Count - 1 do
    begin
      LFunc := FFunctions[LJ];
      LPass.Run(LFunc);
    end;
  end;

  // Function-level DCE - remove unreferenced functions
  RemoveUnreferencedFunctions();
end;

procedure TTigerSSABuilder.RemoveUnreferencedFunctions();
var
  LLive: array of Boolean;
  LIndexMap: array of Integer;  // Old index -> New index (-1 if removed)
  LChanged: Boolean;
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  //LM: Integer;
  LFunc: TTigerSSAFunc;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LTargetIdx: Integer;
  LNewIdx: Integer;
  LRemovedCount: Integer;
  LNewFunctions: TObjectList<TTigerSSAFunc>;
begin
  if FFunctions.Count = 0 then
    Exit;

  //----------------------------------------------------------------------------
  // Step 1: Initialize live set - mark entry points
  //----------------------------------------------------------------------------
  SetLength(LLive, FFunctions.Count);
  for LI := 0 to FFunctions.Count - 1 do
  begin
    LFunc := FFunctions[LI];
    LLive[LI] := LFunc.GetIsEntryPoint() or LFunc.GetIsDllEntry() or LFunc.GetIsPublic();
  end;

  //----------------------------------------------------------------------------
  // Step 1b: Mark exception runtime functions as live if any function uses SEH
  //----------------------------------------------------------------------------
  for LI := 0 to FFunctions.Count - 1 do
  begin
    LFunc := FFunctions[LI];
    if LFunc.GetExceptionScopeCount() > 0 then
    begin
      // This function uses try/except/finally - mark runtime functions as live
      for LJ := 0 to FFunctions.Count - 1 do
      begin
        LFunc := FFunctions[LJ];
        if (LFunc.GetFuncName() = 'Tiger_InitExceptions') or
           (LFunc.GetFuncName() = 'Tiger_SetException') or
           (LFunc.GetFuncName() = 'Tiger_Raise') or
           (LFunc.GetFuncName() = 'Tiger_RaiseCode') or
           (LFunc.GetFuncName() = 'Tiger_GetExceptionCode') or
           (LFunc.GetFuncName() = 'Tiger_GetExceptionMessage') or
           // Linux64-specific: called by backend-generated code, not IR
           (LFunc.GetFuncName() = 'Tiger_PushExceptFrame') or
           (LFunc.GetFuncName() = 'Tiger_PopExceptFrame') or
           (LFunc.GetFuncName() = 'Tiger_InitSignals') or
           (LFunc.GetFuncName() = 'Tiger_SignalHandler') or
           (LFunc.GetFuncName() = 'Tiger_GetExceptFrame') then
          LLive[LJ] := True;
      end;
      Break;  // Only need to check once
    end;
  end;

  //----------------------------------------------------------------------------
  // Step 2: Transitively mark all functions called by live functions
  //----------------------------------------------------------------------------
  repeat
    LChanged := False;

    for LI := 0 to FFunctions.Count - 1 do
    begin
      if not LLive[LI] then
        Continue;

      LFunc := FFunctions[LI];

      // Scan all blocks
      for LJ := 0 to LFunc.GetBlockCount() - 1 do
      begin
        LBlock := LFunc.GetBlock(LJ);

        // Scan all instructions
        for LK := 0 to LBlock.GetInstructionCount() - 1 do
        begin
          LInstr := LBlock.GetInstruction(LK);

          // Check for function calls
          if (LInstr.Kind in [sikCall, sikCallAssign]) and
             (LInstr.CallTarget.Kind = sokFunc) then
          begin
            LTargetIdx := LInstr.CallTarget.FuncIndex;
            if (LTargetIdx >= 0) and (LTargetIdx < Length(LLive)) and (not LLive[LTargetIdx]) then
            begin
              LLive[LTargetIdx] := True;
              LChanged := True;
            end;
          end;

          // Check for function address (indirect calls)
          if (LInstr.Kind = sikFuncAddr) and (LInstr.Op1.Kind = sokFunc) then
          begin
            LTargetIdx := LInstr.Op1.FuncIndex;
            if (LTargetIdx >= 0) and (LTargetIdx < Length(LLive)) and (not LLive[LTargetIdx]) then
            begin
              LLive[LTargetIdx] := True;
              LChanged := True;
            end;
          end;
        end;
      end;
    end;
  until not LChanged;

  //----------------------------------------------------------------------------
  // Step 3: Count removed functions and build index mapping
  //----------------------------------------------------------------------------
  LRemovedCount := 0;
  SetLength(LIndexMap, FFunctions.Count);
  LNewIdx := 0;
  for LI := 0 to FFunctions.Count - 1 do
  begin
    if LLive[LI] then
    begin
      LIndexMap[LI] := LNewIdx;
      Inc(LNewIdx);
    end
    else
    begin
      LIndexMap[LI] := -1;
      Inc(LRemovedCount);
    end;
  end;

  if LRemovedCount = 0 then
  begin
    Status('SSA: Function DCE - no unreferenced functions');
    Exit;
  end;

  Status('SSA: Function DCE - removing %d unreferenced function(s)', [LRemovedCount]);

  //----------------------------------------------------------------------------
  // Step 4: Update function indices in all remaining instructions
  //----------------------------------------------------------------------------
  for LI := 0 to FFunctions.Count - 1 do
  begin
    if not LLive[LI] then
      Continue;

    LFunc := FFunctions[LI];

    for LJ := 0 to LFunc.GetBlockCount() - 1 do
    begin
      LBlock := LFunc.GetBlock(LJ);

      for LK := 0 to LBlock.GetInstructionCount() - 1 do
      begin
        LInstr := LBlock.GetInstruction(LK);

        // Update function call targets
        if (LInstr.Kind in [sikCall, sikCallAssign]) and
           (LInstr.CallTarget.Kind = sokFunc) then
        begin
          LTargetIdx := LInstr.CallTarget.FuncIndex;
          if (LTargetIdx >= 0) and (LTargetIdx < Length(LIndexMap)) then
          begin
            LInstr.CallTarget.FuncIndex := LIndexMap[LTargetIdx];
            LBlock.SetInstruction(LK, LInstr);
          end;
        end;

        // Update function address references
        if (LInstr.Kind = sikFuncAddr) and (LInstr.Op1.Kind = sokFunc) then
        begin
          LTargetIdx := LInstr.Op1.FuncIndex;
          if (LTargetIdx >= 0) and (LTargetIdx < Length(LIndexMap)) then
          begin
            LInstr.Op1.FuncIndex := LIndexMap[LTargetIdx];
            LBlock.SetInstruction(LK, LInstr);
          end;
        end;
      end;
    end;
  end;

  //----------------------------------------------------------------------------
  // Step 5: Build new function list with only live functions
  //----------------------------------------------------------------------------
  LNewFunctions := TObjectList<TTigerSSAFunc>.Create(True);
  try
    // Iterate backwards to avoid index shifting issues when extracting
    for LI := FFunctions.Count - 1 downto 0 do
    begin
      if LLive[LI] then
      begin
        LFunc := FFunctions[LI];
        FFunctions.Extract(LFunc);  // Remove ownership without freeing
        LNewFunctions.Insert(0, LFunc);  // Insert at front to maintain order
      end;
    end;

    // Swap lists - dead functions in FFunctions will be freed
    FFunctions.Free();
    FFunctions := LNewFunctions;
    LNewFunctions := nil;  // Prevent double-free in finally
  finally
    LNewFunctions.Free();
  end;
end;

function TTigerSSABuilder.DumpSSA(): string;
var
  LFuncIdx: Integer;
  LBlockIdx: Integer;
  LInstrIdx: Integer;
  LFunc: TTigerSSAFunc;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LPhiIdx: Integer;
  LArgIdx: Integer;
  LLine: string;
  LResult: TStringBuilder;
begin
  LResult := TStringBuilder.Create();
  try
    LResult.AppendLine('=== SSA Dump ===');
    
    for LFuncIdx := 0 to FFunctions.Count - 1 do
    begin
      LFunc := FFunctions[LFuncIdx];
      LResult.AppendLine('Function: ' + LFunc.GetFuncName());
      LResult.AppendLine('  Locals: ' + IntToStr(LFunc.GetLocalCount()));
      LResult.AppendLine('  Blocks: ' + IntToStr(LFunc.GetBlockCount()));
      
      for LBlockIdx := 0 to LFunc.GetBlockCount() - 1 do
      begin
        LBlock := LFunc.GetBlock(LBlockIdx);
        LResult.AppendLine('  Block ' + IntToStr(LBlockIdx) + ' (' + LBlock.GetBlockName() + ')');
        LResult.AppendLine('    Preds: ' + IntToStr(LBlock.GetPredecessorCount()) + ', Succs: ' + IntToStr(LBlock.GetSuccessorCount()));
        LResult.AppendLine('    IDom: ' + IntToStr(LBlock.GetImmediateDominator()));
        
        for LInstrIdx := 0 to LBlock.GetInstructionCount() - 1 do
        begin
          LInstr := LBlock.GetInstruction(LInstrIdx);
          
          case LInstr.Kind of
            sikNop: LLine := 'nop';
            sikAssign: LLine := Format('%s = %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString()]);
            sikAdd: LLine := Format('%s = %s + %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSub: LLine := Format('%s = %s - %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikMul: LLine := Format('%s = %s * %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikDiv: LLine := Format('%s = %s / %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikFAdd: LLine := Format('%s = float(%s + %s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikFSub: LLine := Format('%s = float(%s - %s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikFMul: LLine := Format('%s = float(%s * %s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikFDiv: LLine := Format('%s = float(%s / %s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikFNeg: LLine := Format('%s = float(-%s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString()]);
            sikCmpEq: LLine := Format('%s = %s == %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikCmpNe: LLine := Format('%s = %s != %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikCmpLt: LLine := Format('%s = %s < %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikCmpLe: LLine := Format('%s = %s <= %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikCmpGt: LLine := Format('%s = %s > %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikCmpGe: LLine := Format('%s = %s >= %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikJump: LLine := Format('jump block_%d', [LInstr.Op1.BlockIndex]);
            sikJumpIf: LLine := Format('jumpif %s block_%d', [LInstr.Op1.ToString(), LInstr.Op2.BlockIndex]);
            sikJumpIfNot: LLine := Format('jumpifnot %s block_%d', [LInstr.Op1.ToString(), LInstr.Op2.BlockIndex]);
            sikCall:
              begin
                if LInstr.CallTarget.Kind = sokFunc then
                  LLine := Format('call func_%d(', [LInstr.CallTarget.FuncIndex])
                else
                  LLine := Format('call import_%d(', [LInstr.CallTarget.ImportIndex]);
                for LArgIdx := 0 to High(LInstr.CallArgs) do
                begin
                  if LArgIdx > 0 then LLine := LLine + ', ';
                  LLine := LLine + LInstr.CallArgs[LArgIdx].ToString();
                end;
                LLine := LLine + ')';
              end;
            sikCallAssign:
              begin
                if LInstr.CallTarget.Kind = sokFunc then
                  LLine := Format('%s = call func_%d(', [LInstr.Dest.ToString(), LInstr.CallTarget.FuncIndex])
                else
                  LLine := Format('%s = call import_%d(', [LInstr.Dest.ToString(), LInstr.CallTarget.ImportIndex]);
                for LArgIdx := 0 to High(LInstr.CallArgs) do
                begin
                  if LArgIdx > 0 then LLine := LLine + ', ';
                  LLine := LLine + LInstr.CallArgs[LArgIdx].ToString();
                end;
                LLine := LLine + ')';
              end;
            sikSyscall:
              begin
                LLine := Format('syscall %d(', [LInstr.SyscallNr]);
                for LArgIdx := 0 to High(LInstr.CallArgs) do
                begin
                  if LArgIdx > 0 then LLine := LLine + ', ';
                  LLine := LLine + LInstr.CallArgs[LArgIdx].ToString();
                end;
                LLine := LLine + ')';
              end;
            sikSyscallAssign:
              begin
                LLine := Format('%s = syscall %d(', [LInstr.Dest.ToString(), LInstr.SyscallNr]);
                for LArgIdx := 0 to High(LInstr.CallArgs) do
                begin
                  if LArgIdx > 0 then LLine := LLine + ', ';
                  LLine := LLine + LInstr.CallArgs[LArgIdx].ToString();
                end;
                LLine := LLine + ')';
              end;
            sikReturn: LLine := 'return';
            sikReturnValue: LLine := Format('return %s', [LInstr.Op1.ToString()]);
            sikPhi:
              begin
                LLine := Format('%s = phi(', [LInstr.Dest.ToString()]);
                for LPhiIdx := 0 to High(LInstr.PhiEntries) do
                begin
                  if LPhiIdx > 0 then LLine := LLine + ', ';
                  LLine := LLine + Format('[block_%d: %s]', [
                    LInstr.PhiEntries[LPhiIdx].BlockIndex,
                    LInstr.PhiEntries[LPhiIdx].Var_.ToString()
                  ]);
                end;
                LLine := LLine + ')';
              end;
            sikLoad: LLine := Format('%s = load(%s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString()]);
            sikStore: LLine := Format('store(%s, %s)', [LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikAddressOf: LLine := Format('%s = addrof(%s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString()]);
            sikFieldAddr: LLine := Format('%s = fieldaddr(%s, %s[off=%d])', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.FieldName, LInstr.Op2.ImmInt]);
            sikIndexAddr: LLine := Format('%s = indexaddr(%s, %s, size=%d)', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString(), LInstr.Op2.ElementSize]);
            sikSetLiteral: LLine := Format('%s = setliteral[%d elements]', [LInstr.Dest.ToString(), Length(LInstr.SetElements)]);
            sikSetUnion: LLine := Format('%s = %s + %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetDiff: LLine := Format('%s = %s - %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetInter: LLine := Format('%s = %s * %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetIn: LLine := Format('%s = %s in %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetEq: LLine := Format('%s = %s =set %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetNe: LLine := Format('%s = %s <>set %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetSubset: LLine := Format('%s = %s <=set %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikSetSuperset: LLine := Format('%s = %s >=set %s', [LInstr.Dest.ToString(), LInstr.Op1.ToString(), LInstr.Op2.ToString()]);
            sikFuncAddr: LLine := Format('%s = funcaddr(%s)', [LInstr.Dest.ToString(), LInstr.Op1.ToString()]);
          else
            LLine := Format('unknown_%d', [Ord(LInstr.Kind)]);
          end;
          
          LResult.AppendLine('      ' + IntToStr(LInstrIdx) + ': ' + LLine);
        end;
      end;
      LResult.AppendLine('');
    end;
    
    LResult.AppendLine('=== End SSA Dump ===');
    Result := LResult.ToString();
  finally
    LResult.Free();
  end;
end;

procedure TTigerSSABuilder.AddPass(const APass: TTigerSSAPass);
begin
  FPasses.Add(APass);
end;

procedure TTigerSSABuilder.ClearPasses();
begin
  FPasses.Clear();
end;

procedure TTigerSSABuilder.EmitTo(const ABackend: TTigerBackend);
var
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  LM: Integer;
  LFunc: TTigerSSAFunc;
  LBlock: TTigerSSABlock;
  LInstr: TTigerSSAInstr;
  LLocal: TTigerSSALocalInfo;
  LLocalHandles: TDictionary<string, TTigerLocalHandle>;
  LLabelHandles: TDictionary<Integer, TTigerLabelHandle>;
  LVarTemps: TDictionary<string, TTigerTempHandle>;
  LVarOperands: TDictionary<string, TTigerOperand>;
  LLocalHandle: TTigerLocalHandle;
  LLabelHandle: TTigerLabelHandle;
  LOp1: TTigerOperand;
  LOp2: TTigerOperand;
  LDestTemp: TTigerTempHandle;
  // Exception scope handling
  LExcScope: TTigerSSAExceptionScope;
  LTryBeginHandle: TTigerLabelHandle;
  LTryEndHandle: TTigerLabelHandle;
  LExceptHandle: TTigerLabelHandle;
  LFinallyHandle: TTigerLabelHandle;
  LEndHandle: TTigerLabelHandle;
  // Syscall argument array
  LArgs: TArray<TTigerOperand>;
begin
  Status('SSA: Emitting to backend (%d functions)', [FFunctions.Count]);
  
  // Eliminate phi nodes before emission (convert to copies)
  for LI := 0 to FFunctions.Count - 1 do
    EliminatePhiNodes(FFunctions[LI]);
  
  for LI := 0 to FFunctions.Count - 1 do
  begin
    LFunc := FFunctions[LI];
    Status('SSA: Emitting function: %s', [LFunc.GetFuncName()]);
    
    LLocalHandles := TDictionary<string, TTigerLocalHandle>.Create();
    LLabelHandles := TDictionary<Integer, TTigerLabelHandle>.Create();
    LVarTemps := TDictionary<string, TTigerTempHandle>.Create();
    LVarOperands := TDictionary<string, TTigerOperand>.Create();
    try
      //------------------------------------------------------------------------
      // Begin function
      //------------------------------------------------------------------------
      ABackend.GetCode.BeginProc(
        LFunc.GetFuncName(),
        LFunc.GetIsEntryPoint(),
        LFunc.GetIsDllEntry(),
        LFunc.GetIsPublic(),
        LFunc.GetLinkage()
      );
      ABackend.GetCode.SetReturnType(LFunc.GetReturnType(), LFunc.GetReturnSize(), LFunc.GetReturnAlignment());
      ABackend.GetCode.SetIsVariadic(LFunc.GetIsVariadic());
      
      //------------------------------------------------------------------------
      // Add parameters first
      //------------------------------------------------------------------------
      for LJ := 0 to LFunc.GetLocalCount() - 1 do
      begin
        LLocal := LFunc.GetLocal(LJ);
        if LLocal.IsParam then
        begin
          if LLocal.LocalTypeRef.IsPrimitive then
            LLocalHandle := ABackend.GetCode.AddParam(LLocal.LocalName, LLocal.LocalTypeRef.Primitive)
          else
            LLocalHandle := ABackend.GetCode.AddParam(LLocal.LocalName, LLocal.LocalSize, LLocal.LocalAlignment);
          LLocalHandles.Add(LLocal.LocalName, LLocalHandle);
        end;
      end;
      
      //------------------------------------------------------------------------
      // Add locals
      //------------------------------------------------------------------------
      for LJ := 0 to LFunc.GetLocalCount() - 1 do
      begin
        LLocal := LFunc.GetLocal(LJ);
        if not LLocal.IsParam then
        begin
          if LLocal.LocalTypeRef.IsPrimitive then
            LLocalHandle := ABackend.GetCode.AddLocal(LLocal.LocalName, LLocal.LocalTypeRef.Primitive)
          else
            LLocalHandle := ABackend.GetCode.AddLocal(LLocal.LocalName, LLocal.LocalSize, LLocal.LocalAlignment);
          LLocalHandles.Add(LLocal.LocalName, LLocalHandle);
        end;
      end;
      
      //------------------------------------------------------------------------
      // Create labels for all blocks
      //------------------------------------------------------------------------
      for LJ := 0 to LFunc.GetBlockCount() - 1 do
      begin
        LBlock := LFunc.GetBlock(LJ);
        LLabelHandle := ABackend.GetCode.DefineLabel(LBlock.GetBlockName());
        LLabelHandles.Add(LBlock.GetBlockId(), LLabelHandle);
      end;
      
      //------------------------------------------------------------------------
      // Emit each block
      //------------------------------------------------------------------------
      for LJ := 0 to LFunc.GetBlockCount() - 1 do
      begin
        LBlock := LFunc.GetBlock(LJ);
        
        // Mark block label
        if LLabelHandles.TryGetValue(LBlock.GetBlockId(), LLabelHandle) then
          ABackend.GetCode.MarkLabel(LLabelHandle);
        
        // Emit instructions
        for LK := 0 to LBlock.GetInstructionCount() - 1 do
        begin
          LInstr := LBlock.GetInstruction(LK);
          
          // Resolve operands
          LOp1 := ResolveOperand(LInstr.Op1, LLocalHandles, LVarTemps, LVarOperands, ABackend);
          LOp2 := ResolveOperand(LInstr.Op2, LLocalHandles, LVarTemps, LVarOperands, ABackend);
          
          case LInstr.Kind of
            sikNop:
              begin
                // Do nothing
              end;
              
            sikAssign:
              begin
                // Store the value in a temp and track it
                if LOp1.Kind = okNone then
                  Continue;
                  
                // For SSA, we track the temp handle by variable name
                LDestTemp := StoreSSAVar(LInstr.Dest, LOp1, LLocalHandles, LVarTemps, LVarOperands, ABackend);
              end;
              
            sikAdd:
              begin
                LDestTemp := ABackend.GetCode.OpAdd(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSub:
              begin
                LDestTemp := ABackend.GetCode.OpSub(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikMul:
              begin
                LDestTemp := ABackend.GetCode.OpMul(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikDiv:
              begin
                LDestTemp := ABackend.GetCode.OpDiv(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikMod:
              begin
                LDestTemp := ABackend.GetCode.OpMod(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikNeg:
              begin
                // Negate: 0 - value
                LDestTemp := ABackend.GetCode.OpSub(0, LOp1);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;

            sikFAdd:
              begin
                LDestTemp := ABackend.GetCode.OpFAdd(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;

            sikFSub:
              begin
                LDestTemp := ABackend.GetCode.OpFSub(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;

            sikFMul:
              begin
                LDestTemp := ABackend.GetCode.OpFMul(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;

            sikFDiv:
              begin
                LDestTemp := ABackend.GetCode.OpFDiv(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;

            sikFNeg:
              begin
                LDestTemp := ABackend.GetCode.OpFNeg(LOp1);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikBitAnd:
              begin
                LDestTemp := ABackend.GetCode.OpAnd(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikBitOr:
              begin
                LDestTemp := ABackend.GetCode.OpOr(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikBitXor:
              begin
                LDestTemp := ABackend.GetCode.OpXor(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikBitNot:
              begin
                LDestTemp := ABackend.GetCode.OpNot(LOp1);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikShl:
              begin
                LDestTemp := ABackend.GetCode.OpShl(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikShr:
              begin
                LDestTemp := ABackend.GetCode.OpShr(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikCmpEq:
              begin
                LDestTemp := ABackend.GetCode.CmpEq(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikCmpNe:
              begin
                LDestTemp := ABackend.GetCode.CmpNe(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikCmpLt:
              begin
                LDestTemp := ABackend.GetCode.CmpLt(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikCmpLe:
              begin
                LDestTemp := ABackend.GetCode.CmpLe(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikCmpGt:
              begin
                LDestTemp := ABackend.GetCode.CmpGt(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikCmpGe:
              begin
                LDestTemp := ABackend.GetCode.CmpGe(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetLiteral:
              begin
                // Use stored type info for LowBound and StorageSize
                LDestTemp := ABackend.GetCode.OpSetLiteral(
                  LInstr.SetElements, 
                  LInstr.SetLowBound,
                  LInstr.SetStorageSize
                );
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetUnion:
              begin
                LDestTemp := ABackend.GetCode.OpSetUnion(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetDiff:
              begin
                LDestTemp := ABackend.GetCode.OpSetDiff(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetInter:
              begin
                LDestTemp := ABackend.GetCode.OpSetInter(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetIn:
              begin
                LDestTemp := ABackend.GetCode.OpSetIn(LOp1, LOp2, LInstr.SetLowBound);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetEq:
              begin
                LDestTemp := ABackend.GetCode.OpSetEq(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetNe:
              begin
                LDestTemp := ABackend.GetCode.OpSetNe(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetSubset:
              begin
                LDestTemp := ABackend.GetCode.OpSetSubset(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikSetSuperset:
              begin
                LDestTemp := ABackend.GetCode.OpSetSuperset(LOp1, LOp2);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikLoad:
              begin
                // Handle sokLocalIndex specially - load from local by index
                if LInstr.Op1.Kind = sokLocalIndex then
                begin
                  LLocal := LFunc.GetLocal(LInstr.Op1.LocalIndex);
                  if LLocalHandles.TryGetValue(LLocal.LocalName, LLocalHandle) then
                  begin
                    LDestTemp := ABackend.GetCode.Load(LLocalHandle);
                    LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
                  end;
                end
                else
                begin
                  LDestTemp := ABackend.GetCode.LoadPtr(LOp1, LInstr.MemSize, LInstr.MemIsFloat);
                  LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
                end;
              end;
              
            sikStore:
              begin
                ABackend.GetCode.StorePtr(LOp1, LOp2, LInstr.MemSize, LInstr.MemIsFloat);
              end;
              
            sikAddressOf:
              begin
                // Get address of local variable
                if LLocalHandles.TryGetValue(LInstr.Op1.Var_.BaseName, LLocalHandle) then
                begin
                  LDestTemp := ABackend.GetCode.AddressOf(LLocalHandle);
                  LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
                end;
              end;
              
            sikFieldAddr:
              begin
                // Compute field address: base + offset
                // LOp1 = base address, LInstr.Op2.FieldName = field name
                // For now, use Op2.ImmInt as field offset (set by caller or default 0)
                // TODO: Look up field offset from type registry
                LDestTemp := ABackend.GetCode.OpAdd(LOp1, LInstr.Op2.ImmInt);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikIndexAddr:
              begin
                // Compute element address: base + (index * elementSize)
                // LOp1 = base address, LOp2 = index, LInstr.Op2.ElementSize = element size
                // addr = base + index * elementSize
                LDestTemp := ABackend.GetCode.OpMul(LOp2, LInstr.Op2.ElementSize);
                LDestTemp := ABackend.GetCode.OpAdd(LOp1, TTigerOperand.FromTemp(LDestTemp));
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikJump:
              begin
                if LLabelHandles.TryGetValue(LInstr.Op1.BlockIndex, LLabelHandle) then
                  ABackend.GetCode.Jump(LLabelHandle);
              end;
              
            sikJumpIf:
              begin
                if LLabelHandles.TryGetValue(LInstr.Op2.BlockIndex, LLabelHandle) then
                  ABackend.GetCode.JumpIf(LOp1, LLabelHandle);
              end;
              
            sikJumpIfNot:
              begin
                if LLabelHandles.TryGetValue(LInstr.Op2.BlockIndex, LLabelHandle) then
                  ABackend.GetCode.JumpIfNot(LOp1, LLabelHandle);
              end;
              
            sikPhi:
              begin
                // TODO: Phi nodes should be eliminated before emission
                // For now, skip them
              end;
              
            sikCall:
              begin
                // Call without return value
                EmitCall(LInstr, LLocalHandles, LVarTemps, LVarOperands, ABackend);
              end;
              
            sikCallAssign:
              begin
                // Call with return value
                LDestTemp := EmitCallWithResult(LInstr, LLocalHandles, LVarTemps, LVarOperands, ABackend);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikFuncAddr:
              begin
                // Get address of a local function
                LDestTemp := ABackend.GetCode.LoadFuncAddr(LInstr.Op1.FuncIndex);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikIndirectCall:
              begin
                // Indirect call (no return value)
                EmitIndirectCall(LInstr, LLocalHandles, LVarTemps, LVarOperands, ABackend);
              end;
              
            sikIndirectCallAssign:
              begin
                // Indirect call with return value
                LDestTemp := EmitIndirectCallWithResult(LInstr, LLocalHandles, LVarTemps, LVarOperands, ABackend);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikReturn:
              begin
                ABackend.GetCode.Return();
              end;
              
            sikReturnValue:
              begin
                ABackend.GetCode.Return(LOp1);
              end;
              
            sikVaCount:
              begin
                LDestTemp := ABackend.GetCode.VaCount();
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
              
            sikVaArgAt:
              begin
                LDestTemp := ABackend.GetCode.VaArgAt(LOp1, LInstr.VaArgType);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;

            sikSyscall:
              begin
                // Syscall as statement (discard result)
                SetLength(LArgs, Length(LInstr.CallArgs));
                for LM := 0 to High(LInstr.CallArgs) do
                  LArgs[LM] := ResolveOperand(LInstr.CallArgs[LM], LLocalHandles, LVarTemps, LVarOperands, ABackend);
                ABackend.GetCode().EmitSyscall(LInstr.SyscallNr, LArgs);
              end;

            sikSyscallAssign:
              begin
                // Syscall as expression (capture result)
                SetLength(LArgs, Length(LInstr.CallArgs));
                for LM := 0 to High(LInstr.CallArgs) do
                  LArgs[LM] := ResolveOperand(LInstr.CallArgs[LM], LLocalHandles, LVarTemps, LVarOperands, ABackend);
                LDestTemp := ABackend.GetCode().EmitSyscallFunc(LInstr.SyscallNr, LArgs);
                LVarTemps.AddOrSetValue(LInstr.Dest.ToString(), LDestTemp);
              end;
          end;
        end;
      end;
      
      //------------------------------------------------------------------------
      // Register exception scopes
      //------------------------------------------------------------------------
      for LK := 0 to LFunc.GetExceptionScopeCount() - 1 do
      begin
        LExcScope := LFunc.GetExceptionScope(LK);
        
        // Map block indices to label handles
        // -1 means no handler for that scope type
        if LLabelHandles.TryGetValue(LExcScope.TryBlockIndex, LTryBeginHandle) and
           LLabelHandles.TryGetValue(LExcScope.EndBlockIndex, LEndHandle) then
        begin
          // Try end block (where try body ends)
          if LExcScope.TryEndBlockIndex >= 0 then
            LLabelHandles.TryGetValue(LExcScope.TryEndBlockIndex, LTryEndHandle)
          else
            LTryEndHandle.Index := -1;
          
          // Except handler
          if LExcScope.ExceptBlockIndex >= 0 then
            LLabelHandles.TryGetValue(LExcScope.ExceptBlockIndex, LExceptHandle)
          else
            LExceptHandle.Index := -1;
          
          // Finally handler
          if LExcScope.FinallyBlockIndex >= 0 then
            LLabelHandles.TryGetValue(LExcScope.FinallyBlockIndex, LFinallyHandle)
          else
            LFinallyHandle.Index := -1;
          
          ABackend.GetCode.AddExceptionScope(
            LTryBeginHandle,
            LTryEndHandle,
            LExceptHandle,
            LFinallyHandle,
            LEndHandle
          );
        end;
      end;
      
      //------------------------------------------------------------------------
      // End function
      //------------------------------------------------------------------------
      ABackend.GetCode.EndProc();
      
    finally
      LVarTemps.Free();
      LVarOperands.Free();
      LLabelHandles.Free();
      LLocalHandles.Free();
    end;
  end;
  
  Status('SSA: Emission complete');
end;

function TTigerSSABuilder.ResolveOperand(
  const AOp: TTigerSSAOperand;
  const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
  const AVarTemps: TDictionary<string, TTigerTempHandle>;
  const AVarOperands: TDictionary<string, TTigerOperand>;
  const ABackend: TTigerBackend
): TTigerOperand;
var
  LLocalHandle: TTigerLocalHandle;
  LTempHandle: TTigerTempHandle;
  LVarKey: string;
  LOp: TTigerOperand;
begin
  case AOp.Kind of
    sokNone:
      Result := TTigerOperand.None();
      
    sokImmediate:
      begin
        if AOp.ImmFloat <> 0 then
          Result := TTigerOperand.FromImm(AOp.ImmFloat)
        else
          Result := TTigerOperand.FromImm(AOp.ImmInt);
      end;
      
    sokVar:
      begin
        LVarKey := AOp.Var_.ToString();
        
        // First check if it's a tracked SSA temp
        if AVarTemps.TryGetValue(LVarKey, LTempHandle) then
        begin
          Result := TTigerOperand.FromTemp(LTempHandle);
        end
        // Next check if it's a tracked SSA operand (e.g. const string okData kept as-is)
        else if Assigned(AVarOperands) and AVarOperands.TryGetValue(LVarKey, LOp) then
        begin
          Result := LOp;
        end
        // Otherwise check if it's a local/param
        else if ALocalHandles.TryGetValue(AOp.Var_.BaseName, LLocalHandle) then
        begin
          // Load the local into a temp
          LTempHandle := ABackend.GetCode.Load(LLocalHandle);
          Result := TTigerOperand.FromTemp(LTempHandle);
        end
        else
        begin
          // Unknown variable - return immediate 0
          Result := TTigerOperand.FromImm(Int64(0));
        end;
      end;
      
    sokData:
      begin
        // Data handle - use actual handle from FStringHandles
        Result := TTigerOperand.FromData(FStringHandles[AOp.DataIndex]);
      end;
      
    sokGlobal:
      begin
        // Global handle - use actual handle from FGlobalHandles (goes to .data section)
        Result := TTigerOperand.FromGlobal(FGlobalHandles[AOp.GlobalIndex]);
      end;
      
    sokImport:
      begin
        // Import handle - use actual handle from FImportHandles
        Result := TTigerOperand.FromImport(FImportHandles[AOp.ImportIndex]);
      end;
      
    sokFunc:
      begin
        // Function handle
        Result := TTigerOperand.FromFunc(TTigerFuncHandle.Invalid());
        Result.FuncHandle.Index := AOp.FuncIndex;
      end;
      
    sokBlock:
      begin
        // Block reference - shouldn't be used as operand directly
        Result := TTigerOperand.None();
      end;
  else
    Result := TTigerOperand.None();
  end;
end;

function TTigerSSABuilder.StoreSSAVar(
  const ADest: TTigerSSAVar;
  const AValue: TTigerOperand;
  const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
  const AVarTemps: TDictionary<string, TTigerTempHandle>;
  const AVarOperands: TDictionary<string, TTigerOperand>;
  const ABackend: TTigerBackend
): TTigerTempHandle;
var
  LLocalHandle: TTigerLocalHandle;
  LVarKey: string;
begin
  LVarKey := ADest.ToString();
  
  // Check if this is a local variable (not a temp)
  if ALocalHandles.TryGetValue(ADest.BaseName, LLocalHandle) then
  begin
    // Store to local - DO NOT cache, always load fresh for loops to work
    ABackend.GetCode.Store(LLocalHandle, AValue);
    Result := TTigerTempHandle.Invalid();
  end
  else
  begin
    // It's a temp variable - track the operand's temp handle
    if AValue.Kind = okTemp then
    begin
      Result := AValue.TempHandle;
      AVarTemps.AddOrSetValue(LVarKey, Result);
    end
    else if AValue.Kind = okImmediate then
    begin
      // Create a temp for immediate value - use add with 0
      Result := ABackend.GetCode.OpAdd(AValue, Int64(0));
      AVarTemps.AddOrSetValue(LVarKey, Result);
    end
    else if AValue.Kind = okData then
    begin
      if Assigned(AVarOperands) then
        AVarOperands.AddOrSetValue(LVarKey, AValue);
      // Create a temp for data address - use add with 0 to materialize address
      Result := ABackend.GetCode.OpAdd(AValue, Int64(0));
      AVarTemps.AddOrSetValue(LVarKey, Result);
    end
    else if AValue.Kind = okGlobal then
    begin
      // Create a temp for global address - use add with 0 to materialize address
      Result := ABackend.GetCode.OpAdd(AValue, Int64(0));
      AVarTemps.AddOrSetValue(LVarKey, Result);
    end
    else if AValue.Kind = okLocal then
    begin
      // Load local into temp
      Result := ABackend.GetCode.Load(AValue.LocalHandle);
      AVarTemps.AddOrSetValue(LVarKey, Result);
    end
    else
    begin
      Result := TTigerTempHandle.Invalid();
    end;
  end;
end;

procedure TTigerSSABuilder.EmitCall(
  const AInstr: TTigerSSAInstr;
  const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
  const AVarTemps: TDictionary<string, TTigerTempHandle>;
  const AVarOperands: TDictionary<string, TTigerOperand>;
  const ABackend: TTigerBackend
);
var
  LArgs: TArray<TTigerOperand>;
  LFuncHandle: TTigerFuncHandle;
  LI: Integer;
  LArg: TTigerSSAOperand;
  LLocalHandle: TTigerLocalHandle;
  LLocalInfo: TTigerLocalInfo;
  LFuncInfo: TTigerFuncInfo;
begin
  // Resolve arguments, with special handling for large struct locals
  SetLength(LArgs, Length(AInstr.CallArgs));
  LFuncInfo := ABackend.GetCode.GetFunc(ABackend.GetCode.GetCurrentFuncHandle().Index);
  for LI := 0 to High(AInstr.CallArgs) do
  begin
    LArg := AInstr.CallArgs[LI];
    // Check for large struct local that needs address passed (Win64 ABI: >8 bytes)
    if (LArg.Kind = sokVar) and
       (not AVarTemps.ContainsKey(LArg.Var_.ToString())) and
       ALocalHandles.TryGetValue(LArg.Var_.BaseName, LLocalHandle) and
       (not LLocalHandle.IsParam) then
    begin
      LLocalInfo := LFuncInfo.Locals[LLocalHandle.Index];
      if (LLocalInfo.LocalType = vtVoid) and (LLocalInfo.LocalSize > 8) then
      begin
        // Large struct local - return local operand for LEA in backend
        LArgs[LI] := TTigerOperand.FromLocal(LLocalHandle);
        Continue;
      end;
    end;
    LArgs[LI] := ResolveOperand(LArg, ALocalHandles, AVarTemps, AVarOperands, ABackend);
  end;
  
  // Call based on target type
  if AInstr.CallTarget.Kind = sokImport then
  begin
    if Length(LArgs) > 0 then
      ABackend.GetCode.Call(FImportHandles[AInstr.CallTarget.ImportIndex], LArgs)
    else
      ABackend.GetCode.Call(FImportHandles[AInstr.CallTarget.ImportIndex]);
  end
  else if AInstr.CallTarget.Kind = sokFunc then
  begin
    // Local function call
    LFuncHandle.Index := AInstr.CallTarget.FuncIndex;
    if Length(LArgs) > 0 then
      ABackend.GetCode.Call(LFuncHandle, LArgs)
    else
      ABackend.GetCode.Call(LFuncHandle);
  end;
end;

function TTigerSSABuilder.EmitCallWithResult(
  const AInstr: TTigerSSAInstr;
  const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
  const AVarTemps: TDictionary<string, TTigerTempHandle>;
  const AVarOperands: TDictionary<string, TTigerOperand>;
  const ABackend: TTigerBackend
): TTigerTempHandle;
var
  LArgs: TArray<TTigerOperand>;
  LFuncHandle: TTigerFuncHandle;
  LI: Integer;
  LArg: TTigerSSAOperand;
  LLocalHandle: TTigerLocalHandle;
  LLocalInfo: TTigerLocalInfo;
  LFuncInfo: TTigerFuncInfo;
begin
  // Resolve arguments, with special handling for large struct locals
  SetLength(LArgs, Length(AInstr.CallArgs));
  LFuncInfo := ABackend.GetCode.GetFunc(ABackend.GetCode.GetCurrentFuncHandle().Index);
  for LI := 0 to High(AInstr.CallArgs) do
  begin
    LArg := AInstr.CallArgs[LI];
    // Check for large struct local that needs address passed (Win64 ABI: >8 bytes)
    if (LArg.Kind = sokVar) and
       (not AVarTemps.ContainsKey(LArg.Var_.ToString())) and
       ALocalHandles.TryGetValue(LArg.Var_.BaseName, LLocalHandle) and
       (not LLocalHandle.IsParam) then
    begin
      LLocalInfo := LFuncInfo.Locals[LLocalHandle.Index];
      if (LLocalInfo.LocalType = vtVoid) and (LLocalInfo.LocalSize > 8) then
      begin
        // Large struct local - return local operand for LEA in backend
        LArgs[LI] := TTigerOperand.FromLocal(LLocalHandle);
        Continue;
      end;
    end;
    LArgs[LI] := ResolveOperand(LArg, ALocalHandles, AVarTemps, AVarOperands, ABackend);
  end;
  
  // Call based on target type
  if AInstr.CallTarget.Kind = sokImport then
  begin
    if Length(LArgs) > 0 then
      Result := ABackend.GetCode.CallFunc(FImportHandles[AInstr.CallTarget.ImportIndex], LArgs)
    else
      Result := ABackend.GetCode.CallFunc(FImportHandles[AInstr.CallTarget.ImportIndex]);
  end
  else if AInstr.CallTarget.Kind = sokFunc then
  begin
    // Local function call
    LFuncHandle.Index := AInstr.CallTarget.FuncIndex;
    if Length(LArgs) > 0 then
      Result := ABackend.GetCode.CallFunc(LFuncHandle, LArgs)
    else
      Result := ABackend.GetCode.CallFunc(LFuncHandle);
  end
  else
  begin
    Result := TTigerTempHandle.Invalid();
  end;
end;

procedure TTigerSSABuilder.EmitIndirectCall(
  const AInstr: TTigerSSAInstr;
  const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
  const AVarTemps: TDictionary<string, TTigerTempHandle>;
  const AVarOperands: TDictionary<string, TTigerOperand>;
  const ABackend: TTigerBackend
);
var
  LArgs: TArray<TTigerOperand>;
  LFuncPtr: TTigerOperand;
  LI: Integer;
begin
  // Resolve the function pointer operand
  LFuncPtr := ResolveOperand(AInstr.Op1, ALocalHandles, AVarTemps, AVarOperands, ABackend);
  
  // Resolve arguments
  SetLength(LArgs, Length(AInstr.CallArgs));
  for LI := 0 to High(AInstr.CallArgs) do
    LArgs[LI] := ResolveOperand(AInstr.CallArgs[LI], ALocalHandles, AVarTemps, AVarOperands, ABackend);
  
  // Indirect call through function pointer
  if Length(LArgs) > 0 then
    ABackend.GetCode.CallIndirect(LFuncPtr, LArgs)
  else
    ABackend.GetCode.CallIndirect(LFuncPtr);
end;

function TTigerSSABuilder.EmitIndirectCallWithResult(
  const AInstr: TTigerSSAInstr;
  const ALocalHandles: TDictionary<string, TTigerLocalHandle>;
  const AVarTemps: TDictionary<string, TTigerTempHandle>;
  const AVarOperands: TDictionary<string, TTigerOperand>;
  const ABackend: TTigerBackend
): TTigerTempHandle;
var
  LArgs: TArray<TTigerOperand>;
  LFuncPtr: TTigerOperand;
  LI: Integer;
begin
  // Resolve the function pointer operand
  LFuncPtr := ResolveOperand(AInstr.Op1, ALocalHandles, AVarTemps, AVarOperands, ABackend);
  
  // Resolve arguments
  SetLength(LArgs, Length(AInstr.CallArgs));
  for LI := 0 to High(AInstr.CallArgs) do
    LArgs[LI] := ResolveOperand(AInstr.CallArgs[LI], ALocalHandles, AVarTemps, AVarOperands, ABackend);
  
  // Indirect call through function pointer with result
  if Length(LArgs) > 0 then
    Result := ABackend.GetCode.CallIndirectFunc(LFuncPtr, LArgs)
  else
    Result := ABackend.GetCode.CallIndirectFunc(LFuncPtr);
end;

function TTigerSSABuilder.GetFunctionCount(): Integer;
begin
  Result := FFunctions.Count;
end;

function TTigerSSABuilder.GetFunction(const AIndex: Integer): TTigerSSAFunc;
begin
  Result := FFunctions[AIndex];
end;

function TTigerSSABuilder.GetStrReleaseFuncIdx(): Integer;
begin
  Result := FStrReleaseFuncIdx;
end;

function TTigerSSABuilder.GetReportLeaksFuncIdx(): Integer;
begin
  Result := FReportLeaksFuncIdx;
end;

function TTigerSSABuilder.GetReleaseOnDetachFuncIdx(): Integer;
begin
  Result := FReleaseOnDetachFuncIdx;
end;

procedure TTigerSSABuilder.Clear();
begin
  FFunctions.Clear();
  FCurrentFuncIndex := -1;
  FStrReleaseFuncIdx := -1;
  FReportLeaksFuncIdx := -1;
  FReleaseOnDetachFuncIdx := -1;
  FExpressionCache.Clear();
  SetLength(FStringHandles, 0);
  SetLength(FImportHandles, 0);
end;

end.
