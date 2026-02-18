{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.JIT;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Tiger.Utils;

type
  { TTigerJIT }
  /// <summary>
  ///   Base class for JIT (Just-In-Time) compiled code execution.
  ///   Holds executable memory containing compiled machine code and provides
  ///   methods to retrieve and invoke functions by name or pointer.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     TTigerJIT instances are created by <see cref="TTigerBackend.BuildJIT"/>
  ///     and should be freed when no longer needed. The executable memory is
  ///     automatically released when the object is destroyed.
  ///   </para>
  ///   <para>
  ///     Platform-specific subclasses (TTigerJITWin64, TTigerJITLinux64) handle
  ///     memory allocation and dynamic library loading.
  ///   </para>
  /// </remarks>
  TTigerJIT = class(TBaseObject)
  protected
    FCodeBase: Pointer;
    FCodeSize: NativeUInt;
    FSymbols: TDictionary<string, NativeUInt>;

    /// <summary>
    ///   Allocates executable memory of the specified size.
    /// </summary>
    /// <param name="ASize">Size in bytes of memory to allocate.</param>
    /// <remarks>
    ///   Platform-specific: Win64 uses VirtualAlloc, Linux64 uses mmap.
    /// </remarks>
    procedure AllocateExecutable(const ASize: NativeUInt); virtual; abstract;

    /// <summary>
    ///   Frees previously allocated executable memory.
    /// </summary>
    procedure FreeExecutable(); virtual; abstract;

  public
    /// <summary>
    ///   Resolves an imported symbol from a dynamic library.
    /// </summary>
    /// <param name="ALibrary">Name of the library (e.g., 'msvcrt.dll', 'libc.so.6').</param>
    /// <param name="ASymbol">Name of the symbol to resolve.</param>
    /// <returns>Pointer to the resolved symbol.</returns>
    /// <remarks>
    ///   Platform-specific: Win64 uses LoadLibrary/GetProcAddress,
    ///   Linux64 uses dlopen/dlsym.
    /// </remarks>
    function ResolveImport(const ALibrary: string; const ASymbol: string): Pointer; virtual; abstract;

    constructor Create(); override;
    destructor Destroy(); override;

    //--------------------------------------------------------------------------
    // Backend Interface (called during BuildJIT)
    //--------------------------------------------------------------------------

    /// <summary>
    ///   Sets the code size and allocates executable memory.
    /// </summary>
    /// <param name="ASize">Size in bytes of the code section.</param>
    /// <remarks>
    ///   Called by the backend during BuildJIT. Triggers AllocateExecutable.
    /// </remarks>
    procedure SetCodeSize(const ASize: NativeUInt);

    /// <summary>
    ///   Returns the base address of the executable memory region.
    /// </summary>
    /// <returns>Pointer to the start of executable memory.</returns>
    /// <remarks>
    ///   The backend copies generated machine code to this address.
    /// </remarks>
    function GetCodeBase(): Pointer;

    /// <summary>
    ///   Registers a symbol (function) with its offset from code base.
    /// </summary>
    /// <param name="AName">Name of the symbol.</param>
    /// <param name="AOffset">Byte offset from FCodeBase.</param>
    procedure AddSymbol(const AName: string; const AOffset: NativeUInt);

    //--------------------------------------------------------------------------
    // User API - Symbol Access
    //--------------------------------------------------------------------------

    /// <summary>
    ///   Gets the address of a named symbol (function).
    /// </summary>
    /// <param name="AName">Name of the function to look up.</param>
    /// <returns>Pointer to the function, or nil if not found.</returns>
    /// <remarks>
    ///   The returned pointer can be cast to a procedural type for direct calls,
    ///   or passed to Invoke for dynamic invocation.
    /// </remarks>
    /// <example>
    ///   <code>
    ///   var
    ///     LAddFunc: function(A, B: Int64): Int64;
    ///   begin
    ///     LAddFunc := LJIT.GetSymbol('MyAdd');
    ///     Result := LAddFunc(10, 20);
    ///   end;
    ///   </code>
    /// </example>
    function GetSymbol(const AName: string): Pointer;

    /// <summary>
    ///   Checks if a symbol exists in the JIT code.
    /// </summary>
    /// <param name="AName">Name of the symbol to check.</param>
    /// <returns>True if the symbol exists, False otherwise.</returns>
    function HasSymbol(const AName: string): Boolean;

    /// <summary>
    ///   Returns an array of all registered symbol names.
    /// </summary>
    /// <returns>Array of symbol names.</returns>
    function GetSymbolNames(): TArray<string>;

    //--------------------------------------------------------------------------
    // User API - Dynamic Invocation
    //--------------------------------------------------------------------------

    /// <summary>
    ///   Invokes a function by pointer with the given arguments.
    /// </summary>
    /// <param name="APtr">Pointer to the function (from GetSymbol).</param>
    /// <param name="AArgs">Array of Int64 arguments.</param>
    /// <returns>Int64 return value from the function.</returns>
    /// <remarks>
    ///   <para>
    ///     All arguments are passed as Int64, which can hold integers, pointers,
    ///     and handles. Cast pointers using Int64(PAnsiChar('text')).
    ///   </para>
    ///   <para>
    ///     Currently supports up to 4 arguments on Win64, 6 on Linux64.
    ///   </para>
    /// </remarks>
    /// <example>
    ///   <code>
    ///   LPtr := LJIT.GetSymbol('MyAdd');
    ///   Result := LJIT.Invoke(LPtr, [10, 20]);
    ///   </code>
    /// </example>
    function Invoke(const APtr: Pointer; const AArgs: array of Int64): Int64; overload;

    /// <summary>
    ///   Invokes a function by name with the given arguments.
    /// </summary>
    /// <param name="AName">Name of the function to invoke.</param>
    /// <param name="AArgs">Array of Int64 arguments.</param>
    /// <returns>Int64 return value from the function.</returns>
    /// <remarks>
    ///   Convenience method that calls GetSymbol then Invoke.
    ///   For repeated calls to the same function, use GetSymbol once
    ///   and call Invoke with the pointer for better performance.
    /// </remarks>
    /// <example>
    ///   <code>
    ///   Result := LJIT.Invoke('MyAdd', [10, 20]);
    ///   </code>
    /// </example>
    function Invoke(const AName: string; const AArgs: array of Int64): Int64; overload;
  end;

implementation

//==============================================================================
// TTigerJIT
//==============================================================================

constructor TTigerJIT.Create();
begin
  inherited Create();
  FCodeBase := nil;
  FCodeSize := 0;
  FSymbols := TDictionary<string, NativeUInt>.Create();
end;

destructor TTigerJIT.Destroy();
begin
  FreeExecutable();
  FSymbols.Free();
  inherited Destroy();
end;

//------------------------------------------------------------------------------
// Backend Interface
//------------------------------------------------------------------------------

procedure TTigerJIT.SetCodeSize(const ASize: NativeUInt);
begin
  if FCodeBase <> nil then
    FreeExecutable();
  AllocateExecutable(ASize);
end;

function TTigerJIT.GetCodeBase(): Pointer;
begin
  Result := FCodeBase;
end;

procedure TTigerJIT.AddSymbol(const AName: string; const AOffset: NativeUInt);
begin
  FSymbols.AddOrSetValue(AName, AOffset);
end;

//------------------------------------------------------------------------------
// User API - Symbol Access
//------------------------------------------------------------------------------

function TTigerJIT.GetSymbol(const AName: string): Pointer;
var
  LOffset: NativeUInt;
begin
  if FSymbols.TryGetValue(AName, LOffset) then
    Result := Pointer(NativeUInt(FCodeBase) + LOffset)
  else
    Result := nil;
end;

function TTigerJIT.HasSymbol(const AName: string): Boolean;
begin
  Result := FSymbols.ContainsKey(AName);
end;

function TTigerJIT.GetSymbolNames(): TArray<string>;
begin
  Result := FSymbols.Keys.ToArray();
end;

//------------------------------------------------------------------------------
// User API - Dynamic Invocation
//------------------------------------------------------------------------------

// Dynamic call helper - sets up Win64 ABI call frame for any argument count
// Input: RCX = AFunc, RDX = AArgs, R8D = AArgCount
// Output: RAX = return value
function DynCall(AFunc: Pointer; AArgs: PInt64; AArgCount: Integer): Int64;
asm
  // Save non-volatile registers
  push rbx
  push rsi
  push rdi
  push r12
  push r13
  push r14
  push r15
  push rbp

  // Save parameters to non-volatiles
  mov r12, rcx           // r12 = function pointer
  mov r13, rdx           // r13 = args pointer
  mov r14d, r8d          // r14 = arg count

  // Calculate stack args: max(0, argcount - 4)
  xor r15d, r15d
  cmp r14d, 4
  jle @@CalcAlloc
  lea r15d, [r14d - 4]

@@CalcAlloc:
  // Alloc = 32 (shadow) + r15 * 8 (stack args)
  lea eax, [r15d * 8 + 32]

  // Align to 16 bytes for call (we're 8-misaligned after pushes)
  test eax, 15
  jnz @@AllocStack
  add eax, 8

@@AllocStack:
  mov ebx, eax
  sub rsp, rbx

  // Copy stack args (indices 4+)
  test r15d, r15d
  jz @@SetupRegs

  xor ecx, ecx
  lea r10, [rsp + 32]

@@CopyStackArgs:
  cmp ecx, r15d
  jge @@SetupRegs
  lea eax, [ecx + 4]
  mov rax, [r13 + rax * 8]
  mov [r10 + rcx * 8], rax
  inc ecx
  jmp @@CopyStackArgs

@@SetupRegs:
  // Load register args (first 4)
  cmp r14d, 1
  jl @@Call
  mov rcx, [r13]

  cmp r14d, 2
  jl @@Call
  mov rdx, [r13 + 8]

  cmp r14d, 3
  jl @@Call
  mov r8, [r13 + 16]

  cmp r14d, 4
  jl @@Call
  mov r9, [r13 + 24]

@@Call:
  call r12

  add rsp, rbx

  pop rbp
  pop r15
  pop r14
  pop r13
  pop r12
  pop rdi
  pop rsi
  pop rbx
end;

function TTigerJIT.Invoke(const APtr: Pointer; const AArgs: array of Int64): Int64;
begin
  if APtr = nil then
    raise Exception.Create('Cannot invoke nil pointer');

  if Length(AArgs) = 0 then
    Result := DynCall(APtr, nil, 0)
  else
    Result := DynCall(APtr, @AArgs[0], Length(AArgs));
end;

function TTigerJIT.Invoke(const AName: string; const AArgs: array of Int64): Int64;
var
  LPtr: Pointer;
begin
  LPtr := GetSymbol(AName);
  if LPtr = nil then
    raise Exception.CreateFmt('Symbol not found: %s', [AName]);
  Result := Invoke(LPtr, AArgs);
end;

end.
