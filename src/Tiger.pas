{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger;

{$I Tiger.Defines.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.IOUtils,
  Tiger.Utils,
  Tiger.Utils.Win64,
  Tiger.Errors,
  Tiger.Common,
  Tiger.Types,
  Tiger.Builders,
  Tiger.Backend,
  Tiger.Backend.Win64,
  Tiger.IR,
  Tiger.Runtime,
  Tiger.Runtime.Win64;

//==============================================================================
// TYPE ALIASES
//==============================================================================
// Re-export all public types so the user only needs "uses Tiger;" in their
// uses clause. Nothing else is required.
//==============================================================================

type
  /// <summary>
  ///   Primitive value types for variables, parameters, and return values.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Determines the size, signedness, and encoding of scalar values in
  ///     generated code. All integer types use two's-complement representation;
  ///     floating-point types follow IEEE 754. Pointer is always 64-bit on
  ///     the x86-64 target.
  ///   </para>
  ///   <para>
  ///     Use <c>vtVoid</c> exclusively for return types of procedures that
  ///     produce no value. It is invalid as a variable or parameter type.
  ///   </para>
  /// </remarks>
  /// <seealso cref="TTigerTypeRef"/>
  TTigerValueType = Tiger.Types.TTigerValueType;

  /// <summary>
  ///   Target subsystem for executable output (console or GUI).
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Controls the <c>IMAGE_SUBSYSTEM</c> field in the PE optional header.
  ///     Console executables receive an attached console window on launch;
  ///     GUI executables do not and must create their own windows if needed.
  ///   </para>
  ///   <para>
  ///     This setting is only meaningful when the output type is an
  ///     executable (<see cref="TTiger.TargetExe"/>).
  ///   </para>
  /// </remarks>
  TTigerSubsystem = Tiger.Types.TTigerSubsystem;

  /// <summary>
  ///   Output binary format (exe, dll, lib, obj).
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Selects the kind of file the linker produces. Executable and DLL
  ///     targets emit a complete PE image; library and object targets emit
  ///     COFF archives or relocatable objects suitable for consumption by
  ///     external linkers.
  ///   </para>
  /// </remarks>
  TTigerOutputType = Tiger.Types.TTigerOutputType;

  /// <summary>
  ///   Function linkage specification controlling name-mangling and calling
  ///   convention metadata.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     <c>plDefault</c> uses Itanium C++ ABI mangling, which encodes
  ///     parameter types into the symbol name and enables function
  ///     overloading at the linker level. <c>plC</c> emits unmangled,
  ///     C-compatible symbols — required when importing or exporting
  ///     functions from DLLs.
  ///   </para>
  ///   <para>
  ///     Use <c>plC</c> for all <see cref="TTiger.ImportDll"/> and
  ///     <see cref="TTiger.ImportLib"/> declarations, and for any function
  ///     intended to be called from C or other non-Tiger code.
  ///   </para>
  /// </remarks>
  TTigerLinkage = Tiger.Common.TTigerLinkage;

  /// <summary>
  ///   Opaque expression handle returned by expression-building methods.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Expression handles are lightweight value types that reference nodes
  ///     in the IR expression graph. They are produced by literal constructors
  ///     (<see cref="TTiger.Int64"/>, <see cref="TTiger.Str"/>, etc.),
  ///     arithmetic operators (<see cref="TTiger.Add"/>, <see cref="TTiger.Mul"/>,
  ///     etc.), and other expression builders.
  ///   </para>
  ///   <para>
  ///     Handles are only valid within the scope of the <see cref="TTiger"/>
  ///     instance that created them and must not be stored beyond the
  ///     lifetime of that instance. They support implicit conversion from
  ///     <c>Integer</c>, <c>Int64</c>, and <c>Double</c> for convenience
  ///     in simple literal contexts.
  ///   </para>
  /// </remarks>
  /// <seealso cref="TTiger.Int64"/>
  /// <seealso cref="TTiger.Str"/>
  /// <seealso cref="TTiger.Get"/>
  /// <seealso cref="TTiger.Invoke"/>
  TTigerExpr = Tiger.IR.TTigerIRExpr;

  /// <summary>
  ///   Reference to a type (primitive or composite) for the type system.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Type references are used when declaring variables or parameters that
  ///     use composite types (records, arrays, enums, etc.) rather than
  ///     simple primitives. Obtain a type reference by calling
  ///     <see cref="TTiger.TypeRef"/> with a previously defined type name.
  ///   </para>
  ///   <para>
  ///     Primitive types can also be wrapped in a <c>TTigerTypeRef</c>
  ///     internally, but for most API methods you can pass
  ///     <see cref="TTigerValueType"/> directly.
  ///   </para>
  /// </remarks>
  /// <seealso cref="TTiger.TypeRef"/>
  /// <seealso cref="TTiger.GetTypeSize"/>
  /// <seealso cref="TTiger.GetTypeAlignment"/>
  TTigerTypeRef = Tiger.IR.TTigerTypeRef;

  /// <summary>
  ///   Severity level of a compiler diagnostic.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Diagnostics are emitted during IR validation, code emission, and
  ///     linking. <c>esHint</c> and <c>esWarning</c> do not prevent
  ///     successful builds; <c>esError</c> marks a failed compilation;
  ///     <c>esFatal</c> immediately aborts compilation.
  ///   </para>
  /// </remarks>
  /// <seealso cref="TError"/>
  /// <seealso cref="TTiger.HasErrors"/>
  /// <seealso cref="TTiger.SetMaxErrors"/>
  TErrorSeverity = Tiger.Errors.TErrorSeverity;

  /// <summary>
  ///   A single compiler diagnostic record containing severity, location,
  ///   and message text.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Each <c>TError</c> captures a <see cref="TSourceRange"/>,
  ///     a <see cref="TErrorSeverity"/>, and a human-readable message.
  ///     Retrieve the full list via <see cref="TTiger.GetErrorItems"/> or
  ///     a formatted string via <see cref="TTiger.GetErrorText"/>.
  ///   </para>
  /// </remarks>
  /// <seealso cref="TErrorSeverity"/>
  /// <seealso cref="TSourceRange"/>
  /// <seealso cref="TTiger.GetErrorItems"/>
  TError = Tiger.Errors.TError;

  /// <summary>
  ///   Source location range for error reporting, identifying the span of
  ///   source text associated with a diagnostic.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Primarily used by the internal error manager. When building code
  ///     programmatically (rather than parsing source files), the range
  ///     fields may be zeroed or set to synthetic locations for
  ///     organizational purposes.
  ///   </para>
  /// </remarks>
  TSourceRange = Tiger.Errors.TSourceRange;

  /// <summary>
  ///   Callback signature for receiving status messages during compilation.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Defined as:
  ///     <c>reference to procedure(const AText: string; const AUserData: Pointer)</c>.
  ///   </para>
  ///   <para>
  ///     The callback is invoked synchronously on the calling thread during
  ///     <see cref="TTiger.Build"/>. Typical messages include function
  ///     emission progress, linking stages, and optimization passes.
  ///   </para>
  /// </remarks>
  /// <seealso cref="TTiger.SetStatusCallback"/>
  TStatusCallback = Tiger.Utils.TStatusCallback;

  /// <summary>
  ///   Handle to a data entry (string literal, byte blob, or typed global
  ///   constant) in the backend data section.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Data handles are opaque indices into the backend's data table.
  ///     They are created internally when you call methods like
  ///     <see cref="TTiger.Str"/> or <see cref="TTiger.WStr"/>, and are
  ///     primarily exposed for advanced users working directly with the
  ///     backend via <see cref="TTiger.GetBackend"/>.
  ///   </para>
  /// </remarks>
  TTigerDataHandle = Tiger.Types.TTigerDataHandle;

  /// <summary>
  ///   Handle to an imported function registered in the backend import table.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Import handles are created internally by
  ///     <see cref="TTiger.ImportDll"/> and <see cref="TTiger.ImportLib"/>.
  ///     They are primarily exposed for advanced users working directly with
  ///     the backend via <see cref="TTiger.GetBackend"/>.
  ///   </para>
  /// </remarks>
  TTigerImportHandle = Tiger.Types.TTigerImportHandle;

  /// <summary>
  ///   Operand for low-level backend code builder instructions.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Represents an immediate value, data reference, import reference,
  ///     local variable, temporary, or function handle in the backend
  ///     instruction stream. Supports implicit conversion from common
  ///     Delphi numeric types for convenience.
  ///   </para>
  ///   <para>
  ///     This type is intended for advanced users who bypass the IR layer
  ///     and emit instructions directly to <see cref="TTiger.GetBackend"/>.
  ///     Most users should use the high-level TTiger API instead.
  ///   </para>
  /// </remarks>
  TTigerOperand = Tiger.Types.TTigerOperand;

//==============================================================================
// ENUM VALUE CONSTANTS
//==============================================================================
// Re-export enum values so the user can write vtInt32 instead of
// Tiger.Types.TTigerValueType.vtInt32.
//==============================================================================

const

  //--- Value Types ------------------------------------------------------------

  /// <summary>No return value (procedure). Invalid for variables or parameters.</summary>
  vtVoid    = Tiger.Types.TTigerValueType.vtVoid;
  /// <summary>Signed 8-bit integer (range -128..127). Maps to a single byte in the output.</summary>
  vtInt8    = Tiger.Types.TTigerValueType.vtInt8;
  /// <summary>Signed 16-bit integer (range -32768..32767).</summary>
  vtInt16   = Tiger.Types.TTigerValueType.vtInt16;
  /// <summary>Signed 32-bit integer. Compatible with the Windows <c>INT</c>/<c>LONG</c> types.</summary>
  vtInt32   = Tiger.Types.TTigerValueType.vtInt32;
  /// <summary>Signed 64-bit integer. Compatible with the Windows <c>LONGLONG</c> type and Delphi <c>Int64</c>.</summary>
  vtInt64   = Tiger.Types.TTigerValueType.vtInt64;
  /// <summary>Unsigned 8-bit integer (range 0..255). Commonly used for byte buffers and <c>BYTE</c> parameters.</summary>
  vtUInt8   = Tiger.Types.TTigerValueType.vtUInt8;
  /// <summary>Unsigned 16-bit integer (range 0..65535). Compatible with <c>WORD</c>.</summary>
  vtUInt16  = Tiger.Types.TTigerValueType.vtUInt16;
  /// <summary>Unsigned 32-bit integer (range 0..4294967295). Compatible with <c>DWORD</c> and Windows API exit codes.</summary>
  vtUInt32  = Tiger.Types.TTigerValueType.vtUInt32;
  /// <summary>Unsigned 64-bit integer. Compatible with <c>QWORD</c> and <c>UInt64</c>.</summary>
  vtUInt64  = Tiger.Types.TTigerValueType.vtUInt64;
  /// <summary>32-bit IEEE 754 single-precision floating point. Compatible with Delphi <c>Single</c> and C <c>float</c>.</summary>
  vtFloat32 = Tiger.Types.TTigerValueType.vtFloat32;
  /// <summary>64-bit IEEE 754 double-precision floating point. Compatible with Delphi <c>Double</c> and C <c>double</c>.</summary>
  vtFloat64 = Tiger.Types.TTigerValueType.vtFloat64;
  /// <summary>64-bit pointer. Used for string arguments, buffer addresses, function pointers, and opaque handles.</summary>
  vtPointer = Tiger.Types.TTigerValueType.vtPointer;

  //--- Subsystem --------------------------------------------------------------

  /// <summary>Console subsystem — the OS attaches a console window providing stdout/stdin/stderr.</summary>
  ssConsole = Tiger.Types.TTigerSubsystem.ssConsole;
  /// <summary>GUI subsystem — no console window is created. The application must create its own windows.</summary>
  ssGui     = Tiger.Types.TTigerSubsystem.ssGui;

  //--- Linkage ----------------------------------------------------------------

  /// <summary>Default linkage — Itanium C++ ABI name mangling. Enables function overloading at the symbol level.</summary>
  plDefault = Tiger.Common.TTigerLinkage.plDefault;
  /// <summary>C linkage — no name mangling. Required for DLL imports/exports and interoperability with C code.</summary>
  plC       = Tiger.Common.TTigerLinkage.plC;

  //--- Error Severity ---------------------------------------------------------

  /// <summary>Informational hint. Does not affect build success.</summary>
  esHint    = Tiger.Errors.TErrorSeverity.esHint;
  /// <summary>Compiler warning. Indicates a potential problem but does not prevent a successful build.</summary>
  esWarning = Tiger.Errors.TErrorSeverity.esWarning;
  /// <summary>Compiler error. The build will fail, but compilation continues to report additional errors.</summary>
  esError   = Tiger.Errors.TErrorSeverity.esError;
  /// <summary>Fatal error. Compilation is immediately aborted — no further diagnostics are emitted.</summary>
  esFatal   = Tiger.Errors.TErrorSeverity.esFatal;

  //--- Version ----------------------------------------------------------------

  /// <summary>
  ///   Tiger version as a packed integer: <c>major * 10000 + minor * 100 + patch</c>.
  ///   Suitable for numeric comparisons (e.g. <c>if TIGER_VERSION >= 10200 then ...</c>).
  /// </summary>
  TIGER_VERSION     = Tiger.Common.TIGER_VERSION;

  /// <summary>
  ///   Tiger version as a human-readable dot-separated string (e.g. <c>'1.2.0'</c>).
  /// </summary>
  TIGER_VERSION_STR = Tiger.Common.TIGER_VERSION_STR;

type
  //============================================================================
  // TTiger — Unified Compiler Facade
  //============================================================================

  /// <summary>
  ///   Unified facade for the Tiger compiler infrastructure. Owns and
  ///   coordinates the error manager, intermediate representation (IR)
  ///   builder, and backend code generator.
  /// </summary>
  /// <remarks>
  ///   <para><b>Overview</b></para>
  ///   <para>
  ///     <c>TTiger</c> is the primary entry point for the Tiger compiler
  ///     infrastructure and the only class most users need to interact with.
  ///     It exposes a fluent (method-chaining) API that covers the full
  ///     compilation pipeline: type definition, function declaration,
  ///     statement and expression building, optimization, and code
  ///     generation.
  ///   </para>
  ///   <para><b>Architecture</b></para>
  ///   <para>
  ///     Internally, TTiger owns three subsystems:
  ///   </para>
  ///   <para>
  ///     • <b>Error Manager</b> (<see cref="TErrors"/>) — collects
  ///     diagnostics (hints, warnings, errors, fatals) during all
  ///     compilation phases.
  ///   </para>
  ///   <para>
  ///     • <b>IR Builder</b> (<see cref="TTigerIR"/>) — constructs a
  ///     high-level intermediate representation of the program including
  ///     types, globals, functions, and statements.
  ///   </para>
  ///   <para>
  ///     • <b>Backend</b> (<see cref="TTigerBackend"/>) — translates
  ///     the IR into x86-64 machine code, links it into a PE/COFF
  ///     binary, and optionally launches executables.
  ///   </para>
  ///   <para><b>Typical Workflow</b></para>
  ///   <para>
  ///     1. Create a <c>TTiger</c> instance.<br/>
  ///     2. Declare imports with <see cref="ImportDll"/> or <see cref="ImportLib"/>.<br/>
  ///     3. Optionally define types with <see cref="DefineRecord"/>,
  ///        <see cref="DefineArray"/>, <see cref="DefineEnum"/>, etc.<br/>
  ///     4. Define globals with <see cref="Global"/>.<br/>
  ///     5. Define one or more functions using <see cref="Func"/> ...
  ///        <see cref="EndFunc"/>.<br/>
  ///     6. Set the output target with <see cref="TargetExe"/>,
  ///        <see cref="TargetDll"/>, <see cref="TargetLib"/>, or
  ///        <see cref="TargetObj"/>.<br/>
  ///     7. Call <see cref="Build"/> and inspect results with
  ///        <see cref="HasErrors"/>, <see cref="GetErrorText"/>, etc.
  ///   </para>
  ///   <para><b>Fluent Chaining</b></para>
  ///   <para>
  ///     Most methods return <c>Self</c> (the TTiger instance) to support
  ///     fluent chaining. Expression-building methods return
  ///     <see cref="TTigerExpr"/> handles instead, which can be composed
  ///     inline.
  ///   </para>
  ///   <para><b>Thread Safety</b></para>
  ///   <para>
  ///     A single <c>TTiger</c> instance is <b>not</b> thread-safe.
  ///     Do not share an instance across threads. It is safe to create
  ///     and use separate instances on different threads concurrently.
  ///   </para>
  ///   <para><b>Rebuild Support</b></para>
  ///   <para>
  ///     After a successful or failed build, you can call
  ///     <see cref="ResetBuild"/> to clear the backend and errors while
  ///     preserving the IR, then reconfigure the target and call
  ///     <see cref="Build"/> again — useful for producing multiple output
  ///     formats from the same program. Call <see cref="Reset"/> to
  ///     discard everything and start from scratch.
  ///   </para>
  ///   <code>
  ///     var
  ///       LTiger: TTiger;
  ///     begin
  ///       LTiger := TTiger.Create();
  ///       try
  ///         // 1. Imports
  ///         LTiger.ImportDll('msvcrt.dll', 'printf', [vtPointer], vtInt32, True);
  ///         LTiger.ImportDll('kernel32.dll', 'ExitProcess', [vtUInt32], vtVoid);
  ///
  ///         // 2. Define a function
  ///         LTiger
  ///           .Func('main', vtVoid, True)
  ///             .Local('n', vtInt64)
  ///             .Local('result', vtInt64)
  ///             .Assign('n', LTiger.Int64(5))
  ///             .Assign('result', LTiger.Int64(1))
  ///             .&While(LTiger.Gt(LTiger.Get('n'), LTiger.Int64(1)))
  ///               .Assign('result', LTiger.Mul(LTiger.Get('result'), LTiger.Get('n')))
  ///               .Assign('n', LTiger.Sub(LTiger.Get('n'), LTiger.Int64(1)))
  ///             .EndWhile()
  ///             .Call('printf', [LTiger.Str('5! = %d'#10), LTiger.Get('result')])
  ///             .Call('ExitProcess', [LTiger.Int64(0)])
  ///           .EndFunc();
  ///
  ///         // 3. Build
  ///         LTiger.TargetExe('output\factorial.exe');
  ///         if LTiger.Build() then
  ///           WriteLn('Build succeeded!')
  ///         else
  ///           WriteLn(LTiger.GetErrorText());
  ///       finally
  ///         LTiger.Free();
  ///       end;
  ///     end;
  ///   </code>
  /// </remarks>
  TTiger = class(TBaseObject)
  private
    FErrors: TErrors;
    FIR: TTigerIR;
    FBackend: TTigerBackend;
    FRuntime: TTigerRuntime;
    FStatus: TCallback<TStatusCallback>;

    // Version info
    FAddVersionInfo: Boolean;
    FVIMajor: Word;
    FVIMinor: Word;
    FVIPatch: Word;
    FVIProductName: string;
    FVIDescription: string;
    FVIFilename: string;
    FVICompanyName: string;
    FVICopyright: string;
    FExeIcon: string;

    procedure ApplyPostBuildResources(const AExePath: string);
  public

    /// <summary>
    ///   Create a new TTiger compiler instance with default settings.
    /// </summary>
    /// <remarks>
    ///   Initializes the error manager, IR builder, and backend. The
    ///   instance is ready to receive type definitions, imports, and
    ///   function declarations immediately after construction.
    /// </remarks>
    constructor Create(); override;

    /// <summary>
    ///   Destroy the compiler instance and release all owned resources.
    /// </summary>
    /// <remarks>
    ///   Frees the error manager, IR builder, and backend. Any
    ///   <see cref="TTigerExpr"/> handles obtained from this instance
    ///   become invalid after destruction.
    /// </remarks>
    destructor Destroy(); override;

    //==========================================================================
    // Target Configuration
    //==========================================================================

    /// <summary>
    ///   Set the build target to a Windows executable (.exe).
    /// </summary>
    /// <param name="APath">
    ///   Output file path for the executable. Intermediate directories are
    ///   <b>not</b> created automatically — ensure the directory exists.
    /// </param>
    /// <param name="ASubsystem">
    ///   PE subsystem flag. <c>ssConsole</c> (default) attaches a console
    ///   window; <c>ssGui</c> suppresses it.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Only one target may be active at a time. Calling any
    ///     <c>Target*</c> method replaces the previous target configuration.
    ///   </para>
    ///   <para>
    ///     The generated executable requires a function marked as the entry
    ///     point (<c>AIsEntryPoint = True</c> in <see cref="Func"/>).
    ///   </para>
    /// </remarks>
    /// <seealso cref="TargetDll"/>
    /// <seealso cref="TargetLib"/>
    /// <seealso cref="TargetObj"/>
    /// <seealso cref="Build"/>
    function TargetExe(
      const APath: string;
      const ASubsystem: TTigerSubsystem = ssConsole
    ): TTiger;

    /// <summary>
    ///   Set the build target to a dynamic link library (.dll).
    /// </summary>
    /// <param name="APath">Output file path for the DLL.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Functions intended for export should be declared with
    ///     <c>AIsPublic = True</c> and <c>plC</c> linkage in
    ///     <see cref="Func"/>. Optionally define a DLL entry point
    ///     via <see cref="DllMain"/>.
    ///   </para>
    /// </remarks>
    /// <seealso cref="TargetExe"/>
    /// <seealso cref="DllMain"/>
    function TargetDll(const APath: string): TTiger;

    /// <summary>
    ///   Set the build target to a static library (.lib) in COFF archive
    ///   format.
    /// </summary>
    /// <param name="APath">Output file path for the static library.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The resulting <c>.lib</c> can be linked by MSVC, LLD, or Tiger
    ///   itself via <see cref="AddLib"/>.
    /// </remarks>
    /// <seealso cref="TargetObj"/>
    /// <seealso cref="AddLib"/>
    function TargetLib(const APath: string): TTiger;

    /// <summary>
    ///   Set the build target to a COFF object file (.obj).
    /// </summary>
    /// <param name="APath">Output file path for the object file.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The resulting <c>.obj</c> is a relocatable COFF object suitable
    ///   for linking with external toolchains (MSVC <c>link.exe</c>,
    ///   LLD, etc.) or with Tiger via <see cref="AddObj"/>.
    /// </remarks>
    /// <seealso cref="TargetLib"/>
    /// <seealso cref="AddObj"/>
    function TargetObj(const APath: string): TTiger;

    //==========================================================================
    // Static Linking
    //==========================================================================

    /// <summary>
    ///   Add a static library (.lib) to link against during the build.
    /// </summary>
    /// <param name="APath">
    ///   Full or relative path to the <c>.lib</c> file. If only a filename
    ///   is given, the linker searches directories added via
    ///   <see cref="AddLibPath"/>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Multiple libraries can be added; they are searched in the order
    ///   they are added. This is used in conjunction with
    ///   <see cref="ImportLib"/> to resolve statically-linked function
    ///   references.
    /// </remarks>
    /// <seealso cref="AddObj"/>
    /// <seealso cref="AddLibPath"/>
    /// <seealso cref="ImportLib"/>
    function AddLib(const APath: string): TTiger;

    /// <summary>
    ///   Add a COFF object file (.obj) to link against during the build.
    /// </summary>
    /// <param name="APath">Path to the <c>.obj</c> file.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="AddLib"/>
    function AddObj(const APath: string): TTiger;

    /// <summary>
    ///   Add a directory to the library search path used during linking.
    /// </summary>
    /// <param name="APath">Directory path to search for <c>.lib</c> files.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Directories are searched in the order they are added. This affects
    ///   resolution of libraries specified by filename only (no directory
    ///   component) in <see cref="AddLib"/>.
    /// </remarks>
    /// <seealso cref="AddLib"/>
    function AddLibPath(const APath: string): TTiger;

    //==========================================================================
    // Build & Lifecycle
    //==========================================================================

    /// <summary>
    ///   Compile and link the program, producing the target binary.
    /// </summary>
    /// <param name="AAutoRun">
    ///   If <c>True</c> (default), the compiled executable is launched
    ///   automatically after a successful build. Set to <c>False</c> for
    ///   batch or CI workflows.
    /// </param>
    /// <param name="AExitCode">
    ///   Optional pointer to a <c>Cardinal</c> that receives the process
    ///   exit code when <c>AAutoRun</c> is <c>True</c> and the executable
    ///   runs successfully. Pass <c>nil</c> (default) to ignore the exit
    ///   code. Not meaningful when <c>AAutoRun</c> is <c>False</c> or the
    ///   output type is not an executable.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the build succeeded without errors; <c>False</c>
    ///   otherwise.
    /// </returns>
    /// <remarks>
    ///   <para>
    ///     Internally performs the following steps: adds the Tiger runtime
    ///     library to the IR, emits all IR declarations and functions to
    ///     the backend, runs the configured optimization passes, assembles
    ///     machine code, and links the final binary.
    ///   </para>
    ///   <para>
    ///     After <c>Build</c> returns, inspect diagnostics with
    ///     <see cref="HasErrors"/>, <see cref="GetErrorText"/>, or
    ///     <see cref="GetErrorItems"/>. If <c>AAutoRun</c> is <c>True</c>
    ///     and the build succeeds, the resulting executable is launched and
    ///     its exit code is written to <c>AExitCode^</c> when the pointer
    ///     is non-nil.
    ///   </para>
    ///   <para>
    ///     If a status callback is set via <see cref="SetStatusCallback"/>,
    ///     progress messages are delivered synchronously during this call.
    ///   </para>
    /// </remarks>
    /// <seealso cref="BuildToMemory"/>
    /// <seealso cref="Reset"/>
    /// <seealso cref="ResetBuild"/>
    function Build(const AAutoRun: Boolean = True; AExitCode: PCardinal = nil): Boolean;

    /// <summary>
    ///   Compile and link to an in-memory byte array instead of writing
    ///   to a file.
    /// </summary>
    /// <returns>
    ///   The compiled PE/COFF binary as a <c>TBytes</c> array, or an
    ///   empty array if compilation failed.
    /// </returns>
    /// <remarks>
    ///   Useful for JIT-style scenarios, testing, or when the binary
    ///   needs to be written to a custom stream or network destination
    ///   rather than the filesystem.
    /// </remarks>
    /// <seealso cref="Build"/>
    function BuildToMemory(): TBytes;

    /// <summary>
    ///   Reset the entire compiler state — clears all IR, backend data,
    ///   and diagnostics.
    /// </summary>
    /// <remarks>
    ///   After calling <c>Reset</c>, the instance is in the same state
    ///   as a freshly constructed <c>TTiger</c>. Use this to compile a
    ///   completely different program without creating a new instance.
    /// </remarks>
    /// <seealso cref="ResetBuild"/>
    procedure Reset();

    /// <summary>
    ///   Reset only the backend and error state, preserving the IR.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     Call this to rebuild the same program with different settings
    ///     — for example, changing the optimization level, switching
    ///     between <c>TargetExe</c> and <c>TargetDll</c>, or producing
    ///     both debug and release binaries from the same IR.
    ///   </para>
    ///   <para>
    ///     After <c>ResetBuild</c>, reconfigure the target and call
    ///     <see cref="Build"/> again. All type definitions, imports,
    ///     globals, and functions remain intact.
    ///   </para>
    /// </remarks>
    /// <seealso cref="Reset"/>
    /// <seealso cref="Build"/>
    procedure ResetBuild();

    //==========================================================================
    // Optimization & Diagnostics
    //==========================================================================

    /// <summary>
    ///   Set the optimization level for the SSA/backend pipeline.
    /// </summary>
    /// <param name="AValue">
    ///   Optimization level: <c>0</c> = no optimization (fastest compile,
    ///   easiest to debug), <c>1</c> = basic optimizations (constant
    ///   folding, copy propagation, dead code elimination),
    ///   <c>2</c> = full optimizations (includes CSE and additional
    ///   backend passes).
    /// </param>
    /// <remarks>
    ///   Must be set before calling <see cref="Build"/>. The default
    ///   level is <c>0</c>. Values outside the range 0..2 are clamped.
    /// </remarks>
    /// <seealso cref="GetOptimizationLevel"/>
    procedure SetOptimizationLevel(const AValue: Integer);

    /// <summary>
    ///   Get the current optimization level.
    /// </summary>
    /// <returns>Current optimization level (0, 1, or 2).</returns>
    /// <seealso cref="SetOptimizationLevel"/>
    function GetOptimizationLevel(): Integer;

    /// <summary>
    ///   Enable or disable SSA/IR dump output for debugging purposes.
    /// </summary>
    /// <param name="AValue">
    ///   <c>True</c> to enable IR dumping; <c>False</c> to disable.
    /// </param>
    /// <remarks>
    ///   When enabled, the SSA intermediate form is captured during the
    ///   build and can be retrieved afterwards via <see cref="GetSSADump"/>.
    ///   This is useful for diagnosing optimization behavior or verifying
    ///   correct IR generation.
    /// </remarks>
    /// <seealso cref="GetDumpIR"/>
    /// <seealso cref="GetSSADump"/>
    procedure SetDumpIR(const AValue: Boolean);

    /// <summary>
    ///   Get whether SSA/IR dumping is currently enabled.
    /// </summary>
    /// <returns><c>True</c> if IR dumping is enabled.</returns>
    /// <seealso cref="SetDumpIR"/>
    /// <seealso cref="GetSSADump"/>
    function GetDumpIR(): Boolean;

    /// <summary>
    ///   Retrieve the SSA dump text from the most recent build.
    /// </summary>
    /// <returns>
    ///   The SSA dump as a multi-line string, or an empty string if
    ///   dumping was not enabled or no build has been performed.
    /// </returns>
    /// <remarks>
    ///   Call <see cref="SetDumpIR"/> with <c>True</c> before
    ///   <see cref="Build"/> to populate the dump.
    /// </remarks>
    /// <seealso cref="SetDumpIR"/>
    function GetSSADump(): string;

    //==========================================================================
    // Status Callback
    //==========================================================================

    /// <summary>
    ///   Register a callback to receive progress and status messages
    ///   during compilation.
    /// </summary>
    /// <param name="ACallback">
    ///   A <see cref="TStatusCallback"/> procedure reference that receives
    ///   a status message string and the user-data pointer. Pass
    ///   <c>nil</c> to remove a previously registered callback.
    /// </param>
    /// <param name="AUserData">
    ///   An optional opaque pointer forwarded to every callback invocation.
    ///   Defaults to <c>nil</c>.
    /// </param>
    /// <remarks>
    ///   The callback is invoked synchronously on the calling thread
    ///   during <see cref="Build"/>. Typical messages include
    ///   <c>"Emitting function main"</c>, <c>"Running optimizer"</c>,
    ///   and <c>"Linking..."</c>.
    /// </remarks>
    procedure SetStatusCallback(
      const ACallback: TStatusCallback;
      const AUserData: Pointer = nil
    );

    //==========================================================================
    // Error Management
    //==========================================================================

    /// <summary>
    ///   Set the maximum number of errors before compilation stops.
    /// </summary>
    /// <param name="AValue">
    ///   Maximum error count. When this many errors (severity
    ///   <c>esError</c> or <c>esFatal</c>) have been recorded, the
    ///   compiler stops emitting further code.
    /// </param>
    /// <remarks>
    ///   The default limit is implementation-defined. Setting a higher
    ///   value allows the compiler to report more issues in a single
    ///   pass, which can be useful for batch diagnostics.
    /// </remarks>
    /// <seealso cref="GetMaxErrors"/>
    /// <seealso cref="HasErrors"/>
    procedure SetMaxErrors(const AValue: Integer);

    /// <summary>
    ///   Get the current maximum error count.
    /// </summary>
    /// <returns>The maximum number of errors allowed before compilation halts.</returns>
    /// <seealso cref="SetMaxErrors"/>
    function GetMaxErrors(): Integer;

    /// <summary>
    ///   Check if any errors (severity <c>esError</c> or <c>esFatal</c>)
    ///   were recorded during the last build.
    /// </summary>
    /// <returns><c>True</c> if one or more errors exist.</returns>
    /// <seealso cref="ErrorCount"/>
    /// <seealso cref="HasFatal"/>
    /// <seealso cref="GetErrorText"/>
    function HasErrors(): Boolean;

    /// <summary>
    ///   Check if any warnings (severity <c>esWarning</c>) were recorded.
    /// </summary>
    /// <returns><c>True</c> if one or more warnings exist.</returns>
    /// <seealso cref="WarningCount"/>
    function HasWarnings(): Boolean;

    /// <summary>
    ///   Check if any hints (severity <c>esHint</c>) were recorded.
    /// </summary>
    /// <returns><c>True</c> if one or more hints exist.</returns>
    function HasHints(): Boolean;

    /// <summary>
    ///   Check if a fatal error (severity <c>esFatal</c>) occurred,
    ///   indicating that compilation was aborted early.
    /// </summary>
    /// <returns><c>True</c> if a fatal error exists.</returns>
    /// <seealso cref="HasErrors"/>
    function HasFatal(): Boolean;

    /// <summary>
    ///   Get the total number of errors (severity <c>esError</c> or
    ///   <c>esFatal</c>) recorded during the last build.
    /// </summary>
    /// <returns>Number of errors.</returns>
    /// <seealso cref="HasErrors"/>
    /// <seealso cref="WarningCount"/>
    function ErrorCount(): Integer;

    /// <summary>
    ///   Get the total number of warnings (severity <c>esWarning</c>)
    ///   recorded during the last build.
    /// </summary>
    /// <returns>Number of warnings.</returns>
    /// <seealso cref="HasWarnings"/>
    /// <seealso cref="ErrorCount"/>
    function WarningCount(): Integer;

    /// <summary>
    ///   Get the raw list of all diagnostic records from the last build.
    /// </summary>
    /// <returns>
    ///   A <c>TList&lt;TError&gt;</c> containing every diagnostic
    ///   (hints, warnings, errors, and fatals) in the order they were
    ///   recorded. The list is owned by the compiler instance — do not
    ///   free it.
    /// </returns>
    /// <remarks>
    ///   Iterate this list for fine-grained inspection of individual
    ///   diagnostics. For a simple formatted dump, use
    ///   <see cref="GetErrorText"/> instead.
    /// </remarks>
    /// <seealso cref="GetErrorText"/>
    /// <seealso cref="TError"/>
    function GetErrorItems(): TList<TError>;

    /// <summary>
    ///   Get a formatted, multi-line string containing all diagnostics
    ///   from the last build.
    /// </summary>
    /// <returns>
    ///   A single string with one diagnostic per line, suitable for
    ///   logging, display in a memo control, or writing to a file.
    ///   Returns an empty string if no diagnostics were recorded.
    /// </returns>
    /// <seealso cref="GetErrorItems"/>
    /// <seealso cref="HasErrors"/>
    function GetErrorText(): string;

    //==========================================================================
    // Imports — Dynamic (DLL) and Static (Lib)
    //==========================================================================

    /// <summary>
    ///   Declare a function imported from a DLL (dynamic linking).
    /// </summary>
    /// <param name="ADllName">
    ///   Name of the DLL containing the function (e.g. <c>'msvcrt.dll'</c>,
    ///   <c>'kernel32.dll'</c>). The <c>.dll</c> extension is required.
    /// </param>
    /// <param name="AFuncName">
    ///   Name of the exported function exactly as it appears in the DLL
    ///   export table (e.g. <c>'printf'</c>, <c>'ExitProcess'</c>).
    /// </param>
    /// <param name="AParams">
    ///   Open array of <see cref="TTigerValueType"/> values describing
    ///   the function's fixed parameter types, in order.
    /// </param>
    /// <param name="AReturn">
    ///   Return type of the function. Defaults to <c>vtVoid</c> for
    ///   procedures.
    /// </param>
    /// <param name="AVarArgs">
    ///   Set to <c>True</c> if the function accepts a variable number
    ///   of arguments after the fixed parameters (e.g. <c>printf</c>).
    ///   Defaults to <c>False</c>.
    /// </param>
    /// <param name="ALinkage">
    ///   Linkage specification. Defaults to <c>plC</c> (no name mangling),
    ///   which is correct for virtually all DLL imports.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     This creates an entry in the PE import table. At load time,
    ///     the Windows loader resolves the function address from the
    ///     specified DLL. Call the imported function by name from any
    ///     Tiger function using <see cref="Call"/> or
    ///     <see cref="Invoke"/>.
    ///   </para>
    /// </remarks>
    /// <seealso cref="ImportLib"/>
    /// <seealso cref="Call"/>
    /// <seealso cref="Invoke"/>
    function ImportDll(
      const ADllName: string;
      const AFuncName: string;
      const AParams: array of TTigerValueType;
      const AReturn: TTigerValueType = vtVoid;
      const AVarArgs: Boolean = False;
      const ALinkage: TTigerLinkage = plC
    ): TTiger;

    /// <summary>
    ///   Declare a function imported from a static library (.lib).
    /// </summary>
    /// <param name="ALibName">
    ///   Name of the library, without the <c>.lib</c> extension
    ///   (e.g. <c>'mymath'</c>). The library must be added via
    ///   <see cref="AddLib"/> or found on a path added with
    ///   <see cref="AddLibPath"/>.
    /// </param>
    /// <param name="AFuncName">
    ///   Symbol name of the function as it appears in the library.
    /// </param>
    /// <param name="AParams">
    ///   Open array of parameter types, in order.
    /// </param>
    /// <param name="AReturn">
    ///   Return type. Defaults to <c>vtVoid</c>.
    /// </param>
    /// <param name="AVarArgs">
    ///   <c>True</c> for variadic functions. Defaults to <c>False</c>.
    /// </param>
    /// <param name="ALinkage">
    ///   Linkage specification. Defaults to <c>plC</c>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Unlike <see cref="ImportDll"/>, statically imported functions are
    ///   resolved at link time and their code is included in the output
    ///   binary. The corresponding <c>.lib</c> file must be available
    ///   during the build.
    /// </remarks>
    /// <seealso cref="ImportDll"/>
    /// <seealso cref="AddLib"/>
    /// <seealso cref="AddLibPath"/>
    function ImportLib(
      const ALibName: string;
      const AFuncName: string;
      const AParams: array of TTigerValueType;
      const AReturn: TTigerValueType = vtVoid;
      const AVarArgs: Boolean = False;
      const ALinkage: TTigerLinkage = plC
    ): TTiger;

    //==========================================================================
    // Global Variables
    //==========================================================================

    /// <summary>
    ///   Declare a global variable with a primitive type, zero-initialized.
    /// </summary>
    /// <param name="AName">
    ///   Unique name for the global variable. Must not conflict with
    ///   function names or other globals.
    /// </param>
    /// <param name="AType">Primitive value type for the variable.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The variable is accessible from all functions via
    ///   <see cref="Get"/>, <see cref="Assign"/>, <see cref="AddrOf"/>,
    ///   etc. Its initial value is zero/nil.
    /// </remarks>
    /// <seealso cref="Local"/>
    /// <seealso cref="Get"/>
    function Global(const AName: string; const AType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Declare a global variable with a primitive type and an explicit
    ///   initial value expression.
    /// </summary>
    /// <param name="AName">Unique name for the global variable.</param>
    /// <param name="AType">Primitive value type.</param>
    /// <param name="AInit">
    ///   Initial value expression, typically a literal such as
    ///   <see cref="Int64"/> or <see cref="Float64"/>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    function Global(const AName: string; const AType: TTigerValueType; const AInit: TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Declare a global variable with a composite type reference.
    /// </summary>
    /// <param name="AName">Unique name for the global variable.</param>
    /// <param name="ATypeRef">
    ///   A <see cref="TTigerTypeRef"/> obtained from <see cref="TypeRef"/>
    ///   or returned by type definition methods.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    function Global(const AName: string; const ATypeRef: TTigerTypeRef): TTiger; overload;

    /// <summary>
    ///   Declare a global variable with a previously defined named type.
    /// </summary>
    /// <param name="AName">Unique name for the global variable.</param>
    /// <param name="ATypeName">
    ///   Name of a type previously defined with <see cref="DefineRecord"/>,
    ///   <see cref="DefineArray"/>, <see cref="DefineEnum"/>, etc.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    function Global(const AName: string; const ATypeName: string): TTiger; overload;

    //==========================================================================
    // Type Definitions — Records
    //==========================================================================

    /// <summary>
    ///   Begin defining a named record (struct) type.
    /// </summary>
    /// <param name="AName">
    ///   Unique type name. Use this name in <see cref="Field"/>,
    ///   <see cref="Global"/>, <see cref="Local"/>, and
    ///   <see cref="TypeRef"/> to reference the type.
    /// </param>
    /// <param name="AIsPacked">
    ///   If <c>True</c>, fields are laid out without padding (packed
    ///   layout). Defaults to <c>False</c> (natural alignment with
    ///   padding).
    /// </param>
    /// <param name="AExplicitAlign">
    ///   Override the alignment in bytes. <c>0</c> (default) uses natural
    ///   alignment derived from the largest field.
    /// </param>
    /// <param name="ABaseTypeName">
    ///   Optional name of a base record type for single-inheritance.
    ///   The new record begins with all fields of the base type. Empty
    ///   string (default) means no inheritance.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     After calling <c>DefineRecord</c>, add fields with
    ///     <see cref="Field"/> and <see cref="BitField"/>. Nested
    ///     anonymous unions can be embedded with <see cref="BeginUnion"/>
    ///     / <see cref="EndUnion"/>. Finalize the definition with
    ///     <see cref="EndRecord"/>.
    ///   </para>
    ///   <para>
    ///     Record definitions must be completed (via <c>EndRecord</c>)
    ///     before starting another type definition or function.
    ///   </para>
    /// </remarks>
    /// <seealso cref="Field"/>
    /// <seealso cref="BitField"/>
    /// <seealso cref="EndRecord"/>
    /// <seealso cref="DefineUnion"/>
    function DefineRecord(
      const AName: string;
      const AIsPacked: Boolean = False;
      const AExplicitAlign: Integer = 0;
      const ABaseTypeName: string = ''
    ): TTiger;

    /// <summary>
    ///   Begin an anonymous record nested inside a union definition.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Used to group multiple fields that share the same union variant.
    ///   Must be paired with <see cref="EndRecord"/>.
    /// </remarks>
    /// <seealso cref="DefineUnion"/>
    /// <seealso cref="EndRecord"/>
    function BeginRecord(): TTiger;

    /// <summary>
    ///   Add a field with a primitive type to the current record or union.
    /// </summary>
    /// <param name="AName">Field name, unique within the enclosing type.</param>
    /// <param name="AType">Primitive value type for the field.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Access this field at runtime via <see cref="GetField"/>.
    /// </remarks>
    /// <seealso cref="GetField"/>
    /// <seealso cref="BitField"/>
    function Field(const AName: string; const AType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Add a field with a previously defined named type to the current
    ///   record or union.
    /// </summary>
    /// <param name="AName">Field name, unique within the enclosing type.</param>
    /// <param name="ATypeName">
    ///   Name of a previously defined type (record, array, enum, alias, etc.).
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="GetField"/>
    function Field(const AName: string; const ATypeName: string): TTiger; overload;

    /// <summary>
    ///   Add a bit field to the current record definition.
    /// </summary>
    /// <param name="AName">Field name.</param>
    /// <param name="AType">
    ///   Underlying integer type that determines the storage unit
    ///   (e.g. <c>vtUInt32</c> for a 32-bit storage unit).
    /// </param>
    /// <param name="ABitWidth">
    ///   Number of bits this field occupies within the storage unit.
    ///   Must be in the range 1..sizeof(AType)*8.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Consecutive bit fields sharing the same underlying type are
    ///   packed into a single storage unit where possible, following
    ///   MSVC bit field layout rules for x86-64.
    /// </remarks>
    function BitField(const AName: string; const AType: TTigerValueType; const ABitWidth: Integer): TTiger;

    /// <summary>
    ///   End the current record definition and register the type.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   After this call, the record type is available for use in
    ///   <see cref="Global"/>, <see cref="Local"/>, <see cref="Field"/>,
    ///   <see cref="TypeRef"/>, and <see cref="DefinePointer"/>
    ///   declarations.
    /// </remarks>
    /// <seealso cref="DefineRecord"/>
    /// <seealso cref="BeginRecord"/>
    function EndRecord(): TTiger;

    //==========================================================================
    // Type Definitions — Unions
    //==========================================================================

    /// <summary>
    ///   Begin defining a named union type where all fields share the
    ///   same memory location.
    /// </summary>
    /// <param name="AName">Unique type name for the union.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Add variants with <see cref="Field"/>. The union's size equals
    ///     the size of its largest variant. Anonymous records can be
    ///     nested inside with <see cref="BeginRecord"/> /
    ///     <see cref="EndRecord"/>. Finalize with <see cref="EndUnion"/>.
    ///   </para>
    /// </remarks>
    /// <seealso cref="BeginUnion"/>
    /// <seealso cref="EndUnion"/>
    /// <seealso cref="DefineRecord"/>
    function DefineUnion(const AName: string): TTiger;

    /// <summary>
    ///   Begin an anonymous union nested inside a record definition.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Used to embed variant (overlapping) fields within a record.
    ///   Must be paired with <see cref="EndUnion"/>.
    /// </remarks>
    /// <seealso cref="DefineRecord"/>
    /// <seealso cref="EndUnion"/>
    function BeginUnion(): TTiger;

    /// <summary>
    ///   End the current union definition and register the type.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineUnion"/>
    /// <seealso cref="BeginUnion"/>
    function EndUnion(): TTiger;

    //==========================================================================
    // Type Definitions — Arrays
    //==========================================================================

    /// <summary>
    ///   Define a fixed-size array type with a primitive element type.
    /// </summary>
    /// <param name="AName">Unique type name for the array.</param>
    /// <param name="AElementType">Primitive type of each element.</param>
    /// <param name="ALowBound">Inclusive lower bound of the array index.</param>
    /// <param name="AHighBound">Inclusive upper bound of the array index.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The array contains <c>AHighBound - ALowBound + 1</c> elements.
    ///   Access elements at runtime via <see cref="GetIndex"/>.
    /// </remarks>
    /// <seealso cref="GetIndex"/>
    /// <seealso cref="DefineDynArray"/>
    /// <seealso cref="High"/>
    /// <seealso cref="Low"/>
    /// <seealso cref="Len"/>
    function DefineArray(const AName: string; const AElementType: TTigerValueType;
      const ALowBound: Integer; const AHighBound: Integer): TTiger; overload;

    /// <summary>
    ///   Define a fixed-size array type with a named element type.
    /// </summary>
    /// <param name="AName">Unique type name for the array.</param>
    /// <param name="AElementTypeName">
    ///   Name of a previously defined type for each element.
    /// </param>
    /// <param name="ALowBound">Inclusive lower bound of the array index.</param>
    /// <param name="AHighBound">Inclusive upper bound of the array index.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="GetIndex"/>
    function DefineArray(const AName: string; const AElementTypeName: string;
      const ALowBound: Integer; const AHighBound: Integer): TTiger; overload;

    /// <summary>
    ///   Define a dynamic array type with a primitive element type.
    /// </summary>
    /// <param name="AName">Unique type name for the dynamic array.</param>
    /// <param name="AElementType">Primitive type of each element.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Dynamic arrays are managed at runtime and can grow or shrink.
    ///   The array is represented internally as a pointer to a
    ///   heap-allocated block with a length prefix.
    /// </remarks>
    /// <seealso cref="DefineArray"/>
    function DefineDynArray(const AName: string; const AElementType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Define a dynamic array type with a named element type.
    /// </summary>
    /// <param name="AName">Unique type name for the dynamic array.</param>
    /// <param name="AElementTypeName">
    ///   Name of a previously defined type for each element.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineArray"/>
    function DefineDynArray(const AName: string; const AElementTypeName: string): TTiger; overload;

    //==========================================================================
    // Type Definitions — Enumerations
    //==========================================================================

    /// <summary>
    ///   Begin defining an enumeration type.
    /// </summary>
    /// <param name="AName">Unique type name for the enumeration.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Add values with <see cref="EnumValue"/> (auto-incrementing
    ///     ordinals starting at 0) or the explicit ordinal overload.
    ///     Finalize with <see cref="EndEnum"/>.
    ///   </para>
    ///   <para>
    ///     Enum values can be used with <see cref="Ord"/>,
    ///     <see cref="Succ"/>, <see cref="Pred"/>, and as
    ///     <see cref="CaseOf"/> selectors. Enum types can also serve
    ///     as the basis for <see cref="DefineSet"/>.
    ///   </para>
    /// </remarks>
    /// <seealso cref="EnumValue"/>
    /// <seealso cref="EndEnum"/>
    /// <seealso cref="DefineSet"/>
    function DefineEnum(const AName: string): TTiger;

    /// <summary>
    ///   Add an enum value with auto-incrementing ordinal.
    /// </summary>
    /// <param name="AName">
    ///   Symbolic name for the value (e.g. <c>'clRed'</c>). Must be
    ///   unique within the enumeration.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The ordinal is 0 for the first value, then increments by 1
    ///   for each subsequent call.
    /// </remarks>
    /// <seealso cref="DefineEnum"/>
    function EnumValue(const AName: string): TTiger; overload;

    /// <summary>
    ///   Add an enum value with an explicit ordinal.
    /// </summary>
    /// <param name="AName">Symbolic name for the value.</param>
    /// <param name="AOrdinal">
    ///   Explicit ordinal value. Subsequent auto-incrementing values
    ///   continue from <c>AOrdinal + 1</c>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineEnum"/>
    function EnumValue(const AName: string; const AOrdinal: Int64): TTiger; overload;

    /// <summary>
    ///   End the current enumeration definition and register the type.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineEnum"/>
    /// <seealso cref="EnumValue"/>
    function EndEnum(): TTiger;

    //==========================================================================
    // Type Definitions — Aliases, Pointers, Routines, Sets
    //==========================================================================

    /// <summary>
    ///   Define a type alias for a primitive type.
    /// </summary>
    /// <param name="AName">Unique alias name.</param>
    /// <param name="AType">The primitive type being aliased.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Creates a named synonym for the underlying type. Useful for
    ///   improving code readability (e.g. aliasing <c>vtInt32</c> as
    ///   <c>'HRESULT'</c>).
    /// </remarks>
    function DefineAlias(const AName: string; const AType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Define a type alias for a previously defined named type.
    /// </summary>
    /// <param name="AName">Unique alias name.</param>
    /// <param name="ATypeName">Name of the type being aliased.</param>
    /// <returns>Self for fluent chaining.</returns>
    function DefineAlias(const AName: string; const ATypeName: string): TTiger; overload;

    /// <summary>
    ///   Define an untyped (void) pointer type.
    /// </summary>
    /// <param name="AName">Unique type name for the pointer.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   An untyped pointer is equivalent to C's <c>void*</c>. Dereference
    ///   requires an explicit type via <see cref="Deref"/>.
    /// </remarks>
    /// <seealso cref="Deref"/>
    /// <seealso cref="AddrOf"/>
    function DefinePointer(const AName: string): TTiger; overload;

    /// <summary>
    ///   Define a typed pointer to a primitive type.
    /// </summary>
    /// <param name="AName">Unique type name for the pointer.</param>
    /// <param name="APointeeType">The primitive type being pointed to.</param>
    /// <param name="AIsConst">
    ///   If <c>True</c>, marks the pointer as pointing to read-only data.
    ///   Defaults to <c>False</c>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Deref"/>
    /// <seealso cref="AddrOf"/>
    function DefinePointer(const AName: string; const APointeeType: TTigerValueType;
      const AIsConst: Boolean = False): TTiger; overload;

    /// <summary>
    ///   Define a typed pointer to a previously defined named type.
    /// </summary>
    /// <param name="AName">Unique type name for the pointer.</param>
    /// <param name="APointeeTypeName">Name of the type being pointed to.</param>
    /// <param name="AIsConst">
    ///   If <c>True</c>, marks the pointer as pointing to read-only data.
    ///   Defaults to <c>False</c>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Deref"/>
    /// <seealso cref="AddrOf"/>
    function DefinePointer(const AName: string; const APointeeTypeName: string;
      const AIsConst: Boolean = False): TTiger; overload;

    /// <summary>
    ///   Begin defining a routine (function pointer / procedural) type.
    /// </summary>
    /// <param name="AName">Unique type name for the routine type.</param>
    /// <param name="ALinkage">
    ///   Linkage specification. Defaults to <c>plDefault</c> (Itanium
    ///   mangling). Use <c>plC</c> for C-compatible callback types.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     After calling <c>DefineRoutine</c>, add parameters with
    ///     <see cref="RoutineParam"/>, set the return type with
    ///     <see cref="RoutineReturns"/>, optionally mark as variadic
    ///     with <see cref="RoutineVarArgs"/>, and finalize with
    ///     <see cref="EndRoutine"/>.
    ///   </para>
    ///   <para>
    ///     Use the resulting type name to declare variables or fields
    ///     that hold function pointers. Call through them using
    ///     <see cref="CallIndirect"/> or <see cref="InvokeIndirect"/>.
    ///   </para>
    /// </remarks>
    /// <seealso cref="RoutineParam"/>
    /// <seealso cref="RoutineReturns"/>
    /// <seealso cref="EndRoutine"/>
    /// <seealso cref="FuncAddr"/>
    /// <seealso cref="CallIndirect"/>
    function DefineRoutine(const AName: string; const ALinkage: TTigerLinkage = plDefault): TTiger;

    /// <summary>
    ///   Add a parameter with a primitive type to the current routine type
    ///   definition.
    /// </summary>
    /// <param name="AType">Primitive type for the parameter.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineRoutine"/>
    function RoutineParam(const AType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Add a parameter with a named type to the current routine type
    ///   definition.
    /// </summary>
    /// <param name="ATypeName">Name of a previously defined type.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineRoutine"/>
    function RoutineParam(const ATypeName: string): TTiger; overload;

    /// <summary>
    ///   Set the return type of the current routine type to a primitive.
    /// </summary>
    /// <param name="AType">Primitive return type (use <c>vtVoid</c> for procedures).</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineRoutine"/>
    function RoutineReturns(const AType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Set the return type of the current routine type to a named type.
    /// </summary>
    /// <param name="ATypeName">Name of a previously defined type.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineRoutine"/>
    function RoutineReturns(const ATypeName: string): TTiger; overload;

    /// <summary>
    ///   Mark the current routine type as accepting variadic arguments.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Must be called after all fixed parameters have been added
    ///   via <see cref="RoutineParam"/>. Variadic routine types follow
    ///   the C calling convention for variable arguments.
    /// </remarks>
    /// <seealso cref="DefineRoutine"/>
    function RoutineVarArgs(): TTiger;

    /// <summary>
    ///   End the current routine type definition and register the type.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineRoutine"/>
    function EndRoutine(): TTiger;

    /// <summary>
    ///   Define a set type with the default range (0..255).
    /// </summary>
    /// <param name="AName">Unique type name for the set.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Sets are implemented as bit vectors. The default range supports
    ///   up to 256 elements, stored in 32 bytes.
    /// </remarks>
    /// <seealso cref="SetLit"/>
    /// <seealso cref="SetIn"/>
    /// <seealso cref="SetUnion"/>
    function DefineSet(const AName: string): TTiger; overload;

    /// <summary>
    ///   Define a set type with an explicit integer range.
    /// </summary>
    /// <param name="AName">Unique type name for the set.</param>
    /// <param name="ALow">Inclusive lower bound of valid set elements.</param>
    /// <param name="AHigh">Inclusive upper bound of valid set elements.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="SetLit"/>
    /// <seealso cref="SetIn"/>
    function DefineSet(const AName: string; const ALow: Integer; const AHigh: Integer): TTiger; overload;

    /// <summary>
    ///   Define a set type based on an enumeration type.
    /// </summary>
    /// <param name="AName">Unique type name for the set.</param>
    /// <param name="AEnumTypeName">
    ///   Name of a previously defined enumeration. The set's range is
    ///   derived from the enum's ordinal bounds.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="DefineEnum"/>
    /// <seealso cref="SetLit"/>
    function DefineSet(const AName: string; const AEnumTypeName: string): TTiger; overload;

    //==========================================================================
    // Type Queries
    //==========================================================================

    /// <summary>
    ///   Look up a type by name and return its internal index.
    /// </summary>
    /// <param name="AName">Name of the type to find.</param>
    /// <returns>
    ///   The type index (>= 0) if found, or <c>-1</c> if no type with
    ///   the given name exists.
    /// </returns>
    /// <seealso cref="TypeRef"/>
    /// <seealso cref="GetTypeSize"/>
    function FindType(const AName: string): Integer;

    /// <summary>
    ///   Get the size in bytes of a type reference.
    /// </summary>
    /// <param name="ATypeRef">
    ///   A <see cref="TTigerTypeRef"/> obtained from <see cref="TypeRef"/>.
    /// </param>
    /// <returns>Size in bytes, accounting for padding and alignment.</returns>
    /// <seealso cref="GetTypeAlignment"/>
    /// <seealso cref="&SizeOf"/>
    function GetTypeSize(const ATypeRef: TTigerTypeRef): Integer;

    /// <summary>
    ///   Get the required alignment in bytes of a type reference.
    /// </summary>
    /// <param name="ATypeRef">
    ///   A <see cref="TTigerTypeRef"/> obtained from <see cref="TypeRef"/>.
    /// </param>
    /// <returns>Alignment in bytes.</returns>
    /// <seealso cref="GetTypeSize"/>
    /// <seealso cref="AlignOf"/>
    function GetTypeAlignment(const ATypeRef: TTigerTypeRef): Integer;

    /// <summary>
    ///   Get a type reference by name, for use in <see cref="Global"/> or
    ///   <see cref="Local"/> declarations that accept
    ///   <see cref="TTigerTypeRef"/>.
    /// </summary>
    /// <param name="AName">Name of a previously defined type.</param>
    /// <returns>
    ///   A <see cref="TTigerTypeRef"/> representing the named type.
    /// </returns>
    /// <seealso cref="FindType"/>
    /// <seealso cref="GetTypeSize"/>
    function TypeRef(const AName: string): TTigerTypeRef;

    //==========================================================================
    // Function Definition
    //==========================================================================

    /// <summary>
    ///   Begin defining a function. All subsequent <see cref="Param"/>,
    ///   <see cref="Local"/>, and statement calls apply to this function
    ///   until <see cref="EndFunc"/> is called.
    /// </summary>
    /// <param name="AName">
    ///   Unique function name. This is the name used by <see cref="Call"/>,
    ///   <see cref="Invoke"/>, and <see cref="FuncAddr"/>.
    /// </param>
    /// <param name="AReturnType">
    ///   Primitive return type. Defaults to <c>vtVoid</c> (procedure).
    /// </param>
    /// <param name="AIsEntryPoint">
    ///   If <c>True</c>, marks this function as the program entry point.
    ///   Exactly one function must be marked as the entry point for
    ///   executable targets. Defaults to <c>False</c>.
    /// </param>
    /// <param name="ALinkage">
    ///   Linkage specification. <c>plDefault</c> (default) applies
    ///   Itanium mangling. Use <c>plC</c> for functions exported from
    ///   DLLs or called from C.
    /// </param>
    /// <param name="AIsPublic">
    ///   If <c>True</c>, the function symbol is exported (visible in DLL
    ///   exports or library symbols). Defaults to <c>False</c>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Function definitions cannot be nested — you must call
    ///     <see cref="EndFunc"/> before starting another function.
    ///   </para>
    ///   <para>
    ///     Within the function body, declare parameters with
    ///     <see cref="Param"/>, local variables with <see cref="Local"/>,
    ///     then emit statements (assignments, calls, control flow).
    ///     Expressions are built using methods like <see cref="Int64"/>,
    ///     <see cref="Get"/>, <see cref="Add"/>, etc.
    ///   </para>
    /// </remarks>
    /// <seealso cref="EndFunc"/>
    /// <seealso cref="Param"/>
    /// <seealso cref="Local"/>
    /// <seealso cref="OverloadFunc"/>
    /// <seealso cref="VariadicFunc"/>
    function Func(const AName: string;
      const AReturnType: TTigerValueType = vtVoid;
      const AIsEntryPoint: Boolean = False;
      const ALinkage: TTigerLinkage = plDefault;
      const AIsPublic: Boolean = False): TTiger;

    /// <summary>
    ///   Begin defining an overloaded function that uses Itanium C++
    ///   name mangling to encode parameter types into the symbol.
    /// </summary>
    /// <param name="AName">
    ///   Function name. Multiple functions may share the same name if
    ///   their parameter signatures differ.
    /// </param>
    /// <param name="AReturnType">
    ///   Primitive return type. Defaults to <c>vtVoid</c>.
    /// </param>
    /// <param name="AIsEntryPoint">
    ///   If <c>True</c>, marks this overload as the entry point.
    /// </param>
    /// <param name="AIsPublic">
    ///   If <c>True</c>, the mangled symbol is exported.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Overloaded functions always use <c>plDefault</c> linkage since
    ///   C linkage does not support overloading.
    /// </remarks>
    /// <seealso cref="Func"/>
    function OverloadFunc(const AName: string;
      const AReturnType: TTigerValueType = vtVoid;
      const AIsEntryPoint: Boolean = False;
      const AIsPublic: Boolean = False): TTiger;

    /// <summary>
    ///   Begin defining a variadic function that accepts a variable
    ///   number of arguments.
    /// </summary>
    /// <param name="AName">Unique function name.</param>
    /// <param name="AReturnType">
    ///   Primitive return type. Defaults to <c>vtVoid</c>.
    /// </param>
    /// <param name="AIsEntryPoint">
    ///   If <c>True</c>, marks this function as the entry point.
    /// </param>
    /// <param name="AIsPublic">
    ///   If <c>True</c>, the symbol is exported.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Within the function body, use <see cref="VaCount"/> to get
    ///     the number of variadic arguments and <see cref="VaArg"/>
    ///     to access individual arguments by index and type.
    ///   </para>
    /// </remarks>
    /// <seealso cref="Func"/>
    /// <seealso cref="VaCount"/>
    /// <seealso cref="VaArg"/>
    function VariadicFunc(const AName: string;
      const AReturnType: TTigerValueType = vtVoid;
      const AIsEntryPoint: Boolean = False;
      const AIsPublic: Boolean = False): TTiger;

    /// <summary>
    ///   Begin defining the DLL entry point (<c>DllMain</c>).
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     The DLL entry point is called by the Windows loader on
    ///     <c>DLL_PROCESS_ATTACH</c>, <c>DLL_PROCESS_DETACH</c>, etc.
    ///     It has a fixed signature and does not require explicit
    ///     parameter declarations.
    ///   </para>
    ///   <para>
    ///     This is only meaningful when the target is a DLL
    ///     (<see cref="TargetDll"/>). Executable targets should use
    ///     <see cref="Func"/> with <c>AIsEntryPoint = True</c>.
    ///   </para>
    /// </remarks>
    /// <seealso cref="TargetDll"/>
    /// <seealso cref="EndFunc"/>
    function DllMain(): TTiger;

    /// <summary>
    ///   Declare a parameter for the current function being defined.
    /// </summary>
    /// <param name="AName">
    ///   Parameter name. Accessible within the function body via
    ///   <see cref="Get"/>.
    /// </param>
    /// <param name="AType">Primitive type of the parameter.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Parameters must be declared before any <see cref="Local"/>
    ///   declarations or statements. The order of <c>Param</c> calls
    ///   defines the calling convention parameter order.
    /// </remarks>
    /// <seealso cref="Func"/>
    /// <seealso cref="Local"/>
    /// <seealso cref="Get"/>
    function Param(const AName: string; const AType: TTigerValueType): TTiger;

    /// <summary>
    ///   Declare a local variable with a primitive type in the current
    ///   function.
    /// </summary>
    /// <param name="AName">
    ///   Variable name, unique within the function scope.
    /// </param>
    /// <param name="AType">Primitive value type for the variable.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Local variables are allocated on the stack frame and are
    ///   zero-initialized. Access them via <see cref="Get"/>.
    /// </remarks>
    /// <seealso cref="Param"/>
    /// <seealso cref="Global"/>
    /// <seealso cref="Get"/>
    function Local(const AName: string; const AType: TTigerValueType): TTiger; overload;

    /// <summary>
    ///   Declare a local variable with a previously defined named
    ///   composite type.
    /// </summary>
    /// <param name="AName">Variable name, unique within the function scope.</param>
    /// <param name="ATypeName">
    ///   Name of a previously defined type (record, array, enum, etc.).
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Param"/>
    /// <seealso cref="Get"/>
    function Local(const AName: string; const ATypeName: string): TTiger; overload;

    /// <summary>
    ///   End the current function definition.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Finalizes the function, validates its IR, and makes it
    ///   available for calls from other functions. Must be called
    ///   exactly once for each <see cref="Func"/>,
    ///   <see cref="OverloadFunc"/>, <see cref="VariadicFunc"/>,
    ///   or <see cref="DllMain"/> call.
    /// </remarks>
    /// <seealso cref="Func"/>
    function EndFunc(): TTiger;

    //==========================================================================
    // Statements — Assignment, Calls, and Return
    //==========================================================================

    /// <summary>
    ///   Assign a value to a named variable (local, parameter, or global).
    /// </summary>
    /// <param name="ADest">Name of the target variable.</param>
    /// <param name="AValue">Expression producing the value to assign.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="AssignTo"/>
    /// <seealso cref="Get"/>
    function Assign(const ADest: string; const AValue: TTigerExpr): TTiger;

    /// <summary>
    ///   Assign a value to an expression target such as a record field
    ///   or array element.
    /// </summary>
    /// <param name="ADest">
    ///   An expression that evaluates to an assignable location, typically
    ///   from <see cref="GetField"/> or <see cref="GetIndex"/>.
    /// </param>
    /// <param name="AValue">Expression producing the value to assign.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Assign"/>
    /// <seealso cref="GetField"/>
    /// <seealso cref="GetIndex"/>
    function AssignTo(const ADest: TTigerExpr; const AValue: TTigerExpr): TTiger;

    /// <summary>
    ///   Call a function with no arguments (discarding any return value).
    /// </summary>
    /// <param name="AFuncName">Name of the function to call.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Invoke"/>
    /// <seealso cref="CallAssign"/>
    function Call(const AFuncName: string): TTiger; overload;

    /// <summary>
    ///   Call a function with arguments (discarding any return value).
    /// </summary>
    /// <param name="AFuncName">Name of the function to call.</param>
    /// <param name="AArgs">
    ///   Open array of <see cref="TTigerExpr"/> argument expressions.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Invoke"/>
    /// <seealso cref="CallAssign"/>
    function Call(const AFuncName: string; const AArgs: array of TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Call a function and assign the return value to a named variable.
    /// </summary>
    /// <param name="ADest">Name of the variable to receive the return value.</param>
    /// <param name="AFuncName">Name of the function to call.</param>
    /// <param name="AArgs">Open array of argument expressions.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Equivalent to <c>Assign(ADest, CallExpr(AFuncName, AArgs))</c>
    ///   but expressed as a single fluent statement.
    /// </remarks>
    /// <seealso cref="Call"/>
    /// <seealso cref="Invoke"/>
    function CallAssign(const ADest: string; const AFuncName: string; const AArgs: array of TTigerExpr): TTiger;

    /// <summary>
    ///   Return from the current function with no value (for procedures).
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Only valid inside functions declared with <c>vtVoid</c> return
    ///   type. For functions that return a value, use the overload that
    ///   accepts a <see cref="TTigerExpr"/>.
    /// </remarks>
    /// <seealso cref="Func"/>
    function Return(): TTiger; overload;

    /// <summary>
    ///   Return from the current function with a value.
    /// </summary>
    /// <param name="AValue">
    ///   Expression producing the return value. Must match the function's
    ///   declared return type.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="Func"/>
    function Return(const AValue: TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Call a function indirectly through a function pointer with no
    ///   arguments (discarding any return value).
    /// </summary>
    /// <param name="AFuncPtr">
    ///   An expression evaluating to a function pointer, typically from
    ///   <see cref="FuncAddr"/> or a variable of a routine type.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="FuncAddr"/>
    /// <seealso cref="InvokeIndirect"/>
    function CallIndirect(const AFuncPtr: TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Call a function indirectly through a function pointer with
    ///   arguments (discarding any return value).
    /// </summary>
    /// <param name="AFuncPtr">Expression evaluating to a function pointer.</param>
    /// <param name="AArgs">Open array of argument expressions.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="FuncAddr"/>
    /// <seealso cref="InvokeIndirect"/>
    function CallIndirect(const AFuncPtr: TTigerExpr; const AArgs: array of TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Call a function indirectly and assign the return value to a
    ///   named variable.
    /// </summary>
    /// <param name="ADest">Name of the variable to receive the return value.</param>
    /// <param name="AFuncPtr">Expression evaluating to a function pointer.</param>
    /// <param name="AArgs">Open array of argument expressions.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="CallIndirect"/>
    /// <seealso cref="InvokeIndirect"/>
    function CallIndirectAssign(const ADest: string; const AFuncPtr: TTigerExpr;
      const AArgs: array of TTigerExpr): TTiger;

    //==========================================================================
    // Control Flow
    //==========================================================================

    /// <summary>
    ///   Begin a conditional (if) block.
    /// </summary>
    /// <param name="ACond">
    ///   Boolean expression. Statements following this call execute only
    ///   when <c>ACond</c> evaluates to non-zero.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Must be paired with <see cref="EndIf"/>. Optionally include an
    ///   <see cref="&Else"/> branch between the if body and
    ///   <c>EndIf</c>.
    /// </remarks>
    /// <seealso cref="&Else"/>
    /// <seealso cref="EndIf"/>
    function &If(const ACond: TTigerExpr): TTiger;

    /// <summary>
    ///   Begin the else branch of a conditional block.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Must appear between <see cref="&If"/> and
    ///   <see cref="EndIf"/>. Statements following this call execute
    ///   only when the condition in <c>&amp;If</c> was false.
    /// </remarks>
    /// <seealso cref="&If"/>
    /// <seealso cref="EndIf"/>
    function &Else(): TTiger;

    /// <summary>
    ///   End a conditional (if/else) block.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&If"/>
    /// <seealso cref="&Else"/>
    function EndIf(): TTiger;

    /// <summary>
    ///   Begin a while loop.
    /// </summary>
    /// <param name="ACond">
    ///   Boolean expression evaluated before each iteration. The loop
    ///   body executes while <c>ACond</c> is non-zero.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="EndWhile"/>
    /// <seealso cref="&For"/>
    /// <seealso cref="&Repeat"/>
    function &While(const ACond: TTigerExpr): TTiger;

    /// <summary>
    ///   End a while loop.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&While"/>
    function EndWhile(): TTiger;

    /// <summary>
    ///   Begin a counting for loop (ascending: from low to high).
    /// </summary>
    /// <param name="AVar">
    ///   Name of the loop counter variable. Must be a previously declared
    ///   local or global of an integer type.
    /// </param>
    /// <param name="AFrom">Starting value expression (inclusive).</param>
    /// <param name="ATo">Ending value expression (inclusive).</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The counter is incremented by 1 after each iteration. The loop
    ///   body does not execute if <c>AFrom > ATo</c>.
    /// </remarks>
    /// <seealso cref="ForDownTo"/>
    /// <seealso cref="EndFor"/>
    function &For(const AVar: string; const AFrom: TTigerExpr; const ATo: TTigerExpr): TTiger;

    /// <summary>
    ///   Begin a counting for loop (descending: from high to low).
    /// </summary>
    /// <param name="AVar">Name of the loop counter variable.</param>
    /// <param name="AFrom">Starting value expression (inclusive, high end).</param>
    /// <param name="ATo">Ending value expression (inclusive, low end).</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The counter is decremented by 1 after each iteration. The loop
    ///   body does not execute if <c>AFrom &lt; ATo</c>.
    /// </remarks>
    /// <seealso cref="&For"/>
    /// <seealso cref="EndFor"/>
    function ForDownTo(const AVar: string; const AFrom: TTigerExpr; const ATo: TTigerExpr): TTiger;

    /// <summary>
    ///   End a for loop (ascending or descending).
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&For"/>
    /// <seealso cref="ForDownTo"/>
    function EndFor(): TTiger;

    /// <summary>
    ///   Begin a repeat..until loop (post-test loop).
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The loop body always executes at least once. The exit condition
    ///   is evaluated at the end via <see cref="Until"/>. The loop
    ///   terminates when the condition becomes true (non-zero).
    /// </remarks>
    /// <seealso cref="Until"/>
    /// <seealso cref="&While"/>
    function &Repeat(): TTiger;

    /// <summary>
    ///   End a repeat..until loop with the exit condition.
    /// </summary>
    /// <param name="ACond">
    ///   Boolean expression. The loop exits when <c>ACond</c> evaluates
    ///   to non-zero (true).
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Repeat"/>
    function &Until(const ACond: TTigerExpr): TTiger;

    /// <summary>
    ///   Begin a case (switch) statement with a selector expression.
    /// </summary>
    /// <param name="ASelector">
    ///   Integer or enum expression to match against case branches.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Add branches with <see cref="CaseOf"/>, an optional default
    ///   branch with <see cref="CaseElse"/>, and finalize with
    ///   <see cref="EndCase"/>.
    /// </remarks>
    /// <seealso cref="CaseOf"/>
    /// <seealso cref="CaseElse"/>
    /// <seealso cref="EndCase"/>
    function &Case(const ASelector: TTigerExpr): TTiger;

    /// <summary>
    ///   Define a case branch matching one or more expression values.
    /// </summary>
    /// <param name="AValues">
    ///   Open array of <see cref="TTigerExpr"/> values to match. The
    ///   branch executes if the selector equals any of these values.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Statements following this call form the branch body. The body
    ///   ends at the next <c>CaseOf</c>, <c>CaseElse</c>, or
    ///   <c>EndCase</c>.
    /// </remarks>
    /// <seealso cref="&Case"/>
    function CaseOf(const AValues: array of TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Define a case branch matching one or more integer values.
    /// </summary>
    /// <param name="AValues">
    ///   Open array of integer values to match. Convenience overload
    ///   that avoids wrapping each value in <see cref="Int64"/>.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Case"/>
    function CaseOf(const AValues: array of Integer): TTiger; overload;

    /// <summary>
    ///   Begin the default (else) branch of a case statement.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Executes when the selector does not match any <c>CaseOf</c>
    ///   branch. Must appear after all <c>CaseOf</c> calls and before
    ///   <see cref="EndCase"/>.
    /// </remarks>
    /// <seealso cref="&Case"/>
    /// <seealso cref="EndCase"/>
    function CaseElse(): TTiger;

    /// <summary>
    ///   End a case statement.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Case"/>
    function EndCase(): TTiger;

    //==========================================================================
    // Exception Handling
    //==========================================================================

    /// <summary>
    ///   Begin a try block for structured exception handling.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   <para>
    ///     Must be followed by either <see cref="Except"/> (to
    ///     handle exceptions) or <see cref="&Finally"/> (to ensure
    ///     cleanup), and terminated with <see cref="EndTry"/>.
    ///   </para>
    ///   <para>
    ///     Tiger's exception handling maps to Windows Structured Exception
    ///     Handling (SEH) at the machine code level.
    ///   </para>
    /// </remarks>
    /// <seealso cref="Except"/>
    /// <seealso cref="&Finally"/>
    /// <seealso cref="EndTry"/>
    /// <seealso cref="&Raise"/>
    function &Try(): TTiger;

    /// <summary>
    ///   Begin the except handler of a try block.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   Inside the except block, use <see cref="ExcCode"/>
    ///   and <see cref="ExcMsg"/> to inspect the caught
    ///   exception.
    /// </remarks>
    /// <seealso cref="&Try"/>
    /// <seealso cref="EndTry"/>
    /// <seealso cref="ExcCode"/>
    /// <seealso cref="ExcMsg"/>
    function &Except(): TTiger;

    /// <summary>
    ///   Begin the finally handler of a try block.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The finally block executes regardless of whether an exception
    ///   occurred. It cannot catch or suppress exceptions — for that,
    ///   use <see cref="Except"/>.
    /// </remarks>
    /// <seealso cref="&Try"/>
    /// <seealso cref="EndTry"/>
    function &Finally(): TTiger;

    /// <summary>
    ///   End a try/except or try/finally block.
    /// </summary>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Try"/>
    function EndTry(): TTiger;

    /// <summary>
    ///   Raise an exception with a message string.
    /// </summary>
    /// <param name="AMsg">
    ///   Expression evaluating to a string message for the exception.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <remarks>
    ///   The exception code defaults to a generic value. Use
    ///   <see cref="RaiseCode"/> to specify both a code and a message.
    /// </remarks>
    /// <seealso cref="RaiseCode"/>
    /// <seealso cref="&Try"/>
    function &Raise(const AMsg: TTigerExpr): TTiger;

    /// <summary>
    ///   Raise an exception with an explicit error code and message.
    /// </summary>
    /// <param name="ACode">
    ///   Integer expression for the exception code.
    /// </param>
    /// <param name="AMsg">
    ///   Expression evaluating to a string message.
    /// </param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Raise"/>
    /// <seealso cref="ExcCode"/>
    function RaiseCode(const ACode: TTigerExpr; const AMsg: TTigerExpr): TTiger;

    //==========================================================================
    // Increment / Decrement
    //==========================================================================

    /// <summary>
    ///   Increment a variable by 1.
    /// </summary>
    /// <param name="AVarName">Name of an integer variable (local, parameter, or global).</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Dec"/>
    function &Inc(const AVarName: string): TTiger; overload;

    /// <summary>
    ///   Increment a variable by a specified amount.
    /// </summary>
    /// <param name="AVarName">Name of an integer variable.</param>
    /// <param name="AAmount">Expression for the increment amount.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Dec"/>
    function &Inc(const AVarName: string; const AAmount: TTigerExpr): TTiger; overload;

    /// <summary>
    ///   Decrement a variable by 1.
    /// </summary>
    /// <param name="AVarName">Name of an integer variable.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Inc"/>
    function &Dec(const AVarName: string): TTiger; overload;

    /// <summary>
    ///   Decrement a variable by a specified amount.
    /// </summary>
    /// <param name="AVarName">Name of an integer variable.</param>
    /// <param name="AAmount">Expression for the decrement amount.</param>
    /// <returns>Self for fluent chaining.</returns>
    /// <seealso cref="&Inc"/>
    function &Dec(const AVarName: string; const AAmount: TTigerExpr): TTiger; overload;

    //==========================================================================
    // Expressions — Literals
    //==========================================================================

    /// <summary>
    ///   Create an ANSI string literal expression.
    /// </summary>
    /// <param name="AValue">
    ///   The string content. Escape sequences such as <c>#10</c> (LF)
    ///   and <c>#13</c> (CR) are supported in the Delphi source that
    ///   calls this method.
    /// </param>
    /// <returns>
    ///   An expression handle representing a pointer to the null-terminated
    ///   ANSI string in the data section.
    /// </returns>
    /// <remarks>
    ///   The string is stored in the PE data section and its address is
    ///   passed as a <c>vtPointer</c> value. Suitable for C-style
    ///   <c>char*</c> parameters (e.g. <c>printf</c> format strings).
    /// </remarks>
    /// <seealso cref="WStr"/>
    function Str(const AValue: string): TTigerExpr;

    /// <summary>
    ///   Create a wide (UTF-16) string literal expression.
    /// </summary>
    /// <param name="AValue">The string content.</param>
    /// <returns>
    ///   An expression handle representing a pointer to the null-terminated
    ///   wide string in the data section.
    /// </returns>
    /// <remarks>
    ///   Use for Windows <c>wchar_t*</c> / <c>LPCWSTR</c> parameters
    ///   (e.g. <c>MessageBoxW</c>).
    /// </remarks>
    /// <seealso cref="Str"/>
    function WStr(const AValue: string): TTigerExpr;

    /// <summary>
    ///   Create a 64-bit signed integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value.</param>
    /// <returns>An expression handle of type <c>vtInt64</c>.</returns>
    /// <remarks>
    ///   This is the most common literal constructor. The value is
    ///   automatically narrowed when used in a context expecting a
    ///   smaller type.
    /// </remarks>
    /// <seealso cref="Int32"/>
    /// <seealso cref="Int64"/>
    /// <seealso cref="Float64"/>
    /// <seealso cref="Bool"/>
    function Int64(const AValue: Int64): TTigerExpr;

    /// <summary>
    ///   Create a 32-bit signed integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value.</param>
    /// <returns>An expression handle of type <c>vtInt32</c>.</returns>
    /// <remarks>
    ///   Use when you need to explicitly control the width of the
    ///   literal, e.g. for <c>DWORD</c>-sized parameters.
    /// </remarks>
    /// <seealso cref="Int64"/>
    function Int32(const AValue: Int32): TTigerExpr;

    /// <summary>
    ///   Create a 64-bit floating-point literal expression.
    /// </summary>
    /// <param name="AValue">The double-precision floating-point value.</param>
    /// <returns>An expression handle of type <c>vtFloat64</c>.</returns>
    /// <seealso cref="Int64"/>
    function Float64(const AValue: Double): TTigerExpr;

    /// <summary>
    ///   Create a boolean literal expression.
    /// </summary>
    /// <param name="AValue"><c>True</c> or <c>False</c>.</param>
    /// <returns>
    ///   An expression handle. <c>True</c> produces a non-zero integer;
    ///   <c>False</c> produces zero.
    /// </returns>
    /// <seealso cref="Int64"/>
    function Bool(const AValue: Boolean): TTigerExpr;

    /// <summary>
    ///   Create an 8-bit signed integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value (−128..127).</param>
    /// <returns>An expression handle of type <c>vtInt8</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect an <c>int8_t</c>
    ///   or <c>signed char</c> parameter.
    /// </remarks>
    /// <seealso cref="Int16"/>
    /// <seealso cref="Int32"/>
    /// <seealso cref="Int64"/>
    function Int8(const AValue: Int8): TTigerExpr;

    /// <summary>
    ///   Create a 16-bit signed integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value (−32768..32767).</param>
    /// <returns>An expression handle of type <c>vtInt16</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect an <c>int16_t</c>
    ///   or <c>short</c> parameter.
    /// </remarks>
    /// <seealso cref="Int8"/>
    /// <seealso cref="Int32"/>
    /// <seealso cref="Int64"/>
    function Int16(const AValue: Int16): TTigerExpr;

    /// <summary>
    ///   Create an 8-bit unsigned integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value (0..255).</param>
    /// <returns>An expression handle of type <c>vtUInt8</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect a <c>uint8_t</c>
    ///   or <c>unsigned char</c> parameter, such as byte-level I/O.
    /// </remarks>
    /// <seealso cref="UInt16"/>
    /// <seealso cref="UInt32"/>
    /// <seealso cref="UInt64"/>
    function UInt8(const AValue: UInt8): TTigerExpr;

    /// <summary>
    ///   Create a 16-bit unsigned integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value (0..65535).</param>
    /// <returns>An expression handle of type <c>vtUInt16</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect a <c>uint16_t</c>
    ///   or <c>unsigned short</c> parameter, such as Windows <c>WORD</c>
    ///   fields.
    /// </remarks>
    /// <seealso cref="UInt8"/>
    /// <seealso cref="UInt32"/>
    /// <seealso cref="UInt64"/>
    function UInt16(const AValue: UInt16): TTigerExpr;

    /// <summary>
    ///   Create a 32-bit unsigned integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value (0..4294967295).</param>
    /// <returns>An expression handle of type <c>vtUInt32</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect a <c>uint32_t</c>
    ///   or Windows <c>DWORD</c> parameter, such as flags and handles.
    /// </remarks>
    /// <seealso cref="UInt8"/>
    /// <seealso cref="UInt16"/>
    /// <seealso cref="UInt64"/>
    /// <seealso cref="Int32"/>
    function UInt32(const AValue: UInt32): TTigerExpr;

    /// <summary>
    ///   Create a 64-bit unsigned integer literal expression.
    /// </summary>
    /// <param name="AValue">The integer value (0..2^64−1).</param>
    /// <returns>An expression handle of type <c>vtUInt64</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect a <c>uint64_t</c>
    ///   or <c>size_t</c> parameter on 64-bit platforms. Also suitable
    ///   for memory sizes and file offsets.
    /// </remarks>
    /// <seealso cref="UInt8"/>
    /// <seealso cref="UInt16"/>
    /// <seealso cref="UInt32"/>
    /// <seealso cref="Int64"/>
    function UInt64(const AValue: UInt64): TTigerExpr;

    /// <summary>
    ///   Create a 32-bit floating-point literal expression.
    /// </summary>
    /// <param name="AValue">The floating-point value.</param>
    /// <returns>An expression handle of type <c>vtFloat32</c>.</returns>
    /// <remarks>
    ///   Use when targeting C ABI functions that expect a <c>float</c>
    ///   parameter. For general-purpose floating-point use
    ///   <see cref="Float64"/> instead, which provides double precision.
    /// </remarks>
    /// <seealso cref="Float64"/>
    function Float32(const AValue: Single): TTigerExpr;
    /// <summary>
    ///   Create a nil (null pointer) literal expression.
    /// </summary>
    /// <returns>An expression handle of type <c>vtPointer</c> with value 0.</returns>
    /// <seealso cref="Deref"/>
    /// <seealso cref="AddrOf"/>
    function Null(): TTigerExpr;

    //==========================================================================
    // Expressions — Variable Reference
    //==========================================================================

    /// <summary>
    ///   Create a reference to a local variable, parameter, or global
    ///   by name.
    /// </summary>
    /// <param name="AName">
    ///   Name of the variable. Lookup order: current function's locals
    ///   and parameters first, then globals.
    /// </param>
    /// <returns>
    ///   An expression handle representing the current value of the
    ///   variable.
    /// </returns>
    /// <remarks>
    ///   This is the primary way to read variable values in expressions.
    ///   To write to a variable, use <see cref="Assign"/>.
    /// </remarks>
    /// <seealso cref="Assign"/>
    /// <seealso cref="AddrOf"/>
    function Get(const AName: string): TTigerExpr;

    //==========================================================================
    // Expressions — Arithmetic
    //==========================================================================

    /// <summary>
    ///   Addition: <c>ALeft + ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>An expression handle for the sum.</returns>
    /// <remarks>
    ///   Operands are widened to a common type following standard
    ///   promotion rules (e.g. Int32 + Int64 → Int64, Int + Float → Float).
    /// </remarks>
    function Add(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Subtraction: <c>ALeft - ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>An expression handle for the difference.</returns>
    function Sub(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Multiplication: <c>ALeft * ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>An expression handle for the product.</returns>
    function Mul(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Division: <c>ALeft / ARight</c>.
    /// </summary>
    /// <param name="ALeft">Dividend expression.</param>
    /// <param name="ARight">Divisor expression. Must not be zero at runtime.</param>
    /// <returns>An expression handle for the quotient.</returns>
    /// <remarks>
    ///   For integer operands, this performs truncating division. For
    ///   floating-point operands, IEEE 754 division rules apply.
    /// </remarks>
    function IDiv(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Modulo (remainder): <c>ALeft mod ARight</c>.
    /// </summary>
    /// <param name="ALeft">Dividend expression.</param>
    /// <param name="ARight">Divisor expression. Must not be zero at runtime.</param>
    /// <returns>An expression handle for the remainder.</returns>
    function IMod(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Arithmetic negation: <c>-AValue</c>.
    /// </summary>
    /// <param name="AValue">Operand expression.</param>
    /// <returns>An expression handle for the negated value.</returns>
    function Neg(const AValue: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Bitwise
    //==========================================================================

    /// <summary>
    ///   Bitwise AND: <c>ALeft and ARight</c> (bit-level).
    /// </summary>
    /// <param name="ALeft">Left operand expression (integer type).</param>
    /// <param name="ARight">Right operand expression (integer type).</param>
    /// <returns>An expression handle for the bitwise AND result.</returns>
    function BitAnd(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Bitwise OR: <c>ALeft or ARight</c> (bit-level).
    /// </summary>
    /// <param name="ALeft">Left operand expression (integer type).</param>
    /// <param name="ARight">Right operand expression (integer type).</param>
    /// <returns>An expression handle for the bitwise OR result.</returns>
    function BitOr(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Bitwise XOR: <c>ALeft xor ARight</c> (bit-level).
    /// </summary>
    /// <param name="ALeft">Left operand expression (integer type).</param>
    /// <param name="ARight">Right operand expression (integer type).</param>
    /// <returns>An expression handle for the bitwise XOR result.</returns>
    function BitXor(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Bitwise NOT: <c>not AValue</c> (bit-level complement).
    /// </summary>
    /// <param name="AValue">Operand expression (integer type).</param>
    /// <returns>An expression handle for the bitwise complement.</returns>
    function BitNot(const AValue: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Shift left: <c>AValue shl ACount</c>.
    /// </summary>
    /// <param name="AValue">Value to shift (integer type).</param>
    /// <param name="ACount">Number of bit positions to shift left.</param>
    /// <returns>An expression handle for the shifted result.</returns>
    function &Shl(const AValue: TTigerExpr; const ACount: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Shift right: <c>AValue shr ACount</c>.
    /// </summary>
    /// <param name="AValue">Value to shift (integer type).</param>
    /// <param name="ACount">Number of bit positions to shift right.</param>
    /// <returns>An expression handle for the shifted result.</returns>
    /// <remarks>
    ///   For unsigned types, this is a logical shift (zero-fill). For
    ///   signed types, this is an arithmetic shift (sign-extending).
    /// </remarks>
    function &Shr(const AValue: TTigerExpr; const ACount: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Comparison
    //==========================================================================

    /// <summary>
    ///   Equality comparison: <c>ALeft = ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>A boolean expression (non-zero if equal, zero otherwise).</returns>
    function Eq(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Inequality comparison: <c>ALeft &lt;&gt; ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>A boolean expression (non-zero if not equal).</returns>
    function Ne(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Less-than comparison: <c>ALeft &lt; ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>A boolean expression (non-zero if less).</returns>
    function Lt(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Less-than-or-equal comparison: <c>ALeft &lt;= ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>A boolean expression (non-zero if less or equal).</returns>
    function Le(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Greater-than comparison: <c>ALeft &gt; ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>A boolean expression (non-zero if greater).</returns>
    function Gt(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Greater-than-or-equal comparison: <c>ALeft &gt;= ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left operand expression.</param>
    /// <param name="ARight">Right operand expression.</param>
    /// <returns>A boolean expression (non-zero if greater or equal).</returns>
    function Ge(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Logical
    //==========================================================================

    /// <summary>
    ///   Logical AND with short-circuit evaluation.
    /// </summary>
    /// <param name="ALeft">Left boolean expression.</param>
    /// <param name="ARight">
    ///   Right boolean expression. Only evaluated if <c>ALeft</c> is true.
    /// </param>
    /// <returns>A boolean expression (non-zero if both are true).</returns>
    function LogAnd(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Logical OR with short-circuit evaluation.
    /// </summary>
    /// <param name="ALeft">Left boolean expression.</param>
    /// <param name="ARight">
    ///   Right boolean expression. Only evaluated if <c>ALeft</c> is false.
    /// </param>
    /// <returns>A boolean expression (non-zero if either is true).</returns>
    function LogOr(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Logical NOT: inverts a boolean value.
    /// </summary>
    /// <param name="AValue">Boolean expression to invert.</param>
    /// <returns>A boolean expression (non-zero if <c>AValue</c> was zero).</returns>
    function LogNot(const AValue: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Pointers
    //==========================================================================

    /// <summary>
    ///   Take the address of a named variable (local, parameter, or global).
    /// </summary>
    /// <param name="AName">Name of the variable.</param>
    /// <returns>A <c>vtPointer</c> expression pointing to the variable.</returns>
    /// <seealso cref="AddrOfVal"/>
    /// <seealso cref="Deref"/>
    function AddrOf(const AName: string): TTigerExpr;

    /// <summary>
    ///   Take the address of an expression that evaluates to an
    ///   addressable location (e.g. a record field or array element).
    /// </summary>
    /// <param name="AExpr">
    ///   An addressable expression, typically from <see cref="GetField"/>
    ///   or <see cref="GetIndex"/>.
    /// </param>
    /// <returns>A <c>vtPointer</c> expression pointing to the location.</returns>
    /// <seealso cref="AddrOf"/>
    /// <seealso cref="Deref"/>
    function AddrOfVal(const AExpr: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Dereference a pointer expression (untyped).
    /// </summary>
    /// <param name="APtr">Pointer expression to dereference.</param>
    /// <returns>
    ///   An expression representing the value at the pointed-to address.
    /// </returns>
    /// <remarks>
    ///   The result type is inferred from context. For explicit typing,
    ///   use the overloads that accept a type name or
    ///   <see cref="TTigerValueType"/>.
    /// </remarks>
    /// <seealso cref="AddrOf"/>
    function Deref(const APtr: TTigerExpr): TTigerExpr; overload;

    /// <summary>
    ///   Dereference a pointer expression, interpreting the result as
    ///   the specified named type.
    /// </summary>
    /// <param name="APtr">Pointer expression to dereference.</param>
    /// <param name="ATypeName">Name of the target type.</param>
    /// <returns>An expression of the specified type.</returns>
    /// <seealso cref="AddrOf"/>
    function Deref(const APtr: TTigerExpr; const ATypeName: string): TTigerExpr; overload;

    /// <summary>
    ///   Dereference a pointer expression, interpreting the result as
    ///   the specified primitive type.
    /// </summary>
    /// <param name="APtr">Pointer expression to dereference.</param>
    /// <param name="AType">Primitive type for the dereferenced value.</param>
    /// <returns>An expression of the specified primitive type.</returns>
    /// <seealso cref="AddrOf"/>
    function Deref(const APtr: TTigerExpr; const AType: TTigerValueType): TTigerExpr; overload;

    //==========================================================================
    // Expressions — Function Pointers
    //==========================================================================

    /// <summary>
    ///   Get the address of a function by name, producing a function
    ///   pointer expression.
    /// </summary>
    /// <param name="AFuncName">
    ///   Name of a defined or imported function.
    /// </param>
    /// <returns>
    ///   A <c>vtPointer</c> expression holding the function's address.
    /// </returns>
    /// <remarks>
    ///   The resulting pointer can be stored in a variable of a routine
    ///   type (see <see cref="DefineRoutine"/>) and called via
    ///   <see cref="CallIndirect"/> or <see cref="InvokeIndirect"/>.
    /// </remarks>
    /// <seealso cref="CallIndirect"/>
    /// <seealso cref="InvokeIndirect"/>
    /// <seealso cref="DefineRoutine"/>
    function FuncAddr(const AFuncName: string): TTigerExpr;

    /// <summary>
    ///   Call a function indirectly through a pointer and return the
    ///   result as an expression.
    /// </summary>
    /// <param name="AFuncPtr">Expression evaluating to a function pointer.</param>
    /// <param name="AArgs">Open array of argument expressions.</param>
    /// <returns>An expression handle for the function's return value.</returns>
    /// <remarks>
    ///   Use this when you need the return value in an expression
    ///   context. For fire-and-forget calls, use
    ///   <see cref="CallIndirect"/> instead.
    /// </remarks>
    /// <seealso cref="CallIndirect"/>
    /// <seealso cref="FuncAddr"/>
    function InvokeIndirect(const AFuncPtr: TTigerExpr; const AArgs: array of TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Composite Access
    //==========================================================================

    /// <summary>
    ///   Access a field of a record or union expression.
    /// </summary>
    /// <param name="AObject">
    ///   An expression evaluating to a record or union value (e.g. from
    ///   <see cref="Get"/> with a record-typed variable).
    /// </param>
    /// <param name="AFieldName">
    ///   Name of the field as declared in the record or union definition.
    /// </param>
    /// <returns>
    ///   An expression handle for the field's value. This expression is
    ///   also assignable via <see cref="AssignTo"/>.
    /// </returns>
    /// <seealso cref="GetIndex"/>
    /// <seealso cref="AssignTo"/>
    /// <seealso cref="DefineRecord"/>
    function GetField(const AObject: TTigerExpr; const AFieldName: string): TTigerExpr;

    /// <summary>
    ///   Index into an array expression.
    /// </summary>
    /// <param name="AArray">
    ///   An expression evaluating to an array value (e.g. from
    ///   <see cref="Get"/> with an array-typed variable).
    /// </param>
    /// <param name="AIndex">
    ///   Integer expression for the element index. For fixed arrays,
    ///   this must be within the declared bounds.
    /// </param>
    /// <returns>
    ///   An expression handle for the element's value. This expression
    ///   is also assignable via <see cref="AssignTo"/>.
    /// </returns>
    /// <seealso cref="GetField"/>
    /// <seealso cref="AssignTo"/>
    /// <seealso cref="DefineArray"/>
    function GetIndex(const AArray: TTigerExpr; const AIndex: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Function Call
    //==========================================================================

    /// <summary>
    ///   Call a function by name and return the result as an expression.
    /// </summary>
    /// <param name="AFuncName">Name of the function to call.</param>
    /// <param name="AArgs">Open array of argument expressions.</param>
    /// <returns>
    ///   An expression handle for the function's return value. Can be
    ///   nested inside other expressions, passed as an argument, or
    ///   used in <see cref="Assign"/>.
    /// </returns>
    /// <remarks>
    ///   Use this when you need the return value in an expression
    ///   context. For fire-and-forget calls, use <see cref="Call"/>
    ///   instead.
    /// </remarks>
    /// <seealso cref="Call"/>
    /// <seealso cref="CallAssign"/>
    /// <seealso cref="InvokeIndirect"/>
    function Invoke(const AFuncName: string; const AArgs: array of TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Exception Intrinsics
    //==========================================================================

    /// <summary>
    ///   Get the current exception code inside an except block.
    /// </summary>
    /// <returns>An integer expression containing the exception code.</returns>
    /// <remarks>
    ///   Only valid inside an except handler started with
    ///   <see cref="Except"/>. The code corresponds to the value
    ///   passed to <see cref="RaiseCode"/> or a default value for
    ///   message-only exceptions.
    /// </remarks>
    /// <seealso cref="ExcMsg"/>
    /// <seealso cref="Except"/>
    /// <seealso cref="RaiseCode"/>
    function ExcCode(): TTigerExpr;

    /// <summary>
    ///   Get the current exception message string inside an except block.
    /// </summary>
    /// <returns>
    ///   A pointer expression to the null-terminated exception message
    ///   string.
    /// </returns>
    /// <remarks>
    ///   Only valid inside an except handler started with
    ///   <see cref="Except"/>.
    /// </remarks>
    /// <seealso cref="ExcCode"/>
    /// <seealso cref="Except"/>
    function ExcMsg(): TTigerExpr;

    //==========================================================================
    // Expressions — Set Literals & Operations
    //==========================================================================

    /// <summary>
    ///   Create a set literal with individual elements.
    /// </summary>
    /// <param name="ATypeName">
    ///   Name of a previously defined set type.
    /// </param>
    /// <param name="AElements">
    ///   Open array of integer ordinals to include in the set.
    /// </param>
    /// <returns>An expression handle for the set value.</returns>
    /// <seealso cref="SetLitRange"/>
    /// <seealso cref="EmptySet"/>
    /// <seealso cref="DefineSet"/>
    function SetLit(const ATypeName: string; const AElements: array of Integer): TTigerExpr;

    /// <summary>
    ///   Create a set literal containing all values in a contiguous range.
    /// </summary>
    /// <param name="ATypeName">Name of a previously defined set type.</param>
    /// <param name="ALow">Inclusive lower bound of the range.</param>
    /// <param name="AHigh">Inclusive upper bound of the range.</param>
    /// <returns>An expression handle for the set value.</returns>
    /// <seealso cref="SetLit"/>
    /// <seealso cref="EmptySet"/>
    function SetLitRange(const ATypeName: string; const ALow: Integer; const AHigh: Integer): TTigerExpr;

    /// <summary>
    ///   Create an empty set of the given type (no elements).
    /// </summary>
    /// <param name="ATypeName">Name of a previously defined set type.</param>
    /// <returns>An expression handle for the empty set.</returns>
    /// <seealso cref="SetLit"/>
    /// <seealso cref="SetLitRange"/>
    function EmptySet(const ATypeName: string): TTigerExpr;

    /// <summary>
    ///   Set union: <c>ALeft + ARight</c>. Returns a set containing all
    ///   elements present in either operand.
    /// </summary>
    /// <param name="ALeft">Left set expression.</param>
    /// <param name="ARight">Right set expression.</param>
    /// <returns>An expression handle for the union result.</returns>
    /// <seealso cref="SetDiff"/>
    /// <seealso cref="SetInter"/>
    function SetUnion(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set difference: <c>ALeft - ARight</c>. Returns a set containing
    ///   elements in <c>ALeft</c> that are not in <c>ARight</c>.
    /// </summary>
    /// <param name="ALeft">Left set expression.</param>
    /// <param name="ARight">Right set expression.</param>
    /// <returns>An expression handle for the difference result.</returns>
    /// <seealso cref="SetUnion"/>
    /// <seealso cref="SetInter"/>
    function SetDiff(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set intersection: <c>ALeft * ARight</c>. Returns a set containing
    ///   only elements present in both operands.
    /// </summary>
    /// <param name="ALeft">Left set expression.</param>
    /// <param name="ARight">Right set expression.</param>
    /// <returns>An expression handle for the intersection result.</returns>
    /// <seealso cref="SetUnion"/>
    /// <seealso cref="SetDiff"/>
    function SetInter(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set membership test: <c>AElement in ASet</c>.
    /// </summary>
    /// <param name="AElement">Integer or enum expression to test.</param>
    /// <param name="ASet">Set expression to search.</param>
    /// <returns>A boolean expression (non-zero if the element is in the set).</returns>
    /// <seealso cref="SetLit"/>
    function SetIn(const AElement: TTigerExpr; const ASet: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set equality: tests whether two sets contain exactly the same elements.
    /// </summary>
    /// <param name="ALeft">Left set expression.</param>
    /// <param name="ARight">Right set expression.</param>
    /// <returns>A boolean expression (non-zero if equal).</returns>
    /// <seealso cref="SetNe"/>
    function SetEq(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set inequality: tests whether two sets differ in at least one element.
    /// </summary>
    /// <param name="ALeft">Left set expression.</param>
    /// <param name="ARight">Right set expression.</param>
    /// <returns>A boolean expression (non-zero if not equal).</returns>
    /// <seealso cref="SetEq"/>
    function SetNe(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set subset test: <c>ALeft &lt;= ARight</c>. Tests whether
    ///   every element of <c>ALeft</c> is also in <c>ARight</c>.
    /// </summary>
    /// <param name="ALeft">Potential subset.</param>
    /// <param name="ARight">Potential superset.</param>
    /// <returns>A boolean expression (non-zero if <c>ALeft</c> is a subset).</returns>
    /// <seealso cref="SetSuperset"/>
    function SetSubset(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Set superset test: <c>ALeft &gt;= ARight</c>. Tests whether
    ///   <c>ALeft</c> contains every element of <c>ARight</c>.
    /// </summary>
    /// <param name="ALeft">Potential superset.</param>
    /// <param name="ARight">Potential subset.</param>
    /// <returns>A boolean expression (non-zero if <c>ALeft</c> is a superset).</returns>
    /// <seealso cref="SetSubset"/>
    function SetSuperset(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Compile-Time Intrinsics
    //==========================================================================

    /// <summary>
    ///   Get the size in bytes of a named type (compile-time constant).
    /// </summary>
    /// <param name="ATypeName">Name of a previously defined type.</param>
    /// <returns>An integer expression containing the type's size.</returns>
    /// <remarks>
    ///   Equivalent to Delphi's <c>SizeOf()</c>. Includes any padding
    ///   required for alignment.
    /// </remarks>
    /// <seealso cref="AlignOf"/>
    /// <seealso cref="GetTypeSize"/>
    function &SizeOf(const ATypeName: string): TTigerExpr;

    /// <summary>
    ///   Get the alignment in bytes of a named type (compile-time constant).
    /// </summary>
    /// <param name="ATypeName">Name of a previously defined type.</param>
    /// <returns>An integer expression containing the type's alignment.</returns>
    /// <seealso cref="&SizeOf"/>
    /// <seealso cref="GetTypeAlignment"/>
    function AlignOf(const ATypeName: string): TTigerExpr;

    /// <summary>
    ///   Get the high bound of a named array or enum type (compile-time constant).
    /// </summary>
    /// <param name="ATypeName">Name of an array or enum type.</param>
    /// <returns>An integer expression for the upper bound.</returns>
    /// <remarks>
    ///   For arrays, returns the declared <c>AHighBound</c>. For enums,
    ///   returns the ordinal of the last value.
    /// </remarks>
    /// <seealso cref="Low"/>
    /// <seealso cref="Len"/>
    function High(const ATypeName: string): TTigerExpr;

    /// <summary>
    ///   Get the low bound of a named array or enum type (compile-time constant).
    /// </summary>
    /// <param name="ATypeName">Name of an array or enum type.</param>
    /// <returns>An integer expression for the lower bound.</returns>
    /// <seealso cref="High"/>
    /// <seealso cref="Len"/>
    function Low(const ATypeName: string): TTigerExpr;

    /// <summary>
    ///   Get the element count of a named array type (compile-time constant).
    /// </summary>
    /// <param name="ATypeName">Name of a fixed-size array type.</param>
    /// <returns>
    ///   An integer expression: <c>High - Low + 1</c>.
    /// </returns>
    /// <seealso cref="High"/>
    /// <seealso cref="Low"/>
    function Len(const ATypeName: string): TTigerExpr;

    //==========================================================================
    // Expressions — Runtime Intrinsics
    //==========================================================================

    /// <summary>
    ///   Get the ordinal (integer) value of an enum expression.
    /// </summary>
    /// <param name="AValue">An enum-typed expression.</param>
    /// <returns>An integer expression for the ordinal value.</returns>
    /// <seealso cref="Chr"/>
    /// <seealso cref="Succ"/>
    /// <seealso cref="Pred"/>
    function Ord(const AValue: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Convert an integer ordinal to a character value.
    /// </summary>
    /// <param name="AValue">An integer expression (0..255 for ANSI).</param>
    /// <returns>An expression representing the character.</returns>
    /// <seealso cref="Ord"/>
    function Chr(const AValue: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Get the successor (next ordinal value) of an enum or integer
    ///   expression.
    /// </summary>
    /// <param name="AValue">An ordinal expression.</param>
    /// <returns>An expression for <c>AValue + 1</c>.</returns>
    /// <seealso cref="Pred"/>
    /// <seealso cref="Ord"/>
    function Succ(const AValue: TTigerExpr): TTigerExpr;

    /// <summary>
    ///   Get the predecessor (previous ordinal value) of an enum or
    ///   integer expression.
    /// </summary>
    /// <param name="AValue">An ordinal expression.</param>
    /// <returns>An expression for <c>AValue - 1</c>.</returns>
    /// <seealso cref="Succ"/>
    /// <seealso cref="Ord"/>
    function Pred(const AValue: TTigerExpr): TTigerExpr;

    //==========================================================================
    // Expressions — Variadic Intrinsics
    //==========================================================================

    /// <summary>
    ///   Get the number of variadic arguments passed to the current
    ///   variadic function.
    /// </summary>
    /// <returns>
    ///   An integer expression for the count of extra arguments beyond
    ///   the fixed parameters.
    /// </returns>
    /// <remarks>
    ///   Only valid inside a function defined with
    ///   <see cref="VariadicFunc"/>.
    /// </remarks>
    /// <seealso cref="VaArg"/>
    /// <seealso cref="VariadicFunc"/>
    function VaCount(): TTigerExpr;

    /// <summary>
    ///   Get a variadic argument by index, cast to the specified type.
    /// </summary>
    /// <param name="AIndex">
    ///   Zero-based index into the variadic argument list.
    /// </param>
    /// <param name="AType">
    ///   The expected type of the argument. The caller is responsible
    ///   for ensuring the type matches the actual argument passed.
    /// </param>
    /// <returns>An expression of the specified type.</returns>
    /// <remarks>
    ///   Only valid inside a function defined with
    ///   <see cref="VariadicFunc"/>. There is no runtime type
    ///   checking — an incorrect type produces undefined behavior.
    /// </remarks>
    /// <seealso cref="VaCount"/>
    /// <seealso cref="VariadicFunc"/>
    function VaArg(const AIndex: TTigerExpr; const AType: TTigerValueType): TTigerExpr;

    //==========================================================================
    // Advanced — Direct Access to Internal Objects
    //==========================================================================

    /// <summary>
    ///   Get the internal IR builder for advanced, low-level access.
    /// </summary>
    /// <returns>
    ///   The <see cref="TTigerIR"/> instance owned by this compiler.
    ///   Do not free the returned object.
    /// </returns>
    /// <remarks>
    ///   <b>Advanced use only.</b> Provides direct access to the IR
    ///   layer, bypassing the fluent facade. Changes made directly to
    ///   the IR must be consistent with any pending facade operations.
    /// </remarks>
    /// <seealso cref="GetBackend"/>
    /// <seealso cref="GetErrors"/>
    function GetIR(): TTigerIR;

    /// <summary>
    ///   Get the internal backend for advanced, low-level access.
    /// </summary>
    /// <returns>
    ///   The <see cref="TTigerBackend"/> instance owned by this compiler.
    ///   Do not free the returned object.
    /// </returns>
    /// <remarks>
    ///   <b>Advanced use only.</b> Provides direct access to the code
    ///   builder, data section, and import table for scenarios not
    ///   covered by the fluent API.
    /// </remarks>
    /// <seealso cref="GetIR"/>
    /// <seealso cref="GetErrors"/>
    function GetBackend(): TTigerBackend;

    /// <summary>
    ///   Get the internal error manager for advanced, low-level access.
    /// </summary>
    /// <returns>
    ///   The <see cref="TErrors"/> instance owned by this compiler.
    ///   Do not free the returned object.
    /// </returns>
    /// <remarks>
    ///   <b>Advanced use only.</b> Allows adding custom diagnostics or
    ///   directly querying the error list without going through the
    ///   facade's convenience methods.
    /// </remarks>
    /// <seealso cref="GetIR"/>
    /// <seealso cref="GetBackend"/>
    function GetErrors(): TErrors;

    /// <summary>
    ///   Returns the total number of user-defined composite types registered
    ///   in the intermediate representation.
    /// </summary>
    /// <returns>
    ///   The count of types defined via <see cref="DefineRecord"/>,
    ///   <see cref="DefineArray"/>, <see cref="DefineEnum"/>,
    ///   <see cref="DefineAlias"/>, and related methods.
    /// </returns>
    /// <remarks>
    ///   Primitive types (vtInt32, vtFloat64, etc.) are not included in
    ///   this count. Only composite types added through the type-definition
    ///   API are counted.
    /// </remarks>
    /// <seealso cref="FindType"/>
    /// <seealso cref="GetTypeSize"/>
    /// <seealso cref="GetTypeAlignment"/>
    function GetTypeCount(): Integer;

    /// <summary>
    ///   Returns the byte offset of a named field within a record type,
    ///   including fields inherited from ancestor records.
    /// </summary>
    /// <param name="ATypeName">
    ///   The name of the record type to search (e.g. <c>'TPoint3D'</c>).
    /// </param>
    /// <param name="AFieldName">
    ///   The name of the field whose offset is requested (e.g. <c>'X'</c>).
    /// </param>
    /// <returns>
    ///   The field's byte offset from the start of the record, or <c>-1</c>
    ///   if the type or field is not found.
    /// </returns>
    /// <remarks>
    ///   This method resolves inherited fields transparently. For example,
    ///   if <c>TPoint3D</c> inherits from <c>TPoint2D</c>, querying
    ///   <c>GetFieldOffset('TPoint3D', 'X')</c> will return the offset
    ///   of <c>X</c> as laid out in the derived record.
    /// </remarks>
    /// <seealso cref="FindType"/>
    /// <seealso cref="DefineRecord"/>
    /// <seealso cref="GetTypeSize"/>
    function GetFieldOffset(const ATypeName: string; const AFieldName: string): Integer;

    //==========================================================================
    // VersionInfo
    //==========================================================================

    /// <summary>
    ///   Enables or disables embedding a Win32 VERSIONINFO resource in the
    ///   output executable or DLL.
    /// </summary>
    /// <param name="AEnable">
    ///   Pass <c>True</c> to embed version information, <c>False</c> to omit it.
    /// </param>
    /// <remarks>
    ///   When enabled, the version fields default to zeroes and empty strings.
    ///   Call <see cref="SetVersionInfo"/> afterwards to populate the actual
    ///   values. Has no effect when the output type is a COFF object file.
    /// </remarks>
    /// <seealso cref="SetVersionInfo"/>
    /// <seealso cref="AddExeIcon"/>
    procedure AddVersionInfo(const AEnable: Boolean);

    /// <summary>
    ///   Sets the product version numbers and descriptive strings that are
    ///   embedded in the Win32 VERSIONINFO resource.
    /// </summary>
    /// <param name="AMajor">
    ///   Major version number (e.g. <c>1</c>).
    /// </param>
    /// <param name="AMinor">
    ///   Minor version number (e.g. <c>0</c>).
    /// </param>
    /// <param name="APatch">
    ///   Patch or build number (e.g. <c>3</c>).
    /// </param>
    /// <param name="AProductName">
    ///   The product name shown in file properties (e.g. <c>'Tiger Compiler'</c>).
    /// </param>
    /// <param name="ADescription">
    ///   A short file description (e.g. <c>'Tiger native code compiler'</c>).
    /// </param>
    /// <param name="AFilename">
    ///   The original filename string (e.g. <c>'tiger.exe'</c>).
    /// </param>
    /// <param name="ACompanyName">
    ///   The company or publisher name (e.g. <c>'tinyBigGAMES LLC'</c>).
    /// </param>
    /// <param name="ACopyright">
    ///   The legal copyright notice (e.g. <c>'Copyright © 2025 tinyBigGAMES'</c>).
    /// </param>
    /// <remarks>
    ///   <see cref="AddVersionInfo"/> must be called with <c>True</c> before
    ///   this method has any effect. The version triple is written into both
    ///   the fixed-info block and the StringFileInfo table.
    /// </remarks>
    /// <seealso cref="AddVersionInfo"/>
    procedure SetVersionInfo(const AMajor, AMinor, APatch: Word;
      const AProductName, ADescription, AFilename, ACompanyName,
      ACopyright: string);

    /// <summary>
    ///   Attaches an <c>.ico</c> file as the main application icon embedded
    ///   in the output executable's resources.
    /// </summary>
    /// <param name="AFilename">
    ///   Full or relative path to a Windows icon file (e.g. <c>'app.ico'</c>).
    /// </param>
    /// <remarks>
    ///   The icon is stored as a standard RT_ICON / RT_GROUP_ICON resource
    ///   so that Windows Explorer and the taskbar display it automatically.
    ///   Has no effect when the output type is a DLL or COFF object file.
    /// </remarks>
    /// <seealso cref="AddVersionInfo"/>
    procedure AddExeIcon(const AFilename: string);

  end;

implementation

{$R Tiger.ResData.res}

//==============================================================================
// TTiger
//==============================================================================

constructor TTiger.Create();
begin
  inherited;

  FErrors := TErrors.Create();
  FErrors.SetMaxErrors(100);

  FIR := TTigerIR.Create();
  FIR.SetErrors(FErrors);

  FBackend := TTigerWin64Backend.Create();
  FBackend.SetErrors(FErrors);

  FRuntime := TTigerWin64Runtime.Create();

  FStatus := Default(TCallback<TStatusCallback>);
end;

destructor TTiger.Destroy();
begin
  FRuntime.Free();
  FBackend.Free();
  FIR.Free();
  FErrors.Free();

  inherited;
end;

procedure TTiger.ApplyPostBuildResources(const AExePath: string);
var
  LIconPath: string;
  LIsExe: Boolean;
  LIsDll: Boolean;
begin
  LIsExe := AExePath.EndsWith('.exe', True);
  LIsDll := AExePath.EndsWith('.dll', True);

  // Only applies to EXE and DLL files
  if not LIsExe and not LIsDll then
    Exit;

  // 1. Add manifest (EXE only)
  if LIsExe then
  begin
    if TWin64Utils.ResourceExist('EXE_MANIFEST') then
    begin
      if not TWin64Utils.AddResManifestFromResource('EXE_MANIFEST', AExePath) then
        FErrors.Add(esWarning, 'W980', 'Failed to add manifest to executable')
      else
        FBackend.Status('Added application manifest');
    end;
  end;

  // 2. Add icon if specified (EXE only)
  if LIsExe and (FExeIcon <> '') then
  begin
    try
      LIconPath := FExeIcon;
      (*
      // Resolve relative paths against source file directory
      if not TPath.IsPathRooted(LIconPath) then
        LIconPath := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(FSourceFile), LIconPath));
      *)
      if TFile.Exists(LIconPath) then
      begin
        TWin64Utils.UpdateIconResource(AExePath, LIconPath);
        FBackend.Status('Added icon: %s', [LIconPath.Replace('\', '/')]);
      end
      else
        FErrors.Add(esWarning, 'W982', Format('Icon file not found: %s', [LIconPath]));
    except
      on E: Exception do
        FErrors.Add(esWarning, 'W981', Format('Failed to add icon: %s', [E.Message]));
    end;
  end;

  // 3. Add version info if enabled (EXE and DLL)
  if FAddVersionInfo then
  begin
    try
      TWin64Utils.UpdateVersionInfoResource(
        AExePath,
        FVIMajor,
        FVIMinor,
        FVIPatch,
        FVIProductName,
        FVIDescription,
        FVIFilename,
        FVICompanyName,
        FVICopyright
      );
      FBackend.Status('Added version info: %d.%d.%d', [FVIMajor, FVIMinor, FVIPatch]);
    except
      on E: Exception do
        FErrors.Add(esWarning, 'W983', Format('Failed to add version info: %s', [E.Message]));
    end;
  end;
end;

//------------------------------------------------------------------------------
// Target Configuration
//------------------------------------------------------------------------------

function TTiger.TargetExe(const APath: string;
  const ASubsystem: TTigerSubsystem): TTiger;
begin
  FBackend.TargetExe(APath, ASubsystem);
  Result := Self;
end;

function TTiger.TargetDll(const APath: string): TTiger;
begin
  FBackend.TargetDll(APath);
  Result := Self;
end;

function TTiger.TargetLib(const APath: string): TTiger;
begin
  FBackend.TargetLib(APath);
  Result := Self;
end;

function TTiger.TargetObj(const APath: string): TTiger;
begin
  FBackend.TargetObj(APath);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Static Linking
//------------------------------------------------------------------------------

function TTiger.AddLib(const APath: string): TTiger;
begin
  FBackend.AddLib(APath);
  Result := Self;
end;

function TTiger.AddObj(const APath: string): TTiger;
begin
  FBackend.AddObj(APath);
  Result := Self;
end;

function TTiger.AddLibPath(const APath: string): TTiger;
begin
  FBackend.AddLibPath(APath);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Build & Lifecycle
//------------------------------------------------------------------------------

function TTiger.Build(const AAutoRun: Boolean; AExitCode: PCardinal): Boolean;
var
  LExitCode: Cardinal;
begin
  // Strip any previously-injected runtime, then mark user code boundary
  FIR.RestoreSnapshot();
  FIR.SaveSnapshot();

  // Inject runtime library (optimizer removes unused routines)
  FRuntime.AddAll(FIR, FBackend.GetOptimizationLevel());

  // Emit IR to the backend
  FIR.EmitTo(FBackend, FStatus.Callback, FStatus.UserData);

  // Produce the target binary
  Result := FBackend.Build();

  if Result then
  begin
    // Apply post build resources
    ApplyPostBuildResources(FBackend.GetOutputPath());

    // Autorun
    if AAutoRun then
    begin
      FBackend.Status('Running: %s', [FBackend.GetOutputPath().Replace('\', '/')]);
      LExitCode := FBackend.Run();
      FBackend.Status('Process exited with code: %d', [LExitCode]);
      if Assigned(AExitCode) then
        AExitCode^ := LExitCode;
    end;
  end;
end;

function TTiger.BuildToMemory(): TBytes;
begin
  // Strip any previously-injected runtime, then mark user code boundary
  FIR.RestoreSnapshot();
  FIR.SaveSnapshot();

  // Inject runtime library
  FRuntime.AddAll(FIR, FBackend.GetOptimizationLevel());

  // Emit IR to the backend
  FIR.EmitTo(FBackend, FStatus.Callback, FStatus.UserData);

  // Produce the binary in memory
  Result := FBackend.BuildToMemory();
end;

procedure TTiger.Reset();
begin
  FIR.Clear();
  FBackend.Clear();
  FErrors.Clear();
end;

procedure TTiger.ResetBuild();
begin
  FBackend.Clear();
  FErrors.Clear();
end;

//------------------------------------------------------------------------------
// Optimization & Diagnostics
//------------------------------------------------------------------------------

procedure TTiger.SetOptimizationLevel(const AValue: Integer);
begin
  FBackend.SetOptimizationLevel(AValue);
end;

function TTiger.GetOptimizationLevel(): Integer;
begin
  Result := FBackend.GetOptimizationLevel();
end;

procedure TTiger.SetDumpIR(const AValue: Boolean);
begin
  FBackend.SetDumpIR(AValue);
end;

function TTiger.GetDumpIR(): Boolean;
begin
  Result := FBackend.GetDumpIR();
end;

function TTiger.GetSSADump(): string;
begin
  Result := FBackend.GetSSADump();
end;

//------------------------------------------------------------------------------
// Status Callback
//------------------------------------------------------------------------------

procedure TTiger.SetStatusCallback(const ACallback: TStatusCallback;
  const AUserData: Pointer);
begin
  FStatus.Callback := ACallback;
  FStatus.UserData := AUserData;
  FBackend.SetStatusCallback(ACallback, AUserData);
end;

//------------------------------------------------------------------------------
// Error Management
//------------------------------------------------------------------------------

procedure TTiger.SetMaxErrors(const AValue: Integer);
begin
  FErrors.SetMaxErrors(AValue);
end;

function TTiger.GetMaxErrors(): Integer;
begin
  Result := FErrors.GetMaxErrors();
end;

function TTiger.HasErrors(): Boolean;
begin
  Result := FErrors.HasErrors();
end;

function TTiger.HasWarnings(): Boolean;
begin
  Result := FErrors.HasWarnings();
end;

function TTiger.HasHints(): Boolean;
begin
  Result := FErrors.HasHints();
end;

function TTiger.HasFatal(): Boolean;
begin
  Result := FErrors.HasFatal();
end;

function TTiger.ErrorCount(): Integer;
begin
  Result := FErrors.ErrorCount();
end;

function TTiger.WarningCount(): Integer;
begin
  Result := FErrors.WarningCount();
end;

function TTiger.GetErrorItems(): TList<TError>;
begin
  Result := FErrors.GetItems();
end;

function TTiger.GetErrorText(): string;
var
  LBuilder: TStringBuilder;
  LI: Integer;
begin
  if FErrors.Count() = 0 then
    Exit('');

  LBuilder := TStringBuilder.Create();
  try
    for LI := 0 to FErrors.GetItems().Count - 1 do
    begin
      if LI > 0 then
        LBuilder.AppendLine();
      LBuilder.Append(FErrors.GetItems()[LI].ToFullString());
    end;
    Result := LBuilder.ToString();
  finally
    LBuilder.Free();
  end;
end;

//------------------------------------------------------------------------------
// Imports
//------------------------------------------------------------------------------

function TTiger.ImportDll(const ADllName: string; const AFuncName: string;
  const AParams: array of TTigerValueType; const AReturn: TTigerValueType;
  const AVarArgs: Boolean; const ALinkage: TTigerLinkage): TTiger;
begin
  FIR.Import(ADllName, AFuncName, AParams, AReturn, AVarArgs, ALinkage);
  Result := Self;
end;

function TTiger.ImportLib(const ALibName: string; const AFuncName: string;
  const AParams: array of TTigerValueType; const AReturn: TTigerValueType;
  const AVarArgs: Boolean; const ALinkage: TTigerLinkage): TTiger;
begin
  FIR.ImportLib(ALibName, AFuncName, AParams, AReturn, AVarArgs, ALinkage);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Global Variables
//------------------------------------------------------------------------------

function TTiger.Global(const AName: string; const AType: TTigerValueType): TTiger;
begin
  FIR.Global(AName, AType);
  Result := Self;
end;

function TTiger.Global(const AName: string; const AType: TTigerValueType;
  const AInit: TTigerExpr): TTiger;
begin
  FIR.Global(AName, AType, AInit);
  Result := Self;
end;

function TTiger.Global(const AName: string; const ATypeRef: TTigerTypeRef): TTiger;
begin
  FIR.Global(AName, ATypeRef);
  Result := Self;
end;

function TTiger.Global(const AName: string; const ATypeName: string): TTiger;
begin
  FIR.Global(AName, ATypeName);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Type Definitions — Records
//------------------------------------------------------------------------------

function TTiger.DefineRecord(const AName: string; const AIsPacked: Boolean;
  const AExplicitAlign: Integer; const ABaseTypeName: string): TTiger;
begin
  FIR.DefineRecord(AName, AIsPacked, AExplicitAlign, ABaseTypeName);
  Result := Self;
end;

function TTiger.BeginRecord(): TTiger;
begin
  FIR.BeginRecord();
  Result := Self;
end;

function TTiger.Field(const AName: string; const AType: TTigerValueType): TTiger;
begin
  FIR.Field(AName, AType);
  Result := Self;
end;

function TTiger.Field(const AName: string; const ATypeName: string): TTiger;
begin
  FIR.Field(AName, ATypeName);
  Result := Self;
end;

function TTiger.BitField(const AName: string; const AType: TTigerValueType;
  const ABitWidth: Integer): TTiger;
begin
  FIR.BitField(AName, AType, ABitWidth);
  Result := Self;
end;

function TTiger.EndRecord(): TTiger;
begin
  FIR.EndRecord();
  Result := Self;
end;

function TTiger.DefineUnion(const AName: string): TTiger;
begin
  FIR.DefineUnion(AName);
  Result := Self;
end;

function TTiger.BeginUnion(): TTiger;
begin
  FIR.BeginUnion();
  Result := Self;
end;

function TTiger.EndUnion(): TTiger;
begin
  FIR.EndUnion();
  Result := Self;
end;

function TTiger.DefineArray(const AName: string;
  const AElementType: TTigerValueType; const ALowBound: Integer;
  const AHighBound: Integer): TTiger;
begin
  FIR.DefineArray(AName, AElementType, ALowBound, AHighBound);
  Result := Self;
end;

function TTiger.DefineArray(const AName: string;
  const AElementTypeName: string; const ALowBound: Integer;
  const AHighBound: Integer): TTiger;
begin
  FIR.DefineArray(AName, AElementTypeName, ALowBound, AHighBound);
  Result := Self;
end;

function TTiger.DefineDynArray(const AName: string;
  const AElementType: TTigerValueType): TTiger;
begin
  FIR.DefineDynArray(AName, AElementType);
  Result := Self;
end;

function TTiger.DefineDynArray(const AName: string;
  const AElementTypeName: string): TTiger;
begin
  FIR.DefineDynArray(AName, AElementTypeName);
  Result := Self;
end;

function TTiger.DefineEnum(const AName: string): TTiger;
begin
  FIR.DefineEnum(AName);
  Result := Self;
end;

function TTiger.EnumValue(const AName: string): TTiger;
begin
  FIR.EnumValue(AName);
  Result := Self;
end;

function TTiger.EnumValue(const AName: string; const AOrdinal: Int64): TTiger;
begin
  FIR.EnumValue(AName, AOrdinal);
  Result := Self;
end;

function TTiger.EndEnum(): TTiger;
begin
  FIR.EndEnum();
  Result := Self;
end;

function TTiger.DefineAlias(const AName: string; const AType: TTigerValueType): TTiger;
begin
  FIR.DefineAlias(AName, AType);
  Result := Self;
end;

function TTiger.DefineAlias(const AName: string; const ATypeName: string): TTiger;
begin
  FIR.DefineAlias(AName, ATypeName);
  Result := Self;
end;

function TTiger.DefinePointer(const AName: string): TTiger;
begin
  FIR.DefinePointer(AName);
  Result := Self;
end;

function TTiger.DefinePointer(const AName: string;
  const APointeeType: TTigerValueType; const AIsConst: Boolean): TTiger;
begin
  FIR.DefinePointer(AName, APointeeType, AIsConst);
  Result := Self;
end;

function TTiger.DefinePointer(const AName: string;
  const APointeeTypeName: string; const AIsConst: Boolean): TTiger;
begin
  FIR.DefinePointer(AName, APointeeTypeName, AIsConst);
  Result := Self;
end;

function TTiger.DefineRoutine(const AName: string;
  const ALinkage: TTigerLinkage): TTiger;
begin
  FIR.DefineRoutine(AName, ALinkage);
  Result := Self;
end;

function TTiger.RoutineParam(const AType: TTigerValueType): TTiger;
begin
  FIR.RoutineParam(AType);
  Result := Self;
end;

function TTiger.RoutineParam(const ATypeName: string): TTiger;
begin
  FIR.RoutineParam(ATypeName);
  Result := Self;
end;

function TTiger.RoutineReturns(const AType: TTigerValueType): TTiger;
begin
  FIR.RoutineReturns(AType);
  Result := Self;
end;

function TTiger.RoutineReturns(const ATypeName: string): TTiger;
begin
  FIR.RoutineReturns(ATypeName);
  Result := Self;
end;

function TTiger.RoutineVarArgs(): TTiger;
begin
  FIR.RoutineVarArgs();
  Result := Self;
end;

function TTiger.EndRoutine(): TTiger;
begin
  FIR.EndRoutine();
  Result := Self;
end;

function TTiger.DefineSet(const AName: string): TTiger;
begin
  FIR.DefineSet(AName);
  Result := Self;
end;

function TTiger.DefineSet(const AName: string; const ALow: Integer;
  const AHigh: Integer): TTiger;
begin
  FIR.DefineSet(AName, ALow, AHigh);
  Result := Self;
end;

function TTiger.DefineSet(const AName: string; const AEnumTypeName: string): TTiger;
begin
  FIR.DefineSet(AName, AEnumTypeName);
  Result := Self;
end;

function TTiger.FindType(const AName: string): Integer;
begin
  Result := FIR.FindType(AName);
end;

function TTiger.GetTypeSize(const ATypeRef: TTigerTypeRef): Integer;
begin
  Result := FIR.GetTypeSize(ATypeRef);
end;

function TTiger.GetTypeAlignment(const ATypeRef: TTigerTypeRef): Integer;
begin
  Result := FIR.GetTypeAlignment(ATypeRef);
end;

function TTiger.TypeRef(const AName: string): TTigerTypeRef;
begin
  Result := FIR.TypeRef(AName);
end;

//------------------------------------------------------------------------------
// Function Definition
//------------------------------------------------------------------------------

function TTiger.Func(const AName: string;
  const AReturnType: TTigerValueType; const AIsEntryPoint: Boolean;
  const ALinkage: TTigerLinkage; const AIsPublic: Boolean): TTiger;
begin
  FIR.Func(AName, AReturnType, AIsEntryPoint, ALinkage, AIsPublic);
  Result := Self;
end;

function TTiger.OverloadFunc(const AName: string;
  const AReturnType: TTigerValueType; const AIsEntryPoint: Boolean;
  const AIsPublic: Boolean): TTiger;
begin
  FIR.OverloadFunc(AName, AReturnType, AIsEntryPoint, AIsPublic);
  Result := Self;
end;

function TTiger.VariadicFunc(const AName: string;
  const AReturnType: TTigerValueType; const AIsEntryPoint: Boolean;
  const AIsPublic: Boolean): TTiger;
begin
  FIR.VariadicFunc(AName, AReturnType, AIsEntryPoint, AIsPublic);
  Result := Self;
end;

function TTiger.DllMain(): TTiger;
begin
  FIR.DllMain();
  Result := Self;
end;

function TTiger.Param(const AName: string; const AType: TTigerValueType): TTiger;
begin
  FIR.Param(AName, AType);
  Result := Self;
end;

function TTiger.Local(const AName: string; const AType: TTigerValueType): TTiger;
begin
  FIR.Local(AName, AType);
  Result := Self;
end;

function TTiger.Local(const AName: string; const ATypeName: string): TTiger;
begin
  FIR.Local(AName, ATypeName);
  Result := Self;
end;

function TTiger.EndFunc(): TTiger;
begin
  FIR.EndFunc();
  Result := Self;
end;

//------------------------------------------------------------------------------
// Statements
//------------------------------------------------------------------------------

function TTiger.Assign(const ADest: string; const AValue: TTigerExpr): TTiger;
begin
  FIR.Assign(ADest, AValue);
  Result := Self;
end;

function TTiger.AssignTo(const ADest: TTigerExpr;
  const AValue: TTigerExpr): TTiger;
begin
  FIR.AssignTo(ADest, AValue);
  Result := Self;
end;

function TTiger.Call(const AFuncName: string): TTiger;
begin
  FIR.Call(AFuncName);
  Result := Self;
end;

function TTiger.Call(const AFuncName: string;
  const AArgs: array of TTigerExpr): TTiger;
begin
  FIR.Call(AFuncName, AArgs);
  Result := Self;
end;

function TTiger.CallAssign(const ADest: string; const AFuncName: string;
  const AArgs: array of TTigerExpr): TTiger;
begin
  FIR.CallAssign(ADest, AFuncName, AArgs);
  Result := Self;
end;

function TTiger.Return(): TTiger;
begin
  FIR.Return();
  Result := Self;
end;

function TTiger.Return(const AValue: TTigerExpr): TTiger;
begin
  FIR.Return(AValue);
  Result := Self;
end;

function TTiger.CallIndirect(const AFuncPtr: TTigerExpr): TTiger;
begin
  FIR.CallIndirect(AFuncPtr);
  Result := Self;
end;

function TTiger.CallIndirect(const AFuncPtr: TTigerExpr;
  const AArgs: array of TTigerExpr): TTiger;
begin
  FIR.CallIndirect(AFuncPtr, AArgs);
  Result := Self;
end;

function TTiger.CallIndirectAssign(const ADest: string;
  const AFuncPtr: TTigerExpr; const AArgs: array of TTigerExpr): TTiger;
begin
  FIR.CallIndirectAssign(ADest, AFuncPtr, AArgs);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Control Flow
//------------------------------------------------------------------------------

function TTiger.&If(const ACond: TTigerExpr): TTiger;
begin
  FIR.&If(ACond);
  Result := Self;
end;

function TTiger.&Else(): TTiger;
begin
  FIR.&Else();
  Result := Self;
end;

function TTiger.EndIf(): TTiger;
begin
  FIR.EndIf();
  Result := Self;
end;

function TTiger.&While(const ACond: TTigerExpr): TTiger;
begin
  FIR.&While(ACond);
  Result := Self;
end;

function TTiger.EndWhile(): TTiger;
begin
  FIR.EndWhile();
  Result := Self;
end;

function TTiger.&For(const AVar: string; const AFrom: TTigerExpr;
  const ATo: TTigerExpr): TTiger;
begin
  FIR.&For(AVar, AFrom, ATo);
  Result := Self;
end;

function TTiger.ForDownTo(const AVar: string; const AFrom: TTigerExpr;
  const ATo: TTigerExpr): TTiger;
begin
  FIR.ForDownTo(AVar, AFrom, ATo);
  Result := Self;
end;

function TTiger.EndFor(): TTiger;
begin
  FIR.EndFor();
  Result := Self;
end;

function TTiger.&Repeat(): TTiger;
begin
  FIR.&Repeat();
  Result := Self;
end;

function TTiger.&Until(const ACond: TTigerExpr): TTiger;
begin
  FIR.&Until(ACond);
  Result := Self;
end;

function TTiger.&Case(const ASelector: TTigerExpr): TTiger;
begin
  FIR.&Case(ASelector);
  Result := Self;
end;

function TTiger.CaseOf(const AValues: array of TTigerExpr): TTiger;
begin
  FIR.CaseOf(AValues);
  Result := Self;
end;

function TTiger.CaseOf(const AValues: array of Integer): TTiger;
begin
  FIR.CaseOf(AValues);
  Result := Self;
end;

function TTiger.CaseElse(): TTiger;
begin
  FIR.CaseElse();
  Result := Self;
end;

function TTiger.EndCase(): TTiger;
begin
  FIR.EndCase();
  Result := Self;
end;

//------------------------------------------------------------------------------
// Exception Handling
//------------------------------------------------------------------------------

function TTiger.&Try(): TTiger;
begin
  FIR.&Try();
  Result := Self;
end;

function TTiger.&Except(): TTiger;
begin
  FIR.&Except();
  Result := Self;
end;

function TTiger.&Finally(): TTiger;
begin
  FIR.&Finally();
  Result := Self;
end;

function TTiger.EndTry(): TTiger;
begin
  FIR.EndTry();
  Result := Self;
end;

function TTiger.&Raise(const AMsg: TTigerExpr): TTiger;
begin
  FIR.&Raise(AMsg);
  Result := Self;
end;

function TTiger.RaiseCode(const ACode: TTigerExpr;
  const AMsg: TTigerExpr): TTiger;
begin
  FIR.RaiseCode(ACode, AMsg);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Increment / Decrement
//------------------------------------------------------------------------------

function TTiger.&Inc(const AVarName: string): TTiger;
begin
  FIR.&Inc(AVarName);
  Result := Self;
end;

function TTiger.&Inc(const AVarName: string; const AAmount: TTigerExpr): TTiger;
begin
  FIR.&Inc(AVarName, AAmount);
  Result := Self;
end;

function TTiger.&Dec(const AVarName: string): TTiger;
begin
  FIR.&Dec(AVarName);
  Result := Self;
end;

function TTiger.&Dec(const AVarName: string; const AAmount: TTigerExpr): TTiger;
begin
  FIR.&Dec(AVarName, AAmount);
  Result := Self;
end;

//------------------------------------------------------------------------------
// Expressions — Literals
//------------------------------------------------------------------------------

function TTiger.Str(const AValue: string): TTigerExpr;
begin
  Result := FIR.Str(AValue);
end;

function TTiger.WStr(const AValue: string): TTigerExpr;
begin
  Result := FIR.WStr(AValue);
end;

function TTiger.Int64(const AValue: Int64): TTigerExpr;
begin
  Result := FIR.Int64(AValue);
end;

function TTiger.Int32(const AValue: Int32): TTigerExpr;
begin
  Result := FIR.Int32(AValue);
end;

function TTiger.Float64(const AValue: Double): TTigerExpr;
begin
  Result := FIR.Float64(AValue);
end;

function TTiger.Bool(const AValue: Boolean): TTigerExpr;
begin
  Result := FIR.Bool(AValue);
end;

function TTiger.Null(): TTigerExpr;
begin
  Result := FIR.Null();
end;

function TTiger.Int8(const AValue: Int8): TTigerExpr;
begin
  Result := FIR.Int8(AValue);
end;

function TTiger.Int16(const AValue: Int16): TTigerExpr;
begin
  Result := FIR.Int16(AValue);
end;

function TTiger.UInt8(const AValue: UInt8): TTigerExpr;
begin
  Result := FIR.UInt8(AValue);
end;

function TTiger.UInt16(const AValue: UInt16): TTigerExpr;
begin
  Result := FIR.UInt16(AValue);
end;

function TTiger.UInt32(const AValue: UInt32): TTigerExpr;
begin
  Result := FIR.UInt32(AValue);
end;

function TTiger.UInt64(const AValue: UInt64): TTigerExpr;
begin
  Result := FIR.UInt64(AValue);
end;

function TTiger.Float32(const AValue: Single): TTigerExpr;
begin
  Result := FIR.Float32(AValue);
end;

//------------------------------------------------------------------------------
// Expressions — Variable Reference
//------------------------------------------------------------------------------

function TTiger.Get(const AName: string): TTigerExpr;
begin
  Result := FIR.Get(AName);
end;

//------------------------------------------------------------------------------
// Expressions — Arithmetic
//------------------------------------------------------------------------------

function TTiger.Add(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Add(ALeft, ARight);
end;

function TTiger.Sub(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Sub(ALeft, ARight);
end;

function TTiger.Mul(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Mul(ALeft, ARight);
end;

function TTiger.IDiv(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.IDiv(ALeft, ARight);
end;

function TTiger.IMod(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.IMod(ALeft, ARight);
end;

function TTiger.Neg(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Neg(AValue);
end;

//------------------------------------------------------------------------------
// Expressions — Bitwise
//------------------------------------------------------------------------------

function TTiger.BitAnd(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.BitAnd(ALeft, ARight);
end;

function TTiger.BitOr(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.BitOr(ALeft, ARight);
end;

function TTiger.BitXor(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.BitXor(ALeft, ARight);
end;

function TTiger.BitNot(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.BitNot(AValue);
end;

function TTiger.&Shl(const AValue: TTigerExpr; const ACount: TTigerExpr): TTigerExpr;
begin
  Result := FIR.&Shl(AValue, ACount);
end;

function TTiger.&Shr(const AValue: TTigerExpr; const ACount: TTigerExpr): TTigerExpr;
begin
  Result := FIR.&Shr(AValue, ACount);
end;

//------------------------------------------------------------------------------
// Expressions — Comparison
//------------------------------------------------------------------------------

function TTiger.Eq(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Eq(ALeft, ARight);
end;

function TTiger.Ne(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Ne(ALeft, ARight);
end;

function TTiger.Lt(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Lt(ALeft, ARight);
end;

function TTiger.Le(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Le(ALeft, ARight);
end;

function TTiger.Gt(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Gt(ALeft, ARight);
end;

function TTiger.Ge(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Ge(ALeft, ARight);
end;

//------------------------------------------------------------------------------
// Expressions — Logical
//------------------------------------------------------------------------------

function TTiger.LogAnd(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.LogAnd(ALeft, ARight);
end;

function TTiger.LogOr(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.LogOr(ALeft, ARight);
end;

function TTiger.LogNot(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.LogNot(AValue);
end;

//------------------------------------------------------------------------------
// Expressions — Pointers
//------------------------------------------------------------------------------

function TTiger.AddrOf(const AName: string): TTigerExpr;
begin
  Result := FIR.AddrOf(AName);
end;

function TTiger.AddrOfVal(const AExpr: TTigerExpr): TTigerExpr;
begin
  Result := FIR.AddrOfVal(AExpr);
end;

function TTiger.Deref(const APtr: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Deref(APtr);
end;

function TTiger.Deref(const APtr: TTigerExpr; const ATypeName: string): TTigerExpr;
begin
  Result := FIR.Deref(APtr, ATypeName);
end;

function TTiger.Deref(const APtr: TTigerExpr; const AType: TTigerValueType): TTigerExpr;
begin
  Result := FIR.Deref(APtr, AType);
end;

//------------------------------------------------------------------------------
// Expressions — Function Pointers
//------------------------------------------------------------------------------

function TTiger.FuncAddr(const AFuncName: string): TTigerExpr;
begin
  Result := FIR.FuncAddr(AFuncName);
end;

function TTiger.InvokeIndirect(const AFuncPtr: TTigerExpr;
  const AArgs: array of TTigerExpr): TTigerExpr;
begin
  Result := FIR.InvokeIndirect(AFuncPtr, AArgs);
end;

//------------------------------------------------------------------------------
// Expressions — Composite Access
//------------------------------------------------------------------------------

function TTiger.GetField(const AObject: TTigerExpr;
  const AFieldName: string): TTigerExpr;
begin
  Result := FIR.GetField(AObject, AFieldName);
end;

function TTiger.GetIndex(const AArray: TTigerExpr;
  const AIndex: TTigerExpr): TTigerExpr;
begin
  Result := FIR.GetIndex(AArray, AIndex);
end;

//------------------------------------------------------------------------------
// Expressions — Function Call
//------------------------------------------------------------------------------

function TTiger.Invoke(const AFuncName: string;
  const AArgs: array of TTigerExpr): TTigerExpr;
begin
  Result := FIR.Invoke(AFuncName, AArgs);
end;

//------------------------------------------------------------------------------
// Expressions — Exception Intrinsics
//------------------------------------------------------------------------------

function TTiger.ExcCode(): TTigerExpr;
begin
  Result := FIR.ExcCode();
end;

function TTiger.ExcMsg(): TTigerExpr;
begin
  Result := FIR.ExcMsg();
end;

//------------------------------------------------------------------------------
// Expressions — Set Literals & Operations
//------------------------------------------------------------------------------

function TTiger.SetLit(const ATypeName: string;
  const AElements: array of Integer): TTigerExpr;
begin
  Result := FIR.SetLit(ATypeName, AElements);
end;

function TTiger.SetLitRange(const ATypeName: string;
  const ALow: Integer; const AHigh: Integer): TTigerExpr;
begin
  Result := FIR.SetLitRange(ATypeName, ALow, AHigh);
end;

function TTiger.EmptySet(const ATypeName: string): TTigerExpr;
begin
  Result := FIR.EmptySet(ATypeName);
end;

function TTiger.SetUnion(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetUnion(ALeft, ARight);
end;

function TTiger.SetDiff(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetDiff(ALeft, ARight);
end;

function TTiger.SetInter(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetInter(ALeft, ARight);
end;

function TTiger.SetIn(const AElement: TTigerExpr; const ASet: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetIn(AElement, ASet);
end;

function TTiger.SetEq(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetEq(ALeft, ARight);
end;

function TTiger.SetNe(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetNe(ALeft, ARight);
end;

function TTiger.SetSubset(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetSubset(ALeft, ARight);
end;

function TTiger.SetSuperset(const ALeft: TTigerExpr; const ARight: TTigerExpr): TTigerExpr;
begin
  Result := FIR.SetSuperset(ALeft, ARight);
end;

//------------------------------------------------------------------------------
// Expressions — Compile-Time Intrinsics
//------------------------------------------------------------------------------

function TTiger.&SizeOf(const ATypeName: string): TTigerExpr;
begin
  Result := FIR.&SizeOf(ATypeName);
end;

function TTiger.AlignOf(const ATypeName: string): TTigerExpr;
begin
  Result := FIR.AlignOf(ATypeName);
end;

function TTiger.High(const ATypeName: string): TTigerExpr;
begin
  Result := FIR.High(ATypeName);
end;

function TTiger.Low(const ATypeName: string): TTigerExpr;
begin
  Result := FIR.Low(ATypeName);
end;

function TTiger.Len(const ATypeName: string): TTigerExpr;
begin
  Result := FIR.Len(ATypeName);
end;

//------------------------------------------------------------------------------
// Expressions — Runtime Intrinsics
//------------------------------------------------------------------------------

function TTiger.Ord(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Ord(AValue);
end;

function TTiger.Chr(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Chr(AValue);
end;

function TTiger.Succ(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Succ(AValue);
end;

function TTiger.Pred(const AValue: TTigerExpr): TTigerExpr;
begin
  Result := FIR.Pred(AValue);
end;

//------------------------------------------------------------------------------
// Expressions — Variadic Intrinsics
//------------------------------------------------------------------------------

function TTiger.VaCount(): TTigerExpr;
begin
  Result := FIR.VaCount();
end;

function TTiger.VaArg(const AIndex: TTigerExpr;
  const AType: TTigerValueType): TTigerExpr;
begin
  Result := FIR.VaArg(AIndex, AType);
end;

//------------------------------------------------------------------------------
// Advanced — Direct Access
//------------------------------------------------------------------------------

function TTiger.GetIR(): TTigerIR;
begin
  Result := FIR;
end;

function TTiger.GetBackend(): TTigerBackend;
begin
  Result := FBackend;
end;

function TTiger.GetErrors(): TErrors;
begin
  Result := FErrors;
end;

function TTiger.GetTypeCount(): Integer;
begin
  Result := FIR.GetTypeCount();
end;

function TTiger.GetFieldOffset(const ATypeName: string; const AFieldName: string): Integer;
var
  LTypeIndex: Integer;
  LFieldInfo: TTigerIR.TIRRecordField;
begin
  LTypeIndex := FindType(ATypeName);
  if LTypeIndex < 0 then
    Exit(-1);

  if FIR.FindRecordField(LTypeIndex, AFieldName, LFieldInfo) then
    Result := LFieldInfo.FieldOffset
  else
    Result := -1;
end;

//------------------------------------------------------------------------------
// VersionInfo
//------------------------------------------------------------------------------
procedure TTiger.AddVersionInfo(const AEnable: Boolean);
begin
  FAddVersionInfo := AEnable;
end;

procedure TTiger.SetVersionInfo(const AMajor, AMinor, APatch: Word;
  const AProductName, ADescription, AFilename, ACompanyName,
  ACopyright: string);
begin
  Self.FVIMajor := AMajor;
  Self.FVIMinor := AMinor;
  Self.FVIPatch := APatch;
  Self.FVIProductName := AProductName;
  Self.FVIDescription := ADescription;
  Self.FVIFilename := AFilename;
  Self.FVICompanyName := ACompanyName;
  Self.FVICopyright := ACopyright;
end;

procedure TTiger.AddExeIcon(const AFilename: string);
begin
  Self.FExeIcon := AFilename;
end;



end.