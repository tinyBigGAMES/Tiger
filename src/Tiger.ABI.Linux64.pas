{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.ABI.Linux64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Backend.X64,
  Tiger.Types;

const
  //============================================================================
  // Linux64 Calling Convention (System V AMD64)
  //============================================================================

  // Integer/pointer argument registers (in order)
  LINUX64_ARG_REGS: array[0..5] of Byte = (
    REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9
  );
  LINUX64_MAX_REG_ARGS = 6;

  // No shadow space; System V uses a 128-byte red zone below RSP instead
  LINUX64_SHADOW_SPACE = 0;

  // Red zone: 128 bytes below RSP that leaf functions may use without
  // adjusting RSP. Interrupted by signals, so non-leaf functions must not
  // rely on it.
  LINUX64_RED_ZONE = 128;

  // Callee-saved registers (must be preserved across calls)
  // Note: RDI and RSI are NOT callee-saved (unlike Win64)
  LINUX64_CALLEE_SAVED: array[0..5] of Byte = (
    REG_RBX, REG_RBP, REG_R12, REG_R13, REG_R14, REG_R15
  );

  // Stack alignment requirement
  LINUX64_STACK_ALIGN = 16;

  //============================================================================
  // Linux Syscall Numbers (x86-64)
  //============================================================================

  LINUX_SYS_WRITE = 1;    // write(fd, buf, count)
  LINUX_SYS_EXIT  = 60;   // exit(status)

  //============================================================================
  // Register Constants for Classification Results
  //============================================================================

  SYSV_REG_NONE = $FF;  // Marker for unused register slot

type
  //============================================================================
  // System V AMD64 ABI Argument Class
  //============================================================================
  // Reference: System V AMD64 ABI, Section 3.2.3
  //
  // Each 8-byte "eightbyte" of an aggregate is classified independently:
  //   - NO_CLASS: Padding or empty - ignored in classification
  //   - INTEGER: Integral types, pointers - passed in general-purpose registers
  //   - SSE: Floating-point types - passed in XMM registers
  //   - MEMORY: Large aggregates or unaligned - passed on stack via pointer
  //============================================================================
  TSysVArgClass = (
    sacNoClass,     // Padding or empty - ignored in classification
    sacInteger,     // Integers, pointers - use RDI, RSI, RDX, RCX, R8, R9
    sacSSE,         // Float/double - use XMM0-XMM7
    sacMemory       // Passed in memory (stack or by hidden pointer)
  );

  //============================================================================
  // Eightbyte Classification Result
  //============================================================================
  // Each 8-byte chunk of an aggregate gets its own classification.
  // For types <= 16 bytes, we have at most 2 eightbytes.
  //============================================================================
  TSysVEightbyte = record
    ArgClass: TSysVArgClass;
    Size: Integer;  // Actual bytes used within this eightbyte (1-8)
  end;

  //============================================================================
  // Complete Type Classification Result
  //============================================================================
  // After classifying all eightbytes, determines final passing mechanism.
  // A type is passed in registers if:
  //   - Size <= 16 bytes
  //   - At most 2 INTEGER registers needed
  //   - At most 2 SSE registers needed (Tiger doesn't use SSE for floats yet)
  // Otherwise passed in MEMORY (by pointer for params, hidden pointer for returns).
  //============================================================================
  TSysVClassification = record
    TotalSize: Integer;
    Alignment: Integer;
    Eightbytes: array[0..1] of TSysVEightbyte;  // Max 2 for register passing
    EightbyteCount: Integer;                     // 0, 1, or 2
    FinalClass: TSysVArgClass;                   // sacMemory = by pointer
    IntegerRegsNeeded: Integer;                  // 0, 1, or 2
    SSERegsNeeded: Integer;                      // 0, 1, or 2
  end;

  //============================================================================
  // Argument Location (where a single argument ends up)
  //============================================================================
  TSysVArgLocation = record
    ArgIndex: Integer;                // Original argument index
    Classification: TSysVClassification;

    // Register assignment (if passed in registers)
    IntegerRegs: array[0..1] of Byte; // REG_RDI, etc. or SYSV_REG_NONE if unused
    SSERegs: array[0..1] of Byte;     // 0=XMM0, etc. or SYSV_REG_NONE if unused

    // Stack location (if passed on stack)
    StackOffset: Integer;             // Offset from RSP at call site

    // Flags
    IsInRegisters: Boolean;
    IsOnStack: Boolean;
    IsHiddenPointer: Boolean;         // MEMORY class struct passed by pointer
  end;

  //============================================================================
  // Return Value Location
  //============================================================================
  TSysVReturnLocation = record
    Classification: TSysVClassification;

    // Register assignment for return
    IntegerRegs: array[0..1] of Byte; // RAX, RDX or SYSV_REG_NONE
    SSERegs: array[0..1] of Byte;     // XMM0, XMM1 or SYSV_REG_NONE

    // Hidden pointer return
    UsesHiddenPointer: Boolean;       // Caller passes dest ptr in RDI
  end;

  //============================================================================
  // Complete Call Layout
  //============================================================================
  // Fully describes how to set up a System V AMD64 call
  //============================================================================
  TSysVCallLayout = record
    Args: TArray<TSysVArgLocation>;
    Return: TSysVReturnLocation;

    // Register usage summary
    IntegerRegsUsed: Integer;         // How many of RDI,RSI,RDX,RCX,R8,R9 used
    SSERegsUsed: Integer;             // How many of XMM0-XMM7 used
    StackArgsSize: Integer;           // Total stack space for args (8-byte aligned)
    HasHiddenReturnPointer: Boolean;  // RDI consumed by hidden return ptr
  end;

  //============================================================================
  // Classification Functions
  //============================================================================

  /// <summary>
  ///   Classifies a primitive type for System V AMD64 ABI.
  /// </summary>
  function SysVClassifyPrimitive(const AType: TTigerValueType): TSysVClassification;

  /// <summary>
  ///   Classifies a composite type based on size and alignment.
  ///   This is the core classification used for records, arrays, etc.
  /// </summary>
  /// <param name="ASize">Total size in bytes</param>
  /// <param name="AAlignment">Alignment requirement in bytes</param>
  /// <returns>Classification result</returns>
  function SysVClassifyBySize(const ASize: Integer; const AAlignment: Integer): TSysVClassification;

  //============================================================================
  // Call Layout Computation
  //============================================================================

  /// <summary>
  ///   Computes argument locations for a function call.
  ///   Assigns each argument to registers or stack based on classification.
  /// </summary>
  /// <param name="AArgSizes">Array of argument sizes in bytes</param>
  /// <param name="AArgAlignments">Array of argument alignments in bytes</param>
  /// <param name="AHasHiddenReturn">True if return uses hidden pointer (consumes RDI)</param>
  /// <returns>Array of argument locations</returns>
  function SysVComputeArgLocations(
    const AArgSizes: TArray<Integer>;
    const AArgAlignments: TArray<Integer>;
    const AHasHiddenReturn: Boolean
  ): TArray<TSysVArgLocation>;

  /// <summary>
  ///   Computes return value location for a function.
  /// </summary>
  /// <param name="ASize">Return type size in bytes (0 for void)</param>
  /// <param name="AAlignment">Return type alignment in bytes</param>
  /// <returns>Return location</returns>
  function SysVComputeReturnLocation(const ASize: Integer; const AAlignment: Integer): TSysVReturnLocation;

  /// <summary>
  ///   Computes complete call layout including all arguments and return.
  /// </summary>
  function SysVComputeCallLayout(
    const AArgSizes: TArray<Integer>;
    const AArgAlignments: TArray<Integer>;
    const AReturnSize: Integer;
    const AReturnAlignment: Integer
  ): TSysVCallLayout;

implementation

//==============================================================================
// SysVClassifyPrimitive
//==============================================================================

function SysVClassifyPrimitive(const AType: TTigerValueType): TSysVClassification;
begin
  Result := Default(TSysVClassification);

  case AType of
    vtVoid:
      begin
        Result.TotalSize := 0;
        Result.Alignment := 1;
        Result.EightbyteCount := 0;
        Result.FinalClass := sacNoClass;
        Result.IntegerRegsNeeded := 0;
        Result.SSERegsNeeded := 0;
      end;

    vtInt8, vtUInt8:
      begin
        Result.TotalSize := 1;
        Result.Alignment := 1;
        Result.EightbyteCount := 1;
        Result.Eightbytes[0].ArgClass := sacInteger;
        Result.Eightbytes[0].Size := 1;
        Result.FinalClass := sacInteger;
        Result.IntegerRegsNeeded := 1;
        Result.SSERegsNeeded := 0;
      end;

    vtInt16, vtUInt16:
      begin
        Result.TotalSize := 2;
        Result.Alignment := 2;
        Result.EightbyteCount := 1;
        Result.Eightbytes[0].ArgClass := sacInteger;
        Result.Eightbytes[0].Size := 2;
        Result.FinalClass := sacInteger;
        Result.IntegerRegsNeeded := 1;
        Result.SSERegsNeeded := 0;
      end;

    vtInt32, vtUInt32:
      begin
        Result.TotalSize := 4;
        Result.Alignment := 4;
        Result.EightbyteCount := 1;
        Result.Eightbytes[0].ArgClass := sacInteger;
        Result.Eightbytes[0].Size := 4;
        Result.FinalClass := sacInteger;
        Result.IntegerRegsNeeded := 1;
        Result.SSERegsNeeded := 0;
      end;

    vtInt64, vtUInt64, vtPointer:
      begin
        Result.TotalSize := 8;
        Result.Alignment := 8;
        Result.EightbyteCount := 1;
        Result.Eightbytes[0].ArgClass := sacInteger;
        Result.Eightbytes[0].Size := 8;
        Result.FinalClass := sacInteger;
        Result.IntegerRegsNeeded := 1;
        Result.SSERegsNeeded := 0;
      end;

    vtFloat32:
      begin
        // Note: Tiger currently passes floats in integer registers
        // Full SSE support would use sacSSE here
        Result.TotalSize := 4;
        Result.Alignment := 4;
        Result.EightbyteCount := 1;
        Result.Eightbytes[0].ArgClass := sacInteger;  // Using INTEGER for now
        Result.Eightbytes[0].Size := 4;
        Result.FinalClass := sacInteger;
        Result.IntegerRegsNeeded := 1;
        Result.SSERegsNeeded := 0;
      end;

    vtFloat64:
      begin
        // Note: Tiger currently passes floats in integer registers
        // Full SSE support would use sacSSE here
        Result.TotalSize := 8;
        Result.Alignment := 8;
        Result.EightbyteCount := 1;
        Result.Eightbytes[0].ArgClass := sacInteger;  // Using INTEGER for now
        Result.Eightbytes[0].Size := 8;
        Result.FinalClass := sacInteger;
        Result.IntegerRegsNeeded := 1;
        Result.SSERegsNeeded := 0;
      end;
  else
    // Unknown type - treat as pointer
    Result.TotalSize := 8;
    Result.Alignment := 8;
    Result.EightbyteCount := 1;
    Result.Eightbytes[0].ArgClass := sacInteger;
    Result.Eightbytes[0].Size := 8;
    Result.FinalClass := sacInteger;
    Result.IntegerRegsNeeded := 1;
    Result.SSERegsNeeded := 0;
  end;
end;

//==============================================================================
// SysVClassifyBySize
//==============================================================================

function SysVClassifyBySize(const ASize: Integer; const AAlignment: Integer): TSysVClassification;
begin
  Result := Default(TSysVClassification);
  Result.TotalSize := ASize;
  Result.Alignment := AAlignment;

  // Rule 1: If size > 16 bytes, pass in MEMORY
  if ASize > 16 then
  begin
    Result.EightbyteCount := 0;
    Result.FinalClass := sacMemory;
    Result.IntegerRegsNeeded := 0;
    Result.SSERegsNeeded := 0;
    Exit;
  end;

  // Rule 2: If size = 0 (empty struct), NO_CLASS
  if ASize = 0 then
  begin
    Result.EightbyteCount := 0;
    Result.FinalClass := sacNoClass;
    Result.IntegerRegsNeeded := 0;
    Result.SSERegsNeeded := 0;
    Exit;
  end;

  // Rule 3: Classify each eightbyte
  // For simplicity, Tiger treats all struct fields as INTEGER class
  // (proper implementation would examine field types for SSE classification)

  if ASize <= 8 then
  begin
    // Single eightbyte
    Result.EightbyteCount := 1;
    Result.Eightbytes[0].ArgClass := sacInteger;
    Result.Eightbytes[0].Size := ASize;
    Result.FinalClass := sacInteger;
    Result.IntegerRegsNeeded := 1;
    Result.SSERegsNeeded := 0;
  end
  else
  begin
    // Two eightbytes (9-16 bytes)
    Result.EightbyteCount := 2;
    Result.Eightbytes[0].ArgClass := sacInteger;
    Result.Eightbytes[0].Size := 8;
    Result.Eightbytes[1].ArgClass := sacInteger;
    Result.Eightbytes[1].Size := ASize - 8;
    Result.FinalClass := sacInteger;
    Result.IntegerRegsNeeded := 2;
    Result.SSERegsNeeded := 0;
  end;
end;

//==============================================================================
// SysVComputeArgLocations
//==============================================================================

function SysVComputeArgLocations(
  const AArgSizes: TArray<Integer>;
  const AArgAlignments: TArray<Integer>;
  const AHasHiddenReturn: Boolean
): TArray<TSysVArgLocation>;
var
  LI: Integer;
  LIntRegIndex: Integer;
  //LSSERegIndex: Integer;
  LStackOffset: Integer;
  LClassification: TSysVClassification;
  LLocation: TSysVArgLocation;
begin
  SetLength(Result, Length(AArgSizes));

  // Start register allocation
  // If hidden return pointer is used, RDI is consumed
  if AHasHiddenReturn then
    LIntRegIndex := 1  // Skip RDI
  else
    LIntRegIndex := 0;

  //LSSERegIndex := 0;
  LStackOffset := 0;

  for LI := 0 to High(AArgSizes) do
  begin
    LLocation := Default(TSysVArgLocation);
    LLocation.ArgIndex := LI;
    LLocation.IntegerRegs[0] := SYSV_REG_NONE;
    LLocation.IntegerRegs[1] := SYSV_REG_NONE;
    LLocation.SSERegs[0] := SYSV_REG_NONE;
    LLocation.SSERegs[1] := SYSV_REG_NONE;

    // Classify this argument
    LClassification := SysVClassifyBySize(AArgSizes[LI], AArgAlignments[LI]);
    LLocation.Classification := LClassification;

    if LClassification.FinalClass = sacMemory then
    begin
      // Large struct: pass by pointer in register (if available) or stack
      LLocation.IsHiddenPointer := True;

      if LIntRegIndex < LINUX64_MAX_REG_ARGS then
      begin
        LLocation.IsInRegisters := True;
        LLocation.IsOnStack := False;
        LLocation.IntegerRegs[0] := LINUX64_ARG_REGS[LIntRegIndex];
        Inc(LIntRegIndex);
      end
      else
      begin
        LLocation.IsInRegisters := False;
        LLocation.IsOnStack := True;
        LLocation.StackOffset := LStackOffset;
        Inc(LStackOffset, 8);  // Pointer is 8 bytes
      end;
    end
    else if LClassification.FinalClass = sacInteger then
    begin
      // Check if we have enough registers
      if LIntRegIndex + LClassification.IntegerRegsNeeded <= LINUX64_MAX_REG_ARGS then
      begin
        // Pass in registers
        LLocation.IsInRegisters := True;
        LLocation.IsOnStack := False;
        LLocation.IsHiddenPointer := False;

        if LClassification.IntegerRegsNeeded >= 1 then
        begin
          LLocation.IntegerRegs[0] := LINUX64_ARG_REGS[LIntRegIndex];
          Inc(LIntRegIndex);
        end;

        if LClassification.IntegerRegsNeeded >= 2 then
        begin
          LLocation.IntegerRegs[1] := LINUX64_ARG_REGS[LIntRegIndex];
          Inc(LIntRegIndex);
        end;
      end
      else
      begin
        // Not enough registers - pass on stack
        LLocation.IsInRegisters := False;
        LLocation.IsOnStack := True;
        LLocation.IsHiddenPointer := False;
        LLocation.StackOffset := LStackOffset;

        // Align stack offset if needed
        if (LClassification.Alignment > 8) and ((LStackOffset mod LClassification.Alignment) <> 0) then
          LStackOffset := ((LStackOffset + LClassification.Alignment - 1) div LClassification.Alignment) * LClassification.Alignment;

        LLocation.StackOffset := LStackOffset;
        Inc(LStackOffset, ((LClassification.TotalSize + 7) div 8) * 8);  // Round up to 8
      end;
    end
    else if LClassification.FinalClass = sacNoClass then
    begin
      // Empty/void - nothing to pass
      LLocation.IsInRegisters := False;
      LLocation.IsOnStack := False;
      LLocation.IsHiddenPointer := False;
    end;

    Result[LI] := LLocation;
  end;
end;

//==============================================================================
// SysVComputeReturnLocation
//==============================================================================

function SysVComputeReturnLocation(const ASize: Integer; const AAlignment: Integer): TSysVReturnLocation;
var
  LClassification: TSysVClassification;
begin
  Result := Default(TSysVReturnLocation);
  Result.IntegerRegs[0] := SYSV_REG_NONE;
  Result.IntegerRegs[1] := SYSV_REG_NONE;
  Result.SSERegs[0] := SYSV_REG_NONE;
  Result.SSERegs[1] := SYSV_REG_NONE;

  if ASize = 0 then
  begin
    // Void return
    Result.Classification := SysVClassifyBySize(0, 1);
    Result.UsesHiddenPointer := False;
    Exit;
  end;

  LClassification := SysVClassifyBySize(ASize, AAlignment);
  Result.Classification := LClassification;

  if LClassification.FinalClass = sacMemory then
  begin
    // Large struct: return via hidden pointer passed in RDI
    Result.UsesHiddenPointer := True;
    // Caller passes destination pointer in RDI
    // Function returns that pointer in RAX
    Result.IntegerRegs[0] := REG_RAX;
  end
  else if LClassification.FinalClass = sacInteger then
  begin
    // Return in RAX (and optionally RDX)
    Result.UsesHiddenPointer := False;

    if LClassification.IntegerRegsNeeded >= 1 then
      Result.IntegerRegs[0] := REG_RAX;

    if LClassification.IntegerRegsNeeded >= 2 then
      Result.IntegerRegs[1] := REG_RDX;
  end
  else
  begin
    // NO_CLASS - nothing to return
    Result.UsesHiddenPointer := False;
  end;
end;

//==============================================================================
// SysVComputeCallLayout
//==============================================================================

function SysVComputeCallLayout(
  const AArgSizes: TArray<Integer>;
  const AArgAlignments: TArray<Integer>;
  const AReturnSize: Integer;
  const AReturnAlignment: Integer
): TSysVCallLayout;
var
  LI: Integer;
begin
  Result := Default(TSysVCallLayout);

  // First determine return location (affects whether RDI is available)
  Result.Return := SysVComputeReturnLocation(AReturnSize, AReturnAlignment);
  Result.HasHiddenReturnPointer := Result.Return.UsesHiddenPointer;

  // Then compute argument locations
  Result.Args := SysVComputeArgLocations(AArgSizes, AArgAlignments, Result.HasHiddenReturnPointer);

  // Summarize register and stack usage
  Result.IntegerRegsUsed := 0;
  Result.SSERegsUsed := 0;
  Result.StackArgsSize := 0;

  // Count hidden return pointer
  if Result.HasHiddenReturnPointer then
    Inc(Result.IntegerRegsUsed);

  // Count argument usage
  for LI := 0 to High(Result.Args) do
  begin
    if Result.Args[LI].IsInRegisters then
    begin
      if Result.Args[LI].IntegerRegs[0] <> SYSV_REG_NONE then
        Inc(Result.IntegerRegsUsed);
      if Result.Args[LI].IntegerRegs[1] <> SYSV_REG_NONE then
        Inc(Result.IntegerRegsUsed);
      if Result.Args[LI].SSERegs[0] <> SYSV_REG_NONE then
        Inc(Result.SSERegsUsed);
      if Result.Args[LI].SSERegs[1] <> SYSV_REG_NONE then
        Inc(Result.SSERegsUsed);
    end
    else if Result.Args[LI].IsOnStack then
    begin
      // Track maximum stack offset used
      if Result.Args[LI].StackOffset + Result.Args[LI].Classification.TotalSize > Result.StackArgsSize then
        Result.StackArgsSize := Result.Args[LI].StackOffset + ((Result.Args[LI].Classification.TotalSize + 7) div 8) * 8;
    end;
  end;

  // Align stack args size to 16 bytes
  if (Result.StackArgsSize mod 16) <> 0 then
    Result.StackArgsSize := ((Result.StackArgsSize + 15) div 16) * 16;
end;

end.
