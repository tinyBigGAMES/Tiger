{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.ABI.MacOS64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Backend.ARM64;

const
  //============================================================================
  // Apple ARM64 Calling Convention (AAPCS64 with Apple platform rules)
  // Parameters: X0-X7 (integer/pointer), stack for the rest.
  // Return: X0 (or X0+X1 for 16-byte), or hidden pointer for large returns.
  //============================================================================

  // Integer/pointer argument registers (in order)
  MACOS64_ARG_REGS: array[0..7] of Byte = (
    REG_X0, REG_X1, REG_X2, REG_X3,
    REG_X4, REG_X5, REG_X6, REG_X7
  );
  MACOS64_MAX_REG_ARGS = 8;

  // No shadow space; stack args start at [SP] at call site
  MACOS64_SHADOW_SPACE = 0;

  // Callee-saved registers (must be preserved across calls)
  // X19-X28, X29 (FP), X30 (LR)
  MACOS64_CALLEE_SAVED: array[0..11] of Byte = (
    REG_X19, REG_X20, REG_X21, REG_X22,
    REG_X23, REG_X24, REG_X25, REG_X26,
    REG_X27, REG_X28, REG_X29, REG_X30
  );

  // Stack alignment requirement (AAPCS64)
  MACOS64_STACK_ALIGN = 16;

  // Large struct return: size > 16 bytes uses hidden pointer in X8 (caller passes)
  MACOS64_HIDDEN_RETURN_REG = REG_X8;

  // Maximum size (bytes) for return in X0 (or X0+X1)
  MACOS64_MAX_RETURN_REG_SIZE = 16;

implementation

end.
