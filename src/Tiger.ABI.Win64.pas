{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.ABI.Win64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Backend.X64;

const
  //============================================================================
  // Win64 Calling Convention (Microsoft x64)
  //============================================================================

  // Integer/pointer argument registers (in order)
  WIN64_ARG_REGS: array[0..3] of Byte = (REG_RCX, REG_RDX, REG_R8, REG_R9);
  WIN64_MAX_REG_ARGS = 4;

  // Shadow space: 32 bytes mandatory for all calls
  WIN64_SHADOW_SPACE = 32;

  // Callee-saved registers (must be preserved across calls)
  WIN64_CALLEE_SAVED: array[0..7] of Byte = (
    REG_RBX, REG_RBP, REG_RDI, REG_RSI,
    REG_R12, REG_R13, REG_R14, REG_R15
  );

  // Stack alignment requirement
  WIN64_STACK_ALIGN = 16;

implementation

end.
