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
  Tiger.Backend.X64;

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

implementation

end.
