{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.ABI.LinuxARM64;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Backend.ARM64;

const
  //============================================================================
  // Linux ARM64 Calling Convention (AAPCS64 / ELF ABI for aarch64)
  // Parameters: X0-X7 (integer/pointer), stack for the rest.
  // Variadic: same as non-variadic for integer args (unlike Darwin arm64).
  // Return: X0 (or X0+X1 for 16-byte), or hidden pointer in X8 for large returns.
  // Syscall: number in X8, args X0-X5, SVC #0, result in X0.
  //============================================================================

  LINUXARM64_ARG_REGS: array[0..7] of Byte = (
    REG_X0, REG_X1, REG_X2, REG_X3,
    REG_X4, REG_X5, REG_X6, REG_X7
  );
  LINUXARM64_MAX_REG_ARGS = 8;

  LINUXARM64_SHADOW_SPACE = 0;

  LINUXARM64_CALLEE_SAVED: array[0..11] of Byte = (
    REG_X19, REG_X20, REG_X21, REG_X22,
    REG_X23, REG_X24, REG_X25, REG_X26,
    REG_X27, REG_X28, REG_X29, REG_X30
  );

  LINUXARM64_STACK_ALIGN = 16;

  LINUXARM64_HIDDEN_RETURN_REG = REG_X8;

  LINUXARM64_MAX_RETURN_REG_SIZE = 16;

  // Exception frame: PrevFrame(8) + glibc sigjmp_buf(312) + FrameType(4) -> 336 aligned
  LINUXARM64_EXCEPT_FRAME_SIZE = 336;
  LINUXARM64_JMPBUF_SIZE = 312;

  //============================================================================
  // Linux Syscall Numbers (aarch64)
  //============================================================================

  LINUXARM64_SYS_READ       = 63;
  LINUXARM64_SYS_WRITE      = 64;
  LINUXARM64_SYS_EXIT       = 93;
  LINUXARM64_SYS_EXIT_GROUP = 94;

  // Aliases for runtime IR that uses Linux x86-64 numbers in SyscallStmt
  LINUXARM64_SYS_READ_X64  = 0;
  LINUXARM64_SYS_WRITE_X64 = 1;
  LINUXARM64_SYS_EXIT_X64  = 60;

implementation

end.
