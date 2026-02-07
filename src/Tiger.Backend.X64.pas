{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend.X64;

{$I Tiger.Defines.inc}

interface

const
  //============================================================================
  // x86-64 Register Encodings (shared across all x64 platforms)
  //============================================================================
  REG_RAX = 0; REG_RCX = 1; REG_RDX = 2; REG_RBX = 3;
  REG_RSP = 4; REG_RBP = 5; REG_RSI = 6; REG_RDI = 7;
  REG_R8  = 8; REG_R9  = 9; REG_R10 = 10; REG_R11 = 11;
  REG_R12 = 12; REG_R13 = 13; REG_R14 = 14; REG_R15 = 15;

  //============================================================================
  // x86-64 Unwind Codes (used by Win64 SEH and potentially DWARF)
  //============================================================================
  UNW_FLAG_NHANDLER  = 0;
  UNW_FLAG_EHANDLER  = 1;
  UNW_FLAG_UHANDLER  = 2;
  UNW_FLAG_CHAININFO = 4;

  UWOP_PUSH_NONVOL   = 0;
  UWOP_ALLOC_LARGE   = 1;
  UWOP_ALLOC_SMALL   = 2;
  UWOP_SET_FPREG     = 3;

  // Standard unwind info for functions with push rbp; mov rbp, rsp prologue.
  // Version=1, Flags=0, SizeOfProlog=4, CountOfCodes=1
  // UnwindCode: offset=4, op=UWOP_PUSH_NONVOL, info=RBP(5)
  STANDARD_UNWIND_INFO: array[0..7] of Byte = (
    $01,  // Version=1, Flags=0
    $04,  // SizeOfProlog=4
    $01,  // CountOfCodes=1
    $05,  // FrameRegister=RBP(5), FrameOffset=0
    $04,  // UnwindCode[0].CodeOffset = 4
    $50,  // UnwindCode[0].UnwindOp = UWOP_PUSH_NONVOL(0), OpInfo = RBP(5)
    $00, $00  // Padding to align to DWORD
  );

implementation

end.
