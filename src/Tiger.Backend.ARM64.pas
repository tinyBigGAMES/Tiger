{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Backend.ARM64;

{$I Tiger.Defines.inc}

interface

const
  //============================================================================
  // AArch64 General-Purpose Registers (shared across ARM64 platforms)
  // X0-X30: 64-bit; encoding 0..30. SP and ZR use encoding 31 in many insns.
  //============================================================================
  REG_X0  = 0;  REG_X1  = 1;  REG_X2  = 2;  REG_X3  = 3;
  REG_X4  = 4;  REG_X5  = 5;  REG_X6  = 6;  REG_X7  = 7;
  REG_X8  = 8;  REG_X9  = 9;  REG_X10 = 10; REG_X11 = 11;
  REG_X12 = 12; REG_X13 = 13; REG_X14 = 14; REG_X15 = 15;
  REG_X16 = 16; REG_X17 = 17; REG_X18 = 18; REG_X19 = 19;
  REG_X20 = 20; REG_X21 = 21; REG_X22 = 22; REG_X23 = 23;
  REG_X24 = 24; REG_X25 = 25; REG_X26 = 26; REG_X27 = 27;
  REG_X28 = 28; REG_X29 = 29; REG_X30 = 30;

  // Special roles (AAPCS64 / Apple ARM64)
  REG_FP = REG_X29;   // Frame pointer
  REG_LR = REG_X30;   // Link register (return address)

  // Stack pointer: in AArch64 encoding, 31 denotes SP (or ZR) depending on context
  REG_SP = 31;
  REG_ZR = 31;

/// <summary>
///   Encode the 21-bit ADRP page delta (with implicit bit 21 set). Uses
///   unsigned arithmetic so this is safe when the host compiler has $Q+ enabled.
/// </summary>
function AdrpPageImm21(const APageDelta: Int64): Cardinal;

/// <summary>Encode an ADRP instruction (imm21 split immlo/immhi).</summary>
function EncodeInsnAdrp(const AReg: Byte; const AImm21: Cardinal): Cardinal;

/// <summary>
///   Encode ADRP using the alternate immhi placement used by EmitAdrp on macOS.
/// </summary>
function EncodeInsnAdrpLo16(const AReg: Byte; const AImm21: Cardinal): Cardinal;

/// <summary>Round a size up to 16-byte alignment.</summary>
function AlignUp16(const AValue: Cardinal): Cardinal;

/// <summary>
///   Combine a 32-bit instruction word from a base opcode and optional
///   operand fragments using unsigned OR (safe with host $Q+).
/// </summary>
function InsnMerge(const ABase: Cardinal; const AExtra: array of Cardinal): Cardinal;

/// <summary>Register number shifted left for AArch64 instruction encoding.</summary>
function RegShl(const AReg: Byte; const AShift: Byte): Cardinal;

implementation

function InsnMerge(const ABase: Cardinal; const AExtra: array of Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := ABase;
  for I := 0 to High(AExtra) do
    Result := Result or AExtra[I];
end;

function RegShl(const AReg: Byte; const AShift: Byte): Cardinal;
begin
  Result := Cardinal(AReg) shl AShift;
end;

function AdrpPageImm21(const APageDelta: Int64): Cardinal;
begin
  Result := (Cardinal(Int32(APageDelta) and $1FFFFF) + (1 shl 21)) and $1FFFFF;
end;

function EncodeInsnAdrp(const AReg: Byte; const AImm21: Cardinal): Cardinal;
begin
  Result := Cardinal($90000000) or Cardinal(AReg)
    or ((AImm21 and 3) shl 29)
    or (((AImm21 shr 2) and $7FFFF) shl 5);
end;

function EncodeInsnAdrpLo16(const AReg: Byte; const AImm21: Cardinal): Cardinal;
begin
  Result := Cardinal($90000000)
    or ((AImm21 and 3) shl 29)
    or ((AImm21 shr 2) shl 16)
    or Cardinal(AReg);
end;

function AlignUp16(const AValue: Cardinal): Cardinal;
begin
  Result := (AValue + 15) and (not Cardinal(15));
end;

end.
