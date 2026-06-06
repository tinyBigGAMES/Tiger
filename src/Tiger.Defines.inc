{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}


{$Z4}
{$A8}

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}

{$WARN UNIT_PLATFORM OFF}
{$WARN UNIT_DEPRECATED OFF}

{$INLINE AUTO}

{ Supported desktop hosts: Windows, Linux, or macOS, 64-bit only. }
{$IF not (
    (defined(MSWINDOWS) and (defined(CPUX64) or defined(CPUARM64))) or
    (defined(LINUX) and (defined(CPUX64) or defined(CPUARM64))) or
    ((defined(MACOS) or defined(OSX)) and (defined(CPUX64) or defined(CPUARM64)))
  )}
  {$MESSAGE Error 'Tiger: unsupported host platform (need Windows, Linux, or macOS, 64-bit)'}
{$ENDIF}
