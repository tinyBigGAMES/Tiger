{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.ExitCodes;

{$I Tiger.Defines.inc}

interface

{ Conventional exit/status codes returned by TTigerBackend.Run and helpers.
  Values match common Win32 ERROR_* numbers for historical consistency; they are
  not OS errno values on POSIX hosts. }

const
  Tiger_ErrorBadFormat   = 11;  // was ERROR_BAD_FORMAT
  Tiger_ErrorFileNotFound = 2;  // was ERROR_FILE_NOT_FOUND

implementation

end.
