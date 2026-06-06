{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}

unit Tiger.Utils.Host;

{$I Tiger.Defines.inc}

interface

uses
  Tiger.Utils
{$IFDEF MSWINDOWS}
  , Tiger.Utils.Win64
{$ELSE}
  , Tiger.Utils.Posix
{$ENDIF}
  ;

type
{$IFDEF MSWINDOWS}
  THostUtils = TWin64Utils;
{$ELSE}
  THostUtils = TPosixUtils;
{$ENDIF}

implementation

end.
