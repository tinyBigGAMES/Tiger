{===============================================================================
  Tiger™ Compiler Infrastructure.

  Copyright © 2025-present tinyBigGAMES™ LLC
  All Rights Reserved.

  https://github.com/tinyBigGAMES/Tiger

  See LICENSE for license information
===============================================================================}


unit Tiger.Resources;

{$I Tiger.Defines.inc}

interface

resourcestring

  //--------------------------------------------------------------------------
  // Severity Names
  //--------------------------------------------------------------------------
  RSSeverityHint = 'Hint';
  RSSeverityWarning = 'Warning';
  RSSeverityError = 'Error';
  RSSeverityFatal = 'Fatal';
  RSSeverityNote = 'Note';
  RSSeverityUnknown = 'Unknown';

  //--------------------------------------------------------------------------
  // Error Format Strings
  //--------------------------------------------------------------------------
  RSErrorFormatSimple = '%s %s: %s';
  RSErrorFormatWithLocation = '%s: %s %s: %s';
  RSErrorFormatRelatedSimple = '  %s: %s';
  RSErrorFormatRelatedWithLocation = '  %s: %s: %s';

  //--------------------------------------------------------------------------
  // SSA Errors (S001-S099)
  //--------------------------------------------------------------------------
  RSSSAUnknownFunction = 'Unknown function: ''%s''';


implementation

end.
