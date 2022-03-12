unit uCHXRscStr;

{$mode objfpc}{$H+}

interface

resourcestring
  // Common error messages.
  rsCUExcNilParameter = '%0:s: %1:s parameter (%2:s) = nil';
  {< A required parameter is nil.
    @param(%0:s Method name) }
  rsCUExcAlreadyExists = '"%0:s" already exists.';
  {< Something unique already exists.
    @param(%0:s Item)}
  rsCUExcCardRange = '"%0:d" is not in cardinal range.';
  {< "10000000000000000" is not in cardinal range.}
  rsENotFilename = '%0:s: Not defined filename';
  {< We want read/write to file ''.}

  rsFileDlgMaskDef = 'All files|*.*';
  rsFileDlgMaskFmt = 'All files|*.*|Supported files|%0:s';

implementation

end.
