unit uCHXRscStr;
{<  Localizable strings unit for CHX utils.

  Copyright (C) 2017-2022 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

resourcestring
  // Common error messages.
  // ----------------------

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

  // File Dialogs
  rsFileDlgMaskDef = 'All files|*.*';
  {< All files dialogs mask}
  rsFileDlgMaskFmt = 'Supported files|%0:s|All files|*.*';
  {< Generic suported file extensions mask

     @param(%0:s Suported files extension mask "*.png;*.jpg")
  }

  rsFileNotFound = '%0:s' + LineEnding +
    'not found.';
  rsCorfirmDeleteFile = '%0:s' + LineEnding +
    'Do you want to delete this file?';
  rsErrorCreatingFile = 'Error creating:' + LineEnding +
    '%0:s';
  rsErrorDeletingFile = 'Error deleting:' + LineEnding +
    '%0:s';
  rsErrorRenamingFile = 'Error renaming:' + LineEnding +
    '%0:s' + LineEnding +
    'to:' + LineEnding +
    '%1:s';

implementation

end.
