unit uCHXRscStr;
{<  Localizable strings unit for CHX utils.

  Not all strings are actually used by CHXPas units,

  Copyright (C) 2017-2022 Chixpy
}
{$mode objfpc}{$H+}

interface

resourcestring

  { TODO : This is a little mess... }

  // Common error messages
  // ---------------------

  rsCUExcNilParameter = '%0:s: %1:s parameter (%2:s) = nil';
  {< A required parameter is not assigned.
    @param(%0:s Method name.)
    @param(%1:s Parameter name.)
    @param(%1:s Parameter type.)
    }
  rsCUExcAlreadyExists = '"%0:s" already exists.';
  {< Something unique already exists. Valid for filenames too.
    @param(%0:s Item)}
  rsCUExcAlreadyExistsAsk = '"%0:s" already exists.' + LineEnding + 'Replace it?';
  {< Something unique already exists and ask if replace it.
    @param(%0:s Item)}
  rsCUExcCardRange = '"%0:d" is not in cardinal range.';
  {< Some number is not in cardinal range.}


  // File and folder questions
  // -------------------------


  rsENotFilename = '%0:s: Not defined filename';
  {< We want read/write to file ''.}
  rsFileNotFound = '%0:s' + LineEnding +
    'not found.';
  // rsCUExcAlreadyExists = '"%0:s" already exists.';
  rsErrorLoadingFile = 'Error loading:' + LineEnding +
    '%0:s';

  rsRenameFileCaption = 'Renaming file';
  rsRenameFile = 'Rename this file:';

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

  // Dialogs
  // -------

  rsSaveChangesCaption = 'Save changes';
  rsSaveChanges = 'Do you want to save current changes?';

  rsFileDlgMaskDef = 'All files|*.*';
  {< All files dialogs mask}
  rsFileDlgMaskFmt = 'Supported files|%0:s|All files|*.*';
  {< Generic suported file extensions mask

     @param(%0:s Suported files extension mask "*.png;*.jpg")
  }

  rsFmtItemCount = '%0:d / %0:d';

implementation

end.
{
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
