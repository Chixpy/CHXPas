unit uCHXDlgUtils;

{< Copyright (C) 2011-2020 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EditBtn, LazFileUtils, Dialogs,
  // CHX units
  uCHXStrUtils;


// TODO: These are a mess, InitialDir in dialogs needs SysPath except last one.
//   Sometimes last used dir is remembered.

procedure SetDlgInitialDir(aDialog: TFileDialog; BaseDir: string);
procedure SetDirEditInitialDir(aDirectoryEdit: TDirectoryEdit;
  BaseDir: string);
procedure SetFileEditInitialDir(aFileEdit: TFileNameEdit; BaseDir: string);


implementation

procedure SetDlgInitialDir(aDialog: TFileDialog; BaseDir: string);
begin
  if not Assigned(aDialog) then
    Exit;

  BaseDir := SysPath(SetAsFolder(BaseDir));
  aDialog.FileName := SysPath(aDialog.FileName);

  if FilenameIsAbsolute(aDialog.FileName) then
  begin
    aDialog.InitialDir :=
      ExtractFileDir(CleanAndExpandFilename(aDialog.FileName));
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := SysPath(SetAsFolder(GetCurrentDirUTF8));

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(SetAsFolder(GetCurrentDirUTF8) + BaseDir)));

    if aDialog.FileName = '' then
      aDialog.InitialDir := ExcludeTrailingPathDelimiter(BaseDir)
    else
      aDialog.InitialDir :=
        ExtractFilePath(CleanAndExpandFilename(BaseDir + aDialog.FileName));
  end;
end;

procedure SetDirEditInitialDir(aDirectoryEdit: TDirectoryEdit;
  BaseDir: string);
begin
  if not Assigned(aDirectoryEdit) then
    Exit;

  BaseDir := SysPath(SetAsFolder(BaseDir));
  aDirectoryEdit.Directory :=
    ExcludeTrailingPathDelimiter(SysPath(aDirectoryEdit.Directory));

  if FilenameIsAbsolute(aDirectoryEdit.Directory) then
  begin
    aDirectoryEdit.Directory :=
      CleanAndExpandFilename(aDirectoryEdit.Directory);
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := GetCurrentDirUTF8;

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(GetCurrentDirUTF8)) + BaseDir);

    aDirectoryEdit.RootDir :=
      CleanAndExpandFilename(BaseDir + aDirectoryEdit.Directory);
  end;
end;

procedure SetFileEditInitialDir(aFileEdit: TFileNameEdit; BaseDir: string);
begin
  if not Assigned(aFileEdit) then
    Exit;

  BaseDir := SysPath(SetAsFolder(BaseDir));
  aFileEdit.FileName := SysPath(aFileEdit.FileName);

  if FilenameIsAbsolute(aFileEdit.FileName) then
  begin
    aFileEdit.InitialDir :=
      ExtractFileDir(CleanAndExpandFilename(aFileEdit.FileName));
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := GetCurrentDirUTF8;

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(GetCurrentDirUTF8)) + BaseDir);

    aFileEdit.InitialDir :=
      ExtractFileDir(CleanAndExpandFilename(BaseDir + aFileEdit.FileName));
  end;
end;

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
