unit uCHXDlgUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EditBtn, LazFileUtils, Dialogs,
  uCHXStrUtils;

procedure SetDlgInitialDir(aDialog: TFileDialog; BaseDir: string);
procedure SetDirEditInitialDir(aDirectoryEdit: TDirectoryEdit;
  BaseDir: string);
procedure SetFileEditInitialDir(aFileEdit: TFileNameEdit; BaseDir: string);

implementation

procedure SetDlgInitialDir(aDialog: TFileDialog; BaseDir: string);
begin
  BaseDir := SysPath(SetAsFolder(BaseDir));
  aDialog.FileName := SysPath(aDialog.FileName);

  if FilenameIsAbsolute(aDialog.FileName) then
  begin
    aDialog.InitialDir :=
      ExtractFilePath(CleanAndExpandFilename(aDialog.FileName));
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := GetCurrentDirUTF8;

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(GetCurrentDirUTF8)) + BaseDir);

    aDialog.InitialDir :=
      ExtractFilePath(CleanAndExpandFilename(BaseDir + aDialog.FileName));
  end;
end;

procedure SetDirEditInitialDir(aDirectoryEdit: TDirectoryEdit;
  BaseDir: string);
begin
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

    aDirectoryEdit.RootDir := CleanAndExpandFilename(BaseDir + aDirectoryEdit.Directory);
  end;
end;

procedure SetFileEditInitialDir(aFileEdit: TFileNameEdit; BaseDir: string);
begin
  BaseDir := SysPath(SetAsFolder(BaseDir));
  aFileEdit.FileName := SysPath(aFileEdit.FileName);

  if FilenameIsAbsolute(aFileEdit.FileName) then
  begin
    aFileEdit.InitialDir :=
      ExtractFilePath(CleanAndExpandFilename(aFileEdit.FileName));
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := GetCurrentDirUTF8;

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(GetCurrentDirUTF8)) + BaseDir);

    aFileEdit.InitialDir :=
      ExtractFilePath(CleanAndExpandFilename(BaseDir + aFileEdit.FileName));
  end;
end;

end.
