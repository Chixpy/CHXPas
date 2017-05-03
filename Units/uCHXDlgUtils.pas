unit uCHXDlgUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EditBtn, LazFileUtils,
  uCHXStrUtils;

procedure SetDirEditInitialDir(aDirectoryEdit: TDirectoryEdit;
  BaseDir: string);
procedure SetFileEditInitialDir(aFileEdit: TFileNameEdit; BaseDir: string);

implementation

procedure SetDirEditInitialDir(aDirectoryEdit: TDirectoryEdit;
  BaseDir: string);
begin
  if FilenameIsAbsolute(aDirectoryEdit.Directory) then
  begin
    aDirectoryEdit.Directory :=
      ExcludeTrailingPathDelimiter(CleanAndExpandFilename(SysPath(aDirectoryEdit.Directory)));
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := GetCurrentDirUTF8;

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(GetCurrentDirUTF8)) + BaseDir);

    aDirectoryEdit.RootDir :=
      ExcludeTrailingPathDelimiter(CleanAndExpandFilename(SysPath(SetAsFolder(BaseDir)) +
      aDirectoryEdit.Directory));
  end;
end;

procedure SetFileEditInitialDir(aFileEdit: TFileNameEdit; BaseDir: string);
begin
  if FilenameIsAbsolute(aFileEdit.FileName) then
  begin
    aFileEdit.InitialDir :=
      ExtractFilePath(CleanAndExpandFilename(SysPath(aFileEdit.FileName)));
  end
  else
  begin
    if BaseDir = '' then
      BaseDir := GetCurrentDirUTF8;

    if not FilenameIsAbsolute(BaseDir) then
      BaseDir := CleanAndExpandFilename(
        SysPath(SetAsFolder(GetCurrentDirUTF8)) + BaseDir);

    aFileEdit.InitialDir :=
      ExtractFilePath(CleanAndExpandFilename(SysPath(SetAsFolder(BaseDir)) +
      aFileEdit.FileName));
  end;
end;

end.
