unit uCHXExecute;
{< Unit with some executing process procedures.

  Copyright (C) 2011-2023 Chixpy

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

uses
  Classes, SysUtils, Process, UTF8Process, LazFileUtils,
  // CHX units
  uCHXStrUtils;

function ExecuteCMDSL(aWorkFolder, aExeFile: string;
  aParams: TStrings; out oStdOut, oStdErr: string;
  out oExitCode: integer): boolean;
{< Executes a external program. Waits to exit, if you don't want to wait maybe
    want to create it in a thread.

    @param(aWorkFolder Folder from command will be executed,
      empty = Current Folder)
    @param(aExeFile Executable filename)
    @Param(aParams List of parameters to pass to executable, one by line)
    @param(oStdOut String where normal output will be stored.)
    @param(oStdErr String where error output will be stored.)
    @param(oExitCode Exit code of command.)
    @return(@true on success calling command, @false otherwise (wich is
      different from command exit code))
}

function ExecuteCMDArray(const aWorkFolder, aExeFile: string;
  aParams: array of string; out oStdOut, oStdErr: string;
  out oExitCode: integer): boolean;

function ExecuteCMDString(const aWorkFolder, aExeFile: string;
  aParams: string; out oStdOut, oStdErr: string;
  out oExitCode: integer): boolean;

implementation

function ExecuteCMDSL(aWorkFolder, aExeFile: string; aParams: TStrings;
  out oStdOut, oStdErr: string; out oExitCode: integer): boolean;
var
  aProcess: TProcessUTF8;
  aOptions: TProcessOptions;
  TempStr: string;
begin
  Result := False;
  oExitCode := -1;

  aWorkFolder := SysPath(aWorkFolder);
  aExeFile := SysPath(aExeFile);

  // Testing WorkFolder existence (if not empty)
  if aWorkFolder <> '' then
  begin
    if FilenameIsAbsolute(aExeFile) then
    begin
      if not DirectoryExistsUTF8(aWorkFolder) then
         Exit;
    end
    else
    begin
      // Try to search aWorkFolder from CurrentDir
      aWorkFolder := TrimAndExpandDirectory(aWorkFolder, GetCurrentDirUTF8);

      if not DirectoryExistsUTF8(aWorkFolder) then
         Exit;
    end;
  end;

  // Testing executable existence
  if FilenameIsAbsolute(aExeFile) then
  begin
    if not FileExistsUTF8(aExeFile) then
      Exit;
  end
  else
  begin
    // Try to search relative path from aWorkFolder
    TempStr := TrimAndExpandFilename(aExeFile, aWorkFolder);

    if FileExistsUTF8(TempStr) then
    begin
      aExeFile := TempStr;
    end
    else
    begin
     // Try to search relative path from CurrentDir
     TempStr := TrimAndExpandFilename(aExeFile, GetCurrentDirUTF8);

     if FileExistsUTF8(TempStr) then
     begin
       aExeFile := TempStr;
     end
     else
     begin
       // Not found
       Exit
     end;
    end;
  end;

  aProcess := TProcessUTF8.Create(nil);
  try
    if aWorkFolder <> '' then
      aProcess.CurrentDirectory := aWorkFolder;

    aProcess.Executable := aExeFile;

    if Assigned(aParams) then
      aProcess.Parameters.Assign(aParams);

    aOptions := aProcess.Options;
    Exclude(aOptions, poRunSuspended);
    Include(aOptions, poNoConsole);
    //Include(aOptions, poWaitOnExit);
    aProcess.Options := aOptions;

    aProcess.RunCommandLoop(oStdOut, oStdErr, oExitCode);

    Result := True;

  finally
    aProcess.Free;
  end;
end;

function ExecuteCMDArray(const aWorkFolder, aExeFile: string;
  aParams: array of string; out oStdOut, oStdErr: string;
  out oExitCode: integer): boolean;
var
  Params: TStringList;
  i: integer;
begin
  Params := TStringList.Create;
  try
    if High(aParams) >= 0 then
    begin
      i := Low(aParams);
      while i <= High(aParams) do
      begin
        Params.Add(aParams[i]);
        Inc(i);
      end;
    end;

    Result := ExecuteCMDSL(aWorkFolder, aExeFile, Params, oStdOut,
      oStdErr, oExitCode);

  finally
    Params.Free;
  end;
end;

function ExecuteCMDString(const aWorkFolder, aExeFile: string;
  aParams: string; out oStdOut, oStdErr: string;
  out oExitCode: integer): boolean;
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    SplitCmdLineParams(aParams, Params, False);

    Result := ExecuteCMDSL(aWorkFolder, aExeFile, Params, oStdOut,
      oStdErr, oExitCode);

  finally
    Params.Free;
  end;
end;

end.
