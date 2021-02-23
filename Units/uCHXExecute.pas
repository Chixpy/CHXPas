unit uCHXExecute;
{< Unit with some executing process procedures.

  Copyright (C) 2011-2021 Chixpy

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
  aParams: TStrings; oStdOut, oStdErr: TCustomMemoryStream;
  out oExitCode: integer): boolean;
{ Executes a external program. Waits to exit, if you don't want to wait maybe
    want to create it in a thread.

    @param(aWorkFolder Folder from command will be executed,
      empty = Current Folder)
    @param(aExeFile Executable filename)
    @Param(aParams List of parameters to pass to executable, one by line)
    @param(oStdOut Stream where normal output will be stored, if not nil
      it will activate the use of pipes.)
    @param(oStdErr Stream where error output will be stored, if not nil
      it will activate the use of pipes.)
    @param(oExitCode Exit code of command.)
    @param(Options Additional TProcess options, example:
      [poNoConsole, poWaitOnExit])
    @return(@true on success calling command, @false otherwise (wich is
      different from command exit code))

    If oStdOut or oStdErr are defined, pipes will we turned on and console off.
    If both are @nil, console will be shown.
}

function ExecuteCMDArray(const aWorkFolder, aExeFile: string;
  aParams: array of string; oStdOut, oStdErr: TCustomMemoryStream;
  out oExitCode: integer): boolean;

function ExecuteCMDString(const aWorkFolder, aExeFile: string;
  aParams: string; oStdOut, oStdErr: TCustomMemoryStream;
  out oExitCode: integer): boolean;

implementation

function ExecuteCMDSL(aWorkFolder, aExeFile: string;
  aParams: TStrings; oStdOut, oStdErr: TCustomMemoryStream;
  out oExitCode: integer): boolean;
var
  aProcess: TProcessUTF8;
  aOptions: TProcessOptions;
  TempStream: TMemoryStream; // Used if one of oStdOut or oStdErr is nil.
  OutputPos, ErrPutPos: int64;
  BytesAvaiable: DWord;
begin
  Result := False;
  oExitCode := -1;
  aWorkFolder := SysPath(aWorkFolder);
  aExeFile := SysPath(aExeFile);

  // Testing executable existence
  if FilenameIsAbsolute(aExeFile) then
  begin
    if not FileExistsUTF8(aExeFile) then
      Exit;
  end
  else
  begin
    if not FileExistsUTF8(TrimAndExpandFilename(aExeFile, aWorkFolder)) then
      Exit;
  end;

  TempStream := TMemoryStream.Create;
  aProcess := TProcessUTF8.Create(nil);
  try
    if aWorkFolder <> '' then
      aProcess.CurrentDirectory := aWorkFolder;

    aProcess.Executable := aExeFile;

    if Assigned(aParams) then
      aProcess.Parameters.Assign(aParams);

    aOptions := aProcess.Options;
    Exclude(aOptions, poRunSuspended);

    if Assigned(oStdOut) or Assigned(oStdErr) then
    begin
      Include(aOptions, poUsePipes);
      Include(aOptions, poNoConsole);
      Exclude(aOptions, poWaitOnExit);

      // One is assigned but the other is nil
      if not Assigned(oStdOut) then
        oStdOut := TempStream;
      if not Assigned(oStdErr) then
        oStdErr := TempStream;
    end
    else
    begin
      Exclude(aOptions, poUsePipes);
      Exclude(aOptions, poNoConsole);
      Include(aOptions, poWaitOnExit);
    end;

    aProcess.Options := aOptions;

    aProcess.Execute;

    // Pipes
    if Assigned(oStdOut) and Assigned(oStdErr) then
    begin
      OutputPos := 0;
      ErrPutPos := 0;
      while aProcess.Running do
      begin
        BytesAvaiable := aProcess.Output.NumBytesAvailable;

        if BytesAvaiable > 0 then
        begin
          oStdOut.Size := OutputPos + BytesAvaiable;
          Inc(OutputPos, aProcess.Output.Read(
            (oStdOut.Memory + OutputPos)^, BytesAvaiable));
        end
        else if Assigned(aProcess.Stderr) then
          // if not used [poStderrToOutput]
        begin
          BytesAvaiable := aProcess.Stderr.NumBytesAvailable;

          if BytesAvaiable > 0 then
          begin
            oStdErr.Size := ErrPutPos + BytesAvaiable;
            Inc(ErrPutPos, aProcess.Stderr.Read(
              (oStdErr.Memory + ErrPutPos)^, BytesAvaiable));
          end;
        end
        else
          Sleep(100);
      end;
    end;

    oExitCode := aProcess.ExitCode;

    Result := True;

  finally
    TempStream.Free;
    aProcess.Free;
  end;
end;


function ExecuteCMDArray(const aWorkFolder, aExeFile: string;
  aParams: array of string; oStdOut, oStdErr: TCustomMemoryStream;
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
  aParams: string; oStdOut, oStdErr: TCustomMemoryStream;
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
