unit uaCHXStorable;

{< caCHXStorable, caCHXStorableIni and caCHXStorableTxt abstract classes unit.

  (C) 2006-2024 Chixpy https://github.com/Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { caCHXStorable }

  caCHXStorable = class(TPersistent) // TPersistent implements IFPObserved
  public
    DefaultFileName : String;
    {< Default filename if not explicit filename is used when calling
         Save or Load method.

       This property is NOT updated when calling Load or Save with
         it's parameter.
    }

    procedure LoadFromFile(const aFilename : String); virtual; abstract;
    {< Loads data from file.

      @note(DefaultFileName property is not updated with aFilename parameter.)

      @param(aFilename Filename of the inifile to read from. If '', try to load
        from DefaultFileName property.)
    }
    procedure SaveToFile(const aFilename : String; const ClearFile : Boolean);
      virtual; abstract;
    {< Saves data to file.

       @note(DefaultFileName property is not updated with aFilename parameter.)

       @param(aFilename Filename of the inifile to write to.)
       @param(ClearFile if @true, remove file content before saving.)
    }
  end;

  { caCHXStorableIni }

  caCHXStorableIni = class(caCHXStorable)
  protected
  type CBIniProc =
    procedure(aIniFile : TMemIniFile) of object;

    procedure DoFileOpen(aFilename : String; aCBProc : CBIniProc;
      FileMustExists : Boolean; ClearFile : Boolean; SaveAfter : Boolean);

  public
    procedure LoadFromFile(const aFilename : String); override;
    {< Loads data from file.

      @param(aFilename Inifile to read from.)
    }
    procedure LoadFromIni(aIniFile : TMemIniFile); virtual; abstract;
    {< Loads data from opened .ini file.

      @param(aIniFile Inifile to read from.)
    }
    procedure SaveToFile(const aFilename : String; const ClearFile : Boolean);
      override;
     {< Saves data to opened .ini file.

      @param(IniFile aFilename to write to.)
      @param(ClearFile Clear file content before saving.)
    }
    procedure SaveToIni(aIniFile : TMemIniFile); virtual; abstract;
    {< Saves data to opened .ini file.

      @param(IniFile Inifile to write to.)
    }
  end;

  { caCHXStorableTxt }

  caCHXStorableTxt = class(caCHXStorable)
  private
    function GetCommaText : String;
    procedure SetCommaText(aValue : String);

  protected
  type CBTxtProc =
    procedure(aIniFile : TStrings) of object;

    procedure DoFileOpen(aFilename : String; aCBProc : CBTxtProc;
      FileMustExists : Boolean; ClearFile : Boolean; SaveAfter : Boolean);

  public
    property CommaText : String read GetCommaText write SetCommaText;

    procedure LoadFromFile(const aFilename : String); override;
    procedure LoadFromStrLst(aTxtFile : TStrings); virtual; abstract;
    {< Loads data from file.

      @param(aTxtFile Text file to read from.)
    }
    procedure SaveToFile(const aFilename : String; const ClearFile : Boolean);
      override;
    procedure SaveToStrLst(aTxtFile : TStrings); virtual; abstract;
    {< Saves data to file.

      @param(aTxtFile Text file to write to.)
    }
  end;


implementation

{ caCHXStorableIni }

procedure caCHXStorableIni.DoFileOpen(aFilename : String;
  aCBProc : CBIniProc; FileMustExists : Boolean; ClearFile : Boolean;
  SaveAfter : Boolean);
var
  aIniFile : TMemIniFile;
  IniFileOps : TIniFileOptions;
begin
  if not Assigned(aCBProc) then
    Exit; // Nothing to do, so we don't waste time

  if aFilename = EmptyStr then // Testing filename
  begin
    aFilename := DefaultFileName;

    if aFilename = EmptyStr then
      Exit;
  end;

  // Testing if file exists
  if FileMustExists and (not FileExists(aFilename)) then
    Exit;

  // Removing file, ini files are autoloaded on creation,
  //   so it may faster than loading and clearing.
  if ClearFile then
    DeleteFile(aFilename);

  aIniFile := TMemIniFile.Create(aFilename);
  try
    IniFileOps := aIniFile.Options;
    Exclude(IniFileOps, ifoCaseSensitive); // Case insesitive
    Exclude(IniFileOps, ifoFormatSettingsActive); // Ignore FormatSettings
    aIniFile.Options := IniFileOps;

    // if assigned(aCBProc) then <-- tested before
    aCBProc(aIniFile);

    if SaveAfter then
      aIniFile.UpdateFile;
  finally
    aIniFile.Free;
  end;
end;

procedure caCHXStorableIni.LoadFromFile(const aFilename : String);
begin
  DoFileOpen(aFilename, @LoadFromIni, True, False, False);
end;

procedure caCHXStorableIni.SaveToFile(const aFilename : String;
  const ClearFile : Boolean);
begin
  DoFileOpen(aFilename, @SaveToIni, False, ClearFile, True);
end;

procedure caCHXStorableTxt.SetCommaText(aValue : String);
var
  aStringList : TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := aValue;

    LoadFromStrLst(aStringList);
  finally
    FreeAndNil(aStringList);
  end;
end;

function caCHXStorableTxt.GetCommaText : String;
var
  aStringList : TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToStrLst(aStringList);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure caCHXStorableTxt.DoFileOpen(aFilename : String;
  aCBProc : CBTxtProc; FileMustExists : Boolean; ClearFile : Boolean;
  SaveAfter : Boolean);
var
  aTxtFile : TStringList;
begin

  if not Assigned(aCBProc) then
    Exit; // Nothing to do, so we don't waste time

  if aFilename = EmptyStr then // Testing filename
  begin
    aFilename := DefaultFileName;

    if aFilename = EmptyStr then
      Exit;
  end;

  // Testing if file exists
  if FileMustExists then
    if not FileExists(aFilename) then
      Exit;

  aTxtFile := TStringList.Create;
  try
    if (not ClearFile) and FileExists(aFilename) then
      aTxtFile.LoadFromFile(aFilename);

    aTxtFile.CaseSensitive := False;

    // if assigned(aCBProc) then <-- tested before
    aCBProc(aTxtFile);

    if SaveAfter then
      aTxtFile.SaveToFile(aFilename);
  finally
    aTxtFile.Free;
  end;
end;

procedure caCHXStorableTxt.LoadFromFile(const aFilename : String);
begin
  DoFileOpen(aFilename, @LoadFromStrLst, True, False, False);
end;

procedure caCHXStorableTxt.SaveToFile(const aFilename : String;
  const ClearFile : Boolean);
begin
  DoFileOpen(aFilename, @SaveToStrLst, False, ClearFile, True);
end;

initialization
  RegisterClass(caCHXStorable);
  RegisterClass(caCHXStorableIni);
  RegisterClass(caCHXStorableTxt);

finalization
  UnRegisterClass(caCHXStorable);
  UnRegisterClass(caCHXStorableIni);
  UnRegisterClass(caCHXStorableTxt);
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
