unit uaCHXStorable;

{< caCHXStorable, caCHXStorableIni and caCHXStorableTxt abstract classes unit.

  Copyright (C) 2006-2024 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, LazFileUtils,
  // CHX units
  uCHXStrUtils;

type
  { caCHXStorable }

  caCHXStorable = class(TComponent)
  private
    FDefaultFileName: string;
    procedure SetDefaultFileName(AValue: string);

  public
    procedure LoadFromFile(const aFilename: string); virtual; abstract;
    {< Loads data from file.

      @param(aFilename Filename of the inifile to read from. If '', try to load
        from DefaultFileName property.)

      DefaultFileName property is not updated with aFilename parameter.
    }
    procedure SaveToFile(const aFilename: string; ClearFile: boolean); virtual;
      abstract;
    {< Saves data to file.

      @param(aFilename Filename of the inifile to write to.)
      @param(ClearFile False -> if File exists load its content before saving.)

      DefaultFileName property is not updated with aFilename parameter.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property DefaultFileName: string read FDefaultFileName
      write SetDefaultFileName;
    {< Default filename if not filename is used when calling Save or Load.

    This property is NOT updated when calling Load or Save with it's parameter.
    }
  end;

  { caCHXStorableIni }

  caCHXStorableIni = class(caCHXStorable)
  protected
  type CBIniProc =
    procedure(aIniFile: TMemIniFile) of object;

    procedure DoFileOpen(aFilename: string; aCBProc: CBIniProc;
      FileMustExists: boolean; ClearFile: boolean; SaveAfter: boolean);

  public
    procedure LoadFromFile(const aFilename: string); override;
    {< Loads data from file.

      @param(aFilename Inifile to read from.)
    }
    procedure LoadFromIni(aIniFile: TMemIniFile); virtual; abstract;
    {< Loads data from opened .ini file.

      @param(aIniFile Inifile to read from.)
    }
    procedure SaveToFile(const aFilename: string; ClearFile: boolean);
      override;
     {< Saves data to opened .ini file.

      @param(IniFile aFilename to write to.)
      @param(ClearFile Clear file content before saving.)
    }
    procedure SaveToIni(aIniFile: TMemIniFile); virtual; abstract;
    {< Saves data to opened .ini file.

      @param(IniFile Inifile to write to.)
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { caCHXStorableTxt }

  caCHXStorableTxt = class(caCHXStorable)
  private
    function GetCommaText: string;
    procedure SetCommaText(AValue: string);

  protected
  type CBTxtProc =
    procedure(aIniFile: TStrings) of object;

    procedure DoFileOpen(aFilename: string; aCBProc: CBTxtProc;
      FileMustExists: boolean; ClearFile: boolean; SaveAfter: boolean);

  public
    property CommaText: string read GetCommaText write SetCommaText;

    procedure LoadFromFile(const aFilename: string); override;
    procedure LoadFromStrLst(aTxtFile: TStrings); virtual; abstract;
    {< Loads data from file.

      @param(aTxtFile Text file to read from.)
    }
    procedure SaveToFile(const aFilename: string; ClearFile: boolean);
      override;
    procedure SaveToStrLst(aTxtFile: TStrings); virtual; abstract;
    {< Saves data to file.

      @param(aTxtFile Text file to write to.)
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

{ caCHXStorable }

procedure caCHXStorable.SetDefaultFileName(AValue: string);
begin
  FDefaultFileName := SetAsFile(AValue);
end;

constructor caCHXStorable.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorable.Destroy;
begin
  inherited Destroy;
end;

{ caCHXStorableIni }

procedure caCHXStorableIni.DoFileOpen(aFilename: string;
  aCBProc: CBIniProc; FileMustExists: boolean; ClearFile: boolean;
  SaveAfter: boolean);
var
  aIniFile: TMemIniFile;
  IniFileOps: TIniFileOptions;
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
  if FileMustExists and (not FileExistsUTF8(aFilename)) then
    Exit;

  // Removing file, ini files are autoloaded on creation,
  //   so it may faster than loading and clearing.
  if ClearFile then
    DeleteFileUTF8(aFilename);

  aIniFile := TMemIniFile.Create(UTF8ToSys(aFilename));
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

procedure caCHXStorableIni.LoadFromFile(const aFilename: string);
begin
  DoFileOpen(aFilename, @LoadFromIni, True, False, False);
end;

procedure caCHXStorableIni.SaveToFile(const aFilename: string;
  ClearFile: boolean);
begin
  DoFileOpen(aFilename, @SaveToIni, False, ClearFile, True);
end;

constructor caCHXStorableIni.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorableIni.Destroy;
begin
  inherited Destroy;
end;

procedure caCHXStorableTxt.SetCommaText(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    LoadFromStrLst(aStringList);
  finally
    FreeAndNil(aStringList);
  end;
end;

function caCHXStorableTxt.GetCommaText: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToStrLst(aStringList);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure caCHXStorableTxt.DoFileOpen(aFilename: string;
  aCBProc: CBTxtProc; FileMustExists: boolean; ClearFile: boolean;
  SaveAfter: boolean);
var
  aTxtFile: TStringList;
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
    if not FileExistsUTF8(aFilename) then
      Exit;

  aTxtFile := TStringList.Create;
  try
    if (not ClearFile) and FileExistsUTF8(aFilename) then
      aTxtFile.LoadFromFile(UTF8ToSys(aFilename));

    aTxtFile.CaseSensitive := False;

    // if assigned(aCBProc) then <-- tested before
    aCBProc(aTxtFile);

    if SaveAfter then
      aTxtFile.SaveToFile(UTF8ToSys(aFilename));
  finally
    aTxtFile.Free;
  end;
end;

procedure caCHXStorableTxt.LoadFromFile(const aFilename: string);
begin
  DoFileOpen(aFilename, @LoadFromStrLst, True, False, False);
end;

procedure caCHXStorableTxt.SaveToFile(const aFilename: string;
  ClearFile: boolean);
begin
  DoFileOpen(aFilename, @SaveToStrLst, False, ClearFile, True);
end;

constructor caCHXStorableTxt.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorableTxt.Destroy;
begin
  inherited Destroy;
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
