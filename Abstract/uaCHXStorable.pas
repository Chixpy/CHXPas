{ Copyright (C) 2006-2017 Chixpy

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
unit uaCHXStorable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, LazFileUtils;

type
  { caCHXStorableIni }

  caCHXStorableIni = class(TComponent)
  private
    FIniFileName: string;
    procedure SetIniFileName(AValue: string);

  protected
  public
    procedure LoadFromFileIni(aFilename: string); virtual;
    {< Loads data from file.

         @param(aFilename Filename of the inifile to read from.)
    }
    procedure LoadFromIni(aIniFile: TMemIniFile); virtual; abstract;
    {< Loads data from file.

         @param(aIniFile Inifile to read from.)
    }
    procedure SaveToFileIni(aFilename: string;
      const ExportMode: boolean); virtual;
    {< Saves data to file.

          @param(IniFile Inifile to write to.)
          @param(ExportMode if @true don't save user data.)
    }
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      virtual; abstract;
    {< Saves data to file.

         @param(IniFile Inifile to write to.)
         @param(ExportMode if @true don't save user data.)
    }


    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property IniFileName: string read FIniFileName write SetIniFileName;

  end;

  caCHXStorableTxt = class(caCHXStorableIni)
  private

    FTxtFileName: string;

    procedure SetTxtFileName(AValue: string);

  protected
    function GetTxtString: string; virtual;
    procedure SetTxtString(AValue: string); virtual;

  public
    property TXTString: string read GetTxtString write SetTxtString;

    procedure LoadFromFileTxt(aFilename: string); virtual;
    {< Loads data from file.

         @param(aFilename Filename of the text file to read from.)
    }
    procedure LoadFromStrLst(aTxtFile: TStrings); virtual; abstract;
    {< Loads data from file.

         @param(aTxtFile Text file to read from.)
    }
    procedure SaveToFileTxt(aFilename: string;
      const ExportMode: boolean); virtual;
    {< Saves data to file.

         @param(TxtFile Text file to write to.)
         @param(ExportMode if @true don't save user data.)
    }
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      virtual; abstract;
    {< Saves data to file.

         @param(TxtFile Text file to write to.)
         @param(ExportMode if @true don't save user data.)
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property TxtFileName: string read FTxtFileName write SetTxtFileName;
  end;


implementation

{ caCHXStorableIni }

procedure caCHXStorableIni.SetIniFileName(AValue: string);
begin
  if FIniFileName = AValue then
    Exit;
  FIniFileName := AValue;
end;

procedure caCHXStorableIni.LoadFromFileIni(aFilename: string);
var
  aIniFile: TMemIniFile;
  IniFileOps: TIniFileOptions;
begin
  if aFilename = '' then
    aFilename := IniFileName;
  if not FileExistsUTF8(aFilename) then
    Exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(aFilename));
    IniFileOps :=  aIniFile.Options;
    Exclude(IniFileOps, ifoCaseSensitive);
    aIniFile.Options := IniFileOps;

    LoadFromIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXStorableIni.SaveToFileIni(aFilename: string;
  const ExportMode: boolean);
var
  aIniFile: TMemIniFile;
  IniFileOps: TIniFileOptions;
begin
  if aFilename = '' then
    aFilename := IniFileName;
  if aFilename = '' then
    exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(aFilename));
       IniFileOps :=  aIniFile.Options;
    Exclude(IniFileOps, ifoCaseSensitive);
    aIniFile.Options := IniFileOps;

    SaveToIni(aIniFile, ExportMode);
    aIniFile.UpdateFile;
  finally
    FreeAndNil(aIniFile);
  end;
end;

constructor caCHXStorableIni.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorableIni.Destroy;
begin
  inherited Destroy;
end;

function caCHXStorableTxt.GetTxtString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToStrLst(aStringList, False);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure caCHXStorableTxt.SetTxtString(AValue: string);
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

procedure caCHXStorableTxt.SetTxtFileName(AValue: string);
begin
  if FTxtFileName = AValue then
    Exit;
  FTxtFileName := AValue;
end;

procedure caCHXStorableTxt.LoadFromFileTxt(aFilename: string);
var
  aTxtFile: TStringList;
begin
  if aFilename = '' then
    aFilename := TxtFileName;
  if not FileExistsUTF8(aFilename) then
    Exit;

  try
    aTxtFile := TStringList.Create;
    aTxtFile.LoadFromFile(UTF8ToSys(aFilename));
    aTxtFile.CaseSensitive := False;

    LoadFromStrLst(aTxtFile);
  finally
    FreeAndNil(aTxtFile);
  end;
end;

procedure caCHXStorableTxt.SaveToFileTxt(aFilename: string;
  const ExportMode: boolean);
var
  aTxtFile: TStringList;
begin
  if aFilename = '' then
    aFilename := TxtFileName;
  if aFilename = '' then
    Exit;

  try
    aTxtFile := TStringList.Create;

    if ExportMode and FileExistsUTF8(aFilename) then
      aTxtFile.LoadFromFile(UTF8ToSys(aFilename));

    aTxtFile.CaseSensitive := False;

    SaveToStrLst(aTxtFile, ExportMode);
    aTxtFile.SaveToFile(UTF8ToSys(aFilename));
  finally
    FreeAndNil(aTxtFile);
  end;
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
