unit uaCHXStorable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, LazFileUtils;

type

  { caCHXStorable }

  caCHXStorable = class(TComponent)
  private
    FDataFile: string;
    function GetDataString: string;
    procedure SetDataFile(AValue: string);
    procedure SetDataString(AValue: string);

  public
    property DataString: string read GetDataString write SetDataString;

    procedure LoadFromFileIni(aFilename: string); virtual;
    {< Loads data from file.

         @param(aFilename Filename of the inifile to read from.)
    }
    procedure LoadFromIni(aIniFile: TCustomIniFile); virtual; abstract;
    {< Loads data from file.

         @param(aIniFile Inifile to read from.)
    }
    procedure LoadFromFileTxt(aFilename: string); virtual;
    {< Loads data from file.

         @param(aFilename Filename of the text file to read from.)
    }
    procedure LoadFromStrLst(aTxtFile: TStrings); virtual; abstract;
    {< Loads data from file.

         @param(aTxtFile Text file to read from.)
    }

    procedure SaveToFileIni(aFilename: string;
      const ExportMode: boolean); virtual;
      {< Saves data to file.

          @param(IniFile Inifile to write to.)
          @param(ExportMode if @true don't save user data.)
     }
    procedure SaveToIni(aIniFile: TCustomIniFile; const ExportMode: boolean);
      virtual; abstract;
     {< Saves data to file.

         @param(IniFile Inifile to write to.)
         @param(ExportMode if @true don't save user data.)
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
    property DataFile: string read FDataFile write SetDataFile;
  end;

implementation

procedure caCHXStorable.SetDataFile(AValue: string);
begin
  if FDataFile = AValue then
    Exit;
  FDataFile := AValue;
end;

function caCHXStorable.GetDataString: string;
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

procedure caCHXStorable.SetDataString(AValue: string);
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

procedure caCHXStorable.LoadFromFileIni(aFilename: string);
var
  aIniFile: TMemIniFile;
begin
  if aFilename = '' then
    aFilename := DataFile;
  if not FileExistsUTF8(aFilename) then
    Exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(aFilename));
    aIniFile.CaseSensitive := False;

    LoadFromIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXStorable.LoadFromFileTxt(aFilename: string);
var
  aTxtFile: TStringList;
begin
  if aFilename = '' then
    aFilename := DataFile;
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

procedure caCHXStorable.SaveToFileIni(aFilename: string;
  const ExportMode: boolean);
var
  aIniFile: TMemIniFile;
begin
  if aFilename = '' then
    aFilename := DataFile;
  if aFilename = '' then
    exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(aFilename));
    aIniFile.CaseSensitive := False;

    SaveToIni(aIniFile, ExportMode);
    aIniFile.UpdateFile;
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXStorable.SaveToFileTxt(aFilename: string;
  const ExportMode: boolean);
var
  aTxtFile: TStringList;
begin
  if aFilename = '' then
    aFilename := DataFile;
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

constructor caCHXStorable.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorable.Destroy;
begin
  inherited Destroy;
end;

end.
