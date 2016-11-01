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
    procedure SetDataFile(AValue: string);

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: string); virtual; abstract;
    {< Loads data from file.

         @param(FileName Path to the file.)
    }
    procedure SaveToFile(FileName: string; const ExportMode: boolean);
      virtual; abstract;
    {< Saves data to file.

         @param(FileName Path to the file.)
         @param(ExportMode if @true don't save user data.)
    }
  published
    property DataFile: string read FDataFile write SetDataFile;
    {< File for read/write data by default. }

  end;

  { caCHXStorableIni }

  caCHXStorableIni = class(caCHXStorable)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: string); override;
    procedure LoadFromFileIni(IniFile: TCustomIniFile); virtual; abstract;
    {< Loads data from file.

         @param(IniFile Inifile to read from.)
    }

    procedure SaveToFile(FileName: string; const ExportMode: boolean);
      override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); virtual; abstract;
     {< Saves data to file.

         @param(IniFile Inifile to write to.)
         @param(ExportMode if @true don't save user data.)
    }
  end;

  { caCHXStorableTxt }

  caCHXStorableTxt = class(caCHXStorable)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: string); override;
    procedure LoadFromFileTxt(TxtFile: TStrings); virtual; abstract;
    {< Loads data from file.

         @param(TxtFile Text file to read from.)
    }

    procedure SaveToFile(FileName: string; const ExportMode: boolean);
      override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      virtual; abstract;
     {< Saves data to file.

         @param(TxtFile Text file to write to.)
         @param(ExportMode if @true don't save user data.)
    }
  end;

implementation

{ caCHXStorableTxt }

constructor caCHXStorableTxt.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorableTxt.Destroy;
begin
  inherited Destroy;
end;

procedure caCHXStorableTxt.LoadFromFile(FileName: string);
var
  aTxtFile: TStringList;
begin
  if FileName = '' then
    FileName := DataFile;
  if not FileExistsUTF8(FileName) then
    Exit;

  aTxtFile := TStringList.Create;
  aTxtFile.LoadFromFile(UTF8ToSys(FileName));
  aTxtFile.CaseSensitive := False;
  try
    LoadFromFileTxt(aTxtFile);
  finally
    FreeAndNil(aTxtFile);
  end;
end;

procedure caCHXStorableTxt.SaveToFile(FileName: string;
  const ExportMode: boolean);
var
  aTxtFile: TStringList;
begin
  if FileName = '' then
    FileName := DataFile;
  if FileName = '' then
    Exit;
  aTxtFile := TStringList.Create;

  { TODO : caCHXStorableTxt.SaveToFile Export mode }
  if ExportMode and FileExistsUTF8(FileName) then
    aTxtFile.LoadFromFile(UTF8ToSys(FileName));

  aTxtFile.CaseSensitive := False;
  try
    SaveToFileTxt(aTxtFile, ExportMode);
    aTxtFile.SaveToFile(UTF8ToSys(FileName));
  finally
    FreeAndNil(aTxtFile);
  end;
end;

{ caCHXStorable }

procedure caCHXStorable.SetDataFile(AValue: string);
begin
  if FDataFile = AValue then
    Exit;
  FDataFile := AValue;
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

constructor caCHXStorableIni.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caCHXStorableIni.Destroy;
begin
  inherited Destroy;
end;

procedure caCHXStorableIni.LoadFromFile(FileName: string);
var
  aIniFile: TMemIniFile;
begin
  if FileName = '' then
    FileName := DataFile;
  if not FileExistsUTF8(FileName) then
    Exit;

  aIniFile := TMemIniFile.Create(UTF8ToSys(FileName));
  aIniFile.CaseSensitive := False;
  try
    LoadFromFileIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXStorableIni.SaveToFile(FileName: string;
  const ExportMode: boolean);
var
  aIniFile: TMemIniFile;
begin
  if FileName = '' then
    FileName := DataFile;
  if FileName = '' then
    exit;
  aIniFile := TMemIniFile.Create(UTF8ToSys(FileName));
  aIniFile.CaseSensitive := False;
  try
    SaveToFileIni(aIniFile, ExportMode);
    aIniFile.UpdateFile;
  finally
    FreeAndNil(aIniFile);
  end;
end;

end.
