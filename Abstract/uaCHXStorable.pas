unit uaCHXStorable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, LazFileUtils;

type

  { caCHXStorable }

  caCHXStorable = class(TComponent)
  private
    FDataFile: TFilename;
    procedure SetDataFile(AValue: TFilename);

  public
    procedure LoadFromFileIni(Filename: TFilename); virtual;
    {< Loads data from file.

         @param(IniFile Inifile to read from.)
    }
    procedure LoadFromIni(aIniFile: TCustomIniFile); virtual; abstract;
    {< Loads data from file.

         @param(IniFile Inifile to read from.)
    }
    procedure LoadFromFileTxt(Filename: TFilename); virtual;
    {< Loads data from file.

         @param(IniFile Text to read from.)
    }
    procedure LoadFromStrLst(aTxtFile: TStrings); virtual; abstract;
    {< Loads data from file.

         @param(TxtFile Text file to read from.)
    }

    procedure SaveToFileIni(Filename: TFilename;
      const ExportMode: boolean); virtual;
      {< Saves data to file.

          @param(IniFile Inifile to write to.)
          @param(ExportMode if @true don't save user data.)
     }
    procedure SaveToIni(IniFile: TCustomIniFile; const ExportMode: boolean);
      virtual; abstract;
     {< Saves data to file.

         @param(IniFile Inifile to write to.)
         @param(ExportMode if @true don't save user data.)
    }
    procedure SaveToFileTxt(Filename: TFilename;
      const ExportMode: boolean); virtual;
     {< Saves data to file.

         @param(TxtFile Text file to write to.)
         @param(ExportMode if @true don't save user data.)
    }
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      virtual; abstract;
     {< Saves data to file.

         @param(TxtFile Text file to write to.)
         @param(ExportMode if @true don't save user data.)
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataFile: TFilename read FDataFile write SetDataFile;
  end;

implementation

procedure caCHXStorable.SetDataFile(AValue: TFilename);
begin
  if FDataFile = AValue then
    Exit;
  FDataFile := AValue;
end;

procedure caCHXStorable.LoadFromFileIni(Filename: TFilename);
var
  aIniFile: TMemIniFile;
begin
  if FileName = '' then
    FileName := DataFile;
  if not FileExistsUTF8(FileName) then
    Exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(FileName));
    aIniFile.CaseSensitive := False;

    LoadFromIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXStorable.LoadFromFileTxt(Filename: TFilename);
var
  aTxtFile: TStringList;
begin
  if FileName = '' then
    FileName := DataFile;
  if not FileExistsUTF8(FileName) then
    Exit;

  try
    aTxtFile := TStringList.Create;
    aTxtFile.LoadFromFile(UTF8ToSys(FileName));
    aTxtFile.CaseSensitive := False;

    LoadFromStrLst(aTxtFile);
  finally
    FreeAndNil(aTxtFile);
  end;
end;

procedure caCHXStorable.SaveToFileIni(Filename: TFilename;
  const ExportMode: boolean);
var
  aIniFile: TMemIniFile;
begin
  if FileName = '' then
    FileName := DataFile;
  if FileName = '' then
    exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(FileName));
    aIniFile.CaseSensitive := False;

    SaveToIni(aIniFile, ExportMode);
    aIniFile.UpdateFile;
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXStorable.SaveToFileTxt(Filename: TFilename;
  const ExportMode: boolean);
var
  aTxtFile: TStringList;
begin
  if FileName = '' then
    FileName := DataFile;
  if FileName = '' then
    Exit;

  try
    aTxtFile := TStringList.Create;

    if ExportMode and FileExistsUTF8(FileName) then
      aTxtFile.LoadFromFile(UTF8ToSys(FileName));

    aTxtFile.CaseSensitive := False;

    SaveToStrLst(aTxtFile, ExportMode);
    aTxtFile.SaveToFile(UTF8ToSys(FileName));
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
