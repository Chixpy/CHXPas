unit uaCHXConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8,
  uCHXRscStr, uCHXStrUtils;

type

  { caCHXConfig }

  caCHXConfig = class(TComponent)
  private
    FConfigFile: string;
    procedure SetConfigFile(AValue: string);

  protected
    procedure OnLoadConfig(IniFile: TIniFile); virtual; abstract;
    procedure OnSaveConfig(IniFile: TIniFile); virtual; abstract;

  public
    procedure LoadConfig(aFileName: string);
    procedure SaveConfig(aFilename: string);
    procedure ResetDefaultConfig; virtual; abstract;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published

    property ConfigFile: string read FConfigFile write SetConfigFile;
  end;

implementation

{ caCHXConfig }

procedure caCHXConfig.SetConfigFile(AValue: string);
begin
  FConfigFile := SetAsFile(AValue);
end;

procedure caCHXConfig.LoadConfig(aFileName: string);
var
  aIniFile: TMemIniFile;

begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then
    raise EInOutError.Create(Format(rsENotFilename, [ClassName + '.LoadConfig']));
  ;
  { TODO : Raise exception? Warning? create file always? Exit?}
  //if not FileExistsUTF8(aFilename) then


  ConfigFile := aFilename;

  aIniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    OnLoadConfig(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caCHXConfig.SaveConfig(aFilename: string);
var
  aIniFile: TMemIniFile;
begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then
    raise EInOutError.Create(Format(rsENotFilename, [ClassName + '.SaveConfig']));
  ConfigFile := aFilename;
  aIniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    OnSaveConfig(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

constructor caCHXConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ResetDefaultConfig;
end;

destructor caCHXConfig.Destroy;
begin
  inherited Destroy;
end;

end.
