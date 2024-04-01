unit ucSDL2Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  // CHX abstracts
  uaCHXConfig;

const
  krsIniSectionSDL2Engine = 'SDL2Engine';
  krsIniKeyFullScreen = 'FullScreen';
  krsIniKeyWindowWidth = 'WindowWidth';
  krsIniKeyWindowHeight = 'WindowHeight';

type

  { cSDL2Config }

  cSDL2Config = class(caCHXConfig)
  private
    FFullScreen: boolean;
    FWindowHeight: integer;
    FWindowWidth: integer;
    procedure SetFullScreen(AValue: boolean);
    procedure SetWindowHeight(AValue: integer);
    procedure SetWindowWidth(AValue: integer);

  public
    procedure ResetDefaultConfig; override;

    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property WindowWidth: integer read FWindowWidth write SetWindowWidth;
    property WindowHeight: integer read FWindowHeight write SetWindowHeight;
    property FullScreen: boolean read FFullScreen write SetFullScreen;
  end;

implementation

{ cSDL2Config }

procedure cSDL2Config.LoadFromIni(aIniFile: TMemIniFile);
begin
  FullScreen := aIniFile.ReadBool(krsIniSectionSDL2Engine,
    krsIniKeyFullScreen, FullScreen);
  WindowWidth := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyWindowWidth, WindowWidth);
  WindowHeight := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyWindowHeight, WindowHeight);
end;

procedure cSDL2Config.SetWindowHeight(AValue: integer);
begin
  if FWindowHeight = AValue then
    Exit;
  FWindowHeight := AValue;
end;

procedure cSDL2Config.SetFullScreen(AValue: boolean);
begin
  if FFullScreen = AValue then
    Exit;
  FFullScreen := AValue;
end;

procedure cSDL2Config.SetWindowWidth(AValue: integer);
begin
  if FWindowWidth = AValue then
    Exit;
  FWindowWidth := AValue;
end;

procedure cSDL2Config.ResetDefaultConfig;
begin
  WindowWidth := 640;
  WindowHeight := 480;
  FullScreen := False;
end;

constructor cSDL2Config.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cSDL2Config.Destroy;
begin
  inherited Destroy;
end;

procedure cSDL2Config.SaveToIni(aIniFile: TMemIniFile);
begin
  aIniFile.WriteBool(krsIniSectionSDL2Engine, krsIniKeyFullScreen,
    FullScreen);
  aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyWindowWidth,
    WindowWidth);
  aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyWindowHeight,
    WindowHeight);

end;

end.
