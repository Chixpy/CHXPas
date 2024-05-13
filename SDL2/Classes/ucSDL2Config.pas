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
  krsIniKeyRendererWidth = 'RendererWidth';
  krsIniKeyRendererHeight = 'RendererHeight';

type

  { cSDL2Config }

  cSDL2Config = class(caCHXConfig)
  public
    // Window properties
    {property} WindowWidth : integer;
    {property} WindowHeight : integer;
    {property} FullScreen : Boolean;

    // Renderer properties
    {property} RendererWidth : integer;
    {property} RendererHeight : integer;

    procedure ResetDefaultConfig; override;

    procedure LoadFromIni(aIniFile : TMemIniFile); override;
    procedure SaveToIni(aIniFile : TMemIniFile); override;

    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cSDL2Config }

procedure cSDL2Config.LoadFromIni(aIniFile : TMemIniFile);
begin
  // Window properties
  FullScreen := aIniFile.ReadBool(krsIniSectionSDL2Engine,
    krsIniKeyFullScreen, FullScreen);
  WindowWidth := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyWindowWidth, WindowWidth);
  WindowHeight := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyWindowHeight, WindowHeight);

  // Renderer properties
  // Renderer size defaults to Window size, not default config values.
  RendererWidth := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyRendererWidth, WindowWidth);
  RendererHeight := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyRendererHeight, WindowHeight);
end;

procedure cSDL2Config.ResetDefaultConfig;
begin
  WindowWidth := 640;
  WindowHeight := 480;
  FullScreen := False;

  RendererWidth := 640;
  RendererHeight := 480;
end;

constructor cSDL2Config.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
end;

destructor cSDL2Config.Destroy;
begin
  inherited Destroy;
end;

procedure cSDL2Config.SaveToIni(aIniFile : TMemIniFile);
begin
  // Window properties
  aIniFile.WriteBool(krsIniSectionSDL2Engine, krsIniKeyFullScreen,
    FullScreen);
  aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyWindowWidth,
    WindowWidth);
  aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyWindowHeight,
    WindowHeight);

  // Renderer properties
  if (RendererWidth <> WindowWidth) or (RendererHeight <> WindowHeight) then
  begin
    aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyRendererWidth,
      RendererWidth);
    aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyRendererHeight,
      RendererHeight);
  end
  else
  begin
    aIniFile.DeleteKey(krsIniSectionSDL2Engine, krsIniKeyRendererWidth);
    aIniFile.DeleteKey(krsIniSectionSDL2Engine, krsIniKeyRendererHeight);
  end;
end;

end.
