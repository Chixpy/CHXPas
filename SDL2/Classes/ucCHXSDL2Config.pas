unit ucCHXSDL2Config;
{< Unit of cCHXSDL2Config class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, CTypes,
  SDL2,
  // CHX abstracts
  uaCHXConfig,
  // CHXSDL2
  uCHXSDL2Utils;

const
  krsIniSectionSDL2Engine = 'SDL2Engine';
  krsIniKeyFullScreen = 'FullScreen';
  krsIniKeyWindowWidth = 'WindowWidth';
  krsIniKeyWindowHeight = 'WindowHeight';
  krsIniKeyRendererWidth = 'RendererWidth';
  krsIniKeyRendererHeight = 'RendererHeight';
  krsIniKeyRendererUseHW = 'RendererUseHW';
  krsIniKeyDefFontFile = 'DefFontFile';
  krsIniKeyDefFontSize = 'DefFontSize';
  krsIniKeyDefFontColor = 'DefFontColor';

type

  { cCHXSDL2Config }

  cCHXSDL2Config = class(caCHXConfig)
  public
    // Window properties
    {property} WindowWidth : CInt;
    {property} WindowHeight : CInt;
    {property} FullScreen : Boolean;

    // Renderer properties
    {property} RendererWidth : CInt;
    {property} RendererHeight : CInt;
    {property} RendererUseHW : Boolean;

    {property} DefFontFile : string;
    {property} DefFontSize : integer;
    {property} DefFontColor : TSDL_Color;

    procedure ResetDefaultConfig; override;

    procedure LoadFromIni(aIniFile : TMemIniFile); override;
    procedure SaveToIni(aIniFile : TMemIniFile); override;

    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Config }

procedure cCHXSDL2Config.LoadFromIni(aIniFile : TMemIniFile);
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
  RendererUseHW := aIniFile.ReadBool(krsIniSectionSDL2Engine,
    krsIniKeyRendererUseHW, RendererUseHW);

  // Fallback Font
  DefFontFile := aIniFile.ReadString(krsIniSectionSDL2Engine,
    krsIniKeyDefFontFile, DefFontFile);
  DefFontSize := aIniFile.ReadInteger(krsIniSectionSDL2Engine,
    krsIniKeyDefFontSize, DefFontSize);
  DefFontColor := Str2SDLColor(aIniFile.ReadString(krsIniSectionSDL2Engine,
    krsIniKeyDefFontColor, '255,255,255,255'));

end;

procedure cCHXSDL2Config.ResetDefaultConfig;
begin
  WindowWidth := 640;
  WindowHeight := 480;
  FullScreen := False;

  RendererWidth := 640;
  RendererHeight := 480;
  RendererUseHW := True;

  DefFontFile := 'FreeMonoBold.ttf';
  DefFontSize := 24;
  DefFontColor := SDLColor(255, 255, 255, 255);
end;

constructor cCHXSDL2Config.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
end;

destructor cCHXSDL2Config.Destroy;
begin
  inherited Destroy;
end;

procedure cCHXSDL2Config.SaveToIni(aIniFile : TMemIniFile);
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
  begin // Remove this keys if it has same size of the window.
    aIniFile.DeleteKey(krsIniSectionSDL2Engine, krsIniKeyRendererWidth);
    aIniFile.DeleteKey(krsIniSectionSDL2Engine, krsIniKeyRendererHeight);
  end;
  aIniFile.WriteBool(krsIniSectionSDL2Engine, krsIniKeyRendererUseHW,
    RendererUseHW);

  // Fallback Font
  aIniFile.WriteString(krsIniSectionSDL2Engine, krsIniKeyDefFontFile,
    DefFontFile);
  aIniFile.WriteInteger(krsIniSectionSDL2Engine, krsIniKeyDefFontSize,
    DefFontSize);
  aIniFile.WriteString(krsIniSectionSDL2Engine, krsIniKeyDefFontColor,
    SDLColor2Str(DefFontColor));
end;

end.
{< This source is free software; you can redistribute it and/or modify it under
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
