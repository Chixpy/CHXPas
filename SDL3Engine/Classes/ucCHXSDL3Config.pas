unit ucCHXSDL3Config;
{< Unit of cCHXSDL3Config class.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  CTypes, IniFiles,SDL3,
  // CHX abstracts
  uaCHXConfig,
  // CHXSDL3
  uCHXSDL3TypeHelpers;

const
  krsIniSectionSDL3Engine = 'SDL3Engine';
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

  { cCHXSDL3Config }

  cCHXSDL3Config = class(caCHXConfig)
  public
    // Window properties
    WindowWidth : CInt;
    WindowHeight : CInt;
    FullScreen : Boolean;

    // Renderer properties
    RendererWidth : CInt;
    RendererHeight : CInt;
    RendererUseHW : Boolean;

    DefFontFile : String;
    DefFontSize : Integer;
    DefFontColor : TSDL_Color;

    procedure ResetDefaultConfig; override;

    procedure LoadFromIni(aIniFile : TMemIniFile); override;
    procedure SaveToIni(aIniFile : TMemIniFile); override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL3Config }

procedure cCHXSDL3Config.LoadFromIni(aIniFile : TMemIniFile);
begin
  // Window properties
  FullScreen := aIniFile.ReadBool(krsIniSectionSDL3Engine,
    krsIniKeyFullScreen, FullScreen);
  WindowWidth := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyWindowWidth, WindowWidth);
  WindowHeight := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyWindowHeight, WindowHeight);

  // Renderer properties
  // Renderer size defaults to Window size, not default config values.
  RendererWidth := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyRendererWidth, WindowWidth);
  RendererHeight := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyRendererHeight, WindowHeight);
  RendererUseHW := aIniFile.ReadBool(krsIniSectionSDL3Engine,
    krsIniKeyRendererUseHW, RendererUseHW);

  // Fallback Font
  DefFontFile := aIniFile.ReadString(krsIniSectionSDL3Engine,
    krsIniKeyDefFontFile, DefFontFile);
  DefFontSize := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyDefFontSize, DefFontSize);
  DefFontColor := Str2SDLColor(aIniFile.ReadString(krsIniSectionSDL3Engine,
    krsIniKeyDefFontColor, '255,255,255,255'));

end;

procedure cCHXSDL3Config.ResetDefaultConfig;
begin
  WindowWidth := 640;
  WindowHeight := 480;
  FullScreen := False;

  RendererWidth := 640;
  RendererHeight := 480;
  RendererUseHW := True;

  DefFontFile := '';
  DefFontSize := 10;
  DefFontColor := SDLColor(255, 255, 255, 255);
end;

constructor cCHXSDL3Config.Create;
begin
  inherited Create;
end;

destructor cCHXSDL3Config.Destroy;
begin
  inherited Destroy;
end;

procedure cCHXSDL3Config.SaveToIni(aIniFile : TMemIniFile);
begin
  // Window properties
  aIniFile.WriteBool(krsIniSectionSDL3Engine, krsIniKeyFullScreen,
    FullScreen);
  aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyWindowWidth,
    WindowWidth);
  aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyWindowHeight,
    WindowHeight);

  // Renderer properties
  if (RendererWidth <> WindowWidth) or (RendererHeight <> WindowHeight) then
  begin
    aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyRendererWidth,
      RendererWidth);
    aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyRendererHeight,
      RendererHeight);
  end
  else
  begin // Remove this keys if it has same size of the window.
    aIniFile.DeleteKey(krsIniSectionSDL3Engine, krsIniKeyRendererWidth);
    aIniFile.DeleteKey(krsIniSectionSDL3Engine, krsIniKeyRendererHeight);
  end;
  aIniFile.WriteBool(krsIniSectionSDL3Engine, krsIniKeyRendererUseHW,
    RendererUseHW);

  // Fallback Font
  aIniFile.WriteString(krsIniSectionSDL3Engine, krsIniKeyDefFontFile,
    DefFontFile);
  aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyDefFontSize,
    DefFontSize);
  aIniFile.WriteString(krsIniSectionSDL3Engine, krsIniKeyDefFontColor,
    SDLColor2Str(DefFontColor));
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
