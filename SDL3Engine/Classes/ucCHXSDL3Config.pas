unit ucCHXSDL3Config;
{< Unit of cCHXSDL3Config class.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  CTypes, IniFiles, // FPC
  uaCHXConfig; // CHXPas

const
  krsIniSectionSDL3Engine = 'SDL3Engine';
  krsIniKeyWidth = 'Width';
  krsIniKeyHeight = 'Height';
  krsIniKeyScale = 'Scale';
  krsIniKeyFullScreen = 'FullScreen';
  krsIniKeyUseGPU = 'UseGPU';

type

  { cCHXSDL3Config }

  cCHXSDL3Config = class(caCHXConfig)
  public
    Width : CInt;
    Height : CInt;
    Scale: CInt;
    FullScreen : Boolean;
    UseGPU : Boolean;

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
  Width := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyWidth, Width);
  Height := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyHeight, Height);
  Scale := aIniFile.ReadInteger(krsIniSectionSDL3Engine,
    krsIniKeyScale, Scale);
  FullScreen := aIniFile.ReadBool(krsIniSectionSDL3Engine,
    krsIniKeyFullScreen, FullScreen);
  UseGPU := aIniFile.ReadBool(krsIniSectionSDL3Engine,
    krsIniKeyUseGPU, UseGPU);
end;

procedure cCHXSDL3Config.ResetDefaultConfig;
begin
  Width := 0; // 0 = Renderer use max window size
  Height := 0;
  Scale := 0; // 0 = Maximize the window
  FullScreen := False;
  UseGPU := True;
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
  aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyWidth, Width);
  aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyHeight, Height);
  aIniFile.WriteInteger(krsIniSectionSDL3Engine, krsIniKeyScale, Scale);
  aIniFile.WriteBool(krsIniSectionSDL3Engine, krsIniKeyFullScreen, FullScreen);
  aIniFile.WriteBool(krsIniSectionSDL3Engine, krsIniKeyUseGPU, UseGPU);
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
