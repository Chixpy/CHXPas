program TestEllipse;
{<
  A simple program with cCHXSDL3Engine for testing Ellipse primitive.

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  // In actual programs use Window.Render[Width/Height]
  kRenderW = 200; { Renderer width. }
  kRenderH = 200; { Renderer height. }
  kWindowScale = 4; { Scale of the Window. }

type

  { cSDL3Eng }

  cSDL3Eng = class(cCHXSDL3Engine)
  protected
    procedure Setup; override; { It's abstract. }
    procedure Finish; override; { It's abstract. }
    procedure Compute(var ExitProg : Boolean); override; { It's abstract. }
    procedure Draw; override; { It's abstract. }
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override; { It's virtual. }

  public
    Color1, Color2: TSDL_FColor;
    FillMode: Boolean;
    RadiusX, RadiusY: CFloat;

    procedure InitColors;
  end;

{ cSDL3Eng }


procedure cSDL3Eng.InitColors;
begin
  Color1.Init(Random, Random, Random, Random);
  Color2.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True;

  InitColors;
  FillMode := False;
  RadiusX := Round(kRenderW div 3);
  RadiusY := Round(kRenderH div 4);
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
begin
  Render.SetDrawColor(1, 1, 1);
  Render.Clear(0, 0, 0);

  if  FillMode then
  begin
    Render.SetDrawColor(Color1);
    Render.EllipseFilled(kRenderW div 2, kRenderW div 2, RadiusX, RadiusY);
  end
  else
    Render.Ellipse(kRenderW div 2, kRenderW div 2, RadiusX, RadiusY,
      Color1, Color2);


  Render.SetDrawColor(1, 0, 1);
  Render.DebugText(2, 10, '[C] Change color');
  Render.DebugText(2, 20, '[F] Change mode');
  Render.DebugText(2, 30, '[<=] [=>] Change X radius');
  Render.DebugText(2, 40, '[UP] [DOWN] Change Y radius');
end;

procedure cSDL3Eng.HandleEvent(const aEvent : TSDL_Event;
var Handled : Boolean; var ExitProg : Boolean);
begin
  inherited;
  if ExitProg or Handled then Exit;

  case aEvent.type_ of
    SDL_EVENT_KEY_DOWN:
    begin
      Handled := True;
      case aEvent.key.key of
        // ESC, F10, F11, F12 handled by cCHXSDL3Engine

        SDLK_UP: RadiusY += 1;

        SDLK_DOWN: RadiusY -= 1;

        SDLK_LEFT: RadiusX -= 1;

        SDLK_RIGHT: RadiusX += 1;

        SDLK_C: InitColors;

        SDLK_F: FillMode := not FillMode;

        SDLK_Q: ExitProg := True;

      otherwise
        Handled := False;
      end;
    end;
  otherwise
    ;
  end;
end;

  { Main program }

var
  SDL3Eng : cSDL3Eng;
  ProgName: String;
begin
  ProgName := ExtractFileName(ParamStr(0));
  ChDir(ExtractFilePath(ParamStr(0)));

  // Aplication metadata
  SDL_SetAppMetadata(PAnsiChar(ProgName), '1.0',
    PAnsiChar('com.chixpy.' + ProgName));
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_CREATOR_STRING, 'Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_COPYRIGHT_STRING,
    '(C) 2026 Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_URL_STRING,
    'https://github.com/Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_TYPE_STRING, 'application');

  SDL3Eng := cSDL3Eng.Create(ExtractFileName(ParamStr(0)), kRenderW, kRenderH,
    kWindowScale);
  try
    SDL3Eng.Run;
  finally
    SDL3Eng.Free;
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
