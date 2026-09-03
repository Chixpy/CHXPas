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
  kRenderW = 50; { Renderer width. }
  kRenderH = 50; { Renderer height. }
  kWindowScale = 16; { Scale of the Window. }
  kFullScreen = False;
  kUseGPU = False;

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
    ShowHelp: Boolean;
    Color1, Color2: TSDL_FColor;
    FillMode: Boolean;
    DiamX, DiamY: CFloat;

    procedure InitColors;
    procedure DrawHelp;
  end;

{ cSDL3Eng }

procedure cSDL3Eng.InitColors;
begin
  Color1.Init(Random, Random, Random, Random);
  Color2.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True; ShowHelp := True;
  InitColors;
  FillMode := False;
  DiamX := Round(kRenderW div 2);
  DiamY := Round(kRenderH div 3);
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
var
  aRect: TSDL_FRect;
begin
  Window.SetRenderSize(kRenderW, kRenderH);
  Render.SetDrawColor(1, 1, 1);
  Render.Clear(0, 0, 0);

  aRect := SDLFRect((kRenderW - DiamX) * 0.5,
    (kRenderH - DiamY) * 0.5, DiamX, DiamY);
  if FillMode then
  begin
    Render.SetDrawColor(Color1);
    Render.EllipseInRectFilled(aRect);
  end
  else
    Render.EllipseInRect(aRect, Color1, Color2);
  Render.SetDrawColor(1, 1, 1, 0.2);
  Render.RectBorder(aRect);

  // Render size and color for FPS and Help
  Window.SetRenderSize(400, 400);
  Render.SetDrawColor(1, 0, 1);
  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
begin
  Render.DebugTextF(0, 0, 'DX: %g DY: %g', [DiamX, DiamY]);
  Render.DebugText(0, 10, '[F1] Toggle help');
  Render.DebugText(0, 20, '[C] Change color');
  Render.DebugText(0, 30, '[F] Change mode');
  Render.DebugText(0, 40, '[<=] [=>] Change X radius');
  Render.DebugText(0, 50, '[UP] [DOWN] Change Y radius');
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

        SDLK_F1: ShowHelp := not ShowHelp;

        SDLK_UP: DiamY += 0.25;

        SDLK_DOWN: DiamY -= 0.25;

        SDLK_RIGHT: DiamX += 0.25;

        SDLK_LEFT: DiamX -= 0.25;

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
    kWindowScale, kFullScreen, kUseGPU);
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
