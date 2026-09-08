program TestEllipse;
{<
  A simple program with cCHXSDL3Engine for testing Ellipse primitive.

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kRenderW = 50; { Renderer width. }
  kRenderH = kRenderW; { Renderer height. }
  kWindowScale = 900 div kRenderH; { Scale of the Window. }
  kFullScreen = False;
  kUseGPU = False;

  kRadiusStep = 0.5;

type

  TState = (stBorFill, stBorder, stTBorder, stFilled, stTFilled);

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
    State: TState;
    sState: String;
    Color1, Color2: TSDL_FColor;

    RadiusX, RadiusY: CFloat;

    procedure InitColors;
    procedure ChangeState;
    procedure DrawHelp;
  end;

{ cSDL3Eng }

procedure cSDL3Eng.InitColors;
begin
  Color1.Init(Random, Random, Random, Random);
  Color2.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.ChangeState;
begin
  if State = High(TState) then
    State := Low(TState)
  else
    Inc(State);

  case State of
    stBorFill: sState := 'Border + OnlyFill';
    stBorder: sState := 'Border';
    stTBorder: sState := 'Triangle Border';
    stFilled: sState := 'Filled';
    stTFilled: sState := 'Triangle Filled';
  otherwise
    ;
  end;
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True; ShowHelp := True;
  InitColors;
  State := High(TState);
  ChangeState;

  RadiusX := kRenderW * 0.4;
  RadiusY := kRenderH * 0.3;
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
var
  X, Y: CFloat;
begin
  Window.SetRenderSize(kRenderW, kRenderH);
  Render.Clear(0.05);

  X := kRenderW * 0.5; Y := kRenderW * 0.5;
  Render.SetDrawColor(Color1);

  case State of
  stBorFill: Render.Ellipse(X, Y, RadiusX, RadiusY, Color1, Color2);

  stBorder: Render.EllipseBorder(X, Y, RadiusX, RadiusY);

  stTBorder: Render.TEllipseBorder(X, Y, RadiusX, RadiusY);

  stFilled: Render.EllipseFilled(X, Y, RadiusX, RadiusY);

  stTFilled: Render.TEllipseFilled(X, Y, RadiusX, RadiusY);

  otherwise
    ;
  end;

  // Render size and color for FPS and Help
  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
begin
  Window.PushRenderSize(400, 400);
  Render.SetDrawColor(1, 0, 1);
  Render.DebugTextF(0, 0, '%s - Radius: %g, %g', [sState, RadiusX, RadiusY]);
  Render.DebugText(0, 10, '[F1] Toggle help');
  Render.DebugText(0, 20, '[C] Change color');
  Render.DebugText(0, 30, '[F] Change mode');
  Render.DebugText(0, 40, '[LEFT] [RIGHT] Change X radius');
  Render.DebugText(0, 50, '[UP] [DOWN] Change Y radius');
  Window.PopRenderSize;
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

        SDLK_UP: RadiusY += kRadiusStep;

        SDLK_DOWN: RadiusY -= kRadiusStep;

        SDLK_LEFT: RadiusX -= kRadiusStep;

        SDLK_RIGHT: RadiusX += kRadiusStep;

        SDLK_C: InitColors;

        SDLK_F: ChangeState;

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
