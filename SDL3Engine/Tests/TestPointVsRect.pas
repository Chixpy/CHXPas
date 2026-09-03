program TestPointVsRect;
{<
  A simple program with cCHXSDL3Engine that compares SDL_RenderPoint
  and SDL_RenderFilledRect[s] when Logical Presentation is used.

  @note(I suspect that SDL actually use Rects to draw points.)

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  // In actual programs use Window.Render[Width/Height]
  kRenderW = 100; // Renderer width.
  kRenderH = 100; // Renderer height.
  kWindowScale = 8; // Scale of the Window.
  kFullScree = False;
  kUseGPU = False; // Soft or GPU renderer

  kNPoints = 2000;

type

  TState = (stPoint, stPoints, stRectPoint, stRectsPoints,
    stRectRects, stRectsRects);

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
    Color: TSDL_FColor;
    ShowHelp: Boolean;

    State: TState;
    Points: TSDLFPointDynArray;
    Rects: TSDLFRectDynArray;

    procedure InitColors;
    procedure InitPoints;
  end;

{ cSDL3Eng }


procedure cSDL3Eng.InitColors;
begin
  Color.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.InitPoints;
var
  i: Integer;
begin
  SetLength(Points, kNPoints);
  SetLength(Rects, kNPoints);
  for i := 0 to (kNPoints - 1) do
  begin
    Points[i].InitRandom(0, kRenderW, 0, kRenderH);
    Rects[i] := SDLFRect(Points[i].X, Points[i].Y, 1, 1);
  end;
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True;

  State := Low(TState);

  InitColors;
  InitPoints;

  ShowHelp := True;
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
var
  aPoint: TSDL_FPoint;
  aRect: TSDL_FRect;
  i: Integer;
  sMode: String;
begin
  // To show framerate at very small render size (2)
  Window.SetRenderSize(kRenderW, kRenderH);
  Render.SetDrawColor(1, 1, 1);
  Render.Clear(0, 0, 0);

  Render.SetDrawColor(Color);

  case State of
  stPoint:
  begin
    sMode := 'Individual Points';
    for aPoint in Points do
      SDL_RenderPoint(SDLRenderer, aPoint.X, aPoint.Y);
  end;

  stPoints:
  begin
    sMode := 'Array of Points';
    SDL_RenderPoints(SDLRenderer, @Points[0], Length(Points));
  end;

  stRectPoint:
  begin
    sMode := 'Individual Points to Rect';
    for aPoint in Points do
    begin
      aRect := SDLFRect(aPoint.X, aPoint.Y, 1, 1);
      SDL_RenderFillRect(SDLRenderer, @aRect);
    end;
  end;

  stRectsPoints:
  begin
    sMode := 'Array of Points to Array of Rects';
    SetLength(Rects, 0);
    SetLength(Rects, Length(Points));
    for i := 0 to (Length(Points) - 1) do
      Rects[i] := SDLFRect(Points[i].X, Points[i].Y, 1, 1);

    SDL_RenderFillRects(SDLRenderer, @Rects[0], Length(Rects));
  end;

  stRectRects:
  begin
    sMode := 'Individual Rects';
    for aRect in Rects do
      SDL_RenderFillRect(SDLRenderer, @aRect);
  end;

  stRectsRects:
  begin
    sMode := 'Array of Rects';
    SDL_RenderFillRects(SDLRenderer, @Rects[0], Length(Rects));
  end;

  otherwise
    ;
  end;

  // To show framerate at very small render size (1)
  Window.SetRenderSize(400, 400);
  if ShowHelp then
  begin
    Render.SetDrawColor(1, 0, 1);
    Render.DebugText(0, 0, PAnsiChar(sMode));
    Render.DebugText(0, 10, '[F1] Toggle help');
    Render.DebugText(0, 20, '[C] Change color');
    Render.DebugText(0, 30, '[P] Change Points');
    Render.DebugText(0, 40, '[M] Change Mode');
  end;
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

        SDLK_C: InitColors;

        SDLK_P: InitPoints;

        SDLK_M:
          if State = High(TState) then
            State := Low(TState)
          else
            Inc(State);

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
    kWindowScale, kFullScree, kUseGPU);
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
