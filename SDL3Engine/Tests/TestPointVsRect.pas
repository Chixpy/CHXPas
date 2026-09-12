program TestPointVsRect;
{<
  A simple program with cCHXSDL3Engine that compares SDL_RenderPoint
  and SDL_RenderFilledRect[s] when Logical Presentation is used.

  @note(I suspect that SDL actually use Rects to draw points.)

  ToDo: Filled Rects with negative width and height.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kRenderH = 100;
  kRenderW = kRenderH * 4 div 3;
  kWinScale = 900 div kRenderH;
  kFullScreen = False;
  kRDriver = '';
  kProgVersion = '1.0';

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
    ShowHelp: Boolean;
    State: TState; sState: String;
    Color: TSDL_FColor;

    Points: TSDLFPointDynArray;
    Rects: TSDLFRectDynArray;

    procedure InitPoints;

    procedure ChangeState;
    procedure InitColors;
    procedure DrawHelp;
  end;

{ cSDL3Eng }

procedure cSDL3Eng.ChangeState;
begin
  if State = High(TState) then
    State := Low(TState)
  else
    Inc(State);

  case State of
  stPoint: sState := 'Individual Points';
  stPoints: sState := 'Array of Points';
  stRectPoint: sState := 'Individual Points to Rect';
  stRectsPoints: sState := 'Array of Points to Array of Rects';
  stRectRects: sState := 'Individual Rects';
  stRectsRects: sState := 'Array of Rects';
  otherwise sState := '<Undefined>';
  end; // case State of
end;

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
  ShowFrameRate := True; ShowHelp := True;
  State := High(TState); ChangeState;
  InitColors;

  InitPoints;
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
begin
  Render.Clear(0.01);

  Render.SetDrawColor(Color);
  case State of
  stPoint:
    for aPoint in Points do
      SDL_RenderPoint(SDLRenderer, aPoint.X, aPoint.Y);

  stPoints:
    SDL_RenderPoints(SDLRenderer, @Points[0], Length(Points));

  stRectPoint:
    for aPoint in Points do
    begin
      aRect := SDLFRect(aPoint.X, aPoint.Y, 1, 1);
      SDL_RenderFillRect(SDLRenderer, @aRect);
    end;

  stRectsPoints:
  begin
    SetLength(Rects, 0);
    SetLength(Rects, Length(Points));
    for i := 0 to (Length(Points) - 1) do
      Rects[i] := SDLFRect(Points[i].X, Points[i].Y, 1, 1);

    SDL_RenderFillRects(SDLRenderer, @Rects[0], Length(Rects));
  end;

  stRectRects:
    for aRect in Rects do
      SDL_RenderFillRect(SDLRenderer, @aRect);

  stRectsRects:
    SDL_RenderFillRects(SDLRenderer, @Rects[0], Length(Rects));

  end;// case State of

  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
begin
  Window.PushRenderSize(400, 400);
  Render.PushDrawColor(1, 0, 1);
  Render.DebugText(0, 0, PAnsiChar(sState));
  Render.DebugText(0, 10, '[F1] Toggle help');
  Render.DebugText(0, 20, '[C] Change color');
  Render.DebugText(0, 30, '[P] Change Points');
  Render.DebugText(0, 40, '[M] Change Mode');
  Render.PopDrawColor;
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

        SDLK_C: InitColors;

        SDLK_P: InitPoints;

        SDLK_M: ChangeState;

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
  SDL_SetAppMetadata(PAnsiChar(ProgName), kProgVersion,
    PAnsiChar('com.chixpy.' + ProgName));
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_CREATOR_STRING, 'Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_COPYRIGHT_STRING,
    '(C) 2026 Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_URL_STRING,
    'https://github.com/Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_TYPE_STRING, 'application');

  SDL3Eng := cSDL3Eng.Create(ExtractFileName(ParamStr(0)), kRenderW, kRenderH,
    kWinScale, kFullScreen, kRDriver);
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
