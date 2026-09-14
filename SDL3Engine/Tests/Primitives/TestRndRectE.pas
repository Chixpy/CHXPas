program TestRndRectE;
{<
  A simple program with cCHXSDL3Engine for testing Rounded Rectangle with
    Ellipse primitive.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kLenStep = 0.25;
  kRadStep = 0.25;

  kRenderH = 50;
  kRenderW = kRenderH * 4 div 3;
  kWinScale = 900 div kRenderH;
  kFullScreen = False;
  kRDriver = '';
  kProgVersion = '1.0';

type

  TState = (stBorFill, stTBorFill, stBorder, stTBorder, stFilled, stTFilled);

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
    Color1, Color2: TSDL_FColor;

    ShowGuide: Boolean;
    RWidth, RHeight, RadiusX, RadiusY: CFloat;

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
    stBorFill : sState := 'Border + Only Fill';
    stTBorFill : sState := 'Triangles Border + Only Fill';
    stBorder : sState := 'Border';
    stTBorder : sState := 'Triangles Border';
    stFilled : sState := 'Full Filled';
    stTFilled : sState := 'Triangles Filled';
  otherwise
    sState := '<Undefined>';
  end;
end;

procedure cSDL3Eng.InitColors;
begin
  Color1.Init(Random, Random, Random, Random);
  Color2.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True; ShowHelp := True;
  State := High(TState); ChangeState;
  InitColors;

  ShowGuide := False;
  RWidth := kRenderW * 0.8;
  RHeight := kRenderH * 0.6;
  RadiusX := kRenderH * 0.25;
  RadiusY := kRenderH * 0.15;
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
  CX, CY: CFloat;
begin
  Render.Clear(0.05);
  Render.SetDrawColor(Color1);

  aRect := SDLFRect((kRenderW - RWidth) * 0.5,
    (kRenderH - RHeight) * 0.5, RWidth, RHeight);
  CX := (kRenderW - 1) * 0.5; CY := (kRenderH - 1) * 0.5;

  case State of

    stBorFill :
    begin
      Render.RndRectE(aRect, RadiusX, RadiusY, Color1, Color2);
      if ShowGuide then
      begin
        Render.Rect(aRect, SDLFColor(1, 0.4), SDLFColor(1, 0.3));
        Render.Ellipse(CX, CY, RadiusX, RadiusY,
          SDLFColor(1, 0.4), SDLFColor(1, 0.3));
      end;
    end;

    // stTBorFill :
    // begin
    //   Render.TRndRectE(aRect, RadiusX, RadiusY, Color1, Color2);
    //   if ShowGuide then
    //   begin
    //     Render.TRect(aRect, Color1, Color2);
    //     Render.TEllipse(CX, CY, Radius, Color1, Color2);
    //   end;
    // end;

    stBorder :
    begin
      Render.RndRectEBorder(aRect, RadiusX, RadiusY);
      if ShowGuide then
      begin
        Render.SetDrawColor(1, 0.3);
        Render.RectBorder(aRect);
        Render.EllipseBorder(CX, CY, RadiusX, RadiusY);
      end;
    end;

    // stTBorder :
    // begin
    //   Render.TRndRectEBorder(aRect, RadiusX, RadiusY);
    //   if ShowGuide then
    //   begin
    //     Render.SetDrawColor(1, 0.3);
    //     Render.TRectBorder(aRect);
    //     Render.TEllipseBorder(CX, CY, RadiusX, RadiusY);
    //   end;
    // end;

    stFilled :
    begin
      Render.RndRectEFilled(aRect, RadiusX, RadiusY);
      if ShowGuide then
      begin
        Render.SetDrawColor(1, 0.3);
        Render.RectFilled(aRect);
        Render.EllipseFilled(CX, CY, RadiusX, RadiusY);
      end;
    end;

    // stTFilled :
    // begin
    //   Render.TRndRectEFilled(aRect, RadiusX, RadiusY);
    //   if ShowGuide then
    //   begin
    //     Render.SetDrawColor(1, 0.3);
    //     Render.TRectFilled(aRect);
    //     Render.TEllipseFilled(CX, CY, RadiusX, RadiusY);
    //   end;
    // end;

  end; // case State of

  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
begin
  Window.PushRenderSize(Window.WindowWidth div 2, Window.WindowHeight div 2);
  Render.PushDrawColor(1, 0, 1);
  Render.DebugTextF(0, 0, '%s', [sState]);
  Render.DebugTextF(0,10, 'DX: %g DY: %g RX: %g RY: %g',
    [RWidth, RHeight, RadiusX, RadiusY]);
  Render.DebugText(0, 20, '[F1] Toggle help');
  Render.DebugText(0, 30, '[F] Change mode');
  Render.DebugText(0, 40, '[C] Change color');
  Render.DebugText(0, 50, '[<=] [=>] Change width');
  Render.DebugText(0, 60, '[UP] [DOWN] Change height');
  Render.DebugText(0, 70, '[WASD] Change radius X and Y');
  Render.DebugText(0, 80, '[G] Toggle guides');
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

        SDLK_UP: RHeight += kLenStep;

        SDLK_DOWN: RHeight -= kLenStep;

        SDLK_RIGHT: RWidth += kLenStep;

        SDLK_LEFT: RWidth -= kLenStep;

        SDLK_W: RadiusY += kRadStep;

        SDLK_S: RadiusY -= kRadStep;

        SDLK_A: RadiusX += kRadStep;

        SDLK_D: RadiusX -= kRadStep;

        SDLK_C: InitColors;

        SDLK_F: ChangeState;

        SDLK_G: ShowGuide := not ShowGuide;

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
