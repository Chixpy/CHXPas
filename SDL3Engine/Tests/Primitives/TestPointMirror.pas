program TestPointMirror;
{<
  A simple program with cCHXSDL3Engine for testing Point Mirror primitives.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kMoveStep = 0.2;

  kRenderH = 20;
  kRenderW = kRenderH * 4 div 3;
  kWinScale = 900 div kRenderH;
  kFullScreen = False;
  kRDriver = '';
  kProgVersion = '1.0';

type

  TState = (stPoint, stHMirror, stVMirror, stHVMirror, stHVMirrorF);

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

    X, Y, X0, Y0, ProjX, ProjY: CFloat;

    DrawHLine, DrawVLine: Boolean;

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
  stPoint: sState := 'Point';
  stHMirror: sState := 'H Mirror';
  stVMirror: sState := 'V Mirror';
  stHVMirror: sState := 'HV Mirror (No Fill)';
  stHVMirrorF: sState := 'HV Mirror Filled';
  otherwise
    ;
  end;
end;

procedure cSDL3Eng.InitColors;
begin
  Color1.Init(Random, Random, Random, Random);
  //Color2.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True; ShowHelp := True;
  State := High(TState); ChangeState;
  InitColors;

  X := kRenderW * 0.25; Y := kRenderH * 0.25;
  X0 := kRenderW * 0.5; Y0 := kRenderH * 0.5;
  DrawHLine := False; DrawVLine := False;
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin
  ProjX := X0 + X; ProjY := Y0 + Y;
end;

procedure cSDL3Eng.Draw;
begin
  Render.Clear(0.05);

  Render.SetDrawColor(1, 0.3);
  Render.Line(X0, 0, X0, kRenderH);
  Render.Line(0, Y0, kRenderW, Y0);

  Render.SetDrawColor(Color1);
  case State of
    stPoint: Render.Point(ProjX, ProjY);

    stHMirror:
      if DrawHLine then
        Render.PointMirrorHFilled(X, ProjY, X0)
      else
        Render.PointMirrorH(X, ProjY, X0);

    stVMirror:
      if DrawVLine then
        Render.PointMirrorVFilled(ProjX, Y, Y0)
      else
        Render.PointMirrorV(ProjX, Y, Y0);

    stHVMirror: Render.PointMirrorHV(X, Y, X0, Y0);

    stHVMirrorF: Render.PointMirrorHVFilled(X, Y,
      DrawHLine, DrawVLine, X0, Y0);

    otherwise ;
  end;

  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
begin
  Window.PushRenderSize(Window.WindowWidth div 2, Window.WindowHeight div 2);
  Render.PushDrawColor(1, 0, 1);
  Render.DebugTextF(0, 0, '%s', [sState]);
  Render.DebugTextF(0, 10, 'X: %g Y: %g', [X, Y]);
  Render.DebugText(0, 20, '[F1] Toggle help');
  Render.DebugText(0, 30, '[F] Change mode');
  Render.DebugText(0, 40, '[C] Change colors');
  Render.DebugText(0, 50, '[ARROWS] Move point');
  Render.DebugText(0, 60, '[H] Horizontal lines');
  Render.DebugText(0, 70, '[V] Vertical lines');
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

        SDLK_F: ChangeState;

        SDLK_C: InitColors;

        SDLK_UP: Y -= kMoveStep;
        SDLK_DOWN: Y += kMoveStep;
        SDLK_LEFT: X -= kMoveStep;
        SDLK_RIGHT: X += kMoveStep;

        SDLK_H: DrawHLine := not DrawHLine;

        SDLK_V: DrawVLine := not DrawVLine;

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
