program TestCircleVsEllipse;
{<
  A simple program with cCHXSDL3Engine that compares Circle and
  Ellipse algorithms.

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  // In actual programs use Window.Render[Width/Height]
  kRenderW = 100; { Renderer width. }
  kRenderH = 100; { Renderer height. }
  kWindowScale = 8; { Scale of the Window. }

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
    Radius: CFloat;
    ShowHelp, ShowCircle, ShowEllipse: Boolean;

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
  Radius := kRenderW div 3;
  ShowHelp := True;
  ShowCircle := True;
  ShowEllipse := True;
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
    if ShowEllipse then
      Render.EllipseFilled(kRenderW div 2, kRenderW div 2, Radius, Radius);
    if ShowCircle then
      Render.CircleFilled(kRenderW div 2, kRenderW div 2, Radius);
  end
  else
  begin
    if ShowEllipse then
      Render.Ellipse(kRenderW div 2, kRenderW div 2, Radius, Radius,
      Color1, Color2);
    if ShowCircle then
      Render.Circle(kRenderW div 2, kRenderW div 2, Radius, Color1, Color2);
  end;

  if ShowHelp then
  begin
    Render.SetDrawColor(1, 0, 1);
    Render.DebugText(2, 10, '[F1] Toggle help');
    Render.DebugText(2, 20, '[C] Change color');
    Render.DebugText(2, 30, '[F] Change mode');
    Render.DebugText(2, 40, '[UP] [DOWN] Change radius');
    Render.DebugText(2, 50, '[E] Toggle ellipse');
    Render.DebugText(2, 60, '[D] Toggle circle');
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

        SDLK_UP: Radius += 1;

        SDLK_DOWN: if Radius > 1 then Radius -= 1;

        SDLK_C: InitColors;

        SDLK_D: ShowCircle := not ShowCircle;

        SDLK_E: ShowEllipse := not ShowEllipse;

        SDLK_F: FillMode := not FillMode;

        SDLK_Q: ExitProg := True;

        SDLK_F1: ShowHelp := not ShowHelp;

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

begin
  SDL3Eng := cSDL3Eng.Create(ExtractFileName(ParamStr(0)), kRenderW, kRenderH,
    False);
  try
    SDL3Eng.Config.WindowWidth := Trunc(kRenderW * kWindowScale);
    SDL3Eng.Config.WindowHeight := Trunc(kRenderH * kWindowScale);
    SDL3Eng.Init;
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
