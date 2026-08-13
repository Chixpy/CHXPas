program TestRect;
{<
  A simple program with cCHXSDL3Engine for testing Rect primitive.

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kNRects = 30;
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
    Rects: Array of TSDL_FRect;
    Colors: Array of TSDL_FColor;
    FillMode: Boolean;

    procedure InitRects;
    procedure InitColors;

  end;

{ cSDL3Eng }

procedure cSDL3Eng.InitRects;
var
  i: Integer;
  X, Y: CFloat;
begin
  for i := Low(Rects) to High(Rects) do
  begin
    X := Random * kRenderW; Y := Random * kRenderH;
    Rects[i].Init(X, Y, Random * (kRenderW - X), Random * (kRenderH - Y));
  end;
end;

procedure cSDL3Eng.InitColors;
var
  i: Integer;
begin
  for i := Low(Colors) to High(Colors) do
    Colors[i].Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True;

  SetLength(Rects, kNRects);
  InitRects;
  SetLength(Colors, kNRects + 1);
  InitColors;

  FillMode := False;
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
var
  i: Integer;
begin
  Render.SetDrawColor(1, 1, 1);
  Render.Clear(0, 0, 0);

  i := 0;
  while i <= High(Rects) do
  begin
    if  FillMode then
    begin
      Render.SetDrawColor(Colors[i]);
      Render.RectFilled(Rects[i]);
    end
    else
      Render.Rect(Rects[i], Colors[i], Colors[i + 1]);
    Inc(i);
  end;

  Render.SetDrawColor(1, 0, 1);
  Render.DebugText(2, 10, '[C] Change color');
  Render.DebugText(2, 20, '[R] Change rectangles');
  Render.DebugText(2, 30, '[F] Change mode');
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

        SDLK_C: InitColors;

        SDLK_R: InitRects;

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
