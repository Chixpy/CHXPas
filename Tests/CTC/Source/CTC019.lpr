program CTC019;
{< The Coding Train Challenge #019 - Superellipse }

// Daniel Shiffman
// https://thecodingtrain.com/CodingChallenges/019-superellipse.html
// https://youtu.be/z86cx2A4_3E
// https://editor.p5js.org/codingtrain/sketches/Hk-1AMTgN
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  ucCTCSlider;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  // CHX:
  Incr = 0.001; // Increment beetwen points
  TWO_PI = PI * 2;
  OffsetX = WinW div 2;
  OffsetY = WinH div 2;

type

  { cCTCEng }

  cCTCEng = class(cCHXSDL2Engine)
  protected
    procedure Setup; override;
    procedure Finish; override;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw; override;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override;

  public
    { CHX: Global variables. }
    slider : cCTSlider;
    // CHX: Added to store the shape
    points : array of TSDL_FPoint;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  begin
    SetLength(points, Ceil(TWO_PI / Incr));

    slider := cCTSlider.Create(-WinW div 2 + 20 + OffsetX,
      -WinH div 2 + 20 + OffsetY, 0.01, 4, 2, 0.01);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    slider.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    a, b, n, angle, na : Double;
    aPoint : TSDL_FPoint;
    idx : integer;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    a := 100;
    b := 100;
    n := slider.Value;
    //n := 0.7;

    angle := 0;
    idx := 0;
    while angle < TWO_PI do
    begin
      //// Simple ellipse
      //aPoint.x := 200 * cos(angle) + WinW / 2;
      //aPoint.y := 200 * sin(angle) + WinH / 2;

      // Superellipse
      na := 2 / n;
      aPoint.x := Power(abs(cos(angle)), na) * a * Sign(cos(angle)) + OffsetX;
      aPoint.y := Power(abs(sin(angle)), na) * b * Sign(sin(angle)) + OffsetY;

      Points[idx] := aPoint;
      Inc(idx);
      angle := angle + Incr;
    end;
  end;

  procedure cCTCEng.Draw;
  var
    x0, x1, y0, y1 : integer;
  begin
    // Background
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 51, 51, 51, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    //translate(width / 2, height / 2);

    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    SDL_RenderDrawPointsF(SDLWindow.PRenderer, @points[0], Length(points));


    // cCTSlider.show
    x0 := slider.x;
    y0 := slider.y + slider.h div 3;
    x1 := slider.w + x0;
    y1 := slider.h div 3 + y0;

    rectangleRGBA(SDLWindow.PRenderer, x0, y0, x1, y1, 255, 255, 255, 255);

    x0 := round(slider.xPosition(slider.Value)) - slider.controlSize div 2;
    y0 := slider.y;
    x1 := slider.controlSize + x0;
    y1 := slider.h + y0;

    boxRGBA(SDLWindow.PRenderer, x0, y0, x1 - 1, y1 - 1, 128, 128, 128, 255);
    rectangleRGBA(SDLWindow.PRenderer, x0, y0, x1, y1, 200, 200, 200, 255);
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    inherited HandleEvent(aEvent, Handled, ExitProg);
    if ExitProg then Exit; { CHX: Inherited Draw can change ExitProg. }

    { CHX: Some common events for fast reference, CTRL+MAYS+U removes comments
        while selecting the block.
      You can see full list in sdlevents.inc
      Window and general quit events are handled automatically in parent.
      Escape key is mapped to exit the program too.
    }

    case aEvent.type_ of
      //SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      //begin
      //    case aEvent.key.keysym.sym of
      //      //SDLK_UP : ;
      //      //SDLK_DOWN : ;
      //      //SDLK_LEFT : ;
      //      //SDLK_RIGHT : ;
      //      //SDLK_SPACE : ;
      //      else
      //        ;
      //    end;
      //end;

      SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      begin
        if aEvent.motion.state <> 0 then
          slider.handleInteractions(aEvent.motion.x, aEvent.motion.y, True);
      end;
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        slider.handleInteractions(aEvent.button.x, aEvent.button.y, False);
      end;
        //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);

      else
        ;
    end;
  end;

  { Main program }

var
  BaseFolder : string;
  CTCEng : cCTCEng;

  {$R *.res}

begin
  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  try
    CTCEng := cCTCEng.Create(ApplicationName, 'CHXSDL.ini', False);
    CTCEng.Config.WindowWidth := WinW;
    CTCEng.Config.RendererWidth := WinW;
    CTCEng.Config.WindowHeight := WinH;
    CTCEng.Config.RendererHeight := WinH;
    CTCEng.Config.RendererUseHW := True;
    CTCEng.Init;
    CTCEng.Run;
  finally
    FreeAndNil(CTCEng);
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
