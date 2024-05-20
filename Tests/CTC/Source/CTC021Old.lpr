program CTC021Old;
{< The Coding Train Challenge #021 - Mandelbrot }

// CHX: Direct drawing pixels on render as CT013.
//   It's About 20% slower than texture direct pixel editing. And very much
//   slower, if hardware acceleration is used.

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/6z7GQewK-Ks
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  // CHX: Making values as constants

  // Establish a range of values on the complex plane
  // A different range will allow us to "zoom" in or out on the fractal

  // It all starts with the width, try higher or lower values
  w = 5;
  h = (w * WinH) / WinW;

  // Maximum number of iterations for each point on the complex plane
  maxiterations = 255;

  // Start at negative half the width and height
  xmin = -w / 2;
  ymin = -h / 2;

  // x goes from xmin to xmax
  xmax = xmin + w;
  // y goes from ymin to ymax
  ymax = ymin + h;

  // Calculate amount we increment x,y for each pixel
  dx = (xmax - xmin) / WinW;
  dy = (ymax - ymin) / WinH;

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

  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  begin

  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }

  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

  end;

  procedure cCTCEng.Draw;
  var
    x, y, a, b, aa, bb, twoab : Double; // fractal coords.
    n, i, j : integer; // screen coords.
  begin
    y := ymin;

    j := 0;
    while j < WinH do
    begin
      x := xmin;

      i := 0;
      while i < WinW do
      begin
        // Now we test, as we iterate z = z^2 + cm does z tend towards infinity?
        a := x;
        b := y;

        n := 0;
        while n < maxiterations do
        begin
          aa := a * a;
          bb := b * b;
          twoab := 2.0 * a * b;

          a := aa - bb + x;
          b := twoab + y;

          if (a * a + b * b) > 16 then
            Break; // CHX: Ouhg... :-(

          Inc(n);
        end;

        // We color each pixel based on how long it takes to get to infinity
        // If we never got there, let's pick the color black
        if n >= maxiterations then
        begin
          //pixelRGBA(SDLWindow.PRenderer, i, j, 0, 0, 0, 255)

          SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
          SDL_RenderDrawPoint(SDLWindow.PRenderer, i, j);
        end
        else
        begin
          // Gosh, we could make fancy colors here if we wanted
          n := round(sqrt(n / maxiterations) * 255);
          //n := round(n / maxiterations * 255);

          //pixelRGBA(SDL2R, i, j, n, n, n, 255);

          SDL_SetRenderDrawColor(SDLWindow.PRenderer, n, n, n, 255);
          SDL_RenderDrawPoint(SDLWindow.PRenderer, i, j);
        end;

        x += dx;

        Inc(i);
      end;

      y += dy;

      Inc(j);
    end;
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

    //case aEvent.type_ of
    //  SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
    //  begin
    //      case aEvent.key.keysym.sym of
    //        //SDLK_UP : ;
    //        //SDLK_DOWN : ;
    //        //SDLK_LEFT : ;
    //        //SDLK_RIGHT : ;
    //        //SDLK_SPACE : ;
    //        else
    //          ;
    //      end;
    //  end;

    //  //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
    //  //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
    //  //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
    //  //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);

    //  else
    //    ;
    //end;
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
    CTCEng.Config.RendererUseHW := False; // True = 3~5 times slower than soft.
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
