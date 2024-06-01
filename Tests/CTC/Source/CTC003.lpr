program CTC003;
{< The Coding Train Challenge #003 - Snake Game }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/AaGK-fj-BAM
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, uCHXSDL2Utils,
  ucCTCSnake;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  scl = 20; // Size of the grid

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
    s : cCTCSnake;
    Food : TPoint;

    procedure pickLocation;
  end;

  { cCTCEng }

  procedure cCTCEng.pickLocation;
  var
    cols, rows : integer;
  begin
    cols := floor(WinW div scl);
    rows := floor(WinH div scl);

    Food := Point(random(cols) * scl, random(rows) * scl);
  end;

  procedure cCTCEng.Setup;
  begin
    s := cCTCSnake.Create;
    pickLocation;
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    s.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    if s.eat(food) then
      pickLocation;
    s.death;
    s.update;
  end;

  procedure cCTCEng.Draw;
  var
    i : integer;
    SDL2R : PSDL_Renderer;
  begin
    SDL2R := SDLWindow.PRenderer;
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 0);
    SDL_RenderClear(SDL2R);

    // Food
    filledCircleRGBA(SDL2R, food.x + scl shr 1, food.y + scl shr
      1, scl shr 1, 255, 0, 0, 255);

    // Snake.show
    i := length(s.tail);
    while i > 0 do
    begin
      Dec(i);
      boxRGBA(SDL2R, s.tail[i].x, s.tail[i].y, s.tail[i].x +
        scl, s.tail[i].y + scl, 255, 255, 255, 255);
    end;
    boxRGBA(SDL2R, s.x, s.y, s.x + scl, s.y + scl, 255, 255, 255, 255);

    SDL_Delay(100); // TODO: Until FrameRate can be set in cSDL2Engine
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

      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          SDLK_UP : s.dir(0, -1);
          SDLK_DOWN : s.dir(0, 1);
          SDLK_LEFT : s.dir(-1, 0);
          SDLK_RIGHT : s.dir(1, 0);
            //SDLK_SPACE : ;
          else
            ;
        end;
      end;

      //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        s.eat(Point(s.x, s.y));
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
    CTCEng := cCTCEng.Create(ApplicationName, WinW, WinH, True, False);
    CTCEng.Config.DefFontSize := WinH div 25;
    // Actually,they are less than 25 lines because of LineHeight
    CTCEng.Config.DefFontColor := SDLColor(255,255,255,255);
    CTCEng.Config.DefFontFile := 'FreeMonoBold.ttf';  
    CTCEng.ShowFrameRate := True;
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
