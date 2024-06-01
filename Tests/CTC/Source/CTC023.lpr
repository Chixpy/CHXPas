program CTC023;
{< The Coding Train Challenge #023 - 2D Supershapes }

// Coding Train
// http://thecodingtrain.com
// http://patreon.com/codingtrain

// Code for: https://youtu.be/ksRoh-10lak
// Processing port by Max (https://github.com/TheLastDestroyer)

// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  // all floats to negate integer devision errors
  n1 = 0.3;
  n2 = 0.3;
  n3 = 0.3;

  a = 1;
  b = 1;

  radius = 300;
  total = 200;

  increment = pi * 2 / total;

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
    m : Float;
    osc : Float;

    PointList : array of TSDL_Point;

    function super_shape(theta : Float) : Float;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  begin
    m := 0;
    osc := 0;

    SetLength(PointList, total + 1);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    SetLength(PointList, 0);
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    r, angle : Float;
    Count : integer;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    m := map(Sin(osc), -1.0, 1.0, 0.0, 10.0);
    osc += 0.02;

    angle := 0;
    Count := 0;
    while angle < (pi * 2) do
    begin
      r := super_shape(angle);

      //translate(width/2, height/2);
      PointList[Count].x := WinW div 2 + Round(radius * r * cos(angle));
      PointList[Count].y := WinH div 2 + Round(radius * r * sin(angle));

      angle += increment;
      Inc(Count);
    end;
  end;

  procedure cCTCEng.Draw;
  begin
    // Background
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 51, 51, 51, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    SDL_RenderDrawLines(SDLWindow.PRenderer, @PointList[0], total);
    // CHX: Closing the shape
    SDL_RenderDrawLine(SDLWindow.PRenderer, PointList[total - 1].x,
      PointList[total - 1].y, PointList[0].x, PointList[0].y);
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

  function cCTCEng.super_shape(theta : Float) : Float;
  var
    part1, part2, part3 : Float;
  begin
    part1 := (1 / a) * cos(theta * m / 4);
    part1 := abs(part1);
    part1 := Power(part1, n2);

    part2 := (1 / b) * sin(theta * m / 4);
    part2 := abs(part2);
    part2 := Power(part2, n3);

    part3 := Power(part1 + part2, 1 / n1);

    if part3 = 0 then
      Result := 0
    else
      Result := 1 / part3;
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
