program CTC035_1;
{< The Coding Train Challenge #035.1 - Traveling Salesperson }

// Coding Train
// Ported to processing by Max (https://github.com/TheLastDestroyer)
// Origional JS by Daniel Shiffman
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/BAejnwN4Ccw
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPoint3DF, uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 400; { CHX: Window logical width. }
  WinH = 400; { CHX: Window logical height. }

  totalCities = 20;

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
    cities : array of TPoint3DF;
    recordDistance : float;
    bestEver : array of TPoint3DF;

    procedure swap(var a : array of TPoint3DF; i, j : integer);
    function calcDistance(var points : array of TPoint3DF) : float;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin
    SetLength(cities, totalCities);
    SetLength(bestEver, totalCities);

    for i := 0 to totalCities - 1 do
      cities[i].Init(random(WinW), random(WinH));

    recordDistance := calcDistance(cities);
    bestEver := Copy(cities);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }

  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    i, j : integer;
    d : Float;
  begin
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

    i := random(length(cities));
    j := random(length(cities));

    swap(cities, i, j);

    d := calcDistance(cities);
    if d < recordDistance then
    begin
      recordDistance := d;
      bestEver := Copy(cities);
    end;
  end;

  procedure cCTCEng.Draw;
  var
    aPoint : TPoint3DF;
    PointList : array of TSDL_FPoint;
    Xs, Ys : array of CInt16;
    i : integer;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    for aPoint in cities do
      filledCircleColor(SDLWindow.PRenderer, Round(aPoint.x), Round(aPoint.y),
        4, $FFFFFFFF);

    // CHX: Using SDL_RenderDrawLines(F)
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    SetLength(PointList, Length(cities) + 1);
    i := 0;
    while i < Length(cities) do
    begin
      PointList[i] := SDLFPoint(cities[i].x, cities[i].y);
      Inc(i);
    end;
    // Closing line
    PointList[Length(PointList) - 1] := PointList[0];
    SDL_RenderDrawLinesF(SDLWindow.PRenderer, @PointList[0],
      Length(PointList));

    // CHX: Using SDL_GFX.polygonRGBA/Color
    SetLength(Xs, Length(bestEver));
    SetLength(Ys, Length(bestEver));
    i := 0;
    while i < Length(bestEver) do
    begin
      Xs[i] := Round(bestEver[i].x);
      Ys[i] := Round(bestEver[i].y);
      Inc(i);
    end;
    polygonColor(SDLWindow.PRenderer, @Xs[0], @Ys[0], Length(Xs), $FF00FFFF);

    // CHX: I found this AFTER implementing cCHXSDL2Font U_U
    //   but 8x8 is too small and ASCII only. Although it's configurable...
    stringColor(SDLWindow.PRenderer, 10, 10, PChar(FloatToStr(recordDistance)),
      $FFFF00FF);
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    inherited HandleEvent(aEvent, Handled, ExitProg);
    if ExitProg then Exit; { CHX: Inherited HandleEvent can change ExitProg. }

    { CHX: Some common events for fast reference, CTRL+MAYS+U removes comments
        while selecting the block.
      You can see full list in sdlevents.inc
      Window and general quit events are handled automatically in parent.
      Escape key is mapped to exit the program too.
    }

    //case aEvent.type_ of
    //  SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
    //  begin
    //    case aEvent.key.keysym.sym of
    //      //SDLK_UP : ;
    //      //SDLK_DOWN : ;
    //      //SDLK_LEFT : ;
    //      //SDLK_RIGHT : ;
    //      //SDLK_SPACE : ;
    //      else
    //        ;
    //    end;
    //  end;
    //  //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
    //  //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
    //  //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
    //  //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);
    //  else
    //    ;
    //end;
  end;

  procedure cCTCEng.swap(var a : array of TPoint3DF; i, j : integer);
  var
    temp : TPoint3DF;
  begin
    temp := a[i];
    a[i] := a[j];
    a[j] := temp;
  end;

  function cCTCEng.calcDistance(var points : array of TPoint3DF) : float;
  var
    i : integer;
  begin
    Result := 0;

    i := 0;
    while i < Length(points) - 2 do
    begin
      Result += points[i].Distance(points[i + 1]);
      Inc(i);
    end;
    // CHX: we are not adding the return to home :-P
    Result += points[i].Distance(points[0]);
  end;

  { Main program }

var
  BaseFolder : string;
  CTCEng : cCTCEng;

  {$R *.res}

begin
  // Changing base folder to parents .exe folder.
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
