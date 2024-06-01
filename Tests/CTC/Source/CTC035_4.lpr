program CTC035_4;
{< The Coding Train Challenge #035.4 - Traveling Salesperson (Genetic Algorithm) }

// Coding Train
// Ported to processing by Max (https://github.com/TheLastDestroyer)
// Origional JS by Daniel Shiffman
// http://patreon.com/codingtrain
// Code for this video: https://www.youtube.com/watch?v=M3KTWnTrU_c
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, fgl,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPoint3DF, uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  uCTCGA;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  totalCities = 11;
  popSize = 500;

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

    population : array of cIntList;
    fitness : array of float;

    recordDistance : float;
    bestEver : cIntList;
    currentBest : cIntList;

    function calcDistance(var points : array of TPoint3DF;
      orders : cIntList) : float;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    order, orderCopy : cIntList;
    i : integer;
  begin
    recordDistance := Infinity;
    bestEver := cIntList.Create;
    currentBest := cIntList.Create;

    order := cIntList.Create;
    SetLength(cities, totalCities);
    SetLength(population, popSize);
    SetLength(fitness, popSize);

    for i := 0 to totalCities - 1 do
    begin
      cities[i] := TPoint3DF.Create(Random(WinW), Random(WinH div 2));
      order.Add(i);
    end;

    orderCopy := cIntList.Create;
    orderCopy.Assign(order); // CHX: Copy order to orderCopy, but Is it needed?
    for i := 0 to popSize - 1 do
    begin
      orderCopy.shuffle();
      population[i] := cIntList.Create;
      population[i].Assign(orderCopy);
    end;

    FreeAndNil(orderCopy);
    FreeAndNil(order);
  end;

  procedure cCTCEng.Finish;
  var
    pop : cIntList;
  begin
    { CHX: Free any created objects. }
    for pop in population do
      pop.Free;
    bestEver.Free;
    currentBest.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    currentRecord, d : Float;
    i : integer;
    newPopulation : array of cIntList;
    orderA , orderB, order, pop: cIntList;
  begin
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

    // Genetic Algorithm
    // CHX: Heavy use of global variables in some functions.
    //void calculateFitness()
    currentRecord := Infinity;
    for i := 0 to Length(population) - 1 do
    begin
      d := calcDistance(cities, population[i]);
      if d < recordDistance then
      begin
        recordDistance := d;
        bestEver.Assign(population[i]);
      end;
      if d < currentRecord then
      begin
        currentRecord := d;
        currentBest.Assign(population[i]);
      end;
      //    // This fitness function has been edited from the original video
      //    // to improve performance, as discussed in The Nature of Code 9.6 video,
      //    // available here: https://www.youtube.com/watch?v=HzaLIO9dLbA
      fitness[i] := 1 / (Power(d, 8) + 1);
    end;

    normalizeFitness(fitness);

    //void nextGeneration()
    SetLength(newPopulation, popSize);
    for i := 0 to Length(newPopulation) - 1 do
    begin
      orderA := pickOne(population, fitness);
      orderB := pickOne(population, fitness);
      order := crossOver(orderA, orderB);
      mutate(order, 0.01);
      newPopulation[i] := order; // CHX: Don't free order
      orderA.Free;
      orderB.Free;
    end;
    // CHX: freeing previous population
    for pop in population do
      pop.Free;
    population := newPopulation;
  end;

  procedure cCTCEng.Draw;
  var
    PointList : array of TSDL_FPoint;
    i : integer;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    // Best path
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 128);
    SetLength(PointList, bestEver.Count + 1);
    i := 0;
    while i < bestEver.Count do
    begin
      PointList[i] := SDLFPoint(cities[bestEver[i]].x, cities[bestEver[i]].y);
      filledCircleColor(SDLWindow.PRenderer, Round(cities[bestEver[i]].x),
        Round(cities[bestEver[i]].y), 8, $FFFFFF88);
      Inc(i);
    end;
    // CHX: Closing line
    PointList[High(PointList)] := PointList[0];
    SDL_RenderDrawLinesF(SDLWindow.PRenderer, @PointList[0],
      Length(PointList));

    // Best current path
    // translate(0, height / 2);
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 128);
    SetLength(PointList, currentBest.Count + 1);
    i := 0;
    while i < currentBest.Count do
    begin
      PointList[i] := SDLFPoint(cities[currentBest[i]].x,
        cities[currentBest[i]].y + (WinH div 2));
      filledCircleColor(SDLWindow.PRenderer, Round(cities[currentBest[i]].x),
        Round(cities[currentBest[i]].y + (WinH div 2)), 8, $FFFFFF88);
      Inc(i);
    end;
    // CHX: Closing line
    PointList[High(PointList)] := PointList[0];
    SDL_RenderDrawLinesF(SDLWindow.PRenderer, @PointList[0],
      Length(PointList));

    // CHX: Added some info
    stringColor(SDLWindow.PRenderer, 10, WinH div 2,
      PChar(FloatToStrF(recordDistance, ffFixed, 4, 4)), $FFFF00FF);
    stringColor(SDLWindow.PRenderer, 10, WinH div 2 + 10,
      PChar(IntToStr(FrameCount)), $FFFF00FF);
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

    function cCTCEng.calcDistance(var points : array of TPoint3DF;
    orders : cIntList) : float;
  var
    i, cityAIndex, cityBIndex : integer;
    cityA, cityB : TPoint3DF;
  begin
    Result := 0;

    i := 0;
    while i < orders.count do
    begin
      cityAIndex := orders[i];
      cityA := points[cityAIndex];
      //CHX: We are not adding the return to home :-P, fixing it by mod
      cityBIndex := orders[(i + 1) mod orders.count];
      cityB := points[cityBIndex];

      Result += cityA.Distance(cityB);
      Inc(i);
    end;
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
