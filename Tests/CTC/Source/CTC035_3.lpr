program CTC035_3;
{< The Coding Train Challenge #035.3 - Traveling Salesperson (Lexicographic Order) }

// Coding Train
// Ported to processing by Max (https://github.com/TheLastDestroyer)
// Origional JS by Daniel Shiffman
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/9Xy-LMAfglE
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPoint3DF, uCHXStrUtils, uCHXMath, // Fastest Factorial 🤣
  ucCHXSDL2Engine, uaCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  totalCities = 11;
  {< CHX: 12 have a lot of Permutations, and we must use UInt64 for more. }

  IterFrame = 25000;
  {< CHX: Iterations per frame, try to keep around 10 ms of compute time. }

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
    order : array of integer;

    totalPermutations : integer;
    Count : integer;

    recordDistance : float;
    bestEver : array of integer;

    Finished : Boolean;

    procedure swap(var a : array of integer; i, j : integer);
    function calcDistance(var points : array of TPoint3DF;
      var orders : array of integer) : float;
    procedure nextOrder();
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin
    Count := 0;
    Finished := False;
    SetLength(cities, totalCities);
    SetLength(order, totalCities);
    SetLength(bestEver, totalCities);

    for i := 0 to High(cities) do
    begin
      cities[i] := TPoint3DF.Create(random(WinW), random(WinH div 2));
      order[i] := i;
    end;

    recordDistance := calcDistance(cities, order);
    bestEver := copy(order);

    totalPermutations := factorial[totalCities];
    //WriteLn(totalPermutations);
    DefFont.AddStaticStr('TP', totalPermutations.ToString);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }

  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    d : Float;
    i : integer;
  begin
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;
    if Finished then Exit;

    i := 0;
    while (i < IterFrame) and (not Finished) do
    begin
      nextOrder();

      d := calcDistance(cities, order);
      if d < recordDistance then
      begin
        recordDistance := d;
        bestEver := copy(order);
      end;

      Inc(i);
    end;
  end;

  procedure cCTCEng.Draw;
  var
    aPoint : TPoint3DF;
    PointList : array of TSDL_FPoint;
    i, percent : integer;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    for aPoint in cities do
    begin
      filledCircleColor(SDLWindow.PRenderer, Round(aPoint.x),
        Round(aPoint.y), 4, $FFFFFFFF);
      // CHX: Drawing cities in current path
      filledCircleColor(SDLWindow.PRenderer, Round(aPoint.x),
        Round(aPoint.y + WinH div 2), 4, $FFFFFFFF);
    end;

    // Best
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 0, 255, 255);
    SetLength(PointList, Length(bestEver) + 1);
    i := 0;
    while i <= High(bestEver) do
    begin
      PointList[i] := SDLFPoint(cities[bestEver[i]].x, cities[bestEver[i]].y);
      Inc(i);
    end;
    // CHX: Closing line
    PointList[High(PointList)] := PointList[0];
    SDL_RenderDrawLinesF(SDLWindow.PRenderer, @PointList[0],
      Length(PointList));

    // Current
    // translate(0, height / 2);
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    SetLength(PointList, Length(order) + 1);
    i := 0;
    while i <= High(bestEver) do
    begin
      PointList[i] := SDLFPoint(cities[order[i]].x,
        cities[order[i]].y + WinH div 2);
      Inc(i);
    end;
    // CHX: Closing line
    PointList[High(PointList)] := PointList[0];
    SDL_RenderDrawLinesF(SDLWindow.PRenderer, @PointList[0],
      Length(PointList));

    // Tests
    stringColor(SDLWindow.PRenderer, 10, WinH div 2,
      PChar(FloatToStrF(recordDistance, ffFixed, 4, 4)), $FFFF00FF);
    DefFont.RenderStatic('TP', 10, DefFont.LinePosY(1, WinH div 2));

    percent := (Count * 100) div totalPermutations;
    DefFont.RenderDynStr(percent.ToString + '% completed', 10,
      DefFont.LinePosY(2, WinH div 2));

    if Finished then
      DefFont.RenderDynStr('¡Finished!', 10,
        DefFont.LinePosY(3, WinH div 2));
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

  procedure cCTCEng.swap(var a : array of integer; i, j : integer);
  var
    temp : integer;
  begin
    temp := a[i];
    a[i] := a[j];
    a[j] := temp;
  end;

  function cCTCEng.calcDistance(var points : array of TPoint3DF;
  var orders : array of integer) : float;
  var
    i, cityAIndex, cityBIndex : integer;
    cityA, cityB : TPoint3DF;
  begin
    Result := 0;

    //CHX: Static arrays don't have to begin with 0
    i := 0;
    while i < High(orders) do //< i <= High(orders) - 1
    begin
      cityAIndex := orders[i];
      cityA := points[cityAIndex];
      cityBIndex := orders[i + 1];
      cityB := points[cityBIndex];

      Result += cityA.Distance(cityB);
      Inc(i);
    end;
    //CHX: We are not adding the return to home :-P
    Result += points[orders[i]].Distance(points[orders[0]]);
  end;

  procedure cCTCEng.nextOrder();
  var
    largestI, largestJ, i, j : integer;
    endArray : array of integer;
  begin
    Inc(Count);

    // STEP 1 of the algorithm
    // https://www.quora.com/How-would-you-explain-an-algorithm-that-generates-permutations-using-lexicographic-ordering
    largestI := -1;
    i := 0;
    for i := 0 to High(order) - 1 do
      if order[i] < order[i + 1] then largestI := i;
    // CHX: ¿¿??? Why not exit at first found in reverse order?
    //   largestI := -1;
    //   for i := MaxVal - 1 downto 0 do  //CHX: I will use a while
    //     if vals[i] < vals[i + 1] then
    //     begin
    //       largestI := i;
    //       break;
    //     end;

    if largestI = -1 then
    begin
      Finished := True;
      Exit;
    end;

    // STEP 2
    largestJ := -1;
    for j := 0 to High(order) do
      if order[largestI] < order[j] then largestJ := j;

    // STEP 3
    swap(order, largestI, largestJ);

    // STEP 4: reverse from largestI + 1 to the end
    //int size = vals.length - largestI - 1;
    //int[] endArray = new int[size];
    //arrayCopy(vals, largestI + 1, endArray, 0, size);
    //endArray = reverse(endArray);
    //arrayCopy(endArray, 0, vals, largestI+1, size);

    //CHX: Uhm.. There is not reverse funtion for pure arrays in Pascal
    //  we can create one, but lets reversing vals from largestI + 1 in place:
    i := largestI + 1;
    j := High(order);
    while i < j do
    begin
      swap(order, i, j);
      Inc(i);
      Dec(j);
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
