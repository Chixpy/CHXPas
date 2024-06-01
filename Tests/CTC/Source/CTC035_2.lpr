program CTC035_2;
{< The Coding Train Challenge #035.2 - Traveling Salesperson (Lexicographic Order) }

// Coding Train
// Ported to processing by Max (https://github.com/TheLastDestroyer)
// Origional JS by Daniel Shiffman
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/goUlyp4rwiU
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

  //CHX: Array length - 1
  MaxVal = 6; // CHX: More than 6 is too much time at 60 permutations por second


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
    vals : array of integer;

    Finished : Boolean; // CHX: Added to print Finished message and stop.

    procedure swap(var a : array of integer; i, j : integer);
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin
    DefFont.ChangeFontStyle(DefFont.Color, 64, -1, -1, -1);
    SetLength(vals, MaxVal + 1);
    for i := 0 to MaxVal do
      vals[i] := i;
    Finished := False;
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }

  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    largestI, largestJ, i, j : integer;
    endArray : array of integer;
  begin
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;
    if Finished then Exit;

    // STEP 1 of the algorithm
    // https://www.quora.com/How-would-you-explain-an-algorithm-that-generates-permutations-using-lexicographic-ordering
    largestI := -1;
    i := 0;
    for i := 0 to MaxVal - 1 do
      if vals[i] < vals[i + 1] then largestI := i;
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
    for j := 0 to MaxVal do
      if vals[largestI] < vals[j] then largestJ := j;

    // STEP 3
    swap(vals, largestI, largestJ);

    // STEP 4: reverse from largestI + 1 to the end
    //int size = vals.length - largestI - 1;
    //int[] endArray = new int[size];
    //arrayCopy(vals, largestI + 1, endArray, 0, size);
    //endArray = reverse(endArray);
    //arrayCopy(endArray, 0, vals, largestI+1, size);

    //CHX: Uhm.. There is not reverse funtion for pure arrays in Pascal
    //  we can create one, but lets reversing vals from largestI + 1 in place:
    i := largestI + 1;
    j := MaxVal;
    while i < j do
    begin
      swap(vals, i, j);
      Inc(i);
      Dec(j);
    end;
  end;

  procedure cCTCEng.Draw;
  var
    s : string;
    i : integer;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    s := '';
    for i in vals do
      s += Format('%0:2d', [i]);

    stringColor(SDLWindow.PRenderer, 20, WinH div 2 - 10, PChar(s), $FFFFFFFF);
    DefFont.RenderDynStr(s, 20, WinH div 2 + 10);

    if Finished then
      DefFont.RenderDynStr('¡Finished!', 20, WinH div 2 + 100);
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
