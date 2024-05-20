program CTC010;
{< The Coding Train Challenge #010 - Maze Generator }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain

// Videos
// https://youtu.be/HyK_Q5rrcr4
// https://youtu.be/D8UgRyRnvXU
// https://youtu.be/8Ju_uxJ9v44
// https://youtu.be/_p5IH0L63wo

// Depth-first search
// Recursive backtracker
// https://en.wikipedia.org/wiki/Maze_generation_algorithm
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  ucCTCCell;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  w = 19;

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
    Cols : integer;
    Rows : integer;
    Grid : TCellList;
    Stack : TGenCellList;
    Current : cCTCCell;

    Speed : CUInt32; // CHX: To handle speed with Up/Down

    procedure removeWalls(a, b : cCTCCell);
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    j, i : integer;
  begin
    Randomize;
    Cols := WinW div w;
    Rows := WinH div w;

    Speed := 210;

    Grid := TCellList.Create(True);
    Grid.Cols := Cols;
    Grid.Rows := Rows;

    Stack := TGenCellList.Create(False);

    for j := 0 to Rows - 1 do
      for  i := 0 to Cols - 1 do
        Grid.Add(cCTCCell.Create(i, j));

    Current := Grid[0];
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    Stack.Free;
    Grid.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    Next : cCTCCell;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;
    Current.visited := True;

    // STEP 1
    Next := Current.CheckNeighbors(Grid);
    if assigned(Next) then
    begin
      Next.visited := True;

      // STEP 2
      stack.add(current);

      // STEP 3
      removeWalls(current, Next);

      // STEP 4
      current := Next;
    end
    else if (stack.Count > 0) then
    begin
      // current = stack.Extract(stack.Last);
      // Faster
      Current := stack[stack.Count - 1];
      Stack.Delete(stack.Count - 1);
    end;
  end;

  procedure cCTCEng.Draw;
  var
    aCell : cCTCCell;
    x, y : integer;
  begin
    // Background
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    // Grid
    for aCell in grid do
    begin
      y := aCell.j * w;
      x := aCell.i * w;

      if (aCell.walls[0]) then
        hlineRGBA(SDLWindow.PRenderer, x, x + w, y, 255, 255, 255, 255);
      if (aCell.walls[1]) then
        vlineRGBA(SDLWindow.PRenderer, x + w, y, y + w, 255, 255, 255, 255);
      if (aCell.walls[2]) then
        hlineRGBA(SDLWindow.PRenderer, x + w, x, y + w, 255, 255, 255, 255);
      if (aCell.walls[3]) then
        vlineRGBA(SDLWindow.PRenderer, x, y + w, y, 255, 255, 255, 255);

      // Visited
      if (aCell.visited) then
        boxRGBA(SDLWindow.PRenderer, x, y, x + w, y + w, 255, 0, 255, 100);
    end;

    // Current
    if assigned(current) then
    begin
      y := Current.j * w;
      x := Current.i * w;
      boxRGBA(SDLWindow.PRenderer, x, y, x + w, y + w, 0, 0, 255, 100);
    end;

    SDL_Delay(Speed);
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
          SDLK_UP : if Speed > 50 then Speed -= 50;
          SDLK_DOWN : Speed += 50;
            //SDLK_LEFT : ;
            //SDLK_RIGHT : ;
            //SDLK_SPACE : ;
          else
            ;
        end;
      end;

        //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
        //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);

      else
        ;
    end;
  end;

  procedure cCTCEng.removeWalls(a, b : cCTCCell);
  var
    z : integer;
  begin
    z := a.i - b.i;
    if (z = 1) then
    begin
      a.walls[3] := False;
      b.walls[1] := False;
    end
    else if (z = -1) then
    begin
      a.walls[1] := False;
      b.walls[3] := False;
    end;

    z := a.j - b.j;
    if (z = 1) then
    begin
      a.walls[0] := False;
      b.walls[2] := False;
    end
    else if (z = -1) then
    begin
      a.walls[2] := False;
      b.walls[0] := False;
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
