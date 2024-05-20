program CTC006;
{< The Coding Train Challenge #006 - Mitosis Simulation }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/jxGS3fKPKJA
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, fgl,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  ucCTCCell;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

type
  TCellList = specialize TFPGObjectList<cCTCCell>;

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
    Cells : TCellList;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    aPoint : TPoint;
  begin
    Cells := TCellList.Create(True);
    Randomize;

    aPoint := Point(Random(WinW div 2) + WinW div 4,
      Random(WinH div 2) + WinH div 4);
    Cells.Add(cCTCCell.Create(aPoint, RandomRange(50, 120), $FF0000FF));

    aPoint := Point(Random(WinW div 2) + WinW div 4,
      Random(WinH div 2) + WinH div 4);
    Cells.Add(cCTCCell.Create(aPoint, RandomRange(50, 120), $FF00FFFF));
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    Cells.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    aCell : cCTCCell;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    for aCell in Cells do
      aCell.Move;
  end;

  procedure cCTCEng.Draw;
  var
    aCell : cCTCCell;
  begin
    // Background
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    // Cells
    for aCell in Cells do
    begin
      filledCircleColor(SDLWindow.PRenderer, aCell.Pos.X, aCell.Pos.Y,
        Round(aCell.R), aCell.C);
      circleRGBA(SDLWindow.PRenderer, aCell.Pos.X, aCell.Pos.Y,
        Round(aCell.R), 0, 0, 0, 255);
    end;
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  var
    i : integer;
    aCell : cCTCCell;
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

      //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        i := Cells.Count - 1;
        while i >= 0 do
        begin
          aCell := Cells[i];
          if aCell.Clicked(aEvent.button.x, aEvent.button.y) then
          begin
            if aCell.R > 10 then
            begin
              Cells.Add(aCell.Mitosis);
              Cells.Add(aCell.Mitosis);
            end;
            Cells.Delete(i);
          end;

          Dec(i);
        end;
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
