program CTC033;
{< The Coding Train Challenge #033 - Poisson Disc Sampling }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Poisson Disc Sampling: https://youtu.be/flQgnCUxHl
// Processing port by Max: https://github.com/TheLastDestroyer
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, fgl,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPoint3DF, uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 800; { CHX: Window logical height. }

  r = 4;
  k = 30;
type
  TVectorList = specialize TFPGList<TPoint3DF>;

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
    grid : array of TPoint3DF;
    w : float;
    active : TVectorList;
    cols : integer;
    rows : integer;
    ordered : TVectorList;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    x, y : Float;
    i, j : integer;
    aPos : TPoint3DF; // Pos is a Pascal function
  begin
    //Changing default font color
    DefFont.ChangeFontStyle(SDLColor(255,255,0),-1,-1,-1,-1);

    w := r / sqrt(2);

    // STEP 0
    cols := floor(WinW / w);
    rows := floor(WinH / w);
    SetLength(grid, cols * rows);
    active := TVectorList.Create;
    ordered := TVectorList.Create;
    for i := 0 to (cols * rows - 1) do
      grid[i] := TPoint3DF.Create(0, 0, -1); // Using z := -1 as null...

    // STEP 1
    x := WinW / 2;
    y := WinH / 2;
    i := floor(x / w);
    j := floor(y / w);
    aPos := TPoint3DF.Create(x, y);
    grid[i + j * cols] := aPos;
    active.add(aPos);
    //println(active);
    //frameRate(1);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    SetLength(grid, 0);
    FreeAndNil(active);
    FreeAndNil(ordered);
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    aPos, sample, neighbor : TPoint3DF; // Pos is a Pascal function
    total, randIndex, n, col, row, i, j, index : integer;
    found, ok : Boolean;
    m, d : Float;
  begin
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

    for total := 0 to 24 do
    begin
      if (active.Count > 0) then
      begin
        randIndex := floor(random(active.Count));
        aPos := active[randIndex];
        found := False;
        n := 0;
        while n < k do
        begin
          sample := TPoint3DF.CreateRnd(True);
          m := r + random * r;
          sample.SetMagnitude(m);
          sample.add(aPos);

          col := floor(sample.x / w);
          row := floor(sample.y / w);
          //println(col, row, cols, rows, grid[col + row * cols]);
          if (col > -1) and (row > -1) and (col < cols) and
            (row < rows) and (grid[col + row * cols].z = -1) then
          begin
            ok := True;

            // CHX: Actually must be -2 to 2...
            for i := -1 to 1 do
            begin
              for j := -1 to 1 do
              begin
                index := (col + i) + (row + j) * cols;
                // CHX: Added range check on borders
                if InRange(index, 0, Length(grid) - 1) then
                begin
                  neighbor := grid[index];
                  if neighbor.z <> -1 then
                  begin
                    d := neighbor.Distance(sample);
                    if (d < r) then ok := False;
                  end;
                end
                else
                  ok := False;
              end;
            end;

            if ok then
            begin
              found := True;
              grid[col + row * cols] := sample;
              active.add(sample);
              ordered.add(sample);
              // Should we break?
              // break; // CHX: U_U
              n := k; // CHX: Exit while loop;
            end;
          end;
          Inc(n);
        end;

        if not found then
          active.Delete(randIndex);

      end;
    end;
  end;

  procedure cCTCEng.Draw;
  var
    i : integer;
    color : CUInt8;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    i := 0;
    while i < ordered.Count do
    begin
      color := i mod 160 + 96; // CHX: To see al points
      filledCircleRGBA(SDLWindow.PRenderer, Round(ordered[i].x),
        Round(ordered[i].y), r div 2, color, color, color, 255);
      Inc(i);
    end;

    // CHX: Display active count
    DefFont.RenderDynStr(IntToStr(active.Count), 10,10);
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
