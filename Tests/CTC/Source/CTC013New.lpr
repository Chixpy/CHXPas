program CTC013New;
{< The Coding Train Challenge #013 - Reaction Diffusion Algorithm }

// CHX: Testing faster direct pixel access of a texture instead renderer.
// Time per frame in my computer in Release build mode at 800x800:
//   - Old Soft:  ~55ms
//   - Old Hard: ~215ms
//   - New Soft:  ~35ms (30ms if SDL_MapRGBA isn't used.)
//   - New Hard:  ~35ms (30ms if SDL_MapRGBA isn't used.)

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/BV9ny785UNc

// Written entirely based on
// http://www.karlsims.com/rd.html

// Also, for reference
// http://hg.postspectacular.com/toxiclibs/src/44d9932dbc9f9c69a170643e2d459f449562b750/src.sim/toxi/sim/grayscott/GrayScott.java?at=default
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

  dA = 1.0;
  dB = 0.5;
  feed = 0.055;
  k = 0.062;

type
  // CHX: Actually it's a class in original code.
  TCell = record
    a : Double;
    b : Double;
  end;

  TCellsArray = array [0..WinW - 1, 0..WinH - 1] of TCell;

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
    aTex : PSDL_Texture;
    aPxFmt : PSDL_PixelFormat;

    Grid : TCellsArray;
    Prev : TCellsArray;

    procedure PutPixelTexture(Base : PCUInt32; Pitch : cint; x, y : word;
      r, g, b, a : byte);
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    aFmt : CUInt32;
    i, j, n, StartX, StartY : integer;
  begin
    // CHX: Allocating Window pixel format
    aFmt := SDL_GetWindowPixelFormat(SDLWindow.PWindow);
    aPxFmt := SDL_AllocFormat(aFmt);

    // CHX: Creating a SDLTexture to edit its pixels
    aTex := SDL_CreateTexture(SDLWindow.PRenderer,
      aFmt, SDL_TEXTUREACCESS_STREAMING, WinW, WinH);


    for i := 1 to WinW - 2 do
      for j := 1 to WinH - 2 do
      begin
        Grid[i, j].a := 1;
        Grid[i, j].b := 0;
        Prev[i, j].a := 1;
        Prev[i, j].b := 0;
      end;

    for n := 0 to 9 do
    begin
      StartX := RandomRange(20, WinW - 19);
      StartY := RandomRange(20, WinH - 19);

      for i := StartX to StartX + 10 do
        for j := StartY to StartY + 10 do
        begin
          Grid[i, j].a := 1;
          Grid[i, j].b := 1;
          Prev[i, j].a := 1;
          Prev[i, j].b := 1;
        end;
    end;
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    SDL_DestroyTexture(aTex);
    SDL_FreeFormat(aPxFmt);
    // As TCell is a record, nothing to do here
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    pitch : cint;
    PPBase : PCUInt32;
    a, b, laplaceA, laplaceB : Double;
    Temp : TCellsArray;
    i, j, c : integer;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    // CHX: As we are editing a SDLTexture and don't need a SDL_Renderer, we
    //   can do in OnCompute.
    SDL_LockTexture(aTex, nil, @PPBase, @pitch);

    // Update()

    // Don't compute borders
    for i := 1 to WinW - 2 do
      for j := 1 to WinH - 2 do
      begin
        a := prev[i][j].a;
        b := prev[i][j].b;

        laplaceA := 0.0;
        laplaceA += a * -1;
        laplaceA += prev[i + 1][j].a * 0.2;
        laplaceA += prev[i - 1][j].a * 0.2;
        laplaceA += prev[i][j + 1].a * 0.2;
        laplaceA += prev[i][j - 1].a * 0.2;
        laplaceA += prev[i - 1][j - 1].a * 0.05;
        laplaceA += prev[i + 1][j - 1].a * 0.05;
        laplaceA += prev[i - 1][j + 1].a * 0.05;
        laplaceA += prev[i + 1][j + 1].a * 0.05;

        laplaceB := 0.0;
        laplaceB += b * -1;
        laplaceB += prev[i + 1][j].b * 0.2;
        laplaceB += prev[i - 1][j].b * 0.2;
        laplaceB += prev[i][j + 1].b * 0.2;
        laplaceB += prev[i][j - 1].b * 0.2;
        laplaceB += prev[i - 1][j - 1].b * 0.05;
        laplaceB += prev[i + 1][j - 1].b * 0.05;
        laplaceB += prev[i - 1][j + 1].b * 0.05;
        laplaceB += prev[i + 1][j + 1].b * 0.05;

        grid[i][j].a := a + (dA * laplaceA - a * b * b + feed * (1 - a)) * 1;
        grid[i][j].b := b + (dB * laplaceB + a * b * b - (k + feed) * b) * 1;

        grid[i][j].a := EnsureRange(grid[i][j].a, 0, 1);
        grid[i][j].b := EnsureRange(grid[i][j].b, 0, 1);
      end;

    // Swap()
    Temp := Prev;
    prev := grid;
    grid := Temp;


    for i := 0 to WinW - 1 do
      for j := 0 to WinH - 1 do
      begin
        c := EnsureRange(round((grid[i, j].a - grid[i, j].b) * 255), 0, 255);

        PutPixelTexture(PPBase, pitch, i, j, c, c, c, 255);
      end;

    SDL_UnlockTexture(aTex);
  end;

  procedure cCTCEng.Draw;
  begin
    // Background and frame clear.
    //SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    //SDL_RenderClear(SDLWindow.PRenderer);

    SDL_RenderCopy(SDLWindow.PRenderer, aTex, nil, nil);
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

  procedure cCTCEng.PutPixelTexture(Base : PCUInt32; Pitch : cint; x, y : word;
    r, g, b, a : byte);
  var
    PPoint : PCUInt32;
  begin
    PPoint := Base + y * (Pitch div 4) + x;
    // CHX: Doesn't apply transparency, only sets it. PPoint is write-only and
    //   it doesn't have previous color value.

    // This is a little faster, less than 5%, but only works in RGBA8888 in
    //   Windows and Intel (component order and endianess);
    //PPoint^ := (a shl 24) or (r shl 16) or (g shl 8) or b;

    // SDL_MapRGBA is the correct way and we need texture pixel format.
    PPoint^ := SDL_MapRGBA(aPxFmt, r, g, b, a);
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
