program CTC028;
{< The Coding Train Challenge #028 - Metaballs }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/ccYLb7cLB1I
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPoint3DF, uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  ucCTCBlob;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

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
    blobs : array[0..7] of cCTCBlob;
    aTex : PSDL_Texture;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin

    // CHX: Creating a SDLTexture to edit its pixels
    aTex := SDL_CreateTexture(SDLWindow.PRenderer, PWinPxFmt^.format,
      SDL_TEXTUREACCESS_STREAMING, WinW, WinH);

    for i := Low(blobs) to High(blobs) do
      blobs[i] := cCTCBlob.Create(random * WinW, random * WinH);
  end;

  procedure cCTCEng.Finish;
  var
    i : integer;
  begin
    // CHX:Free any created objects
    for i := Low(blobs) to High(blobs) do
      blobs[i].Free;

    SDL_DestroyTexture(aTex);
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    pitch : cint;
    PPBase : PCUInt32;

    b : cCTCBlob;
    y, x : integer;
    sum, d : Float;
    Col : byte;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    // CHX: As we are editing a SDLTexture and don't need a SDL_Renderer, we
    //   can do in OnCompute.
    SDL_LockTexture(aTex, nil, @PPBase, @pitch);

    y := 0;
    while y < WinH do
    begin
      x := 0;
      while x < WinW do
      begin
        sum := 0;
        for b in blobs do
        begin
          d := b.pos.Distance(Point3DF(x, y));
          if d <> 0 then // Zero division
            sum += 200 * b.r / d;
        end;

        // CHX: Weigh down color by numbers of blobs
        //   so, we don't need to change constant in sum
        col := EnsureRange(Round(sum) div length(blobs), 0, 255);

        //pixels[index] = color(sum, 255, 255); // no HSB
        PutPixel(PPBase, pitch, x, y, col, col, col, 255);

        Inc(x);
      end;

      Inc(y);
    end;

    SDL_UnlockTexture(aTex);
  end;

  procedure cCTCEng.Draw;
  var
    b : cCTCBlob;
    r : integer;
  begin
    // Background and frame clear.
    // SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    // SDL_RenderClear(SDLWindow.PRenderer);

    SDL_RenderCopy(SDLWindow.PRenderer, aTex, nil, nil);

    for b in blobs do
    begin
      // CHX: Reducing blob radius based in previous color weigh down.
      r := Round(b.r) div length(blobs);
      circleRGBA(SDLWindow.PRenderer, Round(b.pos.X), Round(b.pos.Y), r,
        0, 0, 0, 255);

      // CHX: Updating after draw, so circle is centered on previous
      //   calculated pixels
      b.update;

      if not InRange(b.pos.x, 0, WinW) then
        b.vel.X := -b.vel.X;
      if not InRange(b.pos.y, 0, WinH) then
        b.vel.Y := -b.vel.Y;
    end;
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
