program CTC001;
{< The Coding Train Challenge #001 — Starfield in Processing }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/17WoOqgXsRM
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}
uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  uCHXSDL2Utils, ucCHXSDL2Engine, uProcUtils,
  ucCTC3DStar;

const
  // Renderer scales images to actual size of the window.
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
    Stars : array[0..25000] of cCTC3DStar;
    Speed : LongInt;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin
    // CHX: RandomRange don't get upper limit
    for i := Low(Stars) to High(Stars) do
      Stars[i] := cCTC3DStar.Create(RandomRange(-WinW, WinW),
        RandomRange(-WinH, WinW), Random(WinW));

    Speed := 0;
  end;

  procedure cCTCEng.Finish;
  var
    i : integer;
  begin
    for i := Low(Stars) to High(Stars) do
      Stars[i].Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    i : integer;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    for i := Low(Stars) to High(Stars) do
    begin
      Stars[i].Update(Speed);

      if Stars[i].Z < 1 then
      begin
        Stars[i].X := RandomRange(-WinW, WinW);
        Stars[i].Y := RandomRange(-WinH, WinW);
        Stars[i].Z := Random(WinW);
        Stars[i].PZ := Stars[i].Z;
      end;
    end;
  end;

  procedure cCTCEng.Draw;
  var
    Px, Py, Sx, Sy, i : integer;
    SDL2R : PSDL_Renderer;
  begin
    SDL2R := SDLWindow.PRenderer;
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 0);
    SDL_RenderClear(SDL2R);

    for i := Low(Stars) to High(Stars) do
    begin
      // Stars[i].Draw here
      if (Stars[i].PZ <> 0) and (Stars[i].Z <> 0) then
      begin
        Sx := WinW shr 1 + Round(map(Stars[i].X / Stars[i].Z, 0, 1, 0, WinW));
        Sy := WinH shr 1 + Round(map(Stars[i].Y / Stars[i].Z, 0, 1, 0, WinH));
        Px := WinW shr 1 + Round(map(Stars[i].X / Stars[i].PZ, 0, 1, 0, WinW));
        Py := WinH shr 1 + Round(map(Stars[i].Y / Stars[i].PZ, 0, 1, 0, WinH));

        //LongInt range (and can use floats if needed)
        //SDL_SetRenderDrawColor(SDL2R, 255,255,255,255);
        //SDL_RenderDrawLine(SDL2R, Sx, Sy, Px, Py);

        // With sdl2_gfx: SmallInt (32765) limit
        Px := EnsureRange(Px, -32765, 32765);
        Py := EnsureRange(Py, -32765, 32765);
        Sx := EnsureRange(Sx, -32765, 32765);
        Sy := EnsureRange(Sy, -32765, 32765);
        pixelRGBA(SDL2R, Sx, Py, 255, 255, 0, 255);
        lineRGBA(SDL2R, Sx, Sy, Px, Py, 255, 255, 255, 255);
        pixelRGBA(SDL2R, Px, Py, 255, 255, 0, 255);
      end;
    end;
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    inherited HandleEvent(aEvent, Handled, ExitProg);
    if ExitProg then Exit; { CHX: Inherited Draw don't change ExitProg. }

    { CHX: Some common events for fast reference, CTRL+MAYS+U removes comments
        while selecting the block.
      You can see full list in sdlevents.inc
      Window and general quit events are handled automatically in parent.
      Escape key is mapped to exit the program too.
    }

    case aEvent.type_ of
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

      SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      begin
        Speed := aEvent.motion.x shr 4;
        Handled := True;
      end;
        //  //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
        //  //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
        //  //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);

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
    CTCEng := cCTCEng.Create(ApplicationName, WinW, WinH, True, False);
    CTCEng.Config.DefFontSize := WinH div 25;
    // Actually are less than 25 lines
    CTCEng.Config.DefFontColor := SDLColor(255,255,255,255);
    CTCEng.Config.DefFontFile := 'FreeMonoBold.ttf'; 
    CTCEng.ShowFrameRate := True;
    CTCEng.Init;
    CTCEng.Run;
  finally
    CTCEng.Free;
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
