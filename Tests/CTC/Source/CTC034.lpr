program CTC034;
{< The Coding Train Challenge #034 - Diffusion-Limited Aggregation }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/Cl_Gjj80gPE
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, fgl,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  ucCTCWalker;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 600; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  //r = 4;
  maxWalkers = 50;
  iterations = 1000;

  shrink = 0.995;

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

    tree : cCTCWalkerList;
    walkers : cCTCWalkerList;
    hue : integer;
    radius : float;

  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin
    DefFont.ChangeFontStyle(SDLColor(255, 255, 0), -1, -1, -1, -1);

    tree := cCTCWalkerList.Create(True);
    walkers := cCTCWalkerList.Create(False);
    radius := 12;
    hue := 0;

    //colorMode(HSB, 360, 100, 100);

    // for x := 1 to width step r * 2 do
    //   tree.add(cCTCWalker.CreateStuck(WinW, WinH));

    tree.add(cCTCWalker.CreateStuck(WinW, WinH, WinW div 2,
      WinH div 2, radius));

    radius *= shrink;

    for i := 1 to maxWalkers do
    begin
      walkers.add(cCTCWalker.Create(WinW, WinH, radius));
      radius *= shrink;
    end;
  end;

  procedure cCTCEng.Finish;
  var
    i : cCTCWalker;
  begin
    { CHX: Free any created objects. }
    FreeAndNil(tree);

    // CHX: Ugh dirty patch to free not stucked walkers ...
    for i in walkers do
      i.Free;
    FreeAndNil(walkers);
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    n, i : integer;
    walker : cCTCWalker;
  begin
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

    for n := 1 to iterations do
    begin
      i := walkers.Count - 1;
      while i >= 0 do
      begin
        walker := walkers[i];
        walker.walk;
        if walker.checkStuck(tree) then
        begin
          walker.hu := hue mod 160 + 96;
          hue += 2;
          tree.add(walker);
          walkers.Delete(i);
        end;
        Dec(i);
      end;
    end;

    //r := walkers[walkers.Count - 1].r;
    while (walkers.Count < maxWalkers) and (radius > 1) do
    begin
      radius *= shrink;
      walkers.add(cCTCWalker.Create(WinW, WinH, radius));
    end;
  end;

  procedure cCTCEng.Draw;
    procedure WalkerShow(aWalker : cCTCWalker);
    begin
      //if not aWalker.stuck then
      //  filledCircleRGBA(SDLWindow.PRenderer, Round(aWalker.aPos.x),
      //    Round(aWalker.aPos.y), Round(aWalker.r), 255, 255, 255, 255)
      //else
      filledCircleRGBA(SDLWindow.PRenderer, Round(aWalker.aPos.x),
        Round(aWalker.aPos.y), Round(aWalker.r), aWalker.hu,
        aWalker.hu, aWalker.hu, 255);
    end;
  var
    i : cCTCWalker;
    y : cint;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    for i in tree do
      WalkerShow(i);

    for i in walkers do
      WalkerShow(i);

    //y := DefFont.YPosAtLine(0, 10);
    //DefFont.RenderDynStr(walkers.Count.ToString, 10, y);
    //y := DefFont.YPosAtLine(1, 10);
    //DefFont.RenderDynStr(tree.Count.ToString, 10, y);
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
    CTCEng.Config.RendererUseHW := False;
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
