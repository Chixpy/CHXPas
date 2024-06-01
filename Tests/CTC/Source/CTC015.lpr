program CTC015;
{< The Coding Train Challenge #015 - OO Fractal Trees }

//* makes a FractalTree using Array(Lists)
//* adds one Level per MouseClick
//* at Level 6 Leaves will fall down
//* @author Lukas Klassen
//* translated version of CC_015_FractalTreeArray by Daniel Shiffmann

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for:                            <- Change this
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, fgl,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPoint3DF, uCHXStrUtils,
  ucCHXSDL2Engine, uCHXSDL2Utils, uProcUtils,
  ucCTCBranch;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  NIter = 6; // Number of max iterations

type
  TTree = specialize TFPGObjectList<cCTCBranch>;
  TLeaves = specialize TFPGList<TPoint3DF>;

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
    Tree : TTree;
    Leaves : TLeaves;
    Count : integer;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    Root : cCTCBranch;
  begin
    Tree := TTree.Create(True);
    Leaves := TLeaves.Create;

    Root := cCTCBranch.Create(Point3DF(WinW / 2, WinH, 0),
      Point3DF(WinW / 2, WinH - 100, 0));

    Tree.Add(Root);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    Tree.Free;
    Leaves.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    l : TPoint3DF;
    i : integer;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    { A little dream, l is a copy and it can't be assigned to. }
    //for l in Leaves do
    //  //let the Leave fall
    //  l.Y := l.Y + Random * 4;

    //forEach Leave: draw it
    i := Leaves.Count - 1;
    while i >= 0 do
    begin
      l := Leaves[i]; // CHX: Returns a copy
      //let the Leave fall
      // CHX: I don't know why l.Y := l.Y + Random * 4 don't work.
      l.SetY(l.Y + Random * 4);
      Leaves[i] := l;


      // CHX: removing leaves offscreen
      if Leaves[i].Y > WinH then
        Leaves.Delete(i);
      Dec(i);
    end;
  end;

  procedure cCTCEng.Draw;
  var
    b : cCTCBranch;
    l : TPoint3DF;
  begin
    // Background
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 51, 51, 51, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    //forEach Branch of the Tree: Draw it
    for b in Tree do
      SDL_RenderDrawLineF(SDLWindow.PRenderer, b.beginB.X, b.beginB.Y,
        b.endB.X, b.endB.Y);

    for l in Leaves do
      filledCircleRGBA(SDLWindow.PRenderer, round(l.x), round(l.y),
        4, 255, 0, 100, 100);
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  var
    i : integer;
    Current : cCTCBranch;
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
      //  case aEvent.key.keysym.sym of
      //      //SDLK_UP : ;
      //      //SDLK_DOWN : ;
      //      //SDLK_LEFT : ;
      //      //SDLK_RIGHT : ;
      //      //SDLK_SPACE : ;
      //    else
      //      ;
      //  end;
      //end;

      //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        // CHX: Stop iterating, and make a constant with iteration.
        if Count <= NIter then
        begin
          i := Tree.Count - 1;
          while i >= 0 do
          begin
            Current := Tree[i];

            if not Current.finished then
            begin
              Tree.Add(Current.branchA);
              Tree.Add(Current.branchB);
            end;

            Current.finished := True;

            Dec(i);
          end;

        end;

        //new Level added
        Inc(Count);

        //on the 6. Level: spawn the Leaves
        if Count >= NIter then
          for Current in Tree do
            if not Current.finished then
              Leaves.add(Current.endB);
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
