program CTC005;
{< The Coding Train Challenge #005 - Space Invaders }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/biN3v3ef-Y0
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, fgl,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils,
  ucCTCFlower, ucCTCShip, ucCTCDrop;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

  NFlowers = 5;
type
  TDropList = specialize TFPGObjectList<cCTCDrop>;

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
    Ship : cCTCShip;
    Flowers : array of cCTCFlower;
    // CHX: Dinamic arrays are bad idea if lenght is changed many times,
    //   specialized Object Lists are better
    // drops : array of cCTCDrop;
    Drops : TDropList;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : integer;
  begin
    Drops := TDropList.Create(True);
    Ship := cCTCShip.Create(WinW div 2, WinH - 20);
    // drop = new Drop(width/2, height/2);
    SetLength(Flowers, NFlowers);
    for i := 0 to High(Flowers) do
      Flowers[i] := cCTCFlower.Create(WinW div (NFlowers + 4) * (i + 2), 60);
  end;

  procedure cCTCEng.Finish;
  var
    i : integer;
  begin
    Ship.Free;
    for i := 0 to High(Flowers) do
      Flowers[i].Free;
    Drops.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    i, j : integer;
    Edge : Boolean;
  begin
    { CHX: If we want to pause when minimized or focus lost. }
    // if SDLWindow.Minimized then Exit;

    // Ship
    Ship.Move;

    // Drops
    for i := 0 to Drops.Count - 1 do
    begin
      Drops[i].move;
      for j := 0 to High(Flowers) do
      begin
        if Drops[i].hits(Flowers[j]) then
        begin
          Flowers[j].grow;
          Drops[i].evaporate;
        end;
      end;
    end;

    // CHX: Removing items from a list by index is better backwards...
    for i := Drops.Count - 1 downto 0 do
    begin
      // CHX: Added Drops that go out of window
      if (Drops[i].y < 0) or (Drops[i].y > WinH) or Drops[i].toDelete then
      begin
        Drops.Delete(i);
      end;
    end;

    // Flowers
    Edge := False;
    i := 0;
    // CHX: Added Edge collision check
    while (i <= High(Flowers)) and (not Edge) do
    begin
      Flowers[i].move;
      if (Flowers[i].x > WinW) or (Flowers[i].x < 0) then
        edge := True;
      Inc(i);
    end;

    if Edge then
      for i := 0 to High(Flowers) do
      begin
        Flowers[i].shiftDown;
      end;
  end;

  procedure cCTCEng.Draw;
  var
    i : integer;
  begin
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 51, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    // Ship
    boxRGBA(SDLWindow.PRenderer, Ship.x - 10, Ship.y - 30, Ship.x + 10,
      Ship.y + 30, 255, 0, 0, 255);

    // Drops
    for i := 0 to Drops.Count - 1 do
      filledCircleRGBA(SDLWindow.PRenderer, Drops[i].x, Drops[i].y, Drops[i].r,
        150, 0, 255, 255);

    // Flowers
    for i := 0 to High(Flowers) do
      filledCircleRGBA(SDLWindow.PRenderer, Flowers[i].x, Flowers[i].y,
        Flowers[i].r, 255, 0, 200, 150);
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
      SDL_KEYUP : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          SDLK_SPACE : ;
          else
            Ship.setDir(0);
        end;
      end;

      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          //SDLK_UP : ;
          //SDLK_DOWN : ;
          SDLK_LEFT : Ship.setDir(-1);
          SDLK_RIGHT : Ship.setDir(1);
          SDLK_SPACE : Drops.Add(cCTCDrop.Create(Ship.x, WinH));
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
