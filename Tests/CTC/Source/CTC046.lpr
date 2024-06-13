program CTC046;
{< The Coding Train Challenge #046 - Asteroids }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/hacZU523FyM
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image, sdl2_mixer,
  uCHXStrUtils, uCHXPoint3DF,
  ucCHXSDL2Engine, ucCHXSDL2FontTTF, uCHXSDL2Utils, uProcUtils,
  uaCHXSDL2Comp, ucCHXSDL2TextEdit, ucCHXSDL2Button,
  ucShip, ucAsteroid, ucLaser;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640; { CHX: Window logical width. }
  WinH = 480; { CHX: Window logical height. }

type

  { cCTCEng }

  cCTCEng = class(cCHXSDL2Engine)
  public
    procedure Setup; override;
    procedure Finish; override;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw; override;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override;

  public
    { CHX: Processing global variables and auxiliar functions. }
    ship : cShip;
    asteroids : cAsteroidList;
    lasers : cLaserList;

    // Event text
    EvnTxt : string;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : Integer;
  begin
    ship := cShip.Create(WinW, WinH);
    asteroids := cAsteroidList.Create(True);
    lasers := cLaserList.Create(True);

    for i := 1 to 5 do
      asteroids.Add(cAsteroid.Create(WinW, WinH));
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    lasers.Free;
    ship.Free;
    asteroids.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    Asteroid : cAsteroid;
    Laser : cLaser;
    Astlist : cAsteroidList;
    i, j : Integer;
  begin
    if ExitProg then Exit;
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;
    EvnTxt := '';

    for Asteroid in asteroids do
    begin
      // CHX: Personally, I think that hit checks must be after all entities
      //   (asteroids, lasers and ships) are updated (and before draw them).
      //   This is one of reasons to split Compute and Draw in cCHXSDL2Engine.
      //   But I'll keep CTC algorithm, except drawing stuff.
      if ship.hits(Asteroid) then
        EvnTxt := 'ooops!';
      Asteroid.update();
      Asteroid.edges();
    end;

    i := lasers.Count - 1;
    while i >= 0 do
    begin
      Laser := lasers[i];
      Laser.update;
      if Laser.offscreen then
      begin
        Lasers.Delete(i);
      end
      else
      begin
        j := asteroids.Count - 1;
        while j >= 0 do
        begin
          Asteroid := asteroids[j];
          if Laser.hits(Asteroid) then
          begin
            // CHX: It's weird this test here and not in cAsteroid.breakup
            if Asteroid.r > 10 then
            begin
              Astlist := Asteroid.breakup;
              asteroids.AddList(Astlist);
              FreeAndNil(Astlist);
            end;

            asteroids.Delete(j);
            lasers.Delete(i);
            j := -1; // 'while j >= 0' break;
          end;

          Dec(j);
        end;
      end;

      Dec(i);
    end;

    ship.turn;
    ship.update;
    ship.edges;
  end;

  procedure cCTCEng.Draw;
  var
    Asteroid : cAsteroid;
    Laser : cLaser;
    P1, P2, P3 : TPoint3DF;
    PointList : array of TSDL_FPoint;
    i : Integer;
    angle, r1, Y, X : Double;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    for Asteroid in asteroids do
    begin
      // cAsteroid.render

      //circleColor(SDL2R, Round(Asteroid.pos.X), Round(Asteroid.pos.Y),
      //  Round(Asteroid.r), $FFFFFFFF);

      SetLength(PointList, Asteroid.total + 1);
      SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
      i := 0;
      while i < Asteroid.total do
      begin
        angle := map(i, 0, Asteroid.total, 0, pi * 2);
        r1 := Asteroid.r + Asteroid.offset[i];
        SinCos(angle, Y, X);
        X *= r1;
        Y *= r1;
        PointList[i] := SDLFPoint(X + Asteroid.pos.X, Y + Asteroid.pos.Y);
        Inc(i);
      end;
      // Closing line
      PointList[Length(PointList) - 1] := PointList[0];
      SDL_RenderDrawLinesF(SDLWindow.PRenderer, @PointList[0],
        Length(PointList));
    end;

    for Laser in lasers do
      // cLaser.render
      // strokeWeight(4); so draw a circle
      filledCircleColor(SDL2R, Round(Laser.pos.X), Round(Laser.pos.Y),
        2, $FFFFFFFF);

    // cShip.render
    // Well, it uses pushMatrix to translate and rotate the ship...
    P1.Create(ship.r, 0).RotateXY(ship.heading);
    P2.Create(ship.r / 2, 0).RotateXY(ship.heading + 2 * pi / 3);
    P3.Create(ship.r / 2, 0).RotateXY(ship.heading - 2 * pi / 3);

    filledTrigonColor(SDL2R,
      Round(ship.pos.X + P1.X), Round(ship.pos.Y + P1.Y),
      Round(ship.pos.X + P2.X), Round(ship.pos.Y + P2.Y),
      Round(ship.pos.X + P3.X), Round(ship.pos.Y + P3.Y), $FFFFFFFF);

    DefFont.RenderDynStr(asteroids.Count.ToString + ', ' +
      lasers.Count.ToString, 10, 10, WinW - 10);
    DefFont.RenderDynStr(EvnTxt, 10, WinH - DefFont.LineHeight, WinW - 10);
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    inherited HandleEvent(aEvent, Handled, ExitProg);
    { CHX: Inherited HandleEvent can change ExitProg and Handled. }
    if ExitProg or Handled then Exit;

    { CHX: Some common events for fast reference, CTRL+MAYS+U removes comments
        while selecting the block.
      You can see full list in sdlevents.inc
      Window and general quit events are handled automatically in parent.
      Escape key is mapped to exit the program too.
      When editing text, all keys are handled too until Return is pressed (or
        other event disables it).
    }

    case aEvent.type_ of
      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          SDLK_UP : ship.boosting(True);
          //SDLK_DOWN : ;
          SDLK_LEFT : ship.setRotation(-0.1);
          SDLK_RIGHT : ship.setRotation(0.1);
          SDLK_SPACE : lasers.Add(cLaser.Create(WinW, WinH,
              ship.pos, ship.heading));
          //SDLK_RETURN : ;
        end;
      end;

      SDL_KEYUP : // (key: TSDL_KeyboardEvent);
      begin
        ship.setRotation(0);
        ship.boosting(False);
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
  // Changing base folder to parents .exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  try
    //CTCEng := cCTCEng.Create(ApplicationName, 'SDL2.ini');
    CTCEng := cCTCEng.Create(ApplicationName, WinW, WinH, True, False);
    CTCEng.Config.DefFontSize := WinH div 30;
    // Actually, they are less than 30 lines because of LineHeight
    CTCEng.Config.DefFontColor := SDLColor(255, 255, 255, 255);
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
