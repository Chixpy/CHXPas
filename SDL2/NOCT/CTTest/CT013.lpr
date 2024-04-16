program CT013;
{< The Coding Train Challenge #013 - Title }

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
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils,
  Math, //SDL have math methods too
  SDL2, sdl2_gfx,
  uCHXStrUtils,
  ucSDL2Engine, ucCHXSDL2Window,
  uProcUtils;

const
  // Renderer scales images to actual size of the window.
  WinW = 800; // Window logical width
  WinH = 800; // Window logical height

  dA = 1.0;
  dB = 0.5;
  feed = 0.055;
  k = 0.062;

type
  // Actually it's a class in original code.
  TCell = record
    a : Double;
    b : Double;
  end;

  TCellsArray = array [0..WinW - 1, 0..WinH - 1] of TCell;

var // Global variables :-(
  Grid, Prev : TCellsArray;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  var
    i, j, n, StartX, StartY : integer;
  begin
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

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    // As TCell is a record, nothing to do here
  end;

  function OnCompute(SDL2W : cCHXSDL2Window; DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    a, b, laplaceA, laplaceB : Double;
    Temp : TCellsArray;
    i, j : integer;
  begin
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

    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    i, j, c : integer;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    for i := 0 to WinW - 1 do
      for j := 0 to WinH - 1 do
      begin
        c := EnsureRange(round((grid[i, j].a - grid[i, j].b) * 255), 0, 255);

        SDL_SetRenderDrawColor(SDL2R, c, c, c, 255);
        SDL_RenderDrawPoint(SDL2R, i, j);

        //pixelRGBA(SDL2R, i, j, c, c, c, 255);
      end;

    Result := True; // False -> Finish program
  end;

  function OnEvent(aEvent : TSDL_Event) : Boolean;
  begin
    Result := True;

    // EVENTS

    case aEvent.type_ of
      //SDL_COMMONEVENT : // (common: TSDL_CommonEvent);
      //SDL_DISPLAYEVENT : // (display: TSDL_DisplayEvent);

      // Handled by SDL2Engine: SDL_WINDOWEVENT : //(window: TSDL_WindowEvent)

      //SDL_KEYUP : // (key: TSDL_KeyboardEvent);
      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          //SDLK_UP : ;
          //SDLK_DOWN : ;
          //SDLK_LEFT : ;
          //SDLK_RIGHT : ;
          //SDLK_SPACE : ;
          SDLK_ESCAPE : Result := False; // Exit
          else
            ;
        end;
      end;
        //SDL_TEXTEDITING : // (edit: TSDL_TextEditingEvent);
        //SDL_TEXTEDITING_EXT : // (exitExt: TSDL_TextEditingExtEvent);
        //SDL_TEXTINPUT : // (text: TSDL_TextInputEvent);

        //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
        //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);

        //SDL_JOYAXISMOTION : // (jaxis: TSDL_JoyAxisEvent);
        //SDL_JOYBALLMOTION : // (jball: TSDL_JoyBallEvent);
        //SDL_JOYHATMOTION : // (jhat: TSDL_JoyHatEvent);
        //SDL_JOYBUTTONDOWN : // (jbutton: TSDL_JoyButtonEvent);
        //SDL_JOYBUTTONUP : // (jbutton: TSDL_JoyButtonEvent);
        //SDL_JOYDEVICEADDED : // (jdevice: TSDL_JoyDeviceEvent);
        //SDL_JOYDEVICEREMOVED : // (jdevice: TSDL_JoyDeviceEvent);
        //SDL_JOYBATTERYUPDATED : // (jbattery: TSDL_JoyBatteryEvent);

        //SDL_CONTROLLERAXISMOTION : // (caxis: TSDL_ControllerAxisEvent);
        //SDL_CONTROLLERBUTTONUP : // (cbutton: TSDL_ControllerButtonEvent);
        //SDL_CONTROLLERBUTTONDOWN : // (cbutton: TSDL_ControllerButtonEvent);
        //SDL_CONTROLLERDEVICEADDED : // (cdevice: TSDL_ControllerDeviceEvent);
        //SDL_CONTROLLERDEVICEREMOVED : // (cdevice: TSDL_ControllerDeviceEvent);
        //SDL_CONTROLLERDEVICEREMAPPED : // (cdevice: TSDL_ControllerDeviceEvent);
        //SDL_CONTROLLERTOUCHPADDOWN : // (ctouchpad: TSDL_ControllerTouchpadEvent);
        //SDL_CONTROLLERTOUCHPADMOTION : // (ctouchpad: TSDL_ControllerTouchpadEvent);
        //SDL_CONTROLLERTOUCHPADUP : // (ctouchpad: TSDL_ControllerTouchpadEvent);
        //SDL_CONTROLLERSENSORUPDATE : // (csensor: TSDL_ControllerSensorEvent);

        //SDL_AUDIODEVICEADDED : // (adevice: TSDL_AudioDeviceEvent);
        //SDL_AUDIODEVICEREMOVED : // (adevice: TSDL_AudioDeviceEvent);

        //SDL_SENSORUPDATED : // (sensor: TSDL_SensorEvent);

        // Handled by SDL2Engine: SDL_QUITEV : Result := False;

        //SDL_USEREVENT : // (user: TSDL_UserEvent);
        //SDL_SYSWMEVENT : // (syswm: TSDL_SysWMEvent);

        //SDL_FINGERDOWN : // (tfinger: TSDL_TouchFingerEvent);
        //SDL_FINGERUP : // (tfinger: TSDL_TouchFingerEvent);
        //SDL_FINGERMOTION : // (tfinger: TSDL_TouchFingerEvent);
        //SDL_MULTIGESTURE : // (mgesture: TSDL_MultiGestureEvent);
        //SDL_DOLLARGESTURE : //(dgesture: TSDL_DollarGestureEvent);
        //SDL_DOLLARRECORD : //(dgesture: TSDL_DollarGestureEvent);

        //SDL_DROPFILE : // (drop: TSDL_DropEvent);
      else
        ;
    end;

  end;

var
  SDL2Engine : cSDL2Engine;
  BaseFolder : string;

  {$R *.res}

begin
  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  try
    SDL2Engine := cSDL2Engine.Create(nil, ApplicationName, WinW, WinH, True);
    SDL2Engine.SDL2Setup := @OnSetup;
    SDL2Engine.SDL2Comp := @OnCompute;
    SDL2Engine.SDL2Draw := @OnDraw;
    SDL2Engine.SDL2Event := @OnEvent;
    SDL2Engine.SDL2Finish := @OnFinish;

    SDL2Engine.Run;
  finally
    SDL2Engine.Free;
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
