program CT023;
{< The Coding Train Challenge #023 - 2D Supershapes }


// Coding Train
// http://thecodingtrain.com
// http://patreon.com/codingtrain

// Code for: https://youtu.be/ksRoh-10lak
// Processing port by Max (https://github.com/TheLastDestroyer)

// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  Classes,
  SysUtils,
  CTypes,
  StrUtils,
  FileUtil,
  LazFileUtils,
  Math, //SDL have math methods too
  SDL2,
  sdl2_gfx,
  uCHXStrUtils,
  ucCHXSDL2Window,
  ucSDL2Engine,
  uProcUtils;

const
  // Renderer scales images to actual size of the window.
  WinW = 800; // Window logical width
  WinH = 800; // Window logical height

  // all floats to negate integer devision errors
  n1 =  0.3;
  n2 =  0.3;
  n3 =  0.3;

  a = 1;
  b = 1;

  radius = 300;
  total = 200;

  increment = pi * 2 / total;

var // Global variables :-(
  m, osc : Float;

  PointList : array of TSDL_Point;

  // Any auxiliar procedure/function will be here

  function super_shape(theta : Float) : Float;
  var
    part1, part2, part3 : Float;
  begin
    part1 := (1 / a) * cos(theta * m / 4);
    part1 := abs(part1);
    part1 := Power(part1, n2);

    part2 := (1 / b) * sin(theta * m / 4);
    part2 := abs(part2);
    part2 := Power(part2, n3);

    part3 := Power(part1 + part2, 1 / n1);

    if part3 = 0 then
      Result := 0
    else
      Result := 1 / part3;
  end;

  function OnSetup : Boolean;
  begin
    m := 0;
    osc := 0;

    SetLength(PointList, total + 1);

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    SetLength(PointList, 0);
  end;

  function OnCompute(Window : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    r, angle : Float;
    Count : integer;
  begin
    { If we want to pause when minimized or lost focus.}
    // if Window.Minimized then
    // begin
    //   Result := True;
    //   Exit;
    // end;

    m := map(Sin(osc), -1.0, 1.0, 0.0, 10.0);
    osc += 0.02;

    angle := 0;
    Count := 0;
    while angle < (pi * 2) do
    begin
      r := super_shape(angle);

      //translate(width/2, height/2);
      PointList[Count].x := WinW div 2 + Round(radius * r * cos(angle));
      PointList[Count].y := WinH div 2 + Round(radius * r * sin(angle));

      angle += increment;
      Inc(Count);
    end;

    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 51, 51, 51, 255);
    SDL_RenderClear(SDL2R);

    SDL_SetRenderDrawColor(SDL2R, 255, 255, 255, 255);
    SDL_RenderDrawLines(SDL2R, @PointList[0], total);
    // CHX: Closing the shape
    SDL_RenderDrawLine(SDL2R, PointList[total - 1].x, PointList[total - 1].y,
      PointList[0].x, PointList[0].y);

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
