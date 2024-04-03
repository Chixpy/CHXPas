program CT003;
{< Base program for NOTC and CT

  Copyright (C) 2024 Chixpy
}
{$mode ObjFPC}{$H+}
uses
  Classes,
  SysUtils,
  ctypes,
  StrUtils,
  FileUtil,
  LazFileUtils,
  Math,
  sdl2,
  sdl2_gfx,
  uCHXStrUtils,
  ucSDL2Engine,
  uProcUtils,
  ucSnake;

const
  WinW = 800; // Window width on creation
  WinH = 600; // Window height on creation

var
  s : cSnake;
  scl : integer;
  Food : TPoint;

  procedure pickLocation;
  var
    cols, rows : integer;
  begin
    cols := floor(WinW div scl);
    rows := floor(WinH div scl);

    Food := Point(random(cols) * scl, random(rows) * scl);
  end;

  function OnSetup : Boolean;
  begin
    scl := 20; // Size of the grid

    s := cSnake.Create;
    pickLocation;

    Result := True;
  end;

  function OnCompute(DeltaTime, FrameTime : CUInt32) : Boolean;
  begin
    if s.eat(food) then
      pickLocation;
    s.death;
    s.update;

    Result := True;
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    i : integer;
  begin
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 0);
    SDL_RenderClear(SDL2R);

    // Food
    filledCircleRGBA(SDL2R, food.x + scl shr 1, food.y + scl shr
      1, scl shr 1, 255, 0, 0, 255);

    // Snake
    i := length(s.tail);
    while i > 0 do
    begin
      Dec(i);
      boxRGBA(SDL2R, s.tail[i].x, s.tail[i].y, s.tail[i].x +
        scl, s.tail[i].y + scl, 255, 255, 0, 255);
    end;
    boxRGBA(SDL2R, s.x, s.y, s.x + scl, s.y + scl, 255, 255, 0, 255);

    SDL_Delay(100);
    Result := True;
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
          SDLK_UP : s.dir(0, -1);
          SDLK_DOWN : s.dir(0, 1);
          SDLK_LEFT : s.dir(-1, 0);
          SDLK_RIGHT : s.dir(1, 0);
          //SDLK_SPACE : ;
          SDLK_ESCAPE : Result := False;
          else
            ;
        end;
      end;
      //SDL_TEXTEDITING : // (edit: TSDL_TextEditingEvent);
      //SDL_TEXTEDITING_EXT : // (exitExt: TSDL_TextEditingExtEvent);
      //SDL_TEXTINPUT : // (text: TSDL_TextInputEvent);

      //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        s.eat(Point(s.x, s.y));
      end;
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

  procedure OnFinish;
  begin
    s.Free;
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

    SDL2Engine := cSDL2Engine.Create(nil, ApplicationName, WinW, WinH);
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
