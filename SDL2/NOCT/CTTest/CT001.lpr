program CT001;
{< The Coding Train Challenge #001 — Starfield in Processing }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/17WoOqgXsRM
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  SysUtils,
  CTypes,
  StrUtils,
  FileUtil,
  LazFileUtils,
  Math,
  SDL2,
  sdl2_gfx,
  uCHXStrUtils,
  ucSDL2Engine,
  ucCHXSDL2Window,
  uProcUtils,
  ucCT3DStar;

const
  WinW = 800; // Window width on creation
  WinH = 600; // Window height on creation

var
  Stars : array[0..25000] of cCT3DStar;
  Speed : LongInt;

  function OnSetup : Boolean;
  var
    i : integer;
  begin
    // CHX: RandomRange don't get upper limit
    for i := Low(Stars) to High(Stars) do
      Stars[i] := cCT3DStar.Create(RandomRange(-WinW, WinW),
        RandomRange(-WinH, WinW), Random(WinW));

    Speed := 0;

    Result := True;
  end;

  function OnCompute(SDL2W : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    i : integer;
  begin
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

    Result := True;
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    Px, Py, Sx, Sy, i : integer;
  begin

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
          //SDLK_UP : ;
          //SDLK_DOWN : ;
          //SDLK_LEFT : ;
          //SDLK_RIGHT : ;
          //SDLK_SPACE : ;
          SDLK_ESCAPE : Result := False;
          else
            ;
        end;
      end;
      //SDL_TEXTEDITING : // (edit: TSDL_TextEditingEvent);
      //SDL_TEXTEDITING_EXT : // (exitExt: TSDL_TextEditingExtEvent);
      //SDL_TEXTINPUT : // (text: TSDL_TextInputEvent);

      SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      begin
        Speed := aEvent.motion.x shr 4;
      end;
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

  procedure OnFinish;
  var
    i : integer;
  begin
    for i := Low(Stars) to High(Stars) do
      Stars[i].Free;
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

    SDL2Engine := cSDL2Engine.Create(nil, ApplicationName, WinW, WinH, False);
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
