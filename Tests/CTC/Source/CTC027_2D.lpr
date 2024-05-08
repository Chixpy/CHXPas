program CTC027_2D;
{< The Coding Train Challenge #027 - Fireworks (2D) }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/CKeyIbT3vXI
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  Classes,
  SysUtils,
  CTypes,
  StrUtils,
  FileUtil,
  LazFileUtils,
  Math, //SDL2 have some math methods too
  SDL2,
  sdl2_gfx,
  uCHXPoint3DF,
  uCHXStrUtils,
  ucCHXSDL2Window,
  ucSDL2Engine,
  ucCTCParticle2D,
  ucCTCFirework2D;

const
  // Renderer scales images to actual size of the window.
  WinW = 640; // Window logical width
  WinH = 360; // Window logical height

var // Global variables :-(
  Fireworks : cCTFirework2DList;
  gravity : TPoint3DF;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  begin
    Fireworks := cCTFirework2DList.Create(True);
    gravity := Point3DF(0, 0.2);

    // colorMode(HSB);

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    Fireworks.Free;
  end;

  function OnCompute(Window : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    i : integer;
    f : cCTCFirework2D;
  begin
    { If we want to pause when minimized or lost focus.}
    // if Window.Minimized then
    // begin
    //   Result := True;
    //   Exit;
    // end;

    // CHX: Passing some global variables as parameters...
    if Random < 0.08 then
      fireworks.add(cCTCFirework2D.Create(RandomRange(0, WinW),
        WinH, RandomRange(0, 256), RandomRange(0, 256), RandomRange(0, 256), gravity));

    for i := Fireworks.Count - 1 downto 0 do
    begin
      f := Fireworks[i];
      f.run;
      if f.done then
        Fireworks.Delete(i);
    end;

    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    f : cCTCFirework2D;
    p : cCTCParticle2D;
    r : integer;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 51, 51, 51, 255);
    SDL_RenderClear(SDL2R);

    // CHX: Drawing
    for f in Fireworks do
    begin
      // stroke(hu, 255, 255, lifespan); // Simulated by random RGB
      p := f.firework;

      if assigned(p) then
      begin
        if p.seed then
          r := 4
        else
          r := 2;

        filledcircleRGBA(SDL2R, Round(p.location.x), Round(p.location.y), r,
          p.r, p.g, p.b, Round(p.lifespan));
      end;

      // CHX: I know that this is repeated
      for p in f.particles do
      begin
        if p.seed then
          r := 4
        else
          r := 2;

        filledcircleRGBA(SDL2R, Round(p.location.x), Round(p.location.y), r,
          p.r, p.g, p.b, Round(p.lifespan));
      end;
    end;

    Result := True; // False -> Finish program
  end;

  function OnEvent(aEvent : TSDL_Event) : Boolean;
  begin
    Result := True; // False -> Finish program

    // EVENTS:
    // Commented out for easy reference

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
