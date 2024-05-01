program CT019;
{< The Coding Train Challenge #019 - Superellipse }

// Daniel Shiffman
// https://thecodingtrain.com/CodingChallenges/019-superellipse.html
// https://youtu.be/z86cx2A4_3E
// https://editor.p5js.org/codingtrain/sketches/Hk-1AMTgN
// Processing transcription: Chuck England
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
  uProcUtils,
  ucCTSlider;

const
  // Renderer scales images to actual size of the window.
  WinW = 300; // Window logical width
  WinH = 300; // Window logical height

  // CHX:
  Incr = 0.001; // Increment beetwen points
  TWO_PI = PI * 2;
  OffsetX = WinW div 2;
  OffsetY = WinH div 2;

var // Global variables :-(    .
  slider : cCTSlider;
  // CHX: Added to store the shape
  points : array of TSDL_FPoint;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  begin
    SetLength(points, Ceil(TWO_PI / Incr));

    slider := cCTSlider.Create(-WinW div 2 + 20 + OffsetX,
      -WinH div 2 + 20 + OffsetY, 0.01, 4, 2, 0.01);

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    slider.Free;
  end;

  function OnCompute(Window : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    a, b, n, angle, na : Double;
    aPoint : TSDL_FPoint;
    idx : integer;
  begin

    a := 100;
    b := 100;
    n := slider.Value;
    //n := 0.7;

    angle := 0;
    idx := 0;
    while angle < TWO_PI do
    begin
      //// Simple ellipse
      //aPoint.x := 200 * cos(angle) + WinW / 2;
      //aPoint.y := 200 * sin(angle) + WinH / 2;

      // Superellipse
      na := 2 / n;
      aPoint.x := Power(abs(cos(angle)), na) * a * Sign(cos(angle)) + OffsetX;
      aPoint.y := Power(abs(sin(angle)), na) * b * Sign(sin(angle)) + OffsetY;

      Points[idx] := aPoint;
      Inc(idx);
      angle := angle + Incr;
    end;

    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    x0, x1, y0, y1 : integer;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 51, 51, 51, 255);
    SDL_RenderClear(SDL2R);

    //translate(width / 2, height / 2);

    SDL_SetRenderDrawColor(SDL2R, 255, 255, 255, 255);
    SDL_RenderDrawPointsF(SDL2R, @points[0], Length(points));


    // cCTSlider.show
    x0 := slider.x;
    y0 := slider.y + slider.h div 3;
    x1 := slider.w + x0;
    y1 := slider.h div 3 + y0;

    rectangleRGBA(SDL2R, x0, y0, x1, y1, 255, 255, 255, 255);

    x0 := round(slider.xPosition(slider.Value)) - slider.controlSize div 2;
    y0 := slider.y;
    x1 := slider.controlSize + x0;
    y1 := slider.h + y0;

    boxRGBA(SDL2R, x0, y0, x1 - 1, y1 - 1, 128, 128, 128, 255);
    rectangleRGBA(SDL2R, x0, y0, x1, y1, 200, 200, 200, 255);

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

      SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      begin
        if aEvent.motion.state <> 0 then
          slider.handleInteractions(aEvent.motion.x, aEvent.motion.y, True);
      end;
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        slider.handleInteractions(aEvent.button.x, aEvent.button.y, False);
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
