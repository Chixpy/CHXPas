program CT022;
{< The Coding Train Challenge #022 - Julia Set }

// Julia Set
// The Coding Train / Daniel Shiffman
// https://thecodingtrain.com/CodingChallenges/022-juliaset.html
// https://youtu.be/fAsaSkmbF5s
// https://editor.p5js.org/codingtrain/sketches/G6qbMmaI
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils,
  Math, //SDL have math methods too
  SDL2, sdl2_gfx,
  uCHXStrUtils,
  ucCHXSDL2Window, ucSDL2Engine,
  uProcUtils;

const
  // Renderer scales images to actual size of the window.
  WinW = 800; // Window logical width
  WinH = 800; // Window logical height

  // CHX: Making values as constants, can be set in OnSetup too

  // Establish a range of values on the complex plane
  // A different range will allow us to "zoom" in or out on the fractal

  // It all starts with the width, try higher or lower values
  w = 2;
  h = (w * WinH) / WinW;

  // Maximum number of iterations for each point on the complex plane
  maxiterations = 255;

  // Start at negative half the width and height
  xmin = -w / 2;
  ymin = -h / 2;

  // x goes from xmin to xmax
  xmax = xmin + w;
  // y goes from ymin to ymax
  ymax = ymin + h;

  // Calculate amount we increment x,y for each pixel
  dx = (xmax - xmin) / WinW;
  dy = (ymax - ymin) / WinH;


var // Global variables :-(
  SDL2Engine : cSDL2Engine;
  aTex : PSDL_Texture;
  aPxFmt: PSDL_PixelFormat;

  angle: Double;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  var
    aFmt: CUint32;
  begin
    // CHX: Allocating Window pixel format
    aFmt :=SDL_GetWindowPixelFormat(SDL2Engine.SDLWindow.PWindow);
    aPxFmt := SDL_AllocFormat(aFmt);

    // CHX: Creating a SDLTexture to edit its pixels
    aTex := SDL_CreateTexture(SDL2Engine.SDLWindow.PRenderer,
      aFmt, SDL_TEXTUREACCESS_STREAMING, WinW, WinH);


    angle := 0;

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // CHX:Free any created objects
    SDL_DestroyTexture(aTex);
    SDL_FreeFormat(aPxFmt);
  end;

  procedure PutPixel(Base : PCUInt32; Pitch : cint; x, y : word;
    r, g, b, a : byte);
  var
    PPoint : PCUInt32;
  begin
    PPoint := Base + y * (Pitch div 4) + x;
    // CHX: Don`t apply transparency, only sets it. PPoint is write-only and
    //   it doesn't have previous color value.

    // This is a litte faster, less than 5%, but only works in RGBA8888 in
    //   Windows and Intel (component order and endianess);
    //PPoint^ := (a shl 24) or (r shl 16) or (g shl 8) or b;

    // SDL_MapRGBA is the correct way but we need texture pixel format.
    PPoint^ := SDL_MapRGBA(aPxFmt, r, g, b, a);
  end;

  function OnCompute(Window : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    pitch : cint;
    PPBase : PCUInt32;
    x, y, a, b, aa, bb, twoab, ca, cb : Double; // fractal coords.
    n, i, j : integer; // screen coords.
  begin
    // CHX: As we are editing a SDLTexture and don't need a SDL_Renderer, we
    //   can do in OnCompute.
    SDL_LockTexture(aTex, nil, @PPBase, @pitch);

    angle += 0.01;
    if angle > 2 * pi then
      angle := angle - 2 * pi;

    ca := cos(angle*3.213);//sin(angle);
    cb := sin(angle);

    y := ymin;

    j := 0;
    while j < WinH do
    begin
      x := xmin;

      i := 0;
      while i < WinW do
      begin
        // Now we test, as we iterate z = z^2 + cm does z tend towards infinity?
        a := x;
        b := y;

        n := 0;
        while n < maxiterations do
        begin
          aa := a * a;
          bb := b * b;

          if (aa + bb) > 4 then
            Break; // CHX: Ouhg... :-(

          twoab := 2.0 * a * b;

          a := aa - bb + ca;
          b := twoab + cb;

          Inc(n);
        end;

        // We color each pixel based on how long it takes to get to infinity
        // If we never got there, let's pick the color black
        if n >= maxiterations then
          PutPixel(PPBase, pitch, i, j, 0, 0, 0, 255)
        else
        begin
          // Gosh, we could make fancy colors here if we wanted
          //float hu = sqrt(float(n) / maxiterations);
          //pixels[i+j*width] = color(hu, 255, 150);

          // CHX: Sorry no rainbow colors...
          n := round(sqrt(n / maxiterations) * 255);
          //n := round(n / maxiterations * 255);
          PutPixel(PPBase, pitch, i, j, n, n, n, 255);
        end;

        x += dx;

        Inc(i);
      end;

      y += dy;

      Inc(j);
    end;

    SDL_UnlockTexture(aTex);
    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  begin
    // Background
    //SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    //SDL_RenderClear(SDL2R);

    SDL_RenderCopy(SDL2R, aTex, nil, nil);

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
