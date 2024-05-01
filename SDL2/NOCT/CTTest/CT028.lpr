program CT028;
{< The Coding Train Challenge #028 - Metaballs }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/ccYLb7cLB1I
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
  uProcUtils,
  ucCTBlob;

const
  // Renderer scales images to actual size of the window.
  WinW = 400; // Window logical width
  WinH = 400; // Window logical height

var // Global variables :-(
  SDL2Engine : cSDL2Engine;
  aTex : PSDL_Texture;
  aPxFmt : PSDL_PixelFormat;

  blobs : array[0..7] of cCTBlob;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  var
    aFmt : CUInt32;
    i : integer;
  begin
    // CHX: Allocating Window pixel format
    aFmt := SDL_GetWindowPixelFormat(SDL2Engine.SDLWindow.PWindow);
    aPxFmt := SDL_AllocFormat(aFmt);

    // CHX: Creating a SDLTexture to edit its pixels
    aTex := SDL_CreateTexture(SDL2Engine.SDLWindow.PRenderer,
      aFmt, SDL_TEXTUREACCESS_STREAMING, WinW, WinH);

    for i := Low(blobs) to High(blobs) do
      blobs[i] := cCTBlob.Create(random * WinW, random * WinH);

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  var
    i : integer;
  begin
    // CHX:Free any created objects
    for i := Low(blobs) to High(blobs) do
      blobs[i].Free;

    SDL_DestroyTexture(aTex);
    SDL_FreeFormat(aPxFmt);
  end;

  procedure PutPixel(Base : PCUInt32; Pitch : cint; x, y : word;
    r, g, b, a : byte);
  var
    PPoint : PCUInt32;
  begin
    PPoint := Base + y * (Pitch div 4) + x;
    // CHX: Doesn't apply transparency, only sets it. PPoint is write-only and
    //   it doesn't have previous color value.

    // This is a little faster, less than 5%, but only works in RGBA8888 in
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

    b : cCTBlob;
    y, x : integer;
    sum , d: Float;
    Col: Byte;
  begin
    { If we want to pause when minimized or lost focus. }
    // if Window.Minimized then
    // begin
    //   Result := True;
    //   Exit;
    // end;

    // CHX: As we are editing a SDLTexture and don't need a SDL_Renderer, we
    //   can do in OnCompute.
    SDL_LockTexture(aTex, nil, @PPBase, @pitch);

    y := 0;
    while y < WinH do
    begin
      x := 0;
      while x < WinW do
      begin
        sum := 0;
        for b in blobs do
        begin
          d := b.pos.Distance(Point3DF(x, y));
          if d <> 0 then // Zero division
            sum += 200 * b.r / d;
        end;

        // CHX: Weigh down color by numbers of blobs
        //   so, we don't need to change constant in sum
        col := EnsureRange(Round(sum) div length(blobs), 0, 255);

        //pixels[index] = color(sum, 255, 255); // no HSB
        PutPixel(PPBase, pitch, x, y, col, col, col, 255);

        Inc(x);
      end;

      Inc(y);
    end;

    SDL_UnlockTexture(aTex);
    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    b : cCTBlob;
    r : integer;
  begin
    // Background
    //SDL_SetRenderDrawColor(SDL2R, 51, 51, 51, 255);
    //SDL_RenderClear(SDL2R);

    SDL_RenderCopy(SDL2R, aTex, nil, nil);

    for b in blobs do
    begin
      // CHX: Reducing blob radius based in previous color weigh down.
      r := Round(b.r) div length(blobs);
      circleRGBA(SDL2R, Round(b.pos.X), Round(b.pos.Y), r,
        0, 0, 0, 255);

      // CHX: Updating after draw, so circle is centered on previous
      //   calculated pixels
      b.update;

      if not InRange(b.pos.x, 0, WinW) then
        b.vel.X := -b.vel.X;
      if not InRange(b.pos.y, 0, WinH) then
        b.vel.Y := -b.vel.Y;
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
