program CT005;
{< The Coding Train Challenge #005 - Space Invaders }
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/biN3v3ef-Y0
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  SysUtils,
  ctypes,
  StrUtils,
  FileUtil,
  LazFileUtils,
  fgl,
  Math, //SDL have math
  sdl2,
  sdl2_gfx,
  uCHXStrUtils,
  ucSDL2Engine, ucCHXSDL2Window,
  uProcUtils,
  ucCTFlower,
  ucCTShip,
  ucCTDrop;

const
  WinW = 800; // Window width on creation
  WinH = 600; // Window height on creation

  NFlowers = 5;

type
  TDropList = specialize TFPGObjectList<cCTDrop>;

var
  Ship : cCTShip;
  Flowers : array of cCTFlower;
  // CHX: Dinamic arrays are bad idea if lenght is changed many times,
  //   specialized Object Lists are better
  // drops : array of cCTDrop;
  Drops : TDropList;

  function OnSetup : Boolean;
  var
    i : integer;
  begin
    Drops := TDropList.Create(True);
    Ship := cCTShip.Create(WinW div 2, WinH - 20);
    // drop = new Drop(width/2, height/2);
    SetLength(Flowers, NFlowers);
    for i := 0 to High(Flowers) do
      Flowers[i] := cCTFlower.Create(WinW div (NFlowers + 4) * (i + 2), 60);

    Result := True;
  end;

  function OnCompute(SDL2W : cCHXSDL2Window; DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    i, j : integer;
    Edge : Boolean;
  begin
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

    Result := True;
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    i : Integer;
  begin
    SDL_SetRenderDrawColor(SDL2R, 51, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    // Ship
    boxRGBA(SDL2R, Ship.x - 10, Ship.y - 30, Ship.x + 10, Ship.y + 30,
      255, 0, 0, 255);

    // Drops
    for i := 0 to Drops.Count - 1 do
      filledCircleRGBA(SDL2R, Drops[i].x, Drops[i].y, Drops[i].r,
        150, 0, 255, 255);

    // Flowers
    for i := 0 to High(Flowers) do
      filledCircleRGBA(SDL2R, Flowers[i].x, Flowers[i].y, Flowers[i].r,
        255, 0, 200, 150);

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
          SDLK_SPACE : Drops.Add(cCTDrop.Create(Ship.x, WinH));
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
    Ship.Free;
    for i := 0 to High(Flowers) do
      Flowers[i].Free;
    Drops.Free;
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
