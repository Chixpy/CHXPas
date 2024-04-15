program CT006;
{< The Coding Train Challenge #006 - Mitosis Simulation }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/jxGS3fKPKJA
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, fgl,
  Math, //SDL have math methods too
  SDL2, sdl2_gfx,
  uCHXStrUtils,
  ucSDL2Engine, ucCHXSDL2Window,
  uProcUtils,
  ucCTCell;

const
  // Renderer scales images to actual size of the window.
  WinW = 800; // Window logical width
  WinH = 600; // Window logical height

type
  TCellList = specialize TFPGObjectList<cCTCell>;

var // Global variables :-(
  Cells : TCellList;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  var
    aPoint : TPoint;
  begin
    Cells := TCellList.Create(True);
    Randomize;

    aPoint := Point(Random(WinW div 2) + WinW div 4,
      Random(WinH div 2) + WinH div 4);
    Cells.Add(cCTCell.Create(aPoint, RandomRange(50, 120), $FF0000FF));

    aPoint := Point(Random(WinW div 2) + WinW div 4,
      Random(WinH div 2) + WinH div 4);
    Cells.Add(cCTCell.Create(aPoint, RandomRange(50, 120), $FF00FFFF));

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    Cells.Free;
  end;

  function OnCompute(SDL2W : cCHXSDL2Window; DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    aCell : cCTCell;
  begin
    for aCell in Cells do
      aCell.Move;

    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    aCell : cCTCell;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    for aCell in Cells do
    begin
      filledCircleColor(SDL2R, aCell.Pos.X, aCell.Pos.Y,
        Round(aCell.R), aCell.C);
      circleRGBA(SDL2R, aCell.Pos.X, aCell.Pos.Y,
        Round(aCell.R), 0, 0, 0, 255);
    end;

    Result := True; // False -> Finish program
  end;

  function OnEvent(aEvent : TSDL_Event) : Boolean;
  var
    i : integer;
    aCell : cCTCell;
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

      //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        i := Cells.Count - 1;
        while i >= 0 do
        begin
          aCell := Cells[i];
          if aCell.Clicked(aEvent.button.x, aEvent.button.y) then
          begin
            if aCell.R > 10 then
            begin
              Cells.Add(aCell.Mitosis);
              Cells.Add(aCell.Mitosis);
            end;
            Cells.Delete(i);
          end;

          Dec(i);
        end;
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
