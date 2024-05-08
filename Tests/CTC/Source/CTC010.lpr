program CTC010;
{< The Coding Train Challenge #010 - Maze Generator }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain

// Videos
// https://youtu.be/HyK_Q5rrcr4
// https://youtu.be/D8UgRyRnvXU
// https://youtu.be/8Ju_uxJ9v44
// https://youtu.be/_p5IH0L63wo

// Depth-first search
// Recursive backtracker
// https://en.wikipedia.org/wiki/Maze_generation_algorithm
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
  ucSDL2Engine, ucCHXSDL2Window,
  ucCTCCell;

const
  // Renderer scales images to actual size of the window.
  WinW = 800; // Window logical width
  WinH = 600; // Window logical height

  w = 19;

var // Global variables :-(
  Cols, Rows : integer;
  Grid : TCellList;
  Stack : TGenCellList;
  Current : cCTCCell;

  Speed : CUInt32; // CHX: To handle speed with Up/Down

  // Any auxiliar procedure/function will be here

  procedure removeWalls(a, b : cCTCCell);
  var
    z : integer;
  begin
    z := a.i - b.i;
    if (z = 1) then
    begin
      a.walls[3] := False;
      b.walls[1] := False;
    end
    else if (z = -1) then
    begin
      a.walls[1] := False;
      b.walls[3] := False;
    end;

    z := a.j - b.j;
    if (z = 1) then
    begin
      a.walls[0] := False;
      b.walls[2] := False;
    end
    else if (z = -1) then
    begin
      a.walls[2] := False;
      b.walls[0] := False;
    end;
  end;

  function OnSetup : Boolean;
  var
    j , i: Integer;
  begin
    Randomize;
    Cols := WinW div w;
    Rows := WinH div w;

    Speed := 210;

    Grid := TCellList.Create(True);
    Grid.Cols := Cols;
    Grid.Rows := Rows;

    Stack := TGenCellList.Create(False);

    for j := 0 to Rows - 1 do
      for  i := 0 to Cols - 1 do
        Grid.Add(cCTCCell.Create(i, j));

    Current := Grid[0];

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    Stack.Free;
    Grid.Free;
  end;

  function OnCompute(SDL2W : cCHXSDL2Window; DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    Next : cCTCCell;
  begin
    Current.visited := true;

    // STEP 1
    Next := Current.CheckNeighbors(Grid);
    if assigned(Next) then
    begin
      Next.visited := True;

      // STEP 2
      stack.add(current);

      // STEP 3
      removeWalls(current, Next);

      // STEP 4
      current := Next;
    end
    else if (stack.Count > 0) then
    begin
      // current = stack.Extract(stack.Last);
      // Faster
      Current := stack[stack.Count - 1];
      Stack.Delete(stack.Count - 1);
    end;

    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    aCell : cCTCCell;
    x, y : integer;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    // Grid
    for aCell in grid do
    begin
      y := aCell.j * w;
      x := aCell.i * w;

      if (aCell.walls[0]) then
        hlineRGBA(SDL2R, x, x + w, y, 255, 255, 255, 255);
      if (aCell.walls[1]) then
        vlineRGBA(SDL2R, x + w, y, y + w, 255, 255, 255, 255);
      if (aCell.walls[2]) then
        hlineRGBA(SDL2R, x + w, x, y + w, 255, 255, 255, 255);
      if (aCell.walls[3]) then
        vlineRGBA(SDL2R, x, y + w, y, 255, 255, 255, 255);

      // Visited
      if (aCell.visited) then
        boxRGBA(SDL2R, x, y, x + w, y + w, 255, 0, 255, 100);
    end;

    // Current
    if assigned(current) then
    begin
      y := Current.j * w;
      x := Current.i * w;
      boxRGBA(SDL2R, x, y, x + w, y + w, 0, 0, 255, 100);
    end;

    SDL_Delay(Speed);

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
          SDLK_UP : if Speed > 50 then Speed -= 50;
          SDLK_DOWN : Speed += 50;
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
