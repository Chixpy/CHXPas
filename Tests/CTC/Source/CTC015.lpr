program CTC015;
{< The Coding Train Challenge #015 - OO Fractal Trees }

//* makes a FractalTree using Array(Lists)
//* adds one Level per MouseClick
//* at Level 6 Leaves will fall down
//* @author Lukas Klassen
//* translated version of CC_015_FractalTreeArray by Daniel Shiffmann

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for:                            <- Change this
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}
uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, fgl,
  Math, //SDL have math methods too
  SDL2, sdl2_gfx,
  uCHXStrUtils, uCHXPoint3DF,
  ucCHXSDL2Window, ucSDL2Engine,
  ucCTCBranch;

const
  // Renderer scales images to actual size of the window.
  WinW = 400; // Window logical width
  WinH = 400; // Window logical height

  NIter = 6; // Number of max iterations

type
  TTree = specialize TFPGObjectList<cCTCBranch>;
  TLeaves = specialize TFPGList<TPoint3DF>;

var // Global variables :-(
  Tree : TTree;
  Leaves : TLeaves;
  Count : integer;

  // Any auxiliar procedure/function will be here

  function OnSetup : Boolean;
  var
    Root : cCTCBranch;
  begin
    Tree := TTree.Create(True);
    Leaves := TLeaves.Create;

    Root := cCTCBranch.Create(Point3DF(WinW / 2, WinH, 0),
      Point3DF(WinW / 2, WinH - 100, 0));

    Tree.Add(Root);

    Result := True; // False -> Finish program
  end;

  procedure OnFinish;
  begin
    // Free any created objects
    Tree.Free;
    Leaves.Free;
  end;

  function OnCompute(Window : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    l : TPoint3DF;
    i : Integer;
  begin
    { A little dream, l is a copy and it can't be assigned to. }
    //for l in Leaves do
    //  //let the Leave fall
    //  l.Y := l.Y + Random * 4;

    //forEach Leave: draw it
    i := Leaves.Count - 1;
    while i >= 0 do
    begin
      l := Leaves[i]; // CHX: Returns a copy
      //let the Leave fall
      // CHX: I don't know why l.Y := l.Y + Random * 4 don't work.
      l.SetY(l.Y + Random * 4);
      Leaves[i] := l;


      // CHX: removing leaves offscreen
      if Leaves[i].Y > WinH then
        Leaves.Delete(i);
      Dec(i);
    end;
    Result := True; // False -> Finish program
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    b : cCTCBranch;
    l : TPoint3DF;
  begin
    // Background
    SDL_SetRenderDrawColor(SDL2R, 51, 51, 51, 255);
    SDL_RenderClear(SDL2R);

    SDL_SetRenderDrawColor(SDL2R, 255, 255, 255, 255);
    //forEach Branch of the Tree: Draw it
    for b in Tree do
      SDL_RenderDrawLineF(SDL2R, b.beginB.X, b.beginB.Y,
        b.endB.X, b.endB.Y);

    for l in Leaves do
      filledCircleRGBA(SDL2R, round(l.x), round(l.y), 4, 255, 0, 100, 100);

    Result := True; // False -> Finish program
  end;

  function OnEvent(aEvent : TSDL_Event) : Boolean;
  var
    i : integer;
    Current : cCTCBranch;
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
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        // CHX: Stop iterating, and make a constant with iteration.
        if Count <= NIter then
        begin
        i := Tree.Count - 1;
        while i >= 0 do
        begin
          Current := Tree[i];

          if not Current.finished then
          begin
            Tree.Add(Current.branchA);
            Tree.Add(Current.branchB);
          end;

          Current.finished := True;

          Dec(i);
        end;

        end;

        //new Level added
        Inc(Count);

        //on the 6. Level: spawn the Leaves
        if Count >= NIter then
          for Current in Tree do
            if not Current.finished then
              Leaves.add(Current.endB);

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
