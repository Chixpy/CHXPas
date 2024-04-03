program NOTCMover;
{$mode ObjFPC}{$H+}
uses
  SysUtils,
  ctypes,
  StrUtils,
  FileUtil,
  LazFileUtils,
  sdl2,
  sdl2_gfx,
  uCHXPoint3DF,
  uCHXStrUtils,
  ucSDL2Engine,
  ucNOTCMover;

const
  WinW = 800; // Window width on creation
  WinH = 600; // Window height on creation

var
  aMover : cNOTCMover;
  MoverSize : Double;

  aBorder : Double;

  function OnSetup : Boolean;
  begin
    aMover := cNOTCMover.Create;
    aMover.Position.Init(WinW div 2, WinH div 2, 0);
    aMover.Velocity.SetX(-1);
    MoverSize := 20;
    aMover.Mass := MoverSize;

    aBorder := 10;

    Result := True;
  end;

  function OnCompute(DeltaTime, FrameTime : CUInt32) : Boolean;
  var
    Temp : Double;
  begin

    Temp := MoverSize / 2 + aBorder;

    //Some gravity to Mover
    aMover.Force.Move(0, 1, 0);
    aMover.Update;

    // Bounce
    if (aMover.Position.X < Temp) then
    begin
      aMover.Position.SetX(2 * Temp - aMover.Position.X);
      aMover.Velocity.SetX(-aMover.Velocity.X);
    end
    else if (aMover.Position.X > (WinW - Temp)) then
    begin
      aMover.Position.SetX(2 * (WinW - Temp) - aMover.Position.X);
      aMover.Velocity.SetX(-aMover.Velocity.X);
    end;

    if (aMover.Position.Y > (WinH - Temp)) then
    begin
      aMover.Position.SetY(2 * (WinH - Temp) - aMover.Position.Y);
      aMover.Velocity.SetY(-aMover.Velocity.Y);
    end;

    Result := True;
  end;

  function OnDraw(SDL2W : PSDL_Window; SDL2R : PSDL_Renderer) : Boolean;
  var
    RW, RH : cint;
  begin
    SDL_GetRendererOutputSize(SDL2R, @RW, @RH);

    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    // Draw box
    rectangleRGBA(SDL2R, Round(ABorder), Round(ABorder),
      Round(RW - ABorder), Round(RH - ABorder), 255, 255, 0, 255);

    // Draw Mover
    roundedBoxRGBA(SDL2R, Round(aMover.Position.X - MoverSize / 2),
      Round(aMover.Position.Y - MoverSize / 2),
      Round(aMover.Position.X + MoverSize / 2),
      Round(aMover.Position.Y + MoverSize / 2), 5, 255, 0, 0, 255);

    Result := True;
  end;

  function OnEvent(aEvent : TSDL_Event) : Boolean;
  begin
    Result := True;

    // EVENTS

    case aEvent.type_ of
      //SDL_COMMONEVENT : ;// (common: TSDL_CommonEvent);
      //SDL_DISPLAYEVENT : ;// (display: TSDL_DisplayEvent);
      // SDL2Engine: SDL_WINDOWEVENT : SDLWindow.HandleEvent(aEvent);
      // (window: TSDL_WindowEvent);

      //SDL_KEYUP, // (key: TSDL_KeyboardEvent);
      SDL_KEYDOWN :
      begin
        case aEvent.key.keysym.sym of
          SDLK_UP : aMover.Force.Move(0, -1, 0);
          SDLK_DOWN : aMover.Force.Move(0, 1, 0);
          SDLK_LEFT : aMover.Force.Move(-1, 0, 0);
          SDLK_RIGHT : aMover.Force.Move(1, 0, 0);
          SDLK_SPACE :aMover.Velocity.Init(0, 0, 0);
          SDLK_ESCAPE : Result := False;
          else
            ;
        end;
      end;
        //SDL_TEXTEDITING : ;// (edit: TSDL_TextEditingEvent);
        //SDL_TEXTEDITING_EXT : ;// (exitExt: TSDL_TextEditingExtEvent);
        //SDL_TEXTINPUT : ;// (text: TSDL_TextInputEvent);

        //SDL_MOUSEMOTION : ;// (motion: TSDL_MouseMotionEvent);
        //SDL_MOUSEBUTTONUP,
        //SDL_MOUSEBUTTONDOWN : ;// (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEWHEEL : ;// (wheel: TSDL_MouseWheelEvent);

        //SDL_JOYAXISMOTION : ;// (jaxis: TSDL_JoyAxisEvent);
        //SDL_JOYBALLMOTION : ;// (jball: TSDL_JoyBallEvent);
        //SDL_JOYHATMOTION : ;// (jhat: TSDL_JoyHatEvent);
        //SDL_JOYBUTTONDOWN,
        //SDL_JOYBUTTONUP : ;// (jbutton: TSDL_JoyButtonEvent);
        //SDL_JOYDEVICEADDED,
        //SDL_JOYDEVICEREMOVED : ;// (jdevice: TSDL_JoyDeviceEvent);
        //SDL_JOYBATTERYUPDATED : ;// (jbattery: TSDL_JoyBatteryEvent);

        //SDL_CONTROLLERAXISMOTION : ;// (caxis: TSDL_ControllerAxisEvent);
        //SDL_CONTROLLERBUTTONUP,
        //SDL_CONTROLLERBUTTONDOWN : ;// (cbutton: TSDL_ControllerButtonEvent);
        //SDL_CONTROLLERDEVICEADDED,
        //SDL_CONTROLLERDEVICEREMOVED,
        //SDL_CONTROLLERDEVICEREMAPPED : ;// (cdevice: TSDL_ControllerDeviceEvent);
        //SDL_CONTROLLERTOUCHPADDOWN,
        //SDL_CONTROLLERTOUCHPADMOTION,
        //SDL_CONTROLLERTOUCHPADUP : ;// (ctouchpad: TSDL_ControllerTouchpadEvent);
        //SDL_CONTROLLERSENSORUPDATE : ;// (csensor: TSDL_ControllerSensorEvent);

        //SDL_AUDIODEVICEADDED,
        //SDL_AUDIODEVICEREMOVED : ;// (adevice: TSDL_AudioDeviceEvent);

        //SDL_SENSORUPDATED : ;// (sensor: TSDL_SensorEvent);

        // SDL2Engine: SDL_QUITEV : ProgRunning := False;

        //SDL_USEREVENT : ;// (user: TSDL_UserEvent);
        //SDL_SYSWMEVENT : ;// (syswm: TSDL_SysWMEvent);

        //SDL_FINGERDOWN,
        //SDL_FINGERUP,
        //SDL_FINGERMOTION : ;// (tfinger: TSDL_TouchFingerEvent);
        //SDL_MULTIGESTURE : ;// (mgesture: TSDL_MultiGestureEvent);
        //SDL_DOLLARGESTURE, SDL_DOLLARRECORD : ;
        //// (dgesture: TSDL_DollarGestureEvent);

        //SDL_DROPFILE : ;// (drop: TSDL_DropEvent);
      else
        ;
    end;

  end;

  procedure OnFinish;
  begin
    aMover.Free;
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
