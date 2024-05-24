unit ucCHXSDL2Engine;
{< Unit of cSDL2Engine class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes, Math,
  // SDL2
  SDL2, SDL2_GFX, SDL2_TTF,
  // CHX SDL2
  ucCHXSDL2Config, ucCHXSDL2Window, ucCHXSDL2Font;

resourcestring
  rsErrWinNotCreated = 'cCHXSDL2Window was not created.';
  rsErrSDLWinNotCreated = 'cCHXSDL2Window.PSDLWindow was not created.';

type
  { cCHXSDL2Engine }

  cCHXSDL2Engine = class(TComponent)
  private
    FConfig : cCHXSDL2Config;
    FDefFont : cCHXSDL2Font;
    FPWinPxFmt : PSDL_PixelFormat;
    FSDLWindow : cCHXSDL2Window;
    FShowFrameRate : Boolean;

    {property} SDLFrameMang : TFPSManager;
    {property} FFrameCount : QWord; // More than the age of the universe

    procedure SetShowFrameRate(const AValue : Boolean);

  protected
    property SDLWindow : cCHXSDL2Window read FSDLWindow;

    property ShowFrameRate : Boolean
      read FShowFrameRate write SetShowFrameRate;

    property DefFont : cCHXSDL2Font read FDefFont;

    property PWinPxFmt : PSDL_PixelFormat read FPWinPxFmt;
    //< Window pixel format for new textures.
    procedure PutPixel(const Base : PCUInt32; const Pitch : CInt;
      const x : word; const y : word;
      const r : byte; const g : byte; const b : byte; const a : byte);
    //< Put Pixel in a locked texture.

    function FrameCount : QWord; inline;
    //< Current frame number

    // Virtual methods to implement in child classes
    procedure Setup; virtual; abstract;
    procedure Finish; virtual; abstract;
    procedure Compute(const FrameTime : CUInt32;
      var ExitProg : Boolean); virtual; abstract;
    procedure Draw; virtual; abstract;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); virtual;

  public
    {property} Title : string;

    property Config : cCHXSDL2Config read FConfig;

    procedure Init;
    {< Init engine and window. }

    procedure Run;
    {< Run engine. }

    constructor Create(const aTitle : string; const aWinWidth : CInt;
      const aWinHeight : CInt; const HWAcc : Boolean = True); overload;
    {< Simple constructor.

      @param(aTitle Title of the window.)
      @param(aWinWidth, aWinHeight Size of the window.)
      @param(HWAcc Use HW acceleration.)
    }
    constructor Create(const aTitle : string; const aIniFile : string;
      const AutoInit : Boolean = False); overload;
    {< Constructor reading an .ini file with the settings.

       @param(aTitle Title of the window.)
       @param(aIniFile File with engine settings.)
       @param(AutoInit Init engine after reading the file. If @False,
         cCHXSDL2Engine.Config properties can be changed and then
         cCHXSDL2Engine.Init must be called.)
    }
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Engine }

procedure cCHXSDL2Engine.SetShowFrameRate(const AValue : Boolean);
begin
  if FShowFrameRate = AValue then Exit;
  FShowFrameRate := AValue;

  SDLWindow.Title := Self.Title;
end;

procedure cCHXSDL2Engine.PutPixel(const Base : PCUInt32; const Pitch : CInt;
  const x : word; const y : word; const r : byte; const g : byte;
  const b : byte; const a : byte);
var
  PPoint : PCUInt32;
begin
  PPoint := Base + y * (Pitch div 4) + x;
  // CHX: Doesn't apply transparency, only sets it.
  //   PPoint is write-only and it doesn't have previous color value.

  // Raw editing is a little faster, less than 5%.

  // TODO: Actually, Window pixel format don't have transparency...
  //   Texture pixel format must be a parameter.
  // TODO2: Test endianess for RGB888 and BGR888 is correct.
  case PWinPxFmt^.format of
    {$IF DEFINED(ENDIAN_LITTLE)}SDL_PIXELFORMAT_RGB888,{$IFEND}
    SDL_PIXELFORMAT_ARGB8888 :
      PPoint^ := (a shl 24) or (r shl 16) or (g shl 8) or b;

    {$IF DEFINED(ENDIAN_LITTLE)}SDL_PIXELFORMAT_BGR888,{$IFEND}
    SDL_PIXELFORMAT_ABGR8888 :
      PPoint^ := (a shl 24) or (b shl 16) or (g shl 8) or r;

    {$IF DEFINED(ENDIAN_BIG)}SDL_PIXELFORMAT_RGB888,{$IFEND}
    SDL_PIXELFORMAT_RGBA8888 :
      PPoint^ := (r shl 24) or (g shl 16) or (b shl 8) or a;

    {$IF DEFINED(ENDIAN_BIG)}SDL_PIXELFORMAT_BGR888,{$IFEND}
    SDL_PIXELFORMAT_BGRA8888 :
      PPoint^ := (b shl 24) or (g shl 16) or (r shl 8) or a;
    else
      // SDL_MapRGBA is the correct way, but we need texture pixel format.
      PPoint^ := SDL_MapRGBA(PWinPxFmt, r, g, b, a);
  end;
end;

function cCHXSDL2Engine.FrameCount : QWord;
begin
  // This is reseted every time a frame is late, for example moving,
  //   resizing, focus a window, etc.
  //Result := SDL_getFramecount(@SDLFrameMang);
  Result := FFrameCount;
end;

procedure cCHXSDL2Engine.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
begin
  if Handled then
    Exit;

  // Some events are listed and commented out to have an easy reference.
  // Window and general quit events are handled automatically.
  // Escape key is mapped to exit the program.
  // F11 toggles framerate in window title.

  case aEvent.type_ of

    SDL_WINDOWEVENT :
      case aEvent.window.event of
        SDL_WINDOWEVENT_EXPOSED, SDL_WINDOWEVENT_SIZE_CHANGED :
          //< Not needed in cCHXSDL2Engine context
          Handled := True;
        else
          SDLWindow.HandleEvent(aEvent, Handled);
      end;

    //SDL_KEYUP : // (key: TSDL_KeyboardEvent);
    SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      case aEvent.key.keysym.sym of
        //SDLK_UP : ;
        //SDLK_DOWN : ;
        //SDLK_LEFT : ;
        //SDLK_RIGHT : ;
        //SDLK_SPACE : ;
        SDLK_F11 : ShowFrameRate := not ShowFrameRate;
        SDLK_ESCAPE :
        begin
          ExitProg := True; // Exit
          Handled := True;
        end;
        else
          ;
      end;
    //SDL_TEXTEDITING : // (edit: TSDL_TextEditingEvent);
    //SDL_TEXTEDITING_EXT : // (exitExt: TSDL_TextEditingExtEvent);
    //SDL_TEXTINPUT : // (text: TSDL_TextInputEvent);

    //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
    //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
    //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
    //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);

    SDL_QUITEV : // General exit event
    begin
      ExitProg := True;
      Handled := True;
    end;
    else
      ;
  end;
end;

procedure cCHXSDL2Engine.Init;
begin
  if SDL_WasInit(0) = 0 then
    SDL_Init(SDL_INIT_EVERYTHING);

  SDL_FreeFormat(FPWinPxFmt);
  FreeAndNil(FSDLWindow);
  FreeAndNil(FDefFont);

  FSDLWindow := cCHXSDL2Window.Create(Title, Config.WindowWidth,
    Config.WindowHeight, Config.RendererUseHW, Config.RendererWidth,
    Config.RendererHeight);

  if not assigned(SDLWindow) then
    raise Exception.Create(rsErrWinNotCreated);
  if SDLWindow.WindowID = 0 then
  begin
    FreeAndNil(FSDLWindow);
    raise Exception.Create(rsErrSDLWinNotCreated);
  end;

  FPWinPxFmt := SDL_AllocFormat(SDL_GetWindowPixelFormat(SDLWindow.PWindow));

  if FileExists(Config.DefFontFile) and (Config.DefFontSize > 0) then
    FDefFont := cCHXSDL2Font.Create(SDLWindow.PRenderer, Config.DefFontFile,
      Config.DefFontSize, Config.DefFontColor);
end;

procedure cCHXSDL2Engine.Run;
var
  ProgExit, HandledEvent : Boolean;
  DeltaTime, LastFrameTime : CUInt32;
  aEvent : TSDL_Event;
begin
  ProgExit := False;

  SDL_InitFramerate(@SDLFrameMang);
  SDL_SetFramerate(@SDLFrameMang, 60);
  LastFrameTime := 0;
  DeltaTime := 0;
  FFrameCount := 0;

  try
    Self.Setup;

    while (not ProgExit) do
    begin
      inc(FFrameCount);

      // COMPUTE
      if (not ProgExit) then
        Self.Compute(LastFrameTime, ProgExit);

      // TIMING (1)
      // Actual Compute + Events time in milliseconds.
      DeltaTime := SDL_GetTicks - DeltaTime;
      // Frame rate in milliseconds. Compute + Event + Draw + Delay
      LastFrameTime := SDL_framerateDelay(@SDLFrameMang);

      // Don't draw if minimized
      if (not ProgExit) and (not SDLWindow.Minimized) then
      begin
        // DRAW
        Self.Draw;

        if ShowFrameRate and
          ((FrameCount and 31) = 0) then
          SDLWindow.Title := Format('%0:s: %1:d ms (%2:d ms)',
            [Title, LastFrameTime, DeltaTime]);

        // UPDATE RENDER
        SDL_RenderPresent(SDLWindow.PRenderer);
      end;

      // TIMING (2)
      DeltaTime := SDL_GetTicks;

      // EVENTS

      { NOTE : Listen window events when minimized. }

      // SDL_PumpEvents;

      while (SDL_PollEvent(@aEvent) <> 0) and (not ProgExit) do
      begin
        HandledEvent := False; // Used to see if a event is Handled
        Self.HandleEvent(aEvent, HandledEvent, ProgExit);
      end;
    end;

  finally
    Self.Finish;
  end;
end;

constructor cCHXSDL2Engine.Create(const aTitle : string;
  const aWinWidth : CInt; const aWinHeight : CInt; const HWAcc : Boolean);
begin
  inherited Create(nil);

  Title := aTitle;
  FShowFrameRate := False;

  Config.WindowWidth := aWinWidth;
  Config.RendererWidth := aWinWidth;
  Config.WindowHeight := aWinHeight;
  Config.RendererHeight := aWinHeight;
  Config.RendererUseHW := HWAcc;

  Init;
end;

constructor cCHXSDL2Engine.Create(const aTitle : string;
  const aIniFile : string; const AutoInit : Boolean);
begin
  inherited Create(nil);

  Title := aTitle;
  FShowFrameRate := False;

  FConfig := cCHXSDL2Config.Create(Self);
  Config.DefaultFileName := aIniFile;
  Config.LoadFromFile('');

  if AutoInit then
    Init
  else
    // If Config will be changed manually then no save changes
    Config.DefaultFileName := '';
end;

destructor cCHXSDL2Engine.Destroy;
begin
  if Config.DefaultFileName <> '' then
  begin
    Config.SaveToFile('', False);
    Config.Free;
  end;

  SDL_FreeFormat(FPWinPxFmt);
  FreeAndNil(FDefFont);
  FreeAndNil(FSDLWindow);

  SDL_Quit;

  inherited Destroy;
end;

end.
{< This source is free software; you can redistribute it and/or modify it under
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
