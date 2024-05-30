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
  uCHXSDL2Utils,
  ucCHXSDL2Config, ucCHXSDL2Window, uaCHXSDL2Font, ucCHXSDL2FontGFX,
  ucCHXSDL2FontTTF;

resourcestring
  rsErrWinNotCreated = 'cCHXSDL2Window was not created.';
  rsErrSDLWinNotCreated = 'cCHXSDL2Window.PSDLWindow was not created.';

type
  { cCHXSDL2Engine }

  cCHXSDL2Engine = class(TComponent)
  private
    FConfig : cCHXSDL2Config;
    FDefFont : caCHXSDL2Font;
    FPWinPxFmt : PSDL_PixelFormat;
    FSDLWindow : cCHXSDL2Window;
    FShowFrameRate : Boolean;

    {property} SDLFrameMang : TFPSManager;
    {property} FFrameCount : QWord; // More than the age of the universe

    // Current Input Text properties
    {property} CTIFont : caCHXSDL2Font;
    {property} CTIX : Integer;
    {property} CTIY : Integer;
    {property} CTIWidth : Integer;
    {property} CTIStrVar : PString;
    {property} CTIUpdateLive : Boolean;

    procedure SetShowFrameRate(const AValue : Boolean);

  protected
    {property} CurrTextInput : string;

    property SDLWindow : cCHXSDL2Window read FSDLWindow;

    property ShowFrameRate : Boolean
      read FShowFrameRate write SetShowFrameRate;

    property DefFont : caCHXSDL2Font read FDefFont;
    {< Default TTF font to use with the engine. A TTF file, size and color must
         be set in config file or manually before Init call.

       After font is loaded we can change font style with
         cCHXSDL2FontTTF.ChangeFontStyle, but this not for continuous calls
         because it will remove cached glyphs and texts.

       If not font is loaded, 8 bit ASCII}

    property PWinPxFmt : PSDL_PixelFormat read FPWinPxFmt;
    //< Window pixel format for new textures.
    procedure PutPixel(const Base : PCUInt32; const Pitch : CInt;
      const X, Y : Word; const r, g, b, a : Byte);
    //< Put Pixel in a locked texture.

    function FrameCount : QWord; inline;
    {< Current frame number. }

    procedure TextInput(aFont : caCHXSDL2Font; var aText : string;
      const aX, aY : Integer; const aWidth : Integer = 0;
      const UpdateLive : Boolean = False);
    {< Starts input text, so keyboard events of common keys will be disabled
      until Enter or another event stops it. }
    function IsEditingText : Boolean; inline;
    {< are we currently editing text? }

    // Virtual methods to implement in child classes
    procedure Setup; virtual; abstract;
    procedure Finish; virtual; abstract;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      virtual; abstract;
    procedure Draw; virtual; abstract;
    procedure HandleEvent(const aEvent : TSDL_Event;
      var Handled, ExitProg : Boolean); virtual;

  public
    {property} Title : string;

    property Config : cCHXSDL2Config read FConfig;

    procedure Init;
    {< Init engine and window. }

    procedure Run;
    {< Run engine. }

    constructor Create(const aTitle : string;
      const aWinWidth, aWinHeight : CInt; const HWAcc : Boolean = True;
      const AutoInit : Boolean = True);
      overload;
    {< Simple constructor.

      @param(aTitle Title of the window.)
      @param(aWinWidth, aWinHeight Size of the window.)
      @param(HWAcc Use HW acceleration.)
      @param(AutoInit Init engine automatically. If @False,
         cCHXSDL2Engine.Config properties can be changed and then
         cCHXSDL2Engine.Init must be called.)
    }
    constructor Create(const aTitle : string; const aIniFile : string;
      const AutoInit : Boolean = True); overload;
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
  const X, Y : Word; const r, g, b, a : Byte);
var
  PPoint : PCUInt32;
begin
  PPoint := Base + Y * (Pitch div 4) + X;
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

procedure cCHXSDL2Engine.TextInput(aFont : caCHXSDL2Font; var aText : string;
  const aX, aY : Integer; const aWidth : Integer; const UpdateLive : Boolean);
//var
//  aW : Integer;
begin
  //if aWidth < 8 then
  //  aW := SDLWindow.LogWidth - aX
  //else
  //  aW := aWidth;

  // Not sure about this, but seem that it defines a rect where IME /
  //   keyboard on-screen must not cover.
  // But we need a global SDL_Rect property.
  //SDL_SetTextInputRect(@SDLRect(aX,aY,aW, aFont.LineHeight));

  SDL_StartTextInput;

  CTIFont := aFont;
  CurrTextInput := aText;
  CTIX := aX;
  CTIY := aY;
  CTIWidth := aWidth;
  CTIStrVar := @aText;
  CTIUpdateLive := UpdateLive;
end;

function cCHXSDL2Engine.IsEditingText : Boolean;
begin
  Result := SDL_IsTextInputActive;
end;

procedure cCHXSDL2Engine.HandleEvent(const aEvent : TSDL_Event; var Handled,
  ExitProg : Boolean);
begin
  if Handled then
    Exit;

  // Some events are listed and commented out to have an easy reference.
  // Window and general quit events are handled automatically.
  // Escape key is mapped to exit the program.
  // F11 toggles framerate in window title.
  // When TextInput is active handles character keys automatically too,
  //   but SDL_KEYDOWN and SDL_KEYUP are sended too.

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
        SDLK_RETURN, SDLK_KP_ENTER :
          if SDL_IsTextInputActive then
          begin
            SDL_StopTextInput;
            if not CTIUpdateLive then
              CTIStrVar^ := CurrTextInput;
            Handled := True;
          end;
        else
          ;
      end;

    SDL_TEXTEDITING : // (edit: TSDL_TextEditingEvent);
    begin
      // This is called when a IME window is called (Win+.)
      if SDL_IsTextInputActive then
      begin
        CurrTextInput += aEvent.edit.Text;
        Handled := True;
      end;
    end;

    SDL_TEXTEDITING_EXT : // (exitExt: TSDL_TextEditingExtEvent);
    begin
      if SDL_IsTextInputActive then
      begin
        CurrTextInput += aEvent.exitExt.Text;
        // Freeing as TSDL_TextEditingExtEvent documentation says.
        //   I never triggered this.
        SDL_free(aEvent.exitExt.Text);
        Handled := True;
      end;
    end;
    SDL_TEXTINPUT : // (text: TSDL_TextInputEvent);
    begin
      if SDL_IsTextInputActive then
      begin
        CurrTextInput += aEvent.Text.Text;
        if CTIUpdateLive then
          CTIStrVar^ := CurrTextInput;
        Handled := True;
      end;
    end;
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

  if SDL_IsTextInputActive then
    SDL_StopTextInput;

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

  // Creating a default TTF font or default GFX font.
  if FileExists(Config.DefFontFile) and (Config.DefFontSize > 0) then
  begin
    try
      FDefFont := cCHXSDL2FontTTF.Create(SDLWindow.PRenderer,
        Config.DefFontFile,
        Config.DefFontSize, Config.DefFontColor)
    except
      // Fallback to SDL2_GFX
      FDefFont := cCHXSDL2FontGFX.Create(SDLWindow.PRenderer,
        Config.DefFontColor);
    end;
  end
  else
    FDefFont := cCHXSDL2FontGFX.Create(SDLWindow.PRenderer,
      Config.DefFontColor);
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
      Inc(FFrameCount);

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

        // Drawing current editing text
        if SDL_IsTextInputActive then
        begin
          CTIFont.RenderDynStr(CurrTextInput, CTIX,
            CTIY, CTIWidth);
        end;

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

constructor cCHXSDL2Engine.Create(const aTitle : string; const aWinWidth,
  aWinHeight : CInt; const HWAcc : Boolean; const AutoInit : Boolean);
begin
  inherited Create(nil);

  Title := aTitle;
  FShowFrameRate := False;

  FConfig := cCHXSDL2Config.Create(Self);
  Config.WindowWidth := aWinWidth;
  Config.RendererWidth := aWinWidth;
  Config.WindowHeight := aWinHeight;
  Config.RendererHeight := aWinHeight;
  Config.RendererUseHW := HWAcc;

  if AutoInit then
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
    Config.SaveToFile('', False);

  Config.Free;

  //This must be stopped
  if SDL_IsTextInputActive then
    SDL_StopTextInput;

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
