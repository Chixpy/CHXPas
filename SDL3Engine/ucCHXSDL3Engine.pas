unit ucCHXSDL3Engine;
{< Unit of cSDL3Engine class.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Config, ucCHXSDL3Window;

type
  { cCHXSDL3Engine }

  cCHXSDL3Engine = class //(TPersistent)
  private // Propiedades que tienen Get o Set
    FShowFrameRate: Boolean;
    FWindow: cCHXSDL3Window;
//    FRender: cCHXSDL3Renderer;
    FFrameCount: QWord; // More than the age of the universe

  protected
    procedure SetShowFrameRate(const aValue: Boolean);
    procedure SetWindow(const aValue: cCHXSDL3Window);
//    procedure SetRender(const aValue: cCHXSDL3Renderer);

(*
  private // Gets and Sets
    FCompList: cSDL2CompList;
    Config: cCHXSDL3Config;
    FDefFont: caCHXSDL2Font;
    FPWinPxFmt: PSDL_PixelFormat;
    FWindow: cCHXSDL3Window;

  private // Properties
    {property} SDLFrameMang: TFPSManager;
    {property} FFrameCount: QWord; // More than the age of the universe

    {property} FocusedComp: caCHXSDL2Comp;

    // Simple Input Text properties
    {property} STIActive: Boolean;
    {property} STIFont: caCHXSDL2Font;
    {property} STIX: Integer;
    {property} STIY: Integer;
    {property} STIWidth: Integer;
    {property} STIStrVar: PString;
    {property} STIUpdateLive: Boolean;

    // Default values for component
    {property} DefCompBGColor: CUInt;
    //< Background color $AABBGGRR in Intel/Windows
    {property} DefCompBDColor: CUInt;
    //< Border color if not focused
    {property} DefCompHLColor: CUInt;
    //< Border color if focused
*)

    procedure SetDefaultValues;

  protected
    CurrTextInput: String;

    SDLRenderer: PSDL_Renderer; //< Window.PSDLRenderer shorcut.
    SDLWindow: PSDL_Window; //< Window.PSDLWindow shorcut.

    property Window: cCHXSDL3Window read FWindow write SetWindow;
//    property Render: cCHXSDL3Renderer read FRender write SetRender;

    function FrameCount: QWord; inline;
    {< Current frame number. ToDo: Create a FrameRate Manager}
(*
    property DefFont: caCHXSDL2Font read FDefFont;
    {< Default TTF font to use with the engine. A TTF file, size and color must
         be set in config file or manually before Init call.

       After font is loaded we can change font style with
         cCHXSDL2FontTTF.ChangeFontStyle, but this not for continuous calls
         because it will remove cached glyphs and texts.

       If not font is loaded, 8 bit ASCII from SDL2_GFX will be used. }

    property PWinPxFmt: PSDL_PixelFormat read FPWinPxFmt;
    //< Window pixel format for new textures.
    procedure PutPixel(const Base: PCUInt32; const Pitch: CInt;
      const X, Y: Word; const r, g, b, a: Byte);
    //< Put Pixel in a locked texture.


    procedure TextInput(aFont: caCHXSDL2Font; var aText: String;
      const aX, aY: Integer; const aWidth: Integer = 0;
      const UpdateLive: Boolean = False);
    {< Starts input text, so keyboard events of common keys will be disabled
      until Enter or another event stops it. }
    function IsEditingText: Boolean; inline;
    {< Are we currently editing text? }
*)
    // Abstract methods to implement in child classes.
    procedure Setup; virtual; abstract;
    procedure Compute(const FrameTime: CUInt32; var ExitProg: Boolean);
      virtual; abstract;
    procedure Draw; virtual; abstract;
    procedure HandleEvent(const aEvent: TSDL_Event;
      var Handled, ExitProg: Boolean); virtual;
    procedure Finish; virtual; abstract;


  public
    Title: String;
    Config: cCHXSDL3Config;

    property ShowFrameRate: Boolean
      read FShowFrameRate write SetShowFrameRate;


(*
    property CompList: cSDL2CompList read FCompList;

*)
    constructor Create(const aTitle: String;
      const aRenderWidth, aRenderHeight: CInt; const AutoInit: Boolean = True);
      overload;
    {< Simple constructor.

      @param(aTitle Title of the window.)
      @param(aRenderWidth, aRenderHeight Size of the window.)
      @param(AutoInit Init engine automatically. If @False,
         cCHXSDL3Engine.Config properties can be changed and then
         cCHXSDL3Engine.Init must be called.)
    }
    constructor Create(const aTitle: String; const aIniFile: String;
      const AutoInit: Boolean = True); overload;
    {< Constructor reading an .ini file with the settings.

       @param(aTitle Title of the window.)
       @param(aIniFile File with engine settings.)
       @param(AutoInit Init engine after reading the file. If @False,
         cCHXSDL3Engine.Config properties can be changed and then
         cCHXSDL3Engine.Init must be called.)
    }
    procedure Init;
    {< Init engine and window. }

    procedure Run;
    {< Run engine. }

(*
    function AddComponent(aComp: caCHXSDL2Comp): caCHXSDL2Comp;
*)

    destructor Destroy; override;
  end;

implementation

{ cCHXSDL3Engine }
procedure cCHXSDL3Engine.SetShowFrameRate(const aValue: Boolean);
begin
  if FShowFrameRate = aValue then Exit;
  FShowFrameRate := aValue;

  if Assigned(Window) then
    Window.Title := Self.Title;
end;

procedure cCHXSDL3Engine.SetWindow(const aValue: cCHXSDL3Window);
begin
  if FWindow = aValue then Exit;
  FWindow := aValue;

  SDLWindow := Window.PSDLWindow;
  SDLRenderer := Window.PSDLRenderer;
//  Render := Window.Render; // Sets SDLRender too.
end;

(*
procedure cCHXSDL3Engine.SetRender(const aValue: cCHXSDL3Renderer);
begin
  if FRender = aValue then Exit;
  FRender := aValue;

  SDLRender := Render.PSDLRenderer;
end;
*)

procedure cCHXSDL3Engine.SetDefaultValues;
begin
(*
  FCompList := cSDL2CompList.Create(True);
*)
  FShowFrameRate := False;
(*
  DefCompBGColor := $FF404040; //< Background color
  DefCompBDColor := $FF808080; //< Border color if not focused
  DefCompHLColor := $FF00FFFF; //< Border color if focused
*)
end;

(*
procedure cCHXSDL3Engine.PutPixel(const Base: PCUInt32; const Pitch: CInt;
  const X, Y: Word; const r, g, b, a: Byte);
var
  PPoint: PCUInt32;
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
*)

function cCHXSDL3Engine.FrameCount: QWord;
begin
  // This is reseted every time a frame is late, for example moving,
  //   resizing, focus a window, etc.
  //Result := SDL_getFramecount(@SDLFrameMang);
  Result := FFrameCount;
end;

(*
procedure cCHXSDL3Engine.TextInput(aFont: caCHXSDL2Font; var aText: String;
  const aX, aY: Integer; const aWidth: Integer; const UpdateLive: Boolean);
//var
//  aW: Integer;
begin
  //if aWidth < 8 then
  //  aW := Window.LogWidth - aX
  //else
  //  aW := aWidth;

  // Not sure about this, but seem that it defines a rect where IME /
  //   keyboard on-screen must not cover.
  // But we need a global SDL_Rect property.
  //SDL_SetTextInputRect(@SDLRect(aX,aY,aW, aFont.LineHeight));

  STIActive := True;
  STIFont := aFont;
  CurrTextInput := aText;
  STIX := aX;
  STIY := aY;
  STIWidth := aWidth;
  STIStrVar := @aText;
  STIUpdateLive := UpdateLive;

  SDL_StartTextInput;
end;

function cCHXSDL3Engine.IsEditingText: Boolean;
begin
  Result := STIActive and SDL_IsTextInputActive;
end;
*)

procedure cCHXSDL3Engine.HandleEvent(const aEvent: TSDL_Event; var Handled,
  ExitProg: Boolean);
begin
  if Handled then
    Exit;

  // Some events are listed and commented out to have an easy reference.
  // - Window and general quit events are handled automatically.
  // - ESC: Exits the program.
  // - F11: Toggles framerate.
  // When TextInput is active handles character keys automatically too,
  //   but SDL_KEYDOWN and SDL_KEYUP are sended too, so all keys are
  //   handled.

  case aEvent.type_ of

    // Handled in Run method
    // SDL_WINDOWEVENT: // (window: TSDL_WindowEvent)

    //SDL_KEYUP: // (key: TSDL_KeyboardEvent)
    SDL_EVENT_KEY_DOWN: // (key: TSDL_KeyboardEvent)
    begin
(*
      if STIActive and SDL_IsTextInputActive then
      begin
        case aEvent.key.keysym.sym of
          // Special keys while editing text.
          SDLK_BACKSPACE, SDLK_DELETE :
          begin
            UTF8Delete(CurrTextInput, UTF8Length(CurrTextInput), 1);
            Handled := True;
          end;

          SDLK_RETURN, SDLK_KP_ENTER :
          begin
            SDL_StopTextInput;
            STIActive := False;
            if not STIUpdateLive then
              STIStrVar^ := CurrTextInput;
            // Handled := True; Pass to parent handler
          end;

          { TODO: CTRL+X, CTRL+C, CTRL+V...

          }

          SDLK_CUT :
          begin
            SDL_SetClipboardText(PAnsiChar(CurrTextInput));
            CurrTextInput := '';
            Handled := True;
          end;

          SDLK_COPY :
          begin
            SDL_SetClipboardText(PAnsiChar(CurrTextInput));
            Handled := True;
          end;

          SDLK_PASTE :
          begin
            if SDL_HasClipboardText then
            begin
              CurrTextInput += SDL_GetClipboardText;
              Handled := True;
            end;
          end;

          // Keys
          SDLK_SPACE, SDLK_EXCLAIM, SDLK_QUOTEDBL, SDLK_HASH,
          SDLK_PERCENT, SDLK_DOLLAR, SDLK_AMPERSAND, SDLK_QUOTE,
          SDLK_LEFTPAREN, SDLK_RIGHTPAREN, SDLK_ASTERISK, SDLK_PLUS,
          SDLK_COMMA, SDLK_MINUS, SDLK_PERIOD, SDLK_SLASH, SDLK_0, SDLK_1,
          SDLK_2, SDLK_3, SDLK_4, SDLK_5, SDLK_6, SDLK_7, SDLK_8, SDLK_9,
          SDLK_COLON, SDLK_SEMICOLON, SDLK_LESS, SDLK_EQUALS, SDLK_GREATER,
          SDLK_QUESTION, SDLK_AT, SDLK_LEFTBRACKET, SDLK_BACKSLASH,
          SDLK_RIGHTBRACKET, SDLK_CARET, SDLK_UNDERSCORE, SDLK_BACKQUOTE,
          SDLK_a, SDLK_b, SDLK_c, SDLK_d, SDLK_e, SDLK_f, SDLK_g, SDLK_h,
          SDLK_i, SDLK_j, SDLK_k, SDLK_l, SDLK_m, SDLK_n, SDLK_o, SDLK_p,
          SDLK_q, SDLK_r, SDLK_s, SDLK_t, SDLK_u, SDLK_v, SDLK_w, SDLK_x,
          SDLK_y, SDLK_z, SDLK_KP_DIVIDE, SDLK_KP_MULTIPLY, SDLK_KP_MINUS,
          SDLK_KP_PLUS, SDLK_KP_1, SDLK_KP_2, SDLK_KP_3,
          SDLK_KP_4, SDLK_KP_5, SDLK_KP_6, SDLK_KP_7, SDLK_KP_8, SDLK_KP_9,
          SDLK_KP_0, SDLK_KP_PERIOD :
            Handled := True
          else
            Handled := False;
        end;
      end;
*)
      if not Handled then
      begin
        case aEvent.key.key of
          SDLK_F11 :
          begin
            ShowFrameRate := not ShowFrameRate;
            Handled := True;
          end;

          SDLK_ESCAPE :
          begin
            ExitProg := True; // Exit
            Handled := True;
          end;
          else
            ;
        end;
      end;
    end;
(*
    SDL_TEXTEDITING: // (edit: TSDL_TextEditingEvent)
    begin
      // This is called when a IME window is called (Win+.)
      if STIActive and SDL_IsTextInputActive then
      begin
        CurrTextInput += aEvent.edit.Text;
        Handled := True;
      end;
    end;
    SDL_TEXTEDITING_EXT: // (exitExt: TSDL_TextEditingExtEvent)
    begin
      if STIActive and SDL_IsTextInputActive then
      begin
        CurrTextInput += aEvent.exitExt.Text;
        // Freeing as TSDL_TextEditingExtEvent documentation says.
        //   I never triggered this.
        SDL_free(aEvent.exitExt.Text);
        Handled := True;
      end;
    end;
    SDL_TEXTINPUT: // (text: TSDL_TextInputEvent)
    begin
      if STIActive and SDL_IsTextInputActive then
      begin
        CurrTextInput += aEvent.Text.Text;
        if STIUpdateLive then
          STIStrVar^ := CurrTextInput;
        Handled := True;
      end;
    end;
    //SDL_MOUSEMOTION: // (motion: TSDL_MouseMotionEvent)
    //SDL_MOUSEBUTTONUP: // (button: TSDL_MouseButtonEvent)
    //SDL_MOUSEBUTTONDOWN: // (button: TSDL_MouseButtonEvent)
    //SDL_MOUSEWHEEL: // (wheel: TSDL_MouseWheelEvent)
*)
    SDL_EVENT_QUIT: // General exit event
    begin
      ExitProg := True;
      Handled := True;
    end;
    otherwise
      ;
  end;
end;

procedure cCHXSDL3Engine.Init;
var
  aWindow: cCHXSDL3Window;
begin
(*
  if SDL_IsTextInputActive then
    SDL_StopTextInput;
*)

//  SDL_FreeFormat(FPWinPxFmt);
  FreeAndNil(FWindow);
//  FreeAndNil(FDefFont);

  aWindow := cCHXSDL3Window.Create(Title, Config.RendererWidth,
    Config.RendererHeight, Config.WindowWidth, Config.WindowHeight);

  Window := aWindow; // Sets SDLRenderer and SDLWindow

(*
  // Pixel format of the window
  FPWinPxFmt := SDL_AllocFormat(SDL_GetWindowPixelFormat(SDLWindow.PWindow));

  // Creating a default TTF font.
  if FileExists(Config.DefFontFile) and (Config.DefFontSize > 0) then
    FDefFont := cCHXSDL2FontTTF.Create(SDLRenderer, Config.DefFontFile,
      Config.DefFontSize, Config.DefFontColor);
  // Fallback to SDL2_GFX
  if not Assigned(DefFont) then
    FDefFont := cCHXSDL2FontGFX.Create(SDLRenderer, Config.DefFontColor);

  STIActive := False;
*)
end;

procedure cCHXSDL3Engine.Run;
var
  ProgExit, HandledEvent: Boolean;
  CompTime, LastFrameTime: CUInt32;
  aEvent: TSDL_Event;
  CursorX, i: Integer;
//  aComp: caCHXSDL2Comp;
begin
  ProgExit := False;
(*
  SDL_InitFramerate(@SDLFrameMang);
  SDL_SetFramerate(@SDLFrameMang, 60);
*)
  LastFrameTime := 0;
  CompTime := 0;
  FFrameCount := 0;

  try
    Self.Setup;
(*
    for aComp in CompList do
      aComp.Setup;
*)
    while (not ProgExit) do
    begin
      Inc(FFrameCount);

      // COMPUTE
      Self.Compute(LastFrameTime, ProgExit);
(*
      for aComp in CompList do
        if (not ProgExit) then
          aComp.Compute(LastFrameTime, ProgExit);
*)
      // TIMING (1)
      // Actual Compute + Events time in milliseconds.
      CompTime := SDL_GetTicks - CompTime;
      // Frame rate in milliseconds. Compute + Event + Draw + Delay
//      LastFrameTime := SDL_framerateDelay(@SDLFrameMang);
      LastFrameTime := 300;
      SDL_Delay(1000);

      // Don't draw if minimized
      if (not ProgExit) and (not Window.Minimized) then
      begin
        // DRAW
        Draw;
(*
        for aComp in CompList do
          aComp.Draw;
*)
        if ShowFrameRate and
          ((FrameCount and 31) = 0) then
          Window.Title := Format('%0:s: %1:d ms (%2:d ms)',
            [Title, LastFrameTime, CompTime]);

(*
        // Drawing current editing text
        if STIActive and SDL_IsTextInputActive then
        begin
          CursorX := STIFont.RenderDynStrClipped(CurrTextInput, STIX, STIY,
            STIWidth);

          // Drawing cursor
          if (FrameCount and 32) = 32 then
            vlineRGBA(SDLRenderer, STIX + CursorX,
              STIY, STIY + STIFont.LineHeight, STIFont.Color.r,
              STIFont.Color.g, STIFont.Color.b, STIFont.Color.a);
        end;
*)
        // UPDATE RENDER
        SDL_RenderPresent(SDLRenderer);
      end;

      // TIMING (2)
      CompTime := SDL_GetTicks;

      // EVENTS
      // SDL_PumpEvents;

      while (not ProgExit) and SDL_PollEvent(@aEvent) do
      begin
        
        HandledEvent := False; // Used to see if a event is Handled

        // First: Window events
        case aEvent.type_ of
          SDL_EVENT_WINDOW_EXPOSED, SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED,
          SDL_EVENT_WINDOW_METAL_VIEW_RESIZED : HandledEvent := True;
          //< Not needed in cCHXSDL3Engine context
        otherwise
          Window.HandleEvent(aEvent, HandledEvent);
        end;

(*
        // Second: Pass event to current component
        if assigned(FocusedComp) then
          FocusedComp.HandleEvent(aEvent, HandledEvent, ProgExit);

        // Third: Pass to all components
        for aComp in CompList do
          aComp.HandleEvent(aEvent, HandledEvent, ProgExit);
*)
        // Fourth: Fallback to engine
        Self.HandleEvent(aEvent, HandledEvent, ProgExit);

(* // Esto... ¿Dentro o fuera de loop de eventos?
        // Getting current focused component
        FocusedComp := nil;
        i := 0;
        while (not assigned(FocusedComp)) and (i < CompList.Count) do
        begin
          if CompList[i].Focused then
            FocusedComp := CompList[i];
          Inc(i);
        end;
*)
      end;
    end;

  finally
    Finish;
  end;
end;

(*
function cCHXSDL3Engine.AddComponent(aComp: caCHXSDL2Comp): caCHXSDL2Comp;
begin
  Result := aComp;
  if not assigned(aComp) then Exit;

  aComp.PRenderer := SDLRenderer;
  aComp.BGColor := DefCompBGColor;
  aComp.BDColor := DefCompBDColor;
  aComp.HLColor := DefCompHLColor;

  aComp.UnSetFocus;

  CompList.Add(aComp);
end;
*)

constructor cCHXSDL3Engine.Create(const aTitle: String; const aRenderWidth,
  aRenderHeight: CInt; const AutoInit: Boolean);
begin
  inherited Create;

  SetDefaultValues;

  Title := aTitle;

  Config := cCHXSDL3Config.Create;
  Config.WindowWidth := aRenderWidth;
  Config.RendererWidth := aRenderWidth;
  Config.WindowHeight := aRenderHeight;
  Config.RendererHeight := aRenderHeight;

  if AutoInit then
    Init;
end;

constructor cCHXSDL3Engine.Create(const aTitle: String;
  const aIniFile: String; const AutoInit: Boolean);
begin
  inherited Create;

  SetDefaultValues;

  Title := aTitle;

  Config := cCHXSDL3Config.Create;
  Config.DefaultFileName := aIniFile;
  Config.LoadFromFile('');

  if AutoInit then
    Init
  else
    // if Config will be changed manually then no save changes
    Config.DefaultFileName := '';
end;


destructor cCHXSDL3Engine.Destroy;
begin
(*
  CompList.Free;

  // Saving normal window size
  if (not Window.Maximized) then
  begin
    Config.WindowWidth := Window.WinWidth;
    Config.WindowHeight := Window.WinHeight;
    Config.RendererWidth := Window.LogWidth;
    Config.RendererHeight := Window.LogHeight;
  end;

  if Config.DefaultFileName <> '' then
    Config.SaveToFile('', False);
*)
  Config.Free;
(*
  // This must be stopped?
  if SDL_IsTextInputActive then
    SDL_StopTextInput;

  SDL_FreeFormat(FPWinPxFmt);
  FreeAndNil(FDefFont);
*)
  FreeAndNil(FWindow);
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
