unit ucCHXSDL3Engine;
{< Unit of `cSDL3Engine` class.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, CTypes,
  SDL3,
  ucCHXSDL3Config, ucCHXSDL3Renderer, ucCHXSDL3Window, ucCHXSDL3FPSManager;

type
  {
    `cCHXSDL3Engine`: A basic Game Engine in SDL3.

    Descendants only need to implement abtract or virtual methods:

    - `Setup`: Code of inicialization before enter the loop.
    - `Compute`: Code for every frame.
    - `Draw`: Code for draw on screen every frame.
    - `HandleEvent`: Code handling events. Keyboard, Mouse, 
    - `Finish`: Code after exiting the loop.

    New features are implemented as needed.

    - `cCHXSDL3Config` (property `Config`): Lets load and save configurations
      from a file, or change the configuration before initializing and running
      the engine.
    - `cCHXSDL3Window` (property `Window`): Wraps _SDL_video.h_ and handles
      some window events.
    - `cCHXSDL3Renderer` (property `Render`): Wrap _SDL_render.h_ primitives
      and adds many more.
    - `cCHXSDL3FPSManager` (property `FPSMng`): Controls frames rate,
      time lapsed between frames and frame count.

    Of course, SDL3 native functions can be used: `PSDLRenderer` and
    `PSDLWindow` are the pointers to SDL3 native structures.


  }
  cCHXSDL3Engine = class //(TPersistent)
  private // Propiedades que tienen Get o Set
    FShowFrameRate: Boolean;
    FWindow: cCHXSDL3Window;
    FRender: cCHXSDL3Renderer;

  protected
    procedure SetShowFrameRate(const aValue: Boolean);
    procedure SetWindow(const aValue: cCHXSDL3Window);
    procedure SetRender(const aValue: cCHXSDL3Renderer);

(*
  private // Gets and Sets
    FCompList: cSDL2CompList;
    FDefFont: caCHXSDL2Font;
    FPWinPxFmt: PSDL_PixelFormat;

  private // Properties

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

    FPSMng: cCHXSDL3FPSManager; //< FPS manager

    property Window: cCHXSDL3Window read FWindow write SetWindow;
    property Render: cCHXSDL3Renderer read FRender write SetRender;

   
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
    procedure Compute(var ExitProg: Boolean);
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

  if AutoInit then
  begin
    Config.DefaultFileName := aIniFile;
    Config.LoadFromFile('');
    Init
  end
  else
  begin
    Config.LoadFromFile(aIniFile);
    // if Config is be changed manually then no save changes
    Config.DefaultFileName := '';
  end;
end;

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
  // SDLRenderer := Window.PSDLRenderer;
  Render := Window.Renderer; // Sets SDLRender too.
end;

procedure cCHXSDL3Engine.SetRender(const aValue: cCHXSDL3Renderer);
begin
  if FRender = aValue then Exit;
  FRender := aValue;

  SDLRenderer := Render.SDLRenderer;
end;

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

          // Keys // Use ranges...
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
          SDLK_F10:
          begin
            if FPSMng.FPS > 5 then
               FPSMng.FPS := FPSMng.FPS - 5;
            Handled := True;
          end;

          SDLK_F11:
          begin
            ShowFrameRate := not ShowFrameRate;
            Handled := True;
          end;

          SDLK_F12:
          begin
            FPSMng.FPS := FPSMng.FPS + 5;
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
  aEvent: TSDL_Event;
  CursorX, i: Integer;
//  aComp: caCHXSDL2Comp;
begin
  ProgExit := False;
  FPSMng := cCHXSDL3FPSManager.Create(30);
  {<
    ToDo: Make FPS configurable with Config.
      FPSMng.FPS can be changed in Setup, Compute, Draw and HandleEvent.
  }

  try
    Self.Setup;
(*
    for aComp in CompList do
      aComp.Setup;
*)
    while (not ProgExit) do
    begin
      // COMPUTE
      Self.Compute(ProgExit);
(*
      for aComp in CompList do
        if (not ProgExit) then
          aComp.Compute(LastFrameTime, ProgExit);
*)

      // Wait to next frame. Result not needed.
      FPSMng.Delay;

      // Don't draw if minimized
      if (not ProgExit) and (not Window.Minimized) then
      begin
        // DRAW
        Draw;
(*
        for aComp in CompList do
          aComp.Draw;
*)
        if ShowFrameRate
         // and ((FPSMng.FrameCount and 31) = 0)
          then
        begin
          Render.SetDrawColor(1, 0, 1, 1);
          SDL_RenderDebugTextFormat(SDLRenderer, 0, 0,
          '%dms (%dms)', [FPSMng.LastFrameTime, FPSMng.LastCompTime]);
          
          // Window.Title := Format('%0:s: %1:d ms (%2:d ms)',
          //   [Title, FPSMng.LastFrameTime, FPSMng.LastCompTime]);
        end;
(*
        // Drawing current editing text
        if STIActive and SDL_IsTextInputActive then
        begin
          CursorX := STIFont.RenderDynStrClipped(CurrTextInput, STIX, STIY,
            STIWidth);

          // Drawing cursor
          if (FPSMng.FrameCount and 32) = 32 then
            vlineRGBA(SDLRenderer, STIX + CursorX,
              STIY, STIY + STIFont.LineHeight, STIFont.Color.r,
              STIFont.Color.g, STIFont.Color.b, STIFont.Color.a);
        end;
*)
        // UPDATE RENDER
        // ToDo: Use CHX Render
        SDL_RenderPresent(SDLRenderer);
      end;

      // EVENTS
      // SDL_PumpEvents;

      while (not ProgExit) and SDL_PollEvent(@aEvent) do
      begin
        HandledEvent := False; // Used to see if a event is Handled

        // First: Window events
        case aEvent.type_ of
          SDL_EVENT_WINDOW_EXPOSED, SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED,
          SDL_EVENT_WINDOW_METAL_VIEW_RESIZED : HandledEvent := True;
          //< Not needed in cCHXSDL3Engine context.
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
        HandleEvent(aEvent, HandledEvent, ProgExit);

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
    FPSMng.Free;
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

destructor cCHXSDL3Engine.Destroy;
begin
(*
  CompList.Free;
*)

  // Saving normal window size
  if (not Window.Maximized) then
  begin
    Config.WindowWidth := Window.WindowWidth;
    Config.WindowHeight := Window.WindowHeight;
    Config.RendererWidth := Window.RenderWidth;
    Config.RendererHeight := Window.RenderHeight;
  end;

  if Config.DefaultFileName <> '' then
    Config.SaveToFile('', False);
  Config.Free;
  
(*
  // This must be stopped?
  if SDL_IsTextInputActive then
    SDL_StopTextInput;

  SDL_FreeFormat(FPWinPxFmt);
  FreeAndNil(FDefFont);
*)
  Window.Free; // Frees Render too

  // Little SDL error and leak test.
  if SDL_GetError <> '' then
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, SDL_GetError);
  if SDL_GetNumAllocations >= 0 then
     SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
       'Mem allocations not freed: %d', [SDL_GetNumAllocations]); 

  SDL_Quit;

  inherited;
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
