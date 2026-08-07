unit ucCHXSDL3Window;
{< Unit of cCHXSDL3Window class, a wrapper of SDL3 Window and Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, CTypes,
  ucCHXSDL3Renderer,
  SDL3;

type

  {
    Wrapper of SDL3 Window and creates its asociated SDL3 Renderer.

    Actual `PSDL_Window` and `PSDL_Renderer` are `PSDLWindow` and
    `PSDLRenderer` pointer properties. SDL native methods can called with
    these pointers, `SDL_Render[X](PSDLRenderer, [...]);`.

    `Renderer` property (`cCHXSDL3Render` class) wraps SDL3 Renderer and
    expands it with more primitives `Renderer.[X]([...])`.

    `cCHXSDL3Engine`, in its context, has the properties `Window` and `Render`
    as "shortcuts" for both classes; and `PSDLWindow` and `PSDLRenderer` for
    both pointers.

    Calls `SDL_InitSubSystem(SDL_INIT_VIDEO)` on creation and
    `SDL_QuitSubSystem(SDL_INIT_VIDEO)` on destruction as SDL3 keeps track of
    how many times are called each one.

    Supports creating multiple cCHXSDL3Window in the same program,
      checking its ID in event handling.

    ToDo: Create GPU renderer as option, choose driver and renderer.
  }

  cCHXSDL3Window = class
  private
    FTitle: String;
    FRenderWidth: CInt;
    FRenderHeight: CInt;
    FWindowWidth: CInt;
    FWindowHeight: CInt;
    FWindowID: CUInt32;
    FShown: Boolean;
    FMaximized: Boolean;
    FMinimized: Boolean;
    FMouseFocus: Boolean;
    FKeyboardFocus: Boolean;

    procedure SetTitle(const aValue: String);

  protected
    FullScreen: Boolean;

    procedure CreateWindow;
    procedure DestroyWindow;

  public
    Renderer: cCHXSDL3Renderer;
    //< CHX Renderer of the Window
    PSDLWindow: PSDL_Window;
    //< SDL Window pointer.
    PSDLRenderer: PSDL_Renderer;
    //< SDL Renderer pointer.

    property Title: String read FTitle write SetTitle;
    //< Title of the window.

    property RenderWidth: CInt read FRenderWidth;
    //< Width of render canvas.
    property RenderHeight: CInt read FRenderHeight;
    //< Height of render canvas.
    property WindowWidth: CInt read FWindowWidth;
    //< Actual Window Width.
    property WindowHeight: CInt read FWindowHeight;
    //< Actual Window Height.


    property WindowID: CUInt32 read FWindowID;
    //< ID of the Window

    property Shown: Boolean read FShown;
    property Maximized: Boolean read FMaximized;
    property Minimized: Boolean read FMinimized;
    property MouseFocus: Boolean read FMouseFocus;
    property KeyboardFocus: Boolean read FKeyboardFocus;

    constructor Create(const aTitle: String;
      const aRenderWidth, aRenderHeight: CInt; const aWinWidth: CInt = 0;
      const aWinHeight: CInt = 0);
    {<
      Creates a new SDL Window and its associated renderer.

      Render canvas size is automatically scaled to Window real size.

      @param aTitle Title of the window.
      @param aRenderWidth Logical width of Renderer.
      @param aRenderHeight Logical height of Renderer.
      @param aWinWidth Actual width of the Window.
      @param aWinHeight Actual height of the Window.
    }

    procedure Focus;
    //< Set the focus to this window.

    procedure HandleEvent(const aEvent: TSDL_Event; var Handled: Boolean);
    {<
      Procedure to handle some events (Resizing, Minimizing, etc.)
        - Only handle Window events, others are ignored.
        - If it's already handled (Handled = True), is ignored too.

      @param aEvent SDL Event to handle.
      @param(Handled @IN: Was it already handled?. @OUT: Is it handled by this
        function?);
    }

    destructor Destroy; override;
  end;

implementation

{ cCHXSDL3Window }

constructor cCHXSDL3Window.Create(const aTitle: String;
  const aRenderWidth, aRenderHeight, aWinWidth, aWinHeight: CInt);
begin
  Renderer := nil;
  PSDLWindow := nil;
  PSDLRenderer := nil;

  // Don't call SetTitle
  FTitle := aTitle;

  FRenderWidth := aRenderWidth;
  FRenderHeight := aRenderHeight;


  if aWinWidth <= 0 then
    FWindowWidth := FRenderWidth
  else
    FWindowWidth := aWinWidth;

  if aWinHeight <= 0 then
    FWindowHeight := FRenderHeight
  else
    FWindowHeight := aWinHeight;


  if not SDL_InitSubSystem(SDL_INIT_VIDEO) then
    raise Exception.CreateFmt('[ERROR] SDL_InitSubSystem: %s',
      [SDL_GetError]);

  CreateWindow;
end;

procedure cCHXSDL3Window.SetTitle(const aValue: String);
begin
  if FTitle = aValue then Exit;
  FTitle := aValue;

  SDL_SetWindowTitle(PSDLWindow, PChar(aValue));
end;

procedure cCHXSDL3Window.DestroyWindow;
begin
  FullScreen := False;
  FShown := False;
  FMinimized := False;
  FMaximized := False;
  FMouseFocus := False;
  FKeyboardFocus := False;

  FWindowID := 0;

  FreeAndNil(Renderer);

  if Assigned(PSDLWindow) then
  begin
    SDL_DestroyWindow(PSDLWindow);
    PSDLWindow := nil;
  end;
end;

procedure cCHXSDL3Window.CreateWindow;
var
  Flags: CUInt32;
begin
  DestroyWindow; // if already created and want to init new one

  Flags := SDL_WINDOW_RESIZABLE + SDL_WINDOW_HIGH_PIXEL_DENSITY;
  { SDL_WINDOW_TRANSPARENT: ¿Window with transparent buffer? }

  // ToDo: Create Window and renderer separated, to add config for both
  if not SDL_CreateWindowAndRenderer(PChar(Title), WindowWidth, WindowHeight,
    Flags, @PSDLWindow, @PSDLRenderer) then
    raise Exception.CreateFmt('[ERROR] SDL_CreateWindowAndRenderer: %s',
      [SDL_GetError]);

  // Renderer will destroy SDL Renderer
  // ToDo: Make Renderer Drivers configurables
  Renderer := cCHXSDL3Renderer.Create(PSDLRenderer, True);

  // ToDo: Make use of integer scale configurable:
  //   (SDL_LOGICAL_PRESENTATION_INTEGER_SCALE)
  SDL_SetRenderLogicalPresentation(PSDLRenderer, RenderWidth, RenderHeight,
    SDL_LOGICAL_PRESENTATION_LETTERBOX);

  FWindowID := SDL_GetWindowID(PSDLWindow);

  // Compute transparency by default.
  // ToDo: Change with a Renderer method
  SDL_SetRenderDrawBlendMode(PSDLRenderer, SDL_BLENDMODE_BLEND);
  // Initial default draw color.
  Renderer.SetDrawColor(0, 0, 0, 1);

  // Reading window flags
  Flags := SDL_GetWindowFlags(PSDLWindow);
  FMouseFocus := (Flags and SDL_WINDOW_MOUSE_FOCUS) = SDL_WINDOW_MOUSE_FOCUS;
  FKeyboardFocus :=
    (Flags and SDL_WINDOW_INPUT_FOCUS) = SDL_WINDOW_INPUT_FOCUS;
  FShown := ((not Flags) and SDL_WINDOW_HIDDEN) = SDL_WINDOW_HIDDEN;
end;

procedure cCHXSDL3Window.Focus;
begin
  if not Shown then
    SDL_ShowWindow(PSDLWindow);
  SDL_RaiseWindow(PSDLWindow);
end;

procedure cCHXSDL3Window.HandleEvent(const aEvent: TSDL_Event;
  var Handled: Boolean);
begin
  if Handled //< ¿Is it already handled?
    or (aEvent.type_ < SDL_EVENT_WINDOW_FIRST) //< ¿Is it a window event?
    or (aEvent.type_ > SDL_EVENT_WINDOW_LAST)
    or (aEvent.window.windowID <> WindowID) then //< ¿Is it for this window?
    Exit;

  case aEvent.window.type_ of
    SDL_EVENT_WINDOW_SHOWN: {< Window has been shown. }
    begin
      FShown := True;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_HIDDEN: {< Window has been hidden. }
    begin
      FShown := False;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_EXPOSED:
    {<
      Window has been exposed and should be redrawn, and can be redrawn directly
      from event watchers for this event. data1 is 1 for live-resize expose
      events, 0 otherwise.
    }
    begin
      SDL_RenderPresent(PSDLRenderer);
      Handled := True;
    end;

    SDL_EVENT_WINDOW_RESIZED: {< Window has been resized to data1xdata2. }
    begin
      FWindowWidth := aEvent.window.data1;
      FWindowHeight := aEvent.window.data2;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
    {< The pixel size of the window has changed to data1xdata2. }
    begin
      SDL_RenderPresent(PSDLRenderer);
      Handled := True;
    end;

    SDL_EVENT_WINDOW_METAL_VIEW_RESIZED:
    {< The pixel size of a Metal view associated with the window has changed. }
    begin
      SDL_RenderPresent(PSDLRenderer);
      Handled := True;
    end;

    SDL_EVENT_WINDOW_MINIMIZED: {< Window has been minimized. }
    begin
      FMinimized := True;
      FMaximized := False;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_MAXIMIZED: {< Window has been maximized. }
    begin
      FMinimized := False;
      FMaximized := True;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_RESTORED:
    {< Window has been restored to normal size and position. }
    begin
      FMinimized := False;
      FMaximized := False;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_MOUSE_ENTER: {< Window has gained mouse focus. }
    begin
      FMouseFocus := True;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_MOUSE_LEAVE: {< Window has lost mouse focus. }
    begin
      FMouseFocus := False;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_FOCUS_GAINED: {< Window has gained keyboard focus. }
    begin
      FKeyboardFocus := True;
      Handled := True;
    end;

    SDL_EVENT_WINDOW_FOCUS_LOST: {< Window has lost keyboard focus. }
    begin
      FKeyboardFocus := False;
      Handled := True;
    end;
(*
    SDL_EVENT_WINDOW_MOVED: {< Window has been moved to data1, data2. }

    SDL_EVENT_WINDOW_CLOSE_REQUESTED:
    {< The window manager requests that the window be closed. }

    SDL_EVENT_WINDOW_HIT_TEST:
    {< Window had a hit test that wasn't SDL_HITTEST_NORMAL. }

    SDL_EVENT_WINDOW_ICCPROF_CHANGED:
    {< The ICC profile of the window's display has changed. }

    SDL_EVENT_WINDOW_DISPLAY_CHANGED:
    {< Window has been moved to display data1. }

    SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED:
    {< Window display scale has been changed. }

    SDL_EVENT_WINDOW_SAFE_AREA_CHANGED:
    {< The window safe area has been changed. }

    SDL_EVENT_WINDOW_OCCLUDED: {< The window has been occluded. }

    SDL_EVENT_WINDOW_ENTER_FULLSCREEN:
    {< The window has entered fullscreen mode. }

    SDL_EVENT_WINDOW_LEAVE_FULLSCREEN: {< The window has left fullscreen mode. }

    SDL_EVENT_WINDOW_DESTROYED:
    {<
      The window with the associated ID is being or has been destroyed. If this
      message is being handled in an event watcher, the window handle is still
      valid and can still be used to retrieve any properties associated with
      the window. Otherwise, the handle has already been destroyed and all
      resources associated with it are invalid.
    }

    SDL_EVENT_WINDOW_HDR_STATE_CHANGED: {< Window HDR properties have changed. }
*)

  otherwise
    Handled := False;
  end;
end;

destructor cCHXSDL3Window.Destroy;
begin
  Renderer.Free;
  SDL_DestroyWindow(PSDLWindow);
  SDL_QuitSubSystem(SDL_INIT_VIDEO);

  inherited Destroy;
end;

end.
