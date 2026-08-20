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
    these pointers, `SDL_[X](PSDLRenderer, [...]);` or
    `SDL_[X](PSDLWindow, [...]);`.

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

    ToDo: Choose driver of the renderer.
  }

  cCHXSDL3Window = class
  private
    FTitle: String;
    FWidth: CInt; //< Renderer Target Logical Width.
    FHeight: CInt; //< Renderer Target Logical Height.
    FWindowWidth: CInt; //< Actual Window Width.
    FWindowHeight: CInt; //< Actual Window Height.
    FWindowID: CUInt32;
    FFullScreen: Boolean;
    FShown: Boolean;
    FMaximized: Boolean;
    FMinimized: Boolean;
    FMouseFocus: Boolean;
    FKeyboardFocus: Boolean;

  public // Public setters, why not?
    procedure SetTitle(const aValue: String);
    procedure SetFullScreen(const aValue: Boolean);

  public
    Renderer: cCHXSDL3Renderer;
    //< CHX Renderer of the Window
    PSDLWindow: PSDL_Window;
    //< SDL Window pointer.
    PSDLRenderer: PSDL_Renderer;
    //< SDL Renderer pointer.

    property Title: String read FTitle write SetTitle;
    //< Title of the window.

    property Width: CInt read FWidth;
    //< Width of render canvas.
    property Height: CInt read FHeight;
    //< Height of render canvas.
    property WindowWidth: CInt read FWindowWidth;
    //< Actual Window Width.
    property WindowHeight: CInt read FWindowHeight;
    //< Actual Window Height.

    property WindowID: CUInt32 read FWindowID;
    //< ID of the Window

    property FullScreen: Boolean read FFullScreen write SetFullScreen;
    property Shown: Boolean read FShown;
    property Maximized: Boolean read FMaximized;
    property Minimized: Boolean read FMinimized;
    property MouseFocus: Boolean read FMouseFocus;
    property KeyboardFocus: Boolean read FKeyboardFocus;

    constructor Create(const aTitle: String;
      const aWidth: CInt = 0; const aHeight: CInt = 0;
      Scale: CInt = 0; const aFullScreen: Boolean = False;
      const aUseGPU: Boolean = False);
    {< Create a new SDL Window and its associated renderer.

      Render canvas size is automatically scaled to Window actual size.

      @param aTitle Title of the window.
      @param aWidth Logical width of Renderer.
      @param aHeight Logical height of Renderer.
      @param(Scale Scale window size. `0 = Maximized window.`
      @param aFullScreen Create full screen window.
      @param(aUseGPU Use a GPU renderer. As CHXSDL2Engine, in my tests, GPU
        renderer is **2 times slower** than software one in SDL3 too.) 
    }

    procedure Focus;
    //< Set the focus to this window.

    procedure HandleEvent(const aEvent: TSDL_Event; var Handled: Boolean);
    {< Procedure to handle some events (Resizing, Minimizing, etc.)
        - Only handle Window events, others are ignored.
        - If it's already handled (Handled = True), is ignored too.

      @param aEvent SDL Event to handle.
      @param(Handled @IN: Was it already handled?. @OUT: Is it handled by this
        function?);
    }

    procedure SetRenderSize(aWidth, aHeight: Integer;
      const Mode: TSDL_RendererLogicalPresentation
      = SDL_LOGICAL_PRESENTATION_LETTERBOX);
    {< Change render canvas size (_Logical Size_).

      It can be changed at any time to render at different resolutions.

      If no native `SDL_SetRenderLogicalPresentation` is used, current render
      size can be retrieved directly with `Width` and `Height`
      instead `SDL_GetRenderLogicalPresentation`.

      @param(Width Logical width for the renderer. `<= 0` means current window
        width.)
      @param(Height Logical height for de renderer. `<= 0` means current window
        height.)
      @param(Mode Mode for mapping logical resolution to actual window
        size. Predefined ones are (ToDo: Make shorter alias...):

        - `SDL_LOGICAL_PRESENTATION_DISABLED` (0): Disable logical size.
          (ToDo: Not sure if restores logical presentation to window size.)
        - `SDL_LOGICAL_PRESENTATION_STRETCH` (1): Stretched to the output
          resolution.
        - `SDL_LOGICAL_PRESENTATION_LETTERBOX` (2): Fit to the largest
          dimension and the other dimension is letterboxed with the clear
          color.
        - `SDL_LOGICAL_PRESENTATION_OVERSCAN` (3): Fit to the smallest
          dimension and the other dimension extends beyond the output bounds.
        - `SDL_LOGICAL_PRESENTATION_INTEGER_SCALE` (4): Scaled up by integer
          multiples to fit the output resolution.
      )
    }

    destructor Destroy; override;
  end;

implementation

{ cCHXSDL3Window }

constructor cCHXSDL3Window.Create(const aTitle: String;
  const aWidth, aHeight: CInt; Scale: CInt;
  const aFullScreen, aUseGPU: Boolean);
var
  Flags: TSDL_WindowFlags;
  Maximize: Boolean;
begin
  if not SDL_InitSubSystem(SDL_INIT_VIDEO) then
    raise Exception.CreateFmt('[ERROR] SDL_InitSubSystem: %s',
      [SDL_GetError]);

  FTitle := aTitle; // Don't call SetTitle
  FWidth := aWidth;
  FHeight := aHeight;
  FFullScreen := aFullScreen;

  // Resizable and don't show until correct size is set.
  Flags := SDL_WINDOW_RESIZABLE + SDL_WINDOW_HIDDEN;

  PSDLWindow := SDL_CreateWindow(PAnsiChar(Title), 200, 200, Flags);
  if not Assigned(PSDLWindow) then
    raise Exception.CreateFmt('[ERROR] SDL_CreateWindow: %s', [SDL_GetError]);

  // If we want to make truly sizeable in some systems, set it after
  // creation too.
  SDL_SetWindowResizable(PSDLWindow, True);

  // Getting window and renderer size
  Maximize := (Scale <= 0) or (Width <= 0) or (Height <= 0);
  if not Maximize then
  begin
    FWindowWidth := Width * Scale;
    FWindowHeight := Height * Scale;
    SDL_SetWindowSize(PSDLWindow, FWindowWidth, FWindowHeight);
    SDL_SetWindowPosition(PSDLWindow, SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED);
  end;

// writeln(WindowWidth.ToString + 'x' + WindowHeight.ToString);
// writeln(Width.ToString + 'x' + Height.ToString + '(x' + Scale.ToString + ')');

  SDL_ShowWindow(PSDLWindow);

  if FullScreen or Maximize then
  begin
    // ToDo: This work?
    if Maximize then
      SDL_MaximizeWindow(PSDLWindow);

    if FullScreen then // Full screen
      SDL_SetWindowFullscreen(PSDLWindow, True);

    // Assure that window is update...
    SDL_SyncWindow(PSDLWindow);

    // if Maximize and FullScreen, FullScreen size prevails...
    // ToDo: Must be client size?
    SDL_GetWindowSizeInPixels(PSDLWindow, @FWindowWidth, @FWindowHeight);

// writeln(WindowWidth.ToString + 'x' + WindowHeight.ToString);
// writeln(Width.ToString + 'x' + Height.ToString + '(x' + Scale.ToString + ')');

    if Scale < 1 then
      Scale := 1;

    if (Width <= 0) or (Height <= 0) then
    begin
      FWidth := FWindowWidth div Scale;
      FHeight := FWindowHeight div Scale;
    end;

// writeln(WindowWidth.ToString + 'x' + WindowHeight.ToString);
// writeln(Width.ToString + 'x' + Height.ToString + '(x' + Scale.ToString + ')');

  end;

  FWindowID := SDL_GetWindowID(PSDLWindow);

  // ToDo: Make Renderer Drivers configurables
  if aUseGPU then
    PSDLRenderer := SDL_CreateGPURenderer(nil, PSDLWindow)
  else
    PSDLRenderer := SDL_CreateRenderer(PSDLWindow, nil);
  if not Assigned(PSDLRenderer) then
    raise Exception.CreateFmt('[ERROR] SDL_Create{GPU}Renderer: %s',
      [SDL_GetError]);

  // Renderer will destroy SDL Renderer
  Renderer := cCHXSDL3Renderer.Create(PSDLRenderer, True);

  // ToDo: Make use of integer scale configurable:
  //   (SDL_LOGICAL_PRESENTATION_INTEGER_SCALE)
  SetRenderSize(Width, Height, SDL_LOGICAL_PRESENTATION_LETTERBOX);

  // Initial clear default draw color.
  Renderer.SetDrawColor(0, 0, 0, 1);
  Renderer.Clear;
  Renderer.SetDrawColor(1, 1, 1, 1);

  // Reading window flags to set properties
  Flags := SDL_GetWindowFlags(PSDLWindow);
  FMouseFocus := (Flags and SDL_WINDOW_MOUSE_FOCUS) = SDL_WINDOW_MOUSE_FOCUS;
  FKeyboardFocus :=
    (Flags and SDL_WINDOW_INPUT_FOCUS) = SDL_WINDOW_INPUT_FOCUS;
  FShown := ((not Flags) and SDL_WINDOW_HIDDEN) = SDL_WINDOW_HIDDEN;

  SDL_Log('== %s ==', [PAnsiChar(Title)]);
  SDL_Log('Window  (%s): %dx%d',
    [PAnsiChar(Title), WindowWidth, WindowHeight]);
  SDL_Log('Renderer (GPU %d): %dx%d (x%d)', [aUseGPU, Width, Height, Scale]);
end;

procedure cCHXSDL3Window.SetTitle(const aValue: String);
begin
  if FTitle = aValue then Exit;
  FTitle := aValue;

  SDL_SetWindowTitle(PSDLWindow, PChar(aValue));
end;

procedure cCHXSDL3Window.SetFullScreen(const aValue: Boolean);
begin
  if FFullScreen = aValue then Exit;
  FFullScreen := aValue;

  SDL_SetWindowFullscreen(PSDLWindow, FullScreen);
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

  Handled := True;
  case aEvent.window.type_ of
  SDL_EVENT_WINDOW_SHOWN: {< Window has been shown. }
    FShown := True;

  SDL_EVENT_WINDOW_HIDDEN: {< Window has been hidden. }
    FShown := False;

  SDL_EVENT_WINDOW_EXPOSED:
  {<
    Window has been exposed and should be redrawn, and can be redrawn directly
    from event watchers for this event. data1 is 1 for live-resize expose
    events, 0 otherwise.
  }
    SDL_RenderPresent(PSDLRenderer);

  SDL_EVENT_WINDOW_RESIZED: {< Window has been resized to data1xdata2. }
  begin
    FWindowWidth := aEvent.window.data1;
    FWindowHeight := aEvent.window.data2;
  end;

  SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
  {< The pixel size of the window has changed to data1xdata2. }
    SDL_RenderPresent(PSDLRenderer);

  SDL_EVENT_WINDOW_METAL_VIEW_RESIZED:
  {< The pixel size of a Metal view associated with the window has changed. }
    SDL_RenderPresent(PSDLRenderer);

  SDL_EVENT_WINDOW_MINIMIZED: {< Window has been minimized. }
  begin
    FMinimized := True;
    FMaximized := False;
  end;

  SDL_EVENT_WINDOW_MAXIMIZED: {< Window has been maximized. }
  begin
    FMinimized := False;
    FMaximized := True;
  end;

  SDL_EVENT_WINDOW_RESTORED:
  {< Window has been restored to normal size and position. }
  begin
    FMinimized := False;
    FMaximized := False;
  end;

  SDL_EVENT_WINDOW_MOUSE_ENTER: {< Window has gained mouse focus. }
    FMouseFocus := True;

  SDL_EVENT_WINDOW_MOUSE_LEAVE: {< Window has lost mouse focus. }
    FMouseFocus := False;

  SDL_EVENT_WINDOW_FOCUS_GAINED: {< Window has gained keyboard focus. }
    FKeyboardFocus := True;

  SDL_EVENT_WINDOW_FOCUS_LOST: {< Window has lost keyboard focus. }
    FKeyboardFocus := False;

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
  if FullScreen then
    WriteLn('Enter FullScreen');

  SDL_EVENT_WINDOW_LEAVE_FULLSCREEN: {< The window has left fullscreen mode. }
  if FullScreen then
    WriteLn('Exit FullScreen');

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

procedure cCHXSDL3Window.SetRenderSize(aWidth, aHeight: Integer;
  const Mode: TSDL_RendererLogicalPresentation);
begin
  if (aWidth <= 0) or (Mode = SDL_LOGICAL_PRESENTATION_DISABLED) then
  begin
    if FullScreen then
      SDL_GetWindowSize(PSDLWindow, @FWindowWidth, nil);
    aWidth := WindowWidth;
  end;

  if (aHeight <= 0) or (Mode = SDL_LOGICAL_PRESENTATION_DISABLED) then
  begin
    if FullScreen then
      SDL_GetWindowSize(PSDLWindow, nil, @FWindowHeight);
    aHeight := WindowHeight;
  end;

  FWidth:= aWidth; FHeight := aHeight;

  SDL_SetRenderLogicalPresentation(PSDLRenderer, Width, Height, Mode);
end;

destructor cCHXSDL3Window.Destroy;
begin
  Renderer.Free;
  SDL_DestroyWindow(PSDLWindow);
  SDL_QuitSubSystem(SDL_INIT_VIDEO);

  inherited Destroy;
end;

end.
