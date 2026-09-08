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

  TLogicalPresentation = record
    Width, Height: CInt;
    Mode: TSDL_RendererLogicalPresentation;
  end;
  {< Struct stored in the stack when Logical Presentation is pushed. }

  {
    Wrapper of SDL3 Window and creates its asociated SDL3 Renderer.

    Actual `PSDL_Window` and `PSDL_Renderer` are `PSDLWindow` and
    `PSDLRenderer` pointer properties. SDL native methods can called with
    these pointers, `SDL_[X](PSDLRenderer, [...]);` or
    `SDL_[X](PSDLWindow, [...]);`.

    `Renderer` property (`cCHXSDL3Render` class) wraps SDL3 Renderer and
    expands it with more primitives `Renderer.[X]([...])`.

    `cCHXSDL3Engine`, in its context, has the properties `Window` and `Render`
    as "shortcuts" for both classes; and `SDLWindow` and `SDLRenderer` for
    both pointers.

    Calls `SDL_InitSubSystem(SDL_INIT_VIDEO)` on creation and
    `SDL_QuitSubSystem(SDL_INIT_VIDEO)` on destruction as SDL3 keeps track of
    how many times are called each one.

    Supports creating multiple cCHXSDL3Window in the same program,
      checking its ID in event handling.
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

  protected
    LogPresStack: Array of TLogicalPresentation;
    //< Stack of pushed Logical Presentations pushed with PushRenderSize.

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
      const RenderDrivers: String = '');
    {< Create a new SDL Window and its associated renderer.

      Render canvas size is automatically scaled to Window actual size.

      @param aTitle Title of the window.
      @param aWidth Logical width of Renderer.
      @param aHeight Logical height of Renderer.
      @param(Scale Scale window size. `0 = Maximized window.`
      @param aFullScreen Create full screen window.
      @param(RenderDrivers Comma separated drivers to try for renderer.
        Empty try SDL preference.) 
    }

    destructor Destroy; override;

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

    function SetRenderSize(aWidth, aHeight: Integer;
      const Mode: TSDL_RendererLogicalPresentation
      = SDL_LOGICAL_PRESENTATION_LETTERBOX) : Boolean;
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
          Restores coordinates to actual window ones ignoring previous
          parameters.
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
    function PushRenderSize(const aWidth, aHeight: Integer;
      const Mode: TSDL_RendererLogicalPresentation
      = SDL_LOGICAL_PRESENTATION_LETTERBOX) : Boolean;
    {< Change render canvas size (_Logical Size_) and stores current in a stack.

      Same as SetRenderSize but pushe current config into a stack to restore
      it with PopRenderSize.
    }
    function PopRenderSize(const PopCount: Integer = 1) : Boolean;
    {< Restore previous Logical Presentation configuration.

      @param(PopCount Number of Logical Presentations to pop out.)
    }

    function GetSupportedVideos : String;
    { Get a comma separated list of the supported Video drivers. }

    function GetSupportedRenderers : String;
    { Get a comma separated list of the supported Renderer drivers. }
  end;

implementation

{ cCHXSDL3Window }

constructor cCHXSDL3Window.Create(const aTitle : String;
  const aWidth, aHeight : CInt; Scale: CInt;
  const aFullScreen : Boolean; const RenderDrivers : String);
var
  Flags : TSDL_WindowFlags;
  Maximize : Boolean;
  TempInt1, TempInt2 : CInt;
begin
  if not SDL_InitSubSystem(SDL_INIT_VIDEO) then
    raise Exception.CreateFmt('[ERROR] SDL_InitSubSystem: %s',
      [SDL_GetError]);

  // Some info
  SDL_Log('== %s ==', [PAnsiChar(aTitle)]);
  SDL_Log('Video Drivers: %s', [PAnsiChar(GetSupportedVideos)]);
  SDL_Log('Supported Render Drivers: %s', [PAnsiChar(GetSupportedRenderers)]);

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

  // Setting size if normal parameters
  Maximize := (Scale <= 0) or (Width <= 0) or (Height <= 0);
  if not Maximize then
  begin
    FWindowWidth := Width * Scale;
    FWindowHeight := Height * Scale;
    SDL_SetWindowSize(PSDLWindow, FWindowWidth, FWindowHeight);
    SDL_SetWindowPosition(PSDLWindow, SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED);
  end;

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

    if Scale < 1 then
      Scale := 1;

    if (Width <= 0) or (Height <= 0) then
    begin
      FWidth := FWindowWidth div Scale;
      FHeight := FWindowHeight div Scale;
    end;
  end;

  FWindowID := SDL_GetWindowID(PSDLWindow);

  PSDLRenderer := SDL_CreateRenderer(PSDLWindow, PAnsiChar(RenderDrivers));
  if not Assigned(PSDLRenderer) then
    raise Exception.CreateFmt('[ERROR] SDL_CreateRenderer(''%s''): %s',
      [RenderDrivers, SDL_GetError]);

  // Renderer will destroy SDL Renderer
  Renderer := cCHXSDL3Renderer.Create(PSDLRenderer, True);

  // ToDo: Make use of integer scale configurable:
  //   (SDL_LOGICAL_PRESENTATION_INTEGER_SCALE)
  SetLength(LogPresStack, 1);
  SetRenderSize(Width, Height, SDL_LOGICAL_PRESENTATION_LETTERBOX);

  // Reading window flags to set properties
  Flags := SDL_GetWindowFlags(PSDLWindow);
  FMouseFocus := (Flags and SDL_WINDOW_MOUSE_FOCUS) = SDL_WINDOW_MOUSE_FOCUS;
  FKeyboardFocus := (Flags and SDL_WINDOW_INPUT_FOCUS) = SDL_WINDOW_INPUT_FOCUS;
  FShown := ((not Flags) and SDL_WINDOW_HIDDEN) = SDL_WINDOW_HIDDEN;

  SDL_GetRenderOutputSize(PSDLRenderer, @TempInt1, @TempInt2);
  SDL_Log('Window (%s): %d x %d (Must be %d x %d)',
    [SDL_GetCurrentVideoDriver, WindowWidth, WindowHeight, TempInt1, TempInt2]);

  SDL_Log('Renderer %s: %d x %d (x%d)',
    [SDL_GetRendererName(PSDLRenderer), Width, Height, Scale]);
end;

destructor cCHXSDL3Window.Destroy;
begin
  Renderer.Free;
  SDL_DestroyWindow(PSDLWindow);
  SDL_QuitSubSystem(SDL_INIT_VIDEO);

  inherited Destroy;
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
  begin
    SDL_Log('SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED');
    SDL_RenderPresent(PSDLRenderer);
  end;

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

function cCHXSDL3Window.SetRenderSize(aWidth, aHeight: Integer;
  const Mode: TSDL_RendererLogicalPresentation): Boolean;
begin
  Result := True;
  if (aWidth <= 0) or (Mode = SDL_LOGICAL_PRESENTATION_DISABLED) then
  begin
    if FullScreen then
      Result := SDL_GetWindowSize(PSDLWindow, @FWindowWidth, nil);
    aWidth := WindowWidth;
  end;

  if (aHeight <= 0) or (Mode = SDL_LOGICAL_PRESENTATION_DISABLED) then
  begin
    if FullScreen then
      Result := SDL_GetWindowSize(PSDLWindow, nil, @FWindowHeight)
        and Result;
    aHeight := WindowHeight;
  end;

  LogPresStack[High(LogPresStack)].Width := aWidth;
  LogPresStack[High(LogPresStack)].Height := aHeight;
  LogPresStack[High(LogPresStack)].Mode := Mode;
  FWidth:= aWidth; FHeight := aHeight;

  Result :=
    SDL_SetRenderLogicalPresentation(PSDLRenderer, aWidth, aHeight, Mode)
    and Result;
end;

function cCHXSDL3Window.PushRenderSize(const aWidth, aHeight: Integer;
  const Mode: TSDL_RendererLogicalPresentation): Boolean;
begin
  SetLength(LogPresStack, Length(LogPresStack) + 1);
  Result := SetRenderSize(aWidth, aHeight, Mode);
end;

function cCHXSDL3Window.PopRenderSize(const PopCount: Integer): Boolean;
var
  Size: Integer;
begin
  if (PopCount > 0) and (Length(LogPresStack) > PopCount) then
    Size := Length(LogPresStack) - PopCount
  else
    Size := 1;
  SetLength(LogPresStack, Size);
  Result := SetRenderSize(LogPresStack[High(LogPresStack)].Width,
    LogPresStack[High(LogPresStack)].Height,
    LogPresStack[High(LogPresStack)].Mode);
end;

function cCHXSDL3Window.GetSupportedRenderers : String;
var
  RCount, i: CInt;
begin
  RCount := SDL_GetNumRenderDrivers;
  if RCount <= 0 then Exit('');
  Result := SDL_GetRenderDriver(0);
  for i := 1 to (RCount - 1) do
    Result += ', ' + SDL_GetRenderDriver(i);
end;

function cCHXSDL3Window.GetSupportedVideos : String;
var
  RCount, i: CInt;
begin
  RCount := SDL_GetNumVideoDrivers;
  if RCount <= 0 then Exit('');
  Result := SDL_GetVideoDriver(0);
  for i := 1 to (RCount - 1) do
    Result += ', ' + SDL_GetVideoDriver(i);
end;

end.
