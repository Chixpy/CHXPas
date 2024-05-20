unit ucCHXSDL2Window;
{< Unit of SDL2 Window wrapper class.

  `SDL_InitSubSystem(SDL_INIT_VIDEO)` is called automatically if video system
    is not initialized.

  But `SDL_QuitSubSystem(SDL_INIT_VIDEO)` (or `SDL_Quit`) must be called when
    all windows are destroyed.

  Supports creating multiple windows by the same engine, checking its ID in
    event handling.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  // SDL
  SDL2;

resourcestring
  rsErrSDL_CreateWindow = 'SDL_CreateWindow Error';

type

  { cCHXSDL2Window }

  cCHXSDL2Window = class
  private
    FLogHeight : CInt;
    FLogWidth : CInt;
    FWinHeight : CInt;
    FKeyboardFocus : Boolean;
    FMinimized : Boolean;
    FMouseFocus : Boolean;
    FPRenderer : PSDL_Renderer;
    FPWindow : PSDL_Window;
    FShown : Boolean;
    FTitle : string;
    FWinWidth : CInt;
    FWindowID : CUInt32;
    procedure SetTitle(const AValue : string);

  protected
    // Internal states
    {property} FullScreen : Boolean;
    {property} Accelerated : Boolean;

    procedure InitWindow;

    function CreateWindow : Boolean;

  public
    property Title : string read FTitle write SetTitle;
    property WinWidth : CInt read FWinWidth;
    property WinHeight : CInt read FWinHeight;
    property LogWidth : CInt read FLogWidth;
    property LogHeight : CInt read FLogHeight;

    property PWindow : PSDL_Window read FPWindow;
    property PRenderer : PSDL_Renderer read FPRenderer;

    property WindowID : CUInt32 read FWindowID;

    property Shown : Boolean read FShown;
    property Minimized : Boolean read FMinimized;
    property MouseFocus : Boolean read FMouseFocus;
    property KeyboardFocus : Boolean read FKeyboardFocus;

    procedure Focus;
    //< Set focus to this window.

    function HandleEvent(aEvent : TSDL_Event; var Handled : Boolean) : Boolean;

    constructor Create(const aTitle : string; const aWinWidth : CInt;
      const aWinHeight : CInt; const HWAcc : Boolean = False;
      const LWidth : CInt = 0; const LHeight : CInt = 0);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Window }

procedure cCHXSDL2Window.SetTitle(const AValue : string);
begin
  if FTitle = aValue then Exit;
  FTitle := aValue;

  SDL_SetWindowTitle(PWindow, PChar(aValue));
end;

procedure cCHXSDL2Window.InitWindow;
begin
  FullScreen := False;
  FMinimized := False;
  FShown := False;
  FMouseFocus := False;
  FKeyboardFocus := False;

  FWindowID := 0;
end;

function cCHXSDL2Window.CreateWindow : Boolean;
var
  Flags : CUInt32;
  NRend : cint32;
  i, RendDrv : integer;
  RendInfo : TSDL_RendererInfo;
begin
  Result := False;

  if Assigned(PRenderer) then
    SDL_DestroyRenderer(PRenderer);
  if Assigned(PWindow) then
    SDL_DestroyWindow(PWindow);

  InitWindow;

  Flags := SDL_WINDOW_SHOWN + SDL_WINDOW_ALLOW_HIGHDPI + SDL_WINDOW_RESIZABLE;
  if Accelerated then
    Flags += SDL_WINDOW_OPENGL;

  // Create new window
  FPWindow := SDL_CreateWindow(PChar(Title), SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED, WinWidth, WinHeight, Flags);
  if not assigned(FPWindow) then
  begin
    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'SDL_CreateWindow Error',
      SDL_GetError, nil);
    Exit;
  end;

  // Create renderer for window
  { TODO : Make a renderer driver selector.}

  if Accelerated then
  begin
    { NOTE : On laptops with internal Intel and NVidia. Maybe it happens with
        NVidia cards alone.

      If NVidia chip is used as default in the system (in NVidia control
        panel), SDL_CreateRenderer will fail on "direct3d" driver wich
        usually is autoselected with -1 parameter...

      if it's set to internal Intel, it seems to work always.

      If the default setting 'autoselect chip' it can work or not at random.
    }

    { NOTE : "Accelerated" means 3D, at least initially when direct pixel
        manipulation or primitive drawing in renderer.

      Although flag is named 'SDL_RENDERER_ACCELERATED', in my tests all
        accelerated drivers are actually slower than software in direct pixel
        manipulation or drawing primitives with sdl2_gfx (with Nvidia chip)
        in window renderer.

        - opengl, opengles2: 3~4 times slower.
        - direct3d11, direct3d12: ¡20! times slower.

      But manipulating pixels of a texture and then RenderCopy is near
        2 times faster than software in direct pixel manipulation of renderer.
        (and "opengl" is about the same speed)

      TODO: Test rendering primitives to a texture and test other drivers.

      TODO: Test Intel chip.

    }

    // Searching opengl driver
    // SDL_SetHint(SDL_HINT_RENDER_DRIVER, 'opengl');

    NRend := SDL_GetNumRenderDrivers;
    i := 0;
    RendDrv := -1;
    while (i < NRend) and (RendDrv = -1) do
    begin
      SDL_GetRenderDriverInfo(i, @RendInfo);
      if RendInfo.Name = 'opengl' then
        RendDrv := i;
      Inc(i);
    end;

    if RendDrv <> -1 then
      Set8087CW($133F);  // Seems to be better to use this with OpenGL
    Flags := SDL_RENDERER_ACCELERATED;
    FPRenderer := SDL_CreateRenderer(PWindow, RendDrv, Flags);
  end
  else
  begin
    // SDL_SetHint(SDL_HINT_RENDER_DRIVER, '');
    Flags := SDL_RENDERER_SOFTWARE;
    FPRenderer := SDL_CreateRenderer(PWindow, -1, Flags);
  end;

  if not assigned(PRenderer) then
  begin
    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
      'SDL_CreateRenderer Error', SDL_GetError, PWindow);
    SDL_DestroyWindow(PWindow);
    Exit;
  end;

  // Software Renderer can look ugly on resize with vertical black stripes
  { TODO : Make IntegerScale configurable }
  if not accelerated then
    SDL_RenderSetIntegerScale(PRenderer, True);
  SDL_RenderSetLogicalSize(PRenderer, LogWidth, LogHeight);

  FWindowID := SDL_GetWindowID(PWindow);

  SDL_SetRenderDrawColor(PRenderer, 255, 255, 255, 255);

  FMouseFocus := True;
  FKeyboardFocus := True;
  FShown := True;

  Result := True;
end;

procedure cCHXSDL2Window.Focus;
begin
  if not Shown then
    SDL_ShowWindow(PWindow);
  SDL_RaiseWindow(PWindow);
end;

function cCHXSDL2Window.HandleEvent(aEvent : TSDL_Event;
  var Handled : Boolean) : Boolean;
begin
  Result := True;

  if (aEvent.type_ <> SDL_WINDOWEVENT) or
    (aEvent.window.windowID <> WindowID) or
    Handled then
    Exit;

  Handled := True; // Maybe pass current value always?

  case aEvent.window.event of
    SDL_WINDOWEVENT_SHOWN : {< Window has been shown.}
      FShown := True;

    SDL_WINDOWEVENT_HIDDEN : {< Window has been hidden.}
      FShown := False;

    SDL_WINDOWEVENT_EXPOSED :
      {< Window has been exposed and should be redrawn.}
      SDL_RenderPresent(PRenderer);

    //SDL_WINDOWEVENT_MOVED  : {< Window has been moved to data1; data2.}
    //SDL_WINDOWEVENT_RESIZED: {< Window has been resized to data1xdata2.}

    SDL_WINDOWEVENT_SIZE_CHANGED : {< The window size has changed.}
      SDL_RenderPresent(PRenderer);

    SDL_WINDOWEVENT_MINIMIZED : {< Window has been minimized.}
      FMinimized := True;

    SDL_WINDOWEVENT_MAXIMIZED : {< Window has been maximized.}
      FMinimized := False;

    SDL_WINDOWEVENT_RESTORED :
      {< Window has been restored to normal size and position.}
      FMinimized := False;

    SDL_WINDOWEVENT_ENTER : {< Window has gained mouse focus.}
      FMouseFocus := True;

    SDL_WINDOWEVENT_LEAVE : {< Window has lost mouse focus.}
      FMouseFocus := False;

    SDL_WINDOWEVENT_FOCUS_GAINED : {< Window has gained keyboard focus.}
      FKeyboardFocus := True;

    SDL_WINDOWEVENT_FOCUS_LOST : {< Window has lost keyboard focus.}
      FKeyboardFocus := False;

      //SDL_WINDOWEVENT_CLOSE :
      {< The window manager requests that the window be closed.}
      // SDL_HideWindow( mWindow ); // Done automatically?

      //SDL_WINDOWEVENT_TAKE_FOCUS:
      {< Window is being offered a focus (should SetWindowInputFocus() on
         itself or a subwindow, or ignore.)}
      //SDL_WINDOWEVENT_HIT_TEST:
      {< Window had a hit test that wasn't SDL_HITTEST_NORMAL.}
      //SDL_WINDOWEVENT_ICCPROF_CHANGED:
      {< The ICC profile of the window's display has changed.}
      //SDL_WINDOWEVENT_DISPLAY_CHANGED:
      {< Window has been moved to display data1.}
    else
      Handled := False;
  end;
end;

constructor cCHXSDL2Window.Create(const aTitle : string;
  const aWinWidth : CInt; const aWinHeight : CInt; const HWAcc : Boolean;
  const LWidth : CInt; const LHeight : CInt);
begin
  FPWindow := nil;
  FPRenderer := nil;

  // Don't call SDL2 updates
  FTitle := aTitle;
  FWinWidth := aWinWidth;
  FWinHeight := aWinHeight;

  if LWidth = 0 then
    FLogWidth := aWinWidth
  else
    FLogWidth := LWidth;

  if LHeight = 0 then
    FLogHeight := aWinHeight
  else
    FLogHeight := LHeight;

  Accelerated := HWAcc;

  if SDL_WasInit(SDL_INIT_VIDEO) = 0 then
    SDL_InitSubSystem(SDL_INIT_VIDEO);

  CreateWindow;
end;

destructor cCHXSDL2Window.Destroy;
begin
  SDL_DestroyRenderer(PRenderer);
  SDL_DestroyWindow(PWindow);

  if Accelerated then
    Set8087CW(Default8087CW);

  inherited Destroy;
end;

end.
