unit ucCHXSDL2Window;
{< Unit of SDL2 Window wrapper class.

  SDL_Init(SDL_INIT_VIDEO); must be called before creating the window.

  Supports creating multiple windows

  Copyright (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  // SDL
  SDL2;

type

  { cCHXSDL2Window }

  cCHXSDL2Window = class
  private
    FHeight : CInt;
    FKeyboardFocus : Boolean;
    FMinimized : Boolean;
    FMouseFocus : Boolean;
    FPRenderer : PSDL_Renderer;
    FPWindow : PSDL_Window;
    FShown : Boolean;
    FTitle : string;
    FWidth : CInt;
    FWindowID : CUInt32;
    procedure SetHeight(const AValue : CInt);
    procedure SetTitle(const AValue : string);
    procedure SetWidth(const AValue : CInt);

  protected
    // Internal states
    {property} FullScreen : Boolean;
    {property} Accelerated : Boolean;

    procedure InitWindow;

    function CreateWindow : Boolean;

  public
    property Title : string read FTitle write SetTitle;
    property Width : CInt read FWidth write SetWidth;
    property Height : CInt read FHeight write SetHeight;

    property PWindow : PSDL_Window read FPWindow;
    property PRenderer : PSDL_Renderer read FPRenderer;

    property WindowID : CUInt32 read FWindowID;

    property Shown : Boolean read FShown;
    property Minimized : Boolean read FMinimized;
    property MouseFocus : Boolean read FMouseFocus;
    property KeyboardFocus : Boolean read FKeyboardFocus;

    procedure Focus;

    procedure HandleEvent(aEvent : TSDL_Event);

    constructor Create(aTitle : string; aWidth, aHeight : LongInt;
      HWAcc : Boolean = False);
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

procedure cCHXSDL2Window.SetHeight(const AValue : CInt);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;

  CreateWindow;
end;

procedure cCHXSDL2Window.SetWidth(const AValue : CInt);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;

  CreateWindow;
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
    SDL_WINDOWPOS_CENTERED, Width, Height, Flags);
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

    { NOTE : "Accelerated" means 3D (at least initially).

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
        Test Intel chip.

    }
    { TODO : Test with surfaces, etc. because it can be faster than direct
        texture drawing.
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
      'SDL_CreateRenderer Error', SDL_GetError,
      PWindow);
    SDL_DestroyWindow(PWindow);
    Exit;
  end;

  // Software Renderer can look ugly on resize
  { TODO : Make IntegerScale configurable }
  if not accelerated then
    SDL_RenderSetIntegerScale(PRenderer, True);
  SDL_RenderSetLogicalSize(PRenderer, Width, Height);

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

procedure cCHXSDL2Window.HandleEvent(aEvent : TSDL_Event);
begin
  if (aEvent.type_ <> SDL_WINDOWEVENT) or
    (aEvent.window.windowID <> WindowID) then
    Exit;

  case aEvent.window.event of
    SDL_WINDOWEVENT_SHOWN : // Window is shown now
      FShown := True;

    SDL_WINDOWEVENT_HIDDEN : // Window is hidden now
      FShown := False;

    SDL_WINDOWEVENT_SIZE_CHANGED : // Repaint
      SDL_RenderPresent(PRenderer);

    SDL_WINDOWEVENT_EXPOSED : // Repaint
      SDL_RenderPresent(PRenderer);

    SDL_WINDOWEVENT_ENTER : // Mouse enter
      FMouseFocus := True;

    SDL_WINDOWEVENT_LEAVE : // Mouse exit
      FMouseFocus := False;

    SDL_WINDOWEVENT_FOCUS_GAINED : // Keyboard focus
      FKeyboardFocus := True;

    SDL_WINDOWEVENT_FOCUS_LOST : // Keyboard focus lost
      FKeyboardFocus := False;

    SDL_WINDOWEVENT_MINIMIZED : // Window minimized
      FMinimized := True;

    SDL_WINDOWEVENT_MAXIMIZED : // Window maximized
      FMinimized := False;

    SDL_WINDOWEVENT_RESTORED : // Window restored
      FMinimized := False;

      // SDL_WINDOWEVENT_CLOSE: // Close window
      // SDL_HideWindow( mWindow );
    else
      ;
  end;
end;

constructor cCHXSDL2Window.Create(aTitle : string;
  aWidth, aHeight : LongInt; HWAcc : Boolean);
begin
  FPWindow := nil;
  FPRenderer := nil;

  // Don't call SDL2 updates
  FTitle := aTitle;
  FWidth := aWidth;
  FHeight := aHeight;

  Accelerated := HWAcc;

  CreateWindow;
end;

destructor cCHXSDL2Window.Destroy;
begin
  if Accelerated then
    Set8087CW(Default8087CW);
  inherited Destroy;
end;

end.
