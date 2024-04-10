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
    FTitle : String;
    FWidth : CInt;
    FWindowID : CUInt32;
    procedure SetHeight(const AValue : CInt);
    procedure SetTitle(const AValue : String);
    procedure SetWidth(const AValue : CInt);

  protected
    // Internal states
    {property} FullScreen : Boolean;
    {property} Accelerated : Boolean;

    procedure InitWindow;

    function CreateWindow : Boolean;
  public
    property Title : String read FTitle write SetTitle;
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

    constructor Create(aTitle : String; aWidth, aHeight : LongInt;
      HWAcc : Boolean = False);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Window }

procedure cCHXSDL2Window.SetTitle(const AValue : String);
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
begin
  Result := False;

  if Assigned(PRenderer) then
    SDL_DestroyRenderer(PRenderer);
  if Assigned(PWindow) then
    SDL_DestroyWindow(PWindow);

  InitWindow;

  // Create new window
  FPWindow := SDL_CreateWindow(PChar(Title), SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED, Width, Height, SDL_WINDOW_SHOWN +
    +SDL_WINDOW_OPENGL + SDL_WINDOW_ALLOW_HIGHDPI + SDL_WINDOW_RESIZABLE);
  if not assigned(FPWindow) then
  begin
    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'SDL_CreateWindow Error',
      SDL_GetError, nil);
    Exit;
  end;

  // Create renderer for window
  { TODO : Create a fallback to software renderer. }
  { NOTE : On laptops with internal and NVidia; if NVidia chip is used as
      default in the system (in NVidia control panel), then Renderer can't be
      created Accelerated, OpenGL or HighDPI. }
  if Accelerated then
    Flags := SDL_WINDOW_ALLOW_HIGHDPI + SDL_WINDOW_OPENGL +
      SDL_RENDERER_ACCELERATED
  else
    Flags := SDL_RENDERER_SOFTWARE;

  FPRenderer := SDL_CreateRenderer(PWindow, -1, Flags);
  if not assigned(PRenderer) then
  begin
    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
      'SDL_CreateRenderer Error', SDL_GetError,
      PWindow);
    SDL_DestroyWindow(PWindow);
    Exit;
  end;

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

    SDL_WINDOWEVENT_SIZE_CHANGED : // New dimensions and repaint
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
  end;
end;

constructor cCHXSDL2Window.Create(aTitle : String; aWidth, aHeight : LongInt;
  HWAcc : Boolean);
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
  inherited Destroy;
end;

end.
