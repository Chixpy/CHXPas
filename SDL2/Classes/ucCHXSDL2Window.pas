unit ucCHXSDL2Window;
{< Unit of SDL2 Window wrapper class.

  SDL_Init(SDL_INIT_VIDEO); must be called before creating the window.

  Supports creating

  Copyright (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ctypes,
  // SDL
  sdl2;

type

  { cCHXSDL2Window }

  cCHXSDL2Window = class
  private
    FHeight : CInt;
    FKeyboardFocus : boolean;
    FMinimized : boolean;
    FMouseFocus : boolean;
    FPSDLRenderer : PSDL_Renderer;
    FPSDLWindow : PSDL_Window;
    FShown : boolean;
    FTitle : string;
    FWidth : CInt;
    FWindowID : CUInt32;
    procedure SetHeight(const AValue : CInt);
    procedure SetTitle(const AValue : string);
    procedure SetWidth(const AValue : CInt);

  protected

    // Internal states
    {property} FullScreen : boolean;

    procedure InitWindow;

    function CreateWindow : boolean;

  public
    property Title : string read FTitle write SetTitle;
    property Width : CInt read FWidth write SetWidth;
    property Height : CInt read FHeight write SetHeight;

    property PWindow : PSDL_Window read FPSDLWindow;
    property PRenderer : PSDL_Renderer read FPSDLRenderer;

    property WindowID : CUInt32 read FWindowID;

    property Shown : boolean read FShown;
    property Minimized : boolean read FMinimized;
    property MouseFocus : boolean read FMouseFocus;
    property KeyboardFocus : boolean read FKeyboardFocus;

    procedure Focus;

    procedure HandleEvent(aEvent : TSDL_Event);

    constructor Create(aTitle : string; aWidth, aHeight : longint);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Window }

procedure cCHXSDL2Window.SetTitle(const aValue : string);
begin
  if FTitle = aValue then Exit;
  FTitle := aValue;

  SDL_SetWindowTitle(PWindow, pchar(aValue));
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

function cCHXSDL2Window.CreateWindow : boolean;
begin
  Result := False;

  if Assigned(PRenderer) then
    SDL_DestroyRenderer(PRenderer);
  if Assigned(PWindow) then
    SDL_DestroyWindow(PWindow);

  InitWindow;

  // Creating new Window
  FPSDLWindow := SDL_CreateWindow(PChar(Title), SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED, Width, Height, SDL_WINDOW_SHOWN +
    SDL_WINDOW_RESIZABLE);
  if not assigned(PWindow) then Exit;

  //Create renderer for window
  FPSDLRenderer := SDL_CreateRenderer(PWindow, -1, SDL_RENDERER_SOFTWARE
   // + SDL_RENDERER_ACCELERATED
   // + SDL_RENDERER_PRESENTVSYNC
   );
  if not assigned(PWindow) then
  begin
    SDL_DestroyWindow(PWindow);
    InitWindow;
    Exit;
  end;

  FWindowID := SDL_GetWindowID(PWindow);
  SDL_GetWindowSize(PWindow, @FWidth, @FHeight);

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
    begin
      Width := aEvent.window.data1;
      Height := aEvent.window.data2;
      SDL_RenderPresent(PRenderer);
    end;

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

constructor cCHXSDL2Window.Create(aTitle : string; aWidth, aHeight : longint);
begin
  FPSDLWindow := nil;
  FPSDLRenderer := nil;

  InitWindow;

  Title := aTitle;
  Width := aWidth;
  Height := aHeight;

  CreateWindow;
end;

destructor cCHXSDL2Window.Destroy;
begin
  inherited Destroy;
end;

end.
