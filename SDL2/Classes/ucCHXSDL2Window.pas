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
    FFullScreen : boolean;
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
    procedure SetFullScreen(const aValue : boolean);
    procedure SetHeight(const aValue : CInt);
    procedure SetKeyboardFocus(const aValue : boolean);
    procedure SetMinimized(const aValue : boolean);
    procedure SetMouseFocus(const aValue : boolean);
    procedure SetShown(const aValue : boolean);
    procedure SetTitle(const aValue : string);
    procedure SetWidth(const aValue : CInt);

  protected

    // Internal states
    property FullScreen : boolean read FFullScreen write SetFullScreen;

    procedure InitWindow;

    function CreateWindow : boolean;
  public
    property Title : string read FTitle write SetTitle;
    property Width : CInt read FWidth write SetWidth;
    property Height : CInt read FHeight write SetHeight;

    property PSDLWindow : PSDL_Window read FPSDLWindow;
    property PSDLRenderer : PSDL_Renderer read FPSDLRenderer;

    property WindowID : CUInt32 read FWindowID;

    property Shown : boolean read FShown;
    property Minimized : boolean read FMinimized;
    property MouseFocus : boolean read FMouseFocus;
    property KeyboardFocus : boolean read FKeyboardFocus;

    procedure Focus;

    procedure Render;

    procedure HandleEvent(aEvent : TSDL_Event);

    constructor Create(aTitle : string; aWidth, aHeight : longint);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Window }

procedure cCHXSDL2Window.SetHeight(const aValue : CInt);
begin
  if FHeight = aValue then Exit;
  FHeight := aValue;
end;

procedure cCHXSDL2Window.SetFullScreen(const aValue : boolean);
begin
  if FFullScreen = aValue then Exit;
  FFullScreen := aValue;
end;

procedure cCHXSDL2Window.SetKeyboardFocus(const aValue : boolean);
begin
  if FKeyboardFocus = aValue then Exit;
  FKeyboardFocus := aValue;
end;

procedure cCHXSDL2Window.SetMinimized(const aValue : boolean);
begin
  if FMinimized = aValue then Exit;
  FMinimized := aValue;
end;

procedure cCHXSDL2Window.SetMouseFocus(const aValue : boolean);
begin
  if FMouseFocus = aValue then Exit;
  FMouseFocus := aValue;
end;

procedure cCHXSDL2Window.SetShown(const aValue : boolean);
begin
  if FShown = aValue then Exit;
  FShown := aValue;
end;

procedure cCHXSDL2Window.SetTitle(const aValue : string);
begin
  if FTitle = aValue then Exit;
  FTitle := aValue;

  SDL_SetWindowTitle(PSDLWindow, pchar(aValue));
end;

procedure cCHXSDL2Window.SetWidth(const aValue : CInt);
begin
  if FWidth = aValue then Exit;
  FWidth := aValue;
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

  if Assigned(PSDLRenderer) then
    SDL_DestroyRenderer(PSDLRenderer);
  if Assigned(PSDLWindow) then
    SDL_DestroyWindow(PSDLWindow);

  InitWindow;

  // Creating new Window
  FPSDLWindow := SDL_CreateWindow(PChar(Title), SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED, Width, Height, SDL_WINDOW_SHOWN +
    SDL_WINDOW_RESIZABLE);
  if not assigned(PSDLWindow) then Exit;

  //Create renderer for window
  FPSDLRenderer := SDL_CreateRenderer(PSDLWindow, -1, SDL_RENDERER_SOFTWARE
   // + SDL_RENDERER_ACCELERATED
   // + SDL_RENDERER_PRESENTVSYNC
   );
  if not assigned(PSDLWindow) then
  begin
    SDL_DestroyWindow(PSDLWindow);
    InitWindow;
    Exit;
  end;

  FWindowID := SDL_GetWindowID(PSDLWindow);
  SDL_GetWindowSize(PSDLWindow, @FWidth, @FHeight);

  SDL_SetRenderDrawColor(PSDLRenderer, 255, 255, 255, 255);

  FMouseFocus := True;
  FKeyboardFocus := True;
  FShown := True;

  Result := True;
end;

procedure cCHXSDL2Window.Focus;
begin
  if not Shown then
    SDL_ShowWindow(PSDLWindow);
  SDL_RaiseWindow(PSDLWindow);
end;

procedure cCHXSDL2Window.Render;
begin
  if not Minimized then
  begin
    SDL_SetRenderDrawColor(PSDLRenderer, 255, 255, 255, 255);
    SDL_RenderClear(PSDLRenderer);

    SDL_RenderPresent(PSDLRenderer);
  end;
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
      SDL_RenderPresent(PSDLRenderer);
    end;

    SDL_WINDOWEVENT_EXPOSED : // Repaint
      SDL_RenderPresent(PSDLRenderer);

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
