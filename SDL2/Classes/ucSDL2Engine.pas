unit ucSDL2Engine;
{< Unit of cNOTCEngine class.

  Copyright (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  // SDL2
  SDL2, SDL2_GFX,
  // CHX
  ucSDL2Config,
  ucCHXSDL2Window;

type
  TSDL2SetupFunc = function() : Boolean;
  TSDL2CompFunc = function(DeltaTime, FrameTime : CUInt32) : Boolean;
  TSDL2DrawFunc = function(Window : PSDL_Window;
    Renderer : PSDL_Renderer) : Boolean;
  TSDL2EventFunc = function(aEvent : TSDL_Event) : Boolean;
  TSDL2FinishProc = procedure();

  { cSDL2Engine }

  cSDL2Engine = class(TComponent)
  private
    FConfig : cSDL2Config;
    FDeltaTime : CUInt32;
    FFrameTime : CUInt32;
    FSDL2Comp : TSDL2CompFunc;
    FSDL2Draw : TSDL2DrawFunc;
    FSDL2Event : TSDL2EventFunc;
    FSDL2Finish : TSDL2FinishProc;
    FSDL2Setup : TSDL2SetupFunc;
    FSDLFrameMang : TFPSManager;
    FSDLWindow : cCHXSDL2Window;
    FTitle : string;
    procedure SetDeltaTime(const AValue : CUInt32);
    procedure SetFrameTime(const AValue : CUInt32);
    procedure SetSDL2Comp(const AValue : TSDL2CompFunc);
    procedure SetSDL2Draw(const AValue : TSDL2DrawFunc);
    procedure SetSDL2Event(const AValue : TSDL2EventFunc);
    procedure SetSDL2Finish(const AValue : TSDL2FinishProc);
    procedure SetSDL2Setup(const AValue : TSDL2SetupFunc);

    procedure SetTitle(const AValue : string);

  protected
    property SDLWindow : cCHXSDL2Window read FSDLWindow;
    property SDLFrameMang : TFPSManager read FSDLFrameMang;
    property DeltaTime : CUInt32 read FDeltaTime write SetDeltaTime;
    property FrameTime : CUInt32 read FFrameTime write SetFrameTime;

  public
    property Title : string read FTitle write SetTitle;

    property Config : cSDL2Config read FConfig;

    property SDL2Setup : TSDL2SetupFunc read FSDL2Setup write SetSDL2Setup;
    property SDL2Comp : TSDL2CompFunc read FSDL2Comp write SetSDL2Comp;
    property SDL2Draw : TSDL2DrawFunc read FSDL2Draw write SetSDL2Draw;
    property SDL2Event : TSDL2EventFunc read FSDL2Event write SetSDL2Event;
    property SDL2Finish : TSDL2FinishProc read FSDL2Finish write SetSDL2Finish;

    procedure Run;

    constructor Create(aOwner : TComponent; aTitle : string;
      WinX, WinY : LongInt;  HWAcc : Boolean = False); overload;
    constructor Create(aOwner : TComponent; aTitle : string;
      aIniFile : string;   HWAcc : Boolean = False); overload;
    destructor Destroy; override;

  published

  end;

implementation

{ cSDL2Engine }

procedure cSDL2Engine.SetDeltaTime(const AValue : CUInt32);
begin
  if FDeltaTime = AValue then Exit;
  FDeltaTime := AValue;
end;

procedure cSDL2Engine.SetFrameTime(const AValue : CUInt32);
begin
  if FFrameTime = AValue then Exit;
  FFrameTime := AValue;
end;

procedure cSDL2Engine.SetSDL2Comp(const AValue : TSDL2CompFunc);
begin
  if FSDL2Comp = AValue then Exit;
  FSDL2Comp := AValue;
end;

procedure cSDL2Engine.SetSDL2Draw(const AValue : TSDL2DrawFunc);
begin
  if FSDL2Draw = AValue then Exit;
  FSDL2Draw := AValue;
end;

procedure cSDL2Engine.SetSDL2Event(const AValue : TSDL2EventFunc);
begin
  if FSDL2Event = AValue then Exit;
  FSDL2Event := AValue;
end;

procedure cSDL2Engine.SetSDL2Finish(const AValue : TSDL2FinishProc);
begin
  if FSDL2Finish = AValue then Exit;
  FSDL2Finish := AValue;
end;

procedure cSDL2Engine.SetSDL2Setup(const AValue : TSDL2SetupFunc);
begin
  if FSDL2Setup = AValue then Exit;
  FSDL2Setup := AValue;
end;

procedure cSDL2Engine.SetTitle(const AValue : string);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
end;

procedure cSDL2Engine.Run;
var
  ProgRun : Boolean;
  aEvent : TSDL_Event;
begin
  ProgRun := True;

  SDL_InitFramerate(@SDLFrameMang);
  SDL_SetFramerate(@SDLFrameMang, 60);
  DeltaTime := 0;
  FrameTime := 0;

  try
    if assigned(SDL2Setup) then
      ProgRun := SDL2Setup();

    while ProgRun do
    begin
      // COMPUTE
      if ProgRun and (assigned(SDL2Comp)) then
        ProgRun := SDL2Comp(DeltaTime, FrameTime);

      // DRAW
      if ProgRun and (assigned(SDL2Draw)) then
        ProgRun := SDL2Draw(SDLWindow.PWindow, SDLWindow.PRenderer);

      // UPDATE RENDER
      SDL_RenderPresent(SDLWindow.PRenderer);

      // TIMING
      FrameTime := SDL_GetTicks - FrameTime;
      DeltaTime := SDL_framerateDelay(@SDLFrameMang);
      if (SDL_getFramecount(@SDLFrameMang) mod 30) = 0 then
        SDLWindow.Title :=
          Format('%0:s: %1:5d ms (%2:5d ms)', [Title, DeltaTime, FrameTime]);
      FrameTime := SDL_GetTicks;

      // EVENTS

      // SDL_PumpEvents;
      while SDL_PollEvent(@aEvent) <> 0 do
      begin
        if aEvent.type_ = SDL_WINDOWEVENT then
          SDLWindow.HandleEvent(aEvent)
        else if aEvent.type_ = SDL_QUITEV then
          ProgRun := False
        else if ProgRun and (assigned(SDL2Event)) then
          ProgRun := SDL2Event(aEvent);
      end;
    end;

  finally
    if assigned(SDL2Finish) then
      SDL2Finish();
  end;
end;

constructor cSDL2Engine.Create(aOwner : TComponent; aTitle : string; WinX,
  WinY : LongInt; HWAcc : Boolean);
begin
  inherited Create(aOwner);

  Title := aTitle;

  SDL_Init(SDL_INIT_EVERYTHING);

  FSDLWindow := cCHXSDL2Window.Create(Title, WinX, WinY, HWAcc);

  if not assigned(SDLWindow) then
    raise Exception.Create('cCHXSDL2Window was not created.');
  if SDLWindow.WindowID = 0 then
  begin
    FreeAndNil(FSDLWindow);
    raise Exception.Create('cCHXSDL2Window.PSDLWindow was not created.');
  end;
end;

constructor cSDL2Engine.Create(aOwner : TComponent; aTitle : string;
  aIniFile : string; HWAcc : Boolean);
begin
  inherited Create(aOwner);

  FConfig := cSDL2Config.Create(Self);
  Config.DefaultFileName := aIniFile;
  Config.LoadFromFile('');

  Create(aOwner, aTitle, Config.WindowWidth, Config.WindowHeight, HWAcc);
end;

destructor cSDL2Engine.Destroy;
begin
  if assigned(Config) then
  begin
    Config.SaveToFile('', False);
    Config.Free;
  end;

  FreeAndNil(FSDLWindow);

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
