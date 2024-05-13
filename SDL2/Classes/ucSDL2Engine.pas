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
  TSDL2CompFunc = function(Window : cCHXSDL2Window;
    DeltaTime, FrameTime : CUInt32) : Boolean;
  TSDL2DrawFunc = function(Window : PSDL_Window;
    Renderer : PSDL_Renderer) : Boolean;
  TSDL2EventFunc = function(aEvent : TSDL_Event) : Boolean;
  TSDL2FinishProc = procedure();

  { cSDL2Engine }

  cSDL2Engine = class(TComponent)
  private
    FConfig : cSDL2Config;
    FSDLFrameMang : TFPSManager;
    FSDLWindow : cCHXSDL2Window;

  protected
    {property} DeltaTime : CUInt32;
    {property} FrameTime : CUInt32;

    property SDLFrameMang : TFPSManager read FSDLFrameMang;

  public
    {property} Title : string;

    {property} SDL2Setup : TSDL2SetupFunc;
    {property} SDL2Comp : TSDL2CompFunc;
    {property} SDL2Draw : TSDL2DrawFunc;
    {property} SDL2Event : TSDL2EventFunc;
    {property} SDL2Finish : TSDL2FinishProc;

    property Config : cSDL2Config read FConfig;

    property SDLWindow : cCHXSDL2Window read FSDLWindow;

    procedure Run;

    constructor Create(const aOwner : TComponent; const aTitle : string;
      const aWinWidth : LongInt; const aWinHeight : LongInt;
      const HWAcc : Boolean = False; const aLogWidth : LongInt = 0;
      const aLogHeight : LongInt = 0); overload;
    constructor Create(const aOwner : TComponent; const aTitle : string;
      const aIniFile : string; const HWAcc : Boolean = False); overload;
    destructor Destroy; override;
  end;

implementation

{ cSDL2Engine }

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
        ProgRun := SDL2Comp(SDLWindow, DeltaTime, FrameTime);

      // Don't draw if minimized
      if not SDLWindow.Minimized then
      begin
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
      end;

      // EVENTS

      { NOTE : Listen window events when minimized. }

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

constructor cSDL2Engine.Create(const aOwner : TComponent;
  const aTitle : string; const aWinWidth : LongInt;
  const aWinHeight : LongInt; const HWAcc : Boolean;
  const aLogWidth : LongInt; const aLogHeight : LongInt);
begin
  inherited Create(aOwner);

  Title := aTitle;

  SDL_Init(SDL_INIT_EVERYTHING);

  FSDLWindow := cCHXSDL2Window.Create(Title, aWinWidth, aWinHeight,
    HWAcc, aLogWidth, aLogHeight);

  if not assigned(SDLWindow) then
    raise Exception.Create('cCHXSDL2Window was not created.');
  if SDLWindow.WindowID = 0 then
  begin
    FreeAndNil(FSDLWindow);
    raise Exception.Create('cCHXSDL2Window.PSDLWindow was not created.');
  end;
end;

constructor cSDL2Engine.Create(const aOwner : TComponent;
  const aTitle : string; const aIniFile : string; const HWAcc : Boolean);
begin
  inherited Create(aOwner);

  FConfig := cSDL2Config.Create(Self);
  Config.DefaultFileName := aIniFile;
  Config.LoadFromFile('');

  Create(aOwner, aTitle, Config.WindowWidth, Config.WindowHeight, HWAcc,
    Config.RendererWidth, Config.RendererHeight);
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
