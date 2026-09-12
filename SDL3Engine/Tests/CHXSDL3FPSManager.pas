program CHXSDL3FPSManager;
{<
  A simple program with cCHXSDL3FPSManager.

  Shows a simple text with vanilla SDL Window and Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

{$mode objfpc}{$H+}

uses SysUtils, CTypes, SDL3,
  ucCHXSDL3FPSManager; // Test without CHXSDL3Engine

var
  Window: PSDL_Window = nil;
  Renderer: PSDL_Renderer = nil;
  Event: TSDL_Event;
  Running: Boolean = True;
  FPSMang: cCHXSDL3FPSManager;
  aDelay: CInt64 = 0;

begin
  if not SDL_Init(SDL_INIT_VIDEO) then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM, SDL_GetError);
    Halt(1);
  end;

  Window := SDL_CreateWindow('CHXSDL3FPSManager Test', 600, 200, 0);
  if Window = nil then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO, SDL_GetError);
    SDL_Quit;
    Halt(1);
  end;

  Renderer := SDL_CreateRenderer(Window, nil);
  if Renderer = nil then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO, SDL_GetError);
    SDL_DestroyWindow(Window);
    SDL_Quit;
    Halt(1);
  end;

  SDL_SetRenderLogicalPresentation(Renderer, 300, 100,
    SDL_LOGICAL_PRESENTATION_LETTERBOX);

  FPSMang := cCHXSDL3FPSManager.Create(30);
  try
    while Running do
    begin
      SDL_SetRenderDrawColor(Renderer, 100, 149, 237, 255);
      SDL_RenderClear(Renderer);
      SDL_SetRenderDrawColor(Renderer, 255, 255, 255, 255);
      SDL_RenderDebugText(Renderer, 10 , 10, '[ESC] to exit. ');
      SDL_RenderDebugText(Renderer, 10 , 20, 'ARROWS to change FPS. ');
      SDL_RenderDebugText(Renderer, 10 , 40, PChar('FPS: '
        + IntToStr(FPSMang.FPS)));
      SDL_RenderDebugText(Renderer, 10 , 50, PChar('Frame Count: '
        + IntToStr(FPSMang.FrameCount)));
      SDL_RenderDebugText(Renderer, 10 , 60, PChar('Last Frame Compute: '
        + IntToStr(FPSMang.LastCompTime)));
      SDL_RenderDebugText(Renderer, 10 , 70, PChar('Last Frame Total: '
        + IntToStr(FPSMang.LastFrameTime)));
      SDL_RenderDebugText(Renderer, 10 , 80, PChar('ms ahead (- behind): '
        + IntToStr(aDelay)));

      SDL_RenderPresent(Renderer);

      aDelay := FPSMang.Delay;

      while SDL_PollEvent(@Event) do
      begin
        case Event.type_ of
          SDL_EVENT_QUIT:
            Running := False;
          SDL_EVENT_KEY_DOWN:
            case Event.key.key of
              SDLK_ESCAPE, SDLK_Q: Running := False;
              SDLK_UP: FPSMang.FPS := FPSMang.FPS + 5;
              SDLK_DOWN:
                if FPSMang.FPS > 5 then
                  FPSMang.FPS := FPSMang.FPS - 5
                else
                  FPSMang.FPS := 1;
              SDLK_LEFT:
                if FPSMang.FPS > 1 then
                  FPSMang.FPS := FPSMang.FPS - 1;
              SDLK_RIGHT: FPSMang.FPS := FPSMang.FPS + 1;
              otherwise
                ;
            end;
          otherwise
            ;
        end;
      end;
    end;
  finally
    FPSMang.Free;

    SDL_DestroyRenderer(Renderer);
    SDL_DestroyWindow(Window);
    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, 'Program finished.');
    if SDL_GetError <> '' then
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, SDL_GetError);
    if SDL_GetNumAllocations >= 0 then
       SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
         'Mem allocations not freed: %d', [SDL_GetNumAllocations]);

    SDL_Quit;
  end;
end.
{
  This source is free software; you can redistribute it and/or modify it under
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
