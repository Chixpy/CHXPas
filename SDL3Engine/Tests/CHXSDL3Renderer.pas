program CHXSDL3Renderer;
{$mode objfpc}{$H+}

uses SysUtils, CTypes, SDL3, ucCHXSDL3Renderer;

var
  SDLWindow: PSDL_Window = nil;
  Renderer: cCHXSDL3Renderer;
  Event: TSDL_Event;
  Running: Boolean = True;

begin
  if not SDL_Init(SDL_INIT_VIDEO) then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM, SDL_GetError);
    Halt(1);
  end;

  SDLWindow := SDL_CreateWindow('CHXSDL3Renderer Test', 600, 400, 0);
  if SDLWindow = nil then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO, SDL_GetError);
    SDL_Quit;
    Halt(1);
  end;

  Renderer := cCHXSDL3Renderer.Create(SDLWindow, nil);
  try
    SDL_SetRenderLogicalPresentation(Renderer, 300, 200,
      SDL_LOGICAL_PRESENTATION_LETTERBOX);

    while Running do
    begin
      Renderer.SetColor(100, 149, 237, 255);
      SDL_RenderClear(Renderer);
      Renderer.SetColor(255, 255, 255, 255);
      SDL_RenderDebugText(Renderer.SDLRenderer, 10 , 10, '[ESC] to exit. ');

      SDL_RenderPresent(Renderer.SDLRenderer);

      SDL_Delay(100); // Un respiro

      while SDL_PollEvent(@Event) do
      begin
        case Event.type_ of
          SDL_EVENT_QUIT:
            Running := False;
          SDL_EVENT_KEY_DOWN:
            case Event.key.key of
              SDLK_ESCAPE, SDLK_Q: Running := False;
              //SDLK_UP:
              //SDLK_DOWN:
              //SDLK_LEFT:
              //SDLK_RIGHT: FPSMang.FPS := FPSMang.FPS + 1;
              otherwise
                ;
            end;
          otherwise
            ;
        end;
      end;
    end;
  finally
    Renderer.Free;
    SDL_DestroyWindow(SDLWindow);

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, 'Program finished.');
    if SDL_GetError <> '' then
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, SDL_GetError);
    if SDL_GetNumAllocations >= 0 then
       SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
         'Mem allocations not freed: %d', [SDL_GetNumAllocations]);

    SDL_Quit;
  end;
end.
