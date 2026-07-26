program CHXSDL3Window;
{<
  Test program for cCHXSDL3Window.

  We are only testing cCHXSDL3Window only, althought it has a
    cCHXSDL3Renderer asociated.
}
{$mode objfpc}{$H+}

uses
  SysUtils, SDL3, ucCHXSDL3Window;

var
  PSDLWindow: cCHXSDL3Window;
  Event: TSDL_Event;
  Running: Boolean = True;

begin
  PSDLWindow := cCHXSDL3Window.Create('CHXSDL3Window Test', 300, 200, 900, 600);
  try
    Running := True;
    while Running do 
    begin
      SDL_SetRenderDrawColor(PSDLWindow.PSDLRenderer, 100, 149, 237, 255);
      SDL_RenderClear(PSDLWindow.PSDLRenderer);

      SDL_SetRenderDrawColor(PSDLWindow.PSDLRenderer, 255, 255, 255, 255);
      SDL_RenderDebugText(PSDLWindow.PSDLRenderer, 100, 100, 'ESC to exit.');

      SDL_RenderPresent(PSDLWindow.PSDLRenderer);

      SDL_Delay(1000);
      
      while SDL_PollEvent(@Event) do
      begin
        case Event.type_ of
          SDL_EVENT_QUIT:
            Running := False;
          SDL_EVENT_KEY_DOWN:
            if Event.key.key = SDLK_ESCAPE then
              Running := False;
        otherwise
          ;
        end;
      end;
    end;
    
  finally
    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, 'Program finished.');
    
    if SDL_GetError <> '' then
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, SDL_GetError);
    if SDL_GetNumAllocations >= 0 then
       SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
         'Mem allocations not freed: %d', [SDL_GetNumAllocations]); 
         
    SDL_Quit;
  end;
end.
