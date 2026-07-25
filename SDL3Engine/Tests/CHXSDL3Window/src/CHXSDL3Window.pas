program CHXSDL3Window;
{<
  Test program for cCHXSDL3Window.
}
{$mode objfpc}{$H+}

uses
  SysUtils, SDL3, ucCHXSDL3Window;

var
  Window: cCHXSDL3Window;
  Event: TSDL_Event;
  Running: Boolean = True;

begin
  Window := cCHXSDL3Window.Create('CHXSDL3Window Test', 300, 200, 900, 600);
  try
    if (not Assigned(Window.PSDLWindow)) or
      (not Assigned(Window.PSDLWindow)) then
      Exit;
    WriteLn('Ptr Window = ', PtrUInt(Window));
    WriteLn('Ptr PSDLWindow = ', PtrUInt(Window.PSDLWindow));
    WriteLn('Ptr PSDLRenderer = ', PtrUInt(Window.PSDLRenderer));
    WriteLn('Ventana creada. ESC para salir.');
    
    Running := True;
    while Running do 
    begin
      SDL_SetRenderDrawColor(Window.PSDLRenderer, 100, 149, 237, 255);
      SDL_RenderClear(Window.PSDLRenderer);

      SDL_SetRenderDrawColor(Window.PSDLRenderer, 255, 255, 255, 255);
      SDL_RenderDebugText(Window.PSDLRenderer, 100, 100, 'ESC to exit.');

      SDL_RenderPresent(Window.PSDLRenderer);

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
    WriteLn('SDL_GetError: ', SDL_GetError); 
    SDL_Quit;
  end;
end.
