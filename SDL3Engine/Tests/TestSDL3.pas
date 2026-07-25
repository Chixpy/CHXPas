program TestSDL3;
{$mode objfpc}{$H+}

uses SDL3, SysUtils;

var
  Window: PSDL_Window = nil;
  Renderer: PSDL_Renderer = nil;
  Event: TSDL_Event;
  Running: Boolean = True;

begin
  if not SDL_Init(SDL_INIT_VIDEO) then
  begin
    WriteLn('Error al inicializar SDL3: ', SDL_GetError);
    Halt(1);
  end;

  Window := SDL_CreateWindow('Prueba SDL3 en Android TV', 800, 600, 0);
  if Window = nil then
  begin
    WriteLn('Error al crear Ventana: ', SDL_GetError);
    SDL_Quit;
    Halt(1);
  end;

  Renderer := SDL_CreateRenderer(Window, nil);
  if Renderer = nil then
  begin
    WriteLn('Error al crear el Renderer: ', SDL_GetError);
    SDL_DestroyWindow(Window);
    SDL_Quit;
    Halt(1);    
  end;

  WriteLn('Ventana creada. ESC para salir.');
  while Running do  
  begin
    SDL_SetRenderDrawColor(Renderer, 100, 149, 237, 255);
    SDL_RenderClear(Renderer);

    SDL_RenderPresent(Renderer);

    SDL_Delay(100);
    
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

  SDL_DestroyRenderer(Renderer);
  SDL_DestroyWindow(Window);
  SDL_Quit;
  WriteLn('Programa finalizado correctamente.');  
end.
    
