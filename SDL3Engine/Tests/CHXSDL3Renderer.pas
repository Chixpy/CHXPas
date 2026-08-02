program CHXSDL3Renderer;
{<
  Little program with to test cCHXSDL3Renderer (without cCHXSDL3Window
  or cCHXSDL3Engine) with some random test while developing it.

  ToDo: A dedicated program to test primitives must be done.
}
{$mode objfpc}{$H+}

uses
  SysUtils, CTypes, // FPC RTL
  SDL3, // SDL3
  uCHXSDL3TypeHelpers, ucCHXSDL3Renderer; //CHXSDL3Engine

const
  WinW = 800;
  WinH = 800;

var
  SDLWindow: PSDL_Window = nil;
  Renderer: cCHXSDL3Renderer;
  Event: TSDL_Event;
  Running: Boolean = True;
  Scale: Integer = 8; //(100x100)

  aAngle: CFloat = 0;
  NSides: Integer = 7;
  // PArr: TSDLFPointDynArray;

begin
  if not SDL_Init(SDL_INIT_VIDEO) then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_SYSTEM, SDL_GetError);
    Halt(1);
  end;

  SDLWindow := SDL_CreateWindow('CHXSDL3Renderer Test', WinW, WinH, 0);
  if SDLWindow = nil then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO, SDL_GetError);
    SDL_Quit;
    Halt(1);
  end;

  // Setup
  // SetLength(PArr, 5);
  // PArr[0] := SDLFPoint(10, 20);
  // PArr[1] := SDLFPoint(40, 35);
  // PArr[2] := SDLFPoint(80, 13);
  // PArr[3] := SDLFPoint(25, 60);
  // PArr[4] := SDLFPoint(50, 50);

  // In cCHXSDL3Engine it will be called only Render
  Renderer := cCHXSDL3Renderer.Create(SDLWindow, nil);
  try
    // Setting a x16 scale to see points better. 
    SDL_SetRenderLogicalPresentation(Renderer.SDLRenderer,
      WinW div Scale, WinH div Scale, SDL_LOGICAL_PRESENTATION_LETTERBOX);

    while Running do
    begin
      // Drawing some random things with cCHXSDL3Renderer
      // ---------------------------------------------------------------------
      Renderer.SetDrawColor(0, 0, 0); // Sets Draw color
      Renderer.Clear; // Restores white as draw color.

      Renderer.SetDrawColor(1, 0, 1, 1); // Sets Draw color

      // Renderer.Polygon(PArr, SDLFColor(1, 1, 0, 0.5), SDLFColor(1, 0, 1, 0.5));
      Renderer.RegPolySS(50, 50, 30, NSides,
        SDLFColor(1, 1, 0, 0.5), SDLFColor(1, 0, 1, 0.5), aAngle);
      // Renderer.RegPolyCCFillOnly(50, 50, 40, NSides, aAngle);

      aAngle += 0.02;

      // ---------------------------------------------------------------------
      Renderer.SetDrawColor(1, 1, 1); // Sets Draw color
      SDL_RenderDebugText(Renderer.SDLRenderer, 1, 51,
        '[ESC] or [Q] to exit. ');
      SDL_RenderDebugText(Renderer.SDLRenderer, 1, 59,
        '[J] or [K] to change scale. ');
      SDL_RenderDebugText(Renderer.SDLRenderer, 1, 68,
        '[N] or [M] to change # Sides. ');

      SDL_RenderPresent(Renderer.SDLRenderer); // Update Screen
      SDL_Delay(100); // Wait 100 ms.; 0.1 seconds

      while SDL_PollEvent(@Event) do
      begin
        case Event.type_ of

          SDL_EVENT_QUIT:
            Running := False;

          SDL_EVENT_KEY_DOWN:
            case Event.key.key of
              SDLK_ESCAPE, SDLK_Q: Running := False;
              SDLK_K:
              begin
                Inc(Scale);
                SDL_SetRenderLogicalPresentation(Renderer.SDLRenderer,
                  WinW div Scale, WinH div Scale,
                  SDL_LOGICAL_PRESENTATION_LETTERBOX);
              end;
              SDLK_J:
              begin
                if Scale > 1 then
                begin
                  Dec(Scale);
                  SDL_SetRenderLogicalPresentation(Renderer.SDLRenderer,
                    WinW div Scale, WinH div Scale,
                    SDL_LOGICAL_PRESENTATION_LETTERBOX);
                end;
              end;
              SDLK_N: Inc(NSides);
              SDLK_M: if NSides > 1 then Dec(NSides);
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
