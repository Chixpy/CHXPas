unit ucCHXSDL3Renderer;
{< Unit of cCHXSDL3Renderer class.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
{$inline ON}

interface

uses
  SysUtils, CTypes,
  SDL3;

resourcestring
  rsCHXSDL3RendererNilError = 'cCHXSDL3Renderer.Create: %s is nil. %s';

type
  {
    cCHXSDL3Renderer class.

    Wrapper of SDL Renderer expanded to draw more primitives.

    It doesn't call `SDL_Init[SubSystem]` or `SDL_Quit[SubSystem]` as it 
      expects at least a SDL Window already created.

    All methods are functions with boolean result as SDL renderer funtions. 
  }

  cCHXSDL3Renderer = class
  protected
    PrevBlendMode: TSDL_BlendMode; // Previous blend mode on changing color
  
  public
    SDLRenderer: PSDL_Renderer; //< Actual SDL Renderer.
    FreeRenderer: Boolean; //< Free renderer on Destroy;

    constructor Create(const PSDLWindow: PSDL_Window;
      const Drivers: PAnsiChar = nil); overload;
    {<
      Create with asociated SDL Window. Usual SDL Renderer parameters.
    }
    constructor Create(const PSDLRenderer: PSDL_Renderer;
      const FreeOnDestroy: Boolean); overload;
    {<
      Assigns a already created SDL Renderer.
    }

    function SetColor(const aValue: TSDL_Color): Boolean; overload; inline;
    function SetColor(const R, G, B: Byte; const A: Byte = 255): Boolean; 
      overload;
    function SetColor(const Grey: Byte; const A: Byte = 255): Boolean; 
      overload;

    destructor Destroy; override;
  end;
 
implementation

{ cCHXSDL3Renderer }

constructor cCHXSDL3Renderer.Create(const PSDLWindow: PSDL_Window;
  const Drivers: PAnsiChar); overload;
begin
  if not assigned(PSDLWindow) then
    raise Exception.CreateFmt(rsCHXSDL3RendererNilError, ['PSDLWindow']);

  Create(SDL_CreateRenderer(PSDLWindow, Drivers), True);
end;

constructor cCHXSDL3Renderer.Create(const PSDLRenderer: PSDL_Renderer;
  const FreeOnDestroy: Boolean);
begin
  if not assigned(PSDLRenderer) then
    raise Exception.CreateFmt(rsCHXSDL3RendererNilError, ['PSDLRenderer']);

  inherited Create;
  SDLRenderer := PSDLRenderer;
  FreeRenderer := FreeOnDestroy;
  PrevBlendMode := SDL_BLENDMODE_NONE;
end;

function cCHXSDL3Renderer.SetColor(const aValue: TSDL_Color): Boolean;
begin
  Result := SetColor(aValue.R, aValue.G, aValue.B, aValue.A);
end;

function cCHXSDL3Renderer.SetColor(const R, G, B, A: Byte): Boolean;
var
  PrevR, PrevG, PrevB, PrevA: Byte;
begin
  Result := SDL_GetRenderDrawColor(SDLRenderer,
    @PrevR, @PrevG, @PrevB, @PrevA);

  if (PrevR = R) and (PrevG = G) and (PrevB = B) and (PrevA = A) then 
    Exit(Result);

  if (PrevA = 255) and (A < 255) then
    Result := Result and
      SDL_SetRenderDrawBlendMode(SDLRenderer, PrevBlendMode)
  else if (PrevA < 255) and (A = 255) then
  begin
    Result := Result and
      SDL_GetRenderDrawBlendMode(SDLRenderer, @PrevBlendMode);
    Result := Result and
      SDL_SetRenderDrawBlendMode(SDLRenderer, SDL_BLENDMODE_NONE);
  end;

  Result := Result and SDL_SetRenderDrawColor(SDLRenderer, R, G, B, A);
end;

function cCHXSDL3Renderer.SetColor(const Grey: Byte; const A: Byte): Boolean;
begin
  Result := SetColor(Grey, Grey, Grey, A);
end;

destructor cCHXSDL3Renderer.Destroy;
begin
  if FreeRenderer then
    SDL_Free(SDLRenderer);

  inherited;
end;

end.
