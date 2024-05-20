unit ucCHXSDL2TextCache;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, CTypes,
  SDL2;

type

  { cCHXSDL2TextCache }

  cCHXSDL2TextCache = class
  private
    FRenderer : PSDL_Renderer;
    FTextTex : PSDL_Texture;

  protected
    PosSize : TSDL_Rect;
    {< Size and last position. }

  public
    property TextTex : PSDL_Texture read FTextTex;
    property Renderer : PSDL_Renderer read FRenderer;

    function Width : CInt;
    function Height : CInt;

    procedure Draw(const x: integer; const y: integer);

    constructor Create(const aRenderer : PSDL_Renderer;
      const aTxtSurface : PSDL_Surface);
    destructor Destroy; override;
  end;

  cCHXSDL2GenTextCacheMap = specialize TFPGMapObject<string,
    cCHXSDL2TextCache>;

  { cCHXSDL2TextCacheMap }

  cCHXSDL2TextCacheMap = class(cCHXSDL2GenTextCacheMap)
  public
    constructor Create(AFreeObjects : Boolean);
  end;

implementation

{ cCHXSDL2TextCache }

function cCHXSDL2TextCache.Width : CInt;
begin
  Result := PosSize.w;
end;

function cCHXSDL2TextCache.Height : CInt;
begin
  Result := PosSize.h;
end;

procedure cCHXSDL2TextCache.Draw(const x : integer; const y : integer);
begin
  PosSize.x := x;
  PosSize.y := y;
  SDL_RenderCopy(Renderer, TextTex, nil, @PosSize);
end;

constructor cCHXSDL2TextCache.Create(const aRenderer : PSDL_Renderer;
  const aTxtSurface : PSDL_Surface);
begin
  FRenderer := aRenderer;
  PosSize.w := aTxtSurface^.w;
  PosSize.h := aTxtSurface^.h;
  FTextTex := SDL_CreateTextureFromSurface(Renderer, aTxtSurface);
end;

destructor cCHXSDL2TextCache.Destroy;
begin
  SDL_DestroyTexture(FTextTex);
  inherited Destroy;
end;

{ cCHXSDL2TextCacheMap }

constructor cCHXSDL2TextCacheMap.Create(AFreeObjects : Boolean);
begin
  inherited Create(AFreeObjects);

  // Case insensitive search
  OnKeyCompare := @CompareText;
end;

end.
