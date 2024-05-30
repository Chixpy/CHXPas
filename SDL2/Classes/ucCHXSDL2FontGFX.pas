unit ucCHXSDL2FontGFX;
{< Unit of cCHXSDL2FontGFX class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, CTypes, fgl,
  SDL2, SDL2_GFX,
  uaCHXSDL2Font;

type
  cCHXSDL2GenGFXCacheMap = specialize TFPGMapObject<string,
    TStringList>;

  { cCHXSDL2GFXCacheMap}

  { TODO: Make actual texture caches. }

  cCHXSDL2GFXCacheMap = class(cCHXSDL2GenGFXCacheMap)
  public
    constructor Create(AFreeObjects : Boolean);
  end;

  { cCHXSDL2FontGFX }

  cCHXSDL2FontGFX = class(caCHXSDL2Font)
  private
    FCachedTexts : cCHXSDL2GFXCacheMap;

    function DoWrapText(aStr : string; aWidth : CInt) : string; inline;

  protected
    property CachedTexts : cCHXSDL2GFXCacheMap read FCachedTexts;

  public
    // Static Text methods
    //---------------------
    procedure AddStaticStr(const aKey, aStr : string; const aWidth : CInt = 0);
      override;
    {< See @inherited.

    Actually in this child of @inheritedClass, only stores the string it self
      not a texture with the text written. }
    procedure AddStaticText(const aKey : string; const aText : TStringList;
      const aWidth : CInt; const aAlign : CInt = 0); override;
    {< See @inherited.

      Actually in this child of @inheritedClass, only stores the string it self
        not a texture with the text written.

      `aText` lines are copied to cache.
    }
    procedure RenderStatic(const aKey : string; const aX, aY : CInt); override;
    {< See @inherited. }

    // Dynamic Text routines
    // ---------------------

    procedure RenderDynStr(const aStr : string; const aX, aY : CInt;
      const aWidth : CInt = 0); override;
    {< See @inherited. }
    procedure RenderDynText(const aText : TStringList; const aX, aY,
      aWidth : CInt; const aAlign : CInt = 0); override;
    {< See @inherited. }

    procedure ChangeFontStyle(const aColor : TSDL_Color;
      const aSize : CInt = -1; const aStyle : CInt = -1;
      const aOutline : CInt = -1; const aHinting : CInt = -1); override;
    {< See @inherited.

       In this child of @inheritedClass, only aColor can be changed and
         it doesn't remove cached texts.
    }

    constructor Create(const aRenderer : PSDL_Renderer;
      const aColor : TSDL_Color);
    destructor Destroy; override;
  end;


implementation

{ cCHXSDL2GFXCacheMap }

constructor cCHXSDL2GFXCacheMap.Create(AFreeObjects : Boolean);
begin
  inherited Create(AFreeObjects);

  // Case insensitive search
  OnKeyCompare := @CompareText;
end;

{ cCHXSDL2FontTTF }

function cCHXSDL2FontGFX.DoWrapText(aStr : string; aWidth : CInt) : string;
begin
  if aWidth > 8 then  // 8 because it's SDL2_GFX text width
    Result := WrapText(aStr, aWidth div 8)
  else
    Result := aStr;
end;

procedure cCHXSDL2FontGFX.AddStaticStr(const aKey, aStr : string;
  const aWidth : CInt);
var
  aSL : TStringList;
  WStr : string;
begin
  if (aKey = EmptyStr) or (aStr = EmptyStr) then Exit;

  aSL := TStringList.Create;

  WStr := DoWrapText(aStr, aWidth);
  aSL.Text := WStr;

  CachedTexts.Add(aKey, aSL);
end;

procedure cCHXSDL2FontGFX.AddStaticText(const aKey : string;
  const aText : TStringList; const aWidth : CInt; const aAlign : CInt);
var
  aSL : TStringList;
  CStr, WStr : string;
begin
  if (aKey = EmptyStr) or (not assigned(aText)) then Exit;

  aSL := TStringList.Create;

  for CStr in aText do
  begin
    WStr := DoWrapText(CStr, aWidth);

    aSL.AddText(WStr);
    // Adding and empty line to simulate extra line between paragraphs of
    //   SDL_TTF.
    aSL.Add('');
  end;

  CachedTexts.Add(aKey, aSL);
end;

procedure cCHXSDL2FontGFX.RenderStatic(const aKey : string; const aX,
  aY : CInt);
var
  aSL : TStringList;
  i : Integer;
  WStr : String;
begin
  if (aKey = EmptyStr) then Exit;

  aSL := CachedTexts.KeyData[aKey];
  if not Assigned(aSL) then Exit;

  // Cached texts have a line of separation between paragraphs.
  i := 0;
  while i < aSL.Count do
  begin
    WStr := aSl[i];

    if WStr <> EmptyStr then
      stringRGBA(Renderer, aX, LinePosY(i,aY), PChar(WStr), Color.r, Color.g,
        Color.b, Color.a);

    Inc(i);
  end;
end;

procedure cCHXSDL2FontGFX.RenderDynStr(const aStr : string; const aX,
  aY : CInt; const aWidth : CInt);
begin
  if (aStr = EmptyStr) then Exit;

  // Little trick :-P
  AddStaticStr('RenderDynStr', aStr, aWidth);
  RenderStatic('RenderDynStr', aX, aY);
  CachedTexts.Remove('RenderDynStr');
end;

procedure cCHXSDL2FontGFX.RenderDynText(const aText : TStringList; const aX,
  aY, aWidth : CInt; const aAlign : CInt);
begin
  if not assigned(aText) then Exit;

  // Little trick :-P
  AddStaticText('RenderDynText', aText, aWidth, aAlign);
  RenderStatic('RenderDynText', aX, aY);
  CachedTexts.Remove('RenderDynText');
end;

procedure cCHXSDL2FontGFX.ChangeFontStyle(const aColor : TSDL_Color;
  const aSize : CInt; const aStyle : CInt; const aOutline : CInt;
  const aHinting : CInt);
begin
  FColor := aColor;
  // Other properties sucesfully ignored
end;

constructor cCHXSDL2FontGFX.Create(const aRenderer : PSDL_Renderer;
  const aColor : TSDL_Color);
begin
  // Althogth actual font size is 8, we will define lines of
  //   2 pixels larger to make a little separation between chars
  inherited Create(aRenderer, 10, aColor);

  FCachedTexts := cCHXSDL2GFXCacheMap.Create(True);
end;

destructor cCHXSDL2FontGFX.Destroy;
begin
  FCachedTexts.Free;
  inherited Destroy;
end;

end.
