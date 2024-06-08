unit ucCHXSDL2FontTTF;
{< Unit of cCHXSDL2FontTTF class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  SDL2, SDL2_TTF,
  uCHXConst, uCHXRscStr,
  uaCHXSDL2Font, ucCHXSDL2TextCache;

resourcestring
  rsErrTTFInit = 'TTF_Init Error';

type

  { cCHXSDL2FontTTF }

  cCHXSDL2FontTTF = class(caCHXSDL2Font)
  private
    FCachedTexts : cCHXSDL2TextCacheMap;
    FPFont : PTTF_Font;

  protected
    function StringWidth(const aStr : string) : Integer; override;
    property CachedTexts : cCHXSDL2TextCacheMap read FCachedTexts;

    function CreateStrSurface(const aStr : string;
      const aWidth : CInt) : PSDL_Surface;
    {< Returned PSDL_Surface must be freed. }
    function CreateTextSurface(const aText : TStringList;
      const aWidth : CInt; const aAlign : CInt) : PSDL_Surface;
    {< Returned PSDL_Surface must be freed. }

  public
    property PFont : PTTF_Font read FPFont;

    // Static Text methods
    //---------------------
    function AddStaticStr(const aKey, aStr : string;
      const aWidth : CInt = 0) : Integer; override;
    {< See @inherited. }
    procedure AddStaticText(const aKey : string; const aText : TStringList;
      const aWidth : CInt; const aAlign : CInt = 0); override;
    {< See @inherited. }
    procedure RenderStatic(const aKey : string; const aX, aY : CInt);
      override;
    {< See @inherited. }
    procedure RemoveStatic(const aKey : string); override;
    {< See @inherited. }

    // Dynamic Text routines
    // ---------------------

    function RenderDynStr(const aStr : string; const aX, aY : CInt;
      const aWidth : CInt = 0) : Integer; override;
    {< See @inherited. }
    function RenderDynStrClipped(const aStr : string; const aX, aY : CInt;
      const aWidth : CInt; const aAlign : CInt = 2) : Integer; override;
    {< See @inherited. }
    procedure RenderDynText(const aText : TStringList; const aX, aY,
      aWidth : CInt; const aAlign : CInt = 0); override;
    {< See @inherited. }

    procedure ChangeFontStyle(const aColor : TSDL_Color;
      const aSize : CInt = -1; const aStyle : CInt = -1;
      const aOutline : CInt = -1; const aHinting : CInt = -1); override;
     {< See @inherited.

        Calling this method removes all cached texts and SDL cached glyphs.

        So don't call continuously this method, only to change the font
          globally or when it's loaded.
     }

    constructor Create(const aRenderer : PSDL_Renderer;
      const aFile : string; const aSize : CInt; const aColor : TSDL_Color);
    {< Creates a Font from a TTF file. }
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2FontTTF }

function cCHXSDL2FontTTF.StringWidth(const aStr : string) : Integer;
var
  w, h : CInt;
begin
  Result := 0;
  if TTF_SizeUTF8(PFont, PAnsiChar(aStr), @w, @h) = 0 then
    Result := w;
end;

function cCHXSDL2FontTTF.CreateStrSurface(const aStr : string;
  const aWidth : CInt) : PSDL_Surface;
begin
  if aWidth > 1 then
    Result := TTF_RenderUTF8_Blended_Wrapped(PFont, PAnsiChar(aStr),
      Color, aWidth)
  else
    Result := TTF_RenderUTF8_Blended(PFont, PAnsiChar(aStr), Color);
end;

function cCHXSDL2FontTTF.CreateTextSurface(const aText : TStringList;
  const aWidth : CInt; const aAlign : CInt) : PSDL_Surface;
begin
  TTF_SetFontWrappedAlign(PFont, aAlign);
  Result := TTF_RenderUTF8_Blended_Wrapped(PFont, PAnsiChar(aText.Text),
    Color, aWidth);
end;

function cCHXSDL2FontTTF.AddStaticStr(const aKey, aStr : string;
  const aWidth : CInt): Integer;
var
  TextSFC : PSDL_Surface;
  TextCache : cCHXSDL2TextCache;
begin
  Result := 0;
  if (aKey = EmptyStr) or (aStr = EmptyStr) then Exit;

  TextSFC := CreateStrSurface(aStr, aWidth);
  Result := TextSFC^.w;
  TextCache := cCHXSDL2TextCache.Create(Renderer, TextSFC);
  CachedTexts.Add(aKey, TextCache);
  SDL_FreeSurface(TextSFC);
end;

procedure cCHXSDL2FontTTF.AddStaticText(const aKey : string;
  const aText : TStringList; const aWidth : CInt; const aAlign : CInt);
var
  TextSFC : PSDL_Surface;
  TextCache : cCHXSDL2TextCache;
begin
  if (aKey = EmptyStr) or (not Assigned(aText)) then Exit;

  TextSFC := CreateTextSurface(aText, aWidth, aAlign);
  TextCache := cCHXSDL2TextCache.Create(Renderer, TextSFC);
  CachedTexts.Add(aKey, TextCache);
  SDL_FreeSurface(TextSFC);
end;

procedure cCHXSDL2FontTTF.RenderStatic(const aKey : string;
  const aX, aY : CInt);
var
  TextCache : cCHXSDL2TextCache;
begin
  TextCache := CachedTexts[aKey];
  if Assigned(TextCache) then
    TextCache.Draw(aX, aY);
end;

procedure cCHXSDL2FontTTF.RemoveStatic(const aKey : string);
begin
  CachedTexts.Remove(aKey);
end;

function cCHXSDL2FontTTF.RenderDynStr(const aStr : string; const aX, aY : CInt;
  const aWidth : CInt) : Integer;
var
  TextSFC : PSDL_Surface;
  TextTex : PSDL_Texture;
  aPos : TSDL_Rect;
begin
  Result := 0;
  if aStr = EmptyStr then Exit;

  TextSFC := CreateStrSurface(aStr, aWidth);
  aPos.X := aX;
  aPos.Y := aY;
  aPos.w := TextSFC^.w;
  aPos.h := TextSFC^.h;
  TextTex := SDL_CreateTextureFromSurface(Renderer, TextSFC);

  SDL_RenderCopy(Renderer, TextTex, nil, @aPos);

  Result := TextSFC^.w;
  SDL_FreeSurface(TextSFC);
  SDL_DestroyTexture(TextTex);
end;

function cCHXSDL2FontTTF.RenderDynStrClipped(const aStr : string; const aX,
  aY : CInt; const aWidth : CInt; const aAlign : CInt) : Integer;
var
  TextSFC : PSDL_Surface;
  TextTex : PSDL_Texture;
  aSPos, aTPos : TSDL_Rect;
  Offset, CWidth : Integer;
begin
  Result := 0;
  if aStr = EmptyStr then Exit;
  TextSFC := CreateStrSurface(aStr, 0);

  if (aWidth > 0) and (TextSFC^.w > aWidth) then
  begin
    Offset := TextSFC^.w - aWidth;
    CWidth := aWidth;
  end
  else
  begin
    Offset := 0;
    CWidth := TextSFC^.w;
  end;

  // Source
  case aAlign of
    0 : aSPos.X := 0;
    1 : aSPos.X := Offset div 2;
    else
      aSPos.X := Offset;
  end;
  aSPos.Y := 0;
  aSPos.w := CWidth;
  aSPos.h := TextSFC^.h;

  // Target
  aTPos.X := aX;
  aTPos.Y := aY;
  aTPos.w := CWidth;
  aTPos.h := aSPos.h;

  TextTex := SDL_CreateTextureFromSurface(Renderer, TextSFC);

  SDL_RenderCopy(Renderer, TextTex, @aSPos, @aTPos);

  SDL_FreeSurface(TextSFC);
  SDL_DestroyTexture(TextTex);

  Result := CWidth;
end;

procedure cCHXSDL2FontTTF.RenderDynText(const aText : TStringList;
  const aX, aY, aWidth : CInt; const aAlign : CInt);
var
  TextSFC : PSDL_Surface;
  TextTex : PSDL_Texture;
  aPos : TSDL_Rect;
begin
  if (not Assigned(aText)) then Exit;

  TextSFC := CreateTextSurface(aText, aWidth, aAlign);
  aPos.X := aX;
  aPos.Y := aY;
  aPos.w := TextSFC^.w;
  aPos.h := TextSFC^.h;
  TextTex := SDL_CreateTextureFromSurface(Renderer, TextSFC);

  SDL_RenderCopy(Renderer, TextTex, nil, @aPos);

  SDL_FreeSurface(TextSFC);
  SDL_DestroyTexture(TextTex);
end;

procedure cCHXSDL2FontTTF.ChangeFontStyle(const aColor : TSDL_Color;
  const aSize : CInt; const aStyle : CInt; const aOutline : CInt;
  const aHinting : CInt);
begin
  CachedTexts.Clear;

  FColor := aColor;

  if aStyle <> -1 then
    TTF_SetFontStyle(PFont, aStyle);

  if aOutline <> -1 then
    TTF_SetFontOutline(PFont, aOutline);
  {< Doesn't remove SDL cache, but cached texts must be re-added. }

  if aHinting <> -1 then
    TTF_SetFontHinting(fPFont, aHinting);

  if aSize <> -1 then
  begin
    TTF_SetFontSize(PFont, aSize);
    FLineHeight := TTF_FontLineSkip(FPFont);
  end;
end;

constructor cCHXSDL2FontTTF.Create(const aRenderer : PSDL_Renderer;
  const aFile : string; const aSize : CInt; const aColor : TSDL_Color);
begin
  inherited Create(aRenderer, aSize, aColor);

  if not FileExists(aFile) then
    raise Exception.CreateFmt(krsFmtGenericDef,
      [rsErrTTFInit, Format(rsFileNotFound, [aFile])]);
  if TTF_Init <> 0 then
    raise Exception.CreateFmt(krsFmtGenericDef, [rsErrTTFInit, TTF_GetError]);

  FPFont := TTF_OpenFont(PAnsiChar(aFile), aSize);
  if not assigned(FPFont) then
  begin
    TTF_Quit;
    raise Exception.CreateFmt(krsFmtGenericDef, [rsErrTTFInit, TTF_GetError]);
  end;
  FLineHeight := TTF_FontLineSkip(FPFont); // Actual LineHeight

  FCachedTexts := cCHXSDL2TextCacheMap.Create(True);
end;

destructor cCHXSDL2FontTTF.Destroy;
begin
  FreeAndNil(FCachedTexts);
  TTF_CloseFont(FPFont);
  TTF_Quit;
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
