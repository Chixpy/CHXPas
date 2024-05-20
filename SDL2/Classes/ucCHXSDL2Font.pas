unit ucCHXSDL2Font;
{< Unit of cCHXSDL2Font class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  SDL2, SDL2_TTF,
  uCHXConst, uCHXRscStr,
  ucCHXSDL2TextCache;

resourcestring
  rsErrTTFInit = 'TTF_Init Error';

type

  { cCHXSDL2Font }

  cCHXSDL2Font = class
  private
    FCachedTexts : cCHXSDL2TextCacheMap;
    FColor : TSDL_Color;
    FPFont : PTTF_Font;
    FLineHeight : CInt;
    FRenderer : PSDL_Renderer;

  protected
    property Renderer : PSDL_Renderer read FRenderer;

    property CachedTexts : cCHXSDL2TextCacheMap read FCachedTexts;

    function CreateStrSurface(const aStr : string) : PSDL_Surface;
    {< Returned PSDL_Surface must be freed. }
    function CreateTextSurface(const aText : TStringList;
      const aWidth : CInt; const aAlign : CInt) : PSDL_Surface;
    {< Returned PSDL_Surface must be freed. }

  public
    property PFont : PTTF_Font read FPFont;
    property Color : TSDL_Color read FColor;
    {< Current text color. Read only. }
    property LineHeight : CInt read FLineHeight;
    {< Distance between lines. }

    function YPosAtLine(const aLine : LongInt;
      const aMargin : LongInt = 0) : CInt;
    {< Return the Y coord position where a line of text will be from a
         top margin.

      First line is 0, i.e YPosAtLine(0) then Result := aMargin. }

    // Static Text routines
    // --------------------

    procedure AddStaticStr(const aKey : string; const aStr : string);
    {< Add a cached string for faster draw.

       No wordwrap is applied. }
    procedure AddStaticText(const aKey : string; const aText : TStringList;
      const aWidth : CInt; const aAlign : CInt = TTF_WRAPPED_ALIGN_LEFT);
    {< Add a cached text for faster draw.

       Wordwrap is applied to aWidth, and lines in TStringList are separated
         with an extra empty line. }

    procedure RenderStatic(const aKey : string; const aX : CInt;
      const aY : CInt);
    {< Renders a cached string / text. }

    // Dynamic Text routines
    // ---------------------

    procedure RenderDynStr(const aStr : string; const aX : CInt;
      const aY : CInt);
    {< Render a non cached string. SDL internally caches individual glyphs
         but it's slower.

       No wordwrap is applied. }
    procedure RenderDynText(const aText : TStringList;
      const aX : CInt; const aY : CInt; const aWidth : CInt;
      const aAlign : CInt = TTF_WRAPPED_ALIGN_LEFT);
    {< Render a non cached text. SDL internally caches individual glyphs.

      Wordwrap is applied to aWidth, and lines in TStringList are separated
        with an extra empty line.

      @Param(aStyle New font style. It can be a sum of
        `TTF_WRAPPED_ALIGN_LEFT`, `TTF_WRAPPED_ALIGN_CENTER` or
        `TTF_WRAPPED_ALIGN_RIGHT`.)
    }

    procedure ChangeFontStyle(const aColor : TSDL_Color;
      const aSize : CInt; const aStyle : CInt; const aOutline : CInt;
      const aHinting : CInt);
    {< Change current font style.

       Do NOT call continuosly, this method removes all cached glyphs by SDL
         and all static texts wich must be readded.

       So, basically is to change the font globally or default
         cCHXSDL2Engine font on Setup method.

       Any -1 value means no change from current values, except aColor. If
         you don't want change color pass its Font.Color as parameter.

       @Param(aColor New font color.)
       @Param(aSize New font size.)
       @Param(aStyle New font style. It can be a sum of `TTF_STYLE_NORMAL`,
         `TTF_STYLE_BOLD`, `TTF_STYLE_ITALIC`, `TTF_STYLE_UNDERLINE`,
         `TTF_STYLE_STRIKETHROUGH`.)
       @Param(aOutline New font outline size.)
       @Param(aHinting New font hinting. It can be: `TTF_HINTING_NORMAL`,
         `TTF_HINTING_LIGHT`, `TTF_HINTING_MONO`, `TTF_HINTING_NONE`,
         `TTF_HINTING_LIGHT_SUBPIXEL`.)
    }

    constructor Create(const aRenderer : PSDL_Renderer;
      const aFile : string; const aSize : CInt; const aColor : TSDL_Color);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Font }

function cCHXSDL2Font.CreateStrSurface(const aStr : string) : PSDL_Surface;
begin
  Result := TTF_RenderUTF8_Blended(PFont, PAnsiChar(aStr), Color);
end;

function cCHXSDL2Font.CreateTextSurface(const aText : TStringList;
  const aWidth : CInt; const aAlign : CInt) : PSDL_Surface;
begin
  TTF_SetFontWrappedAlign(PFont, aAlign);
  Result := TTF_RenderUTF8_Blended_Wrapped(PFont, PAnsiChar(aText.Text),
    Color, aWidth);
end;

function cCHXSDL2Font.YPosAtLine(const aLine : LongInt;
  const aMargin : LongInt) : CInt;
begin
  Result := LineHeight * aLine + aMargin;
end;

procedure cCHXSDL2Font.AddStaticStr(const aKey : string; const aStr : string);
var
  TextSFC : PSDL_Surface;
  TextCache : cCHXSDL2TextCache;
begin
  TextSFC := CreateStrSurface(aStr);
  TextCache := cCHXSDL2TextCache.Create(Renderer,TextSFC);
  CachedTexts.Add(aKey, TextCache);
  SDL_FreeSurface(TextSFC);
end;

procedure cCHXSDL2Font.AddStaticText(const aKey : string;
  const aText : TStringList; const aWidth : CInt; const aAlign : CInt);
var
  TextSFC : PSDL_Surface;
  TextCache : cCHXSDL2TextCache;
begin
  TextSFC := CreateTextSurface(aText, aWidth, aAlign);
  TextCache := cCHXSDL2TextCache.Create(Renderer,TextSFC);
  CachedTexts.Add(aKey, TextCache);
  SDL_FreeSurface(TextSFC);
end;

procedure cCHXSDL2Font.RenderStatic(const aKey : string; const aX : CInt;
  const aY : CInt);
var
  TextCache : cCHXSDL2TextCache;
begin
  TextCache := CachedTexts[aKey];
  if Assigned(TextCache) then
    TextCache.Draw(aX, aY);
end;

procedure cCHXSDL2Font.RenderDynStr(const aStr : string;
  const aX : CInt; const aY : CInt);
var
  TextSFC : PSDL_Surface;
  TextTex : PSDL_Texture;
  aPos : TSDL_Rect;
begin
  TextSFC := CreateStrSurface(aStr);
  aPos.x := aX;
  aPos.y := aY;
  aPos.w := TextSFC^.w;
  aPos.h := TextSFC^.h;
  TextTex := SDL_CreateTextureFromSurface(Renderer, TextSFC);

  SDL_RenderCopy(Renderer, TextTex, nil, @aPos);

  SDL_FreeSurface(TextSFC);
  SDL_DestroyTexture(TextTex);
end;

procedure cCHXSDL2Font.RenderDynText(const aText : TStringList;
  const aX : CInt; const aY : CInt; const aWidth : CInt; const aAlign : CInt);
var
  TextSFC : PSDL_Surface;
  TextTex : PSDL_Texture;
  aPos : TSDL_Rect;
begin
  TextSFC := CreateTextSurface(aText, aWidth, aAlign);
  aPos.x := aX;
  aPos.y := aY;
  aPos.w := TextSFC^.w;
  aPos.h := TextSFC^.h;
  TextTex := SDL_CreateTextureFromSurface(Renderer, TextSFC);

  SDL_RenderCopy(Renderer, TextTex, nil, @aPos);

  SDL_FreeSurface(TextSFC);
  SDL_DestroyTexture(TextTex);
end;

procedure cCHXSDL2Font.ChangeFontStyle(const aColor : TSDL_Color;
  const aSize : CInt; const aStyle : CInt; const aOutline : CInt;
  const aHinting : CInt);
begin
  CachedTexts.Clear;

  FColor := aColor;

  if aStyle <> -1 then
    TTF_SetFontStyle(PFont, aStyle);

  if aOutline <> -1 then
    TTF_SetFontOutline(PFont, aOutline);
  {< Doesn't remove SDL cache, but cached texts must be readded. }

  if aHinting <> -1 then
    TTF_SetFontHinting(fPFont, aHinting);

  if aSize <> -1 then
  begin
    TTF_SetFontSize(PFont, aSize);
    FLineHeight := TTF_FontLineSkip(FPFont);
  end;
end;

constructor cCHXSDL2Font.Create(const aRenderer : PSDL_Renderer;
  const aFile : string; const aSize : CInt; const aColor : TSDL_Color);
begin
  if not FileExists(aFile) then
    raise Exception.CreateFmt(krsFmtGenericDef,
      [rsErrTTFInit, Format(rsFileNotFound, [aFile])]);
  if TTF_Init <> 0 then
    raise Exception.CreateFmt(krsFmtGenericDef, [rsErrTTFInit, TTF_GetError]);

  FRenderer := aRenderer;

  FColor := aColor;
  FPFont := TTF_OpenFont(PAnsiChar(aFile), aSize);
  if not assigned(FPFont) then
  begin
    TTF_Quit;
    raise Exception.CreateFmt(krsFmtGenericDef, [rsErrTTFInit, TTF_GetError]);
  end;
  FLineHeight := TTF_FontLineSkip(FPFont);

  FCachedTexts := cCHXSDL2TextCacheMap.Create(True);
end;

destructor cCHXSDL2Font.Destroy;
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
