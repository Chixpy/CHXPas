unit uaCHXSDL2Font;
{< Unit of caCHXSDL2Font abstract class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  SDL2;

type
  { caCHXSDL2Font }

  caCHXSDL2Font = class
  private
    FRenderer : PSDL_Renderer;

  protected
    FColor : TSDL_Color;
    FLineHeight : CInt;

    property Renderer : PSDL_Renderer read FRenderer;

    function StringWidth(const aStr : string) : Integer; virtual; abstract;

  public
    property Color : TSDL_Color read FColor;
    {< Current text color. }
    property LineHeight : CInt read FLineHeight;
    {< Recommended distance between lines. }

    function LinePosX(const aStr : string;
      const aMargin : LongInt = 0) : CInt; inline;
    {< Return the X coord position where next character will be after a string
         with a left margin.
    }
    function LinePosY(const aLine : LongInt;
      const aMargin : LongInt = 0) : CInt; inline;
    {< Return the Y coord position where a line of text will be from appliying
         a top margin.

      First line is 0, i.e YPosAtLine(0) then Result := aMargin.

      @param(aLine Line where we want to put the text.)
      @param(aMargin Top margin in pixels of line 0.)
    }

    // Static Text methods
    //---------------------

    function AddStaticStr(const aKey, aStr : string;
      const aWidth : CInt = 0) : Integer; virtual; abstract;
    {< Add a cached string for faster draw.

       @param(aKey Key to reference the string.)
       @param(aStr String to store in cache.)
       @param(aWidth Width in pixel to wordwrap the string. `0` don't wordwrap
         the string.)

       @returns(Actual width of the added text in pixels.)
    }
    procedure AddStaticText(const aKey : string; const aText : TStringList;
      const aWidth : CInt; const aAlign : CInt = 0); virtual; abstract;
    {< Add a cached text from a TStringList for faster draw latter.

       Lines in aText TStringList are separated with an extra empty line.

       @param(aKey Key to reference the string.)
       @param(aText TStringList lines to store in cache.)
       @param(aWidth Width in pixel to wordwrap the string. `0` don't wordwrap
         the string.)
       @param(aAlign   @definitionList(@itemSpacing(Compact)
         @itemLabel(0 /  TTF_WRAPPED_ALIGN_LEFT)
         @item(Left align.)

         @itemLabel(1 / TTF_WRAPPED_ALIGN_CENTER)
         @item(Center align.)

         @itemLabel(3 / TTF_WRAPPED_ALIGN_RIGHT)
         @item(Right align.)
       )
    }

    procedure RenderStatic(const aKey : string; const aX, aY : CInt);
      virtual; abstract;
    {< Renders a cached string / text.

       @param(aKey Key to reference the string.)
       @param(aX,aY Top left position to draw the text.)
    }

    procedure RemoveStatic(const aKey : string); virtual; abstract;
    {< Removes a static string/text from cache. }

    // Dynamic Text routines
    // ---------------------

    function RenderDynStr(const aStr : string; const aX, aY : CInt;
      const aWidth : CInt = 0) : Integer; virtual; abstract;
    {< Render a non cached string.

       @param(aStr String write.)
       @param(aX,aY Top left position to draw the text.)
       @param(aWidth Width in pixel to wordwrap the string. `0` don't wordwrap
         the string.)
       @returns(Actual width of the rendered text in pixels.)
    }
    function RenderDynStrClipped(const aStr : string;
      const aX, aY : CInt; const aWidth : CInt;
      const aAlign : CInt = 2) : Integer; virtual; abstract;
    {< Render a non cached string but clipped to aWidth in pixels.

       @param(aStr String write.)
       @param(aX,aY Top left position to draw the text.)
       @param(aWidth Width .)
       @param(aAlign   @definitionList(@itemSpacing(Compact)
         @itemLabel(0 /  TTF_WRAPPED_ALIGN_LEFT)
         @item(Show left side of the string.)

         @itemLabel(1 / TTF_WRAPPED_ALIGN_CENTER)
         @item(Show center side of the string.)

         @itemLabel(3 / TTF_WRAPPED_ALIGN_RIGHT)
         @item(Show right side of the string.)
       )
       @returns(Actual width of the rendered text in pixels.)
    }
    procedure RenderDynText(const aText : TStringList;
      const aX, aY, aWidth : CInt; const aAlign : CInt = 0); virtual; abstract;
    {< Render a non cached text.

       Lines in aText TStringList are separated with an extra empty line.

       @param(aText TStringList lines to store in cache.)
       @param(aWidth Width in pixel to wordwrap the string. `0` don't wordwrap
         the string.)
       @param(aAlign   @definitionList(@itemSpacing(Compact)
         @itemLabel(0 /  TTF_WRAPPED_ALIGN_LEFT)
         @item(Left align.)

         @itemLabel(1 / TTF_WRAPPED_ALIGN_CENTER)
         @item(Center align.)

         @itemLabel(3 / TTF_WRAPPED_ALIGN_RIGHT)
         @item(Right align.)
       )
    }

    procedure ChangeFontStyle(const aColor : TSDL_Color;
      const aSize : CInt = -1; const aStyle : CInt = -1;
      const aOutline : CInt = -1; const aHinting : CInt = -1);
      virtual; abstract;
    {< Change current font style.

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
      const aSize : CInt; const aColor : TSDL_Color);
    destructor Destroy; override;
  end;

implementation

{ caCHXSDL2Font }

function caCHXSDL2Font.LinePosX(const aStr : string;
  const aMargin : LongInt) : CInt;
begin
  Result := StringWidth(aStr) + aMargin;
end;

function caCHXSDL2Font.LinePosY(const aLine : LongInt;
  const aMargin : LongInt) : CInt;
begin
  Result := LineHeight * aLine + aMargin;
end;

constructor caCHXSDL2Font.Create(const aRenderer : PSDL_Renderer;
  const aSize : CInt; const aColor : TSDL_Color);
begin
  FRenderer := aRenderer;
  FLineHeight := aSize;
  FColor := aColor;
end;

destructor caCHXSDL2Font.Destroy;
begin
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
