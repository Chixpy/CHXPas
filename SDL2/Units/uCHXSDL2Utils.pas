unit uCHXSDL2Utils;
{< Unit with some useful functions for SDL2.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes, Math,
  SDL2,
  uCHXConst;

function SDLColor(const r : CUInt8; const g : CUInt8; const b : CUInt8;
  const a : CUInt8 = 255) : TSDL_Color;
{< Create a TSDL_Color from red, green, blue and alpha values.}
function SDLColor2Str(aColor : TSDL_Color) : string;
{< Write a TSDL_Color to a string.}
function Str2SDLColor(aColor : string) : TSDL_Color;
{< Write a TSDL_Color to a string.}

function SDLRect(const x : CUInt; const y : CUInt; const w : CUInt;
  const h : CUInt) : TSDL_Rect;
{< Create a TSDL_Rect.}

function SDLFRect(const x : CFloat; const y : CFloat; const w : CFloat;
  const h : CFloat) : TSDL_FRect;
{< Create a TSDL_FRect.}

implementation

function SDLColor(const r : CUInt8; const g : CUInt8; const b : CUInt8;
  const a : CUInt8) : TSDL_Color;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

function SDLColor2Str(aColor : TSDL_Color) : string;
begin
  Result := Format('%0:d, %1:d, %2:d, %3:d', [aColor.r, aColor.g, aColor.b, aColor.a]);
end;

function Str2SDLColor(aColor : string) : TSDL_Color;
var
  Components : array of string;
begin
  Components := aColor.Split(',');

  if Length(Components) < 3 then
    raise EConvertError.CreateFmt(krsFmtGenericDef, [{$I %CURRENTROUTINE%},
      '"' + aColor + '" doesn''t have at least 3 components.']);

  Result.R := EnsureRange(StrToInt(Components[0]), 0, 255);
  Result.G := EnsureRange(StrToInt(Components[1]), 0, 255);
  Result.B := EnsureRange(StrToInt(Components[2]), 0, 255);
  if Length(Components) > 3 then
    Result.A := EnsureRange(StrToInt(Components[3]), 0, 255)
  else
    Result.A := 255;
end;

function SDLRect(const x : CUInt; const y : CUInt; const w : CUInt;
  const h : CUInt) : TSDL_Rect;
begin
  Result.x := x;
  Result.y := y;
  Result.w := w;
  Result.h := h;
end;

function SDLFRect(const x : CFloat; const y : CFloat; const w : CFloat;
  const h : CFloat) : TSDL_FRect;
begin
  Result.x := x;
  Result.y := y;
  Result.w := w;
  Result.h := h;
end;

end.
