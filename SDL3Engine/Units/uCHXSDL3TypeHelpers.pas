unit uCHXSDL3TypeHelpers;
{< Unit with helpers for SDL3 types.

  ToDo: Actually implement helpers for SDL3 Types.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
{$inline ON}

interface

uses
  SysUtils, CTypes, Math, FGL,
  SDL3;

type
  {
    Dynamic lists.
  }
(*
  cSDLColorList = specialize TFPGList<TSDL_Color>;
  //< List of TSDL_Color.
  cSDLPointList = specialize TFPGList<TSDL_Point>;
  //< List of TSDL_Point.
  cSDLFPointList = specialize TFPGList<TSDL_FPoint>;
  //< List of TSDL_FPoint.
  cSDLRectList = specialize TFPGList<TSDL_Rect>;
  //< List of TSDL_Rect.
  cSDLFRectList = specialize TFPGList<TSDL_FRect>;
  //< List of TSDL_FRect.
  cSDLVertexList = specialize TFPGList<TSDL_Vertex>;
  //< List of TSDL_Vertex.

operator = (const a, b: TSDL_Color): Boolean;
operator = (const a, b: TSDL_Point): Boolean;
operator = (const a, b: TSDL_FPoint): Boolean;
operator = (const a, b: TSDL_Rect): Boolean;
operator = (const a, b: TSDL_FRect): Boolean;
operator = (const a, b: TSDL_Vertex): Boolean;
*)

  {
    Dynamic Arrays.
  }
  
  TSDLColorDynArray = specialize TArray<TSDL_Color>;
  //< Dynamic array of TSDL_Color.
  TSDLPointDynArray = specialize TArray<TSDL_Point>;
  //< Dynamic array of TSDL_Point.
  TSDLFPointDynArray = specialize TArray<TSDL_FPoint>;
  //< Dynamic array of TSDL_FPoint.
  TSDLRectDynArray = specialize TArray<TSDL_Rect>;
  //< Dynamic array of TSDL_Rect.
  TSDLFRectDynArray = specialize TArray<TSDL_FRect>;
  //< Dynamic array of TSDL_FRect.
  TSDLVertexDynArray = specialize TArray<TSDL_Vertex>;
  //< Dynamic array of TSDL_Vertex.

{
  Type creation functions. Useful to use them as parameters when calling
    a function without creating a temporal variable.
}

function SDLColor(const r : CUInt8; const g : CUInt8; const b : CUInt8;
  const a : CUInt8 = 255) : TSDL_Color; inline;
{< Create a TSDL_Color from red, green, blue and alpha values.}

function SDLPoint(const x : CInt; const y : CInt) : TSDL_Point; inline;
{< Create a TSDL_Point.}
function SDLFPoint(const x : CFloat; const y : CFloat) : TSDL_FPoint; inline;
{< Create a TSDL_FPoint.}

function SDLRect(const x : CUInt; const y : CUInt; const w : CUInt;
  const h : CUInt) : TSDL_Rect; inline;
{< Create a TSDL_Rect.}
function SDLFRect(const x : CFloat; const y : CFloat; const w : CFloat;
  const h : CFloat) : TSDL_FRect; inline;
{< Create a TSDL_FRect.}


function SDLColor2Str(aColor : TSDL_Color) : String; inline;
{< Write a TSDL_Color to a String.}
function Str2SDLColor(aColor : String) : TSDL_Color;
{< Write a TSDL_Color to a String.}

implementation

function SDLColor(const r : CUInt8; const g : CUInt8; const b : CUInt8;
  const a : CUInt8) : TSDL_Color;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

function SDLPoint(const x : CInt; const y : CInt) : TSDL_Point;
begin
  Result.x := x;
  Result.y := y;
end;

function SDLFPoint(const x : CFloat; const y : CFloat) : TSDL_FPoint;
begin
  Result.x := x;
  Result.y := y;
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

(*
operator = (const a, b: TSDL_Color): Boolean;
begin
  // Si son totalmente transparentes les consideramos iguales
  if (a.A = 0) and (b.A = 0) then Exit(True);
  Result := (a.R = b.R) and (a.G = b.G) and (a.G = b.G) and (a.A = b.A);
end;

operator = (const a, b: TSDL_Point): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y);
end;

operator = (const a, b: TSDL_FPoint): Boolean;
begin
  // ToDo: ¿Hacer estricto?
  Result := SameValue(a.X, b.X) and SameValue(a.Y, b.Y);
end;

operator = (const a, b: TSDL_Rect): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.W = b.W) and (a.H = b.H);
end;

operator = (const a, b: TSDL_FRect): Boolean;
begin
  // ToDo: ¿Hacer estricto?
  Result := SameValue(a.X, b.X) and SameValue(a.Y, b.Y) and SameValue(a.W, b.W)
    and SameValue(a.H, b.H)
end;

operator = (const a, b: TSDL_Vertex): Boolean;
begin
  Result := (a.Position = b.Position) and (a.Color = a.Color) 
    and (a.Tex_Coord = a.Tex_Coord);
end;
*)

function SDLColor2Str(aColor : TSDL_Color) : String;
begin
  Result := Format('%0:d, %1:d, %2:d, %3:d',
    [aColor.r, aColor.g, aColor.b, aColor.a]);
end;

function Str2SDLColor(aColor : String) : TSDL_Color;
var
  Components : array of String;
begin
  Components := aColor.Split(',');
  Result.R := 0; Result.G := 0; Result.B := 0; Result.A := 255;

  // Lazy read
  if Length(Components) < 1 then Exit;
  Result.R := EnsureRange(StrToInt(Components[0]), 0, 255);
  if Length(Components) < 2 then Exit;
  Result.G := EnsureRange(StrToInt(Components[1]), 0, 255);
  if Length(Components) < 3 then Exit;
  Result.B := EnsureRange(StrToInt(Components[2]), 0, 255);
  if Length(Components) < 4 then Exit;
  Result.A := EnsureRange(StrToInt(Components[3]), 0, 255);
end;

end.
