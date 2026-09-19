{ Point[s] drawing methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy

  ToDo:

  - FP versions of PointMirror[x] methods.
}

{
  Point
}

function cCHXSDL3Renderer.Point(const X, Y : CFloat) : Boolean;
begin
  Result := SDL_RenderPoint(SDLRenderer, X, Y);
end;

function cCHXSDL3Renderer.Point(const P : TSDL_FPoint) : Boolean;
begin
  Result := SDL_RenderPoints(SDLRenderer, @P, 1);
end;

{
  SPPoint
}

function cCHXSDL3Renderer.SPPoint(const X, Y : CFloat) : Boolean;
begin
  Result := Point(X, Y); // Same as default.
end;

function cCHXSDL3Renderer.SPPoint(const P : TSDL_FPoint) : Boolean;
begin
  Result := Point(P); // Same as default.
end;

{
  FPPoint
}

function cCHXSDL3Renderer.FPPoint(const X, Y : CInt) : Boolean;
begin
  Result := Point(X, Y); // Same as default, implicit type conversion.
end;

function cCHXSDL3Renderer.FPPoint(const P : TSDL_Point) : Boolean;
begin
  Result := Point(P); // Same as default, type conversion in TypeHelpers.
end;

{
  Points
}

function cCHXSDL3Renderer.PointsUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst, Count : Integer) : Boolean;
{ Notes about SDL_RenderPoints:

  - It doesn't draw anything with `Count <= 0`.
  - No error with negative `Count`.
  - If `Count` exceeds array end, it doesn't care and draw points with
    "invalid" data (usually 0,0). No error is set.
}
begin
  Result := SDL_RenderPoints(SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.Points(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; Count : Integer) : Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit;

  // Try to draw although Result = False
  Result := PointsUnsafe(PArr, idxFirst, Count) and Result;
end;

{
  SPPoints
}

function cCHXSDL3Renderer.SPPointsUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst, Count : Integer) : Boolean;
{ Notes about SDL_RenderPoints:

  - It doesn't draw anything with `Count <= 0`.
  - No error with negative `Count`.
  - If `Count` exceeds array end, it doesn't care and draw points with
    "invalid" data (usually 0,0). No error is set.
}
begin
  Result := SDL_RenderPoints(SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.SPPoints(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; Count : Integer) : Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit;

  // Try to draw although Result = False
  Result := SPPointsUnsafe(PArr, idxFirst, Count) and Result;
end;

{
  FPPoints
}

function cCHXSDL3Renderer.FPPointsUnsafe(const PArr : TSDLPointDynArray;
  const idxFirst, Count : Integer) : Boolean;
var
  TmpArr : TSDLFPointDynArray;
  i : Integer;
begin
  SetLength(TmpArr, Count);
  for i := idxFirst to (idxFirst + Count - 1) do
  begin
    TmpArr[i - idxFirst].X := PArr[i].X;
    TmpArr[i - idxFirst].Y := PArr[i].Y;
  end;

  Result := SDL_RenderPoints(SDLRenderer, @TmpArr[0], Count)
end;

function cCHXSDL3Renderer.FPPoints(const PArr : TSDLPointDynArray;
  const idxFirst : Integer; Count : Integer) : Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  // Try to draw although Result = False
  if Count > 0 then
    Exit(FPPointsUnsafe(PArr, idxFirst, Count) and Result);
end;

{
  PointMirror[x]
}

function cCHXSDL3Renderer.PointMirrorH(const X, Y, OffsetX : CFloat) : Boolean;
var
  PArr : array[0..1] of TSDL_FPoint;
begin
  if (Abs(X) <= 0.5) then
      Exit(RectFilled(OffsetX - Abs(X), Y, Abs(X) * 2 + 1, 1));

  PArr[0].X := OffsetX - X; PArr[0].Y := Y;
  PArr[1].X := OffsetX + X; PArr[1].Y := Y;
  Result := PointsUnsafe(PArr, 0, 2);
end;

function cCHXSDL3Renderer.PointMirrorHFilled(const X, Y, OffsetX : CFloat)
  : Boolean;
begin
  //if IsZero(X) then
  //  Exit(Point(OffsetX, Y));
  Result := Line(OffsetX - X, Y, OffsetX + X, Y);
end;

function cCHXSDL3Renderer.PointMirrorV(const X, Y, OffsetY : CFloat) : Boolean;
var
  PArr : array[0..1] of TSDL_FPoint;
begin
  if (Abs(Y) <= 0.5) then
    Exit(RectFilled(X, OffsetY - Abs(Y), 1, Abs(Y) * 2 + 1));

  PArr[0].X := X; PArr[0].Y := OffsetY - Y;
  PArr[1].X := X; PArr[1].Y := OffsetY + Y;
  Result := PointsUnsafe(PArr, 0, 2);
end;

function cCHXSDL3Renderer.PointMirrorVFilled(const X, Y, OffsetY : CFloat)
  : Boolean;
begin
  //if IsZero(Y) then
  //  Exit(Point(X, OffsetY));
  Result := Line(X, OffsetY - Y, X, OffsetY + Y);
end;

function cCHXSDL3Renderer.PointMirrorHV(const X, Y, OffsetX, OffsetY : CFloat)
  : Boolean;
var
  PArr : array[0..3] of TSDL_FPoint;
begin
  if (Abs(X) <= 0.5) then
    if (Abs(Y) <= 0.5) then
      Exit(RectFilled(OffsetX - Abs(X), OffsetY - Abs(Y),
        Abs(X) * 2 + 1, Abs(Y) * 2 + 1))
    else
      Exit(LineMirrorV(OffsetX - X, Y, OffsetX + X, Y, OffsetY));
  if (Abs(Y) <= 0.5) then
    Exit(LineMirrorH(X, OffsetY - Y, X, OffsetY + Y, OffsetX));

  PArr[0].X := OffsetX - X; PArr[0].Y := OffsetY - Y; // '+
  PArr[1].X := OffsetX + X; PArr[1].Y := PArr[0].Y;   //  +'
  PArr[2].X := PArr[0].X;   PArr[2].Y := OffsetY + Y; // .+
  PArr[3].X := PArr[1].X;   PArr[3].Y := PArr[2].Y;   //  +.

  Result := PointsUnsafe(PArr, 0, 4);
end;

function cCHXSDL3Renderer.PointMirrorHVFilled(const X, Y : CFloat;
  const FillH, FillV : Boolean; const OffsetX, OffsetY : CFloat) : Boolean;
var
  PArr : array[0..4] of TSDL_FPoint;
begin
  { Todo: We can simplify this? }
  if (Abs(X) <= 0.5) then
    if not ((Abs(Y) <= 0.5) or (FillV)) then
    // if (not (Abs(Y) <= 0.5)) and (not FillV) then
      Exit(LineMirrorV(OffsetX - X, Y, OffsetX + X, Y, OffsetY))
    else
      Exit(RectFilled(OffsetX - Abs(X), OffsetY - Abs(Y),
        Abs(X) * 2 + 1, Abs(Y) * 2 + 1));
  if (Abs(Y) <= 0.5) then
    if FillH then
      Exit(RectFilled(OffsetX - Abs(X), OffsetY - Abs(Y),
        Abs(X) * 2 + 1, Abs(Y) * 2 + 1))
    else
      Exit(LineMirrorH(X, OffsetY - Y, X, OffsetY + Y, OffsetX));

  // Sorted this way is better
  PArr[0].X := OffsetX - X; PArr[0].Y := OffsetY - Y; // '+
  PArr[1].X := OffsetX + X; PArr[1].Y := PArr[0].Y;   //  +'
  PArr[2].X := PArr[1].X;   PArr[2].Y := OffsetY + Y; //  +.
  PArr[3].X := PArr[0].X;   PArr[3].Y := PArr[2].Y;   // .+

  if FillV then
  begin
    PArr[4] := PArr[0];
    if FillH then // HV
      Result := LinesUnsafe(PArr, 0, 5)
    else // V
      Result := LinesUnsafe(PArr, 1, 2)
        and LinesUnsafe(PArr, 3, 2);
  end
  else
  begin
    if FillH then // H
      Result := LinesUnsafe(PArr, 0, 2)
        and LinesUnsafe(PArr, 2, 2)
    else // No Fill
      Result := PointsUnsafe(PArr, 0, 4)
  end;
end;

{
  SPPointMirror[x]
}

function cCHXSDL3Renderer.SPPointMirrorH(const X, Y, OffsetX : CFloat)
  : Boolean;
begin
  Result := PointMirrorH(X, Y, OffsetX); // Same as default,
end;

function cCHXSDL3Renderer.SPPointMirrorHFilled(const X, Y, OffsetX : CFloat)
  : Boolean;
begin
  Result := PointMirrorHFilled(X, Y, OffsetX); // Same as default
end;

function cCHXSDL3Renderer.SPPointMirrorV(const X, Y, OffsetY : CFloat) : Boolean;
begin
  Result := PointMirrorV(X, Y, OffsetY); // Same as default
end;

function cCHXSDL3Renderer.SPPointMirrorVFilled(const X, Y, OffsetY : CFloat)
  : Boolean;
begin
  Result := PointMirrorVFilled(X, Y, OffsetY); // Same as default
end;

function cCHXSDL3Renderer.SPPointMirrorHV(const X, Y, OffsetX, OffsetY : CFloat)
  : Boolean;
begin
  Result := PointMirrorHV(X, Y, OffsetX, OffsetY); // Same as default
end;

function cCHXSDL3Renderer.SPPointMirrorHVFilled(const X, Y : CFloat;
  const FillH, FillV : Boolean; const OffsetX, OffsetY : CFloat) : Boolean;
begin
  // Same as default
  Result := PointMirrorHVFilled(X, Y, FillH, FillV, OffsetX, OffsetY);
end;

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
