{ Line[s] methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

{
  Line

  ToDo:

  - Make pixelated lines for SPPoint[s] and FPPoint[s].
    - SPLineMirror[x] too
  - Add width parameter to default methods, and draw it with 2 triangles.
}

function cCHXSDL3Renderer.Line(const P1, P2 : TSDL_FPoint) : Boolean;
begin
  Result := Line(P1.X, P1.Y, P2.X, P2.Y);
end;

function cCHXSDL3Renderer.Line(const X1, Y1, X2, Y2 : CFloat) : Boolean;
{
  Renderer Logical Presentation draw it _smooth_, with 1 logical
  pixel of width. If Lenght < 2, 2 logical pixels will overlap their
  opacity. This was fixed for Horizontal and Vertical lines.
}
begin
  if SameValue(X1, X2) then // Vertical Line
  begin
    if SameValue(Y1, Y2) then // Actually a Point
      Exit(Point(X1, Y2));

    if Y1 < Y2 then
      Exit(RectFilled(X1, Y1, 1, Y2 - Y1 + 1))
    else
      Exit(RectFilled(X1, Y1 + 1, 1, Y2 - Y1 - 1));
  end;

  if SameValue(Y1, Y2) then // Horizontal Line
  begin
    if X1 < X2 then
      Exit(RectFilled(X1, Y1, X2 - X1 + 1, 1))
    else
      Exit(RectFilled(X1 + 1, Y1, X2 - X1 - 1, 1));
  end;

  // Diagonal line
  Result := SDL_RenderLine(SDLRenderer, X1, Y1, X2, Y2);
end;

function cCHXSDL3Renderer.Line(const Seg : TCHXSDLFSegment) : Boolean;
begin
  Result := Line(Seg.P1.X, Seg.P1.Y, Seg.P2.X, Seg.P2.Y);
end;

{
  SPLine
}

function cCHXSDL3Renderer.SPLine(const P1, P2 : TSDL_FPoint) : Boolean;
begin
  Result := SPLine(P1.X, P1.Y, P2.X, P2.Y);
end;

function cCHXSDL3Renderer.SPLine(const X1, Y1, X2, Y2 : CFloat) : Boolean;
{
  ToDo: Make pixelated line with subpixel adaptation.
}
begin
  if SameValue(X1, X2) then // Vertical Line
  begin
    if SameValue(Y1, Y2) then // Actually a Point
      Exit(SPPoint(X1, Y2));

    if Y1 < Y2 then
      Exit(SPRectFilled(X1, Y1, 1, Y2 - Y1 + 1))
    else
      Exit(SPRectFilled(X1, Y1 + 1, 1, Y2 - Y1 - 1));
  end;

  if SameValue(Y1, Y2) then // Horizontal Line
  begin
    if X1 < X2 then
      Exit(SPRectFilled(X1, Y1, X2 - X1 + 1, 1))
    else
      Exit(SPRectFilled(X1 + 1, Y1, X2 - X1 - 1, 1));
  end;

  // Diagonal line
  Result := SDL_RenderLine(SDLRenderer, X1, Y1, X2, Y2);
end;

function cCHXSDL3Renderer.SPLine(const Seg : TCHXSDLFSegment) : Boolean;
begin
  Result := SPLine(Seg.P1.X, Seg.P1.Y, Seg.P2.X, Seg.P2.Y);
end;

{
  FPLine
}


{
  Lines
}

function cCHXSDL3Renderer.LinesUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst, Count : Integer) : Boolean;
{ Notes about SDL_RenderLines:

  - It doesn't draw anything with `Count <= 0`.
  - No error with negative `Count`.
  - If `Count` exceeds array end, it doesn't care and draw lines with
    "invalid" data (usually 0,0). No error, but this time is logical.
}
begin
  // ToDo: Use pixelated SPLine
  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.Lines(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; Count : Integer) : Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  if Count <= 0 then Exit;
  // Try to draw although Result = False
  if Count = 1 then
    Exit(Point(PArr[idxFirst]) and Result);

  Result := LinesUnsafe(PArr, idxFirst, Count) and Result;
end;

{
  SPLines

  Todo: Draw them pixelated.
}

function cCHXSDL3Renderer.SPLinesUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst, Count : Integer) : Boolean;
{ Notes about SDL_RenderLines:

  - It doesn't draw anything with `Count <= 0`.
  - No error with negative `Count`.
  - If `Count` exceeds array end, it doesn't care and draw lines with
      "invalid" data (usually 0,0). No error, but this time is logical.
}
begin
  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.SPLines(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; Count : Integer) : Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  if Count <= 0 then Exit;

  // Try to draw although Result = False
  if Count = 1 then
    Exit(Point(PArr[idxFirst]) and Result);

  Result := LinesUnsafe(PArr, idxFirst, Count) and Result;
end;

{
  LineMirror[x]

  ToDo:

  - FP versions.
}

function cCHXSDL3Renderer.LineMirrorH(const X1, Y1, X2, Y2, OffsetX : CFloat)
  : Boolean;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(PointMirrorH(X1, Y2, OffsetX));

  Result := Line(OffsetX - X1, Y1, OffsetX - X2, Y2)
        and Line(OffsetX + X1, Y1, OffsetX + X2, Y2);
end;

function cCHXSDL3Renderer.LineMirrorV(const X1, Y1, X2, Y2, OffsetY : CFloat)
  : Boolean;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(PointMirrorV(X1, Y2, OffsetY));

  Result := Line(X1, OffsetY - Y1, X2, OffsetY - Y2)
      and Line(X1, OffsetY + Y1, X2, OffsetY + Y2);
end;

function cCHXSDL3Renderer.LineMirrorHV(
  const X1, Y1, X2, Y2, OffsetX, OffsetY : CFloat) : Boolean;
var
  P1XL, P1XR, P1YU, P1YD, P2XL, P2XR, P2YU, P2YD : CFloat;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(PointMirrorHV(X1, Y2, OffsetX, OffsetY));

  P1XL := OffsetX - X1; P1YU := OffsetY - Y1;
  P1XR := OffsetX + X1; P1YD := OffsetY + Y1;
  P2XL := OffsetX - X2; P2YU := OffsetY - Y2;
  P2XR := OffsetX + X2; P2YD := OffsetY + Y2;

  Result := Line(P1XL, P1YU, P2XL, P2YU) and Line(P1XR, P1YU, P2XR, P2YU)
        and Line(P1XL, P1YD, P2XL, P2YD) and Line(P1XR, P1YD, P2XR, P2YD);
end;

{
  SPLineMirror[x]
}

function cCHXSDL3Renderer.SPLineMirrorH(const X1, Y1, X2, Y2, OffsetX : CFloat)
  : Boolean;
begin
  // ToDo: Draw pixelized lines
  Result := LineMirrorH(X1, Y1, X2, Y2, OffsetX);
end;

function cCHXSDL3Renderer.SPLineMirrorV(const X1, Y1, X2, Y2, OffsetY : CFloat)
  : Boolean;
begin
  // ToDo: Draw pixelized lines
  Result := LineMirrorV(X1, Y1, X2, Y2, OffsetY);
end;

function cCHXSDL3Renderer.SPLineMirrorHV(
  const X1, Y1, X2, Y2, OffsetX, OffsetY : CFloat): Boolean;
begin
  // ToDo: Draw pixelized lines
  Result := LineMirrorHV(X1, Y1, X2, Y2, OffsetX, OffsetY);
end;

{
  Segments[X]: For one segment use Line
}

function cCHXSDL3Renderer.SegmentsUnsafe(const SegArr : TSDLFSegmentDynArray;
  const idxFirst, Count : Integer) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := idxFirst to (idxFirst + Count - 1) do
    Result := Line(SegArr[i]) and Result;
end;

function cCHXSDL3Renderer.SegmentsUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst, PairCount : Integer) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to (PairCount - 1) do
    Result := Line(PArr[idxFirst + i * 2], PArr[idxFirst + i * 2 + 1])
      and Result;
end;

function cCHXSDL3Renderer.Segments(const SegArr : TSDLFSegmentDynArray;
  const idxFirst : Integer; Count : Integer) : Boolean;
begin
  Result := IsValidArrayRange(Length(SegArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  if Count <= 0 then Exit;

  Result := SegmentsUnsafe(SegArr, idxFirst, Count) and Result;
end;

function cCHXSDL3Renderer.Segments(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; PairCount : Integer) : Boolean;
var
  MaxCount : integer;
begin
  Result := True;
  if Length(PArr) <= 0 then
  begin
    if PairCount = 0 then
      Exit(True) // Empty array but Count is 0 too
    else
    begin
      SDL_SetError('%s(%s) %s: Empty array and PairCount <> 0',
        [PAnsiChar({$I %FILE%}), PAnsiChar({$I %LINE%}),
        PAnsiChar({$I %CURRENTROUTINE%})]);
      Exit(False);
    end;
  end;

  if (idxFirst < 0) or (idxFirst > High(PArr)) or (PairCount < 0) then
  begin
    SDL_SetError(
      '%s(%s) %s: (Index(%d) not in [0..%d]) or (PairCount(%d) < 0)',
      [PAnsiChar({$I %FILE%}), PAnsiChar({$I %LINE%}),
        PAnsiChar({$I %CURRENTROUTINE%}), idxFirst, High(PArr), PairCount]);
    Exit(False);
  end;

  MaxCount := (Length(PArr) - idxFirst) div 2;

  if PairCount = 0 then
    PairCount := MaxCount
  else if PairCount > MaxCount then
  begin
    SDL_SetError(
      '%s(%s) %s: (Index(%d) + PairCount(%d) * 2) > Array Length(%d)',
      [PAnsiChar({$I %FILE%}), PAnsiChar({$I %LINE%}),
        PAnsiChar({$I %CURRENTROUTINE%}), idxFirst, PairCount, Length(PArr)]);
    PairCount := MaxCount;
    Result := False;
  end;

  if PairCount <= 0 then Exit;

  if Odd(PairCount) then
  begin
    SDL_SetError('%s(%s) %s: Odd number of points (%d)',
      [PAnsiChar({$I %FILE%}), {$I %LINE%}, {$I %CURRENTROUTINE%},
      PairCount]);
    Dec(PairCount);

    Result := False;
    if PairCount <= 0 then Exit;
  end;

  Result := SegmentsUnsafe(PArr, idxFirst, PairCount) and Result;
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
