{ Line[s] methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Line

function cCHXSDL3Renderer.Line(const P1, P2: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderLine(SDLRenderer, P1.X, P1.Y, P2.X, P2.Y);
end;

function cCHXSDL3Renderer.Line(const X1, Y1, X2, Y2: CFloat): Boolean;
begin
  Result := SDL_RenderLine(SDLRenderer, X1, Y1, X2, Y2);
end;

// Lines

function cCHXSDL3Renderer.LinesUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  { Notes about SDL_RenderLines:

    - It doesn't draw anything with `Count <= 0`.
    - No error with negative `Count`.
    - If `Count` exceeds array end, it doesn't care and draw lines with
        "invalid" data (usually 0,0). No error, but this time is logical.
  }
  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.Lines(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  if Count <= 0 then Exit(Result);
  // Try to draw although Result = False
  if Count = 1 then
    Exit(Point(PArr[idxFirst]) and Result);
  if Count > 1 then
    Exit(LinesUnsafe(PArr, idxFirst, Count) and Result);
end;

// LineMirror[x]

function cCHXSDL3Renderer.LineMirrorH(const X1, Y1, X2, Y2, OffsetX: CFloat)
  : Boolean;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(PointMirrorH(X1, Y2, OffsetX));

  Result := Line(OffsetX - X1, Y1, OffsetX - X2, Y2)
        and Line(OffsetX + X1, Y1, OffsetX + X2, Y2);
end;

function cCHXSDL3Renderer.LineMirrorV(const X1, Y1, X2, Y2, OffsetY: CFloat)
  : Boolean;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(PointMirrorV(X1, Y2, OffsetY));

  Result := Line(X1, OffsetY - Y1, X2, OffsetY - Y2)
      and Line(X1, OffsetY + Y1, X2, OffsetY + Y2);
end;

function cCHXSDL3Renderer.LineMirrorHV(
  const X1, Y1, X2, Y2, OffsetX, OffsetY: CFloat): Boolean;
var
  P1XL, P1XR, P1YU, P1YD, P2XL, P2XR, P2YU, P2YD: CFloat;
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
