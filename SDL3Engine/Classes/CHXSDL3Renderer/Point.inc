{ Point[s] drawing methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Point

function cCHXSDL3Renderer.Point(const X, Y: CFloat): Boolean;
begin
  Result := SDL_RenderPoint(SDLRenderer, X, Y);
end;

function cCHXSDL3Renderer.TPoint(const X, Y: CFloat): Boolean;
begin
  Result := SDL_RenderPoint(SDLRenderer, X, Y);
end;

(*
function cCHXSDL3Renderer.FPPoint(const X, Y: CFloat): Boolean;
begin
  Result := SDL_RenderPoint(SDLRenderer, Round(X), Round(Y));
end;
*)

function cCHXSDL3Renderer.Point(const P: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderPoints(SDLRenderer, @P, 1);
end;

function cCHXSDL3Renderer.TPoint(const P: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderPoints(SDLRenderer, @P, 1);
end;

(*
function cCHXSDL3Renderer.FPPoint(const P: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderPoint(SDLRenderer, Round(P.X), Round(P.Y));
end;
*)

// Points

function cCHXSDL3Renderer.PointsUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  { Notes about SDL_RenderPoints:

    - It doesn't draw anything with `Count <= 0`.
    - No error with negative `Count`.
    - If `Count` exceeds array end, it doesn't care and draw points with
        "invalid" data (usually 0,0). No error is set.
  }
  Result := SDL_RenderPoints(SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.Points(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit;
  // Try to draw although Result = False
  Result := PointsUnsafe(PArr, idxFirst, Count) and Result;
end;

(*
function cCHXSDL3Renderer.PointsUnsafeFP(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
var
  TmpArr: TSDLFPointDynArray;
  i: Integer;
begin
  SetLength(TmpArr, Count);
  for i := idxFirst to (idxFirst + Count - 1) do
  begin
    TmpArr[i - idxFirst].X := Round(PArr[i].X);
    TmpArr[i - idxFirst].Y := Round(PArr[i].Y);
  end;
  
  { Notes about SDL_RenderPoints:

    - It doesn't draw anything with `Count <= 0`.
    - No error with negative `Count`.
    - If `Coun`t exceeds array end, it doesn't care and draw points with
        "invalid" data (usually 0,0). No error.
  }
  Result := SDL_RenderPoints(SDLRenderer, @TmpArr[0], Count)
end;

function cCHXSDL3Renderer.PointsFP(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  // Try to draw although Result = False
  if Count > 0 then
    Exit(PointsUnsafeFP(PArr, idxFirst, Count) and Result);
end;
*)

// PointMirror[x]

function cCHXSDL3Renderer.PointMirrorH(const X, Y, OffsetX: CFloat): Boolean;
var
  PArr: array[0..1] of TSDL_FPoint;
begin
  if IsZero(X) then
    Exit(Point(OffsetX, Y));
  PArr[0].X := OffsetX - X; PArr[0].Y := Y;
  PArr[1].X := OffsetX + X; PArr[1].Y := Y;
  Result := PointsUnsafe(PArr, 0, 2);
end;

function cCHXSDL3Renderer.PointMirrorHFilled(const X, Y, OffsetX: CFloat)
  : Boolean;
begin
  if IsZero(X) then
    Exit(Point(OffsetX, Y));
  Result := Line(OffsetX - X, Y, OffsetX + X, Y);
end;

function cCHXSDL3Renderer.PointMirrorV(const X, Y, OffsetY: CFloat): Boolean;
var
  PArr: array[0..1] of TSDL_FPoint;
begin
  if IsZero(Y) then
    Exit(Point(X, OffsetY));
  PArr[0].X := X; PArr[0].Y := OffsetY - Y;
  PArr[1].X := X; PArr[1].Y := OffsetY + Y;
  Result := PointsUnsafe(PArr, 0, 2);
end;

function cCHXSDL3Renderer.PointMirrorVFilled(const X, Y, OffsetY: CFloat)
  : Boolean;
begin
  if IsZero(Y) then
    Exit(Point(X, OffsetY));
  Result := Line(X, OffsetY - Y, X, OffsetY + Y);
end;

function cCHXSDL3Renderer.PointMirrorHV(const X, Y, OffsetX, OffsetY: CFloat)
  : Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  if IsZero(Y) then
    Exit(PointMirrorH(X, OffSetY, OffSetX));
  if IsZero(X) then
    Exit(PointMirrorV(OffsetX, Y, OffSetY));

  PArr[0].X := OffsetX - X; PArr[0].Y := OffsetY - Y; // '+
  PArr[1].X := OffsetX + X; PArr[1].Y := PArr[0].Y;   //  +'
  PArr[2].X := PArr[0].X;   PArr[2].Y := OffsetY + Y; // .+
  PArr[3].X := PArr[1].X;   PArr[3].Y := PArr[2].Y;   //  +.

  Result := PointsUnsafe(PArr, 0, 4);
end;

function cCHXSDL3Renderer.PointMirrorHVFilled(const X, Y: CFloat;
  const FillH, FillV: Boolean; const OffsetX, OffsetY: CFloat): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  if IsZero(Y) then
    if FillH then
      Exit(PointMirrorHFilled(X, OffSetY, OffSetX))
    else
      Exit(PointMirrorH(X, OffSetY, OffSetX));
  if IsZero(X) then
    if FillV then
      Exit(PointMirrorVFilled(OffsetX, Y, OffSetY))
    else
      Exit(PointMirrorV(OffsetX, Y, OffSetY));

  // Sorted this way is better
  PArr[0].X := OffsetX - X; PArr[0].Y := OffsetY - Y; // '+
  PArr[1].X := OffsetX + X; PArr[1].Y := PArr[0].Y;   //  +'
  PArr[2].X := PArr[1].X;   PArr[2].Y := OffsetY + Y; //  +.
  PArr[3].X := PArr[0].X;   PArr[3].Y := PArr[2].Y;   // .+

  if FillV then
  begin
    PArr[4] := PArr[0];
    if FillH then // HV
      // ToDo: Change with RectBorder?
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
