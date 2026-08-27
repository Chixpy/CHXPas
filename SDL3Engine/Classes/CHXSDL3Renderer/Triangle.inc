{ Triangle methods

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Triangle

function cCHXSDL3Renderer.TriangleUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and TriangleFilledUnsafe(PArr, idxFirst);

    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and TriangleFillOnlyUnsafe(PArr, idxFirst);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and TriangleBorderUnsafe(PArr, idxFirst);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.Triangle(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and TriangleUnsafe(PArr, idxFirst, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Triangle(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const P1, P2, P3: TSDL_FPoint;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := TriangleUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const X1, Y1, X2, Y2, X3, Y3: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := TriangleUnsafe(PArr, 0, BorderC, FillC);
end;

// TriangleBorder

function cCHXSDL3Renderer.TriangleBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], 3)
    and Line(PArr[idxFirst + 2], PArr[idxFirst]);
end;

function cCHXSDL3Renderer.TriangleBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and TriangleBorderUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleBorder(const P1, P2, P3: TSDL_FPoint): Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P1;

  // Result := TriangleBorderUnsafe(PArr, idxFirst); And remove PArr[3]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 4);
end;

function cCHXSDL3Renderer.TriangleBorder(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  PArr[3] := PArr[0];

  // Result := TriangleBorderUnsafe(PArr, idxFirst); And remove PArr[3]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 4);
end;

// TriangleFilled

function cCHXSDL3Renderer.TriangleFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  FColor: TSDL_FColor;
begin
  Result := GetDrawColor(FColor)
    and SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[idxFirst],
      SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0);
end;

function cCHXSDL3Renderer.TriangleFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and TriangleFilledUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFilled(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := TriangleFilledUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFilled(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := TriangleFilledUnsafe(PArr, 0);
end;

// TriangleFillOnly.

function cCHXSDL3Renderer.TriangleFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  // ToDo: Maybe there is an optimized method for triangles.
  Result := PolygonFillOnlyUnsafe(PArr, idxFirst, 3);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and TriangleFillOnlyUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := TriangleFillOnlyUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  // ToDo: PArr[0].Init(X1, Y1) and remove compiler warning.
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := TriangleFillOnlyUnsafe(PArr, 0);
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
