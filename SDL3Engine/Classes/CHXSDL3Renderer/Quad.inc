{ Quadrilateral methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Quad(rilateral)

function cCHXSDL3Renderer.QuadUnsafe(const PArr: TSDLFPointDynArray;
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
      and QuadFilledUnsafe(PArr, idxFirst);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and QuadFillOnlyUnsafe(PArr, idxFirst);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and QuadBorderUnsafe(PArr, idxFirst);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.Quad(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Result and QuadUnsafe(PArr, idxFirst, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Quad(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const P1, P2, P3, P4: TSDL_FPoint;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := QuadUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  PArr[3].X := X4; PArr[3].Y := Y4;
  Result := QuadUnsafe(PArr, 0, BorderC, FillC);
end;

// QuadBorder

function cCHXSDL3Renderer.QuadBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], 4)
    and Line(PArr[idxFirst + 3], PArr[idxFirst]);
end;

function cCHXSDL3Renderer.QuadBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Result and QuadBorderUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.QuadBorder(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4; PArr[4] := P1;
  // Result := QuadBorderUnsafe(PArr, idxFirst); And remove PArr[4]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 5);
end;

function cCHXSDL3Renderer.QuadBorder(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  PArr[3].X := X4; PArr[3].Y := Y4;
  PArr[4] := PArr[0];
  // Result := QuadBorderUnsafe(PArr, idxFirst); And remove PArr[4]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 5);
end;

// QuadFilled

function cCHXSDL3Renderer.QuadFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  Result := PolygonFilledUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Result and PolygonFilledUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFilled(const P1, P2, P3, P4: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := PolygonFilledUnsafe(PArr, 0, 4);
end;

function cCHXSDL3Renderer.QuadFilled(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1; PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3; PArr[3].X := X4; PArr[3].Y := Y4;
  Result := PolygonFilledUnsafe(PArr, 0, 4);
end;

// QuadFillOnly.

function cCHXSDL3Renderer.QuadFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  Result := PolygonFillOnlyUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer = 0): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := PolygonFillOnlyUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFillOnly(const P1, P2, P3, P4: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := PolygonFillOnlyUnsafe(PArr, 0, 4);
end;

function cCHXSDL3Renderer.QuadFillOnly(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1; PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3; PArr[3].X := X4; PArr[3].Y := Y4;
  Result := PolygonFillOnlyUnsafe(PArr, 0, 4);
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
