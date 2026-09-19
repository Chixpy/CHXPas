{ Triangle methods

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy

  ToDo:

  - SP and FP Border because they use smooth default draw method.
  - SP and FP Fill and FillOnly use general polygon fill, there are
    optimized methods for triangles.
  - Default Fill and OnlyFill because they fill half of the
    border. Se necesita calcular la bisectriz del angulo que forman los lados
    y desplazarse media ancho del borde  hacia adentro (y hacia a fuera)
    para que concuerde con el tamaño del borde. Y hay que averiguar que es
    "hacia dentro" y "hacia afuera".
  - Make RawTriangleFilled methods that don't consider the size of the
    border.
}

{
  Triangle
}

function cCHXSDL3Renderer.TriangleUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := TriangleFilledUnsafe(PArr, idxFirst)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := TriangleFillOnlyUnsafe(PArr, idxFirst)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and TriangleBorderUnsafe(PArr, idxFirst)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.Triangle(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; const BorderC, FillC : TSDL_FColor) : Boolean;
var
  Count : Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := TriangleUnsafe(PArr, idxFirst, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const PArr : TSDLFPointDynArray;
  const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := Triangle(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const P1, P2, P3 : TSDL_FPoint;
  const BorderC, FillC : TSDL_FColor) : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := TriangleUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const X1, Y1, X2, Y2, X3, Y3 : CFloat;
  const BorderC, FillC : TSDL_FColor) : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := TriangleUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.TriangleBorderUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer) : Boolean;
begin
  Result := PolygonBorderUnsafe(PArr, idxFirst, 3);
end;

function cCHXSDL3Renderer.TriangleBorder(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer) : Boolean;
var
  Count : Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := TriangleBorderUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleBorder(const P1, P2, P3 : TSDL_FPoint)
  : Boolean;
var
  PArr : array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := TriangleBorderUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleBorder(const X1, Y1, X2, Y2, X3, Y3 : CFloat)
  : Boolean;
var
  PArr : array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := TriangleBorderUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFilledUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer) : Boolean;
var
  FColor : TSDL_FColor;
  TempPArr : TSDLFPointDynArray;
  i : Integer;
begin
  // Try not to halt the program.
  try
    // Copying Dynamic Array, as it's actually a pointer
    TempPArr := Copy(PArr, idxFirst, 3);
  except
    on e: Exception do
    begin
      SDL_SetError('%s(%s) %s: PArr[%d..%d] invalid subrange [%d..%d]',
        [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
        Low(PArr), High(PArr), idxFirst, idxFirst + 1]);
      Exit(False);
    end;
  end;

  for i := 0 to High(TempPArr) do
  begin
    // As other primitives includes a border of 1 unit,
    // so, we need to move vertices down right 0.5 units.
    TempPArr[i].X += 0.5;
    TempPArr[i].Y += 0.5;

    // ToDo: After this, Vertices must be moved again 0.5
    // (or half of the border) OUTwards from the center (or along its
    // bisectriz)
  end;

  Result := GetDrawColor(FColor)
    and SDL_RenderGeometryRaw(SDLRenderer, nil, @TempPArr[0],
      SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0);
end;

function cCHXSDL3Renderer.TriangleFilled(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer) : Boolean;
var
  Count : Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := TriangleFilledUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFilled(const P1, P2, P3 : TSDL_FPoint)
  : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := TriangleFilledUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFilled(const X1, Y1, X2, Y2, X3, Y3 : CFloat)
  : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := TriangleFilledUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFillOnlyUnsafe(
  const PArr : TSDLFPointDynArray; const idxFirst : Integer) : Boolean;
var
  FColor : TSDL_FColor;
  TempPArr : TSDLFPointDynArray;
  i : Integer;
begin
  // Try not to halt the program.
  try
    // Copying Dynamic Array, as it's actually a pointer
    TempPArr := Copy(PArr, idxFirst, 3);
  except
    on e: Exception do
    begin
      SDL_SetError('%s(%s) %s: PArr[%d..%d] invalid subrange [%d..%d]',
        [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
        Low(PArr), High(PArr), idxFirst, idxFirst + 1]);
      Exit(False);
    end;
  end;

  for i := 0 to High(TempPArr) do
  begin
    // As other primitives includes a border of 1 unit,
    // so, we need to move vertices down right 0.5 units.
    TempPArr[i].X += 0.5;
    TempPArr[i].Y += 0.5;

    // ToDo: After this, Vertices must be moved again 0.5
    // (or half of the border) INwards from the center (or along its
    // bisectriz)
  end;

  Result := GetDrawColor(FColor)
    and SDL_RenderGeometryRaw(SDLRenderer, nil, @TempPArr[0],
      SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const PArr : TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := TriangleFillOnlyUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := TriangleFillOnlyUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFillOnly(
  const X1, Y1, X2, Y2, X3, Y3: CFloat) : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := TriangleFillOnlyUnsafe(PArr, 0);
end;

{
  SPTriangle
}

function cCHXSDL3Renderer.SPTriangleUnsafe(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := SPTriangleFilledUnsafe(PArr, idxFirst)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := SPTriangleFillOnlyUnsafe(PArr, idxFirst)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and SPTriangleBorderUnsafe(PArr, idxFirst)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.SPTriangle(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer; const BorderC, FillC : TSDL_FColor) : Boolean;
var
  Count : Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := SPTriangleUnsafe(PArr, idxFirst, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPTriangle(const PArr : TSDLFPointDynArray;
  const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := SPTriangle(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPTriangle(const P1, P2, P3 : TSDL_FPoint;
  const BorderC, FillC : TSDL_FColor) : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := SPTriangleUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPTriangle(const X1, Y1, X2, Y2, X3, Y3 : CFloat;
  const BorderC, FillC : TSDL_FColor) : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := SPTriangleUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPTriangleBorderUnsafe(
  const PArr : TSDLFPointDynArray; const idxFirst : Integer) : Boolean;
begin
  Result := SPPolygonBorderUnsafe(PArr, idxFirst, 3);
end;

function cCHXSDL3Renderer.SPTriangleBorder(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer) : Boolean;
var
  Count : Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := SPTriangleBorderUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.SPTriangleBorder(const P1, P2, P3 : TSDL_FPoint)
  : Boolean;
var
  PArr : array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := SPTriangleBorderUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.SPTriangleBorder(
  const X1, Y1, X2, Y2, X3, Y3 : CFloat) : Boolean;
var
  PArr : array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := SPTriangleBorderUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.SPTriangleFilledUnsafe(
  const PArr : TSDLFPointDynArray; const idxFirst : Integer) : Boolean;
begin
  //  ToDo: Create an optimized method for triangles
  {
    Sería dividir el triángulo en dos partes horizontalmente por el
    punto esté verticalmente en el medio y tan solo se necesita se
    buscar los puntos de intersección de dos líneas en vez de tres
    para cada scanline.
  }
  Result := SPPolygonFilled(PArr, idxFirst, 3);
end;

function cCHXSDL3Renderer.SPTriangleFilled(const PArr : TSDLFPointDynArray;
  const idxFirst : Integer) : Boolean;
var
  Count : Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := SPTriangleFilledUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.SPTriangleFilled(const P1, P2, P3 : TSDL_FPoint)
  : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := SPTriangleFilledUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.SPTriangleFilled(
  const X1, Y1, X2, Y2, X3, Y3 : CFloat) : Boolean;
var
  PArr : Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := SPTriangleFilledUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.SPTriangleFillOnlyUnsafe(
  const PArr : TSDLFPointDynArray; const idxFirst : Integer) : Boolean;
begin
  //  ToDo: Create an optimized method for triangles
  {
    Sería dividir el triángulo en dos partes horizontalmente por el
    punto esté verticalmente en el medio y tan solo se necesita se
    buscar los puntos de intersección de dos líneas en vez de tres
    para cada scanline.
  }
  Result := SPPolygonFillOnlyUnsafe(PArr, idxFirst, 3);
end;

function cCHXSDL3Renderer.SPTriangleFillOnly(const PArr : TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := SPTriangleFillOnlyUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.SPTriangleFillOnly(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;

  Result := SPTriangleFillOnlyUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.SPTriangleFillOnly(
  const X1, Y1, X2, Y2, X3, Y3: CFloat) : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;

  Result := SPTriangleFillOnlyUnsafe(PArr, 0);
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
