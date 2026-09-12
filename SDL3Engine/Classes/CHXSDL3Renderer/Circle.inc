{ Circle methods.

  This file is part of ucCHXSDL3Renderer.

  ToDo:

  - Negative radious see RndRect.
  - We can simplify a little see RndRect too (and Ellipse maybe)

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Circle[X]

procedure cCHXSDL3Renderer.TCircleVertices(var PArr: TSDLFPointDynArray;
  const X, Y, R: CFloat; OctPoints: Integer);
const
  kMinTCircle = 0; // Additional vertices for each octant of the circle
  kMaxTCircle = 7; //
  kTCircleRatio = {2 * } Pi / 8;
var
  Angle, dAng, aSin, aCos: CFloat;
  i, k: Integer;
begin
  if OctPoints < kMinTCircle then
    OctPoints := Round(kTCircleRatio * SqRt(Abs(R)));

  if OctPoints < kMinTCircle then OctPoints := kMinTCircle
  else if OctPoints > kMaxTCircle then OctPoints := kMaxTCircle;

  SetLength(PArr, (OctPoints + 1) * 8);

  // Basic points
  aSin := kSin45 * R; // Temp var for 45º position
  k := (OctPoints + 1);
  PArr[0].Init(X + R, Y);
  PArr[k].Init(X + aSin, Y - aSin);
  PArr[k * 2].Init(X, Y - R);
  PArr[k * 3].Init(X - aSin, Y - aSin);
  PArr[k * 4].Init(X - R, Y);
  PArr[k * 5].Init(X - aSin, Y + aSin);
  PArr[k * 6].Init(X, Y + R);
  PArr[k * 7].Init(X + aSin, Y + aSin);

  if OctPoints <= 0 then Exit;

  // Rest of the points.
  Angle := 0;
  dAng := (2 * Pi) / Length(PArr);
  // k := (OctPoints + 1); // Already set
  for i := 1 to OctPoints do
  begin
    Angle += dAng;
    SinCos(Angle, aSin, aCos);
    aSin *= R; aCos *= R;

    PArr[i].Init(X + aCos, Y - aSin);
    PArr[Length(PArr) - i].Init(X + aCos, Y + aSin);

    PArr[(k * 2) - i].Init(X + aSin, Y - aCos);
    PArr[(k * 2) + i].Init(X - aSin, Y - aCos);

    PArr[(k * 4) - i].Init(X - aCos, Y - aSin);
    PArr[(k * 4) + i].Init(X - aCos, Y + aSin);

    PArr[(k * 6) - i].Init(X - aSin, Y + aCos);
    PArr[(k * 6) + i].Init(X + aSin, Y + aCos);
  end;
end;

function cCHXSDL3Renderer.Circle(const X, Y, R: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and CircleFilled(X, Y, R);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and CircleFillOnly(X, Y, R);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and CircleBorder(X, Y, R);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.CircleBorder(const X, Y: CFloat;
  R: CFloat): Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing pixels at cardinal/diagonal angles.
  - Minor initialization optimization.
  - Adapted to subpixels of Logical Presentation.
}
var
  t1, t2, CurrX, CurrY: Integer;
  FracR: CFloat; // Subpixel Radius Offset.
begin
  R := Abs(R);
  if R < 1 then Exit(Point(X, Y));
  CurrX := Floor(R);
  FracR := R - CurrX;

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  //   - Subpixel offset.
  if IsZero(FracR) then
    Result := PointMirrorV(X, R, Y) and PointMirrorH(R, Y, X)
  else
    Result := RectFilled(X - FracR, Y - R, FracR * 2 + 1, 1)
     and RectFilled(X - FracR, Y + R, FracR * 2 + 1, 1)
     and RectFilled(X - R, Y - FracR, 1, FracR * 2 + 1)
     and RectFilled(X + R, Y - FracR, 1, FracR * 2 + 1);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  if t2 >= 0 then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHV(CurrX + FracR, CurrY + FracR, X, Y)
      and PointMirrorHV(CurrY + FracR, CurrX + FracR, X, Y)
      and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if CurrX = CurrY then
    Result := PointMirrorHV(CurrX + FracR, CurrY + FracR, X, Y) and Result;
end;

function cCHXSDL3Renderer.TCircleBorder(const X, Y, R: CFloat;
  const OctPoints: Integer): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  TCircleVertices(PArr, X, Y, R, OctPoints);
  Result := TPolygonBorder(PArr);
end;

function cCHXSDL3Renderer.CircleFilled(const X, Y: CFloat; R: CFloat)
  : Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing lines already drawn.
  - Minor initialization optimization.
  - Fill the circle with horizontal lines.
  - Adapted to subpixels of Logical Presentation.
}
var
  t1, t2, CurrX, CurrY: Integer;
  FracR: CFloat; // Subpixel Radius Offset.
begin
  R := Abs(R);
  if R < 1 then Exit(Point(X, Y));
  CurrX := Floor(R);
  FracR := R - CurrX;

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  //   - Subpixel offset.
  if IsZero(FracR) then
    Result := PointMirrorHFilled(R, Y, X)
  else
    Result := RectFilled(
      SDLFRect(X - R, Y - FracR, (2 * R) + 1, (2 * FracR) + 1));

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;

  if t2 >= 0 then
  begin
    t1 := t2; Dec(CurrX);
    if IsZero(FracR) then
      Result := PointMirrorV(X, R, Y) and Result
    else
      Result := PointMirrorHVFilled(FracR, R, True, False, X, Y)
        and Result;
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHVFilled(CurrX + FracR, CurrY + FracR,
        True, False, X, Y) and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
      Result := PointMirrorHVFilled(CurrY + FracR - 1, CurrX + FracR + 1,
        True, False, X, Y) and Result;
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if (t2 >= 0) and (CurrY = CurrX) then
    Result := PointMirrorHVFilled(CurrX + FracR, CurrY + FracR,
      True, False, X, Y) and Result;
end;

function cCHXSDL3Renderer.TCircleFilled(const X, Y, R: CFloat;
  const OctPoints : Integer): Boolean;
var
  PArr: TSDLFPointDynArray;
  Indices: Array of Byte;
  FColor: TSDL_FColor;
  i, j: Integer;
begin
  // Adding border size
  TCircleVertices(PArr, X + 0.5, Y + 0.5, R + 0.5, OctPoints);

  SetLength(Indices, Length(PArr) * 3);

  // Adding center point for creating triangles
  SetLength(PArr, Length(PArr) + 1);
  PArr[High(PArr)] := SDLFPoint(X, Y);

  for i := 0 to (High(PArr) - 2) do
  begin
    j := i * 3;
    Indices[j] := High(PArr);
    Indices[j + 1] := i;
    Indices[j + 2] := i + 1;
  end;
  j := (High(PArr) - 1) * 3;
  Indices[j] := High(PArr);
  Indices[j + 1] := High(PArr) - 1;
  Indices[j + 2] := Low(PArr);

  Result := GetDrawColor(FColor)
    and SDL_RenderGeometryRaw(SDLRenderer, nil,
      @PArr[0], SizeOf(TSDL_FPoint), // Puntos
      @FColor, 0, // Colores
      nil, 0, // uv
      Length(PArr), @Indices[0], Length(Indices), 1); // Indices
end;


function cCHXSDL3Renderer.CircleFillOnly(const X, Y: CFloat; R: CFloat)
  : Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing lines already drawn.
  - Minor initialization optimization.
  - Fill the circle with horizontal lines without border.
  - Adapted to subpixels of Logical Presentation.
}
var
  t1, t2, CurrX, CurrY: Integer;
  DrawLine: Boolean;
  FracR: CFloat; // Subpixel Radius Offset.
begin
  R := Abs(R);
  if R < 1 then Exit(True); // Nothing to fill
  CurrX := Floor(R);
  FracR := R - CurrX;

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  //   - Subpixel adjust.
  if IsZero(FracR) then
    Result := PointMirrorHFilled(R - 1, Y, X)
  else
    Result := RectFilled(
      SDLFRect(X - R + 1, Y - FracR, 2 * R - 1, 2 * FracR + 1));

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  DrawLine := t2 >= 0;
  if DrawLine then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHVFilled(CurrX + FracR - 1, CurrY + FracR,
        True, False, X, Y) and Result;
    if DrawLine then
      Result := PointMirrorHVFilled(CurrY + FracR - 1, CurrX + FracR,
        True, False, X, Y) and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    DrawLine := t2 >= 0;
    if DrawLine then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if DrawLine and (CurrY = CurrX) then
    Result := PointMirrorHVFilled(CurrX + FracR - 1, CurrY + FracR,
      True, False, X, Y) and Result;
end;

(*
function cCHXSDL3Renderer.CircleBorderFP(const X, Y, R: CFloat): Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing pixels at cardinal/diagonal angles.
  - Minor initialization optimization. }
var
  IntX, IntY, t1, t2, CurrX, CurrY: Integer;
begin
  CurrX := Abs(Round(R));
  if CurrX < 1 then Exit(Point(X, Y));
  IntX := Round(X); IntY := Round(Y); // IntR := Round(R);

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  Result := PointMirrorV(IntX, CurrX, IntY)
    and PointMirrorH(CurrX, IntY, IntX);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  if t2 >= 0 then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHV(CurrX, CurrY, IntX, IntY)
      and PointMirrorHV(CurrY, CurrX, IntX, IntY)
      and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if CurrX = CurrY then
    Result := PointMirrorHV(CurrX, CurrY, IntX, IntY) and Result;
end;
*)

(*
function cCHXSDL3Renderer.CircleFilledFP(const X, Y, R: CFloat): Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing lines already drawn.
  - Minor initialization optimization.
  - Fill the circle with horizontal lines.
}
var
  IntX, IntY, t1, t2, CurrX, CurrY: Integer;
begin
  CurrX := Abs(Round(R));
  if R < 1 then Exit(Point(X, Y));
  IntX := Round(X); IntY := Round(Y); // IntR := Round(R);

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0° - 180°) only once.

  Result := PointMirrorHFilled(CurrX, IntY, IntX);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;

  if t2 >= 0 then
  begin
    Result := PointMirrorV(IntX, CurrX, IntY) and Result;
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHVFilled(CurrX, CurrY, True, False, IntX, IntY)
      and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
      Result := PointMirrorHVFilled(CurrY - 1, CurrX + 1, True, False,
        IntX, IntY) and Result;
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if (t2 >= 0) and (CurrY = CurrX) then
    Result := PointMirrorHVFilled(CurrX, CurrY, True, False, IntX, IntY)
      and Result;
end;

function cCHXSDL3Renderer.CircleFillOnlyFP(const X, Y, R: CFloat)
  : Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing lines already drawn.
  - Minor initialization optimization.
  - Fill the circle with horizontal lines without border. }
var
  IntX, IntY, t1, t2, CurrX, CurrY: Integer;
  DrawLine: Boolean;
begin
  CurrX := Abs(Round(R));
  if R < 1 then Exit(True); // Nothing to fill
  IntX := Round(X); IntY := Round(Y); // IntR := Round(R);

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  Result := PointMirrorHFilled(CurrX - 1, IntY, IntX);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  DrawLine := t2 >= 0;
  if DrawLine then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHVFilled(CurrX - 1, CurrY, True, False,
      IntX, IntY) and Result;
    if DrawLine then
      Result := PointMirrorHVFilled(CurrY - 1, CurrX, True, False,
        IntX, IntY) and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    DrawLine := t2 >= 0;
    if DrawLine then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if DrawLine and (CurrY = CurrX) then
    Result := PointMirrorHVFilled(CurrX - 1, CurrY, True, False,
      IntX, IntY) and Result;
end;
*)

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
