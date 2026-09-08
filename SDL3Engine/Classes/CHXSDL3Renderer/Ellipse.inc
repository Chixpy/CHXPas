{ Ellipse methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy

  In general in all methods with Alois Zingl's algorithm uses squares and
  multiplications that can overflow easily `Integer` range AND break
  `CFloat` (`Single`) unit precision wich must be handled:

  - Integer: Abs(`RX` or `RY`) > 46340 overflows Integer size. Creating an
    exception.
  - CFloat: Abs(`RX` or `RY`) > 4096 will break single unit precision
    of CFloat and can hang the program.
  - In both case variable `e2` can overflow by themself with smaller radii.

  ToDo:

  - Check visibility `((IntX - IntRX) < Width)` or `((IntY - IntRY) < Height)`
}

// Ellipse[X]

procedure cCHXSDL3Renderer.TEllipseVertices(var PArr: TSDLFPointDynArray;
  const X, Y, RX, RY: CFloat; QuadPoints: Integer);
const
  kMinTEllipse = 0; // Additional vertices for each quadrant of the ellipse
  kMaxTEllipse = 16; //
  kTEllipseRatio = {2 * } Pi / 2;
var
  Angle, dAng, aSin, aCos: CFloat;
  i, k: Integer;
begin
  if QuadPoints < kMinTEllipse then
    QuadPoints := Round(kTEllipseRatio * SqRt(Abs(Min(RX, RY))));

  if QuadPoints < kMinTEllipse then QuadPoints := kMinTEllipse
  else if QuadPoints > kMaxTEllipse then QuadPoints := kMaxTEllipse;

  SetLength(PArr, (QuadPoints + 1) * 4);

  // Basic points
  k := (QuadPoints + 1);
  PArr[0].Init(X + RX, Y);
  PArr[k].Init(X, Y - RY);
  PArr[k * 2].Init(X - RX, Y);
  PArr[k * 3].Init(X, Y + RY);

  if QuadPoints <= 0 then Exit;

  // Rest of the points.
  Angle := 0;
  dAng := (2 * Pi) / Length(PArr);
  //k := (QuadPoints + 1); // Already set
  for i := 1 to QuadPoints do
  begin
    Angle += dAng;
    SinCos(Angle, aSin, aCos);
    aCos *= RX; aSin *= RY;

    PArr[i].Init(X + aCos, Y - aSin);
    PArr[Length(PArr) - i].Init(X + aCos, Y + aSin);

    PArr[(k * 2) - i].Init(X - aCos, Y - aSin);
    PArr[(k * 2) + i].Init(X - aCos, Y + aSin);
  end;
end;

function cCHXSDL3Renderer.Ellipse(const X, Y, RX, RY: CFloat;
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
      and EllipseFilled(X, Y, RX, RY);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and EllipseFillOnly(X, Y, RX, RY);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and EllipseBorder(X, Y, RX, RY);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.EllipseBorder(const X, Y: CFloat; RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:
  - Special cases: `RX <= 1` or `RY <= 1`.
  - Avoid redrawing pixels at cardinal points.
  - Precalculate constant terms.
  - Adaptation to Logical Presentation subpixels.
}
var
  // Keep as Integer because Cfloat range with unit precision is 24bits
  //   instead 31bits of (Long) Integer.
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  FracRX, FracRY: CFloat;
begin
  RX := Abs(RX); RY := Abs(RY);
  RX2 := Floor(RX); RY2 := Floor(RY);
  FracRX := RX - RX2; FracRY := RY - RY2;

  // Special cases
  if (RX2 < 1) or (RY2 < 1) then
    // Draws both vertical and horizontal degenerate cases
    Exit(RectFilled(X - RX, Y - RY, 2 * RX + 1, 2 * RY + 1));

  try // Catch overflow exceptions
    RY2 := RY2 * RY2;

    // Initialization
    CurrX := -RX2; CurrY := 0;
    dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
    RX2 := dY * 2; RY2 := RY2 * 2;

    // 1st Iteration: Draw first points only once
    if IsZero(FracRY) then
      Result := PointMirrorH(RX, Y, X)
    else
      Result := RectFilled(X - RX, Y - FracRY, 1, 2 * FracRY + 1)
          and RectFilled(X + RX, Y - FracRY, 1, 2 * FracRY + 1);

    // Main Loop
    while CurrX < 0 do
    begin
      e2 := err * 2;
      if e2 >= dX then
      begin
        CurrX += 1; dX += RY2; err += dX;
      end;
      if e2 <= dY then
      begin
        CurrY += 1; dY += RX2; err += dY;
      end;

      if CurrX >= 0 then Break;

      Result := PointMirrorHV(CurrX - FracRX, CurrY + FracRY, X, Y) and Result;
    end;

    Result := RectFilled(X - FracRX, Y - RY,
        2 * FracRX + 1, RY - CurrY + 1 - FracRY)
      and RectFilled(X - FracRX, Y + RY + 1,
        2 * FracRX + 1, - RY + CurrY - 1 + FracRY)
      and Result;

  except
    on e: Exception do
    begin
      SDL_SetError('%s(%s) %s: %s',
        [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
        PAnsiChar(e.ToString)]);
      Exit(False);
    end;
  end;
end;

function cCHXSDL3Renderer.TEllipseBorder(const X, Y, RX, RY: CFloat;
  const QuadPoints: Integer): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  TEllipseVertices(PArr, X, Y, RX, RY, QuadPoints);
  Result := TPolygonBorder(PArr);
end;

function cCHXSDL3Renderer.EllipseFilled(const X, Y: CFloat; RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases: `RX <= 1` or `RY <= 1`
  - Fails in when RY >> RX.
  - Avoid redrawing pixels at cardinal points.
  - Fill the ellipse with horizontal lines.
  - Precalculate constant terms.
  - Adaptation to Logical Presentation subpixels.

  Use Integer for vars because overflows can be cached and don't hang the
  program.
}
var
  // Keep as Integer because Cfloat range with unit precision is 24bits
  //   instead 31bits of (Long) Integer.
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  FracRX, FracRY: CFloat;
begin
  RX := Abs(RX); RY := Abs(RY);
  RX2 := Floor(RX); RY2 := Floor(RY);
  FracRX := RX - RX2; FracRY := RY - RY2;

  // Special cases
  if (RX2 < 1) or (RY2 < 1) then
    // Draws both vertical and horizontal degenerate cases
    Exit(RectFilled(X - RX, Y - RY, 2 * RX + 1, 2 * RY + 1));

  try // Catch overflow exceptions
    RY2 := RY2 * RY2;

    // Initialization
    CurrX := -RX2; CurrY := 0;
    dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
    RX2 := dY * 2; RY2 := RY2 * 2;

    // 1st Iteration: Draw first points only once
    if IsZero(FracRY) then
      Result := RectFilled(X - RX, Y, 2 * RX + 1, 1)
    else
      Result := RectFilled(X - RX, Y - FracRY, 2 * RX + 1, 2 * FracRY + 1);

    // Main Loop
    while CurrX < 0 do
    begin
      e2 := err * 2;
      if e2 >= dX then
      begin
        CurrX += 1; dX += RY2; err += dX;
      end;
      if e2 <= dY then
      begin
        CurrY += 1; dY += RX2; err += dY;
        Result := PointMirrorHVFilled(CurrX - FracRX, CurrY + FracRY, True,
            False, X, Y)
          and Result;
      end;
    end;

    if CurrY < RY then
      Result := RectFilled(X - FracRX, Y - RY,
          2 * FracRX + 1, RY - CurrY - FracRY)
        and RectFilled(X - FracRX, Y + RY + 1,
          2 * FracRX + 1, - RY + CurrY + FracRY)
        and Result;

  except
    on e: Exception do
    begin
      SDL_SetError('%s(%s) %s: %s',
        [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
        PAnsiChar(e.ToString)]);
      Exit(False);
    end;
  end;
end;

function cCHXSDL3Renderer.TEllipseFilled(const X, Y, RX, RY: CFloat;
  const QuadPoints: Integer): Boolean;
var
  PArr: TSDLFPointDynArray;
  Indices: Array of Byte;
  FColor: TSDL_FColor;
  i, j: Integer;
begin
  TEllipseVertices(PArr, X + 0.5, Y + 0.5, RX + 0.5, RY + 0.5, QuadPoints);

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

function cCHXSDL3Renderer.EllipseFillOnly(const X, Y: CFloat; RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases: `RX <= 1` or `RY <= 1`.
  - Avoid redrawing pixels at cardinal points.
  - Fill the ellipse with horizontal lines without border.
  - Precalculate constant terms.
  - Adaptation to Logical Presentation subpixels.

  Use Integer for vars because overflows can be cached and don't hang the
  program.
}
var
  // Keep as Integer because Cfloat range with unit precision is 24bits
  //   instead 31bits of (Long) Integer.
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  FracRX, FracRY: CFloat;
begin
  RX := Abs(RX); RY := Abs(RY);
  RX2 := Floor(RX); RY2 := Floor(RY);
  FracRX := RX - RX2; FracRY := RY - RY2;

  // Special cases
  if (RX2 < 1) or (RY2 < 1) then
    Exit(True); // Nothing to fill

  try // Catch overflow exceptions
    RY2 := RY2 * RY2;

    // Initialization
    CurrX := -RX2; CurrY := 0;
    dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
    RX2 := dY * 2; RY2 := RY2 * 2;

    // 1st Iteration: Draw first points only once
    //if IsZero(FracRY) then
    //  Result := RectFilled(X - RX + 1, Y, 2 * RX - 1, 1)
    //else
    //  Result := RectFilled(X - RX + 1, Y - FracRY, 2 * RX - 1, 2 * FracRY + 1);

    // Main Loop
    while CurrX < 0 do
    begin
      e2 := err * 2;
      if e2 <= dY then
      begin
        Result := PointMirrorHVFilled(CurrX - FracRX + 1, CurrY + FracRY,
            True, False, X, Y)
          and Result;
        CurrY += 1; dY += RX2; err += dY;
      end;
      if e2 >= dX then
      begin
        CurrX += 1; dX += RY2; err += dX;
      end;
    end;

  except
    on e: Exception do
    begin
      SDL_SetError('%s(%s) %s: %s',
        [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
        PAnsiChar(e.ToString)]);
      Exit(False);
    end;
  end;
end;
(*
function cCHXSDL3Renderer.EllipseFP(const X, Y, RX, RY: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Maybe this is faster (and use normal methods)
  // X := Round(X); Y := Round(Y); RX := Round(RX); RY := Round(RY);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and EllipseFilledFP(X, Y, RX, RY);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and EllipseFillOnlyFP(X, Y, RX, RY);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and EllipseBorderFP(X, Y, RX, RY);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.EllipseBorderFP(const X, Y, RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:
  - Special cases: `RX <= 1` or `RY <= 1`.
  - Avoid redrawing pixels at cardinal points.
  - Precalculate constant terms.

  ToDo:

  - If `RX` or `RY` > Abs(46344,83) then overflows Integer size.
}
var
  IntX, IntY, IntRX, IntRY, CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
begin
  IntX := Round(X); IntY := Round(Y);
  IntRX := Abs(Round(RX)); IntRY := Abs(Round(RY));

  // Not visible
  if ((IntX + IntRX) < 0)
    or ((IntY + IntRY) < 0)
    then Exit;

  // Special cases
  if (IntRX < 1) then
    Exit(Line(X, Y - IntRY, X, Y + IntRY));
  if (IntRY < 1) then
    Exit(Line(X - IntRX, Y, X + IntRX, Y));

  RY2 := IntRY * IntRY;

  // Initialization
  CurrX := -IntRX; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := IntRX * IntRX; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  Result := PointMirrorH(IntRX, IntY, IntX);

  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
  end;
  if e2 <= dY then
  begin
    Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
  end;

  // Main Loop
  while CurrX < 0 do
  begin
    Result := PointMirrorHV(CurrX, CurrY, IntX, IntY) and Result;

    e2 := err * 2;
    if e2 <= dY then
    begin
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
    end;
    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;
  end;

  // Draw last points only once
  if CurrY < IntRY then
    Result := LineMirrorV(IntX, CurrY, IntX, IntRY, IntY)
      and Result
  else
    Result := PointMirrorV(IntX, IntRY, IntY) and Result;
end;

function cCHXSDL3Renderer.EllipseFilledFP(const X, Y, RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases: `RX <= 1` or `RY <= 1`
  - Fails in when RY >> RX.
  - Avoid redrawing pixels at cardinal points.
  - Fill the ellipse with horizontal lines.
  - Precalculate constant terms.

  ToDo:

  - If `RX` or `RY` > Abs(46344,83) then overflows Integer size.
}
var
  IntX, IntY, IntRX, IntRY, CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
begin
  IntX := Round(X); IntY := Round(Y);
  IntRX := Abs(Round(RX)); IntRY := Abs(Round(RY));

  // Not visible
  if ((IntX + IntRX) < 0)
    or ((IntY + IntRY) < 0)
    then Exit;

  // Special cases
  if (IntRX < 1) then
    Exit(Line(X, Y - IntRY, X, Y + IntRY));
  if (IntRY < 1) then
    Exit(Line(X - IntRX, Y, X + IntRX, Y));

  RY2 := IntRY * IntRY;

  // Initialization
  CurrX := -IntRX; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := IntRX * IntRX; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  Result := PointMirrorHFilled(IntRX, IntY, IntX);

  // Main Loop
  while CurrX <= 0 do // 0 included
  begin
    e2 := err * 2;

    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;

    if e2 <= dY then
    begin
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
      // Draw line after changing CurrY only. 
      Result := LineMirrorV(IntX - CurrX, CurrY, IntX + CurrX, CurrY, IntY)
        // PointMirrorHVFilled(CurrX, CurrY, True, False, IntX, IntY)
        and Result;
    end;
  end;

  // Draw last points only once
  if CurrY < IntRY then
    Result := LineMirrorV(IntX, CurrY + 1, IntX, IntRY, IntY)
      and Result;
end;

function cCHXSDL3Renderer.EllipseFillOnlyFP(const X, Y, RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases: `RX <= 1` or `RY <= 1`.
  - Avoid redrawing pixels at cardinal points.
  - Fill the ellipse with horizontal lines without border.
  - Precalculate constant terms.

  ToDo:

  - If `RX` or `RY` > Abs(46344,83) then overflows Integer size.
}
var
  IntX, IntY, IntRX, IntRY, CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  DrawLine: Boolean;
begin
  IntX := Round(X); IntY := Round(Y);
  IntRX := Abs(Round(RX)); IntRY := Abs(Round(RY));

  // Not visible
  if ((IntX + IntRX) <= 0)
    or ((IntY + IntRY) <= 0)
    then Exit;

  // Special cases
  if (RX2 < 1) or (RY2 < 1) then
    Exit(True); // Nothing to fill, well it's a subpixel box

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  //Result := PointMirrorHFilled(IntRX - 1, IntY, IntX);

  Result := True;

  // Skip first iteration
  e2 := err * 2;
  if e2 <= dY then
  begin
    Result := PointMirrorHFilled(IntRX - 1, IntY, IntX)
        and Result;
    Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
  end;
  if e2 >= dX then
  begin
    Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
  end;

  // Main Loop
  while CurrX < 0 do
  begin
    e2 := err * 2;
    DrawLine := e2 <= dY;
    if DrawLine then
    begin
      Result := PointMirrorHVFilled(CurrX + 1, CurrY, True, False, IntX, IntY)
        and Result;
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
    end;
    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;
  end;
end;
*)

// EllipseInRect[x]

function cCHXSDL3Renderer.EllipseInRect(const aRect: TSDL_FRect;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := EllipseInRect(aRect.X, aRect.Y, aRect.W, aRect.H, BorderC,
    FillC);
end;

function cCHXSDL3Renderer.EllipseInRect(const X, Y, W, H: CFloat;
 const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := SetDrawColor(BorderC)
      and EllipseInRectFilled(X, Y, W, H)
      and Result;

    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := SetDrawColor(FillC)
      and EllipseInRectFillOnly(X, Y, W, H)
      and Result;
   end;

  if BorderC.A > 0 then
  begin
    Result := SetDrawColor(BorderC)
      and EllipseInRectBorder(X, Y, W, H)
      and Result;
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.EllipseInRectBorder(const aRect: TSDL_FRect)
  : Boolean;
begin
  Result := EllipseInRectBorder(aRect.X, aRect.Y, aRect.W, aRect.H);
end;

function cCHXSDL3Renderer.EllipseInRectBorder(X, Y, W, H: CFloat)
  : Boolean;
{ With integers there are algorithms to rasterize this ellipses.

  With floats... we can call EllipseBorder with tweaked parameters.
}
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin W := -W; X -= W; end;
  if H < 0 then
    begin H := -H; Y -= H; end;
  W := (W - 1) * 0.5; H := (W - 1) * 0.5;
  Result := EllipseFilled(X + W, Y + H, W, H);
end;

function cCHXSDL3Renderer.EllipseInRectFilled(const aRect: TSDL_FRect)
  : Boolean;
begin
  Result := EllipseInRectFilled(aRect.X, aRect.Y, aRect.W, aRect.H);
end;

function cCHXSDL3Renderer.EllipseInRectFilled(X, Y, W, H: CFloat): Boolean;
{ With integers there are algorithms to rasterize this ellipses.

  With floats... we can call EllipseFilled with tweaked parameters.
}
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin W := -W; X -= W; end;
  if H < 0 then
    begin H := -H; Y -= H; end;
  W := (W - 1) * 0.5; H := (W - 1) * 0.5;
  Result := EllipseFilled(X + W, Y + H, W, H);
end;

function cCHXSDL3Renderer.EllipseInRectFillOnly(const aRect: TSDL_FRect)
 : Boolean;
begin
  Result := EllipseInRectFillOnly(aRect.X, aRect.Y, aRect.W, aRect.H);
end;

function cCHXSDL3Renderer.EllipseInRectFillOnly(X, Y, W, H: CFloat): Boolean;
{ With integers there are algorithms to rasterize this ellipses.

  With floats... we can call EllipseFillOnly with tweaked parameters.
}
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin W := -W; X -= W; end;
  if H < 0 then
    begin H := -H; Y -= H; end;
  W := (W - 1) * 0.5; H := (W - 1) * 0.5;
  Result := EllipseFilled(X + W, Y + H, W, H);
end;
(*
function cCHXSDL3Renderer.EllipseInRectFP(const aRect: TSDL_FRect;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := EllipseInRect(aRect.X, aRect.Y, aRect.W, aRect.H, BorderC,
    FillC);
end;

function cCHXSDL3Renderer.EllipseInRectFP(X, Y, W, H: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Maybe this is the simplest ways to do it (and call normal methods).
  X := Round(X); Y := Round(Y); W := Round(W); H := Round(H);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and EllipseInRectFilledFP(X, Y, W, H);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
   Result := Result
     and SetDrawColor(FillC)
     and EllipseInRectFillOnlyFP(X, Y, W, H);
  end;

  if BorderC.A > 0 then
  begin
   Result := Result
     and SetDrawColor(BorderC)
     and EllipseInRectBorderFP(X, Y, W, H);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.EllipseInRectBorderFP(const aRect: TSDL_FRect)
  : Boolean;
begin
  Result := EllipseInRectBorder(aRect.X, aRect.Y, aRect.W, aRect.H);
end;

function cCHXSDL3Renderer.EllipseInRectBorderFP(const X, Y: CFloat;
  W, H: CFloat): Boolean;
{ Draw an ellipse inside a rectangle.

  Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases.
  - Avoid redrawing pixels some points.
  - Precalculate constant terms.

  ToDo:

  - Untested.
}
var
  X0, X1, Y0, Y1, a, b, aa, bb, b1, dX, dY, err, e2: Integer;
begin
  // Integer ellipse coordinates
  if W > 0 then
  begin
    X0 := Round(X); X1 := Round(X + W);
  end
  else
  begin
    X0 := Round(X + W); X1 := Round(X);
  end;

  if H > 0 then
  begin
    Y0 := Round(Y); Y1 := Round(Y + H);
  end
  else
  begin
    Y0 := Round(Y + H); Y1 := Round(Y);
  end;

  a := X1 - X0; b := Y1 - Y0;

  // Special cases
  if (a <= 1) or (b <= 1) then
    Exit(RectBorder(SDLFRect(X0, Y0, a, b)));

  // Initialization
  aa := a * a; bb := b * b; b1 := b mod 2;
  dX := (1 - a) * bb * 4; dY := ((b1 + 1) * aa) * 4; err := dX + dY + b1 * aa;
  Y0 := Y0 + (b + 1) div 2; Y1 := Y0 - b1;
  a := aa * 8; b1 := bb * 8;

  // 1st Iteration
  if Y0 = Y1 then
    Result := Point(X0, Y0) and Point(X1, Y1)
  else
    Result := Point(X0, Y0) and Point(X0, Y1) and Point(X1, Y0)
      and Point(X1, Y1);

  e2 := err * 2;
  if e2 < dY then
  begin
    Inc(Y0); Dec(Y1); Inc(dY, a); Inc(err, dY);
  end;

  if (e2 > dX) or ((err * 2) > dY) then
  begin
    Inc(X0); Dec(X1); Inc(dX, b1); Inc(err, dX);
  end;

  // Main loop
  while X0 < X1 do
  begin
    Result := Point(X0, Y0) and Point(X1, Y0) and Point(X0, Y1)
      and Point(X1, Y1) and Result;

    e2 := err * 2;
    if e2 < dY then
    begin
      Inc(Y0); Dec(Y1); Inc(dY, a); Inc(err, dY);
    end;

    if (e2 > dX) or ((err * 2) > dY) then
    begin
      Inc(X0); Dec(X1); Inc(dX, b1); Inc(err, dX);
    end;
  end;

  // Last points
  if (X0 = X1) then
    Result := Point(X0, Y0) and Point(X1, Y1) and Result;
end;

function cCHXSDL3Renderer.EllipseInRectFilledFP(const aRect: TSDL_FRect)
  : Boolean;
begin
  Result := EllipseInRectFilled(aRect.X, aRect.Y, aRect.W, aRect.H);
end;

function cCHXSDL3Renderer.EllipseInRectFilledFP(const X, Y: CFloat;
  W, H: CFloat): Boolean;
{ Draw an ellipse inside a rectangle.

  Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases.
  - Avoid redrawing pixels some points.
  - Precalculate constant terms.

  ToDo:

  - Untested.
}
var
  X0, X1, Y0, Y1, a, b, aa, bb, b1, dX, dY, err, e2: Integer;
  DrawLine: Boolean;
begin
  // Integer ellipse coordinates
  if W > 0 then
  begin
    X0 := Round(X); X1 := Round(X + W);
  end
  else
  begin
    X0 := Round(X + W); X1 := Round(X);
  end;

  if H > 0 then
  begin
    Y0 := Round(Y); Y1 := Round(Y + H);
  end
  else
  begin
    Y0 := Round(Y + H); Y1 := Round(Y);
  end;

  a := X1 - X0; b := Y1 - Y0;

  // Special cases
  if (a <= 1) or (b <= 1) then
    Exit(RectFilled(SDLFRect(X0, Y0, a, b)));

  // Initialization
  aa := a * a; bb := b * b; b1 := b mod 2;
  dX := (1 - a) * bb * 4; dY := ((b1 + 1) * aa) * 4; err := dX + dY + b1 * aa;
  Y0 := Y0 + (b + 1) div 2; Y1 := Y0 - b1;
  a := aa * 8; b1 := bb * 8;

  // 1st Iteration: Nothing to fill
  if Y0 = Y1 then
    Result := Line(X0, Y0, X1, Y1)
  else
    Result := Line(X0, Y0, X1, Y0) and Line(X0, Y1, X1, Y1);

  e2 := err * 2; Drawline := e2 < dY;
  if Drawline then
  begin
    Inc(Y0); Dec(Y1); Inc(dY, a); Inc(err, dY);
  end;

  DrawLine := (e2 > dX) or ((err * 2) > dY);
  if DrawLine then
  begin
    Inc(X0); Dec(X1); Inc(dX, b1); Inc(err, dX);
  end;

  // Main loop
  while X0 < X1 do
  begin
    if DrawLine then
      Result := Line(X0, Y0, X0, Y1) and Line(X1, Y0, X1, Y1) and Result;

    e2 := err * 2; Drawline := e2 < dY;
    if Drawline then
    begin
      Inc(Y0); Dec(Y1); Inc(dY, a); Inc(err, dY);
    end;

    DrawLine := (e2 > dX) or ((err * 2) > dY);
    if DrawLine then
    begin
      Inc(X0); Dec(X1); Inc(dX, b1); Inc(err, dX);
    end;
  end;

  // Last points
  if DrawLine and (X0 = X1) then
    Result := Line(X0, Y0, X1, Y1) and Result;
end;

function cCHXSDL3Renderer.EllipseInRectFillOnlyFP(const aRect: TSDL_FRect)
 : Boolean;
begin
  Result := EllipseInRectFillOnly(aRect.X, aRect.Y, aRect.W, aRect.H);
end;

function cCHXSDL3Renderer.EllipseInRectOnlyFillFP(const X, Y, W, H: CFloat)
  : Boolean;
{ Draw an ellipse inside a rectangle.

  Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases.
  - Avoid redrawing pixels some points.
  - Precalculate constant terms.

  ToDo:

  - Change to Draw horizontally instead vertically. It must Draw **previous**
    line when changing Y. See `cCHXSDL3Renderer.EllipseFillOnly`.
  - Untested.
}
var
  X0, X1, Y0, Y1, a, b, aa, bb, b1, dX, dY, err, e2: Integer;
begin
  // Integer ellipse coordinates
  if W > 0 then
  begin
    X0 := Round(X); X1 := Round(X + W);
  end
  else
  begin
    X0 := Round(X + W); X1 := Round(X);
  end;

  if H > 0 then
  begin
    Y0 := Round(Y); Y1 := Round(Y + H);
  end
  else
  begin
    Y0 := Round(Y + H); Y1 := Round(Y);
  end;

  a := X1 - X0; b := Y1 - Y0;

  // Special cases
  if (a <= 1) or (b <= 1) then
    Exit(True); // Nothing to fill, but OK

  // Initialization
  aa := a * a; bb := b * b; b1 := b mod 2;
  dX := (1 - a) * bb * 4; dY := ((b1 + 1) * aa) * 4; err := dX + dY + b1 * aa;
  Y0 := Y0 + (b + 1) div 2; Y1 := Y0 - b1;
  a := aa * 8; b1 := bb * 8;

  // 1st Iteration: Nothing to fill, wait until change Y change.
  Result := True;

  // Skip first iteration. ToDo: Is needed to skips?
  e2 := err * 2;
  if e2 < dY then
  begin
    Inc(Y0); Dec(Y1); Inc(dY, a); Inc(err, dY);
  end;
  if (e2 > dX) or ((err * 2) > dY) then
  begin
    Inc(X0); Dec(X1); Inc(dX, b1); Inc(err, dX);
  end;

  // Main loop
  while X0 < X1 do
  begin
    e2 := err * 2;
    if e2 < dY then
    begin
      // Draw before Y changes
      Result := Line(X0 + 1, Y0, X1 - 1, Y0) and Line(X0 + 1, Y1, X1 - 1, Y1)
        and Result;
      Inc(Y0); Dec(Y1); Inc(dY, a); Inc(err, dY);
    end;

    if (e2 > dX) or ((err * 2) > dY) then
    begin
      Inc(X0); Dec(X1); Inc(dX, b1); Inc(err, dX);
    end;
  end;
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
