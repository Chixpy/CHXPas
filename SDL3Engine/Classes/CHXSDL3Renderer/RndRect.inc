{ Rounded Axix Aligned Rectangle methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

{
  SPRndRectC[x]
}

function cCHXSDL3Renderer.SPRndRectC(const aRect : TSDL_FRect; const R : CFloat;
  const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := SPRndRectC(aRect.X, aRect.Y, aRect.W, aRect.H, R, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPRndRectC(const X, Y, W, H, R : CFloat;
  const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := SPRndRectCFilled(X, Y, W, H, R)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := SPRndRectCFillOnly(X, Y, W, H, R)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and SPRndRectCBorder(X, Y, W, H, R)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.SPRndRectCBorder(const aRect : TSDL_FRect;
  const R : CFloat) : Boolean;
begin
  Result := SPRndRectCBorder(aRect.X, aRect.Y, aRect.W, aRect.H, R);
end;

function cCHXSDL3Renderer.SPRndRectCBorder(X, Y, W, H, R : CFloat) : Boolean;
// Jesko's method for corners + adaptation to Logical Presentation
var
  t1, t2, CurrX, CurrY : CFloat; // Circle algorithm vars
  CX, CY : CFloat; // Center of the rectangle
  W2, H2 : CFloat; // Half sides
  OffSetX, OffSetY : CFloat; // Offset of the center of the arcs
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(SPRectFilled(X, Y, W, H));

  // Radius check.
  CurrX := Abs(R);
  if CurrX < 1 then Exit(SPRectBorder(X, Y, W, H));

  // Checking if radius is too big and setting integer R.
  CurrX := Floor(Min(Min(CurrX, (H - 1) * 0.5), (W - 1) * 0.5));

  W2 := (W - 1) * 0.5; H2 := (H - 1) * 0.5;
  CX := X + W2; CY := Y + H2;
  OffSetX := W2 - CurrX; OffSetY := H2 - CurrX;

  // Rect sides
  Result := SPPointMirrorHFilled(OffSetX, Y, CX)
    and SPPointMirrorHFilled(OffSetX, Y + H - 1, CX)
    and SPPointMirrorVFilled(X, OffSetY, CY)
    and SPPointMirrorVFilled(X + W - 1, OffSetY, CY);

  // Little devil trick for negative radius
  if R < 0 then
    begin OffSetX := -W2 - 1; OffSetY := -H2 - 1; end;

  // Initial Jesko setup.
  CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;

  while True do // CurrX > CurrY do
  begin
    if t2 >= 0 then
      begin t1 := t2; CurrX -= 1; end;

    // This way we don't repeat previous code
    if CurrX <= CurrY then Break;

    Result := SPPointMirrorHV(OffSetX + CurrX, OffSetY + CurrY, CX, CY)
          and SPPointMirrorHV(OffSetX + CurrY, OffSetY + CurrX, CX, CY)
          and Result;

    CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
  end;

  // Drawing 45°, etc. only once
  if SameValue(CurrX, CurrY) then
    Result := SPPointMirrorHV(OffSetX + CurrX, OffSetY + CurrY, CX, CY)
      and Result;
end;

function cCHXSDL3Renderer.SPRndRectCFilled(const aRect : TSDL_FRect;
  const R : CFloat) : Boolean;
begin
  Result := SPRndRectCFilled(aRect.X, aRect.Y, aRect.W, aRect.H, R);
end;

function cCHXSDL3Renderer.SPRndRectCFilled(X, Y, W, H, R : CFloat) : Boolean;
// Jesko's method for corners + adaptation to Logical Presentation
var
  t1, t2, CurrX, CurrY : CFloat; // Circle algorithm vars
  CX, CY : CFloat; // Center of the rectangle
  W2, H2 : CFloat; // Half sides
  OffSetX, OffSetY : CFloat; // Offset of the center of the arcs
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(SPRectFilled(X, Y, W, H));

  // Radius check.
  CurrX := Abs(R);
  if CurrX < 1 then Exit(SPRectFilled(X, Y, W, H));

  // Checking if radius is too big and setting integer R.
  CurrX := Floor(Min(Min(CurrX, (H - 1) * 0.5), (W - 1) * 0.5));

  W2 := (W - 1) * 0.5; H2 := (H - 1) * 0.5;
  CX := X + W2; CY := Y + H2;

  // I tried to combine them, but implies drawing at different moment.
  if R > 0 then // Filled Positive Radius
  begin
    // Rect sides
    Result := SPRectFilled(X, Y + CurrX, W, H - CurrX - CurrX);

    OffSetX := W2 - CurrX; OffSetY := H2 - CurrX;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;
    while True do
    begin
      if t2 >= 0 then
      begin
        Result := SPPointMirrorHVFilled(OffSetX + CurrY - 1, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
        t1 := t2; CurrX -= 1;
      end;

      if CurrX <= CurrY then Break;

      Result := SPPointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;

    // Drawing 45°, etc. only once
    if SameValue(CurrX, CurrY) then
      Result := SPPointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
        True, False, CX, CY)
        and Result;
  end
  else // Filled Negative Radius
  begin
    // Rect sides
    Result := SPRectFilled(X, Y + CurrX - 1, W, H - CurrX - CurrX + 2);

    OffSetX := -W2 - 1; OffSetY := -H2 - 1;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;
    while CurrX > CurrY do
    begin
      if t2 >= 0 then
      begin
        t1 := t2; CurrX -= 1;
        Result := SPPointMirrorHVFilled(OffSetX + CurrY, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
      end;

      if CurrX <= CurrY then Break;

      Result := SPPointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;
  end;
end;

function cCHXSDL3Renderer.SPRndRectCFillOnly(const aRect : TSDL_FRect;
  const R : CFloat) : Boolean;
begin
  Result := SPRndRectCFillOnly(aRect.X, aRect.Y, aRect.W, aRect.H, R);
end;

function cCHXSDL3Renderer.SPRndRectCFillOnly(X, Y, W, H, R : CFloat) : Boolean;
// Jesko's method for corners + adaptation to Logical Presentation
var
  t1, t2, CurrX, CurrY : CFloat; // Circle algorithm vars
  CX, CY : CFloat; // Center of the rectangle
  W2, H2 : CFloat; // Half sides
  OffSetX, OffSetY : CFloat; // Offset of the center of the arcs
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 3) or (H < 3) then
    Exit(SPRectFillOnly(X, Y, W, H));

  // Radius check.
  CurrX := Abs(R);
  if CurrX < 1 then Exit(SPRectFillOnly(X, Y, W, H));

  // Checking if radius is too big and setting integer R.
  CurrX := Floor(Min(Min(CurrX, (H - 1) * 0.5), (W - 1) * 0.5));

  W2 := (W - 1) * 0.5; H2 := (H - 1) * 0.5;
  CX := X + W2; CY := Y + H2;

  // I tried to combine them, but implies drawing at different moment.
  if R > 0 then // Fill Only Positive Radius
  begin
    // Rect sides
    Result := SPRectFilled(X + 1, Y + CurrX, W - 2, H - CurrX - CurrX);

    OffSetX := W2 - CurrX; OffSetY := H2 - CurrX;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;
    while CurrX > CurrY do
    begin
      if t2 >= 0 then
      begin
        t1 := t2; CurrX -= 1;
        Result := SPPointMirrorHVFilled(OffSetX + CurrY - 1, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
      end;

      if CurrX <= CurrY then Break;

      Result := SPPointMirrorHVFilled(OffSetX + CurrX - 1, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;
  end
  else // Fill Only Negative Radius
  begin
    // Rect sides
    Result := SPRectFilled(X + 1, Y + CurrX, W - 2, H - CurrX - CurrX);

    OffSetX := -W2 - 1; OffSetY := -H2 - 1;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;

    // Skip 1st step, its border
    if t2 >= 0 then
      begin t1 := t2; CurrX -= 1; end;
    CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;

    while CurrX > CurrY do
    begin
      if t2 >= 0 then
      begin
        Result := SPPointMirrorHVFilled(OffSetX + CurrY, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
        t1 := t2; CurrX -= 1;
      end;

      //if CurrX <= CurrY then Break;

      Result := SPPointMirrorHVFilled(OffSetX + CurrX + 1, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;

    // Drawing 45°, etc. only once
    if SameValue(CurrX, CurrY) then
      Result := SPPointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;
  end;
end;

{
  SPRndRectE[x]
}

function cCHXSDL3Renderer.SPRndRectE(const aRect : TSDL_FRect;
  const Rx, RY : CFloat; const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := SPRndRectE(aRect.X, aRect.Y, aRect.W, aRect.H, RX, RY,
    BorderC, FillC);
end;

function cCHXSDL3Renderer.SPRndRectE(const X, Y, W, H, RX, RY : CFloat;
  const BorderC, FillC : TSDL_FColor) : Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := SPRndRectEFilled(X, Y, W, H, RX, RY)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := SPRndRectEFillOnly(X, Y, W, H, RX, RY)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and SPRndRectEBorder(X, Y, W, H, RX, RY)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.SPRndRectEBorder(const aRect : TSDL_FRect;
  const RX, RY : CFloat) : Boolean;
begin
  Result := SPRndRectEBorder(aRect.X, aRect.Y, aRect.W, aRect.H, RX, RY);
end;

function cCHXSDL3Renderer.SPRndRectEBorder(X, Y, W, H, RX, RY : CFloat)
  : Boolean;
{ Modification of my modificaction of my modification of Alois Zingl's
  implementation (https://zingl.github.io) of Bresenham's algorithm:
}
var
  // Keep as Integer because Cfloat range with unit precision is 24bits
  //   instead 31bits of (Long) Integer.
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer; // Algorithm variables
  CY, CX: CFloat; // Center of the Rect
  RectX, RectY: CFloat; // Half of Rects borders
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(SPRectFilled(X, Y, W, H));

  RectX := Abs(RX); RectY := Abs(RY); // Temp usage for absolute radii
  // Not rounded corners
  if (RectX < 1) or (RectY < 1) then
    Exit(SPRectBorder(X, Y, W, H));

  // Checking maximum posible radii
  RX := Min(RectX, W * 0.5);
  RY := Min(RectY, H * 0.5);

  // Integer radii for Logical Presentation
  RX2 := Floor(RX); RY2 := Floor(RY);

  // Precalculating coordinates
  W := 0.5 * (W - 1); H := 0.5 * (H - 1); // Half rect lenghts
  CX := X + W; CY := Y + H; // Center of the rect
  RectX := W - RX2; RectY := H - RY2; // Center ellipse offset

  try // Catch overflow exceptions
    RY2 := RY2 * RY2;

    // Initialization
    CurrX := -RX2; CurrY := 0;
    dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
    RX2 := dY * 2; RY2 := RY2 * 2;

    // 1st Iteration: Left and right rectangle borders
    if IsZero(RectY) then
      Result := SPPointMirrorH(W, CY, CX)
    else
      Result := SPRectFilled(CX - W, CY - RectY, 1, 2 * RectY + 1)
          and SPRectFilled(CX + W, CY - RectY, 1, 2 * RectY + 1);

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

      Result := SPPointMirrorHV(CurrX - RectX, CurrY + RectY, CX, CY)
        and Result;
    end;

    // Finish vertical ellipse
    if CurrY < (RY - 1) then
      Result := SPLineMirrorHV(CurrX - RectX, CurrY + RectY,
        CurrX - RectX, H - 1, CX, CY) and Result;

    // Top and bottom rectangle borders
    Result := SPRectFilled(CX - RectX, CY - H, 2 * RectX + 1, 1)
      and SPRectFilled(CX - RectX, CY + H, 2 * RectX + 1, 1)
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

function cCHXSDL3Renderer.SPRndRectEFilled(const aRect : TSDL_FRect;
  const RX, RY : CFloat) : Boolean;
begin
  Result := SPRndRectEFilled(aRect.X, aRect.Y, aRect.W, aRect.H, RX, RY);
end;

function cCHXSDL3Renderer.SPRndRectEFilled(X, Y, W, H, RX, RY : CFloat)
  : Boolean;
{ Modification of my modificaction of my modification of Alois Zingl's
  implementation (https://zingl.github.io) of Bresenham's algorithm:
}
var
  // Keep as Integer because Cfloat range with unit precision is 24bits
  //   instead 31bits of (Long) Integer.
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer; // Algorithm variables
  CY, CX: CFloat; // Center of the Rect
  RectX, RectY: CFloat; // Half of Rects borders
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(SPRectFilled(X, Y, W, H));

  // Not rounded corners
  RectX := Abs(RX); RectY := Abs(RY); // Temp usage for absolute radii
  if (RectX < 1) or (RectY < 1) then
    Exit(SPRectFilled(X, Y, W, H));

  // Checking maximum posible radii
  RX := Min(RectX, W * 0.5);
  RY := Min(RectY, H * 0.5);

  // Integer radii for Logical Presentation
  RX2 := Floor(RX); RY2 := Floor(RY);

  // Precalculating coordinates
  W := 0.5 * (W - 1); H := 0.5 * (H - 1); // Half rect lenghts
  CX := X + W; CY := Y + H; // Center of the rect
  RectX := W - RX2; RectY := H - RY2; // Center ellipse offset

  try // Catch overflow exceptions
    RY2 := RY2 * RY2;

    // Initialization
    CurrX := -RX2; CurrY := 0;
    dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
    RX2 := dY * 2; RY2 := RY2 * 2;

    // 1st Iteration: Central rectangle
    if IsZero(RectY) then
      Result := SPPointMirrorHFilled(W, CY, CX)
    else
      Result := SPRectFilled(CX - W, CY - RectY, 2 * W + 1, 2 * RectY + 1);

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
        Result := SPPointMirrorHVFilled(CurrX - RectX, CurrY + RectY,
          True, False, CX, CY)
          and Result;
      end;
    end;

    // Top and bottom on very thin
    if CurrY < (RY - 1) then
      Result := SPRectFilled(CX + CurrX - RectX, CY - H,
          (-CurrX + RectX) * 2 + 1, Floor(RY - CurrY))
        and SPRectFilled(CX + CurrX - RectX, CY + RectY + CurrY + 1,
          (-CurrX + RectX) * 2 + 1, Floor(RY - CurrY))
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

function cCHXSDL3Renderer.SPRndRectEFillOnly(const aRect : TSDL_FRect;
  const RX, RY : CFloat) : Boolean;
begin
  Result := SPRndRectEFillOnly(aRect.X, aRect.Y, aRect.W, aRect.H, RX, RY);
end;

function cCHXSDL3Renderer.SPRndRectEFillOnly(X, Y, W, H, RX, RY : CFloat)
  : Boolean;
{ Modification of my modificaction of my modification of Alois Zingl's
  implementation (https://zingl.github.io) of Bresenham's algorithm:
}
var
  // Keep as Integer because Cfloat range with unit precision is 24bits
  //   instead 31bits of (Long) Integer.
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer; // Algorithm variables
  CY, CX: CFloat; // Center of the Rect
  RectX, RectY: CFloat; // Half of Rects borders
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(SPRectFilled(X, Y, W, H));

  // Not rounded corners
  RectX := Abs(RX); RectY := Abs(RY); // Temp usage for absolute radii
  if (RectX < 1) or (RectY < 1) then
    Exit(SPRectFilled(X, Y, W, H));

  // Checking maximum posible radii
  RX := Min(RectX, W * 0.5);
  RY := Min(RectY, H * 0.5);

  // Integer radii for Logical Presentation
  RX2 := Floor(RX); RY2 := Floor(RY);

  // Precalculating coordinates
  W := 0.5 * (W - 1); H := 0.5 * (H - 1); // Half rect lenghts
  CX := X + W; CY := Y + H; // Center of the rect
  RectX := W - RX2; RectY := H - RY2; // Center ellipse offset

  try // Catch overflow exceptions
    RY2 := RY2 * RY2;

    // Initialization
    CurrX := -RX2; CurrY := 0;
    dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
    RX2 := dY * 2; RY2 := RY2 * 2;

    // Central rectangle
    Result := SPRectFilled(CX - W + 1, CY - RectY,
        2 * W - 1, 2 * RectY + 1);

    // Skip 1st iteration
    e2 := err * 2;
    if e2 >= dX then
    begin
      CurrX += 1; dX += RY2; err += dX;
    end;
    if e2 <= dY then
    begin
      CurrY += 1; dY += RX2; err += dY;
    end;

    // Main Loop
    while CurrX < 0 do
    begin
      e2 := err * 2;
      if e2 <= dY then
      begin
        Result := SPPointMirrorHVFilled(CurrX - RectX + 1, CurrY + RectY,
          True, False, CX, CY)
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

{
  FPRndRect[x]
}
(*
function cCHXSDL3Renderer.FPRndRectEBorder(const X, Y, W, H, RX, RY: CFloat): Boolean;
// Dibuja un rectángulo con esquinas redondeadas pero con distintos radios para X e Y.
// Modificación de mi modificación del algoritmo de Zingl
var
  CurrX, CurrY, dX, dY, err, e2, X2, Y2, W2, H2, RX2, RY2: Integer;
begin
  RX2 := Abs(Round(RX));
  RY2 := Abs(Round(RY));
  if RX2 = RY2 then
    Exit(RndRectCBorder(X, Y, W, H, RX)); // If Jesko is faster...

  W2 := Abs(Round(W));
  H2 := Abs(Round(H));
  if (W2 <= 1) or (H2 <= 1) then
    Exit(RectangleBorder(X, Y, W, H));

  if W < 0 then
    X2 := Round(X + W)
  else
    X2 := Round(X);

  if H < 0 then
    Y2 := Round(Y + H)
  else
    Y2 := Round(Y);

  // Adjust radii if they are too big. And ellipse offsets
  W2 := W2 div 2; 
  H2 := H2 div 2;
  RX2 := Min([RX2, W2]); 
  RY2 := Min([RY2, H2]);
  W2 := W2 - RX2; 
  H2 := H2 - RY2;

  // Zingl setup
  RY2 := RY2 * RY2;
  CurrX := -RX2; 
  CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; 
  dY := RX2 * RX2; 
  err := dX + dY;
  RX2 := dY * 2; 
  RY2 := RY2 * 2;

  // 1st Iteration: 1st corners point and left-right borders
  Result := PointMirrorHVFilled(CurrX - W2, CurrY + H2, X2, Y2, False, True);

  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); 
    Inc(dX, RY2); 
    Inc(err, dX);
  end;

  if e2 <= dY then
  begin
    Inc(CurrY); 
    Inc(dY, RX2); 
    Inc(err, dY);
  end;

  // Corners loop
  while CurrX < 0 do
  begin
    Result := PointMirrorHV(CurrX - W2, CurrY + H2, X2, Y2) and Result;

    e2 := err * 2;
    if e2 >= dX then
    begin
      Inc(CurrX); 
      Inc(dX, RY2); 
      Inc(err, dX);
    end;

    if e2 <= dY then
    begin
      Inc(CurrY); 
      Inc(dY, RX2); 
      Inc(err, dY);
    end;
  end;

  Result := PointMirrorHVFilled(-W2, CurrY + H2, X2, Y2, True, False) and Result;
end;

function cCHXSDL3Renderer.FPRndRectEFilled(const X, Y, W, H, RX, RY: CFloat): Boolean;
// Usando Zingl remodificado
var
  CurrX, CurrY, dX, dY, err, e2, X2, Y2, W2, H2, RX2, RY2, RYi: Integer;
  DrawLine: Boolean;
begin
  RX2 := Abs(Round(RX)); 
  RYi := Abs(Round(RY));
  if RX2 = RYi then
    Exit(RndRectCFilled(X, Y, W, H, RX)); // If Jesko method is faster.

  W2 := Abs(Round(W)); 
  H2 := Abs(Round(H));
  if (W2 <= 1) or (H2 <= 1) then
    Exit(RectangleFilled(X, Y, W, H));

  if W < 0 then
    X2 := Round(X + W)
  else
    X2 := Round(X);

  if H < 0 then
    Y2 := Round(Y + H)
  else
    Y2 := Round(Y);

  // Adjust radii, if they are too big
  W2 := W2 div 2; 
  H2 := H2 div 2;
  RX2 := Min([RX2, W2]); 
  RYi := Min([RYi, H2]);

  // Ellipse offset
  W2 := W2 - RX2; 
  H2 := H2 - RYi;

  // Zingl ellipse setup
  RY2 := RYi * RYi;
  CurrX := -RX2; 
  CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; 
  dY := RX2 * RX2; 
  err := dX + dY;
  RX2 := dY * 2; 
  RY2 := RY2 * 2;

  // 1st interacción. 1st points and center rectangle
  Result := RectangleFilled(X2, Y2 + RYi, W2, H2 - 2 * RYi);

  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); 
    Inc(dX, RY2); 
    Inc(err, dX);
  end;

  DrawLine := e2 <= dY;
  if DrawLine then
  begin
    Inc(CurrY); 
    Inc(dY, RX2); 
    Inc(err, dY);
  end;

  // Main loop
  while CurrX < 0 do
  begin
    if DrawLine then
      Result := PointMirrorHVFilled(CurrX - W2, CurrY + H2, X2, Y2, True, False) and Result;

    e2 := err * 2;
    if e2 >= dX then
    begin
      Inc(CurrX); 
      Inc(dX, RY2); 
      Inc(err, dX);
    end;

    DrawLine := e2 <= dY;
    if DrawLine then
    begin
      Inc(CurrY); 
      Inc(dY, RX2); 
      Inc(err, dY);
    end;
  end;

  // Esta vez podría incluirse en el bucle principal, pero de esta forma evitamos los cálculos posteriores
  if DrawLine then
    Result := PointMirrorHVFilled(CurrX - W2, CurrY + H2, X2, Y2, True, False) and Result;
end;

function cCHXSDL3Renderer.FPRndRectEOnlyFill(const X, Y, W, H, RX, RY: CFloat): Boolean;
// Modificación de la modificación del algoritmo de Zingl
var
  CurrX, CurrY, dX, dY, err, e2, X2, Y2, W2, H2, RXi, RX2, RY2: Integer;
  DrawLine: Boolean;
begin
  RXi := Abs(Round(RX)); 
  RY2 := Abs(Round(RY));
  if RXi = RY2 then
    Exit(RndRectCOnlyFill(X, Y, W, H, RX));

  W2 := Abs(Round(W)); 
  H2 := Abs(Round(H));
  if (W2 <= 1) or (H2 <= 1) then
    Exit(True); // Nothing to fill, but OK

  if W < 0 then
    X2 := Round(X + W)
  else
    X2 := Round(X);

  if H < 0 then
    Y2 := Round(Y + H)
  else
    Y2 := Round(Y);

  // Adjust radii, if they are too big. And ellipse offset
  W2 := W2 div 2; 
  H2 := H2 div 2;
  RXi := Min([RXi, W2]); 
  RY2 := Min([RY2, H2]);
  W2 := W2 - RXi; 
  H2 := H2 - RY2;

  // Zingl ellipse setup
  RY2 := RY2 * RY2;
  CurrX := -RXi; 
  CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; 
  dY := RXi * RXi; 
  err := dX + dY;
  RX2 := dY * 2; 
  RY2 := RY2 * 2;

  // 1st iteration: Don't draw anything, they are borders and we are filling vertically
  // If horizontal draw is faster, we can swap X and Y for calculate and swap on draw
  e2 := err * 2;
  DrawLine := e2 > dX;
  if DrawLine then
  begin
    Inc(CurrX); 
    Inc(dX, RY2); 
    Inc(err, dX);
  end;

  if e2 <= dY then
  begin
    Inc(CurrY); 
    Inc(dY, RX2); 
    Inc(err, dY);
  end;

  // Main loop
  while CurrX <= 0 do
  begin
    if DrawLine then
      Result := PointMirrorHVFilled(CurrX - W2, CurrY + H2 - 1, X2, Y2, False, True) and Result;

    e2 := err * 2;
    DrawLine := e2 > dX;
    if DrawLine then
    begin
      Inc(CurrX); 
      Inc(dX, RY2); 
      Inc(err, dX);
    end;

    if e2 <= dY then
    begin
      Inc(CurrY); 
      Inc(dY, RX2); 
      Inc(err, dY);
    end;
  end;

  // Last point and center fill
  // If DrawLine then <- Always is True
  Result := RectangleFilled(X2 + RXi, Y2, W2 - 2 * RXi, H2);
end;

function cCHXSDL3Renderer.RndRectE(const X, Y, W, H, RX, RY: CFloat; const BorderC, FillC: TSDL_Color): Boolean;
begin
  {$PUSH} {$BOOLEVAL ON}
  if BorderC.IsEqual(FillC) then
    Exit(SetColor(BorderC) and RndRectEFilled(X, Y, W, H, RX, RY));

  Result := True;

  if FillC.A > 0 then
    Result := SetColor(FillC) and RndRectEOnlyFill(X, Y, W, H, RX, RY) and Result;
  {$POP}

  Result := SetColor(BorderC) and Result; // Poscondición

  if BorderC.A > 0 then
    Result := RndRectEBorder(X, Y, W, H, RX, RY) and Result;
end;

function cCHXSDL3Renderer.FrameBorder(const X, Y, W, H, BSize: CFloat): Boolean;
begin
  if Abs(Round(BSize)) = 0 then
    Exit(RectangleBorder(X, Y, W, H));

  {$PUSH} {$BOOLEVAL ON}
  Result := RectangleBorder(X, Y, W, H) 
        and RectangleBorder(X + BSize, Y + BSize, W - 2 * BSize, H - 2 * BSize);
  {$POP}
end;

function cCHXSDL3Renderer.FrameFilled(const X, Y, W, H, BSize: CFloat): Boolean;
var
  Y1, H1: CFloat;
begin
  if Abs(Round(BSize)) = 0 then
    Exit(RectangleBorder(X, Y, W, H));

  Y1 := Y + BSize + 1;
  H1 := H - 2 * BSize - 2;

  {$PUSH} {$BOOLEVAL ON}
  Result := RectangleFilled(X, Y, W, BSize) 
        and RectangleFilled(X, Y1, BSize, H1)
        and RectangleFilled(X + W - BSize + 1, Y1, BSize, H1)
        and RectangleFilled(X, Y + H - BSize + 1, W, BSize);
  {$POP}
end;

function cCHXSDL3Renderer.Frame(const X, Y, W, H, BSize: CFloat; const BorderC, FillC: TSDL_Color): Boolean;
var
  XF, YF, WF, HF, BSizeF, Y1, H1: CFloat;
begin
  {$PUSH} {$BOOLEVAL ON}
  if Abs(Round(BSize)) = 0 then
    Exit(SetColor(BorderC) and RectangleBorder(X, Y, W, H));

  Result := True;
  XF := X + 1; 
  YF := Y + 1; 
  WF := W - 2; 
  HF := H - 2;
  Y1 := Y + BSize; 
  H1 := H - 2 * BSize; 
  BSizeF := BSize - 2;

  // Fill
  if FillC.A > 0 then
  begin
    Result := SetColor(FillC) 
          and RectangleFilled(XF, YF, WF, BSizeF)
          and RectangleFilled(XF, Y1, BSizeF, H1)
          and RectangleFilled(X + W - BSizeF + 1, Y1, BSizeF, H1)
          and RectangleFilled(XF, Y + H - BSizeF + 1, WF, BSizeF);
  end;

  // Border
  Result := SetColor(BorderC) and Result;
  if BorderC.A > 0 then
    Result := FrameBorder(X, Y, W, H, BSize) and Result;
  {$POP}
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
