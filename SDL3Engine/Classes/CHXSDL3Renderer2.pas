function cCHXSDL3Renderer.CircleFilled(const X, Y, R: CFloat): Boolean;
{ Jesko's method with CircleBorder modifications, adapted for interior filling }
var
  t1, t2, CurrX, CurrY: Integer;
  DrawLine: Boolean;
begin
  CurrX := Abs(Round(R));
  if CurrX = 0 then 
    Exit(Point(X, Y));

  // 1st Iteration
  {$PUSH} {$BOOLEVAL ON}
  Result := PointMirrorHFilled(CurrX, Y, X) and PointMirrorV(X, CurrX, Y);
  {$POP}

  CurrY := 1; 
  t1 := 1 + (CurrX div 16); 
  t2 := t1 - CurrX;
  DrawLine := t2 >= 0;

  if DrawLine then
  begin
    t1 := t2;
    Dec(CurrX);
  end;

  // Main loop
  while CurrX > CurrY do
  begin
    {$PUSH} {$BOOLEVAL ON}
    Result := PointMirrorHVFilled(CurrX, CurrY, X, Y, True, False) and 
              PointMirrorHVFilled(CurrY, CurrX, X, Y, DrawLine, False) and 
              Result;
    {$POP}

    Inc(CurrY);
    Inc(t1, CurrY);
    t2 := t1 - CurrX;
    DrawLine := t2 >= 0;

    if DrawLine then
    begin
      t1 := t2;
      Dec(CurrX);
    end;
  end;

  // Drawing 45° once
  if CurrX = CurrY then
    Result := PointMirrorHVFilled(CurrX, CurrY, X, Y, DrawLine, False) and Result;
end;

function cCHXSDL3Renderer.CircleOnlyFill(const X, Y, R: CFloat): Boolean;
{ Jesko's method, with CircleFilled modifications, tailored to exclude the outer border }
var
  t1, t2, CurrX, CurrY: Integer;
  DrawLine: Boolean;
begin
  CurrX := Abs(Round(R));
  case CurrX of
    0: Exit(True); // Nothing to fill
    1: Exit(Point(X, Y)); // Fill only center point
  end;

  // 1st Iteration
  Result := PointMirrorHFilled(CurrX - 1, Y, X);
  CurrY := 1; 
  t1 := 1 + (CurrX div 16); 
  t2 := t1 - CurrX;
  DrawLine := t2 >= 0;

  if DrawLine then
  begin
    t1 := t2;
    Dec(CurrX);
  end;

  // Main loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHFilled(CurrX - 1, CurrY + Y, X) and Result;
    
    if DrawLine then
      Result := PointMirrorHFilled(CurrY - 1, CurrX + Y, X) and Result;

    Inc(CurrY);
    Inc(t1, CurrY);
    t2 := t1 - CurrX;
    DrawLine := t2 >= 0;

    if DrawLine then
    begin
      t1 := t2;
      Dec(CurrX);
    end;
  end;

  // Drawing 45° once
  if DrawLine and (CurrY = CurrX) then
    Result := PointMirrorHVFilled(CurrX - 1, CurrY + Y, X) and Result;
end;

function cCHXSDL3Renderer.Circle(const X, Y, R: CFloat; const BorderC, FillC: TSDL_Color): Boolean;
begin
  {$PUSH} {$BOOLEVAL ON}
  if BorderC.IsEqual(FillC) then 
    Exit(SetColor(BorderC) and CircleFilled(X, Y, R));

  Result := True;

  if FillC.A > 0 then
    Result := SetColor(FillC) and CircleOnlyFill(X, Y, R) and Result;
  {$POP}

  Result := SetColor(BorderC) and Result; // Postcondition

  if BorderC.A > 0 then
    Result := CircleBorder(X, Y, R) and Result;
end;

function cCHXSDL3Renderer.EllipseBorder(const X, Y, RX, RY: CFloat): Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io) 
  of Bresenham's algorithm:
  - Special cases: RX <= 1 or RY <= 1
  - Avoid redrawing pixels at cardinal points
  - Precalculate constant terms }
var
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
begin
  RX2 := Abs(Round(RX));
  RY2 := Abs(Round(RY));

  // Special cases
  if (RX2 <= 1) or (RY2 <= 1) then
    Exit(RectangleBorder(X - RX, Y - RY, 2 * RX, 2 * RY)); // Draws both vertical and horizontal degenerate cases

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2; 
  CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; 
  dY := RX2 * RX2; 
  err := dX + dY;
  RX2 := dY * 2; 
  RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  Result := PointMirrorH(CurrX, Y, X);
  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); 
    Inc(dX, RY2); 
    Inc(err, dX);
  end;

  // Main Loop
  while CurrX <= 0 do
  begin
    Result := PointMirrorHV(CurrX, CurrY, X, Y) and Result;
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

  // Draw last points only once
  Result := PointMirrorV(X, CurrY, Y) and Result;
end;

function cCHXSDL3Renderer.EllipseFilled(const X, Y, RX, RY: CFloat): Boolean;
var
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  DrawLine: Boolean;
begin
  RX2 := Abs(Round(RX));
  RY2 := Abs(Round(RY));

  if (RX2 <= 0) or (RY2 <= 0) then
    Exit(RectangleFilled(X - RX, Y - RY, 2 * RX, 2 * RY));

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2;
  CurrY := 0;
  dX := (1 + CurrX * 2) * RY2;
  dY := RX2 * RX2;
  err := dX + dY;
  RX2 := dY * 2;
  RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  Result := PointMirrorHFilled(CurrX, Y, X);

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
  while CurrX <= 0 do
  begin
    if DrawLine then
      Result := PointMirrorHVFilled(CurrX, CurrY, X, Y, True, False) and Result;

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

  // Last points only once
  if DrawLine then
    Result := PointMirrorV(X, CurrY, Y) and Result;
end;

function cCHXSDL3Renderer.EllipseOnlyFill(const X, Y, RX, RY: CFloat): Boolean;
var
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  DrawLine: Boolean;
begin
  if (RX < 0) or (RY < 0) then Exit(False);

  // Special Cases
  RX2 := Abs(Round(RX)); 
  RY2 := Abs(Round(RY));
  if (RX2 <= 1) or (RY2 <= 1) then
    Exit(RectangleOnlyFill(X - RX, Y - RY, 2 * RX, 2 * RY));

  // Initialization
  RY2 := RY2 * RY2;
  CurrX := -RX2; 
  CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; 
  dY := RX2 * RX2; 
  err := dX + dY;
  RX2 := dY * 2; 
  RY2 := RY2 * 2;

  // ToDo: Por simplicidad rellenamos verticalmente, pero posiblemente sea mejor 
  // intercambiar X e Y al hacer los cálculos y dibujar líneas horizontales como en EllipseFilled

  // 1st Iteration: Don't draw anything (it's border)
  e2 := err * 2; 
  Result := True;
  DrawLine := e2 >= dX;
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
  while CurrX < 0 do
  begin
    if DrawLine then
      Result := PointMirrorHVFilled(CurrX, CurrY - 1, X, Y, False, True) and Result;

    e2 := err * 2;
    DrawLine := e2 >= dX;
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

  // Last points only once
  if DrawLine then
    Result := PointMirrorVFilled(X, CurrY - 1, Y) and Result;
end;

function cCHXSDL3Renderer.EllipseInRectOnlyFill(const X, Y, W, H: CFloat): Boolean;
{ ToDo: Se rellena verticalmente, si rellenar horizontalmente es más óptimo se pueden
  intercambiar X e Y usando el código de EllipseInRectOnlyFill para DrawLine y dibujando
  la primera línea en vez de la última }
var
  X0, X1, Y0, Y1, a, b, aa, bb, b1, dX, dY, err, e2: Integer;
  DrawLine: Boolean;
begin
  // Integer ellipse coordinates
  if W > 0 then
  begin
    X0 := Round(X);
    X1 := Round(X + W);
  end
  else
  begin
    X0 := Round(X + W);
    X1 := Round(X);
  end;

  if H > 0 then
  begin
    Y0 := Round(Y);
    Y1 := Round(Y + H);
  end
  else
  begin
    Y0 := Round(Y + H);
    Y1 := Round(Y);
  end;

  a := X1 - X0;
  b := Y1 - Y0;

  // Special cases
  if (a <= 1) or (b <= 1) then
    Exit(True); // Nothing to fill, but OK

  // Initialization
  aa := a * a;
  bb := b * b;
  b1 := b mod 2;
  dX := (1 - a) * bb * 4;
  dY := ((b1 + 1) * aa) * 4;
  err := dX + dY + b1 * aa;
  Y0 := Y0 + (b + 1) div 2;
  Y1 := Y0 - b1;
  a := aa * 8;
  b1 := bb * 8;

  // 1st Iteration: Nothing to fill
  Result := True;
  e2 := err * 2;

  if e2 < dY then
  begin
    Inc(Y0);
    Dec(Y1);
    Inc(dY, a);
    Inc(err, dY);
  end;

  DrawLine := (e2 > dX) or ((err * 2) > dY);
  if DrawLine then
  begin
    Inc(X0);
    Dec(X1);
    Inc(dX, b1);
    Inc(err, dX);
  end;

  // Main loop
  while X0 < X1 do
  begin
    if DrawLine then
    begin
      {$PUSH} {$BOOLEVAL ON}
      Result := Line(X0, Y0 + 1, X0, Y1 - 1) and Line(X1, Y0 + 1, X1, Y1 - 1) and Result;
      {$POP}
    end;

    e2 := err * 2;
    if e2 < dY then
    begin
      Inc(Y0);
      Dec(Y1);
      Inc(dY, a);
      Inc(err, dY);
    end;

    DrawLine := (e2 > dX) or ((err * 2) > dY);
    if DrawLine then
    begin
      Inc(X0);
      Dec(X1);
      Inc(dX, b1);
      Inc(err, dX);
    end;
  end;

  // Last points
  if DrawLine and (X0 = X1) then
    Result := Line(X0, Y0 + 1, X1, Y1 - 1) and Result;
end;

function cCHXSDL3Renderer.EllipseInRect(const X, Y, W, H: CFloat; const BorderC, FillC: TSDL_Color): Boolean;
begin
  {$PUSH} {$BOOLEVAL ON}
  if BorderC.IsEqual(FillC) then
    Exit(SetColor(BorderC) and EllipseInRectFilled(X, Y, W, H));

  Result := True;

  if FillC.A > 0 then
    Result := SetColor(FillC) and EllipseInRectOnlyFill(X, Y, W, H) and Result;
  {$POP}

  Result := SetColor(BorderC) and Result;

  if BorderC.A > 0 then
    Result := EllipseInRectBorder(X, Y, W, H) and Result;
end;

function cCHXSDL3Renderer.RndRectCBorder(const X, Y, W, H, R: CFloat): Boolean;
// Método de Jesko para las esquinas
var
  t1, t2, CurrX, CurrY, X2, Y2, W2, H2: Integer;
begin
  CurrX := Abs(Round(R));
  if CurrX = 0 then
    Exit(RectangleBorder(X, Y, W, H));

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

  // Checking if radius is too big
  W2 := W2 div 2; 
  H2 := H2 div 2; 
  CurrX := Min([CurrX, W2, H2]);

  // Final arc offsets
  W2 := W2 - CurrX; 
  H2 := H2 - CurrX;

  // Initial Jesko setup... Well, CurrY, t1 and t2 are set after 1st iteration
  // 1st corner iteration and rectangle sides
  {$PUSH} {$BOOLEVAL ON}
  Result := PointMirrorHVFilled(CurrX + W2, H2, X2, Y2, False, True)
        and PointMirrorHVFilled(W2, CurrX + H2, X2, Y2, True, False);
  {$POP}

  CurrY := 1; 
  t1 := 1 + (CurrX div 16); 
  t2 := t1 - CurrX;
  if t2 >= 0 then
  begin
    t1 := t2; 
    Dec(CurrX);
  end;

  // Main loop
  while CurrX > CurrY do
  begin
    {$PUSH} {$BOOLEVAL ON}
    Result := PointMirrorHV(CurrX + W2, CurrY + H2, X2, Y2)
          and PointMirrorHV(CurrY + W2, CurrX + H2, X2, Y2) and Result;
    {$POP}

    Inc(CurrY); 
    Inc(t1, CurrY); 
    t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; 
      Dec(CurrX);
    end;
  end;

  // Drawing 45°, etc. only once
  if CurrX = CurrY then
    Result := PointMirrorHV(CurrX + W2, CurrY + H2, X2, Y2) and Result;
end;

function cCHXSDL3Renderer.RndRectCFilled(const X, Y, W, H, R: CFloat): Boolean;
// Modificación del método de Jesko para las esquinas
var
  t1, t2, CurrX, CurrY, X2, Y2, W2, H2: Integer;
  DrawLine: Boolean;
begin
  CurrX := Abs(Round(R));
  if CurrX = 0 then
    Exit(RectangleFilled(X, Y, W, H));

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

  // Checking if radius is too big
  W2 := W2 div 2; 
  H2 := H2 div 2;
  CurrX := Min([CurrX, H2, W2]);

  // Arc offset
  W2 := W2 - CurrX; 
  H2 := H2 - CurrX;

  // 1st iteration of corners and inside box. And Jesko setup (optimized a little)
  Result := RectangleFilled(X2, Y2 + CurrX, W2, H2 - 2 * CurrX);

  CurrY := 1; 
  t1 := 1 + (CurrX div 16); 
  t2 := t1 - CurrX;
  DrawLine := t2 >= 0;
  if DrawLine then
  begin
    t1 := t2; 
    Dec(CurrX);
  end;

  // Circle loop
  while CurrX > CurrY do
  begin
    {$PUSH} {$BOOLEVAL ON}
    Result := PointMirrorHVFilled(CurrX + W2, CurrY + H2, X2, Y2, True, False)
          and PointMirrorHVFilled(CurrY + W2, CurrX + H2, X2, Y2, DrawLine, False) and Result;
    {$POP}

    Inc(CurrY); 
    Inc(t1, CurrY); 
    t2 := t1 - CurrX;
    DrawLine := t2 >= 0;
    if DrawLine then
    begin
      t1 := t2; 
      Dec(CurrX);
    end;
  end;

  // Draw only once 45°, etc.
  if CurrX = CurrY then
    Result := PointMirrorHVFilled(CurrY + W2, CurrY + H2, X2, Y2, DrawLine, False);
end;

function cCHXSDL3Renderer.RndRectCOnlyFill(const X, Y, W, H, R: CFloat): Boolean;
// Modificación del método de Jesko para las esquinas
var
  t1, t2, CurrX, CurrY, X2, Y2, W2, H2: Integer;
  DrawLine: Boolean;
begin
  CurrX := Abs(Round(R));
  if CurrX = 0 then
    Exit(RectangleOnlyFill(X, Y, W, H));

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

  if (W2 <= 1) or (H2 <= 1) then
    Exit(True); // Nothing to fill

  // Checking if radius is too big and set arcs offsets
  W2 := W2 div 2; 
  H2 := H2 div 2;
  CurrX := Min([CurrX, W2, H2]);
  Dec(W2, CurrX); 
  Dec(H2, CurrX);

  // 1st iteration of corners and inside box. And Jesko setup
  Result := RectangleFilled(X2 + 1, Y2, W - 2, H - 2 * CurrX);
  // RectangleOnlyFill no dibujaría la parte superior del rectángulo

  CurrY := 1; 
  t1 := 1 + (CurrX div 16); 
  t2 := t1 - CurrX;
  DrawLine := t2 >= 0;
  if DrawLine then
  begin
    t1 := t2; 
    Dec(CurrX);
  end;

  // Corners main loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHVFilled(CurrX + W2 - 1, CurrY + H2, X2, Y2, True, False) and Result;
    
    if DrawLine then
      Result := PointMirrorHVFilled(CurrY + W2 - 1, CurrX + H2, X2, Y2, True, False) and Result;

    Inc(CurrY); 
    Inc(t1, CurrY); 
    t2 := t1 - CurrX;
    DrawLine := t2 >= 0;
    if DrawLine then
    begin
      t1 := t2; 
      Dec(CurrX);
    end;
  end;

  // 45° in corners only once
  if CurrX = CurrY then
    Result := PointMirrorHVFilled(CurrX + W2 - 1, CurrY + H2, X2, Y2, True, False) and Result;
end;

function cCHXSDL3Renderer.RndRectC(const X, Y, W, H, R: CFloat; const BorderC, FillC: TSDL_Color): Boolean;
begin
  {$PUSH} {$BOOLEVAL ON}
  if BorderC.IsEqual(FillC) then
    Exit(SetColor(BorderC) and RndRectCFilled(X, Y, W, H, R));

  Result := True;

  if FillC.A > 0 then
    Result := SetColor(FillC) and RndRectCOnlyFill(X, Y, W, H, R) and Result;
  {$POP}

  Result := SetColor(BorderC) and Result; // Poscondición

  if BorderC.A > 0 then
    Result := RndRectCBorder(X, Y, W, H, R) and Result;
end;

function cCHXSDL3Renderer.RndRectEBorder(const X, Y, W, H, RX, RY: CFloat): Boolean;
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

function cCHXSDL3Renderer.RndRectEFilled(const X, Y, W, H, RX, RY: CFloat): Boolean;
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

function cCHXSDL3Renderer.RndRectEOnlyFill(const X, Y, W, H, RX, RY: CFloat): Boolean;
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
