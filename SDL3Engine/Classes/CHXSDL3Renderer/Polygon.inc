{ Polygon methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy

  ToDo:

  - When optimized Fill and OnlyFill for triangles (¿and Quad?) is done.
    Use it.
  - Default Fill and FillOnly, it uses SP/FP filling.
  - SP/FP Border, they use default smooth one.
}

{
  Polygon
}

function cCHXSDL3Renderer.PolygonUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := PolygonFilledUnsafe(PArr, idxFirst, Count)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := PolygonFillOnlyUnsafe(PArr, idxFirst, Count)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and PolygonBorderUnsafe(PArr, idxFirst, Count)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.Polygon(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer; const BorderC, FillC: TSDL_FColor)
  : Boolean; overload;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  Result := Result and PolygonUnsafe(PArr, idxFirst, Count, BorderC, FillC);
end;

function cCHXSDL3Renderer.Polygon(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Polygon(PArr, 0, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.PolygonBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
var
  PArr2: TSDLFPointDynArray;
begin
  // Closing the polygon
  PArr2 := Copy(PArr, idxFirst, Count);
  SetLength(PArr2, Length(PArr2) + 1);
  PArr2[High(PArr2)] := PArr2[0];
  Result := LinesUnsafe(PArr2, 0, Length(PArr2));
end;

function cCHXSDL3Renderer.PolygonBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit(Result); // It can return True

  case Count of
  // 0: Exit(True); It can't happen
  1: Result := Point(PArr[idxFirst]);
  2: Result := Lines(PArr, idxFirst, Count);
  otherwise
    Result := PolygonBorderUnsafe(PArr, idxFirst, Count);
  end;
end;

function cCHXSDL3Renderer.PolygonFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  // ToDo: Make a proper PolygonFilledUnsafe
  Result := SPPolygonFilledUnsafe(PArr, idxFirst, Count);
end;

function cCHXSDL3Renderer.PolygonFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit(Result); // It can be True

  case Count of
  // 0: Exit(Result); // It can't happen

  1: Result := SPPoint(PArr[idxFirst]);

  2: Result := SPLinesUnsafe(PArr, idxFirst, 2);

  // ToDo: Use optimized SPTriangleFilledUnsafe when optimized.
  // 3: Result := SPTriangleFilledUnsafe(PArr, idxFirst);

  // ToDo: ¿Quads can have an optimized fill too?
  // 4:  Result := SPQuadFilledUnsafe(PArr, idxFirst);
  otherwise
    // ToDo: Make a proper default filled
    Result := PolygonFilledUnsafe(PArr, idxFirst, Count);
  end;
end;

function cCHXSDL3Renderer.PolygonFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  // ToDo: Make a proper Polygon Only Fill
  Result := SPPolygonFillOnlyUnsafe(PArr, idxFirst, Count);
end;

function cCHXSDL3Renderer.PolygonFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  // `Count <= 2` includes:
  //  - Result = False
  //  - 1, 2: Point or line, nothing to fill inside.
  if Count <= 2 then Exit(Result);

  // ToDo: If TriangleFillOnly has an optimized method...
  // if Count = 3 then
  //   Exit(TriangleFillOnlyUnsafe(PArr, idxFirst));

  // ToDo: ¿Optimized Quads too?

  // ToDo: Make a proper Polygon Only Fill
  Result := SPPolygonFillOnlyUnsafe(PArr, idxFirst, Count);
end;

{
  SPPolygon
}

function cCHXSDL3Renderer.SPPolygonUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := SPPolygonFilledUnsafe(PArr, idxFirst, Count)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := SPPolygonFillOnlyUnsafe(PArr, idxFirst, Count)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and SPPolygonBorderUnsafe(PArr, idxFirst, Count)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.SPPolygon(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer; const BorderC, FillC: TSDL_FColor)
  : Boolean; overload;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  Result := Result and SPPolygonUnsafe(PArr, idxFirst, Count, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPPolygon(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := SPPolygon(PArr, 0, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.SPPolygonBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
var
  PArr2: TSDLFPointDynArray;
begin
  // Closing the polygon
  PArr2 := Copy(PArr, idxFirst, Count);
  SetLength(PArr2, Length(PArr2) + 1);
  PArr2[High(PArr2)] := PArr2[0];
  Result := SPLinesUnsafe(PArr2, 0, Length(PArr2));
end;

function cCHXSDL3Renderer.SPPolygonBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit(Result); // It can be True

  case Count of
  // 0: Exit(True); It can't happen here
  1: Result := SPPoint(PArr[idxFirst]);
  2: Result := SPLinesUnsafe(PArr, idxFirst, Count);
  otherwise
    Result := SPPolygonBorderUnsafe(PArr, idxFirst, Count);
  end;
end;

function cCHXSDL3Renderer.SPPolygonFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
{ Algorithm rationale & theoretical breakdown:

  In the absence of a precise, pre-packaged hardware algorithm, polygon filling
  is implemented using the Even-Odd Rule scanline algorithm.

  Each horizontal line (row) is scanned across the polygon's bounding box,
  finding edge intersections, sorting them by X coordinate, and filling
  scanline spans between paired intersections.

  Originally an _integer_ algorithm adapted to Logical Presentation subpixel.
  Althought, not sure how to manage vertical subpixels. Anyways, horizontal
  line corners will overlap polygon borders with Logical Presentation.
  Surprisingly, using floats seems to be less problematic than actual
  integer implementation.

  Line Intersection Mathematics, given:

  - P1, P2: Endpoints of a polygon edge.
  - PI: Intersection point along the scanline.
  - CurrY: Current active scanline Y-coordinate.

  Interpolation formulas:

  - U = (CurrY - P1.Y) / (P2.Y - P1.Y)
  - PI.X = P1.X + U * (P2.X - P1.X)
  - PI.Y = CurrY

  Edge Cases & Constraints:

  - If (P2.Y - P1.Y) = 0: Division by zero occurs because the edge
    is completely horizontal.
  - If U NOT in [0..1]: The intersection point lies outside the physical
    segment/edge bounds.

  Both edge cases and are resolved via logical checks prior to division:

  1. Zero check: If the line is horizontal (P1.Y = P2.Y), it is ignored and
    this helps to fix concave corners (`^` and `v`) too
  2. Removing segments: If both endpoint Y are less than CurrY then segment
    is not useful any more and it can be deleted from list of segments.
  3. Bounds check: If both endpoints Y are simultaneously below
    CurrY, no intersection is possible. Calculating U is skipped entirely.

  ToDo:

  - Optimize not drawing if it's not visible...
}
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: CFloat;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_FPoint;
  SegList: Array of TCHXSDLFSegment;
  XList: Array of CFloat; // Intersection values
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst];
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg];
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    // Don't add horizontal edges.
    // ToDo: ¿Use sigma ~ 0.5?
    if SameValue(P1.Y, P2.Y) then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;

  SegList[0].P1 := P2; // Close the polygon (2)

  // ToDo: Not really sure how to handle vertical subpixel

  CurrY := MinY;
  while CurrY < MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we will delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      // ToDo: Remove nesting
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate precise X intersection point
        xIntersect := P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y);

        // Simple sorted insertion (maybe it is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    // ToDo: ¿It can happen?, we can remove it
    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex];
      X2 := XList[aIndex + 1];

      if X1 <= X2 then
        Result := Line(X1, CurrY, X2, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    CurrY += 1;
  end;
end;

function cCHXSDL3Renderer.SPPolygonFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit(Result); // Includes Result = False

  case Count of
  // 0: Exit(Result); // It can't happen

  1: Result := SPPoint(PArr[idxFirst]);

  2: Result := SPLinesUnsafe(PArr, idxFirst, 2);

  // ToDo: Use optimized SPTriangleFilledUnsafe when optimized.
  // 3: Result := SPTriangleFilledUnsafe(PArr, idxFirst);

  // ToDo: ¿Quads can have an optimized fill too?
  // 4:  Result := SPQuadFilledUnsafe(PArr, idxFirst);
  otherwise
    Result := SPPolygonFilledUnsafe(PArr, idxFirst, Count);
  end;
end;

function cCHXSDL3Renderer.SPPolygonFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
{ See `cCHXSDL3Renderer.SPPolygonFilledUnsafe`

  Modified to draw only interior without borders and Floats.
}
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: CFloat;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_FPoint;
  SegList: Array of TCHXSDLFSegment;
  XList: Array of CFloat; // Intersection values
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst];
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg];
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    // Don't add horizontal edges.
    // ToDo: use a sigma ~ 0.5?
    if SameValue(P1.Y, P2.Y) then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;

  SegList[0].P1 := P2; // Close the polygon (2)

  // Nothing to fill
  if ((MaxX - MinX) < 2) or ((MaxY - MinY) < 2) then
    Exit(True);

  // ToDo: Not really sure how to handle vertical subpixel

  CurrY := MinY + 1; // Skip border
  while CurrY < MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we wil delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate precise X intersection point
        xIntersect := P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y);

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    // ToDo: ¿It can happen?
    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex] + 1;
      X2 := XList[aIndex + 1] - 1;

      if X1 <= X2 then
        Result := Self.Line(X1, CurrY, X2, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    CurrY += 1;
  end;
end;

function cCHXSDL3Renderer.SPPolygonFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  // `Count <= 2` includes:
  //  - Result = False
  //  - 1, 2: Point or line, nothing to fill inside.
  if Count <= 2 then Exit(Result);

  // ToDo: If TriangleFillOnly has an optimized method...
  // if Count = 3 then
  //   Exit(TriangleFillOnlyUnsafe(PArr, idxFirst));

  // ToDo: ¿Optimized Quads too?

  Result := SPPolygonFillOnlyUnsafe(PArr, idxFirst, Count);
end;

{
  FPPolygon
}

(*
function cCHXSDL3Renderer.PolygonFilledUnsafeFP(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
{ See `cCHXSDL3Renderer.SPPolygonFilledUnsafe`

  Using rounded coordinates.
}
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: Integer;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_Point;
  SegList: Array of TCHXSDLSegment;
  XList: Array of Integer;
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst].Round;
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg].Round;
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    if P1.Y = P2.Y then
    begin
      // Dont'n add and removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;
  if SegList[0].P2.Y = P2.Y then
    Delete(SegList, 0, 1)
  else
    SegList[0].P1 := P2; // Close the polygon (2)

  CurrY := MinY;
  while CurrY <= MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we will delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      // ToDo: Remove nesting
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate X intersection point
        xIntersect := Round(
          P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y)
          );

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    X2 := XList[0];
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex];
      // Fix: "^" and "v" vertices double draw.
      if (aIndex <> 0) and (X1 = X2) then Inc(X1);
      X2 := XList[aIndex + 1];

      if X1 < X2 then
        Result := Line(X1, CurrY, X2, CurrY) and Result
      else if X1 = X2 then
        Result := Point(X1, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    Inc(CurrY);
  end;
end;

function cCHXSDL3Renderer.PolygonFillOnlyUnsafeFP(
  const PArr: TSDLFPointDynArray; const idxFirst, Count: Integer): Boolean;
{ See `cCHXSDL3Renderer.SPPolygonFilledUnsafe`

  Using rounded coordinates.

  Modified to draw only interior without borders and Floats.
}
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2: Integer;
  CurrSeg, aIndex, xIntersect: Integer;
  P1, P2: TSDL_Point;
  SegList: Array of TCHXSDLSegment;
  XList: Array of Integer;
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst].Round;
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg].Round;
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    if P1.Y = P2.Y then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;
  if SegList[0].P2.Y = P2.Y then
    Delete(SegList, 0, 1)
  else
    SegList[0].P1 := P2; // Close the polygon (2)

  // Nothing to fill
  if ((MaxX - MinX) <= 1) or ((MaxY - MinY) <= 1) then
    Exit(True);

  CurrY := MinY + 1;
  while CurrY < MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we wil delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate X intersection point
        xIntersect := Round(
          P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y)
          );

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex] + 1;
      X2 := XList[aIndex + 1] - 1;

      if X1 < X2 then
        Result := Self.Line(X1, CurrY, X2, CurrY) and Result
      else if X1 = X2 then
        Result := Self.Point(X1, CurrY) and Result;
      // else draw nothing

      Inc(aIndex, 2);
    end;

    Inc(CurrY);
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
