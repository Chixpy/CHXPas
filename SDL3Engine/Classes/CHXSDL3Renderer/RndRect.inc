{ RndRect[x] methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// RndRectC[X]

function cCHXSDL3Renderer.RndRectC(const aRect: TSDL_FRect; const R: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := RndRectC(aRect.X, aRect.Y, aRect.W, aRect.H, R, BorderC, FillC);
end;

function cCHXSDL3Renderer.RndRectC(const X, Y, W, H, R: CFloat;
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
      and RndRectCFilled(X, Y, W, H, R);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and RndRectCFillOnly(X, Y, W, H, R);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and RndRectCBorder(X, Y, W, H, R);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.RndRectCBorder(const aRect: TSDL_FRect;
  const R: CFloat): Boolean;
begin
  Result := RndRectCBorder(aRect.X, aRect.Y, aRect.W, aRect.H, R);
end;

function cCHXSDL3Renderer.RndRectCBorder(X, Y, W, H, R: CFloat): Boolean;
// Jesko's method for corners + adaptation to Logical Presentation
var
  t1, t2, CurrX, CurrY: CFloat; // Circle algorithm vars
  CX, CY: CFloat; // Center of the rectangle
  W2, H2: CFloat; // Half sides
  OffSetX, OffSetY: CFloat; // Offset of the center of the arcs
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(RectFilled(X, Y, W, H));

  // Radius check.
  CurrX := Abs(R);
  if CurrX < 1 then Exit(RectBorder(X, Y, W, H));

  // Checking if radius is too big and setting integer R.
  CurrX := Floor(Min(Min(CurrX, (H - 1) * 0.5), (W - 1) * 0.5));

  W2 := (W - 1) * 0.5; H2 := (H - 1) * 0.5;
  CX := X + W2; CY := Y + H2;
  OffSetX := W2 - CurrX; OffSetY := H2 - CurrX;

  // Rect sides
  Result := PointMirrorHFilled(OffSetX, Y, CX)
    and PointMirrorHFilled(OffSetX, Y + H - 1, CX)
    and PointMirrorVFilled(X, OffSetY, CY)
    and PointMirrorVFilled(X + W - 1, OffSetY, CY);

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

    Result := PointMirrorHV(OffSetX + CurrX, OffSetY + CurrY, CX, CY)
          and PointMirrorHV(OffSetX + CurrY, OffSetY + CurrX, CX, CY)
          and Result;

    CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
  end;

  // Drawing 45°, etc. only once
  if SameValue(CurrX, CurrY) then
    Result := PointMirrorHV(OffSetX + CurrX, OffSetY + CurrY, CX, CY)
      and Result;
end;

function cCHXSDL3Renderer.RndRectCFilled(const aRect: TSDL_FRect;
  const R: CFloat): Boolean;
begin
  Result := RndRectCFilled(aRect.X, aRect.Y, aRect.W, aRect.H, R);
end;

function cCHXSDL3Renderer.RndRectCFilled(X, Y, W, H, R: CFloat): Boolean;
// Jesko's method for corners + adaptation to Logical Presentation
var
  t1, t2, CurrX, CurrY: CFloat; // Circle algorithm vars
  CX, CY: CFloat; // Center of the rectangle
  W2, H2: CFloat; // Half sides
  OffSetX, OffSetY: CFloat; // Offset of the center of the arcs
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 2) or (H < 2) then
    Exit(RectFilled(X, Y, W, H));

  // Radius check.
  CurrX := Abs(R);
  if CurrX < 1 then Exit(RectFilled(X, Y, W, H));

  // Checking if radius is too big and setting integer R.
  CurrX := Floor(Min(Min(CurrX, (H - 1) * 0.5), (W - 1) * 0.5));

  W2 := (W - 1) * 0.5; H2 := (H - 1) * 0.5;
  CX := X + W2; CY := Y + H2;

  // I tried to combine them, but implies drawing at different moment.
  if R > 0 then // Filled Positive Radius
  begin
    // Rect sides
    Result := RectFilled(X, Y + CurrX, W, H - CurrX - CurrX);

    OffSetX := W2 - CurrX; OffSetY := H2 - CurrX;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;
    while True do
    begin
      if t2 >= 0 then
      begin
        Result := PointMirrorHVFilled(OffSetX + CurrY - 1, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
        t1 := t2; CurrX -= 1;
      end;

      if CurrX <= CurrY then Break;

      Result := PointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;

    // Drawing 45°, etc. only once
    if SameValue(CurrX, CurrY) then
      Result := PointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
        True, False, CX, CY)
        and Result;
  end
  else // Filled Negative Radius
  begin
    // Rect sides
    Result := RectFilled(X, Y + CurrX - 1, W, H - CurrX - CurrX + 2);

    OffSetX := -W2 - 1; OffSetY := -H2 - 1;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;
    while CurrX > CurrY do
    begin
      if t2 >= 0 then
      begin
        t1 := t2; CurrX -= 1;
        Result := PointMirrorHVFilled(OffSetX + CurrY, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
      end;

      if CurrX <= CurrY then Break;

      Result := PointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;
  end;
end;

function cCHXSDL3Renderer.RndRectCFillOnly(const aRect: TSDL_FRect;
  const R: CFloat): Boolean;
begin
  Result := RndRectCFillOnly(aRect.X, aRect.Y, aRect.W, aRect.H, R);
end;

function cCHXSDL3Renderer.RndRectCFillOnly(X, Y, W, H, R: CFloat): Boolean;
// Jesko's method for corners + adaptation to Logical Presentation
var
  t1, t2, CurrX, CurrY: CFloat; // Circle algorithm vars
  CX, CY: CFloat; // Center of the rectangle
  W2, H2: CFloat; // Half sides
  OffSetX, OffSetY: CFloat; // Offset of the center of the arcs
begin
  // Normalizing rectangle coordinates
  if W < 0 then
    begin X += W - 1; W := -W + 2; end;
  if H < 0 then
    begin Y += H - 1; H := -H + 2; end;

  // Rectangle size check
  if (W < 3) or (H < 3) then
    Exit(RectFillOnly(X, Y, W, H));

  // Radius check.
  CurrX := Abs(R);
  if CurrX < 1 then Exit(RectFillOnly(X, Y, W, H));

  // Checking if radius is too big and setting integer R.
  CurrX := Floor(Min(Min(CurrX, (H - 1) * 0.5), (W - 1) * 0.5));

  W2 := (W - 1) * 0.5; H2 := (H - 1) * 0.5;
  CX := X + W2; CY := Y + H2;

  // I tried to combine them, but implies drawing at different moment.
  if R > 0 then // Fill Only Positive Radius
  begin
    // Rect sides
    Result := RectFilled(X + 1, Y + CurrX, W - 2, H - CurrX - CurrX);

    OffSetX := W2 - CurrX; OffSetY := H2 - CurrX;

    // Initial Jesko setup.
    CurrY := 1; t1 := 1 + (CurrX / 16); t2 := t1 - CurrX;
    while CurrX > CurrY do
    begin
      if t2 >= 0 then
      begin
        t1 := t2; CurrX -= 1;
        Result := PointMirrorHVFilled(OffSetX + CurrY - 1, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
      end;

      if CurrX <= CurrY then Break;

      Result := PointMirrorHVFilled(OffSetX + CurrX - 1, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;
  end
  else // Fill Only Negative Radius
  begin
    // Rect sides
    Result := RectFilled(X + 1, Y + CurrX, W - 2, H - CurrX - CurrX);

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
        Result := PointMirrorHVFilled(OffSetX + CurrY, OffSetY + CurrX,
            True, False, CX, CY)
          and Result;
        t1 := t2; CurrX -= 1;
      end;

      //if CurrX <= CurrY then Break;

      Result := PointMirrorHVFilled(OffSetX + CurrX + 1, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;

      CurrY += 1; t1 += CurrY; t2 := t1 - CurrX;
    end;

    // Drawing 45°, etc. only once
    if SameValue(CurrX, CurrY) then
      Result := PointMirrorHVFilled(OffSetX + CurrX, OffSetY + CurrY,
          True, False, CX, CY)
        and Result;
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
