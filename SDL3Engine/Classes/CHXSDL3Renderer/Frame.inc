{ Frame[x] methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy

  ToDo:

  - FP methods.
}

{
  Frame[X]
}

function cCHXSDL3Renderer.Frame(const aRect: TSDL_FRect; const BWidth: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := FrameFilled(aRect, BWidth)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := FrameFillOnly(aRect, BWidth)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and FrameBorder(aRect, BWidth)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.Frame(const X, Y, W, H, BWidth: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Frame(SDLFRect(X, Y, W, H), BWidth, BorderC, FillC);
end;

function cCHXSDL3Renderer.FrameBorder(const aRect: TSDL_FRect;
  const BWidth: CFloat): Boolean;
var
  Rect2: TSDL_FRect;
begin
  Rect2.X := aRect.X + BWidth;
  Rect2.Y := aRect.Y + BWidth;
  Rect2.W := aRect.W - BWidth - BWidth;
  Rect2.H := aRect.H - BWidth - BWidth;

  Result := RectBorder(aRect) and RectBorder(Rect2);
end;

function cCHXSDL3Renderer.FrameBorder(const X, Y, W, H, BWidth: CFloat)
  : Boolean;
begin
  Result := FrameBorder(SDLFRect(X, Y, W, H), BWidth);
end;

function cCHXSDL3Renderer.FrameFilled(const aRect: TSDL_FRect;
  BWidth: CFloat): Boolean;
var
  Rect2: TSDL_FRect;
  X, Y: CFloat;
begin
  BWidth := BWidth + 1; // Rect behavior.
  X := aRect.W - BWidth; Y := aRect.H - BWidth;
  // Top
  Rect2.X := aRect.X; Rect2.Y := aRect.Y;
  Rect2.W := X; Rect2.H := BWidth;
  Result := RectFilled (Rect2);
  // Bottom
  Rect2.X := aRect.X + BWidth; Rect2.Y := aRect.Y + Y;
  Rect2.W := X; Rect2.H := BWidth;
  Result := RectFilled (Rect2) and Result;
  // Left
  Rect2.X := aRect.X; Rect2.Y := aRect.Y + BWidth;
  Rect2.W := BWidth; Rect2.H := Y;
  Result := RectFilled (Rect2) and Result;
  // Right
  Rect2.X := aRect.X + X; Rect2.Y := aRect.Y;
  Rect2.W := BWidth; Rect2.H := Y;
  Result := RectFilled (Rect2) and Result;
end;

function cCHXSDL3Renderer.FrameFilled(const X, Y, W, H, BWidth: CFloat)
  : Boolean;
begin
  Result := FrameFilled(SDLFRect(X, Y, W, H), BWidth);
end;

function cCHXSDL3Renderer.FrameFillOnly(aRect: TSDL_FRect;
  const BWidth: CFloat): Boolean;
begin
  aRect.X += 1; aRect.Y += 1; aRect.W -= 2; aRect.H -= 2;
  Result := FrameFilled(aRect, BWidth - 2);
end;

function cCHXSDL3Renderer.FrameFillOnly(const X, Y, W, H, BWidth: CFloat)
  : Boolean;
begin
  Result := FrameFillOnly(SDLFRect(X, Y, W, H), BWidth);
end;

{
  SPFrame[X]
}

function cCHXSDL3Renderer.SPFrame(const aRect: TSDL_FRect; const BWidth: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Frame(aRect, BWidth, BorderC, FillC); // Same as default
end;

function cCHXSDL3Renderer.SPFrame(const X, Y, W, H, BWidth: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Frame(X, Y, W, H, BWidth, BorderC, FillC); // Same as default
end;

function cCHXSDL3Renderer.SPFrameBorder(const aRect: TSDL_FRect;
  const BWidth: CFloat): Boolean;
begin
  Result := FrameBorder(aRect, BWidth); // Same as default
end;

function cCHXSDL3Renderer.SPFrameBorder(const X, Y, W, H, BWidth: CFloat)
  : Boolean;
begin
  Result := FrameBorder(X, Y, W, H, BWidth); // Same as default
end;

function cCHXSDL3Renderer.SPFrameFilled(const aRect: TSDL_FRect;
  BWidth: CFloat): Boolean;
begin
  Result := FrameFilled(aRect, BWidth); // Same as default
end;

function cCHXSDL3Renderer.SPFrameFilled(const X, Y, W, H, BWidth: CFloat)
  : Boolean;
begin
  Result := FrameFilled(X, Y, W, H, BWidth); // Same as default
end;

function cCHXSDL3Renderer.SPFrameFillOnly(aRect: TSDL_FRect;
  const BWidth: CFloat): Boolean;
begin
  Result := FrameFillOnly(aRect, BWidth); // Same as default
end;

function cCHXSDL3Renderer.SPFrameFillOnly(const X, Y, W, H, BWidth: CFloat)
  : Boolean;
begin
  Result := FrameFillOnly(X, Y, W, H, BWidth);  // Same as default
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
