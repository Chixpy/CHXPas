{ Axis Aligned Rectangle (AABB)

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

{
  Rect[x]
}

function cCHXSDL3Renderer.Rect(const aRect: TSDL_FRect;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := RectFilled(aRect)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := RectFillOnly(aRect)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and RectBorder(aRect)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.Rect(const X, Y, W, H: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Rect(SDLFRect(X, Y, W, H), BorderC, FillC);
end;

function cCHXSDL3Renderer.RectBorder(aRect: TSDL_FRect): Boolean;
begin
  {$IFDEF InvertDraw}
  // Crazy hack:
  //   In my tests, drawing with negative Rects is faster that normalized ones.
  if aRect.W > 0 then
    begin aRect.X += aRect.W; aRect.W := -aRect.W end;
  if aRect.H > 0 then
    begin aRect.Y += aRect.H; aRect.H := -aRect.H end;
  {$ENDIF}

  Result := SDL_RenderRect(SDLRenderer, @aRect);
end;

function cCHXSDL3Renderer.RectBorder(const X, Y, W, H: CFloat): Boolean;
var
  aRect: TSDL_FRect;
begin
  aRect := SDLFRect(X, Y, W, H);
  Result := RectBorder(aRect);
end;

function cCHXSDL3Renderer.RectFilled(aRect: TSDL_FRect): Boolean;
begin
  {$IFDEF InvertDraw}
  // Crazy hack:
  //   In my tests, drawing with negative Rects is faster that normalized ones.
  if aRect.W > 0 then
    begin aRect.X += aRect.W; aRect.W := -aRect.W end;
  if aRect.H > 0 then
    begin aRect.Y += aRect.H; aRect.H := -aRect.H end;
  {$ENDIF}

  Result := SDL_RenderFillRect(SDLRenderer, @aRect);
end;

function cCHXSDL3Renderer.RectFilled(const X, Y, W, H: CFloat): Boolean;
var
  aRect: TSDL_FRect;
begin
  aRect := SDLFRect(X, Y, W, H);
  Result := RectFilled(aRect);
end;

function cCHXSDL3Renderer.RectFillOnly(aRect: TSDL_FRect): Boolean;
begin
  {$IFDEF InvertDraw}
    // Crazy hack:
    // In my tests, drawing with negative Rects is faster that normalized ones.
    if aRect.W > 0 then
      begin aRect.X += aRect.W; aRect.W := -aRect.W end;
    if aRect.H > 0 then
      begin aRect.Y += aRect.H; aRect.H := -aRect.H end;

    if (aRect.W >= -2) or (aRect.H >= -2) then
      Exit(True);
    aRect.X -= 1; aRect.Y -= 1; aRect.W += 2;  aRect.H += 2;
  {$ELSE}
    if (Abs(aRect.W) <= 2) or (Abs(aRect.H) <= 2) then
      Exit(True);

    aRect.X += 1; aRect.Y += 1; aRect.W -= 2;  aRect.H -= 2;
  {$ENDIF}

  Result := SDL_RenderFillRect(SDLRenderer, @aRect);
end;

function cCHXSDL3Renderer.RectFillOnly(const X, Y, W, H: CFloat): Boolean;
var
  aRect: TSDL_FRect;
begin
  aRect := SDLFRect(X, Y, W, H);
  Result := RectFillOnly(aRect);
end;

{
  SPRect[x]
}

function cCHXSDL3Renderer.SPRect(const aRect: TSDL_FRect;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Rect(aRect, BorderC, FillC); // Same as default
end;

function cCHXSDL3Renderer.SPRect(const X, Y, W, H: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Rect(X, Y, W, H, BorderC, FillC); // Same as default
end;

function cCHXSDL3Renderer.SPRectBorder(const aRect: TSDL_FRect): Boolean;
begin
  Result := RectBorder(aRect); // Same as default
end;

function cCHXSDL3Renderer.SPRectBorder(const X, Y, W, H: CFloat): Boolean;
begin
  Result := RectBorder(X, Y, W, H); // Same as default
end;

function cCHXSDL3Renderer.SPRectFilled(const aRect: TSDL_FRect): Boolean;
begin
  Result := RectFilled(aRect); // Same as default
end;

function cCHXSDL3Renderer.SPRectFilled(const X, Y, W, H: CFloat): Boolean;
begin
  Result := RectFilled(X, Y, W, H); // Same as default
end;

function cCHXSDL3Renderer.SPRectFillOnly(const aRect: TSDL_FRect): Boolean;
begin
  Result := RectFillOnly(aRect); // Same as default
end;

function cCHXSDL3Renderer.SPRectFillOnly(const X, Y, W, H: CFloat): Boolean;
begin
  Result := RectFillOnly(X, Y, W, H); // Same as default
end;

(*
  Rects

  ToDo:

  - ¿"Optimize" Rects with {.$IFDEF InvertDraw}?
    It doesn't seem to be worth copying and modifying an array.
    But more testing is needed.
  - SP and FP methods.
*)

function cCHXSDL3Renderer.RectsBorderUnsafe(const RArr: TSDLFRectDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  Result := SDL_RenderRects(SDLRenderer, @RArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.RectsBorder(const RArr: TSDLFRectDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(RArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit;
  // Draw althougth Result = False.
  Result := RectsBorderUnsafe(RArr, idxFirst, Count) and Result;
end;

function cCHXSDL3Renderer.RectsFilledUnsafe(const RArr: TSDLFRectDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  Result := SDL_RenderFillRects(SDLRenderer, @RArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.RectsFilled(const RArr: TSDLFRectDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := IsValidArrayRange(Length(RArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit;
  // Draw althougth Result = False.
  Result := RectsFilledUnsafe(RArr, idxFirst, Count) and Result;
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
