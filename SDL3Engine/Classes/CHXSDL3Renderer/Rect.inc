{ Axis Aligned Rectangle (AABB)

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Rect[angle]

function cCHXSDL3Renderer.Rect(const aRect: TSDL_FRect; const BorderC,
  FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and RectFilled(aRect);
    // Try to restore previous color anyway
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(FillC)
      and RectFillOnly(aRect);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and SetDrawColor(BorderC)
      and RectBorder(aRect);
  end;

  // Try to restore previous color anyway
  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.RectBorder(const aRect: TSDL_FRect): Boolean;
begin
  Result := SDL_RenderRect(SDLRenderer, @aRect);
end;

function cCHXSDL3Renderer.RectFilled(const aRect: TSDL_FRect): Boolean;
begin
  Result := SDL_RenderFillRect(SDLRenderer, @aRect);
end;

function cCHXSDL3Renderer.RectFillOnly(aRect: TSDL_FRect): Boolean;
begin
  if (aRect.W <= 2) or (aRect.H <= 2) then
    Exit(False); // ToDo: ¿False?

  aRect.X += 1; aRect.Y += 1; aRect.W -= 2;  aRect.H -= 2;
  Result := SDL_RenderFillRect(SDLRenderer, @aRect);
end;

// Rect[angle]s

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
