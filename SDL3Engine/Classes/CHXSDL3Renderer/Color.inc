{ Color methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// SetDrawColor

function cCHXSDL3Renderer.SetDrawColor(const aColor: TSDL_FColor): Boolean;
begin
  Result := SetDrawColor(aColor.R, aColor.G, aColor.B, aColor.A);
end;

function cCHXSDL3Renderer.SetDrawColor(const R, G, B, A: CFloat): Boolean;
var
  PrevR, PrevG, PrevB, PrevA: CFloat;
begin
  Result := SDL_GetRenderDrawColorFloat(SDLRenderer,
    @PrevR, @PrevG, @PrevB, @PrevA);

  if SameValue(PrevR, R) and SameValue(PrevG, G) and SameValue(PrevB, B)
    and SameValue(PrevA, A) then
    Exit;

  if (PrevA >= 1) and (A < 1) then
    Result := Result
      and SDL_SetRenderDrawBlendMode(SDLRenderer, PrevBlendMode)
  else if (PrevA < 1) and (A >= 1) then
    Result := Result
      and SDL_GetRenderDrawBlendMode(SDLRenderer, @PrevBlendMode)
      and SDL_SetRenderDrawBlendMode(SDLRenderer, SDL_BLENDMODE_NONE);

  // Try to set color anyways if something before fails
  Result := SDL_SetRenderDrawColorFloat(SDLRenderer, R, G, B, A)
    and Result;
end;

function cCHXSDL3Renderer.SetDrawColor(const Grey, A: CFloat): Boolean;
begin
  Result := SetDrawColor(Grey, Grey, Grey, A);
end;

// GetDrawColor

function cCHXSDL3Renderer.GetDrawColor: TSDL_FColor;
begin
  SDL_GetRenderDrawColorFloat(SDLRenderer, @Result.R, @Result.G,
    @Result.B, @Result.A);
end;

function cCHXSDL3Renderer.GetDrawColor(out aColor: TSDL_FColor): Boolean;
begin
  Result := SDL_GetRenderDrawColorFloat(SDLRenderer, @aColor.R,
    @aColor.G, @aColor.B, @aColor.A);
end;

function cCHXSDL3Renderer.GetDrawColor(out R, G, B, A: CFloat): Boolean;
begin
  Result := SDL_GetRenderDrawColorFloat(SDLRenderer, @R, @G, @B, @A);
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
