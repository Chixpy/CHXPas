{ Renderer target clear methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// Clear

function cCHXSDL3Renderer.Clear: Boolean;
begin
  Result := SDL_RenderClear(SDLRenderer);
end;

function cCHXSDL3Renderer.Clear(const aColor: TSDL_FColor): Boolean;
begin
  Result := Clear(aColor.R, aColor.G, aColor.B, aColor.A);
end;

function cCHXSDL3Renderer.Clear(const R, G, B, A: CFloat): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor)
    and SetDrawColor(R, G, B, A)
    and SDL_RenderClear(SDLRenderer);
  // Try to restore color if something failed
  Result := SetDrawColor(TempColor) and Result;
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
