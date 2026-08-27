{ Auxiliar methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}
function cCHXSDL3Renderer.IsValidArrayRange(const ArrLength, idxFirst: Integer;
  var Count: Integer; const LineNumber, FuncName: String): Boolean;
var
  MaxCount: Integer;
begin
  if ArrLength <= 0 then
  begin
    if Count = 0 then
      Exit(True)
    else
    begin
      SDL_SetError('%s(%s) %s: Empty array',
        [PAnsiChar({$I %FILE%}), PAnsiChar(LineNumber), PAnsiChar(FuncName)]);
      Count := -1;
      Exit(False);
    end;
  end;

  if (idxFirst < 0) or (idxFirst >= ArrLength) or (Count < 0) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) not in [0..%d]) or (Count(%d) < 0)',
      [PAnsiChar({$I %FILE%}), PAnsiChar(LineNumber), PAnsiChar(FuncName),
      idxFirst, ArrLength - 1, Count]);
    Count := -1;
    Exit(False);
  end;

  MaxCount := ArrLength - idxFirst; // It's at least 1

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [PAnsiChar({$I %FILE%}), PAnsiChar(LineNumber), PAnsiChar(FuncName),
      idxFirst, Count, ArrLength]);
    Count := MaxCount;
    Exit(False);
  end;

  Result := True;
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

