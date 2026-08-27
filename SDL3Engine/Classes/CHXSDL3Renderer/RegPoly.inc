{ Regular Polygon methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy
}

// RegPolyCC[x]

function cCHXSDL3Renderer.RegPolyCCVertices(out PArr: TSDLFPointDynArray;
  const X, Y, R: CFloat; const NSides: Integer; Angle: CFloat): Boolean;
var
  i: Integer;
  dAng, aSin, aCos: CFloat;
begin
  if NSides < 1 then // `1` doesn't have sense but add a point.
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      NSides]);
    SetLength(PArr, 0); // Sorry, we don't keep previous data.
    Exit(False);
  end;

  SetLength(PArr, NSides);

  dAng := (2.0 * Pi) / NSides;
  for i := 0 to High(PArr) do
  begin
    // Actually must be SinCos(Angle, aSin, aCos) but this way the polygon
    //  has a vertex at the top for better look.
    SinCos(Angle, aCos, aSin);
    PArr[i].Init(aCos * R + X, aSin * R + Y);
    Angle += dAng;
  end;

  Result := True;
end;

function cCHXSDL3Renderer.RegPolyCC(const X, Y, R: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and Polygon(PArr, BorderC, FillC);
end;

function cCHXSDL3Renderer.RegPolyCCBorder(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonBorder(PArr);
end;

function cCHXSDL3Renderer.RegPolyCCFilled(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonFilled(PArr);
end;

function cCHXSDL3Renderer.RegPolyCCFillOnly(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonFillOnly(PArr);
end;

// RegPolySS[x]

function cCHXSDL3Renderer.RegPolySSVertices(out PArr: TSDLFPointDynArray;
  const X, Y, SideSize: CFloat; const NSides: Integer; const Angle: CFloat)
  : Boolean;
begin
  if NSides < 1 then // `1` doesn't have sense but add a point.
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      NSides]);
    SetLength(PArr, 0); // Sorry, we don't keep previous data.
    Exit(False);
  end;

  Result := RegPolyCCVertices(PArr, X, Y,
    SideSize * 0.5 * Cosecant(Pi / NSides), NSides, Angle);
end;

function cCHXSDL3Renderer.RegPolySS(const X, Y, SideSize: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and Polygon(PArr, BorderC, FillC);
end;

function cCHXSDL3Renderer.RegPolySSBorder(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and PolygonBorder(PArr);
end;

function cCHXSDL3Renderer.RegPolySSFilled(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and PolygonFilled(PArr);
end;

function cCHXSDL3Renderer.RegPolySSFillOnly(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and PolygonFillOnly(PArr);
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
