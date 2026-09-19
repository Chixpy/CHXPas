{ Regular Polygon methods.

  This file is part of ucCHXSDL3Renderer.

  (C) 2026 Chixpy https://github.com/Chixpy

  ToDo:

  - Make SP and FP methods.
  - Maybe use general default PolygonFill when created (as it will not add
    a center point)...
}

{
  RegPolyCC[x]
}

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
    SinCos(Angle, aSin, aCos);
    PArr[i].Init(aSin * R + X, -aCos * R + Y);
    Angle += dAng;
  end;

  Result := True;
end;

function cCHXSDL3Renderer.RegPolyCC(const X, Y, R: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
begin
  // Quick test
  if NSides < 1 then
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}, NSides]);
    Exit(False);
  end;

  Result := PushDrawColor(FillC);

  if BorderC = FillC then
  begin
    Result := RegPolyCCFilled(X, Y, R, NSides, Angle)
      and Result;
    Exit(PopDrawColor and Result);
  end;

  if FillC.A > 0 then
    Result := RegPolyCCFillOnly(X, Y, R, NSides, Angle)
      and Result;

  if BorderC.A > 0 then
    Result := SetDrawColor(BorderC) // Don't Push
      and RegPolyCCBorder(X, Y, R, NSides, Angle)
      and Result;

  Result := PopDrawColor and Result;
end;

function cCHXSDL3Renderer.RegPolyCCBorder(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonBorder(PArr);
end;

function cCHXSDL3Renderer.RegPolyCCFilledRaw(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
  Indices: Array of Byte;
  FColor: TSDL_FColor;
  i, j: Integer;
begin
  if not RegPolyCCVertices(PArr, X, Y, R, NSides, Angle) then Exit(False);

  SetLength(Indices, NSides * 3);
  SetLength(PArr, Length(PArr) + 1);
  PArr[High(PArr)] := SDLFPoint(X, Y);

  for i := 0 to (NSides - 2) do
  begin
    j := i * 3;
    Indices[j] := High(PArr);
    Indices[j + 1] := i;
    Indices[j + 2] := i + 1;
  end;
  j := (NSides - 1) * 3;
  Indices[j] := High(PArr);
  Indices[j + 1] := High(PArr) - 1;
  Indices[j + 2] := Low(PArr);

  Result := GetDrawColor(FColor)
    and SDL_RenderGeometryRaw(SDLRenderer, nil,
      @PArr[0], SizeOf(TSDL_FPoint), // Puntos
      @FColor, 0, // Colores
      nil, 0, // uv
      Length(PArr), @Indices[0], NSides * 3, 1); // Indices
end;

function cCHXSDL3Renderer.RegPolyCCFilled(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
begin
  Result := RegPolyCCFilledRaw(X + 0.5, Y + 0.5, R + 0.5, NSides, Angle);
end;

function cCHXSDL3Renderer.RegPolyCCFillOnly(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
begin
  case NSides of
    1, 2: Exit(True); // Nothing to fill but OK
  otherwise
    Result := RegPolyCCFilledRaw(X + 0.5, Y + 0.5, R - 0.5, NSides, Angle);
  end;
end;

{
  RegPolySS[x]
}

function cCHXSDL3Renderer.RegPolySSVertices(out PArr: TSDLFPointDynArray;
  const X, Y, SideSize: CFloat; const NSides: Integer; const Angle: CFloat)
  : Boolean;
begin
  if NSides < 1 then // `1` doesn't have sense but add a point.
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}, NSides]);
    SetLength(PArr, 0); // Sorry, we don't keep previous data.
    Exit(False);
  end;

  Result := RegPolyCCVertices(PArr, X, Y, RPCirCircleSL(NSides, SideSize),
    NSides, Angle);
end;

function cCHXSDL3Renderer.RegPolySS(const X, Y, SideSize: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
begin
  if NSides < 1 then
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}, NSides]);
    Exit(False);
  end;

  Result := RegPolyCC(X, Y, RPCirCircleSL(NSides, SideSize), NSides,
    BorderC, FillC, Angle);
end;

function cCHXSDL3Renderer.RegPolySSBorder(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
begin
  if NSides < 1 then
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}, NSides]);
    Exit(False);
  end;

  Result := RegPolyCCBorder(X, Y, RPCirCircleSL(NSides, SideSize),
    NSides, Angle);
end;

function cCHXSDL3Renderer.RegPolySSFilled(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
begin
  if NSides < 1 then
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}, NSides]);
    Exit(False);
  end;

  Result := RegPolyCCFilled(X, Y, RPCirCircleSL(NSides, SideSize),
    NSides, Angle);
end;

function cCHXSDL3Renderer.RegPolySSFillOnly(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
begin
  if NSides < 1 then
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}, NSides]);
    Exit(False);
  end;

  Result := RegPolyCCFillOnly(X, Y, RPCirCircleSL(NSides, SideSize),
    NSides, Angle);
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
