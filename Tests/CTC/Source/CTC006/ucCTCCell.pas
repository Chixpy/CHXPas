unit ucCTCCell;

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/jxGS3fKPKJA
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes,
  Math;

type

  { cCTCCell }

  cCTCCell = class
  private

  public
    Pos: TPoint;
    R: Double;
    C: CUint32; // color c;

    function Clicked(aX, aY: integer): boolean;
    function Mitosis: cCTCCell; // Creates a new Cell object
    procedure Move;
    // procedure Show;

    constructor Create(aPos: TPoint; aR: Double; aColor: CUint32);
    destructor Destroy; override;
  end;

implementation

{ cCTCCell }

function cCTCCell.Clicked(aX, aY : integer) : boolean;
var
  d : Float;
begin
    d := Hypot(Pos.X -aX, Pos.Y -aY);
    Result := d < Self.R;
end;

function cCTCCell.Mitosis : cCTCCell;
begin
  Result := cCTCCell.Create(Self.Pos, Self.R * 0.8, Self.C);
end;

procedure cCTCCell.Move;
begin
  // RandomRange never got upper value
  Pos.X += RandomRange(-2,3);
  Pos.Y += RandomRange(-2,3);
end;

constructor cCTCCell.Create(aPos : TPoint; aR : Double; aColor : CUint32);
begin
  Pos := aPos;
  R := aR;
  C := aColor;
end;

destructor cCTCCell.Destroy;
begin
  inherited Destroy;
end;

end. 
{
This source is free software; you can redistribute it and/or modify it under
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

