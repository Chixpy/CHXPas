unit ucCTCDrop;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/biN3v3ef-Y0
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  ucCTCFlower;

type

  { cCTCDrop }

  cCTCDrop = class
    x : integer;
    y : integer;
    r : integer;
    toDelete : Boolean;

    procedure Evaporate;
    function Hits(flower : cCTCFlower) : Boolean;
    procedure Move;

    constructor Create(aX, aY : integer);
    destructor Destroy; override;
  end;

implementation

{ cCTCDrop }

procedure cCTCDrop.Evaporate;
begin
  toDelete := True;
end;

function cCTCDrop.Hits(Flower : cCTCFlower) : Boolean;
var
  d : Float;
begin
  // d := dist(this.x, this.y, flower.x, flower.y);
  d := Hypot(Self.x - flower.x, Self.y - flower.y);
  if d < (Self.r + flower.r) then
    Result := True
  else
    Result := False;
end;

procedure cCTCDrop.Move;
begin
  Self.y := Self.y - 5;
end;

constructor cCTCDrop.Create(aX, aY : integer);
begin
  Self.x := aX;
  Self.y := aY;
  Self.r := 8;
  Self.toDelete := False;
end;

destructor cCTCDrop.Destroy;
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
