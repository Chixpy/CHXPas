unit ucCTCDrop;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/KkyIDI6rQJI
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uProcUtils;

type

  { cCTCDrop }

  cCTCDrop = class
  private

  public
    X : Double;
    Y : Double;
    Z : Double;
    Len : Double;
    YSpeed : Double;

    procedure Fall;

    constructor Create(aX, aY, aZ : Double);
    destructor Destroy; override;
  end;

implementation

{ cCTCDrop }

procedure cCTCDrop.Fall;
var
  grav : Double;
begin
  Self.y := Self.y + Self.yspeed;
  grav := map(Self.z, 0, 20, 0, 0.2);
  Self.yspeed := Self.yspeed + grav;
end;

constructor cCTCDrop.Create(aX, aY, aZ : Double);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;

  Self.len := map(Self.z, 0, 20, 10, 20);
  Self.yspeed := map(Self.z, 0, 20, 1, 20);
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
