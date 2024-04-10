unit ucCTFlower;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/biN3v3ef-Y0
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cCTFlower }

  cCTFlower = class
  public
    x : integer;
    y : integer;
    r : integer;

    xdir: integer;

    procedure Grow;
    procedure ShiftDown;
    procedure Move;

    constructor Create(aX, aY : integer);
    destructor Destroy; override;
  end;

implementation

{ cCTFlower }

procedure cCTFlower.Grow;
begin
  r += 2;
end;

procedure cCTFlower.ShiftDown;
begin
  xdir := - xdir;
  y += r;
end;

procedure cCTFlower.Move;
begin
  x += xdir;
end;

constructor cCTFlower.Create(aX, aY : integer);
begin
  x := aX;
  y := aY;
  r := 30;

  xdir := 1;
end;

destructor cCTFlower.Destroy;
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
