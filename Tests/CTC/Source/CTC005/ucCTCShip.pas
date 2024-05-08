unit ucCTCShip;
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

  { cCTCShip }

  cCTCShip = class
  public
    x : integer;
    y : integer; // CHX: Added to set y position from main program
    xdir : integer;

    procedure SetDir(aDir : integer); // CHX: Dir parameter not used;
    procedure Move;

    constructor Create(aX, aY : integer);
    destructor Destroy; override;
  end;

implementation

{ cCTCShip }

procedure cCTCShip.SetDir(aDir : integer);
begin
  xdir := aDir;
end;

procedure cCTCShip.Move; //(aDir : integer);
begin
  Self.x += Self.xdir * 5;
end;

constructor cCTCShip.Create(aX, aY : integer);
begin
  x := aX;
  y := aY;
end;

destructor cCTCShip.Destroy;
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
