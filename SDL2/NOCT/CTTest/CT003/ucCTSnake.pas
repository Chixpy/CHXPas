unit ucCTSnake;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/AaGK-fj-BAM
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  { cCTSnake }

  cCTSnake = class
  private

  public
    x : integer;
    y : integer;
    xspeed : integer;
    yspeed : integer;
    total : integer;
    tail : array of TPoint;
    size : integer; // scl global

    function Eat(const Food : TPoint) : Boolean;
    procedure Dir(const ax, ay : integer);
    procedure Death;
    procedure Update;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cCTSnake }

function cCTSnake.Eat(const Food : TPoint) : Boolean;
var
  p: TPoint;
begin
  p := Point(Self.x, self.y);
  if Food.Distance(p) < 1 then
  begin
    Inc(total);
    SetLength(tail, total);
    tail[total - 1].Create(-size,-size);
    Result := True;
  end
  else
    Result := False;
end;

procedure cCTSnake.Dir(const ax, ay : integer);
begin
  self.xspeed := ax;
  self.yspeed := ay;
end;

procedure cCTSnake.Death;
var
  i : integer;
  p: TPoint;
begin
  p := Point(Self.x, self.y);
  i := 0;
  while i < length(tail) do
  begin
    if self.tail[i].Distance(p) < 1 then
    begin
      self.total := 0;
      setlength(tail, 0);
    end;
    Inc(i);
  end;
end;

procedure cCTSnake.Update;
var
  i : integer;
begin
  i := 0;
  while i < length(tail) - 1 do
  begin
    self.tail[i] := self.tail[i + 1];
    Inc(i);
  end;
  if self.total >= 1 then
    self.tail[self.total - 1] := Point(self.x, self.y);

  self.x := self.x + self.xspeed * self.size;
  self.y := self.y + self.yspeed * self.size;
end;

constructor cCTSnake.Create;
begin
  self.x := 0;
  self.y := 0;
  self.xspeed := 1;
  self.yspeed := 0;
  self.total := 0;
  self.size := 20;
end;

destructor cCTSnake.Destroy;
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
