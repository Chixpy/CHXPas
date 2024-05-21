unit ucCTCWalker;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/Cl_Gjj80gPE
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl,
  uCHXPoint3DF;

type
  cCTCWalker = class; // Forward declaration

  cCTCWalkerList = specialize TFPGObjectList<cCTCWalker>;

  { cCTCWalker }

  cCTCWalker = class
  private
    WWidth : integer;
    WHeight : integer;

  public
    aPos : TPoint3DF;
    stuck : Boolean;
    r : Float;
    hu : integer;

    function checkStuck(others : cCTCWalkerList) : Boolean;

    procedure walk;

    constructor Create(WinWidth, WinHeight : integer; radius : Float);
    constructor CreateStuck(WinWidth, WinHeight : integer; PosX, PosY : integer;
      radius : Float);
    destructor Destroy; override;
  end;

function randomPoint(Width, Heigth : integer) : TPoint3DF;

function distSq(a, b : TPoint3DF) : float;

implementation

function randomPoint(Width, Heigth : integer) : TPoint3DF;
var
  i : integer;
begin
  i := Random(4);

  case i of
    0 : Result.Create(random(Width), 0);
    1 : Result.Create(random(Width), Heigth);
    2 : Result.Create(0, random(Heigth));
    else
      Result.Create(Width, random(Heigth));
  end;
end;

function distSq(a, b : TPoint3DF) : float;
var
  dx, dy : Float;
begin
  dx := b.x - a.x;
  dy := b.y - a.y;
  Result := dx * dx + dy * dy;
end;

{ cCTCWalker }

function cCTCWalker.checkStuck(others : cCTCWalkerList) : Boolean;
var
  i : integer;
  other : cCTCWalker;
  d : Float;
begin
  Result := False;

  i := 0;
  while i < others.Count do
  begin
    other := others[i];
    d := distSq(aPos, other.aPos);

    if d < (r * r + other.r * other.r + 2 * other.r * r) then
    begin
      //if random < 0.1 then
      //begin
      stuck := True;
      Result := True;
      Exit;
      //end;
    end;

    Inc(i);
  end;
end;

procedure cCTCWalker.walk;
var
  vel : TPoint3DF;
begin
  vel := TPoint3DF.CreateRnd(True);
  //// vel := TPoint3DF.create(random * 2 - 1, random * 1.5 -0.5);
  apos.add(vel);
  apos.x := EnsureRange(apos.x, 0, WWidth);
  apos.y := EnsureRange(apos.y, 0, WHeight);
end;

constructor cCTCWalker.Create(WinWidth, WinHeight : integer; radius : Float);
begin
  WWidth := WinWidth;
  WHeight := WinHeight;
  apos := randomPoint(WWidth, WHeight);
  stuck := False;
  r := radius;
  hu := 96;
end;

constructor cCTCWalker.CreateStuck(WinWidth, WinHeight : integer;
  PosX, PosY : integer; radius : Float);
begin
  apos.Init(PosX, PosY);
  stuck := True;
  r := radius;
  hu := 64;
end;

destructor cCTCWalker.Destroy;
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
