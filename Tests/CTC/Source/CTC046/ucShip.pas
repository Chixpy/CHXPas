unit ucShip;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/hacZU523FyM
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uCHXPoint3DF, Math,
  ucAsteroid;

type

  { cShip }

  cShip = class
  protected
    WinW : Integer;
    WinH : Integer;

  public
    pos : TPoint3DF;
    r : float;
    heading : float;
    rotation : float;
    vel : TPoint3DF;
    isBoosting : Boolean;

    constructor Create(const aWinW, aWinH : Integer);

    procedure boosting(b : Boolean);
    procedure update;
    procedure boost;

    function hits(Asteroid : cAsteroid) : Boolean;

    procedure edges;
    procedure setRotation(a : Double);
    procedure turn;
  end;

implementation

{ cShip }

constructor cShip.Create(const aWinW, aWinH : Integer);
begin
  WinW := aWinW;
  WinH := aWinH;

  pos.Create(WinW div 2, WinH div 2);
  r := 10;
  heading := 0;
  rotation := 0;
  vel.Create(0, 0);
  isBoosting := False;
end;

procedure cShip.boosting(b : Boolean);
begin
  isBoosting := b;
end;

procedure cShip.update;
begin
  if isBoosting then boost;
  pos.Add(vel);
end;

procedure cShip.boost;
var
  force : TPoint3DF;
begin
  force.Create(0.1, 0);
  force.SetAngleXY(heading);
  vel.Add(force);
end;

function cShip.hits(Asteroid : cAsteroid) : Boolean;
var
  d : Double;
begin
  d := pos.Distance(Asteroid.pos);
  Result := d < r + Asteroid.r;
end;

procedure cShip.edges;
begin
  if pos.X > winw + r then
    pos.X := -r
  else if pos.x < -r then
    pos.X := winW + r;

  if pos.Y > winh + r then
    pos.Y := -r
  else if pos.y < -r then
    pos.Y := winh + r;
end;

procedure cShip.setRotation(a : Double);
begin
  rotation := a;
end;

procedure cShip.turn;
begin
  heading += rotation;
end;

end.
