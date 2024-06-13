unit ucLaser;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/hacZU523FyM
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uCHXPoint3DF, FGL,
  ucAsteroid;

type

  { cLaser }

  cLaser = class
  protected
    WinW : Integer;
    WinH : Integer;

  public
    pos : TPoint3DF;
    vel : TPoint3DF;

    constructor Create(const aWinW, aWinH : Integer; const aPos : TPoint3DF;
      const aAngle : Double);

    procedure update;
    function hits(Asteroid : cAsteroid) : Boolean;
    function offscreen : Boolean;
  end;

  cLaserList = specialize TFPGObjectList<cLaser>;

implementation

{ cLaser }

constructor cLaser.Create(const aWinW, aWinH : Integer; const aPos : TPoint3DF;
  const aAngle : Double);
begin
  WinW := aWinW;
  WinH := aWinH;

  pos := aPos;
  vel := TPoint3DF.Create(10, 0);
  vel.SetAngleXY(aAngle);
end;

procedure cLaser.update;
begin
  pos.add(vel);
end;

function cLaser.hits(Asteroid : cAsteroid) : Boolean;
var
  d : Double;
begin
  d := pos.Distance(Asteroid.pos);
  Result := d < Asteroid.r;
end;

function cLaser.offscreen : Boolean;
begin
  Result := (pos.X > WinW) or (pos.X < 0) or
    (pos.Y > WinH) or (pos.Y < 0);
end;

end.
