unit ucAsteroid;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/hacZU523FyM
// Processing transcription: Chuck England
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FGL, Math,
  uCHXPoint3DF;

type
  cAsteroid = class; //forward declaration

  cAsteroidList = specialize TFPGObjectList<cAsteroid>;

  { cAsteroid }

  cAsteroid = class
  protected
    WinW : Integer;
    WinH : Integer;

  public
    r : Double;
    pos : TPoint3DF;
    vel : TPoint3DF;
    total : Integer;
    offset : array of float;

    constructor Create(const aWinW, aWinH : Integer);
    constructor Create(const aWinW, aWinH : Integer; const aPos : TPoint3DF;
      const aR : Double);

    procedure update;
    function breakup : cAsteroidList;
    procedure edges;
  end;

implementation

{ cAsteroid }

constructor cAsteroid.Create(const aWinW, aWinH : Integer);
begin
  Create(aWinW, aWinH, Point3DF(Random(aWinW), Random(aWinH)),
    RandomRange(15, 50));
end;

constructor cAsteroid.Create(const aWinW, aWinH : Integer;
  const aPos : TPoint3DF; const aR : Double);
var
  i : Integer;
begin
  WinW := aWinW;
  WinH := aWinH;
  vel := TPoint3DF.CreateRnd(True);
  total := RandomRange(5, 15);
  pos := aPos;
  r := aR * 0.5;
  SetLength(offset, total);
  for i := 0 to total - 1 do
    offset[i] := (Random - 0.5) * r;
end;

procedure cAsteroid.Update;
begin
  pos.add(vel);
end;

function cAsteroid.breakup : cAsteroidList;
begin
  Result := cAsteroidList.Create(False);
  Result.Add(cAsteroid.Create(WinW, WinH, pos, r));
  Result.Add(cAsteroid.Create(WinW, WinH, pos, r));
end;

procedure cAsteroid.edges;
begin
  if (pos.X > WinW + r) then
    pos.X := -r
  else if (pos.X < -r) then
    pos.X := WinW + r;

  if (pos.Y > WinH + r) then
    pos.Y := -r
  else if (pos.Y < -r) then
    pos.Y := WinH + r;
end;

end.
