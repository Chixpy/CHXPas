unit ucCTCBird;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/cXgA1d_E-jY
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes, Math,
  uCHXPoint3DF;

type

  { cCTCBird }

  cCTCBird = class
  private

  public
    WinHeight: integer;

    pos : TPoint3DF;
    vel : TPoint3DF;
    acc : TPoint3DF;
    r : float;

    procedure applyForce(force : TPoint3DF);

    procedure update;

    constructor Create(aHeight : Integer);
    destructor Destroy; override;
  end;

implementation

{ cCTCBird }

procedure cCTCBird.applyForce(force : TPoint3DF);
begin
  acc.add(force);
end;

procedure cCTCBird.update;
begin
  applyForce(TPoint3DF.Create(0, 0.1)); // gravity
  pos.add(vel);
  vel.add(acc);
  if vel.GetMagnitude > 4 then
    vel.SetMagnitude(4);
  acc.Init(0, 0, 0);

  if pos.y > WinHeight then
  begin
    pos.y := WinHeight;
    vel.Init(0, 0, 0);
  end;
end;

constructor cCTCBird.Create(aHeight : Integer);
begin
  r := 16;

  WinHeight := aHeight;

  pos := TPoint3DF.Create(50, aHeight / 2);
  vel := TPoint3DF.Create(0, 0);
  acc := TPoint3DF.Create(0, 0);
end;

destructor cCTCBird.Destroy;
begin
  inherited Destroy;
end;

end.
