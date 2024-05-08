unit ucCTCBlob;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/ccYLb7cLB1I
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  uCHXPoint3DF;

type

  { cCTCBlob }

  cCTCBlob = class
  private

  protected

  public
    pos : TPoint3DF;
    r : float;
    vel : TPoint3DF;

    procedure update;

    constructor Create(x, y : Float);
    destructor Destroy; override;
  end;

implementation

{ cCTCBlob }

procedure cCTCBlob.update;
begin
  pos.add(vel);
end;

constructor cCTCBlob.Create(x, y : Float);
begin
  pos := Point3DF(x, y);
  vel := TPoint3DF.CreateRnd(True);
  vel.scale(RandomRange(2, 6));
  r := RandomRange(40, 200);
end;

destructor cCTCBlob.Destroy;
begin
  inherited Destroy;
end;

end.
