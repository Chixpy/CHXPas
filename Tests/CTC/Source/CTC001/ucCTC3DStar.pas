unit ucCTC3DStar;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/17WoOqgXsRM
// Port: (C) 2024 Chixpy https://github.com/Chixpy


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cCTC3DStar }

  cCTC3DStar = class
  public
    X: Single;
    Y: Single;
    Z: Single;

    PZ: Single;
    {< Previous Z}

    procedure Update(Speed: Single);

    constructor Create(aX,aY,aZ: Single);
    destructor Destroy; override;
  end;

implementation

{ cCTC3DStar }

procedure cCTC3DStar.Update(Speed : Single);
begin
  PZ := Z;
  Z := Z - Speed;
end;

constructor cCTC3DStar.Create(aX, aY, aZ : Single);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
  Self.PZ := Self.Z;
end;

destructor cCTC3DStar.Destroy;
begin
  inherited Destroy;
end;

end.

