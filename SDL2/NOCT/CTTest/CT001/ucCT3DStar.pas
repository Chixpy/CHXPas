unit ucCT3DStar;
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

  { cCT3DStar }

  cCT3DStar = class
  private
    FPZ : Single;
    FX : Single;
    FY : Single;
    FZ : Single;
    procedure SetPZ(const AValue : Single);
    procedure SetX(const AValue : Single);
    procedure SetY(const AValue : Single);
    procedure SetZ(const AValue : Single);

  public
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
    property Z: Single read FZ write SetZ;

    property PZ: Single read FPZ write SetPZ;
    {< Previous Z}

    procedure Update(Speed: Single);

    constructor Create(aX,aY,aZ: Single);
    destructor Destroy; override;
  end;

implementation

{ cCT3DStar }

procedure cCT3DStar.SetX(const AValue : Single);
begin
  if FX = AValue then Exit;
  FX := AValue;
end;

procedure cCT3DStar.SetPZ(const AValue : Single);
begin
  if FPZ = AValue then Exit;
  FPZ := AValue;
end;

procedure cCT3DStar.SetY(const AValue : Single);
begin
  if FY = AValue then Exit;
  FY := AValue;
end;

procedure cCT3DStar.SetZ(const AValue : Single);
begin
  if FZ = AValue then Exit;
  FZ := AValue;
end;

procedure cCT3DStar.Update(Speed : Single);
begin
  PZ := Z;
  Z := Z - Speed;
end;

constructor cCT3DStar.Create(aX, aY, aZ : Single);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
  Self.PZ := Self.Z;
end;

destructor cCT3DStar.Destroy;
begin
  inherited Destroy;
end;

end.

