unit uc3DStar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { c3DStar }

  c3DStar = class
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

    procedure Update(Speed: Single);

    constructor Create(aX,aY,aZ: Single);
    destructor Destroy; override;
  end;

implementation

{ c3DStar }

procedure c3DStar.SetX(const AValue : Single);
begin
  if FX = AValue then Exit;
  FX := AValue;
end;

procedure c3DStar.SetPZ(const AValue : Single);
begin
  if FPZ = AValue then Exit;
  FPZ := AValue;
end;

procedure c3DStar.SetY(const AValue : Single);
begin
  if FY = AValue then Exit;
  FY := AValue;
end;

procedure c3DStar.SetZ(const AValue : Single);
begin
  if FZ = AValue then Exit;
  FZ := AValue;
end;

procedure c3DStar.Update(Speed : Single);
begin
  PZ := Z;
  Z := Z - Speed;
end;

constructor c3DStar.Create(aX, aY, aZ : Single);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
  Self.PZ := Self.Z;
end;

destructor c3DStar.Destroy;
begin
  inherited Destroy;
end;

end.

