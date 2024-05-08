unit ucCTCParticle2D;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/CKeyIbT3vXI
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl,
  uCHXPoint3DF;

type

  { cCTCParticle2D }

  cCTCParticle2D = class
  private

  protected

  public
    location : TPoint3DF;
    velocity : TPoint3DF;
    acceleration: TPoint3DF;

    lifespan: Float;

    seed: Boolean;

    // hu: float
    r : Byte;
    g : Byte;
    b : Byte;

    procedure applyForce(const force: TPoint3DF);

    procedure update;

    function explode: Boolean;

    function isDead: Boolean;

    constructor Create(const x,y: float;const aR, aG, AB : Byte);
    constructor Create(const l: TPoint3DF;const aR, aG, AB : Byte);
    destructor Destroy; override;

  published

  end;

  cCTParticle2DList = specialize TFPGObjectList<cCTCParticle2D>;

implementation

{ cCTCParticle2D }

procedure cCTCParticle2D.applyForce(const force : TPoint3DF);
begin
  acceleration.add(force);
end;

procedure cCTCParticle2D.update;
begin
  velocity.add(acceleration);
  location.add(velocity);
  if (not seed) then
  begin
    lifespan -= 5.0;
    velocity.Scale(0.95);
  end;
  acceleration.Scale(0);
end;

function cCTCParticle2D.explode : Boolean;
begin
  if seed and (velocity.y > 0) then
  begin
    lifespan := 0;
    Result := True;
  end
  else
    Result := False;
end;

function cCTCParticle2D.isDead : Boolean;
begin
  Result := lifespan < 0.0;
end;

constructor cCTCParticle2D.Create(const x, y : float;const aR, aG, AB : Byte);
begin
  // hu := h;
   acceleration := Point3DF(0, 0);
   velocity := Point3DF(0, RandomRange(-12, -5));
   location :=  Point3DF(x, y);
   seed := true;
   lifespan := 255.0;

   r :=aR;
   g :=aG;
   b :=aB;
end;

constructor cCTCParticle2D.Create(const l : TPoint3DF;const aR, aG, AB : Byte);
begin
  // hu := h;
  acceleration := Point3DF(0, 0);
  velocity := TPoint3DF.CreateRnd(True);
  velocity.Scale(Random * 4 + 4);
  location := l;
  lifespan := 255.0;

     r :=aR;
     g :=aG;
     b :=aB;
end;

destructor cCTCParticle2D.Destroy;
begin
  inherited Destroy;
end;

end.

