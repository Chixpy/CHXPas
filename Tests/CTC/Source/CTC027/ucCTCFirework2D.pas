unit ucCTCFirework2D;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/CKeyIbT3vXI
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl,
  uCHXPoint3DF,
  ucCTCParticle2D;

type

  { cCTCFirework2D }

  // A class to describe a group of Particles
  // An ArrayList is used to manage the list of Particles

  cCTCFirework2D = class
  private

  protected

  public
    particles : cCTParticle2DList;    //< An arraylist for all the particles
    firework : cCTCParticle2D;

    gravity : TPoint3DF;

    // hu: float
     r : Byte;
     g : Byte;
     b : Byte;

    function done: Boolean;

    function dead: Boolean;
    //< A method to test if the particle system still has particles

    procedure run;

    constructor Create(const x, y : Float; const aR, aG, AB : Byte; const aGravity: TPoint3DF);
    destructor Destroy; override;
  end;

  cCTFirework2DList = specialize TFPGObjectList<cCTCFirework2D>;

implementation

{ cCTCFirework2D }

function cCTCFirework2D.done : Boolean;
begin
  Result := (firework = nil) and dead;
end;

function cCTCFirework2D.dead : Boolean;
begin
  Result := particles.Count = 0;
end;

procedure cCTCFirework2D.run;
var
  i : Integer;
  p : cCTCParticle2D;
begin
  if firework <> nil then
  begin
    //fill(hu,255,255);
    firework.applyForce(gravity);
    firework.update();
    //firework.display();

    if firework.explode then
    begin
      for i := 0 to 99 do
      begin
        // Add "num" amount of particles to the arraylist
        particles.Add(cCTCParticle2D.Create(firework.location, r, g, b));
      end;

      FreeAndNil(firework);
    end;
  end;

  for i := particles.Count - 1 downto 0 do
  begin
    p := particles[i];
    p.applyForce(gravity);
    p.update;

    if p.isDead then particles.Delete(i);
  end;
end;

constructor cCTCFirework2D.Create(const x, y : Float; const aR, aG, AB : Byte;
  const aGravity : TPoint3DF);
begin
  // hu = random(255);
  firework := cCTCParticle2D.Create(x, y, aR, aG, AB);
  particles := cCTParticle2DList.Create(True);   // Initialize the arraylist
  gravity := aGravity;

   r :=aR;
   g :=aG;
   b :=aB;
end;

destructor cCTCFirework2D.Destroy;
begin
  firework.Free;
  particles.Free;

  inherited Destroy;
end;

end.
