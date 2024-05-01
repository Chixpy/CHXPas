unit ucCTFirework2D;
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
  ucCTParticle2D;

type

  { cCTFirework2D }

  // A class to describe a group of Particles
  // An ArrayList is used to manage the list of Particles

  cCTFirework2D = class
  private

  protected

  public
    particles : cCTParticle2DList;    //< An arraylist for all the particles
    firework : cCTParticle2D;

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

  cCTFirework2DList = specialize TFPGObjectList<cCTFirework2D>;

implementation

{ cCTFirework2D }

function cCTFirework2D.done : Boolean;
begin
  Result := (firework = nil) and dead;
end;

function cCTFirework2D.dead : Boolean;
begin
  Result := particles.Count = 0;
end;

procedure cCTFirework2D.run;
var
  i : Integer;
  p : cCTParticle2D;
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
        particles.Add(cCTParticle2D.Create(firework.location, r, g, b));
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

constructor cCTFirework2D.Create(const x, y : Float; const aR, aG, AB : Byte;
  const aGravity : TPoint3DF);
begin
  // hu = random(255);
  firework := cCTParticle2D.Create(x, y, aR, aG, AB);
  particles := cCTParticle2DList.Create(True);   // Initialize the arraylist
  gravity := aGravity;

   r :=aR;
   g :=aG;
   b :=aB;
end;

destructor cCTFirework2D.Destroy;
begin
  firework.Free;
  particles.Free;

  inherited Destroy;
end;

end.
