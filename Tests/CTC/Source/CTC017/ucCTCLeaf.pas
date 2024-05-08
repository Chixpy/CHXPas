unit ucCTCLeaf;
// Coding Rainbow
// Daniel Shiffman
// http://patreon.com/codingtrain
// Code for: https://youtu.be/kKT0v3qhIQY
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  uCHXPoint3DF;

type

  { cCTCLeaf }

  cCTCLeaf = class
  private

  public
    pos : TPoint3DF;
    IsReached : Boolean;

    procedure Reached;

    constructor Create(const MaxX, MaxY : integer);
    destructor Destroy; override;
  end;

implementation

{ cCTCLeaf }

procedure cCTCLeaf.Reached;
begin
  IsReached := True;
end;

constructor cCTCLeaf.Create(const MaxX, MaxY : integer);
var
  RndAngle : Double;
begin
  IsReached := False;

  //// pos = PVector.random2D();
  //RndAngle := Random * 2 * pi; // Random angle
  //pos.Init(cos(RndAngle), Sin(RndAngle), 0); // Length 1
  //pos.Scale(random * MaxX);
  //pos.x := pos.x + MaxX;
  //pos.y := pos.y + MaxY;

  // pos = new PVector(random(10, MaxX-10), random(10, MaxX-40));
  pos.Init(RandomRange(10, MaxX-10), RandomRange(10, MaxY-40), 0);
end;

destructor cCTCLeaf.Destroy;
begin
  inherited Destroy;
end;

end.
