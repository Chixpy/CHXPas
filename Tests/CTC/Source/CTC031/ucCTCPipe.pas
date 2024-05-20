unit ucCTCPipe;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/cXgA1d_E-jY
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  ucCTCBird;

type

  { cCTCPipe }

  cCTCPipe = class
  private
    WindowHeight: integer;

  public
    x : float;
    top : float;
    bottom : float;
    w : float;

    hited : Boolean; // CHX: Added to

    procedure update;

    function hits(b : cCTCBird) : Boolean;

    constructor Create(const WWidth : integer; const WHeight : integer);
    destructor Destroy; override;
  end;

implementation

{ cCTCPipe }

procedure cCTCPipe.update;
begin
  x -= 3;
end;

function cCTCPipe.hits(b : cCTCBird) : Boolean;
begin
  Result := False;
  hited := False;
  if (b.pos.x > x) and (b.pos.x < (x + w)) then
    if (b.pos.y < (top + b.r)) or (b.pos.y > (WindowHeight - bottom - b.r)) then
    begin
      hited := True;
      Result := True;
    end;
end;

constructor cCTCPipe.Create(const WWidth : integer; const WHeight : integer);
begin
  WindowHeight := WHeight;
  w := 40;
  x := WWidth + w;
  top := RandomRange(100, WindowHeight div 2);
  bottom := RandomRange(100, WindowHeight div 2);
end;

destructor cCTCPipe.Destroy;
begin
  inherited Destroy;
end;

end.
