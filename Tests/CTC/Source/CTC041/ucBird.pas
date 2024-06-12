unit ucBird;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/cXgA1d_E-jY&
// Ported to Processing 4 by Spencer Stith<http://github.com/spencerstith>
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cBird }

  cBird = class
  protected
    WinW : Integer;
    WinH : Integer;

  public
    Y : Integer;
    X : Integer;

    gravity : Double;
    lift : Double;
    velocity : Double;

    constructor Create(const aWinW, aWinH : Integer);
    destructor Destroy; override;

    procedure up;
    procedure update;
  end;

implementation

{ cBird }

constructor cBird.Create(const aWinW, aWinH : Integer);
begin
  WinW := aWinW;
  WinH := aWinH;

  Y := WinH div 2;
  X := 64;

  gravity := 0.6;
  lift := -15;
  velocity := 0;
end;

destructor cBird.Destroy;
begin
  inherited Destroy;
end;

procedure cBird.up;
begin
  velocity += lift;
end;

procedure cBird.update;
begin
  velocity += gravity;
  velocity *= 0.9;
  Y += Round(velocity);

  if (Y > WinH) then
  begin
    Y := WinH;
    velocity := 0;
  end;

  if (Y < 0) then
  begin
    Y := 0;
    velocity := 0;
  end;
end;

end.
