unit ucPipe;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/cXgA1d_E-jY&
// Ported to Processing 4 by Spencer Stith<http://github.com/spencerstith>
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  ucBird;

type

  { cPipe }

  cPipe = class
  protected
    WinW : Integer;
    WinH : Integer;

  public
    spacing : Double;
    centery : Double;

    top : Double;
    bottom : Double;
    X : Integer;
    w : Integer;
    speed : Integer;

    highlight : Boolean;

    constructor Create(const aWinW, aWinH : Integer);
    destructor Destroy; override;

    function hits(bird : cBird) : Boolean;
    procedure Update;
    function offscreen : Boolean;
  end;

implementation

{ cPipe }

constructor cPipe.Create(const aWinW, aWinH : Integer);
begin
  inherited Create;
  WinW := aWinW;
  WinH := aWinH;

  spacing := RandomRange(50, WinH div 2);
  centery := RandomRange(Round(spacing), WinH - Round(spacing));

  top := centery - spacing / 2;
  bottom := WinH - (centery + spacing / 2);
  X := WinW;
  w := 50;
  speed := 2;

  highlight := False;
end;

destructor cPipe.Destroy;
begin
  inherited Destroy;
end;

function cPipe.hits(bird : cBird) : Boolean;
begin
  highlight := False;
  if (bird.Y < top) or (bird.Y > WinH - bottom) then
    if (bird.X > self.X) and (bird.X < self.X + self.w) then
      highlight := True;
  Result := highlight;
end;

procedure cPipe.Update;
begin
  X -= speed;
end;

function cPipe.offscreen : Boolean;
begin
  Result := X < -w;
end;

end.
