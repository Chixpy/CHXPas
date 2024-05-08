unit ucCTCBranch;
//* @author Lukas Klassen
//* translated version of CC_015_FractalTreeArray by Daniel Shiffmann

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for:                            <- Change this
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCHXPoint3DF;

type

  { cCTCBranch }

  cCTCBranch = class
  private

  public
    // Ignoring z in TPoint3DF
    beginB : TPoint3DF;
    endB : TPoint3DF;
    finished : Boolean;

    procedure Jitter;

    // Generates a new Branch for the right-side
    function branchA : cCTCBranch;
    // Generates a new Branch for the left-side
    function branchB : cCTCBranch;

    constructor Create(aBegin, aEnd : TPoint3DF);
    destructor Destroy; override;
  end;

implementation

{ cCTCBranch }

procedure cCTCBranch.Jitter;
begin
  endB.x += random * 3 - 1.5;
  endB.y += random * 3 - 1.5;
end;

function cCTCBranch.branchA : cCTCBranch;
var
  dir , newEnd: TPoint3DF;
begin
  dir := endB - beginB;
  dir.RotateXY(PI / 6);
  dir.Scale(0.67);

  newEnd := endB + dir;

  Result := cCTCBranch.Create(endB, newEnd);
end;

function cCTCBranch.branchB : cCTCBranch;
var
  dir , newEnd: TPoint3DF;
begin
  dir := endB - beginB;
  dir.RotateXY(- PI / 4);
  dir.Scale(0.67);

  newEnd := endB + dir;

  Result := cCTCBranch.Create(endB, newEnd);
end;

constructor cCTCBranch.Create(aBegin, aEnd : TPoint3DF);
begin
  beginB := aBegin;
  endB := aEnd;
  finished := False;
end;

destructor cCTCBranch.Destroy;
begin
  inherited Destroy;
end;

end.
