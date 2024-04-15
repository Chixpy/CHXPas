unit ucCTBranch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCHXPoint3DF;

type

  { cCTBranch }

  cCTBranch = class
  private

  public
    // Ignoring z in TPoint3DF
    beginB : TPoint3DF;
    endB : TPoint3DF;
    finished : Boolean;

    procedure Jitter;

    // Generates a new Branch for the right-side
    function branchA : cCTBranch;
    // Generates a new Branch for the left-side
    function branchB : cCTBranch;

    constructor Create(aBegin, aEnd : TPoint3DF);
    destructor Destroy; override;
  end;

implementation

{ cCTBranch }

procedure cCTBranch.Jitter;
begin
  endB.x += random * 3 - 1.5;
  endB.y += random * 3 - 1.5;
end;

function cCTBranch.branchA : cCTBranch;
var
  dir , newEnd: TPoint3DF;
begin
  dir := endB - beginB;
  dir.RotateXY(PI / 6);
  dir.Scale(0.67);

  newEnd := endB + dir;

  Result := cCTBranch.Create(endB, newEnd);
end;

function cCTBranch.branchB : cCTBranch;
var
  dir , newEnd: TPoint3DF;
begin
  dir := endB - beginB;
  dir.RotateXY(- PI / 4);
  dir.Scale(0.67);

  newEnd := endB + dir;

  Result := cCTBranch.Create(endB, newEnd);
end;

constructor cCTBranch.Create(aBegin, aEnd : TPoint3DF);
begin
  beginB := aBegin;
  endB := aEnd;
  finished := False;
end;

destructor cCTBranch.Destroy;
begin
  inherited Destroy;
end;

end.
