unit ucCTCTree;
// Coding Rainbow
// Daniel Shiffman
// http://patreon.com/codingtrain
// Code for: https://youtu.be/kKT0v3qhIQY
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  uCHXPoint3DF,
  ucCTCBranch, ucCTCLeaf;

const
  max_dist = 100;
  min_dist = 10;

type
  TBranchList = specialize TFPGObjectList<cCTCBranch>;
  TLeafList = specialize TFPGObjectList<cCTCLeaf>;

  { cCTCTree }

  cCTCTree = class
  private

  public
    branches : TBranchList;
    leaves : TLeafList;

    function closeEnough(b : cCTCBranch) : Boolean;
    procedure Grow;

    constructor Create(const WinW, WinH : integer);
    destructor Destroy; override;
  end;

implementation

{ ucCTTree }

function cCTCTree.closeEnough(b : cCTCBranch) : Boolean;
var
  i : integer;
begin
  Result := False;
  i := 0;
  while (i < leaves.Count) and (not Result) do
  begin
    if b.pos.Distance(leaves[i].pos) < max_dist then
      Result := True;
    Inc(i);
  end;
end;

procedure cCTCTree.Grow;
var
  l : cCTCLeaf;
  closest, b, newB : cCTCBranch;
  closestDir, dir : TPoint3DF;
  aRecord : Double;
  d : Double;
  i : integer;
begin
  for l in leaves do
  begin
    closest := nil;
    closestDir := closestDir.Zero;
    aRecord := -1;

    for b in branches do
    begin
      dir := l.pos - b.pos;
      d := dir.GetMagnitude;

      if d < min_dist then
      begin
        l.Reached;
        closest := nil;
        // CHX: U_U Ahhh!
        Break;
      end
      else if d > max_dist then
      begin
        // Nothing
      end
      else if (not assigned(closest)) or (d < aRecord) then
      begin
        closest := b;
        closestDir := dir;
        arecord := d;
      end;
    end;

    if assigned(closest) then
    begin
      closestDir.Normalize;
      closest.dir.add(closestDir);
      closest.Count := closest.Count + 1;
    end;
  end;

  i := leaves.Count - 1;
  while i >= 0 do
  begin
    if leaves[i].IsReached then leaves.Delete(i);
    Dec(i);
  end;

  i := branches.Count - 1;
  while i >= 0 do
  begin
    b := branches[i];
    if b.Count > 0 then
    begin
      // b.dir.DivScale(b.Count); // CHX: why it is divided by b.count and
      b.dir.Normalize;            //      then normalized?
      newB := cCTCBranch.Create(b);
      branches.Add(newB);
      b.Reset;
    end;
    Dec(i);
  end;
end;

constructor cCTCTree.Create(const WinW, WinH : integer);
var
  i : integer;
  root, current, trunk : cCTCBranch;
begin
  branches := TBranchList.Create(True);
  leaves := TLeafList.Create(True);

  for i := 0 to 1999 do
    leaves.Add(cCTCLeaf.Create(WinW, WinH - 100));

  root := cCTCBranch.Create(Point3DF(WinW div 2, WinH, 0), Point3DF(0, -1, 0));
  branches.Add(root);

  { CHX: !!!! Why create a clone? }
  //current := cCTCBranch.Create(root);
  current := root;

  while not closeEnough(current) do
  begin
    trunk := cCTCBranch.Create(current);
    branches.add(trunk);
    current := trunk;
  end;
end;

destructor cCTCTree.Destroy;
begin
  leaves.Free;
  branches.Free;

  inherited Destroy;
end;

end.
