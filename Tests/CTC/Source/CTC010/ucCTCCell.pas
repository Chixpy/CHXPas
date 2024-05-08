unit ucCTCCell;
// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain

// Videos
// https://youtu.be/HyK_Q5rrcr4
// https://youtu.be/D8UgRyRnvXU
// https://youtu.be/8Ju_uxJ9v44
// https://youtu.be/_p5IH0L63wo

// Depth-first search
// Recursive backtracker
// https://en.wikipedia.org/wiki/Maze_generation_algorithm
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TCellList = class;

  { cCTCCell }

  cCTCCell = class
  public
    i : integer;
    j : integer;

    walls : array[0..3] of Boolean; // = (True, True, True, True) Dreaming
    Visited : Boolean;

    function CheckNeighbors(Grid : TCellList) : cCTCCell;

    constructor Create(ii, jj : integer);
    destructor Destroy; override;
  end;

  TGenCellList = specialize TFPGObjectList<cCTCCell>;

  { TCellList }

  TCellList = class(TGenCellList)
  public
    Rows : integer;
    Cols : integer;

    function Cellidx(i, j : integer) : cCTCCell;
  end;

implementation

{ cCTCCell }

function cCTCCell.CheckNeighbors(Grid : TCellList) : cCTCCell;
var
  Neighbors : TCellList;
  Top, Right, Bottom, Left : cCTCCell;
begin
  Neighbors := TCellList.Create(False);

  Top := grid.Cellidx(i, j-1);
  Right := grid.Cellidx(i+1, j);
  Bottom := grid.Cellidx(i, j+1);
  Left := grid.Cellidx(i-1, j);

  if (Top <> nil) and (not top.visited) then
    neighbors.add(top);
  if (right <> nil) and (not right.visited) then
    neighbors.add(right);
  if (bottom <> nil) and (not bottom.visited) then
    neighbors.add(bottom);
  if (left <> nil) and (not left.visited) then
    neighbors.add(left);

  if (neighbors.Count > 0) then
    Result := neighbors[random(neighbors.Count)]
  else
    Result := nil;

  Neighbors.Free;
end;

constructor cCTCCell.Create(ii, jj : integer);
begin
  //for a in walls do  <- Another dream
  //  a := True;
  walls[0] := True;
  walls[1] := True;
  walls[2] := True;
  walls[3] := True;
  Visited := False;

  i := ii;
  j := jj;
end;

destructor cCTCCell.Destroy;
begin
  inherited Destroy;
end;

{ TCellList }

function TCellList.Cellidx(i, j : integer) : cCTCCell;
begin
  if (i < 0) or (j < 0) or (i > Cols - 1) or (j > Rows - 1) then
    Result := nil
  else
    Result := items[i + j * cols];
end;

end.
{
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
}
