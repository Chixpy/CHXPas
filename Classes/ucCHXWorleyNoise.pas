unit ucCHXWorleyNoise;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCHXPoint3DF;

type
  { 
    Worley Noise implementation.
    This type of noise is fairly simple to implement:
    - Places a series of random points in space.
    - Returns the distance to the n-th closest point (usually the closest one is used).
    
    Formula: Nw(x, y, z, n) = sqrt((X_n - X)^2 + (Y_n - Y)^2 + (Z_n - Z)^2)
    
    In principle, it can be optimized using some space partitioning method.
  }
  cCHXWorleyNoise = class
  public
    Points: cPoint3DFList; // Reference points list. Editable.
    
    constructor Create(const NPoints: Integer; 
                       const MinX: Real = 0; const MaxX: Real = 0;
                       const MinY: Real = 0; const MaxY: Real = 0;
                       const MinZ: Real = 0; const MaxZ: Real = 0);
    destructor Destroy; override;

    function GetValue(const X: Real; const Y: Real = 0; const Z: Real = 0;
      idxDist: Integer = 0): Real;
  end;

implementation

{ cCHXWorleyNoise }

constructor cCHXWorleyNoise.Create(const NPoints: Integer; 
                                const MinX, MaxX: Real;
                                const MinY, MaxY: Real; 
                                const MinZ, MaxZ: Real);
var
  i: Integer;
  RangeX, RangeY, RangeZ: Real;
  X, Y, Z: Real;
begin
  inherited Create;
  
  Points := cPoint3DFList.Create;
  
  // With 0 points, the list can be edited manually
  if NPoints <= 0 then 
    Exit;

  RangeX := MaxX - MinX;
  RangeY := MaxY - MinY; // Corrected from MaxY - MaxY in notes
  RangeZ := MaxZ - MinZ;

  for i := 1 to NPoints do
  begin
    X := MinX + Random * RangeX;
    Y := MinY + Random * RangeY;
    Z := MinZ + Random * RangeZ;
    Points.Add(Point3DF(X, Y, Z));
  end;
end;

destructor cCHXWorleyNoise.Destroy;
begin
  Points.Free;
  inherited Destroy;
end;

function cCHXWorleyNoise.GetValue(const X, Y, Z: Real; idxDist: Integer): Real;
var
  Distances: array of Real; // Corrected from Array of Integer to match aDist/Real
  aDist: Real;
  i: Integer;
begin
  // Hack: Negative numbers select from the last (far) point.
  if idxDist < 0 then 
    idxDist := Points.Count + idxDist;
    
  Result := 0.0;

  // Check bounds. Think about whether always returning 0 is the best approach.
  if (idxDist < 0) or (idxDist >= Points.Count) or (Points.Count <= 0) then
    Exit;

  SetLength(Distances, Points.Count);

  // Distances to points.
  // This could be optimized with space partitioning, as long as the 
  // returned point list has a number of elements greater than idxDist.
  for i := 0 to High(Distances) do
  begin
    // Assuming Points[i] returns an object/record that allows subtraction 
    // or has a method to calculate the distance/magnitude.
    aDist := (Points[i] - Point3DF(X, Y, Z)).GetMagnitude;
    Distances[i] := aDist;
    
    if Result < aDist then 
      Result := aDist;
  end;

  // Selecting the shortest distance at the idxDist position.
  // Faster than sorting the array.
  for aDist in Distances do
  begin
    if Result >= aDist then
    begin
      if idxDist > 0 then
        Dec(idxDist)
      else
        Result := aDist;
    end;
  end;
end;

end. 
