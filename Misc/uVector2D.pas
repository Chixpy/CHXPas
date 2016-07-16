unit uVector2D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  rVector2D = record
    x: single;
    y: single;
  end;

function Vector2DLenght(const aVector: rVector2D): Single;
function Vector2DNormalize(const aVector: rVector2D): rVector2D;

function Vector2DAngle(const aVector: rVector2D): Single;

function Vector2DRotate(const aVector: rVector2D; aAngle: single): rVector2D;

operator + (const aVector, bVector: rVector2D): rVector2D;
operator - (const aVector, bVector: rVector2D): rVector2D;

operator * (const aVector: rVector2D;const aScale: Single): rVector2D;



implementation

function Vector2DLenght(const aVector: rVector2D): single;
begin
  Result := Sqrt(sqr(aVector.x) + sqr(aVector.y));
end;

function Vector2DNormalize(var aVector: rVector2D): rVector2D;
var
  VectorLenght: single;
begin
  VectorLenght := Vector2DLenght(aVector);
  if VectorLenght = 0 then
  begin
    aVector.x := 1;
    aVector.y := 0;
  end
  else
  begin
    aVector.x := aVector.x / VectorLenght;
    aVector.y := aVector.y / VectorLenght;
  end;
end;

function Vector2DAngle(aVector: rVector2D): Single;
begin
  Result := ArcTan2(aVector.x, aVector.y);
end;

function Vector2DRotate(const aVector: rVector2D; aAngle: single): rVector2D;
var
  s, c: single;
begin
  s := sin(aAngle);
  c := cos(aAngle);
  Result.x := +aVector.x * c + aVector.y * s;
  Result.y := -aVector.x * s + aVector.y * c;
end;

operator + (const aVector, bVector: rVector2D): rVector2D;
begin
  Result.x := Vec1.x + Vec2.x;
  Result.y := Vec1.y + Vec2.y;
end;

operator - (const aVector, bVector: rVector2D): rVector2D;
begin
  Result.x := Vec1.x - Vec2.x;
  Result.y := Vec1.y - Vec2.y;
end;

operator * (const aVector: rVector2D; aScale: Single): rVector2D;
begin
  aVector.x := aVector.x * aScale;
  aVector.y := aVector.y * aScale;
end;

end.
