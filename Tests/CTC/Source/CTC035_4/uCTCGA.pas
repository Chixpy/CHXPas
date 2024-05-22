unit uCTCGA;
// Coding Train
// Ported to processing by Max (https://github.com/TheLastDestroyer)
// Origional JS by Daniel Shiffman
// http://patreon.com/codingtrain
// Code for this video: https://www.youtube.com/watch?v=M3KTWnTrU_c
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl;

type
  cIntGenList = specialize TFPGList<integer>;

  { cIntList }

  cIntList = class(cIntGenList)
  public
    // CHX: Adding shuffle function.
    procedure shuffle();
    {< Shuffle current items order. }
  end;

procedure normalizeFitness(var fitness : array of float);
// External "constructor" functions... U_U
function pickOne(var list : array of cIntList;
  var prob : array of float) : cIntList;
function crossOver(orderA, orderB : cIntList) : cIntList;
procedure mutate(order : cIntList; mutationRate : float);
procedure swap(a : cIntList; i, j : integer);

implementation

procedure normalizeFitness(var fitness : array of float);
var
  aSum : Float;
  i : integer;
begin
  aSum := 0;

  for i := 0 to Length(fitness) - 1 do
    aSum += fitness[i];

  for i := 0 to Length(fitness) - 1 do
    fitness[i] := fitness[i] / aSum;
end;

function pickOne(var list : array of cIntList;
  var prob : array of float) : cIntList;
var
  index : integer;
  r : Float;
begin
  Result := cIntList.Create;
  index := 0;
  r := Random;

  while r > 0 do
  begin
    r := r - prob[index];
    Inc(index);
  end;
  Dec(index);

  Result.Assign(list[index]);
end;

function crossOver(orderA, orderB : cIntList) : cIntList;
var
  start, aEnd, i, city : integer;
begin
  Result := cIntList.Create; // neworder

  start := random(orderA.Count);
  aEnd := RandomRange(start + 1, orderA.Count);

  { CHX: Fixing range overflow.
    If start = orderA.Count - 1 (last item) then
      aEnd := RandomRange(orderA.Count, orderA.Count), so aEnd := orderA.Count
      the next For will be `for i := orderA.Count - 1 to orderA.Count`
      doing a range overflow in orderA[i]
  }
  while aEnd >= orderA.Count do
    dec(aEnd);
  for i := start to aEnd do
    Result.Add(orderA[i]);

  for i := 0 to orderB.Count - 1 do
  begin
    city := orderB[i];
    if Result.IndexOf(city) = -1 then
      Result.Add(city);
  end;
end;

procedure mutate(order : cIntList; mutationRate : float);
var
  i, indexA, indexB : integer;
begin
  // CHX: Changed totalCities -> order.Count
  for i := 0 to order.Count - 1 do
  begin
    if Random < mutationRate then
    begin
      indexA := random(order.Count);
      indexB := (indexA + 1) mod order.Count;
      swap(order, indexA, indexB);
    end;
  end;
end;

procedure swap(a : cIntList; i, j : integer);
var
  temp : integer;
begin
  temp := a[i];
  a[i] := a[j];
  a[j] := temp;
end;

procedure cIntList.shuffle();
var
  i, j : integer;
begin
  // Random swaps on every item position.
  i := 0;
  while i < Count do
  begin
    j := Random(Count);
    if i <> j then
      Exchange(i, j);
    Inc(i);
  end;
end;

end.
