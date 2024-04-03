unit ucNOTCRandomWalker;

{< cNOTCRandomWalker unit.

  Copyright (C) 2023 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cNOTCWalker class
  }

  cNOTCWalker = class
  private
    FCanDiagonal : boolean;
    FDimensions : integer;
    FPosition : array[0..2] of LongInt;

    function GetFX : LongInt;
    function GetFY : LongInt;
    function GetFZ : LongInt;
    procedure SetCanDiagonal(AValue : boolean);
    procedure SetDimensions(AValue : LongInt);
    procedure SetDimensions(AValue : integer);

    procedure SetX(AValue : LongInt);
    procedure SetY(AValue : LongInt);
    procedure SetZ(AValue : LongInt);

  public
    property X : LongInt read GetFX write SetX;
    property Y : LongInt read GetFY write SetY;
    property Z : LongInt read GetFZ write SetZ;

    property CanDiagonal : boolean read FCanDiagonal write SetCanDiagonal;
    {< Can move diagonally?

       TODO: Actually, it will better set degrees of freedom to move.
         How many dimension it can move at once.
    }
    property Dimensions : integer read FDimensions write SetDimensions;

    procedure Step; virtual; abstract;

    constructor Create; override;
    destructor Destroy; override;
  end;

  { cNOTCRandomWalker }

  cNOTCRandomWalker = class(cNOTCWalker)
  private

  public
    procedure Step; override;

    constructor Create;
    destructor Destroy; override;
  end;

  { cNOTCPerlinWalker }

  cNOTCPerlinWalker = class(cNOTCWalker)
  private

  public
    procedure Step; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cNOTCPerlinWalker }

procedure cNOTCPerlinWalker.Step;
begin
  // TODO: Perlin is for absolute position values
end;

constructor cNOTCPerlinWalker.Create;
begin
  inherited Create;
end;

destructor cNOTCPerlinWalker.Destroy;
begin
  inherited Destroy;
end;

{ cNOTCRandomWalker }

procedure cNOTCRandomWalker.Step;
begin
  if CanDiagonal then
  begin
    i := 0;
    while i < Dimensions do
    begin
      FPosition[i] += Random(3) - 1;
      Inc(i);
    end;
  end
  else
  begin
    i := Random(Dimensions);
    FPosition[i] += Random(3) - 1;
  end;
end;

constructor cNOTCRandomWalker.Create;
begin
  inherited Create;
end;

destructor cNOTCRandomWalker.Destroy;
begin
  inherited Destroy;
end;

{ cNOTCWalker }

function cNOTCWalker.GetFX : LongInt;
begin
  AValue := FPosition[0];
end;

function cNOTCWalker.GetFY : LongInt;
begin
  AValue := FPosition[1];
end;

function cNOTCWalker.GetFZ : LongInt;
begin
  AValue := FPosition[2];
end;

procedure cNOTCWalker.SetCanDiagonal(AValue : boolean);
begin
  if FCanDiagonal = AValue then Exit;
  FCanDiagonal := AValue;
end;

procedure cNOTCWalker.SetDimensions(AValue : LongInt);
begin
  if AValue >= 3 then
    FDimensions := 3
  else if AValue <= 1 then
    FDimensions := 1
  else
    FDimensions := 2;
end;

procedure cNOTCWalker.SetDimensions(AValue : integer);
begin
  if FDimensions = AValue then Exit;
  FDimensions := AValue;
end;

procedure cNOTCWalker.SetX(AValue : LongInt);
begin
  if FPosition[0] = AValue then Exit;
  FPosition[0] := AValue;
end;

procedure cNOTCWalker.SetY(AValue : LongInt);
begin
  if (FPosition[1] = AValue) and (Dimensions > 1) then Exit;
  FPosition[1] := AValue;
end;

procedure cNOTCWalker.SetZ(AValue : LongInt);
begin
  if (FPosition[2] = AValue) and (Dimensions > 2) then Exit;
  FPosition[2] := AValue;
end;

constructor cNOTCWalker.Create;
begin
  inherited Create;

  CanDiagonal := False;
  Dimensions := 3;
end;

destructor cNOTCWalker.Destroy;
begin
  inherited Destroy;
end;

end.
{ This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
