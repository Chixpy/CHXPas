unit uCHXMatrix;
{< Unit of TMatrix record.

  Copyright (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$inline ON}

interface

uses
  Classes, SysUtils;

type
  { TMatrix }
  TMatrixType = double;

  TMatrix = record
  private
    Data : array of array of TMatrixType;
    FHeight : byte;
    FWidth : byte;
    function GetItems(const aRow, aCol : byte) : TMatrixType;
    procedure SetHeight(const AValue : byte);
    procedure SetItems(const aRow, aCol : byte; const AValue : TMatrixType);
    procedure SetWidth(const AValue : byte);
  public
    property Width : byte read FWidth write SetWidth;
    property Height : byte read FHeight write SetHeight;

    property Items[aRow, aCol : byte] : TMatrixType
      read GetItems write SetItems; default;

    {$ifdef VER3}
      constructor Create(const NRows, NCols: Byte); overload;
      {< Creates a TMatrix. }
      constructor CreateIdentity(const NSize: Byte); overload;
      {< Creates a TMatrix cloning another TMatrix. }
    {$endif VER3}

    procedure Init(const NRows, NCols : byte);
    procedure Clone(const aMatrix : TMatrix);

    function IsAllZero : boolean;
    function IsDetZero : boolean;
    function IsIdentity : boolean;
    function IsSquare : boolean;

    class function Transpose(const aMatrix : TMatrix) : TMatrix; overload; static;
    procedure Transpose; overload;

    function Determinant : TMatrixType;

    class function Inverse(const aMatrix : TMatrix) : TMatrix; overload; static;
    procedure Inverse; overload;

  end;

  PMatrix = ^TMatrix;

implementation

{ TMatrix }

function TMatrix.GetItems(const aRow, aCol : byte) : TMatrixType;
begin
  if (aRow >= Self.Height) or (aCol >= Self.Width) then
  begin
    { TODO : Exception }
    Result := 0;
    Exit;
  end;

  Result := Self.Data[aRow, aCol];
end;

procedure TMatrix.SetHeight(const AValue : byte);
begin
  if Self.FHeight = AValue then Exit;
  // FHeight := AValue; Set on Init
  Self.Init(AValue, Self.Width);
end;

procedure TMatrix.SetItems(const aRow, aCol : byte;
  const AValue : TMatrixType);
begin
  if (aRow >= Self.Height) or (aCol >= Self.Width) then
    { TODO : Exception }
    Exit;

  Self.Data[aRow, aCol] := AValue;
end;

procedure TMatrix.SetWidth(const AValue : byte);
begin
  if FWidth = AValue then Exit;
  //FWidth := AValue;
  Self.Init(Self.Height, AValue);
end;

constructor TMatrix.Create(const NRows, NCols : byte);
begin
  Self.Init(NRows, NCols);
end;

constructor TMatrix.CreateIdentity(const NSize : byte);
var
  i : byte;
begin
  Self.Init(NSize, NSize);

  i := 0;
  while i < NSize do
  begin
    Self.Data[i, i] := 1;
    Inc(i);
  end;
end;

procedure TMatrix.Init(const NRows, NCols : byte);
begin
  FWidth := NCols;
  FHeight := NRows;
  SetLength(Self.Data, NRows, NCols);
end;

procedure TMatrix.Clone(const aMatrix : TMatrix);
begin
  Self := aMatrix;
end;

function TMatrix.IsAllZero : boolean;
begin

end;

function TMatrix.IsDetZero : boolean;
begin

end;

function TMatrix.IsIdentity : boolean;
begin

end;

function TMatrix.IsSquare : boolean;
begin
  Result := Self.Width = Self.Height;
end;

class function TMatrix.Transpose(const aMatrix : TMatrix) : TMatrix;
var
  i, j : byte;
begin
  Result.Init(aMatrix.Width, aMatrix.Height);

  i := 0;
  while i < Result.Width do
  begin
    j := 0;
    while j < Result.Height do
    begin
      Result.Data[j, i] := aMatrix.Data[i, j];
      Inc(j);
    end;
    Inc(i);
  end;
end;

procedure TMatrix.Transpose;
begin
  Self := Transpose(Self);
end;

function TMatrix.Determinant : TMatrixType;
begin

end;

class function TMatrix.Inverse(const aMatrix : TMatrix) : TMatrix;
begin

end;

procedure TMatrix.Inverse;
begin

end;

end.
{< This source is free software; you can redistribute it and/or modify it under
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
