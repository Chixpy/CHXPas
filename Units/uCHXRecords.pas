unit uCHXRecords;

{< Unit with some useful records and operators between different types.

  // WARNING: Remember to remove ALL OPTIMIZATIONS (-O0) in debug mode.

  Copyright (C) 2023-2024 Chixpy https://github.com/Chixpy
}

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Types;

const
  krsIntValueSep = ',';
  krsFltValueSep = ';';

resourcestring
  rseInvalidTPointStr = '"%s" is an invalid TPoint string.';
  rseInvalidTRectStr = '"%s" is an invalid TRect string.';

type
  { TPointString }

  TPointString = record helper for TPoint
    function FromString(const S : string; const Delim : char = krsIntValueSep) : boolean;
    function ToString(const Delim : char = krsIntValueSep) : string; inline;
    {< For storing it and to read it later with FromString. }
    function ToFmtString(const FmsStr : string) : string; inline;
    {< For pretty formating, but not to read later. }
  end;

operator := (const aValue : string) : TPoint; inline;
operator := (const aValue : TPoint) : string; inline;

type
  { TRectString }

  TRectString = record helper for TRect
    function FromString(const S : string; const Delim : char = krsIntValueSep) : boolean;
    function ToString(const Delim : char = krsIntValueSep) : string; inline;
    {< For storing it and to read it later with FromString. }
  end;

operator := (const aValue : string) : TRect; inline;
operator := (const aValue : TRect) : string; inline;

type
  { TPoint3DF }

  TPoint3DF = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed {$endif FPC_REQUIRES_PROPER_ALIGNMENT} record
  type
    TSingle3Array = array [0..2] of single;

  public
{$ifdef VER3}
    constructor Create(aX, aY, aZ : single); overload;
    {< Creates a TPoint3DF. }
    constructor Create(aPoint3DF : TPoint3DF); overload;
    {< Creates a TPoint3DF cloning another TPoint3DF. }
{$endif}
    class function Zero : TPoint3DF; static; inline;
    {< Returns a new TPoint3DF at (0, 0, 0). }
    function IsZero : boolean;
    {< Checks if it's (0, 0, 0). }

    function Magnitude : double;
    {< Returns the distance to the origin. The vector modulus or length. }
    procedure SetMagnitude(aMag : single);
    {< Changes the distance to the origin or the vector modulus or length. }
    function Distance(const aPoint3DF : TPoint3DF) : double;
    {< Returns the distance between this point and another one. }

    procedure Add(const aPoint3DF : TPoint3DF);
    {< Adds point components separately. Vector addition. }
    procedure Subtract(const aPoint3DF : TPoint3DF);
    {< Subtracts point components separately. Vector substraction. }
    procedure Scale(aScale : single);
    {< Scales the distance of the point from the origin or scales the
       vector length. }
    procedure DivScale(aScale : single);
    {< Divides the distance of the point from the origin or the
       vector length. }
    procedure Invert;
    {< Opposite point from the origin. Inverts the vector. }
    procedure Normalize;
    {< Makes the vector length equal 1, with same direction. }

    procedure SetLocation(aX, aY, aZ : single);
    {< Set the components of the point or vector in single instruction. }
    procedure Copy(const aPoint3DF : TPoint3DF);
    {< Copy aPoint3DF data. }
    procedure Move(dX, dY, dZ : single);
    {< Moves the point from current position (actually is the same as Add
      but with components as parameters). }

    function InSphere(const aCenter : TPoint3DF;
      const aRadius : integer) : boolean;
    {< The point is inside a sphere? }

    class operator = (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : boolean; inline;
    {< Check if two point or vectors are equal. }
    class operator <> (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : boolean; inline;
    {< Check if two point or vectors are different. }
    class operator +(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF; inline;
    {< Adds two points. Adds vectors. }
    class operator -(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF; inline;
    {< Subtracts two points. Subtracts vectors. }
    class operator -(const aPoint3DF : TPoint3DF) : TPoint3DF; inline;
    {< Opposite point from origin. Inverted vector. }
    class operator * (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : single;
    {< Scalar product. Multiplies components and returns the sum. }
    //class operator * (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF;
    {< Multiplies point components individually. }
    class operator * (const aPoint3DF : TPoint3DF; const aFactor : single) : TPoint3DF;
    {< Scales the vector length by a factor (1). }
    class operator * (const aFactor : single; const aPoint3DF : TPoint3DF) : TPoint3DF;
    {< Scales the vector length by a factor (2). }
    class operator / (const aPoint3DF : TPoint3DF; const aFactor : single) : TPoint3DF;
     {< Divides the vector length by a factor (2). }
    class function VectProd(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF; static;
    {< Returns the vectorial product between two vectors. }


    function FromString(const S : string; const Delim : char = krsFltValueSep) : boolean;
    {< Tryes to read point components from a string. }
    function ToString(const Delim : char = krsFltValueSep) : string;
    {< Writes point as string for storing it, and ready to read it later with
      FromString. }

    case integer of
      0 : (
        Data : TSingle3Array;
      );
      1 : (
        X : single;
        Y : single;
        Z : single;
      );
  end;
  {< Single precision 3D point or 3D vector (from the origin). }

  PPoint3DF = ^TPoint3DF;

{< Pointer to a TPoint3DF. }

function Point3DF(aX, aY, aZ : single) : TPoint3DF;
{< Creates a new TPoint3DF. }

implementation

{ TPointString }

function TPointString.FromString(const S : string;
  const Delim : char) : boolean;
var
  StrLst : TStringList;
begin
  Self := Zero;

  StrLst := TStringList.Create;
  StrLst.Delimiter := Delim;
  StrLst.CommaText := S;
  try
    Result := StrLst.Count = 2;
    if Result then
      Result := TryStrToInt(Trim(StrLst[0]), Self.X);
    if Result then
      Result := TryStrToInt(Trim(StrLst[1]), Self.Y);
  finally
    StrLst.Free;
  end;

  if not Result then
    Self := Zero;
end;

function TPointString.ToString(const Delim : char) : string;
begin
  Result := Self.X.ToString + Delim + Self.Y.ToString;
end;

function TPointString.ToFmtString(const FmsStr : string) : string;
begin
  Result := Format(FmsStr, [Self.X, Self.Y]);
end;

operator := (const aValue : string) : TPoint;
begin
  Result.FromString(aValue);
end;

operator := (const aValue : TPoint) : string;
begin
  Result := aValue.ToString;
end;

{ TRectString }

function TRectString.FromString(const S : string;
  const Delim : char) : boolean;
var
  StrLst : TStringList;
begin
  Self := Empty;

  StrLst := TStringList.Create;
  StrLst.Delimiter := Delim;
  StrLst.CommaText := S;
  try
    Result := StrLst.Count = 4;
    if Result then
      Result := TryStrToInt(Trim(StrLst[0]), Self.Left);
    if Result then
      Result := TryStrToInt(Trim(StrLst[1]), Self.Top);
    if Result then
      Result := TryStrToInt(Trim(StrLst[2]), Self.Right);
    if Result then
      Result := TryStrToInt(Trim(StrLst[3]), Self.Bottom);
  finally
    StrLst.Free;
  end;

  if not Result then
    Self := Empty;
end;

function TRectString.ToString(const Delim : char) : string;
begin
  Result := Self.Left.ToString + Delim + Self.Top.ToString +
    Delim + Self.Right.ToString + Delim + Self.Bottom.ToString;
end;

operator := (const aValue : string) : TRect;
begin
  Result.FromString(aValue);
end;

operator := (const aValue : TRect) : string;
begin
  Result := aValue.ToString;
end;

{ TPoint3DF }

constructor TPoint3DF.Create(aX, aY, aZ : single);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
end;

constructor TPoint3DF.Create(aPoint3DF : TPoint3DF);
begin
  X := aPoint3DF.X;
  Y := aPoint3DF.Y;
  Z := aPoint3DF.Z;
end;

class function TPoint3DF.Zero : TPoint3DF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
end;

function TPoint3DF.IsZero : boolean;
begin
  Result := (Self.X = 0) and (Self.Y = 0) and (Self.Z = 0);
end;

function TPoint3DF.Magnitude : double;
begin
  Result := Distance(TPoint3DF.Zero);
end;

procedure TPoint3DF.SetMagnitude(aMag : single);
begin
  Self.Normalize;
  Self.Scale(aMag);
end;

function TPoint3DF.Distance(const aPoint3DF : TPoint3DF) : double;
begin
  // ValReal
  Result := Sqrt(Sqr(double(aPoint3DF.X) - ValReal(Self.X)) +
    Sqr(ValReal(aPoint3DF.Y) - ValReal(Self.Y)) +
    Sqr(ValReal(aPoint3DF.Z) - ValReal(Self.Z)));
end;

procedure TPoint3DF.Add(const aPoint3DF : TPoint3DF);
begin
  Self.X += aPoint3DF.X;
  Self.Y += aPoint3DF.Y;
  Self.Z += aPoint3DF.Z;
end;

procedure TPoint3DF.Subtract(const aPoint3DF : TPoint3DF);
begin
  Self.X -= aPoint3DF.X;
  Self.Y -= aPoint3DF.Y;
  Self.Z -= aPoint3DF.Z;
end;

procedure TPoint3DF.Scale(aScale : single);
begin
  Self.X *= aScale;
  Self.Y *= aScale;
  Self.Z *= aScale;
end;

procedure TPoint3DF.DivScale(aScale : single);
begin
  Self.X /= aScale;
  Self.Y /= aScale;
  Self.Z /= aScale;
end;

procedure TPoint3DF.Invert;
begin
  Self.X := -X;
  Self.Y := -Y;
  Self.Z := -Z;
end;

procedure TPoint3DF.Normalize;
var
  l : single;
begin
  l := Self.Magnitude;

  if l = 0 then Exit;

  Self.X /= l;
  Self.Y /= l;
  Self.Z /= l;
end;

procedure TPoint3DF.SetLocation(aX, aY, aZ : single);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
end;

procedure TPoint3DF.Copy(const aPoint3DF : TPoint3DF);
begin
  Self.X := aPoint3DF.X;
  Self.Y := aPoint3DF.Y;
  Self.Z := aPoint3DF.Z;
end;

procedure TPoint3DF.Move(dX, dY, dZ : single);
begin
  Self.X += dX;
  Self.Y += dY;
  Self.Z += dZ;
end;

function TPoint3DF.InSphere(const aCenter : TPoint3DF;
  const aRadius : integer) : boolean;
begin
  Result := Distance(aCenter) <= aRadius;
end;

class operator TPoint3DF. = (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : boolean;
begin
  Result := (aPoint3DF1.X = aPoint3DF2.X) and (aPoint3DF1.Y = aPoint3DF2.Y) and
    (aPoint3DF1.Z = aPoint3DF2.Z);
end;

class operator TPoint3DF. <> (
  const aPoint3DF1, aPoint3DF2 : TPoint3DF) : boolean;
begin
  Result := (aPoint3DF1.X <> aPoint3DF2.X) or (aPoint3DF1.Y <> aPoint3DF2.Y) or
    (aPoint3DF1.Z <> aPoint3DF2.Z);
end;

class operator TPoint3DF. +(const aPoint3DF1, aPoint3DF2 : TPoint3DF)
: TPoint3DF;
begin
  Result.Create(aPoint3DF1.X + aPoint3DF2.X, aPoint3DF1.Y +
    aPoint3DF2.Y, aPoint3DF1.Z + aPoint3DF2.Z);
end;

class operator TPoint3DF. -(const aPoint3DF1, aPoint3DF2 : TPoint3DF)
: TPoint3DF;
begin
  Result.Create(aPoint3DF1.X - aPoint3DF2.X, aPoint3DF1.Y -
    aPoint3DF2.Y, aPoint3DF1.Z - aPoint3DF2.Z);
end;

class operator TPoint3DF. -(const aPoint3DF : TPoint3DF) : TPoint3DF;
begin
  Result.Create(-aPoint3DF.X, -aPoint3DF.Y, -aPoint3DF.Z);
end;

class operator TPoint3DF. * (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : single;
begin
  Result := (aPoint3DF1.X * aPoint3DF2.X) + (aPoint3DF1.Y * aPoint3DF2.Y) +
    (aPoint3DF1.Z * aPoint3DF2.Z);
end;

class operator TPoint3DF. * (const aPoint3DF : TPoint3DF; const aFactor : single) : TPoint3DF;
begin
  Result.X := aPoint3DF.X * aFactor;
  Result.Y := aPoint3DF.Y * aFactor;
  Result.Z := aPoint3DF.Z * aFactor;
end;

class operator TPoint3DF. * (const aFactor : single; const aPoint3DF : TPoint3DF) : TPoint3DF;
begin
  Result.X := aPoint3DF.X * aFactor;
  Result.Y := aPoint3DF.Y * aFactor;
  Result.Z := aPoint3DF.Z * aFactor;
end;

class operator TPoint3DF. / (const aPoint3DF : TPoint3DF; const aFactor : single
  ): TPoint3DF;
begin
  if aFactor = 0 then Exit;

  Result.X := aPoint3DF.X / aFactor;
  Result.Y := aPoint3DF.Y / aFactor;
  Result.Z := aPoint3DF.Z / aFactor;
end;

class function TPoint3DF.VectProd(
  const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF;
begin
  Result.X := (aPoint3DF1.Y * aPoint3DF1.Z) - (aPoint3DF2.Y * aPoint3DF1.Z);
  Result.Y := (aPoint3DF1.Z * aPoint3DF1.X) - (aPoint3DF2.Z * aPoint3DF1.X);
  Result.Z := (aPoint3DF1.X * aPoint3DF1.Y) - (aPoint3DF2.X * aPoint3DF1.Y);
end;

function TPoint3DF.FromString(const S : string; const Delim : char) : boolean;
var
  StrLst : TStringList;
begin
  Self := Zero;

  StrLst := TStringList.Create;
  StrLst.Delimiter := Delim;
  StrLst.CommaText := S;
  try
    Result := StrLst.Count = 3;
    if Result then
      Result := TryStrToFloat(Trim(StrLst[0]), Self.X);
    if Result then
      Result := TryStrToFloat(Trim(StrLst[1]), Self.Y);
    if Result then
      Result := TryStrToFloat(Trim(StrLst[2]), Self.Z);
  finally
    StrLst.Free;
  end;

  if not Result then
    Self := Zero;
end;

function TPoint3DF.ToString(const Delim : char) : string;
begin
  Result := Self.X.ToString + Delim + Self.Y.ToString + Delim +
    Self.Z.ToString;
end;

function Point3DF(aX, aY, aZ : single) : TPoint3DF;
begin
  Result := TPoint3DF.Create(aX, aY, aZ);
end;

end.
{  This source is free software; you can redistribute it and/or modify it under
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
