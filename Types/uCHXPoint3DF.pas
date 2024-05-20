unit uCHXPoint3DF;
{< Unit of TPoint3DF record.

  Copyright (C) 2023-2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$inline ON}

// ToDo: Make inline functions.

interface

uses
  Classes, SysUtils, Math;

const
  krsFltValueSep = ';';

type
  { TPoint3DF }
  TPoint3DFType = Double;

  TPoint3DF = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed {$endif FPC_REQUIRES_PROPER_ALIGNMENT} record
    X : TPoint3DFType;
    Y : TPoint3DFType;
    Z : TPoint3DFType;
  public

    {Not sure why i can't do aPoint3DF.X := aValue; }
    procedure SetX(const AValue : TPoint3DFType);
    procedure SetY(const AValue : TPoint3DFType);
    procedure SetZ(const AValue : TPoint3DFType);

    {$ifdef VER3}
      constructor Create(const aX, aY: TPoint3DFType; const aZ : TPoint3DFType = 0); overload;
      {< Creates a TPoint3DF. }
      constructor Create(const aPoint3DF : TPoint3DF); overload;
      {< Creates a TPoint3DF cloning another TPoint3DF. }
      constructor CreateRnd(Only2D: Boolean; aMag: TPoint3DFType = 1);
      {< Creates a TPoint3DF with `aMag` lenght (default = 1) and random angles.

        If Only2D is true, only rotates it in XY plane.
      }
    {$endif VER3}

    procedure Init(const aX, aY : TPoint3DFType;
      const aZ : TPoint3DFType = 0);
    {< Set the components of the point or vector in single instruction. }
    procedure Clone(const aPoint3DF : TPoint3DF);
    {< Copy aPoint3DF data. }

    class function Zero : TPoint3DF; static;
    {< Returns a new TPoint3DF at (0, 0, 0). }
    function IsZero : Boolean;
    {< Checks if it's (0, 0, 0). }

    function GetMagnitude : TPoint3DFType;
    {< Magnitude, modulus or length of the vector. Returns the distance
       to the origin.
    }
    procedure SetMagnitude(const aMag : TPoint3DFType);
    {< Changes the distance to the origin or the vector modulus or length. }

    function GetAngleXY : TPoint3DFType;
    {< Angle of the vector on X/Y plane projection. }
    function GetAngleXZ : TPoint3DFType;
    {< Angle of the vector on X/Z plane projection. }
    function GetAngleYZ : TPoint3DFType;
    {< Angle of the vector on Y/Z plane projection. }

    { TODO: Set absolute angles. }
    //function SetAngleXY: TPoint3DFType;
    //{< Change the angle of the vector on X/Y plane projection. }
    //function SetAngleXZ: TPoint3DFType;
    //{< Change the angle of the vector on X/Z plane projection. }
    //function SetAngleYZ: TPoint3DFType;
    //{< Change the angle of the vector on Y/Z plane projection. }

    procedure RotateXY(const aAngle : TPoint3DFType); overload;
    {< Rotates the counter clockwise angle of the vector on X/Y plane
      projection around origin. }
    procedure RotateXY(const aAngle : TPoint3DFType;
      const aCenter : TPoint3DF); overload;
    {< Rotates the counter clockwise angle of the vector on X/Y plane projection
      with another point as center. }
    procedure RotateXZ(const aAngle : TPoint3DFType); overload;
    {< Rotates the counter clockwise angle of the vector on X/Z plane
      projection around origin. }
    procedure RotateXZ(const aAngle : TPoint3DFType;
      const aCenter : TPoint3DF); overload;
    {< Rotates the counter clockwise angle of the vector on X/Z plane projection
      with another point as center. }
    procedure RotateYZ(const aAngle : TPoint3DFType); overload;
    {< Rotates the counter clockwise angle of the vector on Y/Z plane
      projection around origin. }
    procedure RotateYZ(const aAngle : TPoint3DFType;
      const aCenter : TPoint3DF); overload;
    {< Rotates the counter clockwise angle of the vector on Y/Z plane projection
      with another point as center. }

    function Distance(const aPoint3DF : TPoint3DF) : TPoint3DFType;
    {< Returns the distance between this point and another one. }
    function InSphere(const aCenter3DF : TPoint3DF;
      const aRadius : TPoint3DFType) : Boolean;
    {< The point is inside a sphere? }

    function IsEqual(const aPoint3DF : TPoint3DF) : Boolean;
    class operator =(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : Boolean;
    //{< Check if two point or vectors are equal. }
    { NOTE: Automatically created by FPC if = created. }
    //class operator <>(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : Boolean;
    //{< Check if two point or vectors are different. }

    procedure Add(const aPoint3DF : TPoint3DF);
    {< Vector addition. Adds point components separately. }
    procedure Move(const dX, dY, dZ : TPoint3DFType);
    {< Moves the point from current position (actually is the same as Add
      but with components as parameters). }
    class operator +(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF;
    {< Adds two points. Adds vectors. }


    procedure Substract(const aPoint3DF : TPoint3DF);
    {< Vector substraction. Subtracts point components separately. }
    class operator -(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF;
    {< Subtracts two points. Subtracts vectors. }

    procedure Scale(aScale : TPoint3DFType);
    {< Scales the distance of the point from the origin or scales the vector length. }
    class operator *(const aPoint3DF : TPoint3DF;
      const aFactor : TPoint3DFType) : TPoint3DF;
    {< Scales the vector length by a factor (1). }
    class operator *(const aFactor : TPoint3DFType;
      const aPoint3DF : TPoint3DF) : TPoint3DF;
    {< Scales the vector length by a factor (2). }

    procedure DivScale(const aScale : TPoint3DFType);
    {< Divides the distance of the point from the origin or the vector length. }
    class operator /(const aPoint3DF : TPoint3DF;
      const aFactor : TPoint3DFType) : TPoint3DF;
    {< Divides the vector length by a factor. }

    procedure Invert;
    {< Opposite point from the origin. Inverts the vector. }
    class operator -(const aPoint3DF : TPoint3DF) : TPoint3DF;
    {< Opposite point from origin. Inverted vector. }

    procedure Normalize;
    {< Makes the vector length equal 1, with same direction. }

    class function VectProd(const aPoint3DF1, aPoint3DF2 : TPoint3DF)
      : TPoint3DF; static;
    {< Returns the vectorial product between two vectors. }

    function ScalProd(const aPoint3DF : TPoint3DF) : TPoint3DFType;
    class operator *(const aPoint3DF1, aPoint3DF2 : TPoint3DF)
      : TPoint3DFType;
    {< Scalar product. Multiplies components and returns the sum. }

    // class operator * (const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF;
    {< Multiplies point components individually. }


    function FromString(const S : string;
      const Delim : char = krsFltValueSep) : Boolean;
    {< Tryes to read point components from a string. }
    function ToString(const Delim : char = krsFltValueSep) : string;
    {< Writes point as string for storing it, and ready to read it later with
      FromString. }
  end;
  {< Single precision 3D point or 3D vector (from the origin). }

  PPoint3DF = ^TPoint3DF;
  {< Pointer to a TPoint3DF. }

function Point3DF(aX, aY: TPoint3DFType; aZ : TPoint3DFType = 0) : TPoint3DF;
{< Creates a new TPoint3DF. }

implementation

function Point3DF(aX, aY, aZ : TPoint3DFType) : TPoint3DF;
begin
  Result.X := aX;
  Result.Y := aY;
  Result.Z := aZ;
end;

{ TPoint3DF }
{$ifdef VER3}

procedure TPoint3DF.SetX(const AValue : TPoint3DFType);
begin
  X := AValue;
end;

procedure TPoint3DF.SetY(const AValue : TPoint3DFType);
begin
  Y := AValue;
end;

procedure TPoint3DF.SetZ(const AValue : TPoint3DFType);
begin
  Z := AValue;
end;

constructor TPoint3DF.Create(const aX, aY : TPoint3DFType;
  const aZ : TPoint3DFType);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
end;

constructor TPoint3DF.Create(const aPoint3DF : TPoint3DF);
begin
  //Self.X := aPoint3DF.X;
  //Self.Y := aPoint3DF.Y;
  //Self.Z := aPoint3DF.Z;
  Self := aPoint3DF;
end;

constructor TPoint3DF.CreateRnd(Only2D : Boolean; aMag : TPoint3DFType);
begin
  Self.X := aMag;
  Self.Y := 0;
  Self.Z := 0;
  RotateXY(Random * 2 * pi);
  if not Only2D then
    RotateXZ(Random * 2 * pi);
  // RotateYZ(Random * 2 * pi); Not needed as far I know
end;

{$endif VER3}

procedure TPoint3DF.Init(const aX, aY : TPoint3DFType;
  const aZ : TPoint3DFType);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
end;

procedure TPoint3DF.Clone(const aPoint3DF : TPoint3DF);
begin
  //Self.X := aPoint3DF.X;
  //Self.Y := aPoint3DF.Y;
  //Self.Z := aPoint3DF.Z;
  Self := aPoint3DF;
end;

function TPoint3DF.GetMagnitude : TPoint3DFType;
begin
  Result := Sqrt(Sqr(Self.X) + Sqr(Self.Y) + Sqr(Self.Z));
end;

procedure TPoint3DF.SetMagnitude(const aMag : TPoint3DFType);
begin
  Self.Normalize;
  Self.Scale(aMag);
end;

function TPoint3DF.Distance(const aPoint3DF : TPoint3DF) : TPoint3DFType;
begin
  Result := Sqrt(Sqr(Self.X - aPoint3DF.X) + Sqr(Self.Y - aPoint3DF.Y) +
    Sqr(Self.Z - aPoint3DF.Z));
end;

function TPoint3DF.InSphere(const aCenter3DF : TPoint3DF;
  const aRadius : TPoint3DFType) : Boolean;
begin
  // Actually Sqrt is slow itself, so Distance call maybe doesn't have
  //   much impatc after all
  Result := Self.Distance(aCenter3DF) <= aRadius;
end;

function TPoint3DF.GetAngleXY : TPoint3DFType;
begin
  Result := ArcTan2(Self.Y, Self.X); // Params: y, x
end;

function TPoint3DF.GetAngleXZ : TPoint3DFType;
begin
  Result := ArcTan2(Self.Z, Self.X); // Params: z, x
end;

function TPoint3DF.GetAngleYZ : TPoint3DFType;
begin
  Result := ArcTan2(Self.Z, Self.Y); // Params: z, y
end;

procedure TPoint3DF.RotateXY(const aAngle : TPoint3DFType);
var
  Sinus, Cosinus, TempX : TPoint3DFType;
begin
  SinCos(aAngle, Sinus, Cosinus);

  TempX := Self.X;
  Self.X := Self.X * Cosinus - Self.Y * Sinus; // |cos(θ)  −sin(θ)| |x0|
  Self.Y := TempX * Sinus + Self.Y * Cosinus;  // |sin(θ)   cos(θ)| |y0|
end;

procedure TPoint3DF.RotateXY(const aAngle : TPoint3DFType;
  const aCenter : TPoint3DF);
begin
  Self.Substract(aCenter);
  Self.RotateXY(aAngle);
  Self.Add(aCenter);
end;

procedure TPoint3DF.RotateXZ(const aAngle : TPoint3DFType);
var
  Sinus, Cosinus, TempX : TPoint3DFType;
begin
  SinCos(aAngle, Sinus, Cosinus);

  TempX := Self.X;
  Self.X := Self.X * Cosinus - Self.Z * Sinus;
  Self.Z := TempX * Sinus + Self.Z * Cosinus;
end;

procedure TPoint3DF.RotateXZ(const aAngle : TPoint3DFType;
  const aCenter : TPoint3DF);
begin
  Self.Substract(aCenter);
  Self.RotateXZ(aAngle);
  Self.Add(aCenter);
end;

procedure TPoint3DF.RotateYZ(const aAngle : TPoint3DFType);
var
  Sinus, Cosinus, TempY : TPoint3DFType;
begin
  SinCos(aAngle, Sinus, Cosinus);

  TempY := Self.Y;
  Self.Y := Self.Y * Cosinus - Self.Z * Sinus;
  Self.Z := TempY * Sinus + Self.Z * Cosinus;
end;

procedure TPoint3DF.RotateYZ(const aAngle : TPoint3DFType;
  const aCenter : TPoint3DF);
begin
  Self.Substract(aCenter);
  Self.RotateYZ(aAngle);
  Self.Add(aCenter);
end;

class function TPoint3DF.Zero : TPoint3DF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
end;

function TPoint3DF.IsZero : Boolean;
begin
  Result := (Self.X = 0) and (Self.Y = 0) and (Self.Z = 0);
end;

function TPoint3DF.IsEqual(const aPoint3DF : TPoint3DF) : Boolean;
begin
  Result := (Self.X = aPoint3DF.X) and (Self.Y = aPoint3DF.Y) and
    (Self.Z = aPoint3DF.Z);
end;

class operator TPoint3DF.=(const aPoint3DF1, aPoint3DF2 : TPoint3DF) : Boolean;
begin
  Result := (aPoint3DF1.X = aPoint3DF2.X) and (aPoint3DF1.Y = aPoint3DF2.Y) and
    (aPoint3DF1.Z = aPoint3DF2.Z);
end;

{ NOTE: Automatically created by FPC if = created. }
//class operator TPoint3DF.<>(
//  const aPoint3DF1, aPoint3DF2 : TPoint3DF) : Boolean;
//begin
//  Result := (aPoint3DF1.X <> aPoint3DF2.X) or (aPoint3DF1.Y <> aPoint3DF2.Y) or
//    (aPoint3DF1.Z <> aPoint3DF2.Z);
//end;

procedure TPoint3DF.Add(const aPoint3DF : TPoint3DF);
begin
  Self.X += aPoint3DF.X;
  Self.Y += aPoint3DF.Y;
  Self.Z += aPoint3DF.Z;
end;

procedure TPoint3DF.Move(const dX, dY, dZ : TPoint3DFType);
begin
  Self.X += dX;
  Self.Y += dY;
  Self.Z += dZ;
end;

class operator TPoint3DF.+(const aPoint3DF1, aPoint3DF2 : TPoint3DF)
: TPoint3DF;
begin
  Result.X := aPoint3DF1.X + aPoint3DF2.X;
  Result.Y := aPoint3DF1.Y + aPoint3DF2.Y;
  Result.Z := aPoint3DF1.Z + aPoint3DF2.Z;
end;

procedure TPoint3DF.Substract(const aPoint3DF : TPoint3DF);
begin
  Self.X -= aPoint3DF.X;
  Self.Y -= aPoint3DF.Y;
  Self.Z -= aPoint3DF.Z;
end;

class operator TPoint3DF.-(const aPoint3DF1, aPoint3DF2 : TPoint3DF)
: TPoint3DF;
begin
  Result.X := aPoint3DF1.X - aPoint3DF2.X;
  Result.Y := aPoint3DF1.Y - aPoint3DF2.Y;
  Result.Z := aPoint3DF1.Z - aPoint3DF2.Z;
end;

procedure TPoint3DF.Scale(aScale : TPoint3DFType);
begin
  Self.X *= aScale;
  Self.Y *= aScale;
  Self.Z *= aScale;
end;

class operator TPoint3DF.*(const aPoint3DF : TPoint3DF;
  const aFactor : TPoint3DFType) : TPoint3DF;
begin
  Result.X := aPoint3DF.X * aFactor;
  Result.Y := aPoint3DF.Y * aFactor;
  Result.Z := aPoint3DF.Z * aFactor;
end;

class operator TPoint3DF.*(const aFactor : TPoint3DFType;
  const aPoint3DF : TPoint3DF) : TPoint3DF;
begin
  Result.X := aPoint3DF.X * aFactor;
  Result.Y := aPoint3DF.Y * aFactor;
  Result.Z := aPoint3DF.Z * aFactor;
end;

procedure TPoint3DF.DivScale(const aScale : TPoint3DFType);
begin
  Self.X /= aScale;
  Self.Y /= aScale;
  Self.Z /= aScale;
end;

class operator TPoint3DF./(const aPoint3DF : TPoint3DF;
  const aFactor : TPoint3DFType) : TPoint3DF;
begin
  Result.X := aPoint3DF.X / aFactor;
  Result.Y := aPoint3DF.Y / aFactor;
  Result.Z := aPoint3DF.Z / aFactor;
end;

procedure TPoint3DF.Invert;
begin
  Self.X := -Self.X;
  Self.Y := -Self.Y;
  Self.Z := -Self.Z;
end;

class operator TPoint3DF.-(const aPoint3DF : TPoint3DF) : TPoint3DF;
begin
  Result.X := -aPoint3DF.X;
  Result.Y := -aPoint3DF.Y;
  Result.Z := -aPoint3DF.Z;
end;

procedure TPoint3DF.Normalize;
var
  l : TPoint3DFType;
begin
  l := Self.GetMagnitude;

  // This generate a DivZero exception.
  Self.X /= l;
  Self.Y /= l;
  Self.Z /= l;
end;

class function TPoint3DF.VectProd(
  const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DF;
begin
  Result.X := (aPoint3DF1.Y * aPoint3DF1.Z) - (aPoint3DF2.Y * aPoint3DF1.Z);
  Result.Y := (aPoint3DF1.Z * aPoint3DF1.X) - (aPoint3DF2.Z * aPoint3DF1.X);
  Result.Z := (aPoint3DF1.X * aPoint3DF1.Y) - (aPoint3DF2.X * aPoint3DF1.Y);
end;

function TPoint3DF.ScalProd(const aPoint3DF : TPoint3DF) : TPoint3DFType;
begin
  Result := (Self.X * aPoint3DF.X) + (Self.Y * aPoint3DF.Y) +
    (Self.Z * aPoint3DF.Z);
end;

class operator TPoint3DF.*(
  const aPoint3DF1, aPoint3DF2 : TPoint3DF) : TPoint3DFType;
begin
  Result := (aPoint3DF1.X * aPoint3DF2.X) + (aPoint3DF1.Y * aPoint3DF2.Y) +
    (aPoint3DF1.Z * aPoint3DF2.Z);
end;

function TPoint3DF.FromString(const S : string; const Delim : char) : Boolean;
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
