unit uCHXComplexNumber;
{< Unit of TPoint3DF record.

  Copyright (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$inline ON}

// ToDo: Make inline functions.

interface

uses
  Classes, SysUtils, Math;

type
  { TComplexNumber }
  TComplexNumberType = double; // Single, Double or Extended;

  TComplexNumber = record
  public
    R : TComplexNumberType;
    i : TComplexNumberType;

    {$ifdef VER3}
      constructor Create(const aR : TComplexNumberType; const aI : TComplexNumberType = 0); overload;
      {< Creates a TComplexNumber. }
      constructor Create(const aCN : TComplexNumber); overload;
      {< Creates a TComplexNumber cloning another TComplexNumber. }
    {$endif}

    procedure Init(const aR, aI : TComplexNumberType);
    procedure Clone(const aCN : TComplexNumber);
    {< Copy TComplexNumber data. }
    // Same type records can be assigned, no?
    // class operator := (aR : TComplexNumber) : TComplexNumber; inline;
    class operator := (const aR : TComplexNumberType) : TComplexNumber;
    {< Assigns floating number to a complex. }

    function Modulus : TComplexNumberType;
    function Argument : TComplexNumberType;

    procedure Add(const aCN : TComplexNumber); overload;
    class operator +(const aCN1, aCN2 : TComplexNumber) : TComplexNumber;
    procedure Add(const aR : TComplexNumberType); overload;
    class operator +(const aCN : TComplexNumber;
      const aR : TComplexNumberType) : TComplexNumber;
    class operator +(const aR : TComplexNumberType;
      const aCN : TComplexNumber) : TComplexNumber;

    procedure Substract(const aCN : TComplexNumber); overload;
    class operator -(const aCN1, aCN2 : TComplexNumber) : TComplexNumber;
    procedure Substract(const aR : TComplexNumberType); overload;
    class operator -(const aCN : TComplexNumber;
      const aR : TComplexNumberType) : TComplexNumber;
    class operator -(const aR : TComplexNumberType;
      const aCN : TComplexNumber) : TComplexNumber;

    procedure Multiply(const aCN : TComplexNumber); overload;
    class operator *(const aCN1, aCN2 : TComplexNumber) : TComplexNumber;
    procedure Multiply(const aR : TComplexNumberType); overload;
    class operator *(const aCN : TComplexNumber;
      const aR : TComplexNumberType) : TComplexNumber;
    class operator *(const aR : TComplexNumberType;
      const aCN : TComplexNumber) : TComplexNumber;

    procedure Divide(const aCNDen : TComplexNumber); overload;
    class operator /(const aCNNum, aCNDen : TComplexNumber) : TComplexNumber;
    procedure Divide(const aRDenom : TComplexNumberType); overload;
    class operator /(const aCN : TComplexNumber;
      const aRDenom : TComplexNumberType) : TComplexNumber;
    class operator /(const aRNum : TComplexNumberType;
      const aCNDenom : TComplexNumber) : TComplexNumber;

    procedure Power(const aCN : TComplexNumber); overload;
    class operator **(const aCN1, aCN2 : TComplexNumber) : TComplexNumber;
    procedure Power(const aR : TComplexNumberType); overload;
    class operator **(const aCN : TComplexNumber;
      const aR : TComplexNumberType) : TComplexNumber;
    class operator **(const aR : TComplexNumberType;
      const aCN : TComplexNumber) : TComplexNumber;

    procedure Negate;
    class operator -(const aCN : TComplexNumber) : TComplexNumber;

    function IsEqual(const aCN : TComplexNumber) : boolean; overload;
    { NOTE: Not needed. }
    //class operator =(const aCN1, aCN2 : TComplexNumber) : boolean;
    { NOTE: Automatically created by FPC if = created and not needed. }
    //class operator <>(const aCN1, aCN2 : TComplexNumber) : boolean;
    function IsEqual(const aR : TComplexNumberType) : boolean; overload;
    class operator =(const aCN : TComplexNumber;
      const aR : TComplexNumberType) : boolean;
    class operator =(const aR : TComplexNumberType;
      const aCN : TComplexNumber) : boolean;
    { NOTE: Automatically created by FPC if = created. }
    //class operator <>(const aCN : TComplexNumber;
    //  const aR : TComplexNumberType) : boolean;
    //class operator <>(const aR : TComplexNumberType;
    //  const aCN : TComplexNumber) : boolean;

    class function Conjugate(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Conjugate;overload;
    {< a + bi -> a - bi }

      class function Inverse(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Inverse;overload;
    {< a + bi -> 1 / (a + bi) }

    class function Exp(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Exp; overload;
    {< Exponential. }

    class function Ln(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Ln; overload;
    {< Natural logarithm. }

    class function Sqr(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Sqr; overload;
    {< Square, Power of 2. }

    class function Sqrt(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Sqrt; overload;
    {< Square root. }

    class function Cos(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Cos; overload;
    {< Cosine. }

    class function Sin(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Sin; overload;
    {< Sine. }

    class function Tan(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure Tan; overload;
    {< Tangent. }

    class function ArcCos(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure ArcCos; overload;
    {< ArcCosine. }

    class function ArcSin(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure ArcSin; overload;
    {< ArcSine. }

    class function ArcTan(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure ArcTan; overload;
    {< ArcTangent. }

    class function CosH(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure CosH; overload;
    {< Hyperbolic Cosine. }

    class function SinH(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure SinH; overload;
    {< Hyperbolic Sine. }

    class function TanH(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure TanH; overload;
    {< Hyperbolic Tangent. }

    class function ArcCosH(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure ArcCosH; overload;
    {< Hyperbolic ArcCosine. }

    class function ArcSinH(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure ArcSinH; overload;
    {< Hyperbolic ArcSine. }

    class function ArcTanH(const aCN : TComplexNumber) : TComplexNumber;
      overload; static;
  procedure ArcTanH; overload;
    {< Hyperbolic ArcTangent. }

    function ToString : string; overload;
    function ToString(const Prec, Dec : integer) : string; overload;
    {< Write in a string. }
  end;

  PComplexNumber = ^TComplexNumber;

function ComplexNumber(const aR : TComplexNumberType;
  const aI : TComplexNumberType = 0) : TComplexNumber;
{< Creates a new TComplexNumber. }

implementation

function ComplexNumber(const aR : TComplexNumberType;
  const aI : TComplexNumberType) : TComplexNumber;
begin
  Result.Init(aR, aI);
end;

{ TComplexNumber }

constructor TComplexNumber.Create(const aR : TComplexNumberType;
  const aI : TComplexNumberType);
begin
  Self.Init(aR, aI);
end;

constructor TComplexNumber.Create(const aCN : TComplexNumber);
begin
  //Self.R := aCN.R;
  //Self.i := aCN.i;
  Self := aCN;
end;

procedure TComplexNumber.Init(const aR, aI : TComplexNumberType);
begin
  Self.R := aR;
  Self.i := aI;
end;

procedure TComplexNumber.Clone(const aCN : TComplexNumber);
begin
  //Self.R := aCN.R;
  //Self.i := aCN.i;
  Self := aCN;
end;

class operator TComplexNumber.:=(
  const aR : TComplexNumberType) : TComplexNumber;
begin
  Result.R := aR;
  Result.i := 0;
end;

function TComplexNumber.Modulus : TComplexNumberType;
begin
  Result := Hypot(Self.R, Self.i);
end;

function TComplexNumber.Argument : TComplexNumberType;
begin
  Result := ArcTan2(Self.i, Self.R); // Params: y, x
end;

procedure TComplexNumber.Add(const aCN : TComplexNumber);
begin
  Self.R += aCN.R;
  Self.i += aCN.i;
end;

class operator TComplexNumber.+(const aCN1, aCN2 : TComplexNumber)
: TComplexNumber;
begin
  Result.R := aCN1.R + aCN2.R;
  Result.i := aCN1.i + aCN2.i;
end;

procedure TComplexNumber.Add(const aR : TComplexNumberType);
begin
  Self.R += aR;
  //Self.i += 0;
end;

class operator TComplexNumber.+(const aCN : TComplexNumber;
  const aR : TComplexNumberType) : TComplexNumber;
begin
  Result.R := aCN.R + aR;
  Result.i := aCN.i;
end;

class operator TComplexNumber.+(const aR : TComplexNumberType;
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := aCN.R + aR;
  Result.i := aCN.i;
end;

procedure TComplexNumber.Substract(const aCN : TComplexNumber);
begin
  Self.R -= aCN.R;
  Self.i -= aCN.i;
end;

class operator TComplexNumber.-(
  const aCN1, aCN2 : TComplexNumber) : TComplexNumber;
begin
  Result.R := aCN1.R - aCN2.R;
  Result.i := aCN1.i - aCN2.i;
end;

procedure TComplexNumber.Substract(const aR : TComplexNumberType);
begin
  Self.R := Self.R - aR;
  //Self.i := Self.i;
end;

class operator TComplexNumber.-(const aCN : TComplexNumber;
  const aR : TComplexNumberType) : TComplexNumber;
begin
  Result.R := aCN.R - aR;
  Result.i := aCN.i;
end;

class operator TComplexNumber.-(const aR : TComplexNumberType;
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := aR - aCN.R;
  Result.i := -aCN.i;
end;

procedure TComplexNumber.Multiply(const aCN : TComplexNumber);
var
  aR : TComplexNumberType;
begin
  aR := Self.R; // Self.R is modified before Self.i is set
  Self.R := (Self.R * aCN.R) - (Self.i * aCN.i);
  Self.i := (aR * aCN.i) + (Self.i * aCN.R);
end;

class operator TComplexNumber.*(const aCN1, aCN2 : TComplexNumber)
: TComplexNumber;
begin
  Result.R := (aCN1.R * aCN2.R) - (aCN1.i * aCN2.i);
  Result.i := (aCN1.R * aCN2.i) + (aCN1.i * aCN2.R);
end;

procedure TComplexNumber.Multiply(const aR : TComplexNumberType);
begin
  Self.R *= aR;
  Self.i *= aR;
end;

class operator TComplexNumber.*(const aCN : TComplexNumber;
  const aR : TComplexNumberType) : TComplexNumber;
begin
  Result.R := aCN.R * aR;
  Result.i := aCN.i * aR;
end;

class operator TComplexNumber.*(const aR : TComplexNumberType;
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := aCN.R * aR;
  Result.i := aCN.i * aR;
end;

procedure TComplexNumber.Divide(const aCNDen : TComplexNumber);
var
  aR, Ratio, Denom : TComplexNumberType;
begin
  aR := Self.R; // Self.R is modified before Self.i is set
  if Abs(aCNDen.R) > Abs(aCNDen.i) then
  begin
    Ratio := aCNDen.i / aCNDen.R;
    Denom := aCNDen.R + aCNDen.i * Ratio;
    Self.R := (Self.R + Self.i * Ratio) / Denom;
    Self.i := (Self.i - aR * Ratio) / Denom;
  end
  else
  begin
    Ratio := aCNDen.R / aCNDen.i;
    Denom := aCNDen.R * Ratio + aCNDen.i;
    Self.R := (Self.R * Ratio + Self.i) / Denom;
    Self.i := (Self.i * Ratio - aR) / Denom;
  end;
end;

class operator TComplexNumber./(
  const aCNNum, aCNDen : TComplexNumber) : TComplexNumber;
var
  Ratio, Denom : TComplexNumberType;
begin
  if Abs(aCNDen.R) > Abs(aCNDen.i) then
  begin
    Ratio := aCNDen.i / aCNDen.R;
    Denom := aCNDen.R + aCNDen.i * Ratio;
    Result.R := (aCNNum.R + aCNNum.I * Ratio) / Denom;
    Result.i := (aCNNum.I - aCNNum.R * Ratio) / Denom;
  end
  else
  begin
    Ratio := aCNDen.R / aCNDen.i;
    Denom := aCNDen.R * Ratio + aCNDen.i;
    Result.R := (aCNNum.R * Ratio + aCNNum.I) / Denom;
    Result.i := (aCNNum.I * Ratio - aCNNum.R) / Denom;
  end;
end;

procedure TComplexNumber.Divide(const aRDenom : TComplexNumberType);
begin
  Self.R /= aRDenom;
  Self.i /= aRDenom;
end;

class operator TComplexNumber./(const aCN : TComplexNumber;
  const aRDenom : TComplexNumberType) : TComplexNumber;
begin
  Result.R := aCN.R / aRDenom;
  Result.i := aCN.i / aRDenom;
end;

class operator TComplexNumber./(const aRNum : TComplexNumberType;
  const aCNDenom : TComplexNumber) : TComplexNumber;
var
  Denom : TComplexNumberType;
begin
  Denom := IntPower(aCNDenom.R, 2) + IntPower(aCNDenom.i, 2);
  Result.R := (aRNum * aCNDenom.R) / Denom;
  Result.i := -(aRNum * aCNDenom.i) / Denom;
end;

procedure TComplexNumber.Power(const aCN : TComplexNumber);
begin
  Self := Exp(aCN * Ln(Self));
end;

class operator TComplexNumber.**(
  const aCN1, aCN2 : TComplexNumber) : TComplexNumber;
begin
  Result := Exp(aCN2 * Ln(aCN1));
end;

procedure TComplexNumber.Power(const aR : TComplexNumberType);
begin
  Self := Exp(aR * Ln(Self));
end;

class operator TComplexNumber.**(const aCN : TComplexNumber;
  const aR : TComplexNumberType) : TComplexNumber;
begin
  Result := Exp(aR * Ln(aCN));
end;

class operator TComplexNumber.**(const aR : TComplexNumberType;
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := Exp(aCN * System.Ln(aR));
end;

procedure TComplexNumber.Negate;
begin
  Self.R := -Self.R;
  Self.i := -Self.i;
end;

class operator TComplexNumber.-(const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := -aCN.R;
  Result.i := -aCN.i;
end;

function TComplexNumber.IsEqual(const aCN : TComplexNumber) : boolean;
begin
  Result := Self = aCN;
end;

{ NOTE: Not needed. }
//class operator TComplexNumber.=(const aCN1, aCN2 : TComplexNumber) : boolean;
//begin
//  Result := (aCN1.R = aCN2.R) and (aCN1.i = aCN2.i);
//end;

{ NOTE: Automatically created by FPC and not needed. }
//class operator TComplexNumber.<>(const aCN1, aCN2 : TComplexNumber) : boolean;
//begin
//  Result := (aCN1.R <> aCN2.R) or (aCN1.i <> aCN2.i);
//end;

function TComplexNumber.IsEqual(const aR : TComplexNumberType) : boolean;
begin
  Result := (Self.i = 0) and (Self.R = aR);
end;

class operator TComplexNumber.=(const aCN : TComplexNumber;
  const aR : TComplexNumberType) : boolean;
begin
  Result := (aCN.i = 0) and (aCN.R = aR);
end;

class operator TComplexNumber.=(const aR : TComplexNumberType;
  const aCN : TComplexNumber) : boolean;
begin
  Result := (aCN.i = 0) and (aCN.R = aR);
end;

{ NOTE: Automatically created by FPC if = created. }
//class operator TComplexNumber.<>(const aCN : TComplexNumber;
//  const aR : TComplexNumberType) : boolean;
//begin
//  Result := (aCN.i <> 0) or (aCN.R <> aR);
//end;

//class operator TComplexNumber.<>(const aR : TComplexNumberType;
//  const aCN : TComplexNumber) : boolean;
//begin
//  Result := (aCN.i <> 0) or (aCN.R <> aR);
//end;

class function TComplexNumber.Conjugate(const aCN : TComplexNumber
  ) : TComplexNumber;
begin
  Result.R := aCN.R;
  Result.i := -aCN.i;
end;

procedure TComplexNumber.Conjugate;
begin
  Self := TComplexNumber.Conjugate(Self);
end;

class function TComplexNumber.Inverse(const aCN : TComplexNumber
  ) : TComplexNumber;
begin
  Result.R := -aCN.R;
  Result.i := -aCN.i;
end;

procedure TComplexNumber.Inverse;
begin
  Self := TComplexNumber.Inverse(Self);
end;

class function TComplexNumber.Exp(const aCN : TComplexNumber) : TComplexNumber;
var
  ExpM : TComplexNumberType;
begin
  ExpM := System.Exp(aCN.R);
  Result.R := ExpM * System.Cos(aCN.i);
  Result.i := ExpM * System.Sin(aCN.i);
end;

procedure TComplexNumber.Exp;
begin
  Self := TComplexNumber.Exp(Self);
end;

class function TComplexNumber.Ln(const aCN : TComplexNumber) : TComplexNumber;
begin
  // Call explicit System.Ln
  Result.R := System.Ln(aCN.Modulus);
  Result.i := ArcTan2(aCN.i, aCN.R); // Params: y, x
end;

procedure TComplexNumber.Ln;
begin
  Self := TComplexNumber.Ln(Self);
end;

class function TComplexNumber.Sqr(const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := IntPower(aCN.R, 2) - IntPower(aCN.i, 2);
  Result.i := 2 * aCN.R * aCN.i;
end;

procedure TComplexNumber.Sqr;
begin
  Self := TComplexNumber.Sqr(Self);
end;

class function TComplexNumber.Sqrt(
  const aCN : TComplexNumber) : TComplexNumber;
var
  Root, aR : TComplexNumberType;
begin
  if (aCN.R <> 0.0) or (aCN.i <> 0.0) then
  begin
    Root := System.Sqrt(0.5 * (Abs(aCN.R) + aCN.Modulus));
    aR := aCN.i / (2.0 * Root);

    if aCN.R >= 0.0 then
    begin
      Result.R := Root;
      Result.i := aR;
    end
    else if aCN.i < 0.0 then
    begin
      Result.R := -aR;
      Result.i := -Root;
    end
    else
    begin
      Result.R := aR;
      Result.i := Root;
    end;
  end
  else
    Result := aCN;
end;

procedure TComplexNumber.Sqrt;
begin
  Self := TComplexNumber.Sqrt(Self);
end;

class function TComplexNumber.Cos(const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := System.Cos(aCN.R) * Math.CosH(aCN.i);
  Result.i := -System.Sin(aCN.R) * Math.SinH(aCN.i);
end;

procedure TComplexNumber.Cos;
begin
  Self := TComplexNumber.Cos(Self);
end;

class function TComplexNumber.Sin(const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := System.Sin(aCN.R) * Math.CosH(aCN.i);
  Result.i := System.Cos(aCN.R) * Math.SinH(aCN.i);
end;

procedure TComplexNumber.Sin;
begin
  Self := TComplexNumber.Sin(Self);
end;

class function TComplexNumber.Tan(const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := Sin(aCN) / Cos(aCN);
end;  

procedure TComplexNumber.Tan;
begin
  Self := TComplexNumber.Tan(Self);
end;

class function TComplexNumber.ArcCos(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := ComplexNumber(0, -1) * ArcCosH(aCN);
end;   

procedure TComplexNumber.ArcCos;
begin
  Self := TComplexNumber.ArcCos(Self);
end;

class function TComplexNumber.ArcSin(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := ComplexNumber(0, -1) * ArcSinH(ComplexNumber(0, 1) * aCN);
end;       

procedure TComplexNumber.ArcSin;
begin
  Self := TComplexNumber.ArcSin(Self);
end;

class function TComplexNumber.ArcTan(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := ComplexNumber(0, -1) * ArcTanH(ComplexNumber(0, 1) * aCN);
end;      

procedure TComplexNumber.ArcTan;
begin
  Self := TComplexNumber.ArcTan(Self);
end;

class function TComplexNumber.CosH(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result.R := Math.CosH(aCN.R) * System.Cos(aCN.i);
  Result.i := Math.SinH(aCN.R) * System.Sin(aCN.i);
end;

procedure TComplexNumber.CosH;
begin
  Self := TComplexNumber.CosH(Self);
end;

class function TComplexNumber.SinH(
  const aCN : TComplexNumber) : TComplexNumber;
begin
   Result.Clone(aCN);
  Result.R := Math.SinH(aCN.R) * System.Cos(aCN.i);
  Result.i := Math.CosH(aCN.R) * System.Sin(aCN.i);
end;   

procedure TComplexNumber.SinH;
begin
  Self := TComplexNumber.SinH(Self);
end;

class function TComplexNumber.TanH(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := SinH(aCN) / CosH(aCN);
end;  

procedure TComplexNumber.TanH;
begin
  Self := TComplexNumber.TanH(Self);
end;

class function TComplexNumber.ArcCosH(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := -Ln(aCN + ComplexNumber(0, 1) * Sqrt(1.0 - Sqr(aCN)));
end;     

procedure TComplexNumber.ArcCosH;
begin
  Self := TComplexNumber.ArcCosH(Self);
end;

class function TComplexNumber.ArcSinH(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result:= Ln(aCN + Sqrt(1.0 + Sqr(aCN)));
end;   

procedure TComplexNumber.ArcSinH;
begin
  Self := TComplexNumber.ArcSinH(Self);
end;

class function TComplexNumber.ArcTanH(
  const aCN : TComplexNumber) : TComplexNumber;
begin
  Result := Ln((aCN + 1.0) / (1.0 - aCN)) / 2.0;
end;     

procedure TComplexNumber.ArcTanH;
begin
  Self := TComplexNumber.ArcTanH(Self);
end;

function TComplexNumber.ToString : string;
begin
  Result := '(' + Self.R.ToString + ' + ' + Self.i.ToString + 'i)';
end;

function TComplexNumber.ToString(const Prec, Dec : integer) : string;
begin
  Result := '(' + Self.R.ToString(ffGeneral, Prec, Dec) + ' + ' +
    Self.i.ToString(ffGeneral, Prec, Dec) + 'i)';
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
