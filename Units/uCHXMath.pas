unit uCHXMath;
{<
  Unit with some mathematical functions that are not in Math unit.

  @unorderedlist(
    @item(Constants: PI related, Roots, Golden Ratios, Fractal Dimensions,
      Mathematical, Physics, etc.)
    @item(Fastests Factorial and Fibonacci "functions". Actually, they are
      static arrays.)
    @item(Greatest Common Divisor and Least Common Multiples.)
    @item(Interpolation and Cycles.)
    @item(Regular poligon calculus (`NSides >= 3` is not test for inlining.))
    @item(3D: FocalLength )
    @item(Permutations)
  )

  (C) 2024-2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
{$inline ON}

interface

uses Types, Math;

const
{<
  Constants

  Double type stores ~15 decimals, 20 are provided.
  - Actual Pi    = 3.14159 26535 89793 23846 26433 82795 028
  - Pi in Double = 3.14159 26535 89793 11599 79634

  Single type stores about 4 decimals
}

{
  Pi
}

  //kPi = Pi; //< Pi is actually an internal function
  k2Pi = 2 * Pi; //< Proposed as **Tau**
  kHalfPi = Pi * 0.5;
  kQuarterPi = Pi * 0.25;
  k360Degree = Pi / 180;
  k400Degree = Pi / 200; // Yep, this exists

  kRad2Deg = 180 / Pi; // 1 / k360Degree
  kRad2Gra = 200 / Pi; // 1 / k400Degree

{
  Roots and Inverses

  ToDo: ¿Change them to actual operations? kSqRt2 = SqRt(2)
}

  kSqRt2 = 1.41421356237309504880; //< = SqRt(2)
  kSqRt3 = 1.73205080756887729352; //< = SqRt(3)
  kSqRt5 = 2.23606797749978969640; //< = SqRt(5)
  kCbRt2 = 1.25992104989487316476; //< = Power(2, 1/3)
  kCbRt3 = 1.44224957030740838232; //< = Power(3, 1/3)
  k4Rt5  = 1.49534878122122054191; //< = Power(5, 1/4)
  k12Rt2 = 1.05946309435929526456; //< = Power(2, 1/12)

  kInvSqRt2 = 1 / kSqRt2; //< = 1 / SqRt(2)
  kInvSqRt3 = 1 / kSqRt3; //< = 1 / SqRt(3)
  kInvSqRt5 = 1 / kSqRt5; //< = 1 / SqRt(5)
  kInvCbRt2 = 1 / kCbRt2; //< = 1 / Power(2, 1/3)
  kInvCbRt3 = 1 / kCbRt3; //< = 1 / Power(3, 1/3)
  kInv4Rt5  = 1 / k4Rt5;  //< = 1 / Power(5, 1/4)
  kInv12Rt2 = 1 / k12Rt2; //< = 1 / Power(2, 1/12)

{
  Sinus and Cosinus
}
  kSin45 = SqRt(2) * 0.5;
  kSin30 = 0.5;
  kSin15 = (SqRt(6) - SqRt(2)) * 0.25;

{
  Golden / Silver Ratios
}

  kGoldRatio      = 1.61803398874989484820;
  kSuperGoldRatio = 1.46557123187676802665;
  kGoldAngle      = 2.39996322972865332223; //< 137,5077º (Sunflower)
  kGoldSpire      = 1.35845657418299843520;
  kSilverRatio    = 2.41421356237309504880; //< SrRt(2) + 1

{
  Fractal Dimensions
}

  kDimCantor     = 0.63092975357145743709;
  kDimApollonio  = 1.3056867; //< Aprox.
  kDimDragon     = 1.52362708620249210627;
  kDimSierpinski = 1.58496250072115618145;

{
  Other Operations
}

  kLn2      = 0.69314718055994530941; //< = Ln(2)
  kHalfFact = 0.88622692545275801364; //< = (0.5)!
  kHexAreaF = 2.59807621135331594029; //< HexagonArea = kHexAreaF * Side * Side

{
  Physics
}

  kAvogadroN = 6.02214076e23;
  //< mol^-1 (1/mol) = Number of particles in a mol.
  kAvogadroV = 22.4127;
  //< dm^3 * mol^-1 (or l / mol) = Volume 1 mol of gas 0ºC at 1 atm.
  kBoltzmann = 1.380649e-23;
  //< m^2 * kg * s^-2 K^-1 (or J / -K)
  kHP2W = 745.7; //< Horse Power to Watts
  kCV2W = 736; //< Metric Horse Power to Watts

{
  Other constants
}

  kEulerN = 2.71828182845904523536;
  kEulerK = 0.57721566490153286060;
  kGaussK = 0.83462684167407318628;

{
  Constant Arrays for some "functions"
}

  Factorial : array[0..20] of UInt64 =
    (1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800,
    479001600, 6227020800, 87178291200, 1307674368000, 20922789888000,
    355687428096000, 6402373705728000, 121645100408832000,
    2432902008176640000);
  {< Fastest Factorial "function" I know.

    Limits:
    - QWord < 20!
    - LongInt & LongWord < 12!
    - ShortInt & Word < 8!
    - Byte < 5!
  }

  Fibonacci : array[0..46] of UInt32 =
    (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987,
    1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025,
    121393, 196418, 317811, 514229, 832040, 1346269,
    2178309, 3524578, 5702887, 9227465, 14930352, 24157817,
    39088169, 63245986, 102334155, 165580141, 267914296, 433494437,
    701408733, 1836311903, 2971215073);
  {< Fastest Fibonacci "function" I know.

    Limits:
    - ToDo: Extend to  QWord
    - LongInt & LongWord < 46
    - ShortInt & Word < 24
    - Byte < 13
  }

{
  Greatest Common Divisor and Least Common Multiple
}

function GCD(aValue1, aValue2: Int64): Int64;
{< Greatest Common Divisor between two values.
}

function LCM(const aValue1, aValue2: Int64): Int64;
{< Least Common Multiple between two values.
}

{
  Interpolation functions
}

function InterpolateLinear(const aValue, aStart, aEnd: Double): Double; inline;
{< Linear interpolation between 2 values.

  It's posible to pass an @code(aValue) out of range [a,b].

  @param(aValue Normalized interpolation value [0..1]. @br
    0 returns @code(aStart) ; 1 return @code(aEnd).)
  @param(aStart First value of the range.)
  @param(aEnd Last value of the range.)
}

function InterpolateCos(const aValue, aStart, aEnd: Double): Double; inline;
{< Cosine interpolation between 2 values.

  Generates a smother curve than linear (actually "linear" is not a "curve"),
    with egdes in some case.

  @param(aValue Normalized interpolation value [0..1]. @br
    `0` returns `aStart`; `1` return `aEnd`.)
  @param(aStart First value of the range.)
  @param(aEnd Last value of the range.)
}

function InterpolateCubic(const aValue, aPrev, aStart, aEnd,
  aNext: Double) : Double;
{< Cubic interpolation between 2 values.

  Generates a smother curve than Cosine, but it's time consuming and
    requires a previous and later points.

  @param(aValue Normalized interpolation value [0..1]. @br
    `0` returns `aStart`; `1` return `aEnd`.)
  @param(aPrev First value of the previous range.)
  @param(aStart First value of the range.)
  @param(aEnd Last value of the range.)
  @param(aNext Last value of the next range.)
}

{
  Cycle normalization
}

function CycleNorm(const aValue, aStart, aEnd: Real): Real; overload;
{< Normalize a float value in a cyclic range: Angles, Hours, &c.

  `aValue` is normalized in range [`aStart`, `aEnd`). `aEnd` is NOT included,
    i.e. 360º becomes 0º in a circle

  @param(aValue Value to normalize.)
  @param(aStart Start value of the range.)
  @param(aEnd End value of the range. It's NOT included.)
  @result Normalized value.
}

function CycleNorm(const aValue: Integer;  aStart, aEnd: Integer): Integer;
{< Normalize a float value in a cyclic range: Angles, Hours, &c.

  `aValue` is normalized in range [`aStart`, `aEnd`]. `aEnd` IS included.

  @param(aValue Value to normalize.)
  @param(aStart Start value of the range.)
  @param(aEnd End value of the range. It IS included.)
  @result Normalized value.
}

{
  Regular Poligons calculus.

  Function sufix tells what parameters expects:
  - ~SL: Side Lenght.
  - ~CC: Circumscribed Circle Radius.

  Number of sides is not tested for inlining (FPC don't inline if the function
    has an if or a loop). Some functions can return weird results or
    a system "division by cero" exception.
}

function RPApothemSL(const NSides: Integer; const SideL: Double): Double;
  inline;
{< Apothem: Radius of the Inscribed Circle; in other words, distance from
    center of polygon to the middle point of a side.

  @param NSides Number of sides.
  @param SideL Lenght of the side.
  @result Apothem of the polygon.
}
function RPApothemCC(const NSides: Integer; const CCRad: Double): Double;
  inline;
{< Apothem: Radius of the Inscribed Circle; in other words, distance from
    center of polygon to the middle point of a side.

  @param NSides Number of sides.
  @param CCRad Circumscribed Circle Radius.
  @result Apothem of the polygon.
}

function RPSagittaSL(const NSides: Integer; const SideL: Double): Double;
{< Sagitta: Difference between Circumscribed and Inscribed circles.

  @param NSides Number of sides.
  @param SideL Lenght of the side.
}
function RPSagittaCC(const NSides: Integer; const CCRad: Double): Double;
{< Sagitta: Difference between Circumscribed and Inscribed circles.

  @param NSides Number of sides.
  @param CCRad Circumscribed Circle Radius.
}

function RPIntAngle(const NSides: Integer): Double; inline;
{< Internal Angle of the regular polygon.

  @param NSides Number of sides.
}
function RPIntAngleSum(const NSides: Integer): Double; inline;
{< Sum of all Internal Angles of the regular polygon.

  @param NSides Number of sides.
}

function RPExtAngle(const NSides: Integer): Double; inline;
{< External Angle.

  @param NSides Number of sides.
}
function RPExtAngleSum: Double; inline;
{< External Angle Sum, actually it is always `2 * Pi`.
}

function RPAreaSL(const NSides: Integer; const SideL: Double): Double; inline;
{< Area of regular de polygon.

  @param NSides Number of sides.
  @param SideL Lenght of the side.
}
function RPAreaCC(const NSides: Integer; const CCRad: Double): Double; inline;
{< Area of regular de polygon.

  @param NSides Number of sides.
  @param CCRad Circumscribed Circle Radius.
}

function RPNDiagonals(const NSides: Integer): Integer; inline;
{< Number of diagonal in a regular polygon, actually any convex polygon.

  @param NSides Number of sides.
}

function RPCirCircleSL(const NSides: Integer; const SideL: Double): Double;
  inline;
{< Circumscribed Circle Radius with known Side Lenght of the regular polygon.

  @param CCRad Circumscribed Circle Radius.
}
function RPSideLCC(const NSides: Integer; const CCRad: Double): Double;
  inline;
{< Side Lenght with known Circumscribed Circle Radius of the regular polygon.

  @param CCRad Circumscribed Circle Radius.
}

{
  3D
}

function FocalLengthV(const Height: Double): Double; inline;
{< Shortcut for a "good looking" Focal Length for 3D in any screen.

  Using a vertical FOV of 50 - 60, adapts automatically a good FOV any
  aspect ratio screen.

  @param(Height Height of the viewport.)
  @param(Degrees Degrees of Field Of View. Range: (0º..360º).)
}

function FocalLengthDeg(const Size, Degrees: Double): Double; inline;
{< Focal Length for 3D projections.


  @param(Size Width (or Height for vertical Focal Length) of the viewport.)
  @param(Degrees Degrees of Field Of View. Range: (0º..360º).)
}
function FocalLengthRad(const Size, Radians: Double): Double; inline;
{< Focal Length for 3D projections.

  @param(Size Width (or Height for vertical Focal Length) of the viewport.)
  @param(Radians of Field Of View. Range: (0..2Pi).)
}

{
  Permutations
}
procedure SwapArrayItems(var OrderList : TIntegerDynArray; const i, j: Integer);
  inline;
{< Auxiliar procedure for swapping two items in OrderList.

  @warning(No range check is performed for inlining.)

  @param(OrderList Array to iterate next permutation and the permutation
    returned.)
  @param(i,j Item indices to swap.)
}

procedure PermutateRandSwap(var OrderList : TIntegerDynArray; NSwaps: Integer = 1);
{< Generate a new permutation of OrderList, swaping random items.

  @param(OrderList Array to iterate next permutation and the permutation
    returned.)
  @param(NSwaps Number of pair swaps to do.)
}

procedure PermutateShuffle(var OrderList : TIntegerDynArray);

function PermutateLexOrd(var OrderList : TIntegerDynArray;
  const CyclicEq : Boolean = False): Boolean;
{< Get the next Lexical Order Permutation of OrderList.

  Uses Narayana Pandita's Lexical Order Algorithm.

  Returns @true, when it's called with the last permutation, not when the
  last permutation is generated. To be used with:

  '''
  repeat
    [Process Permutation]
  until PermutateLexOrd(...);
  '''

  @note(Using `while` may require proccess initial permutation before
    entering the loop:

    '''
    [Process Permutation]
    while not PermutateLexOrd(...) do
    begin
      [Process Permutation]
    end;
    '''

    This is why Boolean Result is set for using with an `until` loop in mind.
  )

  When finished, returns last permutation again; it doesn't revert OrderList
  to initial permutation.

  @warning(
    Any loop with an OrderList with more than 6~7 items will take
    very long time. It's recommended to add any other means to break the
    loop.
  )

  @param(OrderList
    Array to iterate next permutation and the permutation returned.
  )
  @param(CyclicEq
    If @true, it doesn't generate Cyclic order permutations
    ([1, 2, 3] is equivalent to [2, 3, 1]). First item will be the same
    always.
  )

  @returns(@true if it's finished, @false if there a more permutations.)
  @returns(OrderList with the next permutation, or last one if finished.)
}

implementation

function GCD(aValue1, aValue2: Int64): Int64;
var
  Temp : Int64;
begin
  // ToDo: Raise Exception?
  if (aValue1 = 0) or (aValue2 = 0) then Exit(0);
  while aValue2 <> 0 do
  begin
    Temp := aValue2;
    aValue2 := aValue1 mod aValue2;
    aValue1 := Temp;
  end;
  Result := aValue1; // Return absolute value?
end;

function LCM(const aValue1, aValue2: Int64): Int64;
begin
  // ToDo: Raise Exception?
  if (aValue1 = 0) or (aValue2 = 0) then Exit(0);
  // ToDo: Return absolute value?
  Result := aValue2 * (aValue1 div GCD(aValue1, aValue2));
end;

function InterpolateLinear(const aValue, aStart, aEnd: Double) : Double;
begin
  Result := aStart + aValue * (aEnd - aStart);
end;

function InterpolateCos(const aValue, aStart, aEnd: Double) : Double;
begin
  Result := aStart + ((1.0 - cos(aValue * Pi)) * 0.5) * (aEnd - aStart);
end;

function InterpolateCubic(const aValue, aPrev, aStart, aEnd,
  aNext: Double) : Double;
var
  P, Q, R, S : Double;
begin
  P := (aNext - aEnd) - (aPrev - aStart);
  Q := (aPrev - aStart) - P;
  R := aEnd - aPrev;
  S := aStart;

  Result := P * aValue * aValue * aValue + Q * aValue * aValue +
    R * aValue + S;
end;

function CycleNorm(const aValue, aStart, aEnd: Real): Real; overload;
var
  Range, Offset: Real;
begin
  if (aValue >= aStart) and (aValue < aEnd) then
    Exit(aValue);
  Range := aEnd - aStart;
  if IsZero(Range) then
    Exit(aStart);
  Offset := aValue - aStart;
  Result := Offset - (Floor(Offset / Range) * Range) + aStart;
end;

function CycleNorm(const aValue: Integer;  aStart, aEnd: Integer): Integer;
var
  Range, Offset: Integer;
begin
  if aStart = aEnd then Exit(aStart);
  if aStart > aEnd then
  begin
    Range := aStart;
    aStart := aEnd;
    aEnd := Range;
  end;
  if (aValue >= aStart) and (aValue <= aEnd) then Exit(aValue);

  Range := aEnd - aStart + 1;
  Offset := aValue - aStart;

  Result := ((Offset mod Range + Range) mod Range) + aStart;
end;

function RPApothemSL(const NSides: Integer; const SideL: Double): Double;
begin
  Result := 0.5 * SideL * Cotan(Pi / NSides);
end;

function RPApothemCC(const NSides: Integer; const CCRad: Double): Double;
begin
  Result := CCRad * cos(Pi / NSides);
end;

function RPSagittaSL(const NSides: Integer; const SideL: Double): Double;
var
  aSin: Double;
begin
  aSin := sin(kHalfPi / NSides);
  Result := SideL * Cosecant(Pi / NSides) * aSin * aSin;
end;

function RPSagittaCC(const NSides: Integer; const CCRad: Double): Double;
var
  aSin: Double;
begin
  aSin := sin(kHalfPi / NSides);
  Result := 2.0 * CCRad * aSin * aSin;
end;

function RPIntAngle(const NSides: Integer): Double;
begin
  Result := Pi * (NSides - 2) / NSides;
end;

function RPIntAngleSum(const NSides: Integer): Double;
begin
  Result := Pi * (NSides - 2);
end;

function RPExtAngle(const NSides: Integer): Double;
begin
  Result := k2Pi / NSides;
end;

function RPExtAngleSum: Double;
begin
  Result := k2Pi;
end;

function RPAreaSL(const NSides: Integer; const SideL: Double): Double;
begin
  Result := 0.25 * NSides * SideL * SideL * Cotan(Pi / NSides);
end;

function RPAreaCC(const NSides: Integer; const CCRad: Double): Double;
begin
  Result := 0.5 * CCRad * CCRad * sin(k2Pi / NSides);
end;

function RPNDiagonals(const NSides: Integer): Integer;
begin
  Result := (NSides * (NSides - 3)) div 2;
end;

function RPCirCircleSL(const NSides: Integer; const SideL: Double): Double;
begin
  Result := SideL * 0.5 * Cosecant(Pi / NSides);
end;

function RPSideLCC(const NSides: Integer; const CCRad: Double): Double;
begin
  Result := 2.0 * CCRad * sin(Pi / NSides);
end;

{
  3D
}

function FocalLengthV(const Height: Double): Double; inline;
begin
  Result := Height * 0.866025403; // 60º Vertical FOV
end;

function FocalLengthDeg(const Size, Degrees: Double): Double;
begin
  Result := FocalLengthRad(Size, Degrees * k360Degree);
end;

function FocalLengthRad(const Size, Radians: Double): Double;
begin
  Result := Size * 0.5 / Tan(Radians * 0.5);
end;

{
  Permutations
}

procedure SwapArrayItems(var OrderList : TIntegerDynArray; const i, j: Integer);
var
  Temp: Integer;
begin
  Temp := OrderList[i];
  OrderList[i] := OrderList[j];
  OrderList[j] := Temp;
end;

procedure PermutateRandSwap(var OrderList : TIntegerDynArray; NSwaps: Integer);
begin
  while NSwaps > 0 do
  begin
    SwapArrayItems(OrderList, RandomRange(0, Length(OrderList)),
      RandomRange(0, Length(OrderList)));
    Dec(NSwaps);
  end;
end;

procedure PermutateShuffle(var OrderList : TIntegerDynArray);
var
  TempArray : TIntegerDynArray;
  i, j : Integer;
begin
  TempArray := Copy(OrderList, 0, Length(OrderList));

  i := 0;
  while Length(TempArray) > 0 do
  begin
    j := RandomRange(0, Length(TempArray));
    OrderList[i] := TempArray[j];
    Delete(TempArray, j, 1);
    Inc(i);
  end;
end;

function PermutateLexOrd(var OrderList : TIntegerDynArray;
  const CyclicEq : Boolean): Boolean;
var
  i, j : integer;
begin
  i := High(OrderList) - 1;
  if CyclicEq then j := 1 // j := IfThen(CyclicEq, 1, 0);
    else j := 0;

  while (i >= j) and (OrderList[i] >= OrderList[i + 1]) do
    Dec(i);

  Dec(j);
  if i = j then
    Exit(True);

  Result := False;

  j := High(OrderList);
  while (OrderList[i] >= OrderList[j]) do // OrderList[i + 1] at least
    Dec(j);

  SwapArrayItems(OrderList, i, j);

  Inc(i);
  j := High(OrderList);
  while i < j do
  begin
    SwapArrayItems(OrderList, i, j);
    Inc(i); Dec(j);
  end;
end;

end.
{<This source is free software; you can redistribute it and/or modify it under
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
