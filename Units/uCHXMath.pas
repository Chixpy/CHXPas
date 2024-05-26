unit uCHXMath;
{< Unit with some mathematical functions that are noy in Math unit.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  // Fastest factorial function I know.
  // Limits: QWord < 20!; LongWord
  // LongInt and LongWord only 12!
  Factorial : array[0..20] of UInt64 =
    (1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800,
    479001600, 6227020800, 87178291200, 1307674368000, 20922789888000,
    355687428096000, 6402373705728000, 121645100408832000,
    2432902008176640000);

  // Same with Fibonacci :-P
  Fibonacci : array[0..46] of UInt32 =
    (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987,
    1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025,
    121393, 196418, 317811, 514229, 832040, 1346269,
    2178309, 3524578, 5702887, 9227465, 14930352, 24157817,
    39088169, 63245986, 102334155, 165580141, 267914296, 433494437,
    701408733, 1836311903, 2971215073);

function GCD(a, b : Int64) : Int64;
{< Greatest common divisor }
function LCM(a, b : Int64) : Int64;
{< Least Common Multiple}

function Linear_Interpolate(const a, b, x : Double) : Double;
{< Linear interpolation between 2 values.

   @param(a First point.)
   @param(b Second point.)
   @param(x Interpolation value [0..1]. 0 returns `a`; 1 return `b`.)

   Is posible to pass an `x` out of range, but it is not the place to
     explain it. XD
}
function Cosine_Interpolate(const a, b, x : Double) : Double;
{< Cosine interpolation between 2 values. Same as before.

   Generates a smother curve than linear (actually "linear" is not a "curve"),
     with egdes in some case.

   @param(a First point.)
   @param(b Second point.)
   @param(x Interpolation value [0..1]. 0 returns `a`; 1 return `b`.)
}
function Cubic_Interpolate(const v0, v1, v2, v3, x : Double) : Double;
{< Cubic interpolation between 2 values, but requires 4 of them.

   Generates a smother curve than Cosine, but time consuming.

   @param(v0 Point before first point.)
   @param(v1 First point.)
   @param(v2 Second point.)
   @param(v3 Point after second point.)
   @param(x Interpolation value [0..1]. 0 returns `v1`; 1 return `v2`.)
}
implementation

function GCD(a, b : Int64) : Int64;
var
  Temp : Int64;
begin
  while b <> 0 do
  begin
    Temp := b;
    b := a mod b;
    a := Temp;
  end;
  Result := a;
end;

function LCM(a, b : Int64) : Int64;
begin
  Result := b * (a div GCD(a, b));
end;


// Interpolations
// --------------

function Linear_Interpolate(const a, b, x : Double) : Double;
begin
  Result := a + x * (b - a);
end;

function Cosine_Interpolate(const a, b, x : Double) : Double;
var
  f : Double;
begin
  f := (1 - cos(x * Pi)) * 0.5;
  Result := a + f * (b - a);
end;

function Cubic_Interpolate(const v0, v1, v2, v3, x : Double) : Double;
var
  P, Q, R, S : Double;
begin
  P := (v3 - v2) - (v0 - v1);
  Q := (v0 - v1) - P;
  R := v2 - v0;
  S := v1;

  Result := P * x * x * x + Q * x * x + R * x + S;
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
