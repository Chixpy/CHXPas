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
  // QWord only handle up to 20!
  // LongInt and LongWord only 12!
  Factorial : array[0..20] of uint64 =
    (1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800,
    479001600, 6227020800, 87178291200, 1307674368000, 20922789888000,
    355687428096000, 6402373705728000, 121645100408832000, 2432902008176640000);

  // Same with Fibonacci :-P
  Fibonacci : array[0..46] of uint32 =
    (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987,
    1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025,
    121393, 196418, 317811, 514229, 832040, 1346269,
    2178309, 3524578, 5702887, 9227465, 14930352, 24157817,
    39088169, 63245986, 102334155, 165580141, 267914296, 433494437,
    701408733, 1836311903, 2971215073);

function GCD(a, b : int64) : int64;
{< Greatest common divisor }
function LCM(a, b : int64) : int64;
{< Least Common Multiple}

implementation

function GCD(a, b : int64) : int64;
var
  Temp : int64;
begin
  while b <> 0 do
  begin
    Temp := b;
    b := a mod b;
    a := Temp;
  end;
  Result := a;
end;

function LCM(a, b : int64) : int64;
begin
  Result := b * (a div GCD(a, b));
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
