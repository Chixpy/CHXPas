unit uCHXMath;
{< Unit with some mathematical functions that are noy in Math unit.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function GCD(a, b : Int64) : Int64;
{< Greatest common divisor }
function LCM(a, b: Int64): Int64;
{< Least Common Multiple}

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

function LCM(a, b: Int64): Int64;
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
