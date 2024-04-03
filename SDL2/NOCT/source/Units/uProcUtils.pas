unit uProcUtils;
{< Unit with methods to help in Processing "simulation".

  Copyright (C) 2024 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function map(const value: Double; const min1, max1, min2, max2: Double): Double;
{< Maps a value from a rengo to another range. An interpolation. }

implementation

function map(const value: Double; const min1, max1, min2, max2: Double): Double;
begin
  map := min2 + (value - min1) * (max2 - min2) / (max1 - min1);
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
