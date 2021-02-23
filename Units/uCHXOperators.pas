// A lo mejor quiers usar: uCHXRecordHelpers

unit uCHXOperators;
{< Unit with some operators between different types.

  Copyright (C) 2021-2021 Chixpy

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


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rseInvalidTPoint = '"%s" is an invalid TPoint.';
  rseInvalidTRect = '"%s" is an invalid TRect.';

// TPoint related
// --------------
operator = (const a, b: TPoint): boolean;
operator := (const Value : string) : TPoint;
operator := (const Value : TPoint) : string;

// TRect related
operator = (const a, b: TRect): boolean;
operator := (const Value : string) : TRect;
operator := (const Value : TRect) : string;

implementation

operator = (const a, b: TPoint): boolean;
begin
  Result := PointsEqual(a,b);
end;

operator := (const Value: string): TPoint;
var
  Position: integer;
begin
  try
    Position := Pos(',', Value);
    Result.x := StrToInt(Trim(Copy(Value, 1, Position - 1)));
    Result.y := StrToInt(Trim(Copy(Value, Position + 1, MaxInt)));
  except
    raise EConvertError.CreateFmt(rseInvalidTPoint, [Value]);
  end;
end;

operator := (const Value: TPoint): string;
begin
  Result := IntToStr(Value.x) + ',' + IntToStr(Value.y);
end;

operator = (const a, b: TRect): boolean;
begin
  Result := (a.TopLeft = b.TopLeft) and (a.BottomRight = b.BottomRight);
end;

operator := (const Value: string): TRect;
var
  Position: integer;
  tmpStr: string;
begin
  try
    tmpStr := Value;
    Position := Pos(',', tmpStr);
    Result.Left := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

    tmpStr := Trim(Copy(tmpStr, Position + 1, MaxInt));
    Position := Pos(',', tmpStr);
    Result.Top := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

    tmpStr := Trim(Copy(tmpStr, Position + 1, MaxInt));
    Position := Pos(',', tmpStr);
    Result.Right := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

    Result.Bottom := StrToInt(Trim(Copy(tmpStr, Position + 1, MaxInt)));
  except
    raise EConvertError.CreateFmt(rseInvalidTRect, [Value]);
  end;
end;

operator := (const Value: TRect): string;
begin
  Result := IntToStr(Value.Left) + ',' + IntToStr(Value.Top) + ',' +
    IntToStr(Value.Right) + ',' + IntToStr(Value.Bottom);
end;

end.

