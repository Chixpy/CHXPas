unit uCHXRecordHelpers;

{< Unit with some useful records helpers for some common FPC types.

  Copyright (C) 2023-2024 Chixpy https://github.com/Chixpy
}

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$inline ON}

interface

uses
  Classes, SysUtils, Types;

const
  krsIntValueSep = ',';

resourcestring
  rseInvalidTPointStr = '"%s" is an invalid TPoint string.';
  rseInvalidTRectStr = '"%s" is an invalid TRect string.';

type
  { TPointString }

  TPointString = record helper for TPoint
    function FromString(const S : string;
      const Delim : char = krsIntValueSep) : boolean;
    {< Try to read a TPoint from a string. }
    function ToString(const Delim : char = krsIntValueSep) : string; inline;
    {< For storing it and to read it later with FromString. }
    function ToFmtString(const FmsStr : string) : string; inline;
    {< For pretty formating, but not to read later. }

    //class operator :=(const aValue : string) : TPoint; inline;
    //class operator :=(const aValue : TPoint) : string; inline;
  end;

operator :=(const aValue : string) : TPoint; inline;
operator :=(const aValue : TPoint) : string; inline;

type
  { TRectString }

  TRectString = record helper for TRect
    function FromString(const S : string;
      const Delim : char = krsIntValueSep) : boolean;
    {< Try to read a TRect from a string. }
    function ToString(const Delim : char = krsIntValueSep) : string; inline;
    {< For storing it and to read it later with FromString. }

    //class operator :=(const aValue : string) : TRect; inline;
    //class operator :=(const aValue : TRect) : string; inline;
  end;

operator :=(const aValue : string) : TRect; inline;
operator :=(const aValue : TRect) : string; inline;


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
