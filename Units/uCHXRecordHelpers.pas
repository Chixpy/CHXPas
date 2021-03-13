unit uCHXRecordHelpers;

{< Unit with some operators between difiernte types.

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
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils;

type
  TRectString = record helper for TRect
    function FromString(const S: string; const Delim: char = ','): boolean;
    function ToString(const Delim: char = ','): string;
  end;

  { TPointString }

  TPointString = record helper for TPoint
    function FromString(const S: string; const Delim: char = ','): boolean;
    function ToString(const Delim: char = ','): string;
  end;

implementation

{ TPointString }

function TPointString.FromString(const S: string; const Delim: char): boolean;
var
  StrLst: TStringList;
begin
  Self := Zero;

  StrLst := TStringList.Create;
  StrLst.Delimiter := Delim;
  StrLst.CommaText := S;
  try
    Result := StrLst.Count = 2;
    if Result then
      Result := TryStrToInt(StrLst[0], self.X);
    if Result then
      Result := TryStrToInt(StrLst[1], self.Y);
  finally
    StrLst.Free;
  end;

  if not Result then
    Self := Zero;
end;

function TPointString.ToString(const Delim: char): string;
begin
  Result := Format('%d%s%d', [self.X, Delim, self.Y]);
end;

function TRectString.FromString(const S: string; const Delim: char): boolean;
var
  StrLst: TStringList;
begin
  Self := Empty;

  StrLst := TStringList.Create;
  StrLst.Delimiter := Delim;
  StrLst.CommaText := S;
  try
    Result := StrLst.Count = 4;
    if Result then
      Result := TryStrToInt(StrLst[0], self.Left);
    if Result then
      Result := TryStrToInt(StrLst[1], self.Top);
    if Result then
      Result := TryStrToInt(StrLst[2], self.Right);
    if Result then
      Result := TryStrToInt(StrLst[3], self.Bottom);
  finally
    StrLst.Free;
  end;

  if not Result then
    Self := Empty;
end;

function TRectString.ToString(const Delim: char): string;
begin
  Result := Format('%d%s%d%s%d%s%d', [self.Left, Delim, self.Top,
    Delim, self.Right, Delim, self.Bottom]);
end;

end.
