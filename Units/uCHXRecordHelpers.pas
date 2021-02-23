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
    procedure FromString(const S: string; const Delim: Char = ',');
    function ToString(const Delim: char = ','): string;
  end;

implementation

procedure TRectString.FromString(const S: string; const Delim: Char);
var
  aSuccess: boolean;
  StrLst: TStringList;
begin
  Self := Empty;

  StrLst := TStringList.Create;
  StrLst.Delimiter := Delim;
  StrLst.CommaText := S;
  try
    aSuccess := StrLst.Count = 4;
    if aSuccess then
      aSuccess := TryStrToInt(StrLst[0], self.Left);
    if aSuccess then
      aSuccess := TryStrToInt(StrLst[1], self.Top);
    if aSuccess then
      aSuccess := TryStrToInt(StrLst[2], self.Right);
    if aSuccess then
      aSuccess := TryStrToInt(StrLst[3], self.Bottom);
  finally
    StrLst.Free;
  end;

  if not aSuccess then
    Self := Empty;
end;

function TRectString.ToString(const Delim: Char): string;
begin
  Result := Format('%d%s%d%s%d%s%d', [self.Left, Delim, self.Top,
    Delim, self.Right, Delim, self.Bottom]);
end;

end.
