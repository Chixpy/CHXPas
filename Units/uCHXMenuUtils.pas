unit uCHXMenuUtils;

{< Copyright (C) 2019-2020 Chixpy

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
  Classes, SysUtils, Menus;

function CloneMenuItem(Src: TMenuItem): TMenuItem;
procedure CopyMenu(Src, Dst: TMenu);
procedure AddSubMenu(Src: TMenu; Dst: TMenuItem);

implementation

function CloneMenuItem(Src: TMenuItem): TMenuItem;
var
  i: integer;
begin
  Result := nil;

  if not Assigned(Src) then
    Exit;

  Result := TMenuItem.Create(Src.Owner);

  if Assigned(Src.Action) then
  begin
    Result.Action := Src.Action;
  end
  else
  begin
    Result.AutoCheck := Src.AutoCheck;
    Result.Caption := Src.Caption;
    Result.Checked := Src.Checked;
    Result.Enabled := Src.Enabled;
    Result.HelpContext := Src.HelpContext;
    Result.Hint := Src.Hint;
    Result.GroupIndex := Src.GroupIndex;
    Result.RadioItem := Src.RadioItem;
    Result.ImageIndex := Src.ImageIndex;
    Result.Visible := Src.Visible;
    Result.Tag := Src.Tag;
    Result.OnClick := Src.OnClick;
  end;

  i := 0;
  while i < Src.Count do
  begin
    Result.Add(CloneMenuItem(Src[i]));
    Inc(i);
  end;
end;

procedure CopyMenu(Src, Dst: TMenu);
var
  i: integer;
begin
  Dst.Items.Clear;

  i := 0;
  while i < Src.Items.Count do
  begin
    Dst.Items.Add(CloneMenuItem(Src.Items[i]));
    Inc(i);
  end;
end;

procedure AddSubMenu(Src: TMenu; Dst: TMenuItem);
var
  i: integer;
begin
  i := 0;
  while i < Src.Items.Count do
  begin
    Dst.Add(CloneMenuItem(Src.Items[i]));
    Inc(i);
  end;
end;

end.
