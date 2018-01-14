{ Copyright (C) 2017-2018 Chixpy

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
unit ufCHXStrLstPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
    ufCHXListPreview;

type

  { TfmCHXStrLstPreview }

  TfmCHXStrLstPreview = class(TfmCHXListPreview)
  private
    FStrList: TStrings;
    procedure SetStrList(AValue: TStrings);

  public
    property StrList: TStrings read FStrList write SetStrList;

  end;

implementation

{$R *.lfm}

{ TfmCHXStrLstPreview }

procedure TfmCHXStrLstPreview.SetStrList(AValue: TStrings);
begin
  if FStrList = AValue then Exit;
  FStrList := AValue;

  if Assigned(StrList) then
    ItemCount := StrList.Count
  else
    ItemCount := 0;
end;

end.

