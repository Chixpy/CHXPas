unit ufCHXTxtListPreview;
{< TfmCHXTxtListPreview frame unit.

  Copyright (C) 2017-2019 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList,
  // CHX frames
  ufCHXFileListPreview;

type

  { TfmCHXTxtListPreview }

  TfmCHXTxtListPreview = class(TfmCHXFileListPreview)
    mText: TMemo;

  private

  protected
    procedure OnCurrItemChange; override;

  public
    procedure ClearFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXTxtListPreview }

procedure TfmCHXTxtListPreview.OnCurrItemChange;
begin
  if (ItemIndex < 0) or (not Assigned(FileList)) or
    (FileList.Count = 0) then
  begin
    mText.Clear;
    Exit;
  end;

  mText.Lines.LoadFromFile(FileList[ItemIndex]);
end;

procedure TfmCHXTxtListPreview.ClearFrameData;
begin
  inherited ClearFrameData;

  mText.Clear;
end;

constructor TfmCHXTxtListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXTxtListPreview.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmCHXTxtListPreview);

finalization
  UnRegisterClass(TfmCHXTxtListPreview);
end.
