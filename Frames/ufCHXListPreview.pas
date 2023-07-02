unit ufCHXListPreview;

{< TfmCHXListPreview frame unit.

  Copyright (C) 2017-2023 Chixpy

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
  StdCtrls, ExtCtrls, ActnList, Menus, IniFiles,
  // CHX units
  uCHXImageUtils,
  // CHX frames
  ufCHXFrame;

resourcestring
  rsTotalItemsCount = ' / %0:d';

type

  { TfmCHXListPreview }

  TfmCHXListPreview = class(TfmCHXFrame)
    actFirstItem: TAction;
    actLastItem: TAction;
    actNextItem: TAction;
    actPreviousItem: TAction;
    alPreviewList: TActionList;
    cbxCurrItem: TComboBox;
    ilPreviewList: TImageList;
    lMaxItems: TLabel;
    miniLastItem: TMenuItem;
    miniNextItem: TMenuItem;
    pmpiFirstItem: TMenuItem;
    pmpiPreviousItem: TMenuItem;
    mipmPreviousItem: TMenuItem;
    mipmNextItem: TMenuItem;
    mipmLastItem: TMenuItem;
    mipmFirstItem: TMenuItem;
    pmNextItems: TPopupMenu;
    pmPreviewList: TPopupMenu;
    pmPreviousItems: TPopupMenu;
    tbNextItem: TToolButton;
    tbPreviewList: TToolBar;
    tbSeparator1: TToolButton;
    tbPreviousItem: TToolButton;
    tbSepNavButtons: TToolButton;
    procedure actFirstItemExecute(Sender: TObject);
    procedure actLastItemExecute(Sender: TObject);
    procedure actNextItemExecute(Sender: TObject);
    procedure actPreviousItemExecute(Sender: TObject);
    procedure cbxCurrItemSelect(Sender: TObject);

  private
    FItemIndex: integer;
    FItemCount: integer;
    procedure SetItemIndex(AValue: integer);
    procedure SetItemCount(AValue: integer);

  protected
    procedure OnCurrItemChange; virtual; abstract;

    procedure DoLoadGUIIcons(aIniFile: TIniFile;
      const aBaseFolder: string); override;

  public
    property ItemCount: integer read FItemCount write SetItemCount;
    {< Total number of items. }
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    {< Current selected item position. }

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXListPreview }

procedure TfmCHXListPreview.SetItemCount(AValue: integer);
begin
  if FItemCount = AValue then
    Exit;
  FItemCount := AValue;

  LoadFrameData;
end;

procedure TfmCHXListPreview.DoLoadGUIIcons(aIniFile: TIniFile;
  const aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIniFile, aBaseFolder);

  ReadActionsIconsIni(aIniFile, aBaseFolder, Name, ilPreviewList,
    alPreviewList);
end;

constructor TfmCHXListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ItemIndex := -1;
end;

destructor TfmCHXListPreview.Destroy;
begin
  inherited Destroy;
end;

procedure TfmCHXListPreview.actNextItemExecute(Sender: TObject);
begin
  // If there is only 1 item, do nothing.
  if ItemCount < 2 then
    Exit;

  if ItemIndex >= (ItemCount - 1) then
    ItemIndex := 0
  else
    ItemIndex := ItemIndex + 1;
end;

procedure TfmCHXListPreview.actFirstItemExecute(Sender: TObject);
begin
  // If there is only 1 item, do nothing.
  if ItemCount < 2 then
    Exit;

  ItemIndex := 0;
end;

procedure TfmCHXListPreview.actLastItemExecute(Sender: TObject);
begin
  // If there is only 1 item, do nothing.
  if ItemCount < 2 then
    Exit;

  ItemIndex := ItemCount - 1;
end;

procedure TfmCHXListPreview.actPreviousItemExecute(Sender: TObject);
begin
  // If there is only 1 item, do nothing.
  if ItemCount < 2 then
    Exit;

  if ItemIndex <= 0 then
    ItemIndex := ItemCount - 1
  else
    ItemIndex := ItemIndex - 1;
end;

procedure TfmCHXListPreview.cbxCurrItemSelect(Sender: TObject);
begin
  ItemIndex := cbxCurrItem.ItemIndex;
end;

procedure TfmCHXListPreview.SetItemIndex(AValue: integer);
begin
  if ItemCount > 0 then
  begin
    // Keep in range...
    if AValue >= ItemCount then
    begin
      AValue := ItemCount - 1;
    end
    else if AValue < 0 then
    begin
      AValue := 0;
    end;
  end
  else
  begin
    // if ItemCount <= 0 then
    AValue := -1;
  end;

  if FItemIndex = AValue then
    Exit;

  FItemIndex := AValue;

  cbxCurrItem.ItemIndex := FItemIndex;

  OnCurrItemChange;
end;

procedure TfmCHXListPreview.ClearFrameData;
begin
  inherited ClearFrameData;

  lMaxItems.Caption := format(rsTotalItemsCount, [ItemCount]);
  cbxCurrItem.Clear;
  ItemIndex := -1;
end;

procedure TfmCHXListPreview.LoadFrameData;
var
  i: integer;
  ButtonEnabled: boolean;
begin
  inherited LoadFrameData;

  Enabled := ItemCount > 0;

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxCurrItem.Items.BeginUpdate;
  try
    cbxCurrItem.Items.Clear;
    for i := 1 to ItemCount do
      cbxCurrItem.Items.Add(IntToStr(i));
    cbxCurrItem.Enabled := ItemCount > 1;
  finally
    cbxCurrItem.Items.EndUpdate;
  end;

  lMaxItems.Caption := format(rsTotalItemsCount, [ItemCount]);

  ButtonEnabled := ItemCount > 1;

  actFirstItem.Enabled := ButtonEnabled;
  actLastItem.Enabled := ButtonEnabled;
  actNextItem.Enabled := ButtonEnabled;
  actPreviousItem.Enabled := ButtonEnabled;
  cbxCurrItem.Enabled := ButtonEnabled;

  ItemIndex := 0;
end;

initialization
  RegisterClass(TfmCHXListPreview);

finalization
  UnRegisterClass(TfmCHXListPreview);
end.
