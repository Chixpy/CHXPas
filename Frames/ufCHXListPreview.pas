unit ufCHXListPreview;
{< TfmCHXListPreview frame unit.

  Copyright (C) 2017-2018 Chixpy

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
  StdCtrls, ExtCtrls, ActnList, IniFiles,
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
    tbNextItem: TToolButton;
    tbPreviewList: TToolBar;
    ToolButton1: TToolButton;
    tbPreviousItem: TToolButton;
    tbFirstItem: TToolButton;
    tbLastItem: TToolButton;
    ToolButton6: TToolButton;
    procedure actFirstItemExecute(Sender: TObject);
    procedure actLastItemExecute(Sender: TObject);
    procedure actNextItemExecute(Sender: TObject);
    procedure actPreviousItemExecute(Sender: TObject);
    procedure cbxCurrItemSelect(Sender: TObject);

  private
    FCurrItem: integer;
    FItemCount: integer;
    procedure SetCurrItem(AValue: integer);
    procedure SetItemCount(AValue: integer);

  protected
    procedure OnCurrItemChange; virtual; abstract;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

    procedure DoLoadGUIIcons(aIniFile: TIniFile; const aBaseFolder: string); virtual;

  public
    property ItemCount: integer read FItemCount write SetItemCount;
    property CurrItem: integer read FCurrItem write SetCurrItem;

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
  ReadActionsIconsIni(aIniFile, aBaseFolder, Name, ilPreviewList,
    alPreviewList);
end;

constructor TfmCHXListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnLoadGUIIcons := @DoLoadGUIIcons;
end;

destructor TfmCHXListPreview.Destroy;
begin
  inherited Destroy;
end;

procedure TfmCHXListPreview.actNextItemExecute(Sender: TObject);
begin
  if ItemCount < 1 then
    Exit;
  if CurrItem = ItemCount then
    CurrItem := 1
  else
    CurrItem := CurrItem + 1;
end;

procedure TfmCHXListPreview.actFirstItemExecute(Sender: TObject);
begin
  if ItemCount < 1 then
    Exit;
  CurrItem := 1;
end;

procedure TfmCHXListPreview.actLastItemExecute(Sender: TObject);
begin
  if ItemCount < 1 then
    Exit;
  CurrItem := ItemCount;
end;

procedure TfmCHXListPreview.actPreviousItemExecute(Sender: TObject);
begin
  if ItemCount < 1 then
    Exit;
  if CurrItem = 1 then
    CurrItem := ItemCount
  else
    CurrItem := CurrItem - 1;
end;

procedure TfmCHXListPreview.cbxCurrItemSelect(Sender: TObject);
begin
  CurrItem := cbxCurrItem.ItemIndex + 1;
end;

procedure TfmCHXListPreview.SetCurrItem(AValue: integer);
begin

  if ItemCount > 0 then
  begin
    // Keep in range [1..ItemCount]
    if AValue > ItemCount then
    begin
      AValue := ItemCount;
    end
    else if AValue < 1 then
    begin
      AValue := 1;
    end;
  end
  else
  begin
    // if ItemCount = 0 then
    AValue := 0;
  end;

  if FCurrItem = AValue then
    Exit;

  FCurrItem := AValue;

  cbxCurrItem.ItemIndex := FCurrItem - 1;

  OnCurrItemChange;
end;

procedure TfmCHXListPreview.DoClearFrameData;
begin
  lMaxItems.Caption := format(rsTotalItemsCount, [ItemCount]);
  cbxCurrItem.Clear;
  CurrItem := 0;
end;

procedure TfmCHXListPreview.DoLoadFrameData;
var
  i: integer;
begin
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

  actNextItem.Enabled := ItemCount > 1;
  actPreviousItem.Enabled := ItemCount > 1;
  cbxCurrItem.Enabled := ItemCount > 1;

  CurrItem := 1;
end;

end.
