unit ufCHXListPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ActnList,
  uCHXImageUtils,
  ufCHXFrame;

resourcestring
  rsTotalItemsCount = ' / %0:d';

type

  { TfmCHXListPreview }

  TfmCHXListPreview = class(TfmCHXFrame)
    actNextItem: TAction;
    actPreviousItem: TAction;
    alPreviewList: TActionList;
    cbxCurrItem: TComboBox;
    ilPreviewList: TImageList;
    lMaxItems: TLabel;
    tbNextItem: TToolButton;
    tbPreviewList: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton6: TToolButton;
    procedure actNextItemExecute(Sender: TObject);
    procedure actPreviousItemExecute(Sender: TObject);
    procedure cbxCurrItemSelect(Sender: TObject);

  private
    FCurrItem: integer;
    FItemCount: integer;
    procedure SetCurrItem(AValue: integer);
    procedure SetItemCount(AValue: integer);

  protected
    procedure SetGUIIconsIni(AValue: string); override;
    procedure OnCurrItemChange; virtual; abstract;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property ItemCount: integer read FItemCount write SetItemCount;
    property CurrItem: integer read FCurrItem write SetCurrItem;

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

procedure TfmCHXListPreview.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
  ReadActionsIcons(GUIIconsIni, Name, ilPreviewList, alPreviewList);
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

procedure TfmCHXListPreview.ClearFrameData;
begin
  lMaxItems.Caption := format(rsTotalItemsCount, [ItemCount]);
  cbxCurrItem.Clear;
  CurrItem := 0;
end;

procedure TfmCHXListPreview.LoadFrameData;
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
