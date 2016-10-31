unit ufCHXChkLstPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls, CheckLst,
  ActnList, Menus,
  ufCHXPropEditor;

resourcestring
  rsLECLPESaveList = 'Save list';

type

  { TfmCHXChkLstPropEditor }

  TfmCHXChkLstPropEditor = class(TfmCHXPropEditor)
    actAddItem: TAction;
    actDeleteItem: TAction;
    actExportList: TAction;
    actImportList: TAction;
    actCheckAll: TAction;
    actUncheckAll: TAction;
    clbPropItems: TCheckListBox;
    miAddItem: TMenuItem;
    MenuItem10: TMenuItem;
    miDeleteItem: TMenuItem;
    miImportList: TMenuItem;
    MenuItem4: TMenuItem;
    miExportList: TMenuItem;
    miSaveList: TMenuItem;
    MenuItem7: TMenuItem;
    miCheckAll: TMenuItem;
    miUncheckAll: TMenuItem;
    pmPropChkList: TPopupMenu;
    pPropChkList: TPanel;
    sbPropChkList: TStatusBar;
    splPropChkList: TSplitter;
    tbPropChkList: TToolBar;
    tbAddItem: TToolButton;
    tbSaveList: TToolButton;
    tbDeleteItem: TToolButton;
    ToolButton3: TToolButton;
    tbImportList: TToolButton;
    tbExportList: TToolButton;
    ToolButton6: TToolButton;
    procedure actAddItemExecute(Sender: TObject);
    procedure actCheckAllExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actExportListExecute(Sender: TObject);
    procedure actImportListExecute(Sender: TObject);
    procedure actUncheckAllExecute(Sender: TObject);
    procedure clbPropItemsClick(Sender: TObject);
    procedure clbPropItemsClickCheck(Sender: TObject);

  private
    { private declarations }

  protected

    procedure ExportList; virtual; abstract;
    procedure ImportList; virtual; abstract;

    procedure AddItemToList; virtual; abstract;
    procedure DeleteItemFromList; virtual; abstract;

    procedure OnListClick(aObject: TObject);  virtual; abstract;
    procedure OnListClickCheck(aObject: TObject; aBool: Boolean);  virtual; abstract;
    procedure SetCheckedAll(aBool: Boolean); virtual; abstract;

  public
    procedure SaveData; override; abstract;
    {< Save current data. }
    procedure LoadData; override; abstract;
    {< Load data. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXChkLstPropEditor }

procedure TfmCHXChkLstPropEditor.actUncheckAllExecute(Sender: TObject);
begin
  clbPropItems.CheckAll(cbUnchecked);
  SetCheckedAll(False);
end;

procedure TfmCHXChkLstPropEditor.clbPropItemsClick(Sender: TObject);
begin
  // TODO 3: Until TCheckListBox.OnItemClick works as expected...
  // http://www.lazarus.freepascal.org/index.php?topic=12319.0
  if clbPropItems.ItemIndex = -1 then
    OnListClick(nil)
  else
    OnListClick(clbPropItems.Items.Objects[clbPropItems.ItemIndex]);
end;

procedure TfmCHXChkLstPropEditor.clbPropItemsClickCheck(Sender: TObject);
begin
   if clbPropItems.ItemIndex = -1 then
    OnListClickCheck(nil, False)
  else
    OnListClickCheck(clbPropItems.Items.Objects[clbPropItems.ItemIndex],
      clbPropItems.Checked[clbPropItems.ItemIndex]);
end;

procedure TfmCHXChkLstPropEditor.actCheckAllExecute(Sender: TObject);
begin
   clbPropItems.CheckAll(cbChecked);
  SetCheckedAll(True);
end;

procedure TfmCHXChkLstPropEditor.actDeleteItemExecute(Sender: TObject);
begin
  DeleteItemFromList;
end;

procedure TfmCHXChkLstPropEditor.actExportListExecute(Sender: TObject);
begin
  ExportList;
end;

procedure TfmCHXChkLstPropEditor.actImportListExecute(Sender: TObject);
begin
   ImportList;
end;

procedure TfmCHXChkLstPropEditor.actAddItemExecute(Sender: TObject);
begin
  AddItemToList;
end;

constructor TfmCHXChkLstPropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  actSaveData.Caption := rsLECLPESaveList; // Make clear that is item list save
end;

destructor TfmCHXChkLstPropEditor.Destroy;
begin
  inherited Destroy;
end;

end.