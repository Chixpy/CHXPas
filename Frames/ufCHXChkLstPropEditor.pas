unit ufCHXChkLstPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls, CheckLst,
  ActnList, Menus,
  uCHXImageUtils,
  ufCHXPropEditor;

resourcestring
  rsLECLPESaveList = 'Save list';

type

  { TfmCHXChkLstPropEditor }

  TfmCHXChkLstPropEditor = class(TfmCHXPropEditor)
    actAddItem: TAction;
    actCancelAndExit: TAction;
    actCheckAll: TAction;
    actCreateSystemStructure: TAction;
    actDeleteItem: TAction;
    actExportList: TAction;
    actImportList: TAction;
    alPropChkList: TActionList;
    actSaveAndExit: TAction;
    actSaveList: TAction;
    actUncheckAll: TAction;
    clbPropItems: TCheckListBox;
    ilListPropEditor: TImageList;
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
    procedure actCancelAndExitExecute(Sender: TObject);
    procedure actCheckAllExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actExportListExecute(Sender: TObject);
    procedure actImportListExecute(Sender: TObject);
    procedure actSaveAndExitExecute(Sender: TObject);
    procedure actSaveListExecute(Sender: TObject);
    procedure actUncheckAllExecute(Sender: TObject);
    procedure clbPropItemsClick(Sender: TObject);
    procedure clbPropItemsClickCheck(Sender: TObject);

  private
    FIconsIni: string;
    procedure SetIconsIni(AValue: string);
    { private declarations }

  protected
    procedure SaveData; override;
    procedure LoadData; override;

  public
    property IconsIni: string read FIconsIni write SetIconsIni;

    procedure SaveList; virtual; abstract;
    procedure LoadList; virtual; abstract;

    procedure ExportList; virtual; abstract;
    procedure ImportList; virtual; abstract;

    procedure AddItemToList; virtual; abstract;
    procedure DeleteItemFromList; virtual; abstract;

    procedure OnListCheckAll; virtual; abstract;
    procedure OnListUncheckAll; virtual; abstract;
    procedure OnListClick;  virtual; abstract;
    procedure OnListClickCheck;  virtual; abstract;


    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXChkLstPropEditor }

procedure TfmCHXChkLstPropEditor.actSaveAndExitExecute(Sender: TObject);
begin
  SaveData;
end;

procedure TfmCHXChkLstPropEditor.actSaveListExecute(Sender: TObject);
begin
  SaveData;
end;

procedure TfmCHXChkLstPropEditor.actUncheckAllExecute(Sender: TObject);
begin
  clbPropItems.CheckAll(cbUnchecked);
  OnListUncheckAll;
end;

procedure TfmCHXChkLstPropEditor.clbPropItemsClick(Sender: TObject);
begin
  // TODO 3: Until TCheckListBox.OnItemClick works as expected...
  // http://www.lazarus.freepascal.org/index.php?topic=12319.0
  OnListClick;
end;

procedure TfmCHXChkLstPropEditor.clbPropItemsClickCheck(Sender: TObject);
begin
  OnListClickCheck;
end;

procedure TfmCHXChkLstPropEditor.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then Exit;
  FIconsIni := AValue;

    if IconsIni <> '' then
    ReadActionsIcons(IconsIni, Self.Name, '', ilListPropEditor, alPropChkList);
end;

procedure TfmCHXChkLstPropEditor.actCancelAndExitExecute(Sender: TObject);
begin
  LoadData;
end;

procedure TfmCHXChkLstPropEditor.actCheckAllExecute(Sender: TObject);
begin
  clbPropItems.CheckAll(cbChecked);
  OnListCheckAll;
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

procedure TfmCHXChkLstPropEditor.SaveData;
begin
  SaveList;
end;

procedure TfmCHXChkLstPropEditor.LoadData;
begin
  LoadList;
end;

constructor TfmCHXChkLstPropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  bSave.Caption := rsLECLPESaveList; // Make clear that is item list save
end;

destructor TfmCHXChkLstPropEditor.Destroy;
begin
  inherited Destroy;
end;

end.
