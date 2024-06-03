unit ufCHXChkLstPropEditor;

{< TfmCHXChkLstPropEditor frame unit.

  Copyright (C) 2006-2019 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls, CheckLst, ActnList, Menus,
  // CHX frames
  ufCHXPropEditor;

type

  { TfmCHXChkLstPropEditor }

  TfmCHXChkLstPropEditor = class(TfmCHXPropEditor)
    actAddItem : TAction;
    actDeleteItem : TAction;
    actExportList : TAction;
    actImportList : TAction;
    actCheckAll : TAction;
    actUncheckAll : TAction;
    clbPropItems : TCheckListBox;
    miAddItem : TMenuItem;
    MenuItem10 : TMenuItem;
    miDeleteItem : TMenuItem;
    miImportList : TMenuItem;
    MenuItem4 : TMenuItem;
    miExportList : TMenuItem;
    miSaveList : TMenuItem;
    MenuItem7 : TMenuItem;
    miCheckAll : TMenuItem;
    miUncheckAll : TMenuItem;
    pmPropChkList : TPopupMenu;
    pPropChkList : TPanel;
    sbPropChkList : TStatusBar;
    splPropChkList : TSplitter;
    tbPropChkList : TToolBar;
    tbAddItem : TToolButton;
    tbDeleteItem : TToolButton;
    ToolButton3 : TToolButton;
    tbImportList : TToolButton;
    tbExportList : TToolButton;
    ToolButton6 : TToolButton;
    procedure actAddItemExecute(Sender : TObject);
    procedure actCheckAllExecute(Sender : TObject);
    procedure actDeleteItemExecute(Sender : TObject);
    procedure actExportListExecute(Sender : TObject);
    procedure actImportListExecute(Sender : TObject);
    procedure actUncheckAllExecute(Sender : TObject);
    procedure clbPropItemsClick(Sender : TObject);
    procedure clbPropItemsClickCheck(Sender : TObject);

  private
    { private declarations }

  protected
    procedure ExportList; virtual; abstract;
    procedure ImportList; virtual; abstract;

    procedure AddItemToList; virtual; abstract;
    procedure DeleteItemFromList; virtual; abstract;

    procedure OnListClick(aObject : TObject); virtual; abstract;
    procedure OnListClickCheck(aObject : TObject; aBool : Boolean);
      virtual; abstract;
    procedure SetCheckedAll(aBool : Boolean); virtual; abstract;

  public
    procedure ClearFrameData; override;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXChkLstPropEditor }

procedure TfmCHXChkLstPropEditor.actUncheckAllExecute(Sender : TObject);
begin
  clbPropItems.CheckAll(cbUnchecked);
  SetCheckedAll(False);
end;

procedure TfmCHXChkLstPropEditor.clbPropItemsClick(Sender : TObject);
begin
  { TODO 3: Until TCheckListBox.OnItemClick works as expected...
      http://www.lazarus.freepascal.org/index.php?topic=12319.0 }
  if clbPropItems.ItemIndex = -1 then
    OnListClick(nil)
  else
    OnListClick(clbPropItems.Items.Objects[clbPropItems.ItemIndex]);
end;

procedure TfmCHXChkLstPropEditor.clbPropItemsClickCheck(Sender : TObject);
begin
  if clbPropItems.ItemIndex = -1 then
    OnListClickCheck(nil, False)
  else
    OnListClickCheck(clbPropItems.Items.Objects[clbPropItems.ItemIndex],
      clbPropItems.Checked[clbPropItems.ItemIndex]);
end;

procedure TfmCHXChkLstPropEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  clbPropItems.Clear;
end;

procedure TfmCHXChkLstPropEditor.actCheckAllExecute(Sender : TObject);
begin
  clbPropItems.CheckAll(cbChecked);
  SetCheckedAll(True);
end;

procedure TfmCHXChkLstPropEditor.actDeleteItemExecute(Sender : TObject);
begin
  DeleteItemFromList;
end;

procedure TfmCHXChkLstPropEditor.actExportListExecute(Sender : TObject);
begin
  ExportList;
end;

procedure TfmCHXChkLstPropEditor.actImportListExecute(Sender : TObject);
begin
  ImportList;
end;

procedure TfmCHXChkLstPropEditor.actAddItemExecute(Sender : TObject);
begin
  AddItemToList;
end;

constructor TfmCHXChkLstPropEditor.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXChkLstPropEditor.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmCHXChkLstPropEditor);

finalization
  UnRegisterClass(TfmCHXChkLstPropEditor);
end.
{
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
