unit ufCHXTagTree;

{< TfmCHXTagTree frame unit.

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
  Classes, SysUtils, FileUtil, VirtualTrees, VTHeaderPopup, Forms, Controls,
  Graphics, Dialogs, Menus, ActnList, ExtCtrls, ComCtrls, LCLIntf,
  LazFileUtils, LazUTF8,
  uCHXStrUtils, ufCHXFrame;

const
  krsCHXTagTreeFileMask = '*.ini';

type
  TCHXTagTreeData = record
    Title: string;
    Folder: string;
    FileName: string;
  end;

  PCHXTagTreeData = ^TCHXTagTreeData;

  TFuncCheckChange = procedure(aList: TStrings) of object;

  { TfmCHXTagTree }
  // TODO: Use LoadFrameData and ClearFrameData

  TfmCHXTagTree = class(TfmCHXFrame)
    actAddRootFolder: TAction;
    actAddRootFile: TAction;
    actAddSubFolder: TAction;
    actAddFile: TAction;
    actAddGroup2TagFile: TAction;
    actDeleteFolder: TAction;
    actDeleteFile: TAction;
    actOpenTagFolder: TAction;
    actUpdateTagTree: TAction;
    actUncheckAll: TAction;
    actRenameFile: TAction;
    actRenameFolder: TAction;
    actRemoveTagFile: TAction;
    actRemoveGroupFromFile: TAction;
    alCHXTagTree: TActionList;
    ilCHXTagTree: TImageList;
    miOpenTagFolder: TMenuItem;
    miTreeUpdateTree: TMenuItem;
    miTreeUncheckAll: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mipmFileAddRootFile: TMenuItem;
    mipmFileAddRootFolder: TMenuItem;
    mipmFileRoot: TMenuItem;
    mipmRemoveGroup: TMenuItem;
    mipmAddGroup: TMenuItem;
    MenuItem3: TMenuItem;
    mipmFolderRoot: TMenuItem;
    mipmFAddFolder: TMenuItem;
    mipmFAddTagFile: TMenuItem;
    pmTree: TPopupMenu;
    tbTagTree: TToolBar;
    tbOpenTagFolder: TToolButton;
    ToolButton1: TToolButton;
    tbUncheckAll: TToolButton;
    ToolButton2: TToolButton;
    tbUpdateTagTree: TToolButton;
    VST: TVirtualStringTree;
    VTHeaderPopupMenu: TVTHeaderPopupMenu;
    procedure actOpenTagFolderExecute(Sender: TObject);
    procedure actUncheckAllExecute(Sender: TObject);
    procedure actUpdateTagTreeExecute(Sender: TObject);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FCheckedList: TStringList;
    FTagsFolder: string;
    FOnCheckChange: TFuncCheckChange;
    FTagsFileMask: string;
    procedure SetCheckedList(AValue: TStringList);
    procedure SetTagsFolder(AValue: string);
    procedure SetOnCheckChange(AValue: TFuncCheckChange);
    procedure SetTagsFileMask(AValue: string);

  protected
    procedure SearchTagFiles(aFolder: string; aRootNode: PVirtualNode);

    procedure UncheckAllFiles(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: boolean);

    function AddFolder(const aFolder: string;
      aRootNode: PVirtualNode): PVirtualNode;
    function AddFile(const aFile: string;
      aRootNode: PVirtualNode): PVirtualNode;

  public
    property TagsFolder: string read FTagsFolder write SetTagsFolder;
    property TagsFileMask: string read FTagsFileMask write SetTagsFileMask;
    property OnCheckChange: TFuncCheckChange
      read FOnCheckChange write SetOnCheckChange;
    property CheckedList: TStringList read FCheckedList write SetCheckedList;

    procedure UpdateTree;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXTagTree }

procedure TfmCHXTagTree.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PCHXTagTreeData;
begin
  Data := Sender.GetNodeData(Node);

  if Assigned(Data) then
  begin
    Data^.Title := '';
    Data^.Folder := '';
    Data^.FileName := '';
  end;

  Finalize(Data^);
end;

procedure TfmCHXTagTree.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: integer);
begin
  NodeDataSize := SizeOf(TCHXTagTreeData);
end;

procedure TfmCHXTagTree.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pData: PCHXTagTreeData;
begin
  pData := Sender.GetNodeData(Node);

  case TextType of
    ttNormal:
    begin
      case Column of
        1: CellText := pData^.Folder;
        2: CellText := pData^.FileName;
        else
          CellText := pData^.Title;
      end;
    end;
    else
      ;
  end;
end;

procedure TfmCHXTagTree.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PCHXTagTreeData;
  i: integer;
begin
  Data := VST.GetNodeData(Node);

  if Data^.FileName = '' then
    Exit;

  // This is called after un/checked
  if node^.CheckState = csCheckedNormal then
  begin
    AddToStringList(CheckedList, Data^.Folder + Data^.FileName);
  end
  else
  begin
    i := CheckedList.IndexOf(Data^.Folder + Data^.FileName);
    if i <> -1 then
      CheckedList.Delete(i);
  end;

  if Assigned(OnCheckChange) then
    OnCheckChange(CheckedList);
end;

procedure TfmCHXTagTree.actUpdateTagTreeExecute(Sender: TObject);
begin
  UpdateTree;
end;

procedure TfmCHXTagTree.actUncheckAllExecute(Sender: TObject);
begin
  VST.BeginUpdate;
  VST.IterateSubtree(nil, @UncheckAllFiles, nil);
  vst.EndUpdate;

  CheckedList.Clear;

  if Assigned(OnCheckChange) then
    OnCheckChange(CheckedList);
end;

procedure TfmCHXTagTree.actOpenTagFolderExecute(Sender: TObject);
begin
  if not DirectoryExistsUTF8(TagsFolder) then
    Exit;

  OpenDocument(TagsFolder);
end;

procedure TfmCHXTagTree.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pData1, pData2: PCHXTagTreeData;
begin
  Result := 0;
  pData1 := Sender.GetNodeData(Node1);
  pData2 := Sender.GetNodeData(Node2);

  case Column of
    0: // Title
      Result := UTF8CompareText(pData1^.Title, pData2^.Title);
    1: // TagsFolder
      Result := UTF8CompareText(pData1^.Folder, pData2^.Folder);
    2: // Filename
      Result := UTF8CompareText(pData1^.FileName, pData2^.FileName);
    else
      Result := 0;
  end;
end;

procedure TfmCHXTagTree.SetCheckedList(AValue: TStringList);
begin
  if FCheckedList = AValue then
    Exit;
  FCheckedList := AValue;
end;

procedure TfmCHXTagTree.SetTagsFolder(AValue: string);
begin
  FTagsFolder := SetAsFolder(AValue);
  UpdateTree;
end;

procedure TfmCHXTagTree.SetOnCheckChange(AValue: TFuncCheckChange);
begin
  if FOnCheckChange = AValue then
    Exit;
  FOnCheckChange := AValue;
end;

procedure TfmCHXTagTree.SetTagsFileMask(AValue: string);
begin
  if FTagsFileMask = AValue then
    Exit;
  FTagsFileMask := AValue;
end;

procedure TfmCHXTagTree.SearchTagFiles(aFolder: string;
  aRootNode: PVirtualNode);
var
  aList: TStringList;
  CurrNode: PVirtualNode;
  Pdata: PCHXTagTreeData;
  i: integer;
begin
  if not assigned(aRootNode) then
    aRootNode := VST.RootNode;

  // TODO: Maybe load tree dinamically with VSTInitNode, but
  //   when tree is shown and children are added AddChild calls
  //   VSTInitNode before PData is set.

  VST.BeginUpdate;
  aList := TStringList.Create;
  try
    // 1.- Add Directories
    FindAllDirectories(aList, aFolder, False);
    i := 0;
    while i < aList.Count do
    begin
      CurrNode := AddFolder(aList[i], aRootNode);
      SearchTagFiles(aList[i], CurrNode);
      Inc(i);
    end;

    // 2.- Add Category files
    aList.Clear;
    FindAllFiles(aList, aFolder, TagsFileMask, False);
    i := 0;
    while i < aList.Count do
    begin
      AddFile(aList[i], aRootNode);
      Inc(i);
    end;

  finally
    aList.Free;
  end;
  VST.EndUpdate;
end;

procedure TfmCHXTagTree.UncheckAllFiles(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: boolean);
var
  Pdata: PCHXTagTreeData;
begin
  Abort := False;

  Sender.CheckState[Node] := csUncheckedNormal;
end;

function TfmCHXTagTree.AddFolder(const aFolder: string;
  aRootNode: PVirtualNode): PVirtualNode;
var
  Pdata: PCHXTagTreeData;
begin
  Result := VST.AddChild(aRootNode);
  Pdata := VST.GetNodeData(Result);
  Pdata^.Title := ExcludeTrailingPathDelimiter(ExtractFileName(aFolder));
  Pdata^.Folder := SetAsFolder(aFolder);
  Pdata^.FileName := '';
end;

function TfmCHXTagTree.AddFile(const aFile: string;
  aRootNode: PVirtualNode): PVirtualNode;
var
  Pdata: PCHXTagTreeData;
begin
  Result := VST.AddChild(aRootNode);
  Pdata := VST.GetNodeData(Result);
  Pdata^.Title := ExtractFileNameOnly(aFile);
  Pdata^.Folder := SetAsFolder(ExtractFilePath(aFile));
  Pdata^.FileName := ExtractFileName(aFile);
  // Don't use: Node^.CheckState or Node^.CheckType...;
  VST.CheckType[Result] := ctCheckBox;
  VST.CheckState[Result] := csUncheckedNormal;
end;

procedure TfmCHXTagTree.UpdateTree;
begin
  VST.Clear;
  if not DirectoryExistsUTF8(TagsFolder) then
    exit;
  VST.BeginUpdate;
  SearchTagFiles(TagsFolder, nil);
  VST.EndUpdate;
end;

constructor TfmCHXTagTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  TagsFileMask := krsCHXTagTreeFileMask;
  VST.NodeDataSize := SizeOf(TCHXTagTreeData);
  FCheckedList := TStringList.Create;

  Enabled := True; // Always enabled
end;

destructor TfmCHXTagTree.Destroy;
begin
  FreeAndNil(FCheckedList);
  inherited Destroy;
end;

end.
