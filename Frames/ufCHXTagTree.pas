{ Copyright (C) 2006-2017 Chixpy

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
unit ufCHXTagTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls,
  Graphics, Dialogs,
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
    VST: TVirtualStringTree;
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);

  private
    FCheckedList: TStringList;
    FFolder: string;
    FOnCheckChange: TFuncCheckChange;
    FTagsFileMask: string;
    procedure SetCheckedList(AValue: TStringList);
    procedure SetFolder(AValue: string);
    procedure SetOnCheckChange(AValue: TFuncCheckChange);
    procedure SetTagsFileMask(AValue: string);

  protected
    procedure SearchTagFiles(aFolder: string; aRootNode: PVirtualNode);

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

  public
    property Folder: string read FFolder write SetFolder;
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
  Data := VST.GetNodeData(Node);
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
  Data: PCHXTagTreeData;
begin
  Data := VST.GetNodeData(Node);
  case Column of
    1: CellText := Data^.Folder;
    2: CellText := Data^.FileName;
    else
      CellText := Data^.Title;
  end;
end;

procedure TfmCHXTagTree.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pData: PCHXTagTreeData;
begin
  pdata := Sender.GetNodeData(Node);

  if (pData^.FileName = '') then
  begin
    // It's a Folder, read it's subtree :-P
    if pData^.Folder <> '' then
      SearchTagFiles(pData^.Folder, Node);
  end
  else
  begin
    // Don't use: Node^.CheckState or Node^.CheckType...;
    Sender.CheckType[Node] := ctCheckBox;
    Sender.CheckState[Node] := csUncheckedNormal;
  end;
end;

procedure TfmCHXTagTree.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PCHXTagTreeData;
  i: integer;
begin
  Data := VST.GetNodeData(Node);
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

procedure TfmCHXTagTree.SetCheckedList(AValue: TStringList);
begin
  if FCheckedList = AValue then
    Exit;
  FCheckedList := AValue;
end;

procedure TfmCHXTagTree.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
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
  aList := TStringList.Create;
  try
    // 1.- Add Directories
    FindAllDirectories(aList, aFolder, False);
    aList.Sort;
    i := 0;
    while i < aList.Count do
    begin
      CurrNode := VST.AddChild(aRootNode);
      Pdata := VST.GetNodeData(CurrNode);
      Pdata^.Title := ExtractFileNameOnly(aList[i]);
      Pdata^.Folder := SetAsFolder(aList[i]);
      Pdata^.FileName := '';
      Inc(i);
    end;

    // 2.- Add Category files
    aList.Clear;
    FindAllFiles(aList, aFolder, TagsFileMask, False);
    aList.Sort;
    i := 0;
    while i < aList.Count do
    begin
      CurrNode := VST.AddChild(aRootNode);
      Pdata := VST.GetNodeData(CurrNode);
      Pdata^.Title := ExtractFileNameOnly(aList[i]);
      Pdata^.Folder := SetAsFolder(ExtractFilePath(aList[i]));
      Pdata^.FileName := ExtractFileName(aList[i]);
      CurrNode := VST.GetNextSibling(CurrNode);
      Inc(i);
    end;

  finally
    FreeAndNil(aList);
  end;
end;

procedure TfmCHXTagTree.DoClearFrameData;
begin

end;

procedure TfmCHXTagTree.DoLoadFrameData;
begin

end;

procedure TfmCHXTagTree.UpdateTree;
begin
  VST.Clear;
  if not DirectoryExistsUTF8(Folder) then
    exit;
  VST.BeginUpdate;
  SearchTagFiles(Folder, nil);
  VST.EndUpdate;
end;

constructor TfmCHXTagTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  // OnSaveFrameData := @DoSaveFrameData;

  TagsFileMask := krsCHXTagTreeFileMask;
  VST.NodeDataSize := SizeOf(TCHXTagTreeData);
  FCheckedList := TStringList.Create;
end;

destructor TfmCHXTagTree.Destroy;
begin
  FreeAndNil(FCheckedList);
  inherited Destroy;
end;

end.
