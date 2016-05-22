unit ufTagTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, VirtualTrees, Dialogs,
  Forms, Controls,
  uCHXStrUtils;

const
  kTTFileMask = '*.ini';

type
  PTreeData = ^TTreeData;

  TTreeData = record
    Title: string;
    Folder: string;
    FileName: string;
  end;

  TFuncCheckChange = procedure(aList: TStrings) of object;

  { TfmTagTree }

  TfmTagTree = class(TFrame)
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
    { private declarations }
    FFolder: string;
    FOnCheckChange: TFuncCheckChange;
    FTagsFileMask: string;
    procedure SetCheckedList(AValue: TStringList);
    procedure SetFolder(AValue: string);
    procedure SetOnCheckChange(AValue: TFuncCheckChange);
    procedure SetTagsFileMask(AValue: string);

  protected
    procedure SearchTagFiles(aFolder: string; aRootNode: PVirtualNode);

  public
    { public declarations }
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

{ TfmTagTree }

procedure TfmTagTree.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
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

procedure TfmTagTree.VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
  i: integer;
begin
  Data := VST.GetNodeData(Node);
  // Node it's in its final state
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

procedure TfmTagTree.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TfmTagTree.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PTreeData;
begin
  Data := VST.GetNodeData(Node);
  case Column of
    1: CellText := Data^.Folder;
    2: CellText := Data^.FileName;
    else
      CellText := Data^.Title;
  end;
end;

procedure TfmTagTree.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pData: PTreeData;
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

procedure TfmTagTree.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
  UpdateTree;
end;

procedure TfmTagTree.SetOnCheckChange(AValue: TFuncCheckChange);
begin
  if FOnCheckChange = AValue then
    Exit;
  FOnCheckChange := AValue;
end;

procedure TfmTagTree.SetCheckedList(AValue: TStringList);
begin
  if FCheckedList = AValue then
    Exit;
  FCheckedList := AValue;
end;

procedure TfmTagTree.SetTagsFileMask(AValue: string);
begin
  if FTagsFileMask = AValue then
    Exit;
  FTagsFileMask := AValue;
end;

procedure TfmTagTree.SearchTagFiles(aFolder: string; aRootNode: PVirtualNode);
var
  aList: TStringList;
  CurrNode: PVirtualNode;
  Pdata: PTreeData;
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

procedure TfmTagTree.UpdateTree;
begin
  VST.Clear;
  if not DirectoryExistsUTF8(Folder) then
    exit;
  VST.BeginUpdate;
  SearchTagFiles(Folder, nil);
  VST.EndUpdate;
end;

constructor TfmTagTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  TagsFileMask := kTTFileMask;
  VST.NodeDataSize := SizeOf(TTreeData);
  FCheckedList := TStringList.Create;
end;

destructor TfmTagTree.Destroy;
begin
  FreeAndNil(FCheckedList);
  inherited Destroy;
end;

end.
