unit ufCHXMultiFolderEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, StdCtrls, EditBtn, ComCtrls,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXPropEditor;

type

  { TfmCHXMultiFolderEditor }

  TfmCHXMultiFolderEditor = class(TfmCHXPropEditor)
    actAddFolder: TAction;
    actDeleteFolder: TAction;
    actFolderUp: TAction;
    actFolderDown: TAction;
    actUpdateFolder: TAction;
    eImageCaption: TEdit;
    eImageFolder: TDirectoryEdit;
    lbxImageCaptions: TListBox;
    lbxImageFolders: TListBox;
    pEditFolder: TPanel;
    pImageFolderLists: TPanel;
    tbImageFolderButtons: TToolBar;
    bAddFolder: TToolButton;
    bDeleteFolder: TToolButton;
    bUpdateFolder: TToolButton;
    procedure actAddFolderExecute(Sender: TObject);
    procedure actDeleteFolderExecute(Sender: TObject);
    procedure actUpdateFolderExecute(Sender: TObject);
    procedure eImageFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure eImageFolderButtonClick(Sender: TObject);
    procedure lbxImageCaptionsSelectionChange(Sender: TObject; User: boolean);
    procedure lbxImageFoldersSelectionChange(Sender: TObject; User: boolean);
  private
    FCaptionList: TStrings;
    FFolderList: TStrings;
    procedure SetCaptionList(AValue: TStrings);
    procedure SetFolderList(AValue: TStrings);

  protected

    procedure UpdateFolderData;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property FolderList: TStrings read FFolderList write SetFolderList;
    property CaptionList: TStrings read FCaptionList write SetCaptionList;

        procedure SetBaseFolder(AValue: string);

       constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXMultiFolderEditor }

procedure TfmCHXMultiFolderEditor.lbxImageFoldersSelectionChange(
  Sender: TObject; User: boolean);
begin
  if not User then
    Exit;
  lbxImageCaptions.ItemIndex := lbxImageFolders.ItemIndex;
  UpdateFolderData;
end;

procedure TfmCHXMultiFolderEditor.SetCaptionList(AValue: TStrings);
begin
  if FCaptionList = AValue then
    Exit;
  FCaptionList := AValue;

  LoadFrameData;
end;

procedure TfmCHXMultiFolderEditor.SetBaseFolder(AValue: string);
begin
  SetDirEditInitialDir(eImageFolder, AValue);
end;

procedure TfmCHXMultiFolderEditor.SetFolderList(AValue: TStrings);
begin
  if FFolderList = AValue then
    Exit;
  FFolderList := AValue;

  LoadFrameData;
end;

procedure TfmCHXMultiFolderEditor.lbxImageCaptionsSelectionChange(
  Sender: TObject; User: boolean);
begin
  if not User then
    Exit;
  lbxImageFolders.ItemIndex := lbxImageCaptions.ItemIndex;
  UpdateFolderData;
end;

procedure TfmCHXMultiFolderEditor.eImageFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  if DirectoryExistsUTF8(Value) then
    eImageCaption.Text := ExtractFileName(ExcludeTrailingPathDelimiter(Value))
  else
    eImageCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.eImageFolderButtonClick(Sender: TObject);
begin
end;

procedure TfmCHXMultiFolderEditor.actAddFolderExecute(Sender: TObject);
begin
  if eImageFolder.Directory = '' then
    Exit;

  lbxImageFolders.Items.Add(SetAsFolder(eImageFolder.Directory));


  if eImageCaption.Text = '' then
  begin
    lbxImageCaptions.Items.Add(
      ExtractFileName(ExcludeTrailingPathDelimiter(eImageFolder.Directory)));
  end
  else
  begin
    lbxImageCaptions.Items.Add(eImageCaption.Text);
  end;
end;

procedure TfmCHXMultiFolderEditor.actDeleteFolderExecute(Sender: TObject);
begin
  if lbxImageFolders.ItemIndex = -1 then
    Exit;

  lbxImageCaptions.Items.Delete(lbxImageFolders.ItemIndex);
  lbxImageFolders.Items.Delete(lbxImageFolders.ItemIndex);
end;

procedure TfmCHXMultiFolderEditor.actUpdateFolderExecute(Sender: TObject);
var
  aPos: integer;
begin
  if eImageFolder.Directory = '' then
    Exit;

  aPos := lbxImageFolders.ItemIndex;
  if aPos = -1 then
    exit;

  lbxImageFolders.Items.BeginUpdate;
  lbxImageFolders.Items.Insert(aPos, eImageFolder.Directory);
  lbxImageFolders.Items.Delete(aPos + 1);
  lbxImageFolders.Items.EndUpdate;

  lbxImageCaptions.Items.BeginUpdate;
  if eImageCaption.Text = '' then
  begin
    lbxImageCaptions.Items.Insert(aPos,
      ExtractFileName(ExcludeTrailingPathDelimiter(eImageFolder.Directory)));
  end
  else
  begin
    lbxImageCaptions.Items.Insert(aPos, eImageCaption.Text);
  end;
  lbxImageCaptions.Items.Delete(aPos + 1);
  lbxImageCaptions.Items.EndUpdate;
end;

procedure TfmCHXMultiFolderEditor.UpdateFolderData;
begin
  if lbxImageFolders.ItemIndex <> -1 then
    eImageFolder.Directory := lbxImageFolders.Items[lbxImageFolders.ItemIndex]
  else
    eImageFolder.Clear;

  if lbxImageCaptions.ItemIndex <> -1 then
    eImageCaption.Text := lbxImageCaptions.Items[lbxImageCaptions.ItemIndex]
  else
    eImageCaption.Clear;
end;

constructor TfmCHXMultiFolderEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmCHXMultiFolderEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmCHXMultiFolderEditor.DoClearFrameData;
begin
  lbxImageFolders.Clear;
  lbxImageCaptions.Clear;
  eImageFolder.Clear;
  eImageCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.DoLoadFrameData;
begin
  Enabled := (Assigned(FolderList)) and (Assigned(CaptionList));

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  lbxImageFolders.Items.AddStrings(FolderList, True);
  lbxImageCaptions.Items.AddStrings(CaptionList, True);
  eImageFolder.Clear;
  eImageCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.DoSaveFrameData;
begin
  if not Enabled then
    Exit;

  FolderList.AddStrings(lbxImageFolders.Items, True);
  CaptionList.AddStrings(lbxImageCaptions.Items, True);
end;

end.
