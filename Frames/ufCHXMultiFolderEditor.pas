unit ufCHXMultiFolderEditor;
{< TfmCHXMultiFolderEditor frame unit.

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
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Buttons, ActnList, StdCtrls, EditBtn, ComCtrls,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor;

type

  { TfmCHXMultiFolderEditor }

  TfmCHXMultiFolderEditor = class(TfmCHXPropEditor)
    actAddFolder: TAction;
    actDeleteFolder: TAction;
    actFolderUp: TAction;
    actFolderDown: TAction;
    actUpdateFolder: TAction;
    eCaption: TEdit;
    eFolder: TDirectoryEdit;
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
    procedure eFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure eFolderButtonClick(Sender: TObject);
    procedure lbxImageCaptionsSelectionChange(Sender: TObject; User: boolean);
    procedure lbxImageFoldersSelectionChange(Sender: TObject; User: boolean);
  private
    FCaptionList: TStrings;
    FFolderList: TStrings;
    FInitialFolder: string;
    procedure SetCaptionList(AValue: TStrings);
    procedure SetFolderList(AValue: TStrings);
    procedure SetInitialFolder(AValue: string);

  protected

    procedure UpdateFolderData;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property FolderList: TStrings read FFolderList write SetFolderList;
    property CaptionList: TStrings read FCaptionList write SetCaptionList;
    property InitialFolder: string read FInitialFolder write SetInitialFolder;

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

procedure TfmCHXMultiFolderEditor.SetFolderList(AValue: TStrings);
begin
  if FFolderList = AValue then
    Exit;
  FFolderList := AValue;

  LoadFrameData;
end;

procedure TfmCHXMultiFolderEditor.SetInitialFolder(AValue: string);
begin
  if FInitialFolder=AValue then Exit;
  FInitialFolder:=AValue;
end;

procedure TfmCHXMultiFolderEditor.lbxImageCaptionsSelectionChange(
  Sender: TObject; User: boolean);
begin
  if not User then
    Exit;
  lbxImageFolders.ItemIndex := lbxImageCaptions.ItemIndex;
  UpdateFolderData;
end;

procedure TfmCHXMultiFolderEditor.eFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  if DirectoryExistsUTF8(Value) then
    eCaption.Text := ExtractFileName(ExcludeTrailingPathDelimiter(Value))
  else
    eCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.eFolderButtonClick(Sender: TObject);
begin
  SetDirEditInitialDir(eFolder, InitialFolder);
end;

procedure TfmCHXMultiFolderEditor.actAddFolderExecute(Sender: TObject);
begin
  if eFolder.Directory = '' then
    Exit;

  lbxImageFolders.Items.Add(SetAsFolder(eFolder.Directory));


  if eCaption.Text = '' then
  begin
    lbxImageCaptions.Items.Add(
      ExtractFileName(ExcludeTrailingPathDelimiter(eFolder.Directory)));
  end
  else
  begin
    lbxImageCaptions.Items.Add(eCaption.Text);
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
  if eFolder.Directory = '' then
    Exit;

  aPos := lbxImageFolders.ItemIndex;
  if aPos = -1 then
    exit;

  lbxImageFolders.Items.BeginUpdate;
  lbxImageFolders.Items.Insert(aPos, eFolder.Directory);
  lbxImageFolders.Items.Delete(aPos + 1);
  lbxImageFolders.Items.EndUpdate;

  lbxImageCaptions.Items.BeginUpdate;
  if eCaption.Text = '' then
  begin
    lbxImageCaptions.Items.Insert(aPos,
      ExtractFileName(ExcludeTrailingPathDelimiter(eFolder.Directory)));
  end
  else
  begin
    lbxImageCaptions.Items.Insert(aPos, eCaption.Text);
  end;
  lbxImageCaptions.Items.Delete(aPos + 1);
  lbxImageCaptions.Items.EndUpdate;
end;

procedure TfmCHXMultiFolderEditor.UpdateFolderData;
begin
  if lbxImageFolders.ItemIndex <> -1 then
    eFolder.Directory := lbxImageFolders.Items[lbxImageFolders.ItemIndex]
  else
    eFolder.Clear;

  if lbxImageCaptions.ItemIndex <> -1 then
    eCaption.Text := lbxImageCaptions.Items[lbxImageCaptions.ItemIndex]
  else
    eCaption.Clear;
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
  eFolder.Clear;
  eCaption.Clear;
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
  eFolder.Clear;
  eCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.DoSaveFrameData;
begin
  if not Enabled then
    Exit;

  FolderList.AddStrings(lbxImageFolders.Items, True);
  CaptionList.AddStrings(lbxImageCaptions.Items, True);
end;

end.
