unit ufCHXMultiFolderEditor;

{< TfmCHXMultiFolderEditor frame unit.

  Copyright (C) 2017-2023 Chixpy
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
    lbxFolderCaptions: TListBox;
    lbxFoldersPaths: TListBox;
    pEditFolder: TPanel;
    pFolderList: TPanel;
    tbImageFolderButtons: TToolBar;
    bAddFolder: TToolButton;
    bDeleteFolder: TToolButton;
    bUpdateFolder: TToolButton;
    bFolderUp: TToolButton;
    bFolderDown: TToolButton;
    procedure actAddFolderExecute(Sender: TObject);
    procedure actDeleteFolderExecute(Sender: TObject);
    procedure actFolderDownExecute(Sender: TObject);
    procedure actFolderUpExecute(Sender: TObject);
    procedure actUpdateFolderExecute(Sender: TObject);
    procedure eFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure eFolderButtonClick(Sender: TObject);
    procedure lbxFolderCaptionsSelectionChange(Sender: TObject; User: boolean);
    procedure lbxFoldersPathsSelectionChange(Sender: TObject; User: boolean);

  private
    FCaptionList: TStrings;
    FFolderList: TStrings;
    FInitialFolder: string;
    procedure SetCaptionList(AValue: TStrings);
    procedure SetFolderList(AValue: TStrings);
    procedure SetInitialFolder(AValue: string);

  protected
    procedure UpdateFolderData;

  public

    property FolderList: TStrings read FFolderList write SetFolderList;
    property CaptionList: TStrings read FCaptionList write SetCaptionList;
    property InitialFolder: string read FInitialFolder write SetInitialFolder;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXMultiFolderEditor }

procedure TfmCHXMultiFolderEditor.lbxFoldersPathsSelectionChange(
  Sender: TObject; User: boolean);
begin
  // if not User then
  //   Exit;

  lbxFolderCaptions.ItemIndex := lbxFoldersPaths.ItemIndex;
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
  if FInitialFolder = AValue then
    Exit;
  FInitialFolder := AValue;
end;

procedure TfmCHXMultiFolderEditor.lbxFolderCaptionsSelectionChange(
  Sender: TObject; User: boolean);
begin
  if not User then
    Exit;
  lbxFoldersPaths.ItemIndex := lbxFolderCaptions.ItemIndex;
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

  lbxFoldersPaths.Items.Add(SetAsFolder(eFolder.Directory));


  if eCaption.Text = '' then
  begin
    lbxFolderCaptions.Items.Add(
      ExtractFileName(ExcludeTrailingPathDelimiter(eFolder.Directory)));
  end
  else
  begin
    lbxFolderCaptions.Items.Add(eCaption.Text);
  end;
end;

procedure TfmCHXMultiFolderEditor.actDeleteFolderExecute(Sender: TObject);
begin
  if lbxFoldersPaths.ItemIndex = -1 then
    Exit;

  lbxFolderCaptions.Items.Delete(lbxFoldersPaths.ItemIndex);
  lbxFoldersPaths.Items.Delete(lbxFoldersPaths.ItemIndex);
end;

procedure TfmCHXMultiFolderEditor.actFolderDownExecute(Sender: TObject);
begin
  if (lbxFoldersPaths.ItemIndex = -1) or
    (lbxFoldersPaths.ItemIndex >= (lbxFoldersPaths.Count - 1)) then
    Exit;

  lbxFoldersPaths.Items.Exchange(lbxFoldersPaths.ItemIndex,
    lbxFoldersPaths.ItemIndex + 1);
  lbxFolderCaptions.Items.Exchange(lbxFoldersPaths.ItemIndex,
    lbxFoldersPaths.ItemIndex + 1);

  lbxFoldersPaths.ItemIndex := lbxFoldersPaths.ItemIndex + 1;
end;

procedure TfmCHXMultiFolderEditor.actFolderUpExecute(Sender: TObject);
begin
  if lbxFoldersPaths.ItemIndex < 1 then
    Exit;

  lbxFoldersPaths.Items.Exchange(lbxFoldersPaths.ItemIndex,
    lbxFoldersPaths.ItemIndex - 1);
  lbxFolderCaptions.Items.Exchange(lbxFoldersPaths.ItemIndex,
    lbxFoldersPaths.ItemIndex - 1);

  lbxFoldersPaths.ItemIndex := lbxFoldersPaths.ItemIndex - 1;
end;

procedure TfmCHXMultiFolderEditor.actUpdateFolderExecute(Sender: TObject);
var
  aPos: integer;
begin
  if eFolder.Directory = '' then
    Exit;

  aPos := lbxFoldersPaths.ItemIndex;
  if aPos = -1 then
    exit;

  lbxFoldersPaths.Items.BeginUpdate;
  lbxFoldersPaths.Items.Insert(aPos, eFolder.Directory);
  lbxFoldersPaths.Items.Delete(aPos + 1);
  lbxFoldersPaths.Items.EndUpdate;

  lbxFolderCaptions.Items.BeginUpdate;
  if eCaption.Text = '' then
  begin
    lbxFolderCaptions.Items.Insert(aPos,
      ExtractFileName(ExcludeTrailingPathDelimiter(eFolder.Directory)));
  end
  else
  begin
    lbxFolderCaptions.Items.Insert(aPos, eCaption.Text);
  end;
  lbxFolderCaptions.Items.Delete(aPos + 1);
  lbxFolderCaptions.Items.EndUpdate;
end;

procedure TfmCHXMultiFolderEditor.UpdateFolderData;
begin
  if lbxFoldersPaths.ItemIndex <> -1 then
    eFolder.Directory := lbxFoldersPaths.Items[lbxFoldersPaths.ItemIndex]
  else
    eFolder.Clear;

  if lbxFolderCaptions.ItemIndex <> -1 then
    eCaption.Text := lbxFolderCaptions.Items[lbxFolderCaptions.ItemIndex]
  else
    eCaption.Clear;
end;

constructor TfmCHXMultiFolderEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXMultiFolderEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmCHXMultiFolderEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  lbxFoldersPaths.Clear;
  lbxFolderCaptions.Clear;
  eFolder.Clear;
  eCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := (Assigned(FolderList)) and (Assigned(CaptionList));

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  lbxFoldersPaths.Items.AddStrings(FolderList, True);
  lbxFolderCaptions.Items.AddStrings(CaptionList, True);
  eFolder.Clear;
  eCaption.Clear;
end;

procedure TfmCHXMultiFolderEditor.SaveFrameData;
begin
  inherited SaveFrameData;

  if not Enabled then
    Exit;

  FolderList.AddStrings(lbxFoldersPaths.Items, True);
  CaptionList.AddStrings(lbxFolderCaptions.Items, True);
end;
 
initialization
  RegisterClass(TfmCHXMultiFolderEditor);

finalization
  UnRegisterClass(TfmCHXMultiFolderEditor);
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
