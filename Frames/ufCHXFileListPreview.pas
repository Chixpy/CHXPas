unit ufCHXFileListPreview;

{< TfmCHXFileListPreview frame unit.

  Copyright (C) 2017-2022 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, LCLIntf, Menus, ExtCtrls, LazFileUtils,
  // CHX units
  uCHXRscStr, uCHXStrUtils,
  // CHX frames
  ufCHXListPreview;

type

  { TfmCHXFileListPreview }

  TfmCHXFileListPreview = class(TfmCHXListPreview, IFPObserver)
    actDeleteFile: TAction;
    actRenameFile: TAction;
    actOpenFileFolder: TAction;
    actOpenWithDefApp: TAction;
    miopDeleteFile: TMenuItem;
    miopRenameFile: TMenuItem;
    miopOpenFileFolder: TMenuItem;
    miopOpenWithDefApp: TMenuItem;
    mipmOpen: TMenuItem;
    mipmOpenFileFolder: TMenuItem;
    mipmOpenWithDefApp: TMenuItem;
    pmOpenFile: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    tbOpenFile: TToolButton;
    tbSepOpenFile: TToolButton;
    procedure actDeleteFileExecute(Sender: TObject);
    procedure actOpenFileFolderExecute(Sender: TObject);
    procedure actOpenWithDefAppExecute(Sender: TObject);
    procedure actRenameFileExecute(Sender: TObject);

  private
    FFileList: TStrings;
    procedure SetFileList(AValue: TStrings);

  protected
    procedure DoLoadFrameData; override;

  public
    property FileList: TStrings read FFileList write SetFileList;
    {< File list. }

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {< IFPObserver callback. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXFileListPreview }

procedure TfmCHXFileListPreview.actOpenWithDefAppExecute(Sender: TObject);
var
  aFilename: string;
begin
  if (ItemIndex < 0) or (not Assigned(FileList)) or (FileList.Count = 0) then
    Exit;

  aFilename := FileList[ItemIndex];

  if not FileExistsUTF8(aFilename) then
  begin
    ShowMessageFmt(rsFileDontExists, [aFilename]);
    Exit;
  end;

  OpenDocument(aFilename);
end;

procedure TfmCHXFileListPreview.actRenameFileExecute(Sender: TObject);
var
  aFilename: string;
begin
  if (ItemIndex < 0) or (not Assigned(FileList)) or (FileList.Count = 0) then
    Exit;

  aFilename := FileList[ItemIndex];

  SaveDialog1.FileName := ExtractFileName(aFilename);
  SaveDialog1.InitialDir := SysPath(ExtractFileDir(aFilename));

  if not SaveDialog1.Execute then Exit;

  if not RenameFileUTF8(aFilename, SaveDialog1.FileName) then
  begin
    ShowMessageFmt(rsErrorRenamingFile, [aFilename, SaveDialog1.FileName]);
    Exit;
  end;

  FileList[ItemIndex] := SaveDialog1.FileName;
end;

procedure TfmCHXFileListPreview.actOpenFileFolderExecute(Sender: TObject);
var
  aFolder: string;
begin
  if (ItemIndex < 0) or (not Assigned(FileList)) or (FileList.Count = 0) then
    Exit;

  aFolder := ExtractFileDir(FileList[ItemIndex]);


  if not DirectoryExistsUTF8(aFolder) then
  begin
    ShowMessageFmt(rsFileDontExists, [aFolder]);
    Exit;
  end;

  OpenDocument(aFolder);
end;

procedure TfmCHXFileListPreview.actDeleteFileExecute(Sender: TObject);
begin
  if (ItemIndex < 0) or (not Assigned(FileList)) or (FileList.Count = 0) then
    Exit;

  if MessageDlg(Format(rsCorfirmDeleteFile, [FileList[ItemIndex]]),
    mtConfirmation, [mbYes, mbNo], -1) = mrNo then
    Exit;

  if not DeleteFileUTF8(FileList[ItemIndex]) then
  begin
    ShowMessageFmt(rsErrorDeletingFile, [FileList[ItemIndex]]);
    Exit;
  end;

  FileList.Delete(ItemIndex);
end;

procedure TfmCHXFileListPreview.SetFileList(AValue: TStrings);
begin
  if FFileList = AValue then
    Exit;

  if Assigned(FileList) then
    FileList.FPODetachObserver(self);

  FFileList := AValue;

  if Assigned(FileList) then
  begin
    FileList.FPOAttachObserver(self);
    ItemCount := FileList.Count;
  end
  else
    ItemCount := 0;
end;

procedure TfmCHXFileListPreview.DoLoadFrameData;
var
  aEnabled: boolean;
begin
  inherited DoLoadFrameData;

  aEnabled := ItemCount > 0;
  actOpenWithDefApp.Enabled := aEnabled;
  actOpenFileFolder.Enabled := aEnabled;
  actRenameFile.Enabled := aEnabled;
  actDeleteFile.Enabled := aEnabled;
end;

procedure TfmCHXFileListPreview.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FileList then
    case Operation of
      ooFree: FileList := nil;
      else
      begin
        ItemCount := FileList.Count;
      end;
    end;
end;

constructor TfmCHXFileListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXFileListPreview.Destroy;
begin
  if Assigned(FileList) then
    FileList.FPODetachObserver(self);
  inherited Destroy;
end;

end.
