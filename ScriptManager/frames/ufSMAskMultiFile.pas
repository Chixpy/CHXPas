unit ufSMAskMultiFile;
{< TfmSMAskMultiFile frame unit for Pascal Script.

  Copyright (C) 2017-2024 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, ComCtrls,
  // CHX units
  uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor,
  //CHX forms
  ufrCHXForm;

const
  krsSMAskMultiFileName = 'frmSMAskMultiFile';

type

  { TfmSMAskMultiFile }

  TfmSMAskMultiFile = class(TfmCHXPropEditor)
    actAddFiles : TAction;
    actSortFileList : TAction;
    actMoveFileDown : TAction;
    actMoveFileUp : TAction;
    actRemoveFiles : TAction;
    gbxSelectFiles : TGroupBox;
    lbxFiles : TListBox;
    OpenDialog1 : TOpenDialog;
    ToolBar1 : TToolBar;
    tbRemoveFiles : TToolButton;
    tbAddFiles : TToolButton;
    ToolButton1 : TToolButton;
    tbMoveFileUp : TToolButton;
    ToolButton3 : TToolButton;
    tbSortFileList : TToolButton;
    tbMoveFileDown : TToolButton;
    procedure actAddFilesExecute(Sender : TObject);
    procedure actMoveFileDownExecute(Sender : TObject);
    procedure actMoveFileUpExecute(Sender : TObject);
    procedure actRemoveFilesExecute(Sender : TObject);
    procedure actSortFileListExecute(Sender : TObject);

  private
    FOutFileList : TStrings;
    procedure SetOutFileList(AValue : TStrings);

  protected

  public
    property OutFileList : TStrings read FOutFileList write SetOutFileList;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    // Creates a form with AskMultiFile frame.
    class function SimpleForm(aFileList : TStrings; const aTitle : string;
      const aExtFilter : string; const DefFolder : string;
      aGUIConfigIni : string; aGUIIconsIni : string) : integer;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmSMAskMultiFile }

procedure TfmSMAskMultiFile.actRemoveFilesExecute(Sender : TObject);
var
  i : integer;
begin
  i := lbxFiles.ItemIndex;

  lbxFiles.DeleteSelected;

  if i > lbxFiles.Count then
    lbxFiles.ItemIndex := lbxFiles.Count - 1
  else if lbxFiles.Count > 0 then
    lbxFiles.ItemIndex := i;
end;

procedure TfmSMAskMultiFile.actSortFileListExecute(Sender : TObject);
begin
  lbxFiles.Sorted := actSortFileList.Checked;
  actMoveFileDown.Enabled := not actSortFileList.Checked;
  actMoveFileUp.Enabled := not actSortFileList.Checked;
end;

procedure TfmSMAskMultiFile.actMoveFileUpExecute(Sender : TObject);
begin
  if lbxFiles.Sorted or (lbxFiles.ItemIndex < 1) then Exit;

  lbxFiles.Items.Exchange(lbxFiles.ItemIndex, lbxFiles.ItemIndex - 1);

  lbxFiles.ItemIndex := lbxFiles.ItemIndex - 1;
end;

procedure TfmSMAskMultiFile.actMoveFileDownExecute(Sender : TObject);
begin
  if lbxFiles.Sorted or (lbxFiles.ItemIndex = -1) or
    (lbxFiles.ItemIndex > (lbxFiles.Count - 2)) then Exit;

  lbxFiles.Items.Exchange(lbxFiles.ItemIndex, lbxFiles.ItemIndex + 1);

  lbxFiles.ItemIndex := lbxFiles.ItemIndex + 1;
end;

procedure TfmSMAskMultiFile.actAddFilesExecute(Sender : TObject);
begin
  if not OpenDialog1.Execute then Exit;

  if OpenDialog1.Files.Count > 0 then
    SetDlgInitialDir(OpenDialog1, ExtractFileDir(OpenDialog1.Files[0]));

  lbxFiles.Items.AddStrings(OpenDialog1.Files);
end;

procedure TfmSMAskMultiFile.SetOutFileList(AValue : TStrings);
begin
  if FOutFileList = AValue then Exit;
  FOutFileList := AValue;

  LoadFrameData;
end;

procedure TfmSMAskMultiFile.ClearFrameData;
begin
  inherited ClearFrameData;

  lbxFiles.Clear;
end;

procedure TfmSMAskMultiFile.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(OutFileList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  lbxFiles.Items.Assign(OutFileList);
end;

procedure TfmSMAskMultiFile.SaveFrameData;
begin
  inherited SaveFrameData;

  if assigned(OutFileList) then
    OutFileList.Assign(lbxFiles.Items);
end;

class function TfmSMAskMultiFile.SimpleForm(aFileList : TStrings;
  const aTitle : string; const aExtFilter : string;
  const DefFolder : string; aGUIConfigIni : string;
  aGUIIconsIni : string) : integer;
var
  aFrame : TfmSMAskMultiFile;
begin
  aFrame := TfmSMAskMultiFile.Create(nil);
  aFrame.SaveButtons := True;
  aFrame.ButtonClose := True;
  aFrame.Align := alClient;

  aFrame.OpenDialog1.Title := aTitle;
  aFrame.OpenDialog1.Filter := aExtFilter;
  SetDlgInitialDir(aFrame.OpenDialog1, DefFolder);
  aFrame.OutFileList := aFileList;

  Result := GenSimpleModalForm(aFrame, krsSMAskMultiFileName,
    aTitle, aGUIConfigIni, aGUIIconsIni);
end;

constructor TfmSMAskMultiFile.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmSMAskMultiFile.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmSMAskMultiFile);

finalization
  UnRegisterClass(TfmSMAskMultiFile);
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
