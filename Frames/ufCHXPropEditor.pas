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
unit ufCHXPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, IniFiles,
  uCHXImageUtils,
  ufCHXFrame;

type

  { TfmCHXPropEditor }

  TfmCHXPropEditor = class(TfmCHXFrame)
    actFrameCancel: TAction;
    actFrameOK: TAction;
    alPropEditor: TActionList;
    bCancel: TBitBtn;
    bSave: TBitBtn;
    chkCloseOnSave: TCheckBox;
    ilPropEditor: TImageList;
    pButtons: TPanel;
    procedure actFrameCancelExecute(Sender: TObject);
    procedure actFrameOKExecute(Sender: TObject);
    procedure chkCloseOnSaveChange(Sender: TObject);

  private
    FButtonClose: boolean;
    FOnSaveFrameData: TCHXFrameDataUpdate;
    FSaveButtons: boolean;
    procedure SetButtonClose(AValue: boolean);
    procedure SetOnSaveFrameData(AValue: TCHXFrameDataUpdate);
    procedure SetSaveButtons(AValue: boolean);

  protected
    property OnSaveFrameData: TCHXFrameDataUpdate
      read FOnSaveFrameData write SetOnSaveFrameData;

    procedure DoLoadGUIIcons(aIniFile: TIniFile; aBaseFolder: string); virtual;

  public
    { public declarations }

    property SaveButtons: boolean read FSaveButtons write SetSaveButtons;
    //< Show save and cancel buttons?
    property ButtonClose: boolean read FButtonClose write SetButtonClose;
    //< Close window on button click?

    procedure SaveFrameData;
    // Saves changed data in the frame

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXPropEditor }

procedure TfmCHXPropEditor.actFrameOKExecute(Sender: TObject);
begin
  SaveFrameData;
end;

procedure TfmCHXPropEditor.chkCloseOnSaveChange(Sender: TObject);
begin
  if chkCloseOnSave.Checked <> ButtonClose then
    ButtonClose := chkCloseOnSave.Checked;
end;

procedure TfmCHXPropEditor.actFrameCancelExecute(Sender: TObject);
begin
  LoadFrameData;
end;

procedure TfmCHXPropEditor.SetButtonClose(AValue: boolean);
begin
  FButtonClose := AValue;

  if ButtonClose then
  begin
    bSave.ModalResult := mrOk;
    bCancel.ModalResult := mrCancel;
  end
  else
  begin
    bSave.ModalResult := mrNone;
    bCancel.ModalResult := mrNone;
  end;

  if chkCloseOnSave.Checked <> ButtonClose then
    chkCloseOnSave.Checked := ButtonClose;
end;

procedure TfmCHXPropEditor.SetOnSaveFrameData(AValue: TCHXFrameDataUpdate);
begin
  if FOnSaveFrameData = AValue then
    Exit;
  FOnSaveFrameData := AValue;
end;

procedure TfmCHXPropEditor.SetSaveButtons(AValue: boolean);
begin
  if FSaveButtons = AValue then
    Exit;
  FSaveButtons := AValue;
  pButtons.Visible := SaveButtons;
  pButtons.Enabled := SaveButtons;
end;

procedure TfmCHXPropEditor.DoLoadGUIIcons(aIniFile: TIniFile;
  aBaseFolder: string);
begin
  ReadActionsIconsIni(aIniFile, aBaseFolder, Name, ilPropEditor, alPropEditor);
end;

procedure TfmCHXPropEditor.SaveFrameData;
begin
  if Assigned(OnSaveFrameData) then
    OnSaveFrameData;
end;

constructor TfmCHXPropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SaveButtons := True; // Show buttons by default;

  // If parent/Owner is Tform then autoclose by default;
  if TheOwner is TForm then
  begin
    ButtonClose := True;
    chkCloseOnSave.Visible := True;
  end
  else
  begin
    ButtonClose := False;
    chkCloseOnSave.Visible := False;
  end;

  OnLoadGUIIcons := @DoLoadGUIIcons;
end;

destructor TfmCHXPropEditor.Destroy;
begin
  inherited Destroy;
end;

end.
