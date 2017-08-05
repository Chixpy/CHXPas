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
  Buttons, ActnList,
  uCHXImageUtils,
  ufCHXFrame;

type

  { TfmCHXPropEditor }

  TfmCHXPropEditor = class(TfmCHXFrame)
    actCancelData: TAction;
    actSaveData: TAction;
    alPropEditor: TActionList;
    bCancel: TBitBtn;
    bSave: TBitBtn;
    ilPropEditor: TImageList;
    pButtons: TPanel;
    procedure actCancelDataExecute(Sender: TObject);
    procedure actSaveDataExecute(Sender: TObject);

  private
    FButtonClose: boolean;
    FSaveButtons: boolean;
    procedure SetButtonClose(AValue: boolean);
    procedure SetSaveButtons(AValue: boolean);

  protected
    procedure SetGUIIconsIni(AValue: string); override;
    procedure SetGUIConfigIni(AValue: string); override;

  public
    { public declarations }

    property SaveButtons: boolean read FSaveButtons write SetSaveButtons;
    //< Show save and cancel buttons?
    property ButtonClose: boolean read FButtonClose write SetButtonClose;
    //< Close window on button click?

    procedure SaveFrameData; virtual; abstract;
    // Saves changed data in the frame

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXPropEditor }

procedure TfmCHXPropEditor.actSaveDataExecute(Sender: TObject);
begin
  SaveFrameData;
end;

procedure TfmCHXPropEditor.actCancelDataExecute(Sender: TObject);
begin
  LoadFrameData;
end;

procedure TfmCHXPropEditor.SetButtonClose(AValue: boolean);
begin
  FButtonClose := AValue;

  if FButtonClose then
  begin
    bSave.ModalResult := mrOK;
    bCancel.ModalResult := mrCancel;
  end
  else
  begin
    bSave.ModalResult := mrNone;
    bCancel.ModalResult := mrNone;
  end;
end;

procedure TfmCHXPropEditor.SetSaveButtons(AValue: boolean);
begin
  if FSaveButtons = AValue then
    Exit;
  FSaveButtons := AValue;
  pButtons.Visible := SaveButtons;
  pButtons.Enabled := SaveButtons;
end;

procedure TfmCHXPropEditor.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);

  ReadActionsIcons(GUIIconsIni, Name, ilPropEditor, alPropEditor);
end;

procedure TfmCHXPropEditor.SetGUIConfigIni(AValue: string);
begin
  inherited SetGUIConfigIni(AValue);
end;

constructor TfmCHXPropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SaveButtons := True; // Show buttons by default;
  // If parent/Owner is Tform then autoclose by default;
  ButtonClose := TheOwner is TForm;
end;

destructor TfmCHXPropEditor.Destroy;
begin
  inherited Destroy;
end;

end.

