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
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, ActnList,
  LazFileUtils,
  uCHXStrUtils, uCHXImageUtils;

type

  { TfmCHXPropEditor }

  TfmCHXPropEditor = class(TFrame)
    actSaveData: TAction;
    actCancelData: TAction;
    alPropEditor: TActionList;
    bCancel: TBitBtn;
    bSave: TBitBtn;
    ilPropEditor: TImageList;
    pButtons: TPanel;
    procedure actCancelDataExecute(Sender: TObject);
    procedure actSaveDataExecute(Sender: TObject);

  private
    FButtonClose: boolean;
    FGUIIconsIni: string;
    FSaveButtons: boolean;
    procedure SetButtonClose(AValue: boolean);
    procedure SetSaveButtons(AValue: boolean);

  protected
    procedure SetGUIIconsIni(AValue: string); virtual;
    procedure ClearData; virtual; abstract;

  public
    { public declarations }
    property SaveButtons: boolean read FSaveButtons write SetSaveButtons;
    //< Show save and cancel buttons?
    property ButtonClose: boolean read FButtonClose write SetButtonClose;
    //< Close window on button click?

    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;

    procedure SaveData; virtual; abstract;
    {< Save current data. }
    procedure LoadData; virtual; abstract;
    {< Load data. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXPropEditor }

procedure TfmCHXPropEditor.actSaveDataExecute(Sender: TObject);
begin
  SaveData;
end;

procedure TfmCHXPropEditor.actCancelDataExecute(Sender: TObject);
begin
  LoadData;
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
  FGUIIconsIni := SetAsFile(AValue);

  ReadActionsIcons(GUIIconsIni, Name, ilPropEditor, alPropEditor);
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

constructor TfmCHXPropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SaveButtons := True; // Show buttons by default;
  ButtonClose := False; // Don't auto close;

  Enabled := False; // Created disabled, enabled it when ready for use
end;

destructor TfmCHXPropEditor.Destroy;
begin
  inherited Destroy;
end;

end.
