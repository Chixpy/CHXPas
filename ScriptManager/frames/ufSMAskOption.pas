unit ufSMAskOption;

{< TfmSMAskOption frame unit for Pascal Script.

  Copyright (C) 2017-2023 Chixpy

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
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  // CHX frames
  ufCHXPropEditor,
  //CHX forms
  ufrCHXForm;

type

  { TfmSMAskOption }

  TfmSMAskOption = class(TfmCHXPropEditor)
    lQuestion: TLabel;
    rgbAnswer: TRadioGroup;

  private
    FOptionList: TStrings;
    procedure SetOptionList(AValue: TStrings);

  protected

  public
    property OptionList: TStrings read FOptionList write SetOptionList;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    // Creates a form with AskOption frame.
    class function SimpleForm(const aTitle, aQuestion: string;
      aOptionList: TStrings; var aOption: integer;
      aGUIConfigIni: string; aGUIIconsIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmSMAskOption }

procedure TfmSMAskOption.SetOptionList(AValue: TStrings);
begin
  if FOptionList = AValue then Exit;
  FOptionList := AValue;

  LoadFrameData;
end;

procedure TfmSMAskOption.ClearFrameData;
begin
  inherited ClearFrameData;

  rgbAnswer.Items.Clear;
end;

procedure TfmSMAskOption.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(OptionList);
  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  rgbAnswer.Items.Assign(OptionList);

  if rgbAnswer.Items.Count > 0 then
    rgbAnswer.ItemIndex := 0;
end;

procedure TfmSMAskOption.SaveFrameData;
begin
  inherited SaveFrameData;
end;

class function TfmSMAskOption.SimpleForm(const aTitle, aQuestion: string;
  aOptionList: TStrings; var aOption: integer; aGUIConfigIni: string;
  aGUIIconsIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmSMAskOption;
begin
  aFrame := TfmSMAskOption.Create(nil);

    aFrame.SaveButtons := True;
    aFrame.chkCloseOnSave.Visible := False;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.lQuestion.Caption := aQuestion;
    aFrame.OptionList := aOptionList;
    if aOption in [0..aFrame.rgbAnswer.Items.Count - 1] then
      aFrame.rgbAnswer.ItemIndex := aOption;

    Result := GenSimpleModalFormDontFree(aFrame, 'frmSMAskOption',
      aTitle, aGUIIconsIni, aGUIConfigIni);

    if Result <> mrOk then
      aOption := -1
    else
      aOption := aFrame.rgbAnswer.ItemIndex;
end;

constructor TfmSMAskOption.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmSMAskOption.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmSMAskOption);

finalization
  UnRegisterClass(TfmSMAskOption);
end.
