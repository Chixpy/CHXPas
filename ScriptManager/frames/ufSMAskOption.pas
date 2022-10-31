unit ufSMAskOption;

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
    FPOption: PInteger;
    procedure SetOptionList(AValue: TStrings);
    procedure SetPOption(AValue: PInteger);

   protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property OptionList: TStrings read FOptionList write SetOptionList;
    property POption: PInteger read FPOption write SetPOption;

    // Creates a form with AskOption frame.
    class function SimpleForm(const aTitle, aQuestion: string;
      aOptionList: TStrings; var aOption: integer;
      aGUIIconsIni: string; aGUIConfigIni: string): integer;

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

procedure TfmSMAskOption.SetPOption(AValue: PInteger);
begin
  if FPOption = AValue then Exit;
  FPOption := AValue;
end;

procedure TfmSMAskOption.DoClearFrameData;
begin
  rgbAnswer.Items.Clear;
end;

procedure TfmSMAskOption.DoLoadFrameData;
begin
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

procedure TfmSMAskOption.DoSaveFrameData;
begin
  if Assigned(POption) then
    POption^ := rgbAnswer.ItemIndex;
end;

class function TfmSMAskOption.SimpleForm(const aTitle, aQuestion: string;
  aOptionList: TStrings; var aOption: integer; aGUIIconsIni: string;
  aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmSMAskOption;
begin
  Result := mrNone;
  aOption := -1;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmSMAskOption';
    aForm.Caption := aTitle;

    aFrame := TfmSMAskOption.Create(aForm);
    aFrame.POption := @aOption;
    aFrame.SaveButtons := True;
    aFrame.chkCloseOnSave.Visible := False;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.lQuestion.Caption := aQuestion;
    aFrame.OptionList := aOptionList;
    if aOption in [0..aFrame.rgbAnswer.Items.Count-1] then
      aFrame.rgbAnswer.ItemIndex := aOption;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;

    if Result <> mrOK then
      aOption := -1;

  finally
    aForm.Free;
  end;
end;

constructor TfmSMAskOption.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  POption := nil;

    OnClearFrameData := @DoClearFrameData;
    OnLoadFrameData := @DoLoadFrameData;
    OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmSMAskOption.Destroy;
begin
  inherited Destroy;
end;

end.

