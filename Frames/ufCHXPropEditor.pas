unit ufCHXPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, ActnList,
  LazFileUtils,
  uCHXImageUtils;

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
    FIconsIni: string;
    FSaveButtons: boolean;
    procedure SetButtonClose(AValue: boolean);
    procedure SetIconsIni(AValue: string);
    procedure SetSaveButtons(AValue: boolean);

  protected
    procedure ClearData; virtual; abstract;

  public
    { public declarations }
    property SaveButtons: boolean read FSaveButtons write SetSaveButtons;
    //< Show save and cancel buttons?
    property ButtonClose: boolean read FButtonClose write SetButtonClose;
    //< Close window on button click?

    property IconsIni: string read FIconsIni write SetIconsIni;

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

procedure TfmCHXPropEditor.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;

  ReadActionsIcons(IconsIni, Self.Name, ilPropEditor, alPropEditor);
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
