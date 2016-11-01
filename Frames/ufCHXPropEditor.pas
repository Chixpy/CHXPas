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
    FIconsIni: string;
    FSaveButtons: boolean;
    procedure SetIconsIni(AValue: string);
    procedure SetSaveButtons(AValue: boolean);

  protected
    procedure ClearData; virtual; abstract;

  public
    { public declarations }
    property SaveButtons: boolean read FSaveButtons write SetSaveButtons;
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

  if IconsIni <> '' then
    ReadActionsIcons(IconsIni, Self.Name, '', ilPropEditor, alPropEditor);
end;

constructor TfmCHXPropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SaveButtons := True;
end;

destructor TfmCHXPropEditor.Destroy;
begin
  inherited Destroy;
end;

end.
