unit ufCHXPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, LazFileUtils;

type

  { TfmCHXPropEditor }

  TfmCHXPropEditor = class(TFrame)
    bCancel: TBitBtn;
    bSave: TBitBtn;
    pButtons: TPanel;
    procedure bCancelClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);

  private
    FSaveButtons: boolean;
    procedure SetSaveButtons(AValue: boolean);

  public
    { public declarations }
    property SaveButtons: boolean read FSaveButtons write SetSaveButtons;

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

procedure TfmCHXPropEditor.bSaveClick(Sender: TObject);
begin
  SaveData;
end;

procedure TfmCHXPropEditor.bCancelClick(Sender: TObject);
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
