{ Unit of a simple progress bar form. }
unit ufCHXProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons;

type

  { TODO: Some ideas.
    * Use a timered event for update. So we don't need to test in every call.
      En critiano: En vez de hacer las comprobaciones de actualizar cada vez
      que se llama, tan solo actualizar las propiedades, y cuando salte el timer
      que lo muestre.
    * Añadir tiempo aproximado de finalizacion. Obviamente actualizarlo menos
      amenudo que los gráficos... Hora de inicio...
  }

  { TfrmCHXProgressBar }

  TfrmCHXProgressBar = class(TForm)
    bCancel: TBitBtn;
    lInfo1: TLabel;
    lAction: TLabel;
    lInfo2: TLabel;
    ProgressBar: TProgressBar;
    procedure bCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { private declarations }
    FContinue: boolean;
    FNextTime: TDateTime;
    FUpdateInterval: TDateTime;
    procedure SetContinue(AValue: boolean);
    procedure SetNextTime(AValue: TDateTime);
    procedure SetUpdateInterval(AValue: TDateTime);

  protected
    property Continue: boolean read FContinue write SetContinue;
    property NextTime: TDateTime read FNextTime write SetNextTime;

  public
    { public declarations }
    property UpdateInterval: TDateTime read FUpdateInterval write SetUpdateInterval;

    function UpdateProgressBar(const Value, MaxValue: int64): boolean;
    function UpdTextAndBar(const aAction, Info1, Info2: string;
      const Value, MaxValue: int64): boolean;

    procedure Start;
    procedure Finnish;

    constructor Create(TheOwner: TComponent); override;
  end;

var
  frmCHXProgressBar: TfrmCHXProgressBar;

implementation

{ TfrmCHXProgressBar }

procedure TfrmCHXProgressBar.FormCreate(Sender: TObject);
begin
  Continue := True;
  Visible := False;
end;

procedure TfrmCHXProgressBar.SetContinue(AValue: boolean);
begin
  FContinue := AValue;
end;

procedure TfrmCHXProgressBar.SetNextTime(AValue: TDateTime);
begin
  if FNextTime=AValue then Exit;
  FNextTime:=AValue;
end;

procedure TfrmCHXProgressBar.SetUpdateInterval(AValue: TDateTime);
begin
  if FUpdateInterval=AValue then Exit;
  FUpdateInterval:=AValue;
end;

procedure TfrmCHXProgressBar.bCancelClick(Sender: TObject);
begin
  Continue := False;
  Self.Visible := False;
end;

function TfrmCHXProgressBar.UpdateProgressBar(
  const Value, MaxValue: int64): boolean;
begin
  // Showing Form
  if not Self.Visible then
    Start;

    // Uhm...
  if (Value >= MaxValue) or (MaxValue <= 0) then
    Self.Finnish;

  if Now < NextTime then Exit;

  NextTime := Now + UpdateInterval;

  // Catching Cancel button
  Application.ProcessMessages;
  Result := Continue;

  ProgressBar.Max := MaxValue;
  ProgressBar.Position := Value;
end;

function TfrmCHXProgressBar.UpdTextAndBar(const aAction, Info1, Info2: string;
  const Value, MaxValue: int64): boolean;
begin
  lAction.Caption := aAction;
  lInfo1.Caption := Info1;
  lInfo2.Caption := Info2;
  Result := UpdateProgressBar(Value, MaxValue);
end;

procedure TfrmCHXProgressBar.Start;
begin
  Continue := True;
  Visible := True;
  NextTime:= Now;
end;

procedure TfrmCHXProgressBar.Finnish;
begin
  Self.Continue := True;
  Self.Visible := False;
end;

constructor TfrmCHXProgressBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  UpdateInterval := EncodeTime(0,0,0,250);
end;

initialization
  {$I ufCHXProgressBar.lrs}

end.
