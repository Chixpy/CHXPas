{ Unit of a simple progress bar form. }
unit ufCHXProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons;

type

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
    FContinue: Boolean;
    procedure SetContinue(AValue: Boolean);

  protected
    property Continue: Boolean read FContinue write SetContinue;

  public
    { public declarations }
    function UpdateProgressBar(const Value, MaxValue : Int64): Boolean;
    function UpdTextAndBar(const aAction, Info1, Info2: String;
      const Value, MaxValue: Int64): Boolean;
    procedure Start;
    procedure Finnish;
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

procedure TfrmCHXProgressBar.SetContinue(AValue: Boolean);
begin
  if FContinue = AValue then Exit;
  FContinue := AValue;
end;

procedure TfrmCHXProgressBar.bCancelClick(Sender: TObject);
begin
  Continue := False;
  Self.Visible := False;
end;

function TfrmCHXProgressBar.UpdateProgressBar(const Value, MaxValue : Int64): Boolean;
begin
  // Showing Form
  if not Self.Visible then
    Self.Visible := True;

  // Catching Cancel button
  Application.ProcessMessages;
  Result := Continue;

  // Uhm...
  if (Value >= MaxValue) or (MaxValue <= 0) then
  begin
    Self.Finnish;
    Exit;
  end;

  ProgressBar.Max := MaxValue;
  ProgressBar.Position := Value;
end;

function TfrmCHXProgressBar.UpdTextAndBar(const aAction, Info1, Info2: String;
  const Value, MaxValue: Int64): Boolean;
begin
  lAction.Caption:= aAction;
  lInfo1.Caption := Info1;
  lInfo2.Caption := Info2;
  Result := UpdateProgressBar(Value, MaxValue);
end;

procedure TfrmCHXProgressBar.Start;
begin
  Continue := True;
  Visible := True;
end;

procedure TfrmCHXProgressBar.Finnish;
begin
  Self.Continue := True;
  Self.Visible := False;
end;

initialization
  {$I ufCHXProgressBar.lrs}

end.

