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

   TCHXProgressCallBack = function(const Title, Info1, Info2: string;
    const Value, MaxValue: int64): boolean of object;
{< Callback funtion to show progress }

  { TfrmCHXProgressBar }

  TfrmCHXProgressBar = class(TForm)
    bCancel: TBitBtn;
    lAction: TLabel;
    lInfo1: TLabel;
    lInfo2: TLabel;
    ProgressBar: TProgressBar;
    procedure bCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    FCancelable: boolean;
    { private declarations }
    FContinue: boolean;
    FNextTime: TDateTime;
    FUpdateInterval: TDateTime;
    procedure SetCancelable(AValue: boolean);
    procedure SetContinue(AValue: boolean);
    procedure SetNextTime(AValue: TDateTime);
    procedure SetUpdateInterval(AValue: TDateTime);

  protected
    property Continue: boolean read FContinue write SetContinue;
    property NextTime: TDateTime read FNextTime write SetNextTime;

  public
    { public declarations }
    property UpdateInterval: TDateTime read FUpdateInterval
      write SetUpdateInterval;
    property Cancelable: boolean read FCancelable write SetCancelable;

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

procedure TfrmCHXProgressBar.SetCancelable(AValue: boolean);
begin
  if FCancelable = AValue then
    Exit;
  FCancelable := AValue;

  bCancel.Enabled := Cancelable;
  bCancel.Visible := Cancelable;
end;

procedure TfrmCHXProgressBar.SetNextTime(AValue: TDateTime);
begin
  if FNextTime = AValue then
    Exit;
  FNextTime := AValue;
end;

procedure TfrmCHXProgressBar.SetUpdateInterval(AValue: TDateTime);
begin
  if FUpdateInterval = AValue then
    Exit;
  FUpdateInterval := AValue;
end;

procedure TfrmCHXProgressBar.bCancelClick(Sender: TObject);
begin
  Continue := False;
  Visible := False;
end;

function TfrmCHXProgressBar.UpdateProgressBar(
  const Value, MaxValue: int64): boolean;
begin
  // Showing Form
  if not Visible then
    Start;

  // Uhm...
  if (Value >= MaxValue) or (MaxValue <= 0) then
    Finnish;

  if Now < NextTime then
    Exit;

  // Catching Cancel button
  Application.ProcessMessages;
  Result := Continue;

  ProgressBar.Max := MaxValue;
  ProgressBar.Position := Value;

  NextTime := Now + UpdateInterval;
end;

function TfrmCHXProgressBar.UpdTextAndBar(const aAction, Info1, Info2: string;
  const Value, MaxValue: int64): boolean;
begin
  if Now >= NextTime then
  begin
    lAction.Caption := aAction;
    lInfo1.Caption := Info1;
    lInfo2.Caption := Info2;
  end;
  Result := UpdateProgressBar(Value, MaxValue);
end;

procedure TfrmCHXProgressBar.Start;
begin
  Continue := True;
  Visible := True;
  NextTime := Now;
end;

procedure TfrmCHXProgressBar.Finnish;
begin
  Continue := True;
  Visible := False;
end;

constructor TfrmCHXProgressBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  UpdateInterval := EncodeTime(0, 0, 0, 300);
  Cancelable := False;
end;

initialization
  {$I ufCHXProgressBar.lrs}

end.
