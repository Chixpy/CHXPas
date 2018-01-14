{ Copyright (C) 2006-2018 Chixpy

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
unit ufCHXProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, ActnList, dateutils,
  uCHXStrUtils,
  ufrCHXForm, ufCHXFrame;

const
  krsProgressBarFormKey = 'frmCHXProgressBar';

resourcestring
  rsProgressBarTitle = 'Please Wait...';
  rsEstimatedTime = '%0:s (Left: %1:s)';
  rsEstimatedTime0 = 'Estimated Time to Finish: Unknown';

type

  TCHXProgressCallBack = function(const aAction, aInfo: string;
    const aValue, aMaxValue: int64; const IsCancelable: boolean): boolean of
    object;
  {< Callback function type to show progress }

  { TfmCHXProgressBar }

  TfmCHXProgressBar = class(TfmCHXFrame)
    actCancel: TAction;
    actUpdate1000: TAction;
    actUpdate500: TAction;
    actUpdate250: TAction;
    actUpdate100: TAction;
    ActionList: TActionList;
    bCancel: TBitBtn;
    lAction: TLabel;
    lInfo: TLabel;
    lTime: TLabel;
    mipbUpdate1000: TMenuItem;
    mipbUpdate500: TMenuItem;
    mipbUpdate300: TMenuItem;
    mipbUpdate100: TMenuItem;
    mipbUpdateFrecuency: TMenuItem;
    mipbCancel: TMenuItem;
    pbProgress: TProgressBar;
    pmProgressBar: TPopupMenu;
    pProgress: TPanel;
    procedure actCancelExecute(Sender: TObject);
    procedure actUpdate1000Execute(Sender: TObject);
    procedure actUpdate100Execute(Sender: TObject);
    procedure actUpdate250Execute(Sender: TObject);
    procedure actUpdate500Execute(Sender: TObject);

  private
    FCancelable: boolean;
    FContinue: boolean;
    FNextTime: TDateTime;
    FStartTime: TDateTime;
    FUpdateInterval: TDateTime;
    procedure SetCancelable(AValue: boolean);
    procedure SetContinue(AValue: boolean);
    procedure SetNextTime(AValue: TDateTime);
    procedure SetStartTime(AValue: TDateTime);
    procedure SetUpdateInterval(AValue: TDateTime);

  protected
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property NextTime: TDateTime read FNextTime write SetNextTime;

  public
    property Cancelable: boolean read FCancelable write SetCancelable;
    property Continue: boolean read FContinue write SetContinue;
    property UpdateInterval: TDateTime read FUpdateInterval
      write SetUpdateInterval;

    function UpdTextAndBar(const aAction, aInfo: string;
      const aValue, aMaxValue: int64; const IsCancelable: boolean): boolean;

    procedure Start;
    procedure Finish;

    class function SimpleForm(aGUIConfigIni: string): TfmCHXProgressBar;
    {< Creates a hidden singleton form with Progress Callback.
      Returns the fmCHXProgressBar created. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

     {$R *.lfm}
var
  frmCHXProgressBar: TfrmCHXForm;
  {< Singleton form. }
  fmCHXProgressBar: TfmCHXProgressBar;

{< Internal CHXProgressBar of frmCHXProgressBar. }

{ TfmCHXProgressBar }

procedure TfmCHXProgressBar.SetCancelable(AValue: boolean);
begin
  if FCancelable = AValue then
    Exit;
  FCancelable := AValue;

  actCancel.Enabled := Cancelable;

  // Updating action if changed
  Application.ProcessMessages;
end;

procedure TfmCHXProgressBar.SetContinue(AValue: boolean);
begin
  if FContinue = AValue then
    Exit;
  FContinue := AValue;
end;

procedure TfmCHXProgressBar.SetNextTime(AValue: TDateTime);
begin
  if FNextTime = AValue then
    Exit;
  FNextTime := AValue;
end;

procedure TfmCHXProgressBar.SetStartTime(AValue: TDateTime);
begin
  if FStartTime = AValue then
    Exit;
  FStartTime := AValue;
end;

procedure TfmCHXProgressBar.SetUpdateInterval(AValue: TDateTime);
begin
  if FUpdateInterval = AValue then
    Exit;
  FUpdateInterval := AValue;
end;

procedure TfmCHXProgressBar.actUpdate100Execute(Sender: TObject);
begin
  UpdateInterval := EncodeTime(0, 0, 0, 100);
end;

procedure TfmCHXProgressBar.actUpdate1000Execute(Sender: TObject);
begin
  UpdateInterval := EncodeTime(0, 0, 1, 0);
end;

procedure TfmCHXProgressBar.actCancelExecute(Sender: TObject);
begin
  Finish;
end;

procedure TfmCHXProgressBar.actUpdate250Execute(Sender: TObject);
begin
  UpdateInterval := EncodeTime(0, 0, 0, 250);
end;

procedure TfmCHXProgressBar.actUpdate500Execute(Sender: TObject);
begin
  UpdateInterval := EncodeTime(0, 0, 0, 500);
end;

function TfmCHXProgressBar.UpdTextAndBar(const aAction, aInfo: string;
  const aValue, aMaxValue: int64; const IsCancelable: boolean): boolean;
var
  TimeDiff: TDateTime;
begin
  // Uhm... Finishing
  if (aValue >= aMaxValue) or (aMaxValue <= 0) then
  begin
    Finish;
    Result := False;
    Exit;
  end;

  // Starting...
  if StartTime = 0 then
    Start;

  Application.ProcessMessages;

  if Now < NextTime then
    Exit;

  lAction.Caption := aAction;
  lInfo.Caption := aInfo;
  pbProgress.Max := aMaxValue;
  pbProgress.Position := aValue;
  Cancelable := IsCancelable;


  if aValue <> 0 then
  begin
    TimeDiff := Now - StartTime;
    lTime.Caption := Format(rsEstimatedTime,
      [TimeToStr(TimeDiff), TimeToStr((aMaxValue - aValue) * TimeDiff / aValue)]);
  end
  else
    lTime.Caption := rsEstimatedTime0;

  NextTime := Now + UpdateInterval;

  Result := Continue;
end;

procedure TfmCHXProgressBar.Start;
begin
  Continue := True;
  StartTime := Now;
  NextTime := StartTime;

  if Parent is TForm then
    Parent.Show;
  Application.ProcessMessages;
end;

procedure TfmCHXProgressBar.Finish;
begin
  Continue := False;
  StartTime := 0;
  NextTime := 0;

  if Parent is TForm then
    TForm(Parent).Close;
  Application.ProcessMessages;
end;

class function TfmCHXProgressBar.SimpleForm(aGUIConfigIni: string): TfmCHXProgressBar;
begin

  // Singleton check
  if not Assigned(frmCHXProgressBar) then
  begin

    Application.CreateForm(TfrmCHXForm, frmCHXProgressBar);
    try
      frmCHXProgressBar.Name := krsProgressBarFormKey;
      frmCHXProgressBar.Caption :=
        Application.Title + ': ' + rsProgressBarTitle;

      frmCHXProgressBar.FormStyle := fsStayOnTop;
      frmCHXProgressBar.Visible := False;
      frmCHXProgressBar.Position := poDesktopCenter;
      frmCHXProgressBar.BorderIcons := [biSystemMenu, biMinimize];
      frmCHXProgressBar.ShowInTaskBar := stAlways;

      // Some default values, overriden with aGUIConfigIni
      frmCHXProgressBar.ClientHeight := 128;
      frmCHXProgressBar.ClientWidth := 480;

      fmCHXProgressBar := TfmCHXProgressBar.Create(frmCHXProgressBar);
      fmCHXProgressBar.Align := alClient;

      frmCHXProgressBar.LoadGUIConfig(aGUIConfigIni);
      fmCHXProgressBar.Parent := frmCHXProgressBar;
    finally
      // Don't Free as it's a hidden form, freed on parent close
      // frmCHXProgressBar.Free;
    end;
  end;

  Result := fmCHXProgressBar;
end;

constructor TfmCHXProgressBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Enabled := True;

  Continue := True;
  Cancelable := actCancel.Enabled;

  StartTime := 0;
  NextTime := 0;
  UpdateInterval := EncodeTime(0, 0, 0, 300);
end;

destructor TfmCHXProgressBar.Destroy;
begin
  inherited Destroy;
end;

end.
