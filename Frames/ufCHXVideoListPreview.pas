{ Frame for previewing a list of video/music files.

  Copyright (C) 2017-2018 Chixpy

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
unit ufCHXVideoListPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, MPlayerCtrl, LazFileUtils,
  // CHX frames
  ufCHXStrLstPreview;

type

  { TfmCHXVideoListPreview }

  TfmCHXVideoListPreview = class(TfmCHXStrLstPreview)
    actMute: TAction;
    actStop: TAction;
    actPause: TAction;
    actPlay: TAction;
    MPlayerControl: TMPlayerControl;
    tbPlay: TToolButton;
    tbPause: TToolButton;
    tbPlayer: TToolBar;
    tbrVolume: TTrackBar;
    tbStop: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure actMuteExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure tbrVolumeClick(Sender: TObject);
  private
    FMPlayerPath: string;
    FVolume: integer;
    procedure SetMPlayerPath(const aMPlayerPath: string);
    procedure SetVolume(const aVolume: integer);

  protected
    property Volume: integer read FVolume write SetVolume;
    procedure OnCurrItemChange; override;

  public
    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;

  end;

implementation

{$R *.lfm}

{ TfmCHXVideoListPreview }

procedure TfmCHXVideoListPreview.actPlayExecute(Sender: TObject);
begin
  if (not FileExistsUTF8(MPlayerControl.Filename)) or
    (not MPlayerControl.FindMPlayerPath) then
    Exit;

  if MPlayerControl.Playing then
    actStop.Execute
  else
    actPlay.Execute;
end;

procedure TfmCHXVideoListPreview.actStopExecute(Sender: TObject);
begin
  MPlayerControl.Stop;
end;

procedure TfmCHXVideoListPreview.tbrVolumeClick(Sender: TObject);
begin
  // It's not only "on Click".
  // It works at end of drag, too
  MPlayerControl.Volume := tbrVolume.Position;
end;

procedure TfmCHXVideoListPreview.SetMPlayerPath(const aMPlayerPath: string);
begin
  if FMPlayerPath = aMPlayerPath then
    Exit;
  FMPlayerPath := aMPlayerPath;

  MPlayerControl.MPlayerPath := MPlayerPath;
end;

procedure TfmCHXVideoListPreview.SetVolume(const aVolume: integer);
begin
  if FVolume = aVolume then Exit;
  FVolume := aVolume;
end;

procedure TfmCHXVideoListPreview.actPauseExecute(Sender: TObject);
begin
  MPlayerControl.Paused := not MPlayerControl.Paused;
end;

procedure TfmCHXVideoListPreview.actMuteExecute(Sender: TObject);
begin
 if actMute.Checked then
   begin
     Volume := MPlayerControl.Volume;
     MPlayerControl.Volume := 0;
   end
   else
   begin
     MPlayerControl.Volume := Volume;
   end;
end;

procedure TfmCHXVideoListPreview.OnCurrItemChange;
begin
  if (CurrItem < 1) or (not Assigned(StrList)) or (StrList.Count = 0) then
  begin
    if MPlayerControl.Playing then
      actPlay.Execute;
    MPlayerControl.Filename := '';
    Exit;
  end;

  MPlayerControl.Filename := StrList[CurrItem - 1];
end;

end.
