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
    actStop: TAction;
    actPause: TAction;
    actPlay: TAction;
    MPlayerControl: TMPlayerControl;
    tbPlay: TToolButton;
    tbPause: TToolButton;
    tbStop: TToolButton;
    procedure actPauseExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);

  private
    FMPlayerPath: string;
    procedure SetMPlayerPath(const aMPlayerPath: string);

  protected
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
    MPlayerControl.Stop
  else
    MPlayerControl.Play;
end;

procedure TfmCHXVideoListPreview.actStopExecute(Sender: TObject);
begin
  MPlayerControl.Stop;
end;

procedure TfmCHXVideoListPreview.SetMPlayerPath(const aMPlayerPath: string);
begin
  if FMPlayerPath = aMPlayerPath then
    Exit;
  FMPlayerPath := aMPlayerPath;

  MPlayerControl.MPlayerPath := MPlayerPath;
end;

procedure TfmCHXVideoListPreview.actPauseExecute(Sender: TObject);
begin
  MPlayerControl.Paused := not MPlayerControl.Paused;
end;

procedure TfmCHXVideoListPreview.OnCurrItemChange;
begin
  if (CurrItem < 1) or (not Assigned(StrList)) or (StrList.Count = 0) then
  begin
    if MPlayerControl.Playing then
      MPlayerControl.Stop;
    MPlayerControl.Filename := '';
    Exit;
  end;

  MPlayerControl.Filename := StrList[CurrItem - 1];
end;

end.
