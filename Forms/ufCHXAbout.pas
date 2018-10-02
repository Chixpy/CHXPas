unit ufCHXAbout;
{< TfrmCHXAbout form unit.

  Copyright (C) 2006-2018 Chixpy

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  strutils,
  // Misc
  uVersionSupport;

type

  { TfrmCHXAbout }
  // TODO: Make as frame TCHXFrame

  TfrmCHXAbout = class(TForm)
    gbxCompilation: TGroupBox;
    gbxImageExt: TGroupBox;
    lCompilation: TLabel;
    lImageExt: TLabel;
    lTitle: TLabel;
    lVersion: TLabel;
    mAditional: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmCHXAbout: TfrmCHXAbout;

implementation

{$R *.lfm}

{ TfrmCHXAbout }

procedure TfrmCHXAbout.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ': ' + Caption;

  lTitle.Caption := Application.Title;

  lVersion.Caption := GetFileVersion;

  if GetProductVersion <> '0.0.0.0' then
    lVersion.Caption := GetProductVersion + ' (' + lVersion.Caption + ')';

  lCompilation.Caption := GetTargetInfo + LineEnding +
    GetCompilerInfo + ' - ' + GetLCLVersion + LineEnding +
    GetWidgetSet + LineEnding + '(' + GetCompiledDate + ')';

  lImageExt.Caption := AnsiReplaceText(AnsiReplaceText(
    GraphicFileMask(TGraphic), '*.', ''), ';', ' ');

end;

end.
