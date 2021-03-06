unit ufCHXAbout;

{< TfmCHXAbout form unit.

  Copyright (C) 2006-2019 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  strutils,
  // CHX forms
  ufCHXFrame, ufrCHXForm,
  // Misc
  uVersionSupport;

type

  { TfmCHXAbout }

  TfmCHXAbout = class(TfmCHXFrame)
    gbxCompilation: TGroupBox;
    gbxImageExt: TGroupBox;
    lCompilation: TLabel;
    lImageExt: TLabel;
    lTitle: TLabel;
    lVersion: TLabel;
    mAditional: TMemo;
  private

  public
   class function SimpleFormAbout(aInfo: TStrings; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;
   //< Creates a form with About Box.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXAbout }

class function TfmCHXAbout.SimpleFormAbout(aInfo: TStrings;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  fmCHXAbout: TfmCHXAbout;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    // TODO. Use inherited GenSimpleModalForm
    aForm.Name := 'frmCHXAbout';
    aForm.Caption := Application.Title + ': About...';
    aForm.AutoSize := False;
    fmCHXAbout := TfmCHXAbout.Create(aForm);
    fmCHXAbout.Align := alClient;

    fmCHXAbout.mAditional.Assign(aInfo);

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    fmCHXAbout.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    fmCHXAbout.Free;
    aForm.Free;
  end;
end;

constructor TfmCHXAbout.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

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

destructor TfmCHXAbout.Destroy;
begin
  inherited Destroy;
end;

end.
