unit ufCHXAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // LCL
  strutils,
  // Version support
  uVersionSupport;

type

  { TfrmCHXAbout }

  TfrmCHXAbout = class(TForm)
    gbxCompilation: TGroupBox;
    gbxImageExt: TGroupBox;
    lCompilation: TLabel;
    lImageExt: TLabel;
    lTitle: TLabel;
    lVersion: TLabel;
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
  Self.Caption := Application.Title + ': ' + Self.Caption;

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
