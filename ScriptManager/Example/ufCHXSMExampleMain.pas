unit ufCHXSMExampleMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // CHX frames
  ufCHXScriptManager;

type

  { TfrmCHXSMExampleMain }

  TfrmCHXSMExampleMain = class(TForm)
    bOpenScriptManager: TButton;
    procedure bOpenScriptManagerClick(Sender: TObject);
  private

  public

  end;

var
  frmCHXSMExampleMain: TfrmCHXSMExampleMain;

implementation

{$R *.lfm}

{ TfrmCHXSMExampleMain }

procedure TfrmCHXSMExampleMain.bOpenScriptManagerClick(Sender: TObject);
begin
  TfmCHXScriptManager.SimpleForm(
    ExtractFileDir(ExtractFileDir(ExtractFileDir(GetCurrentDir))), '', '');
end;

end.
