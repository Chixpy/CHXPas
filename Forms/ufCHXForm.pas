unit ufCHXForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  IniPropStorage,
  uCHXImageUtils, uCHXStrUtils;

type

  { TfrmCHXForm }

  TfrmCHXForm = class(TForm)
    alCHXActions: TActionList;
    ilCHXActIcons: TImageList;
    IniPropStorage1: TIniPropStorage;
  private
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    procedure SetGUIConfigIni(AValue: string); virtual;
    procedure SetGUIIconsIni(AValue: string); virtual;

  public
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;

  end;

var
  frmCHXForm: TfrmCHXForm;

implementation

{$R *.lfm}

{ TfrmCHXForm }

procedure TfrmCHXForm.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);

  IniPropStorage1.IniFileName := GUIConfigIni;
  IniPropStorage1.Restore;
end;

procedure TfrmCHXForm.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
  ReadActionsIcons(GUIIconsIni, Self.Name, ilCHXActIcons, alCHXActions);
end;

end.

