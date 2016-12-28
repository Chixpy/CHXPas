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
    FGUIConfigIni: TFilename;
    FGUIIconsIni: TFilename;
    procedure SetGUIConfigIni(AValue: TFilename); virtual;
    procedure SetGUIIconsIni(AValue: TFilename); virtual;

  public
    property GUIConfigIni: TFilename read FGUIConfigIni write SetGUIConfigIni;
    property GUIIconsIni: TFilename read FGUIIconsIni write SetGUIIconsIni;

  end;

var
  frmCHXForm: TfrmCHXForm;

implementation

{$R *.lfm}

{ TfrmCHXForm }

procedure TfrmCHXForm.SetGUIConfigIni(AValue: TFilename);
begin
  FGUIConfigIni := SetAsFile(AValue);

  IniPropStorage1.IniFileName := GUIConfigIni;
  IniPropStorage1.Restore;
end;

procedure TfrmCHXForm.SetGUIIconsIni(AValue: TFilename);
begin
  FGUIIconsIni := SetAsFile(AValue);
  ReadActionsIcons(GUIIconsIni, Self.Name, ilCHXActIcons, alCHXActions);
end;

end.

