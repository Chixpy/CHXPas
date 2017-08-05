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
unit ufCHXForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  IniPropStorage,
  uCHXImageUtils, uCHXStrUtils,
  ufCHXFrame;

type

  { TfrmCHXForm }

  TfrmCHXForm = class(TForm)
    alCHXActions: TActionList;
    ilCHXActIcons: TImageList;
    IniPropStorage: TIniPropStorage;
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

  procedure SetGUIConfigChildren(const aComponent: TComponent);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).GUIConfigIni := GUIConfigIni;
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        SetGUIConfigChildren(aComponent.Components[i]);
        Inc(i);
      end;
    end;
  end;

var
  i: integer;
begin
  AValue := SetAsFile(AValue);
  if FGUIConfigIni = AValue then
    Exit;
  FGUIConfigIni := AValue;

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    SetGUIConfigChildren(Components[i]);
    Inc(i);
  end;

  IniPropStorage.IniFileName := GUIConfigIni;
  IniPropStorage.Restore;
end;

procedure TfrmCHXForm.SetGUIIconsIni(AValue: string);

  procedure SetGUIIconsChildren(const aComponent: TComponent);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).GUIIconsIni := GUIIconsIni;
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        SetGUIIconsChildren(aComponent.Components[i]);
        Inc(i);
      end;
    end;
  end;

var
  i: integer;
begin
  AValue := SetAsFile(AValue);
  if FGUIIconsIni = AValue then
    Exit;
  FGUIIconsIni := SetAsFile(AValue);

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    SetGUIIconsChildren(Components[i]);
    Inc(i);
  end;

  ReadActionsIcons(GUIIconsIni, Name, ilCHXActIcons, alCHXActions);
end;

end.
