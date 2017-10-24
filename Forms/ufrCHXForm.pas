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
unit ufrCHXForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  inifiles, IniPropStorage,
  uCHXStrUtils,
  ufCHXFrame;

type

  { TfrmCHXForm }

  TfrmCHXForm = class(TForm)
    IniPropStorage: TIniPropStorage;

  private
    FOnLoadGUIConfig: TCHXUseGUIConfigIni;
    FOnLoadGUIIcons: TCHXUseGUIConfigIni;
    FOnSaveGUIConfig: TCHXUseGUIConfigIni;
    procedure SetOnLoadGUIConfig(AValue: TCHXUseGUIConfigIni);
    procedure SetOnLoadGUIIcons(AValue: TCHXUseGUIConfigIni);
    procedure SetOnSaveGUIConfig(AValue: TCHXUseGUIConfigIni);

  protected
    property OnLoadGUIConfig: TCHXUseGUIConfigIni
      read FOnLoadGUIConfig write SetOnLoadGUIConfig;
    property OnSaveGUIConfig: TCHXUseGUIConfigIni
      read FOnSaveGUIConfig write SetOnSaveGUIConfig;

    property OnLoadGUIIcons: TCHXUseGUIConfigIni
      read FOnLoadGUIIcons write SetOnLoadGUIIcons;

  public
    procedure LoadGUIConfig(aGUIConfigIni: string);

    procedure LoadGUIIcons(aIconsIni: string);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmCHXForm: TfrmCHXForm;

implementation

{$R *.lfm}

{ TfrmCHXForm }
procedure TfrmCHXForm.SetOnLoadGUIConfig(AValue: TCHXUseGUIConfigIni);
begin
  if FOnLoadGUIConfig = AValue then
    Exit;
  FOnLoadGUIConfig := AValue;
end;

procedure TfrmCHXForm.SetOnLoadGUIIcons(AValue: TCHXUseGUIConfigIni);
begin
  if FOnLoadGUIIcons = AValue then
    Exit;
  FOnLoadGUIIcons := AValue;
end;

procedure TfrmCHXForm.SetOnSaveGUIConfig(AValue: TCHXUseGUIConfigIni);
begin
  if FOnSaveGUIConfig = AValue then
    Exit;
  FOnSaveGUIConfig := AValue;
end;

procedure TfrmCHXForm.LoadGUIConfig(aGUIConfigIni: string);

  procedure LoadGUIConfigChildren(const aComponent: TComponent;
    aIniFile: TIniFile);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).LoadGUIConfig(aIniFile);
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        LoadGUIConfigChildren(aComponent.Components[i], aIniFile);
        Inc(i);
      end;
    end;
  end;

var
  aIniFile: TMemIniFile;
begin
  aGUIConfigIni := SetAsFile(aGUIConfigIni);

  IniPropStorage.IniFileName := aGUIConfigIni;
  IniPropStorage.Restore;

  if aGUIConfigIni = '' then
    Exit;

  aIniFile := TMemIniFile.Create(aGUIConfigIni);
  try
    if Assigned(OnLoadGUIConfig) then
      OnLoadGUIConfig(aIniFile);

    // Updating all TfmCHXFrame components
    LoadGUIConfigChildren(Self, aIniFile);
  finally
    aIniFile.Free;
  end;
end;

procedure TfrmCHXForm.LoadGUIIcons(aIconsIni: string);
  procedure LoadGUIIconsChildren(aComponent: TComponent; aGUIIconsIni: TIniFile; aBaseFolder: string);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).LoadGUIIcons(aGUIIconsIni, aBaseFolder);
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        LoadGUIIconsChildren(aComponent.Components[i], aGUIIconsIni, aBaseFolder);
        Inc(i);
      end;
    end;
  end;

var
  aIniFile: TMemIniFile;
begin
  if aIconsIni = '' then
    Exit;

  aIconsIni := SetAsFile(aIconsIni);

  aIniFile := TMemIniFile.Create(aIconsIni);
  try
    if Assigned(OnLoadGUIConfig) then
      OnLoadGUIIcons(aIniFile);

    // Updating all TfmCHXFrame components
    LoadGUIIconsChildren(Self, aIniFile, ExtractFilePath(aIconsIni));
  finally
    aIniFile.Free;
  end;
end;

constructor TfrmCHXForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfrmCHXForm.Destroy;

  procedure SaveGUIConfigChildren(const aComponent: TComponent;
    aIniFile: TIniFile);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).SaveGUIConfig(aIniFile);
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        SaveGUIConfigChildren(aComponent.Components[i], aIniFile);
        Inc(i);
      end;
    end;
  end;

var
  aIniFile: TMemIniFile;
begin
  if IniPropStorage.IniFileName <> '' then
  begin
    // Saving all TfmCHXFrame components
    aIniFile := TMemIniFile.Create(IniPropStorage.IniFileName);
    try
      if Assigned(OnSaveGUIConfig) then
        OnSaveGUIConfig(aIniFile);

      SaveGUIConfigChildren(Self, aIniFile);
      aIniFile.UpdateFile;
    finally
      aIniFile.Free;
    end;
  end;

  inherited Destroy;
end;

end.