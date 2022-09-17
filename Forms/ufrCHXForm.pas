unit ufrCHXForm;
{< TfrmCHXForm form unit.

  Copyright (C) 2017-2022 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  IniFiles, DefaultTranslator,
  // CHX units
  uCHXStrUtils, uCHXImageUtils,
  // CHX frames
  ufCHXFrame;

type
  { TfrmCHXForm }

  TfrmCHXForm = class(TForm)
  private
    FFormGUIConfig: string;
    FOnLoadGUIConfig: TCHXUseGUIConfigIni;
    FOnLoadGUIIcons: TCHXUseIconsConfigIni;
    FOnSaveGUIConfig: TCHXUseGUIConfigIni;
    procedure SetFormGUIConfig(const aFormGUIConfig: string);
    procedure SetOnLoadGUIConfig(AValue: TCHXUseGUIConfigIni);
    procedure SetOnLoadGUIIcons(AValue: TCHXUseIconsConfigIni);
    procedure SetOnSaveGUIConfig(AValue: TCHXUseGUIConfigIni);

  protected
    property FormGUIConfig: string read FFormGUIConfig write SetFormGUIConfig;

    property OnLoadGUIConfig: TCHXUseGUIConfigIni
      read FOnLoadGUIConfig write SetOnLoadGUIConfig;
    property OnSaveGUIConfig: TCHXUseGUIConfigIni
      read FOnSaveGUIConfig write SetOnSaveGUIConfig;

    property OnLoadGUIIcons: TCHXUseIconsConfigIni
      read FOnLoadGUIIcons write SetOnLoadGUIIcons;

  public
    procedure LoadGUIConfig(const aGUIConfigIni: string);

    procedure LoadGUIIcons(aIconsIni: string);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfrmCHXForm }
procedure TfrmCHXForm.SetOnLoadGUIConfig(AValue: TCHXUseGUIConfigIni);
begin
  if FOnLoadGUIConfig = AValue then
    Exit;
  FOnLoadGUIConfig := AValue;
end;

procedure TfrmCHXForm.SetFormGUIConfig(const aFormGUIConfig: string);
begin
  FFormGUIConfig := SetAsFile(aFormGUIConfig);
end;

procedure TfrmCHXForm.SetOnLoadGUIIcons(AValue: TCHXUseIconsConfigIni);
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

procedure TfrmCHXForm.LoadGUIConfig(const aGUIConfigIni: string);

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
  aValue: string;
begin
  FormGUIConfig := aGUIConfigIni;

  if FormGUIConfig = '' then
    Exit;

  aIniFile := TMemIniFile.Create(FormGUIConfig);
  try
    // Loading Form properties
    aValue := aIniFile.ReadString('Forms', Name + '_WindowState', '');

    if CompareText(aValue, 'wsMaximized') = 0 then
    begin
      WindowState := wsMaximized
    end
    else if CompareText(aValue, 'wsNormal') = 0 then
    begin
      WindowState := wsNormal;
      Width := aIniFile.ReadInteger('Forms', Name + '_Width', Width);
      Height := aIniFile.ReadInteger('Forms', Name + '_Height', Height);
    end;


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
    if Assigned(OnLoadGUIIcons) then
      OnLoadGUIIcons(aIniFile, ExtractFilePath(aIconsIni));

    // Updating all TfmCHXFrame components
    LoadGUIIconsChildren(Self, aIniFile, ExtractFilePath(aIconsIni));
  finally
    aIniFile.Free;
  end;

  FixComponentImagesFromActions(Self);
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

  if FormGUIConfig <> '' then
  begin
    // Saving all TfmCHXFrame components
    aIniFile := TMemIniFile.Create(FormGUIConfig);
    try
      // Loading Form properties
      if WindowState = wsMaximized then
        aIniFile.WriteString('Forms', Name + '_WindowState', 'wsMaximized')
      else {if WindowState = wsNormal then }
      begin
        // Saving Width & Height only if wsNormal
        aIniFile.WriteString('Forms', Name + '_WindowState', 'wsNormal');
        aIniFile.WriteInteger('Forms', Name + '_Width', Width);
        aIniFile.WriteInteger('Forms', Name + '_Height', Height);
      end;

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
