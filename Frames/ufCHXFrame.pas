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
unit ufCHXFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, inifiles,
  // CHX units
  uCHXImageUtils;

type
  TCHXUseGUIConfigIni = procedure(aIniFile: TIniFile) of object;
  TCHXUseIconsConfigIni = procedure(aIniFile: TIniFile;
    aBaseFolder: string) of object;
  TCHXFrameDataUpdate = procedure of object;

  { TfmCHXFrame }
  TfmCHXFrame = class(TFrame)
  private
    FOnClearFrameData: TCHXFrameDataUpdate;
    FOnLoadFrameData: TCHXFrameDataUpdate;
    FOnLoadGUIConfig: TCHXUseGUIConfigIni;
    FOnLoadGUIIcons: TCHXUseIconsConfigIni;
    FOnSaveGUIConfig: TCHXUseGUIConfigIni;
    procedure SetOnClearFrameData(AValue: TCHXFrameDataUpdate);
    procedure SetOnLoadFrameData(AValue: TCHXFrameDataUpdate);
    procedure SetOnLoadGUIConfig(AValue: TCHXUseGUIConfigIni);
    procedure SetOnLoadGUIIcons(AValue: TCHXUseIconsConfigIni);
    procedure SetOnSaveGUIConfig(AValue: TCHXUseGUIConfigIni);

  protected
    property OnLoadGUIConfig: TCHXUseGUIConfigIni
      read FOnLoadGUIConfig write SetOnLoadGUIConfig;
    property OnSaveGUIConfig: TCHXUseGUIConfigIni
      read FOnSaveGUIConfig write SetOnSaveGUIConfig;

    property OnLoadGUIIcons: TCHXUseIconsConfigIni
      read FOnLoadGUIIcons write SetOnLoadGUIIcons;

    property OnClearFrameData: TCHXFrameDataUpdate
      read FOnClearFrameData write SetOnClearFrameData;
    property OnLoadFrameData: TCHXFrameDataUpdate
      read FOnLoadFrameData write SetOnLoadFrameData;

    class function GenSimpleModalForm(aCHXFrame: TfmCHXFrame;
      const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string): integer;

    class procedure GenSimpleForm(aCHXFrame: TfmCHXFrame;
      const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string);

  public

    procedure LoadGUIConfig(aIniFile: TIniFile);
    //< Load GUI config.
    procedure SaveGUIConfig(aIniFile: TIniFile);
    //< Save GUI config.

    procedure LoadGUIIcons(aIconsIni: TIniFile; aBaseFolder: string);
    //< Load GUI icons.

    procedure ClearFrameData;
    procedure LoadFrameData;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  // I don't like this here...
  // CHX forms
  ufrCHXForm;

{$R *.lfm}

{ TfmCHXFrame }

procedure TfmCHXFrame.SetOnClearFrameData(AValue: TCHXFrameDataUpdate);
begin
  if FOnClearFrameData = AValue then
    Exit;
  FOnClearFrameData := AValue;
end;

procedure TfmCHXFrame.SetOnLoadFrameData(AValue: TCHXFrameDataUpdate);
begin
  if FOnLoadFrameData = AValue then
    Exit;
  FOnLoadFrameData := AValue;
end;

procedure TfmCHXFrame.SetOnLoadGUIConfig(AValue: TCHXUseGUIConfigIni);
begin
  if FOnLoadGUIConfig = AValue then
    Exit;
  FOnLoadGUIConfig := AValue;
end;

procedure TfmCHXFrame.SetOnLoadGUIIcons(AValue: TCHXUseIconsConfigIni);
begin
  if FOnLoadGUIIcons = AValue then
    Exit;
  FOnLoadGUIIcons := AValue;
end;

procedure TfmCHXFrame.SetOnSaveGUIConfig(AValue: TCHXUseGUIConfigIni);
begin
  if FOnSaveGUIConfig = AValue then
    Exit;
  FOnSaveGUIConfig := AValue;
end;

class function TfmCHXFrame.GenSimpleModalForm(aCHXFrame: TfmCHXFrame;
  const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aForm: TfrmCHXForm;
begin
  Result := mrNone;
  if not Assigned(aCHXFrame) then
    Exit;
  if aFormName = '' then
    Exit;
  if aFormTitle = '' then
    Exit;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := aFormName;
    aForm.Caption := aFormTitle;

    // Removing from previous owner...
    if Assigned(aCHXFrame.Owner) then
      aCHXFrame.Owner.RemoveComponent(aCHXFrame);
    // ... assigning to aForm
    aForm.InsertComponent(aCHXFrame);

    aCHXFrame.Align := alClient;
    aCHXFrame.Parent := aForm;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;

end;

class procedure TfmCHXFrame.GenSimpleForm(aCHXFrame: TfmCHXFrame;
  const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string);
var
  aForm: TfrmCHXForm;
begin
  if not Assigned(aCHXFrame) then
    Exit;
  if aFormName = '' then
    Exit;
  if aFormTitle = '' then
    Exit;

  Application.CreateForm(TfrmCHXForm, aForm);

  aForm.Name := aFormName;
  aForm.Caption := aFormTitle;

  // Removing from previous owner...
  if Assigned(aCHXFrame.Owner) then
    aCHXFrame.Owner.RemoveComponent(aCHXFrame);
  // ... assigning to aForm
  aForm.InsertComponent(aCHXFrame);

  aCHXFrame.Align := alClient;
  aCHXFrame.Parent := aForm;

  aForm.LoadGUIConfig(aGUIConfigIni);
  aForm.LoadGUIIcons(aGUIIconsIni);
  aForm.Show;
end;

procedure TfmCHXFrame.LoadGUIConfig(aIniFile: TIniFile);

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
  i: integer;
begin
  if not Assigned(aIniFile) then
    Exit;

  if Assigned(OnLoadGUIConfig) then
    OnLoadGUIConfig(aIniFile);

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    LoadGUIConfigChildren(Components[i], aIniFile);
    Inc(i);
  end;
end;

procedure TfmCHXFrame.SaveGUIConfig(aIniFile: TIniFile);

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
  i: integer;
begin
  if not Assigned(aIniFile) then
    Exit;
  if Assigned(OnSaveGUIConfig) then
    OnSaveGUIConfig(aIniFile);

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    SaveGUIConfigChildren(Components[i], aIniFile);
    Inc(i);
  end;
end;

procedure TfmCHXFrame.LoadGUIIcons(aIconsIni: TIniFile; aBaseFolder: string);

  procedure LoadGUIIconsChildren(const aComponent: TComponent;
    aIconsIni: TIniFile; aBaseFolder: string);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).LoadGUIIcons(aIconsIni, aBaseFolder);
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        LoadGUIIconsChildren(aComponent.Components[i], aIconsIni, aBaseFolder);
        Inc(i);
      end;
    end;
  end;

var
  i: integer;
begin
  if not assigned(aIconsIni) then
    Exit;

  if Assigned(OnLoadGUIIcons) then
    OnLoadGUIIcons(aIconsIni, aBaseFolder);

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    LoadGUIIconsChildren(Components[i], aIconsIni, aBaseFolder);
    Inc(i);
  end;

  FixComponentImagesFromActions(Self);
end;

procedure TfmCHXFrame.ClearFrameData;
begin
  if Assigned(OnClearFrameData) then
    OnClearFrameData;
end;

procedure TfmCHXFrame.LoadFrameData;
begin
  if Assigned(OnLoadFrameData) then
    OnLoadFrameData;
end;

constructor TfmCHXFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Enabled := False; // Created disabled, enable it when ready for use
end;

destructor TfmCHXFrame.Destroy;
begin
  inherited Destroy;
end;

end.
