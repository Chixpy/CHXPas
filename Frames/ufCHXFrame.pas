unit ufCHXFrame;

{< TfmCHXFrame frame unit.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, inifiles,
  // CHX units
  uCHXImageUtils;

type
  { TfmCHXFrame }
  TfmCHXFrame = class(TFrame)
  private

  protected
    procedure DoLoadGUIConfig(aIniFile: TIniFile); virtual;
    procedure DoSaveGUIConfig(aIniFile: TIniFile); virtual;

    procedure DoLoadGUIIcons(aIniFile: TIniFile;
      const aBaseFolder: string); virtual;

    class function GenSimpleModalFormDontFree(aCHXFrame: TfmCHXFrame;
      const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni:
      string): integer;
    {< Generic function to create a Modal Form from a frame,
      without freeing the frame at Form Close; so we can read any value
      from it and return it.
    }

    class function GenSimpleModalForm(aCHXFrame: TfmCHXFrame;
      const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni:
      string): integer;
    {< Generic function to create a Modal Form from a frame. }

    class procedure GenSimpleForm(aCHXFrame: TfmCHXFrame;
      const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string);
    {< Generic procedure to create a Normal Form from a frame. }

  public
    procedure ClearFrameData; virtual;
    procedure LoadFrameData; virtual;

    procedure LoadGUIConfig(aIniFile: TIniFile);
    //< Load GUI config.
    procedure SaveGUIConfig(aIniFile: TIniFile);
    //< Save GUI config.

    procedure LoadGUIIcons(aIconsIni: TIniFile; aBaseFolder: string);
    //< Load GUI icons.

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
procedure TfmCHXFrame.ClearFrameData;
begin
  // To override if needed
end;

procedure TfmCHXFrame.LoadFrameData;
begin
  // To override if needed
end;

procedure TfmCHXFrame.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  // To override if needed
end;

procedure TfmCHXFrame.DoSaveGUIConfig(aIniFile: TIniFile);
begin
  // To override if needed
end;

procedure TfmCHXFrame.DoLoadGUIIcons(aIniFile: TIniFile;
  const aBaseFolder: string);
begin
  // To override if needed
end;

class function TfmCHXFrame.GenSimpleModalFormDontFree(aCHXFrame: TfmCHXFrame;
  const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aForm: TfrmCHXForm;
  aOwner: TComponent;
begin
  Result := mrNone;
  if not Assigned(aCHXFrame) then
    Exit;
  if aFormName = EmptyStr then
    Exit;
  if aFormTitle = EmptyStr then
    Exit;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := aFormName;
    aForm.Caption := aFormTitle;

    // Removing from previous owner...
    aOwner := aCHXFrame.Owner;
    if Assigned(aOwner) then
      aCHXFrame.Owner.RemoveComponent(aCHXFrame);

    // ...assigning to aForm...
    aForm.InsertComponent(aCHXFrame);

    aCHXFrame.Align := alClient;
    aCHXFrame.Parent := aForm;

    // ...so, we can read GUI config and icons
    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);

    // Restoring previous Owner
    aForm.RemoveComponent(aCHXFrame);
    if Assigned(aOwner) then
      aOwner.InsertComponent(aCHXFrame);

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

class function TfmCHXFrame.GenSimpleModalForm(aCHXFrame: TfmCHXFrame;
  const aFormName, aFormTitle, aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aForm: TfrmCHXForm;
begin
  Result := mrNone;
  if not Assigned(aCHXFrame) then
    Exit;
  if aFormName = EmptyStr then
    Exit;
  if aFormTitle = EmptyStr then
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
  if aFormName = EmptyStr then
    Exit;
  if aFormTitle = EmptyStr then
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

  DoLoadGUIConfig(aIniFile);

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
  DoSaveGUIConfig(aIniFile);

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

  DoLoadGUIIcons(aIconsIni, aBaseFolder);

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    LoadGUIIconsChildren(Components[i], aIconsIni, aBaseFolder);
    Inc(i);
  end;

  FixComponentImagesFromActions(Self);
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

initialization
  RegisterClass(TfmCHXFrame);

finalization
  UnRegisterClass(TfmCHXFrame);

end.
{
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
