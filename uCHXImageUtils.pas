{ Copyright (C) 2006-2016 Chixpy

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

{ Custom image utils. }
unit uCHXImageUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Menus, ActnList, Graphics,
  IniFiles, LazFileUtils, LazUTF8, Buttons, ImgList,
  // Custom
  uCHXStrUtils;

procedure ReadActionsIcons(const aFileName, Section: string;
  ImageList: TImageList; ActionList: TCustomActionList);
{< Reads icons for the diferent actions a ImageList and assigns them.

  It reads a .ini file to search which images must be loaded, relative paths
    are searched from BaseDir.

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aFileName Filename of a ini file where the icons filenames are
    stored.)
  @param(Section Section where nfo will be searched.)
  @param(ImageList An image list where images are stored)
  @param(ActionList An action list which actions will be assigned an image.)
}

procedure ReadMenuIcons(const aFileName, Section: string; ImageList: TImageList;
  Menu: TMenu);
{< Reads icons for menu items with no action assigned and assigns them.

  It reads a .ini file to search which images must be loaded, relative paths
    are searched from BaseDir (or relative to ini file).

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aFileName Filename of a ini file where the icons filenames are
    stored.)
  @param(Section Section where info will be searched.)
  @param(ImageList An image list where images are stored)
  @param(Menu An menu which its items will be assigned an image.)
}

procedure FixComponentImagesFromActions(aComponent: TComponent);
{ Assings images from actions to components (and subcomponents).

  Some components (TBitButton, SpeedButton, ...) load their glyphs directly;
    and when a TAction is assigned, them don't load de corresponding image
    because they are not TImageList dependant.

  This procedure loads their glyphs from the image list assigned to their
    action.}

function AddToImageList(aImageList: TImageList;
  const FileName: string): integer;

function CorrectAspectRatio(OrigRect: TRect; aImage: TPicture): TRect;
{< Returns a TRect with the correct aspect ratio for the picture inside the
  OrigRect.
}

implementation

procedure ReadActionsIcons(const aFileName, Section: string;
  ImageList: TImageList; ActionList: TCustomActionList);
var
  IniFile: TMemIniFile;
  Cont: integer;
  IconFile: string;
  BaseDir: string;
begin
  if aFileName = '' then
    Exit;
  if Section = '' then
    Exit;
  if not Assigned(ImageList) then
    Exit;

  BaseDir := ExtractFilePath(aFileName);
  ActionList.Images := ImageList;

  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    Cont := 0;
    while Cont < ActionList.ActionCount do
    begin
      IconFile := IniFile.ReadString(Section,
        ActionList.Actions[Cont].Name, '');
      if IconFile = '' then
      begin
        IconFile := ActionList.Actions[Cont].Name + '.png';
        IniFile.WriteString(Section, ActionList.Actions[Cont].Name, IconFile);
        IniFile.UpdateFile;
      end;
      TCustomAction(ActionList.Actions[Cont]).ImageIndex :=
        AddToImageList(ImageList, SetAsFolder(BaseDir) + IconFile);
      Inc(Cont);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure ReadMenuIcons(const aFileName, Section: string; ImageList: TImageList;
  Menu: TMenu);

  procedure ReadIcon(IniFile: TMemIniFile; ImageList: TImageList;
    Menu: TMenuItem; Section: string; BaseDir: string);
  var
    IconFile: string;
    Cont: integer;
  begin
    if not (Menu.IsLine or Assigned(Menu.Action)) then
    begin
      IconFile := IniFile.ReadString(Section, Menu.Name, '');
      if IconFile = '' then
      begin
        IconFile := Menu.Name + '.png';
        IniFile.WriteString(Section, Menu.Name, IconFile);
        IniFile.UpdateFile;
      end;
      Menu.ImageIndex := AddToImageList(ImageList, BaseDir + IconFile);
    end;

    Cont := 0;
    while Cont < Menu.Count do
    begin
      ReadIcon(IniFile, ImageList, Menu.Items[Cont], Section, BaseDir);
      Inc(Cont);
    end;
  end;

  //procedure ReadMenuIcons(const aFileName, Section, BaseDir: String;
  //  ImageList: TImageList; Menu: TMenu);
var
  BaseDir: string;
  IniFile: TMemIniFile;
  Cont: integer;
begin
  if aFileName = '' then
    Exit;
  if Section = '' then
    Exit;
  if not Assigned(ImageList) then
    Exit;

  BaseDir := ExtractFilePath(aFileName);

  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    Cont := 0;
    while Cont < Menu.Items.Count do
    begin
      ReadIcon(IniFile, ImageList, Menu.Items[Cont], Section, BaseDir);
      Inc(Cont);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure FixComponentImagesFromActions(aComponent: TComponent);

  procedure FixComponent(aComponent: TComponent);
  var
    ImageList: TCustomImageList;
    aAction: TCustomAction;
  begin
    if aComponent is TCustomBitBtn then
    begin
      with aComponent as TCustomBitBtn do
      begin
        if Assigned(Action) then
        begin
          // Not safe...
          aAction := TCustomAction(Action);
          ImageList := aAction.ActionList.Images;
          if (ImageList <> nil) and (aAction.Imageindex >= 0) then
            ImageList.GetBitmap(aAction.Imageindex, Glyph);
        end;
      end;
    end
    else
    begin
      if aComponent is TCustomSpeedButton then
      begin
        with aComponent as TCustomSpeedButton do
        begin
          if Assigned(Action) then
          begin
            // Not safe...
            aAction := TCustomAction(Action);
            ImageList := aAction.ActionList.Images;
            if (ImageList <> nil) and (aAction.Imageindex >= 0) then
              ImageList.GetBitmap(aAction.Imageindex, Glyph);
          end;
        end;
      end;
    end;
  end;

var
  i: integer;
begin
  i := 0;
  while i < aComponent.ComponentCount do
  begin
    FixComponent(aComponent.Components[i]);
    Inc(i);
  end;
end;

function AddToImageList(aImageList: TImageList;
  const FileName: string): integer;
var
  Image: TPicture;
  Extension: string;
begin
  Result := -1;
  if aImageList = nil then
    Exit;
  if not FileExistsUTF8(FileName) then
    Exit;

  Image := TPicture.Create;
  try
    Image.LoadFromFile(FileName);
    // Cutrada para que los iconos se dibujen transparentes...
    Extension := ExtractFileExt(FileName);
    if (Extension = '.ico') or (Extension = '.icns') or
      (Extension = '.cur') then
      Result := aImageList.AddMasked(Image.Bitmap,
        Image.Icon.TransparentColor)
    else
      Result := aImageList.Add(Image.PNG, nil);
  finally
    FreeAndNil(Image);
  end;
end;

function CorrectAspectRatio(OrigRect: TRect; aImage: TPicture): TRect;
var
  Adjustment: integer;
begin
  Result := OrigRect;

  if aImage.Width > aImage.Height then
  begin
    if aImage.Width = 0 then
      Exit;
    // Crazy formula, don't ask
    Adjustment := Round(((OrigRect.Right - OrigRect.Left) *
      (1 - (aImage.Height / aImage.Width))) / 2);
    Result.Top := OrigRect.Top + Adjustment;
    Result.Bottom := OrigRect.Bottom - Adjustment;
  end
  else
  begin
    if aImage.Height = 0 then
      Exit;
    Adjustment := Round(((OrigRect.Bottom - OrigRect.Top) *
      (1 - (aImage.Width / aImage.Height))) / 2);
    Result.Left := OrigRect.Left + Adjustment;
    Result.Right := OrigRect.Right - Adjustment;
  end;
end;

end.
