{ Copyright (C) 2006-2018 Chixpy

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
  IniFiles, LazFileUtils, LazUTF8, Buttons, ImgList, typinfo,
  // CHX units
  uCHXStrUtils;

procedure ReadActionsIconsIni(aIniFile: TIniFile; aBaseFolder: string;
  const aSection: string; aImageList: TImageList;
  aActionList: TCustomActionList);
{< Reads icons for the diferent actions a ImageList and assigns them.

  It reads from inifile to search which images must be loaded, relative paths
    are searched from aBaseFolder.

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aIniFile Inifile where the icons filenames are stored.)
  @param(aBaseFolder Base folder.)
  @param(Section Section where info will be searched.)
  @param(ImageList An image list where images are stored)
  @param(ActionList An action list which actions will be assigned an image.)
}
procedure ReadActionsIconsFile(const aFileName, aSection: string;
  aImageList: TImageList; aActionList: TCustomActionList);
{< Reads icons for the diferent actions a ImageList and assigns them.

  It reads a .ini file to search which images must be loaded, relative paths
    are searched from aFileName folder.

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aFileName Filename of a ini file where the icons filenames are
    stored.)
  @param(Section Section where info will be searched.)
  @param(ImageList An image list where images are stored)
  @param(ActionList An action list which actions will be assigned an image.)
}

procedure ReadMenuIconsFile(const aFileName, aSection: string;
  aImageList: TImageList; aMenu: TMenu);
{< Reads icons for menu items with no action assigned and assigns them.

It reads a .ini file to search which images must be loaded, relative paths
  are searched from aFileName folder.

  If ini file don't have the necesary key=value pair, then it will be created.

  @param(aFileName Filename of a ini file where the icons filenames are
    stored.)
  @param(Section Section where info will be searched.)
  @param(ImageList An image list where images are stored)
  @param(Menu An menu which its items will be assigned an image.)
}

procedure ReadMenuIconsIni(aIniFile: TIniFile; aBaseFolder: string;
  const aSection: string; aImageList: TImageList; aMenu: TMenu);

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

// Font related
procedure SaveFontToIni(FStream: TIniFile; Section, Key: string; smFont: TFont);
procedure LoadFontFromIni(FStream: TIniFile; Section, Key: string; smFont: TFont);

implementation

procedure ReadActionsIconsIni(aIniFile: TIniFile; aBaseFolder: string;
  const aSection: string; aImageList: TImageList;
  aActionList: TCustomActionList);
var
  Cont: integer;
  IconFile: String;
begin
  if aSection = '' then
    Exit;
  if not Assigned(aImageList) then
    Exit;
  if not Assigned(aActionList) then
    Exit;

  // To be sure that aImageList is assigned to aActionList
  aActionList.Images := aImageList;
  aBaseFolder := SetAsFolder(aBaseFolder);

  Cont := 0;
  while Cont < aActionList.ActionCount do
  begin
    IconFile := aIniFile.ReadString(aSection,
      aActionList.Actions[Cont].Name, '');

    if IconFile = '' then
    begin
      IconFile := aActionList.Actions[Cont].Name + '.png';
      aIniFile.WriteString(aSection, aActionList.Actions[Cont].Name, IconFile);
      aIniFile.UpdateFile;
    end;

    // Only override default icon if already exists.
    if FileExistsUTF8(aBaseFolder + IconFile) then
    begin
      TCustomAction(aActionList.Actions[Cont]).ImageIndex :=
        AddToImageList(aImageList, aBaseFolder + IconFile);
    end;

    Inc(Cont);
  end;
end;

procedure ReadActionsIconsFile(const aFileName, aSection: string;
  aImageList: TImageList; aActionList: TCustomActionList);
var
  IniFile: TMemIniFile;
  BaseDir: string;
begin
  if not FileExistsUTF8(aFileName) then
    Exit;

  BaseDir := ExtractFilePath(aFileName);

  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    ReadActionsIconsIni(IniFile, BaseDir, aSection, aImageList, aActionList);
  finally
    IniFile.Free;
  end;
end;

procedure ReadMenuIconsFile(const aFileName, aSection: string;
  aImageList: TImageList; aMenu: TMenu);
var
  BaseDir: string;
  IniFile: TMemIniFile;
begin
  if aFileName = '' then
    Exit;

  BaseDir := ExtractFilePath(aFileName);

  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    ReadMenuIconsIni(IniFile, BaseDir, aSection, aImageList, aMenu);
  finally
    IniFile.Free;
  end;
end;

procedure ReadMenuIconsIni(aIniFile: TIniFile; aBaseFolder: string;
  const aSection: string; aImageList: TImageList; aMenu: TMenu);

  procedure ReadMenuItemIcon(aIniFile: TIniFile; aImageList: TImageList;
    aMenu: TMenuItem; aSection: string; aBaseDir: string);
  var
    IconFile: string;
    Cont: integer;
  begin
    if not (aMenu.IsLine or Assigned(aMenu.Action)) then
    begin
      IconFile := aIniFile.ReadString(aSection, aMenu.Name, '');
      if IconFile = '' then
      begin
        IconFile := aMenu.Name + '.png';
        aIniFile.WriteString(aSection, aMenu.Name, IconFile);
        aIniFile.UpdateFile;
      end;
      aMenu.ImageIndex := AddToImageList(aImageList, aBaseDir + IconFile);
    end;

    Cont := 0;
    while Cont < aMenu.Count do
    begin
      ReadMenuItemIcon(aIniFile, aImageList, aMenu.Items[Cont], aSection, aBaseDir);
      Inc(Cont);
    end;
  end;

//procedure ReadMenuIconsIni(aIniFile: TIniFile; aBaseFolder: string;
//  const aSection: string; aImageList: TImageList; aMenu: TMenu);
var
  Cont: integer;
begin
  if (not Assigned(aIniFile)) or
  (aSection = '') or
  (not Assigned(aImageList)) or
  (not Assigned(aMenu)) then
    Exit;

  // To be sure that aImageList is assigned to aMenu
  aMenu.Images := aImageList;
  aBaseFolder := SetAsFolder(aBaseFolder);

    Cont := 0;
    while Cont < aMenu.Items.Count do
    begin
      ReadMenuItemIcon(aIniFile, aImageList, aMenu.Items[Cont], aSection, aBaseFolder);
      Inc(Cont);
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
          if (Assigned(ImageList)) and (aAction.Imageindex >= 0) then
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
            if (Assigned(ImageList)) and (aAction.Imageindex >= 0) then
            begin
              ImageList.GetBitmap(aAction.Imageindex, Glyph);
              ShowCaption := False; // Speed buttons -> no caption
            end;
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
  if not Assigned(aImageList) then
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

  if not Assigned(aImage) then
    Exit;

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

procedure SaveFontToIni(FStream: TIniFile; Section, Key: string; smFont: TFont);
var
  FontProps: TStringList;
begin
  if not Assigned(FStream) then Exit;
  if not Assigned(smFont) then Exit;

  FontProps := TStringList.Create;
  try
    FontProps.Add(smFont.Name);
    FontProps.Add(IntToStr(smFont.CharSet));
    FontProps.Add(IntToStr(smFont.Color));
    FontProps.Add(IntToStr(smFont.Size));

    FontProps.Add(SetToString(GetPropInfo(smFont, 'Style'), LongInt(smFont.Style),True));


    FStream.WriteString(Section, Key, FontProps.CommaText);
finally
    FontProps.Free;
  end;
end;

procedure LoadFontFromIni(FStream: TIniFile; Section, Key: string; smFont: TFont);
var
  FontProps: TStringList;
begin
  if not Assigned(FStream) then Exit;
  if not Assigned(smFont) then Exit;
    FontProps := TStringList.Create;
  try
    FontProps.CommaText := FStream.ReadString(Section, Key, ',,,,');
    while FontProps.Count < 5 do
      FontProps.Add('');

    if FontProps[0] <> '' then
        smFont.Name := FontProps[0];
    if FontProps[1] <> '' then
    smFont.Charset := TFontCharSet(StrToIntDef(FontProps[1], smFont.Charset));
    if FontProps[2] <> '' then
     smFont.Color := TColor(StrToIntDef(FontProps[2], smFont.Color));
    if FontProps[3] <> '' then
        smFont.Size := StrToIntDef(FontProps[3], smFont.Size);
    if FontProps[4] <> '' then
        smFont.Style := TFontStyles(StringToSet(GetPropInfo(smFont, 'Style'), FontProps[4]));

  finally
      FontProps.Free;
  end;
end;


end.
