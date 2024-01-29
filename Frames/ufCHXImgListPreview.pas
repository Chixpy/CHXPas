unit ufCHXImgListPreview;
{< TfmCHXImgListPreview frame unit.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, ExtCtrls, LazFileUtils, IniFiles,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXFileListPreview, ufCHXImgViewer;

resourcestring
  rsNoImageLoaded = 'No image loaded.';
  rsImageProps = '%2:s: %0:d x %1:d';

type

  { TfmCHXImgListPreview }

  TfmCHXImgListPreview = class(TfmCHXFileListPreview)
    iImage: TImage;
    lImageProps: TLabel;
    procedure iImageDblClick(Sender: TObject);

  private
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FSHA1Folder: string;
    procedure SetGUIConfigIni(const AValue: string);
    procedure SetGUIIconsIni(const AValue: string);
    procedure SetSHA1Folder(const AValue: string);

  protected
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;

    procedure OnCurrItemChange; override;

    procedure DoLoadGUIIcons(aIniFile: TIniFile; const aBaseFolder: string);
      override;
    procedure DoLoadGUIConfig(aIniFile: TIniFile); override;

  public
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    procedure ClearFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXImgListPreview }

procedure TfmCHXImgListPreview.iImageDblClick(Sender: TObject);
begin
  if assigned(FileList) and (ItemCount > 0) then
    TfmCHXImgViewer.SimpleFormIL(FileList, SHA1Folder, ItemIndex,
      GUIIconsIni, GUIConfigIni);
end;

procedure TfmCHXImgListPreview.SetSHA1Folder(const AValue: string);
begin
  if DirectoryExistsUTF8(AValue) then
    FSHA1Folder := SetAsFolder(AValue)
  else
    FSHA1Folder := EmptyStr;
end;

procedure TfmCHXImgListPreview.SetGUIConfigIni(const AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmCHXImgListPreview.SetGUIIconsIni(const AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmCHXImgListPreview.OnCurrItemChange;
begin
  if (ItemIndex < 0) or (not Assigned(FileList)) or (FileList.Count = 0) then
  begin
    iImage.Picture.Clear;
    lImageProps.Caption := rsNoImageLoaded;
    Exit;
  end;

  iImage.Picture.LoadFromFile(FileList[ItemIndex]);
  lImageProps.Caption := Format(rsImageProps, [iImage.Picture.Width,
    iImage.Picture.Height, ExtractFileExt(FileList[ItemIndex])]);
end;

procedure TfmCHXImgListPreview.DoLoadGUIIcons(aIniFile: TIniFile;
  const aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIniFile, aBaseFolder);

  GUIIconsIni := aIniFile.FileName;
end;

procedure TfmCHXImgListPreview.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  Inherited DoLoadGUIConfig(aIniFile);

  GUIConfigIni := aIniFile.FileName;
end;

procedure TfmCHXImgListPreview.ClearFrameData;
begin
  inherited ClearFrameData;

  iImage.Picture.Clear;
end;

constructor TfmCHXImgListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXImgListPreview.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmCHXImgListPreview);

finalization
  UnRegisterClass(TfmCHXImgListPreview);
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
