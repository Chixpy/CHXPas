unit ufCHXImgViewer;
{< TfmCHXImgViewer frame unit.

  Copyright (C) 2006-2019 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, ExtCtrls, LazFileUtils, IniFiles, DateUtils,
  // CHX units
  uCHXStrUtils, uCHXFileUtils,
  // CHX frames
  ufCHXFileListPreview,
  // CHX forms
  ufrCHXForm;

type

  { TfmCHXImgViewer }

  TfmCHXImgViewer = class(TfmCHXFileListPreview)
    actClose: TAction;
    actToggleFileList: TAction;
    actStretch: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    actOriginalSize: TAction;
    Image: TImage;
    lbxFiles: TListBox;
    sbInfo: TStatusBar;
    sbxImage: TScrollBox;
    splFileList: TSplitter;
    tbTogleFileList: TToolButton;
    tbZoomIn: TToolButton;
    tbOriginalSize: TToolButton;
    tbZoomOut: TToolButton;
    tbStretch: TToolButton;
    tbSepZoom: TToolButton;
    procedure actOriginalSizeExecute(Sender: TObject);
    procedure actStretchExecute(Sender: TObject);
    procedure actToggleFileListExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageResize(Sender: TObject);
    procedure lbxFilesSelectionChange(Sender: TObject; User: boolean);
    procedure sbxImageResize(Sender: TObject);

  private
    FDragBeginX: longint;
    FDragBeginY: longint;
    FSHA1: string;
    FSHA1Folder: string;
    FStartTime: TTime;
    procedure SetDragBeginX(const aDragBeginX: longint);
    procedure SetDragBeginY(const aDragBeginY: longint);
    procedure SetSHA1(const aSHA1: string);
    procedure SetSHA1Folder(const aSHA1Folder: string);
    procedure SetStartTime(const aStartTime: TTime);

  protected
    procedure OnCurrItemChange; override;

    property DragBeginX: longint read FDragBeginX write SetDragBeginX;
    property DragBeginY: longint read FDragBeginY write SetDragBeginY;

    property SHA1: string read FSHA1 write SetSHA1;
    property StartTime: TTime read FStartTime write SetStartTime;

    procedure StretchImage;
    procedure FixPosition;

    procedure ChangeImage;
    procedure UpdateStatusBar;

    procedure SaveStats;

  public
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    {< Folder to store picture visualization statistics. }

    // Creates a form with image viewer.
    class function SimpleFormIL(aImageList: TStrings;
      aSHA1Folder: string; aCurrItem: integer; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;
    class function SimpleFormI(aImage: string; aSHA1Folder: string;
      aGUIIconsIni: string; aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXImgViewer }

procedure TfmCHXImgViewer.actOriginalSizeExecute(Sender: TObject);
begin
  actStretch.Checked := False;
  Image.Height := Image.Picture.Height;
  Image.Width := Image.Picture.Width;
  FixPosition;
  sbxImage.HorzScrollBar.Position := Image.Width shr 1;
  sbxImage.VertScrollBar.Position := Image.Height shr 1;
end;

procedure TfmCHXImgViewer.actStretchExecute(Sender: TObject);
begin
  if actStretch.Checked then
    StretchImage;

  FixPosition;
end;

procedure TfmCHXImgViewer.actToggleFileListExecute(Sender: TObject);
begin
  splFileList.Visible := actToggleFileList.Checked;
  lbxFiles.Visible := actToggleFileList.Checked;
end;

procedure TfmCHXImgViewer.actZoomInExecute(Sender: TObject);
var
  CorrectX, CorrectY: integer;
begin
  actStretch.Checked := False;
  CorrectX := 0;
  CorrectY := 0;
  if Image.Left > 0 then
    CorrectX := sbxImage.ClientWidth - Image.Width;
  if Image.Top > 0 then
    CorrectY := sbxImage.ClientHeight - Image.Height;
  Image.Height := Image.Height shl 1;
  Image.Width := Image.Width shl 1;
  FixPosition;
  sbxImage.HorzScrollBar.Position :=
    -CorrectX + (sbxImage.HorzScrollBar.Position shl 1) +
    (sbxImage.ClientWidth shr 1);
  sbxImage.VertScrollBar.Position :=
    -CorrectY + (sbxImage.VertScrollBar.Position shl 1) +
    (sbxImage.ClientHeight shr 1);
end;

procedure TfmCHXImgViewer.actZoomOutExecute(Sender: TObject);
begin
  actStretch.Checked := False;
  sbxImage.HorzScrollBar.Position :=
    (sbxImage.HorzScrollBar.Position shr 1) - (sbxImage.ClientWidth shr 2);
  sbxImage.VertScrollBar.Position :=
    (sbxImage.VertScrollBar.Position shr 1) - (sbxImage.ClientHeight shr 2);
  Image.Height := Image.Height shr 1;
  Image.Width := Image.Width shr 1;
  FixPosition;
end;

procedure TfmCHXImgViewer.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft:
    begin
      DragBeginX := X;
      DragBeginY := Y;
    end;
    else
      ;
  end;
end;

procedure TfmCHXImgViewer.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft:
    begin
      sbxImage.VertScrollBar.Position :=
        sbxImage.VertScrollBar.Position + (DragBeginY - Y);
      sbxImage.HorzScrollBar.Position :=
        sbxImage.HorzScrollBar.Position + (DragBeginX - X);
    end;
    else
      ;
  end;
end;

procedure TfmCHXImgViewer.ImageResize(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TfmCHXImgViewer.lbxFilesSelectionChange(Sender: TObject;
  User: boolean);
begin
  if User then
    ItemIndex := lbxFiles.ItemIndex;
end;

procedure TfmCHXImgViewer.sbxImageResize(Sender: TObject);
begin
  if actStretch.Checked then
    StretchImage;
  FixPosition;
end;

procedure TfmCHXImgViewer.SetDragBeginX(const aDragBeginX: longint);
begin
  if FDragBeginX = aDragBeginX then
    Exit;
  FDragBeginX := aDragBeginX;
end;

procedure TfmCHXImgViewer.SetDragBeginY(const aDragBeginY: longint);
begin
  if FDragBeginY = aDragBeginY then
    Exit;
  FDragBeginY := aDragBeginY;
end;

procedure TfmCHXImgViewer.SetSHA1(const aSHA1: string);
begin
  if FSHA1 = aSHA1 then
    Exit;
  FSHA1 := aSHA1;
end;

procedure TfmCHXImgViewer.SetSHA1Folder(const aSHA1Folder: string);
begin
  if DirectoryExistsUTF8(aSHA1Folder) then
    FSHA1Folder := SetAsFolder(aSHA1Folder)
  else
    FSHA1Folder := '';
end;

procedure TfmCHXImgViewer.SetStartTime(const aStartTime: TTime);
begin
  if FStartTime = aStartTime then
    Exit;
  FStartTime := aStartTime;
end;

procedure TfmCHXImgViewer.OnCurrItemChange;
begin
    ChangeImage;
end;

procedure TfmCHXImgViewer.StretchImage;
var
  Factor: extended;
begin
  // Factor of the stretched image
  if Image.Picture.Height = 0 then
    Factor := 0
  else
    Factor := sbxImage.ClientHeight / Image.Picture.Height;

  if Image.Picture.Height <> 0 then
  begin
    if Factor > sbxImage.ClientWidth / Image.Picture.Width then
      Factor := sbxImage.ClientWidth / Image.Picture.Width;
  end;

  Image.Height := trunc(image.Picture.Height * Factor);
  Image.Width := trunc(image.Picture.Width * Factor);
end;

procedure TfmCHXImgViewer.FixPosition;
begin
  // Horizontal position
  if Image.Width > sbxImage.ClientWidth then
  begin
    Image.Left := 0;
  end
  else
  begin
    Image.Left := (sbxImage.ClientWidth - Image.Width) shr 1; // div 2
  end;

  // Vertical position
  if Image.Height > sbxImage.ClientHeight then
  begin
    Image.Top := 0;
  end
  else
  begin
    Image.Top := (sbxImage.ClientHeight - Image.Height) shr 1; // div 2
  end;
end;

procedure TfmCHXImgViewer.ChangeImage;
var
  aFilename: string;
begin
  if (SHA1Folder <> '') then
  begin
    SaveStats;
    SHA1 := '';
    StartTime := 0;
  end;

  if (ItemIndex < 0) or (ItemCount <= 0) then
  begin
    Image.Picture.Clear;
    UpdateStatusBar;
    Exit;
  end;

  aFilename := FileList[ItemIndex];

  if FileExistsUTF8(aFilename) then
  begin
    if (SHA1Folder <> '') then
      SHA1 := SHA1FileStr(aFilename);
    Image.Picture.LoadFromFile(aFilename);
    StretchImage;
    if (SHA1Folder <> '') then
      StartTime := Now;
    UpdateStatusBar;
  end
  else
  begin
    Image.Picture.Clear;
    FileList.Delete(ItemIndex);
    ChangeImage;
  end;
end;

procedure TfmCHXImgViewer.UpdateStatusBar;
begin
  if ItemIndex >= 0 then
  begin
    sbInfo.Panels[0].Text :=
      IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
    sbInfo.Panels[1].Text :=
      ' (' + IntToStr(Image.Width) + 'x' + IntToStr(Image.Height) + ')';
    sbInfo.Panels[2].Text := FileList[ItemIndex];
  end
  else
  begin
    sbInfo.Panels[0].Text := '';
    sbInfo.Panels[1].Text := '';
    sbInfo.Panels[2].Text := '';
  end;

  actToggleFileList.Enabled := ItemCount > 1;
end;

procedure TfmCHXImgViewer.SaveStats;
var
  TimePassed: int64;
  NTimes: int64;
  aFileName: string;
  aIni: TMemIniFile;
begin
  if (SHA1Folder = '') or (StartTime = 0) or (SHA1 = '') then
    Exit;

  TimePassed := SecondsBetween(Now, StartTime);
  aFileName := SHA1Folder + SetAsFolder(copy(SHA1, 1, 1)) +
    copy(SHA1, 1, 3) + '.ini';

  ForceDirectories(ExtractFileDir(aFileName));

  aIni := TMemIniFile.Create(aFileName);
  try
    TimePassed := TimePassed + aIni.ReadInt64(SHA1, 'Pic.TimeViewed', 0);
    NTimes := 1 + aIni.ReadInt64(SHA1, 'Pic.NTimes', 0);
    aIni.WriteInt64(SHA1, 'Pic.TimeViewed', TimePassed);
    aIni.WriteInt64(SHA1, 'Pic.NTimes', NTimes);
    { TODO : Other Stats? }
    aIni.UpdateFile;
  finally
    aIni.Free;
  end;
end;

class function TfmCHXImgViewer.SimpleFormIL(aImageList: TStrings;
  aSHA1Folder: string; aCurrItem: integer; aGUIIconsIni: string;
  aGUIConfigIni: string): integer;
var
  fmCHXImageViewer: TfmCHXImgViewer;
begin
  fmCHXImageViewer := TfmCHXImgViewer.Create(nil);

  fmCHXImageViewer.SHA1Folder := aSHA1Folder;
  fmCHXImageViewer.FileList := aImageList;
  fmCHXImageViewer.ItemIndex := aCurrItem;

  Result := GenSimpleModalForm(fmCHXImageViewer, 'frmCHXImgViewer',
    Application.Title + ': Image Viewer', aGUIConfigIni, aGUIIconsIni);
end;

class function TfmCHXImgViewer.SimpleFormI(aImage: string;
  aSHA1Folder: string; aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aImageList: TStringList;
begin
  Result := mrNone;
  if not FileExistsUTF8(aImage) then
    Exit;

  aImageList := TStringList.Create;
  try
    aImageList.Add(aImage);
    Result := SimpleFormIL(aImageList, aSHA1Folder, 1, aGUIIconsIni,
      aGUIConfigIni);
  finally
    aImageList.Free;
  end;
end;

constructor TfmCHXImgViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXImgViewer.Destroy;
begin
  if (SHA1Folder <> '') then
    SaveStats;

  inherited Destroy;
end;

initialization
  RegisterClass(TfmCHXImgViewer);

finalization
  UnRegisterClass(TfmCHXImgViewer);
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
