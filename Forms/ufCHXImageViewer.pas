{ Copyright (C) 2006-2015 Chixpy

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

{ Unit of a simple Image Viewer. }
unit ufCHXImageViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, vte_stringlist, LResources, Forms, Controls,
  Graphics, Dialogs, ActnList, ComCtrls, Spin, ExtCtrls, StdCtrls, Menus,
  Buttons, ShellCtrls,
  // Custom
  uCHXImageUtils, uCHXStrUtils;

type

  { TODO : Make it as a Frame }

  { TfrmCHXImageViewer }

  TfrmCHXImageViewer = class(TForm)
    actClose: TAction;
    actFirst: TAction;
    actShowFileList: TAction;
    actNext: TAction;
    actPrevious: TAction;
    actLast: TAction;
    actStretch: TAction;
    actOriginalSize: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    ActionList: TActionList;
    eCurrImage: TSpinEdit;
    ilActions: TImageList;
    Image: TImage;
    lTotalImages: TLabel;
    mClose: TMenuItem;
    mOriginalSize: TMenuItem;
    mZoomIn: TMenuItem;
    mZoomOut: TMenuItem;
    pmAction: TPopupMenu;
    sbInfo: TStatusBar;
    Splitter: TSplitter;
    tbImage: TToolBar;
    bSep1: TToolButton;
    tbZoomIn: TToolButton;
    tbOriginalSize: TToolButton;
    tbZoomOut: TToolButton;
    tbClose: TToolButton;
    bSep2: TToolButton;
    bStretch: TToolButton;
    bFirst: TToolButton;
    bPrevious: TToolButton;
    bNext: TToolButton;
    bLast: TToolButton;
    bSep3: TToolButton;
    bShowFileList: TToolButton;
    bSep4: TToolButton;
    vmImages: TVirtualMemo;
    procedure actCloseExecute(Sender: TObject);
    procedure actFirstExecute(Sender: TObject);
    procedure actShowFileListExecute(Sender: TObject);
    procedure actLastExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actOriginalSizeExecute(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
    procedure actStretchExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure eCurrImageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageResize(Sender: TObject);

  private
    FDragBeginX: longint;
    FDragBeginY: longint;
    FIconsIniFile: string;
    procedure SetDragBeginX(AValue: longint);
    procedure SetDragBeginY(AValue: longint);
    procedure SetIconsIniFile(AValue: TFilename);

  protected
    property DragBeginX: longint read FDragBeginX write SetDragBeginX;
    property DragBeginY: longint read FDragBeginY write SetDragBeginY;

    procedure ChangeImage;
    procedure FixPosition;

    procedure DisableStretch;
    procedure StretchImage;

  public
    property IconsIniFile: TFilename read FIconsIniFile write SetIconsIniFile;

    procedure LoadIcons(aIconIniFile: TFilename); deprecated;

    procedure AddImages(aImageList: TStrings; Index: integer = 0);
    procedure AddImage(aImageFile: string; aObject: TObject = nil);
  end;

var
  frmCHXImageViewer: TfrmCHXImageViewer;

implementation

{ TfrmCHXImageViewer }

procedure TfrmCHXImageViewer.actZoomInExecute(Sender: TObject);
begin
  DisableStretch;

  // Trying to do the zoom around the center of the visible zone..
  if Image.Left <= 0 then
    Image.Left := Image.Left - Image.Width shr 1;
  if Image.Top <= 0 then
    Image.Top := Image.Top - Image.Height shr 1;

  Image.Height := Image.Height shl 1;
  Image.Width := Image.Width shl 1;
  FixPosition;
end;

procedure TfrmCHXImageViewer.actOriginalSizeExecute(Sender: TObject);
begin
  DisableStretch;
  Image.Height := Image.Picture.Height;
  Image.Width := Image.Picture.Width;
  FixPosition;
end;

procedure TfrmCHXImageViewer.actPreviousExecute(Sender: TObject);
begin
  if eCurrImage.Value > 1 then
    eCurrImage.Value := eCurrImage.Value - 1;
end;

procedure TfrmCHXImageViewer.actStretchExecute(Sender: TObject);
begin
  // Inverted logic because actStretch is (un)checked before
  //   execute the action.
  if actStretch.Checked then
    StretchImage
  else
  begin
    DisableStretch;
    FixPosition;
  end;
end;

procedure TfrmCHXImageViewer.actCloseExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmCHXImageViewer.actFirstExecute(Sender: TObject);
begin
  eCurrImage.Value := 1;
  // ChangeImage; OnChange is called;
end;

procedure TfrmCHXImageViewer.actShowFileListExecute(Sender: TObject);
begin
  Splitter.Visible := actShowFileList.Checked;
  vmImages.Visible := actShowFileList.Checked;
end;

procedure TfrmCHXImageViewer.actLastExecute(Sender: TObject);
begin
  eCurrImage.Value := vmImages.Lines.Count;
  // ChangeImage; OnChange is called;
end;

procedure TfrmCHXImageViewer.actNextExecute(Sender: TObject);
begin
  if eCurrImage.Value < vmImages.Lines.Count then
    eCurrImage.Value := eCurrImage.Value + 1;
end;

procedure TfrmCHXImageViewer.actZoomOutExecute(Sender: TObject);
begin
  DisableStretch;
  // Trying to do the zoom around the center of the visible zone..
  if Image.Left <= 0 then
    Image.Left := Image.Left + Image.Width shr 2;
  if Image.Top <= 0 then
    Image.Top := Image.Top + Image.Height shr 2;
  Image.Height := Image.Height shr 1;
  Image.Width := Image.Width shr 1;
  FixPosition;
end;

procedure TfrmCHXImageViewer.eCurrImageChange(Sender: TObject);
begin
  ChangeImage;
end;

procedure TfrmCHXImageViewer.FormCreate(Sender: TObject);
begin
  Self.Caption := Application.Title + ': ' + Self.Caption;
end;

procedure TfrmCHXImageViewer.FormResize(Sender: TObject);
begin
  FixPosition;
end;

procedure TfrmCHXImageViewer.ImageDblClick(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmCHXImageViewer.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft:
    begin
      DragBeginX := X;
      DragBeginY := Y;
    end;
  end;
end;

procedure TfrmCHXImageViewer.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft:
    begin
      Image.Left := Image.Left + X - DragBeginX;
      Image.Top := Image.Top + Y - DragBeginY;
      FixPosition;
    end;
  end;
end;

procedure TfrmCHXImageViewer.ImageResize(Sender: TObject);
begin
  if Image.Align = alClient then
    sbInfo.Panels[0].Text :=
      '(' + IntToStr(Image.Width) + 'x' + IntToStr(Image.Height) + ')';
end;

procedure TfrmCHXImageViewer.SetDragBeginX(AValue: longint);
begin
  if FDragBeginX = AValue then
    Exit;
  FDragBeginX := AValue;
end;

procedure TfrmCHXImageViewer.SetDragBeginY(AValue: longint);
begin
  if FDragBeginY = AValue then
    Exit;
  FDragBeginY := AValue;
end;

procedure TfrmCHXImageViewer.SetIconsIniFile(AValue: TFilename);
begin
  FIconsIniFile:=SetAsFile(AValue);
  ReadActionsIcons(IconsIniFile, Self.Name, '', ilActions, ActionList);
end;

procedure TfrmCHXImageViewer.LoadIcons(aIconIniFile: TFilename);
begin
  IconsIniFile := aIconIniFile;
end;

procedure TfrmCHXImageViewer.ChangeImage;
var
  aFilename: string;
begin
  if vmImages.Lines.Count = 0 then
    Exit;

  if eCurrImage.Value > vmImages.Lines.Count then
    eCurrImage.Value := vmImages.Lines.Count;

  aFilename:= vmImages.Lines[eCurrImage.Value - 1];

  if FileExistsUTF8(aFilename) then
  begin
    Image.Picture.LoadFromFile(aFilename);
    sbInfo.Panels[1].Text :=
      IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
    sbInfo.Panels[2].Text := aFilename;
    StretchImage;
  end
  else
  begin
    vmImages.Lines.Delete(eCurrImage.Value - 1);
    eCurrImage.MaxValue := vmImages.Lines.Count;
    ChangeImage;
  end;

  lTotalImages.Caption := '/ ' + IntToStr(vmImages.Lines.Count);
end;

procedure TfrmCHXImageViewer.FixPosition;
begin
  // Horizontal position
  if Image.Width > self.ClientWidth then
  begin
    if -Image.Left > (Image.Width - self.ClientWidth) then
      Image.Left := -(Image.Width - self.ClientWidth);
    if Image.Left > 0 then
      Image.Left := 0;
  end
  else
  begin
    Image.Left := (self.ClientWidth - Image.Width) div 2;
  end;

  // Vertical position
  if Image.Height > self.ClientHeight - sbInfo.Height - tbImage.Height then
  begin
    if -Image.Top > (Image.Height - self.ClientHeight + sbInfo.Height) then
      Image.Top := -(Image.Height - self.ClientHeight + sbInfo.Height);
    if Image.Top > tbImage.Height then
      Image.Top := tbImage.Height;
  end
  else
  begin
    Image.Top := ((self.ClientHeight - tbImage.Height - Image.Height) div 2) +
      tbImage.Height;
  end;

  sbInfo.Panels[0].Text := IntToStr(Image.Width) + 'x' +
    IntToStr(Image.Height);
end;

procedure TfrmCHXImageViewer.DisableStretch;
var
  factor: real;
begin
  if Image.Align <> alNone then
  begin
    // Factor of the stretched image
    factor := Image.Height / Image.Picture.Height;
    if factor < 1 then
    begin
      if factor < Image.Width / Image.Picture.Width then
        factor := Image.Height / Image.Picture.Height;
      factor := 1 / (round(1 / factor));
    end
    else
    begin
      if factor > Image.Width / Image.Picture.Width then
        factor := Image.Height / Image.Picture.Height;
      factor := round(factor);
    end;

    Image.Align := alNone;
    actStretch.Checked := False;

    Image.Height := round(image.Picture.Height * factor);
    Image.Width := round(image.Picture.Width * factor);
  end;
end;

procedure TfrmCHXImageViewer.StretchImage;
begin
  if Image.Align <> alClient then
  begin
    Image.Align := alClient;
    actStretch.Checked := True;
    sbInfo.Panels[0].Text :=
      '(' + IntToStr(Image.Width) + 'x' + IntToStr(Image.Height) + ')';
  end;
end;

procedure TfrmCHXImageViewer.AddImages(aImageList: TStrings;
  Index: integer = 0);
begin
  vmImages.Lines.AddStrings(aImageList);
  eCurrImage.MaxValue := vmImages.Lines.Count;
  eCurrImage.Value := Index + 1;
  ChangeImage;
end;

procedure TfrmCHXImageViewer.AddImage(aImageFile: string; aObject: TObject);
begin
  vmImages.Lines.AddObject(aImageFile, aObject);
  eCurrImage.MaxValue := vmImages.Lines.Count;
  ChangeImage;
end;

initialization
  {$I ufCHXImageViewer.lrs}

end.
