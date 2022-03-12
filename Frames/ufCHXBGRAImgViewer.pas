unit ufCHXBGRAImgViewer;

{< TfmCHXBGRAImgViewer frame unit.

  Copyright (C) 2020-2022 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  Menus, Math, BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXFrame;

type

  { TfmCHXBGRAImgViewer }

  TfmCHXBGRAImgViewer = class(TfmCHXFrame)
    actAutoZoom: TAction;
    actOriginalSize: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    alImgViewer: TActionList;
    ilImgViewer: TImageList;
    mipmZoomOut: TMenuItem;
    mipmZoomIn: TMenuItem;
    mipmOriginalSize: TMenuItem;
    mipmAutoZoom: TMenuItem;
    pbxImage: TPaintBox;
    pmImgViewer: TPopupMenu;
    sbxImage: TScrollBox;
    procedure actAutoZoomExecute(Sender: TObject);
    procedure actOriginalSizeExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure pbxImagePaint(Sender: TObject);
    procedure pbxImageResize(Sender: TObject);
    procedure sbxImageResize(Sender: TObject);

  private
    FActualImage: TBGRABitmap;
    FAutoCenterOnLoad: boolean;
    FAutoZoomOnLoad: boolean;
    FVisibleImage: TBGRABitmap;
    FZoom: integer;
    procedure SetAutoCenterOnLoad(AValue: boolean);
    procedure SetAutoZoomOnLoad(AValue: boolean);
    procedure SetVisibleImage(AValue: TBGRABitmap);
    procedure SetZoom(AValue: integer);

  protected
    property VisibleImage: TBGRABitmap
      read FVisibleImage write SetVisibleImage;
    {< Visible image with zoom, selection, effects... }

    procedure SetActualImage(AValue: TBGRABitmap); virtual;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

    procedure DrawImage;

    procedure AfterDrawImage; virtual;

    procedure OnZoomChange; virtual;

  public
    property ActualImage: TBGRABitmap read FActualImage write SetActualImage;
    {< Actual image. }

    property AutoZoomOnLoad: boolean read FAutoZoomOnLoad
      write SetAutoZoomOnLoad;
    {< Change Zoom on load image. }
    property AutoCenterOnLoad: boolean
      read FAutoCenterOnLoad write SetAutoCenterOnLoad;
    {< Center image when loaded. }
    property Zoom: integer read FZoom write SetZoom;
    {< Percentaje Zoom * 100. }

    procedure ZoomIn;
    procedure ZoomOut;
    procedure AutoZoom;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXBGRAImgViewer }

procedure TfmCHXBGRAImgViewer.pbxImagePaint(Sender: TObject);
begin
  if not Assigned(VisibleImage) then
    Exit;

  VisibleImage.Draw(pbxImage.Canvas, 0, 0, False);
end;

procedure TfmCHXBGRAImgViewer.actZoomInExecute(Sender: TObject);
begin
  ZoomIn;
end;

procedure TfmCHXBGRAImgViewer.actAutoZoomExecute(Sender: TObject);
begin
  AutoZoom;
end;

procedure TfmCHXBGRAImgViewer.actOriginalSizeExecute(Sender: TObject);
begin
  Zoom := 100;
end;

procedure TfmCHXBGRAImgViewer.actZoomOutExecute(Sender: TObject);
begin
  ZoomOut;
end;

procedure TfmCHXBGRAImgViewer.pbxImageResize(Sender: TObject);
begin
  FreeAndNil(FVisibleImage);
end;

procedure TfmCHXBGRAImgViewer.sbxImageResize(Sender: TObject);
begin
  FreeAndNil(FVisibleImage);

  if AutoZoomOnLoad then
    AutoZoom
  else
    DrawImage;
end;

procedure TfmCHXBGRAImgViewer.SetActualImage(AValue: TBGRABitmap);
begin
  if FActualImage = AValue then
    Exit;
  FActualImage := AValue;

  LoadFrameData;
end;

procedure TfmCHXBGRAImgViewer.SetAutoZoomOnLoad(AValue: boolean);
begin
  if FAutoZoomOnLoad = AValue then
    Exit;
  FAutoZoomOnLoad := AValue;

  FreeAndNil(FVisibleImage);

  if AutoZoomOnLoad then
    AutoZoom
  else
    DrawImage;
end;

procedure TfmCHXBGRAImgViewer.SetAutoCenterOnLoad(AValue: boolean);
begin
  if FAutoCenterOnLoad = AValue then
    Exit;
  FAutoCenterOnLoad := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetVisibleImage(AValue: TBGRABitmap);
begin
  if FVisibleImage = AValue then
    Exit;
  FVisibleImage := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetZoom(AValue: integer);
begin
  FreeAndNil(FVisibleImage);

  if not Assigned(ActualImage) then
  begin
    FZoom := AValue;
    // lZoomInput.Caption := Format('%dx', [ZoomInput]);
    Exit;
  end;

  // Checking very high zoom, to avoid memory overflow
  while ((Max(ActualImage.Width, ActualImage.Height) div 100) * AVAlue) >
    (2 ** 14) do
    Dec(AValue);

  if AValue <= 0 then
    AValue := 1;

  FZoom := AValue;

  OnZoomChange;

  DrawImage;
end;

procedure TfmCHXBGRAImgViewer.DoLoadFrameData;
begin
  // Updating Zoom, if it's to high for new image.
  if AutoZoomOnLoad then
    AutoZoom
  else
    SetZoom(Zoom);

  DrawImage;

  Enabled := Assigned(ActualImage);
end;

procedure TfmCHXBGRAImgViewer.DoClearFrameData;
begin
  FreeAndNil(FVisibleImage);
  Zoom := 100;
  ActualImage := nil;
  Enabled := False;
end;

procedure TfmCHXBGRAImgViewer.DrawImage;
var
  Temp: TBGRABitmap;
  ZWidth, ZHeight: integer;
begin
  FreeAndNil(FVisibleImage);

  if not Assigned(ActualImage) then
  begin
    pbxImage.ClientWidth := 0;
    pbxImage.ClientHeight := 0;
    pbxImage.Top := 0;
    pbxImage.Left := 0;
    Exit;
  end;

  ZWidth := (ActualImage.Width * Zoom) div 100;
  ZHeight := (ActualImage.Height * Zoom) div 100;

  if (pbxImage.ClientWidth <> ZWidth) or
    (pbxImage.ClientHeight <> ZHeight) then
  begin
    pbxImage.ClientWidth := ZWidth;
    pbxImage.ClientHeight := ZHeight;

    if AutoCenterOnLoad then
    begin
      if sbxImage.ClientHeight > pbxImage.Height then
        pbxImage.Top := (sbxImage.ClientHeight - pbxImage.Height) shr 1;
      if sbxImage.ClientWidth > pbxImage.Width then
        pbxImage.Left := (sbxImage.ClientWidth - pbxImage.Width) shr 1;
    end
    else
    begin
      pbxImage.Top := 0;
      pbxImage.Left := 0;
    end;
  end;

  FVisibleImage := TBGRABitmap.Create(ZWidth, ZHeight);

  //  case rgbBackGround.ItemIndex of
  //    1: ; // Transparent
  //    2: // Color
  //      VisibleImage.Fill(ColorToBGRA(cbxColorBackground.Selected));
  //    else
  //      // Checker
  VisibleImage.DrawCheckers(Rect(0, 0, ZWidth, ZHeight),
    BGRA(224, 224, 224), BGRA(192, 192, 192));
  //  end;

  Temp := ActualImage.Resample(ZWidth, ZHeight, rmSimpleStretch);

  VisibleImage.PutImage(0, 0, Temp, dmDrawWithTransparency);

  Temp.Free;

  AfterDrawImage;

  pbxImage.Invalidate;
end;

procedure TfmCHXBGRAImgViewer.AfterDrawImage;
begin

end;

procedure TfmCHXBGRAImgViewer.OnZoomChange;
begin

end;

procedure TfmCHXBGRAImgViewer.ZoomIn;
begin
  FreeAndNil(FVisibleImage);

  Zoom := Zoom * 2;
end;

procedure TfmCHXBGRAImgViewer.ZoomOut;
begin
  FreeAndNil(FVisibleImage);

  Zoom := Zoom div 2;
end;

procedure TfmCHXBGRAImgViewer.AutoZoom;
var
  i: integer;
begin
  FreeAndNil(FVisibleImage);

  if not assigned(ActualImage) then
    Exit;

  i := Min((sbxImage.ClientWidth * 100) div ActualImage.Width,
    (sbxImage.ClientHeight * 100) div ActualImage.Height);
  if i < 1 then
    i := 1;

  Zoom := i;
end;

constructor TfmCHXBGRAImgViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Zoom := 100;
  AutoCenterOnLoad := True;

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCHXBGRAImgViewer.Destroy;
begin
  FreeAndNil(FVisibleImage);

  inherited Destroy;
end;

end.
