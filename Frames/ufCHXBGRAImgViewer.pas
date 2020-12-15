unit ufCHXBGRAImgViewer;

{< TfmCHXBGRAImgViewer frame unit.

  Copyright (C) 2020 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Math,
  BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXFrame;

type

  { TfmCHXBGRAImgViewer }

  TfmCHXBGRAImgViewer = class(TfmCHXFrame)
    pbxImage: TPaintBox;
    sbxImage: TScrollBox;
    procedure pbxImagePaint(Sender: TObject);

  private
    FActualImage: TBGRABitmap;
    FAutoZoomOnLoad: Boolean;
    FVisibleImage: TBGRABitmap;
    FZoom: integer;
    procedure SetActualImage(AValue: TBGRABitmap);
    procedure SetAutoZoomOnLoad(AValue: Boolean);
    procedure SetVisibleImage(AValue: TBGRABitmap);
    procedure SetZoom(AValue: integer);

  protected

    property VisibleImage: TBGRABitmap
      read FVisibleImage write SetVisibleImage;
    {< Visible image with zoom, selection, effects... }

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

    procedure DrawImage;

  public

    property ActualImage: TBGRABitmap read FActualImage write SetActualImage;
    {< Actual image. }

    property AutoZoomOnLoad: Boolean read FAutoZoomOnLoad write SetAutoZoomOnLoad;
    {< Change Zoom on load image}
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

procedure TfmCHXBGRAImgViewer.SetActualImage(AValue: TBGRABitmap);
begin
  if FActualImage = AValue then
    Exit;
  FActualImage := AValue;

  LoadFrameData;
end;

procedure TfmCHXBGRAImgViewer.SetAutoZoomOnLoad(AValue: Boolean);
begin
  if FAutoZoomOnLoad = AValue then Exit;
  FAutoZoomOnLoad := AValue;

  if AutoZoomOnLoad then
    AutoZoom;
end;

procedure TfmCHXBGRAImgViewer.SetVisibleImage(AValue: TBGRABitmap);
begin
  if FVisibleImage = AValue then
    Exit;
  FVisibleImage := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetZoom(AValue: integer);
begin

  if not Assigned(ActualImage) then
  begin
    FZoom := 100;
    // lZoomInput.Caption := Format('%dx', [ZoomInput]);
    Exit;
  end;

  // Checking very high zoom
  while ((max(ActualImage.Width, ActualImage.Height) div 100) * AVAlue) > (2 ** 14) do
    Dec(AValue);

  if AValue <= 0 then
    AValue := 1;

  FZoom := AValue;

  // lZoomInput.Caption := Format('%dx', [Zoom]);

  DrawImage;
end;

procedure TfmCHXBGRAImgViewer.DoLoadFrameData;
begin
  // Updating Zoom, if it's to high for new image.
  if AutoZoomOnLoad then AutoZoom else SetZoom(Zoom);

  Enabled := Assigned(ActualImage);
end;

procedure TfmCHXBGRAImgViewer.DoClearFrameData;
begin
  Zoom := 1;
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
    Exit;
  end;

  ZWidth := (ActualImage.Width * Zoom) div 100;
  ZHeight := (ActualImage.Height * Zoom) div 100;

  if (pbxImage.ClientWidth <> ZWidth) or
    (pbxImage.ClientHeight <> ZHeight) then
  begin
    pbxImage.ClientWidth := ZWidth;
    pbxImage.ClientHeight := ZHeight;
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

  //  if not SelectionInput.isEmpty then
  //    VisibleImage.Rectangle(SelectionZoom,
  //      BGRA(127, 127, 127, 255), dmXor);

  pbxImage.Invalidate;
end;

procedure TfmCHXBGRAImgViewer.ZoomIn;
begin
  Zoom := Zoom * 2;
end;

procedure TfmCHXBGRAImgViewer.ZoomOut;
begin
  Zoom := Zoom div 2;
end;

procedure TfmCHXBGRAImgViewer.AutoZoom;
var
  i: Double;
begin
   if not assigned(ActualImage) then
    Exit;

  i := Min(sbxImage.ClientWidth / ActualImage.Width * 100,
    sbxImage.ClientHeight / ActualImage.Height * 100);
  if i < 1 then
    i := 1;

  Zoom := Floor(i);
end;

constructor TfmCHXBGRAImgViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Zoom := 100;

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCHXBGRAImgViewer.Destroy;
begin
  FreeAndNil(FVisibleImage);

  inherited Destroy;
end;

end.
