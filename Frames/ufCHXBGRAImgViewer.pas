unit ufCHXBGRAImgViewer;

{< TfmCHXBGRAImgViewer frame unit.

  (C) 2020-2024 Chixpy https://github.com/Chixpy
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  Menus, Math, BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXFrame, Types;

type

  TCHXBGRAIVBackground = (bkTransparent, bkColor, bkChecker);

  { TfmCHXBGRAImgViewer }

  { TODO : Instead of drawing full image, only process and draw visible part
      of the image. Will be Faster? }

  TfmCHXBGRAImgViewer = class(TfmCHXFrame)
    actAutoZoom : TAction;
    actIntegerZoom : TAction;
    actOriginalSize : TAction;
    actZoomOutX2 : TAction;
    actZoomInX2 : TAction;
    alImgViewer : TActionList;
    ilImgViewer : TImageList;
    mipmIntegerZoom : TMenuItem;
    mipmZoomOut : TMenuItem;
    mipmZoomIn : TMenuItem;
    mipmOriginalSize : TMenuItem;
    mipmAutoZoom : TMenuItem;
    pbxImage : TPaintBox;
    pmImgViewer : TPopupMenu;
    sbxImage : TScrollBox;
    Separator1 : TMenuItem;
    procedure actAutoZoomExecute(Sender : TObject);
    procedure actIntegerZoomExecute(Sender : TObject);
    procedure actOriginalSizeExecute(Sender : TObject);
    procedure actZoomInX2Execute(Sender : TObject);
    procedure actZoomOutX2Execute(Sender : TObject);
    procedure pbxImageMouseWheelDown(Sender : TObject; Shift : TShiftState;
      MousePos : TPoint; var Handled : Boolean);
    procedure pbxImageMouseWheelUp(Sender : TObject; Shift : TShiftState;
      MousePos : TPoint; var Handled : Boolean);
    procedure pbxImagePaint(Sender : TObject);

  private
    FActualImage : TBGRABitmap;
    FAutoCenterOnLoad : Boolean;
    FAutoZoomEnabled : Boolean;
    FAutoZoomOnLoad : Boolean;
    FBackgroundChecker : TBGRAPixel;
    FBackgroundColor : TBGRAPixel;
    FBackgroundType : TCHXBGRAIVBackground;
    FCenterEnabled : Boolean;
    FIntegerZoom : Boolean;
    FPopUpMenuEnabled : Boolean;
    FVisibleImage : TBGRABitmap;
    FZoom : LongWord;
    procedure SetAutoCenterOnLoad(const AValue : Boolean);
    procedure SetAutoZoomEnabled(const AValue : Boolean);
    procedure SetAutoZoomOnLoad(const AValue : Boolean);
    procedure SetBackgroundChecker(const AValue : TBGRAPixel);
    procedure SetBackgroundColor(const AValue : TBGRAPixel);
    procedure SetBackgroundType(const AValue : TCHXBGRAIVBackground);
    procedure SetCenterEnabled(const AValue : Boolean);
    procedure SetIntegerZoom(const AValue : Boolean);
    procedure SetPopUpMenuEnabled(const AValue : Boolean);
    procedure SetVisibleImage(const AValue : TBGRABitmap);
    procedure SetZoom(const AValue : LongWord);

  protected
    property VisibleImage : TBGRABitmap read FVisibleImage
      write SetVisibleImage;
    {< Visible image with zoom, selection, effects... }

    property CenterEnabled : Boolean
      read FCenterEnabled write SetCenterEnabled;
    {< Image was moved manually, so don't center it on draw.

       This is different than AutoCenterOnLoad. }

    property AutoZoomEnabled : Boolean
      read FAutoZoomEnabled write SetAutoZoomEnabled;
    {< Image size was changed manually, so don't center in on draw.

       This is different than AutoZoomOnLoad. }

    procedure SetActualImage(const AValue : TBGRABitmap); virtual;
    {< Sets actual image. }

    procedure AfterDrawImage; virtual;
    {< Virtual method called after image is drawed.

       For example: To add a selection box or other effects. }

    procedure OnZoomChange; virtual;
    {< Virtual method called after image is drawed.

       For example: Show current zoom value. }

  public
    property ActualImage : TBGRABitmap read FActualImage write SetActualImage;
    {< Actual image. }

    property BackgroundType : TCHXBGRAIVBackground
      read FBackgroundType write SetBackgroundType;
    {< Type of background used draw the image:
         - bkTransparent: Keep transparency
         - bkColor: Use BackgroundColor.
         - bkChecker: Use BackgroundColor and BackgroundChecker.

       This backgroung it's only on draw, it don't change actual image.
    }
    property BackgroundColor : TBGRAPixel
      read FBackgroundColor write SetBackgroundColor;
    {< Color of background used if BackgroundType = bkColor, and one of the
         colors used if BackgroundType = bkChecker }
    property BackgroundChecker : TBGRAPixel
      read FBackgroundChecker write SetBackgroundChecker;
    {< Second color used if BackgroundType = bkChecker }
    property AutoZoomOnLoad : Boolean read FAutoZoomOnLoad
      write SetAutoZoomOnLoad;
    {< Change Zoom on load image, stretching to frame size. }
    property AutoCenterOnLoad : Boolean
      read FAutoCenterOnLoad write SetAutoCenterOnLoad;
    {< Center image when image loaded. }
    property IntegerZoom : Boolean read FIntegerZoom write SetIntegerZoom;
    {< Keep zoom in a integer value (divisible by 100). }
    property Zoom : LongWord read FZoom write SetZoom;
    {< Zoom of image (in percentage). }
    property PopUpMenuEnabled : Boolean
      read FPopUpMenuEnabled write SetPopUpMenuEnabled;
    {< Shows default popup menu on right click? }

    procedure DrawImage;
    {< Draws or updates image. }

    procedure ZoomInX2;
    {< Zooms image in by a factor of 2. }
    procedure ZoomOutX2;
    {< Zooms image out by a factor of 2. }
    procedure AutoZoom;
    {< Stretchs image to frame size. }

    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXBGRAImgViewer }

procedure TfmCHXBGRAImgViewer.pbxImagePaint(Sender : TObject);
begin
  if not Assigned(VisibleImage) then Exit;

  VisibleImage.Draw(pbxImage.Canvas, 0, 0, False);
end;

procedure TfmCHXBGRAImgViewer.actZoomInX2Execute(Sender : TObject);
begin
  ZoomInX2;
end;

procedure TfmCHXBGRAImgViewer.actAutoZoomExecute(Sender : TObject);
begin
  AutoZoom;
end;

procedure TfmCHXBGRAImgViewer.actIntegerZoomExecute(Sender : TObject);
begin
  IntegerZoom := actIntegerZoom.Checked;
end;

procedure TfmCHXBGRAImgViewer.actOriginalSizeExecute(Sender : TObject);
begin
  Zoom := 100;
end;

procedure TfmCHXBGRAImgViewer.actZoomOutX2Execute(Sender : TObject);
begin
  ZoomOutX2;
end;

procedure TfmCHXBGRAImgViewer.pbxImageMouseWheelDown(Sender : TObject;
  Shift : TShiftState; MousePos : TPoint; var Handled : Boolean);
begin
  ZoomOutX2;
end;

procedure TfmCHXBGRAImgViewer.pbxImageMouseWheelUp(Sender : TObject;
  Shift : TShiftState; MousePos : TPoint; var Handled : Boolean);
begin
  ZoomInX2;
end;

procedure TfmCHXBGRAImgViewer.SetActualImage(const AValue : TBGRABitmap);
begin
  if FActualImage = AValue then Exit;
  FActualImage := AValue;

  LoadFrameData;
end;

procedure TfmCHXBGRAImgViewer.SetAutoZoomOnLoad(const AValue : Boolean);
begin
  if FAutoZoomOnLoad = AValue then Exit;
  FAutoZoomOnLoad := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetBackgroundChecker(const AValue : TBGRAPixel);
begin
  if FBackgroundChecker = AValue then Exit;
  FBackgroundChecker := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetBackgroundColor(const AValue : TBGRAPixel);
begin
  if FBackgroundColor = AValue then Exit;
  FBackgroundColor := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetBackgroundType(
  const AValue : TCHXBGRAIVBackground);
begin
  if FBackgroundType = AValue then Exit;
  FBackgroundType := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetCenterEnabled(const AValue : Boolean);
begin
  if FCenterEnabled = AValue then Exit;
  FCenterEnabled := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetIntegerZoom(const AValue : Boolean);
begin
  if FIntegerZoom = AValue then Exit;
  FIntegerZoom := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetPopUpMenuEnabled(const AValue : Boolean);
begin
  if FPopUpMenuEnabled = AValue then Exit;
  FPopUpMenuEnabled := AValue;

  if PopUpMenuEnabled then
    sbxImage.PopupMenu := pmImgViewer
  else
    sbxImage.PopupMenu := nil;
end;

procedure TfmCHXBGRAImgViewer.SetAutoCenterOnLoad(const AValue : Boolean);
begin
  if FAutoCenterOnLoad = AValue then Exit;
  FAutoCenterOnLoad := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetAutoZoomEnabled(const AValue : Boolean);
begin
  if FAutoZoomEnabled = AValue then Exit;
  FAutoZoomEnabled := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetVisibleImage(const AValue : TBGRABitmap);
begin
  if FVisibleImage = AValue then Exit;
  FVisibleImage := AValue;
end;

procedure TfmCHXBGRAImgViewer.SetZoom(const AValue : LongWord);
begin
  if AValue = 0 then
    FZoom := 1
  else if aValue > 384000 then
    // A single pixel is bigger that the screen in 4K!! (3840x2160)
    FZoom := 384000
  else
    FZoom := AValue;

  if IntegerZoom then
  begin
    FZoom := FZoom - (FZoom mod 100);
    if FZoom < 100 then
       FZoom := 100;
  end;

  OnZoomChange;

  AutoZoomEnabled := False;

  DrawImage;
end;

procedure TfmCHXBGRAImgViewer.LoadFrameData;
begin
  inherited;

  Enabled := Assigned(ActualImage);

  if not Enabled then
  begin
    DrawImage; // Clear current image
    Exit;
  end;

  // Updating Zoom, if it's to high for new image.
  AutoZoomEnabled := AutoZoomOnLoad;
  if AutoZoomOnLoad then
    AutoZoom
  else
    SetZoom(Zoom);
end;

procedure TfmCHXBGRAImgViewer.ClearFrameData;
begin
  inherited;

  Zoom := 100;
  ActualImage := nil;
  Enabled := False;
  DrawImage;
end;

procedure TfmCHXBGRAImgViewer.DrawImage;
var
  Temp : TBGRABitmap;
  ZWidth, ZHeight : LongWord;
begin
  FreeAndNil(FVisibleImage);

  // There is not ActualImage
  if not Assigned(ActualImage) then
  begin
    pbxImage.ClientWidth := 0;
    pbxImage.ClientHeight := 0;
    pbxImage.Top := 0;
    pbxImage.Left := 0;
    Exit;
  end;

  // Really big image...
  ZWidth := (ActualImage.Width * Zoom) div 100;
  if ZWidth > MaxLongint then
  begin
    ZWidth := ActualImage.Width div MaxLongint;
    Zoom :=  ZWidth * 100;
  end;
  ZHeight := (ActualImage.Height * Zoom) div 100;
  if ZHeight > MaxLongint then
  begin
    ZHeight := ActualImage.Height div MaxLongint;
    Zoom :=  ZHeight * 100;
  end;

  if (pbxImage.ClientWidth <> ZWidth) or
    (pbxImage.ClientHeight <> ZHeight) then
  begin
    pbxImage.ClientWidth := ZWidth;
    pbxImage.ClientHeight := ZHeight;

    if CenterEnabled then
    begin
      if sbxImage.ClientHeight > pbxImage.Height then
        pbxImage.Top := (sbxImage.ClientHeight - pbxImage.Height) div 2;
      if sbxImage.ClientWidth > pbxImage.Width then
        pbxImage.Left := (sbxImage.ClientWidth - pbxImage.Width) div 2;
    end
    else
    begin
      pbxImage.Top := 0;
      pbxImage.Left := 0;
    end;
  end;

  FVisibleImage := TBGRABitmap.Create(ZWidth, ZHeight);

  case BackgroundType of
    bkColor : VisibleImage.Fill(BackgroundColor);
    bkChecker : VisibleImage.DrawCheckers(Rect(0, 0, ZWidth, ZHeight),
        BackgroundColor, BackgroundChecker);
    else //bkTransparent
      ;
  end;

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

procedure TfmCHXBGRAImgViewer.ZoomInX2;
begin
  FreeAndNil(FVisibleImage);

  Zoom := Zoom shl 1; // * 2
end;

procedure TfmCHXBGRAImgViewer.ZoomOutX2;
begin
  FreeAndNil(FVisibleImage);

  Zoom := Zoom shr 1; // div 2
end;

procedure TfmCHXBGRAImgViewer.AutoZoom;
var
  i : integer;
begin
  FreeAndNil(FVisibleImage);

  if not assigned(ActualImage) then
    Exit;

  i := Floor(Min((sbxImage.ClientWidth * 100) / ActualImage.Width,
    (sbxImage.ClientHeight * 100) / ActualImage.Height));

  Zoom := i;

  AutoZoomEnabled := True;
end;

constructor TfmCHXBGRAImgViewer.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);

  ActualImage := nil;
  VisibleImage := nil;

  Zoom := 100;
  IntegerZoom := False;
  AutoZoomOnLoad := True;
  AutoZoomEnabled := True;
  AutoCenterOnLoad := True;
  AutoCenterOnLoad := True;
  PopUpMenuEnabled := True;

  BackgroundType := bkTransparent;
  BackgroundColor := BGRA(0, 0, 0, 0);
  BackgroundChecker := BGRA(128, 128, 128);
end;

destructor TfmCHXBGRAImgViewer.Destroy;
begin
  FreeAndNil(FVisibleImage);

  inherited Destroy;
end;

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
