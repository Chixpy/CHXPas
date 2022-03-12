unit ufCHXBGRAImgViewerEx;

{< TfmCHXBGRAImgViewerEx frame unit.

  Copyright (C) 2021-2022 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Math, BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXBGRAImgViewer;

const
  kFmtZoom = '%d%%';
  kFmtPoint = '%d:%d';
  kFmtColor = '%3d, %3d, %3d, %3d';
  kFmtRectSize = '%d x %d';

type

  TMouseActionDrag = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; aRect: TRect) of object;

  TMouseActionMode = (
    maiNone,              // Don't interact
    maiMouseClick,        // Notify mouse clicks (press and release button)
    maiMouseSelectRect,   // Notify rectangle selections (on finish drag)
    maiMouseSelectingRect // Internal state while dragging
    );

  { TfmCHXBGRAImgViewerEx }

  TfmCHXBGRAImgViewerEx = class(TfmCHXBGRAImgViewer)
    StatusBar: TStatusBar;

    procedure pbxImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pbxImageMouseLeave(Sender: TObject);
    procedure pbxImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure pbxImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  private
    FMouseActionMode: TMouseActionMode;
    FOnImgMouseDown: TMouseEvent;
    FOnImgMouseDrag: TMouseActionDrag;
    FOnImgMouseUp: TMouseEvent;

    procedure SetMouseActionMode(AValue: TMouseActionMode);
    procedure SetOnImgMouseDown(AValue: TMouseEvent);
    procedure SetOnImgMouseDrag(AValue: TMouseActionDrag);
    procedure SetOnImgMouseUp(AValue: TMouseEvent);

  protected
    procedure SetActualImage(AValue: TBGRABitmap); override;

    procedure OnZoomChange; override;

    procedure AfterDrawImage; override;

    function ZoomedRect: TRect;

  public
    SelectionRect: TRect;

    property MouseActionMode: TMouseActionMode
      read FMouseActionMode write SetMouseActionMode;

    property OnImgMouseDown: TMouseEvent
      read FOnImgMouseDown write SetOnImgMouseDown;
    property OnImgMouseUp: TMouseEvent
      read FOnImgMouseUp write SetOnImgMouseUp;
    property OnImgMouseDrag: TMouseActionDrag
      read FOnImgMouseDrag write SetOnImgMouseDrag;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXBGRAImgViewerEx }

procedure TfmCHXBGRAImgViewerEx.pbxImageMouseLeave(Sender: TObject);
begin
  StatusBar.Panels[1].Text := '';
end;

procedure TfmCHXBGRAImgViewerEx.pbxImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ImgX, ImgY: integer;
begin
  ImgX := (X * 100) div Zoom;
  ImgY := (Y * 100) div Zoom;

  //ImgX := trunc((X / Zoom) * 100);
  //ImgY := trunc((Y / Zoom) * 100);

  case MouseActionMode of
    maiNone: ;

    maiMouseClick:
    begin
      if Assigned(OnImgMouseDown) then
        OnImgMouseDown(Self, Button, Shift, ImgX, ImgY);
    end;

    maiMouseSelectRect:
      case Button of
        mbLeft:
        begin
          SelectionRect.Create(ImgX, ImgY, ImgX, ImgY);

          StatusBar.Panels[1].Text :=
            Format(kFmtRectSize, [SelectionRect.Width, SelectionRect.Height]);

          StatusBar.Panels[2].Text :=
            Format(kFmtPoint, [SelectionRect.Left, SelectionRect.Top]);

          MouseActionMode := maiMouseSelectingRect;
        end;
        else
          ;
      end;

    maiMouseSelectingRect: ;

    else
      ;
  end;

end;

procedure TfmCHXBGRAImgViewerEx.pbxImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  ImgX, ImgY: integer;
  aPixel: PBGRAPixel;
  CurrSelection: TRect;
begin
  ImgX := (X * 100) div Zoom;
  ImgY := (Y * 100) div Zoom;

  //ImgX := trunc((X / Zoom) * 100);
  //ImgY := trunc((Y / Zoom) * 100);

  case MouseActionMode of
    maiNone: ;

    maiMouseClick: ;

    maiMouseSelectRect: ;

    maiMouseSelectingRect:
    begin
      CurrSelection := ZoomedRect;
      CurrSelection.Offset(-sbxImage.HorzScrollBar.Position,
        -sbxImage.VertScrollBar.Position);
      pbxImage.Canvas.DrawFocusRect(CurrSelection);

      SelectionRect.Right := ImgX + 1;
      SelectionRect.Bottom := ImgY + 1;

      StatusBar.Panels[1].Text :=
        Format(kFmtRectSize, [SelectionRect.Width, SelectionRect.Height]);

      StatusBar.Panels[2].Text :=
        Format(kFmtPoint, [SelectionRect.Left, SelectionRect.Top]);

      CurrSelection := ZoomedRect;
      CurrSelection.Offset(-sbxImage.HorzScrollBar.Position,
        -sbxImage.VertScrollBar.Position);

      pbxImage.Canvas.DrawFocusRect(CurrSelection);
    end;

    else
      ;
  end;

  if MouseActionMode <> maiMouseSelectingRect then
  begin
    if InRange(ImgX, 0, ActualImage.Width - 1) and
      InRange(ImgY, 0, ActualImage.Height - 1) then
    begin
      aPixel := ActualImage.ScanLine[ImgY] + ImgX;
      StatusBar.Panels[1].Text :=
        Format(kFmtColor, [aPixel^.red, aPixel^.green,
        aPixel^.blue, aPixel^.alpha]);

      StatusBar.Panels[2].Text := Format(kFmtPoint, [ImgX, ImgY]);
    end;
  end;
end;

procedure TfmCHXBGRAImgViewerEx.pbxImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ImgX, ImgY: integer;

begin
  ImgX := (X * 100) div Zoom;
  ImgY := (Y * 100) div Zoom;

  case MouseActionMode of
    maiNone: ;

    maiMouseClick: if Assigned(OnImgMouseUp) then
        OnImgMouseUp(Self, Button, Shift, ImgX, ImgY);

    maiMouseSelectRect: ;

    maiMouseSelectingRect:
      if Button = mbLeft then
      begin
        SelectionRect.NormalizeRect;

        pbxImage.Canvas.DrawFocusRect(ZoomedRect);

        if SelectionRect.isEmpty then
        begin
          StatusBar.Panels[1].Text := '';
          StatusBar.Panels[2].Text := '';
        end;

        DrawImage;

        if Assigned(OnImgMouseDrag) then
        begin
          SelectionRect.Intersect(TRect.Create(0, 0, ActualImage.Width,
            ActualImage.Height));
          OnImgMouseDrag(Self, Button, Shift, SelectionRect);
        end;

        MouseActionMode := maiMouseSelectRect;
      end
      else
      begin
        SelectionRect.NormalizeRect;

        if Assigned(OnImgMouseDrag) then
          OnImgMouseDrag(Self, Button, Shift, SelectionRect);

        MouseActionMode := maiMouseSelectRect;
      end;
  end;
end;

procedure TfmCHXBGRAImgViewerEx.SetMouseActionMode(AValue: TMouseActionMode);
begin
  if FMouseActionMode = AValue then
    Exit;
  FMouseActionMode := AValue;
end;

procedure TfmCHXBGRAImgViewerEx.SetOnImgMouseDown(AValue: TMouseEvent);
begin
  if FOnImgMouseDown = AValue then
    Exit;
  FOnImgMouseDown := AValue;
end;

procedure TfmCHXBGRAImgViewerEx.SetOnImgMouseDrag(AValue: TMouseActionDrag);
begin
  if FOnImgMouseDrag = AValue then
    Exit;
  FOnImgMouseDrag := AValue;
end;

procedure TfmCHXBGRAImgViewerEx.SetOnImgMouseUp(AValue: TMouseEvent);
begin
  if FOnImgMouseUp = AValue then
    Exit;
  FOnImgMouseUp := AValue;
end;

procedure TfmCHXBGRAImgViewerEx.SetActualImage(AValue: TBGRABitmap);
begin
  inherited SetActualImage(AValue);

  SelectionRect := TRect.Empty;
end;

procedure TfmCHXBGRAImgViewerEx.OnZoomChange;
begin
  inherited OnZoomChange;

  StatusBar.Panels[0].Text := Format(kFmtZoom, [Zoom]);
end;

procedure TfmCHXBGRAImgViewerEx.AfterDrawImage;
begin
  inherited AfterDrawImage;

  SelectionRect.NormalizeRect;

  if not SelectionRect.isEmpty then
    VisibleImage.Rectangle(ZoomedRect, BGRA(255, 0, 255, 255), dmSet);
end;

function TfmCHXBGRAImgViewerEx.ZoomedRect: TRect;
begin
  // Result := SelectionRect;
  Result.Left := (SelectionRect.Left * Zoom) div 100;
  Result.Right := (SelectionRect.Right * Zoom) div 100;
  Result.Top := (SelectionRect.Top * Zoom) div 100;
  Result.Bottom := (SelectionRect.Bottom * Zoom) div 100;
  Result.NormalizeRect;
end;

constructor TfmCHXBGRAImgViewerEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXBGRAImgViewerEx.Destroy;
begin
  inherited Destroy;
end;

end.
