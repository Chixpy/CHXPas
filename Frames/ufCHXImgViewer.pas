unit ufCHXImgViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Menus, ComCtrls,
  StdCtrls, LazUTF8, ExtCtrls, ActnList,
  uCHXStrUtils, uCHXImageUtils;

type

  { TfmCHXImgViewer }

  TfmCHXImgViewer = class(TFrame)
    actFirst: TAction;
    ActionList: TActionList;
    actLast: TAction;
    actNext: TAction;
    actOriginalSize: TAction;
    actPrevious: TAction;
    actShowFileList: TAction;
    actStretch: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    bFirst: TToolButton;
    bLast: TToolButton;
    bNext: TToolButton;
    bPrevious: TToolButton;
    bSep1: TToolButton;
    bSep3: TToolButton;
    bSep4: TToolButton;
    bShowFileList: TToolButton;
    bStretch: TToolButton;
    cbxCurrImage: TComboBox;
    ilActions: TImageList;
    Image: TImage;
    lbxFiles: TListBox;
    lTotalImages: TLabel;
    mClose: TMenuItem;
    mOriginalSize: TMenuItem;
    mZoomIn: TMenuItem;
    mZoomOut: TMenuItem;
    pmAction: TPopupMenu;
    sbInfo: TStatusBar;
    sbxImage: TScrollBox;
    Splitter: TSplitter;
    tbImage: TToolBar;
    tbOriginalSize: TToolButton;
    tbZoomIn: TToolButton;
    tbZoomOut: TToolButton;
    procedure actFirstExecute(Sender: TObject);
    procedure actLastExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actOriginalSizeExecute(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
    procedure actShowFileListExecute(Sender: TObject);
    procedure actStretchExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure cbxCurrImageSelect(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageResize(Sender: TObject);
    procedure sbxImageResize(Sender: TObject);
  private
    FDragBeginX: longint;
    FDragBeginY: longint;
    FIconsIniFile: TFilename;
    FImageIndex: integer;
    procedure SetDragBeginX(AValue: longint);
    procedure SetDragBeginY(AValue: longint);
    procedure SetIconsIniFile(AValue: TFilename);
    procedure SetImageIndex(AValue: integer);

  protected
    property DragBeginX: longint read FDragBeginX write SetDragBeginX;
    property DragBeginY: longint read FDragBeginY write SetDragBeginY;

    procedure DisableStretch;
    procedure StretchImage;
    procedure FixPosition;

    procedure ChangeImage;
    procedure UpdateStatusBar;

  public
    property IconsIniFile: TFilename read FIconsIniFile write SetIconsIniFile;
    property ImageIndex: integer read FImageIndex write SetImageIndex;

    procedure AddImages(aImageList: TStrings; Index: integer = 0);
    procedure AddImage(aImageFile: TFilename; aObject: TObject = nil);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXImgViewer }

procedure TfmCHXImgViewer.actFirstExecute(Sender: TObject);
begin
  ImageIndex := 1;
end;

procedure TfmCHXImgViewer.actLastExecute(Sender: TObject);
begin
  ImageIndex := lbxFiles.Items.Count;
end;

procedure TfmCHXImgViewer.actNextExecute(Sender: TObject);
begin
  if ImageIndex < lbxFiles.Items.Count then
    ImageIndex := ImageIndex + 1
  else
    ImageIndex := 1;
end;

procedure TfmCHXImgViewer.actOriginalSizeExecute(Sender: TObject);
begin
  DisableStretch;
  Image.Height := Image.Picture.Height;
  Image.Width := Image.Picture.Width;
  sbxImage.HorzScrollBar.Position := Image.Width shr 1;
  sbxImage.VertScrollBar.Position := Image.Height shr 1;
  FixPosition;
end;

procedure TfmCHXImgViewer.actPreviousExecute(Sender: TObject);
begin
  if ImageIndex > 1 then
    ImageIndex := ImageIndex - 1
  else
    ImageIndex := lbxFiles.Items.Count;
end;

procedure TfmCHXImgViewer.actShowFileListExecute(Sender: TObject);
begin
  Splitter.Visible := actShowFileList.Checked;
  lbxFiles.Visible := actShowFileList.Checked;
end;

procedure TfmCHXImgViewer.actStretchExecute(Sender: TObject);
begin
  if actStretch.Checked then
  begin
    StretchImage;
  end
  else
  begin
    DisableStretch;
  end;
end;

procedure TfmCHXImgViewer.actZoomInExecute(Sender: TObject);
begin
  DisableStretch;
  Image.Height := Image.Height shl 1;
  Image.Width := Image.Width shl 1;
  sbxImage.HorzScrollBar.Position := (sbxImage.HorzScrollBar.Position shl 1) + (sbxImage.ClientWidth shr 1);
  sbxImage.VertScrollBar.Position := (sbxImage.VertScrollBar.Position shl 1) + (sbxImage.ClientHeight shr 1);
  FixPosition;
end;

procedure TfmCHXImgViewer.actZoomOutExecute(Sender: TObject);
begin
  DisableStretch;
    sbxImage.HorzScrollBar.Position := (sbxImage.HorzScrollBar.Position shr 1) - (sbxImage.ClientWidth shr 2);
  sbxImage.VertScrollBar.Position := (sbxImage.VertScrollBar.Position shr 1) - (sbxImage.ClientHeight shr 2);
  Image.Height := Image.Height shr 1;
  Image.Width := Image.Width shr 1;
  FixPosition;
end;

procedure TfmCHXImgViewer.cbxCurrImageSelect(Sender: TObject);
begin
  ImageIndex := cbxCurrImage.ItemIndex + 1;
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
  end;
end;

procedure TfmCHXImgViewer.ImageResize(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TfmCHXImgViewer.sbxImageResize(Sender: TObject);
begin
  FixPosition;
end;

procedure TfmCHXImgViewer.SetIconsIniFile(AValue: TFilename);
begin
  FIconsIniFile := SetAsFile(AValue);
  ReadActionsIcons(FIconsIniFile, Self.Name, ilActions, ActionList);
end;

procedure TfmCHXImgViewer.SetDragBeginX(AValue: longint);
begin
  if FDragBeginX = AValue then
    Exit;
  FDragBeginX := AValue;
end;

procedure TfmCHXImgViewer.SetDragBeginY(AValue: longint);
begin
  if FDragBeginY = AValue then
    Exit;
  FDragBeginY := AValue;
end;

procedure TfmCHXImgViewer.SetImageIndex(AValue: integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  cbxCurrImage.ItemIndex := ImageIndex - 1;
  ChangeImage;
end;

procedure TfmCHXImgViewer.DisableStretch;
var
  Factor: real;
begin
  if Image.Align = alNone then
    Exit;

  // Factor of the stretched image
  Factor := (sbxImage.ClientHeight - sbxImage.HorzScrollBar.Size) /
    Image.Picture.Height;
  if Factor > ((sbxImage.ClientWidth - sbxImage.VertScrollBar.Size)) /
    Image.Picture.Width then
    Factor := (sbxImage.ClientWidth - sbxImage.VertScrollBar.Size) /
      Image.Picture.Width;

  Image.Align := alNone;
  actStretch.Checked := False;

  Image.Height := round(image.Picture.Height * Factor);
  Image.Width := round(image.Picture.Width * Factor);

  FixPosition;
end;

procedure TfmCHXImgViewer.StretchImage;
begin
  Image.Height := sbxImage.ClientHeight;
  Image.Width := sbxImage.ClientWidth;
  Image.Align := alClient;
  UpdateStatusBar;
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
  if (ImageIndex < 1) or (lbxFiles.Items.Count = 0) then
  begin
    Image.Picture.Clear;
    UpdateStatusBar;
    Exit;
  end;

  aFilename := lbxFiles.Items[ImageIndex - 1];

  if FileExistsUTF8(aFilename) then
  begin
    Image.Picture.LoadFromFile(aFilename);
    StretchImage;
    UpdateStatusBar;
  end
  else
  begin
    Image.Picture.Clear;
    lbxFiles.Items.Delete(ImageIndex - 1);
    ChangeImage;
  end;
end;

procedure TfmCHXImgViewer.UpdateStatusBar;
begin
  if ImageIndex > 0 then
  begin
    sbInfo.Panels[0].Text :=
      IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
    sbInfo.Panels[1].Text :=
      ' (' + IntToStr(Image.Width) + 'x' + IntToStr(Image.Height) + ')';
    sbInfo.Panels[2].Text := lbxFiles.Items[ImageIndex - 1];
  end
  else
  begin
    sbInfo.Panels[0].Text := '';
    sbInfo.Panels[1].Text := '';
    sbInfo.Panels[2].Text := '';
  end;

  lTotalImages.Caption := '/ ' + IntToStr(lbxFiles.Items.Count);

  cbxCurrImage.Enabled := lbxFiles.Items.Count > 1;
  actFirst.Enabled := cbxCurrImage.Enabled;
  actPrevious.Enabled := cbxCurrImage.Enabled;
  actNext.Enabled := cbxCurrImage.Enabled;
  actLast.Enabled := cbxCurrImage.Enabled;
  actShowFileList.Enabled := cbxCurrImage.Enabled;
end;

procedure TfmCHXImgViewer.AddImages(aImageList: TStrings; Index: integer);
var
  i: integer;
begin
  Index := Index + lbxFiles.Items.Count;
  lbxFiles.Items.AddStrings(aImageList);

  cbxCurrImage.Items.BeginUpdate;
  try
    cbxCurrImage.Items.Clear;
    for i := 1 to lbxFiles.Items.Count do
      cbxCurrImage.Items.Add(IntToStr(i));
  finally
    cbxCurrImage.Items.EndUpdate;
  end;

  ImageIndex := Index;
end;

procedure TfmCHXImgViewer.AddImage(aImageFile: TFilename; aObject: TObject);
begin
  lbxFiles.Items.AddObject(aImageFile, aObject);
  cbxCurrImage.Items.Add(IntToStr(lbxFiles.Count));
  ImageIndex := lbxFiles.Count;
end;

constructor TfmCHXImgViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ImageIndex := -1;
end;

destructor TfmCHXImgViewer.Destroy;
begin
  inherited Destroy;
end;

end.
