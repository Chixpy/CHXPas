unit ufCHXImgViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Menus, ComCtrls,
  StdCtrls, LazUTF8, ExtCtrls, ActnList, dateutils, IniFiles,
  uCHXStrUtils, uCHXImageUtils, uCHXFileUtils;

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
    procedure lbxFilesSelectionChange(Sender: TObject; User: boolean);
    procedure sbxImageResize(Sender: TObject);

  private
    FStartTime: TTime;
    FDragBeginX: longint;
    FDragBeginY: longint;
    FIconsIniFile: TFilename;
    FImageIndex: integer;
    FSHA1: string;
    FSHA1Folder: TFilename;
    procedure SetStartTime(AValue: TTime);
    procedure SetDragBeginX(AValue: longint);
    procedure SetDragBeginY(AValue: longint);
    procedure SetIconsIniFile(AValue: TFilename);
    procedure SetImageIndex(AValue: integer);
    procedure SetSHA1(AValue: string);
    procedure SetSHA1Folder(AValue: TFilename);

  protected
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
    property SHA1Folder: TFilename read FSHA1Folder write SetSHA1Folder;

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
  actStretch.Checked := False;
  Image.Height := Image.Picture.Height;
  Image.Width := Image.Picture.Width;
  FixPosition;
  sbxImage.HorzScrollBar.Position := Image.Width shr 1;
  sbxImage.VertScrollBar.Position := Image.Height shr 1;
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
    StretchImage;

  FixPosition;
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

procedure TfmCHXImgViewer.lbxFilesSelectionChange(Sender: TObject;
  User: boolean);
begin
  if User then
    ImageIndex := lbxFiles.ItemIndex + 1;
end;

procedure TfmCHXImgViewer.sbxImageResize(Sender: TObject);
begin
  if actStretch.Checked then
    StretchImage;
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

procedure TfmCHXImgViewer.SetStartTime(AValue: TTime);
begin
  if FStartTime = AValue then
    Exit;
  FStartTime := AValue;
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
  lbxFiles.ItemIndex := ImageIndex - 1;
  ChangeImage;
end;

procedure TfmCHXImgViewer.SetSHA1(AValue: string);
begin
  if FSHA1 = AValue then
    Exit;
  FSHA1 := AValue;
end;

procedure TfmCHXImgViewer.SetSHA1Folder(AValue: TFilename);
begin
  FSHA1Folder := SetAsFolder(AValue);
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

  if (ImageIndex < 1) or (lbxFiles.Items.Count = 0) then
  begin
    Image.Picture.Clear;
    UpdateStatusBar;
    Exit;
  end;

  aFilename := lbxFiles.Items[ImageIndex - 1];

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
  aFileName := SHA1Folder + SetAsFolder(copy(SHA1, 1, 2)) +
    copy(SHA1, 3, 2) + '.ini';

  ForceDirectories(ExtractFileDir(aFileName)); // Actually a folder now

  aIni := TMemIniFile.Create(aFileName);
  try
    TimePassed := TimePassed + aIni.ReadInt64(SHA1, 'Pic.TimeViewed', 0);
    NTimes := 1 + aIni.ReadInt64(SHA1, 'Pic.NTimes', 0);
    aIni.WriteInt64(SHA1, 'Pic.TimeViewed', TimePassed);
    aIni.WriteInt64(SHA1, 'Pic.NTimes', NTimes);
    { TODO : Other Stats? }
    aIni.UpdateFile;
  finally
    FreeAndNil(aIni);
  end;
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
  if (SHA1Folder <> '') then
    SaveStats;
  inherited Destroy;
end;

end.
