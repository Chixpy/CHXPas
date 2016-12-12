unit ufCHXImgViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Menus, ComCtrls,
  Spin, StdCtrls, LazUTF8,
  ExtCtrls, ActnList,
  uCHXStrUtils, uCHXImageUtils;

type

  { TfmCHXImgViewer }

  TfmCHXImgViewer = class(TFrame)
    actClose: TAction;
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
    bSep2: TToolButton;
    bSep3: TToolButton;
    bSep4: TToolButton;
    bShowFileList: TToolButton;
    bStretch: TToolButton;
    eCurrImage: TSpinEdit;
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
    ScrollBox1: TScrollBox;
    Splitter: TSplitter;
    tbClose: TToolButton;
    tbImage: TToolBar;
    tbOriginalSize: TToolButton;
    tbZoomIn: TToolButton;
    tbZoomOut: TToolButton;
    procedure actFirstExecute(Sender: TObject);
    procedure actLastExecute(Sender: TObject);
    procedure actStretchExecute(Sender: TObject);

  private
    FIconsIniFile: TFilename;
    FImageIndex: integer;
    procedure SetIconsIniFile(AValue: TFilename);
    procedure SetImageIndex(AValue: integer);

  protected
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

procedure TfmCHXImgViewer.actStretchExecute(Sender: TObject);
begin
  Image.Stretch := actStretch.Checked;
end;

procedure TfmCHXImgViewer.SetIconsIniFile(AValue: TFilename);
begin
  FIconsIniFile := SetAsFile(AValue);
  ReadActionsIcons(FIconsIniFile, Self.Name, ilActions, ActionList);
end;

procedure TfmCHXImgViewer.SetImageIndex(AValue: integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  ChangeImage;
end;

procedure TfmCHXImgViewer.ChangeImage;
var
  aFilename: string;
begin
  if lbxFiles.Items.Count = 0 then
  begin
    Image.Picture.Clear;
    Exit;
  end;

  aFilename := lbxFiles.Items[ImageIndex - 1];

  if FileExistsUTF8(aFilename) then
  begin
    Image.Picture.LoadFromFile(aFilename);
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
  sbInfo.Panels[1].Text :=
    IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
  if ImageIndex > 0 then
    sbInfo.Panels[2].Text := lbxFiles.Items[ImageIndex - 1];
end;

procedure TfmCHXImgViewer.AddImages(aImageList: TStrings; Index: integer);
begin
  Index := Index + lbxFiles.Items.Count;
  lbxFiles.Items.AddStrings(aImageList);
  ImageIndex := Index;
end;

procedure TfmCHXImgViewer.AddImage(aImageFile: TFilename; aObject: TObject);
begin
  ImageIndex := lbxFiles.Items.AddObject(aImageFile, aObject);
end;

constructor TfmCHXImgViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXImgViewer.Destroy;
begin
  inherited Destroy;
end;

end.
