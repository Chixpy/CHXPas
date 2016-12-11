unit ufCHXImgViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Menus, ComCtrls,
  Spin, StdCtrls,
  ExtCtrls, ActnList;

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
    Splitter: TSplitter;
    tbClose: TToolButton;
    tbImage: TToolBar;
    tbOriginalSize: TToolButton;
    tbZoomIn: TToolButton;
    tbZoomOut: TToolButton;

  private
    FIconsIniFile: TFilename;
    procedure SetIconsIniFile(AValue: TFilename);

  public
    property IconsIniFile: TFilename read FIconsIniFile write SetIconsIniFile;

    procedure AddImages(aImageList: TStrings; Index: integer = 0);
    procedure AddImage(aImageFile: TFilename; aObject: TObject = nil);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXImgViewer }

procedure TfmCHXImgViewer.SetIconsIniFile(AValue: TFilename);
begin
  FIconsIniFile := SetAsFile(AValue);
  ReadActionsIcons(IconsIni, Self.Name, ilActions, ActionList);
end;

procedure TfmCHXImgViewer.AddImages(aImageList: TStrings; Index: integer);
begin
  lbxFiles.Items.AddStrings(aImageList);
end;

procedure TfmCHXImgViewer.AddImage(aImageFile: TFilename; aObject: TObject);
begin
  lbxFiles.Items.AddObject(aImageFile, aObject);
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
