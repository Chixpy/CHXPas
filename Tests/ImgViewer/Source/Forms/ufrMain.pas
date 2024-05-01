unit ufrMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileUtil,
  LCLTranslator, ExtCtrls, StdCtrls, ExtDlgs, BGRABitmapTypes, BGRABitmap,
  // CHX units
  uCHXConst, uCHXStrUtils, uCHXVerInfo,
  // CHX frames
  ufCHXImgViewer, ufCHXBGRAImgViewer, ufCHXBGRAImgViewerEx,
  // CHX forms
  ufrCHXForm;

type

  { TCHXForm }

  TCHXForm = class(TfrmCHXForm)
    aTImage : TImage;
    bOpenFile : TButton;
    gbxCHXBGRAImgViewer : TGroupBox;
    gbxCHXBGRAImgViewerEx : TGroupBox;
    gbxCHXImgViewer : TGroupBox;
    gbxTImage : TGroupBox;
    OpenPictureDialog1 : TOpenPictureDialog;
    pImage : TPanel;
    pImgLeft : TPanel;
    pImgRight : TPanel;
    pLeft : TPanel;
    Splitter1 : TSplitter;
    Splitter2 : TSplitter;
    Splitter3 : TSplitter;
    Splitter4 : TSplitter;
    procedure bOpenFileClick(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
    procedure FormCreate(Sender : TObject);

  private
    FaImageList : TStringList;
    FBaseFolder : string;
    FfmCHXBGRAImgViewer : TfmCHXBGRAImgViewer;
    FfmCHXBGRAImgViewerEx : TfmCHXBGRAImgViewerEx;
    FfmCHXImgViewer : TfmCHXImgViewer;
    FGUIConfig : string;
    FIconsFile : string;
    FImage : TBGRABitmap;
    procedure SetBaseFolder(AValue : string);
    procedure SetfmCHXBGRAImgViewerEx(const AValue : TfmCHXBGRAImgViewerEx);
    procedure SetfmCHXImgViewer(const AValue : TfmCHXImgViewer);
    procedure SetGUIConfig(AValue : string);
    procedure SetIconsFile(AValue : string);

  protected
    property aImageList : TStringList read FaImageList;

  public
    property fmCHXBGRAImgViewer : TfmCHXBGRAImgViewer read FfmCHXBGRAImgViewer;
    property fmCHXBGRAImgViewerEx : TfmCHXBGRAImgViewerEx
      read FfmCHXBGRAImgViewerEx write SetfmCHXBGRAImgViewerEx;
    property fmCHXImgViewer : TfmCHXImgViewer
      read FfmCHXImgViewer write SetfmCHXImgViewer;

    property Image : TBGRABitmap read FImage;

    property BaseFolder : string read FBaseFolder write SetBaseFolder;
    {< Base folder for relative paths of files. }
    property GUIConfig : string read FGUIConfig write SetGUIConfig;
    property IconsFile : string read FIconsFile write SetIconsFile;
  end;

var
  CHXForm : TCHXForm;

implementation

{$R *.lfm}

{ TCHXForm }

procedure TCHXForm.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  Image.Free;
  aImageList.Free;

  CanClose := True;
end;

procedure TCHXForm.bOpenFileClick(Sender : TObject);
var
  aFile : string;
begin
  FreeAndNil(FImage);
  FreeAndNil(FaImageList);

  if OpenPictureDialog1.Execute then
  begin
    aFile := OpenPictureDialog1.FileName;
    FImage := TBGRABitmap.Create(aFile);

    aTImage.Picture.LoadFromFile(aFile);
  end;

  fmCHXBGRAImgViewer.ActualImage := Image;
  fmCHXBGRAImgViewerEx.ActualImage := Image;

  //fmCHXImgViewer.FileList := aImageList;
end;

procedure TCHXForm.FormCreate(Sender : TObject);
begin
  // Tipical CHX program init...

  // Title of application, usually it's autodeleted in .lpr file...
  //   There is an option
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);
  Self.Caption := Format(krsFmtWindowCaption,
    [Application.Title, Self.Caption]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Loading translation
  SetDefaultLang('', 'Locale');

  // File to load window states and config
  GUIConfig := 'GUI.ini';
  // IconsFile can be a fixed value or a value readed from GUIConfig
  IconsFile := 'Images\GUI\Icons.ini';

  // TODO: Load program config from previous file, a fixed value or
  //   a value from previous file with the filename of config file.

  // Actually loading icons and window configuration
  LoadGUIIcons(IconsFile);
  LoadGUIConfig(GUIConfig);


  FfmCHXBGRAImgViewer := TfmCHXBGRAImgViewer.Create(gbxCHXBGRAImgViewer);
  fmCHXBGRAImgViewer.Align := alClient;
  fmCHXBGRAImgViewer.Parent := gbxCHXBGRAImgViewer;
  fmCHXBGRAImgViewer.BackgroundType := bkChecker;

  FfmCHXBGRAImgViewerEx := TfmCHXBGRAImgViewerEx.Create(gbxCHXBGRAImgViewerEx);
  fmCHXBGRAImgViewerEx.Align := alClient;
  fmCHXBGRAImgViewerEx.Parent := gbxCHXBGRAImgViewerEx;
  fmCHXBGRAImgViewerEx.BackgroundType := bkChecker;
  fmCHXBGRAImgViewerEx.MouseActionMode := maiMouseSelectRect;

  //FfmCHXImgViewer := TfmCHXImgViewer.Create(gbxCHXImgViewer);
  //fmCHXImgViewer.Align := alClient;
  //fmCHXImgViewer.Parent := gbxCHXImgViewer;
end;

procedure TCHXForm.SetBaseFolder(AValue : string);
begin
  FBaseFolder := SetAsFile(AValue);
end;

procedure TCHXForm.SetfmCHXBGRAImgViewerEx(
  const AValue : TfmCHXBGRAImgViewerEx);
begin
  if FfmCHXBGRAImgViewerEx = AValue then Exit;
  FfmCHXBGRAImgViewerEx := AValue;
end;

procedure TCHXForm.SetfmCHXImgViewer(const AValue : TfmCHXImgViewer);
begin
  if FfmCHXImgViewer = AValue then Exit;
  FfmCHXImgViewer := AValue;
end;

procedure TCHXForm.SetGUIConfig(AValue : string);
begin
  FGUIConfig := SetAsFile(AValue);
end;

procedure TCHXForm.SetIconsFile(AValue : string);
begin
  FIconsFile := SetAsFile(AValue);
end;

end.
