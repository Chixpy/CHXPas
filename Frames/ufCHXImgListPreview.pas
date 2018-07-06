unit ufCHXImgListPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, ExtCtrls, LazFileUtils, IniFiles,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXStrLstPreview, ufCHXImgViewer;

type

  { TfmCHXImgListPreview }

  TfmCHXImgListPreview = class(TfmCHXStrLstPreview)
    iImage: TImage;
    procedure iImageDblClick(Sender: TObject);

  private
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FSHA1Folder: string;
    procedure SetGUIConfigIni(AValue: string);
    procedure SetGUIIconsIni(AValue: string);
    procedure SetSHA1Folder(AValue: string);

  protected
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;

    procedure OnCurrItemChange; override;

    procedure DoLoadGUIIcons(aIniFile: TIniFile; const aBaseFolder: string); override;
    procedure DoLoadGUIConfig(aIniFile: TIniFile);

  public
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXImgListPreview }

procedure TfmCHXImgListPreview.iImageDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormIL(StrList, SHA1Folder, CurrItem, GUIIconsIni, GUIConfigIni);
end;

procedure TfmCHXImgListPreview.SetSHA1Folder(AValue: string);
begin
  AValue := SetAsFolder(AValue);
  if CompareFilenames(FSHA1Folder,AValue) =  0 then Exit;
  FSHA1Folder := AValue;
end;

procedure TfmCHXImgListPreview.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmCHXImgListPreview.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmCHXImgListPreview.OnCurrItemChange;
begin
  if (CurrItem < 1) or (not Assigned(StrList)) or (StrList.Count = 0) then
  begin
    iImage.Picture.Clear;
    Exit;
  end;

  iImage.Picture.LoadFromFile(StrList[CurrItem - 1]);
end;

procedure TfmCHXImgListPreview.DoLoadGUIIcons(aIniFile: TIniFile;
  const aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIniFile, aBaseFolder);

  GUIIconsIni := aIniFile.FileName;
end;

procedure TfmCHXImgListPreview.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

constructor TfmCHXImgListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadGUIConfig := @DoLoadGUIConfig;
end;

destructor TfmCHXImgListPreview.Destroy;
begin
  inherited Destroy;
end;

end.
