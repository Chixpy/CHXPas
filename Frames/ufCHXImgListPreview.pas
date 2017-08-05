unit ufCHXImgListPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, ExtCtrls, LazFileUtils,
  uCHXStrUtils,
  ufCHXStrLstPreview, ufCHXImgViewer;

type

  { TfmCHXImgListPreview }

  TfmCHXImgListPreview = class(TfmCHXStrLstPreview)
    iImage: TImage;
    procedure iImageDblClick(Sender: TObject);
  private
    FSHA1Folder: string;
    procedure SetSHA1Folder(AValue: string);

  protected
    procedure OnCurrItemChange; override;
  public
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
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

procedure TfmCHXImgListPreview.OnCurrItemChange;
begin
  if (CurrItem < 1) or (not Assigned(StrList)) or (StrList.Count = 0) then
  begin
    iImage.Picture.Clear;
    Exit;
  end;

  iImage.Picture.LoadFromFile(StrList[CurrItem - 1]);
end;

end.
