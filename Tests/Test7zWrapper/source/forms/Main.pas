unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lclintf, LazFileUtils, LazUTF8, LConvEncoding,
  uCHX7zWrapper;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCacheFolder: TButton;
    bScanFiles: TButton;
    lbx7zFiles: TListBox;
    lbxCompFiles: TListBox;
    mErrores: TMemo;
    procedure bCacheFolderClick(Sender: TObject);
    procedure bScanFilesClick(Sender: TObject);
    procedure lbx7zFilesClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bScanFilesClick(Sender: TObject);
begin
 FindAllFiles(lbx7zFiles.Items, '', '*.7z',false);
end;

procedure TForm1.bCacheFolderClick(Sender: TObject);
begin
 if DirectoryExistsUTF8(w7zGetCacheDir) then
  OpenDocument(w7zGetCacheDir);
end;

procedure TForm1.lbx7zFilesClick(Sender: TObject);
var
  aFile: string;
begin
 aFile := '';
 if lbx7zFiles.ItemIndex < 0 then
 begin
   lbxCompFiles.Clear;
   mErrores.Clear;
 end
 else
 begin
  aFile := lbx7zFiles.Items[lbx7zFiles.ItemIndex];
  w7zListFiles(aFile, lbxCompFiles.Items, True, '');
  mErrores.Lines.Assign(w7zGetErrorList);
 end;
end;

end.

