unit ufCHXFileList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  ExtCtrls, Menus, FileUtil, LazFileUtils,
  // CHX units
  uCHXConst, uCHXRscStr,
  // CHX frames
  ufCHXFrame;

type

  { TfmCHXFileList }

  TfmCHXFileList = class(TfmCHXFrame)
    actAddFile: TAction;
    actAddFolder: TAction;
    actClearList: TAction;
    actRemoveItem: TAction;
    alFileList: TActionList;
    bAddFile: TButton;
    bAddFolder: TButton;
    bClearList: TButton;
    bRemoveItem: TButton;
    dlgAddFile: TOpenDialog;
    dlgAddFolder: TSelectDirectoryDialog;
    FileList: TListBox;
    ilFileList: TImageList;
    mipmClearList: TMenuItem;
    mipmRemoveItem: TMenuItem;
    mipmAddFolder: TMenuItem;
    mipmAddFile: TMenuItem;
    pButtonsFile: TPanel;
    pmFileList: TPopupMenu;
    Separator1: TMenuItem;
    procedure actAddFileExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actClearListExecute(Sender: TObject);
    procedure actRemoveItemExecute(Sender: TObject);
    procedure FileListSelectionChange(Sender: TObject; User: boolean);

  private
    FFileMask: string;
    FOnFileSelect: TCHXStrObjCB;
    FSelectNextOnRemove: Boolean;
    procedure SetFileMask(AValue: string);
    procedure SetOnFileSelect(AValue: TCHXStrObjCB);
    procedure SetSelectNextOnRemove(AValue: Boolean);

  protected

  public
    property OnFileSelect: TCHXStrObjCB
      read FOnFileSelect write SetOnFileSelect;

    property SelectNextOnRemove: Boolean read FSelectNextOnRemove write SetSelectNextOnRemove;

    function NextFile: string;
    {< Returns next item after selected one, without changing current selection.

      @returns(The filename of the next item, empty string if last item
       is selected.)}

    procedure ClearFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FileMask: string read FFileMask write SetFileMask;
  end;

implementation

{$R *.lfm}

{ TfmCHXFileList }

procedure TfmCHXFileList.actAddFileExecute(Sender: TObject);
begin
  if FileMask <> '' then
  begin
    dlgAddFile.Filter := Format(rsFileDlgMaskFmt, [FileMask]);
    dlgAddFile.FilterIndex := 2;
  end
  else
  begin
    dlgAddFile.Filter := rsFileDlgMaskDef;
    dlgAddFile.FilterIndex := 1;
  end;

  if not dlgAddFile.Execute then
    Exit;

  // Remember last dir...
  // if dlgAddFile.Files.Count > 0 then
  //  BaseDir := ExtractFileDir(dlgAddFile.Files[0]);

  FileList.Items.AddStrings(dlgAddFile.Files, False);
end;

procedure TfmCHXFileList.actAddFolderExecute(Sender: TObject);
var
  i: integer;
  slFiles: TStringList;
begin
  if not dlgAddFolder.Execute then
    Exit;

  // Remember last dir...
  // if dlgAddFolder.Files.Count > 0 then
  //   BaseDir := ExtractFileDir(dlgAddFolder.Files[0]);

  i := 0;
  slFiles := TStringList.Create;
  while i < dlgAddFolder.Files.Count do
  begin
    slFiles.Clear;
    slFiles.BeginUpdate;
    FindAllFiles(slFiles, dlgAddFolder.Files[i], FileMask, True);
    FileList.Items.AddStrings(slFiles, False);
    slFiles.EndUpdate;
    Inc(i);
  end;
  slFiles.Free;
end;

procedure TfmCHXFileList.actClearListExecute(Sender: TObject);
begin
  FileList.Clear;
end;

procedure TfmCHXFileList.actRemoveItemExecute(Sender: TObject);
var
  i: Integer;
begin
  i := FileList.ItemIndex;
  FileList.DeleteSelected;
  FileList.ItemIndex := -1;

  if SelectNextOnRemove and (FileList.Count > 0) then
  begin
    if i < FileList.Count then
      FileList.ItemIndex := i
    else
      FileList.ItemIndex := FileList.Count - 1;
  end;
end;

procedure TfmCHXFileList.FileListSelectionChange(Sender: TObject;
  User: boolean);
begin
  if Assigned(OnFileSelect) then
    OnFileSelect(FileList.GetSelectedText);
end;

procedure TfmCHXFileList.SetFileMask(AValue: string);
begin
  if FFileMask = AValue then Exit;
  FFileMask := AValue;
end;

procedure TfmCHXFileList.SetOnFileSelect(AValue: TCHXStrObjCB);
begin
  if FOnFileSelect = AValue then Exit;
  FOnFileSelect := AValue;
end;

procedure TfmCHXFileList.SetSelectNextOnRemove(AValue: Boolean);
begin
  if FSelectNextOnRemove = AValue then Exit;
  FSelectNextOnRemove := AValue;
end;

procedure TfmCHXFileList.ClearFrameData;
begin
  inherited;

  FileList.Clear;
end;

function TfmCHXFileList.NextFile: string;
begin
  Result := '';

  if (FileList.ItemIndex + 1) < FileList.Count then
    Result := FileList.Items[FileList.ItemIndex + 1];
end;

constructor TfmCHXFileList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Enabled := True;
end;

destructor TfmCHXFileList.Destroy;
begin
  inherited Destroy;
end;

end.
