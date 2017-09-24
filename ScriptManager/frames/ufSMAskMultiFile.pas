unit ufSMAskMultiFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, EditBtn, ComCtrls,
  uCHXDlgUtils,
  ufCHXPropEditor, ufCHXForm;

type

  { TfmSMAskMultiFile }

  TfmSMAskMultiFile = class(TfmCHXPropEditor)
    actAddFiles: TAction;
    actSortFileList: TAction;
    actMoveFileDown: TAction;
    actMoveFileUp: TAction;
    actRemoveFiles: TAction;
    gbxSelectFiles: TGroupBox;
    lbxFiles: TListBox;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    tbRemoveFiles: TToolButton;
    tbAddFiles: TToolButton;
    ToolButton1: TToolButton;
    tbMoveFileUp: TToolButton;
    ToolButton3: TToolButton;
    tbSortFileList: TToolButton;
    tbMoveFileDown: TToolButton;
    procedure actAddFilesExecute(Sender: TObject);
    procedure actMoveFileDownExecute(Sender: TObject);
    procedure actMoveFileUpExecute(Sender: TObject);
    procedure actRemoveFilesExecute(Sender: TObject);
    procedure actSortFileListExecute(Sender: TObject);
  private
    FOutFileList: TStrings;
    procedure SetOutFileList(AValue: TStrings);

  protected
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property OutFileList: TStrings read FOutFileList write SetOutFileList;
    procedure SaveFrameData; override;

    // Creates a form with AddSoft frame.
    class function SimpleForm(aFileList: TStrings; const aTitle: string;
      const aExtFilter: string; const DefFolder: string; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmSMAskMultiFile }

procedure TfmSMAskMultiFile.actRemoveFilesExecute(Sender: TObject);
var
  i: Integer;
begin
  i := lbxFiles.ItemIndex;

  lbxFiles.DeleteSelected;

  if i > lbxFiles.Count then
    lbxFiles.ItemIndex := lbxFiles.Count - 1
  else if lbxFiles.Count > 0 then
    lbxFiles.ItemIndex := i;
end;

procedure TfmSMAskMultiFile.actSortFileListExecute(Sender: TObject);
begin
  lbxFiles.Sorted := actSortFileList.Checked;
  actMoveFileDown.Enabled := not actSortFileList.Checked;
  actMoveFileUp.Enabled := not actSortFileList.Checked;
end;

procedure TfmSMAskMultiFile.actMoveFileUpExecute(Sender: TObject);
begin
  if lbxFiles.Sorted or (lbxFiles.ItemIndex < 1) then Exit;

  lbxFiles.Items.Exchange(lbxFiles.ItemIndex, lbxFiles.ItemIndex - 1);

  lbxFiles.ItemIndex := lbxFiles.ItemIndex - 1;
end;

procedure TfmSMAskMultiFile.actMoveFileDownExecute(Sender: TObject);
begin
  if lbxFiles.Sorted or (lbxFiles.ItemIndex = -1) or (lbxFiles.ItemIndex > (lbxFiles.Count - 2))then Exit;

  lbxFiles.Items.Exchange(lbxFiles.ItemIndex, lbxFiles.ItemIndex + 1);

  lbxFiles.ItemIndex := lbxFiles.ItemIndex + 1;
end;

procedure TfmSMAskMultiFile.actAddFilesExecute(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;

  lbxFiles.Items.AddStrings(OpenDialog1.Files);
end;

procedure TfmSMAskMultiFile.SetOutFileList(AValue: TStrings);
begin
  if FOutFileList = AValue then Exit;
  FOutFileList := AValue;

  LoadFrameData;
end;

procedure TfmSMAskMultiFile.ClearFrameData;
begin
  lbxFiles.Clear;
end;

procedure TfmSMAskMultiFile.LoadFrameData;
begin
  Enabled := Assigned(OutFileList);
  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  lbxFiles.Items.Assign(OutFileList);
end;

procedure TfmSMAskMultiFile.SaveFrameData;
begin
  if assigned(OutFileList) then
    OutFileList.Assign(lbxFiles.Items);
end;

class function TfmSMAskMultiFile.SimpleForm(aFileList: TStrings;
  const aTitle: string; const aExtFilter: string; const DefFolder: string;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmSMAskMultiFile;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmSMAskMultiFile';
    aForm.Caption := aTitle;

    aFrame := TfmSMAskMultiFile.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.OpenDialog1.Title := aTitle;
    aFrame.OpenDialog1.Filter := aExtFilter;
    SetDlgInitialDir(aFrame.OpenDialog1, DefFolder);
    aFrame.OutFileList := aFileList;

    aForm.GUIConfigIni := aGUIConfigIni;
    aForm.GUIIconsIni := aGUIIconsIni;
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmSMAskMultiFile.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmSMAskMultiFile.Destroy;
begin
  inherited Destroy;
end;

end.
