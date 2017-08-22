unit ufCHXScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, SynEdit, SynHighlighterPas,
  SynMacroRecorder, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ShellCtrls, EditBtn, Buttons, ActnList, StdActns, IniFiles,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXFrame,
  uCHXImageUtils, ucCHXScriptEngine;

const
  kFSMScriptFileExt = '.pas';
  kFSMScriptFileMask = '*' + kFSMScriptFileExt;
  kFSMInfoSection = 'Info';

resourcestring
  rsFSMFileSaved = 'File saved: %s';
  rsFSMScriptFileMaskDesc = 'Pascal script file';
  rsFSMAllFilesMaskDesc = 'All files';
  rsFSMAskSaveChanges = 'The sourcefile has changed.' +
    sLineBreak + 'Save?' + sLineBreak + '%s';

type

  { TfmCHXScriptManager }

  TfmCHXScriptManager = class(TfmCHXFrame)
    actCompile: TAction;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditDelete: TEditDelete;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actExecute: TAction;
    actFileSaveAs: TFileSaveAs;
    actFileSave: TAction;
    actOutputClear: TAction;
    ActionList: TActionList;
    actSearchFind: TSearchFind;
    actSearchReplace: TSearchReplace;
    bCompile: TToolButton;
    bEditCopy: TToolButton;
    bEditCut: TToolButton;
    bEditDelete: TToolButton;
    bEditPaste: TToolButton;
    bEditSelectAll: TToolButton;
    bEditUndo: TToolButton;
    bExecute: TToolButton;
    bExecute2: TBitBtn;
    bSaveFileAs: TToolButton;
    bSearchFind: TToolButton;
    bSearchReplace: TToolButton;
    bSeparator1: TToolButton;
    bSeparator2: TToolButton;
    bSeparator3: TToolButton;
    bSeparator4: TToolButton;
    bSeparator5: TToolButton;
    DirectoryEdit1: TDirectoryEdit;
    FontEdit1: TFontEdit;
    gbxScript: TGroupBox;
    ilActions: TImageList;
    mInfo: TMemo;
    mOutPut: TMemo;
    mScriptInfo: TMemo;
    PageControl: TPageControl;
    pagScriptList: TTabSheet;
    pagOutput: TTabSheet;
    pagSourceCode: TTabSheet;
    pFolders: TPanel;
    pRight: TPanel;
    sbInfo: TStatusBar;
    ShellTreeView1: TShellTreeView;
    slvGeneral: TShellListView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynEdit: TSynEdit;
    SynFreePascalSyn: TSynFreePascalSyn;
    SynMacroRecorder: TSynMacroRecorder;
    tbEditor: TToolBar;
    tbOutput: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    bSave: TToolButton;
    procedure actCompileExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actFileSaveAsAccept(Sender: TObject);
    procedure actFileSaveAsBeforeExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actOutputClearExecute(Sender: TObject);
    procedure FontEdit1Accept(Sender: TObject);
    procedure FontEdit1BeforeExecute(Sender: TObject);
    procedure slvSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);

  private
    FCurrentFile: string;
    FScriptEngine: cCHXScriptEngine;
    procedure SetCurrentFile(AValue: string);
    procedure SetScriptEngine(AValue: cCHXScriptEngine);


  protected
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    property CurrentFile: string read FCurrentFile write SetCurrentFile;
    property ScriptEngine: cCHXScriptEngine
      read FScriptEngine write SetScriptEngine;

    procedure LoadScriptFile(const aFile: string);
    procedure CheckChanged;

    function Compile: boolean;
    function Execute: boolean;

    procedure UpdateSLV;

    procedure SetGUIIconsIni(AValue: string); override;

  public
    procedure SetBaseFolder(const aFolder: string);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmCHXScriptManager }

procedure TfmCHXScriptManager.slvSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
var
  aSLV: TShellListView;
begin
  if not (Sender is TShellListView) then
    Exit;
  aSLV := TShellListView(Sender);

  if Selected then
    LoadScriptFile(SetAsFolder(aSLV.Root) + Item.Caption)
  else
    LoadScriptFile('');
end;

procedure TfmCHXScriptManager.actCompileExecute(Sender: TObject);
begin
  Compile;
end;

procedure TfmCHXScriptManager.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;

procedure TfmCHXScriptManager.actFileSaveAsAccept(Sender: TObject);
begin
  CurrentFile := actFileSaveAs.Dialog.FileName;
  SynEdit.Lines.SaveToFile(CurrentFile);

  mInfo.Lines.Add(Format(rsFSMFileSaved, [CurrentFile]));
  UpdateSLV;

  if SynEdit.CanFocus then
    SynEdit.SetFocus;
end;

procedure TfmCHXScriptManager.actFileSaveAsBeforeExecute(Sender: TObject);
begin
  actFileSaveAs.Dialog.FileName := CurrentFile;
  SetDlgInitialDir(actFileSaveAs.Dialog, ShellTreeView1.Path);

  actFileSaveAs.Dialog.Filter :=
    rsFSMScriptFileMaskDesc + '|' + kFSMScriptFileMask + '|' +
    rsFSMAllFilesMaskDesc + '|' + AllFilesMask;
  actFileSaveAs.Dialog.DefaultExt := kFSMScriptFileExt;
end;

procedure TfmCHXScriptManager.actFileSaveExecute(Sender: TObject);
begin
  if not FileExistsUTF8(CurrentFile) then
    Exit;
  SynEdit.Lines.SaveToFile(CurrentFile);

  mInfo.Lines.Add(Format(rsFSMFileSaved, [CurrentFile]));

end;

procedure TfmCHXScriptManager.actOutputClearExecute(Sender: TObject);
begin
  mOutPut.Clear;
end;

procedure TfmCHXScriptManager.FontEdit1Accept(Sender: TObject);
begin
  mOutPut.Font.Assign(FontEdit1.Dialog.Font);
end;

procedure TfmCHXScriptManager.FontEdit1BeforeExecute(Sender: TObject);
begin
  FontEdit1.Dialog.Font.Assign(mOutPut.Font);
end;

procedure TfmCHXScriptManager.SetCurrentFile(AValue: string);
begin
  if FCurrentFile = AValue then
    Exit;
  FCurrentFile := AValue;
end;

procedure TfmCHXScriptManager.SetScriptEngine(AValue: cCHXScriptEngine);
begin
  if FScriptEngine = AValue then
    Exit;
  FScriptEngine := AValue;
end;

procedure TfmCHXScriptManager.ClearFrameData;
begin

end;

procedure TfmCHXScriptManager.LoadFrameData;
begin

end;

procedure TfmCHXScriptManager.LoadScriptFile(const aFile: string);
var
  i: SizeInt;
  aIni: TIniFile;
begin
  CheckChanged;

  mScriptInfo.Clear;
  mInfo.Clear;

  CurrentFile := aFile;

  if not FileExistsUTF8(CurrentFile) then
  begin
    SynEdit.Lines.Clear;
    Exit;
  end;

  // TODO 2: Temporal script info until section is parsed properly.
  aIni := TIniFile.Create(CurrentFile, True);
  try
    aIni.ReadSectionRaw(kFSMInfoSection, mScriptInfo.Lines);
  finally
    aIni.Free;
  end;

  SynEdit.Lines.LoadFromFile(CurrentFile);

  // Removing UTF-8 BOM...
  if SynEdit.Lines.Count > 0 then
    i := Pos(UTF8FileHeader, SynEdit.Lines[0]);
  if i = 1 then
    SynEdit.Lines[0] := Copy(SynEdit.Lines[0], Length(UTF8FileHeader) +
      1, MaxInt);

end;

procedure TfmCHXScriptManager.CheckChanged;
begin
  if SynEdit.Modified and FileExistsUTF8(CurrentFile) then
    if MessageDlg(Format(rsFSMAskSaveChanges, [CurrentFile]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      SynEdit.Lines.SaveToFile(CurrentFile);
end;

function TfmCHXScriptManager.Compile: boolean;
begin
  // Why not in FormCreate? jajajaja
  // Simple reason, if ScripEngine is not assigned already we will create
  //   a default one. This way, a children form can create a custom engine in
  //   it's FormCreate overriding the default one.
  if not assigned(ScriptEngine) then
    FScriptEngine := cCHXScriptEngine.Create;

  ScriptEngine.ScriptText := SynEdit.Lines;

  // TODO 4: Put this in a better place near ScriptEngine creation.
  //   But search why a SIGEVN error is raised...
  //   ScriptEngine.ScriptOutput, ScriptEngine.ScriptInfo and
  //   ScriptEngine.ScriptError change to nil... When and Where?
  ScriptEngine.ScriptOutput := mOutPut.Lines;
  ScriptEngine.ScriptInfo := mInfo.Lines;
  ScriptEngine.ScriptError := mInfo.Lines;

  Result := ScriptEngine.CompileScript;
end;

function TfmCHXScriptManager.Execute: boolean;
begin
  PageControl.ActivePage := pagOutput;

  Result := Compile;

  if not Result then
    Exit;

  Result := ScriptEngine.RunScript;
end;

procedure TfmCHXScriptManager.UpdateSLV;
begin
  slvGeneral.Update;
end;

procedure TfmCHXScriptManager.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);

  ReadActionsIcons(GUIIconsIni, Name, ilActions, ActionList);
end;

procedure TfmCHXScriptManager.SetBaseFolder(const aFolder: string);
begin
  DirectoryEdit1.Directory := aFolder;
  ShellTreeView1.Root := aFolder;
end;

constructor TfmCHXScriptManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  PageControl.ActivePage := pagScriptList;
end;

destructor TfmCHXScriptManager.Destroy;
begin
  CheckChanged;
  FScriptEngine.Free;
  inherited Destroy;
end;

end.
