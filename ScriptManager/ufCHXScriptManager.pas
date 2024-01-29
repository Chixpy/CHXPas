unit ufCHXScriptManager;

{< TfmCHXScriptManager frame unit.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, SynEdit, SynHighlighterPas,
  SynMacroRecorder, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ShellCtrls, Buttons, ActnList, StdActns, IniFiles, SynEditTypes,
  SynCompletion, LCLIntf, Menus,
  // CHX units
  uCHXRscStr, uCHXStrUtils, uCHXDlgUtils, uCHXImageUtils,
  // CHX classes
  ucCHXScriptEngine,
  // CHX frames
  ufrCHXForm,
  // CHX frames
  ufCHXFrame,
  // CHX Script Engine frames
  ufSMAskMultiFile, ufSMAskOption;

const
  krsFormCHXScriptMName = 'frmCHXScriptManager';
  krsFormCHXScriptMTitle = 'CHX Script Manager';

  kFSMScriptFileExt = '.pas';
  kFSMScriptFileMask = '*' + kFSMScriptFileExt;
  kFSMInfoSection = 'Info';

  krsIniScriptMngSection = 'Script Manager';
  { Config file section name. }
  krsIniScriptMngSourceFont = 'SourceFont';
  krsIniScriptMngOutputFont = 'OutputFont';

resourcestring
  rsFSMFileSaved = 'File saved: %s';
  rsFSMScriptFileMaskDesc = 'Pascal script file';
  rsFSMAskSaveChanges = 'The sourcefile has changed.' + sLineBreak +
    'Save?' + sLineBreak + '%s';

type

  { TfmCHXScriptManager }

  TfmCHXScriptManager = class(TfmCHXFrame)
    actCompile: TAction;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditDelete: TEditDelete;
    actEditPaste: TEditPaste;
    actEditUndo: TEditUndo;
    actExecute: TAction;
    actFileSaveAs: TFileSaveAs;
    actFileSave: TAction;
    actOpenFileFolder: TAction;
    actOpenRootFolder: TAction;
    actChangeBaseFolder: TAction;
    actStop: TAction;
    actOutputClear: TAction;
    ActionList: TActionList;
    actSearchFind: TSearchFind;
    actSearchReplace: TSearchReplace;
    bChangeRootFolder: TButton;
    bCompile: TToolButton;
    bEditCopy: TToolButton;
    bEditCut: TToolButton;
    bEditDelete: TToolButton;
    bEditPaste: TToolButton;
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
    bStop: TBitBtn;
    actOutputSaveAs: TFileSaveAs;
    actOutputFontEdit: TFontEdit;
    actSourceFontEdit: TFontEdit;
    gbxScript: TGroupBox;
    ilActions: TImageList;
    lCurrentFile: TLabel;
    lbxInfo: TListBox;
    miChangeBaseFolder: TMenuItem;
    miFileOpenFolder: TMenuItem;
    mOutPut: TMemo;
    mScriptInfo: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl: TPageControl;
    pagScriptList: TTabSheet;
    pagOutput: TTabSheet;
    pagSourceCode: TTabSheet;
    pFolders: TPanel;
    pBottom: TPanel;
    pmFileList: TPopupMenu;
    pmFolderList: TPopupMenu;
    sbInfo: TStatusBar;
    sbSourceEditor: TStatusBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    stvFolders: TShellTreeView;
    slvFiles: TShellListView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynCompletion: TSynCompletion;
    SynEdit: TSynEdit;
    SynFreePascalSyn: TSynFreePascalSyn;
    SynMacroRecorder: TSynMacroRecorder;
    tbEditor: TToolBar;
    tbOutput: TToolBar;
    tbOutputClear: TToolButton;
    tbSaveOutput: TToolButton;
    bSeparator6: TToolButton;
    ToolButton2: TToolButton;
    tbFontEdit: TToolButton;
    bSave: TToolButton;
    bEditorFont: TToolButton;
    procedure actChangeBaseFolderExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actSourceFontEditAccept(Sender: TObject);
    procedure actSourceFontEditBeforeExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actFileSaveAsAccept(Sender: TObject);
    procedure actFileSaveAsBeforeExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actOpenFileFolderExecute(Sender: TObject);
    procedure actOpenRootFolderExecute(Sender: TObject);
    procedure actOutputClearExecute(Sender: TObject);
    procedure actOutputFontEditAccept(Sender: TObject);
    procedure actOutputFontEditBeforeExecute(Sender: TObject);
    procedure actOutputSaveAsAccept(Sender: TObject);
    procedure actOutputSaveAsBeforeExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure lbxInfoDblClick(Sender: TObject);
    procedure slvSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure SynEditSpecialLineColors(Sender: TObject;
      Line: integer; var Special: boolean; var FG, BG: TColor);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);

  private
    FCurrentFile: string;
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FScriptEngine: cCHXScriptEngine;
    procedure SetCurrentFile(AValue: string);
    procedure SetGUIConfigIni(AValue: string);
    procedure SetGUIIconsIni(AValue: string);
    procedure SetScriptEngine(AValue: cCHXScriptEngine);

  protected
    property CurrentFile: string read FCurrentFile write SetCurrentFile;

    property ScriptEngine: cCHXScriptEngine
      read FScriptEngine write SetScriptEngine;

    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;

    procedure CreateCustomEngine; virtual;

    procedure LoadScriptFile(const aFile: string);
    procedure CheckChanged;

    function Compile: boolean;
    function Execute: boolean;
    procedure Stop;

    procedure UpdateSLV;

    procedure DoWriteLn(const aStr: string); virtual;
    function DoReadLn(const aQuestion, DefAnswer: string): string; virtual;
    function DoAskFile(const aCaption, aExtFilter, DefFile: string): string;
      virtual;
    procedure DoAskMultiFile(aFileList: TStrings;
      const aCaption, aExtFilter, DefFolder: string); virtual;
    function DoAskFolder(const aCaption, DefFolder: string): string; virtual;
    function DoAskOption(const aCaption, aQuestion: string;
      aOptionList: TStrings): integer; virtual;
    function DoAskYesNoCancel(const aCaption, aQuestion: string): integer;
      virtual;

    procedure DoOnline(Sender: TObject); virtual;

    procedure DoLoadGUIConfig(aIniFile: TIniFile); override;
    procedure DoSaveGUIConfig(aIniFile: TIniFile); override;
    procedure DoLoadGUIIcons(aIconsIni: TIniFile;
      const aBaseFolder: string); override;

  public
    procedure SetBaseFolder(const aFolder: string); virtual;

    // Creates a form with Script Manager.
    class function SimpleForm(aBaseFolder: string; aGUIConfigIni: string;
      aGUIIconsIni: string): integer;

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
    LoadScriptFile(EmptyStr);
end;

procedure TfmCHXScriptManager.SynEditSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  {if ce.HasBreakPoint(ce.MainFileName, Line) then
  begin
    Special := True;
    if Line = FActiveLine then
    begin
      BG := clWhite;
      FG := clRed;
    end else
    begin
      FG := clWhite;
      BG := clRed;
    end;
  end else
  if Line = FActiveLine then
  begin
    Special := True;
    FG := clWhite;
    bg := clBlue;
  end
  else }
  Special := False;
end;

procedure TfmCHXScriptManager.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  sbSourceEditor.Panels[0].Text :=
    IntToStr(SynEdit.CaretY) + ':' + IntToStr(SynEdit.CaretX);
end;

procedure TfmCHXScriptManager.actCompileExecute(Sender: TObject);
begin
  Compile;
end;

procedure TfmCHXScriptManager.actSourceFontEditAccept(Sender: TObject);
begin
  SynEdit.Font.Assign(actSourceFontEdit.Dialog.Font);
end;

procedure TfmCHXScriptManager.actSourceFontEditBeforeExecute(Sender: TObject);
begin
  actSourceFontEdit.Dialog.Font.Assign(SynEdit.Font);
end;

procedure TfmCHXScriptManager.actChangeBaseFolderExecute(Sender: TObject);
begin
  SetDlgInitialDir(SelectDirectoryDialog1, stvFolders.Root);

  if SelectDirectoryDialog1.Execute then
    SetBaseFolder(SelectDirectoryDialog1.FileName);
end;

procedure TfmCHXScriptManager.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;

procedure TfmCHXScriptManager.actFileSaveAsAccept(Sender: TObject);
begin
  CurrentFile := actFileSaveAs.Dialog.FileName;
  SynEdit.Lines.SaveToFile(CurrentFile);
  SynEdit.Modified := False;

  lbxInfo.Items.Add(Format(rsFSMFileSaved, [CurrentFile]));
  UpdateSLV;

  if SynEdit.CanFocus then
    SynEdit.SetFocus;
end;

procedure TfmCHXScriptManager.actFileSaveAsBeforeExecute(Sender: TObject);
begin
  actFileSaveAs.Dialog.FileName := CurrentFile;
  SetDlgInitialDir(actFileSaveAs.Dialog, stvFolders.Path);

  actFileSaveAs.Dialog.Filter :=
    rsFSMScriptFileMaskDesc + '|' + kFSMScriptFileMask + '|' + rsFileDlgMaskDef;
  actFileSaveAs.Dialog.DefaultExt := kFSMScriptFileExt;
end;

procedure TfmCHXScriptManager.actFileSaveExecute(Sender: TObject);
begin
  if FileExistsUTF8(CurrentFile) then
  begin
    SynEdit.Lines.SaveToFile(CurrentFile);
    SynEdit.Modified := False;
    lbxInfo.Items.Add(Format(rsFSMFileSaved, [CurrentFile]));
  end
  else
    actFileSaveAs.Execute;
end;

procedure TfmCHXScriptManager.actOpenFileFolderExecute(Sender: TObject);
begin
  OpenDocument(slvFiles.Root);
end;

procedure TfmCHXScriptManager.actOpenRootFolderExecute(Sender: TObject);
begin
  OpenDocument(stvFolders.Root);
end;

procedure TfmCHXScriptManager.actOutputClearExecute(Sender: TObject);
begin
  mOutPut.Clear;
end;

procedure TfmCHXScriptManager.actOutputFontEditAccept(Sender: TObject);
begin
  mOutPut.Font.Assign(actOutputFontEdit.Dialog.Font);
end;

procedure TfmCHXScriptManager.actOutputFontEditBeforeExecute(Sender: TObject);
begin
  actOutputFontEdit.Dialog.Font.Assign(mOutPut.Font);
end;

procedure TfmCHXScriptManager.actOutputSaveAsAccept(Sender: TObject);
begin
  mOutPut.Lines.SaveToFile(actOutputSaveAs.Dialog.FileName);
  lbxInfo.Items.Add(Format(rsFSMFileSaved, [actOutputSaveAs.Dialog.FileName]));
end;

procedure TfmCHXScriptManager.actOutputSaveAsBeforeExecute(Sender: TObject);
begin
  actOutputSaveAs.Dialog.FileName := ChangeFileExt(CurrentFile, '.txt');
  SetDlgInitialDir(actOutputSaveAs.Dialog, stvFolders.Path);

  actOutputSaveAs.Dialog.Filter :=
    'Text file|*.txt|' + rsFileDlgMaskDef;
  actOutputSaveAs.Dialog.DefaultExt := '.txt';
end;

procedure TfmCHXScriptManager.actStopExecute(Sender: TObject);
begin
  Stop;
end;

procedure TfmCHXScriptManager.lbxInfoDblClick(Sender: TObject);
var
  aPosXY: TPoint;
  aStr: string;
  aPos, Col, Row: integer;
begin
  aStr := lbxInfo.Items[lbxInfo.ItemIndex];

  aPos := Pos('(', aStr);
  if aPos = 0 then
    Exit;
  aStr := Copy(aStr, aPos + 1, MaxInt);
  aPos := Pos(':', aStr);
  if aPos = 0 then
    Exit;
  Row := StrToIntDef(Copy(aStr, 1, aPos - 1), 0);
  if Row = 0 then
    Exit;
  aStr := Copy(aStr, aPos + 1, MaxInt);
  aPos := Pos(')', aStr);
  Col := StrToIntDef(Copy(aStr, 1, aPos - 1), 0);
  if Col = 0 then
    Exit;

  aPosXY.X := Col;
  aPosXY.Y := Row;

  SynEdit.CaretXY := aPosXY;
  if SynEdit.CanFocus then
    SynEdit.SetFocus;
end;

procedure TfmCHXScriptManager.SetCurrentFile(AValue: string);
begin
  if FCurrentFile = AValue then
    Exit;
  FCurrentFile := AValue;
end;

procedure TfmCHXScriptManager.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmCHXScriptManager.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmCHXScriptManager.SetScriptEngine(AValue: cCHXScriptEngine);
begin
  if FScriptEngine = AValue then
    Exit;
  FScriptEngine := AValue;
end;

procedure TfmCHXScriptManager.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  inherited DoLoadGUIConfig(aIniFile);

  GUIConfigIni := aIniFile.FileName;

  LoadFontFromIni(aIniFile, krsIniScriptMngSection,
    krsIniScriptMngSourceFont, SynEdit.Font);
  LoadFontFromIni(aIniFile, krsIniScriptMngSection,
    krsIniScriptMngOutputFont, mOutPut.Font);
end;

procedure TfmCHXScriptManager.DoSaveGUIConfig(aIniFile: TIniFile);
begin
  inherited DoSaveGUIConfig(aIniFile);

  SaveFontToIni(aIniFile, krsIniScriptMngSection,
    krsIniScriptMngSourceFont, SynEdit.Font);
  SaveFontToIni(aIniFile, krsIniScriptMngSection,
    krsIniScriptMngOutputFont, mOutPut.Font);
end;

procedure TfmCHXScriptManager.DoLoadGUIIcons(aIconsIni: TIniFile;
  const aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIconsIni, aBaseFolder);

  GUIIconsIni := aIconsIni.FileName;
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Name, ilActions, ActionList);
end;

procedure TfmCHXScriptManager.CreateCustomEngine;
begin
  // An inherited form can override with it's own ScripEngine.
  if not assigned(ScriptEngine) then
    FScriptEngine := cCHXScriptEngine.Create;


  // TODO: READ TfmCHXScriptManager.Compile
  // if not assigned(ScriptEngine.ScriptError) then
  //   ScriptEngine.ScriptError := lbxInfo.Items;

  // Basic I/O and Dialogs
  if not assigned(ScriptEngine.OnWriteLn) then
    ScriptEngine.OnWriteLn := @DoWriteLn;
  if not assigned(ScriptEngine.OnReadLn) then
    ScriptEngine.OnReadLn := @DoReadLn;
  if not assigned(ScriptEngine.OnAskFile) then
    ScriptEngine.OnAskFile := @DoAskFile;
  if not assigned(ScriptEngine.OnAskMultiFile) then
    ScriptEngine.OnAskMultiFile := @DoAskMultiFile;
  if not assigned(ScriptEngine.OnAskFolder) then
    ScriptEngine.OnAskFolder := @DoAskFolder;
  if not assigned(ScriptEngine.OnAskOption) then
    ScriptEngine.OnAskOption := @DoAskOption;
  if not assigned(ScriptEngine.OnAskYesNoCancel) then
    ScriptEngine.OnAskYesNoCancel := @DoAskYesNoCancel;

  // Debug
  if not assigned(ScriptEngine.OnLine) then
    ScriptEngine.OnLine := @DoOnline;
end;

procedure TfmCHXScriptManager.LoadScriptFile(const aFile: string);
var
  i: SizeInt;
  aIni: TIniFile;
begin
  CheckChanged;

  mScriptInfo.Clear;
  lbxInfo.Clear;

  CurrentFile := aFile;

  if not FileExistsUTF8(CurrentFile) then
  begin
    lCurrentFile.Caption := ' '; // An space to keeo size
    SynEdit.Lines.Clear;
    Exit;
  end;

  lCurrentFile.Caption := aFile;

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
  SynEdit.Modified := False;
end;

procedure TfmCHXScriptManager.CheckChanged;
begin
  if SynEdit.Modified then
    if MessageDlg(Format(rsFSMAskSaveChanges, [CurrentFile]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      actFileSave.Execute;
end;

function TfmCHXScriptManager.Compile: boolean;
begin
  ScriptEngine.ScriptFile := CurrentFile;
  ScriptEngine.ScriptText := SynEdit.Lines;

  // TODO: IF ASSIGNED IN CreateCustomEngine THIS IS LOST!!!!????
  //   OR WORSE... IT CHANGES TO A TFONT CLASS !!!!

  if not Assigned(ScriptEngine.ScriptError) then
    ScriptEngine.ScriptError := lbxInfo.Items;

  Result := ScriptEngine.CompileScript;
end;

function TfmCHXScriptManager.Execute: boolean;
begin
  PageControl.ActivePage := pagOutput;

  Result := Compile;

  if not Result then
    Exit;

  actStop.Enabled := True;
  Result := ScriptEngine.RunScript;
  actStop.Enabled := False;
end;

procedure TfmCHXScriptManager.Stop;
begin
  ScriptEngine.Stop;
end;

procedure TfmCHXScriptManager.UpdateSLV;
begin
  slvFiles.Update;
end;

procedure TfmCHXScriptManager.DoWriteLn(const aStr: string);
begin
  mOutPut.Lines.Add(aStr);

  // TODO: Maybe this must go with a timer...
  Application.ProcessMessages;
end;

function TfmCHXScriptManager.DoReadLn(
  const aQuestion, DefAnswer: string): string;
begin
  Result := InputBox(Application.Title, aQuestion, DefAnswer);
end;

function TfmCHXScriptManager.DoAskFile(
  const aCaption, aExtFilter, DefFile: string): string;
begin
  Result := '';
  OpenDialog1.Title := aCaption;
  OpenDialog1.FileName := DefFile;
  SetDlgInitialDir(OpenDialog1, DefFile);
  OpenDialog1.Filter := aExtFilter;
  if OpenDialog1.Execute then
    Result := OpenDialog1.FileName;
end;

procedure TfmCHXScriptManager.SetBaseFolder(const aFolder: string);
begin
  // Fixing SIGENV error on create frame (1/2)
  slvFiles.ShellTreeView := nil;

  // Fixing SIGENV error on create frame (2/2)
  stvFolders.Root := SysPath(aFolder);

  slvFiles.ShellTreeView := stvFolders;
end;

class function TfmCHXScriptManager.SimpleForm(aBaseFolder: string;
  aGUIConfigIni: string; aGUIIconsIni: string): integer;
var
  aFrame: TfmCHXScriptManager;
begin
  aFrame := TfmCHXScriptManager.Create(nil);

  aFrame.SetBaseFolder(aBaseFolder);

  Result := GenSimpleModalForm(aFrame, krsFormCHXScriptMName,
    krsFormCHXScriptMTitle, aGUIConfigIni, aGUIIconsIni);
end;

procedure TfmCHXScriptManager.DoAskMultiFile(aFileList: TStrings;
  const aCaption, aExtFilter, DefFolder: string);
begin
  TfmSMAskMultiFile.SimpleForm(aFileList, aCaption, aExtFilter,
    DefFolder, GUIConfigIni, GUIIconsIni);
end;

function TfmCHXScriptManager.DoAskFolder(
  const aCaption, DefFolder: string): string;
begin
  Result := EmptyStr;
  SelectDirectoryDialog1.Title := aCaption;
  SetDlgInitialDir(SelectDirectoryDialog1, DefFolder);
  if SelectDirectoryDialog1.Execute then
    Result := IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
end;

function TfmCHXScriptManager.DoAskOption(const aCaption, aQuestion: string;
  aOptionList: TStrings): integer;

begin
  Result := -1;
  if TfmSMAskOption.SimpleForm(aCaption, aQuestion, aOptionList,
    Result, GUIConfigIni, GUIIconsIni) <> mrOk then
    Result := -1;
end;

function TfmCHXScriptManager.DoAskYesNoCancel(
  const aCaption, aQuestion: string): integer;
begin
  Result := MessageDlg(aCaption, aQuestion, mtConfirmation, mbYesNoCancel, 0);
end;

procedure TfmCHXScriptManager.DoOnline(Sender: TObject);
begin
  Application.ProcessMessages;
end;

constructor TfmCHXScriptManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  PageControl.ActivePage := pagScriptList;

  CreateCustomEngine;

  Enabled := assigned(ScriptEngine);
end;

destructor TfmCHXScriptManager.Destroy;
begin
  CheckChanged;
  FScriptEngine.Free;
  inherited Destroy;
end;

initialization
  RegisterClass(TfmCHXScriptManager);

finalization
  UnRegisterClass(TfmCHXScriptManager);
end.
{
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
}
