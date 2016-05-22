{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

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

{ Unit of Script Manager form. }
unit ufCHXScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ActnList, Controls, ComCtrls, ExtCtrls, LResources,
  StdCtrls, ShellCtrls, FileUtil, Dialogs, StdActns, Buttons, EditBtn, IniFiles,
  SynHighlighterPas, SynEdit, SynMacroRecorder, LazFileUtils,
  uCHXScriptEngine,
  // CHX
  uCHXStrUtils; //, uCHXImageUtils;

const
  kFileExtensionScript = 'pas';
    kFileMaskScript = '*.' + kFileExtensionScript;
    kFileMaskAllFiles = '*.*';
    kFSMDataSection = 'SCRIPTDATA';

resourcestring
  rsFSMScriptFileSaved = 'File saved: %s';
  rsFileMaskScriptDescription = 'Pascal script file';
  rsFileMaskAllFilesDescription = 'All files';
  rsFSMSaveChanges = '%s has changed. Save';


type

  TSMLoadingListCallBack = function(const Game, Version: string;
    const Max, Value: int64): boolean of object;

  { TfrmScriptManager }

  TfrmScriptManager = class(TForm)
    actCompile: TAction;
    actExecute: TAction;
    ActionList: TActionList;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditDelete: TEditDelete;
    bExecute2: TBitBtn;
    actFileSaveAs: TFileSaveAs;
    DirectoryEdit1: TDirectoryEdit;
    gbxScript: TGroupBox;
    ilActions: TImageList;
    lGame: TLabel;
    lGroup: TLabel;
    lSystem: TLabel;
    mInfo: TMemo;
    mOutPut: TMemo;
    PageControl: TPageControl;
    Panel1: TPanel;
    pCurrentData: TPanel;
    pRight: TPanel;
    pInfo: TPanel;
    sbInfo: TStatusBar;
    pagGeneralScriptList: TTabSheet;
    pagSourceCode: TTabSheet;
    pagOutput: TTabSheet;
    actSearchFind: TSearchFind;
    actSearchReplace: TSearchReplace;
    ShellTreeView1: TShellTreeView;
    slvGeneral: TShellListView;
    Splitter1: TSplitter;
    SynEdit: TSynEdit;
    SynFreePascalSyn: TSynFreePascalSyn;
    SynMacroRecorder: TSynMacroRecorder;
    tbEditor: TToolBar;
    bEditCopy: TToolButton;
    bEditCut: TToolButton;
    bEditDelete: TToolButton;
    bEditPaste: TToolButton;
    bSeparator1: TToolButton;
    bEditSelectAll: TToolButton;
    bSeparator2: TToolButton;
    bEditUndo: TToolButton;
    bSeparator3: TToolButton;
    bSearchFind: TToolButton;
    bSearchReplace: TToolButton;
    bSeparator4: TToolButton;
    bSaveFileAs: TToolButton;
    bSeparator5: TToolButton;
    bCompile: TToolButton;
    bExecute: TToolButton;
    procedure actCompileExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actFileSaveAsAccept(Sender: TObject);
    procedure actFileSaveAsBeforeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure slvSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);

  private
    { private declarations }
    FCurrentFile: string;
    FScriptEngine: cScriptEngEmuteca;
    FScriptFolder: string;
    procedure SetCurrentFile(AValue: string);
    procedure SetScriptEngine(AValue: cScriptEngEmuteca);
    procedure SetScriptFolder(AValue: string);

  protected
    property CurrentFile: string read FCurrentFile write SetCurrentFile;
    property ScriptEngine: cScriptEngEmuteca
      read FScriptEngine write SetScriptEngine;

    procedure LoadScriptFile(const aFile: string);

    function Compile: boolean;
    function Execute: boolean;

    procedure UpdateSLV;

  public

  end;

var
  frmScriptManager: TfrmScriptManager;

implementation

{ TfrmScriptManager }

procedure TfrmScriptManager.slvSelectItem(Sender: TObject;
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

procedure TfrmScriptManager.actCompileExecute(Sender: TObject);
begin
  Compile;
end;

procedure TfrmScriptManager.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;

procedure TfrmScriptManager.actFileSaveAsAccept(Sender: TObject);
begin
  SynEdit.Lines.SaveToFile(actFileSaveAs.Dialog.FileName);

  mInfo.Lines.Add(Format(rsFSMScriptFileSaved, [actFileSaveAs.Dialog.FileName]));
  CurrentFile := actFileSaveAs.Dialog.FileName;
  UpdateSLV;
  if SynEdit.CanFocus then
    SynEdit.SetFocus;
end;

procedure TfrmScriptManager.actFileSaveAsBeforeExecute(Sender: TObject);
begin
  actFileSaveAs.Dialog.InitialDir := ExtractFileDir(CurrentFile);
  actFileSaveAs.Dialog.FileName := ExtractFileName(CurrentFile);

  actFileSaveAs.Dialog.Filter := rsFileMaskScriptDescription + '|' + kFileMaskScript
  + '|' + rsFileMaskAllFilesDescription + '|' + kFileMaskAllFiles;
  actFileSaveAs.Dialog.DefaultExt := kFileExtensionScript;
end;

procedure TfrmScriptManager.FormCreate(Sender: TObject);
begin
  FScriptEngine := cScriptEngEmuteca.Create;
end;

procedure TfrmScriptManager.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScriptEngine);
end;

procedure TfrmScriptManager.SetCurrentFile(AValue: string);
begin
  if FCurrentFile = AValue then
    Exit;
  FCurrentFile := AValue;
end;

procedure TfrmScriptManager.SetScriptEngine(AValue: cScriptEngEmuteca);
begin
  if FScriptEngine = AValue then
    Exit;
  FScriptEngine := AValue;
end;

procedure TfrmScriptManager.SetScriptFolder(AValue: string);
begin
  if FScriptFolder = AValue then
    Exit;
  FScriptFolder := AValue;
end;

procedure TfrmScriptManager.LoadScriptFile(const aFile: string);
var
  i: SizeInt;
  aIni: TIniFile;
begin
  if SynEdit.Modified then
    if MessageDlg(Format(rsFSMSaveChanges, [CurrentFile]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      SynEdit.Lines.SaveToFile(CurrentFile);

  CurrentFile := aFile;

  if not FileExistsUTF8(CurrentFile) then
  begin
    SynEdit.Lines.Clear;
    Exit;
  end;

  SynEdit.Lines.LoadFromFile(CurrentFile);

  // Removing UTF-8 BOM...
  if SynEdit.Lines.Count > 0 then
    i := Pos(UTF8FileHeader, SynEdit.Lines[0]);
  if i = 1 then
    SynEdit.Lines[0] := Copy(SynEdit.Lines[0], Length(UTF8FileHeader) +
      1, MaxInt);

  mInfo.Clear;

  // TODO 2: Temporal script info until section is parsed properly.
  aIni := TIniFile.Create(CurrentFile, True);
  try
    aIni.ReadSectionRaw(kFSMDataSection, mInfo.Lines);
  finally
    FreeAndNil(aIni);
  end;
end;

function TfrmScriptManager.Compile: boolean;
begin
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

function TfrmScriptManager.Execute: boolean;
begin
  PageControl.ActivePage := pagOutput;

  Result := Compile;

  if not Result then
    Exit;

  Result := ScriptEngine.RunScript;
end;

procedure TfrmScriptManager.UpdateSLV;
begin
  slvGeneral.Update;
end;

initialization
  {$I ufCHXScriptManager.lrs}

end.
