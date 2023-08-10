unit ucCHXScriptEngine;

{< cCHXScriptEngine class unit.

  Copyright (C) 2006-2023 Chixpy

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
{$mode objfpc}{$H+}
{$DEFINE PS_USESSUPPORT}

interface

uses
  // Common units
  Classes, SysUtils, Controls, LazUTF8, Dialogs,
  Forms, LazFileUtils,
  // Pascal Script main units
  uPSComponent, uPSRuntime, uPSCompiler, uPSUtils,
  // Pascal script common units import
  uPSR_std, uPSR_controls, uPSR_stdctrls, uPSR_forms, uPSR_buttons,
  uPSR_classes, uPSR_dateutils, uPSR_dll, uPSR_DB, uPSR_extctrls,
  uPSR_graphics, uPSR_menus, uPSR_comobj,
  uPSC_std, uPSC_controls, uPSC_stdctrls,
  uPSC_forms, uPSC_buttons, uPSC_classes, uPSC_dateutils, uPSC_dll,
  uPSC_DB, uPSC_extctrls, uPSC_graphics, uPSC_menus, uPSC_comobj,
  // CHX
  uCHXStrUtils, uCHX7zWrapper,
  // CHX Imported units
  uPSI_CHXBasic, uPSI_FPCDateUtils,
  uPSI_FPCSysUtils, uPSI_FPCStrUtils, uPSI_FPCLazUTF8, uPSI_FPCFileUtil,
  uPSI_uCHXStrUtils, uPSI_uCHXFileUtils,
  uPSI_u7zWrapper,
  uPSI_uaCHXStorable;

const
  rsSEMsgFormat = '[%0:s](%1:.4d:%2:.4d): %3:s';
  {< Format for messages.

    @definitionList(
      @itemLabel(0)
        @item(String of error level: rsSEELError, rsSEELWarning, rsSEELInfo,
          rsSEELOK)
      @itemLabel(1, 2)
        @item(Integers of row and col of the message.)
      @itemLabel(3)
        @item(String of actual message test.)
    )
  }

resourcestring
  // Error levels
  rsSEELError = 'Error';
  rsSEELWarning = 'Warning';
  rsSEELInfo = 'Info';
  rsSEELOK = 'OK';

  // Compilation / Execute messajes
  rsSEECompilationMsg = 'Compiling script.';
  rsSEEExecutionMsg = 'Executing script.';
  rsSEEFinishedMsg = 'Script finished.';

type
  TCHXSEWriteLnCB = procedure(const aStr: string) of object;
  TCHXSEReadLnCB = function(const aQuestion, DefAnswer: string): string of
    object;
  TCHXSEAskFileCB = function(
    const aCaption, aExtFilter, DefFile: string): string of object;
  TCHXSEAskMultiFileCB = procedure(aFileList: TStrings;
    const aCaption, aExtFilter, DefFolder: string) of object;
  TCHXSEAskFolderCB = function(const aCaption, DefFolder: string): string of
    object;
  TCHXSEAskOptionCB = function(const aCaption, aQuestion: string;
    aOptionList: TStrings): integer of object;
  TCHXSEAskYesNoCancelCB = function(
    const aCaption, aQuestion: string): integer of object;

  { cCHXScriptEngine }

  cCHXScriptEngine = class(TObject)
  private
    FCommonUnitFolder: string;
    FOnAskFile: TCHXSEAskFileCB;
    FOnAskFolder: TCHXSEAskFolderCB;
    FOnAskMultiFile: TCHXSEAskMultiFileCB;
    FOnAskOption: TCHXSEAskOptionCB;
    FOnAskYesNoCancel: TCHXSEAskYesNoCancelCB;
    FOnLine: TNotifyEvent;
    FOnReadLn: TCHXSEReadLnCB;
    FOnWriteLn: TCHXSEWriteLnCB;
    FPasScript: TPSScriptDebugger;
    FScriptError: TStrings;
    function getScriptFile: string;
    function getScriptText: TStrings;
    procedure SetCommonUnitFolder(AValue: string);
    procedure SetOnAskFile(AValue: TCHXSEAskFileCB);
    procedure SetOnAskFolder(AValue: TCHXSEAskFolderCB);
    procedure SetOnAskMultiFile(AValue: TCHXSEAskMultiFileCB);
    procedure SetOnAskOption(AValue: TCHXSEAskOptionCB);
    procedure SetOnAskYesNoCancel(AValue: TCHXSEAskYesNoCancelCB);
    procedure SetOnLine(AValue: TNotifyEvent);
    procedure SetOnReadLn(AValue: TCHXSEReadLnCB);
    procedure SetOnWriteLn(AValue: TCHXSEWriteLnCB);
    procedure SetPasScript(AValue: TPSScriptDebugger);
    procedure SetScriptError(AValue: TStrings);
    procedure setScriptFile(AValue: string);
    procedure setScriptText(AValue: TStrings);

  protected
    property PasScript: TPSScriptDebugger read FPasScript write SetPasScript;
    {< PSScript object.}

    procedure PasScriptOnCompImport(Sender: TObject;
      x: TPSPascalCompiler); virtual;
    procedure PasScriptOnCompile(Sender: TPSScript); virtual;
    procedure PasScriptOnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter); virtual;
    procedure PasScriptOnExecute(Sender: TPSScript); virtual;
    function PasScriptOnFindUnknownFile(Sender: TObject;
      const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; virtual;
    function PasScriptOnNeedFile(Sender: TObject;
      const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; virtual;

    // Added functions
    // ---------------

    // CallBacks
    procedure CHXWriteLn(const aStr: string);
    function CHXReadLn(const aQuestion, DefAnswer: string): string;
    function CHXAskFile(const aCaption, aExtFilter, DefFile: string): string;
    procedure CHXAskMultiFile(aFileList: TStrings;
      const aCaption, aExtFilter, DefFolder: string);
    function CHXAskFolder(const aCaption, DefFolder: string): string;
    function CHXAskOption(const aCaption, aQuestion: string;
      aOptionList: TStrings): integer;
    function CHXAskYesNoCancel(const aCaption, aQuestion: string): integer;

    // HACK: We can't create Stringlists!!!
    // TODO: Make a generic constructor?
    function CHXCreateStringList: TStringList;

  public
    property ScriptFile: string read getScriptFile write setScriptFile;
    property CommonUnitFolder: string
      read FCommonUnitFolder write SetCommonUnitFolder;

    property ScriptText: TStrings read getScriptText write setScriptText;

    property ScriptError: TStrings read FScriptError write SetScriptError;

    property OnLine: TNotifyEvent read FOnLine write SetOnLine;

    property OnWriteLn: TCHXSEWriteLnCB read FOnWriteLn write SetOnWriteLn;
    property OnReadLn: TCHXSEReadLnCB read FOnReadLn write SetOnReadLn;
    property OnAskFile: TCHXSEAskFileCB read FOnAskFile write SetOnAskFile;
    property OnAskMultiFile: TCHXSEAskMultiFileCB
      read FOnAskMultiFile write SetOnAskMultiFile;
    property OnAskFolder: TCHXSEAskFolderCB
      read FOnAskFolder write SetOnAskFolder;
    property OnAskOption: TCHXSEAskOptionCB
      read FOnAskOption write SetOnAskOption;
    property OnAskYesNoCancel: TCHXSEAskYesNoCancelCB
      read FOnAskYesNoCancel write SetOnAskYesNoCancel;

    function RunScript: boolean;
    procedure Stop;
    function CompileScript: boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cCHXScriptEngine }


function cCHXScriptEngine.getScriptFile: string;
begin
  Result := PasScript.MainFileName;
end;

function cCHXScriptEngine.getScriptText: TStrings;
begin
  Result := PasScript.Script;
end;

procedure cCHXScriptEngine.SetCommonUnitFolder(AValue: string);
begin
  FCommonUnitFolder := SetAsFolder(AValue);
end;

procedure cCHXScriptEngine.SetOnAskFile(AValue: TCHXSEAskFileCB);
begin
  if FOnAskFile = AValue then
    Exit;
  FOnAskFile := AValue;
end;

procedure cCHXScriptEngine.SetOnAskFolder(AValue: TCHXSEAskFolderCB);
begin
  if FOnAskFolder = AValue then
    Exit;
  FOnAskFolder := AValue;
end;

procedure cCHXScriptEngine.SetOnAskMultiFile(AValue: TCHXSEAskMultiFileCB);
begin
  if FOnAskMultiFile = AValue then
    Exit;
  FOnAskMultiFile := AValue;
end;

procedure cCHXScriptEngine.SetOnAskOption(AValue: TCHXSEAskOptionCB);
begin
  if FOnAskOption = AValue then Exit;
  FOnAskOption := AValue;
end;

procedure cCHXScriptEngine.SetOnAskYesNoCancel(AValue: TCHXSEAskYesNoCancelCB);
begin
  if FOnAskYesNoCancel = AValue then Exit;
  FOnAskYesNoCancel := AValue;
end;

procedure cCHXScriptEngine.SetOnLine(AValue: TNotifyEvent);
begin
  if FOnLine = AValue then Exit;
  FOnLine := AValue;

  PasScript.OnLine := OnLine;
end;

procedure cCHXScriptEngine.SetOnReadLn(AValue: TCHXSEReadLnCB);
begin
  if FOnReadLn = AValue then
    Exit;
  FOnReadLn := AValue;
end;

procedure cCHXScriptEngine.SetOnWriteLn(AValue: TCHXSEWriteLnCB);
begin
  if FOnWriteLn = AValue then
    Exit;
  FOnWriteLn := AValue;
end;

procedure cCHXScriptEngine.SetPasScript(AValue: TPSScriptDebugger);
begin
  if FPasScript = AValue then
    Exit;
  FPasScript := AValue;
end;

procedure cCHXScriptEngine.SetScriptError(AValue: TStrings);
begin
  if FScriptError = AValue then
    Exit;
  FScriptError := AValue;
end;

procedure cCHXScriptEngine.setScriptFile(AValue: string);
begin
  PasScript.MainFileName := SetAsFile(AValue);
end;

procedure cCHXScriptEngine.setScriptText(AValue: TStrings);
begin
  PasScript.Script := AValue;
end;

procedure cCHXScriptEngine.PasScriptOnCompile(Sender: TPSScript);
begin
  // Input and Output
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXWriteLn,
    'procedure WriteLn(const s: String);');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXReadLn,
    'function ReadLn(const aQuestion, DefAnswer: String): String;');

  // Dialogs
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskFile,
    'function AskFile(const aTitle, aExt, DefFile: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskMultiFile,
    'procedure AskMultiFile(aFileList: TStrings; const aTitle: string;' +
    ' const aExtFilter: string; const DefFolder: string)');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskFolder,
    'function AskFolder(const aTitle, DefFolder: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskOption,
    'function AskOption(const aCaption, aQuestion: string;' +
    ' aOptionList: TStrings): integer');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskYesNoCancel,
    'function AskYesNoCancel(const aCaption, aQuestion: string): integer');

  // Things not imported

  // HACK: We can't create Stringlist!!!
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXCreateStringList,
    'function CreateStringList: TStringList;');

end;

procedure cCHXScriptEngine.PasScriptOnExecute(Sender: TPSScript);
begin

end;

function cCHXScriptEngine.PasScriptOnFindUnknownFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
var
  FullFileName: string;
  fStream: TFileStream;
begin

  // TODO: Remove this info... or make a error level filter...
  if Assigned(ScriptError) then
  begin
    ScriptError.Add(Format(rsSEMsgFormat, [rsSEELInfo, 0, 0,
      'PasScriptOnFindUnknownFile']));
    ScriptError.Add('- OriginFileName: ' + OriginFileName);
    ScriptError.Add('- FileName: ' + FileName);
  end;

  Result := False;

  FullFileName := SetAsFolder(ExtractFilePath(OriginFileName)) +
    FileName + '.pas';
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := SetAsFolder(CommonUnitFolder) + FileName + '.pas';
    if not FileExistsUTF8(FullFileName) then
      Exit;
  end;

  try
    fStream := TFileStream.Create(FullFileName, fmOpenRead or
      fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, fStream.Size);
    fStream.Read(Output[1], Length(Output));
  finally
    fStream.Free;
  end;
  Result := True;
end;

function cCHXScriptEngine.PasScriptOnNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
var
  FullFileName: string;
  fStream: TFileStream;
begin
  FileName := AnsiDequotedStr(FileName, '''');
  FileName := AnsiDequotedStr(FileName, '"');

  Result := False;
  FullFileName := CleanAndExpandFilename(
    SetAsFolder(ExtractFilePath(OriginFileName)) + FileName);
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := CleanAndExpandFilename(SetAsFolder(CommonUnitFolder) +
      FileName);
    if not FileExistsUTF8(FullFileName) then
    begin
      if Assigned(ScriptError) then
      begin
        Result := True; // Don't halt or create an exception.

        ScriptError.Add(Format(rsSEMsgFormat,
          [rsSEELWarning, 0, 0, 'PasScriptOnFindUnknownFile']));
        ScriptError.Add('- $I file not found: ' + FileName +
          ' (' + OriginFileName + ')');
      end;
      Exit;
    end;
  end;

  try
    fStream := TFileStream.Create(FullFileName, fmOpenRead or
      fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, fStream.Size);
    fStream.Read(Output[1], Length(Output));
  finally
    fStream.Free;
  end;
  Result := True;
end;

procedure cCHXScriptEngine.CHXWriteLn(const aStr: string);
begin
  if Assigned(OnWriteLn) then
    OnWriteLn(aStr)
  else
    raise ENotImplemented.Create('OnWriteLn not assigned.');
end;

function cCHXScriptEngine.CHXReadLn(
  const aQuestion, DefAnswer: string): string;
begin
  Result := '';
  if Assigned(OnReadLn) then
    Result := OnReadLn(aQuestion, DefAnswer)
  else
    raise ENotImplemented.Create('OnReadLn not assigned.');
end;

function cCHXScriptEngine.CHXAskFile(
  const aCaption, aExtFilter, DefFile: string): string;
begin
  Result := '';
  if Assigned(OnAskFile) then
    Result := OnAskFile(aCaption, aExtFilter, DefFile)
  else
    raise ENotImplemented.Create('OnAskFile not assigned.');
end;

procedure cCHXScriptEngine.CHXAskMultiFile(aFileList: TStrings;
  const aCaption, aExtFilter, DefFolder: string);
begin
  if Assigned(OnAskMultiFile) then
    OnAskMultiFile(aFileList, aCaption, aExtFilter, DefFolder)
  else
    raise ENotImplemented.Create('OnAskMultiFile not assigned.');
end;

function cCHXScriptEngine.CHXAskFolder(
  const aCaption, DefFolder: string): string;
begin
  Result := '';

  if Assigned(OnAskFolder) then
    Result := OnAskFolder(aCaption, DefFolder)
  else
    raise ENotImplemented.Create('OnAskFolder not assigned.');
end;

function cCHXScriptEngine.CHXAskOption(const aCaption, aQuestion: string;
  aOptionList: TStrings): integer;
begin
  Result := -1;

  if Assigned(OnAskOption) then
    Result := OnAskOption(aCaption, aQuestion, aOptionList)
  else
    raise ENotImplemented.Create('OnAskOption not assigned.');
end;

function cCHXScriptEngine.CHXAskYesNoCancel(
  const aCaption, aQuestion: string): integer;
begin
  Result := mrCancel;

  if Assigned(OnAskYesNoCancel) then
    Result := OnAskYesNoCancel(aCaption, aQuestion)
  else
    raise ENotImplemented.Create('OnAskYesNoCancel not assigned.');
end;

function cCHXScriptEngine.CHXCreateStringList: TStringList;
begin
  Result := TStringList.Create;
end;

procedure cCHXScriptEngine.PasScriptOnCompImport(Sender: TObject;
  x: TPSPascalCompiler);
begin
  RegisterDateTimeLibrary_C(x);
  RegisterDll_Compiletime(x);
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  // HACK: Adding TStrings.AddDelimitedText
  x.FindClass('TStrings').RegisterMethod('procedure AddDelimitedText(' +
    'const S: String; ADelimiter: char; AStrictDelimiter: Boolean);');
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_StdCtrls(x);
  SIRegister_ExtCtrls(x);
  SIRegister_Forms(x);
  SIRegister_Buttons(x);
  SIRegister_Menus(x);
  SIRegister_DB(x);
  SIRegister_ComObj(x);

  // CHX Basic
  SIRegister_CHXBasic(x);

  // FPC
  SIRegister_FPCSysUtils(x);
  SIRegister_FPCStrUtils(x);
  SIRegister_FPCLazUTF8(x);
  SIRegister_FPCFileUtil(x);
  SIRegister_FPCDateUtils(x);

  // CHX units
  SIRegister_u7zWrapper(x);
  SIRegister_uCHXStrUtils(x);
  SIRegister_uCHXFileUtils(x);

  // CHX abstracts
  SIRegister_uaCHXStorable(x);
end;

procedure cCHXScriptEngine.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RegisterDateTimeLibrary_R(se);
  RegisterDLLRuntime(se);
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  // HACK: Adding TStrings.AddDelimitedText
  x.FindClass('TStrings').RegisterMethod(@TStrings.AddDelimitedText,
    'AddDelimitedText');
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_ExtCtrls(x);
  RIRegister_Forms(x);
  RIRegister_Buttons(x);
  RIRegister_Menus(x);
  RIRegister_DB(x);
  RIRegister_ComObj(se);

  // CHX Basic
  RIRegister_CHXBasic_Routines(se);

  // FPC units
  RIRegister_FPCSysUtils_Routines(se);
  RIRegister_FPCStrUtils_Routines(se);
  RIRegister_FPCLazUTF8_Routines(se);
  RIRegister_FPCFileUtil_Routines(se);
  RIRegister_FPCDateUtils_Routines(se);

  // CHX units
  RIRegister_u7zWrapper_Routines(se);
  RIRegister_uCHXStrUtils_Routines(se);
  RIRegister_uCHXFileUtils_Routines(se);

  // CHX abstracts
  RIRegister_uaCHXStorable(x);
end;

function cCHXScriptEngine.RunScript: boolean;
begin
  Result := False;
  if not CompileScript then
    Exit;

  if Assigned(ScriptError) then
    ScriptError.Add(Format(rsSEMsgFormat, [rsSEELOK, 0, 0,
      rsSEEExecutionMsg]));

  Result := PasScript.Execute;

  if Result then
  begin
    if Assigned(ScriptError) then
      ScriptError.Add(Format(rsSEMsgFormat, [rsSEELOK, 0, 0,
        rsSEEFinishedMsg]));
    Exit;
  end;

  if not Assigned(ScriptError) then
    Exit;

  ScriptError.BeginUpdate;
  ScriptError.Add(Format(rsSEMsgFormat, [rsSEELError, 0, 0,
    rsSEEExecutionMsg]));
  ScriptError.Add(PasScript.ExecErrorToString);
  ScriptError.Add(Format('[Runtime error] %s(%d:%d)',
    [ScriptFile, PasScript.ExecErrorRow, PasScript.ExecErrorCol]));
  ScriptError.Add(Format('bytecode(%d:%d): %s',
    [PasScript.ExecErrorProcNo, PasScript.ExecErrorByteCodePosition,
    PasScript.ExecErrorToString]));
  ScriptError.EndUpdate;
end;

procedure cCHXScriptEngine.Stop;
begin
  PaSScript.Stop;
end;

function cCHXScriptEngine.CompileScript: boolean;
var
  i: integer;
begin
  if Assigned(ScriptError) then
    ScriptError.Clear;

  if Assigned(ScriptError) then
    ScriptError.Add(Format(rsSEMsgFormat, [rsSEELOK, 0, 0,
      rsSEECompilationMsg]));

  Result := PaSScript.Compile;

  if Result then
    Exit;

  if not Assigned(ScriptError) then
    Exit;

  ScriptError.BeginUpdate;

  for i := 0 to PaSScript.CompilerMessageCount - 1 do
    ScriptError.add(PaSScript.CompilerMessages[i].MessageToString);

  ScriptError.Add(Format(rsSEMsgFormat, [rsSEELError, 0, 0,
    rsSEECompilationMsg]));

  for i := 0 to PaSScript.CompilerMessageCount - 1 do
    ScriptError.Add(PasScript.CompilerErrorToStr(i));
  ScriptError.EndUpdate;
end;

constructor cCHXScriptEngine.Create;
begin
  inherited Create;

  FPasScript := TPSScriptDebugger.Create(nil);
  PasScript.UsePreProcessor := True;
  PasScript.OnCompImport := @PasScriptOnCompImport;
  PasScript.OnCompile := @PasScriptOnCompile;
  PasScript.OnExecImport := @PasScriptOnExecImport;
  PasScript.OnExecute := @PasScriptOnExecute;
  PasScript.OnNeedFile := @PasScriptOnNeedFile;
  PasScript.OnFindUnknownFile := @PasScriptOnFindUnknownFile;
end;

destructor cCHXScriptEngine.Destroy;
begin
  PasScript.Free;
  inherited Destroy;
end;

end.
