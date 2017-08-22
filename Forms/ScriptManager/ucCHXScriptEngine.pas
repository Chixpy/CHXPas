{ Copyright (C) 2006-2017 Chixpy

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

{ Unit of Script Engine class. }
unit ucCHXScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  // Common units
  Classes, SysUtils, Controls, StrUtils, LazUTF8, Dialogs,
  Forms, LazFileUtils,
  // Pascal Script main units
  uPSComponent, uPSRuntime, uPSCompiler, uPSUtils,
  // Pascal script common units import
  uPSR_std, uPSR_controls, uPSR_stdctrls, uPSR_forms, uPSR_buttons,
  uPSR_classes, uPSR_dateutils, uPSR_dll, uPSR_DB, uPSR_extctrls,
  uPSR_graphics, uPSR_menus,
  uPSC_std, uPSC_controls, uPSC_stdctrls, uPSC_forms, uPSC_buttons,
  uPSC_classes, uPSC_dateutils, uPSC_dll, uPSC_DB, uPSC_extctrls,
  uPSC_graphics, uPSC_menus, uPSC_strutils,
  // CHX
  uCHXStrUtils, u7zWrapper,
  // Imported units
  uPSI_u7zWrapper,
  // TODO 2: Generalize input and output as events,
  //   and move these units to fScriptManager.
  ufSMAskFile, ufSMAskFolder;

resourcestring
  rsSEECompilationMsg = 'Compiling: %s.';
  rsSEEExecutionMsg = 'Executing: %s.';
  rsSEEError = 'Error!';
  rsSEEOK = 'Success!';


type

  { cCHXScriptEngine }

  cCHXScriptEngine = class(TObject)

  private
    FCommonUnitFolder: string;
    FOwnsScriptError: boolean;
    FOwnsScriptInfo: boolean;
    FOwnsScriptOutput: boolean;
    FPasScript: TPSScript;
    FScriptError: TStrings;
    FScriptInfo: TStrings;
    FScriptOutput: TStrings;
    function getScriptFile: string;
    function getScriptText: TStrings;
    procedure SetCommonUnitFolder(AValue: string);
    procedure SetOwnsScriptError(AValue: boolean);
    procedure SetOwnsScriptInfo(AValue: boolean);
    procedure SetOwnsScriptOutput(AValue: boolean);
    procedure SetPasScript(AValue: TPSScript);
    procedure SetScriptError(AValue: TStrings);
    procedure setScriptFile(AValue: string);
    procedure SetScriptInfo(AValue: TStrings);
    procedure SetScriptOutput(AValue: TStrings);
    procedure setScriptText(AValue: TStrings);

  protected
    property PasScript: TPSScript read FPasScript write SetPasScript;
    {< PSScript object.}

    property OwnsScriptOutput: boolean
      read FOwnsScriptOutput write SetOwnsScriptOutput;
    {< Script output must be freed? }
    property OwnsScriptInfo: boolean read FOwnsScriptInfo
      write SetOwnsScriptInfo;
    {< Script info must be freed? }
    property OwnsScriptError: boolean read FOwnsScriptError
      write SetOwnsScriptError;
    {< Script error must be freed? }

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
    // TODO: Make them external?.

    // Input / Output
    procedure CHXWriteLn(const Str: string);
    function CHXReadLn(const aQuestion, DefAnswer: string): string;

    // Strings
    function CHXRPos(const Substr, Source: string): integer;

    function CHXLowerCase(const AInStr: string): string;
    function CHXUpperCase(const AInStr: string): string;

    function CHXCompareText(const S1, S2: string): integer;
    function CHXCompareStr(const S1, S2: string): integer;

    function CHXUTF8ToSys(const S: string): string;
    function CHXSysToUTF8(const S: string): string;

    function CHXBoolToStr(const aBool:Boolean):string;

    // Path and filename strings
    function CHXCleanFileName(const AFileName: string): string;
    function CHXExcludeTrailingPathDelimiter(const aString: string): string;
    function CHXExtractFilePath(const aFileName: string): string;
    function CHXExtractFileName(const aFileName: string): string;
    function CHXExtractFileNameOnly(const AFilename: string): string;
    function CHXExtractFileExt(const AFilename: string): string;
    function CHXChangeFileExt(const aFileName, aExtension: string): string;

    // Files and Folders UTF8
    function CHXFileExistsUTF8(const aFileName: string): boolean;
    function CHXDirectoryExistsUTF8(const aFileName: string): boolean;

    // Dialog forms
    function CHXAskFile(const aTitle, aExt, DefFile: string): string;
    function CHXAskFolder(const aTitle, DefFolder: string): string;

    // HACK: We can't create Stringlist!!!
    function CHXCreateStringList: TStringList;

  public
    property ScriptFile: string read getScriptFile write setScriptFile;
    property CommonUnitFolder: string
      read FCommonUnitFolder write SetCommonUnitFolder;

    property ScriptText: TStrings read getScriptText write setScriptText;
    property ScriptOutput: TStrings read FScriptOutput write SetScriptOutput;
    property ScriptInfo: TStrings read FScriptInfo write SetScriptInfo;
    property ScriptError: TStrings read FScriptError write SetScriptError;

    function RunScript: boolean;
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

procedure cCHXScriptEngine.SetOwnsScriptError(AValue: boolean);
begin
  if FOwnsScriptError = AValue then
    Exit;
  FOwnsScriptError := AValue;
end;

procedure cCHXScriptEngine.SetOwnsScriptInfo(AValue: boolean);
begin
  if FOwnsScriptInfo = AValue then
    Exit;
  FOwnsScriptInfo := AValue;
end;

procedure cCHXScriptEngine.SetOwnsScriptOutput(AValue: boolean);
begin
  if FOwnsScriptOutput = AValue then
    Exit;
  FOwnsScriptOutput := AValue;
end;

procedure cCHXScriptEngine.SetPasScript(AValue: TPSScript);
begin
  if FPasScript = AValue then
    Exit;
  FPasScript := AValue;
end;

procedure cCHXScriptEngine.SetScriptError(AValue: TStrings);
begin
  if OwnsScriptError then
    FreeAndNil(FScriptError);

  if AValue = nil then
  begin
    FScriptError := TStringList.Create;
    OwnsScriptError := True;
  end
  else
  begin
    FScriptError := AValue;
    OwnsScriptError := False;
  end;
end;

procedure cCHXScriptEngine.setScriptFile(AValue: string);
begin
  PasScript.MainFileName := SetAsFile(AValue);
end;

procedure cCHXScriptEngine.SetScriptInfo(AValue: TStrings);
begin
  if OwnsScriptInfo then
    FreeAndNil(FScriptInfo);

  if AValue = nil then
  begin
    FScriptInfo := TStringList.Create;
    OwnsScriptInfo := True;
  end
  else
  begin
    FScriptInfo := AValue;
    OwnsScriptInfo := False;
  end;
end;

procedure cCHXScriptEngine.SetScriptOutput(AValue: TStrings);
begin
  if OwnsScriptOutput then
    FreeAndNil(FScriptOutput);

  if AValue = nil then
  begin
    FScriptOutput := TStringList.Create;
    OwnsScriptOutput := True;
  end
  else
  begin
    FScriptOutput := AValue;
    OwnsScriptOutput := False;
  end;
end;

procedure cCHXScriptEngine.setScriptText(AValue: TStrings);
begin
  PasScript.Script := AValue;
end;

procedure cCHXScriptEngine.PasScriptOnCompImport(Sender: TObject;
  x: TPSPascalCompiler);
begin
  SIRegister_StrUtils(x);
  RegisterDateTimeLibrary_C(x);
  RegisterDll_Compiletime(x);
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_StdCtrls(x);
  SIRegister_ExtCtrls(x);
  SIRegister_Forms(x);
  SIRegister_Buttons(x);
  SIRegister_Menus(x);
  SIRegister_DB(x);

  // CHX
  SIRegister_u7zWrapper(x);
end;

procedure cCHXScriptEngine.PasScriptOnCompile(Sender: TPSScript);
begin
  // Input and Output
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXWriteLn,
    'procedure WriteLn(const s: String)');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXReadLn,
    'function ReadLn(const aQuestion, DefAnswer: String): String;');

  // String handling
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXRPos,
    'function RPos(const Substr: String; const Source: String) : Integer;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXCompareText,
    'function CompareText(const S1, S2: String): Integer;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXCompareStr,
    'function CompareStr(const S1, S2: String): Integer;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXLowerCase,
    'function LowerCase(const AInStr: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXUpperCase,
    'function UpperCase(const AInStr: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXUTF8ToSys,
    'function UTF8ToSys(const S: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXSysToUTF8,
    'function SysToUTF8(const S: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXBoolToStr,
    'function BoolToStr(const aBool: Boolean): String;');

  // Path and filename strings
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXCleanFileName,
    'function CleanFileName(const AFileName: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXExcludeTrailingPathDelimiter,
    'function ExcludeTrailingPathDelimiter(const aString: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXExtractFilePath,
    'function ExtractFilePath(const aFileName: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXExtractFileName,
    'function ExtractFileName(const aFileName: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXExtractFileNameOnly,
    'function ExtractFileNameOnly(const AFilename: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXExtractFileExt,
    'function ExtractFileExt(const AFilename: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXChangeFileExt,
    'function ChangeFileExt(const aFileName, aExtension: String): String;');

  // Files and Folders UTF8
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXFileExistsUTF8,
    'function FileExistsUTF8(const aFileName: String): Boolean;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXDirectoryExistsUTF8,
    'function DirectoryExistsUTF8(const aFileName: String): Boolean;');

  // Dialogs
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskFile,
    'function AskFile(const aTitle, aExt, DefFile: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskFolder,
    'function AskFolder(const aTitle, DefFolder: String): String;');

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
  f: TFileStream;
begin
  Result := False;
  FullFileName := SetAsFolder(ExtractFilePath(OriginFileName)) + FileName;
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := SetAsFolder(CommonUnitFolder) + FileName;
    if not FileExistsUTF8(FullFileName) then
      Exit;
  end;

  try
    f := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

function cCHXScriptEngine.PasScriptOnNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
var
  FullFileName: string;
  F: TFileStream;
begin
  ShowMessage('PSScriptNeedFile:' + sLineBreak + 'OriginFileName: ' +
    OriginFileName + sLineBreak + 'FileName: ' + FileName +
    sLineBreak + 'Output: ' + Output + sLineBreak
    );


  Result := False;
  FullFileName := SetAsFolder(ExtractFilePath(OriginFileName)) + FileName;
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := SetAsFolder(CommonUnitFolder) + FileName;
    if not FileExistsUTF8(FullFileName) then
      Exit;
  end;

  try
    F := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

procedure cCHXScriptEngine.CHXWriteLn(const Str: string);
begin
  ScriptOutput.Add(Str);
end;

function cCHXScriptEngine.CHXReadLn(
  const aQuestion, DefAnswer: string): string;
begin
  Result := InputBox(Application.Title, aQuestion, DefAnswer);
end;

function cCHXScriptEngine.CHXRPos(const Substr, Source: string): integer;
begin
 Result := RPos(Substr, Source);
end;

function cCHXScriptEngine.CHXLowerCase(const AInStr: string): string;
begin
  Result := UTF8LowerCase(AInStr, '');
end;

function cCHXScriptEngine.CHXUpperCase(const AInStr: string): string;
begin
  Result := UTF8UpperCase(AInStr, '');
end;

function cCHXScriptEngine.CHXCompareText(const S1, S2: string): integer;
begin
  Result := UTF8CompareText(S1, S2);
end;

function cCHXScriptEngine.CHXCompareStr(const S1, S2: string): integer;
begin
  Result := UTF8CompareStr(S1, S2);
end;

function cCHXScriptEngine.CHXUTF8ToSys(const S: string): string;
begin
  Result := UTF8ToSys(S);
end;

function cCHXScriptEngine.CHXSysToUTF8(const S: string): string;
begin
  Result := SysToUTF8(S);
end;

function cCHXScriptEngine.CHXBoolToStr(const aBool: Boolean): string;
begin
  Result := BoolToStr(aBool, True);
end;

function cCHXScriptEngine.CHXCleanFileName(const AFileName: string): string;
begin
  Result := CleanFileName(AFileName);
end;

function cCHXScriptEngine.CHXExcludeTrailingPathDelimiter(
  const aString: string): string;
begin
   Result := ExcludeTrailingPathDelimiter(aString);
end;

function cCHXScriptEngine.CHXExtractFilePath(const aFileName: string): string;
begin
  Result := ExtractFilePath(aFileName);
end;

function cCHXScriptEngine.CHXExtractFileName(const aFileName: string): string;
begin
  Result := ExtractFileName(aFileName);
end;

function cCHXScriptEngine.CHXExtractFileNameOnly(
  const AFilename: string): string;
begin
  Result := ExtractFileNameOnly(aFileName);
end;

function cCHXScriptEngine.CHXExtractFileExt(const AFilename: string): string;
begin
  Result := ExtractFileExt(aFileName);
end;

function cCHXScriptEngine.CHXChangeFileExt(
  const aFileName, aExtension: string): string;
begin
   Result := ChangeFileExt(aFileName, aExtension);
end;

function cCHXScriptEngine.CHXFileExistsUTF8(const aFileName: string): boolean;
begin
   Result := FileExistsUTF8(SysPath(aFileName));
end;

function cCHXScriptEngine.CHXDirectoryExistsUTF8(const aFileName: string
  ): boolean;
begin
   Result:=DirectoryExistsUTF8(SysPath(aFileName));
end;

function cCHXScriptEngine.CHXAskFile(
  const aTitle, aExt, DefFile: string): string;
begin
    Result := '';

  Application.CreateForm(TfrmSMAskFile, frmSMAskFile);
  try
    frmSMAskFile.Caption := aTitle;
    frmSMAskFile.eFileName.DialogTitle := aTitle;
    frmSMAskFile.eFileName.Filter := aExt;
    frmSMAskFile.eFileName.FileName := DefFile;
    if frmSMAskFile.ShowModal = mrOk then
      Result := frmSMAskFile.eFileName.FileName;
  finally
    FreeAndNil(frmSMAskFile);
  end;
end;

function cCHXScriptEngine.CHXAskFolder(
  const aTitle, DefFolder: string): string;
begin
  Result := '';

  Application.CreateForm(TfrmSMAskFolder, frmSMAskFolder);
  try
    frmSMAskFolder.Caption := aTitle;
    frmSMAskFolder.eDirectory.DialogTitle := aTitle;
    frmSMAskFolder.eDirectory.Directory := SysPath(DefFolder);
    if frmSMAskFolder.ShowModal = mrOk then
      Result := IncludeTrailingPathDelimiter(frmSMAskFolder.eDirectory.Directory);
  finally
    FreeAndNil(frmSMAskFolder);
  end;
end;

function cCHXScriptEngine.CHXCreateStringList: TStringList;
begin
   Result := TStringList.Create;
end;

procedure cCHXScriptEngine.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_StrUtils_Routines(se);
  RegisterDateTimeLibrary_R(se);
  RegisterDLLRuntime(se);
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_ExtCtrls(x);
  RIRegister_Forms(x);
  RIRegister_Buttons(x);
  RIRegister_Menus(x);
  RIRegister_DB(x);

  // CHX
  RIRegister_u7zWrapper_Routines(se);
  RIRegister_u7zWrapper(x);
end;

function cCHXScriptEngine.RunScript: boolean;
begin
  Result := False;
  if not CompileScript then
    Exit;

  Result := PasScript.Execute;
  if not Result then
  begin
    ScriptError.BeginUpdate;
    ScriptError.Add(Format(rsSEEExecutionMsg, [rsSEEError]));
    ScriptError.Add(PasScript.ExecErrorToString);
    ScriptError.EndUpdate;
    Exit;
  end;

  ScriptError.Add(Format(rsSEEExecutionMsg, [rsSEEOK]));
end;

function cCHXScriptEngine.CompileScript: boolean;
var
  i: integer;
begin
  ScriptError.Clear;

  Result := PaSScript.Compile;

  if not Result then
  begin
    ScriptError.BeginUpdate;
    ScriptError.Add(Format(rsSEECompilationMsg, [rsSEEError]));
    for i := 0 to PaSScript.CompilerMessageCount - 1 do
    begin
      ScriptError.Add(PasScript.CompilerErrorToStr(i));
    end;
    ScriptError.EndUpdate;
    Exit;
  end;
  ScriptError.Add(Format(rsSEECompilationMsg, [rsSEEOK]));
end;

constructor cCHXScriptEngine.Create;
begin
  inherited Create;

  // This assigments autocreates the owned TStringList
  ScriptOutput := nil;
  ScriptInfo := nil;
  ScriptError := nil;

  FPasScript := TPSScript.Create(nil);
  PasScript.OnCompImport := @PasScriptOnCompImport;
  PasScript.OnCompile := @PasScriptOnCompile;
  PasScript.OnExecImport := @PasScriptOnExecImport;
  PasScript.OnExecute := @PasScriptOnExecute;
  PasScript.OnNeedFile := @PasScriptOnNeedFile;
  PasScript.OnFindUnknownFile := @PasScriptOnFindUnknownFile;

end;

destructor cCHXScriptEngine.Destroy;
begin
  if OwnsScriptError then
    FreeAndNil(FScriptError);
  if OwnsScriptInfo then
    FreeAndNil(FScriptInfo);
  if OwnsScriptOutput then
    FreeAndNil(FScriptOutput);
  FreeAndNil(FPasScript);
  inherited Destroy;
end;

end.
