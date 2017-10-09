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
  uCHXStrUtils, u7zWrapper, uCHXDlgUtils,
  // Imported units
  uPSI_u7zWrapper, uPSI_uCHXStrUtils, uPSI_uCHXFileUtils;

resourcestring
  rsSEECompilationMsg = 'Compiling: %s.';
  rsSEEExecutionMsg = 'Executing: %s.';
  rsSEEError = 'Error!';
  rsSEEOK = 'Success!';


type
  TCHXSEWriteLnCB = procedure(const aStr: string) of object;
  TCHXSEReadLnCB = function(const aQuestion, DefAnswer: string): string of
    object;
  TCHXSEAskFileCB = function(const aCaption, aExtFilter, DefFile: string):
    string of object;
  TCHXSEAskMultiFileCB = procedure(aFileList: TStrings;
    const aCaption, aExtFilter, DefFolder: string) of object;
  TCHXSEAskFolderCB = function(const aCaption, DefFolder: string): string of
    object;

  { cCHXScriptEngine }

  cCHXScriptEngine = class(TObject)

  private
    FCommonUnitFolder: string;
    FOnAskFile: TCHXSEAskFileCB;
    FOnAskFolder: TCHXSEAskFolderCB;
    FOnAskMultiFile: TCHXSEAskMultiFileCB;
    FOnReadLn: TCHXSEReadLnCB;
    FOnWriteLn: TCHXSEWriteLnCB;
    FPasScript: TPSScript;
    FScriptError: TStrings;
    function getScriptFile: string;
    function getScriptText: TStrings;
    procedure SetCommonUnitFolder(AValue: string);
    procedure SetOnAskFile(AValue: TCHXSEAskFileCB);
    procedure SetOnAskFolder(AValue: TCHXSEAskFolderCB);
    procedure SetOnAskMultiFile(AValue: TCHXSEAskMultiFileCB);
    procedure SetOnReadLn(AValue: TCHXSEReadLnCB);
    procedure SetOnWriteLn(AValue: TCHXSEWriteLnCB);
    procedure SetPasScript(AValue: TPSScript);
    procedure SetScriptError(AValue: TStrings);
    procedure setScriptFile(AValue: string);
    procedure setScriptText(AValue: TStrings);

  protected
    property PasScript: TPSScript read FPasScript write SetPasScript;
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
    // TODO: Make them external.

    // Strings
    function CHXRPos(const Substr, Source: string): integer;

    function CHXLowerCase(const AInStr: string): string;
    function CHXUpperCase(const AInStr: string): string;

    function CHXCompareText(const S1, S2: string): integer;
    function CHXCompareStr(const S1, S2: string): integer;

    function CHXUTF8ToSys(const S: string): string;
    function CHXSysToUTF8(const S: string): string;

    function CHXBoolToStr(const aBool: boolean): string;

    // Path and filename strings
    function CHXExcludeTrailingPathDelimiter(const aString: string): string;
    function CHXExtractFilePath(const aFileName: string): string;
    function CHXExtractFileName(const aFileName: string): string;
    function CHXExtractFileNameOnly(const AFilename: string): string;
    function CHXExtractFileExt(const AFilename: string): string;
    function CHXChangeFileExt(const aFileName, aExtension: string): string;

    // Files and Folders UTF8
    function CHXFileExistsUTF8(const aFileName: string): boolean;
    function CHXDirectoryExistsUTF8(const aFileName: string): boolean;

    // CallBacks
    procedure CHXWriteLn(const aStr: string);
    function CHXReadLn(const aQuestion, DefAnswer: string): string;
    function CHXAskFile(const aCaption, aExtFilter, DefFile: string): string;
    procedure CHXAskMultiFile(aFileList: TStrings;
      const aCaption, aExtFilter, DefFolder: string);
    function CHXAskFolder(const aCaption, DefFolder: string): string;

    // HACK: We can't create Stringlist!!!
    // TODO: Make a generic constructor? TApl
    function CHXCreateStringList: TStringList;

  public
    property ScriptFile: string read getScriptFile write setScriptFile;
    property CommonUnitFolder: string
      read FCommonUnitFolder write SetCommonUnitFolder;

    property ScriptText: TStrings read getScriptText write setScriptText;

    property ScriptError: TStrings read FScriptError write SetScriptError;

    property OnWriteLn: TCHXSEWriteLnCB read FOnWriteLn write SetOnWriteLn;
    property OnReadLn: TCHXSEReadLnCB read FOnReadLn write SetOnReadLn;
    property OnAskFile: TCHXSEAskFileCB read FOnAskFile write SetOnAskFile;
    property OnAskMultiFile: TCHXSEAskMultiFileCB
      read FOnAskMultiFile write SetOnAskMultiFile;
    property OnAskFolder: TCHXSEAskFolderCB
      read FOnAskFolder write SetOnAskFolder;

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

procedure cCHXScriptEngine.SetPasScript(AValue: TPSScript);
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
  SIRegister_uCHXStrUtils(x);
  SIRegister_uCHXFileUtils(x);
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
  Sender.AddMethod(Self, @cCHXScriptEngine.CHXAskMultiFile,
    'procedure AskMultiFile(aFileList: TStrings; const aTitle: string;' +
    ' const aExtFilter: string; const DefFolder: string)');
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
  raise ENotImplemented.Create('PasScriptOnFindUnknownFile not implemented');

  if Assigned(ScriptError) then
  begin
    ScriptError.Add('PasScriptOnFindUnknownFile - OriginFileName: ' +
      OriginFileName);
    ScriptError.Add('PasScriptOnFindUnknownFile - FileName: ' + FileName);
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
  FileName := AnsiDequotedStr(FileName, '''');
  FileName := AnsiDequotedStr(FileName, '"');

  if Assigned(ScriptError) then
  begin
    ScriptError.Add('PSScriptNeedFile - OriginFileName: ' + OriginFileName);
    ScriptError.Add('PSScriptNeedFile - FileName: ' + FileName);
  end;

  Result := False;
  FullFileName := CleanAndExpandFilename(
    SetAsFolder(ExtractFilePath(OriginFileName)) + FileName);
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := CleanAndExpandFilename(SetAsFolder(CommonUnitFolder) +
      FileName);
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

function cCHXScriptEngine.CHXBoolToStr(const aBool: boolean): string;
begin
  Result := BoolToStr(aBool, True);
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

function cCHXScriptEngine.CHXDirectoryExistsUTF8(
  const aFileName: string): boolean;
begin
  Result := DirectoryExistsUTF8(SysPath(aFileName));
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
  RIRegister_uCHXStrUtils_Routines(se);
  RIRegister_uCHXFileUtils_Routines(se);
end;

function cCHXScriptEngine.RunScript: boolean;
begin
  Result := False;
  if not CompileScript then
    Exit;

  Result := PasScript.Execute;

  if Result then
  begin
    if Assigned(ScriptError) then
      ScriptError.Add(Format(rsSEEExecutionMsg, [rsSEEOK]));
    Exit;
  end;

  if not Assigned(ScriptError) then
    Exit;

  ScriptError.BeginUpdate;
  ScriptError.Add(Format(rsSEEExecutionMsg, [rsSEEError]));
  ScriptError.Add(PasScript.ExecErrorToString);
  ScriptError.Add(Format('[Runtime error] %s(%d:%d)',
    [ScriptFile, PasScript.ExecErrorRow, PasScript.ExecErrorCol]));
  ScriptError.Add(Format('bytecode(%d:%d): %s',
    [PasScript.ExecErrorProcNo, PasScript.ExecErrorByteCodePosition,
    PasScript.ExecErrorToString]));
  ScriptError.EndUpdate;
end;

function cCHXScriptEngine.CompileScript: boolean;
var
  i: integer;
begin
  if Assigned(ScriptError) then
    ScriptError.Clear;

  Result := PaSScript.Compile;

  if Result then
  begin
    if Assigned(ScriptError) then
      ScriptError.Add(Format(rsSEECompilationMsg, [rsSEEOK]));
    Exit;
  end;

  if not Assigned(ScriptError) then
    Exit;

  ScriptError.BeginUpdate;

  for i := 0 to PaSScript.CompilerMessageCount - 1 do
    ScriptError.add(PaSScript.CompilerMessages[i].MessageToString);

  ScriptError.Add(Format(rsSEECompilationMsg, [rsSEEError]));

  for i := 0 to PaSScript.CompilerMessageCount - 1 do
    ScriptError.Add(PasScript.CompilerErrorToStr(i));
  ScriptError.EndUpdate;
end;

constructor cCHXScriptEngine.Create;
begin
  inherited Create;

  FPasScript := TPSScript.Create(nil);
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
