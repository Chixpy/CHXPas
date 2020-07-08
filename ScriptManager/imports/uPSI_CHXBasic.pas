unit uPSI_CHXBasic;
{< Basic types and functions for Pascal Script.

  Copyright (C) 2011-2020 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uPSRuntime, uPSCompiler;

procedure SIRegister_CHXBasic(CL: TPSPascalCompiler);
{< Compile-time registration functions. }

procedure RIRegister_CHXBasic_Routines(S: TPSExec);
{< Run-time registration functions. }

implementation

// Helper functions
// ----------------
// finah.inc
//   Parameters are PathStr type
function CHXChangeFileExt(const FileName, Extension: string): string;
begin
  Result := ChangeFileExt(FileName, Extension);
end;

function CHXConcatPaths(const Paths: array of string): string;
begin
  Result := ConcatPaths(Paths);
end;

function CHXExcludeLeadingPathDelimiter(const Path: string): string;
begin
  Result := ExcludeLeadingPathDelimiter(Path);
end;

function CHXExcludeTrailingBackslash(const Path: string): string;
begin
  Result := ExcludeTrailingBackslash(Path);
end;

function CHXExcludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(Path);
end;

function CHXExpandFileName(const FileName : string): string;
begin
  Result := ExpandFileName(FileName);
end;

function CHXExpandUNCFileName(const FileName : string): string;
begin
  Result := ExpandUNCFileName(FileName);
end;

// function CHXExpandFileNameCase (const FileName: string; out MatchFound: TFilenameCaseMatch): string;

function CHXExtractFileDir(const FileName : string): string;
begin
  Result := ExtractFileDir(FileName);
end;

function CHXExtractFileDrive(const FileName: string): string;
begin
  Result := ExtractFileDrive(FileName);
end;

function CHXExtractFileExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
end;

function CHXExtractFileName(const FileName: string): string;
begin
  Result := ExtractFileName(FileName);
end;

function CHXExtractFilePath(const FileName: string): string;
begin
  Result := ExtractFilePath(FileName);
end;

function CHXExtractRelativepath(const BaseName,DestNAme : string): string;
begin
  Result := ExtractRelativepath(BaseName, DestNAme);
end;

function CHXExtractShortPathName(const FileName : string) : string;
begin
  Result := ExtractShortPathName(FileName);
end;

// function GetDirs (Var DirName : PathStr; Var Dirs : Array of PathPChar) : Longint;'); // {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif}

function CHXIncludeLeadingPathDelimiter(const Path : string) : string;
begin
  Result := IncludeLeadingPathDelimiter(Path);
end;

function CHXIncludeTrailingBackslash(const Path : string) : string;
begin
  Result := IncludeTrailingBackslash(Path);
end;

function CHXIncludeTrailingPathDelimiter(const Path : string) : string;
begin
  Result := IncludeTrailingPathDelimiter(Path);
end;

function CHXIsPathDelimiter(const Path: string; Index: Integer): Boolean;
begin
  Result := IsPathDelimiter(Path, Index);
end;

function CHXSetDirSeparators(const FileName : string) : string;
begin
  Result := SetDirSeparators(FileName);
end;

procedure CHXDoDirSeparators(var FileName : string);
begin
  DoDirSeparators(FileName);
end;



procedure SIRegister_CHXBasic(CL: TPSPascalCompiler);
begin
  // CL.AddDelphiFunction('function XXX;');

  // Basic types
  // -----------
  {$ifdef CPU64}
  CL.AddTypeS('SizeInt', 'Int64');
  //CL.AddTypeS('SizeUInt', 'QWord');
  CL.AddTypeS('SizeUInt', 'Int64');
  {$endif CPU64}
  {$ifdef CPU32}
  CL.AddTypeS('SizeInt','Longint');
  CL.AddTypeS('SizeUInt','DWord');
  {$endif CPU32}
  {$ifdef CPU16}
  CL.AddTypeS('SizeInt','Integer');
  CL.AddTypeS('SizeUInt','Word');
  {$endif CPU32}

   // finah.inc
   // ---------
   // System Utilities For Free Pascal

   CL.AddDelphiFunction('function ChangeFileExt(const FileName, Extension: string): string;');
   CL.AddDelphiFunction('function ConcatPaths(const Paths: array of string): string;');
   CL.AddDelphiFunction('function ExcludeLeadingPathDelimiter(const Path: string): string;');
   CL.AddDelphiFunction('function ExcludeTrailingBackslash(const Path: string): string;');
   CL.AddDelphiFunction('function ExcludeTrailingPathDelimiter(const Path: string): string;');
   CL.AddDelphiFunction('function ExpandFileName (const FileName : string): string;');
   CL.AddDelphiFunction('function ExpandUNCFileName (const FileName : string): string;');
   // CL.AddDelphiFunction('function ExpandFileNameCase (const FileName: string; out MatchFound: TFilenameCaseMatch): string;
   CL.AddDelphiFunction('function ExtractFileDir(const FileName : string): string;');
   CL.AddDelphiFunction('function ExtractFileDrive(const FileName: string): string;');
   CL.AddDelphiFunction('function ExtractFileExt(const FileName: string): string;');
   CL.AddDelphiFunction('function ExtractFileName(const FileName: string): string;');
   CL.AddDelphiFunction('function ExtractFilePath(const FileName: string): string;');
   CL.AddDelphiFunction('function ExtractRelativepath (const BaseName,DestNAme : string): string;');
   CL.AddDelphiFunction('function ExtractShortPathName(const FileName : string) : string;');
   // CL.AddDelphiFunction('function GetDirs (Var DirName : PathStr; Var Dirs : Array of PathPChar) : Longint;'); // {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif}
   CL.AddDelphiFunction('function IncludeLeadingPathDelimiter(const Path : string) : string;');
   CL.AddDelphiFunction('function IncludeTrailingBackslash(const Path : string) : string;');
   CL.AddDelphiFunction('function IncludeTrailingPathDelimiter(const Path : string) : string;');
   CL.AddDelphiFunction('function IsPathDelimiter(const Path: string; Index: Integer): Boolean;');
   CL.AddDelphiFunction('function SetDirSeparators (const FileName : string) : string;');
   CL.AddDelphiFunction('procedure DoDirSeparators (var FileName : string);'); // {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif}

end;

procedure RIRegister_CHXBasic_Routines(S: TPSExec);
begin
  // S.RegisterDelphiFunction(@XXX, 'XXX', cdRegister);

  // finah.inc
  // ---------
  // System Utilities For Free Pascal

  S.RegisterDelphiFunction(@CHXChangeFileExt, 'ChangeFileExt', cdRegister);
  S.RegisterDelphiFunction(@CHXConcatPaths, 'ConcatPaths', cdRegister);
  S.RegisterDelphiFunction(@CHXExcludeLeadingPathDelimiter, 'ExcludeLeadingPathDelimiter', cdRegister);
  S.RegisterDelphiFunction(@CHXExcludeTrailingBackslash, 'ExcludeTrailingBackslash', cdRegister);
  S.RegisterDelphiFunction(@CHXExcludeTrailingPathDelimiter, 'ExcludeTrailingPathDelimiter', cdRegister);
  S.RegisterDelphiFunction(@CHXExpandFileName, 'ExpandFileName', cdRegister);
  S.RegisterDelphiFunction(@CHXExpandUNCFileName, 'ExpandUNCFileName', cdRegister);
  // S.RegisterDelphiFunction(@CHXExpandFileNameCase, 'ExpandFileNameCase', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractFileDir, 'ExtractFileDir', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractFileDrive, 'ExtractFileDrive', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractFileExt, 'ExtractFileExt', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractFileName, 'ExtractFileName', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractFilePath, 'ExtractFilePath', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractRelativepath, 'ExtractRelativepath', cdRegister);
  S.RegisterDelphiFunction(@CHXExtractShortPathName, 'ExtractShortPathName', cdRegister);
  // S.RegisterDelphiFunction(@CHXGetDirs, 'GetDirs', cdRegister);
  S.RegisterDelphiFunction(@CHXIncludeLeadingPathDelimiter, 'IncludeLeadingPathDelimiter', cdRegister);
  S.RegisterDelphiFunction(@CHXIncludeTrailingBackslash, 'IncludeTrailingBackslash', cdRegister);
  S.RegisterDelphiFunction(@CHXIncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  S.RegisterDelphiFunction(@CHXIsPathDelimiter, 'IsPathDelimiter', cdRegister);
  S.RegisterDelphiFunction(@CHXSetDirSeparators, 'SetDirSeparators', cdRegister);
  S.RegisterDelphiFunction(@CHXDoDirSeparators, 'DoDirSeparators', cdRegister);

end;

end.

