{ u7zWrapper for Pascal Script.

  Copyright (C) 2011-2018 Chixpy

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
unit uPSI_u7zWrapper;

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  u7zWrapper;

{ compile-time registration functions }
procedure SIRegister_u7zWrapper(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_u7zWrapper_Routines(S: TPSExec);

implementation

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_u7zWrapper(CL: TPSPascalCompiler);
begin
  // Resource Strings
  CL.AddConstantN('w7zFileNotFound', 'String').SetString(rsw7zFileNotFound);
  CL.AddConstantN('rsw7zExeError', 'String').SetString(rsw7zExeError);
  CL.AddConstantN('w7zNoCRC32', 'String').SetString(rsw7zNoCRC32);

  // Constants
  CL.AddConstantN('kw7zGCIniPrefix', 'String').SetString(kw7zGCIniPrefix);
  CL.AddConstantN('kw7zGCIniFiles', 'String').SetString(kw7zGCIniFiles);
  CL.AddConstantN('kw7zGCIniSizes', 'String').SetString(kw7zGCIniSizes);
  CL.AddConstantN('kw7zGCIniPSizes', 'String').SetString(kw7zGCIniPSizes);
  CL.AddConstantN('kw7zGCIniDates', 'String').SetString(kw7zGCIniDates);
  CL.AddConstantN('kw7zGCIniCRCs', 'String').SetString(kw7zGCIniCRCs);
  CL.AddConstantN('kw7zGCIniSHA1s', 'String').SetString(kw7zGCIniSHA1s);

  CL.AddConstantN('kw7zCacheFileExt', 'String').SetString(kw7zCacheFileExt);
  CL.AddConstantN('kw7zFileExts', 'String').SetString(kw7zFileExts);

  // Types
  CL.AddClassN(CL.FindClass('TOBJECT'), 'w7zException');

  // Methods
  CL.AddDelphiFunction('function w7zGetFileExts: string');
  CL.AddDelphiFunction('procedure w7zSetFileExts(aExtList: string)');
  CL.AddDelphiFunction('function w7zGetPathTo7zexe: string');
  CL.AddDelphiFunction('procedure w7zSetPathTo7zexe(aPath: string)');
  CL.AddDelphiFunction('function w7zGetPathTo7zGexe: string');
  CL.AddDelphiFunction('procedure w7zSetPathTo7zGexe(aPath: string)');
  CL.AddDelphiFunction('function w7zGetCacheDir: string');
  CL.AddDelphiFunction('procedure w7zSetCacheDir(aPath: string)');
  CL.AddDelphiFunction('function w7zGetGlobalCache: string');
  CL.AddDelphiFunction('procedure w7zSetGlobalCache(aPath: string)');
  CL.AddDelphiFunction('function w7zFileExists(a7zArchive: string; const aInnerFile: string; const Password: string): integer');
  CL.AddDelphiFunction('procedure w7zListFiles(a7zArchive: string; PackedFiles: TStrings; const OnlyPaths: boolean; const Password: string)');
  CL.AddDelphiFunction('procedure w7zFilesByExt(AOutFolderList, AOutFileList: TStrings; aBaseFolder: string; aExtList: TStrings; Recursive: boolean)');
  CL.AddDelphiFunction('function w7zExtractFile(a7zArchive: string; const aFileMask: string; aFolder: string; const ShowProgress: boolean;const Password: string): integer');
  CL.AddDelphiFunction('function w7zCompressFile(a7zArchive: string; aFileList: TStrings; const ShowProgress: boolean; const CompType: string): integer');
  CL.AddDelphiFunction('function w7zCRC32InnerFile(a7zArchive: string; const aInnerFile: string; const Password: string): cardinal');
  CL.AddDelphiFunction('function w7zCRC32InnerFileStr(a7zArchive: string; const aInnerFile: string; const Password: string): string');
//  CL.AddDelphiFunction('function w7zSHA32InnerFile(a7zArchive: string; const aInnerFile: string; const Password: string): TSHA1Digest');
  CL.AddDelphiFunction('function w7zSHA32InnerFileStr(a7zArchive: string; const aInnerFile: string; const Password: string): string');
end;

procedure RIRegister_u7zWrapper_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@w7zGetFileExts, 'w7zGetFileExts', cdRegister);
  S.RegisterDelphiFunction(@w7zSetFileExts, 'w7zSetFileExts', cdRegister);
  S.RegisterDelphiFunction(@w7zGetPathTo7zexe, 'w7zGetPathTo7zexe', cdRegister);
  S.RegisterDelphiFunction(@w7zSetPathTo7zexe, 'w7zSetPathTo7zexe', cdRegister);
  S.RegisterDelphiFunction(@w7zGetPathTo7zGexe, 'w7zGetPathTo7zGexe', cdRegister);
  S.RegisterDelphiFunction(@w7zSetPathTo7zGexe, 'w7zSetPathTo7zGexe', cdRegister);
  S.RegisterDelphiFunction(@w7zGetCacheDir, 'w7zGetCacheDir', cdRegister);
  S.RegisterDelphiFunction(@w7zSetCacheDir, 'w7zSetCacheDir', cdRegister);
  S.RegisterDelphiFunction(@w7zGetGlobalCache, 'w7zGetGlobalCache', cdRegister);
  S.RegisterDelphiFunction(@w7zSetGlobalCache, 'w7zSetGlobalCache', cdRegister);
  S.RegisterDelphiFunction(@w7zFileExists, 'w7zFileExists', cdRegister);
  S.RegisterDelphiFunction(@w7zListFiles, 'w7zListFiles', cdRegister);
  S.RegisterDelphiFunction(@w7zFilesByExt, 'w7zFilesByExt', cdRegister);
  S.RegisterDelphiFunction(@w7zExtractFile, 'w7zExtractFile', cdRegister);
  S.RegisterDelphiFunction(@w7zCompressFile, 'w7zCompressFile', cdRegister);
  S.RegisterDelphiFunction(@w7zCRC32InnerFile, 'w7zCRC32InnerFile', cdRegister);
  S.RegisterDelphiFunction(@w7zCRC32InnerFileStr, 'w7zCRC32InnerFileStr', cdRegister);
//  S.RegisterDelphiFunction(@w7zSHA32InnerFile, 'w7zSHA32InnerFile', cdRegister);
  S.RegisterDelphiFunction(@w7zSHA32InnerFileStr, 'w7zSHA32InnerFileStr', cdRegister);
end;

end.
