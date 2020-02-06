unit uPSI_uCHXFileUtils;
{< uCHXFileUtils for Pascal Script.

  Copyright (C) 2018-2020 Chixpy

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
interface

uses
  SysUtils, Classes, uPSRuntime, uPSCompiler,
  FileUtil, LazFileUtils, LazUTF8,
  uCHX7zWrapper, uCHXFileUtils;

{ compile-time registration functions }
procedure SIRegister_uCHXFileUtils(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_uCHXFileUtils_Routines(S: TPSExec);


implementation

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_uCHXFileUtils(CL: TPSPascalCompiler);
begin
  // Resource Strings

  // Constants
  //CL.AddConstantN('kCHXSHA1Empty', 'TSHA1Digest').SetString(kCHXSHA1Empty);

  // Types
  //CL.AddTypeS('TItFolderObj', 'function(aFolder: string; FileInfo: TSearchRec): boolean of object');
  //CL.AddTypeS('TItFolderFun', 'function(aFolder: string; FileInfo: TSearchRec): boolean');

  // Methods
  CL.AddDelphiFunction('function CRC32FileInt(const aFileName: string): cardinal');
  CL.AddDelphiFunction('function CRC32FileStr(const aFileName: string): string');
  CL.AddDelphiFunction('function SHA1FileStr(const aFileName: string): string');
  //CL.AddDelphiFunction('function StringToSHA1Digest(aSHA1String: string):TSHA1Digest');

  //CL.AddDelphiFunction('function IterateFolderObj(aFolder: string; aFunction: TItFolderObj; Recursive: boolean = True): boolean');
  //CL.AddDelphiFunction('function IterateFolderFun(aFolder: string; aFunction: TItFolderFun; Recursive: boolean = True): boolean');

  CL.AddDelphiFunction('function FilesInFolder(aFolder, aFileMask: string): integer');
end;

procedure RIRegister_uCHXFileUtils_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@CRC32FileInt, 'CRC32FileInt', cdRegister);
  S.RegisterDelphiFunction(@CRC32FileStr, 'CRC32FileStr', cdRegister);
  S.RegisterDelphiFunction(@SHA1FileStr, 'SHA1FileStr',  cdRegister);
  //S.RegisterDelphiFunction(@StringToSHA1Digest, 'StringToSHA1Digest', cdRegister);

  //S.RegisterDelphiFunction(@IterateFolderObj, 'IterateFolderObj', cdRegister);
  //S.RegisterDelphiFunction(@IterateFolderFun, 'IterateFolderFun', cdRegister);

  S.RegisterDelphiFunction(@FilesInFolder, 'FilesInFolder', cdRegister);
end;

end.
