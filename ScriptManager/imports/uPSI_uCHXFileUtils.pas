unit uPSI_uCHXFileUtils;

interface

uses
  SysUtils, Classes, uPSRuntime, uPSCompiler,
  FileUtil, LazFileUtils, LazUTF8
  , u7zWrapper
  , uCHXFileUtils;

{ compile-time registration functions }
procedure SIRegister_uCHXFileUtils(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_uCHXFileUtils_Routines(S: TPSExec);


implementation

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_uCHXFileUtils(CL: TPSPascalCompiler);
begin
//  CL.AddTypeS('TItFolderObj',
//    'Function ( aFolder : string; FileInfo : TSearchRec) : boolean');
  CL.AddDelphiFunction(
    'Function SearchFirstFileInFolderByExtCT( aFolder : string; Extensions : string) : string');
  CL.AddDelphiFunction(
    'Function SearchFirstFileInFolderByExtSL( aFolder : string; Extensions : TStrings) : string');
  CL.AddDelphiFunction(
    'Procedure w7zFilesByExt( AOutFolderList, AOutFileList : TStrings; aBaseFolder : string; aExtList : TStrings; Recursive : boolean)');
  CL.AddDelphiFunction(
    'Function CRC32FileInt( const aFileName : string) : cardinal');
  CL.AddDelphiFunction(
    'Function CRC32FileStr( const aFileName : string) : string');
  CL.AddDelphiFunction('Function SHA1FileStr( const aFileName : string) : string');
//  CL.AddDelphiFunction(
//    'Function StringToSHA1Digest( aSHA1String : string) : TSHA1Digest');
//  CL.AddDelphiFunction(
//    'Function IterateFolderObj( Folder : string; aFunction : TItFolderObj; Recursive : boolean) : boolean');
//  CL.AddDelphiFunction(
//    'Function IterateFolderFun( Folder : string; aFunction : TItFolderFun; Recursive : boolean) : boolean');
  CL.AddDelphiFunction('Function FilesInFolder( Folder : string) : integer');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_uCHXFileUtils_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@SearchFirstFileInFolderByExtCT,
    'SearchFirstFileInFolderByExtCT', cdRegister);
  S.RegisterDelphiFunction(@SearchFirstFileInFolderByExtSL,
    'SearchFirstFileInFolderByExtSL', cdRegister);
  S.RegisterDelphiFunction(@w7zFilesByExt, 'w7zFilesByExt',
    cdRegister);
  S.RegisterDelphiFunction(@CRC32FileInt, 'CRC32FileInt', cdRegister);
  S.RegisterDelphiFunction(@CRC32FileStr, 'CRC32FileStr', cdRegister);
  S.RegisterDelphiFunction(@SHA1FileStr, 'SHA1FileStr', cdRegister);
//  S.RegisterDelphiFunction(@StringToSHA1Digest, 'StringToSHA1Digest',
//    cdRegister);
//  S.RegisterDelphiFunction(@IterateFolderObj, 'IterateFolderObj', cdRegister);
//  S.RegisterDelphiFunction(@IterateFolderFun, 'IterateFolderFun', cdRegister);
  S.RegisterDelphiFunction(@FilesInFolder, 'FilesInFolder', cdRegister);
end;

end.
