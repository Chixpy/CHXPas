unit uPSI_u7zWrapper;

interface

uses
  SysUtils, Classes, u7zWrapper, uPSComponent, uPSRuntime, uPSCompiler;

{ compile-time registration functions }
procedure SIRegister_u7zWrapper(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_u7zWrapper(CL: TPSRuntimeClassImporter);
procedure RIRegister_u7zWrapper_Routines(S: TPSExec);

implementation

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_u7zWrapper(CL: TPSPascalCompiler);
begin
  CL.AddConstantN('w7zExeError', 'String').SetString(w7zExeError);
  CL.AddConstantN('kw7zGCIniPrefix', 'String').SetString(kw7zGCIniPrefix);
  CL.AddConstantN('kw7zGCIniFiles', 'String').SetString(kw7zGCIniFiles);
  CL.AddConstantN('kw7zGCIniSizes', 'String').SetString(kw7zGCIniSizes);
  CL.AddConstantN('kw7zGCIniPSizes', 'String').SetString(kw7zGCIniPSizes);
  CL.AddConstantN('kw7zGCIniDates', 'String').SetString(kw7zGCIniDates);
  CL.AddConstantN('kw7zGCIniCRCs', 'String').SetString(kw7zGCIniCRCs);
  CL.AddConstantN('kw7zCacheFileExt', 'String').SetString(kw7zCacheFileExt);
  CL.AddConstantN('kw7zFileExts', 'String').SetString(kw7zFileExts);
  CL.AddClassN(CL.FindClass('TOBJECT'), 'w7zException');
  CL.AddDelphiFunction('Function w7zGetFileExts : string');
  CL.AddDelphiFunction('Procedure w7zSetFileExts( aExtList : string)');
  CL.AddDelphiFunction('Function w7zGetPathTo7zexe : string');
  CL.AddDelphiFunction('Procedure w7zSetPathTo7zexe( aPath : string)');
  CL.AddDelphiFunction('Function w7zGetPathTo7zGexe : string');
  CL.AddDelphiFunction('Procedure w7zSetPathTo7zGexe( aPath : string)');
  CL.AddDelphiFunction('Function w7zGetCacheDir : string');
  CL.AddDelphiFunction('Procedure w7zSetCacheDir( aPath : string)');
  CL.AddDelphiFunction('Function w7zGetGlobalCache : string');
  CL.AddDelphiFunction('Procedure w7zSetGlobalCache( aPath : string)');
  CL.AddDelphiFunction(
    'Procedure w7zListFiles( const aFilename : string; PackedFiles : TStrings; const OnlyPaths : boolean; const UseCache : boolean; const Password : string)');
  CL.AddDelphiFunction(
    'Function w7zExtractFile( a7zArchive : string; const aFileMask : string; aFolder : string; const ShowProgress : boolean; const Password : string) : integer');
  CL.AddDelphiFunction(
    'Function w7zCompressFile( const a7zArchive : string; aFileList : TStrings; const ShowProgress : boolean; const CompType : string) : integer');
  CL.AddDelphiFunction(
    'Function w7zCRC32InnerFile( a7zArchive : string; const aInnerFile : string; const Password : string) : cardinal');
  CL.AddDelphiFunction(
    'Function w7zCRC32InnerFileStr( a7zArchive : string; const aInnerFile : string; const Password : string) : string');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_u7zWrapper_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@w7zGetFileExts, 'w7zGetFileExts', cdRegister);
  S.RegisterDelphiFunction(@w7zSetFileExts, 'w7zSetFileExts', cdRegister);
  S.RegisterDelphiFunction(@w7zGetPathTo7zexe, 'w7zGetPathTo7zexe', cdRegister);
  S.RegisterDelphiFunction(@w7zSetPathTo7zexe, 'w7zSetPathTo7zexe', cdRegister);
  S.RegisterDelphiFunction(@w7zGetPathTo7zGexe, 'w7zGetPathTo7zGexe',
    cdRegister);
  S.RegisterDelphiFunction(@w7zSetPathTo7zGexe, 'w7zSetPathTo7zGexe',
    cdRegister);
  S.RegisterDelphiFunction(@w7zGetCacheDir, 'w7zGetCacheDir', cdRegister);
  S.RegisterDelphiFunction(@w7zSetCacheDir, 'w7zSetCacheDir', cdRegister);
  S.RegisterDelphiFunction(@w7zGetGlobalCache, 'w7zGetGlobalCache', cdRegister);
  S.RegisterDelphiFunction(@w7zSetGlobalCache, 'w7zSetGlobalCache', cdRegister);
  S.RegisterDelphiFunction(@w7zListFiles, 'w7zListFiles', cdRegister);
  S.RegisterDelphiFunction(@w7zExtractFile, 'w7zExtractFile', cdRegister);
  S.RegisterDelphiFunction(@w7zCompressFile, 'w7zCompressFile', cdRegister);
  S.RegisterDelphiFunction(@w7zCRC32InnerFile, 'w7zCRC32InnerFile', cdRegister);
  S.RegisterDelphiFunction(@w7zCRC32InnerFileStr, 'w7zCRC32InnerFileStr',
    cdRegister);
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_u7zWrapper(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(w7zException) do
  ;
end;

end.
