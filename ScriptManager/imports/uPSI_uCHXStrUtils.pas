unit uPSI_uCHXStrUtils;

{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis.
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface

uses
  SysUtils, Classes, uPSRuntime, uPSCompiler,
  LazFileUtils, LazUTF8,
  uCHXStrUtils;

{ compile-time registration functions }
procedure SIRegister_uCHXStrUtils(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_uCHXStrUtils_Routines(S: TPSExec);

implementation

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_uCHXStrUtils(CL: TPSPascalCompiler);
begin
  // Resource Strings

  // Constants

  // Types

  // Methods
  CL.AddDelphiFunction('function UTF8TextReplace(const S, OldPattern, NewPattern: string; ALanguage: string): string');
  CL.AddDelphiFunction('function RemoveFromBrackets(const aString: string): string');
  CL.AddDelphiFunction('function CopyFromBrackets(const aString: string): string');
  CL.AddDelphiFunction('function TextSimilarity(const aString1, aString2: string): byte');

  CL.AddDelphiFunction('function SetAsFolder(const aValue: string): string');
  CL.AddDelphiFunction('function SysPath(const aPath: string): string');
  CL.AddDelphiFunction('function WinPath(const aPath: string): string');
  CL.AddDelphiFunction('function UnixPath(const aPath: string): string');

  CL.AddDelphiFunction('function CleanFileName(const AFileName: string; const DoTrim: boolean; const PathAware: boolean): string');
  CL.AddDelphiFunction('function SetAsRelativeFile(const aFileName: string; BaseDir: string): string');
  CL.AddDelphiFunction('function SetAsAbsoluteFile(const aFileName: string; BaseDir: string): string');
  CL.AddDelphiFunction('function SetAsFile(const aFileName: string): string');
  CL.AddDelphiFunction('function SupportedExtCT(aFilename: string; aExtCT: string): boolean');
  CL.AddDelphiFunction('function SupportedExtSL(aFilename: string; aExt: TStrings): boolean');

  CL.AddDelphiFunction('procedure CleanStringList(aStringList: TStrings; CommentChar: string)');
  CL.AddDelphiFunction('function AddToStringList(aList: TStrings; aString: string): integer');
  CL.AddDelphiFunction('function FileMaskFromStringList(aList: TStrings): string');
  CL.AddDelphiFunction('function FileMaskFromCommaText(aText: string): string');

  CL.AddDelphiFunction('procedure StandardFormatSettings');
  CL.AddDelphiFunction('function StrCount(aString, ToSearch: string; CaseSensitve: boolean): cardinal');
  CL.AddDelphiFunction('function StrToCardinal(const aString: string): cardinal');
  CL.AddDelphiFunction('function StrToCardinalDef(const aString: string; const Default: cardinal): cardinal');
  CL.AddDelphiFunction('function SecondsToFmtStr(aValue: int64): string');
end;

procedure RIRegister_uCHXStrUtils_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@UTF8TextReplace, 'UTF8TextReplace', cdRegister);
  S.RegisterDelphiFunction(@RemoveFromBrackets, 'RemoveFromBrackets', cdRegister);
  S.RegisterDelphiFunction(@CopyFromBrackets, 'CopyFromBrackets', cdRegister);
  S.RegisterDelphiFunction(@TextSimilarity, 'TextSimilarity', cdRegister);

  S.RegisterDelphiFunction(@SetAsFolder, 'SetAsFolder', cdRegister);
  S.RegisterDelphiFunction(@SysPath, 'SysPath', cdRegister);
  S.RegisterDelphiFunction(@WinPath, 'WinPath', cdRegister);
  S.RegisterDelphiFunction(@UnixPath, 'UnixPath', cdRegister);

  S.RegisterDelphiFunction(@CleanFileName, 'CleanFileName', cdRegister);
  S.RegisterDelphiFunction(@SetAsRelativeFile, 'SetAsRelativeFile', cdRegister);
  S.RegisterDelphiFunction(@SetAsAbsoluteFile, 'SetAsAbsoluteFile', cdRegister);
  S.RegisterDelphiFunction(@SetAsFile, 'SetAsFile', cdRegister);
  S.RegisterDelphiFunction(@SupportedExtCT, 'SupportedExtCT', cdRegister);
  S.RegisterDelphiFunction(@SupportedExtSL, 'SupportedExtSL', cdRegister);

  S.RegisterDelphiFunction(@CleanStringList, 'CleanStringList', cdRegister);
  S.RegisterDelphiFunction(@AddToStringList, 'AddToStringList', cdRegister);
  S.RegisterDelphiFunction(@FileMaskFromStringList, 'FileMaskFromStringList', cdRegister);
  S.RegisterDelphiFunction(@FileMaskFromCommaText, 'FileMaskFromCommaText', cdRegister);

  S.RegisterDelphiFunction(@StandardFormatSettings, 'StandardFormatSettings', cdRegister);
  S.RegisterDelphiFunction(@StrCount, 'StrCount', cdRegister);
  S.RegisterDelphiFunction(@StrToCardinal, 'StrToCardinal', cdRegister);
  S.RegisterDelphiFunction(@StrToCardinalDef, 'StrToCardinalDef', cdRegister);
  S.RegisterDelphiFunction(@SecondsToFmtStr, 'SecondsToFmtStr', cdRegister);
end;

end.
