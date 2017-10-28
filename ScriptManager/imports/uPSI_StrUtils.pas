unit uPSI_StrUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, uPSRuntime, uPSCompiler;


// TODO: Import more from StrUtils

{ compile-time registration functions }
procedure SIRegister_StrUtils(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_StrUtils_Routines(S: TPSExec);

implementation

procedure SIRegister_StrUtils(CL: TPSPascalCompiler);
begin

  // Case insensitive search/replace

  CL.AddDelphiFunction(
    'function AnsiResemblesText(const AText, AOther: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiContainsText(const AText, ASubText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiStartsText(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiEndsText(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiReplaceText(const AText, AFromText, AToText: string): string');
  CL.AddDelphiFunction(
    'function AnsiMatchText(const AText: string; const AValues: array of string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiIndexText(const AText: string; const AValues: array of string): Integer');

  // Case sensitive search/replace

  CL.AddDelphiFunction(
    'function AnsiContainsStr(const AText, ASubText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiStartsStr(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiEndsStr(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiReplaceStr(const AText, AFromText, AToText: string): string');
  CL.AddDelphiFunction(
    'function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiIndexStr(const AText: string; const AValues: array of string): Integer');
  //CL.AddDelphiFunction(
  //  'function MatchStr(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean');
  //CL.AddDelphiFunction(
  //  'function IndexStr(const AText: UnicodeString; const AValues: array of UnicodeString): Integer');

end;

procedure RIRegister_StrUtils_Routines(S: TPSExec);
begin
  // Case insensitive search/replace

  S.RegisterDelphiFunction(@AnsiResemblesText, 'AnsiResemblesText', cdRegister);
  S.RegisterDelphiFunction(@AnsiContainsText, 'AnsiContainsText', cdRegister);
  S.RegisterDelphiFunction(@AnsiStartsText, 'AnsiStartsText', cdRegister);
  S.RegisterDelphiFunction(@AnsiEndsText, 'AnsiEndsText', cdRegister);
  S.RegisterDelphiFunction(@AnsiReplaceText, 'AnsiReplaceText', cdRegister);
  S.RegisterDelphiFunction(@AnsiMatchText, 'AnsiMatchText', cdRegister);
  S.RegisterDelphiFunction(@AnsiIndexText, 'AnsiIndexText', cdRegister);

  // Case sensitive search/replace

  S.RegisterDelphiFunction(@AnsiContainsStr, 'AnsiContainsStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiStartsStr, 'AnsiStartsStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiEndsStr, 'AnsiEndsStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiReplaceStr, 'AnsiReplaceStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiMatchStr, 'AnsiMatchStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiIndexStr, 'AnsiIndexStr', cdRegister);
  //S.RegisterDelphiFunction(@MatchStr, 'MatchStr', cdRegister);
  //S.RegisterDelphiFunction(@IndexStr, 'IndexStr', cdRegister);

end;

end.
