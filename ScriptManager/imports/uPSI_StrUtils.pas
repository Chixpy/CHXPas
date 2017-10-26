unit uPSI_StrUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, uPSRuntime, uPSCompiler;


// TODO: Importar StrUtils

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
end;

end.
