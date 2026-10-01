{$IFNDEF FPC_DOTTEDUNITS}
unit ucCHXTestReport;
{$ENDIF FPC_DOTTEDUNITS}
{< Unit of `cCHXResultsWriter`. A simple writer for FPCUnit test results.

  Heavy Modified from the example PlainTestWriter.pas by Dean Zobec of the
  Free Component Library (FCL).

  TCustomResultsWriter seems to be actually intended to set `On[x]` event
  properties instead inheriting and overriding it's methods. `CurrFailure`
  is the actual reason to override methods.

  This class writes tests progression in console (instead only dots of
  ConsoleTestRunner.pas or need to wait to finish of SimpleTestRunner).

  (c) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$inline ON}{$WARN 6058 OFF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Reports,
  FpcUnit.Decorator;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FPCUnit, FPCUnitReport, TestDecorator;
{$ENDIF FPC_DOTTEDUNITS}

type

  { cCHXResultsWriter }

  cCHXResultsWriter = class(TCustomResultsWriter)
  private
    CurrFailure : TTestFailure;

    function GetPadding(aLevel : Integer) : String;
    function FormatedTime(ATiming : TDateTime) : String;

  protected
    // procedure WriteHeader; override;
    // procedure WriteFooter; override;
    // procedure WriteTestHeader(ATest : TTest; ALevel : Integer;
    //   ACount : Integer); override;
    procedure WriteTestFooter(ATest : TTest; ALevel : Integer;
      ATiming : TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite : TTestSuite; ALevel : Integer);
      override;
    procedure WriteSuiteFooter(ATestSuite : TTestSuite; ALevel : Integer;
      ATiming : TDateTime; ANumRuns : Integer; ANumErrors : Integer;
      ANumFailures : Integer; ANumIgnores : Integer); override;

  public
    constructor Create(aOwner : TComponent); override;
    destructor  Destroy; override;

    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    // procedure StartTest(ATest: TTest); override;
    // procedure EndTest(ATest: TTest); override;
    // procedure StartTestSuite(ATestSuite: TTestSuite); override;
    // procedure EndTestSuite(ATestSuite: TTestSuite); override;
    procedure WriteResult(aResult: TTestResult); override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils;
{$ELSE FPC_DOTTEDUNITS}
uses DateUtils;
{$ENDIF FPC_DOTTEDUNITS}

{
  cCHXResultsWriter
}

function cCHXResultsWriter.GetPadding(aLevel : Integer) : String;
var
  i : Integer;
begin
  Result := '';

  for i := 1 to aLevel do
  begin
    Result += ' |  ';
  end;
end;

function cCHXResultsWriter.FormatedTime(ATiming : TDateTime): String;
var
  M : Int64;
  FmtStr : String;
begin
  FmtStr := 'ss.zzz';
  M := MinutesBetween(ATiming, 0);
  if M >= 60 then
    FmtStr := 'hh:mm:' + FmtStr
  else if M >= 1 then
   FmtStr := 'nn:' + FmtStr;

  Result := FormatDateTime(FmtStr, ATiming);
end;

procedure cCHXResultsWriter.WriteTestFooter(ATest : TTest; ALevel : Integer;
  ATiming : TDateTime);
var
  S, Padding : String;
begin
  inherited;

  Padding := GetPadding(ALevel + 1);

  S := Padding;
  if not Sparse then
    if not SkipTiming then
      S := S + FormatedTime(ATiming) + '  ';

  S := S + ATest.TestName;

  if not Assigned(CurrFailure) then
  begin
    WriteLn(S);
    Exit;
  end;

  // Testing Error first, then Ignored and finally Failure
  if not CurrFailure.IsFailure then
    S := S + ' ¡¡ERROR!!'
  else if CurrFailure.IsIgnoredTest then
    S := S + ' Ignored: ' + CurrFailure.ExceptionMessage
  else // CurrFailure.IsFailure
    S := S + ' ¡¡Failed!!';

  WriteLn(S);

  CurrFailure := nil;
end;

procedure cCHXResultsWriter.WriteSuiteHeader(ATestSuite : TTestSuite;
  ALevel : Integer);
begin
  inherited;

  if ATestSuite.TestName <> '' then
    WriteLn(GetPadding(ALevel), ATestSuite.TestName)
  else
    WriteLn(GetPadding(ALevel), 'Tests');
end;

procedure cCHXResultsWriter.WriteSuiteFooter(ATestSuite : TTestSuite;
  ALevel : Integer; ATiming : TDateTime; ANumRuns : Integer;
  ANumErrors : Integer; ANumFailures : Integer; ANumIgnores : Integer);
begin
  inherited;

  // WriteLn(GetPadding(ALevel + 1));
  if ATestSuite.TestName <> '' then
    Write(GetPadding(ALevel), ATestSuite.TestName, ' Results:')
  else
    Write('TOTAL:');
  if not SkipTiming then
    Write(' T=', FormatedTime(ATiming));
  WriteLn(' N=', ANumRuns, ' E=', ANumErrors, ' F=', ANumFailures,
    ' I=', ANumIgnores);
end;

constructor cCHXResultsWriter.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  CurrFailure := nil;
end;

destructor  cCHXResultsWriter.Destroy;
begin
  inherited Destroy;
end;

procedure cCHXResultsWriter.AddFailure(ATest : TTest; AFailure : TTestFailure);
begin
  inherited AddFailure(ATest, AFailure);
  CurrFailure := AFailure;
end;

procedure cCHXResultsWriter.AddError(ATest : TTest; AError : TTestFailure);
begin
  inherited AddError(ATest, AError);
  CurrFailure := AError;
end;

procedure cCHXResultsWriter.WriteResult(aResult : TTestResult);

  procedure WriteFailure(const aFail : TTestFailure;
    const IsIgnored : Boolean = False);
  begin
    WriteLn;
    WriteLn(aFail.AsString);
    if IsIgnored or SkipAddressInfo then Exit;
    WriteLn(aFail.LocationInfo);
  end;

var
  aFailure : Pointer;
begin

  if aResult.NumberOfErrors > 0 then
  begin
    WriteLn;
    WriteLn(StringOfChar('-', 20));
    WriteLn('List of errors:', aResult.NumberOfErrors);
    for aFailure in aResult.Errors do
      WriteFailure(TTestFailure(aFailure), False);
  end;

  if aResult.NumberOfFailures > 0 then
  begin
    WriteLn;
    WriteLn(StringOfChar('-', 20));
    WriteLn('List of failures: ', aResult.NumberOfFailures);
    for aFailure in aResult.Failures do
      WriteFailure(TTestFailure(aFailure), False);
  end;

  if aResult.NumberOfIgnoredTests > 0 then
  begin
    WriteLn;
    WriteLn(StringOfChar('-', 20));
    WriteLn('List of ignored tests: ', aResult.NumberOfIgnoredTests);
    for aFailure in aResult.IgnoredTests do
    begin
      WriteLn;
      WriteFailure(TTestFailure(aFailure), True);
    end;
  end;
  WriteLn;
end;

end.
{<
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
