{$IFNDEF FPC_DOTTEDUNITS}
unit ucCHXTestRunner;
{$ENDIF FPC_DOTTEDUNITS}
{< Modification of SimpleTestRunner (and ConsoleTestRunner then) of FPCUnit
  to use a TCHXResultsWriter and some other customizations.

  Copyright (C) 2006 Vincent Snijders (ConsoleTestRunner.pas)
  Copyright (C) 2016 Sven Barth (SimpleTestRunner.pas)
  (c) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$inline ON}{$WARN 6058 OFF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Fcl.CustApp, System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Registry,
  FpcUnit.Reports, CHXPas.Classes.ucCHXTestReport;
{$ELSE FPC_DOTTEDUNITS}
uses
  CustApp, Classes, SysUtils, FPCUnit, TestRegistry, FPCUnitReport,
  ucCHXTestReport;
{$ENDIF FPC_DOTTEDUNITS}

const
  Version = '1.0';

type
  { cCHXTestRunner }

  cCHXTestRunner = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function DoTestRun(ATest: TTest): Boolean;
  end;

implementation

function cCHXTestRunner.DoTestRun(ATest: TTest): Boolean;
var
  ResultsWriter: TCustomResultsWriter;
  TestResult: TTestResult;
begin
  ResultsWriter := cCHXResultsWriter.Create(Nil);
  TestResult := TTestResult.Create;
  try
    TestResult.AddListener(ResultsWriter);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);
    Result := (TestResult.NumberOfErrors = 0) and (TestResult.NumberOfFailures = 0);
  finally
    TestResult.Free;
    ResultsWriter.Free;
  end;
end;

procedure cCHXTestRunner.DoRun;
begin
  if not DoTestRun(GetTestRegistry) then
    ExitCode := 1;
  Terminate;
end;

end.
{<
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
