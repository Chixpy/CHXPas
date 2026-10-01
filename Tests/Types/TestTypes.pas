program TestTypes;

{$mode objfpc}{$H+}

uses
  SysUtils, ucCHXTestRunner, ucCHXVec3Test;

var
  AppTest: cCHXTestRunner;
begin
  // Changing to executable directory
  ChDir(ExtractFilePath(ParamStr(0)));

  AppTest := cCHXTestRunner.Create(nil);
  AppTest.Initialize;
  AppTest.Title := 'CHX Types Tests';
  AppTest.Run;
  AppTest.Free;
end.
