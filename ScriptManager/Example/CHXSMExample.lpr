program CHXSMExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pascalscript, ufCHXSMExampleMain, ucCHXScriptEngine, uPSI_FPCStrUtils,
  uPSI_uCHXStrUtils, uPSI_uCHXFileUtils, uPSI_u7zWrapper, uCHX7zWrapper
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmCHXSMExampleMain, frmCHXSMExampleMain);
  Application.Run;
end.

