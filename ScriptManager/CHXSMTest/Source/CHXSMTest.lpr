program CHXSMTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pascalscript, ufrMain, ufrCHXForm, ufCHXFrame, uCHXConst,
  uCHXImageUtils, uCHXRscStr, uCHXStrUtils, uCHXVerInfo, uCHXDlgUtils,
  uCHX7zWrapper, uCHXExecute, uCHXFileUtils, ufCHXScriptManager,
  ucCHXScriptEngine, ufSMAskMultiFile, ufSMAskOption, uaCHXStorable,
  uPSI_CHXBasic, uPSI_FPCDateUtils, uPSI_FPCFileUtil, uPSI_FPCLazUTF8,
  uPSI_FPCStrUtils, uPSI_FPCSysUtils, uPSI_u7zWrapper, uPSI_uaCHXStorable,
  uPSI_uCHXFileUtils, uPSI_uCHXStrUtils;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TCHXForm, CHXForm);
  Application.Run;
end.

