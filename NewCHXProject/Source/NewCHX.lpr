program NewCHX;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrMain, ufrCHXForm, uCHXConst, uCHXStrUtils,
  uCHXRscStr, uCHXImageUtils, uCHXVerInfo, ufCHXFrame
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TCHXForm, CHXForm);
  Application.Run;
end.

