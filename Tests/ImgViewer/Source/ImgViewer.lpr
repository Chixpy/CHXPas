program ImgViewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrMain, ufrCHXForm, uCHXImageUtils, uCHXStrUtils, uCHXConst,
  uCHXVerInfo, uCHXRscStr, ufCHXFrame, ufCHXBGRAImgViewerEx, ufCHXBGRAImgViewer,
  ufCHXImgViewer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TCHXForm, CHXForm);
  Application.Run;
end.

