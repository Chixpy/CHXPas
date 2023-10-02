program Test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufTest, uPunto, uFigura, uCirculo, uCuadrado, uCuadrilatero, uElipse,
  uFAbierta, uFCerrada, uListaFiguras, uPoligono, uPolilinea, uRectangulo,
  uSegmento, uTriangulo, ucPoint, uCHXStrUtils, uc2DShape, ucPointList,
uc2DShapeList
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

