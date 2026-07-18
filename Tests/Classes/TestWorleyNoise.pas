program TestWorleyNoise;

{$mode objfpc}{$H+}

uses
  SysUtils, ucCHXWorleyNoise, uCHXPoint3DF;

const
  WIDTH = 60;
  HEIGHT = 20;
  // Caracteres de densidades para representar los niveles de ruido (distancia)
  ASCII_SHADES: array[0..5] of Char = (' ', '.', ':', '+', '*', '#');

procedure TestConsoleRender;
var
  Noise: cCHXWorleyNoise;
  x, y: Integer;
  Val: Real;
  CharIdx: Integer;
  Line: string;
begin
  // Inicializamos el generador de aleatorios de FPC
  Randomize;

  // Creamos el ruido generando 5 puntos en un espacio 2D de 0 a WIDTH y 0 a HEIGHT
  // Dejamos Z en 0 ya que renderizaremos un plano
  Noise := cCHXWorleyNoise.Create(5, 0, WIDTH, 0, HEIGHT, 0, 0);
  try
    Writeln('--- Worley Noise Console Test ---');
    Writeln('Points generated: ', Noise.Points.Count);
    Writeln('---------------------------------');

    for y := 0 to HEIGHT - 1 do
    begin
      Line := '';
      for x := 0 to WIDTH - 1 do
      begin
        // Evaluamos el ruido en la posición actual (idxDist = 0 para el más cercano)
        Val := Noise.GetValue(x, y, 0, 0);

        // Mapeamos la distancia a un índice del array de caracteres.
        // Ajusta el divisor (5.0) para cambiar el "contraste" del mapa
        CharIdx := Trunc(Val / 3.0);
        if CharIdx < 0 then CharIdx := 0;
        if CharIdx > Length(ASCII_SHADES) then CharIdx := Length(ASCII_SHADES);

        Line := Line + ASCII_SHADES[CharIdx];
      end;
      Writeln(Line);
    end;

  finally
    Noise.Free;
  end;
end;

begin
  TestConsoleRender;
  Writeln('Press Enter to exit...');
  Readln;
end.
