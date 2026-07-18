program TestCHXMatrix;

// ToDo: Traducir al inglés
{$mode objfpc}{$H+}

uses
  SysUtils, Math, uCHXMatrix;

// Procedimiento auxiliar para mostrar matrices de forma bonita en la terminal
procedure PrintMatrix(const Name: string; const M: TCHXMatrixD);
var
  Row, Col: Integer;
begin
  WriteLn('--- ', Name, ' ---');
  if M.IsEmpty then
  begin
    WriteLn('[ Matrix is Empty ]');
    WriteLn;
    Exit;
  end;
  
  for Row := 0 to M.HighRow do
  begin
    Write('[ ');
    for Col := 0 to M.HighCol(Row) do
    begin
      Write(M[Row, Col]:8:4, ' ');
    end;
    WriteLn(']');
  end;
  WriteLn;
end;

var
  M1, M2, MProduct, MInverse, MIdentityTest: TCHXMatrixD;
  SystemMatrix, Solutions: TCHXMatrixD;
  Det: Double;

begin
  WriteLn('=== PRUEBAS UNITARIAS: uCHXMatrix ===');
  WriteLn;

  // 1. Inicialización y visualización
  M1.Init3x3(1.0, 2.0, 3.0,
             0.0, 1.0, 4.0,
             5.0, 6.0, 0.0);
  PrintMatrix('Matriz M1 Original', M1);

  // 2. Determinante 
  WriteLn('Determinante de M1: ', M1.Determinant:0:4);
  WriteLn('Rango de M1: ', M1.Rank);
  WriteLn;

  // 3. Aritmética y Operadores
  M2 := M1 * 2.0;
  PrintMatrix('Matriz M2 (M1 multiplicada por Escalar 2)', M2);

  MProduct := M1 * M1.Transpose;
  PrintMatrix('Matriz Producto (M1 multiplicada por su Transpuesta)', MProduct);
  WriteLn('Debe ser simétrica.');
  WriteLn;
  PrintMatrix('Matriz M1 Original', M1);
    
  // 4. Matriz Inversa con Gauss-Jordan
  MInverse := M1.Inverse;
  PrintMatrix('Matriz Inversa de M1', MInverse);
  Det := MInverse.Determinant;
  WriteLn('Determinante de la Inversa de M1: ', Det:0:4);
  WriteLn('Rango de la Inversa de M1: ', M1.Rank);
  WriteLn;
  // Verificación de la inversa: M1 * MInverse debería dar la Matriz Identidad
  MIdentityTest := M1 * MInverse;
  PrintMatrix('Verificación (M1 * Inversa)', MIdentityTest);
  WriteLn('Debe ser la identidad.');
  WriteLn;

  // 5. Resolución de Sistemas Lineales
  //  1x - 2y = 3 ; x = 1 
  //  5x - 5y = 10; y = -1
  // Matriz aumentada [A | B] de tamaño 2 filas x 3 columnas
  SystemMatrix.Init(2, 3, True);
  // Fila 0: coeficientes de x, y, y el término independiente
  SystemMatrix[0, 0] := 1.0;  
  SystemMatrix[0, 1] := -2.0; 
  SystemMatrix[0, 2] := 3.0; 
  // Fila 1
  SystemMatrix[1, 0] := 5.0;  
  SystemMatrix[1, 1] := -5.0; 
  SystemMatrix[1, 2] := 10.0;

  PrintMatrix('Sistema Lineal Aumentado [A | B]', SystemMatrix);

  // Resolvemos el sistema
  Solutions := SystemMatrix.SolveLinear;
  
  WriteLn('--- Soluciones del Sistema ---');
  if not Solutions.IsEmpty then
  begin
    WriteLn('X = ', Solutions[0, 0]:0:4, ' ( 1)');
    WriteLn('Y = ', Solutions[1, 0]:0:4, ' (-1)');
  end
  else
    WriteLn('El sistema no tiene una solución única.');
    
  WriteLn;
  WriteLn('=== Fin de los Tests ===');
end.
