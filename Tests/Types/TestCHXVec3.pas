program TestCHXVec3;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, utCHXVec3R;

procedure Assert(const Condition: Boolean; const Msg: string);
begin
  if Condition then
    Writeln('[ OK ] ', Msg)
  else
    Writeln('[FAIL] ', Msg);
end;

procedure TestBasicsAndOperators;
var
  V1, V2, V3: TCHXVec3R;
begin
  Writeln('--- Testing Basics & Operators ---');
  
  // Test de la función global de creación al vuelo
  V1 := CHXVec3R(1.0, 2.0, 3.0);
  Assert((V1.X = 1.0) and (V1.Y = 2.0) and (V1.Z = 3.0), 'Global constructor CHXVec3');

  // Test de operadores de clase (Suma y Resta)
  V2 := CHXVec3R(4.0, 5.0, 6.0);
  V3 := V1 + V2;
  Assert(V3.IsEqual3D(CHXVec3R(5.0, 7.0, 9.0)), 'Class Operator + (v1 + v2)');
  
  V3 := V2 - V1;
  Assert(V3.IsEqual3D(CHXVec3R(3.0, 3.0, 3.0)), 'Class Operator - (v2 - v1)');

  // Test de escalado
  V3 := V1 * 2.0;
  Assert(V3.IsEqual3D(CHXVec3R(2.0, 4.0, 6.0)), 'Class Operator * (Vector * Scale)');
end;

procedure TestGeometrics;
var
  V: TCHXVec3R;
  Normal, Reflected: TCHXVec3R;
begin
  Writeln;
  Writeln('--- Testing Geometric & Advanced Methods ---');

  // Test de Magnitud y Distancia
  V := CHXVec3R(3.0, 4.0, 0.0);
  Assert(Math.SameValue(V.GetMagnitude3D(), 5.0), 'Magnitude calculation (3, 4, 0) -> 5');
  
  // Test de Normalización
  V.Normalize;
  Assert(Math.SameValue(V.GetMagnitude3D(), 1.0), 'Normalization magnitude equals 1.0');

  // Test de Reflexión (Incidencia de luz/física)
  // Un vector que baja en diagonal (-1, -1, 0) choca con un suelo cuya normal es (0, 1, 0)
  V := CHXVec3R(-1.0, -1.0, 0.0);
  Normal := CHXVec3R(0.0, 1.0, 0.0);
  Reflected := V.Reflect(Normal);
  // Debería rebotar hacia arriba en diagonal (-1, 1, 0)
  Assert(Reflected.IsEqual3D(CHXVec3R(-1.0, 1.0, 0.0)), 'Vector reflection against plane normal');
end;

begin
  try
    TestBasicsAndOperators;
    TestGeometrics;
  except
    on E: Exception do
      Writeln('Exception occurred: ', E.Message);
  end;
  
  Writeln;
  Writeln('Press Enter to exit...');
  Readln;
end.
