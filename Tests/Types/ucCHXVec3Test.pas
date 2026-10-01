unit ucCHXVec3Test;

{$mode objfpc}{$H+}

{.$DEFINE StatusImplemented}

interface
uses
  SysUtils, Math, utCHXVec3R,
  FPCUnit, TestRegistry;

const
  kNIterations = 100;

type
  cCHXVec3Test = class(TTestCase)
  protected
    Vec1, Vec2, VecR : TCHXVec3R;

    procedure SetUp; override;
    // procedure TearDown; override;
    // procedure RunTest; override;

  public
    class procedure StatusVec(const aVec : TCHXVec3R; const aCallStr : String);
    class procedure CheckVecEqu(const aVecR, aVecF : TCHXVec3R;
      const aCallStr : String);
    class procedure CheckVecNEq(const aVecR, aVecF : TCHXVec3R;
      const aCallStr : String);

  published
    procedure OperatorEqual;
    procedure Init3D;
    procedure InitXYZ;
    procedure InitPolar3D;
    procedure InitPolarXYZ;
    procedure InitRandom3DXYZ;
    procedure InitRndPolar3D;
    procedure InitRndPolarXYZ;
  end;

implementation

procedure cCHXVec3Test.SetUp;
begin
  // Some random initial values
  Vec1.Init3D(1, 2, 3);
  Vec2.Init3D(4, 5, 6);
  VecR.Init3D(7, 8, 9);
end;

procedure cCHXVec3Test.StatusVec(const aVec : TCHXVec3R;
  const aCallStr : String);
begin
{$IFDEF StatusImplemented}
  Status(aCallStr + ' = (%s)', [aVec.ToStringFixed]);
{$ELSE}
  // ToDo: Better Status substitute
  WriteLn(Format(aCallStr + ' = (%s)', [aVec.ToStringFixed]));
{$ENDIF}
end;

procedure cCHXVec3Test.CheckVecEqu(const aVecR, aVecF : TCHXVec3R;
  const aCallStr : String);
begin
  if (aVecR <> aVecF) then
    Fail(aCallStr + ' = (%s) <> (%s)',
      [aVecF.ToStringFixed, aVecR.ToStringFixed])
  else
    Inc(AssertCount);
end;

procedure cCHXVec3Test.CheckVecNEq(const aVecR, aVecF : TCHXVec3R;
  const aCallStr : String);
begin
  if (aVecR = aVecF) then
    Fail(aCallStr + ': (%s) = (%s)',
      [aVecF.ToStringFixed, aVecR.ToStringFixed])
  else
    Inc(AssertCount);
end;

procedure cCHXVec3Test.OperatorEqual;
begin
  Vec1.X := 1; Vec1.Y := 2; Vec1.Z := 3;
  Vec2.R := 4; Vec2.G := 5; Vec2.B := 6;
  VecR.Comp[0] := 1; VecR.Comp[1] := 2; VecR.Comp[2] := 3;
  CheckVecEqu(VecR, Vec1, 'Operator =');
  CheckVecNEq(VecR, Vec2, 'Operator <>');
end;

procedure cCHXVec3Test.Init3D;
begin
  Vec1.Init3D(1, 2, 3);
  // Mixing Alias: Comp[0] = R = X, Comp[1] = G = Y, Comp[2] = B = Z
  VecR.R := 1; VecR.G := 2; VecR.B := 3;

  CheckEquals(VecR.X, Vec1.Comp[0], 'Init3D: Setting X or R');
  CheckEquals(VecR.Y, Vec1.Comp[1], 'Init3D: Setting Y or G');
  CheckEquals(VecR.Z, Vec1.Comp[2], 'Init3D: Setting Z or B');

  Vec1 := CHXVec3R(1, 2, 3);
  VecR.Init3D(1, 2, 3);
  CheckVecEqu(VecR, Vec1, 'CHXVec3R(1, 2, 3)');
end;

procedure cCHXVec3Test.InitXYZ;
begin
  Vec1.Init3D(7, 8, 9);

  Vec1.InitXY(1, 2);
  VecR.Init3D(1, 2, 0);
  CheckVecEqu(VecR, Vec1, 'InitXY(1, 2)');

  Vec1.InitXZ(1, 2);
  VecR.Init3D(1, 0, 2);
  CheckVecEqu(VecR, Vec1, 'InitXZ(1, 2)');

  Vec1.InitZY(1, 2);
  VecR.Init3D(0, 2, 1);
  CheckVecEqu(VecR, Vec1, 'InitZY(1, 2)');
end;

procedure cCHXVec3Test.InitPolar3D;
begin
  Vec1.InitPolar3D(1, 0, 0);
  VecR.Init3D(0, 0, 1);
  CheckVecEqu(VecR, Vec1, 'InitPolar3D(1, 0, 0)');

  Vec1.InitPolar3D(2, Pi, 0);
  VecR.Init3D(0, 0, -2);
  CheckVecEqu(VecR, Vec1, 'InitPolar3D(2, Pi, 0)');

  Vec1.InitPolar3D(3, Pi, Pi * 0.5);
  VecR.Init3D(0, 3, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolar3D(3, Pi, Pi * 0.5)');

  Vec1.InitPolar3D(4, 0, -Pi * 0.5);
  VecR.Init3D(0, -4, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolar3D(4, 0, -Pi * 0.5)');

  Vec1.InitPolar3D(5, Pi * 0.5, 0);
  VecR.Init3D(-5, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolar3D(5, Pi * 0.5, 0)');

  Vec1.InitPolar3D(6, -Pi * 0.5, 0);
  VecR.Init3D(6, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolar3D(6, -Pi * 0.5, 0)');
end;

procedure cCHXVec3Test.InitPolarXYZ;
begin
  Vec1.InitPolarXY(1, 0);
  VecR.Init3D(1, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarXY(1, 0)');

  Vec1.InitPolarXY(2, Pi * 0.5);
  VecR.Init3D(0, 2, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarXY(2, Pi * 0.5)');

  Vec1.InitPolarXY(3, -Pi);
  VecR.Init3D(-3, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarXY(3, -Pi)');

  Vec1.InitPolarXY(4, 1.5 * Pi);
  VecR.Init3D(0, -4, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarXY(4, 1.5 * Pi)');

  Vec1.InitPolarXZ(1, 2 * Pi);
  VecR.Init3D(1, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarXZ(1, 2 * Pi)');

  Vec1.InitPolarXZ(2, -1.5 * Pi);
  VecR.Init3D(0, 0, -2);
  CheckVecEqu(VecR, Vec1, 'InitPolarXZ(2, -1.5 * Pi)');

  Vec1.InitPolarXZ(3, Pi);
  VecR.Init3D(-3, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarXZ(3, Pi)');

  Vec1.InitPolarXZ(4, -0.5 * Pi);
  VecR.Init3D(0, 0, 4);
  CheckVecEqu(VecR, Vec1, 'InitPolarXZ(4, -0.5 * Pi)');

  Vec1.InitPolarZY(1, 2 * Pi);
  VecR.Init3D(1, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarZY(1, 2 * Pi)');

  Vec1.InitPolarZY(2, -1.5 * Pi);
  VecR.Init3D(0, 0, -2);
  CheckVecEqu(VecR, Vec1, 'InitPolarZY(2, -1.5 * Pi)');

  Vec1.InitPolarZY(3, Pi);
  VecR.Init3D(-3, 0, 0);
  CheckVecEqu(VecR, Vec1, 'InitPolarZY(3, Pi)');

  Vec1.InitPolarZY(4, -0.5 * Pi);
  VecR.Init3D(0, 0, 4);
  CheckVecEqu(VecR, Vec1, 'InitPolarZY(4, -0.5 * Pi)');
end;

procedure cCHXVec3Test.InitRandom3DXYZ;
var
  TempStr : String;
  MaxMin: Real = 10.0;
  i : Integer;
begin
  TempStr := 'InitRandom3D(-%0:g, -%0:g, -%0:g, %0:g, -%0:g, %0:g): ';
  for i := 1 to kNIterations do
  begin
    Vec1.InitRandom3D(-MaxMin, MaxMin, -MaxMin, MaxMin, -MaxMin, MaxMin);

    Check(Vec1.X >= -MaxMin, Format(TempStr + 'X < -%0:g', [MaxMin]));
    Check(Vec1.X < MaxMin, Format(TempStr + 'X >= %0:g', [MaxMin]));
    Check(Vec1.Y >= -MaxMin, Format(TempStr + 'Y < -%0:g', [MaxMin]));
    Check(Vec1.Y < MaxMin, Format(TempStr + 'Y >= %0:g', [MaxMin]));
    Check(Vec1.Z >= -MaxMin, Format(TempStr + 'Z < -%0:g', [MaxMin]));
    Check(Vec1.Z < MaxMin, Format(TempStr + 'Z >= %0:g', [MaxMin]));
  end;

  TempStr := 'InitRandomXY(-%0:g, -%0:g, -%0:g, %0:g): ';
  for i := 1 to kNIterations do
  begin
    Vec1.InitRandomXY(-MaxMin, MaxMin, -MaxMin, MaxMin);

    Check(Vec1.X >= -MaxMin, Format(TempStr + 'X < -%0:g', [MaxMin]));
    Check(Vec1.X < MaxMin, Format(TempStr + 'X >= %0:g', [MaxMin]));
    Check(Vec1.Y >= -MaxMin, Format(TempStr + 'Y < -%0:g', [MaxMin]));
    Check(Vec1.Y < MaxMin, Format(TempStr + 'Y >= %0:g', [MaxMin]));
    CheckEquals(0, Vec1.Z, Format(TempStr + 'Z = %1:g <> 0',
      [MaxMin, Vec1.Z]));
  end;

  TempStr := 'InitRandomXZ(-%0:g, -%0:g, -%0:g, %0:g): ';
  for i := 1 to kNIterations do
  begin
    Vec1.InitRandomXZ(-MaxMin, MaxMin, -MaxMin, MaxMin);

    Check(Vec1.X >= -MaxMin, Format(TempStr + 'X < -%0:g', [MaxMin]));
    Check(Vec1.X < MaxMin, Format(TempStr + 'X >= %0:g', [MaxMin]));
    CheckEquals(0, Vec1.Y, Format(TempStr + 'Y = %1:g <> 0',
      [MaxMin, Vec1.Y]));
    Check(Vec1.Z >= -MaxMin, Format(TempStr + 'Z < -%0:g', [MaxMin]));
    Check(Vec1.Z < MaxMin, Format(TempStr + 'Z >= %0:g', [MaxMin]));
  end;

  TempStr := 'InitRandomZY(-%0:g, -%0:g, -%0:g, %0:g): ';
  for i := 1 to kNIterations do
  begin
    Vec1.InitRandomZY(-MaxMin, MaxMin, -MaxMin, MaxMin);

    CheckEquals(0, Vec1.X, Format(TempStr + 'X = %1:g <> 0',
      [MaxMin, Vec1.X]));
    Check(Vec1.Y >= -MaxMin, Format(TempStr + 'Y < -%0:g', [MaxMin]));
    Check(Vec1.Y < MaxMin, Format(TempStr + 'Y >= %0:g', [MaxMin]));
    Check(Vec1.Z >= -MaxMin, Format(TempStr + 'Z < -%0:g', [MaxMin]));
    Check(Vec1.Z < MaxMin, Format(TempStr + 'Z >= %0:g', [MaxMin]));
  end;
end;

procedure cCHXVec3Test.InitRndPolar3D;
var
  i : Integer;
begin
  for i := 1 to kNIterations do
  begin
    Vec1.InitRndPolar3D(i);

    CheckEquals(i, Vec1.GetMagnitude3D, Format(
      'InitRndPolar3D(%0:d).GetMagnitude3D = %1:g <> %0:d',
      [i, Vec1.GetMagnitude3D]));
  end;
end;

procedure cCHXVec3Test.InitRndPolarXYZ;
var
  i : Integer;
begin
  for i := 1 to kNIterations do
  begin
    Vec1.InitRndPolarXY(i);

    CheckEquals(0, Vec1.Z, Format('InitRndPolarXY(%0:d).Z = %1:g <> 0',
      [i, Vec1.Z]));
    CheckEquals(i, Vec1.GetMagnitude3D, Format(
      'InitRndPolarXY(%0:d).GetMagnitude3D = %1:g <> %0:d',
      [i, Vec1.GetMagnitude3D]));
  end;

  for i := 1 to kNIterations do
  begin
    Vec1.InitRndPolarXZ(i);

    CheckEquals(i, Vec1.GetMagnitude3D, Format(
      'InitRndPolarXZ(%0:g).GetMagnitude3D = %1:g <> %0:g',
      [i, Vec1.GetMagnitude3D]));
    CheckEquals(0, Vec1.Y, Format('InitRndPolarXZ(%0:g).Y = %1:g <> %0:g',
      [i, Vec1.Y]));
  end;

  for i := 1 to kNIterations do
  begin
    Vec1.InitRndPolarZY(i);

    CheckEquals(i, Vec1.GetMagnitude3D, Format(
      'InitRndPolarZY(%0:g).GetMagnitude3D = %1:g <> %0:g',
      [i, Vec1.GetMagnitude3D]));
    CheckEquals(0, Vec1.X, Format('InitRndPolarZY(%0:g).Z = %1:g <> %0:g',
      [i, Vec1.X]));
  end;
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

initialization

RegisterTest('Types', cCHXVec3Test);

finalization

end.
