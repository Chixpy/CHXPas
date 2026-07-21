(*< Unit Template for custom 3 component records.

  Usage: Create a unit file, replace this IDs:

    unit <UnitName>;

    {$macro ON}
    {$define TRealType := <FloatDataType> }
    //< Actual type of the components.
    {$define TCHXVec3Type := <Vec3ID> }
    //< ID for the main type.
    {$define CHXVec3Func := <Vec3Func> }
    //< Name for the function that returns a new TCHXVec3Type.
    {$define PCHXVec3 := <PointerID> }
    //< ID of the pointer type to TCHXVec3Type type.

    {$define TCHXPoint3DF := <TCHXPoint3DFID> }
    //< TCHXPoint3DF type equivalent for retrocompatibility.
    {$define TCHXColorF := <TCHXColorFID> }
    //< TCHXColorF type equivalent for retrocompatibility.

    {$define cCHXVec3GenList := <Vec3GenListID> }
    //< ID for generic list specialized to TCHXVec3Type type.
    {$define cCHXVec3List := <Vec3ListID> }
    //< ID for a generic list descendant.

    {$define UnitsUsed := , <Units> ; }
    {<
      Units needed for TRealType, compare and mathematical functions.
      if no unit needed, must be ';'.
      Maybe function names below will require <Unit>.<Function> syntax.
    }
    {$define DataIsZero := <FunctionName> }
    //< function DataIsZero(TRealType): Boolean;
    {$define SameValueData := <FunctionName> }
    //< function SameValueData(TRealType, TRealType): Boolean;
    {$define SameValueSigma := <FunctionName> }
    //< function SameValueSigma(TRealType, TRealType, TRealType): Boolean;
    {$define SinCosData := <FunctionName> }
    //< procedure SinCosData(Angle, out aSin, out aCos);
    {$define ArcTan2Data := <FunctionName> }
    //< function ArcTan2Data(Y, X): TRealType;
    {$define ArcSinData := <FunctionName> }
    //< function ArcSinData(Angle): TRealType;

    {$I '<ThisFilename>'}


  Four types of Vec3 are implemented, for all basic float types:
    - @code(TCHXVec3S) for Single in uTCHXVec3S.pas
    - @code(TCHXVec3D) for Double in uTCHXVec3D.pas
    - @code(TCHXVec3E) for Extended in uTCHXVec3E.pas
    - @code(TCHXVec3R) for Real in uTCHXVec3R.pas
*)
(*
  This template is a generic data type designed to store 3 components of
    type TRealType, which must be replaced by desired floating-point type via 
    a {$Define} macro. The names of the types declared here must also be 
    redefined to avoid naming collisions.

  This type aims to replace TCHXPoint3DF and expand its functionality.
  
  Although primarily intended to store a 3D point or vector, it includes 
    several methods for 2D handling and other 3-component structures.
*)
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$inline ON}

interface
uses
  SysUtils, uCHXMath, FGL UnitsUsed

type
  TCHXVec3Type = packed record
    X, Y, Z: TRealType;
    { case Byte of }
    {   0: (X, Y, Z: TRealType); // Standard components }
    {   1: (R, G, B: TRealType); // Floating-point color [0..1] }
    {   2: (Comp: array[0..2] of TRealType); // Index-based array access }
    { end; }

    procedure Init3D(const aX: TRealType = 0; const aY: TRealType = 0;
      const aZ: TRealType = 0); inline;
    procedure InitXY(const aX: TRealType = 0; const aY: TRealType = 0); inline;
    procedure InitXZ(const aX: TRealType = 0; const aZ: TRealType = 0); inline;
    procedure InitZY(const aZ: TRealType = 0; const aY: TRealType = 0); inline;

    procedure InitPolar3D(const aMag: TRealType = 0; const aYaw: TRealType = 0;
      const aPitch: TRealType = 0);
    procedure InitPolarXY(const aMag: TRealType; const aAngle: TRealType = 0);
      inline;
    procedure InitPolarXZ(const aMag: TRealType; const aAngle: TRealType = 0); 
      inline;
    procedure InitPolarZY(const aMag: TRealType; const aAngle: TRealType = 0);
      inline;

    procedure Random3D(const aMag: TRealType = 1); inline;
    procedure RandomXY(const aMag: TRealType = 1); inline;
    procedure RandomXZ(const aMag: TRealType = 1); inline;
    procedure RandomZY(const aMag: TRealType = 1); inline;

    procedure GetAngles3D(out aYaw, aPitch: TRealType);
    function GetAngleXY: TRealType; inline;
    function GetAngleXZ: TRealType; inline;
    function GetAngleZY: TRealType; inline;

    function SqrMag3D: TRealType; inline;
    function SqrMagXY: TRealType; inline;
    function SqrMagXZ: TRealType; inline;
    function SqrMagZY: TRealType; inline;
    function GetMagnitude3D: TRealType; inline;
    function GetMagnitudeXY: TRealType; inline;
    function GetMagnitudeXZ: TRealType; inline;
    function GetMagnitudeZY: TRealType; inline;
    procedure Normalize;
    function GetNormalized: TCHXVec3Type; inline;

    procedure SetMagnitude(const aMag: TRealType); inline;

    // ToDo: SetMagnitudeXY, SetMagnitudeXZ and SetMagnitudeZY

    procedure SetAngles3D(const aYaw, aPitch: TRealType); inline;
    procedure SetAngleXY(const aAngle: TRealType); inline;
    procedure SetAngleXZ(const aAngle: TRealType); inline;
    procedure SetAngleZY(const aAngle: TRealType); inline;

    procedure RotateXY(const aAngle: TRealType); overload;
    procedure RotateXZ(const aAngle: TRealType); overload;
    procedure RotateZY(const aAngle: TRealType); overload;
    procedure RotateXY(const aAngle: TRealType; const Center: TCHXVec3Type);
      overload;
    procedure RotateXZ(const aAngle: TRealType; const Center: TCHXVec3Type);
      overload;
    procedure RotateZY(const aAngle: TRealType; const Center: TCHXVec3Type);
      overload;

    class operator = (const aVec1, aVec2: TCHXVec3Type): Boolean; inline;
    function IsZero3D: Boolean; inline; overload;
    function IsZeroXY: Boolean; inline; overload;
    function IsZeroXZ: Boolean; inline; overload;
    function IsZeroZY: Boolean; inline; overload;
    function IsZero3D(const Sigma: TRealType): Boolean; inline; overload;
    function IsZeroXY(const Sigma: TRealType): Boolean; inline; overload;
    function IsZeroXZ(const Sigma: TRealType): Boolean; inline; overload;
    function IsZeroZY(const Sigma: TRealType): Boolean; inline; overload;
    
    function IsEqual3D(const aVector: TCHXVec3Type): Boolean; inline; overload;
    function IsEqualXY(const aVector: TCHXVec3Type): Boolean; inline; overload;
    function IsEqualXZ(const aVector: TCHXVec3Type): Boolean; inline; overload;
    function IsEqualZY(const aVector: TCHXVec3Type): Boolean; inline; overload;
    function IsEqual3D(const aVector: TCHXVec3Type; const Sigma: TRealType)
      : Boolean; inline; overload;
    function IsEqualXZ(const aVector: TCHXVec3Type; const Sigma: TRealType)
      : Boolean; inline; overload;
    function IsEqualZY(const aVector: TCHXVec3Type; const Sigma: TRealType)
      : Boolean; inline; overload;
    function IsEqualXY(const aVector: TCHXVec3Type; const Sigma: TRealType)
      : Boolean; inline; overload;

    function IsOpposite(const aVector: TCHXVec3Type): Boolean; inline;
    // ToDo: Make version with Sigma
    function IsParallel(const aVector: TCHXVec3Type): Boolean; inline;
    //< Ignore direction.
    //< ToDo: Make version with Sigma
    function IsCoaligned(const aVector: TCHXVec3Type): Boolean; inline;
    //< Parallel, same direction.
    //< ToDo: Make version with Sigma
    function IsAntiParallel(const aVector: TCHXVec3Type): Boolean; inline;
    //< Parallel, opposite direction.
    // ToDo: Make version with Sigma
    function IsPerpendicular(const aVector: TCHXVec3Type): Boolean; inline;
    // ToDo: Make version with Sigma

    class operator -(const aVector: TCHXVec3Type): TCHXVec3Type; inline;
    procedure Negate;
    //< Other names: Opposite or Invert
    class operator + (const aVec1, aVec2: TCHXVec3Type): TCHXVec3Type; inline;
    procedure Add(const aVector: TCHXVec3Type); inline;
    class operator - (const aVec1, aVec2: TCHXVec3Type): TCHXVec3Type; inline;
    procedure Subtract(const aVector: TCHXVec3Type); inline;
    class operator * (const aVector: TCHXVec3Type; const aScale: TRealType)
      : TCHXVec3Type; inline;
    class operator * (const aScale: TRealType; const aVector: TCHXVec3Type)
      : TCHXVec3Type; inline;
    procedure Multiply(const aScale: TRealType); inline;
    class operator / (const aVector: TCHXVec3Type; const aScale: TRealType)
      : TCHXVec3Type; inline;
    procedure Divide(const aScale: TRealType); inline;
    class operator * (const aVec1, aVec2: TCHXVec3Type): TCHXVec3Type; inline;
    //< Component-wise scaling (Hadamard product)
    procedure Scale(const aVector: TCHXVec3Type); inline; overload;
    class operator / (const aScale: TRealType; const aVector: TCHXVec3Type)
      : TCHXVec3Type; inline;
    procedure DivInv(const aScale: TRealType); inline;

    function VectProd(const aVector: TCHXVec3Type): TCHXVec3Type; inline;
    function ScalProd(const aVector: TCHXVec3Type): TRealType; inline;

    procedure Move(const dX, dY, dZ: TRealType); inline;
    procedure Scale(const sX, sY, sZ: TRealType); inline; overload;

    function SqrDistance(const aPoint: TCHXVec3Type): TRealType; inline;
    function Distance(const aPoint: TCHXVec3Type): TRealType; inline;
    function InDistance(const aPoint: TCHXVec3Type; const aDistance: TRealType;
      const IncEqual: Boolean = False): Boolean;
    {<
      If IncEqual = False, strictly less than. 
      If True, it includes the value + Precision.
      ToDo: Versión with Sigma
    }

    function Reflect(const aNormal: TCHXVec3Type): TCHXVec3Type; inline;
    function Refract(const aNormal: TCHXVec3Type; const RefIdx: TRealType)
      : TCHXVec3Type;

    function ToString(const Delim : char = ',') : string;
    function ToStringFmt(const aStr: string) : string;
  end;

  // Aliases for retrocompatibility and clarity
  TCHXPoint3DF = TCHXVec3Type;
  TCHXColorF   = TCHXVec3Type;

  PCHXVec3 = ^TCHXVec3Type;

  // Generic and specialized lists using FGL (Free Pascal Generics Library)
  cCHXVec3GenList = specialize TFPGList<TCHXVec3Type>;
  
  cCHXVec3List = class(cCHXVec3GenList)
    { 
      This list could implement methods that apply operations to all 
      elements more efficiently. For example, applying rotations without 
      having to recalculate the same sines and cosines multiple times.
    }
  end;

  function CHXVec3Func(const aX: TRealType = 0; const aY: TRealType = 0;
    const aZ: TRealType = 0): TCHXVec3Type; inline;
  {<
    Global function to 'create' a TCHXVec3Type. 
    Useful to pass as a parameter without the need to explicitly 
    create a variable: MyFunc(CHXVec3Func(1, 2, 3), ...)
  }

implementation

{ TCHXVec3Type }

procedure TCHXVec3Type.Init3D(const aX, aY, aZ: TRealType);
begin
  X := aX; Y := aY; Z := aZ;
end;

procedure TCHXVec3Type.InitXY(const aX, aY: TRealType);
begin
  X := aX; Y := aY; Z := 0;
end;

procedure TCHXVec3Type.InitXZ(const aX, aZ: TRealType); 
begin
  X := aX; Y := 0; Z := aZ;
end;

procedure TCHXVec3Type.InitZY(const aZ, aY: TRealType); 
begin
  X := 0; Y := aY; Z := aZ;
end;

procedure TCHXVec3Type.InitPolar3D(const aMag, aYaw, aPitch: TRealType);
var
  SinXZ, CosXZ, SinY, CosY: TRealType;
  ProyXZ: TRealType;
begin
  { 
    aMag: Distance from the origin.
    aYaw: Azimuth.
    aPitch: Inclination.
    
    InitPolar(1, 0, 0) -> (0, 0, 1)
    
    Think in 1st person, not as a sphere we are looking at, 
      making this consistent with coordinate initialization.
  }
  SinCosData(aYaw, SinXZ, CosXZ);
  SinCosData(aPitch, SinY, CosY);

  Y := aMag * SinY;
  ProyXZ := aMag * CosY;
  X := ProyXZ * SinXZ;
  Z := ProyXZ * CosXZ;
end;

procedure TCHXVec3Type.InitPolarXY(const aMag, aAngle: TRealType); 
begin
  X := aMag * Cos(aAngle); Y := aMag * Sin(aAngle); Z := 0;
end;

procedure TCHXVec3Type.InitPolarXZ(const aMag, aAngle: TRealType); 
begin
  X := aMag * Cos(aAngle); Y := 0; Z := aMag * Sin(aAngle);
end;

procedure TCHXVec3Type.InitPolarZY(const aMag, aAngle: TRealType); 
begin
  X := 0; Y := aMag * Sin(aAngle); Z := aMag * Cos(aAngle);
end;

procedure TCHXVec3Type.Random3D(const aMag: TRealType); 
begin
  // FixMe: It can never be completely vertical upwards (0, 1, 0),
  //   but it can be (0, -1, 0)
  InitPolar3D(aMag, Random * k2Pi, Random * Pi - kHalfPi);
end;

procedure TCHXVec3Type.RandomXY(const aMag: TRealType); 
begin
  InitPolarXY(aMag, Random * k2Pi);
end;

procedure TCHXVec3Type.RandomXZ(const aMag: TRealType); 
begin
  InitPolarXZ(aMag, Random * k2Pi);
end;

procedure TCHXVec3Type.RandomZY(const aMag: TRealType); 
begin
  InitPolarZY(aMag, Random * k2Pi);
end;

procedure TCHXVec3Type.GetAngles3D(out aYaw, aPitch: TRealType);
begin
  if Self.IsZero3D then // Null Vector
  begin
    aYaw := 0; aPitch := 0;
    Exit;
  end;
  
  aYaw := ArcTan2Data(X, Z);
  aPitch := ArcSinData(Y / Self.GetMagnitude3D);
end;

function TCHXVec3Type.GetAngleXY: TRealType; 
begin
  Result := ArcTan2Data(Y, X);
end;

function TCHXVec3Type.GetAngleXZ: TRealType; 
begin
  Result := ArcTan2Data(X, Z); // 0° at (0, 0, 1)
end;

function TCHXVec3Type.GetAngleZY: TRealType; 
begin
  Result := ArcTan2Data(Y, Z);
end;

function TCHXVec3Type.SqrMag3D: TRealType; 
begin
  Result := X * X + Y * Y + Z * Z;
end;

function TCHXVec3Type.SqrMagXY: TRealType; 
begin
  Result := X * X + Y * Y;
end;

function TCHXVec3Type.SqrMagXZ: TRealType; 
begin
  Result := X * X + Z * Z;
end;

function TCHXVec3Type.SqrMagZY: TRealType; 
begin
  Result := Z * Z + Y * Y;
end;

function TCHXVec3Type.GetMagnitude3D: TRealType; 
begin
  Result := SqRt(Self.SqrMag3D);
end;

function TCHXVec3Type.GetMagnitudeXY: TRealType; 
begin
  Result := SqRt(Self.SqrMagXY);
end;

function TCHXVec3Type.GetMagnitudeXZ: TRealType; 
begin
  Result := SqRt(Self.SqrMagXZ);
end;

function TCHXVec3Type.GetMagnitudeZY: TRealType; 
begin
  Result := SqRt(Self.SqrMagZY);
end;

procedure TCHXVec3Type.Normalize;
begin
  if Self.IsZero3D then 
    Exit; // Keep at Zero

  // This way we only divide once
  Self.Multiply(1 / Self.GetMagnitude3D);
end;

function TCHXVec3Type.GetNormalized: TCHXVec3Type; 
begin
  Result := Self;
  Result.Normalize;
end;

procedure TCHXVec3Type.SetMagnitude(const aMag: TRealType); 
begin
  if Self.IsZero3D then 
    Exit; // Keep at zero

  Self.Multiply(aMag / Self.GetMagnitude3D);
end;

procedure TCHXVec3Type.SetAngles3D(const aYaw, aPitch: TRealType); 
begin
  Self.InitPolar3D(Self.GetMagnitude3D, aYaw, aPitch);
end;

procedure TCHXVec3Type.SetAngleXY(const aAngle: TRealType); 
begin
  // ToDo: Test is rotation more efficient than obtaining the vector magnitude
  Self.RotateXY(aAngle - Self.GetAngleXY);
end;

procedure TCHXVec3Type.SetAngleXZ(const aAngle: TRealType); 
begin
  // ToDo: Test is rotation more efficient than obtaining the vector magnitude
  Self.RotateXZ(aAngle - Self.GetAngleXZ);
end;

procedure TCHXVec3Type.SetAngleZY(const aAngle: TRealType); 
begin
  // ToDo: Test is rotation more efficient than obtaining the vector magnitude
  Self.RotateZY(aAngle - Self.GetAngleZY);
end;

procedure TCHXVec3Type.RotateXY(const aAngle: TRealType); 
var
  SinXY, CosXY: TRealType;
  TempX: TRealType;
begin
  SinCosData(aAngle, SinXY, CosXY);
  TempX := X;
  X := TempX * CosXY - Y * SinXY;
  Y := TempX * SinXY + Y * CosXY;
end;

procedure TCHXVec3Type.RotateXZ(const aAngle: TRealType); 
var
  SinXZ, CosXZ: TRealType;
  TempX: TRealType;
begin
  SinCosData(aAngle, SinXZ, CosXZ);
  TempX := X;
  X := TempX * CosXZ - Z * SinXZ;
  Z := TempX * SinXZ + Z * CosXZ;
end;

procedure TCHXVec3Type.RotateZY(const aAngle: TRealType); 
var
  SinZY, CosZY: TRealType;
  TempZ: TRealType;
begin
  SinCosData(aAngle, SinZY, CosZY);
  TempZ := Z;
  Z := TempZ * CosZY - Y * SinZY;
  Y := TempZ * SinZY + Y * CosZY;
end;

procedure TCHXVec3Type.RotateXY(const aAngle: TRealType; 
  const Center: TCHXVec3Type); 
begin
  Self.Subtract(Center);
  Self.RotateXY(aAngle);
  Self.Add(Center);
end;

procedure TCHXVec3Type.RotateXZ(const aAngle: TRealType; 
  const Center: TCHXVec3Type); 
begin
  Self.Subtract(Center);
  Self.RotateXZ(aAngle);
  Self.Add(Center);
end;

procedure TCHXVec3Type.RotateZY(const aAngle: TRealType; 
  const Center: TCHXVec3Type); 
begin
  Self.Subtract(Center);
  Self.RotateZY(aAngle);
  Self.Add(Center);
end;

function TCHXVec3Type.IsZero3D: Boolean;  
begin
  Result := DataIsZero(X) and DataIsZero(Y) and DataIsZero(Z);
end;

function TCHXVec3Type.IsZeroXY: Boolean;  
begin
  Result := DataIsZero(X) and DataIsZero(Y);
end;

function TCHXVec3Type.IsZeroXZ: Boolean;  
begin
  Result := DataIsZero(X) and DataIsZero(Z);
end;

function TCHXVec3Type.IsZeroZY: Boolean;  
begin
  Result := DataIsZero(Z) and DataIsZero(Y);
end;

function TCHXVec3Type.IsZero3D(const Sigma: TRealType): Boolean;  
begin
  Result := DataIsZero(X, Sigma) and DataIsZero(Y, Sigma) 
    and DataIsZero(Z, Sigma);
end;

function TCHXVec3Type.IsZeroXY(const Sigma: TRealType): Boolean;  
begin
  Result := DataIsZero(X, Sigma) and DataIsZero(Y, Sigma);
end;

function TCHXVec3Type.IsZeroXZ(const Sigma: TRealType): Boolean;  
begin
  Result := DataIsZero(X, Sigma) and DataIsZero(Z, Sigma);
end;

function TCHXVec3Type.IsZeroZY(const Sigma: TRealType): Boolean;  
begin
  Result := DataIsZero(Z, Sigma) and DataIsZero(Y, Sigma);
end;

class operator TCHXVec3Type.= (const aVec1, aVec2: TCHXVec3Type): Boolean; 
begin
  Result := SameValueData(aVec1.X, aVec2.X) and 
            SameValueData(aVec1.Y, aVec2.Y) and 
            SameValueData(aVec1.Z, aVec2.Z);
end;

function TCHXVec3Type.IsEqual3D(const aVector: TCHXVec3Type): Boolean;  
begin
  Result := Self = aVector;
end;

function TCHXVec3Type.IsEqualXY(const aVector: TCHXVec3Type): Boolean;  
begin
  Result := SameValueData(X, aVector.X) and SameValueData(Y, aVector.Y);
end;

function TCHXVec3Type.IsEqualXZ(const aVector: TCHXVec3Type): Boolean;  
begin
  Result := SameValueData(X, aVector.X) and SameValueData(Z, aVector.Z);
end;

function TCHXVec3Type.IsEqualZY(const aVector: TCHXVec3Type): Boolean;  
begin
  Result := SameValueData(Z, aVector.Z) and SameValueData(Y, aVector.Y);
end;

function TCHXVec3Type.IsEqual3D(const aVector: TCHXVec3Type; 
  const Sigma: TRealType): Boolean;  
begin
  Result := SameValueSigma(X, aVector.X, Sigma) 
    and SameValueSigma(Y, aVector.Y, Sigma) 
    and SameValueSigma(Z, aVector.Z, Sigma);
end;

function TCHXVec3Type.IsEqualXY(const aVector: TCHXVec3Type; 
  const Sigma: TRealType): Boolean;  
begin
  Result := SameValueSigma(X, aVector.X, Sigma) 
    and SameValueSigma(Y, aVector.Y, Sigma);
end;

function TCHXVec3Type.IsEqualXZ(const aVector: TCHXVec3Type; 
  const Sigma: TRealType): Boolean;  
begin
  Result := SameValueSigma(X, aVector.X, Sigma) 
    and SameValueSigma(Z, aVector.Z, Sigma);
end;

function TCHXVec3Type.IsEqualZY(const aVector: TCHXVec3Type; 
  const Sigma: TRealType): Boolean;  
begin
  Result := SameValueSigma(Z, aVector.Z, Sigma) 
    and SameValueSigma(Y, aVector.Y, Sigma);
end;

function TCHXVec3Type.IsOpposite(const aVector: TCHXVec3Type): Boolean; 
var
  Temp: TCHXVec3Type;
begin
  Temp.X := X + aVector.X;
  Temp.Y := Y + aVector.Y;
  Temp.Z := Z + aVector.Z;
  Result := Temp.IsZero3D;
end;

function TCHXVec3Type.IsParallel(const aVector: TCHXVec3Type): Boolean; 
begin
  Result := Self.VectProd(aVector).IsZero3D;
end;

function TCHXVec3Type.IsCoaligned(const aVector: TCHXVec3Type): Boolean; 
begin
  Result := (Self.ScalProd(aVector) > 0) and Self.IsParallel(aVector);
end;

function TCHXVec3Type.IsAntiParallel(const aVector: TCHXVec3Type): Boolean; 
begin
  Result := (Self.ScalProd(aVector) < 0) and Self.IsParallel(aVector);
end;

function TCHXVec3Type.IsPerpendicular(const aVector: TCHXVec3Type): Boolean; 
begin
  Result := DataIsZero(Self.ScalProd(aVector));
end;

class operator TCHXVec3Type.- (const aVector: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := -aVector.X; Result.Y := -aVector.Y; Result.Z := -aVector.Z;
end;

procedure TCHXVec3Type.Negate;
begin
  X := -X; Y := -Y; Z := -Z;
end;

class operator TCHXVec3Type.+ (const aVec1, aVec2: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := aVec1.X + aVec2.X;
  Result.Y := aVec1.Y + aVec2.Y;
  Result.Z := aVec1.Z + aVec2.Z;
end;

procedure TCHXVec3Type.Add(const aVector: TCHXVec3Type); 
begin
  X += aVector.X; Y += aVector.Y; Z += aVector.Z;
end;

class operator TCHXVec3Type.- (const aVec1, aVec2: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := aVec1.X - aVec2.X;
  Result.Y := aVec1.Y - aVec2.Y;
  Result.Z := aVec1.Z - aVec2.Z; 
end;

procedure TCHXVec3Type.Subtract(const aVector: TCHXVec3Type); 
begin
  X -= aVector.X; Y -= aVector.Y; Z -= aVector.Z;
end;

class operator TCHXVec3Type.* (const aVector: TCHXVec3Type; 
  const aScale: TRealType): TCHXVec3Type; 
begin
  Result.X := aVector.X * aScale;
  Result.Y := aVector.Y * aScale;
  Result.Z := aVector.Z * aScale;
end;

class operator TCHXVec3Type.* (const aScale: TRealType; 
  const aVector: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := aScale * aVector.X;
  Result.Y := aScale * aVector.Y;
  Result.Z := aScale * aVector.Z;
end;

procedure TCHXVec3Type.Multiply(const aScale: TRealType); 
begin
  X *= aScale; Y *= aScale; Z *= aScale;
end;

class operator TCHXVec3Type./ (const aVector: TCHXVec3Type; 
  const aScale: TRealType): TCHXVec3Type; 
begin
  Result.X := aVector.X / aScale;
  Result.Y := aVector.Y / aScale;
  Result.Z := aVector.Z / aScale;
end;

procedure TCHXVec3Type.Divide(const aScale: TRealType); 
begin
  X /= aScale; Y /= aScale; Z /= aScale;
end;

class operator TCHXVec3Type.* (const aVec1, aVec2: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := aVec1.X * aVec2.X;
  Result.Y := aVec1.Y * aVec2.Y;
  Result.Z := aVec1.Z * aVec2.Z;
end;

procedure TCHXVec3Type.Scale(const aVector: TCHXVec3Type);  
begin
  X *= aVector.X; Y *= aVector.Y; Z *= aVector.Z;
end;

class operator TCHXVec3Type./ (const aScale: TRealType; 
  const aVector: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := aScale / aVector.X;
  Result.Y := aScale / aVector.Y;
  Result.Z := aScale / aVector.Z;
end;

procedure TCHXVec3Type.DivInv(const aScale: TRealType); 
begin
  X := aScale / X; Y := aScale / Y; Z := aScale / Z;
end;

function TCHXVec3Type.VectProd(const aVector: TCHXVec3Type): TCHXVec3Type; 
begin
  Result.X := (Y * aVector.Z) - (Z * aVector.Y);
  Result.Y := (Z * aVector.X) - (X * aVector.Z);
  Result.Z := (X * aVector.Y) - (Y * aVector.X);
end;

function TCHXVec3Type.ScalProd(const aVector: TCHXVec3Type): TRealType; 
begin
  Result := (X * aVector.X) + (Y * aVector.Y) + (Z * aVector.Z);
end;

procedure TCHXVec3Type.Move(const dX, dY, dZ: TRealType); 
begin
  X += dX; Y += dY; Z += dZ;
end;

procedure TCHXVec3Type.Scale(const sX, sY, sZ: TRealType);  
begin
  X *= sX; Y *= sY; Z *= sZ;
end;

function TCHXVec3Type.SqrDistance(const aPoint: TCHXVec3Type): TRealType; 
begin
  Result := Sqr(X - aPoint.X) + Sqr(Y - aPoint.Y) + Sqr(Z - aPoint.Z);
end;

function TCHXVec3Type.Distance(const aPoint: TCHXVec3Type): TRealType; 
begin
  Result := SqRt(Self.SqrDistance(aPoint));
end;

function TCHXVec3Type.InDistance(const aPoint: TCHXVec3Type; 
  const aDistance: TRealType; const IncEqual: Boolean): Boolean;
var
  aDistSq: TRealType;
begin
  aDistSq := Self.SqrDistance(aPoint);

  // Optimization: Compare squared distances first to avoid SqRt
  if aDistSq < (aDistance * aDistance) then
    Exit(True);
  { 
    Used SqRt(aDistSq) instead aDistance * aDistance to adjust to value
      automatic Sigma of SameValueData.
  }
  Result := IncEqual and SameValueData(SqRt(aDistSq), aDistance);
end;

function TCHXVec3Type.Reflect(const aNormal: TCHXVec3Type): TCHXVec3Type; 
begin
  Result := Self - 2 * (Self.ScalProd(aNormal) * aNormal);
end;

function TCHXVec3Type.Refract(const aNormal: TCHXVec3Type; 
  const RefIdx: TRealType): TCHXVec3Type;
var
  CosI: TRealType;
  k: TRealType;
begin
  CosI := Self.ScalProd(aNormal);
  k := 1 - RefIdx * RefIdx * (1 - CosI * CosI);
  
  if k < 0 then
  begin
    Result.Init3D(0, 0, 0);
  end
  else
  begin
    Result := (Self * RefIdx) - (aNormal * (RefIdx * CosI + SqRt(k)));
  end;
end;

function TCHXVec3Type.ToString(const Delim : char) : string;
begin
  Result := FloatToStr(X) + Delim + FloatToStr(Y) + Delim + FloatToStr(Z);
end;

function TCHXVec3Type.ToStringFmt(const aStr: string) : string;
begin
  Result := Format(aStr, [X,Y,Z]);
end;

function CHXVec3Func(const aX, aY, aZ: TRealType): TCHXVec3Type; 
begin
  Result.X := aX; Result.Y := aY; Result.Z := aZ;
end;
end.
