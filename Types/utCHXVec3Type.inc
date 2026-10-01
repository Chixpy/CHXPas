(*< Unit Template for a record with custom 3 component,
  represent Points, Vector, Colors, etc..

  Usage: Create a unit file, replace this IDs:

  ```pas
    unit <UnitName>;

    {$MACRO ON}
    {$DEFINE TRealType := <FloatDataType> }
    //< Actual type of the components.
    {$DEFINE TCHXVec3Type := <Vec3ID> }
    //< ID for the main type.
    {$DEFINE CHXVec3Func := <Vec3Func> }
    //< Name for the function that returns a new TCHXVec3Type.
    {$DEFINE PCHXVec3 := <PointerID> }
    //< ID of the pointer type to TCHXVec3Type type.

    {$DEFINE TCHXPoint3DType := <TCHXPoint3DFID> }
    //< TCHXPoint3DF type equivalent for retrocompatibility.
    {$DEFINE TCHXColorType := <TCHXColorFID> }
    //< TCHXColorF type equivalent for retrocompatibility.

    {$DEFINE cCHXVec3GenList := <Vec3GenListID> }
    //< ID for generic list specialized to TCHXVec3Type type.
    {$DEFINE cCHXVec3List := <Vec3ListID> }
    //< ID for a generic list descendant.

    {$DEFINE UnitsUsed := , <Units> ; }
    {< Units needed for TRealType, to compare and mathematical functions.
      if no unit is needed, must be ';'.
      Maybe function names below will require <Unit>.<Function> syntax.
    }
    {$DEFINE DataIsZero := <FunctionName> }
    //< function DataIsZero(TRealType) : Boolean;
    {$DEFINE SameValueData := <FunctionName> }
    //< function SameValueData(TRealType, TRealType) : Boolean;
    {$DEFINE SameValueSigma := <FunctionName> }
    //< function SameValueSigma(TRealType, TRealType, TRealType) : Boolean;
    {$DEFINE SinCosData := <FunctionName> }
    //< procedure SinCosData(Angle, out aSin, out aCos);
    {$DEFINE ArcTan2Data := <FunctionName> }
    //< function ArcTan2Data(Y, X) : TRealType;
    {$DEFINE ArcSinData := <FunctionName> }
    //< function ArcSinData(Angle) : TRealType;

    {$DEFINE Proj2DPType := <2DFloatPointType> }
    //< Type of 2D Point (with X and Y fields) for Proj[x] methods' Result.

    {$I 'utCHXVec3Type.inc'}
  ```

  Four types of Vec3 are implemented, for all basic float types:

  - `TCHXVec3S` for Single in uTCHXVec3S.pas
  - `TCHXVec3D` for Double in uTCHXVec3D.pas
  - `TCHXVec3E` for Extended in uTCHXVec3E.pas
  - `TCHXVec3R` for Real in uTCHXVec3R.pas
*)

(*
  This template is a generic data type designed to store 3 components of
    type TRealType, which must be replaced by desired floating-point type via
    a {$DEFINE} macro. The names of the types declared here must also be
    redefined to avoid naming collisions.

  This type aims to replace TCHXPoint3DF and expand its functionality.

  Although primarily intended to store a 3D point or vector, it includes
    several methods for 2D handling and other 3-component structures.

  ToDo:
    - Use "Self." in every field and method?.
    - Maybe change DataIsZero, SameValueData and SameValueSigma with direct
      comparison and define DefaultSigma:
      - So we don't need define this functions. But we will need define
        Abs function for data...
        - DataIsZero: Abs(X) < Sigma.
        - SameValueData: Abs(X - Y) < Sigma.
      - Other way is:
        - DataIsZero: (X < Sigma) and (X > -Sigma).
        - SameValueData: ((X - Y) < Sigma) and ((X - Y) > -Sigma).
    - If not defined SinCosData, ArcTan, ArcSinData then remove that use
      them.
      - Simplify to use only Sin and Cos? Tan = Sin / Cos
*)
{$MODE ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
{$MODESWITCH AdvancedRecords}
{$MODESWITCH TypeHelpers}

interface
uses
  SysUtils, uCHXMath, FGL UnitsUsed

type
  TCHXVec3Type = packed record
  public
    // This goes last, keep here for fast reference
    //case Byte of
    //  0 : (X, Y, Z : TRealType); // Standard components
    //  1 : (R, G, B : TRealType); // Floating-point color [0..1]
    //  2 : (Comp : array[0..2] of TRealType); // Index-based array access
    //end;

  {
    Init
  }

    procedure Init3D(const aX : TRealType = 0; const aY : TRealType = 0;
      const aZ : TRealType = 0); inline;
    procedure InitXY(const aX : TRealType = 0; const aY : TRealType = 0);
      inline;
    procedure InitXZ(const aX : TRealType = 0; const aZ : TRealType = 0);
      inline;
    procedure InitZY(const aZ : TRealType = 0; const aY : TRealType = 0);
      inline;

    procedure InitRandom3D(
      const MinX, MaxX, MinY, MaxY, MinZ, MaxZ : TRealType);
    procedure InitRandomXY(const MinX, MaxX, MinY, MaxY : TRealType);
    procedure InitRandomXZ(const MinX, MaxX, MinZ, MaxZ : TRealType);
    procedure InitRandomZY(const MinZ, MaxZ, MinY, MaxY : TRealType);

    procedure InitPolar3D(const aMag : TRealType; const aYaw : TRealType = 0;
      const aPitch : TRealType = 0);
    procedure InitPolarXY(const aMag : TRealType;
      const aAngle : TRealType = 0); inline;
    procedure InitPolarXZ(const aMag : TRealType;
      const aAngle : TRealType = 0); inline;
    procedure InitPolarZY(const aMag : TRealType;
      const aAngle : TRealType = 0); inline;

    procedure InitRndPolar3D(const aMag : TRealType = 1); inline;
    procedure InitRndPolarXY(const aMag : TRealType = 1); inline;
    procedure InitRndPolarXZ(const aMag : TRealType = 1); inline;
    procedure InitRndPolarZY(const aMag : TRealType = 1); inline;

  {
    Polar coordinates
  }

    procedure GetAngles3D(out aYaw, aPitch : TRealType);
    function GetAngleXY : TRealType; inline;
    function GetAngleXZ : TRealType; inline;
    function GetAngleZY : TRealType; inline;
    procedure SetAngles3D(const aYaw, aPitch : TRealType); inline;
    procedure SetAngleXY(const aAngle : TRealType); inline;
    procedure SetAngleXZ(const aAngle : TRealType); inline;
    procedure SetAngleZY(const aAngle : TRealType); inline;

    function GetSqrMag3D : TRealType; inline;
    function GetSqrMagXY : TRealType; inline;
    function GetSqrMagXZ : TRealType; inline;
    function GetSqrMagZY : TRealType; inline;
    function GetMagnitude3D : TRealType; inline;
    function GetMagnitudeXY : TRealType; inline;
    function GetMagnitudeXZ : TRealType; inline;
    function GetMagnitudeZY : TRealType; inline;
    procedure SetMagnitude(const aMag : TRealType); inline;
    // ToDo: SetMagnitudeXY, SetMagnitudeXZ and SetMagnitudeZY

    procedure Normalize;
    function GetNormalized : TCHXVec3Type; inline;

  {
    Comparisons

    Sigma = 0 means use default sigma value for TRealType.
  }

    function IsZero3D(const Sigma : TRealType = 0) : Boolean; inline;
    function IsZeroXY(const Sigma : TRealType = 0) : Boolean; inline;
    function IsZeroXZ(const Sigma : TRealType = 0) : Boolean; inline;
    function IsZeroZY(const Sigma : TRealType = 0) : Boolean; inline;

    function IsEqual3D(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    function IsEqualXZ(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    function IsEqualZY(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    function IsEqualXY(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;

    function IsOpposite(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    function IsParallel(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    //< Ignore direction.
    function IsCoaligned(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    //< Parallel with same direction.
    function IsAntiParallel(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;
    //< Parallel with opposite direction.
    function IsPerpendicular(const aVector : TCHXVec3Type;
      const Sigma : TRealType = 0) : Boolean; inline;

  {
    Self operations
  }

    procedure Negate;
    procedure Add(const aVector : TCHXVec3Type); inline;
    procedure Subtract(const aVector : TCHXVec3Type); inline;
    procedure Multiply(const aScale : TRealType); inline;
    procedure Scale(const aVector : TCHXVec3Type); inline; overload;
    procedure Divide(const aScale : TRealType); inline;
    procedure DivInv(const aScale : TRealType); inline;

  {
    Other operations
  }

    procedure Move(const dX, dY, dZ : TRealType); inline;
    procedure Scale(const sX, sY, sZ : TRealType); inline; overload;

    procedure RotateXY(const aAngle : TRealType); overload;
    procedure RotateXZ(const aAngle : TRealType); overload;
    procedure RotateZY(const aAngle : TRealType); overload;
    procedure RotateXY(const aAngle : TRealType; const Center : TCHXVec3Type);
      overload;
    procedure RotateXZ(const aAngle : TRealType; const Center : TCHXVec3Type);
      overload;
    procedure RotateZY(const aAngle : TRealType; const Center : TCHXVec3Type);
      overload;

    function VectProd(const aVector : TCHXVec3Type) : TCHXVec3Type; inline;
    function ScalProd(const aVector : TCHXVec3Type) : TRealType; inline;

    function SqrDistance3D(const aPoint : TCHXVec3Type) : TRealType; inline;
    function SqrDistanceXY(const aPoint : TCHXVec3Type) : TRealType; inline;
    function SqrDistanceXZ(const aPoint : TCHXVec3Type) : TRealType; inline;
    function SqrDistanceZY(const aPoint : TCHXVec3Type) : TRealType; inline;
    function Distance3D(const aPoint : TCHXVec3Type) : TRealType; inline;
    function DistanceXY(const aPoint : TCHXVec3Type) : TRealType; inline;
    function DistanceXZ(const aPoint : TCHXVec3Type) : TRealType; inline;
    function DistanceZY(const aPoint : TCHXVec3Type) : TRealType; inline;
    function InDistance3D(const aPoint : TCHXVec3Type;
      const aDistance : TRealType; const IncEqual : Boolean = False) : Boolean;
    function InDistanceXY(const aPoint : TCHXVec3Type;
      const aDistance : TRealType; const IncEqual : Boolean = False) : Boolean;
    function InDistanceXZ(const aPoint : TCHXVec3Type;
      const aDistance : TRealType; const IncEqual : Boolean = False) : Boolean;
    function InDistanceZY(const aPoint : TCHXVec3Type;
      const aDistance : TRealType; const IncEqual : Boolean = False) : Boolean;
    {<
      If IncEqual = False, strictly less than.
      If True, it includes the value + Precision.
      ToDo: Versión with Sigma
    }
    function MidPoint(const aPoint : TCHXVec3Type) : TCHXVec3Type; inline;

    function Reflect(const aNormal : TCHXVec3Type) : TCHXVec3Type; inline;
    function Refract(const aNormal : TCHXVec3Type; const RefIdx : TRealType)
      : TCHXVec3Type;

  {
    3D Projections
  }

    function ProjFront(const aScale : TRealType = 1) : Proj2DPType; inline;
    function ProjBack(const aScale : TRealType = 1) : Proj2DPType; inline;
    function ProjTop(const aScale : TRealType = 1) : Proj2DPType; inline;
    function ProjBottom(const aScale : TRealType = 1) : Proj2DPType; inline;
    function ProjLeft(const aScale : TRealType = 1) : Proj2DPType; inline;
    function ProjRight(const aScale : TRealType = 1) : Proj2DPType; inline;
    {<< Get the Front, Back, Top, Bottom, Left or Right view.

      @param(aScale Scale for the projection.)
      @return(2D point with the projection.)
    }

    function ProjIsometric(const aScale : TRealType = 1) : Proj2DPType;
    {< Get the Isometric Projection (Filmation).

      @param(aScale Scale for the projection.)
      @return(2D point with the projection.)
    }
    function ProjDimetric(const AngleXZ : TRealType;
      const aScale : TRealType = 1; const ScaleXZ : TRealType = 1)
      : Proj2DPType;
    {< Get the Dimetric Projection.

      Vertical axis (Y) is vertical in projection.

      @param(AngleXZ Angle in Radians for X and Z from horizontal.)
      @param(aScale Global scale for the projection, and Y unit length.)
      @param(ScaleXZ Scale reduction for X and Z (final unit length is
        `aScale * ScaleXZ`).)
      @return(2D point with the projection.)
    }
    function ProjTrimetric(
      const AngleX, AngleZ : TRealType; const ScaleX : TRealType = 1;
      const ScaleY : TRealType = 1; const ScaleZ : TRealType = 1)
      : Proj2DPType;
    {< Get the Trimetric Projection.

      Vertical axis (Y) is vertical in projection.

      @param(AngleX Angle in Radians for X axis from horizontal.)
      @param(AngleZ Angle in Radians for Z axis from horizontal.)
      @param(ScaleX Scale for X (Absolute unit size).)
      @param(ScaleY Scale for Y (Absolute unit size).)
      @param(ScaleZ Scale for Z (Absolute unit size).)
      @return(2D point with the projection.)
    }

    function ProjCavalier(const aScale : TRealType = 1;
      const ScaleZ : TRealType = 0.707106781) : Proj2DPType;
    {< Get the Cavalier Projection.

      Vertical axis (Y) is vertical in projection.

      @param(aScale Global scale for the projection, and X and Y unit length.)
      @param(ScaleY Scale reduction for Z (final unit length is
        `aScale * ScaleZ`).)
      @return(2D point with the projection.)
    }
    function ProjMilitary(const aScale : TRealType = 1;
      const ScaleY : TRealType = 0.5) : Proj2DPType;
    {< Get the Military Projection.

      Vertical axis (Y) is vertical in projection.

      @param(aScale Global scale for the projection, and X and Z unit length.)
      @param(ScaleY Scale reduction for Y (final unit length is
        `aScale * ScaleY`).)
      @return(2D point with the projection.)
    }

    function Proj3D(var ProjPoint : Proj2DPType; const WinHeight : TRealType)
      : Boolean;
    {< Simple 3D projection of the point.

      Uses a vertical FOV of ~53º wich is OK for usual use, and faster than
      other configurations.

      For use a specific FOV or Focal Length use `Proj3DFOV` or `Proj3DFL`.

      @param(ProjPoint Projected 2D Point returned.)
      @param(WinHeight Height in pixels of Screen, Window, Viewport.)
      @return(@false if proyected point is infinite (Z = 0),
        @true if a valid point.)
    }

  {
    Conversion to Integer coords and remainder

    ToDo: ¿Use TPoint or TCHXVec2?
  }

    function Ceil : TCHXVec3Type; //< Ceiling(-2.3, 1.3) = -2, 2
    function Truncate : TCHXVec3Type; //< Truncate(-2.3, 1.3) = -2, 1
    function Floor : TCHXVec3Type; //< Floor(-2.3, 1.3) = -3, 1
    function Round : TCHXVec3Type;
    function FracCeil : TCHXVec3Type; //< FracCeil(-2.3, 1.3) = -0.3, -0.7
    function FracTrunc : TCHXVec3Type;  //< Frac(-2.3, 1.3) = -0.3, 0.3
    function FracFloor : TCHXVec3Type; //< FracFloor(-2.3, 1.3) = 0.7, 0.3

  {
    Operators
  }

    class operator = (const aVec1, aVec2 : TCHXVec3Type) : Boolean; inline;
    class operator + (const aVec1, aVec2 : TCHXVec3Type) : TCHXVec3Type;
      inline;
    class operator - (const aVector : TCHXVec3Type) : TCHXVec3Type; inline;
    class operator - (const aVec1, aVec2 : TCHXVec3Type) : TCHXVec3Type;
      inline;
    class operator * (const aVec1, aVec2 : TCHXVec3Type) : TCHXVec3Type;
      inline;
    //< Component-wise scaling (Hadamard product)
    class operator * (const aVector : TCHXVec3Type; const aScale : TRealType)
      : TCHXVec3Type; inline;
    class operator * (const aScale : TRealType; const aVector : TCHXVec3Type)
      : TCHXVec3Type; inline;
    class operator / (const aVector : TCHXVec3Type; const aScale : TRealType)
      : TCHXVec3Type; inline;
    class operator / (const aScale : TRealType; const aVector : TCHXVec3Type)
      : TCHXVec3Type; inline;

  {
    String related
  }

    // ToDo: procedure FromString(const Delim : char = ',');
    function ToString(const Delim : char = ',') : String;
    function ToStringFixed(const Decimals : Integer = 3;
      const Delim : char = ',') : String;
    function ToStringFmt(const aStr : String) : String;

    // P.S.: I never liked this "union" syntax, and the case without "end;"
    case Byte of
      0 : (X, Y, Z : TRealType); // Standard components
      1 : (R, G, B : TRealType); // Floating-point color [0..1]
      2 : (Comp : array[0..2] of TRealType;); // Index-based array access
  end;

  // Aliases for retrocompatibility and clarity
  TCHXPoint3DType = TCHXVec3Type;
  TCHXColorType   = TCHXVec3Type;

  PCHXVec3 = ^TCHXVec3Type;

  // Generic and specialized lists using FGL (Free Pascal Generics Library)
  // ToDo: Change to Generic.Collections?
  cCHXVec3GenList = specialize TFPGList<TCHXVec3Type>;
  cCHXVec3List = class(cCHXVec3GenList)
  {<
    This list implement methods that apply operations to all
    elements, sometimes more efficiently. For example, applying rotations
    without having to recalculate the same sines and cosines multiple times.
  }
    procedure Init3DAll(const aX : TRealType = 0; const aY : TRealType = 0;
      const aZ : TRealType = 0);
    procedure InitXYAll(const aX : TRealType = 0; const aY : TRealType = 0);
    procedure InitXZAll(const aX : TRealType = 0; const aZ : TRealType = 0);
    procedure InitZYAll(const aZ : TRealType = 0; const aY : TRealType = 0);

    procedure InitRandom3DAll(
      const MinX, MaxX, MinY, MaxY, MinZ, MaxZ : TRealType);
    procedure InitRandomXYAll(const MinX, MaxX, MinY, MaxY : TRealType);
    procedure InitRandomXZAll(const MinX, MaxX, MinZ, MaxZ : TRealType);
    procedure InitRandomZYAll(const MinZ, MaxZ, MinY, MaxY : TRealType);

    procedure InitPolar3DAll(const aMag : TRealType;
      const aYaw : TRealType = 0; const aPitch : TRealType = 0);
    procedure InitPolarXYAll(const aMag : TRealType;
      const aAngle : TRealType = 0);
    procedure InitPolarXZAll(const aMag : TRealType;
      const aAngle : TRealType = 0);
    procedure InitPolarZYAll(const aMag : TRealType;
      const aAngle : TRealType = 0);

    procedure InitRndPolar3DAll(const aMag : TRealType = 1);
    procedure InitRndPolarXYAll(const aMag : TRealType = 1);
    procedure InitRndPolarXZAll(const aMag : TRealType = 1);
    procedure InitRndPolarZYAll(const aMag : TRealType = 1);

    procedure SetAngles3DAll(const aYaw, aPitch : TRealType);
    procedure SetAngleXYAll(const aAngle : TRealType);
    procedure SetAngleXZAll(const aAngle : TRealType);
    procedure SetAngleZYAll(const aAngle : TRealType);

    procedure SetMagnitudeAll(const aMag : TRealType);
    // ToDo: SetMagnitudeXY, SetMagnitudeXZ and SetMagnitudeZY

    procedure NormalizeAll;
    // ToDo: function GetNormalized : cCHXVec3List;

    procedure NegateAll;
    procedure AddAll(const aVector : TCHXVec3Type);
    procedure SubtractAll(const aVector : TCHXVec3Type);
    procedure MultiplyAll(const aScale : TRealType);
    procedure ScaleAll(const aVector : TCHXVec3Type); overload;
    procedure DivideAll(const aScale : TRealType);
    procedure DivInvAll(const aScale : TRealType);

    procedure MoveAll(const dX, dY, dZ : TRealType);
    procedure ScaleAll(const sX, sY, sZ : TRealType); overload;

    procedure RotateXYAll(const aAngle : TRealType); overload;
    procedure RotateXZAll(const aAngle : TRealType); overload;
    procedure RotateZYAll(const aAngle : TRealType); overload;
    procedure RotateXYAll(const aAngle : TRealType;
      const Center : TCHXVec3Type); overload;
    procedure RotateXZAll(const aAngle : TRealType;
      const Center : TCHXVec3Type); overload;
    procedure RotateZYAll(const aAngle : TRealType;
      const Center : TCHXVec3Type); overload;

    function GetSqrDistance3D(const Cyclic : Boolean = False) : TRealType;
    function GetSqrDistanceXY(const Cyclic : Boolean = False) : TRealType;
    function GetSqrDistanceXZ(const Cyclic : Boolean = False) : TRealType;
    function GetSqrDistanceZY(const Cyclic : Boolean = False) : TRealType;
    function GetDistance3D(const Cyclic : Boolean = False) : TRealType;
    function GetDistanceXY(const Cyclic : Boolean = False) : TRealType;
    function GetDistanceXZ(const Cyclic : Boolean = False) : TRealType;
    function GetDistanceZY(const Cyclic : Boolean = False) : TRealType;

    function MidPointAll : TCHXVec3Type;

    { ToDo: procedure FromString(const ListBegin : char = '['; const ListEnd : char = ']';
          const PointBegin : char = '('; const PointEnd : char = ')';
          const Delim : char = ',');
    }
    // ToDo: inherited ToString
    function WriteString(const ListBegin : char = '['; const ListEnd : char = ']';
      const PointBegin : char = '('; const PointEnd : char = ')';
      const Delim : char = ',') : String;
  end;

{
  Creation function
}

function CHXVec3Func(const aX : TRealType = 0; const aY : TRealType = 0;
  const aZ : TRealType = 0) : TCHXVec3Type; inline;
{<
  Global function to 'create' a TCHXVec3Type.

  Useful to pass as a parameter without the need to explicitly
  create a variable: MyFunc(CHXVec3Func(1, 2, 3), ...)
}

implementation

{ TCHXVec3Type }

procedure TCHXVec3Type.Init3D(const aX, aY, aZ : TRealType);
begin
  Self.X := aX; Self.Y := aY; Self.Z := aZ;
end;

procedure TCHXVec3Type.InitXY(const aX, aY : TRealType);
begin
  Self.X := aX; Self.Y := aY; Self.Z := 0;
end;

procedure TCHXVec3Type.InitXZ(const aX, aZ : TRealType);
begin
  Self.X := aX; Self.Y := 0; Self.Z := aZ;
end;

procedure TCHXVec3Type.InitZY(const aZ, aY : TRealType);
begin
  Self.X := 0; Self.Y := aY; Self.Z := aZ;
end;

procedure TCHXVec3Type.InitRandom3D(
  const MinX, MaxX, MinY, MaxY, MinZ, MaxZ : TRealType);
begin
  Self.X := Random * (MaxX - MinX) + MinX;
  Self.Y := Random * (MaxY - MinY) + MinY;
  Self.Z := Random * (MaxZ - MinZ) + MinZ;
end;

procedure TCHXVec3Type.InitRandomXY(const MinX, MaxX, MinY, MaxY : TRealType);
begin
  Self.X := Random * (MaxX - MinX) + MinX;
  Self.Y := Random * (MaxY - MinY) + MinY;
  Self.Z := 0;
end;

procedure TCHXVec3Type.InitRandomXZ(const MinX, MaxX, MinZ, MaxZ : TRealType);
begin
  Self.X := Random * (MaxX - MinX) + MinX;
  Self.Y := 0;
  Self.Z := Random * (MaxZ - MinZ) + MinZ;
end;

procedure TCHXVec3Type.InitRandomZY(const MinZ, MaxZ, MinY, MaxY : TRealType);
begin
  Self.X := 0;
  Self.Y := Random * (MaxY - MinY) + MinY;
  Self.Z := Random * (MaxZ - MinZ) + MinZ;
end;

procedure TCHXVec3Type.InitPolar3D(const aMag, aYaw, aPitch : TRealType);
var
  SinXZ, CosXZ, SinY, CosY : TRealType;
  ProyXZ : TRealType;
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

  Self.Y := aMag * SinY;
  ProyXZ := aMag * CosY;
  Self.X := ProyXZ * SinXZ;
  Self.Z := ProyXZ * CosXZ;
end;

procedure TCHXVec3Type.InitPolarXY(const aMag, aAngle : TRealType);
begin
  Self.X := aMag * Cos(aAngle); Self.Y := aMag * Sin(aAngle); Self.Z := 0;
end;

procedure TCHXVec3Type.InitPolarXZ(const aMag, aAngle : TRealType);
begin
  Self.X := aMag * Cos(aAngle); Self.Y := 0; Self.Z := aMag * Sin(aAngle);
end;

procedure TCHXVec3Type.InitPolarZY(const aMag, aAngle : TRealType);
begin
  Self.X := 0; Self.Y := aMag * Sin(aAngle); Self.Z := aMag * Cos(aAngle);
end;

procedure TCHXVec3Type.InitRndPolar3D(const aMag : TRealType);
begin
  // FixMe: It can never be completely vertical upwards (0, 1, 0),
  //   but it can be (0, -1, 0)
  Self.InitPolar3D(aMag, Random * k2Pi, Random * Pi - kHalfPi);
end;

procedure TCHXVec3Type.InitRndPolarXY(const aMag : TRealType);
begin
  Self.InitPolarXY(aMag, Random * k2Pi);
end;

procedure TCHXVec3Type.InitRndPolarXZ(const aMag : TRealType);
begin
  Self.InitPolarXZ(aMag, Random * k2Pi);
end;

procedure TCHXVec3Type.InitRndPolarZY(const aMag : TRealType);
begin
  Self.InitPolarZY(aMag, Random * k2Pi);
end;

procedure TCHXVec3Type.GetAngles3D(out aYaw, aPitch : TRealType);
begin
  if Self.IsZero3D then // Null Vector
  begin
    aYaw := 0; aPitch := 0;
    Exit;
  end;

  aYaw := ArcTan2Data(Self.X, Self.Z);
  aPitch := ArcSinData(Self.Y / Self.GetMagnitude3D);
end;

function TCHXVec3Type.GetAngleXY : TRealType;
begin
  Result := ArcTan2Data(Self.Y, Self.X);
end;

function TCHXVec3Type.GetAngleXZ : TRealType;
begin
  Result := ArcTan2Data(Self.X, Self.Z); // 0° at (0, 0, 1)
end;

function TCHXVec3Type.GetAngleZY : TRealType;
begin
  Result := ArcTan2Data(Self.Y, Self.Z);
end;

procedure TCHXVec3Type.SetAngles3D(const aYaw, aPitch : TRealType);
begin
  Self.InitPolar3D(Self.GetMagnitude3D, aYaw, aPitch);
end;

procedure TCHXVec3Type.SetAngleXY(const aAngle : TRealType);
begin
  // ToDo: Test wich is more efficient and apply to XZ and ZY methods:
  //   aMag := GetMagnitudeXY;
  //   Self.InitPolarXY(aMag, aAngle);
  // or:
  Self.RotateXY(aAngle - Self.GetAngleXY);
end;

procedure TCHXVec3Type.SetAngleXZ(const aAngle : TRealType);
begin
  Self.RotateXZ(aAngle - Self.GetAngleXZ);
end;

procedure TCHXVec3Type.SetAngleZY(const aAngle : TRealType);
begin
  Self.RotateZY(aAngle - Self.GetAngleZY);
end;

function TCHXVec3Type.GetSqrMag3D : TRealType;
begin
  Result := Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z;
end;

function TCHXVec3Type.GetSqrMagXY : TRealType;
begin
  Result := Self.X * Self.X + Self.Y * Self.Y;
end;

function TCHXVec3Type.GetSqrMagXZ : TRealType;
begin
  Result := Self.X * Self.X + Self.Z * Self.Z;
end;

function TCHXVec3Type.GetSqrMagZY : TRealType;
begin
  Result := Self.Z * Self.Z + Self.Y * Self.Y;
end;

function TCHXVec3Type.GetMagnitude3D : TRealType;
begin
  Result := SqRt(Self.GetSqrMag3D);
end;

function TCHXVec3Type.GetMagnitudeXY : TRealType;
begin
  Result := SqRt(Self.GetSqrMagXY);
end;

function TCHXVec3Type.GetMagnitudeXZ : TRealType;
begin
  Result := SqRt(Self.GetSqrMagXZ);
end;

function TCHXVec3Type.GetMagnitudeZY : TRealType;
begin
  Result := SqRt(Self.GetSqrMagZY);
end;

procedure TCHXVec3Type.SetMagnitude(const aMag : TRealType);
begin
  if Self.IsZero3D then
    Exit; // Keep at zero

  // This way we only divide once
  Self.Multiply(aMag / Self.GetMagnitude3D);
end;

procedure TCHXVec3Type.Normalize;
begin
  if Self.IsZero3D then
    Exit; // Keep at Zero

  // This way we only divide once
  Self.Multiply(1 / Self.GetMagnitude3D);
end;

function TCHXVec3Type.GetNormalized : TCHXVec3Type;
begin
  Result := Self;
  Result.Normalize;
end;

function TCHXVec3Type.IsZero3D(const Sigma : TRealType) : Boolean;
begin
  Result := DataIsZero(Self.X, Sigma) and DataIsZero(Self.Y, Sigma)
    and DataIsZero(Self.Z, Sigma);
end;

function TCHXVec3Type.IsZeroXY(const Sigma : TRealType) : Boolean;
begin
  Result := DataIsZero(Self.X, Sigma) and DataIsZero(Self.Y, Sigma);
end;

function TCHXVec3Type.IsZeroXZ(const Sigma : TRealType) : Boolean;
begin
  Result := DataIsZero(Self.X, Sigma) and DataIsZero(Self.Z, Sigma);
end;

function TCHXVec3Type.IsZeroZY(const Sigma : TRealType) : Boolean;
begin
  Result := DataIsZero(Self.Z, Sigma) and DataIsZero(Self.Y, Sigma);
end;

function TCHXVec3Type.IsEqual3D(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := SameValueSigma(Self.X, aVector.X, Sigma)
    and SameValueSigma(Self.Y, aVector.Y, Sigma)
    and SameValueSigma(Self.Z, aVector.Z, Sigma);
end;

function TCHXVec3Type.IsEqualXY(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := SameValueSigma(Self.X, aVector.X, Sigma)
    and SameValueSigma(Self.Y, aVector.Y, Sigma);
end;

function TCHXVec3Type.IsEqualXZ(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := SameValueSigma(Self.X, aVector.X, Sigma)
    and SameValueSigma(Self.Z, aVector.Z, Sigma);
end;

function TCHXVec3Type.IsEqualZY(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := SameValueSigma(Self.Z, aVector.Z, Sigma)
    and SameValueSigma(Self.Y, aVector.Y, Sigma);
end;

function TCHXVec3Type.IsOpposite(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := SameValue(Self.X, -aVector.X, Sigma)
    and SameValue(Self.Y, -aVector.Y, Sigma)
    and SameValue(Self.Z, -aVector.Z, Sigma);
end;

function TCHXVec3Type.IsParallel(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := Self.VectProd(aVector).IsZero3D(Sigma);
end;

function TCHXVec3Type.IsCoaligned(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := (Self.ScalProd(aVector) > 0)
    and Self.IsParallel(aVector, Sigma);
end;

function TCHXVec3Type.IsAntiParallel(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := (Self.ScalProd(aVector) < 0)
    and Self.IsParallel(aVector, Sigma);
end;

function TCHXVec3Type.IsPerpendicular(const aVector : TCHXVec3Type;
  const Sigma : TRealType) : Boolean;
begin
  Result := DataIsZero(Self.ScalProd(aVector), Sigma);
end;

procedure TCHXVec3Type.Negate;
begin
  Self.X := -Self.X; Self.Y := -Self.Y; Self.Z := -Self.Z;
end;

procedure TCHXVec3Type.Add(const aVector : TCHXVec3Type);
begin
  Self.X += aVector.X; Self.Y += aVector.Y; Self.Z += aVector.Z;
end;

procedure TCHXVec3Type.Subtract(const aVector : TCHXVec3Type);
begin
  Self.X -= aVector.X; Self.Y -= aVector.Y; Self.Z -= aVector.Z;
end;

procedure TCHXVec3Type.Multiply(const aScale : TRealType);
begin
  Self.X *= aScale; Self.Y *= aScale; Self.Z *= aScale;
end;

procedure TCHXVec3Type.Scale(const aVector : TCHXVec3Type);
begin
  Self.X *= aVector.X; Self.Y *= aVector.Y; Self.Z *= aVector.Z;
end;

procedure TCHXVec3Type.Divide(const aScale : TRealType);
var
  Inv : TRealType;
begin
  Inv := 1 / aScale;
  Self.X *= Inv; Self.Y *= Inv; Self.Z *= Inv;
end;

procedure TCHXVec3Type.DivInv(const aScale : TRealType);
begin
  Self.X := aScale / Self.X;
  Self.Y := aScale / Self.Y;
  Self.Z := aScale / Self.Z;
end;

procedure TCHXVec3Type.Move(const dX, dY, dZ : TRealType);
begin
  Self.X += dX; Self.Y += dY; Self.Z += dZ;
end;

procedure TCHXVec3Type.Scale(const sX, sY, sZ : TRealType);
begin
  Self.X *= sX; Self.Y *= sY; Self.Z *= sZ;
end;

procedure TCHXVec3Type.RotateXY(const aAngle : TRealType);
var
  SinXY, CosXY : TRealType;
  TempX : TRealType;
begin
  SinCosData(aAngle, SinXY, CosXY);
  TempX := Self.X;
  Self.X := TempX * CosXY - Self.Y * SinXY;
  Self.Y := TempX * SinXY + Self.Y * CosXY;
end;

procedure TCHXVec3Type.RotateXZ(const aAngle : TRealType);
var
  SinXZ, CosXZ : TRealType;
  TempX : TRealType;
begin
  SinCosData(aAngle, SinXZ, CosXZ);
  TempX := Self.X;
  Self.X := TempX * CosXZ - Self.Z * SinXZ;
  Self.Z := TempX * SinXZ + Self.Z * CosXZ;
end;

procedure TCHXVec3Type.RotateZY(const aAngle : TRealType);
var
  SinZY, CosZY : TRealType;
  TempZ : TRealType;
begin
  SinCosData(aAngle, SinZY, CosZY);
  TempZ := Self.Z;
  Self.Z := TempZ * CosZY - Self.Y * SinZY;
  Self.Y := TempZ * SinZY + Self.Y * CosZY;
end;

procedure TCHXVec3Type.RotateXY(const aAngle : TRealType;
  const Center : TCHXVec3Type);
begin
  Self.Subtract(Center);
  Self.RotateXY(aAngle);
  Self.Add(Center);
end;

procedure TCHXVec3Type.RotateXZ(const aAngle : TRealType;
  const Center : TCHXVec3Type);
begin
  Self.Subtract(Center);
  Self.RotateXZ(aAngle);
  Self.Add(Center);
end;

procedure TCHXVec3Type.RotateZY(const aAngle : TRealType;
  const Center : TCHXVec3Type);
begin
  Self.Subtract(Center);
  Self.RotateZY(aAngle);
  Self.Add(Center);
end;

function TCHXVec3Type.VectProd(const aVector : TCHXVec3Type) : TCHXVec3Type;
begin
  Result.X := (Self.Y * aVector.Z) - (Self.Z * aVector.Y);
  Result.Y := (Self.Z * aVector.X) - (Self.X * aVector.Z);
  Result.Z := (Self.X * aVector.Y) - (Self.Y * aVector.X);
end;

function TCHXVec3Type.ScalProd(const aVector : TCHXVec3Type) : TRealType;
begin
  Result := (Self.X * aVector.X) + (Self.Y * aVector.Y) + (Self.Z * aVector.Z);
end;

function TCHXVec3Type.SqrDistance3D(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := Sqr(Self.X - aPoint.X) + Sqr(Self.Y - aPoint.Y)
    + Sqr(Self.Z - aPoint.Z);
end;

function TCHXVec3Type.SqrDistanceXY(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := Sqr(Self.X - aPoint.X) + Sqr(Self.Y - aPoint.Y);
end;

function TCHXVec3Type.SqrDistanceXZ(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := Sqr(Self.X - aPoint.X) + Sqr(Self.Z - aPoint.Z);
end;

function TCHXVec3Type.SqrDistanceZY(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := Sqr(Self.Z - aPoint.Z) + Sqr(Self.Y - aPoint.Y);
end;

function TCHXVec3Type.Distance3D(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := SqRt(Self.SqrDistance3D(aPoint));
end;

function TCHXVec3Type.DistanceXY(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := SqRt(Self.SqrDistanceXY(aPoint));
end;

function TCHXVec3Type.DistanceXZ(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := SqRt(Self.SqrDistanceXZ(aPoint));
end;

function TCHXVec3Type.DistanceZY(const aPoint : TCHXVec3Type) : TRealType;
begin
  Result := SqRt(Self.SqrDistanceZY(aPoint));
end;

function TCHXVec3Type.InDistance3D(const aPoint : TCHXVec3Type;
  const aDistance : TRealType; const IncEqual : Boolean) : Boolean;
var
  aDistSq : TRealType;
begin
  aDistSq := Self.SqrDistance3D(aPoint);

  // Optimization: Compare squared distances first to avoid SqRt
  if aDistSq < (aDistance * aDistance) then
    Exit(True);
  {
    Used SqRt(aDistSq) instead aDistance * aDistance to adjust to value
      automatic Sigma of SameValueData.
  }
  Result := IncEqual and SameValueData(SqRt(aDistSq), aDistance);
end;

function TCHXVec3Type.InDistanceXY(const aPoint : TCHXVec3Type;
  const aDistance : TRealType; const IncEqual : Boolean) : Boolean;
var
  aDistSq : TRealType;
begin
  aDistSq := Self.SqrDistanceXY(aPoint);

  // Optimization: Compare squared distances first to avoid SqRt
  if aDistSq < (aDistance * aDistance) then
    Exit(True);
  {
    Used SqRt(aDistSq) instead aDistance * aDistance to adjust to value
      automatic Sigma of SameValueData.
  }
  Result := IncEqual and SameValueData(SqRt(aDistSq), aDistance);
end;

function TCHXVec3Type.InDistanceXZ(const aPoint : TCHXVec3Type;
  const aDistance : TRealType; const IncEqual : Boolean) : Boolean;
var
  aDistSq : TRealType;
begin
  aDistSq := Self.SqrDistanceXZ(aPoint);

  // Optimization: Compare squared distances first to avoid SqRt
  if aDistSq < (aDistance * aDistance) then
    Exit(True);
  {
    Used SqRt(aDistSq) instead aDistance * aDistance to adjust to value
      automatic Sigma of SameValueData.
  }
  Result := IncEqual and SameValueData(SqRt(aDistSq), aDistance);
end;

function TCHXVec3Type.InDistanceZY(const aPoint : TCHXVec3Type;
  const aDistance : TRealType; const IncEqual : Boolean) : Boolean;
var
  aDistSq : TRealType;
begin
  aDistSq := Self.SqrDistanceZY(aPoint);

  // Optimization: Compare squared distances first to avoid SqRt
  if aDistSq < (aDistance * aDistance) then
    Exit(True);
  {
    Used SqRt(aDistSq) instead aDistance * aDistance to adjust to value
      automatic Sigma of SameValueData.
  }
  Result := IncEqual and SameValueData(SqRt(aDistSq), aDistance);
end;

function TCHXVec3Type.MidPoint(const aPoint : TCHXVec3Type) : TCHXVec3Type;
begin
  Result.X := (Self.X + aPoint.X) * 0.5;
  Result.Y := (Self.Y + aPoint.Y) * 0.5;
  Result.Z := (Self.Y + aPoint.Z) * 0.5;
end;

function TCHXVec3Type.Reflect(const aNormal : TCHXVec3Type) : TCHXVec3Type;
begin
  Result := Self - 2 * (Self.ScalProd(aNormal) * aNormal);
end;

function TCHXVec3Type.Refract(const aNormal : TCHXVec3Type;
  const RefIdx : TRealType) : TCHXVec3Type;
var
  CosI : TRealType;
  k : TRealType;
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

{
  3D Projections
}

function TCHXVec3Type.ProjFront(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := Self.X * aScale; Result.Y := Self.Y * aScale;
end;

function TCHXVec3Type.ProjBack(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := -Self.X * aScale; Result.Y := Self.Y * aScale;
end;

function TCHXVec3Type.ProjTop(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := Self.X * aScale; Result.Y := -Self.Z * aScale;
end;

function TCHXVec3Type.ProjBottom(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := Self.X * aScale; Result.Y := Self.Z * aScale;
end;

function TCHXVec3Type.ProjLeft(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := Self.Z * aScale; Result.Y := Self.Y * aScale;
end;

function TCHXVec3Type.ProjRight(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := -Self.Z * aScale; Result.Y := Self.Y * aScale;
end;

function TCHXVec3Type.ProjIsometric(const aScale : TRealType) : Proj2DPType;
begin
  Result.X := aScale * ((Self.X - Self.Z) * 0.8660254); // Cos(30º)
  Result.Y := aScale * (Self.Y + (Self.X + Self.Z) * 0.5); // Sin(30º)
end;

function TCHXVec3Type.ProjDimetric(const AngleXZ, aScale, ScaleXZ : TRealType)
  : Proj2DPType;
var
  Temp : TRealType;
begin
  Temp := (Self.X - Self.Z) * aScale * ScaleXZ;
  Result.X := Temp * Cos(AngleXZ);
  Result.Y := Self.Y * aScale - Temp * Sin(AngleXZ);
end;

function TCHXVec3Type.ProjTrimetric(
  const AngleX, AngleZ, ScaleX, ScaleY, ScaleZ : TRealType)
  : Proj2DPType;
begin
  Result.X := Self.X * ScaleX * Cos(AngleX) + Self.Z * ScaleZ * Cos(AngleZ);
  Result.Y := Self.X * ScaleX * Sin(AngleX) + Self.Z * ScaleZ * Sin(AngleZ)
    + Self.Y * ScaleY;
end;

function TCHXVec3Type.ProjCavalier(const aScale, ScaleZ : TRealType)
  : Proj2DPType;
var
  Temp : TRealType;
begin
  Temp := Self.Z * ScaleZ; // Sin(45º) = Cos(45º) = 0.707106781
  Result.X := (Self.X + Temp) * aScale;
  Result.Y := (Self.Y + Temp) * aScale;
end;

function TCHXVec3Type.ProjMilitary(const aScale, ScaleY : TRealType)
  : Proj2DPType;
var
  Temp : TRealType;
begin
  Temp := aScale * 0.707106781; // Sin(45º) = Cos(45º)
  Result.X := X * Temp + Z * Temp;
  Result.Y := Y * ScaleY + Result.X;
end;

function TCHXVec3Type.Proj3D(var ProjPoint : Proj2DPType;
  const WinHeight : TRealType) : Boolean;
var
  Temp : TRealType;
begin
  Result := not DataIsZero(Self.Z);
  if Result then
  begin
    Temp := WinHeight / Self.Z;
    ProjPoint.X := Self.X * Temp;
    ProjPoint.Y := Self.Y * Temp;
  end;
end;

{
  Conversion to Integer coords and remainder
}

function TCHXVec3Type.Ceil : TCHXVec3Type;
begin
  Result.X := Math.Ceil(Self.X);
  Result.Y := Math.Ceil(Self.Y);
  Result.Z := Math.Ceil(Self.Z);
end;

function TCHXVec3Type.Truncate : TCHXVec3Type;
begin
  Result.X := Trunc(Self.X);
  Result.Y := Trunc(Self.Y);
  Result.Z := Trunc(Self.Z);
end;

function TCHXVec3Type.Floor : TCHXVec3Type;
begin
  Result.X := Math.Floor(Self.X);
  Result.Y := Math.Floor(Self.Y);
  Result.Z := Math.Floor(Self.Z);
end;

function TCHXVec3Type.Round : TCHXVec3Type;
begin
  Result.X := System.Round(Self.X);
  Result.Y := System.Round(Self.Y);
  Result.Z := System.Round(Self.Z);
end;

function TCHXVec3Type.FracCeil : TCHXVec3Type;
begin
  Result.X := Self.X - Math.Ceil(Self.X);
  Result.Y := Self.Y - Math.Ceil(Self.Y);
  Result.Z := Self.Z - Math.Ceil(Self.Z);
end;

function TCHXVec3Type.FracTrunc : TCHXVec3Type;
begin
  Result.X := Self.X - Trunc(Self.X);
  Result.Y := Self.Y - Trunc(Self.Y);
  Result.Z := Self.Z - Trunc(Self.Z);
end;

function TCHXVec3Type.FracFloor : TCHXVec3Type;
begin
  Result.X := Self.X - Math.Floor(Self.X);
  Result.Y := Self.Y - Math.Floor(Self.Y);
  Result.Z := Self.Z - Math.Floor(Self.Z);
end;

class operator TCHXVec3Type.= (const aVec1, aVec2 : TCHXVec3Type) : Boolean;
begin
  Result := SameValueData(aVec1.X, aVec2.X) and
            SameValueData(aVec1.Y, aVec2.Y) and
            SameValueData(aVec1.Z, aVec2.Z);
end;

class operator TCHXVec3Type.+ (const aVec1, aVec2 : TCHXVec3Type)
  : TCHXVec3Type;
begin
  Result.X := aVec1.X + aVec2.X;
  Result.Y := aVec1.Y + aVec2.Y;
  Result.Z := aVec1.Z + aVec2.Z;
end;

class operator TCHXVec3Type.- (const aVector : TCHXVec3Type) : TCHXVec3Type;
begin
  Result.X := -aVector.X; Result.Y := -aVector.Y; Result.Z := -aVector.Z;
end;

class operator TCHXVec3Type.- (const aVec1, aVec2 : TCHXVec3Type)
  : TCHXVec3Type;
begin
  Result.X := aVec1.X - aVec2.X;
  Result.Y := aVec1.Y - aVec2.Y;
  Result.Z := aVec1.Z - aVec2.Z;
end;

class operator TCHXVec3Type.* (const aVec1, aVec2 : TCHXVec3Type)
  : TCHXVec3Type;
begin
  Result.X := aVec1.X * aVec2.X;
  Result.Y := aVec1.Y * aVec2.Y;
  Result.Z := aVec1.Z * aVec2.Z;
end;

class operator TCHXVec3Type.* (const aVector : TCHXVec3Type;
  const aScale : TRealType) : TCHXVec3Type;
begin
  Result.X := aVector.X * aScale;
  Result.Y := aVector.Y * aScale;
  Result.Z := aVector.Z * aScale;
end;

class operator TCHXVec3Type.* (const aScale : TRealType;
  const aVector : TCHXVec3Type) : TCHXVec3Type;
begin
  Result.X := aScale * aVector.X;
  Result.Y := aScale * aVector.Y;
  Result.Z := aScale * aVector.Z;
end;

class operator TCHXVec3Type./ (const aVector : TCHXVec3Type;
  const aScale : TRealType) : TCHXVec3Type;
begin
  Result.X := aVector.X / aScale;
  Result.Y := aVector.Y / aScale;
  Result.Z := aVector.Z / aScale;
end;

class operator TCHXVec3Type./ (const aScale : TRealType;
  const aVector : TCHXVec3Type) : TCHXVec3Type;
begin
  Result.X := aScale / aVector.X;
  Result.Y := aScale / aVector.Y;
  Result.Z := aScale / aVector.Z;
end;

function TCHXVec3Type.ToString(const Delim : char) : String;
begin
  Result := FloatToStr(Self.X) + Delim + FloatToStr(Self.Y)
    + Delim + FloatToStr(Self.Z);
end;

function TCHXVec3Type.ToStringFixed(const Decimals : Integer;
  const Delim : char) : String;
begin
  Result := FloatToStrF(Self.X, ffFixed, Decimals, Decimals) + Delim
    + FloatToStrF(Self.Y, ffFixed, Decimals, Decimals) + Delim
    + FloatToStrF(Self.Z, ffFixed, Decimals, Decimals);
end;

function TCHXVec3Type.ToStringFmt(const aStr : String) : String;
begin
  Result := Format(aStr, [Self.X, Self.Y, Self.Z]);
end;

{
  cCHXVec3List

  CAUTION:

  - `for aPoint in Self do` returns a copy.
  - `Self[i].[X|Y|Z]` can't be assigned, use `Self[i].Init[x](...)`
}

procedure cCHXVec3List.Init3DAll(const aX, aY, aZ : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Init3D(aX, aY, aZ);
end;

procedure cCHXVec3List.InitXYAll(const aX, aY : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitXY(aX, aY);
end;

procedure cCHXVec3List.InitXZAll(const aX, aZ : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitXZ(aX, aZ);
end;

procedure cCHXVec3List.InitZYAll(const aZ, aY : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitZY(aZ, aY);
end;

procedure cCHXVec3List.InitRandom3DAll(
  const MinX, MaxX, MinY, MaxY, MinZ, MaxZ : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRandom3D(MinX, MaxX, MinY, MaxY, MinZ, MaxZ);
end;

procedure cCHXVec3List.InitRandomXYAll(
  const MinX, MaxX, MinY, MaxY : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRandomXY(MinX, MaxX, MinY, MaxY);
end;

procedure cCHXVec3List.InitRandomXZAll(
  const MinX, MaxX, MinZ, MaxZ : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRandomXZ(MinX, MaxX, MinZ, MaxZ);
end;

procedure cCHXVec3List.InitRandomZYAll(
  const MinZ, MaxZ, MinY, MaxY : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRandomZY(MinZ, MaxZ, MinY, MaxY);
end;

procedure cCHXVec3List.InitPolar3DAll(const aMag, aYaw, aPitch : TRealType);
var
  i : Integer;
begin
  if Self.Count < 1 then Exit;
  // We only calculate actual coordinates once.
  Self[0].InitPolar3D(aMag, aYaw, aPitch);
  for i := 1 to (Self.Count - 1) do
    Self[i] := Self[0];
end;

procedure cCHXVec3List.InitPolarXYAll(const aMag, aAngle : TRealType);
var
  i : Integer;
begin
  if Self.Count < 1 then Exit;
  // We only calculate actual coordinates once.
  Self[0].InitPolarXY(aMag, aAngle);
  for i := 1 to (Self.Count - 1) do
    Self[i] := Self[0];
end;

procedure cCHXVec3List.InitPolarXZAll(const aMag, aAngle : TRealType);
var
  i : Integer;
begin
  if Self.Count < 1 then Exit;
  // We only calculate actual coordinates once.
  Self[0].InitPolarXZ(aMag, aAngle);
  for i := 1 to (Self.Count - 1) do
    Self[i] := Self[0];
end;

procedure cCHXVec3List.InitPolarZYAll(const aMag, aAngle : TRealType);
var
  i : Integer;
begin
  if Self.Count < 1 then Exit;
  // We only calculate actual coordinates once.
  Self[0].InitPolarZY(aMag, aAngle);
  for i := 1 to (Self.Count - 1) do
    Self[i] := Self[0];
end;

procedure cCHXVec3List.InitRndPolar3DAll(const aMag : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRndPolar3D(aMag);
end;

procedure cCHXVec3List.InitRndPolarXYAll(const aMag : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRndPolarXY(aMag);
end;

procedure cCHXVec3List.InitRndPolarXZAll(const aMag : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRndPolarXZ(aMag);
end;

procedure cCHXVec3List.InitRndPolarZYAll(const aMag : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].InitRndPolarZY(aMag);
end;

procedure cCHXVec3List.SetAngles3DAll(const aYaw, aPitch : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].SetAngles3D(aYaw, aPitch);
end;

procedure cCHXVec3List.SetAngleXYAll(const aAngle : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].SetAngleXY(aAngle);
end;

procedure cCHXVec3List.SetAngleXZAll(const aAngle : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].SetAngleXZ(aAngle);
end;

procedure cCHXVec3List.SetAngleZYAll(const aAngle : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].SetAngleZY(aAngle);
end;

procedure cCHXVec3List.SetMagnitudeAll(const aMag : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].SetMagnitude(aMag);
end;

procedure cCHXVec3List.NormalizeAll;
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Normalize;
end;

procedure cCHXVec3List.NegateAll;
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Negate;
end;

procedure cCHXVec3List.AddAll(const aVector : TCHXVec3Type);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Add(aVector);
end;

procedure cCHXVec3List.SubtractAll(const aVector : TCHXVec3Type);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Subtract(aVector);
end;

procedure cCHXVec3List.MultiplyAll(const aScale : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Multiply(aScale);
end;

procedure cCHXVec3List.ScaleAll(const aVector : TCHXVec3Type);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Scale(aVector);
end;

procedure cCHXVec3List.DivideAll(const aScale : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Divide(aScale);
end;

procedure cCHXVec3List.DivInvAll(const aScale : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].DivInv(aScale);
end;

procedure cCHXVec3List.MoveAll(const dX, dY, dZ : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Move(dX, dY, dZ);
end;

procedure cCHXVec3List.ScaleAll(const sX, sY, sZ : TRealType);
var
  i : Integer;
begin
  for i := 0 to (Self.Count - 1) do
    Self[i].Scale(sX, sY, sZ);
end;

procedure cCHXVec3List.RotateXYAll(const aAngle : TRealType);
var
  i : Integer;
  SinXY, CosXY : TRealType;
begin
  if Self.Count < 1 then Exit;
  SinCosData(aAngle, SinXY, CosXY);
  for i := 0 to (Self.Count - 1) do
    Self[i].Init3D(Self[i].X * CosXY - Self[i].Y * SinXY,
      Self[i].X * SinXY + Self[i].Y * CosXY,
      Self[i].Z); // Keep Z
end;

procedure cCHXVec3List.RotateXZAll(const aAngle : TRealType);
var
  i : Integer;
  SinXZ, CosXZ : TRealType;
begin
  if Self.Count < 1 then Exit;
  SinCosData(aAngle, SinXZ, CosXZ);
  for i := 0 to (Self.Count - 1) do
    Self[i].Init3D(Self[i].X * CosXZ - Self[i].Z * SinXZ,
      Self[i].Y, // Keep Y
      Self[i].X * SinXZ + Self[i].Z * CosXZ);
end;

procedure cCHXVec3List.RotateZYAll(const aAngle : TRealType);
var
  i : Integer;
  SinZY, CosZY : TRealType;
begin
  if Self.Count < 1 then Exit;
  SinCosData(aAngle, SinZY, CosZY);
  for i := 0 to (Self.Count - 1) do
    Self[i].Init3D(Self[i].X, // Keep X
      Self[i].Z * SinZY + Self[i].Y * CosZY,
      Self[i].Z * CosZY - Self[i].Y * SinZY);
end;

procedure cCHXVec3List.RotateXYAll(const aAngle : TRealType;
  const Center : TCHXVec3Type);
var
  i : Integer;
  SinXY, CosXY : TRealType;
begin
  if Self.Count < 1 then Exit;
  SinCosData(aAngle, SinXY, CosXY);
  for i := 0 to (Self.Count - 1) do
  begin
    Self[i].Subtract(Center);
    Self[i].Init3D(Self[i].X * CosXY - Self[i].Y * SinXY,
      Self[i].X * SinXY + Self[i].Y * CosXY,
      Self[i].Z); // Keep Z
    Self[i].Add(Center);
  end;
end;

procedure cCHXVec3List.RotateXZAll(const aAngle : TRealType;
  const Center : TCHXVec3Type);
var
  i : Integer;
  SinXZ, CosXZ : TRealType;
begin
  if Self.Count < 1 then Exit;
  SinCosData(aAngle, SinXZ, CosXZ);
  for i := 0 to (Self.Count - 1) do
  begin
    Self[i].Subtract(Center);
    Self[i].Init3D(Self[i].X * CosXZ - Self[i].Z * SinXZ,
      Self[i].Y, // Keep Y
      Self[i].X * SinXZ + Self[i].Z * CosXZ);
    Self[i].Add(Center);
  end;
end;

procedure cCHXVec3List.RotateZYAll(const aAngle : TRealType;
  const Center : TCHXVec3Type);
var
  i : Integer;
  SinZY, CosZY : TRealType;
begin
  if Self.Count < 1 then Exit;
  SinCosData(aAngle, SinZY, CosZY);
  for i := 0 to (Self.Count - 1) do
  begin
    Self[i].Subtract(Center);
    Self[i].Init3D(Self[i].X, // Keep X
      Self[i].Z * SinZY + Self[i].Y * CosZY,
      Self[i].Z * CosZY - Self[i].Y * SinZY);
    Self[i].Add(Center);
  end;
end;

function cCHXVec3List.GetSqrDistance3D(const Cyclic : Boolean) : TRealType;
var
  i : Integer;
begin
  Result := 0;
  if Self.Count < 2 then Exit;
  for i := 0 to (Self.Count - 2) do
    Result += Self[i].SqrDistance3D(Self[i + 1]);
  if Cyclic then
    Self[0].SqrDistance3D(Self.Last);
end;

function cCHXVec3List.GetSqrDistanceXY(const Cyclic : Boolean) : TRealType;
var
  i : Integer;
begin
  Result := 0;
  if Self.Count < 2 then Exit;
  for i := 0 to (Self.Count - 2) do
    Result += Self[i].SqrDistanceXY(Self[i + 1]);
  if Cyclic then
    Self[0].SqrDistanceXY(Self.Last);
end;

function cCHXVec3List.GetSqrDistanceXZ(const Cyclic : Boolean) : TRealType;
var
  i : Integer;
begin
  Result := 0;
  if Self.Count < 2 then Exit;
  for i := 0 to (Self.Count - 2) do
    Result += Self[i].SqrDistanceXZ(Self[i + 1]);
  if Cyclic then
    Self[0].SqrDistanceXZ(Self.Last);
end;

function cCHXVec3List.GetSqrDistanceZY(const Cyclic : Boolean) : TRealType;
var
  i : Integer;
begin
  Result := 0;
  if Self.Count < 2 then Exit;
  for i := 0 to (Self.Count - 2) do
    Result += Self[i].SqrDistanceZY(Self[i + 1]);
  if Cyclic then
    Self[0].SqrDistanceZY(Self.Last);
end;

function cCHXVec3List.GetDistance3D(const Cyclic : Boolean) : TRealType;
begin
  if Self.Count < 2 then Exit;
  Result := SqRt(GetSqrDistance3D(Cyclic));
end;

function cCHXVec3List.GetDistanceXY(const Cyclic : Boolean) : TRealType;
begin
  if Self.Count < 2 then Exit;
  Result := SqRt(GetSqrDistanceXY(Cyclic));
end;

function cCHXVec3List.GetDistanceXZ(const Cyclic : Boolean) : TRealType;
begin
  if Self.Count < 2 then Exit;
  Result := SqRt(GetSqrDistanceXZ(Cyclic));
end;

function cCHXVec3List.GetDistanceZY(const Cyclic : Boolean) : TRealType;
begin
  if Self.Count < 2 then Exit;
  Result := SqRt(GetSqrDistanceZY(Cyclic));
end;

function cCHXVec3List.MidPointAll : TCHXVec3Type;
var
  aPoint : TCHXVec3Type;
begin
  Result.X := 0; Result.Y := 0; Result.Z := 0;
  if Self.Count < 1 then Exit; // ToDo: Division by Zero exception?
  for aPoint in Self do
    Result += aPoint;
  Result.Divide(Self.Count);
end;

function cCHXVec3List.WriteString(
  const ListBegin, ListEnd, PointBegin, PointEnd, Delim : char) : String;
var
  i : Integer;
begin
  Result := ListBegin;
  if Self.Count < 1 then
    Exit(Result + ListEnd);
  for i := 0 to (Self.Count - 2) do
  begin
    Result += PointBegin;
    Result += Self[i].ToString(Delim);
    Result += PointEnd + Delim;
  end;
  Result += PointBegin;
  Result += Self[Self.Count - 1].ToString(Delim);
  Result += PointEnd + ListEnd;
end;

{
  Funciones constructoras
}
function CHXVec3Func(const aX, aY, aZ : TRealType) : TCHXVec3Type;
begin
  Result.X := aX; Result.Y := aY; Result.Z := aZ;
end;

end.
