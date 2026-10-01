unit utCHXVec3S;

{$macro ON}
{$define TRealType := Single }
//< Actual type of the components.
{$define TCHXVec3Type := TCHXVec3S }
//< ID for the main type.
{$define CHXVec3Func := CHXVec3S }
//< Name for the function that returns a new TCHXVec3Type.
{$define PCHXVec3 := PCHXVec3S }
//< ID of the pointer type to TCHXVec3Type type.

{$define TCHXPoint3DType := TCHXPoint3DS }
//< TCHXPoint3DF type equivalent for retrocompatibility.
{$define TCHXColorType := TCHXColorS }
//< TCHXColorF type equivalent for retrocompatibility.

{$define cCHXVec3GenList := cCHXVec3SGenList }
//< ID for generic list specialized to TCHXVec3Type type.
{$define cCHXVec3List := cCHXVec3SList }
//< ID for a generic list descendant.

{$define UnitsUsed := , Types, Math; }
{<
  Units needed for TRealType, compare and mathematical functions.
  if no unit needed, must be ';'.
  Maybe function names below will require <Unit>.<Function> syntax.
}
{$define DataIsZero := Math.IsZero }
//< function DataIsZero(TRealType): Boolean;
{$define SameValueData := Math.SameValue }
//< function SameValueData(TRealType, TRealType): Boolean;
{$define SameValueSigma := Math.SameValue }
//< function SameValueSigma(TRealType, TRealType, TRealType): Boolean;
{$define SinCosData := Math.SinCos }
//< procedure SinCosData(Angle, out aSin, out aCos);
{$define ArcTan2Data := Math.ArcTan2 }
//< function ArcTan2Data(Y, X): TRealType;
{$define ArcSinData := Math.ArcSin }
//< function ArcSinData(Angle): TRealType;

{$define Proj2DPType := TPointF }
//< Type of 2D Point (with X and Y fields) for Proj[x] methods' Result.

{$I 'utCHXVec3Type.inc'}
