unit uTCHXMatrixE;
{$macro ON}

{$define TRealType := Extended }
//< Actual type of the elements.
{$define TCHXMatrixType := TCHXMatrixE }
//< ID for the main type.
{$define PCHXMatrix := PCHXMatrixE }
//< ID of the pointer type to TCHXMatrixType type.
{$define CHXMatrix2X2Func := CHXMatrixE2X2 }
//< Name for the function that returns a 2x2 Matrix.
{$define CHXMatrix3X3Func := CHXMatrixE3X3 }
//< Name for the function that returns a 3x3 Matrix.
{$define CHXMatrix4X4Func := CHXMatrixE4X4 }
//< Name for the function that returns a 4x4 Matrix.

{$define UnitsUsed := , Math; }
{<
  Units needed for TRealType, compare and mathematical functions.
  if no unit needed, must be ';'.
  Maybe function names below will require <Unit>.<Function> syntax.
}
{$define DataIsZero := Math.IsZero }
//< function DataIsZero(TRealType): Boolean;

{$I 'uTCHXMatrixType.inc'}
