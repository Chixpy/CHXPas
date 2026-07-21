(*< Unit Template for custom dimension matrix records.

  Usage: Create a unit file:

    unit <UnitName>;

    {$macro ON}

    {$define TRealType := <FloatDataType> }
    //< Actual type of the elements.
    {$define TCHXMatrixType := <MatrixTypeID> }
    //< ID for the main type.
    {$define PCHXMatrix := <PointerID> }
    //< ID of the pointer type to TCHXMatrixType type.
    {$define CHXMatrix2X2Func := <Matrix2X2Func> }
    //< Name for the function that returns a 2x2 Matrix.
    {$define CHXMatrix3X3Func := <Matrix3X3Func> }
    //< Name for the function that returns a 3x3 Matrix.
    {$define CHXMatrix4X4Func := <Matrix4X4Func> }
    //< Name for the function that returns a 4x4 Matrix.

    {$define UnitsUsed := , <Units> ; }
    {<
      Units needed for TRealType, compare and mathematical functions.
      if no unit needed, must be ';'.
      Maybe function names below will require <Unit>.<Function> syntax.
    }
    {$define DataIsZero := <FunctionName> }
    //< function DataIsZero(TRealType): Boolean;
    
    {$I '<ThisFilename>'}

  Four types of matrices are implemented, for all basic float types:
    - @code(TCHXMatrixS) for Single in uTCHXMatrixS.pas
    - @code(TCHXMatrixD) for Double in uTCHXMatrixD.pas
    - @code(TCHXMatrixE) for Extended in uTCHXMatrixE.pas
    - @code(TCHXMatrixR) for Real in uTCHXMatrixR.pas

  For square matrices of 2x2, 3x3 or 4x4 (i.e. 2D/3D transforms), surely
    FPC's Matrix unit is more eficient.

  The purpouse of this types is more generalist, oriented towards generic
    operations of any matrix size and other uses such as solving systems of
    equations or optimizing via Simplex method.

  @bold(Note:) Indices for element positions start at 0, unlike standard
    mathematics which usually start at 1.

  @bold(On errors:) Range and operation errors will default to system
    exceptions.

  ToDo:
  - Create TCHXMatrixC for Complex Numbers.
  - Use a generic type..., but maybe we need helpers for some methods.

  Copyright (C) 2024-2026 Chixpy https://github.com/Chixpy
*)
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline ON}

interface

uses
  SysUtils UnitsUsed
  
resourcestring
  krsAddition = 'Addition';
  krsSubtraction = 'Substraction';
  krsMultiplication = 'Multiplication';
  krsDimensError = '%s: Matrix dimension error.';

type
  { 
    Matrix with TRealType type data.
  }
  TCHXMatrixType = record
  type
    TRow = array of TRealType;
    TData = array of TRow;

  private
    function GetItem(const Row, Col: Integer): TRealType; inline;
    //< Internal getter for Items property.
    procedure SetItem(const Row, Col: Integer; const Value: TRealType); inline;
    //< Internal setter for Items property.

  public
    Data: TData;
    //< Actual field with matrix data.

    property Items[const Row, Col: Integer]: TRealType read GetItem write SetItem;
      default;
    //< Default indexed property (Allows Matrix[r, c] syntax)

    procedure Init(const Rows, Cols: Integer; const ForceClear: Boolean = True);
    {<
      Init the matrix. Actually, it's the same as Resize but clear current
        values by default.
    }
    procedure InitSqr(const Rank: Integer; const ForceClear: Boolean = True);
      inline;
    //< Init a square matrix.
    procedure InitIdentity(const Rank: Integer);
    //< Init an identity matrix.
    procedure Init2x2(const a00, a01, a10, a11: TRealType);
    //< Init a 2x2 matrix.
    procedure Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, a22: TRealType);
    //< Init a 3x3 matrix.
    procedure Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
      a22, a23, a30, a31, a32, a33: TRealType);
    //< Init a 4x4 matrix.

    procedure Resize(const Rows, Cols: Integer; 
      const ForceClear: Boolean = False); inline;
    {<
      Resize the matrix. Actually is equivalent to Init, 
        but by default it preserves previous values.
    }

    function RowCount: Integer; inline;
    //< Number of rows.
    function HighRow: Integer; inline;
    //< Highest row index.
    function ColCount(const Row: Integer = 0): Integer;
    //< Number of columns (at any Row).
    function HighCol(const Row: Integer = 0): Integer;
    //< Highest column index (at any Row).

    function GetRow(const Row: Integer): TCHXMatrixType;
    //< Get a matrix Row.
    function GetCol(const Col: Integer): TCHXMatrixType;
    //< Get a matrix Column.
    procedure SetRow(const Row: Integer; const RowData: TRow);
    //< Set a matrix Row.
    procedure SetCol(const Col: Integer; const ColData: TCHXMatrixType);
    //< Set a matrix Column.

    function IsSquare: Boolean; inline;
    //< Test if a matrix is square.
    function IsEmpty: Boolean; inline;
    //< Test if a matrix is empty (Dimension = 0)
    function IsZero: Boolean;
    //< Test if all elements are 0.

    function Transpose: TCHXMatrixType;
    //< Transpose
    procedure SwapRows(const Row1, Row2: Integer);
    //< Swap two rows.
    procedure SwapCols(const Col1, Col2: Integer);
    //< Swap two columns.

    function MinorMatrix(const Row, Col: Integer): TCHXMatrixType;
    //< Submatrix created by removing one row and one column from the matrix.
    function SubMatrix(const FirstRow, FirstCol, LastRow, 
      LastCol: Integer): TCHXMatrixType;
    //< Submatrix created from a range of rows and columns.
    function Adjugate: TCHXMatrixType;
    //< Adjugate matrix (matrix of minors/cofactors).
    function Determinant: TRealType;
    //< Determinant of the matrix.
    function Rank: Integer;
    //< Rank of the matrix.
    function Inverse: TCHXMatrixType;

    procedure InitRot3DXY(const Angle: TRealType);
    //< Create a 3D rotation matrix.
    procedure InitRot3DXZ(const Angle: TRealType);
    //< Create a 3D rotation matrix.
    procedure InitRot3DYZ(const Angle: TRealType);
    //< Create a 3D rotation matrix.

    function SolveLinear: TCHXMatrixType;

    class operator Copy(constref Src: TCHXMatrixType; var Dst: TCHXMatrixType);
    //< Assignment operator overload (:=) for deep copying.
    class operator =(const M1, M2: TCHXMatrixType): Boolean;
    //< Comparing operator overload (=). "<>" is autocreated by FPC.
    class operator +(const M1, M2: TCHXMatrixType): TCHXMatrixType;
    class operator -(const M1, M2: TCHXMatrixType): TCHXMatrixType;
    class operator -(const M: TCHXMatrixType): TCHXMatrixType;
    // Unary minus (Negate matrix elements)
    class operator *(const M1, M2: TCHXMatrixType): TCHXMatrixType;
    class operator *(const M: TCHXMatrixType; const Factor: TRealType): TCHXMatrixType;
    class operator *(const Factor: TRealType; const M: TCHXMatrixType): TCHXMatrixType;
    class operator /(const M: TCHXMatrixType; const Factor: TRealType): TCHXMatrixType;
    // Matrix / Factor.
    class operator /(const Factor: TRealType; const M: TCHXMatrixType): TCHXMatrixType;
    // Factor / Matrix (¿It has use?).
  end;

  PCHXMatrix = ^TCHXMatrixType;

  function CHXMatrix2x2(const a00, a01, a10, a11: TRealType): TCHXMatrixType;
  function CHXMatrix3x3(const a00, a01, a02, a10, a11, a12, a20, a21, a22
    : TRealType): TCHXMatrixType;
  function CHXMatrix4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
      a22, a23, a30, a31, a32, a33: TRealType): TCHXMatrixType;
  

implementation

{ TCHXMatrixType }

function TCHXMatrixType.GetItem(const Row, Col: Integer): TRealType; inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Result := Data[Row, Col];
end;

procedure TCHXMatrixType.SetItem(const Row, Col: Integer; const Value: TRealType); inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Data[Row, Col] := Value;
end;

procedure TCHXMatrixType.Init(const Rows, Cols: Integer; const ForceClear: Boolean);
begin
  if ForceClear then
    SetLength(Data, 0);

  SetLength(Data, Rows, Cols);
end;

procedure TCHXMatrixType.InitSqr(const Rank: Integer; const ForceClear: Boolean);
begin
  Self.Init(Rank, Rank, ForceClear);
end;

procedure TCHXMatrixType.InitIdentity(const Rank: Integer);
var
  aPos: Integer;
begin
  Self.Init(Rank, Rank, True);
  for aPos := 0 to HighRow do
    Data[aPos, aPos] := 1; // Assuming implicit conversion or TRealType initialization
end;

procedure TCHXMatrixType.Init2x2(const a00, a01, a10, a11: TRealType);
begin
  Self.InitSqr(2, False);
  Data[0, 0] := a00; Data[0, 1] := a01;
  Data[1, 0] := a10; Data[1, 1] := a11;
end;

procedure TCHXMatrixType.Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
  a22: TRealType);
begin
  Self.InitSqr(3, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22;
end;

procedure TCHXMatrixType.Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, 
  a20, a21, a22, a23, a30, a31, a32, a33: TRealType);
begin
  Self.InitSqr(4, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02; Data[0, 3] := a03;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12; Data[1, 3] := a13;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22; Data[2, 3] := a23;
  Data[3, 0] := a30; Data[3, 1] := a31; Data[3, 2] := a32; Data[3, 3] := a33;
end;

procedure TCHXMatrixType.Resize(const Rows, Cols: Integer; 
  const ForceClear: Boolean);
begin
  Self.Init(Rows, Cols, ForceClear);
end;

function TCHXMatrixType.RowCount: Integer;
begin
  Result := Length(Data);
end;

function TCHXMatrixType.HighRow: Integer;
begin
  Result := High(Data);
end;

function TCHXMatrixType.ColCount(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(0);
  Result := Length(Data[Row]);
end;

function TCHXMatrixType.HighCol(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(-1);
  Result := High(Data[Row]);
end;

function TCHXMatrixType.GetRow(const Row: Integer): TCHXMatrixType;
begin
  Result.Init(1, ColCount, False); // Initializing Result structure
  Result.Data[0] := Copy(Self.Data[Row]); // Deep copy of the row data
end;

function TCHXMatrixType.GetCol(const Col: Integer): TCHXMatrixType;
var
  Row: Integer;
begin
  Result.Init(RowCount, 1, False);
  // Iterating through rows to extract the specific column value.
  for Row := 0 to HighRow do
    Result[Row, 0] := Data[Row, Col];
end;

procedure TCHXMatrixType.SetRow(const Row: Integer; const RowData: TRow);
begin
  // ToDo: Validate that RowData has the correct number of columns.
  Data[Row] := RowData;
end;

procedure TCHXMatrixType.SetCol(const Col: Integer; const ColData: TCHXMatrixType);
var
  Row: Integer;
begin
  // ToDo: Validate that ColData has the correct number of rows.
  for Row := 0 to HighRow do
    Data[Row, Col] := ColData[Row, 0];
end;

function TCHXMatrixType.IsSquare: Boolean;
begin
  Result := RowCount = ColCount;
end;

function TCHXMatrixType.IsEmpty: Boolean;
begin
  Result := RowCount <= 0;
end;

function TCHXMatrixType.IsZero: Boolean;
var
  Row, Col: Integer;
begin
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      if not DataIsZero(Data[Col, Row]) then
        Exit(False);
  Result := True;
end;

function TCHXMatrixType.Transpose: TCHXMatrixType;
var
  Row, Col: Integer;
begin
  Result.Init(ColCount, RowCount, False);
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      Result[Col, Row] := Data[Row, Col];
end;

procedure TCHXMatrixType.SwapRows(const Row1, Row2: Integer);
var
  Temp: TRow;
begin
  // Direct pointer swapping for maximum efficiency
  Temp := Data[Row1];
  Data[Row1] := Data[Row2];
  Data[Row2] := Temp;
end;

procedure TCHXMatrixType.SwapCols(const Col1, Col2: Integer);
var
  Row: Integer;
  Temp: TRealType;
begin
  for Row := 0 to HighRow do
  begin
    Temp := Data[Row, Col1];
    Data[Row, Col1] := Data[Row, Col2];
    Data[Row, Col2] := Temp;
  end;
end;

function TCHXMatrixType.MinorMatrix(const Row, Col: Integer): TCHXMatrixType;
var
  RRow, RCol, SRow, SCol: Integer;
begin
  Result.Init(HighRow, HighCol, False);
  RRow := 0; // Current Result matrix row

  for SRow := 0 to HighRow do
  begin
    if SRow = Row then 
      Continue;

    RCol := 0;
    for SCol := 0 to HighCol(SRow) do
    begin
      if SCol = Col then 
        Continue;

      Result[RRow, RCol] := Data[SRow, SCol];
      Inc(RCol);
    end;
    Inc(RRow);
  end;
end;

function TCHXMatrixType.SubMatrix(const FirstRow, FirstCol, LastRow, 
  LastCol: Integer): TCHXMatrixType;
var
  Rows, Cols, Row, Col, CRow: Integer;
begin
  if (FirstRow > LastRow) or (FirstCol > LastCol) then
  begin
    Result.Init(0, 0, False); // Or raise EException
    Exit;
  end;

  Rows := LastRow - FirstRow;
  Cols := LastCol - FirstCol;
  Result.Init(Rows + 1, Cols + 1, False);

  for Row := 0 to Rows do
  begin
    CRow := Row + FirstRow;
    for Col := 0 to Cols do
      Result[Row, Col] := Data[CRow, Col + FirstCol];
  end;
end;

function TCHXMatrixType.Adjugate: TCHXMatrixType;
var
  Row, Col: Integer;
begin
  if not IsSquare then
  begin
    Result.Init(0, 0, False); // Or raise EException
    Exit;
  end;

  Result.Init(RowCount, ColCount, False);
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      Result[Row, Col] := MinorMatrix(Row, Col).Determinant;
end;

function TCHXMatrixType.Determinant: TRealType;
var
  Row: Integer;
  Elem: TRealType;
begin
  Result := 0;
  if not IsSquare then 
    Exit; // Or exception

  case RowCount of
    0: Result := 1;
    1: Result := Data[0, 0];
    2: Result := (Data[0, 0] * Data[1, 1]) - (Data[0, 1] * Data[1, 0]);
    3: Result := (Data[0, 0] * 
                    ((Data[1, 1] * Data[2, 2]) - (Data[1, 2] * Data[2, 1])))
               - (Data[0, 1] * 
                    ((Data[1, 0] * Data[2, 2]) - (Data[1, 2] * Data[2, 0])))
               + (Data[0, 2] * 
                    ((Data[1, 0] * Data[2, 1]) - (Data[1, 1] * Data[2, 0])));
  otherwise
    for Row := 0 to HighRow do
    begin
      if Odd(Row) then
        Elem := -Data[Row, 0]
      else
        Elem := Data[Row, 0];

      // Accumulating cofactors for Laplace expansion
      Result := Result + (Elem * MinorMatrix(Row, 0).Determinant);
    end;
  end;
end;

function TCHXMatrixType.Rank: Integer;
var
  M: TCHXMatrixType;
  PivotCol, MaxRow, Row, Col: Integer;
  MaxVal, CurrVal, Factor: TRealType;
begin
  Result := 0; // This also serves as the current row index (PivotRow)
  if RowCount <= 0 then 
    Exit;

  M := Self; // Deep copy to prevent modifying the original matrix

  PivotCol := 0;
  while (Result < M.RowCount) and (PivotCol < M.ColCount) do
  begin
    // Searching for the maximum value (in absolute terms)
    //   within the current column
    MaxVal := Abs(M[Result, PivotCol]);
    MaxRow := Result;

    for Row := (Result + 1) to M.HighRow do
    begin
      CurrVal := Abs(M[Row, PivotCol]);
      if CurrVal > MaxVal then
      begin
        MaxVal := CurrVal;
        MaxRow := Row;
      end;
    end;

    // If the pivot value is zero, we skip to the next column
    if DataIsZero(MaxVal) then
    begin
      Inc(PivotCol);
      Continue;
    end;

    // Swap current row with the row containing the highest pivot value
    if Result <> MaxRow then
      M.SwapRows(Result, MaxRow);

    // Normalize the pivot row
    CurrVal := M[Result, PivotCol];
    for Col := PivotCol to M.HighCol(Result) do
      M[Result, Col] := M[Result, Col] / CurrVal;

    // Eliminate entries below the pivot element (create zeros in this column)
    for Row := (Result + 1) to M.HighRow do
    begin
      Factor := M[Row, PivotCol];
      for Col := PivotCol to M.HighCol(Row) do
        M[Row, Col] := M[Row, Col] - (Factor * M[Result, Col]);
    end;

    Inc(Result);
    Inc(PivotCol);
  end;
end;

function TCHXMatrixType.Inverse: TCHXMatrixType;
var
  M: TCHXMatrixType;
  PivotPos, Row, Col, TempI: Integer;
  PivotVal, Factor, TempT: TRealType;
begin
  if Self.IsEmpty or (not Self.IsSquare) then
  begin
    Result.Init(0, 0, False); // Or raise Exception
    Exit;
  end;

  // Augmented matrix approach, split into two separate structures
  M := Self; // Deep copy to preserve the original matrix
  Result.InitIdentity(RowCount);

  // Gauss-Jordan Method
  for PivotPos := 0 to M.HighRow do
  begin
    PivotVal := M[PivotPos, PivotPos];
    Row := PivotPos;

    // Searching row with highest absolute value to avoid division by zero
    // and to maintain floating-point numerical stability.
    for TempI := (PivotPos + 1) to M.HighRow do
    begin
      TempT := M[TempI, PivotPos];
      if Abs(TempT) > Abs(PivotVal) then
      begin
        PivotVal := TempT;
        Row := TempI;
      end;
    end;

    // If the entire column below the pivot is zero, the matrix is 
    //   singular (no inverse)
    if DataIsZero(PivotVal) then
    begin
      Result.Init(0, 0, False);
      Exit; // Or raise Exception
    end;

    // Swap current row with the row that contains the valid pivot
    if Row <> PivotPos then
    begin
      M.SwapRows(Row, PivotPos);
      Result.SwapRows(Row, PivotPos);
    end;

    // Normalize the pivot row
    // Already set: PivotVal := M[PivotPos, PivotPos];
    for Col := PivotPos to M.HighCol(PivotPos) do
      M[PivotPos, Col] := M[PivotPos, Col] / PivotVal;

    for Col := 0 to Result.HighCol(PivotPos) do
      Result[PivotPos, Col] := Result[PivotPos, Col] / PivotVal;

    // Eliminate entries in the current column for all other rows
    //   (make them zero)
    for Row := 0 to M.HighRow do
    begin
      if Row = PivotPos then
        Continue;

      Factor := M[Row, PivotPos];

      for Col := 0 to M.HighCol(Row) do
        M[Row, Col] := M[Row, Col] - (Factor * M[PivotPos, Col]);

      for Col := 0 to Result.HighCol(Row) do
        Result[Row, Col] := Result[Row, Col] - (Factor * Result[PivotPos, Col]);
    end;
  end;
end;

procedure TCHXMatrixType.InitRot3DXY(const Angle: TRealType);
var
  aSin, aCos: TRealType;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, -aSin, 0, aSin, aCos, 0, 0, 0, 1);
end;

procedure TCHXMatrixType.InitRot3DXZ(const Angle: TRealType);
var
  aSin, aCos: TRealType;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, 0, aSin, 0, 1, 0, -aSin, 0, aCos);
end;

procedure TCHXMatrixType.InitRot3DYZ(const Angle: TRealType);
var
  aSin, aCos: TRealType;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(1, 0, 0, 0, aCos, -aSin, 0, aSin, aCos);
end;

function TCHXMatrixType.SolveLinear: TCHXMatrixType;
var
  M: TCHXMatrixType;
  PivotPos, Row, Col: Integer;
  PivotVal, Factor: TRealType;
begin
  // Assumes Self is an augmented matrix [A | B] where the last column is B
  if Self.IsEmpty or (Self.ColCount < 2) then
  begin
    Result.Init(0, 0, False);
    Exit;
  end;

  M := Self; 
  // Result will hold the solution vector (1 column)
  Result.Init(M.RowCount, 1, False);
  
  // Extract the independent terms (last column) into Result to separate A and B
  for Row := 0 to M.HighRow do
  begin
    Result[Row, 0] := M[Row, M.HighCol(Row)];
    // Shrink M's row virtual size or ignore the last column in calculation
  end;

  // Gauss-Jordan elimination over the coefficient matrix part
  for PivotPos := 0 to M.HighRow do
  begin
    // Check bounds since we excluded the last column
    if PivotPos >= (M.ColCount - 1) then Break;

    PivotVal := M[PivotPos, PivotPos];

    // Pivoting
    Row := PivotPos;
    while DataIsZero(PivotVal) and (Row < M.HighRow) do
    begin
      Inc(Row);
      PivotVal := M[Row, PivotPos];
    end;

    // If pivot is zero, the system doesn't have a unique solution
    if DataIsZero(PivotVal) then
    begin
      Result.Init(0, 0, False); // Return empty matrix on error/no unique solution
      Exit;
    end;

    if Row <> PivotPos then
    begin
      M.SwapRows(Row, PivotPos);
      Result.SwapRows(Row, PivotPos);
    end;

    // Normalize pivot row (excluding the last column of M since it's now in Result)
    PivotVal := M[PivotPos, PivotPos];
    for Col := PivotPos to (M.ColCount - 2) do
      M[PivotPos, Col] := M[PivotPos, Col] / PivotVal;

    Result[PivotPos, 0] := Result[PivotPos, 0] / PivotVal;

    // Eliminate other rows
    for Row := 0 to M.HighRow do
    begin
      if Row = PivotPos then Continue;

      Factor := M[Row, PivotPos];
      for Col := PivotPos to (M.ColCount - 2) do
        M[Row, Col] := M[Row, Col] - (Factor * M[PivotPos, Col]);

      Result[Row, 0] := Result[Row, 0] - (Factor * Result[PivotPos, 0]);
    end;
  end;
end;

class operator TCHXMatrixType.Copy(constref Src: TCHXMatrixType; var Dst: TCHXMatrixType);
var
  i: Integer;
begin
  { 
    Deep copy of the multi-dimensional dynamic array structure.

    In Pascal, assigning a record copies its fields. However, since dynamic
      arrays are used internally, only the pointer reference would be copied
      by default. Overloading this operator forces FPC to perform a deep copy
      of the array content.

    Tecnically you can't do:
      class operator :=(const Src: aType): aType; // Both are the same type

    With FPC 3.1.1, you can redefine this management operator to change the
      behavior of ":=". The usual solution was to create a method
      CopyFrom(Source) or CopyTo(Target).
}
  SetLength(Dst.Data, Length(Src.Data));

  for i := 0 to High(Src.Data) do
  begin
    SetLength(Dst.Data[i], Length(Src.Data[i]));
    if Length(Src.Data[i]) <= 0 then Continue;
    Move(Src.Data[i][0], Dst.Data[i][0], Length(Src.Data[i]) * SizeOf(TRealType));
  end;
end;

class operator TCHXMatrixType.=(const M1, M2: TCHXMatrixType): Boolean;
var
  Row, Col: Integer;
begin
  // Matrices are only considered equal if they have the same dimensions 
  //   and elements.
  if (M1.RowCount <> M2.RowCount) or (M1.ColCount <> M2.ColCount) then
    Exit(False);

  for Row := 0 to M1.HighRow do
    for Col := 0 to M1.HighCol do
      if not SameValue(M1.Data[Row, Col], M2.Data[Row, Col]) then
        Exit(False);

  Result := True;
end;

class operator TCHXMatrixType.+(const M1, M2: TCHXMatrixType): TCHXMatrixType;
var
  Row, Col: Integer;
begin
  if (M1.RowCount <> M2.RowCount) or (M1.ColCount <> M2.ColCount) then
    raise Exception.CreateFmt(krsDimensError, [krsAddition]);

  Result.Init(M1.RowCount, M2.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := M1[Row, Col] + M2[Row, Col];
end;

class operator TCHXMatrixType.-(const M1, M2: TCHXMatrixType): TCHXMatrixType;
var
  Row, Col: Integer;
begin
  if (M1.RowCount <> M2.RowCount) or (M1.ColCount <> M2.ColCount) then
    raise Exception.CreateFmt(krsDimensError, [krsSubtraction]);

  Result.Init(M1.RowCount, M2.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := M1[Row, Col] - M2[Row, Col];
end;

class operator TCHXMatrixType.-(const M: TCHXMatrixType): TCHXMatrixType;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := -M[Row, Col];
end;

class operator TCHXMatrixType.*(const M1, M2: TCHXMatrixType): TCHXMatrixType;
var
  Row1, Col2, K: Integer;
  Value: TRealType;
begin
  if M1.ColCount <> M2.RowCount then
    raise Exception.CreateFmt(krsDimensError, [krsMultiplication]);

  Result.Init(M1.RowCount, M2.ColCount, False);
  for Row1 := 0 to M1.HighRow do
  begin
    for Col2 := 0 to M2.HighCol do
    begin
      Value := 0;
      // Dot product of M1 row and M2 column
      for K := 0 to M1.HighCol do
        Value := Value + (M1[Row1, K] * M2[K, Col2]);

      Result[Row1, Col2] := Value;
    end;
  end;
end;

class operator TCHXMatrixType.*(const M: TCHXMatrixType; const Factor: TRealType): TCHXMatrixType;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] * Factor;
end;

class operator TCHXMatrixType.*(const Factor: TRealType; const M: TCHXMatrixType): TCHXMatrixType;
begin
  // Commutative property for scalar multiplication
  Result := M * Factor;
end;

class operator TCHXMatrixType./(const M: TCHXMatrixType; const Factor: TRealType): TCHXMatrixType;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if Factor is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] / Factor;
end;

class operator TCHXMatrixType./(const Factor: TRealType; const M: TCHXMatrixType): TCHXMatrixType;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if any item is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := Factor / M[Row, Col];
end;

function CHXMatrix2x2(const a00, a01, a10, a11: TRealType): TCHXMatrixType;
begin
  Result.Init2x2(a00, a01, a10, a11);
end;

function CHXMatrix3x3(const a00, a01, a02, a10, a11, a12, a20, a21, a22: TRealType)
  : TCHXMatrixType;
begin
  Result.Init3x3(a00, a01, a02, a10, a11, a12, a20, a21, a22);
end;

function CHXMatrix4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
  a22, a23, a30, a31, a32, a33: TRealType): TCHXMatrixType;
begin
  Result.Init4x4(a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, 
    a30, a31, a32, a33);
end;


end.
