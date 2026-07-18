unit uCHXMatrix;
{< Unit of custom dimension matrix records.

  Four types of matrices are implemented, for all basic float types:
    - @code(TCHXMatrixS) for Single.
    - @code(TCHXMatrixD) for Double.
    - @code(TCHXMatrixE) for Extended.
    - @code(TCHXMatrixR) for Real.

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
  - Create TCHXMatrixC for Complex Numbers
  - Use a generic type..., but maybe we need helpers for some methods.

  Copyright (C) 2024-2026 Chixpy https://github.com/Chixpy
}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline ON}

interface

uses
  SysUtils, Math;

resourcestring
  krsAddition = 'Addition';
  krsSubtraction = 'Substraction';
  krsMultiplication = 'Multiplication';
  krsDimensError = '%s: Matrix dimension error.';

type
  { 
    Matrix with Single type data.
  }
  TCHXMatrixS = record
  type
    TRow = array of Single;
    TData = array of TRow;

  private
    function GetItem(const Row, Col: Integer): Single; inline;
    //< Internal getter for Items property.
    procedure SetItem(const Row, Col: Integer; const Value: Single); inline;
    //< Internal setter for Items property.

  public
    Data: TData;
    //< Actual field with matrix data.

    property Items[const Row, Col: Integer]: Single read GetItem write SetItem;
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
    procedure Init2x2(const a00, a01, a10, a11: Single);
    //< Init a 2x2 matrix.
    procedure Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
      a22: Single);
    //< Init a 3x3 matrix.
    procedure Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
      a22, a23, a30, a31, a32, a33: Single);
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

    function GetRow(const Row: Integer): TCHXMatrixS;
    //< Get a matrix Row.
    function GetCol(const Col: Integer): TCHXMatrixS;
    //< Get a matrix Column.
    procedure SetRow(const Row: Integer; const RowData: TRow);
    //< Set a matrix Row.
    procedure SetCol(const Col: Integer; const ColData: TCHXMatrixS);
    //< Set a matrix Column.

    function IsSquare: Boolean; inline;
    //< Test if a matrix is square.
    function IsEmpty: Boolean; inline;
    //< Test if a matrix is empty (Dimension = 0)
    function IsZero: Boolean;
    //< Test if all elements are 0.

    function Transpose: TCHXMatrixS;
    //< Transpose
    procedure SwapRows(const Row1, Row2: Integer);
    //< Swap two rows.
    procedure SwapCols(const Col1, Col2: Integer);
    //< Swap two columns.
    
    function MinorMatrix(const Row, Col: Integer): TCHXMatrixS;
    //< Submatrix created by removing one row and one column from the matrix.
    function SubMatrix(const FirstRow, FirstCol, LastRow, 
      LastCol: Integer): TCHXMatrixS;
    //< Submatrix created from a range of rows and columns.
    function Adjugate: TCHXMatrixS;
    //< Adjugate matrix (matrix of minors/cofactors).
    function Determinant: Single;
    //< Determinant of the matrix.
    function Rank: Integer;
    //< Rank of the matrix.
    function Inverse: TCHXMatrixS;

    procedure InitRot3DXY(const Angle: Single);
    //< Create a 3D rotation matrix.
    procedure InitRot3DXZ(const Angle: Single);
    //< Create a 3D rotation matrix.
    procedure InitRot3DYZ(const Angle: Single);
    //< Create a 3D rotation matrix.

    function SolveLinear: TCHXMatrixS;

    class operator Copy(constref Src: TCHXMatrixS; var Dst: TCHXMatrixS);
    //< Assignment operator overload (:=) for deep copying.
    class operator =(const M1, M2: TCHXMatrixS): Boolean;
    //< Comparing operator overload (=). "<>" is autocreated by FPC.
    class operator +(const M1, M2: TCHXMatrixS): TCHXMatrixS;
    class operator -(const M1, M2: TCHXMatrixS): TCHXMatrixS;
    class operator -(const M: TCHXMatrixS): TCHXMatrixS;
    // Unary minus (Negate matrix elements)
    class operator *(const M1, M2: TCHXMatrixS): TCHXMatrixS;
    class operator *(const M: TCHXMatrixS; const Factor: Single): TCHXMatrixS;
    class operator *(const Factor: Single; const M: TCHXMatrixS): TCHXMatrixS;
    class operator /(const M: TCHXMatrixS; const Factor: Single): TCHXMatrixS;
    // Matrix / Factor.
    class operator /(const Factor: Single; const M: TCHXMatrixS): TCHXMatrixS;
    // ToDo: Impossible operator overload
    // Factor / Matrix (¿It has use?).
  end; 

  { 
    Matrix with Double type data.
  }
  TCHXMatrixD = record
  type
    TRow = array of Double;
    TData = array of TRow;

  private
    function GetItem(const Row, Col: Integer): Double; inline;
    //< Internal getter for Items property.
    procedure SetItem(const Row, Col: Integer; const Value: Double); inline;
    //< Internal setter for Items property.

  public
    Data: TData;
    //< Actual field with matrix data.

    property Items[const Row, Col: Integer]: Double read GetItem write SetItem;
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
    procedure Init2x2(const a00, a01, a10, a11: Double);
    //< Init a 2x2 matrix.
    procedure Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
      a22: Double);
    //< Init a 3x3 matrix.
    procedure Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
      a22, a23, a30, a31, a32, a33: Double);
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

    function GetRow(const Row: Integer): TCHXMatrixD;
    //< Get a matrix Row.
    function GetCol(const Col: Integer): TCHXMatrixD;
    //< Get a matrix Column.
    procedure SetRow(const Row: Integer; const RowData: TRow);
    //< Set a matrix Row.
    procedure SetCol(const Col: Integer; const ColData: TCHXMatrixD);
    //< Set a matrix Column.

    function IsSquare: Boolean; inline;
    //< Test if a matrix is square.
    function IsEmpty: Boolean; inline;
    //< Test if a matrix is empty (Dimension = 0)
    function IsZero: Boolean;
    //< Test if all elements are 0.

    function Transpose: TCHXMatrixD;
    //< Transpose
    procedure SwapRows(const Row1, Row2: Integer);
    //< Swap two rows.
    procedure SwapCols(const Col1, Col2: Integer);
    //< Swap two columns.
    
    function MinorMatrix(const Row, Col: Integer): TCHXMatrixD;
    //< Submatrix created by removing one row and one column from the matrix.
    function SubMatrix(const FirstRow, FirstCol, LastRow, 
      LastCol: Integer): TCHXMatrixD;
    //< Submatrix created from a range of rows and columns.
    function Adjugate: TCHXMatrixD;
    //< Adjugate matrix (matrix of minors/cofactors).
    function Determinant: Double;
    //< Determinant of the matrix.
    function Rank: Integer;
    //< Rank of the matrix.
    function Inverse: TCHXMatrixD;

    procedure InitRot3DXY(const Angle: Double);
    //< Create a 3D rotation matrix.
    procedure InitRot3DXZ(const Angle: Double);
    //< Create a 3D rotation matrix.
    procedure InitRot3DYZ(const Angle: Double);
    //< Create a 3D rotation matrix.

    function SolveLinear: TCHXMatrixD;

    class operator Copy(constref Src: TCHXMatrixD; var Dst: TCHXMatrixD);
    //< Assignment operator overload (:=) for deep copying.
    class operator =(const M1, M2: TCHXMatrixD): Boolean;
    //< Comparing operator overload (=). "<>" is autocreated by FPC.
    class operator +(const M1, M2: TCHXMatrixD): TCHXMatrixD;
    class operator -(const M1, M2: TCHXMatrixD): TCHXMatrixD;
    class operator -(const M: TCHXMatrixD): TCHXMatrixD;
    // Unary minus (Negate matrix elements)
    class operator *(const M1, M2: TCHXMatrixD): TCHXMatrixD;
    class operator *(const M: TCHXMatrixD; const Factor: Double): TCHXMatrixD;
    class operator *(const Factor: Double; const M: TCHXMatrixD): TCHXMatrixD;
    class operator /(const M: TCHXMatrixD; const Factor: Double): TCHXMatrixD;
    // Matrix / Factor.
    class operator /(const Factor: Double; const M: TCHXMatrixD): TCHXMatrixD;
    // Factor / Matrix (¿It has use?).
  end; 

  { 
    Matrix with Extended type data.
  }
  TCHXMatrixE = record
  type
    TRow = array of Extended;
    TData = array of TRow;

  private
    function GetItem(const Row, Col: Integer): Extended; inline;
    //< Internal getter for Items property.
    procedure SetItem(const Row, Col: Integer; const Value: Extended); inline;
    //< Internal setter for Items property.

  public
    Data: TData;
    //< Actual field with matrix data.

    property Items[const Row, Col: Integer]: Extended read GetItem 
      write SetItem; default;
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
    procedure Init2x2(const a00, a01, a10, a11: Extended);
    //< Init a 2x2 matrix.
    procedure Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
      a22: Extended);
    //< Init a 3x3 matrix.
    procedure Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
      a22, a23, a30, a31, a32, a33: Extended);
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

    function GetRow(const Row: Integer): TCHXMatrixE;
    //< Get a matrix Row.
    function GetCol(const Col: Integer): TCHXMatrixE;
    //< Get a matrix Column.
    procedure SetRow(const Row: Integer; const RowData: TRow);
    //< Set a matrix Row.
    procedure SetCol(const Col: Integer; const ColData: TCHXMatrixE);
    //< Set a matrix Column.

    function IsSquare: Boolean; inline;
    //< Test if a matrix is square.
    function IsEmpty: Boolean; inline;
    //< Test if a matrix is empty (Dimension = 0)
    function IsZero: Boolean;
    //< Test if all elements are 0.

    function Transpose: TCHXMatrixE;
    //< Transpose
    procedure SwapRows(const Row1, Row2: Integer);
    //< Swap two rows.
    procedure SwapCols(const Col1, Col2: Integer);
    //< Swap two columns.
    
    function MinorMatrix(const Row, Col: Integer): TCHXMatrixE;
    //< Submatrix created by removing one row and one column from the matrix.
    function SubMatrix(const FirstRow, FirstCol, LastRow, 
      LastCol: Integer): TCHXMatrixE;
    //< Submatrix created from a range of rows and columns.
    function Adjugate: TCHXMatrixE;
    //< Adjugate matrix (matrix of minors/cofactors).
    function Determinant: Extended;
    //< Determinant of the matrix.
    function Rank: Integer;
    //< Rank of the matrix.
    function Inverse: TCHXMatrixE;

    procedure InitRot3DXY(const Angle: Extended);
    //< Create a 3D rotation matrix.
    procedure InitRot3DXZ(const Angle: Extended);
    //< Create a 3D rotation matrix.
    procedure InitRot3DYZ(const Angle: Extended);
    //< Create a 3D rotation matrix.

    function SolveLinear: TCHXMatrixE;

    class operator Copy(constref Src: TCHXMatrixE; var Dst: TCHXMatrixE);
    //< Assignment operator overload (:=) for deep copying.
    class operator =(const M1, M2: TCHXMatrixE): Boolean;
    //< Comparing operator overload (=). "<>" is autocreated by FPC.
    class operator +(const M1, M2: TCHXMatrixE): TCHXMatrixE;
    class operator -(const M1, M2: TCHXMatrixE): TCHXMatrixE;
    class operator -(const M: TCHXMatrixE): TCHXMatrixE;
    // Unary minus (Negate matrix elements)
    class operator *(const M1, M2: TCHXMatrixE): TCHXMatrixE;
    class operator *(const M: TCHXMatrixE; const Factor: Extended): TCHXMatrixE;
    class operator *(const Factor: Extended; const M: TCHXMatrixE): TCHXMatrixE;
    class operator /(const M: TCHXMatrixE; const Factor: Extended): TCHXMatrixE;
    // Matrix / Factor.
    class operator /(const Factor: Extended; const M: TCHXMatrixE): TCHXMatrixE;
    // Factor / Matrix (¿It has use?).
  end; 

  { 
    Matrix with Real type data.
  }
  TCHXMatrixR = record
  type
    TRow = array of Real;
    TData = array of TRow;

  private
    function GetItem(const Row, Col: Integer): Real; inline;
    //< Internal getter for Items property.
    procedure SetItem(const Row, Col: Integer; const Value: Real); inline;
    //< Internal setter for Items property.

  public
    Data: TData;
    //< Actual field with matrix data.

    property Items[const Row, Col: Integer]: Real read GetItem write SetItem;
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
    procedure Init2x2(const a00, a01, a10, a11: Real);
    //< Init a 2x2 matrix.
    procedure Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, a22: Real);
    //< Init a 3x3 matrix.
    procedure Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, 
      a22, a23, a30, a31, a32, a33: Real);
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

    function GetRow(const Row: Integer): TCHXMatrixR;
    //< Get a matrix Row.
    function GetCol(const Col: Integer): TCHXMatrixR;
    //< Get a matrix Column.
    procedure SetRow(const Row: Integer; const RowData: TRow);
    //< Set a matrix Row.
    procedure SetCol(const Col: Integer; const ColData: TCHXMatrixR);
    //< Set a matrix Column.

    function IsSquare: Boolean; inline;
    //< Test if a matrix is square.
    function IsEmpty: Boolean; inline;
    //< Test if a matrix is empty (Dimension = 0)
    function IsZero: Boolean;
    //< Test if all elements are 0.

    function Transpose: TCHXMatrixR;
    //< Transpose
    procedure SwapRows(const Row1, Row2: Integer);
    //< Swap two rows.
    procedure SwapCols(const Col1, Col2: Integer);
    //< Swap two columns.
    
    function MinorMatrix(const Row, Col: Integer): TCHXMatrixR;
    //< Submatrix created by removing one row and one column from the matrix.
    function SubMatrix(const FirstRow, FirstCol, LastRow, 
      LastCol: Integer): TCHXMatrixR;
    //< Submatrix created from a range of rows and columns.
    function Adjugate: TCHXMatrixR;
    //< Adjugate matrix (matrix of minors/cofactors).
    function Determinant: Real;
    //< Determinant of the matrix.
    function Rank: Integer;
    //< Rank of the matrix.
    function Inverse: TCHXMatrixR;

    procedure InitRot3DXY(const Angle: Real);
    //< Create a 3D rotation matrix.
    procedure InitRot3DXZ(const Angle: Real);
    //< Create a 3D rotation matrix.
    procedure InitRot3DYZ(const Angle: Real);
    //< Create a 3D rotation matrix.

    function SolveLinear: TCHXMatrixR;

    class operator Copy(constref Src: TCHXMatrixR; var Dst: TCHXMatrixR);
    //< Assignment operator overload (:=) for deep copying.
    class operator =(const M1, M2: TCHXMatrixR): Boolean;
    //< Comparing operator overload (=). "<>" is autocreated by FPC.
    class operator +(const M1, M2: TCHXMatrixR): TCHXMatrixR;
    class operator -(const M1, M2: TCHXMatrixR): TCHXMatrixR;
    class operator -(const M: TCHXMatrixR): TCHXMatrixR;
    // Unary minus (Negate matrix elements)
    class operator *(const M1, M2: TCHXMatrixR): TCHXMatrixR;
    class operator *(const M: TCHXMatrixR; const Factor: Real): TCHXMatrixR;
    class operator *(const Factor: Real; const M: TCHXMatrixR): TCHXMatrixR;
    class operator /(const M: TCHXMatrixR; const Factor: Real): TCHXMatrixR;
    // Matrix / Factor.
    class operator /(const Factor: Real; const M: TCHXMatrixR): TCHXMatrixR;
    // Factor / Matrix (¿It has use?).
  end; 

implementation

{ TCHXMatrixS }

function TCHXMatrixS.GetItem(const Row, Col: Integer): Single; inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Result := Data[Row, Col];
end;

procedure TCHXMatrixS.SetItem(const Row, Col: Integer; 
  const Value: Single); inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Data[Row, Col] := Value;
end;

procedure TCHXMatrixS.Init(const Rows, Cols: Integer; 
  const ForceClear: Boolean);
begin
  if ForceClear then
    SetLength(Data, 0);

  SetLength(Data, Rows, Cols);
end;

procedure TCHXMatrixS.InitSqr(const Rank: Integer; const ForceClear: Boolean);
begin
  Self.Init(Rank, Rank, ForceClear);
end;

procedure TCHXMatrixS.InitIdentity(const Rank: Integer);
var
  aPos: Integer;
begin
  Self.Init(Rank, Rank, True);
  for aPos := 0 to HighRow do
    Data[aPos, aPos] := 1.0;
end;

procedure TCHXMatrixS.Init2x2(const a00, a01, a10, a11: Single);
begin
  Self.InitSqr(2, False);
  Data[0, 0] := a00; Data[0, 1] := a01;
  Data[1, 0] := a10; Data[1, 1] := a11;
end;

procedure TCHXMatrixS.Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
  a22: Single);
begin
  Self.InitSqr(3, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22;
end;

procedure TCHXMatrixS.Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, 
  a20, a21, a22, a23, a30, a31, a32, a33: Single);
begin
  Self.InitSqr(4, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02; Data[0, 3] := a03;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12; Data[1, 3] := a13;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22; Data[2, 3] := a23;
  Data[3, 0] := a30; Data[3, 1] := a31; Data[3, 2] := a32; Data[3, 3] := a33;
end;

procedure TCHXMatrixS.Resize(const Rows, Cols: Integer; 
  const ForceClear: Boolean);
begin
  Self.Init(Rows, Cols, ForceClear);
end;

function TCHXMatrixS.RowCount: Integer;
begin
  Result := Length(Data);
end;

function TCHXMatrixS.HighRow: Integer;
begin
  Result := High(Data);
end;

function TCHXMatrixS.ColCount(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(0);
  Result := Length(Data[Row]);
end;

function TCHXMatrixS.HighCol(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(-1);
  Result := High(Data[Row]);
end;

function TCHXMatrixS.GetRow(const Row: Integer): TCHXMatrixS;
begin
  Result.Init(1, ColCount, False); // Initializing Result structure
  Result.Data[0] := Copy(Self.Data[Row]); // Deep copy of the row data
end;

function TCHXMatrixS.GetCol(const Col: Integer): TCHXMatrixS;
var
  Row: Integer;
begin
  Result.Init(RowCount, 1, False);
  // Iterating through rows to extract the specific column value.
  for Row := 0 to HighRow do
    Result[Row, 0] := Data[Row, Col];
end;

procedure TCHXMatrixS.SetRow(const Row: Integer; const RowData: TRow);
begin
  // ToDo: Validate that RowData has the correct number of columns.
  Data[Row] := RowData;
end;

procedure TCHXMatrixS.SetCol(const Col: Integer; const ColData: TCHXMatrixS);
var
  Row: Integer;
begin
  // ToDo: Validate that ColData has the correct number of rows.
  for Row := 0 to HighRow do
    Data[Row, Col] := ColData[Row, 0];
end;

function TCHXMatrixS.IsSquare: Boolean;
begin
  Result := RowCount = ColCount;
end;

function TCHXMatrixS.IsEmpty: Boolean;
begin
  Result := RowCount <= 0;
end;

function TCHXMatrixS.IsZero: Boolean;
var
  Row, Col: Integer;
begin
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      if not Math.IsZero(Data[Col, Row]) then
        Exit(False);
  Result := True;
end;

function TCHXMatrixS.Transpose: TCHXMatrixS;
var
  Row, Col: Integer;
begin
  Result.Init(ColCount, RowCount, False);
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      Result[Col, Row] := Data[Row, Col];
end;

procedure TCHXMatrixS.SwapRows(const Row1, Row2: Integer);
var
  Temp: TRow;
begin
  // Direct pointer swapping for maximum efficiency
  Temp := Data[Row1];
  Data[Row1] := Data[Row2];
  Data[Row2] := Temp;
end;

procedure TCHXMatrixS.SwapCols(const Col1, Col2: Integer);
var
  Row: Integer;
  Temp: Single;
begin
  for Row := 0 to HighRow do
  begin
    Temp := Data[Row, Col1];
    Data[Row, Col1] := Data[Row, Col2];
    Data[Row, Col2] := Temp;
  end;
end;

function TCHXMatrixS.MinorMatrix(const Row, Col: Integer): TCHXMatrixS;
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

function TCHXMatrixS.SubMatrix(const FirstRow, FirstCol, LastRow, 
  LastCol: Integer): TCHXMatrixS;
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

function TCHXMatrixS.Adjugate: TCHXMatrixS;
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

function TCHXMatrixS.Determinant: Single;
var
  Row: Integer;
  Elem: Single;
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

function TCHXMatrixS.Rank: Integer;
var
  M: TCHXMatrixS;
  PivotCol, MaxRow, Row, Col: Integer;
  MaxVal, CurrVal, Factor: Single;
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
    if Math.IsZero(MaxVal) then
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

function TCHXMatrixS.Inverse: TCHXMatrixS;
var
  M: TCHXMatrixS;
  PivotPos, Row, Col, TempI: Integer;
  PivotVal, Factor, TempT: Single;
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
    if Math.IsZero(PivotVal) then
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

procedure TCHXMatrixS.InitRot3DXY(const Angle: Single);
var
  aSin, aCos: Single;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, -aSin, 0, aSin, aCos, 0, 0, 0, 1);
end;

procedure TCHXMatrixS.InitRot3DXZ(const Angle: Single);
var
  aSin, aCos: Single;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, 0, aSin, 0, 1, 0, -aSin, 0, aCos);
end;

procedure TCHXMatrixS.InitRot3DYZ(const Angle: Single);
var
  aSin, aCos: Single;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(1, 0, 0, 0, aCos, -aSin, 0, aSin, aCos);
end;

function TCHXMatrixS.SolveLinear: TCHXMatrixS;
var
  M: TCHXMatrixS;
  PivotPos, Row, Col: Integer;
  PivotVal, Factor: Single;
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
    while Math.IsZero(PivotVal) and (Row < M.HighRow) do
    begin
      Inc(Row);
      PivotVal := M[Row, PivotPos];
    end;

    // If pivot is zero, the system doesn't have a unique solution
    if Math.IsZero(PivotVal) then
    begin
      Result.Init(0, 0, False); // Return empty matrix
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

class operator TCHXMatrixS.Copy(constref Src: TCHXMatrixS;
  var Dst: TCHXMatrixS);
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
    Move(Src.Data[i][0], Dst.Data[i][0], Length(Src.Data[i]) * SizeOf(Single));
  end;
end;

class operator TCHXMatrixS.=(const M1, M2: TCHXMatrixS): Boolean;
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

class operator TCHXMatrixS.+(const M1, M2: TCHXMatrixS): TCHXMatrixS;
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

class operator TCHXMatrixS.-(const M1, M2: TCHXMatrixS): TCHXMatrixS;
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

class operator TCHXMatrixS.-(const M: TCHXMatrixS): TCHXMatrixS;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := -M[Row, Col];
end;

class operator TCHXMatrixS.*(const M1, M2: TCHXMatrixS): TCHXMatrixS;
var
  Row1, Col2, K: Integer;
  Value: Single;
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

class operator TCHXMatrixS.*(const M: TCHXMatrixS; 
  const Factor: Single): TCHXMatrixS;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] * Factor;
end;

class operator TCHXMatrixS.*(const Factor: Single; const M: TCHXMatrixS)
  : TCHXMatrixS;
begin
  // Commutative property for scalar multiplication
  Result := M * Factor;
end;

class operator TCHXMatrixS./(const M: TCHXMatrixS; const Factor: Single)
  : TCHXMatrixS;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if Factor is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] / Factor;
end;

class operator TCHXMatrixS./(const Factor: Single; const M: TCHXMatrixS)
  : TCHXMatrixS;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if any item is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := Factor / M[Row, Col];
end;

{ TCHXMatrixD }

function TCHXMatrixD.GetItem(const Row, Col: Integer): Double; inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Result := Data[Row, Col];
end;

procedure TCHXMatrixD.SetItem(const Row, Col: Integer; const Value: Double); inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Data[Row, Col] := Value;
end;

procedure TCHXMatrixD.Init(const Rows, Cols: Integer; const ForceClear: Boolean);
begin
  if ForceClear then
    SetLength(Data, 0);

  SetLength(Data, Rows, Cols);
end;

procedure TCHXMatrixD.InitSqr(const Rank: Integer; const ForceClear: Boolean);
begin
  Self.Init(Rank, Rank, ForceClear);
end;

procedure TCHXMatrixD.InitIdentity(const Rank: Integer);
var
  aPos: Integer;
begin
  Self.Init(Rank, Rank, True);
  for aPos := 0 to HighRow do
    Data[aPos, aPos] := 1; // Assuming implicit conversion or Double initialization
end;

procedure TCHXMatrixD.Init2x2(const a00, a01, a10, a11: Double);
begin
  Self.InitSqr(2, False);
  Data[0, 0] := a00; Data[0, 1] := a01;
  Data[1, 0] := a10; Data[1, 1] := a11;
end;

procedure TCHXMatrixD.Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
  a22: Double);
begin
  Self.InitSqr(3, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22;
end;

procedure TCHXMatrixD.Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, 
  a20, a21, a22, a23, a30, a31, a32, a33: Double);
begin
  Self.InitSqr(4, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02; Data[0, 3] := a03;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12; Data[1, 3] := a13;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22; Data[2, 3] := a23;
  Data[3, 0] := a30; Data[3, 1] := a31; Data[3, 2] := a32; Data[3, 3] := a33;
end;

procedure TCHXMatrixD.Resize(const Rows, Cols: Integer; 
  const ForceClear: Boolean);
begin
  Self.Init(Rows, Cols, ForceClear);
end;

function TCHXMatrixD.RowCount: Integer;
begin
  Result := Length(Data);
end;

function TCHXMatrixD.HighRow: Integer;
begin
  Result := High(Data);
end;

function TCHXMatrixD.ColCount(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(0);
  Result := Length(Data[Row]);
end;

function TCHXMatrixD.HighCol(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(-1);
  Result := High(Data[Row]);
end;

function TCHXMatrixD.GetRow(const Row: Integer): TCHXMatrixD;
begin
  Result.Init(1, ColCount, False); // Initializing Result structure
  Result.Data[0] := Copy(Self.Data[Row]); // Deep copy of the row data
end;

function TCHXMatrixD.GetCol(const Col: Integer): TCHXMatrixD;
var
  Row: Integer;
begin
  Result.Init(RowCount, 1, False);
  // Iterating through rows to extract the specific column value.
  for Row := 0 to HighRow do
    Result[Row, 0] := Data[Row, Col];
end;

procedure TCHXMatrixD.SetRow(const Row: Integer; const RowData: TRow);
begin
  // ToDo: Validate that RowData has the correct number of columns.
  Data[Row] := RowData;
end;

procedure TCHXMatrixD.SetCol(const Col: Integer; const ColData: TCHXMatrixD);
var
  Row: Integer;
begin
  // ToDo: Validate that ColData has the correct number of rows.
  for Row := 0 to HighRow do
    Data[Row, Col] := ColData[Row, 0];
end;

function TCHXMatrixD.IsSquare: Boolean;
begin
  Result := RowCount = ColCount;
end;

function TCHXMatrixD.IsEmpty: Boolean;
begin
  Result := RowCount <= 0;
end;

function TCHXMatrixD.IsZero: Boolean;
var
  Row, Col: Integer;
begin
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      if not Math.IsZero(Data[Col, Row]) then
        Exit(False);
  Result := True;
end;

function TCHXMatrixD.Transpose: TCHXMatrixD;
var
  Row, Col: Integer;
begin
  Result.Init(ColCount, RowCount, False);
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      Result[Col, Row] := Data[Row, Col];
end;

procedure TCHXMatrixD.SwapRows(const Row1, Row2: Integer);
var
  Temp: TRow;
begin
  // Direct pointer swapping for maximum efficiency
  Temp := Data[Row1];
  Data[Row1] := Data[Row2];
  Data[Row2] := Temp;
end;

procedure TCHXMatrixD.SwapCols(const Col1, Col2: Integer);
var
  Row: Integer;
  Temp: Double;
begin
  for Row := 0 to HighRow do
  begin
    Temp := Data[Row, Col1];
    Data[Row, Col1] := Data[Row, Col2];
    Data[Row, Col2] := Temp;
  end;
end;

function TCHXMatrixD.MinorMatrix(const Row, Col: Integer): TCHXMatrixD;
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

function TCHXMatrixD.SubMatrix(const FirstRow, FirstCol, LastRow, 
  LastCol: Integer): TCHXMatrixD;
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

function TCHXMatrixD.Adjugate: TCHXMatrixD;
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

function TCHXMatrixD.Determinant: Double;
var
  Row: Integer;
  Elem: Double;
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

function TCHXMatrixD.Rank: Integer;
var
  M: TCHXMatrixD;
  PivotCol, MaxRow, Row, Col: Integer;
  MaxVal, CurrVal, Factor: Double;
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
    if Math.IsZero(MaxVal) then
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

function TCHXMatrixD.Inverse: TCHXMatrixD;
var
  M: TCHXMatrixD;
  PivotPos, Row, Col, TempI: Integer;
  PivotVal, Factor, TempT: Double;
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
    if Math.IsZero(PivotVal) then
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

procedure TCHXMatrixD.InitRot3DXY(const Angle: Double);
var
  aSin, aCos: Double;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, -aSin, 0, aSin, aCos, 0, 0, 0, 1);
end;

procedure TCHXMatrixD.InitRot3DXZ(const Angle: Double);
var
  aSin, aCos: Double;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, 0, aSin, 0, 1, 0, -aSin, 0, aCos);
end;

procedure TCHXMatrixD.InitRot3DYZ(const Angle: Double);
var
  aSin, aCos: Double;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(1, 0, 0, 0, aCos, -aSin, 0, aSin, aCos);
end;

function TCHXMatrixD.SolveLinear: TCHXMatrixD;
var
  M: TCHXMatrixD;
  PivotPos, Row, Col: Integer;
  PivotVal, Factor: Double;
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
    while Math.IsZero(PivotVal) and (Row < M.HighRow) do
    begin
      Inc(Row);
      PivotVal := M[Row, PivotPos];
    end;

    // If pivot is zero, the system doesn't have a unique solution
    if Math.IsZero(PivotVal) then
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

class operator TCHXMatrixD.Copy(constref Src: TCHXMatrixD; 
  var Dst: TCHXMatrixD);
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
    Move(Src.Data[i][0], Dst.Data[i][0], Length(Src.Data[i]) * SizeOf(Double));
  end;
end;

class operator TCHXMatrixD.=(const M1, M2: TCHXMatrixD): Boolean;
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

class operator TCHXMatrixD.+(const M1, M2: TCHXMatrixD): TCHXMatrixD;
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

class operator TCHXMatrixD.-(const M1, M2: TCHXMatrixD): TCHXMatrixD;
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

class operator TCHXMatrixD.-(const M: TCHXMatrixD): TCHXMatrixD;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := -M[Row, Col];
end;

class operator TCHXMatrixD.*(const M1, M2: TCHXMatrixD): TCHXMatrixD;
var
  Row1, Col2, K: Integer;
  Value: Double;
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

class operator TCHXMatrixD.*(const M: TCHXMatrixD; const Factor: Double): TCHXMatrixD;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] * Factor;
end;

class operator TCHXMatrixD.*(const Factor: Double; const M: TCHXMatrixD): TCHXMatrixD;
begin
  // Commutative property for scalar multiplication
  Result := M * Factor;
end;

class operator TCHXMatrixD./(const M: TCHXMatrixD; const Factor: Double): TCHXMatrixD;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if Factor is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] / Factor;
end;

class operator TCHXMatrixD./(const Factor: Double; const M: TCHXMatrixD): TCHXMatrixD;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if any item is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := Factor / M[Row, Col];
end;

{ TCHXMatrixE }

function TCHXMatrixE.GetItem(const Row, Col: Integer): Extended; inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Result := Data[Row, Col];
end;

procedure TCHXMatrixE.SetItem(const Row, Col: Integer; const Value: Extended); inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Data[Row, Col] := Value;
end;

procedure TCHXMatrixE.Init(const Rows, Cols: Integer; const ForceClear: Boolean);
begin
  if ForceClear then
    SetLength(Data, 0);

  SetLength(Data, Rows, Cols);
end;

procedure TCHXMatrixE.InitSqr(const Rank: Integer; const ForceClear: Boolean);
begin
  Self.Init(Rank, Rank, ForceClear);
end;

procedure TCHXMatrixE.InitIdentity(const Rank: Integer);
var
  aPos: Integer;
begin
  Self.Init(Rank, Rank, True);
  for aPos := 0 to HighRow do
    Data[aPos, aPos] := 1; // Assuming implicit conversion or Extended initialization
end;

procedure TCHXMatrixE.Init2x2(const a00, a01, a10, a11: Extended);
begin
  Self.InitSqr(2, False);
  Data[0, 0] := a00; Data[0, 1] := a01;
  Data[1, 0] := a10; Data[1, 1] := a11;
end;

procedure TCHXMatrixE.Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
  a22: Extended);
begin
  Self.InitSqr(3, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22;
end;

procedure TCHXMatrixE.Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, 
  a20, a21, a22, a23, a30, a31, a32, a33: Extended);
begin
  Self.InitSqr(4, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02; Data[0, 3] := a03;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12; Data[1, 3] := a13;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22; Data[2, 3] := a23;
  Data[3, 0] := a30; Data[3, 1] := a31; Data[3, 2] := a32; Data[3, 3] := a33;
end;

procedure TCHXMatrixE.Resize(const Rows, Cols: Integer; 
  const ForceClear: Boolean);
begin
  Self.Init(Rows, Cols, ForceClear);
end;

function TCHXMatrixE.RowCount: Integer;
begin
  Result := Length(Data);
end;

function TCHXMatrixE.HighRow: Integer;
begin
  Result := High(Data);
end;

function TCHXMatrixE.ColCount(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(0);
  Result := Length(Data[Row]);
end;

function TCHXMatrixE.HighCol(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(-1);
  Result := High(Data[Row]);
end;

function TCHXMatrixE.GetRow(const Row: Integer): TCHXMatrixE;
begin
  Result.Init(1, ColCount, False); // Initializing Result structure
  Result.Data[0] := Copy(Self.Data[Row]); // Deep copy of the row data
end;

function TCHXMatrixE.GetCol(const Col: Integer): TCHXMatrixE;
var
  Row: Integer;
begin
  Result.Init(RowCount, 1, False);
  // Iterating through rows to extract the specific column value.
  for Row := 0 to HighRow do
    Result[Row, 0] := Data[Row, Col];
end;

procedure TCHXMatrixE.SetRow(const Row: Integer; const RowData: TRow);
begin
  // ToDo: Validate that RowData has the correct number of columns.
  Data[Row] := RowData;
end;

procedure TCHXMatrixE.SetCol(const Col: Integer; const ColData: TCHXMatrixE);
var
  Row: Integer;
begin
  // ToDo: Validate that ColData has the correct number of rows.
  for Row := 0 to HighRow do
    Data[Row, Col] := ColData[Row, 0];
end;

function TCHXMatrixE.IsSquare: Boolean;
begin
  Result := RowCount = ColCount;
end;

function TCHXMatrixE.IsEmpty: Boolean;
begin
  Result := RowCount <= 0;
end;

function TCHXMatrixE.IsZero: Boolean;
var
  Row, Col: Integer;
begin
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      if not Math.IsZero(Data[Col, Row]) then
        Exit(False);
  Result := True;
end;

function TCHXMatrixE.Transpose: TCHXMatrixE;
var
  Row, Col: Integer;
begin
  Result.Init(ColCount, RowCount, False);
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      Result[Col, Row] := Data[Row, Col];
end;

procedure TCHXMatrixE.SwapRows(const Row1, Row2: Integer);
var
  Temp: TRow;
begin
  // Direct pointer swapping for maximum efficiency
  Temp := Data[Row1];
  Data[Row1] := Data[Row2];
  Data[Row2] := Temp;
end;

procedure TCHXMatrixE.SwapCols(const Col1, Col2: Integer);
var
  Row: Integer;
  Temp: Extended;
begin
  for Row := 0 to HighRow do
  begin
    Temp := Data[Row, Col1];
    Data[Row, Col1] := Data[Row, Col2];
    Data[Row, Col2] := Temp;
  end;
end;

function TCHXMatrixE.MinorMatrix(const Row, Col: Integer): TCHXMatrixE;
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

function TCHXMatrixE.SubMatrix(const FirstRow, FirstCol, LastRow, 
  LastCol: Integer): TCHXMatrixE;
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

function TCHXMatrixE.Adjugate: TCHXMatrixE;
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

function TCHXMatrixE.Determinant: Extended;
var
  Row: Integer;
  Elem: Extended;
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

function TCHXMatrixE.Rank: Integer;
var
  M: TCHXMatrixE;
  PivotCol, MaxRow, Row, Col: Integer;
  MaxVal, CurrVal, Factor: Extended;
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
    if Math.IsZero(MaxVal) then
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

function TCHXMatrixE.Inverse: TCHXMatrixE;
var
  M: TCHXMatrixE;
  PivotPos, Row, Col, TempI: Integer;
  PivotVal, Factor, TempT: Extended;
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
    if Math.IsZero(PivotVal) then
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

procedure TCHXMatrixE.InitRot3DXY(const Angle: Extended);
var
  aSin, aCos: Extended;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, -aSin, 0, aSin, aCos, 0, 0, 0, 1);
end;

procedure TCHXMatrixE.InitRot3DXZ(const Angle: Extended);
var
  aSin, aCos: Extended;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, 0, aSin, 0, 1, 0, -aSin, 0, aCos);
end;

procedure TCHXMatrixE.InitRot3DYZ(const Angle: Extended);
var
  aSin, aCos: Extended;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(1, 0, 0, 0, aCos, -aSin, 0, aSin, aCos);
end;

function TCHXMatrixE.SolveLinear: TCHXMatrixE;
var
  M: TCHXMatrixE;
  PivotPos, Row, Col: Integer;
  PivotVal, Factor: Extended;
begin
  // Assumes Self is an augmented matrix [A | B] where the last column is B
  if Self.IsEmpty or (Self.ColCount < 2) then
  begin
    Result.Init(0, 0, False);
    Exit;
  end;

  M := (Self); 
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
    while Math.IsZero(PivotVal) and (Row < M.HighRow) do
    begin
      Inc(Row);
      PivotVal := M[Row, PivotPos];
    end;

    // If pivot is zero, the system doesn't have a unique solution
    if Math.IsZero(PivotVal) then
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

class operator TCHXMatrixE.Copy(constref Src: TCHXMatrixE;
  var Dst: TCHXMatrixE);
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
    Move(Src.Data[i][0], Dst.Data[i][0], 
      Length(Src.Data[i]) * SizeOf(Extended));
  end;
end;

class operator TCHXMatrixE.=(const M1, M2: TCHXMatrixE): Boolean;
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

class operator TCHXMatrixE.+(const M1, M2: TCHXMatrixE): TCHXMatrixE;
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

class operator TCHXMatrixE.-(const M1, M2: TCHXMatrixE): TCHXMatrixE;
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

class operator TCHXMatrixE.-(const M: TCHXMatrixE): TCHXMatrixE;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := -M[Row, Col];
end;

class operator TCHXMatrixE.*(const M1, M2: TCHXMatrixE): TCHXMatrixE;
var
  Row1, Col2, K: Integer;
  Value: Extended;
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

class operator TCHXMatrixE.*(const M: TCHXMatrixE; const Factor: Extended): TCHXMatrixE;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] * Factor;
end;

class operator TCHXMatrixE.*(const Factor: Extended; const M: TCHXMatrixE): TCHXMatrixE;
begin
  // Commutative property for scalar multiplication
  Result := M * Factor;
end;

class operator TCHXMatrixE./(const M: TCHXMatrixE; const Factor: Extended): TCHXMatrixE;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if Factor is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] / Factor;
end;

class operator TCHXMatrixE./(const Factor: Extended; const M: TCHXMatrixE): TCHXMatrixE;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if any item is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := Factor / M[Row, Col];
end;

{ TCHXMatrixR }

function TCHXMatrixR.GetItem(const Row, Col: Integer): Real; inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Result := Data[Row, Col];
end;

procedure TCHXMatrixR.SetItem(const Row, Col: Integer; const Value: Real); inline;
begin
  // Standard array access (Exception thrown by system if out of bounds)
  Data[Row, Col] := Value;
end;

procedure TCHXMatrixR.Init(const Rows, Cols: Integer; const ForceClear: Boolean);
begin
  if ForceClear then
    SetLength(Data, 0);

  SetLength(Data, Rows, Cols);
end;

procedure TCHXMatrixR.InitSqr(const Rank: Integer; const ForceClear: Boolean);
begin
  Self.Init(Rank, Rank, ForceClear);
end;

procedure TCHXMatrixR.InitIdentity(const Rank: Integer);
var
  aPos: Integer;
begin
  Self.Init(Rank, Rank, True);
  for aPos := 0 to HighRow do
    Data[aPos, aPos] := 1; // Assuming implicit conversion or Real initialization
end;

procedure TCHXMatrixR.Init2x2(const a00, a01, a10, a11: Real);
begin
  Self.InitSqr(2, False);
  Data[0, 0] := a00; Data[0, 1] := a01;
  Data[1, 0] := a10; Data[1, 1] := a11;
end;

procedure TCHXMatrixR.Init3x3(const a00, a01, a02, a10, a11, a12, a20, a21, 
  a22: Real);
begin
  Self.InitSqr(3, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22;
end;

procedure TCHXMatrixR.Init4x4(const a00, a01, a02, a03, a10, a11, a12, a13, 
  a20, a21, a22, a23, a30, a31, a32, a33: Real);
begin
  Self.InitSqr(4, False);
  Data[0, 0] := a00; Data[0, 1] := a01; Data[0, 2] := a02; Data[0, 3] := a03;
  Data[1, 0] := a10; Data[1, 1] := a11; Data[1, 2] := a12; Data[1, 3] := a13;
  Data[2, 0] := a20; Data[2, 1] := a21; Data[2, 2] := a22; Data[2, 3] := a23;
  Data[3, 0] := a30; Data[3, 1] := a31; Data[3, 2] := a32; Data[3, 3] := a33;
end;

procedure TCHXMatrixR.Resize(const Rows, Cols: Integer; 
  const ForceClear: Boolean);
begin
  Self.Init(Rows, Cols, ForceClear);
end;

function TCHXMatrixR.RowCount: Integer;
begin
  Result := Length(Data);
end;

function TCHXMatrixR.HighRow: Integer;
begin
  Result := High(Data);
end;

function TCHXMatrixR.ColCount(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(0);
  Result := Length(Data[Row]);
end;

function TCHXMatrixR.HighCol(const Row: Integer): Integer;
begin
  if RowCount <= 0 then
    Exit(-1);
  Result := High(Data[Row]);
end;

function TCHXMatrixR.GetRow(const Row: Integer): TCHXMatrixR;
begin
  Result.Init(1, ColCount, False); // Initializing Result structure
  Result.Data[0] := Copy(Self.Data[Row]); // Deep copy of the row data
end;

function TCHXMatrixR.GetCol(const Col: Integer): TCHXMatrixR;
var
  Row: Integer;
begin
  Result.Init(RowCount, 1, False);
  // Iterating through rows to extract the specific column value.
  for Row := 0 to HighRow do
    Result[Row, 0] := Data[Row, Col];
end;

procedure TCHXMatrixR.SetRow(const Row: Integer; const RowData: TRow);
begin
  // ToDo: Validate that RowData has the correct number of columns.
  Data[Row] := RowData;
end;

procedure TCHXMatrixR.SetCol(const Col: Integer; const ColData: TCHXMatrixR);
var
  Row: Integer;
begin
  // ToDo: Validate that ColData has the correct number of rows.
  for Row := 0 to HighRow do
    Data[Row, Col] := ColData[Row, 0];
end;

function TCHXMatrixR.IsSquare: Boolean;
begin
  Result := RowCount = ColCount;
end;

function TCHXMatrixR.IsEmpty: Boolean;
begin
  Result := RowCount <= 0;
end;

function TCHXMatrixR.IsZero: Boolean;
var
  Row, Col: Integer;
begin
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      if not Math.IsZero(Data[Col, Row]) then
        Exit(False);
  Result := True;
end;

function TCHXMatrixR.Transpose: TCHXMatrixR;
var
  Row, Col: Integer;
begin
  Result.Init(ColCount, RowCount, False);
  for Row := 0 to HighRow do
    for Col := 0 to HighCol(Row) do
      Result[Col, Row] := Data[Row, Col];
end;

procedure TCHXMatrixR.SwapRows(const Row1, Row2: Integer);
var
  Temp: TRow;
begin
  // Direct pointer swapping for maximum efficiency
  Temp := Data[Row1];
  Data[Row1] := Data[Row2];
  Data[Row2] := Temp;
end;

procedure TCHXMatrixR.SwapCols(const Col1, Col2: Integer);
var
  Row: Integer;
  Temp: Real;
begin
  for Row := 0 to HighRow do
  begin
    Temp := Data[Row, Col1];
    Data[Row, Col1] := Data[Row, Col2];
    Data[Row, Col2] := Temp;
  end;
end;

function TCHXMatrixR.MinorMatrix(const Row, Col: Integer): TCHXMatrixR;
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

function TCHXMatrixR.SubMatrix(const FirstRow, FirstCol, LastRow, 
  LastCol: Integer): TCHXMatrixR;
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

function TCHXMatrixR.Adjugate: TCHXMatrixR;
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

function TCHXMatrixR.Determinant: Real;
var
  Row: Integer;
  Elem: Real;
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

function TCHXMatrixR.Rank: Integer;
var
  M: TCHXMatrixR;
  PivotCol, MaxRow, Row, Col: Integer;
  MaxVal, CurrVal, Factor: Real;
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
    if Math.IsZero(MaxVal) then
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

function TCHXMatrixR.Inverse: TCHXMatrixR;
var
  M: TCHXMatrixR;
  PivotPos, Row, Col, TempI: Integer;
  PivotVal, Factor, TempT: Real;
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
    if Math.IsZero(PivotVal) then
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

procedure TCHXMatrixR.InitRot3DXY(const Angle: Real);
var
  aSin, aCos: Real;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, -aSin, 0, aSin, aCos, 0, 0, 0, 1);
end;

procedure TCHXMatrixR.InitRot3DXZ(const Angle: Real);
var
  aSin, aCos: Real;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(aCos, 0, aSin, 0, 1, 0, -aSin, 0, aCos);
end;

procedure TCHXMatrixR.InitRot3DYZ(const Angle: Real);
var
  aSin, aCos: Real;
begin
  SinCos(Angle, aSin, aCos);
  Self.Init3x3(1, 0, 0, 0, aCos, -aSin, 0, aSin, aCos);
end;

function TCHXMatrixR.SolveLinear: TCHXMatrixR;
var
  M: TCHXMatrixR;
  PivotPos, Row, Col: Integer;
  PivotVal, Factor: Real;
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
    while Math.IsZero(PivotVal) and (Row < M.HighRow) do
    begin
      Inc(Row);
      PivotVal := M[Row, PivotPos];
    end;

    // If pivot is zero, the system doesn't have a unique solution
    if Math.IsZero(PivotVal) then
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

class operator TCHXMatrixR.Copy(constref Src: TCHXMatrixR; var Dst: TCHXMatrixR);
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
    Move(Src.Data[i][0], Dst.Data[i][0], Length(Src.Data[i]) * SizeOf(Real));
  end;
end;

class operator TCHXMatrixR.=(const M1, M2: TCHXMatrixR): Boolean;
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

class operator TCHXMatrixR.+(const M1, M2: TCHXMatrixR): TCHXMatrixR;
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

class operator TCHXMatrixR.-(const M1, M2: TCHXMatrixR): TCHXMatrixR;
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

class operator TCHXMatrixR.-(const M: TCHXMatrixR): TCHXMatrixR;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to Result.HighRow do
    for Col := 0 to Result.HighCol(Row) do
      Result[Row, Col] := -M[Row, Col];
end;

class operator TCHXMatrixR.*(const M1, M2: TCHXMatrixR): TCHXMatrixR;
var
  Row1, Col2, K: Integer;
  Value: Real;
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

class operator TCHXMatrixR.*(const M: TCHXMatrixR; const Factor: Real): TCHXMatrixR;
var
  Row, Col: Integer;
begin
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] * Factor;
end;

class operator TCHXMatrixR.*(const Factor: Real; const M: TCHXMatrixR): TCHXMatrixR;
begin
  // Commutative property for scalar multiplication
  Result := M * Factor;
end;

class operator TCHXMatrixR./(const M: TCHXMatrixR; const Factor: Real): TCHXMatrixR;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if Factor is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := M[Row, Col] / Factor;
end;

class operator TCHXMatrixR./(const Factor: Real; const M: TCHXMatrixR): TCHXMatrixR;
var
  Row, Col: Integer;
begin
  // Default system exception will trigger if any item is 0
  Result.Init(M.RowCount, M.ColCount, False);
  for Row := 0 to M.HighRow do
    for Col := 0 to M.HighCol(Row) do
      Result[Row, Col] := Factor / M[Row, Col];
end;

end.
