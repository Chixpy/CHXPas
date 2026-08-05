unit uCHXSDL3TypeHelpers;
{< Unit with helpers for SDL3 types, and some useful new types and methods:

  - **CHXSDLFSegment**: Struct with a pair of `SDL_FPoint`, sometimes it's
    useful store it's endpoint instead using `TSDL_FRect` and calculate
    end point every time adding `X + W` and `Y + H`.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$MODE ObjFPC}{$H+}
{$MODESWITCH AdvancedRecords}
{$MODESWITCH TypeHelpers}
{$INLINE ON}
interface

uses
  SysUtils, CTypes, Math, // FPC
  SDL3;

const
  kSigmaCFloat = 1e-4; //< Usual Sigma value for Single
  kInv255 = 0.00392156863; //< 1 / 255

type
{
  ### Types
}
{
  Struct with a pair of `SDL_FPoint`

  Sometimes it's useful store it's endpoint instead using `TSDL_FRect` and 
  calculate endpoint every time adding `X + W` and `Y + H`.
}
  TCHXSDLFSegment = packed record
  public
    P1, P2: TSDL_FPoint;

  {
    Init
  }

    procedure Init(const aP1, aP2: TSDL_FPoint); overload; inline;
    procedure Init(const X1, Y1, X2, Y2: CFloat); overload; inline;

  end;

{
  ### Dynamic lists.

  Useful for inheriting a create methods for batch operation.

  For example, rotate all points in the list, calculating sin and cos only
  one time for all.

  ToDo: Try Generics.Collections as FGL don't work and actually uses an array.
}
(*
  cSDLColorList = specialize TList<TSDL_FColor>;
  //< List of TSDL_FColor.
  cSDLFPointList = specialize TList<TSDL_FPoint>;
  //< List of TSDL_FPoint.
  cSDLFRectList = specialize TList<TSDL_FRect>;
  //< List of TSDL_FRect.
  cSDLVertexList = specialize TList<TSDL_Vertex>;
  //< List of TSDL_Vertex.
*)

{
  ### Dynamic Arrays.
}

  TSDLFColorDynArray = Array of TSDL_FColor;
  //< Dynamic array of TSDL_FColor.
  TSDLFPointDynArray = Array of TSDL_FPoint;
  //< Dynamic array of TSDL_FPoint.
  TSDLFRectDynArray = Array of TSDL_FRect;
  //< Dynamic array of TSDL_FRect.
  TSDLVertexDynArray = Array of TSDL_Vertex;
  //< Dynamic array of TSDL_Vertex.

{
  ### Helpers for SDL types
}

{
  Helper for TSDL_FColor
}

  TSDLFColorH = record helper for TSDL_FColor
  public
  {
    Init
  }

    procedure Init(const aR, aG, aB: CFloat; const aA: CFloat = 1); overload;
      inline;
    procedure Init(const Grey: CFloat; const aA: CFloat = 1); overload; inline;

    procedure InitByte(const aR, aG, aB: Byte; const aA: Byte = 255); overload;
      inline;
    procedure InitByte(const Grey: Byte; const aA: Byte = 255); overload;

  {
    Comparisons
  }

    function IsEqual(const aColor: TSDL_FColor): Boolean;

  {
    Operators (They can't be in a helper as FPC 3.3.1)
  }

    // operator = (const C1, C2: TSDL_FColor): Boolean; overload;

  {
    Strings
  }
    function ToString(const Delim : char = ','): String;
    function ToStringFmt(const aFmtStr: String): String;
    procedure FromString(const aString: String; const Delim : char = ',');
  end;

{
  Helper for TSDL_FPoint
}

  TSDLFPointH = record helper for TSDL_FPoint
  public
  {
    Init methods
  }

    procedure Init(const aX, aY: CFloat); inline;
    procedure InitPolar(const aMag, aAngle: CFloat); inline;
    procedure InitRandom(const MinX, MaxX, MinY, MaxY: CFloat); inline;
    procedure InitRandomPolar(const aMag: CFloat = 1); inline;

  {
    Polar data
  }

    function GetAngle: CFloat; inline;
    procedure SetAngle(const aAngle: CFloat); inline;

    function GetSqrMag: CFloat; inline; //< Square of the magnitude
    function GetMagnitude: CFloat; inline;
    procedure SetMagnitude(const aMag: CFloat);

  {
    Comparisons
  }

    function IsZero(const aEpsilon: CFloat = 0): Boolean; inline;
    function IsEqual(const P: TSDL_FPoint; const aEpsilon: CFloat = 0):
      Boolean; inline;
    function IsOpposite(const P: TSDL_FPoint; const aEpsilon: CFloat = 0):
      Boolean; inline;

  {
    Self operations
  }

    procedure Negate; //< Other posible names: Opposite or Invert
    procedure Add(const P: TSDL_FPoint); inline;
    procedure Subtract(const P: TSDL_FPoint); inline; //< Self - P
    procedure Multiply(const aScale: CFloat); inline;
    procedure CompScale(const P: TSDL_FPoint); inline;
    //< Scale by components
    procedure Divide(const aScale: CFloat); inline; //< Self / AScale
    procedure DivInv(const aScale: CFloat); inline; //< AScale / Self

  {
    Common operations (as Point or Vector)
  }

    procedure Normalize;
    function GetNormalized: TSDL_FPoint; inline;

    procedure Move(const dX, dY: CFloat); inline;
    procedure Scale(const sX, sY: CFloat); inline;
    procedure Rotate(const aAngle: CFloat);

    function VectProd(const P: TSDL_FPoint): CFloat; inline;
    function ScalProd(const P: TSDL_FPoint): CFloat; inline;

    function SqrDistance(const P: TSDL_FPoint): CFloat; inline;
    function Distance(const P: TSDL_FPoint): CFloat; inline;
    function InDistance(const P: TSDL_FPoint; const aDistance: CFloat;
      const IncEqual: Boolean = False): Boolean;
    function MidPoint(const P: TSDL_FPoint): TSDL_FPoint; inline;


    function Reflect(const aNormal: TSDL_FPoint): TSDL_FPoint; inline;
    function Refract(const aNormal: TSDL_FPoint; const RefIdx: CFloat)
      : TSDL_FPoint;

  {
    Conversion to integer coords and remainder.

    ToDo: Overload for TSDL_Point (except Frac)
  }

    function Ceil: TSDL_FPoint;
    //< Ceil(-2.3, 1.3) = -2, 2 -> +inf
    function Truncate: TSDL_FPoint;
    //< Truncate(-2.3, 1.3) = -2, 1 -> 0
    function Floor: TSDL_FPoint;
    //< Floor(-2.3, 1.3) = -3, 1 -> -inf
    function Round: TSDL_FPoint;
    function FracCeil: TSDL_FPoint;
    //< FloorFrac(-2.3, 1.3) = -0.3, -0.7 -> +inf
    function FracTrunc: TSDL_FPoint;
    //< Frac(-2.3, 1.3) = -0.3, 0.3 -> 0 -> 0
    function FracFloor: TSDL_FPoint;
    //< FloorFrac(-2.3, 1.3) = 0.7, 0.3 - -inf

  {
    Operators (They can't be in a helper as FPC 3.3.1)
  }

    // class operator = (const P1, P2: TSDL_FPoint): Boolean; inline;
    // class operator + (const P1, P2: TSDL_FPoint): TSDL_FPoint;
    // class operator - (const P1: TSDL_FPoint): TSDL_FPoint;
    // class operator - (const P1, P2: TSDL_FPoint): TSDL_FPoint;
    // class operator * (const P1, P2: TSDL_FPoint): TSDL_FPoint;
    // class operator * (const P1: TSDL_FPoint; const aFactor: CFloat)
    //   : TSDL_FPoint;
    // class operator * (const aFactor: CFloat; const P1: TSDL_FPoint)
    //   : TSDL_FPoint;
    // class operator / (const P1: TSDL_FPoint; const aFactor: CFloat)
    //   : TSDL_FPoint;
    // class operator / (const aFactor: CFloat; const P1: TSDL_FPoint)
    //   : TSDL_FPoint;

  {
    String
  }

    function ToString(const Delim : char = ','): String;
    function ToStringFmt(const aFmtStr: String): String;
    procedure FromString(const aString: String; const Delim : char = ',');
  end;

{
  Helper for TSDL_FRect
}

  TSDLFRectH = record helper for TSDL_FRect
  public
  {
    Init methods
  }

    procedure Init(const aX, aY, aW, aH: CFloat); overload; inline;

    procedure Shrink(const aSize: CFloat);

    // operator = (const R1, R2: TSDL_FRect): Boolean; overload; inline;

  {
    String
  }

    function ToString(const Delim : char = ','): String;
    function ToStringFmt(const aFmtStr: String): String;
    procedure FromString(const aString: String; const Delim : char = ',');
  end;

{
  ### Operator overloading.

  Ideally they would be in helpers as class operators...
}

{
  TSDL_FColor operators.
}

operator = (const C1, C2: TSDL_FColor): Boolean; overload;

{
  TSDL_FPoint operators.
}

operator = (const P1, P2: TSDL_FPoint): Boolean; overload; inline;
operator + (const P1, P2: TSDL_FPoint): TSDL_FPoint; overload; inline;
operator - (const P: TSDL_FPoint): TSDL_FPoint; overload; inline;
operator - (const P1, P2: TSDL_FPoint): TSDL_FPoint; overload; inline;
operator * (const P: TSDL_FPoint; const aFactor: CFloat): TSDL_FPoint; overload;
  inline;
operator * (const aFactor: CFloat; const P: TSDL_FPoint): TSDL_FPoint; overload;
  inline;
operator * (const P1, P2: TSDL_FPoint): TSDL_FPoint; overload; inline;
//< Component-wise scaling (Hadamard product)
operator / (const P: TSDL_FPoint; const aFactor: CFloat): TSDL_FPoint; overload;
  inline;

{
  TSDL_FRect operators.
}

operator = (const R1, R2: TSDL_FRect): Boolean; overload; inline;

{
  TSDL_Vertex operators.
}

operator = (const V1, V2: TSDL_Vertex): Boolean; overload; inline;

{
  ### Type creation functions.

  Useful to use them as parameters when calling a function without creating
  a temporal variable.
}

function SDLFColor(const R, G, B: CFloat; const A: CFloat = 1): TSDL_FColor;
  overload; inline;
{< Create a TSDL_FColor from Red, Green, Blue and Alpha values.}

function SDLFColor(const Grey: CFloat; const A: CFloat = 1): TSDL_FColor;
  overload; inline;
{< Create a TSDL_FColor with a Grey value.}

function SDLFPoint(const X, Y: CFloat): TSDL_FPoint; inline;
{< Create a TSDL_FPoint.}

function SDLFRect(const X, Y, W, H: CFloat): TSDL_FRect; inline;
{< Create a TSDL_FRect.}

function SDLFSegment(const aP1, aP2: TSDL_FPoint): TCHXSDLFSegment;
  overload; inline;
function SDLFSegment(const X1, Y1, X2, Y2: CFloat): TCHXSDLFSegment;
  overload; inline;

implementation

{
  TCHXSDLFSegment
}

procedure TCHXSDLFSegment.Init(const aP1, aP2: TSDL_FPoint);
begin
  Self.P1 := aP1; Self.P2 := aP2;
end;

procedure TCHXSDLFSegment.Init(const X1, Y1, X2, Y2: CFloat);
begin
  Self.P1.X := X1; Self.P1.Y := Y1;
  Self.P2.X := X2; Self.P2.Y := Y2;
end;

{
  TSDLFColorH
}

procedure TSDLFColorH.Init(const aR, aG, aB, aA: CFloat);
begin
  Self.R := aR; Self.G := aG; Self.B := aB; Self.A := aA;
end;

procedure TSDLFColorH.Init(const Grey, aA: CFloat);
begin
  Self.R := Grey; Self.G := Grey; Self.B := Grey; Self.A := aA;
end;

procedure TSDLFColorH.InitByte(const aR, aG, aB, aA: Byte);
begin
  Self.Init(aR * kInv255, aG * kInv255, aB * kInv255, aA * kInv255);
end;
procedure TSDLFColorH.InitByte(const Grey, aA: Byte);
var
 aGrey: CFloat;
begin
  aGrey := Grey * kInv255;
  Self.Init(aGrey, aGrey, aGrey, aA * kInv255);
end;

function TSDLFColorH.IsEqual(const aColor: TSDL_FColor): Boolean;
begin
  // If both are totally transparent are considered the same always.
  if IsZero(Self.A) and IsZero(aColor.A) then Exit(True);
  Result := SameValue(Self.R, aColor.R) and SameValue(Self.G, aColor.G) 
      and SameValue(Self.B, aColor.B) and SameValue(Self.A, aColor.A);
end;

function TSDLFColorH.ToString(const Delim : Char): String;
begin
  Result := Format('%0:g%4:s%1:g%4:s%2:g%4:s%3:g',
    [Self.R, Self.G, Self.B, Self.A, Delim]);
end;

function TSDLFColorH.ToStringFmt(const aFmtStr: String): String;
begin
  Result := Format(aFmtStr, [Self.R, Self.G, Self.B, Self.A]);
end;

procedure TSDLFColorH.FromString(const aString: String; const Delim : Char);
var
  Components: array of String;
begin
  Components := aString.Split(Delim);
  Self.Init(0, 0, 0, 1);

  // Lazy read
  if Length(Components) < 1 then Exit;
  Self.R := StrToFloat(Components[0]);
  if Length(Components) < 2 then Exit;
  Self.G := StrToFloat(Components[1]);
  if Length(Components) < 3 then Exit;
  Self.B := StrToFloat(Components[2]);
  if Length(Components) < 4 then Exit;
  Self.A := StrToFloat(Components[3]);
end;

{
  TSDLFPointH
}

procedure TSDLFPointH.Init(const aX, aY: CFloat);
begin
  Self.X := aX; Self.Y := aY;
end;

procedure TSDLFPointH.InitPolar(const aMag, aAngle: CFloat);
begin
  Self.X := aMag * Cos(aAngle); Self.Y := aMag * Sin(aAngle);
end;

procedure TSDLFPointH.InitRandom(const MinX, MaxX, MinY, MaxY: CFloat);
begin
  Self.X := Random * (MaxX - MinX) + MinX;
  Self.Y := Random * (MaxY - MinY) + MinY;
end;

procedure TSDLFPointH.InitRandomPolar(const aMag: CFloat);
begin
  Self.InitPolar(aMag, Random * 2 * Pi);
end;

function TSDLFPointH.GetAngle: CFloat;
begin
  Result := ArcTan2(Y, X);
end;

procedure TSDLFPointH.SetAngle(const aAngle: CFloat);
begin
  // ToDo: Test wich is more efficient:
  //   aMag := GetMagnitude;
  //   Self.InitPolar(aMag, aAngle);
  // or:
  Self.Rotate(aAngle - Self.GetAngle);
end;

function TSDLFPointH.GetSqrMag: CFloat;
begin
  Result := Self.X * Self.X + Self.Y * Self.Y;
end;

function TSDLFPointH.GetMagnitude: CFloat;
begin
  Result := SqRt(Self.GetSqrMag);
end;

procedure TSDLFPointH.SetMagnitude(const aMag: CFloat);
begin
  if Self.IsZero then Exit; // Keep at zero

  Self.Multiply(aMag / Self.GetMagnitude);
end;

function TSDLFPointH.IsZero(const aEpsilon: CFloat): Boolean;
begin
  // aEpsilon = 0 means default Epsilon
  Result := Math.IsZero(Self.X, aEpsilon) and Math.IsZero(Self.Y, aEpsilon);
end;

function TSDLFPointH.IsEqual(const P: TSDL_FPoint; const aEpsilon: CFloat):
  Boolean;
begin
  // aEpsilon = 0 means default Epsilon
  Result := SameValue(Self.X, P.X, aEpsilon)
    and SameValue(Self.Y, P.Y, aEpsilon);
end;

function TSDLFPointH.IsOpposite(const P: TSDL_FPoint; const aEpsilon: CFloat):
  Boolean;
begin
  // aEpsilon = 0 means default Epsilon
  Result := SameValue(Self.X, -P.X, aEpsilon)
    and SameValue(Self.Y, -P.Y, aEpsilon);
end;

procedure TSDLFPointH.Negate;
begin
  Self.X := -Self.X; Self.Y := -Self.Y;
end;

procedure TSDLFPointH.Add(const P: TSDL_FPoint);
begin
  Self.X += P.X; Self.Y += P.Y;
end;

procedure TSDLFPointH.Subtract(const P: TSDL_FPoint); //< Self - P
begin
  Self.X -= P.X; Self.Y -= P.Y;
end;

procedure TSDLFPointH.Multiply(const aScale: CFloat);
begin
  Self.X *= aScale; Self.Y *= aScale;
end;

procedure TSDLFPointH.CompScale(const P: TSDL_FPoint);
begin
  Self.X *= P.X; Self.Y *= P.Y;
end;

procedure TSDLFPointH.Divide(const aScale: CFloat);
begin
  Self.X /= aScale; Self.Y /= aScale;
end;

procedure TSDLFPointH.DivInv(const aScale: CFloat);
begin
  Self.X := aScale / Self.X; Self.Y := aScale / Self.Y;
end;

procedure TSDLFPointH.Normalize;
begin
  if Self.IsZero then Exit; // Keep at Zero

  // This way we only divide once
  Self.Multiply(1 / Self.GetMagnitude);
end;

function TSDLFPointH.GetNormalized: TSDL_FPoint;
begin
  Result := Self;
  Result.Normalize;
end;

procedure TSDLFPointH.Move(const dX, dY: CFloat);
begin
  Self.X += dX; Self.Y += dY;
end;

procedure TSDLFPointH.Scale(const sX, sY: CFloat);
begin
  Self.X *= sX; Self.Y *= sY;
end;

procedure TSDLFPointH.Rotate(const aAngle: CFloat);
var
  SinXY, CosXY, TempX: CFloat;
begin
  SinCos(aAngle, SinXY, CosXY);
  TempX := Self.X;
  Self.X := TempX * CosXY - Self.Y * SinXY;
  Self.Y := TempX * SinXY + Self.Y * CosXY;
end;

function TSDLFPointH.VectProd(const P: TSDL_FPoint): CFloat;
begin
  Result := Self.X * P.Y - Self.Y * P.X;
end;

function TSDLFPointH.ScalProd(const P: TSDL_FPoint): CFloat;
begin
  Result := Self.X * P.X + Self.Y * P.Y;
end;

function TSDLFPointH.SqrDistance(const P: TSDL_FPoint): CFloat;
begin
  Result := Sqr(Self.X - P.X) + Sqr(Self.Y - P.Y)
end;

function TSDLFPointH.Distance(const P: TSDL_FPoint): CFloat;
begin
  Result := SqRt(Self.SqrDistance(P));
end;

function TSDLFPointH.InDistance(const P: TSDL_FPoint; const aDistance: CFloat;
  const IncEqual: Boolean): Boolean;
var
  aDistSq: CFloat;
begin
  aDistSq := Self.SqrDistance(P);

  if aDistSq < (aDistance * aDistance) then
    Exit(True);

  Result := IncEqual and SameValue(SqRt(aDistSq), aDistance);
end;

function TSDLFPointH.MidPoint(const P: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := (Self.X + P.X) * 0.5; Result.Y := (Self.Y + P.Y) * 0.5;
end;

function TSDLFPointH.Reflect(const aNormal: TSDL_FPoint): TSDL_FPoint;
begin
  Result := Self + (-2 * Self.ScalProd(aNormal)) * aNormal;
end;

function TSDLFPointH.Refract(const aNormal: TSDL_FPoint; const RefIdx: CFloat)
  : TSDL_FPoint;
var
  CosI: CFloat;
  k: CFloat;
begin
  CosI := Self.ScalProd(aNormal);
  k := 1 - RefIdx * RefIdx * (1 - CosI * CosI);

  if k < 0 then
  begin
    Result.Init(0, 0);
  end
  else
  begin
    Result := (Self * RefIdx) - (aNormal * (RefIdx * CosI + SqRt(k)));
  end;
end;

function TSDLFPointH.Ceil: TSDL_FPoint;
begin
  Result.X := Math.Ceil(Self.X); Result.Y := Math.Ceil(Self.Y);
end;

function TSDLFPointH.Truncate: TSDL_FPoint;
begin
  Result.X := Trunc(Self.X); Result.Y := Trunc(Self.Y);
end;

function TSDLFPointH.Floor: TSDL_FPoint;
begin
  Result.X := Math.Floor(Self.X); Result.Y := Math.Floor(Self.Y);
end;

function TSDLFPointH.Round: TSDL_FPoint;
begin
  Result.X := System.Round(Self.X); Result.Y := System.Round(Self.Y);
end;

function TSDLFPointH.FracCeil: TSDL_FPoint;
begin
  Result.X := Self.X - Math.Ceil(Self.X);
  Result.Y := Self.Y - Math.Ceil(Self.Y);
end;

function TSDLFPointH.FracTrunc: TSDL_FPoint;
begin
  Result.X := Self.X - Trunc(Self.X); Result.Y := Self.Y - Trunc(Self.Y);
end;

function TSDLFPointH.FracFloor: TSDL_FPoint;
begin
  Result.X := Self.X - Math.Floor(Self.X);
  Result.Y := Self.Y - Math.Floor(Self.Y);
end;

function TSDLFPointH.ToString(const Delim : Char): String;
begin
  Result := Format('%0:g%2:s%1:g', [Self.X, Self.Y, Delim]);
end;

function TSDLFPointH.ToStringFmt(const aFmtStr: String): String;
begin
  Result := Format(aFmtStr, [Self.X, Self.Y]);
end;

procedure TSDLFPointH.FromString(const aString: String; const Delim : Char);
var
  Components: array of String;
begin
  Components := aString.Split(Delim);
  Self.Init(0,0);

  // Lazy read
  if Length(Components) < 1 then Exit;
  Self.X := StrToFloat(Components[0]);
  if Length(Components) < 2 then Exit;
  Self.Y := StrToFloat(Components[1]);
end;

{
  TSDLFRectH
}

procedure TSDLFRectH.Init(const aX, aY, aW, aH: CFloat);
begin
  Self.X := aX; Self.Y := aY; Self.W := aW; Self.H := aH;
end;

procedure TSDLFRectH.Shrink(const aSize: CFloat);
begin
  Self.X += aSize; Self.Y += aSize;
  Self.W -= (aSize + aSize); Self.H -= (aSize + aSize);
end;

function TSDLFRectH.ToString(const Delim : Char): String;
begin
  Result := Format('%0:g,%1:g,%2:g,%3:g', [Self.X, Self.Y, Self.W, Self.H]);
end;

function TSDLFRectH.ToStringFmt(const aFmtStr: String): String;
begin
  Result := Format(aFmtStr, [Self.X, Self.Y, Self.W, Self.H]);
end;

procedure TSDLFRectH.FromString(const aString: String; const Delim : Char);
var
  Components: array of String;
begin
  Components := aString.Split(Delim);
  Self.Init(0,0,0,0);

  // Lazy read
  if Length(Components) < 1 then Exit;
  Self.X := Components[0].ToSingle;
  if Length(Components) < 2 then Exit;
  Self.Y := Components[1].ToSingle;
  if Length(Components) < 3 then Exit;
  Self.W := Components[2].ToSingle;
  if Length(Components) < 4 then Exit;
  Self.H := Components[3].ToSingle;
end;

// Operators

operator = (const C1, C2: TSDL_FColor): Boolean;
begin
  // If both are totally transparent are considered the same always.
  if IsZero(C1.A) and IsZero(C2.A) then Exit(True);
  Result := SameValue(C1.R, C2.R) and SameValue(C1.G, C2.G) 
    and SameValue(C1.B, C2.B) and SameValue(C1.A, C2.A);
end;

// TSDL_FPoint operators

operator = (const P1, P2: TSDL_FPoint): Boolean;
begin
  Result := SameValue(P1.X, P2.X) and SameValue(P1.Y, P2.Y);
end;

operator + (const P1, P2: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P1.X + P2.X; Result.Y := P1.Y + P2.Y;
end;

operator - (const P: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := -P.X; Result.Y := -P.Y;
end;

operator - (const P1, P2: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P1.X - P2.X; Result.Y := P1.Y - P2.Y;
end;

operator * (const P1, P2: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P1.X * P2.X; Result.Y := P1.Y * P2.Y;
end;

operator * (const P: TSDL_FPoint; const aFactor: CFloat): TSDL_FPoint;
begin
  Result.X := P.X * aFactor; Result.Y := P.Y * aFactor;
end;

operator * (const aFactor: CFloat; const P: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P.X * aFactor; Result.Y := P.Y * aFactor;
end;

operator / (const P: TSDL_FPoint; const aFactor: CFloat): TSDL_FPoint;
begin
  Result.X := P.X / aFactor; Result.Y := P.Y / aFactor;
end;

// TSDL_FRect operators

operator = (const R1, R2: TSDL_FRect): Boolean;
begin
  // ToDo: ¿Hacer estricto?
  Result := SameValue(R1.X, R2.X) and SameValue(R1.Y, R2.Y) 
    and SameValue(R1.W, R2.W) and SameValue(R1.H, R2.H)
end;

// TSDL_Vertex operators

operator = (const V1, V2: TSDL_Vertex): Boolean;
begin
  Result := (V1.Position = V2.Position) and (V1.Color = V2.Color) 
    and (V1.Tex_Coord = V2.Tex_Coord);
end;



// Type creation functions

function SDLFColor(const R, G, B, A: CFloat): TSDL_FColor;
begin
  Result.R := R; Result.G := G; Result.B := B; Result.A := A;
end;

function SDLFColor(const Grey, A: CFloat): TSDL_FColor;
begin
  Result.R := Grey; Result.G := Grey; Result.B := Grey; Result.A := A;
end;

function SDLFPoint(const X, Y: CFloat): TSDL_FPoint;
begin
  Result.X := X; Result.Y := Y;
end;

function SDLFRect(const X, Y, W, H: CFloat): TSDL_FRect;
begin
  Result.X := X; Result.Y := Y; Result.W := W; Result.H := H;
end;

function SDLFSegment(const X1, Y1, X2, Y2: CFloat): TCHXSDLFSegment;
begin
  Result.P1.X := X1; Result.P1.Y := Y1;
  Result.P2.X := X2; Result.P2.Y := Y2;
end;

function SDLFSegment(const aP1, aP2: TSDL_FPoint): TCHXSDLFSegment;
begin
  Result.P1 := aP1; Result.P2 := aP2;
end;
end.
