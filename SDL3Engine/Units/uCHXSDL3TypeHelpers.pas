unit uCHXSDL3TypeHelpers;
{< Unit with helpers for SDL3 types, and some useful new types and methods:

  - Dynamic arrays of `TSDL_FColor`, `TSDL_FPoint`, `TSDL_FRect`, etc.:
      `TSDLFColorDynArray`, `TSDLFPointDynArray`, etc.
  - ToDo: Specialized generic lists for those types, wich can be
    inherited to add custom methods: `cSDLFPointList`, `cSDLFRectList`, etc.
    -`FGL` unit returns a not found operator overload error.
    - Try with other container generics: `Generics.Collections` use an
      actual array.
  - Type helpers for SDL types.
  - Global functions returning SDL types. Useful to be used directly
    as parameters without explicitly declaring a variable.

  Some new types that they can be useful:

  - **CHXSDLFSegment**: Struct with a pair of `SDL_FPoint`, sometimes it's
    useful store it's endpoints instead using `TSDL_FRect` and calculate
    end point every time adding `X + W` and `Y + H`.


  (C) 2026 Chixpy https://github.com/Chixpy
}
{$MODE ObjFPC}{$H+}
{$MODESWITCH AdvancedRecords}
{$MODESWITCH TypeHelpers}
{$INLINE ON}{$WARN 6058 OFF}
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
  #### TCHXSDLFSegment

  Struct with a pair of `SDL_FPoint` (Float 2D points)

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
  #### TCHXSDLSegment

  Struct with a pair of `SDL_Point` (Integer 2D Points)

  Sometimes it's useful store it's endpoint instead using `TSDL_Rect` and 
  calculate endpoint every time adding `X + W` and `Y + H`.
}
  TCHXSDLSegment = packed record
  public
    P1, P2: TSDL_Point;

  {
    TCHXSDLSegment.Init
  }

    procedure Init(const aP1, aP2: TSDL_Point); overload; inline;
    procedure Init(const X1, Y1, X2, Y2: Integer); overload; inline;

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
  TSDLPointDynArray = Array of TSDL_Point;
  //< Dynamic array of TSDL_FPoint.
  TSDLFRectDynArray = Array of TSDL_FRect;
  //< Dynamic array of TSDL_FRect.
  TSDLRectDynArray = Array of TSDL_Rect;
  //< Dynamic array of TSDL_FRect.
  TSDLVertexDynArray = Array of TSDL_Vertex;
  //< Dynamic array of TSDL_Vertex.

  TSDLFSegmentDynArray = Array of TCHXSDLFSegment;
  //< Dynamic array of TCHXSDLFSegment.
  TSDLSegmentDynArray = Array of TCHXSDLSegment;
  //< Dynamic array of TCHXSDLSegment.

{
  ### Helpers for SDL types
}

{
  #### Helper for TSDL_FColor
}

  TSDLFColorH = record helper for TSDL_FColor
  public
  {
    TSDLFColorH Init
  }

    procedure Init(const aR, aG, aB: CFloat; const aA: CFloat = 1); overload;
      inline;
    procedure Init(const Grey: CFloat; const aA: CFloat = 1); overload; inline;

    procedure InitByte(const aR, aG, aB: Byte; const aA: Byte = 255); overload;
      inline;
    procedure InitByte(const Grey: Byte; const aA: Byte = 255); overload;

    procedure InitFastHUE(Hue: CFloat; const Alpha: CFloat = 1);
    {< Init the colors from a HUE with range [0..1) at full brightness.

      It can be useful for:

      - Give some color to boring greyscales... but cyclic ones,
        For range scales, we can use a gradient from black to one component or
        use a rainbow one (red -> green -> violet).
      - Select random colors without worrying about dark colors.

      @warning(It's a dirty fast imprecise algorithm addapted from 
        InitFastHUEByte.)
    }
    procedure InitFastHUEByte(const Hue: Byte; const Alpha: Byte = 255);
    {< Init the colors from a HUE with 255 degrees at full brightness.

      It can be useful for:

      - Give some color to boring greyscales... but cyclic ones,
        For range scales, we can use a gradient from black to one component or
        use a rainbow one (red -> green -> violet).
      - Select random colors without worrying about dark colors.

      @warning(It's a dirty fast imprecise algorithm. See CHXPas' `uCHXColor`
        unit for more info about algorithm.)
    }

  {
    TSDLFColorH Comparisons
  }

    function IsEqual(const aColor: TSDL_FColor): Boolean;

  {
    TSDLFColorH Operators (They can't be in a helper as FPC 3.3.1)
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
  #### Helper for TSDL_Color
}

  TSDLColorH = record helper for TSDL_Color
  public
  {
    TSDLColorH Init
  }

    procedure Init(const aR, aG, aB: Byte; const aA: Byte = 255); overload;
      inline;
    procedure Init(const Grey: Byte; const aA: Byte = 255); overload; inline;

    procedure InitFastHUE(const Hue: Byte; const Alpha: Byte = 255);
    {< Init the colors from a HUE with 255 degrees at full brightness.

      It can be useful for:

      - Give some color to boring greyscales... but cyclic ones,
        For range scales, we can use a gradient from black to one component or
        use a rainbow one (red -> green -> violet).
      - Select random colors without worrying about dark colors.

      @warning(It's a dirty fast imprecise algorithm. See CHXPas' `uCHXColor`
        unit for more info about algorithm.)
    }

  {
    TSDLColorH Comparisons
  }

    function IsEqual(const aColor: TSDL_Color): Boolean;

  {
    TSDLColorH Operators (They can't be in a helper as FPC 3.3.1)
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
  #### Helper for TSDL_FPoint
}

  TSDLFPointH = record helper for TSDL_FPoint
  public
  {
    TSDLFPointH Init
  }
    procedure Init(const aX, aY: CFloat); inline;
    procedure InitPolar(const aMag, aAngle: CFloat); inline;
    procedure InitRandom(const MinX, MaxX, MinY, MaxY: CFloat); inline;
    procedure InitRandomPolar(const aMag: CFloat = 1); inline;

  {
    TSDLFPointH Polar data
  }

    function GetAngle: CFloat; inline;
    procedure SetAngle(const aAngle: CFloat); inline;

    function GetSqrMag: CFloat; inline; //< Square of the magnitude
    function GetMagnitude: CFloat; inline;
    procedure SetMagnitude(const aMag: CFloat);

  {
    TSDLFPointH Comparisons
  }

    function IsZero(const aEpsilon: CFloat = 0): Boolean; inline;
    function IsEqual(const P: TSDL_FPoint; const aEpsilon: CFloat = 0):
      Boolean; inline;
    function IsOpposite(const P: TSDL_FPoint; const aEpsilon: CFloat = 0):
      Boolean; inline;

    function IsParallel(const P: TSDL_FPoint; const aEpsilon: CFloat = 0)
      : Boolean; inline;
    //< Parallel. Ignore direction.
    function IsCoaligned(const P: TSDL_FPoint; const aEpsilon: CFloat = 0)
      : Boolean; inline;
    //< Parallel and same direction.
    function IsAntiParallel(const P: TSDL_FPoint; const aEpsilon: CFloat = 0)
      : Boolean; inline;
    //< Parallel, opposite direction.
    function IsPerpendicular(const P: TSDL_FPoint; const aEpsilon: CFloat = 0)
      : Boolean; inline;

  {
    TSDLFPointH Self operations
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
    TSDLFPointH Common operations (as Point or Vector)
  }

    procedure Normalize;
    function GetNormalized: TSDL_FPoint; inline;

    procedure Move(const dX, dY: CFloat); inline;
    procedure Scale(const sX, sY: CFloat); inline;
    procedure Rotate(const aAngle: CFloat);

    function ScalProd(const P: TSDL_FPoint): CFloat; inline;
    function VectProd(const P: TSDL_FPoint): CFloat; inline;

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

    ToDo: Overload for TSDL_FPoint (except Frac, as itself is TSDL_FPoint)
  }

    function Ceil: TSDL_Point; overload;
    //< Ceil(-2.3, 1.3) = -2, 2 -> +inf
    function Truncate: TSDL_Point; overload;
    //< Truncate(-2.3, 1.3) = -2, 1 -> 0
    function Floor: TSDL_Point; overload;
    //< Floor(-2.3, 1.3) = -3, 1 -> -inf
    function Round: TSDL_Point; overload;
    //< Round to nearest integer.
    function FracCeil: TSDL_FPoint;
    //< FracCeil(-2.3, 1.3) = -0.3, -0.7 -> +inf
    function FracTrunc: TSDL_FPoint;
    //< FracTrunc(-2.3, 1.3) = -0.3, 0.3 -> 0
    function FracFloor: TSDL_FPoint;
    //< FracFloor(-2.3, 1.3) = 0.7, 0.3 - -inf

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
  #### Helper for TSDL_Point
}

  TSDLPointH = record helper for TSDL_Point
  public
    procedure Init(const aX, aY: CInt); inline;
    procedure InitRandom(const MinX, MaxX, MinY, MaxY: CInt); inline;

    function GetAngle: CFloat;
    function GetSqrMag: CFloat; inline; //< Square of the magnitude
    function GetMagnitude: CFloat; inline;

    function IsZero: Boolean; inline;
    function IsEqual(const P: TSDL_Point): Boolean; inline;
    function IsOpposite(const P: TSDL_Point): Boolean; inline;

    function IsParallel(const P: TSDL_Point): Boolean; inline;
    //< Parallel. Ignore direction.
    function IsCoaligned(const P: TSDL_Point): Boolean; inline;
    //< Parallel and same direction.
    function IsAntiParallel(const P: TSDL_Point): Boolean; inline;
    //< Parallel, opposite direction.
    function IsPerpendicular(const P: TSDL_Point): Boolean; inline;

    procedure Negate; //< Other posible names: Opposite or Invert
    procedure Add(const P: TSDL_Point); inline;
    procedure Subtract(const P: TSDL_Point); inline; //< Self - P
    procedure Multiply(const aScale: CInt); inline;
    procedure CompScale(const P: TSDL_Point); inline;
    //< Scale by components
    procedure Divide(const aScale: CInt); inline; //< Self div AScale
    procedure DivInv(const aScale: CInt); inline; //< AScale div Self
    procedure Modulo(const aScale: CInt); inline; //< Self mod AScale
    procedure ModInv(const aScale: CInt); inline; //< AScale mod Self

    procedure Move(const dX, dY: CInt); inline;
    procedure Scale(const sX, sY: CInt); inline;
    procedure ScaleDiv(const sX, sY: CInt); inline;
    procedure Rotate90(const ClockWise: Boolean = True);

    function ScalProd(const P: TSDL_Point): CFloat; inline;
    function VectProd(const P: TSDL_Point): CFloat; inline;

    function SqrDistance(const P: TSDL_Point): CFloat; inline;
    function Distance(const P: TSDL_Point): CFloat; inline;
    function InDistance(const P: TSDL_Point; const aDistance: CFloat;
      const IncEqual: Boolean = False): Boolean;
    function MidPoint(const P: TSDL_Point): TSDL_Point; inline;

    function ToString(const Delim : char = ','): String;
    function ToStringFmt(const aFmtStr: String): String;
    procedure FromString(const aString: String; const Delim : char = ',');
  end;

{
  #### Helper for TSDL_FRect
}

  TSDLFRectH = record helper for TSDL_FRect
  public
    procedure Init(const aX, aY, aW, aH: CFloat); overload; inline;

    procedure Normalize;
    {< Normalize the FRect to have positive Width and Height. }
    function Normalized: TSDL_FRect;
    {< Returns the FRect normalized to have positive Width and Height. }

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
  #### Helper for TSDL_Rect
}

  TSDLRectH = record helper for TSDL_Rect
  public
    procedure Init(const aX, aY, aW, aH: CInt); overload; inline;
  end;

{
  #### Helper for TSDL_Vertex
}

  TSDLVertexH = record helper for TSDL_Vertex
  public
    procedure Init(const PosX, PosY, R, G, B, A: CFloat;
      const PosU: CFloat = 0; const PosV: CFloat = 0); overload; inline;
    procedure Init(const Pos: TSDL_FPoint; const aColor: TSDL_FColor;
      const TexCoord: TSDL_FPoint); overload; inline;
    procedure Init(const Pos: TSDL_FPoint; const aColor: TSDL_FColor);
      overload; inline;
  end;

{
  ### Operator overloading.

  Ideally they would be in helpers as class operators...
}

{
  #### TSDL_FColor operators.
}

operator = (const C1, C2: TSDL_FColor): Boolean; overload;

{
  #### TSDL_FPoint operators.
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
operator / (const aFactor: CFloat; const P: TSDL_FPoint): TSDL_FPoint; overload;
  inline;
operator / (const P1, P2: TSDL_FPoint): TSDL_FPoint; overload; inline;

{
  #### TSDL_Point operators.
}

operator = (const P1, P2: TSDL_Point): Boolean; overload; inline;
operator + (const P1, P2: TSDL_Point): TSDL_Point; overload; inline;
operator - (const P: TSDL_Point): TSDL_Point; overload; inline;
operator - (const P1, P2: TSDL_Point): TSDL_Point; overload; inline;
operator * (const P: TSDL_Point; const aFactor: CInt): TSDL_Point; overload;
  inline;
operator * (const aFactor: CInt; const P: TSDL_Point): TSDL_Point; overload;
  inline;
operator * (const P1, P2: TSDL_Point): TSDL_Point; overload; inline;
//< Component-wise scaling (Hadamard product)
operator div (const P: TSDL_Point; const aFactor: CInt): TSDL_Point; overload;
  inline;
operator div (const aFactor: CInt; const P: TSDL_Point): TSDL_Point; overload;
  inline;
operator div (const P1, P2: TSDL_Point): TSDL_Point; overload; inline;
operator mod (const P: TSDL_Point; const aFactor: CInt): TSDL_Point; overload;
  inline;
operator mod (const aFactor: CInt; const P: TSDL_Point): TSDL_Point; overload;
  inline;
operator mod (const P1, P2: TSDL_Point): TSDL_Point; overload; inline;

{
  #### TSDL_FRect operators.
}

operator = (const R1, R2: TSDL_FRect): Boolean; overload; inline;

{
  #### TSDL_Vertex operators.
}

operator = (const V1, V2: TSDL_Vertex): Boolean; overload; inline;

{
  #### Misc operators.
}

operator := (const Src: TSDL_Point): TSDL_FPoint;

{
  ### Type creation functions.

  Useful to use them as parameters when calling a function without creating
  a temporal variable.
}

{
  #### TSDL_FColor creation.
}

function SDLFColor(const R, G, B: CFloat; const A: CFloat = 1): TSDL_FColor;
  overload; inline;
{< Create a TSDL_FColor from Red, Green, Blue and Alpha values.
}
function SDLFColor(const Grey: CFloat; const A: CFloat = 1): TSDL_FColor;
  overload; inline;
{< Create a TSDL_FColor with a Grey value.
}
function SDLFColorFastHUE(const Hue: CFloat; const Alpha: CFloat = 1)
  : TSDL_FColor; inline;
{< Create a TSDL_FColor using InitFastHUE.
}
function SDLFColorFastHUEByte(const Hue: Byte; const Alpha: Byte = 255)
  : TSDL_FColor; inline;
{< Create a TSDL_FColor using InitFastHUEByte.
}

{
  #### TSDL_Color creation.
}

function SDLColor(const R, G, B: Byte; const A: Byte = 255): TSDL_Color;
  overload; inline;
{< Create a TSDL_FColor from Red, Green, Blue and Alpha values.
}
function SDLColor(const Grey: Byte; const A: Byte = 255): TSDL_Color;
  overload; inline;
{< Create a TSDL_FColor with a Grey value.
}
function SDLColorFastHUE(const Hue: Byte; const Alpha: Byte = 255)
  : TSDL_Color; inline;
{< Create a TSDL_FColor using InitFastHUE.
}

{
  #### TSDL_FPoint creation.
}

function SDLFPoint(const X: CFloat = 0; const Y: CFloat = 0): TSDL_FPoint;
  inline;
{< Create a TSDL_FPoint.}

{
  #### TSDL_Point creation.
}

function SDLPoint(const X: CInt = 0; const Y: CInt = 0): TSDL_Point; inline;
{< Create a TSDL_Point.}

{
  #### TSDL_FRect creation.
}

function SDLFRect(const X, Y, W, H: CFloat): TSDL_FRect; inline;
{< Create a TSDL_FRect.}

{
  #### TCHXSDLFSegment operators.
}

function SDLFSegment(const aP1, aP2: TSDL_FPoint): TCHXSDLFSegment;
  overload; inline;
function SDLFSegment(const X1, Y1, X2, Y2: CFloat): TCHXSDLFSegment;
  overload; inline;

implementation

{
  ##### TSDLFColorH
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

procedure TSDLFColorH.InitFastHUE(Hue: CFloat; const Alpha: CFloat);
const
  // Preprocessing this values, althought I'm nearly sure that compiler
  // will evaluate it as constants if used directly.
  k1_6 = 1 / 6; k2_6 = 2 / 6; k3_6 = 3 / 6; k4_6 = 4 / 6; k5_6 = 5 / 6;
begin
  // FloorFrac -> Range [1..0] and continuous with negatives.
  // Abs(TruncFrac) will be inverse color order with negatives.
  Hue := Hue - Floor(Hue);

  if Hue < k1_6 then
  begin
    Self.R := 1; Self.G := Hue * 6; Self.B := 0;
  end
  else if Hue < k2_6 then
  begin
    // 1 - ((Hue - 1 / 6) * 6) => 2 - 6 * Hue
    Self.R := 2 - 6 * Hue; Self.G := 1; Self.B := 0;
  end
  else if Hue < k3_6 then
  begin
    // (Hue - 2 / 6) * 6 => 6 * Hue - 2
    Self.R := 0; Self.G := 1; Self.B := 6 * Hue - 2;
  end
  else if Hue < k4_6 then
  begin
    // 1 - ((Hue - 3 / 6) * 6) => 4 - 6 * Hue
    Self.R := 0; Self.G := 4 - 6 * Hue; Self.B := 1;
  end
  else if Hue < k5_6 then
    // (Hue - 4 / 6) * 6 => 6 * Hue - 4
  begin
    Self.R := 6 * Hue - 4; Self.G := 0; Self.B := 1;
  end
  else // [k5_6..1) range
  begin
    // 1 - ((Hue - 5 / 6) * 6) => 6 - 6 * Hue => 6 * (1 - Hue)
    Self.R := 1; Self.G := 0; B := 6 - 6 * Hue;
  end;

  Self.A := Alpha;
end;

procedure TSDLFColorH.InitFastHUEByte(const Hue, Alpha: Byte);
begin
  case Hue of
    0..42:
      begin R := 1; G := (Hue * 6) * kInv255; B := 0; end;
    43:
      begin R := 1; G := 1; B := 0; end;
    44..85:
      // 255 - ((Hue - 43) * 6) => 513 - 6 * Hue
      begin R := (513 - 6 * Hue) * kInv255; G := 1; B := 0; end;
    86..127:
      // (Hue - 85) * 6 => 6 * Hue - 510
      begin R := 0; G := 1; B := ((Hue - 85) * 6) * kInv255; end;
    128..170:
      // 255 - ((Hue - 128) * 6) => 1023 - 6 * Hue
      begin R := 0; G := (1023 - 6 * Hue) * kInv255; B := 1; end;
    171:
      begin R := 0; G := 0; B := 1; end;
    172..213:
      // (Hue - 171) * 6 => 6 * Hue - 1026
      begin R := ((Hue - 171) * 6) * kInv255; G := 0; B := 1; end;
    214..255:
      // 255 - ((Hue - 128) * 6) => 1278 - 6 * Hue
      begin R := 1; G := 0; B := (1278 - 6 * Hue) * kInv255; end;
  end;

  A := Alpha * kInv255;
end;

function TSDLFColorH.IsEqual(const aColor: TSDL_FColor): Boolean;
begin
  // If both are totally transparent are considered the same always.
  if Math.IsZero(Self.A) and Math.IsZero(aColor.A) then Exit(True);
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
  ##### TSDLColorH
}

procedure TSDLColorH.Init(const aR, aG, aB, aA: Byte);
begin
  Self.R := aR; Self.G := aG; Self.B := aB; Self.A := aA;
end;

procedure TSDLColorH.Init(const Grey, aA: Byte);
begin
  Self.R := Grey; Self.G := Grey; Self.B := Grey; Self.A := aA;
end;

procedure TSDLColorH.InitFastHUE(const Hue, Alpha: Byte);
begin
  case Hue of
    0..42:
      begin R := 255; G := Hue * 6; B := 0; end;
    43:
      begin R := 255; G := 255; B := 0; end;
    44..85:
      // 255 - ((Hue - 43) * 6) => 513 - 6 * Hue
      begin R := 513 - 6 * Hue; G := 255; B := 0; end;
    86..127:
      // (Hue - 85) * 6 => 6 * Hue - 510
      begin R := 0; G := 255; B := (Hue - 85) * 6; end;
    128..170:
      // 255 - ((Hue - 128) * 6) => 1023 - 6 * Hue
      begin R := 0; G := 1023 - 6 * Hue; B := 255; end;
    171:
      begin R := 0; G := 0; B := 255; end;
    172..213:
      // (Hue - 171) * 6 => 6 * Hue - 1026
      begin R := (Hue - 171) * 6; G := 0; B := 255; end;
    214..255:
      // 255 - ((Hue - 128) * 6) => 1278 - 6 * Hue
      begin R := 255; G := 0; B := 1278 - 6 * Hue; end;
  end;

  A := Alpha;
end;

function TSDLColorH.IsEqual(const aColor: TSDL_Color): Boolean;
begin
  // If both are totally transparent are considered the same always.
  if (Self.A = 0) and (aColor.A = 0) then Exit(True);

  Result := (Self.R = aColor.R) and (Self.G = aColor.G)
    and (Self.B = aColor.B) and (Self.A = aColor.A);
end;


function TSDLColorH.ToString(const Delim : Char): String;
begin
  Result := Format('%0:d%4:s%1:d%4:s%2:d%4:s%3:d',
    [Self.R, Self.G, Self.B, Self.A, Delim]);
end;

function TSDLColorH.ToStringFmt(const aFmtStr: String): String;
begin
  Result := Format(aFmtStr, [Self.R, Self.G, Self.B, Self.A]);
end;

procedure TSDLColorH.FromString(const aString: String; const Delim : Char);
var
  Components: array of String;
begin
  Components := aString.Split(Delim);
  Self.Init(0, 0, 0, 255);

  // Lazy read
  if Length(Components) < 1 then Exit;
  Self.R := StrToInt(Components[0]);
  if Length(Components) < 2 then Exit;
  Self.G := StrToInt(Components[1]);
  if Length(Components) < 3 then Exit;
  Self.B := StrToInt(Components[2]);
  if Length(Components) < 4 then Exit;
  Self.A := StrToInt(Components[3]);
end;

{
  ##### TSDLFPointH
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

function TSDLFPointH.IsParallel(const P: TSDL_FPoint; const aEpsilon: CFloat)
  : Boolean;
begin
  Result := Math.IsZero(Self.VectProd(P), aEpsilon);
end;

function TSDLFPointH.IsCoaligned(const P: TSDL_FPoint; const aEpsilon: CFloat)
  : Boolean;
begin
  Result := (Self.ScalProd(P) > 0) and Self.IsParallel(P, aEpsilon);
end;

function TSDLFPointH.IsAntiParallel(const P: TSDL_FPoint;
  const aEpsilon: CFloat): Boolean;
begin
  Result := (Self.ScalProd(P) < 0) and Self.IsParallel(P, aEpsilon);
end;

function TSDLFPointH.IsPerpendicular(const P: TSDL_FPoint;
  const aEpsilon: CFloat): Boolean;
begin
  Result := Math.IsZero(Self.ScalProd(P), aEpsilon);
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

function TSDLFPointH.ScalProd(const P: TSDL_FPoint): CFloat;
begin
  Result := Self.X * P.X + Self.Y * P.Y;
end;

function TSDLFPointH.VectProd(const P: TSDL_FPoint): CFloat;
begin
  Result := Self.X * P.Y - Self.Y * P.X;
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

function TSDLFPointH.Ceil: TSDL_Point;
begin
  Result.X := Math.Ceil(Self.X); Result.Y := Math.Ceil(Self.Y);
end;

function TSDLFPointH.Truncate: TSDL_Point;
begin
  Result.X := Trunc(Self.X); Result.Y := Trunc(Self.Y);
end;

function TSDLFPointH.Floor: TSDL_Point;
begin
  Result.X := Math.Floor(Self.X); Result.Y := Math.Floor(Self.Y);
end;

function TSDLFPointH.Round: TSDL_Point;
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
  #### Helper for TSDL_Point
}

procedure TSDLPointH.Init(const aX, aY: CInt);
begin
  Self.X := aX; Self.Y := aY;
end;

procedure TSDLPointH.InitRandom(const MinX, MaxX, MinY, MaxY: CInt);
begin
  Self.X := RandomRange(MinX, MaxX); Self.Y := RandomRange(MinY, MaxY);
end;

function TSDLPointH.GetAngle: CFloat;
begin
  Result := ArcTan2(Y, X);
end;

function TSDLPointH.GetSqrMag: CFloat;
begin
  Result := Self.X * Self.X + Self.Y * Self.Y;
end;

function TSDLPointH.GetMagnitude: CFloat;
begin
  Result := SqRt(Self.GetSqrMag);
end;

function TSDLPointH.IsZero: Boolean;
begin
  Result := (Self.X = 0) and (Self.Y = 0);
end;

function TSDLPointH.IsEqual(const P: TSDL_Point): Boolean;
begin
  Result := (Self.X = P.X) and (Self.Y = P.Y);
end;

function TSDLPointH.IsOpposite(const P: TSDL_Point): Boolean;
begin
  Result := (Self.X = -P.X) and (Self.Y = -P.Y);
end;

function TSDLPointH.IsParallel(const P: TSDL_Point): Boolean;
begin
  Result := Math.IsZero(Self.VectProd(P));
end;

function TSDLPointH.IsCoaligned(const P: TSDL_Point): Boolean;
begin
  Result := (Self.ScalProd(P) > 0) and Self.IsParallel(P);
end;

function TSDLPointH.IsAntiParallel(const P: TSDL_Point): Boolean;
begin
  Result := (Self.ScalProd(P) < 0) and Self.IsParallel(P);
end;

function TSDLPointH.IsPerpendicular(const P: TSDL_Point): Boolean;
begin
  Result := Self.ScalProd(P) = 0;
end;

procedure TSDLPointH.Negate;
begin
  Self.X := -Self.X; Self.Y := -Self.Y;
end;

procedure TSDLPointH.Add(const P: TSDL_Point);
begin
  Self.X += P.X; Self.Y += P.Y;
end;

procedure TSDLPointH.Subtract(const P: TSDL_Point); //< Self - P
begin
  Self.X -= P.X; Self.Y -= P.Y;
end;

procedure TSDLPointH.Multiply(const aScale: CInt);
begin
  Self.X *= aScale; Self.Y *= aScale;
end;

procedure TSDLPointH.CompScale(const P: TSDL_Point);
begin
  Self.X *= P.X; Self.Y *= P.Y;
end;

procedure TSDLPointH.Divide(const aScale: CInt);
begin
  Self.X := Self.X div aScale; Self.Y := Self.Y div aScale;
end;

procedure TSDLPointH.DivInv(const aScale: CInt);
begin
  Self.X := aScale div Self.X; Self.Y := aScale div Self.Y;
end;

procedure TSDLPointH.Modulo(const aScale: CInt);
begin
  Self.X := Self.X mod aScale; Self.Y := Self.Y mod aScale;
end;

procedure TSDLPointH.ModInv(const aScale: CInt);
begin
  Self.X := aScale mod Self.X; Self.Y := aScale mod Self.Y;
end;

procedure TSDLPointH.Move(const dX, dY: CInt);
begin
  Self.X += dX; Self.Y += dY;
end;

procedure TSDLPointH.Scale(const sX, sY: CInt);
begin
  Self.X *= sX; Self.Y *= sY;
end;

procedure TSDLPointH.ScaleDiv(const sX, sY: CInt);
begin
  Self.X := Self.X div sX; Self.Y := Self.Y div sY;
end;

procedure TSDLPointH.Rotate90(const Clockwise: Boolean);
begin
  if Clockwise then
    Self.Init(-Self.Y, -Self.X)
  else
    Self.Init(-Self.Y, Self.X)
end;

function TSDLPointH.ScalProd(const P: TSDL_Point): CFloat;
begin
  Result := Self.X * P.X + Self.Y * P.Y;
end;

function TSDLPointH.VectProd(const P: TSDL_Point): CFloat;
begin
  Result := Self.X * P.Y - Self.Y * P.X;
end;

function TSDLPointH.SqrDistance(const P: TSDL_Point): CFloat;
begin
  Result := Sqr(Self.X - P.X) + Sqr(Self.Y - P.Y)
end;

function TSDLPointH.Distance(const P: TSDL_Point): CFloat;
begin
  Result := SqRt(Self.SqrDistance(P));
end;

function TSDLPointH.InDistance(const P: TSDL_Point; const aDistance: CFloat;
  const IncEqual: Boolean): Boolean;
var
  aDistSq: CFloat;
begin
  aDistSq := Self.SqrDistance(P);

  if aDistSq < (aDistance * aDistance) then
    Exit(True);

  Result := IncEqual and SameValue(SqRt(aDistSq), aDistance);
end;

function TSDLPointH.MidPoint(const P: TSDL_Point): TSDL_Point;
begin
  Result.X := (Self.X + P.X) div 2; Result.Y := (Self.Y + P.Y) div 2;
end;

function TSDLPointH.ToString(const Delim : Char): String;
begin
  Result := Format('%0:d%2:s%1:d', [Self.X, Self.Y, Delim]);
end;

function TSDLPointH.ToStringFmt(const aFmtStr: String): String;
begin
  Result := Format(aFmtStr, [Self.X, Self.Y]);
end;

procedure TSDLPointH.FromString(const aString: String; const Delim : Char);
var
  Components: array of String;
begin
  Components := aString.Split(Delim);
  Self.Init(0,0);

  // Lazy read
  if Length(Components) < 1 then Exit;
  Self.X := StrToInt(Components[0]);
  if Length(Components) < 2 then Exit;
  Self.Y := StrToInt(Components[1]);
end;

{
  ##### TSDLFRectH
}

procedure TSDLFRectH.Init(const aX, aY, aW, aH: CFloat);
begin
  Self.X := aX; Self.Y := aY; Self.W := aW; Self.H := aH;
end;

procedure TSDLFRectH.Normalize;
begin
  if Self.W < 0 then
    begin Self.X += Self.W; Self.W := -Self.W end;
  if Self.H < 0 then
    begin Self.Y += Self.H; Self.H := -Self.H end;
end;

function TSDLFRectH.Normalized: TSDL_FRect;
begin
  Result := Self;
  Result.Normalize;
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

{
  ##### TSDLRectH
}

procedure TSDLRectH.Init(const aX, aY, aW, aH: CInt);
begin
  Self.X := aX; Self.Y := aY; Self.W := aW; Self.H := aH;
end;

{
  ##### TSDLVertexH
}
procedure TSDLVertexH.Init(const PosX, PosY, R, G, B, A: CFloat;
  const PosU, PosV: CFloat);
begin
  Self.Position.X := PosX; Self.Position.Y := PosY;
  Self.Color.R := R; Self.Color.G := G; Self.Color.B := B; Self.Color.A := A;
  Self.Tex_Coord.X := PosU; Self.Tex_Coord.Y := PosV;
end;

procedure TSDLVertexH.Init(const Pos: TSDL_FPoint; const aColor: TSDL_FColor;
  const TexCoord: TSDL_FPoint);
begin
  Self.Position := Pos;
  Self.Color := aColor;
  Self.Tex_Coord := TexCoord;
end;

procedure TSDLVertexH.Init(const Pos: TSDL_FPoint; const aColor: TSDL_FColor);
begin
  Self.Position := Pos;
  Self.Color := aColor;
  Self.Tex_Coord.Init(0, 0);
end;

{
  ##### TCHXSDLFSegment
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
  ##### TCHXSDLSegment
}

procedure TCHXSDLSegment.Init(const aP1, aP2: TSDL_Point);
begin
  Self.P1 := aP1; Self.P2 := aP2;
end;

procedure TCHXSDLSegment.Init(const X1, Y1, X2, Y2: Integer);
begin
  Self.P1.X := X1; Self.P1.Y := Y1;
  Self.P2.X := X2; Self.P2.Y := Y2;
end;

{
  #### Operators
}

{
  ##### TSDL_FColor
}

operator = (const C1, C2: TSDL_FColor): Boolean;
begin
  // If both are totally transparent are considered the same always.
  if Math.IsZero(C1.A) and Math.IsZero(C2.A) then Exit(True);
  Result := SameValue(C1.R, C2.R) and SameValue(C1.G, C2.G) 
    and SameValue(C1.B, C2.B) and SameValue(C1.A, C2.A);
end;

{
  ##### TSDL_FPoint
}

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

operator * (const P: TSDL_FPoint; const aFactor: CFloat): TSDL_FPoint;
begin
  Result.X := P.X * aFactor; Result.Y := P.Y * aFactor;
end;

operator * (const aFactor: CFloat; const P: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P.X * aFactor; Result.Y := P.Y * aFactor;
end;

operator * (const P1, P2: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P1.X * P2.X; Result.Y := P1.Y * P2.Y;
end;

operator / (const P: TSDL_FPoint; const aFactor: CFloat): TSDL_FPoint;
begin
  Result.X := P.X / aFactor; Result.Y := P.Y / aFactor;
end;

operator / (const aFactor: CFloat; const P: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := aFactor / P.X; Result.Y := aFactor / P.Y;
end;

operator / (const P1, P2: TSDL_FPoint): TSDL_FPoint;
begin
  Result.X := P1.X / P2.X; Result.Y := P1.Y / P2.Y;
end;

{
  #### TSDL_Point operators.
}

operator = (const P1, P2: TSDL_Point): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

operator + (const P1, P2: TSDL_Point): TSDL_Point;
begin
  Result.X := P1.X + P2.X; Result.Y := P1.Y + P2.Y;
end;

operator - (const P: TSDL_Point): TSDL_Point;
begin
  Result.X := -P.X; Result.Y := -P.Y;
end;

operator - (const P1, P2: TSDL_Point): TSDL_Point;
begin
  Result.X := P1.X - P2.X; Result.Y := P1.Y - P2.Y;
end;

operator * (const P: TSDL_Point; const aFactor: CInt): TSDL_Point;
begin
  Result.X := P.X * aFactor; Result.Y := P.Y * aFactor;
end;

operator * (const aFactor: CInt; const P: TSDL_Point): TSDL_Point;
begin
  Result.X := P.X * aFactor; Result.Y := P.Y * aFactor;
end;

operator * (const P1, P2: TSDL_Point): TSDL_Point;
begin
  Result.X := P1.X * P2.X; Result.Y := P1.Y * P2.Y;
end;

operator div (const P: TSDL_Point; const aFactor: CInt): TSDL_Point;
begin
  Result.X := P.X div aFactor; Result.Y := P.Y div aFactor;
end;

operator div (const aFactor: CInt; const P: TSDL_Point): TSDL_Point;
begin
  Result.X := aFactor div P.X; Result.Y := aFactor div P.Y;
end;

operator div (const P1, P2: TSDL_Point): TSDL_Point;
begin
  Result.X := P1.X div P2.X; Result.Y := P1.Y div P2.Y;
end;

operator mod (const P: TSDL_Point; const aFactor: CInt): TSDL_Point;
begin
  Result.X := P.X mod aFactor; Result.Y := P.Y mod aFactor;
end;

operator mod (const aFactor: CInt; const P: TSDL_Point): TSDL_Point;
begin
  Result.X := aFactor mod P.X; Result.Y := aFactor mod P.Y;
end;

operator mod (const P1, P2: TSDL_Point): TSDL_Point;
begin
  Result.X := P1.X div P2.X; Result.Y := P1.Y div P2.Y;
end;

{
  ##### TSDL_FRect
}

operator = (const R1, R2: TSDL_FRect): Boolean;
begin
  // ToDo: ¿Hacer estricto?
  Result := SameValue(R1.X, R2.X) and SameValue(R1.Y, R2.Y) 
    and SameValue(R1.W, R2.W) and SameValue(R1.H, R2.H)
end;

{
  ##### TSDL_Vertex
}

operator = (const V1, V2: TSDL_Vertex): Boolean;
begin
  Result := (V1.Position = V2.Position) and (V1.Color = V2.Color) 
    and (V1.Tex_Coord = V2.Tex_Coord);
end;

{
  ##### Misc operators
}
operator := (const Src: TSDL_Point): TSDL_FPoint;
begin
  Result.X := Src.X; Result.Y := Src.Y;
end;

{
  #### Type creation functions
}

{
  ##### TSDL_FColor
}

function SDLFColor(const R, G, B, A: CFloat): TSDL_FColor;
begin
  Result.R := R; Result.G := G; Result.B := B; Result.A := A;
end;

function SDLFColor(const Grey, A: CFloat): TSDL_FColor;
begin
  Result.R := Grey; Result.G := Grey; Result.B := Grey; Result.A := A;
end;

function SDLFColorFastHUE(const Hue, Alpha: CFloat): TSDL_FColor;
begin
  Result.InitFastHUE(Hue, Alpha);
end;

function SDLFColorFastHUEByte(const Hue, Alpha: Byte): TSDL_FColor;
begin
  Result.InitFastHUEByte(Hue, Alpha);
end;

{
  ##### TSDL_Color
}

function SDLColor(const R, G, B, A: Byte): TSDL_Color;
begin
  Result.R := R; Result.G := G; Result.B := B; Result.A := A;
end;

function SDLColor(const Grey, A: Byte): TSDL_Color;
begin
  Result.R := Grey; Result.G := Grey; Result.B := Grey; Result.A := A;
end;

function SDLColorFastHUE(const Hue, Alpha: Byte): TSDL_Color;
begin
  Result.InitFastHUE(Hue, Alpha);
end;

{
  ##### TSDL_FPoint
}

function SDLFPoint(const X, Y: CFloat): TSDL_FPoint;
begin
  Result.X := X; Result.Y := Y;
end;

{
  ##### TSDL_Point
}

function SDLPoint(const X, Y: CInt): TSDL_Point;
begin
  Result.X := X; Result.Y := Y;
end;

{
  ##### TSDL_FRect
}

function SDLFRect(const X, Y, W, H: CFloat): TSDL_FRect;
begin
  Result.X := X; Result.Y := Y; Result.W := W; Result.H := H;
end;

{
  ##### TCHXSDLFSegment
}

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
