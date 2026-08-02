unit uCHXSDL3TypeHelpers;
{< Unit with helpers for SDL3 types, and some useful new types and method:

  - **CHXSDLFSegment**: Struct with a pair of `SDL_FPoint`, sometimes it's
    usefull store it's endpoint instead using `TSDL_FRect` and calculate
    end point every time adding `X + W` and `Y + H`.



  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$inline ON}{$modeswitch ADVANCEDRECORDS}

interface

uses
  SysUtils, CTypes, Math,
  SDL3;

const
  kSigmaCFloat = 1e-4; // Usual Sigma value for Single
  kInv255 = 0.00392156863; // 1 / 255

type

  TCHXSDLFSegment = record
    P1, P2: TSDL_FPoint;

    procedure Init(const aP1, aP2: TSDL_FPoint); overload; inline;
    procedure Init(const X1, Y1, X2, Y2: CFloat); overload; inline;

  end;
  
  {
    Dynamic lists.

    Useful for inheriting a create methods for batch operation.

    For example, rotate all points in the list, calculating sen and cos only
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
    Dynamic Arrays.

    Many of the methods can actually have open array parameters, but some
    of them modify and returns arrays.
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
  Helpers for SDL types
}
  TSDLFColorH = record helper for TSDL_FColor
    procedure Init(const aR, aG, aB: CFloat; const aA: CFloat = 1); overload;
      inline;
    procedure Init(const Grey: CFloat; const aA: CFloat = 1); overload; inline;

    procedure InitByte(const aR, aG, aB: Byte; const aA: Byte = 255); overload;
      inline;
    procedure InitByte(const Grey: Byte; const aA: Byte = 255); overload;
      inline;

    function IsEqual(const aColor: TSDL_FColor): Boolean;

    // operator = (const C1, C2: TSDL_FColor): Boolean; overload;

  end;

  TSDLFPointH = record helper for TSDL_FPoint
    procedure Init(const aX, aY: CFloat); overload; inline;

    // operator = (const P1, P2: TSDL_FPoint): Boolean; overload; inline;
  end;

  TSDLFRectH = record helper for TSDL_FRect
    procedure Init(const aX, aY, aW, aH: CFloat); overload; inline;

    procedure Shrink(const aSize: CFloat);

    // operator = (const R1, R2: TSDL_FRect): Boolean; overload; inline;
  end;

{
  Operator overloading.

  Ideally they would be in helpers as class operators...
}

operator = (const C1, C2: TSDL_FColor): Boolean; overload;
operator = (const P1, P2: TSDL_FPoint): Boolean; overload; inline;
operator = (const R1, R2: TSDL_FRect): Boolean; overload; inline;
operator = (const V1, V2: TSDL_Vertex): Boolean; overload; inline;

{
  Type creation functions. Useful to use them as parameters when calling
    a function without creating a temporal variable.
}

function SDLFColor(const R, G, B: CFloat; const A : CFloat = 1): TSDL_FColor;
  overload; inline;
{< Create a TSDL_FColor from Red, Green, Blue and Alpha values.}

function SDLFColor(const Grey: CFloat; const A : CFloat = 1): TSDL_FColor;
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

(*
function SDLColor2Str(aColor : TSDL_Color) : String; inline;
{< Write a TSDL_Color to a String.}

function Str2SDLColor(aColor : String) : TSDL_Color;
{< Write a TSDL_Color to a String.}
*)

implementation

// TCHXSDLFSegment

procedure TCHXSDLFSegment.Init(const aP1, aP2: TSDL_FPoint);
begin
  Self.P1 := aP1; Self.P2 := aP2;
end;

procedure TCHXSDLFSegment.Init(const X1, Y1, X2, Y2: CFloat);
begin
  Self.P1.X := X1; Self.P1.Y := Y1;
  Self.P2.X := X2; Self.P2.Y := Y2;
end;

// TSDLFColorH

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

// TSDLFPointH
procedure TSDLFPointH.Init(const aX, aY: CFloat);
begin
  Self.X := aX; Self.Y := aY;
end;

// TSDLFRectH

procedure TSDLFRectH.Init(const aX, aY, aW, aH: CFloat);
begin
  Self.X := aX; Self.Y := aY; Self.W := aW; Self.H := aH;
end;

procedure TSDLFRectH.Shrink(const aSize: CFloat);
begin
  X += aSize; Y += aSize; W -= (aSize + aSize); H -= (aSize + aSize);
end;

// Operators

operator = (const C1, C2: TSDL_FColor): Boolean;
begin
  // If both are totally transparent are considered the same always.
  if IsZero(C1.A) and IsZero(C2.A) then Exit(True);
  Result := SameValue(C1.R, C2.R) and SameValue(C1.G, C2.G) 
    and SameValue(C1.B, C2.B) and SameValue(C1.A, C2.A);
end;

operator = (const P1, P2: TSDL_FPoint): Boolean;
begin
  // ToDo: ¿Hacer estricto?
  Result := SameValue(P1.X, P2.X) and SameValue(P1.Y, P2.Y);
end;

operator = (const R1, R2: TSDL_FRect): Boolean;
begin
  // ToDo: ¿Hacer estricto?
  Result := SameValue(R1.X, R2.X) and SameValue(R1.Y, R2.Y) 
    and SameValue(R1.W, R2.W) and SameValue(R1.H, R2.H)
end;

operator = (const V1, V2: TSDL_Vertex): Boolean;
begin
  Result := (V1.Position = V2.Position) and (V1.Color = V2.Color) 
    and (V1.Tex_Coord = V2.Tex_Coord);
end;



// Type creation functions

function SDLFColor(const R, G, B, A : CFloat) : TSDL_FColor;
begin
  Result.R := R; Result.G := G; Result.B := B; Result.A := A;
end;

function SDLFColor(const Grey, A : CFloat): TSDL_FColor;
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

function SDLFSegment(const aP1, aP2: TSDL_FPoint) : TCHXSDLFSegment;
begin
  Result.P1 := aP1; Result.P2 := aP2;
end;

(*
function SDLColor2Str(aColor : TSDL_Color) : String;
begin
  Result := Format('%0:d, %1:d, %2:d, %3:d',
    [aColor.r, aColor.g, aColor.b, aColor.a]);
end;

function Str2SDLColor(aColor : String) : TSDL_Color;
var
  Components : array of String;
begin
  Components := aColor.Split(',');
  Result.R := 0; Result.G := 0; Result.B := 0; Result.A := 255;

  // Lazy read
  if Length(Components) < 1 then Exit;
  Result.R := EnsureRange(StrToInt(Components[0]), 0, 255);
  if Length(Components) < 2 then Exit;
  Result.G := EnsureRange(StrToInt(Components[1]), 0, 255);
  if Length(Components) < 3 then Exit;
  Result.B := EnsureRange(StrToInt(Components[2]), 0, 255);
  if Length(Components) < 4 then Exit;
  Result.A := EnsureRange(StrToInt(Components[3]), 0, 255);
end;
*)
end.
