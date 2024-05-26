unit uCHXPNoise;
{ Initially copied from:
   https://sourceforge.net/p/lazarus-ccr/svn/48/tree/examples/noise/noise.pas
   on 2024/05/22

It is a partial implementation of Hugo Elias' Perlin Noise implementation:
  http://web.archive.org/web/20160325134143/http://freespace.virgin.net/hugo.elias/models/m_perlin.htm

Changes made by Chixpy:
  - Renamed functions names.
  - Added some Help and explanations.
  - Some code modifications and optimizations, but not the base algorithms.
  - Moved generic interpolation functions to uCHXMath and improved a little.
  - "Implemented" all Hugo Elias method called HENoiseXD, except different
    prime numbers for RNGs in every octave.
  - Implemented actual Improved Perlin Noise function:
    [from Ken Perlin](https://cs.nyu.edu/~perlin/noise/) dO_ob

ToDo:
  - Implement [old/classic/original Perlin Noise](https://mrl.cs.nyu.edu/~perlin/doc/oscar.html#noise).
  - Maybe create classes as there are some configurable properties:
    - Primes, Interpolation used, persistence, octaves in HE.
    - Hash table in Improved Perlin.
    - Cache tables for nearby values .
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  uCHXMath;

type
  TPNoiseInterp = (PNILinear, PNICos, PNICubic);

  // Hugo Elias' Noise 1D
  // -------------------

function HENoiseRNG1D(const X : LongInt) : Double;
{< Random number generator between [-1..1] to generate Hugo Elias' Noise.

   Actually Hugo Elias says that we must change the big prime numbers in this
     generator on every octave iteration.
}
function HESmoothNoise1D(const X : LongInt) : Double;
{< Smoothed Noise 1D.

   Result value is affected by neightbours values. For example to get a 1 with
     this function, three consecutive 1 must be returned by HENoiseRNG1D.
}
function HEInterpNoise1D(const X : Double;
  const InterMth : TPNoiseInterp = PNICos) : Double;
{< Continuous Smoothed Noise 1D, created by interpolation on HESmoothNoise1D. }
function HENoise1D(const X : Double; Octaves : Byte = 4;
  const Persistence : Double = 0.25;
  const InterMth : TPNoiseInterp = PNICos) : Double;
{< Actual Hugo Elias' Noise (1D) function. }

// Hugo Elias' Noise 2D
// -------------------

function HENoiseRNG2D(const X, Y : Integer) : Double;
{< Random number generator between [-1..1] to generate Hugo Elias' Noise on 2D. }
function HESmoothNoise2D(const X, Y : Integer) : Double;
{< Smoothed Noise 2D.

   Result value is affected by neightbours values.

   For example to get a 1 with this function, nine contiguous 1 must be
     returned by RNGNoise2D.
}
function HEInterpNoise2D(const X, Y : Double;
  const InterMth : TPNoiseInterp = PNILinear) : Double;
{< Continuous Smoothed Noise 2D, created by interpolation on HESmoothNoise2D. }
function HENoise2D(const X, Y : Double; const Octaves : Byte = 4;
  const Persistence : Double = 0.25;
  const InterMth : TPNoiseInterp = PNILinear) : Double;
{< Actual Hugo Elias' Perlin Noise (2D) function.

   Repeats every 57 units in x as seen in HENoiseRNG2D, not sure in y.}

// Perlin Improved Noise
// ---------------------

function PINNoise(const X : Double; const Y : Double = 0;
  const Z : Double = 0) : Double;
{< https://mrl.cs.nyu.edu/~perlin/noise/ }

implementation

// Hugo Elias Noise 1D
// -------------------

function HENoiseRNG1D(const X : LongInt) : Double;
var
  xl : LongInt;
begin
  {$PUSH}
  {$Q-}{$R-}// Removing arithmetic overflow error
  xl := (X shl 13) xor X;
  // Not sure about 1376312589, is not a prime.
  // 15731 and 789221 are prime numbers.
  // $7fffffff basically removes negative sign and removes -2147483648 value.
  xl := (xl * (xl * xl * 15731 + 789221) + 1376312589) and $7fffffff;
  {$POP}

  // xl = [0..2147483647] -> 2147483648 / 2 = 1073741824
  // So xl / 1073741824 = [0, 2) - 1 -> Result = [-1..1)
  Result := 1 - (xl / 1073741824);
end;

function HESmoothNoise1D(const X : LongInt) : Double;
begin
  //Result := HENoiseRNG1D(x) / 2 + HENoiseRNG1D(x - 1) / 4 +
  //  HENoiseRNG1D(x + 1) / 4;
  Result := HENoiseRNG1D(X) * 0.5 + HENoiseRNG1D(X - 1) * 0.25 +
    HENoiseRNG1D(X + 1) * 0.25;
end;

function HEInterpNoise1D(const X : Double;
  const InterMth : TPNoiseInterp) : Double;
var
  XInt : LongInt;
  XFrc : Double;
  v1, v2, v0, v3 : Double;
begin
  XInt := Floor(X); // Rounds x towards infinite negative
  XFrc := X - XInt; // Works with negative values.

  v1 := HESmoothNoise1D(XInt);
  v2 := HESmoothNoise1D(XInt + 1);

  case InterMth of
    PNILinear : Result := Linear_Interpolate(v1, v2, XFrc);
    // PNICos: In else;
    PNICubic :
    begin
      v0 := HESmoothNoise1D(XInt - 1);
      v3 := HESmoothNoise1D(XInt + 2);
      Result := Cubic_Interpolate(v0, v1, v2, v3, XFrc);
    end
    else // PNICos
      Result := Cosine_Interpolate(v1, v2, XFrc);
  end;
end;

function HENoise1D(const X : Double; Octaves : Byte;
  const Persistence : Double; const InterMth : TPNoiseInterp) : Double;
var
  i : Integer;
begin
  Result := 0;

  for i := 0 to Octaves do
  begin
    //Frequency := 2 ** i; // IntPower(2, i)
    //Amplitude := Persistence ** i; // Power(Persistence,i)
    Result := Result + HEInterpNoise1D(X * IntPower(2, i), InterMth) *
      Power(Persistence, i);
  end;
end;

// Hugo Elias Noise 2D
// -------------------

function HENoiseRNG2D(const X, Y : Integer) : Double;
begin
  Result := HENoiseRNG1D(X + Y * 57);
end;

function HESmoothNoise2D(const X, Y : Integer) : Double;
begin
  Result := 0;
  // Corners
  Result := (HENoiseRNG2D(X - 1, Y - 1) + HENoiseRNG2D(X + 1, Y - 1) +
    HENoiseRNG2D(X - 1, Y + 1) + HENoiseRNG2D(X + 1, Y + 1)) * 0.0625; // /16
  // Sides
  Result += (HENoiseRNG2D(X - 1, Y) + HENoiseRNG2D(X + 1, Y) +
    HENoiseRNG2D(X, Y - 1) + HENoiseRNG2D(X, Y + 1)) * 0.125; // /8
  // Center
  Result += HENoiseRNG2D(X, Y) * 0.25; // /4
end;

function HEInterpNoise2D(const X, Y : Double;
  const InterMth : TPNoiseInterp) : Double;
var
  XInt, YInt : Int64;
  XFrc, YFrc, v0, v1, v2, v3, i0, i1, i2, i3 : Double;
begin
  XInt := Floor(X); // Rounds x towards infinite negative
  XFrc := X - XInt; // Works with negative values.
  YInt := Floor(Y);
  YFrc := Y - YInt;

  // Algorithms kept separated for clarity.

  case InterMth of
    // PNILinear : ; Implemented in else;
    PNICos :
    begin
      // X-Interpolation
      v1 := HESmoothNoise2D(XInt, YInt);
      v2 := HESmoothNoise2D(XInt + 1, YInt);
      i1 := Cosine_Interpolate(v1, v2, XFrc);

      v1 := HESmoothNoise2D(XInt, YInt + 1);
      v2 := HESmoothNoise2D(XInt + 1, YInt + 1);
      i2 := Cosine_Interpolate(v1, v2, XFrc);

      // Y-Interpolation
      Result := Cosine_Interpolate(i1, i2, YFrc);
    end;
    PNICubic :
    begin
      // X-Interpolation
      // Before first point
      v0 := HESmoothNoise2D(XInt - 1, YInt - 1);
      v1 := HESmoothNoise2D(XInt, YInt - 1);
      v2 := HESmoothNoise2D(XInt + 1, YInt - 1);
      v3 := HESmoothNoise2D(XInt + 2, YInt - 1);
      i0 := Cubic_Interpolate(v0, v1, v2, v3, XFrc);

      // First point
      v0 := HESmoothNoise2D(XInt - 1, YInt);
      v1 := HESmoothNoise2D(XInt, YInt);
      v2 := HESmoothNoise2D(XInt + 1, YInt);
      v3 := HESmoothNoise2D(XInt + 2, YInt);
      i1 := Cubic_Interpolate(v0, v1, v2, v3, XFrc);

      // Second point
      v0 := HESmoothNoise2D(XInt - 1, YInt + 1);
      v1 := HESmoothNoise2D(XInt, YInt + 1);
      v2 := HESmoothNoise2D(XInt + 1, YInt + 1);
      v3 := HESmoothNoise2D(XInt + 2, YInt + 1);
      i2 := Cubic_Interpolate(v0, v1, v2, v3, XFrc);

      // After second point
      v0 := HESmoothNoise2D(XInt - 1, YInt + 2);
      v1 := HESmoothNoise2D(XInt, YInt + 2);
      v2 := HESmoothNoise2D(XInt + 1, YInt + 2);
      v3 := HESmoothNoise2D(XInt + 2, YInt + 2);
      i3 := Cubic_Interpolate(v0, v1, v2, v3, XFrc);

      // Y-Interpolation
      Result := Cubic_Interpolate(i0, i1, i2, i3, YFrc);
    end;
    else // PNILinear
    begin
      // X-Interpolation
      v1 := HESmoothNoise2D(XInt, YInt);
      v2 := HESmoothNoise2D(XInt + 1, YInt);
      i1 := Linear_Interpolate(v1, v2, XFrc);

      v1 := HESmoothNoise2D(XInt, YInt + 1);
      v2 := HESmoothNoise2D(XInt + 1, YInt + 1);
      i2 := Linear_Interpolate(v1, v2, XFrc);

      // Y-Interpolation
      Result := Linear_Interpolate(i1, i2, YFrc);
    end;
  end;
end;

function HENoise2D(const X, Y : Double; const Octaves : Byte;
  const Persistence : Double; const InterMth : TPNoiseInterp) : Double;
var
  Frequency, i : Integer;
begin
  Result := 0;

  for i := 0 to Octaves do
  begin
    Frequency := 2 ** i;
    //Amplitude := Power(Persistence, i);
    Result := Result + HEInterpNoise2D(X * Frequency,
      Y * Frequency, InterMth) * Power(Persistence, i);
  end;
end;

function PINNoise(const X : Double; const Y : Double;
  const Z : Double) : Double;
const
  p : array[0..511] of Byte = // Original permutation table
    (151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
    140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247,
    120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177,
    33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165,
    71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211,
    133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63,
    161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135,
    130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226,
    250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59,
    227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152,
    2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253,
    19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
    251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235,
    249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176,
    115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29,
    24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180,
    // CHX: Duplicated table to avoid range errors, instead of duplicating it
    //   at runtime as in original code.
    // This saves at least ten AND|mod
    151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
    140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247,
    120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177,
    33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165,
    71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211,
    133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63,
    161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135,
    130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226,
    250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59,
    227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152,
    2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253,
    19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
    251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235,
    249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176,
    115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29,
    24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180);

  function fade(const t : Double) : Double; inline;
  begin
    Result := t * t * t * (t * (t * 6 - 15) + 10);
  end;

  function lerp(const t, a, b : Double) : Double; inline;
  begin
    Result := a + t * (b - a);
  end;

  function grad(const hash : Integer; const aX, aY, aZ : Double) : Double;
    //var
    //  h : Integer;
    //  u, v : Double;
  begin
    // Original algorithm ported to Pascal:
    //h := hash and $FFFF; // CONVERT LO 4 BITS OF HASH CODE
    //u := IfThen(h < 8, aX, aY); // INTO 12 GRADIENT DIRECTIONS.
    //v := IfThen(h < 4, aY, IfThen((h = 12) or (h = 14), aX, aZ));
    //Result := IfThen((h and 1) = 0, u, -u) + IfThen((h and 2) = 0, v, -v);

    //Optimization by Riven
    // https://riven8192.blogspot.com/2010/08/calculate-perlinnoise-twice-as-fast.html

    case (hash and $F) of
      0 : Result := aX + aY;
      1 : Result := -aX + aY;
      2 : Result := aX - aY;
      3 : Result := -aX - aY;
      4 : Result := aX + aZ;
      5 : Result := -aX + aZ;
      6 : Result := aX - aZ;
      7 : Result := -aX - aZ;
      8 : Result := aY + aZ;
      9 : Result := -aY + aZ;
      $A : Result := aY - aZ;
      $B : Result := -aY - aZ;
      $C : Result := aY + aX; // Same as $0
      $D : Result := -aY + aZ; // Same as $9
      $E : Result := aY - aX; // Same as $1
      else // $F
        Result := -aY - aZ;   // Same as $B
    end;
  end;
var
  XInt, YInt, ZInt, A, B, AA, AB, BA, BB : Integer;
  XFrc, YFrc, ZFrc, u, v, w : Double;
begin
  // FIND UNIT CUBE THAT CONTAINS POINT. CHX: (1/2)
  XInt := Floor(X);
  YInt := Floor(Y);
  ZInt := Floor(Z);

  // CHX: Saving 3 Floor() calls
  // FIND RELATIVE X,Y,Z OF POINT IN CUBE.
  XFrc := X - XInt;
  YFrc := Y - YInt;
  ZFrc := Z - ZInt;

  // FIND UNIT CUBE THAT CONTAINS POINT. CHX: (1/2)
  XInt := XInt and $FF;
  YInt := YInt and $FF;
  ZInt := ZInt and $FF;

  // COMPUTE FADE CURVES FOR EACH OF X,Y,Z.
  u := fade(XFrc);
  v := fade(YFrc);
  w := fade(ZFrc);

  // HASH COORDINATES OF THE 8 CUBE CORNERS,
  A := p[XInt] + YInt; AA := p[A] + ZInt; AB := p[A + 1] + ZInt;
  B := p[XInt + 1] + YInt; BA := p[B] + ZInt; BB := p[B + 1] + ZInt;

  // AND ADD BLENDED RESULTS FROM 8 CORNERS OF CUBE
  Result := lerp(w, lerp(v, lerp(u, grad(p[AA], XFrc, YFrc, ZFrc),
    { C                            }grad(p[BA], XFrc - 1, YFrc, ZFrc)),
    { O    T               }lerp(u, grad(p[AB], XFrc, YFrc - 1, ZFrc),
    { M    R                       }grad(p[BB], XFrc - 1, YFrc - 1, ZFrc))),
    { M    I       }lerp(v, lerp(u, grad(p[AA + 1], XFrc, YFrc, ZFrc - 1),
    { E    C                       }grad(p[BA + 1], XFrc - 1, YFrc, ZFrc - 1)),
    { N    K               }lerp(u, grad(p[AB + 1], XFrc, YFrc - 1, ZFrc - 1),
    { T                            }grad(p[BB + 1], XFrc - 1,
    YFrc - 1, ZFrc - 1))));
end;

end.
