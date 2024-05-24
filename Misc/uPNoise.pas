unit uPNoise;
// Initially copied from:
//   https://sourceforge.net/p/lazarus-ccr/svn/48/tree/examples/noise/noise.pas
// on 2024/05/22

// It is a partial implementation of Hugo Elias noise:
//   http://web.archive.org/web/20160325134143/http://freespace.virgin.net/hugo.elias/models/m_perlin.htm

// Changes made by Chixpy:
//   - Renamed functions names.
//   - Added some Help and explanations.
//   - Some code modifications, but not the base algorithms.
//   - "Implemented" all Hugo Elias method, called HENoise
//     - Except different prime numbers for RNGs.

// ToDo:
//   - Implement actual Improved Perlin Noise function... from Ken Perlin
//     https://cs.nyu.edu/~perlin/noise/dO_ob
//   - Implement old/classic/original Perlin Noise.
//   - Interpolation functions are enough generic and useful to move them
//     to uCHXMath.
//   - Maybe create classes as there are some configurable properties:
//      - Primes, Interpolation used, persistence, octaves in HE.
//      - Hash table in Perlin.
//      - Cache tables for nearby values .

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TPNoiseInterp = (PNILinear, PNICos, PNICubic);

  // Interpolations
  // --------------

function Linear_Interpolate(a, b, x : Double) : Double;
{< Linear interpolation between 2 values.

   @param(a First point.)
   @param(b Second point.)
   @param(x Interpolation value [0..1]. 0 returns `a`; 1 return `b`.)

   Is posible to pass an `x` out of range, but it is not the place to
     explain it. XD
}
function Cosine_Interpolate(a, b, x : Double) : Double;
{< Cosine interpolation between 2 values. Same as before.

   Generates a smother curve than linear (actually "linear" is not a "curve"),
     with egdes in some case.

   @param(a First point.)
   @param(b Second point.)
   @param(x Interpolation value [0..1]. 0 returns `a`; 1 return `b`.)
}
function Cubic_Interpolate(v0, v1, v2, v3, x : Double) : Double;
{< Cubic interpolation between 2 values, but requires 4 of them.

   Generates a smother curve than Cosine, but time consuming.

   @param(v0 Point before first point.)
   @param(v1 First point.)
   @param(v2 Second point.)
   @param(v3 Point after second point.)
   @param(x Interpolation value [0..1]. 0 returns `v1`; 1 return `v2`.)
}

// Hugo Elias' Noise 1D
// -------------------

function HENoiseRNG1D(x : LongInt) : Double;
{< Random number generator between [-1..1] to generate Hugo Elias' Noise.

   Actually Hugo Elias says that we must change the big prime numbers in this
     generator on every octave iteration.
}
function HESmoothNoise1D(x : LongInt) : Double;
{< Smoothed Noise 1D.

   Result value is affected by neightbours values. For example to get a 1 with
     this function, three consecutive 1 must be returned by HENoiseRNG1D.
}
function HEInterpNoise1D(x : Double;
  InterMth : TPNoiseInterp = PNICos) : Double;
{< Continuous Smoothed Noise 1D, created by interpolation on HESmoothNoise1D. }
function HENoise1D(x : Double; Octaves : byte = 4;
  Persistence : Double = 0.25; InterMth : TPNoiseInterp = PNICos) : Double;
{< Actual Hugo Elias' Noise (1D) function. }

// Hugo Elias' Noise 2D
// -------------------

function HENoiseRNG2D(x, y : integer) : Double;
{< Random number generator between [-1..1] to generate Hugo Elias' Noise on 2D. }
function HESmoothNoise2D(x, y : integer) : Double;
{< Smoothed Noise 2D.

   Result value is affected by neightbours values.

   For example to get a 1 with this function, nine contiguous 1 must be
     returned by RNGNoise2D.
}
function HEInterpNoise2D(x, y : Double;
  InterMth : TPNoiseInterp = PNILinear) : Double;
{< Continuous Smoothed Noise 2D, created by interpolation on HESmoothNoise2D. }
function HENoise2D(x, y : Double; Octaves : byte = 4;
  Persistence : Double = 0.25; InterMth : TPNoiseInterp = PNILinear) : Double;
{< Actual Hugo Elias' Perlin Noise (2D) function.

   Repeats every 57 units in x as seen in HENoiseRNG2D, not sure in y.}


implementation

// Interpolations
// --------------

function Linear_Interpolate(a, b, x : Double) : Double;
begin
  Result := a * (1 - x) + b * x;
end;

function Cosine_Interpolate(a, b, x : Double) : Double;
var
  f : Double;
begin
  f := (1 - cos(x * Pi)) * 0.5;
  Result := a * (1 - f) + b * f;
end;

function Cubic_Interpolate(v0, v1, v2, v3, x : Double) : Double;
var
  P, Q, R, S : Double;
begin
  P := (v3 - v2) - (v0 - v1);
  Q := (v0 - v1) - P;
  R := v2 - v0;
  S := v1;

  Result := P * x * x * x + Q * x * x + R * x + S;
end;

// Hugo Elias Noise 1D
// -------------------

function HENoiseRNG1D(x : LongInt) : Double;
var
  xl : LongInt;
begin
  {$PUSH}
  {$Q-}{$R-}// Removing arithmetic overflow
  xl := (x shl 13) xor x;
  // Not sure about 1376312589, I know 15731 and 789221 are prime numbers.
  xl := (xl * (xl * xl * 15731 + 789221) + 1376312589) and $7fffffff;
  {$POP}

  // LongInt := [-2147483648..2147483647] -> 2147483648 / 2 = 1073741824
  Result := 1.0 - (xl / 1073741824);
end;

function HESmoothNoise1D(x : LongInt) : Double;
begin
  //Result := HENoiseRNG1D(x) / 2 + HENoiseRNG1D(x - 1) / 4 +
  //  HENoiseRNG1D(x + 1) / 4;
  Result := HENoiseRNG1D(x) * 0.5 + HENoiseRNG1D(x - 1) * 0.25 +
    HENoiseRNG1D(x + 1) * 0.25;
end;

function HEInterpNoise1D(x : Double; InterMth : TPNoiseInterp) : Double;
var
  IntX : LongInt;
  FrcX : Double;
  v1, v2, v0, v3 : Double;
begin
  IntX := Floor(x);
  FrcX := Frac(x); // Returns sign
  if FrcX < 0 then FrcX := 1 + FrcX; // -0.25 -> 0.75

  v1 := HESmoothNoise1D(IntX);
  v2 := HESmoothNoise1D(IntX + 1);

  case InterMth of
    PNILinear : Result := Linear_Interpolate(v1, v2, FrcX);
    // PNICos: ;
    PNICubic :
    begin
      v0 := HESmoothNoise1D(IntX - 1);
      v3 := HESmoothNoise1D(IntX + 2);
      Result := Cubic_Interpolate(v0, v1, v2, v3, FrcX);
    end
    else // PNICos
      Result := Cosine_Interpolate(v1, v2, FrcX);
  end;
end;

function HENoise1D(x : Double; Octaves : byte; Persistence : Double;
  InterMth : TPNoiseInterp) : Double;
var
  i : integer;
begin
  Result := 0;

  for i := 0 to Octaves do
  begin
    //Frequency := 2 ** i; // IntPower(2, i)
    //Amplitude := Persistence ** i; // Power(Persistence,i)
    Result := Result + HEInterpNoise1D(x * IntPower(2, i), InterMth) *
      Power(Persistence, i);
  end;
end;

// Hugo Elias Noise 2D
// -------------------

function HENoiseRNG2D(x, y : integer) : Double;
begin
  Result := HENoiseRNG1D(x + y * 57);
end;

function HESmoothNoise2D(x, y : integer) : Double;
begin
  Result := 0;
  // Corners
  Result := (HENoiseRNG2D(x - 1, y - 1) + HENoiseRNG2D(x + 1, y - 1) +
    HENoiseRNG2D(x - 1, y + 1) + HENoiseRNG2D(x + 1, y + 1)) * 0.0625; // /16
  // Sides
  Result += (HENoiseRNG2D(x - 1, y) + HENoiseRNG2D(x + 1, y) +
    HENoiseRNG2D(x, y - 1) + HENoiseRNG2D(x, y + 1)) * 0.125; // /8
  // Center
  Result += HENoiseRNG2D(x, y) * 0.25; // /4
end;

function HEInterpNoise2D(x, y : Double; InterMth : TPNoiseInterp) : Double;
var
  IntX, IntY : int64;
  FrcX, FrcY, v0, v1, v2, v3, i0, i1, i2, i3 : Double;
begin
  IntX := Floor(x);
  FrcX := Frac(x); // Returns sign
  if FrcX < 0 then FrcX := 1 + FrcX; // -0.25 -> 0.75
  IntY := Floor(y);
  FrcY := Frac(y); // Returns sign
  if FrcY < 0 then FrcY := 1 + FrcY; // -0.25 -> 0.75

  // PNILinear, PNICos can

  case InterMth of
    // PNILinear : ; Implemented in else;
    PNICos :
    begin
      // X-Interpolation
      v1 := HESmoothNoise2D(IntX, IntY);
      v2 := HESmoothNoise2D(IntX + 1, IntY);
      i1 := Cosine_Interpolate(v1, v2, FrcX);

      v1 := HESmoothNoise2D(IntX, IntY + 1);
      v2 := HESmoothNoise2D(IntX + 1, IntY + 1);
      i2 := Cosine_Interpolate(v1, v2, FrcX);

      // Y-Interpolation
      Result := Cosine_Interpolate(i1, i2, FrcY);
    end;
    PNICubic :
    begin
      // X-Interpolation
      // Before first point
      v0 := HESmoothNoise2D(IntX - 1, IntY - 1);
      v1 := HESmoothNoise2D(IntX, IntY - 1);
      v2 := HESmoothNoise2D(IntX + 1, IntY - 1);
      v3 := HESmoothNoise2D(IntX + 2, IntY - 1);
      i0 := Cubic_Interpolate(v0, v1, v2, v3, FrcX);

      // First point
      v0 := HESmoothNoise2D(IntX - 1, IntY);
      v1 := HESmoothNoise2D(IntX, IntY);
      v2 := HESmoothNoise2D(IntX + 1, IntY);
      v3 := HESmoothNoise2D(IntX + 2, IntY);
      i1 := Cubic_Interpolate(v0, v1, v2, v3, FrcX);

      // Second point
      v0 := HESmoothNoise2D(IntX - 1, IntY + 1);
      v1 := HESmoothNoise2D(IntX, IntY + 1);
      v2 := HESmoothNoise2D(IntX + 1, IntY + 1);
      v3 := HESmoothNoise2D(IntX + 2, IntY + 1);
      i2 := Cubic_Interpolate(v0, v1, v2, v3, FrcX);

      // After second point
      v0 := HESmoothNoise2D(IntX - 1, IntY + 2);
      v1 := HESmoothNoise2D(IntX, IntY + 2);
      v2 := HESmoothNoise2D(IntX + 1, IntY + 2);
      v3 := HESmoothNoise2D(IntX + 2, IntY + 2);
      i3 := Cubic_Interpolate(v0, v1, v2, v3, FrcX);

      // Y-Interpolation
      Result := Cubic_Interpolate(i0, i1, i2, i3, FrcY);
    end;
    else // PNILinear
    begin
      // X-Interpolation
      v1 := HESmoothNoise2D(IntX, IntY);
      v2 := HESmoothNoise2D(IntX + 1, IntY);
      i1 := Linear_Interpolate(v1, v2, FrcX);

      v1 := HESmoothNoise2D(IntX, IntY + 1);
      v2 := HESmoothNoise2D(IntX + 1, IntY + 1);
      i2 := Linear_Interpolate(v1, v2, FrcX);

      // Y-Interpolation
      Result := Linear_Interpolate(i1, i2, FrcY);
    end;
  end;
end;

function HENoise2D(x, y : Double; Octaves : byte; Persistence : Double;
  InterMth : TPNoiseInterp) : Double;
var
  Frequency, i : integer;
begin
  Result := 0;

  for i := 0 to Octaves do
  begin
    Frequency := 2 ** i;
    //Amplitude := Persistence ** i;
    Result := Result + HEInterpNoise2D(x * Frequency,
      y * Frequency, InterMth) * Power(Persistence, i);
  end;
end;

end.
