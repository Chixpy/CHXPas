unit uCHXColor;
{<
  Color Manipulation Functions.

  Lazarus (LCL) provides the 'GraphUtil' unit with HSL and HSV handling
    functions. However:
  - HSL functions use the [0..255] range for all values (and a Delphi-compatible
    version using [0..65535]).
  - HSV functions use the [0..1] float range, which is more common.

  Nevertheless, it is very common to work with HSX colors using the following
    ranges:
  - H in [0..360);
  - S in [0..100];
  - L/V/I in [0..100]

  These are the target ranges implemented in this unit's conversion functions.

  The simple approach would be scaling original ranges to GraphUtil ranges:
  - H256 := H360 * 256 / 360
  - S256 := S100 * 256 / 100
  - L256 := L100 * 256 / 100;
  - H01  := H360 / 360
  - S01  := S100 / 100
  - V01  := V100 / 100;

  Aside from range checks and the fact that H is cyclic.

  General HSL Algorithm (RGB -> HSL):
  1. Normalize RGB values to [0..1] range.
  2. Find components with maximum and minimum values, and their difference.
  3. Hue formula depends on which component is the maximum:
    - If R is max: Hue := (G - B) / (max - min)
    - If G is max: Hue := 2 + (B - R) / (max - min)
    - If B is max: Hue := 4 + (R - G) / (max - min)
  4. Convert to hexagesimal degrees: [-1..5] -> [0..360)
  5. Lightness is the average of max and min: L := (max + min) / 2
  6. Saturation:
    - If L <= 1/2: S := (max - min) / (max + min) = (max - min) / 2L
    - If L > 1/2:  S := (max - min) / (2 - (max + min)) = (max - min) / (2 - 2L)
  
  Implemented Optimizations & Custom Tweaks:
  - Avoid normalizing R, G, B or their difference to [0..1] float range
    initially. This saves 3 or 4 divisions at the cost of adding a
    multiplication in the saturation divisor (which is also optimized out
    later).
  - To solve negative Hue when R is the maximum and (G - B) < 0, we simply check
    if B > G and then add 6. Other solutions use modulo operations, which are
    slower.
  - For saturation, some implementations use absolute value to eliminate the
    L < 1/2 conditional. We reverse this and only compare integers.
  - Separated the function that returns the Hue angle so it can be shared by
    both HSV and HSI logic.

  Fast HUE to RGB Function (CHXFastHue):
  - Returns a RGB color from a HUE in [0..255] range faster than GraphUtil, 
    without Saturation or Lightness parameters. Useful for representing 8-bit 
    values that would normally be rendered in grayscale.
  - Logic: Vary 1 component while another is at max (255) and the last one is 
    at min (0). 
  - Note: It is slightly imprecise because 256 is not perfectly divisible by 6, 
    but using integers only makes it worthwhile.
    - HUE 360:   0    60     120    180    240    300    360=0
    - HUE 256:   0   42.66  85.33   128   170.66 213.33  256=0
                      |      |              |      |
                     43     85             171    213
  - 43 and 171 must be treated separately to avoid escaping the [0..255] range:
    (6 * 43 = 258);  6 * (171 - 128) = 258
  =============================================================================
}
{$mode objfpc}{$H+}
{$inline ON}

interface

uses
  Classes, SysUtils, Math;

type
  THueColor = record // Common type for HSX Colors.
    A: Byte;        // Alpha first, because it can't be last (FPC Limitation)
    H: Word;        // Hue.
    S: Byte;        // Saturation
    case Byte of  // Variant part must be last in the record
      1: (L: Byte); // HSL
      2: (V: Byte); // HSV
      3: (I: Byte); // HSI
  end;

procedure RGB2HSL(const R, G, B: Byte; out H: Word; out S, L: Byte); 
  inline; overload;
procedure RGB2HSV(const R, G, B: Byte; out H: Word; out S, V: Byte);
  inline; overload;
procedure RGB2HSI(const R, G, B: Byte; out H: Word; out S, I: Byte);
  inline; overload;

// TColor
(*
procedure RGB2HSL(const Color: TColor; out HSXColor: THueColor);
  inline; overload;
procedure RGB2HSV(const Color: TColor; out HSXColor: THueColor);
  inline; overload;
procedure RGB2HSI(const Color: TColor; out HSXColor: THueColor);
  inline; overload;
*)

procedure CHXFastHue(const Hue: Byte; out R, G, B: Byte); inline; overload;
{<
  Returns a RGB color from a HUE in [0..255] range faster than GraphUtil,
    without Saturation or Lightness parameters. Useful for representing 8-bit
    values that would normally be rendered in grayscale.
}
{ function CHXFastHue(const Hue: Byte): TColor; inline; overload; }

procedure HSL2RGB(const H: Word; const S, L: Byte; out R, G, B: Byte); inline;
procedure HSV2RGB(const H: Word; const S, V: Byte; out R, G, B: Byte); inline;

Implementation
procedure IntHue2RGB(const H: Word; const C1, C2, C3: Byte; out R, G, B: Byte);
{ Internal procedure get RGB from HUE }
begin
  case H of
    0..59:    begin R := C1; G := C2; B := C3; end;
    60..119:  begin R := C2; G := C1; B := C3; end;
    120..179: begin R := C3; G := C1; B := C2; end;
    180..239: begin R := C3; G := C2; B := C1; end;
    240..299: begin R := C2; G := C3; B := C1; end;
    otherwise begin R := C1; G := C3; B := C2; end;
  end;
end;

procedure IntRGB2Hue(const R, G, B : Byte; out H: Word; out cMax, cMin, cDif: Byte);
{<
  Internal procedure to calculate HUE and some useful data from RGB.
}
begin
  cMax := max(max(R, G), B);
  cMin := min(min(R, G), B);
  cDif := cMax - cMin;

  if cDif = 0 then // Gris puro
  begin
    H := 0;
    Exit;
  end;

  if cMax = G then
    H := Round((2 + (B - R) / cDif) * 60)
  else if cMax = B then
    H := Round((4 + (R - G) / cDif) * 60)
  else // cMax = R
    if B > G then
      H := Round((6 + (G - B) / cDif) * 60)
    else
      H := Round(((G - B) / cDif) * 60);
end;

procedure RGB2HSL(const R, G, B: Byte; out H: Word; out S, L: Byte); inline; overload;
var
  cMax, cMin, cDif: Byte;
  cSum: Integer;
begin
  IntRGB2Hue(R, G, B, H, cMax, cMin, cDif);
  cSum := cMax + cMin;

  { 
    Actually is:
      L = ((cMax / 255) + (cMin / 255)) / 2 --> In range [0..1]
    To change it to [0..100]:
      L = (cMax + cMin) / (2 * 255) * 100 --> L = cSum * (100 / 510)
  }
  L := Round(cSum * 0.19607843137);

  if cDif = 0 then
    S := 0
  else
  begin
    if cSum <= 255 then
      S := Round((cDif / cSum) * 100)
    else
      S := Round((cDif / (510 - cSum)) * 100);
  end;  
end;

procedure RGB2HSV(const R, G, B: Byte; out H: Word; out S, V: Byte); inline; overload;
var
  cMax, cMin, cDif: Byte;
begin
  IntRGB2Hue(R, G, B, H, cMax, cMin, cDif);
  
  // V = (cMax / 255) * 100 --> cMax * (100 / 255)
  V := System.Round(cMax * 0.392156863);

  if cMax = 0 then
    S := 0
  else
    S := System.Round((cDif / cMax) * 100);
end;

procedure RGB2HSI(const R, G, B: Byte; out H: Word; out S, I: Byte); inline; overload;
var
  cMax, cMin, cDif: Byte;
  cSum: Integer;
begin
  IntRGB2Hue(R, G, B, H, cMax, cMin, cDif);
  cSum := R + G + B;

  // I = ((R + G + B) / (3 * 255)) * 100 -> cSum * (100 / 765)
  I := System.Round(cSum * 0.130718954);

  if cSum = 0 then
    S := 0
  else
    S := System.Round(100 - ((300 * cMin) / cSum));
end;

(*
procedure RGB2HSL(const Color: TColor; out HSXColor: THueColor); inline; overload;
begin
  // Extraemos los canales usando operaciones de bits estándar sobre TColor (AARRGGBB o $00BBGGRR según plataforma)
  RGB2HSL(Red(Color), Green(Color), Blue(Color), HSXColor.H, HSXColor.S, HSXColor.L);
  HSXColor.A := 0; // Por defecto
end;

procedure RGB2HSV(const Color: TColor; out HSXColor: THueColor); inline; overload;
begin
  RGB2HSV(Red(Color), Green(Color), Blue(Color), HSXColor.H, HSXColor.S, HSXColor.V);
  HSXColor.A := 0;
end;

procedure RGB2HSI(const Color: TColor; out HSXColor: THueColor); inline; overload;
begin
  RGB2HSI(Red(Color), Green(Color), Blue(Color), HSXColor.H, HSXColor.S, HSXColor.I);
  HSXColor.A := 0;
end;
*)

procedure CHXFastHue(const Hue: Byte; out R, G, B: Byte); inline; overload;
begin
  case Hue of
    0..42: 
      begin R := 255; G := Hue * 6; B := 0; end;
    43: 
      begin R := 255; G := 255; B := 0; end;
    44..85: 
      begin R := 255 - ((Hue - 43) * 6); G := 255; B := 0; end;
    86..127: 
      begin R := 0; G := 255; B := (Hue - 85) * 6; end;
    128..170: 
      begin R := 0; G := 255 - ((Hue - 128) * 6); B := 255; end;
    171: 
      begin R := 0; G := 0; B := 255; end;
    172..213: 
      begin R := (Hue - 171) * 6; G := 0; B := 255; end;
    214..255: 
      begin R := 255; G := 0; B := 255 - ((Hue - 213) * 6); end;
  end;
end;

(*
function CHXFastHue(const Hue: Byte): TColor; inline; overload;
var
  R, G, B: Byte;
begin
  CHXFastHue(Hue, R, G, B);
  Result := RGBToColor(R, G, B); // Conversión nativa de FPC para empaquetar en TColor
end;
*)

procedure HSL2RGB(const H: Word; const S, L: Byte; out R, G, B: Byte); inline;
var
  fH: Word;
  fS, fL, fC, fX, fm: Real;
begin
  fH := H mod 360;
  fS := Math.EnsureRange(S / 100.0, 0.0, 1.0);
  fL := Math.EnsureRange(L / 100.0, 0.0, 1.0);

  fC := (1.0 - System.Abs(2.0 * fL - 1.0)) * fS;
  fX := fC * (1 - Abs(round(H/60) mod 2 - 1));
  fm := fL - fC / 2.0;

  IntHue2RGB(fH, 
    System.Round((fC + fm) * 255), 
    System.Round((fX + fm) * 255), 
    System.Round(fm * 255), 
    R, G, B
  );
end;

procedure HSV2RGB(const H: Word; const S, V: Byte; out R, G, B: Byte); inline;
var
  fH: Word;
  fS, fV, fC, fX, fm: Real;
begin
  fH := H mod 360;
  fS := Math.EnsureRange(S / 100.0, 0.0, 1.0);
  fV := Math.EnsureRange(V / 100.0, 0.0, 1.0);

  fC := fV * fS;
  fX := fC * (1 - Abs(round(H/60) mod 2 - 1));
  fm := fV - fC;

  IntHue2RGB(fH, 
    System.Round((fC + fm) * 255), 
    System.Round((fX + fm) * 255), 
    System.Round(fm * 255), 
    R, G, B
  );
end;

end.
