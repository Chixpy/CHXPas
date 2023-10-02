unit ucPoint;

{< Reimplementation of uPunto.pas from 2003. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  { Note: FreePascal has Types.TPointF as a extended record (wich is more
    similar to a Turbo Pascal object than a Delphi class).

    Actually, TPoint is defined as an object in FreePascal help:
      https://www.freepascal.org/docs-html/rtl/objects/tpoint.html
      (but in FreePascal source is an extended record).

    This is reimplementation of a University exercise, there is not need
      to implement a cPoint class.
  }

  { cPoint }

  cPoint = class // (TObject) or (TPersistent) or (TComponent) or ....
  private
    FX: double;
    FY: double;
    procedure SetX(AValue: double);
    procedure SetY(AValue: double);

  public
    property X: double read FX write SetX;
    {< Cartesian X coordinate. }
    property Y: double read FY write SetY;
    {< Cartesian Y coordinate. }

    { Polar coordinates.

      They can be:
        property Distance: Double read GetDistance; // write SetDistance;
        property Angle: Double read GetAngle; // write SetAngle;

      But as they are calculated from cartesian, it's better left them as
        methods. And for commented write methods, see SetPolar, SetDistance and
        SetAngle comments.
    }
    function GetDistance: double;
    {< Polar radial distance from origin (0, 0). }
    function GetAngle: double;
    {< Polar angle in radians. 0 is positive X axis and counterclockwise. }

    { To set a Point from polar it's better to set both values together,
        instead of procedures:
        - procedure SetDistance(Distance: Double);
        - procedure SetAngle(Angle: Double);

      But, this procedures are useful to "change" the angle or distance of a
        Point keeping the other factor. So, we will implement this methods
        later with a better name.
    }
    procedure SetPolar(Distance, Angle: double);
    {< Sets the Point from polar coordinates. }

    procedure ChangeDistance(Distance: double);
    {< Changes the polar distance of the Point keeping its angle. }
    procedure ChangeAngle(Angle: double);
    {< Changes the polar angle of the Point keeping its distance.

      If polar distance is 0, angle is not keeped.
    }

    procedure Rotate(Angle: double; Center: cPoint = nil);
    {< Rotates the Point using another as rotation center.

      If center is not defined, the Point will be rotated from axes origin at
        (0, 0).
    }
    procedure Scale(Factor: double);
    {< Scales the polar distance by a factor. }
    procedure Translate(DX, DY: double);
    {< Translates the Point a cartesian distance. }

    procedure ReadString(aString: string);
    {< Read Point's data from a string. }
    function WriteString: string;
    {< Returns Point's data as string. }

    constructor Create; overload;
    {< Creates a Point at (0,0). }
    constructor Create(Distance, Angle: double); overload;
    {< Creates a Point from polar coordinates. }
    constructor Clone(aPoint: cPoint); overload;
    {< Creates a Point copying another Point coordinates. }

    destructor Destroy; override;
  end;

implementation

{ cPoint }

procedure cPoint.SetX(AValue: double);
begin
  if FX = AValue then Exit;
  FX := AValue;
end;

procedure cPoint.SetY(AValue: double);
begin
  if FY = AValue then Exit;
  FY := AValue;
end;

function cPoint.GetDistance: double;
begin
  // With Math unit
  Result := Hypot(X, Y);

  { With Math unit too:
      Result := sqrt(power(X, 2) + power(Y, 2));

    Without Math unit:
      Result := sqrt(sqr(X) + sqr(Y));
    or
      Result := sqrt((X * X) + (Y * Y));
    or
      Result := sqrt(X ^ 2 + X ^ 2);
    or
      etc.
  }
end;

function cPoint.GetAngle: double;
begin
  if (X = 0) and (Y = 0) then
  begin
    Result := 0; // or raise an error
    Exit;
  end;

  // With Math unit
  Result := ArcTan2(Y, X);

  { Without Math unit:

    1. Tipical ArcTan method (or copy ArtTan2 implementation):

      if x = 0 then
      begin
        if y = 0 then
          Result := 0 // or raise an error
        else
          Result := Sign(Y) * Pi / 2;
      end
      else if x > 0 then
        Result := ArcTan(Y/X)
      else
      begin
        if y >= 0 then
          Result := ArcTan(Y/X) + Pi
        else
          Result := ArcTan(Y/X) - Pi;
      end;

    2. ArcCos method:

      Dist := GetDistance;
      if Dist = 0 then
      begin
        Result := 0; // or raise an error
        Exit;
      end;

      if y >= 0 then
        Result := ArcCos(X / Dist);
      else
        Result := - ArcCos(X / Dist);

  }
end;

procedure cPoint.SetPolar(Distance, Angle: double);
begin
  X := abs(Distance) * cos(Angle);
  Y := abs(Distance) * sin(Angle);
end;

procedure cPoint.ChangeDistance(Distance: double);
var
  Angle: double;
begin
  Angle := GetAngle;
  SetPolar(Distance, Angle);
end;

procedure cPoint.ChangeAngle(Angle: double);
var
  Distance: double;
begin
  Distance := GetDistance;
  SetPolar(Distance, Angle);
end;

procedure cPoint.Rotate(Angle: double; Center: cPoint);
var
  Sinus, Cosinus, TempX: double;
begin
  if Assigned(Center) then
  begin
    X := X - Center.X;
    Y := Y - Center.Y;
  end;

  SinCos(Angle, Sinus, Cosinus);

  TempX := (X * Cosinus) - (Y * Sinus);
  Y := (X * Sinus) + (Y * Cosinus);
  X := TempX;

  if Assigned(Center) then
  begin
    X := X + Center.X;
    Y := Y + Center.Y;
  end;
end;

procedure cPoint.Scale(Factor: double);
begin
  X := X * Factor;
  Y := Y * Factor;
end;

procedure cPoint.Translate(DX, DY: double);
begin
  X := X + DX;
  Y := Y + DY;
end;

procedure cPoint.ReadString(aString: string);
var
  aPos1, aPos2: longint;
begin
  // We must make sure to call StandardFormatSettings of CHXStrUtils in the
  //   main program file.

  aPos1 := Pos('(', aString) + 1;
  if aPos1 < 2 then
    Exit; // or Raise an error.
  aPos2 := Pos(';', aString, aPos1);
  if (aPos2 < 1) then
    Exit; // or Raise an error.

  X := StrToFloatDef(Trim(Copy(aString, aPos1, aPos2 - aPos1)), 0);

  aPos1 := aPos2 + 1;
  aPos2 := Pos(')', aString, aPos1);
  if (aPos2 < 1) then
    // Well, try anyway to the end of the sctring without ')'
    aPos2 := Length(aString);
  //Exit; // Raise an error.
  Y := StrToFloatDef(Trim(Copy(aString, aPos1, aPos2 - aPos1)), 0);

end;

function cPoint.WriteString: string;
begin
  // We must make sure to call StandardFormatSettings of CHXStrUtils in the
  //   main program file.

  Result := '(' + FloatToStr(X) + '; ' + FloatToStr(Y) + ')';
end;

constructor cPoint.Create;
begin
  inherited Create;
  // This properties actually initialize with 0
  X := 0;
  Y := 0;
end;

constructor cPoint.Create(Distance, Angle: double);
begin
  Create;

  SetPolar(Distance, Angle);
end;

constructor cPoint.Clone(aPoint: cPoint);
begin
  Create;

  X := aPoint.X;
  Y := aPoint.Y;
end;

destructor cPoint.Destroy;
begin
  inherited Destroy;
end;

end.
