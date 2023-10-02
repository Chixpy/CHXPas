unit uc2DShape;

{< Reimplementation of uFigura.pas, uFAbierta, uSegmento, uPolilinea,
     uFCerrada, uPoligono, uTriangulo, uCuadrilatero, uRectangulo and
     uCuadrado from 2003.

   The reason all this shapes (Segment, Polyline, Triangle, Rectangle, etc.)
     are a list of points. We can create constructors or procedures to make
     specific shapes more easy. Being a open or close shape is only a property.
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  ucPoint, ucPointList;

type

  { c2DShape }

  c2DShape = class // (TObject) or (TPersistent) or (TComponent) or ....
  private
    FIsClosed: boolean;
    FPoints: cPointList;
    procedure SetIsClosed(AValue: boolean);

  public
    property Points: cPointList read FPoints;
    {< List of points of the shape. }
    property IsClosed: boolean read FIsClosed write SetIsClosed;
    {< The shape is closed? }

    procedure AddPoint(X, Y: double);
    {< Easier way to add a point.}

    procedure Rotate(Angle: double; Center: cPoint = nil);
    {< Rotates the Shape using a Point as rotation center.

      If center is not defined, the Shape will be rotated from axes origin at
        (0, 0).
    }
    procedure Scale(Factor: double; Origin: cPoint = nil);
    {< Scales the polar distance by a factor using a Point as scale origin.

    If origin is not defined, the Shape will be scaled from axes origin at
      (0, 0).

    So, if origin is not a vertex (or, in some sense, inside the shape) the
      Shape will be translated proportionally from the expecified origin. You
      can think that origin is point in the 2D space that will not be moved
      at all.
    }
    procedure Translate(DX, DY: double);
    {< Translates the Shape a cartesian distance. }

    procedure ReadString(aString: string);
    {< Read Shape's data from a string. }
    function WriteString: string;
    {< Returns Shape's data as string. }

    constructor Create; overload;
    {< Creates a new Shape. }
    constructor Clone(a2DShape: c2DShape); overload;
    {< Creates a new Shape copying another Shape points. }
    destructor Destroy; override;
  end;

  T2DShapeClass = class of c2DShape;

implementation

{ c2DShape }

procedure c2DShape.SetIsClosed(AValue: boolean);
begin
  if FIsClosed = AValue then Exit;
  FIsClosed := AValue;
end;

procedure c2DShape.AddPoint(X, Y: double);
var
  aPoint: cPoint;
begin
  aPoint := cPoint.Create;
  aPoint.X := X;
  aPoint.Y := Y;
  Points.Add(aPoint);
end;

procedure c2DShape.Rotate(Angle: double; Center: cPoint);
var
  i: LongInt;
begin
  i := 0;
  while i < Points.Count do
  begin
    Points[i].Rotate(Angle, Center);
    Inc(i);
  end;
end;

procedure c2DShape.Scale(Factor: double; Origin: cPoint);
var
  i: LongInt;
begin
  i := 0;
  while i < Points.Count do
  begin
    if Assigned(Origin) then
    begin
      Points[i].X := Points[i].X - Origin.X;
      Points[i].Y := Points[i].Y - Origin.Y;
    end;

    Points[i].Scale(Factor);
    Inc(i);

    if Assigned(Origin) then
    begin
      Points[i].X := Points[i].X + Origin.X;
      Points[i].Y := Points[i].Y + Origin.Y;
    end;
  end;
end;

procedure c2DShape.Translate(DX, DY: double);
var
  i: LongInt;
begin
  i := 0;
  while i < Points.Count do
  begin
    Points[i].Translate(DX, DY);
    Inc(i);
  end;
end;

procedure c2DShape.ReadString(aString: string);
begin

end;

function c2DShape.WriteString: string;
begin

end;

constructor c2DShape.Create;
begin
  inherited Create;

  FPoints := cPointList.Create(True);
end;

constructor c2DShape.Clone(a2DShape: c2DShape);
var
  i: LongInt;
  aPoint: cPoint;
begin
  Create;

  i := 0;
  while i < a2DShape.Points.Count do
  begin
    aPoint := cPoint.Clone(a2DShape.Points[i]);
    Points.Add(aPoint);
    Inc(i);
  end;
end;

destructor c2DShape.Destroy;
begin
  FPoints.Free;

  inherited Destroy;
end;

end.
