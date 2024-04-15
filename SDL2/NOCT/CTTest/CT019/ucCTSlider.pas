unit ucCTSlider;
// Chuck England
// Written for Processing:
//   Daniel Shiffman
//   http://youtube.com/thecodingtrain
//   http://codingtra.in

// Note that the original p5.js example uses a DOM
// object for the slider.
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  uProcUtils;

type

  { cCTSlider }

  cCTSlider = class
  private

  public
    x : integer;
    y : integer;
    w : integer;
    h : integer;
    controlSize : integer;

    minValue : Double;
    maxValue : Double;
    Value : Double;
    increment : Double;

    isDragging : Boolean;

    procedure handleInteractions(Mx, My : LongWord; Dragging: Boolean);

    function xPosition(val : Double) : Double;

    constructor Create(x_, y_ : integer; min_, max_, start_, increment_ : Double);
    destructor Destroy; override;
  end;

implementation

{ cCTSlider }

procedure cCTSlider.handleInteractions(Mx, My : LongWord; Dragging : Boolean);
var
  x1 , d: Int64;
  change : Double;
begin

  if not Dragging then
    isDragging := False;

  if isDragging then
  begin
    x1 := Round(xPosition(value));
    d := Mx - x1;
    change := d * (maxValue - minValue) / w;
    Value += change;
    value -= value mod increment;
    value := EnsureRange(value, minValue, maxValue);
  end
  else
  begin
    x1 := Round(xPosition(value));
    if (My >= y) and (My <= y + h) and (Mx >= x1 - controlSize div 2) and
      (Mx <= x1 + controlSize div 2) then
      isDragging := true;
  end;

  // if (isDragging) {
  //  if (mousePressed) {
  //    float x1 = screenX(xPosition(value), y);
  //    float d = mouseX - x1;
  //    float change = d * (max - min) / w;
  //    value += change;
  //    value -= value % increment;
  //    value = min(max, max(min, value));
  //  } else {
  //    isDragging = false;
  //    println("end drag");
  //  }
  //} else {
  //  float x1 = xPosition(value);
  //  if (mousePressed && mouseY >= screenY(x1, y) && mouseY <= screenY(x1, y+h)
  //  && mouseX >= screenX(x1-controlSize/2,y) && mouseX <= screenX(x1+controlSize/2,y)) {
  //    println("start drag");
  //    isDragging = true;
  //  }
  //}
end;

function cCTSlider.xPosition(val : Double) : Double;
begin
  Result := min(x + w, max(x, map(val, minValue , maxValue, x, x + w)));
end;

constructor cCTSlider.Create(x_, y_ : integer; min_, max_, start_, increment_ : Double);
begin
  w := 200;
  h := 20;
  controlSize := 7;

  x := x_;
  y := y_;
  minValue := min_;
  maxValue := max_;
  Value := start_;
  increment := increment_;

  isDragging := False;
end;

destructor cCTSlider.Destroy;
begin
  inherited Destroy;
end;

end.
