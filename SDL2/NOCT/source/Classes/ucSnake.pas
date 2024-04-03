unit ucSnake;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  { cSnake }

  cSnake = class
  private

  public
    x : integer;
    y : integer;
    xspeed : integer;
    yspeed : integer;
    total : integer;
    tail : array of TPoint;
    size : integer; // scl global

    function Eat(const Food : TPoint) : Boolean;
    procedure Dir(const ax, ay : integer);
    procedure Death;
    procedure Update;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cSnake }

function cSnake.Eat(const Food : TPoint) : Boolean;
var
  p: TPoint;
begin
  p := Point(Self.x, self.y);
  if Food.Distance(p) < 1 then
  begin
    Inc(total);
    SetLength(tail, total);
    tail[total - 1].Create(-size,-size);
    Result := True;
  end
  else
    Result := False;
end;

procedure cSnake.Dir(const ax, ay : integer);
begin
  self.xspeed := ax;
  self.yspeed := ay;
end;

procedure cSnake.Death;
var
  i : integer;
  p: TPoint;
begin
  p := Point(Self.x, self.y);
  i := 0;
  while i < length(tail) do
  begin
    if self.tail[i].Distance(p) < 1 then
    begin
      self.total := 0;
      setlength(tail, 0);
    end;
    Inc(i);
  end;
end;

procedure cSnake.Update;
var
  i : integer;
begin
  i := 0;
  while i < length(tail) - 1 do
  begin
    self.tail[i] := self.tail[i + 1];
    Inc(i);
  end;
  if self.total >= 1 then
    self.tail[self.total - 1] := Point(self.x, self.y);

  self.x := self.x + self.xspeed * self.size;
  self.y := self.y + self.yspeed * self.size;
end;

constructor cSnake.Create;
begin
  self.x := 0;
  self.y := 0;
  self.xspeed := 1;
  self.yspeed := 0;
  self.total := 0;
  self.size := 20;
end;

destructor cSnake.Destroy;
begin
  inherited Destroy;
end;

end.
