unit ucCTCBranch;
// Coding Rainbow
// Daniel Shiffman
// http://patreon.com/codingtrain
// Code for: https://youtu.be/kKT0v3qhIQY
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCHXPoint3DF;

type

  { cCTCBranch }

  cCTCBranch = class
  private

  public
    parent : cCTCBranch;
    pos : TPoint3DF;
    dir : TPoint3DF;
    Count : integer;
    saveDir : TPoint3DF;
    len : Double;

    procedure Reset;
    function Next : TPoint3DF;

    constructor Create(v, d : TPoint3DF); overload;
    constructor Create(p : cCTCBranch); overload;
    destructor Destroy; override;
  end;

implementation

{ cCTCBranch }

procedure cCTCBranch.Reset;
begin
  Count := 0;
  dir.Clone(saveDir);
end;

function cCTCBranch.Next : TPoint3DF;
var
 v : TPoint3DF;
begin
  v.clone(dir);
  v.Scale(len);
  Result := pos + v;
end;

constructor cCTCBranch.Create(v, d : TPoint3DF);
begin
  Count := 0;
  len := 5;

  parent := nil;
  pos.Clone(v);
  dir.Clone(d);
  saveDir.Clone(dir);
end;

constructor cCTCBranch.Create(p : cCTCBranch);
begin
  Count := 0;
  len := 5;

  parent := p;
  pos.Clone(parent.Next);
  dir.Clone(parent.dir);
  saveDir.Clone(dir);
end;

destructor cCTCBranch.Destroy;
begin
  inherited Destroy;
end;

end.
