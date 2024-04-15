unit ucCTBranch;
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

  { cCTBranch }

  cCTBranch = class
  private

  public
    parent : cCTBranch;
    pos : TPoint3DF;
    dir : TPoint3DF;
    Count : integer;
    saveDir : TPoint3DF;
    len : Double;

    procedure Reset;
    function Next : TPoint3DF;

    constructor Create(v, d : TPoint3DF); overload;
    constructor Create(p : cCTBranch); overload;
    destructor Destroy; override;
  end;

implementation

{ cCTBranch }

procedure cCTBranch.Reset;
begin
  Count := 0;
  dir.Clone(saveDir);
end;

function cCTBranch.Next : TPoint3DF;
var
 v : TPoint3DF;
begin
  v.clone(dir);
  v.Scale(len);
  Result := pos + v;
end;

constructor cCTBranch.Create(v, d : TPoint3DF);
begin
  Count := 0;
  len := 5;

  parent := nil;
  pos.Clone(v);
  dir.Clone(d);
  saveDir.Clone(dir);
end;

constructor cCTBranch.Create(p : cCTBranch);
begin
  Count := 0;
  len := 5;

  parent := p;
  pos.Clone(parent.Next);
  dir.Clone(parent.dir);
  saveDir.Clone(dir);
end;

destructor cCTBranch.Destroy;
begin
  inherited Destroy;
end;

end.
