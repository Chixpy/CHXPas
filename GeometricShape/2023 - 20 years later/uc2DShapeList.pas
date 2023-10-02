unit uc2DShapeList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  uc2DShape;

type

  cGen2DShapeList = specialize TFPGObjectList<c2DShape>;

  { c2DShapeList }

  c2DShapeList = class(cGen2DShapeList)
    function AddNewShape(a2DShapeClass: T2DShapeClass) : c2DShape;

    procedure ReadSL(aSL: TStringList);
    {< Read Shape list from a a string list. }
    procedure WriteSL(aSL: TStringList);
    {< Writes a Shape list into a string list. }
  end;

implementation

{ c2DShapeList }

function c2DShapeList.AddNewShape(a2DShapeClass: T2DShapeClass): c2DShape;
begin
  Result := a2DShapeClass.Create;
  Add(Result);
end;

procedure c2DShapeList.ReadSL(aSL: TStringList);
begin

end;

procedure c2DShapeList.WriteSL(aSL: TStringList);
begin

end;

end.

