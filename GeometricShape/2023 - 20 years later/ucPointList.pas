unit ucPointList;
{< Reimplementation of uListaPuntos.pas from 2003. }
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  ucPoint;

type

  cGenPointList = specialize TFPGObjectList<cPoint>;

  { cPointList }

  cPointList = class(cGenPointList)
    function AddNewPoint : cPoint;
    procedure ReadString(aString: string);
    {< Read point list from a string. }
    function WriteString: string;
    {< Returns point list as string. }
  end;


implementation

{ cPointList }

function cPointList.AddNewPoint: cPoint;
begin
  Result := cPoint.Create;
  Add(Result);
end;

procedure cPointList.ReadString(aString: string);
begin

end;

function cPointList.WriteString: string;
begin

end;

end.

