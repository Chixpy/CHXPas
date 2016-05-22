unit uCHXGroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { cCHXGroupManager.

  This class manages a generic pseudo-tree of owned groups and items.

  Not teorical implementation, only crazy shit testing generics.

  Groups are handled internally and GroupIDs can't repeat.

  ParentGroup1/ChildGroup1
  ParentGroup2/ChildGroup1
  ParentGroup3/ParentGroup31/ChildGroup1

  ChildGroup1 will be the same group in all cases.
  }

  generic cGenericGroupManager<TGGMData> = class(TObject)
  private
    FFreeObjects: boolean;
    procedure SetFreeObjects(AValue: boolean);

  protected

  public
    constructor Create(FreeObjects: boolean = True);
    destructor Destroy; override;

    property FreeObjects: boolean read FFreeObjects write SetFreeObjects;
  end;

implementation

{ cGenericGroupManager }

procedure cGenericGroupManager.SetFreeObjects(AValue: boolean);
begin
  if FFreeObjects = AValue then
    Exit;
  FFreeObjects := AValue;
end;

constructor cGenericGroupManager.Create(FreeObjects: boolean);
begin
  inherited Create;
  self.FreeObjects := FreeObjects;
end;

destructor cGenericGroupManager.Destroy;
begin
  inherited Destroy;
end;

end.
