unit uGenericGroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { cGenericGroup.

    Basic group class for cGenericGroupManager.

    Data property is actual GroupItem.
  }

  generic cGenericGroup<TGOGMGroupKey, TGOGMGroupItem, TGOGMItemKey,
    TGOGMItem> = class (TObject)
  private
    type
      cGroupMap = specialize TFPGMap<TGOGMGroupKey, cGenericGroup>;
      cItemMap = specialize TFPGMap<TGOGMItemKey, TGOGMItem>;

  private
    FData: TGOGMGroupItem;
    FGroups: cGroupMap;
    FItems: cItemMap;

  public
    property Groups: cGroupMap read FGroups;
    property Data: TGOGMGroupItem read FData;
    property Items: cItemMap read FItems;

    constructor Create;
    destructor Destroy; override;
  end;

  { cGenericGroupManager class.

  This class manages a generic pseudo-tree of owned groups and items.

  Not teorical implementation, direct implementation of crazy shit testing
  generics.

  Groups are handled internally and GroupIDs can't repeat but used in many
  subgroups.

  ParentGroup1/ChildGroup1
  ParentGroup2/ChildGroup1
  ParentGroup3/ParentGroup31/ChildGroup1

  ChildGroup1 will be the same group in all cases.
  }
  generic cGenericGroupManager<TGOGMGroupKey, TGOGMGroupItem, TGOGMItemKey,
    TGOGMItem> = class (TObject)
  private
    type
      cGenOwnedGroups = specialize TFPGObjectList<TGOGMGroupItem>;
      cGenOwnedItems = specialize TFPGObjectList<TGOGMItem>;

      cRoot = specialize cGenericGroup<TGOGMGroupKey, TGOGMGroupItem, TGOGMItemKey,
    TGOGMItem>;

  private
    FOwnedGroups: cGenOwnedGroups;
    FOwnedItems: cGenOwnedItems;
    FRoot: cRoot;

  protected
    property OwnedItems: cGenOwnedItems read FOwnedItems;
    property OwnedGroups: cGenOwnedGroups read FOwnedGroups;

  public
    property Root: cRoot read FRoot;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cGenericGroupManager }
constructor cGenericGroupManager.Create;
begin
  inherited Create;

  FOwnedItems := cGenOwnedItems.Create;
  FOwnedGroups := cGenOwnedGroups.Create;

  FRoot := cRoot.Create;
end;

destructor cGenericGroupManager.Destroy;
begin
  FreeAndNil(FOwnedItems);
  FreeAndNil(FOwnedGroups);
  FreeAndNil(FRoot);
  inherited Destroy;
end;

{ cGenericGroup }
constructor cGenericGroup.Create;
begin
  inherited Create;
  FGroups := cGroupMap.Create;
  FItems := cItemMap.Create;
end;

destructor cGenericGroup.Destroy;
begin
  FreeAndNil(FGroups);
  FreeAndNil(FItems);
  inherited Destroy;
end;

end.

