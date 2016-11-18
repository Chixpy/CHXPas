{ cImageList unit. }
unit ucCHXImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, LazFileUtils, fgl;

type

  { cCHXImageList }
  cCHXImageList = class (specialize TFPGObjectList<TPicture>)
  public
    function AddImageFile(aFile: String): Integer;
    function AddEmptyImage: Integer;
  end;

  { cCHXImageMap }

  cCHXImageMap= class (specialize TFPGMapObject<string, TPicture>)
   public
    function AddImageFile(aKey, aFile: String): Boolean;

    constructor Create(AFreeObjects: Boolean);
    constructor Create;
  end;
implementation

{ cCHXImageMap }

function cCHXImageMap.AddImageFile(aKey, aFile: String): Boolean;
var
  Img: TPicture;
begin
  Result := True;
    Img := TPicture.Create;
  try
    Img.LoadFromFile(aFile);
  except
    // WOOPS, it can't be loaded.
    FreeAndNil(Img);
    Result := False; // Not added.
  end;
  Self.AddOrSetData(aKey, Img);
end;

constructor cCHXImageMap.Create(AFreeObjects: Boolean);
begin
  inherited Create(AFreeObjects);

  // FIX: Sometimes Key is not found !!!???
  Sorted := True;
end;

constructor cCHXImageMap.Create;
begin
  Create(True);
end;

{ cCHXImageList }

function cCHXImageList.AddImageFile(aFile: String): Integer;
var
  Img: TPicture;
begin
  Result := -1;
  Img := TPicture.Create;
  try
    Img.LoadFromFile(aFile);
  except
    // WOOPS, it can't be loaded.
    FreeAndNil(Img);
    Result := AddEmptyImage;
    Exit;
  end;
  Result := Self.Add(Img);
end;

function cCHXImageList.AddEmptyImage: Integer;
var
  aImage: TPicture;
begin
  aImage := TPicture.Create;
  Result := Self.Add(aImage);
end;

end.

