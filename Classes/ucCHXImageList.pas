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
    function AddImageFile(aKey, aFile: String): Integer;
  end;
implementation

{ cCHXImageMap }

function cCHXImageMap.AddImageFile(aKey, aFile: String): Integer;
begin

end;

{ cCHXImageList }

function cCHXImageList.AddImageFile(aFile: String): Integer;
var
  Img: TPicture;
begin
  Result := -1;
  if not FileExistsUTF8(aFile) then Exit;
  Img := TPicture.Create;
  try
    Img.LoadFromFile(aFile);
  except
    // WOOPS, it can't be loaded.
    FreeAndNil(Img);
    AddEmptyImage;
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

