{ cImageList unit. }
unit ucCHXImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, LazFileUtils, fgl;

type

  { cCHXImageList }
  cCHXFPGImageList = specialize TFPGObjectList<TPicture>;

  cCHXImageList = class(cCHXFPGImageList)
  private
    FFileList: TStringList;
  protected
    property FileList: TStringList read FFileList;

  public
    function AddImageFile(aFile: string): integer;

    constructor Create(aFreeObjects: boolean = True);
    destructor Destroy; override;
  end;

  { cCHXImageMap }
  cCHXFPGImageMap = specialize TFPGMapObject<string, TPicture>;

  cCHXImageMap = class(cCHXFPGImageMap)
  public
    function AddImageFile(aKey, aFile: string): boolean;

    constructor Create(AFreeObjects: boolean);
    constructor Create;
  end;

implementation

{ cCHXImageMap }

function cCHXImageMap.AddImageFile(aKey, aFile: string): boolean;
var
  Img: TPicture;
begin
  Result := True;
  Img := TPicture.Create;
  try
     if FileExistsUTF8(aFile) then
       Img.LoadFromFile(aFile);
  except
    // WOOPS, it can't be loaded.
    FreeAndNil(Img);
    Result := False; // Not added.
  end;
  Self.AddOrSetData(aKey, Img);
end;

constructor cCHXImageMap.Create(AFreeObjects: boolean);
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

function cCHXImageList.AddImageFile(aFile: string): integer;
var
  Img: TPicture;
begin
  Result := FileList.IndexOf(aFile);

  if Result = -1 then
  begin
    Img := TPicture.Create;
    try
      if FileExistsUTF8(aFile) then
        Img.LoadFromFile(aFile);
    except
      // Add it empty
      ;
    end;
    FileList.Add(aFile);
    Result := Self.Add(Img);
  end;
end;

constructor cCHXImageList.Create(aFreeObjects: boolean);
begin
  inherited Create(aFreeObjects);

  FFileList := TStringList.Create;
  FileList.CaseSensitive := False;
  FileList.Sorted := False;
  FileList.Duplicates := dupError;
end;

destructor cCHXImageList.Destroy;
begin
  FreeAndNil(FFileList);
  inherited Destroy;
end;

end.
