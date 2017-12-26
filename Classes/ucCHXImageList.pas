{ Copyright (C) 2006-2017 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
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

    procedure Clear;

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
  AddOrSetData(aKey, Img);
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
    finally
      FileList.Add(aFile);
      Result := Add(Img);
    end;
  end;
end;

procedure cCHXImageList.Clear;
begin
  inherited Clear;
  FileList.Clear;
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
