unit uGenComponentLists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lazutf8, fgl,
  uCHXRscStr;

type

  { cComponentOwnedList }

  generic cComponentOwnedList<TComponentChild> = class (specialize TFPGObjectList<TComponentChild>)
  public
    procedure LoadFromFile(aFile: string; Merge: Boolean = False);
    procedure LoadFromStream(aStream: TStream; Merge: Boolean = False);

    procedure SaveToFile(aFile: string);
    procedure SaveToStream(aStream: TStream);
  end;

implementation

{ cComponentOwnedList }

procedure cComponentOwnedList.LoadFromFile(aFile: string; Merge: Boolean);
var
  aStream: TFileStream;
begin
  // Nothing to load.
  if not FileExistsUTF8(aFile) then
    Exit;

  aStream := TFileStream.Create(UTF8ToSys(aFile), fmOpenRead, fmShareCompat);
  try
    LoadFromStream(aStream, Merge);
  finally
    FreeAndNil(aStream);
  end;
end;

procedure cComponentOwnedList.LoadFromStream(aStream: TStream; Merge: Boolean);
var
  NComponents: LongInt;
  i: LongInt;
  aComponentChild: TComponentChild;
begin
  if not Assigned(aStream) then
    raise EInOutError.CreateFmt(rsCUExcNilParameter,
      [self.ClassName + '.LoadFromStream', 'aStream', 'TStream']);

  // Empty or end of stream, nothing to load
  if aStream.Position >= (aStream.Size - 4) then Exit;

  // Clearing if not merge
  if not Merge then Self.Clear;

  NComponents := aStream.ReadDWord;

  for i := 1 to NComponents do
  begin
    aComponentChild := TComponentChild.Create(nil);
    try
      aComponentChild := TComponentChild(aStream.ReadComponent(aComponentChild));
      self.Add(aComponentChild);
    except
      FreeAndNil(aComponentChild);
    end;
  end;
end;

procedure cComponentOwnedList.SaveToFile(aFile: string);
var
  aStream: TFileStream;
begin
  if aFile = '' then Exit;

  // Only this list is expected to be saved on the file this way.
  aStream := TFileStream.Create(UTF8ToSys(aFile), fmCreate);
  try
    SaveToStream(aStream);
  finally
    FreeAndNil(aStream);
  end;
end;

procedure cComponentOwnedList.SaveToStream(aStream: TStream);
var
  i: LongInt;
begin
  if not Assigned(aStream) then
    raise EInOutError.CreateFmt(rsCUExcNilParameter,
      [self.ClassName + '.SaveToStream', 'aStream', 'TStream']);

  // Number of owned objects
  aStream.WriteDWord(self.Count);

  for i := 0 to (self.Count - 1) do
  begin
    aStream.WriteComponent(self.Items[i]);
  end;
end;

end.

