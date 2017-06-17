unit uCHXFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crc, sha1, FileUtil, LazFileUtils, LazUTF8,
  uCHXStrUtils, u7zWrapper;

type
  TItFolderObj = function(aFolder: string;
    FileInfo: TSearchRec): boolean of object;
  TItFolderFun = function(aFolder: string; FileInfo: TSearchRec): boolean;

{ RemoveDir does the job...
procedure RemoveEmptyFolders(aPath: string);
}

procedure Search7ZFilesByExt(AOutFolderList, AOutFileList: TStrings;
  aBaseFolder: string; aExtList: TStrings; Recursive: boolean = True);
{< Searches all files with selected extensions, searching in compressed archives too.

     @param(AOutFolderList StringList with the folder or compressed archive
       were files in AOutFileList are found. If nil it will be created,
       so must be freed elsewhere. Note: archive will have trailing path
       delimiter.)
     @param(AOutFileList Files found (if they are in a compressed archive,
       they have the internal folder structure). If nil it will be created,
       so must be freed elsewhere.)
     @param(aBaseFolder Folder were search.)
     @param(aExtList Extensions StringList, one extension by line.)
     @param(Recursive Search in (actual) subfolders too? If compressed archives
       have internal folder structure file are found any way)
}

// Some hashing
function CRC32FileInt(const aFileName: string): cardinal;
{< Calculates CRC32 checksum of a file.
}
function CRC32FileStr(const aFileName: string): string;
{< Calculates CRC32 checksum of a file and return as string.
}
function SHA1FileStr(const aFileName: string): string;
{< Calculates SHA1 checksum of a file and return as string.
}

function IterateFolderObj(Folder: string; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
function IterateFolderFun(Folder: string; aFunction: TItFolderFun;
  Recursive: boolean = True): boolean;
{< Recorre el directorio especificado y ejecuta aFuncion(TSearchRec) con cada uno
  de los archivos encontrados

Parámetros:
  - Directorio: string -> Directorio a recorrer
  - Funcion: function(Directorio: string; Info: TSearchRec): Boolean -> Metodo
    de un objeto al que se le pasa el TSearchRec y el directorio del archivo
    actual como parámetro para operar con él. Si devuelve True continua con el
    siguiente fichero.
  - Recursivo: Boolean -> Indica si también se deben recorrer los subdirectorios
  - Result: Boolean -> Indica si se ha continuar la operación

Notas:
  - Si Funcion devuelve True continúa recorriendo el directorio, si devuelve
    False no pasa al siguiente archivo.
}

function FilesInFolder(Folder: string): integer;
//< TODO 2: Is there a better way?

implementation

function CRC32FileInt(const aFileName: string): cardinal;
var
  aFile: TFileStream;
  BufferCRC: array[0..32767] of char;
  BufferSize: cardinal;
begin
  BufferCRC[0] := #32; // Fix inicialization warning
  BufferSize := SizeOf(BufferCRC);
  Result := crc32(0, nil, 0);

  if not FileExistsUTF8(aFileName) then
    Exit;

  aFile := TFileStream.Create(UTF8ToSys(aFileName), fmOpenRead);
  try
    aFile.Position := 0;

    while (aFile.Position < aFile.Size) do
    begin
      if (aFile.Size - aFile.Position) < BufferSize then
        BufferSize := aFile.Size - aFile.Position;
      aFile.ReadBuffer(BufferCRC, BufferSize);
      Result := crc32(Result, @BufferCRC, BufferSize);
    end;
  finally
    FreeAndNil(aFile);
  end;
end;

function CRC32FileStr(const aFileName: string): string;
begin
  Result := '';
  if FileExistsUTF8(aFileName) then
    Result := IntToHex(CRC32FileInt(aFileName), 8);
end;

function SHA1FileStr(const aFileName: string): string;
begin
  Result := '';
  if FileExistsUTF8(aFileName) then
    Result := SHA1Print(SHA1File(aFileName, 32768));
end;

procedure Search7ZFilesByExt(AOutFolderList, AOutFileList: TStrings;
  aBaseFolder: string; aExtList: TStrings; Recursive: boolean);
var
  FileMask: string;
  Archives, Compressed: TStringList;
  i, j: integer;
begin
  if not assigned(AOutFolderList) then
    AOutFolderList := TStringList.Create;
  if not assigned(AOutFileList) then
    AOutFileList := TStringList.Create;

  AOutFolderList.BeginUpdate;
  AOutFileList.BeginUpdate;

  FileMask := FileMaskFromStringList(aExtList);

  // 1.- Straight search
  FindAllFiles(AOutFileList, aBaseFolder, FileMask, Recursive);

  // 1.1.- Splitting Folders and Filenames
  i := 0;
  while i < AOutFileList.Count do
  begin
    AOutFolderList.Add(ExtractFilePath(AOutFileList[i]));
    AOutFileList[i] := ExtractFileName(AOutFileList[i]);
    Inc(i);
  end;

  // 2.- Search compressed archives
  Archives := TStringList.Create;
  Archives.BeginUpdate;
  Compressed := TStringList.Create;
  Compressed.BeginUpdate;
  try
    // Creating FileMask for compressed archives
    Compressed.CommaText := w7zGetFileExts;
    FileMask := FileMaskFromStringList(Compressed);
    Compressed.Clear;

    FindAllFiles(Archives, aBaseFolder, FileMask, Recursive);

    // 2.1.- For every archive search files with this extension
    i := 0;
    while i < Archives.Count do
    begin
      Compressed.Clear;
      w7zListFiles(Archives[i], Compressed, True, True, '');
      j := 0;
      while j < Compressed.Count do
      begin
        if SupportedExt(Compressed[j], aExtList) then
        begin
          AOutFolderList.Add(SetAsFolder(Archives[i]));
          AOutFileList.Add(Compressed[j]);
        end;
        Inc(j);
      end;
      Inc(i);
    end;

  finally
    AOutFolderList.EndUpdate;
    AOutFileList.EndUpdate;
    FreeAndNil(Archives);
    FreeAndNil(Compressed);
  end;
end;

function IterateFolderObj(Folder: string; aFunction: TItFolderObj;
  Recursive: boolean): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  Folder := SetAsFolder(Folder);
  if (Folder = '') or (not DirectoryExistsUTF8(Folder)) then
    Exit;

  if FindFirstUTF8(Folder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(Folder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(Folder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderObj(Folder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function IterateFolderFun(Folder: string; aFunction: TItFolderFun;
  Recursive: boolean): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  Folder := SetAsFolder(Folder);


  // '' ? if we want run in current directory?
  if (Folder = '') or (not DirectoryExistsUTF8(Folder)) then
    Exit;

  if FindFirstUTF8(Folder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(Folder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(Folder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderFun(Folder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function FilesInFolder(Folder: string): integer;
var
  Info: TSearchRec;
begin
  // Podría usar IterateFolderObj pero no es plan de complicar la cosa
  Result := 0;
  Folder := SetAsFolder(Folder);

  if FindFirstUTF8(Folder + '*', 0, Info) = 0 then
    try
      repeat
        Inc(Result);
      until (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
end;

end.
