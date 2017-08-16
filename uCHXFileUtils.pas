unit uCHXFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crc, sha1, FileUtil, LazFileUtils, LazUTF8,
  uCHXStrUtils, u7zWrapper;

const
  kCHXSHA1Empty: TSHA1Digest =
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);


type
  TItFolderObj = function(aFolder: string;
    FileInfo: TSearchRec): boolean of object;
  TItFolderFun = function(aFolder: string; FileInfo: TSearchRec): boolean;


// Searching...
function SearchFirstFileInFolderByExtCT(aFolder: string;
  Extensions: string): string;
function SearchFirstFileInFolderByExtSL(aFolder: string;
  Extensions: TStrings): string;
{< Searches first file found with a matched extension from a list in a folder.
}
procedure Search7ZFilesByExt(AOutFolderList, AOutFileList: TStrings;
  aBaseFolder: string; aExtList: TStrings; Recursive: boolean);
{< Searches all files with selected extensions, searching in compressed archives too.

     @param(AOutFolderList StringList with the folder or compressed archive
       were files in AOutFileList are found. If nil it will be created,
       so must be freed elsewhere. Note: Compressed archives will have trailing
       path delimiter.)
     @param(AOutFileList Files found (if they are in a compressed archive,
       they have the internal folder structure). If nil it will be created,
       so must be freed elsewhere.)
     @param(aBaseFolder Folder where search.)
     @param(aExtList Extensions StringList, one extension by line.)
     @param(Recursive Search in (actual) subfolders too? If compressed archives
       have internal folder structure file are found any way)
}

// Some hashing
// ------------
function CRC32FileInt(const aFileName: string): cardinal;
{< Calculates CRC32 checksum of a file.
}
function CRC32FileStr(const aFileName: string): string;
{< Calculates CRC32 checksum of a file and return as string.
}
function SHA1FileStr(const aFileName: string): string;
{< Calculates SHA1 checksum of a file and return as string.
}
function StringToSHA1Digest(aSHA1String: string):TSHA1Digest;


// Iterating Folders
// -----------------
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
  BufferCRC[0] := #0; // Fix inicialization warning
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

function StringToSHA1Digest(aSHA1String: string): TSHA1Digest;
var
  i: integer;
begin
  Result := kCHXSHA1Empty;

  if Length(aSHA1String) <> 40 then Exit;

  i := 0;
  while i <= 19 do
  begin
    Result[i] := (StrToInt('$' + aSHA1String[(i shl 1) + 1]) shl 4) or StrToInt('$' + aSHA1String[(i shl 1) + 2]);
    inc(i);
  end;
end;

function SearchFirstFileInFolderByExtCT(aFolder: string;
  Extensions: string): string;
var
  aTempSL: TStringList;
begin
  aTempSL := TStringList.Create;
  try
    aTempSL.CommaText := Extensions;
    Result := SearchFirstFileInFolderByExtSL(aFolder, aTempSL);
  finally
    aTempSL.Free;
  end;
end;

function SearchFirstFileInFolderByExtSL(aFolder: string;
  Extensions: TStrings): string;
var
  Info: TSearchRec;
begin
  Result := '';
  aFolder := SetAsFolder(aFolder);

  // We want to exit at first match found, so we use old method.

  if (aFolder = '') or (not DirectoryExistsUTF8(aFolder)) then
    Exit;

  if FindFirstUTF8(aFolder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        if SupportedExtSL(Info.Name, Extensions) then
          Result := aFolder + Info.Name;
      until (Result <> '') or (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;

  if Result <> '' then
    Exit;

  if FindFirstUTF8(aFolder + AllFilesMask, faDirectory, Info) = 0 then
    try
      repeat
        if (Info.Name <> '.') and (Info.Name <> '') and
          (Info.Name <> '..') and
          ((Info.Attr and faDirectory) <> 0) then
          Result := SearchFirstFileInFolderByExtSL(aFolder +
            Info.Name, Extensions);
      until (Result <> '') or (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
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
    AOutFolderList.Add(SetAsFolder(ExtractFilePath(AOutFileList[i])));
    AOutFileList[i] := SetAsFile(ExtractFileName(AOutFileList[i]));
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
        if SupportedExtSL(Compressed[j], aExtList) then
        begin
          AOutFolderList.Add(SetAsFolder(Archives[i]));
          AOutFileList.Add(SetAsFile(Compressed[j]));
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
