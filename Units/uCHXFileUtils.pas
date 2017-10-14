unit uCHXFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crc, sha1, FileUtil, LazFileUtils, LazUTF8,
  uCHXStrUtils;

const
  kCHXSHA1Empty: TSHA1Digest =
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);


type
  TItFolderObj = function(aFolder: string; FileInfo: TSearchRec): boolean of object;
  TItFolderFun = function(aFolder: string; FileInfo: TSearchRec): boolean;


// Searching...
function SearchFirstFileInFolderByExtCT(aFolder: string;
  Extensions: string): string;
function SearchFirstFileInFolderByExtSL(aFolder: string;
  Extensions: TStrings): string;
{< Searches first file found with a matched extension from a list in a folder.
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
function IterateFolderObj(aFolder: string; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
function IterateFolderFun(aFolder: string; aFunction: TItFolderFun;
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

function FilesInFolder(aFolder, aFileMAsk: string): integer;
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
begin
  if Length(aSHA1String) < 20 then
    Result := kCHXSHA1Empty
  else
    HexToBin(PChar(aSHA1String), @Result, 20);
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

function IterateFolderObj(aFolder: string; aFunction: TItFolderObj;
  Recursive: boolean): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  aFolder := SetAsFolder(aFolder);
  if (aFolder = '') or (not DirectoryExistsUTF8(aFolder)) then
    Exit;

  if FindFirstUTF8(aFolder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(aFolder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(aFolder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderObj(aFolder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function IterateFolderFun(aFolder: string; aFunction: TItFolderFun;
  Recursive: boolean): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  aFolder := SetAsFolder(aFolder);


  // '' ? if we want run in current directory?
  if (aFolder = '') or (not DirectoryExistsUTF8(aFolder)) then
    Exit;

  if FindFirstUTF8(aFolder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(aFolder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(aFolder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderFun(aFolder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function FilesInFolder(aFolder, aFileMask: string): integer;
var
  Info: TSearchRec;
begin
  // Podría usar IterateFolderObj pero no es plan de complicar la cosa
  Result := 0;
  aFolder := SetAsFolder(aFolder);
  if aFileMask = '' then
    aFileMask := AllFilesMask;

  if FindFirstUTF8(aFolder + aFileMask, 0, Info) = 0 then
    try
      repeat
        Inc(Result);
      until (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
end;

end.
