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
       so must be freed elsewhere.)
     @param(AOutFileList Files found (if they are in a compressed archive,
       they have the internal folder structure). If nil it will be created,
       so must be freed elsewhere.)
     @param(aBaseFolder Folder were search.)
     @param(aExtList Extensions StringList, one extension by line.)
     @param(Recursive Search in (actual) subfolders too? If compressed archives
       have internal folder structure file are found any way)
}

procedure SearchMediaFiles(FileList: TStrings; aFolder: string;
  aFileName: string; Extensions: TStrings);

function SearchFirstMediaFile(aFolder: string; aFileName: string;
  Extensions: TStrings): string;
{< Same as SearchMediaFiles but returns only first matched file. }

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
  Compressed := TStringList.Create;
  try
    // Creating FileMask for compressed archives
    Compressed.CommaText := w7zFileExts;
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
          AOutFolderList.Add(Archives[i]);
          AOutFileList.Add(Compressed[j]);
        end;
        Inc(j);
      end;
      Inc(i);
    end;
  finally
    FreeAndNil(Archives);
    FreeAndNil(Compressed);
  end;
end;

procedure SearchMediaFiles(FileList: TStrings; aFolder: string;
  aFileName: string; Extensions: TStrings);

  procedure SearchFileByExt(aFileList: TStrings; aBaseFileName: string;
    aExtList: TStrings);
  var
    i, j: integer;
  begin
    i := 0;
    j := aExtList.Count;
    while i < j do
    begin
      if FileExistsUTF8(aBaseFileName + ExtensionSeparator +
        aExtList[i]) then
        aFileList.Add(aBaseFileName + ExtensionSeparator + aExtList[i]);
      Inc(i);
    end;
  end;

  procedure AddFilesFromFolder(FileList: TStrings; aFolder: string;
    Extensions: TStrings);
  var
    Info: TSearchRec;
  begin
    aFolder := SetAsFolder(aFolder);
    if (aFolder = '') or (not DirectoryExistsUTF8(aFolder)) then
      Exit;

    if FindFirstUTF8(aFolder + AllFilesMask, faAnyFile, Info) = 0 then
      try
        repeat
          if Extensions.IndexOf(UTF8LowerCase(UTF8Copy(
            ExtractFileExt(Info.Name), 2, MaxInt))) <> -1 then
            FileList.Add(aFolder + Info.Name);
        until (FindNextUTF8(Info) <> 0);
      finally
        FindCloseUTF8(Info);
      end;


    if FindFirstUTF8(aFolder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            AddFilesFromFolder(FileList, aFolder + Info.Name, Extensions);
        until (FindNextUTF8(Info) <> 0);
      finally
        FindCloseUTF8(Info);
      end;
  end;

var
  TempTypeSubFolder: string;
  CompressedArchives: TStringList;
  i, j: integer;

  Info: TSearchRec;
begin
  { TODO : Check this }
  {
  if FileList = nil then
    FileList := TStringList.Create
  else
    FileList.Clear;

  aFolder := SetAsFolder(aFolder);
  aFileName := RemoveFromBrackets(aFileName);
  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  SearchFileByExt(FileList, aFolder + aFileName, Extensions);

  // 2. Search in folder
  // Folder/aFileName/*.mext
  AddFilesFromFolder(FileList, aFolder + SetAsFolder(aFileName), Extensions);

  // 3.a Search in cache folder
  // TempFolder/Type/aFileName/*.mext
  TempTypeSubFolder := TempFolder +
    SetAsFolder(ExtractFileName(ExcludeTrailingPathDelimiter(aFolder))) +
    SetAsFolder(aFileName);

  if DirectoryExistsUTF8(TempTypeSubFolder) then
    AddFilesFromFolder(FileList, TempTypeSubFolder, Extensions)
  else
  begin
    // 3.b Search in compressed archive
    // Folder/aFileName.zip/*.mext (extract to TempFolder/Type/aFileName/*.mext)

    CompressedArchives := TStringList.Create;
    try
      SearchFileByExt(CompressedArchives, aFolder + aFileName, CompressedExt);

      i := 0;
      j := CompressedArchives.Count;
      while i < j do
      begin
        w7zExtractFile(CompressedArchives[i], AllFilesMask, TempTypeSubFolder,
          False, '');
        Inc(i);
      end;

      AddFilesFromFolder(FileList, TempTypeSubFolder, Extensions);
    finally
      FreeAndNil(CompressedArchives);
    end;
  end;

  if FileList.Count > 0 then
    Exit;

  // 4. If none found, search ONLY ONE from every compressed archive.
  // Folder/*.zip/aFileName.mext
  if FindFirstUTF8(aFolder + AllFilesMask, 0, Info) = 0 then
    try
      repeat
        // Ough, we really need a easy way to check extensions
        if CompressedExt.IndexOf(UTF8LowerCase(UTF8Copy(
          ExtractFileExt(Info.Name), 2, MaxInt))) <> -1 then
        begin
          // AllFilesMask... Maybe is a good idea...
          w7zExtractFile(aFolder + Info.Name, aFileName + '.*',
            TempTypeSubFolder, False, '');
          AddFilesFromFolder(FileList, TempTypeSubFolder, Extensions);
        end;
      until (FileList.Count > 0) or (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
  }
end;

function SearchFirstMediaFile(aFolder: string; aFileName: string;
  Extensions: TStrings): string;

  function SearchFileByExt(aBaseFileName: string; aExtList: TStrings): string;
  var
    i: integer;
  begin
    Result := '';
    i := 0;
    while (i < aExtList.Count) and (Result = '') do
    begin
      if FileExistsUTF8(aBaseFileName + ExtensionSeparator + aExtList[i]) then
        Result := aBaseFileName + ExtensionSeparator + aExtList[i];
      Inc(i);
    end;
  end;

begin
  Result := '';

  aFolder := SetAsFolder(aFolder);
  aFileName := RemoveFromBrackets(aFileName);
  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  Result := SearchFileByExt(aFolder + aFileName, Extensions);

  // 2. Search in folder
  // Folder/aFileName/*.mext

  // 3.a Search in cache folder
  // TempFolder/Type/aFileName/*.mext

  // 3.b Search in compressed archive
  // Folder/aFileName.zip/*.mext (extract to TempFolder/Type/aFileName/*.mext)

  // 4. If none found, search ONLY ONE from every compressed archive.
  // Folder/*.zip/aFileName.mext

end;

function IterateFolderObj(Folder: string; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
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
