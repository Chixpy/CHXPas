unit uCHX7zWrapper;

{< 7z.exe and 7zG.exe Wrapper.

  Copyright (C) 2011-2017 Chixpy
}

{ Simple 7z.exe AND 7zG.exe wrapper until something better is found.

  Last version tested 16.02, although may be works with any newer.

  On Win10, older versions of 7zip don't work.

  @definitionList(
    @itemLabel(NOTE:)
    @item(7z.exe, 7zG.exe and 7z.dll have their own licenses.)
  )
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, sha1,
  IniFiles,
  // CHX units
  uCHXStrUtils, uCHXFileUtils, uCHXExecute;

resourcestring
  rsw7zErrorLine = '%0:s - %1:s: %2:s';
  {< Translatable string, line in file error: '%0:s - %1:s: %2:s' }
  rsw7zFileNotFound = '"%0:s" file not found.';
  //< Translatable string: '"%0:s" file not found'
  rsw7zExeError = '7z.exe/7zG.exe error %1:d exit code.' +
    LineEnding + 'File: "%0:s"';
  //< Translatable string: '7z.exe/7zG.exe error %1:d exit code.'
  rsw7zExeWarning = '7z.exe/7zG.exe warning %1:d exit code.' +
    LineEnding + 'File: "%0:s"';
  //< Translatable string: '7z.exe/7zG.exe warning %1:d exit code.'
  rsw7zNoCRC32 = 'Error extracting CRC32: "%0:s"';
  //< Translatable string: 'Error extracting CRC32: %0:s'

const
  // Experimental Global Cache
  kw7zGCIniPrefix = 'w7z.';
  kw7zGCIniFiles = kw7zGCIniPrefix + 'Files';
  kw7zGCIniSizes = kw7zGCIniPrefix + 'Sizes';
  kw7zGCIniPSizes = kw7zGCIniPrefix + 'PSizes';
  kw7zGCIniDates = kw7zGCIniPrefix + 'Dates';
  kw7zGCIniCRCs = kw7zGCIniPrefix + 'CRCs';
  kw7zGCIniSHA1s = kw7zGCIniPrefix + 'SHA1s';

  kw7zCacheFileExt = '.txt';
  kw7zFileExts = '001,7z,arj,bpl,bz2,bzip2,cab,cba,cb7,cbr,cbz,chi,chm,chq,chw,'
    + 'cpio,cramfs,deb,dll,dmg,doc,exe,fat,flv,gz,gzip,hfs,hxi,hxq,hxr,hxs,' +
    'hxw,img,iso,jar,lha,lit,lzh,lzma,lzma86,mbr,msi,msp,nsis,ntfs,ppt,' +
    'r00,rar,rpm,scap,squashfs,swf,swm,sys,tar,taz,tbz,tbz2,tgz,tpz,txz,' +
    'vhd,wim,xar,xls,xpi,xz,z,zip';

  {< Initial suported file extensions, use w7zGetFileExts to get current
     ones. A program can drop or add supported extensions. After all, it's
     only a reference list. }

function w7zGetErrorList : TStringList;
function w7zGetLastError : string;
procedure w7zSetErrorListFile(aFile : string = '');

function w7zGetFileExts : string;
procedure w7zSetFileExts(aExtList : string = '');
{< Get/Set default file extensions. Empty restore default ones.

  String with suported file extensions by 7z.

  Format: 'ext,ext,ext' for easy creating a TStringList. At least until
    we found a better way for searching files with different extension.

  Warning: It's not used for test if the files passed as params are
    compressed files. It's only a reference list.
}

function w7zPathsOK : Boolean;

function w7zGetPathTo7zexe : string;
procedure w7zSetPathTo7zexe(aPath : string);
{< Path to 7z.exe executable.

  It can be useful for hidding the process, but it's
    needed for listing archives anyways.
}

function w7zGetPathTo7zGexe : string;
procedure w7zSetPathTo7zGexe(aPath : string);
{< Path to 7zG.exe executable.

  Absolute path
}
function w7zGetCacheDir : string;
procedure w7zSetCacheDir(aPath : string);
{< Directory were lists of files from compressed archives are stored.

  Defaults to '%USERTEMPDIR%/w7zCache', and the directory is deleted at
    program exit.
}
function w7zGetGlobalCache : string;
procedure w7zSetGlobalCache(aPath : string);
{< ¡EXPERIMENTAL!

  Similar to w7zCacheDir, but different folder structure:
    'w7zGlobalCache/SHA[1]/SHA[1-3].ini'

  NOT DELETED AT EXIT. FOLDER MUST EXISTS.
}

function w7zFileExists(a7zArchive : string; aInnerFile : string;
  const Password : string) : Integer;
{< Search if a file exists.}

procedure w7zListFiles(a7zArchive : string; PackedFiles : TStrings;
  const OnlyPaths : Boolean; const Password : string);
{< List files and properties in a 7z (or other format) archive.

  Executes "7z.exe l -slt aFilename" but don't use wildcards.

  7zG.exe can't list files so always 7z.exe is used, and console is hidden.

  @param(aFilename Name of the 7z archive.)
  @param(PackedFiles StringList where the files will be added. If the
    StringList = nil will be created @(and you must free it, of course@),
    otherwise it will be cleared. If OnlyPaths = @false, every string have the
    following format for easy TStringList.CommaText reading:
    "Dir/Filename","size","packed size","Date modified","CRC")
  @param(OnlyPaths List only file names and no properties)
  @param(UseCache Use cached file list?)
  @param(Password Is there archives that need a password to list files?
    Just in case.)
  @return(Exit code)
}

procedure w7zFilesByExt(AOutFolderList, AOutFileList : TStrings;
  aBaseFolder : string; aExtList : TStrings; Recursive : Boolean);
{< Searches all files with selected extensions, searching in compressed
   archives too.

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

function w7zExtractFile(a7zArchive : string; const aFileMask : string;
  aFolder : string; const ShowProgress : Boolean;
  const Password : string) : Integer;
{< Extract de file (or files) from 7z archive.

  @param(aFilename Name of the 7z archive.)
  @param(aFolder Folder where the file(s) will be extracted.)
  @param(aFileMask Mask or file to extract. Remember, for 7z.exe '*' means all
    files, not '*.*' wich means all files with extension.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(Password Password for 7z archive.)
  @return(Exit code)
}

function w7zCompressFile(a7zArchive : string; aFileList : TStrings;
  const ShowProgress : Boolean; const CompType : string = '') : Integer;
{< Compress files in a 7z (or other type) archive.

  @param(a7zArchive Name of the 7z/zip archive.)
  @param(aFileList List of files to add to the archive.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(CompType Type of the archive.)
  @return(Exit code)
}

function w7zCompressFolder(a7zArchive, aFolder : string;
  IncludeRoot : Boolean; const ShowProgress : Boolean;
  const CompType : string = '') : Integer;
{< Compress files in a 7z (or other type) archive.

  @param(a7zArchive Name of the 7z/zip archive.)
  @param(aFolder Folder to compress.)
  @param(IncludeRoot Include root folder inside archive.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(CompType Type of the archive.)
  @return(Exit code)
}

function w7zCRC32InnerFile(a7zArchive : string; const aInnerFile : string;
  const Password : string) : Cardinal;
function w7zCRC32InnerFileStr(a7zArchive : string; aInnerFile : string;
  const Password : string) : string;
function w7zSHA32InnerFile(a7zArchive : string; const aInnerFile : string;
  const Password : string) : TSHA1Digest;
function w7zSHA32InnerFileStr(a7zArchive : string; aInnerFile : string;
  const Password : string) : string;

implementation

var
  w7zPathTo7zexeOK : Boolean;
  w7zPathTo7zGexeOK : Boolean;

  w7zFileExts : string;
  {< String with suported file extensions by 7z.

     Format: 'ext,ext,ext' for easy creating a TStringList. At least until
       we found a better way for searching files with different extension.

     Warning: It's not used for test if the files passed as params are
       compressed files. It's only a reference list.
  }
  w7zPathTo7zexe : string;
  {< Path to 7z.exe executable.

    It can be useful for hidding the process, but it's
      needed for listing archives anyways.
  }
  w7zPathTo7zGexe : string;
  {< Path to 7zG.exe executable.
  }

  w7zCacheDir : string;
  {< Directory were lists of files from compressed archives are stored.

    Defaults to '%USERTEMPDIR%/w7zCache', and the directory is deleted at
      program exit.
  }

  w7zGlobalCache : string;

  {< ¡EXPERIMENTAL!

    Similar to w7zCacheDir, but different folder structure:
      'w7zGlobalCache/SHA[1-2]/SHA[3-4].ini'

    NOT DELETED AT EXIT. FOLDER MUST EXISTS.
  }
  w7zErrorListFile : string;
  w7zErrorList : TStringList;

  {< Keeps a list of errors produced.
  }
procedure w7zErrorAdd(const aFunction, aError : string);
begin
  w7zErrorList.Add(Format(rsw7zErrorLine, [DateTimeToStr(Now),
    aFunction, aError]));
end;

procedure w7zErrorAddStdErr(aStdErrSL : TStrings);
begin
  w7zErrorList.AddStrings(aStdErrSL);
end;

procedure w7zErrorOK;
begin
  if (w7zErrorList.Count > 0) and
    (w7zErrorList[w7zErrorList.Count - 1] <> '') then
    w7zErrorList.Add('');
end;

procedure SaveGlobalCache(FileSHA1 : string; PackedFiles : TStrings);
var
  aFile : string;
  sl, slPath, slSizes, slPSizes, slDates, slCRC, slSHA1 : TStringList;
  aIni : TMemIniFile;
  i : Integer;
begin
  if (w7zGetGlobalCache = '') or
    (not DirectoryExistsUTF8(w7zGetGlobalCache)) then
    Exit; // No global cache

  aFile := SetAsFolder(w7zGetGlobalCache + Copy(FileSHA1, 1, 1)) +
    Copy(FileSHA1, 1, 3) + '.ini';

  aIni := TMemIniFile.Create(UTF8ToSys(aFile));
  sl := TStringList.Create;
  slPath := TStringList.Create;
  slSizes := TStringList.Create;
  slPSizes := TStringList.Create;
  slDates := TStringList.Create;
  slCRC := TStringList.Create;
  slSHA1 := TStringList.Create;
  sl.BeginUpdate;
  slPath.BeginUpdate;
  slSizes.BeginUpdate;
  slPSizes.BeginUpdate;
  slDates.BeginUpdate;
  slCRC.BeginUpdate;
  slSHA1.BeginUpdate;
  try
    i := 0;
    while i < PackedFiles.Count do
    begin
      sl.CommaText := PackedFiles[i];
      while sl.Count < 6 do
        sl.Add('');
      slPath.add(sl[0]);
      slSizes.add(sl[1]);
      slPSizes.add(sl[2]);
      slDates.add(sl[3]);
      slCRC.add(sl[4]);
      slSHA1.add(sl[5]);
      Inc(i);
    end;

    aIni.WriteString(FileSHA1, kw7zGCIniFiles, slPath.CommaText);
    aIni.WriteString(FileSHA1, kw7zGCIniSizes, slSizes.CommaText);
    aIni.WriteString(FileSHA1, kw7zGCIniPSizes, slPSizes.CommaText);
    aIni.WriteString(FileSHA1, kw7zGCIniDates, slDates.CommaText);
    aIni.WriteString(FileSHA1, kw7zGCIniCRCs, slCRC.CommaText);
    aIni.WriteString(FileSHA1, kw7zGCIniSHA1s, slSHA1.CommaText);

    aIni.UpdateFile;

  finally
    sl.Free;
    slPath.Free;
    slSizes.Free;
    slPSizes.Free;
    slDates.Free;
    slCRC.Free;
    slSHA1.Free;
    aIni.Free;
  end;
end;

function w7zGetErrorList : TStringList;
begin
  Result := w7zErrorList;
end;

function w7zGetLastError : string;
begin
  if w7zErrorList.Count > 0 then
    Result := w7zErrorList[w7zErrorList.Count - 1]
  else
    Result := '';
end;

procedure w7zSetErrorListFile(aFile : string);
begin
  w7zErrorListFile := aFile;
end;

function w7zPathsOK : Boolean;
begin
  Result := w7zPathTo7zexeOK and w7zPathTo7zGexeOK;
end;

function w7zGetFileExts : string;
begin
  Result := w7zFileExts;
end;

procedure w7zSetFileExts(aExtList : string);
begin
  if aExtList = '' then
    w7zFileExts := kw7zFileExts
  else
    w7zFileExts := aExtList;
end;

function w7zGetPathTo7zexe : string;
begin
  Result := w7zPathTo7zexe;
end;

procedure w7zSetPathTo7zexe(aPath : string);
begin
  w7zPathTo7zexe := CleanAndExpandFilename(SetAsFile(aPath));
  w7zPathTo7zexeOK := FileExistsUTF8(w7zPathTo7zexe);

  if not w7zPathTo7zexeOK then
    w7zErrorAdd('w7zSetPathTo7zexe', Format(rsw7zFileNotFound,
      [w7zPathTo7zexe]))
  else
    w7zErrorOK;
end;

function w7zGetPathTo7zGexe : string;
begin
  Result := w7zPathTo7zGexe;
end;

procedure w7zSetPathTo7zGexe(aPath : string);
begin
  w7zPathTo7zGexe := CleanAndExpandFilename(SetAsFile(aPath));
  w7zPathTo7zGexeOK := FileExistsUTF8(w7zPathTo7zGexe);

  if not w7zPathTo7zGexeOK then
    w7zErrorAdd('w7zSetPathTo7zGexe', Format(rsw7zFileNotFound,
      [w7zPathTo7zGexe]))
  else
    w7zErrorOK;
end;

function w7zGetCacheDir : string;
begin
  Result := w7zCacheDir;
end;

procedure w7zSetCacheDir(aPath : string);
begin
  if aPath = '' then
    aPath := SetAsFolder(GetTempDir(False)) + 'w7zCache';
  aPath := CleanAndExpandDirectory(SetAsFolder(aPath));

  // TODO: Better security check...
  if Length(w7zCacheDir) > 15 then
    DeleteDirectory(w7zCacheDir, False);
  ForceDirectoriesUTF8(aPath);
  w7zCacheDir := aPath;
end;

function w7zGetGlobalCache : string;
begin
  Result := w7zGlobalCache;
end;

procedure w7zSetGlobalCache(aPath : string);
begin
  w7zGlobalCache := SetAsFolder(aPath);
end;

function w7zFileExists(a7zArchive : string; aInnerFile : string;
  const Password : string) : Integer;
var
  aFileList : TStringList;
  i : Integer;
begin
  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);
  if not FileExistsUTF8(a7zArchive) then
  begin
    Result := -1; // a7zArchive not found;
    Exit;
  end;

  aInnerFile := SysPath(aInnerFile);

  aFileList := TStringList.Create;
  try
    aFileList.BeginUpdate;
    w7zListFiles(a7zArchive, aFileList, True, Password);
    aFileList.EndUpdate;

    i := 0;
    Result := 1;
    while (Result <> 0) and (i < aFileList.Count) do
    begin
      Result := CompareFilenames(SysPath(aFileList[i]), aInnerFile);
      Inc(i);
    end;

    if (Result <> 0) then
      Result := -2; // Inner file not found
  finally
    aFileList.Free;
  end;
end;

procedure w7zListFiles(a7zArchive : string; PackedFiles : TStrings;
  const OnlyPaths : Boolean; const Password : string);

  procedure ReturnOnlyPaths(aFileList : TStrings);
  var
    slLine : TStringList;
    i : Integer;
  begin
    // Removing additional data
    // slLine is out of the iteration to avoid creating-deleting every time.
    slLine := TStringList.Create;
    try
      i := 0;
      while i < aFileList.Count do
      begin
        slLine.CommaText := aFileList[i];
        if slLine.Count > 0 then
        begin
          aFileList[i] := slLine[0];
          Inc(i);
        end
        else
        begin
          // Uhm... this must not happen... but...
          aFileList.Delete(i);
        end;
      end;
    finally
      FreeAndNil(slLine);
    end;
  end;

  procedure LoadFromGlobalCache(FileSHA1 : string; PackedFiles : TStrings;
    OnlyPaths : Boolean);
  var
    aFile : string;
    sl, slPath, slSizes, slPSizes, slDates, slCRC, slSHA1 : TStringList;
    aIni : TMemIniFile;
    i : Integer;
  begin
    aFile := SetAsFolder(w7zGetGlobalCache + Copy(FileSHA1, 1, 1)) +
      Copy(FileSHA1, 1, 3) + '.ini';
    if FileExistsUTF8(aFile) then
    begin
      aIni := TMemIniFile.Create(aFile);
      try
        if OnlyPaths then
        begin
          PackedFiles.CommaText :=
            aIni.ReadString(FileSHA1, kw7zGCIniFiles, '');
        end
        else
        begin
          sl := TStringList.Create;
          sl.BeginUpdate;
          slPath := TStringList.Create;
          slSizes := TStringList.Create;
          slPSizes := TStringList.Create;
          slDates := TStringList.Create;
          slCRC := TStringList.Create;
          slSHA1 := TStringList.Create;
          try
            slPath.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniFiles, '');
            slSizes.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniSizes, '');
            slPSizes.CommaText :=
              aIni.ReadString(FileSHA1, kw7zGCIniPSizes, '');
            slDates.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniDates, '');
            slCRC.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniCRCs, '');
            slSHA1.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniSHA1s, '');

            while slSizes.Count < slPath.Count do
              slSizes.Add('');
            while slPSizes.Count < slPath.Count do
              slPSizes.Add('');
            while slDates.Count < slPath.Count do
              slDates.Add('');
            while slCRC.Count < slPath.Count do
              slCRC.Add('');
            while slSHA1.Count < slPath.Count do
              slSHA1.Add('');

            i := 0;
            while i < slPath.Count do
            begin
              sl.Clear;
              sl.Add(slPath[i]);
              sl.Add(slSizes[i]);
              sl.Add(slPSizes[i]);
              sl.Add(slDates[i]);
              sl.Add(slCRC[i]);
              sl.Add(slSHA1[i]);
              PackedFiles.Add(sl.CommaText);
              Inc(i);
            end;

          finally
            sl.EndUpdate;
            sl.Free;
            slPath.Free;
            slSizes.Free;
            slPSizes.Free;
            slDates.Free;
            slCRC.Free;
            slSHA1.Free;
          end;
        end;
      finally
        aIni.Free;
      end;
    end;
  end;
var
  FileSHA1 : string;
  aPos, i : Integer;
  slLine, slOutput : TStringList;
  aParam, aValue : string;
  sOutput, sStdErr : string;
  aPath, Size, PSize, aDate, aCRC, aSHA1 : string;
begin
  // Clearing PackedFiles file list
  if Assigned(PackedFiles) then
    PackedFiles.Clear
  else
    PackedFiles := TStringList.Create;

  // Checking needed file: 7z.exe
  if not w7zPathTo7zexeOK then
    Exit;

  // Sometimes are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);

  if not FileExistsUTF8(a7zArchive) then
  begin
    w7zErrorAdd('w7zListFiles', Format(rsw7zFileNotFound, [a7zArchive]));
    Exit;
  end;

  // SHA1 of the file... cache file is saved always
  FileSHA1 := SHA1Print(SHA1File(UTF8ToSys(a7zArchive)));

  // Searching for cache file
  // ------------------------
  aValue := w7zGetCacheDir + FileSHA1 + kw7zCacheFileExt;

  if FileExistsUTF8(aValue) then
  begin
    PackedFiles.LoadFromFile(UTF8ToSys(aValue));
    if OnlyPaths then
      ReturnOnlyPaths(PackedFiles);
  end
  else
  begin
    // Trying global cache
    if (w7zGetGlobalCache <> '') and DirectoryExistsUTF8(
      w7zGetGlobalCache) then
    begin
      LoadFromGlobalCache(FileSHA1, PackedFiles, OnlyPaths);
    end;
  end;

  if PackedFiles.Count > 0 then
    Exit;

  // If not cached data found...
  // Executing '7z.exe l -slt -scsUTF-8 -sccUTF-8 <archive>'
  // -------------------------------------------------------
  if Password <> '' then;
  aParam := '-p' + Password;

  i := 0;
  ExecuteCMDArray('', w7zGetPathTo7zexe,
    ['l', '-slt', '-scsUTF-8', '-sccUTF-8', aParam, '--',
    SysPath(a7zArchive)],
    sOutput, sStdErr, i, False);

  slOutput := TStringList.Create;
  try
    slOutput.Text := sOutput;
    slOutput.SaveToFile(UTF8ToSys(w7zGetCacheDir + 'w' +
      FileSHA1 + kw7zCacheFileExt));

    // Checking errors
    if i > 1 then
    begin
      slOutput.Text := sStdErr;
      w7zErrorAdd('w7zListFiles', Format(rsw7zExeError, [a7zArchive, i]));
      w7zErrorAddStdErr(slOutput);
    end;

    if i = 1 then // Warning
    begin
      slOutput.Text := sStdErr;
      w7zErrorAdd('w7zListFiles', Format(rsw7zExeWarning, [a7zArchive, i]));
      w7zErrorAddStdErr(slOutput);
    end;

  finally
    FreeAndNil(slOutput);
  end;

  w7zErrorOK;

  if i > 1 then
    Exit;

  // Reading files and creating cache file
  // -------------------------------------

  slOutput := TStringList.Create;
  slLine := TStringList.Create;
  try
    slOutput.LoadFromFile(UTF8ToSys(w7zGetCacheDir + 'w' +
      FileSHA1 + kw7zCacheFileExt));

    // Skipping until '----------'
    i := 0;
    while (i < slOutput.Count) and (slOutput[i] <> '----------') do
      Inc(i);

    // Now adding files
    aPath := '';
    Size := '';
    PSize := '';
    aDate := '';
    aCRC := '';
    aSHA1 := '';
    while (i < slOutput.Count) do
    begin
      aPos := UTF8Pos('=', slOutput[i]);

      if aPos <> 0 then
      begin
        aParam := UTF8LowerCase(Trim(UTF8Copy(slOutput[i], 1, aPos - 1)));
        aValue := Trim(UTF8Copy(slOutput[i], aPos + 1, MaxInt));

        // Well, I hope that always 'Path = ' will be the first line
        //   of the file data, because a new line to the StringList is added.
        if UTF8CompareText(aParam, 'path') = 0 then
        begin
          // Adding the file
          if aPath <> '' then
          begin
            slLine.Clear;
            slLine.Add(aPath);
            slLine.Add(Size);
            slLine.Add(PSize);
            slLine.Add(aDate);
            slLine.Add(aCRC);
            slLine.Add(aSHA1);
            PackedFiles.Add(slLine.CommaText);
          end;

          aPath := aValue;
          Size := '';
          PSize := '';
          aDate := '';
          aCRC := '';
          aSHA1 := '';
        end
        else if UTF8CompareText(aParam, 'size') = 0 then
          Size := aValue
        else if UTF8CompareText(aParam, 'packed size') = 0 then
          PSize := aValue
        else if UTF8CompareText(aParam, 'modified') = 0 then
          aDate := aValue
        else if UTF8CompareText(aParam, 'crc') = 0 then
          aCRC := aValue
        else if UTF8CompareText(aParam, 'sha1') = 0 then // Dummy...
          aSHA1 := aValue;
      end;
      Inc(i);
    end;

    // Adding the last packed file
    if aPath <> '' then
    begin
      slLine.Clear;
      slLine.Add(aPath);
      slLine.Add(Size);
      slLine.Add(PSize);
      slLine.Add(aDate);
      slLine.Add(aCRC);
      slLine.Add(aSHA1);
      PackedFiles.Add(slLine.CommaText);

      // Only save if there is at least one file in the compressed archive
      PackedFiles.SaveToFile(UTF8ToSys(w7zGetCacheDir + FileSHA1 +
        kw7zCacheFileExt));

      // Global Cache stuff
      SaveGlobalCache(FileSHA1, PackedFiles);
    end;

  finally
    FreeAndNil(slLine);
    FreeAndNil(slOutput);
  end;

  if OnlyPaths then
    ReturnOnlyPaths(PackedFiles);

  w7zErrorOK;
end;

procedure w7zFilesByExt(AOutFolderList, AOutFileList : TStrings;
  aBaseFolder : string; aExtList : TStrings; Recursive : Boolean);
var
  FileMask : string;
  Archives, Compressed : TStringList;
  i, j : Integer;
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
      w7zListFiles(Archives[i], Compressed, True, '');
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


function w7zExtractFile(a7zArchive : string; const aFileMask : string;
  aFolder : string; const ShowProgress : Boolean;
  const Password : string) : Integer;
var
  aExeString : string;
  Params, slOutput : TStringList;
  sStdOut, sStdErr : string;
begin
  Result := -1;

  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);

  if not FileExistsUTF8(a7zArchive) then
  begin
    w7zErrorAdd('w7zExtractFile', Format(rsw7zFileNotFound, [a7zArchive]));
    Exit;
  end;

  // 7z.exe returns Fatal Error if not changed back to windows style :-(
  aFolder := SysPath(SetAsFolder(aFolder));


  // Selecting graphical exe to show progress.
  if ShowProgress then
  begin
    if w7zPathTo7zGexeOK then
    begin
      aExeString := w7zGetPathTo7zGexe;
    end
    else if w7zPathTo7zexeOK then
    begin
      aExeString := w7zGetPathTo7zexe;
    end
    else
      Exit;
  end
  else
  begin
    if not w7zPathTo7zexeOK then
      Exit;

    aExeString := w7zGetPathTo7zexe;
  end;

  // Parameters
  Params := TStringList.Create;
  try
    Params.Add('x');
    Params.Add('-scsUTF-8');
    Params.Add('-sccUTF-8');

    if not ShowProgress then
    begin
      // if progress is not shown then answer yes to all queries
      //   and overwrite if file exist... use it with care.
      Params.Add('-aoa');
      Params.Add('-y');
    end;

    if Password <> '' then
      Params.Add('-p' + Password);

    Params.Add('-o' + aFolder);
    Params.Add('--');

    Params.Add(a7zArchive);
    Params.Add(aFileMask);

    ExecuteCMDSL('', aExeString, Params, sStdOut, sStdErr, Result, False);

    if Result > 0 then
    begin
      slOutput := TStringList.Create;
      try
        // Checking errors
        if Result > 1 then
        begin
          slOutput.Text := sStdErr;
          w7zErrorAdd('w7zExtractFile', Format(rsw7zExeError,
            [a7zArchive, Result]));
          w7zErrorAddStdErr(slOutput);
        end;

        if Result = 1 then // Warning
        begin
          slOutput.Text := sStdErr;
          w7zErrorAdd('w7zExtractFile', Format(rsw7zExeWarning,
            [a7zArchive, Result]));
          w7zErrorAddStdErr(slOutput);
        end;

      finally
        FreeAndNil(slOutput);
      end;
    end;

  finally
    Params.Free;
  end;

  RemoveReadOnlyFolderRecursive(aFolder);

  w7zErrorOK;
end;

function w7zCompressFile(a7zArchive : string; aFileList : TStrings;
  const ShowProgress : Boolean; const CompType : string) : Integer;
var
  aExeString : string;
  i : Integer;
  sStdOut, sStdErr : string;
  Params, slOutput : TStringList;
begin
  Result := -1;

  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);

  // Selecting graphical exe to show progress.
  if ShowProgress then
  begin
    if w7zPathTo7zGexeOK then
    begin
      aExeString := w7zGetPathTo7zGexe;
    end
    else if w7zPathTo7zexeOK then
    begin
      aExeString := w7zGetPathTo7zexe;
    end
    else
      Exit;
  end
  else
  begin
    if not w7zPathTo7zexeOK then
      Exit;

    aExeString := w7zGetPathTo7zexe;
  end;

  // Parameters
  Params := TStringList.Create;
  try
    Params.Add('a');
    if CompType <> '' then
      Params.Add('-t' + CompType);
    Params.Add('-scsUTF-8');
    Params.Add('-sccUTF-8');
    Params.Add('-mx=9');

    if not ShowProgress then
    begin
      // if progress is not shown then answer yes to all queries
      //   and overwrite if file exist... use it with care.
      Params.Add('-aoa');
      Params.Add('-y');
    end;

    //if Password <> '' then
    //  Params.Add('-p' + Password);

    Params.Add('--');

    Params.Add(a7zArchive);

    for i := 0 to aFileList.Count - 1 do
      Params.Add(aFileList[i]);

    ExecuteCMDSL('', aExeString, Params, sStdOut, sStdErr, Result, False);

    if Result > 0 then
    begin
      slOutput := TStringList.Create;
      try
        // Checking errors
        if Result > 1 then
        begin
          slOutput.Text := sStdErr;
          w7zErrorAdd('w7zCompressFile', Format(rsw7zExeError,
            [a7zArchive, Result]));
          w7zErrorAddStdErr(slOutput);
        end;

        if Result = 1 then // Warning
        begin
          slOutput.Text := sStdErr;
          w7zErrorAdd('w7zCompressFile', Format(rsw7zExeWarning,
            [a7zArchive, Result]));
          w7zErrorAddStdErr(slOutput);
        end;

      finally
        FreeAndNil(slOutput);
      end;
    end;

  finally
    Params.Free;
  end;

  w7zErrorOK;
end;

function w7zCompressFolder(a7zArchive, aFolder : string;
  IncludeRoot : Boolean; const ShowProgress : Boolean;
  const CompType : string) : Integer;
var
  aExeString : string;
  sStdOut, sStdErr : string;
  Params, slOutput : TStringList;
begin
  Result := -1;

  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);
  aFolder := SysPath(SetAsFolder(aFolder));

  // Selecting graphical exe to show progress.
  if ShowProgress then
  begin
    if w7zPathTo7zGexeOK then
    begin
      aExeString := w7zGetPathTo7zGexe;
    end
    else if w7zPathTo7zexeOK then
    begin
      aExeString := w7zGetPathTo7zexe;
    end
    else
      Exit;
  end
  else
  begin
    if not w7zPathTo7zexeOK then
      Exit;

    aExeString := w7zGetPathTo7zexe;
  end;

  // Parameters
  Params := TStringList.Create;
  try
    Params.Add('a');
    if CompType <> '' then
      Params.Add('-t' + CompType);
    Params.Add('-scsUTF-8');
    Params.Add('-sccUTF-8');
    Params.Add('-mx=9');

    if not ShowProgress then
    begin
      // if progress is not shown then answer yes to all queries
      //   and overwrite if file exist... use it with care.
      Params.Add('-aoa');
      Params.Add('-y');
    end;

    // if Password <> '' then
    //   Params.Add('-p' + Password);

    Params.Add('--');

    Params.Add(a7zArchive);

    if IncludeRoot then
      Params.Add(aFolder)
    else
      Params.Add(aFolder + '*');

    ExecuteCMDSL('', aExeString, Params, sStdOut, sStdErr, Result, False);

    if Result > 0 then
    begin
      slOutput := TStringList.Create;
      try
        // Checking errors
        if Result > 1 then
        begin
          slOutput.Text := sStdErr;
          w7zErrorAdd('w7zCompressFile', Format(rsw7zExeError,
            [a7zArchive, Result]));
          w7zErrorAddStdErr(slOutput);
        end;

        if Result = 1 then // Warning
        begin
          slOutput.Text := sStdErr;
          w7zErrorAdd('w7zCompressFile', Format(rsw7zExeWarning,
            [a7zArchive, Result]));
          w7zErrorAddStdErr(slOutput);
        end;

      finally
        FreeAndNil(slOutput);
      end;
    end;

  finally
    Params.Free;
  end;

  w7zErrorOK;
end;

function w7zCRC32InnerFile(a7zArchive : string; const aInnerFile : string;
  const Password : string) : Cardinal;
begin
  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);

  Result := StrToUIntDef('$' + w7zCRC32InnerFileStr(a7zArchive,
    aInnerFile, Password), 0);
end;

function w7zCRC32InnerFileStr(a7zArchive : string; aInnerFile : string;
  const Password : string) : string;
var
  aFileList, TmpStrList : TStringList;
  Found : Boolean;
  FileSHA1 : string;
  i : Integer;
begin
  Result := '';

  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);
  aInnerFile := SysPath(aInnerFile);

  aFileList := TStringList.Create;
  TmpStrList := TStringList.Create;
  try
    aFileList.BeginUpdate;
    w7zListFiles(a7zArchive, aFileList, False, Password);
    aFileList.EndUpdate;

    i := 0;
    Found := False;
    while (not Found) and (i < aFileList.Count) do
    begin
      TmpStrList.Clear;
      TmpStrList.CommaText := aFileList[i];

      if (TmpStrList.Count > 4) and
        (CompareFilenamesIgnoreCase(SysPath(TmpStrList[0]),
        aInnerFile) = 0) then
      begin
        Result := TmpStrList[4]; // CRC32
        Found := True;
      end;
      Inc(i);
    end;

    // Ops, we don't have CRC32
    if Found and (Result = '') then
    begin
      Dec(i); //Actual position of found file

      // Extracting/CRC32/Deleting
      w7zExtractFile(a7zArchive, aInnerFile, w7zGetCacheDir, False, '');
      if FileExistsUTF8(w7zGetCacheDir + aInnerFile) then
      begin
        Result := CRC32FileStr(w7zGetCacheDir + aInnerFile);
        DeleteFileUTF8(w7zGetCacheDir + aInnerFile);
      end;

      // Saving to caches
      if Result <> '' then
      begin
        FileSHA1 := SHA1FileStr(a7zArchive);

        TmpStrList.Clear;
        TmpStrList.CommaText := aFileList[i];

        while TmpStrList.Count < 6 do
          TmpStrList.Add('');
        TmpStrList[4] := Result; // CRC32

        aFileList[i] := TmpStrList.CommaText;

        // Temp Cache, if exists
        if FileExistsUTF8(w7zGetCacheDir + FileSHA1 + kw7zCacheFileExt) then
          aFileList.SaveToFile(UTF8ToSys(w7zGetCacheDir +
            FileSHA1 + kw7zCacheFileExt));

        // Global Cache
        SaveGlobalCache(FileSHA1, aFileList);
      end;
    end;
  finally
    TmpStrList.Free;
    aFileList.Free;
  end;
end;

function w7zSHA32InnerFile(a7zArchive : string; const aInnerFile : string;
  const Password : string) : TSHA1Digest;
begin
  Result := StringToSHA1Digest(w7zSHA32InnerFileStr(
    a7zArchive, aInnerFile, Password));
end;

function w7zSHA32InnerFileStr(a7zArchive : string; aInnerFile : string;
  const Password : string) : string;
var
  aFileList, TmpStrList : TStringList;
  Found : Boolean;
  FileSHA1 : string;
  i : Integer;
begin
  Result := '';

  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(SysPath(a7zArchive));
  aInnerFile := SysPath(aInnerFile);

  aFileList := TStringList.Create;
  TmpStrList := TStringList.Create;
  try
    aFileList.BeginUpdate;
    w7zListFiles(a7zArchive, aFileList, False, Password);
    aFileList.EndUpdate;

    i := 0;
    Found := False;
    while (not Found) and (i < aFileList.Count) do
    begin
      TmpStrList.Clear;
      TmpStrList.CommaText := aFileList[i];

      if (TmpStrList.Count > 5) and
        (CompareFilenamesIgnoreCase(SysPath(TmpStrList[0]),
        aInnerFile) = 0) then
      begin
        Result := TmpStrList[5]; // SHA1
        Found := True;
      end;
      Inc(i);
    end;

    // Ops, we don't have SHA1
    if Found and (Result = '') then
    begin
      Dec(i); //Actual position of found file

      // Extracting/SHA1/Deleting
      w7zExtractFile(a7zArchive, aInnerFile, w7zGetCacheDir, False, '');
      if FileExistsUTF8(w7zGetCacheDir + aInnerFile) then
      begin
        Result := SHA1FileStr(w7zGetCacheDir + aInnerFile);
        DeleteFileUTF8(w7zGetCacheDir + aInnerFile);
      end;

      // Saving to caches
      if Result <> '' then
      begin
        FileSHA1 := SHA1FileStr(a7zArchive);

        TmpStrList.Clear;
        TmpStrList.CommaText := aFileList[i];

        while TmpStrList.Count < 6 do
          TmpStrList.Add('');
        TmpStrList[5] := Result; // SHA1

        aFileList[i] := TmpStrList.CommaText;

        // Temp Cache, if exists
        if FileExistsUTF8(w7zGetCacheDir + FileSHA1 + kw7zCacheFileExt) then
          aFileList.SaveToFile(UTF8ToSys(w7zGetCacheDir +
            FileSHA1 + kw7zCacheFileExt));

        // Global Cache
        SaveGlobalCache(FileSHA1, aFileList);
      end;
    end;
  finally
    TmpStrList.Free;
    aFileList.Free;
  end;
end;

initialization
  w7zErrorList := TStringList.Create;
  w7zSetErrorListFile('');

  w7zSetCacheDir('');
  w7zSetFileExts('');

  w7zPathTo7zexeOK := False;
  w7zPathTo7zGexeOK := False;

  // Little checks before default location...
  if FileExistsUTF8(SetAsFolder(ProgramDirectory) + '7z.exe') then
    w7zSetPathTo7zexe(SetAsFolder(ProgramDirectory) + '7z.exe')
  else if FileExistsUTF8(SetAsFolder(SetAsFolder(ProgramDirectory) + '7z') +
    '7z.exe') then
    w7zSetPathTo7zexe(SetAsFolder(SetAsFolder(ProgramDirectory) + '7z') +
      '7z.exe');

  if FileExistsUTF8(SetAsFolder(ProgramDirectory) + '7zG.exe') then
    w7zSetPathTo7zGexe(SetAsFolder(ProgramDirectory) + '7zG.exe')
  else if FileExistsUTF8(SetAsFolder(SetAsFolder(ProgramDirectory) + '7z') +
    '7zG.exe') then
    w7zSetPathTo7zGexe(SetAsFolder(SetAsFolder(ProgramDirectory) + '7z') +
      '7zG.exe');

finalization
  if w7zErrorListFile <> '' then
    if not (w7zErrorList.Count = 0) then
      w7zErrorList.SaveToFile(w7zErrorListFile);
  w7zErrorList.Free;

  // Removing Temp dir
  // TODO: Crappy security check
  if Length(w7zGetCacheDir) > 15 then
    DeleteDirectory(w7zGetCacheDir, False);

end.
{
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
