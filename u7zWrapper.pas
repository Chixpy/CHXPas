{ 7z.exe and 7zG.exe Wrapper

  Copyright (C) 2011-2016 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}

{ Simple 7z.exe AND 7zG.exe wrapper until something better is found.

  Last version tested 16.02, although may be works with any newer.

  On Win10, older versions of 7zip don't work.

  @definitionList(
    @itemLabel(NOTE:)
    @item(7z.exe, 7zG.exe and 7z.dll have their own licenses.)
  )
}
unit u7zWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Process, LazUTF8, sha1,
  IniFiles,
  uCHXStrUtils;

resourcestring
  w7zFileNotFound = '"%0:s" file not found. ' + LineEnding + 'Current Dir: %1:s';
  //< Translatable string: '"%0:s" file not found'
  w7zExeError = '7z.exe/7zG.exe returned %0:d exit code';

//< Translatable string: '7z.exe/7zG.exe returned %0:d exit code'

const
  // Experimental Global Cache
  kw7zGCIniPrefix = 'w7z.';
  kw7zGCIniFiles = kw7zGCIniPrefix + 'Files';
  kw7zGCIniSizes = kw7zGCIniPrefix + 'Sizes';
  kw7zGCIniPSizes = kw7zGCIniPrefix + 'PSizes';
  kw7zGCIniDates = kw7zGCIniPrefix + 'Dates';
  kw7zGCIniCRCs = kw7zGCIniPrefix + 'CRCs';

  kw7zCacheFileExt = '.txt';
  kw7zFileExts = '001,7z,arj,bpl,bz2,bzip2,cab,cba,cb7,cbr,cbz,chi,chm,chq,chw,'
    + 'cpio,cramfs,deb,dll,dmg,doc,exe,fat,flv,gz,gzip,hfs,hxi,hxq,hxr,hxs,' +
    'hxw,img,iso,jar,lha,lit,lzh,lzma,lzma86,mbr,msi,msp,nsis,ntfs,ppt,' +
    'r00,rar,rpm,scap,squashfs,swf,swm,sys,tar,taz,tbz,tbz2,tgz,tpz,txz,' +
    'vhd,wim,xar,xls,xpi,xz,z,zip';

type
  w7zException = class(Exception);

function w7zGetFileExts: string;
procedure w7zSetFileExts(aExtList: string = '');
{< Get/Set default file extensions. Empty restore default ones.

  String with suported file extensions by 7z.

   Format: 'ext,ext,ext' for easy creating a TStringList. At least until
     we found a better way for searching files with different extension.

   Warning: It's not used for test if the files passed as params are
     compressed files. It's only a reference list.
}

function w7zGetPathTo7zexe: string;
procedure w7zSetPathTo7zexe(aPath: string);
{< Path to 7z.exe executable.

  It can be useful for hidding the process, but it's
    needed for listing archives anyways.
}

function w7zGetPathTo7zGexe: string;
procedure w7zSetPathTo7zGexe(aPath: string);
{< Path to 7zG.exe executable.
}
function w7zGetCacheDir: string;
procedure w7zSetCacheDir(aPath: string);
{< Directory were lists of files from compressed archives are stored.

  Defaults to '%USERTEMPDIR%/w7zCache', and the directory is deleted at
    program exit.
}
function w7zGetGlobalCache: string;
procedure w7zSetGlobalCache(aPath: string);
{< ¡EXPERIMENTAL!

  Similar to w7zCacheDir, but different folder structure:
    'w7zGlobalCache/SHA[1-2]/SHA[3-4].ini'

  NOT DELETED AT EXIT. FOLDER MUST EXISTS.
}

procedure w7zListFiles(const aFilename: string; PackedFiles: TStrings;
  const OnlyPaths: boolean = False; const UseCache: boolean = True;
  const Password: string = '');
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

function w7zExtractFile(a7zArchive: string; const aFileMask: string;
  aFolder: string; const ShowProgress: boolean;
  const Password: string): integer;
{< Extract de file (or files) from 7z archive.

  @param(aFilename Name of the 7z archive.)
  @param(aFolder Folder where the file(s) will be extracted.)
  @param(aFileMask Mask or file to extract. Remember, for 7z.exe '*' means all
    files, not '*.*' wich means all files with extension.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(Password Password for 7z archive.)
  @return(Exit code)
}

function w7zCompressFile(const a7zArchive: string; aFileList: TStrings;
  const ShowProgress: boolean; const CompType: string = ''): integer;
{< Compress files in a 7z (or other type) archive.

  @param(a7zArchive Name of the 7z/zip archive.)
  @param(aFileList List of files to add to the archive.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(CompType Type of the archive.)
  @return(Exit code)
}

implementation

var
  w7zFileExts: string;
  {< String with suported file extensions by 7z.

     Format: 'ext,ext,ext' for easy creating a TStringList. At least until
       we found a better way for searching files with different extension.

     Warning: It's not used for test if the files passed as params are
       compressed files. It's only a reference list.
  }
  w7zPathTo7zexe: string;
  {< Path to 7z.exe executable.

    It can be useful for hidding the process, but it's
      needed for listing archives anyways.
  }
  w7zPathTo7zGexe: string;
  {< Path to 7zG.exe executable.
  }

  w7zCacheDir: string;
  {< Directory were lists of files from compressed archives are stored.

    Defaults to '%USERTEMPDIR%/w7zCache', and the directory is deleted at
      program exit.
  }

  w7zGlobalCache: string;

  {< ¡EXPERIMENTAL!

    Similar to w7zCacheDir, but different folder structure:
      'w7zGlobalCache/SHA[1-2]/SHA[3-4].ini'

    NOT DELETED AT EXIT. FOLDER MUST EXISTS.
  }

function w7zGetFileExts: string;
begin
  Result := w7zFileExts;
end;

procedure w7zSetFileExts(aExtList: string);
begin
  if aExtList = '' then
    w7zFileExts := kw7zFileExts
  else
    w7zFileExts := aExtList;
end;

function w7zGetPathTo7zexe: string;
begin
  Result := w7zPathTo7zexe;
end;

procedure w7zSetPathTo7zexe(aPath: string);
begin
  w7zPathTo7zexe := CleanAndExpandFilename(SetAsFile(aPath));
end;

function w7zGetPathTo7zGexe: string;
begin
  Result := w7zPathTo7zGexe;
end;

procedure w7zSetPathTo7zGexe(aPath: string);
begin
  w7zPathTo7zGexe := CleanAndExpandFilename(SetAsFile(aPath));
end;

function w7zGetCacheDir: string;
begin
  Result := w7zCacheDir;
end;

procedure w7zSetCacheDir(aPath: string);
begin
  if aPath = '' then
    aPath := SetAsFolder(GetTempDir(False)) + 'w7zCache';
  aPath := CleanAndExpandDirectory(SetAsFolder(aPath));

  // TODO: Better security check...
  if Length(w7zCacheDir) > 25 then
    DeleteDirectory(w7zCacheDir, False);
  ForceDirectoriesUTF8(aPath);
  w7zCacheDir := aPath;
end;

function w7zGetGlobalCache: string;
begin
  Result := w7zGlobalCache;
end;

procedure w7zSetGlobalCache(aPath: string);
begin
  w7zGlobalCache := SetAsFolder(aPath);
end;

procedure w7zListFiles(const aFilename: string; PackedFiles: TStrings;
  const OnlyPaths: boolean = False; const UseCache: boolean = True;
  const Password: string = '');

  procedure ReturnOnlyPaths(aFileList: TStrings);
  var
    //slLine: TStringList;
    i, aPos: integer;
  begin
    // Simpler and faster way?
    i := 0;
    while i < aFileList.Count do
    begin
      aPos := UTF8Pos(',', aFileList[i]);
      if aPos <> 0 then
      begin
        aFileList[i] := AnsiDequotedStr(UTF8Copy(aFileList[i], 1, aPos - 1),
          aFileList.QuoteChar);
      end;
      Inc(i);
    end;

    {
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
          PackedFiles[i] := slLine[0];
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
    }
  end;

  procedure LoadGlobalCache(FileSHA1: string; PackedFiles: TStrings;
    OnlyPaths: boolean);
  var
    aFile: string;
    sl, slPath, slSizes, slPSizes, slDates, slCRC: TStringList;
    aIni: TMemIniFile;
    i: integer;
  begin
    aFile := SetAsFolder(w7zGetGlobalCache + Copy(FileSHA1, 1, 2)) +
      Copy(FileSHA1, 3, 2) + '.ini';
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
          try
            slPath.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniFiles, '');
            slSizes.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniSizes, '');
            slPSizes.CommaText :=
              aIni.ReadString(FileSHA1, kw7zGCIniPSizes, '');
            slDates.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniDates, '');
            slCRC.CommaText := aIni.ReadString(FileSHA1, kw7zGCIniCRCs, '');

            while slSizes.Count < slPath.Count do
              slSizes.Add('');
            while slPSizes.Count < slPath.Count do
              slPSizes.Add('');
            while slDates.Count < slPath.Count do
              slDates.Add('');
            while slCRC.Count < slPath.Count do
              slCRC.Add('');

            i := 0;
            while i < slPath.Count do
            begin
              sl.Clear;
              sl.Add(slPath[i]);
              sl.Add(slSizes[i]);
              sl.Add(slPSizes[i]);
              sl.Add(slDates[i]);
              sl.Add(slCRC[i]);
              PackedFiles.Add(sl.CommaText);
              Inc(i);
            end;

          finally
            sl.EndUpdate;
            FreeAndNil(sl);
            FreeAndNil(slPath);
            FreeAndNil(slSizes);
            FreeAndNil(slPSizes);
            FreeAndNil(slDates);
            FreeAndNil(slCRC);
          end;
        end;
      finally
        FreeAndNil(aIni);
      end;
    end;
  end;

  procedure SaveGlobalCache(FileSHA1: string; PackedFiles: TStrings);
  var
    aFile: string;
    sl, slPath, slSizes, slPSizes, slDates, slCRC: TStringList;
    aIni: TMemIniFile;
    i: Integer;
  begin
    aFile := SetAsFolder(w7zGetGlobalCache + Copy(FileSHA1, 1, 2)) +
      Copy(FileSHA1, 3, 2) + '.ini';

    aIni := TMemIniFile.Create(aFile);
    sl := TStringList.Create;
    sl.BeginUpdate;
    slPath := TStringList.Create;
    slPath.BeginUpdate;
    slSizes := TStringList.Create;
    slSizes.BeginUpdate;
    slPSizes := TStringList.Create;
    slPSizes.BeginUpdate;
    slDates := TStringList.Create;
    slDates.BeginUpdate;
    slCRC := TStringList.Create;
    slCRC.BeginUpdate;
    try
      i := 0;
      while i < PackedFiles.Count do
      begin
        sl.CommaText := PackedFiles[i];
        slPath.add(sl[0]);
        slSizes.add(sl[1]);
        slPSizes.add(sl[2]);
        slDates.add(sl[3]);
        slCRC.add(sl[4]);

        Inc(i);
      end;

      aIni.WriteString(FileSHA1, kw7zGCIniFiles, slPath.CommaText);
      aIni.WriteString(FileSHA1, kw7zGCIniSizes, slSizes.CommaText);
      aIni.WriteString(FileSHA1, kw7zGCIniPSizes, slPSizes.CommaText);
      aIni.WriteString(FileSHA1, kw7zGCIniDates, slDates.CommaText);
      aIni.WriteString(FileSHA1, kw7zGCIniCRCs, slCRC.CommaText);

      aIni.UpdateFile;

    finally
      sl.EndUpdate;
      FreeAndNil(sl);
      slPath.EndUpdate;
      FreeAndNil(slPath);
      slSizes.EndUpdate;
      FreeAndNil(slSizes);
      slPSizes.EndUpdate;
      FreeAndNil(slPSizes);
      slDates.EndUpdate;
      FreeAndNil(slDates);
      slCRC.EndUpdate;
      FreeAndNil(slCRC);
      FreeAndNil(aIni);
    end;
  end;

var
  FileSHA1: string;
  aPos, i: integer;
  slLine, slOutput: TStringList;
  msOutput: TMemoryStream;
  aProcess: TProcess;
  aParam, aValue: string;
  aPath, Size, PSize, aDate, aCRC: string;

begin
  // Clearing PackedFiles file list
  if PackedFiles <> nil then
    PackedFiles.Clear
  else
    PackedFiles := TStringList.Create;

  // Checking needed files
  if not FileExistsUTF8(w7zGetPathTo7zexe) then
    raise EInOutError.CreateFmt(w7zFileNotFound, [w7zGetPathTo7zexe]);

  if not FileExistsUTF8(aFilename) then
    raise EInOutError.CreateFmt(w7zFileNotFound, [aFilename]);

  // SHA1 of the file... cache file is saved always
  FileSHA1 := SHA1Print(SHA1File(UTF8ToSys(aFilename)));

  // Searching for cache file
  // ------------------------
  if UseCache then
  begin
    aValue := w7zGetCacheDir + FileSHA1 + kw7zCacheFileExt;
    if FileExistsUTF8(aValue) then
    begin
      PackedFiles.LoadFromFile(UTF8ToSys(aValue));
      if OnlyPaths then
        ReturnOnlyPaths(PackedFiles);
    end
    else
    begin
      if (w7zGetGlobalCache <> '') and DirectoryExistsUTF8(
        w7zGetGlobalCache) then
      begin
        LoadGlobalCache(FileSHA1, PackedFiles, OnlyPaths);
      end;
    end;
            if PackedFiles.Count > 0 then Exit;

  end;

  // Executing '7z.exe l -slt -scsUTF-8 -sccUTF-8 <archive>'
  // -------------------------------------------------------
  aProcess := TProcess.Create(nil);
  msOutput := TMemoryStream.Create;
  try
    aProcess.Executable := UTF8ToSys(w7zGetPathTo7zexe);
    aProcess.Parameters.Add('l');
    aProcess.Parameters.Add('-slt');
    aProcess.Parameters.Add('-scsUTF-8');
    aProcess.Parameters.Add('-sccUTF-8');
    if Password <> '' then
      aProcess.Parameters.Add('-p' + UTF8ToSys(Password));
    aProcess.Parameters.Add(UTF8ToSys(aFilename));
    aProcess.Options := aProcess.Options + [poUsePipes, poNoConsole];
    aProcess.Execute;

    // Reading output
    aPos := 0;
    while (aProcess.Running) or (aProcess.Output.NumBytesAvailable > 0) do
    begin
      i := aProcess.Output.NumBytesAvailable;
      if i > 0 then
      begin
        msOutput.SetSize(aPos + i);
        Inc(aPos, aProcess.Output.Read((msOutput.Memory + aPos)^, i));
      end;
          { Meh, don't sleep
          else
            Sleep(100); // Waiting for more output
          }
    end;
    msOutput.SaveToFile(UTF8ToSys(w7zGetCacheDir + 'w7zOutput' +
      kw7zCacheFileExt));
  finally
    i := aProcess.ExitStatus;
    FreeAndNil(aProcess);
    FreeAndNil(msOutput);
  end;

  // TODO 3: Handle Warnings too...
  if (i <> 0) and (i <> 1) then // 1 = Warning
    raise w7zException.CreateFmt(w7zExeError, [i]);

  // Reading files and creating cache file
  // -------------------------------------

  slOutput := TStringList.Create;
  slLine := TStringList.Create;
  try
    slOutput.LoadFromFile(UTF8ToSys(w7zGetCacheDir + 'w7zOutput' +
      kw7zCacheFileExt));

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
            PackedFiles.Add(slLine.CommaText);
          end;

          aPath := aValue;
          Size := '';
          PSize := '';
          aDate := '';
          aCRC := '';
        end
        else if UTF8CompareText(aParam, 'size') = 0 then
          Size := aValue
        else if UTF8CompareText(aParam, 'packed size') = 0 then
          PSize := aValue
        else if UTF8CompareText(aParam, 'modified') = 0 then
          aDate := aValue
        else if UTF8CompareText(aParam, 'crc') = 0 then
          aCRC := aValue;
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
      PackedFiles.Add(slLine.CommaText);

      // Only save if there is at least one file in the compressed archive
      PackedFiles.SaveToFile(UTF8ToSys(w7zGetCacheDir + FileSHA1 +
        kw7zCacheFileExt));

      // Global Cache stuff
      if (w7zGetGlobalCache <> '') and DirectoryExistsUTF8(
        w7zGetGlobalCache) then
        SaveGlobalCache(FileSHA1, PackedFiles);
    end;

  finally
    FreeAndNil(slLine);
    FreeAndNil(slOutput);
  end;

  if OnlyPaths then
    ReturnOnlyPaths(PackedFiles);
end;

function w7zExtractFile(a7zArchive: string; const aFileMask: string;
  aFolder: string; const ShowProgress: boolean;
  const Password: string): integer;
var
  aProcess: TProcess;
  aOptions: TProcessOptions;
  aExeString: string;
begin
  Result := 0;
  // Sometime are stored as directories
  a7zArchive := ExcludeTrailingPathDelimiter(a7zArchive);
  if not FileExistsUTF8(a7zArchive) then
    raise EInOutError.CreateFmt(w7zFileNotFound, [a7zArchive]);

  aOptions := [poWaitOnExit];
  aExeString := w7zGetPathTo7zexe;
  aFolder := SetAsFolder(aFolder);
  if ShowProgress then
  begin
    if not FileExistsUTF8(w7zGetPathTo7zGexe) then
      raise EInOutError.CreateFmt(w7zFileNotFound, [w7zGetPathTo7zGexe, GetCurrentDirUTF8]);
    aExeString := w7zGetPathTo7zGexe;
  end
  else
  begin
    if not FileExistsUTF8(w7zGetPathTo7zexe) then
      raise EInOutError.CreateFmt(w7zFileNotFound, [w7zGetPathTo7zexe, GetCurrentDirUTF8]);

    if ShowProgress then
      aOptions := aOptions + [poNewConsole]
    else
      aOptions := aOptions + [poNoConsole];

    // 7z.exe returns Fatal Error if not changed back to windows style :-(
    aFolder := SysPath(aFolder);
  end;

  aProcess := TProcess.Create(nil);
  try
    aProcess.Executable := UTF8ToSys(aExeString);
    aProcess.Options := aOptions;

    aProcess.Parameters.Add('x');
    aProcess.Parameters.Add(UTF8ToSys(a7zArchive));
    aProcess.Parameters.Add('-scsUTF-8');
    aProcess.Parameters.Add('-sccUTF-8');

    if not ShowProgress then
    begin
      // if progress is not shown then respond yes to all queries
      //   and overwrite if file exist... use it with care.
      aProcess.Parameters.Add('-aoa');
      aProcess.Parameters.Add('-y');
    end;
    if Password <> '' then
      aProcess.Parameters.Add('-p' + UTF8ToSys(Password));
    aProcess.Parameters.Add('-o' + UTF8ToSys(aFolder));
    aProcess.Parameters.Add('--');
    aProcess.Parameters.Add(UTF8ToSys(aFileMask));
    aProcess.Execute;
    Result := aProcess.ExitStatus;
  finally
    FreeAndNil(aProcess);
  end;
end;

function w7zCompressFile(const a7zArchive: string; aFileList: TStrings;
  const ShowProgress: boolean; const CompType: string): integer;
var
  aProcess: TProcess;
  aOptions: TProcessOptions;
  aExeString: string;
  i: integer;

begin
  aOptions := [poWaitOnExit];
  aExeString := w7zGetPathTo7zexe;
  if (ShowProgress) and (FileExistsUTF8(w7zGetPathTo7zGexe)) then
    aExeString := w7zGetPathTo7zGexe
  else
  begin
    if FileExistsUTF8(w7zGetPathTo7zexe) then
      raise EInOutError.CreateFmt(w7zFileNotFound, [w7zGetPathTo7zexe]);

    if ShowProgress then
      aOptions := aOptions + [poNewConsole]
    else
      aOptions := aOptions + [poNoConsole];
  end;

  aProcess := TProcess.Create(nil);
  try
    aProcess.Executable := UTF8ToSys(aExeString);
    aProcess.Options := aOptions;

    aProcess.Parameters.Add('a');
    if CompType <> '' then
      aProcess.Parameters.Add('-t' + UTF8ToSys(CompType));
    aProcess.Parameters.Add('-scsUTF-8');
    aProcess.Parameters.Add('-sccUTF-8');
    aProcess.Parameters.Add('-mx=9');

    if not ShowProgress then
    begin
      // if progress is not shown then respond yes to all queries
      //   and overwrite if file exist... use it with care.
      aProcess.Parameters.Add('-aoa');
      aProcess.Parameters.Add('-y');
    end;
    aProcess.Parameters.Add('--');

    aProcess.Parameters.Add(UTF8ToSys(a7zArchive));


    for i := 0 to aFileList.Count - 1 do
      aProcess.Parameters.Add(UTF8ToSys(aFileList[i]));

    aProcess.Execute;
    Result := aProcess.ExitStatus;
  finally
    FreeAndNil(aProcess);
  end;
end;

initialization
  w7zSetFileExts('');
  // Little checks before default location...
  w7zSetPathTo7zexe('7z.exe');
  if not FileExistsUTF8(w7zGetPathTo7zexe) then
    w7zSetPathTo7zexe('7z/7z.exe');
  w7zSetPathTo7zGexe('7zG.exe');
  if not FileExistsUTF8(w7zGetPathTo7zGexe) then
    w7zSetPathTo7zGexe('7z/7zG.exe');

  w7zSetCacheDir('');

finalization
  // Removing Temp dir
  // TODO: Crappy security check
  if Length(w7zGetCacheDir) > 15 then
    DeleteDirectory(w7zGetCacheDir, False);

end.
