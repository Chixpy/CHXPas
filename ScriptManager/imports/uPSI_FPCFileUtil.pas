unit uPSI_FPCFileUtil;

{< FileUtil and LazFileUtils for Pascal Script.

  Copyright (C) 2020-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, uPSRuntime, uPSCompiler,
  // CHX units
  uCHXStrUtils;

procedure SIRegister_FPCFileUtil(CL: TPSPascalCompiler);

procedure RIRegister_FPCFileUtil_Routines(S: TPSExec);

implementation
// Helper functions
// ----------------
procedure CHXFindAllFiles(AList: TStrings; const SearchPath: string;
  SearchMask: string; SearchSubDirs: boolean);
begin
  FindAllFiles(AList, SysPath(SearchPath), SearchMask, SearchSubDirs);
end;

procedure CHXFindAllDirectories(AList: TStrings; const SearchPath: string;
  SearchSubDirs: boolean);
begin
  FindAllDirectories(AList, SysPath(SearchPath), SearchSubDirs);
end;

procedure SIRegister_FPCFileUtil(CL: TPSPascalCompiler);
begin
  // Resource Strings
  // ----------------

  // Constants
  // ---------
  //CL.AddConstantN('kCHXSHA1Empty', 'TSHA1Digest').SetString(kCHXSHA1Empty);

  // Types
  // -----
  //Ejemplo
  //CL.AddTypeS('TItFolderObj', 'String');

  // Methods
  // -------

  // File names
  CL.AddDelphiFunction(
    'function CompareFilenames(const Filename1, Filename2: string): integer;');
  CL.AddDelphiFunction(
    'function ExtractFileNameOnly(const AFilename: string): string;');
  CL.AddDelphiFunction(
    'function ExtractFileNameWithoutExt(const AFilename: string): string;');

  // File operations
  CL.AddDelphiFunction('function CreateDir(const NewDir: String): Boolean;');
  CL.AddDelphiFunction('function DeleteFile(const FileName: string): boolean;');
  CL.AddDelphiFunction(
    'function DirectoryExists(const Directory: string): boolean;');
  CL.AddDelphiFunction('function FileExists(const Filename: string): boolean;');
  CL.AddDelphiFunction('function ForceDirectories(const Dir: string): Boolean;');
  CL.AddDelphiFunction('function RemoveDir(const Dir: String): Boolean;');
  CL.AddDelphiFunction(
    'function RenameFile(const OldName, NewName: string): boolean;');


  // File searching

  CL.AddDelphiFunction(
    'procedure FindAllDirectories(AList: TStrings; const SearchPath: String; SearchSubDirs: Boolean);');
  CL.AddDelphiFunction(
    'procedure FindAllFiles(AList: TStrings; const SearchPath: string; SearchMask: string; SearchSubDirs: boolean);');
  // Tambien están en versión función; PascalScript no permite overload.
  //   function FindAllDirectories(const SearchPath: string;
  //     SearchSubDirs: Boolean = True): TStringList; overload;
  //   function FindAllFiles(const SearchPath: String; SearchMask: String = '';
  //     SearchSubDirs: Boolean = True; DirAttr: Word = faDirectory): TStringList;
  //     overload;


  // Falta TSearchRec
  //CL.AddDelphiFunction('function FindFirst(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;')
  //CL.AddDelphiFunction('function FindNext(var Rslt: TSearchRec): Longint;')
  //CL.AddDelphiFunction('procedure FindClose(var F: TSearchrec);')


  // TODO: ----- TO IMPORT -----
  // File names
  //function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
  //function CompareFileExt(const Filename, Ext: string;
  //                        CaseSensitive: boolean = False): integer;
  //function CompareFilenameStarts(const Filename1, Filename2: string): integer;

  //function DirPathExists(DirectoryName: string): boolean;
  //function DirectoryIsWritable(const DirectoryName: string): boolean;
  //function FilenameIsAbsolute(const TheFilename: string):boolean;
  //function FilenameIsWinAbsolute(const TheFilename: string):boolean;
  //function FilenameIsUnixAbsolute(const TheFilename: string):boolean;
  //function ForceDirectory(DirectoryName: string): boolean;
  //procedure CheckIfFileIsExecutable(const AFilename: string);
  //procedure CheckIfFileIsSymlink(const AFilename: string);
  //function FileIsExecutable(const AFilename: string): boolean;
  //function FileIsSymlink(const AFilename: string): boolean;
  //function FileIsHardLink(const AFilename: string): boolean;
  //function FileIsReadable(const AFilename: string): boolean;
  //function FileIsWritable(const AFilename: string): boolean;
  //function FileIsText(const AFilename: string): boolean;
  //function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
  //function FilenameIsTrimmed(const TheFilename: string): boolean;
  //function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
  //function TrimFilename(const AFilename: string): string;
  //function ResolveDots(const AFilename: string): string;
  //function CleanAndExpandFilename(const Filename: string): string; // empty string returns current directory
  //function CleanAndExpandDirectory(const Filename: string): string; // empty string returns current directory
  //function TrimAndExpandFilename(const Filename: string; const BaseDir: string = ''): string; // empty string returns empty string
  //function TrimAndExpandDirectory(const Filename: string; const BaseDir: string = ''): string; // empty string returns empty string
  //function CreateAbsolutePath(const Filename, BaseDirectory: string): string;
  //function TryCreateRelativePath(const Dest, Source: String; UsePointDirectory: boolean;
  //  AlwaysRequireSharedBaseFolder: Boolean; out RelPath: String): Boolean;
  //function CreateRelativePath(const Filename, BaseDirectory: string;
  //  UsePointDirectory: boolean = false; AlwaysRequireSharedBaseFolder: Boolean = True): string;
  //function FileIsInPath(const Filename, Path: string): boolean;

  //type
  //  TPathDelimSwitch = (
  //    pdsNone,    // no change
  //    pdsSystem,  // switch to current PathDelim
  //    pdsUnix,    // switch to slash /
  //    pdsWindows  // switch to backslash \
  //    );
  //const
  //  PathDelimSwitchToDelim: array[TPathDelimSwitch] of char = (
  //    PathDelim, // pdsNone
  //    PathDelim, // pdsSystem
  //    '/',       // pdsUnix
  //    '\'        // pdsWindows
  //    );

  //// Path delimiters
  //procedure ForcePathDelims(Var FileName: string);
  //function GetForcedPathDelims(const FileName: string): string;
  //function AppendPathDelim(const Path: string): string;
  //function ChompPathDelim(const Path: string): string;
  //function SwitchPathDelims(const Filename: string; Switch: TPathDelimSwitch): string;
  //function SwitchPathDelims(const Filename: string; Switch: boolean): string;
  //function CheckPathDelim(const OldPathDelim: string; out Changed: boolean): TPathDelimSwitch;
  //function IsCurrentPathDelim(Switch: TPathDelimSwitch): boolean;

  //// search paths
  //function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
  //function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string): string;
  //function MinimizeSearchPath(const SearchPath: string): string;
  //function FindPathInSearchPath(APath: PChar; APathLen: integer;
  //                              SearchPath: PChar; SearchPathLen: integer): PChar; overload;
  //function FindPathInSearchPath(const APath, SearchPath: string): integer; overload;

  //// file operations
  //function FileAgeUTF8(const FileName: string): Longint;
  //function ExpandFileNameUTF8(const FileName: string; {const} BaseDir: string = ''): string;
  //function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
  //function FileGetAttrUTF8(const FileName: String): Longint;
  //function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
  //function FileSearchUTF8(const Name, DirList : String; ImplicitCurrentDir : Boolean = True): String;
  //function FileIsReadOnlyUTF8(const FileName: String): Boolean;
  //function GetCurrentDirUTF8: String;
  //function SetCurrentDirUTF8(const NewDir: String): Boolean;
  //function CreateDirUTF8(const NewDir: String): Boolean;

  //function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
  //function FileCreateUTF8(Const FileName : string) : THandle; overload;
  //function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle; overload;
  //Function FileCreateUtf8(Const FileName : String; ShareMode : Integer; Rights : Cardinal) : THandle; overload;

  //function FileSizeUtf8(const Filename: string): int64;
  //function GetFileDescription(const AFilename: string): string;
  //function ReadAllLinks(const Filename: string;
  //                 {%H-}ExceptionOnError: boolean): string; // if a link is broken returns ''
  //function TryReadAllLinks(const Filename: string): string; // if a link is broken returns Filename
  //function GetShellLinkTarget(const FileName: string): string;

  //// for debugging
  //function DbgSFileAttr(Attr: LongInt): String;


  //type
  //  TPhysicalFilenameOnError = (pfeException,pfeEmpty,pfeOriginal);
  //function GetPhysicalFilename(const Filename: string;
  //        OnError: TPhysicalFilenameOnError): string;
  //{$IFDEF Unix}
  //function GetUnixPhysicalFilename(const Filename: string;
  //                      ExceptionOnError: boolean): string; // if a link is broken returns ''
  //{$ENDIF}

  //function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
  //function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean = false;
  //  CreateDir: boolean = false): string;
  //function GetTempFileNameUTF8(const Dir, Prefix: String): String;

  //// UNC paths
  //function IsUNCPath(const {%H-}Path: String): Boolean;
  //function ExtractUNCVolume(const {%H-}Path: String): String;
  //function ExtractFileRoot(FileName: String): String;

  //// darwin paths
  //{$IFDEF darwin}
  //function GetDarwinSystemFilename(Filename: string): string;
  //function GetDarwinNormalizedFilename(Filename: string; nForm:Integer=2): string;
  //{$ENDIF}

  //// windows paths
  //{$IFDEF windows}
  //function SHGetFolderPathUTF8(ID :  Integer) : String;
  //{$ENDIF}

  //// Command line
  //procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
  //                             ReadBackslash: boolean = false);
  //function StrToCmdLineParam(const Param: string): string;
  //function MergeCmdLineParams(ParamList: TStrings): string;
  //// ToDo: Study if they are needed or if the above functions could be used instead.
  //procedure SplitCmdLine(const CmdLine: string;
  //                       out ProgramFilename, Params: string);
  //function PrepareCmdLineOption(const Option: string): string;


  //type
  //  TInvalidateFileStateCacheEvent = procedure(const Filename: string);
  //var
  //  OnInvalidateFileStateCache: TInvalidateFileStateCacheEvent = nil;
  //procedure InvalidateFileStateCache(const Filename: string = ''); inline;


  //  function ComparePhysicalFilenames(const Filename1, Filename2: string): integer;
  //function CompareFilenames(Filename1: PChar; Len1: integer;
  //  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer; overload;
  //function ExtractShortPathNameUTF8(Const FileName : String) : String;
  //function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
  //function ProgramDirectory: string;
  //function ProgramDirectoryWithBundle: string;

  //function ExpandUNCFileNameUTF8(const FileName: string): string;
  //function FileSize(const Filename: string): int64; overload; inline;
  //function FilenameIsPascalUnit(const Filename: string): boolean;
  //function FileIsInPath(const Filename, Path: string): boolean;
  //function FileIsInDirectory(const Filename, Directory: string): boolean;

  //function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string; deprecated 'Use the function from unit LazFileUtils';
  //function CreateAbsolutePath(const Filename, BaseDirectory: string): string; deprecated 'Use the function from unit LazFileUtils';

  //function GetAllFilesMask: string; inline;
  //function GetExeExt: string; inline;
  //function ReadFileToString(const Filename: string): string;

  //// file search
  //type
  //  TSearchFileInPathFlag = (
  //    sffDontSearchInBasePath, // do not search in BasePath, search only in SearchPath.
  //    sffSearchLoUpCase
  //    );
  //  TSearchFileInPathFlags = set of TSearchFileInPathFlag;

  //function SearchFileInPath(const Filename, BasePath, SearchPath,
  //  Delimiter: string; Flags: TSearchFileInPathFlags): string; overload;
  //function SearchAllFilesInPath(const Filename, BasePath, SearchPath,
  //  Delimiter: string; Flags: TSearchFileInPathFlags): TStrings;
  //function FindDiskFilename(const Filename: string): string;
  //function FindDiskFileCaseInsensitive(const Filename: string): string;
  //function FindDefaultExecutablePath(const Executable: string; const BaseDir: string = ''): string;

  //type

  //  { TFileIterator }

  //  TFileIterator = class
  //  private
  //    FPath: String;
  //    FLevel: Integer;
  //    FFileInfo: TSearchRec;
  //    FSearching: Boolean;
  //    function GetFileName: String;
  //  public
  //    procedure Stop;
  //    function IsDirectory: Boolean;
  //  public
  //    property FileName: String read GetFileName;
  //    property FileInfo: TSearchRec read FFileInfo;
  //    property Level: Integer read FLevel;
  //    property Path: String read FPath;
  //    property Searching: Boolean read FSearching;
  //  end;

  //  TFileFoundEvent = procedure (FileIterator: TFileIterator) of object;
  //  TDirectoryFoundEvent = procedure (FileIterator: TFileIterator) of object;
  //  TDirectoryEnterEvent = procedure (FileIterator: TFileIterator) of object;

  //  { TFileSearcher }

  //  TFileSearcher = class(TFileIterator)
  //  private
  //    FMaskSeparator: char;
  //    FFollowSymLink: Boolean;
  //    FOnFileFound: TFileFoundEvent;
  //    FOnDirectoryFound: TDirectoryFoundEvent;
  //    FOnDirectoryEnter: TDirectoryEnterEvent;
  //    FFileAttribute: Word;
  //    FDirectoryAttribute: Word;
  //    procedure RaiseSearchingError;
  //  protected
  //    procedure DoDirectoryEnter; virtual;
  //    procedure DoDirectoryFound; virtual;
  //    procedure DoFileFound; virtual;
  //  public
  //    constructor Create;
  //    procedure Search(ASearchPath: String; ASearchMask: String = '';
  //      ASearchSubDirs: Boolean = True; CaseSensitive: Boolean = False);
  //  public
  //    property MaskSeparator: char read FMaskSeparator write FMaskSeparator;
  //    property FollowSymLink: Boolean read FFollowSymLink write FFollowSymLink;
  //    property FileAttribute: Word read FFileAttribute write FFileAttribute default faAnyfile;
  //    property DirectoryAttribute: Word read FDirectoryAttribute write FDirectoryAttribute default faDirectory;
  //    property OnDirectoryFound: TDirectoryFoundEvent read FOnDirectoryFound write FOnDirectoryFound;
  //    property OnFileFound: TFileFoundEvent read FOnFileFound write FOnFileFound;
  //    property OnDirectoryEnter: TDirectoryEnterEvent read FOnDirectoryEnter write FOnDirectoryEnter;
  //  end;

  //  { TListFileSearcher }

  //  TListFileSearcher = class(TFileSearcher)
  //  private
  //    FList: TStrings;
  //  protected
  //    procedure DoFileFound; override;
  //  public
  //    constructor Create(AList: TStrings);
  //  end;

  //  { TListDirectoriesSearcher }

  //  TListDirectoriesSearcher = class(TFileSearcher)
  //  private
  //    FDirectoriesList :TStrings;
  //  protected
  //    procedure DoDirectoryFound; override;
  //  public
  //    constructor Create(AList: TStrings);
  //  end;


  //// flags for copy
  //type
  //  TCopyFileFlag = (
  //    cffOverwriteFile,
  //    cffCreateDestDirectory,
  //    cffPreserveTime
  //    );
  //  TCopyFileFlags = set of TCopyFileFlag;

  //// Copy a file and a whole directory tree
  //function CopyFile(const SrcFilename, DestFilename: string;
  //                  Flags: TCopyFileFlags=[cffOverwriteFile]; ExceptionOnError: Boolean=False): boolean;
  //function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean; ExceptionOnError: Boolean=False): boolean;
  //function CopyDirTree(const SourceDir, TargetDir: string; Flags: TCopyFileFlags=[]): Boolean;

  //// filename parts
  //const
  //  PascalFileExt: array[1..3] of string = ('.pas','.pp','.p');
  //  PascalSourceExt: array[1..6] of string = ('.pas','.pp','.p','.lpr','.dpr','.dpk');

  //  AllDirectoryEntriesMask = '*';

end;

procedure RIRegister_FPCFileUtil_Routines(S: TPSExec);
begin

  // File names
  S.RegisterDelphiFunction(@CompareFilenames, 'CompareFileNames', cdRegister);
  S.RegisterDelphiFunction(@ExtractFileNameOnly, 'ExtractFileNameOnly',
    cdRegister);
  S.RegisterDelphiFunction(@ExtractFileNameWithoutExt,
    'ExtractFileNameWithoutExt', cdRegister);

  // File operations
  S.RegisterDelphiFunction(@CreateDirUTF8, 'CreateDir', cdRegister);
  S.RegisterDelphiFunction(@DeleteFileUTF8, 'DeleteFile', cdRegister);
  S.RegisterDelphiFunction(@DirectoryExistsUTF8, 'DirectoryExists',
    cdRegister);
  S.RegisterDelphiFunction(@FileExistsUTF8, 'FileExists', cdRegister);
  S.RegisterDelphiFunction(@ForceDirectoriesUTF8, 'ForceDirectories',
    cdRegister);
  S.RegisterDelphiFunction(@RemoveDirUTF8, 'RemoveDir', cdRegister);
  S.RegisterDelphiFunction(@RenameFileUTF8, 'RenameFile', cdRegister);


  // File searching
  S.RegisterDelphiFunction(@CHXFindAllFiles, 'FindAllFiles', cdRegister);
  S.RegisterDelphiFunction(@CHXFindAllDirectories,
    'FindAllDirectories', cdRegister);

  //S.RegisterDelphiFunction(@FindFirstUTF8, 'FindFirst', cdRegister);
  //S.RegisterDelphiFunction(@FindNextUTF8, 'FindNext', cdRegister);
  //S.RegisterDelphiFunction(@FindCloseUTF8, 'FindClose', cdRegister);


  //S.RegisterDelphiFunction(@CRC32FileInt, 'CRC32FileInt', cdRegister);
  //S.RegisterDelphiFunction(@CRC32FileStr, 'CRC32FileStr', cdRegister);
  //S.RegisterDelphiFunction(@SHA1FileStr, 'SHA1FileStr',  cdRegister);
  ////S.RegisterDelphiFunction(@StringToSHA1Digest, 'StringToSHA1Digest', cdRegister);

  ////S.RegisterDelphiFunction(@IterateFolderObj, 'IterateFolderObj', cdRegister);
  ////S.RegisterDelphiFunction(@IterateFolderFun, 'IterateFolderFun', cdRegister);

  //S.RegisterDelphiFunction(@FilesInFolder, 'FilesInFolder', cdRegister);

end;

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
