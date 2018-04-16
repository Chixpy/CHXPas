{ Copyright (C) 2011-2018 Chixpy

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
{ Unit with some string related functions. }
unit uCHXStrUtils;

{$DEBUGINFO OFF}

interface

uses Classes, SysUtils, LazFileUtils, LazUTF8, LazUTF8Classes,
  uCHXConst, uCHXRscStr;


// STRING UTILS
// ------------
function UTF8TextReplace(const S, OldPattern, NewPattern: string;
  ALanguage: string = ''): string;
{< Until it is not in LazUTF8...}

function RemoveFromBrackets(const aString: string): string;
{< Removes text from the first '(' o '[' found in the aString. }
function CopyFromBrackets(const aString: string): string;
{< Copy text from the first '(' o '[' found in the aString. }

function TextSimilarity(const aString1, aString2: string): byte;
{< Returns the similarity between 2 strings.

  Based on http://www.catalysoft.com/articles/StrikeAMatch.html method tweaked
    a little.
}

// DIRECTORY NAME UTILS
// --------------------
function SetAsFolder(const aValue: string): string;
{< Adds PathDelim at the end of string and changes them to '/'

  IncludeTrailingPathDelimiter includes the PathDelim even if aValue = ''.
    This function non't add it in this case, so when testing if it's empty
    we don't need test @code (@(aFolder=''@) or @(aFolder=PathDelim@))

  In the other hand, paths are converted to Linux one as Windows AND
    MS-DOS (+2.0) can recognise them without problem.
}
function SysPath(const aPath: string): string;
function WinPath(const aPath: string): string;
function UnixPath(const aPath: string): string;

// FILENAME UTILS
// ---------------
function CleanFileName(const AFileName: string; const DoTrim: boolean = True;
  const PathAware: boolean = False): string;
{< Changes some invalid characters in filenames.

  @param(DoTrim Trim spaces at beggining and end, preventing filenames beggining
    with space.)
    @param(PathAware Keep paths)
}
function SetAsRelativeFile(const aFileName: string; BaseDir: string): string;
function SetAsAbsoluteFile(const aFileName: string; BaseDir: string): string;

function SetAsFile(const aFileName: string): string;
{< Paths are converted to Linux one as Windows AND
  MS-DOS (+2.0) can recognise them without problem.
}

function SupportedExtCT(aFilename: string; aExtCT: string): boolean;
function SupportedExtSL(aFilename: string; aExt: TStrings): boolean;
{< Search if a file is in a list of supported extensions.

  aExt has a list of extensions (with or without dot).
}

// TSTRINGLIST UTILS
// ----------------------

procedure CleanStringList(aStringList: TStrings; CommentChar: string = ';');
{< Removes comments and empty lines from a TStringList.
}

function AddToStringList(aList: TStrings; aString: string): integer;
{< Add a String to a StringList.

  Don't add repeated or empty strings.

  Remember that you can use:
    aTStrings.Duplicates := dupIgnore
    aTStrings.AddStrings(aTStrings)
}

function FileMaskFromStringList(aList: TStrings): string;
{< Creates a file mask from a TStrings with an extension by line}
function FileMaskFromCommaText(aText: string): string;
{< Creates a file mask from a string with extension separated by a comma }

// MISC
// ----

procedure StandardFormatSettings;
{< Standarizes the format settings.

   Using local settings can cause errors reading data from a file generated
   with a computer with different local settings

}

function StrCount(aString, ToSearch: string;
  CaseSensitve: boolean = False): cardinal;
{< Counts the times that a substring is in a string.

  NOTE: StrCount('ooo', 'oo') = 2 .
}

function StrToCardinal(const aString: string): cardinal;

function StrToCardinalDef(const aString: string;
  const Default: cardinal): cardinal;

function SecondsToFmtStr(aValue: int64): string;


implementation

function UTF8TextReplace(const S, OldPattern, NewPattern: string;
  ALanguage: string): string;
begin
  Result := UTF8StringReplace(S, OldPattern, NewPattern,
    [rfReplaceAll, rfIgnoreCase], ALanguage);
end;

// STRING UTILS
// ------------
function RemoveFromBrackets(const aString: string): string;
var
  Pos1, Pos2: integer;
begin
  Pos1 := UTF8Pos('(', aString);
  Pos2 := UTF8Pos('[', aString);

  // if not found...
  if Pos1 < 1 then
    Pos1 := MaxInt;
  if Pos2 < 1 then
    Pos2 := MaxInt;

  if Pos1 < Pos2 then
    Result := UTF8Trim(UTF8Copy(aString, 1, Pos1 - 1))
  else
    Result := UTF8Trim(UTF8Copy(aString, 1, Pos2 - 1));
end;

function CopyFromBrackets(const aString: string): string;
var
  Pos1, Pos2: integer;
begin
  Pos1 := UTF8Pos('(', aString);
  Pos2 := UTF8Pos('[', aString);

  // if not found...
  if Pos1 < 1 then
    Pos1 := MaxInt;
  if Pos2 < 1 then
    Pos2 := MaxInt;

  if Pos1 < Pos2 then
    Result := UTF8Trim(UTF8Copy(aString, Pos1, MaxInt))
  else
    Result := UTF8Trim(UTF8Copy(aString, Pos2, MaxInt));
end;

function TextSimilarity(const aString1, aString2: string): byte;

  procedure LetterPairs(aStrList: TStrings; const aString: string);
  var
    i: integer;
    CurrPair: string;
    CharUTF8: string;
  begin
    if not Assigned(aStrList) then
      aStrList := TStringListUTF8.Create
    else
      aStrList.Clear;

    i := 1;
    while i < UTF8Length(aString) do
    begin
      CurrPair := UTF8Copy(aString, i, 2);

      if UTF8Length(CurrPair) <> 2 then
      begin
        Inc(i);
        Continue;
      end;

      // Removing some separators...
      if CurrPair[1] in kCUUTF8Delimiters then
      begin
        Inc(i);
        Continue;
      end;

      CharUTF8 := UTF8Copy(CurrPair, 2, 1);
      if CharUTF8[1] in kCUUTF8Delimiters then
      begin
        CurrPair := UTF8Copy(CurrPair, 1, 1);
      end;

      aStrList.Add(CurrPair);
      Inc(i, Length(CurrPair));
    end;
  end;

var
  StrList1, StrList2: TStringListUTF8;
  CurrPair: string;
  i, j: integer;
  Intersection: integer;
  Union: integer;
begin
  Result := 0;
  if (aString1 = '') or (aString2 = '') then
    Exit;

  StrList1 := TStringListUTF8.Create;
  StrList2 := TStringListUTF8.Create;
  StrList1.CaseSensitive := False;
  StrList2.CaseSensitive := False;
  try
    LetterPairs(StrList1, UTF8UpperCase(aString1));
    StrList1.Sort;
    LetterPairs(StrList2, UTF8UpperCase(aString2));
    StrList2.Sort;

    Intersection := 0;
    Union := StrList1.Count + StrList2.Count;

    i := StrList1.Count - 1;
    while i >= 0 do
    begin
      CurrPair := StrList1[i];
      j := StrList2.IndexOf(CurrPair);
      if j <> -1 then
      begin
        StrList2.Delete(j);
        Inc(Intersection, 2);
      end;
      Dec(i);
    end;
  finally
    FreeAndNil(StrList1);
    FreeAndNil(StrList2);
  end;

  if Union <> 0 then
    Result := Round(Intersection / Union * 100);
end;

// DIRECTORY NAME UTILS
// --------------------
function SetAsFolder(const aValue: string): string;
begin
  Result := aValue;

  Result := ExcludeTrailingPathDelimiter(Result);

  { Always with TrailingPathDelimiter, but only if it's not empty or root }
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);

  // I like UNIX PathSep :-) (and it's better for cross-configuring)
  Result := SetAsFile(Result);
end;

function SetAsRelativeFile(const aFileName: string; BaseDir: string): string;
begin
  // CreateRelativePath doesn't like Unix Style under Windows... :-(
  Result := CreateRelativeSearchPath(SysPath(aFileName), SysPath(BaseDir));

  Result := SetAsFile(Result);
end;

function SetAsAbsoluteFile(const aFileName: string; BaseDir: string): string;
var
  IsFolder: boolean;
begin
  IsFolder := False;
  if Length(aFileName) > 0 then
    IsFolder := aFileName[Length(aFileName)] in AllowDirectorySeparators;

  // CreateAbsoluteSearchPath doesn't like Unix Style under Windows... :-(
  Result := CreateAbsoluteSearchPath(SysPath(aFileName), SysPath(BaseDir));
  Result := TrimAndExpandFilename(Result);

  if IsFolder then
    Result := SetAsFolder(Result)
  else
    Result := SetAsFile(Result);
end;

function SetAsFile(const aFileName: string): string;
begin
  Result := UnixPath(aFileName);
end;

function SupportedExtCT(aFilename: string; aExtCT: string): boolean;
var
  aTempSL: TStringList;
begin
  aTempSL := TStringList.Create;
  try
    aTempSL.CommaText := aExtCT;
    Result := SupportedExtSL(aFilename, aTempSL);
  finally
    aTempSL.Free;
  end;
end;

function SupportedExtSL(aFilename: string; aExt: TStrings): boolean;
var
  i: integer;
  TempExt: string;
begin
  Result := False;
  if (not assigned(aExt)) or (aExt.Count = 0) then
    Exit;
  if aFilename = '' then
    Exit;

  // Extract extension, remove dot.
  aFilename := copy(ExtractFileExt(aFilename), 2, MaxInt);

  i := 0;
  while (i < aExt.Count) and (not Result) do
  begin
    TempExt := aExt[i];
    if (Length(TempExt) > 0) then // Take care about no extension
      if TempExt[1] = ExtensionSeparator then // remove dot
        TempExt := copy(TempExt, 2, MaxInt);

    Result := UTF8CompareText(aFilename, TempExt) = 0;
    Inc(i);
  end;
end;

function SysPath(const aPath: string): string;
begin
  {$IFDEF Windows}
  Result := WinPath(aPath);
  {$ELSE}
  Result := UnixPath(aPath);
  {$ENDIF}
end;

function WinPath(const aPath: string): string;
var
  i: integer;
begin
  // Seems to be faster than UTF8TextReplace...
  Result := aPath;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] = kLinuxDirSeparator then
      Result[i] := kWinDirSeparator;
    Dec(i);
  end;
end;

function UnixPath(const aPath: string): string;
var
  i: integer;
begin
  // Seems to be faster than UTF8TextReplace...
  Result := aPath;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] = kWinDirSeparator then
      Result[i] := kLinuxDirSeparator;
    Dec(i);
  end;
end;

// FILE NAME UTILS
// ---------------
function CleanFileName(const AFileName: string; const DoTrim: boolean = True;
  const PathAware: boolean = False): string;
begin

  // Windows (and Linux) invalid characters
  Result := UTF8TextReplace(AFileName, '?', '_');
  Result := UTF8TextReplace(Result, '*', 'ª');
  Result := UTF8TextReplace(Result, '"', '·');
  Result := UTF8TextReplace(Result, '|', '$');
  Result := UTF8TextReplace(Result, '<', '{');
  Result := UTF8TextReplace(Result, '>', '}');

  if not PathAware then
  begin
    Result := UTF8TextReplace(Result, '\', '&');
    Result := UTF8TextReplace(Result, '/', '%');
  end;

  if (length(Result) > 1) and (Result[2] = ':') and (PathAware) then
  begin
    // C:\...
    Result := UTF8TextReplace(Result, ':', '*'); // '*' allready replaced
    Result[2] := ':';
    Result := UTF8TextReplace(Result, '*', ' - ');
  end
  else
    Result := UTF8TextReplace(Result, ':', ' - ');

  if DoTrim then
  begin
    while UTF8Pos('  ', Result) <> 0 do
      Result := UTF8TextReplace(Result, '  ', ' ');
    Result := Trim(Result);
    if PathAware then
    begin
      while UTF8Pos(' \', Result) <> 0 do
        Result := UTF8TextReplace(Result, ' \', '\');
      while UTF8Pos('\ ', Result) <> 0 do
        Result := UTF8TextReplace(Result, '\ ', '\');
      while UTF8Pos(' /', Result) <> 0 do
        Result := UTF8TextReplace(Result, ' /', '/');
      while UTF8Pos('/ ', Result) <> 0 do
        Result := UTF8TextReplace(Result, '/ ', '/');
    end;
  end;
end;

// UTILIDADES TSTRINGLIST
// ----------------------

procedure CleanStringList(aStringList: TStrings; CommentChar: string = ';');
var
  Cont: cardinal;
begin
  if not Assigned(aStringList) then
    Exit;

  for Cont := aStringList.Count - 1 downto 0 do
  begin
    if Pos(CommentChar, aStringList.Strings[Cont]) <> 0 then
      aStringList.Strings[Cont] :=
        Copy(aStringList.Strings[Cont], 1, Pos(CommentChar,
        aStringList.Strings[Cont]) - 1);
    aStringList.Strings[Cont] := Trim(aStringList.Strings[Cont]);
    if aStringList.Strings[Cont] = '' then
      aStringList.Delete(Cont);
  end;
end;

function AddToStringList(aList: TStrings; aString: string): integer;
begin
  Result := -1;
  aString := Trim(aString);
  if (not Assigned(aList)) or (aString = '') then
    Exit;
  Result := aList.IndexOf(aString);
  if Result = -1 then
    Result := aList.Add(aString);
end;

function FileMaskFromStringList(aList: TStrings): string;
begin
  Result := '';
  if not assigned(aList) then
    Exit;

  Result := aList.CommaText;
  Result := FileMaskFromCommaText(Result);
end;

function FileMaskFromCommaText(aText: string): string;
begin
  Result := '';
  if aText = '' then
    Exit;

  Result := UTF8TextReplace(aText, '"', '');
  if Result = '' then
    Exit;

  // Dots...
  if Result[1] = '.' then
    Result := Copy(Result, 2, Length(Result));
  Result := UTF8TextReplace(Result, ',.', ',');

  Result := '*.' + UTF8TextReplace(Result, ',', ';*.');
end;

// UTILIDADES VARIAS
// -----------------

procedure StandardFormatSettings;
begin
  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings that can cause errors
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.DateSeparator := '/';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.ShortTimeFormat := 'hh:nn:ss';
  DefaultFormatSettings.ListSeparator := ';';
end;

function StrCount(aString, ToSearch: string;
  CaseSensitve: boolean = False): cardinal;
var
  Cont: cardinal;
  TempCadena: string;
begin
  Result := 0;
  if not CaseSensitve then
  begin
    aString := AnsiUpperCase(aString);
    ToSearch := AnsiUpperCase(ToSearch);
  end;

  for Cont := 1 to Length(aString) do
  begin
    TempCadena := Copy(aString, Cont, Length(ToSearch));
    if TempCadena = ToSearch then
      Result := Result + 1;
  end;
end;

function StrToCardinalDef(const aString: string;
  const Default: cardinal): cardinal;
var
  h: int64;
begin
  h := StrToInt64Def(aString, Default);
  if (h > High(cardinal)) or (h < 0) then
    h := Default;
  Result := h;
end;

function SecondsToFmtStr(aValue: int64): string;
begin
  Result := RightStr('00' + IntToStr(aValue mod 60), 2);
  aValue := aValue div 60;
  Result := RightStr('00' + IntToStr(aValue mod 60), 2) + ':' + Result;
  aValue := aValue div 60;

  // days too.. :-P
  if aValue > 23 then
  begin
    Result := RightStr('00' + IntToStr(aValue mod 24), 2) + ':' + Result;
    aValue := aValue div 24;
    Result := IntToStr(aValue) + 'd ' + Result;
  end
  else
    Result := IntToStr(aValue) + ':' + Result;
end;

function StrToCardinal(const aString: string): cardinal;
var
  h: int64;
begin
  h := StrToInt64(aString);
  if (h > High(cardinal)) or (h < 0) then
    raise EConvertError.CreateFmt(rsCUExcCardRange, [h]);
  Result := h;
end;


end.
