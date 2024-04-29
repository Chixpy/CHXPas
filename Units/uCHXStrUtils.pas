unit uCHXStrUtils;

{< Unit with some string related functions.

  (C) 2011-2024 Chixpy https://github.com/Chixpy
}
{$DEBUGINFO OFF}

interface

uses Classes, SysUtils, LazFileUtils, LazUTF8, StrUtils,
  // CHX units
  uCHXConst, uCHXRscStr;


// STRING UTILS
// ------------
function UTF8TextReplace(const S, OldPattern, NewPattern: string;
  const ALanguage: string = ''): string;
{< A little shortcut of UTF8StringReplace, replacing all ocurrences and
     case-insesitive.
}

function SimpleStringSplit(aString, aDelimiter: string;
  var aStr1, aStr2: string): integer;
{< Splits a string into aStr1 and aStr2; aDelimiter is deleted.

  @returns(Returns the position where aDelimiter is found.)
}

function RemoveFromBrackets(const aString: string): string;
{< Removes text from the first ' (' o ' [' found in the aString. }
function CopyFromBrackets(const aString: string): string;
{< Copy text from the first ' (' o ' [' found in the aString. }

function TextSimilarityDice(const aString1, aString2: string): byte;
{< Returns the similarity between 2 strings.

  Actually based on http://www.catalysoft.com/articles/StrikeAMatch.html method
    tweaked a little. Wich is very similar to Dice's algorithm.

  TODO. Must be tweaked a little more, don't work well with symbols.
}
function TextSimilarityCHX(const aString1, aString2: string): byte;
{< Returns the similarity between 2 strings.

   Improvised to make anything faster or better.
}

// DIRECTORY NAME UTILS
// --------------------
function SetAsFolder(const aValue: string): string;
{< Adds PathDelim at the end of string and changes them to '/'

  IncludeTrailingPathDelimiter includes the PathDelim even if aValue = ''.
    This function non't add it in this case, so when testing if it's empty
    we don't need test @code (@(aFolder=''@) or @(aFolder=PathDelim@))

  In the other hand, paths are converted to Linux one as Windows AND
    MS-DOS @(+2.0@) can recognize them without problem.
}
function SysPath(const aPath: string): string;
{< Changes path delimiters to system ones... Some Lazarus function don't work}
function WinPath(const aPath: string): string;
function UnixPath(const aPath: string): string;

// FILENAME UTILS
// ---------------
function CleanFileName(const AFileName: string; const DoTrim: boolean = True;
  const PathAware: boolean = False): string;
{< Changes some invalid characters in filenames.

  @param(DoTrim Trim spaces at beggining and end, preventing filenames beginning
    with space. Contiguous space)
  @param(PathAware Keep special characters of paths (:\/))
}
function SetAsRelativeFile(const aFileName, BaseDir: string): string;
function SetAsAbsoluteFile(const aFileName, BaseDir: string): string;

function SetAsFile(const aFileName: string): string;
{< Paths are converted to Linux one as Windows AND
  MS-DOS (+2.0) can recognise them without problem.
}

function SupportedExtCT(const aFilename, aExtCT: string): boolean;
function SupportedExtSL(aFilename: string; aExt: TStrings): boolean;
{< Search if a file is in a list of supported extensions.

  aExt has a list of extensions (with or without dot).
}

// TSTRINGLIST UTILS
// ----------------------

procedure CleanStringList(aStringList: TStrings;
  const CommentChar: string = ';');
{< Removes comments and empty lines from a TStringList.
}

function AddToStringList(aList: TStrings; aString: string): integer;
{< Add a String to a StringList.

  Don't add repeated or empty strings.

  Remember that you can use for TStringList:
    aTStringList.Duplicates := dupIgnore
    aTStringList.AddStrings(aTStrings)

  But TStrings don't have TStrings.Duplicates;
}

procedure StringToFile(const aString, aFilename: string);
{< Saves a string to a File, lines separated with sLineBreak.
}

function FileMaskFromStringList(aList: TStrings): string;
{< Creates a file mask from a TStrings with an extension by line}
function FileMaskFromCommaText(const aText: string): string;
{< Creates a file mask from a string with extension separated by a comma }

// MISC
// ----

procedure StandardFormatSettings;
{< Standarizes the format settings.

   Using local settings can cause errors reading data from a file generated
   with a computer with different local settings
}

function StrCount(aString, ToSearch: string;
  const CaseSensitve: boolean = False): cardinal;
{< Counts the times that a substring is in a string.

  NOTE: StrCount('ooo', 'oo') = 2 .
}

function StrToCardinal(const aString: string): cardinal;

function StrToCardinalDef(const aString: string;
  const Default: cardinal): cardinal;

function SecondsToFmtStr(aValue: int64): string;


implementation

function UTF8TextReplace(const S, OldPattern, NewPattern: string;
  const ALanguage: string): string;
begin
  Result := UTF8StringReplace(S, OldPattern, NewPattern,
    [rfReplaceAll, rfIgnoreCase], ALanguage);
end;

function SimpleStringSplit(aString, aDelimiter: string;
  var aStr1, aStr2: string): integer;
begin
  Result := Pos(aDelimiter, aString);

  if Result < 1 then
  begin
    aStr1 := aString;
    aStr2 := EmptyStr;
    Exit;
  end;

  aStr1 := Copy(aString, 1, Result - 1);
  aStr2 := Copy(aString, Result + Length(aDelimiter), MaxInt);
end;


function RemoveFromBrackets(const aString: string): string;
var
  Pos1, Pos2: integer;
begin
  Pos1 := UTF8Pos(' (', aString);
  Pos2 := UTF8Pos(' [', aString);

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
  Pos1 := UTF8Pos(' (', aString);
  Pos2 := UTF8Pos(' [', aString);

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

// STRING UTILS
// ------------
function TextSimilarityDice(const aString1, aString2: string): byte;

  procedure LetterPairs(aStrList: TStrings; const aString: string);
  var
    i: integer;
    CurrPair: string;
    CharUTF8: string;
  begin
    if not Assigned(aStrList) then
      aStrList := TStringList.Create
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
  StrList1, StrList2: TStringList;
  CurrPair: string;
  i, j: integer;
  Intersection: integer;
  Union: integer;
begin
  Result := 0;
  if (aString1 = EmptyStr) or (aString2 = EmptyStr) then
    Exit;

  StrList1 := TStringList.Create;
  StrList2 := TStringList.Create;
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

function TextSimilarityCHX(const aString1, aString2 : string) : byte;
var
  Diff : array[0..31] of LongInt = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  aByte: Byte;
  i, Sum : LongInt;
begin
  Result := 0;


  for i := aString1.Length downto 1 do
  begin
    aByte := Byte(aString1[i]) and Byte(%00011111);
    Diff[aByte] := Diff[aByte] + 1;
  end;

  for i := aString2.Length downto 1 do
  begin
    aByte := Byte(aString1[i]) and Byte(%00011111);
    Diff[aByte] := Diff[aByte] - 1;
  end;

  i := Length(Diff) - 1;
  Sum := 0;
  while i >= 0 do
  begin
    Sum += Abs(Diff[i]);
    Dec(i);
  end;

  Result := Round(100 - ((100 * Sum) / (aString1.Length + aString2.Length)));
end;

// DIRECTORY NAME UTILS
// --------------------
function SetAsFolder(const aValue: string): string;
begin
  Result := aValue;

  Result := ExcludeTrailingPathDelimiter(Result);

  // HACK:
  //   Windows have problems removing folders ended with a dot...
  if Utf8EndsText('.', Result) then
    Result[UTF8LengthFast(Result)] := '_';

  { Always with TrailingPathDelimiter, but only if it's not empty or root }
  if Result <> EmptyStr then
    Result := IncludeTrailingPathDelimiter(Result);

  // I like UNIX PathSep :-) (and it's better for cross-configuring)
  Result := SetAsFile(Result);
end;

function SetAsRelativeFile(const aFileName, BaseDir: string): string;
begin
  // CreateRelativeSearchPath don't work with already relative paths
  //   CreateRelativeSearchPath('a\b', 'a\') returns 'a\b' instead 'b'.
  // CreateRelativePath doesn't like Unix Style under Windows... :-(
  Result := CreateRelativePath(SysPath(aFileName), SysPath(BaseDir));

  Result := SetAsFile(Result);
end;

function SetAsAbsoluteFile(const aFileName, BaseDir: string): string;
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

function SupportedExtCT(const aFilename, aExtCT: string): boolean;
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
  if aFilename = EmptyStr then
    Exit;

  // Extract extension, remove dot.
  TempExt := ExtractFileExt(aFilename);
  if Length(TempExt) > 1 then
    aFilename := Copy(TempExt, 2, MaxInt)
  else
    Exit;

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
  Result := UTF8TextReplace(Result, '*', '·');
  Result := UTF8TextReplace(Result, '"', '''');
  Result := UTF8TextReplace(Result, '|', '$');
  Result := UTF8TextReplace(Result, '<', '{');
  Result := UTF8TextReplace(Result, '>', '}');

  if PathAware then
  begin
    // C:\, ftp://, etc.
    Result := UTF8TextReplace(Result, ':\', '>'); // Temp '>' already replaced
    Result := UTF8TextReplace(Result, ':/', '<'); // Temp '<' already replaced
    Result := UTF8TextReplace(Result, ':', ' - ');
    Result := UTF8TextReplace(Result, '>', ':\');
    Result := UTF8TextReplace(Result, '<', ':/');
  end
  else
  begin
    Result := UTF8TextReplace(Result, '\', '&');
    Result := UTF8TextReplace(Result, '/', '%');
    Result := UTF8TextReplace(Result, ':', ' - ');
  end;

  if DoTrim then
  begin
    while UTF8Pos('  ', Result) <> 0 do
      Result := UTF8TextReplace(Result, '  ', ' ');

    while UTF8Pos(' .', Result) <> 0 do
       Result := UTF8TextReplace(Result, ' .', '.');
    // "A. file.ext" <- Keep this space...
    // while UTF8Pos('. ', Result) <> 0 do
    //   Result := UTF8TextReplace(Result, '. ', '.');

   if PathAware then // Faster that replacing directly
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

    Result := Trim(Result);
  end;
end;

// UTILIDADES TSTRINGLIST
// ----------------------

procedure CleanStringList(aStringList: TStrings; const CommentChar: string);
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
    if aStringList.Strings[Cont] = EmptyStr then
      aStringList.Delete(Cont);
  end;
end;

function AddToStringList(aList: TStrings; aString: string): integer;
begin
  Result := -1;
  aString := Trim(aString);
  if (not Assigned(aList)) or (aString = EmptyStr) then
    Exit;
  Result := aList.IndexOf(aString);
  if Result < 0 then
    Result := aList.Add(aString);
end;

procedure StringToFile(const aString, aFilename: string);
var
  slOutput: TStringList;
begin
  slOutput := TStringList.Create;
  try
    slOutput.Text := aString;
    slOutput.SaveToFile(UTF8ToSys(aFilename));
  finally
    slOutput.Free;
  end;
end;

function FileMaskFromStringList(aList: TStrings): string;
begin
  Result := EmptyStr;
  if not assigned(aList) then
    Exit;

  Result := aList.CommaText;
  Result := FileMaskFromCommaText(Result);
end;

function FileMaskFromCommaText(const aText: string): string;
begin
  Result := EmptyStr;
  if aText = EmptyStr then
    Exit;

  Result := UTF8TextReplace(aText, '"', EmptyStr);
  if Result = EmptyStr then
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

  DefaultFormatSettings.CurrencyFormat := 1;  // 1€
  DefaultFormatSettings.NegCurrFormat := 5;   // -1€
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.CurrencyDecimals := 2;
  DefaultFormatSettings.DateSeparator := '/';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ListSeparator := ';';
  DefaultFormatSettings.CurrencyString := '$';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.LongDateFormat := 'yyyy" "mmmm" "dd';
  DefaultFormatSettings.TimeAMString := 'AM';
  DefaultFormatSettings.TimePMString := 'PM';
  DefaultFormatSettings.ShortTimeFormat := 'hh:nn';
  DefaultFormatSettings.LongTimeFormat := 'hh:nn:ss';
  //DefaultFormatSettings.ShortMonthNames :=
  //  ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
  //  'Oct', 'Nov', 'Dec');
  //DefaultFormatSettings.LongMonthNames :=
  //  ('January', 'February', 'March', 'April', 'May', 'June',
  //  'July', 'August', 'September', 'October', 'November', 'December');
  //DefaultFormatSettings.ShortDayNames :=
  //  ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  //DefaultFormatSettings.LongDayNames :=
  //  ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday',
  //  'Friday', 'Saturday');
  DefaultFormatSettings.TwoDigitYearCenturyWindow := 50;
end;

function StrCount(aString, ToSearch: string;
  const CaseSensitve: boolean): cardinal;
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
