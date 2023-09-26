unit uPSI_FPCStrUtils;
{< StrUtils unit for Pascal Script.

  Copyright (C) 2019-2020 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, uPSRuntime, uPSCompiler;

// uPSI_CHXBasic must be loaded first in Pascal Engine (SizeInt / SizeUInt)

// TODO: Fix AnsiString parameters, a helper function must be created.
//   See CHXRPos in implementation.

procedure SIRegister_FPCStrUtils(CL: TPSPascalCompiler);

procedure RIRegister_FPCStrUtils_Routines(S: TPSExec);

implementation
// Helper functions
function CHXLeftStr(const AText: string; const ACount: SizeInt): string;
begin
  Result := LeftStr(AText, ACount);
end;

function CHXRightStr(const AText: string; const ACount: SizeInt): string;
begin
  Result := RightStr(AText, ACount);
end;

function CHXMidStr(const AText: string; const AStart, ACount: SizeInt): string;
begin
  Result := MidStr(AText, AStart, ACount);
end;

function CHXRightBStr(const AText: string; const AByteCount: SizeInt): string;
begin
  Result := RightBStr(AText, AByteCount);
end;

function CHXMidBStr(const AText: string; const AByteStart, AByteCount: SizeInt): string;
begin
  Result := MidBStr(AText, AByteStart, AByteCount);
end;

function CHXAnsiLeftStr(const AText: string; const ACount: SizeInt): string;
begin
  Result := AnsiLeftStr(AText, ACount);
end;

function CHXAnsiRightStr(const AText: string; const ACount: SizeInt): string;
begin
  Result := AnsiRightStr(AText, ACount);
end;

function CHXAnsiMidStr(const AText: string; const AStart, ACount: SizeInt): string;
begin
  Result := AnsiMidStr(AText, AStart, ACount);
end;

function CHXLeftBStr(const AText: string; const AByteCount: SizeInt): string;
begin
  Result := LeftBStr(AText, AByteCount);
end;

function CHXRPos(const Substr, Source: string): SizeInt;
begin
  Result := RPos(Substr, Source);
end;

function CHXRPosEx(const Substr, Source: string; OffSet: SizeInt): SizeInt;
begin
  Result := RPosEX(Substr, Source, OffSet);
end;

procedure SIRegister_FPCStrUtils(CL: TPSPascalCompiler);
begin
  // Case insensitive search/replace

  CL.AddDelphiFunction(
    'function AnsiResemblesText(const AText, AOther: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiContainsText(const AText, ASubText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiStartsText(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiEndsText(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiReplaceText(const AText, AFromText, AToText: string): string');
  CL.AddDelphiFunction(
    'function AnsiMatchText(const AText: string; const AValues: array of string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiIndexText(const AText: string; const AValues: array of string): Integer');

  // Case sensitive search/replace

  CL.AddDelphiFunction(
    'function AnsiContainsStr(const AText, ASubText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiStartsStr(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiEndsStr(const ASubText, AText: string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiReplaceStr(const AText, AFromText, AToText: string): string');
  CL.AddDelphiFunction(
    'function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean');
  CL.AddDelphiFunction(
    'function AnsiIndexStr(const AText: string; const AValues: array of string): Integer');
  //CL.AddDelphiFunction(
  //  'function MatchStr(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean');
  //CL.AddDelphiFunction(
  //  'function IndexStr(const AText: UnicodeString; const AValues: array of UnicodeString): Integer');

  // Miscellaneous

  CL.AddDelphiFunction(
    'function DupeString(const AText: string; ACount: Integer): string;');
  CL.AddDelphiFunction(
    'function ReverseString(const AText: string): string;');
  //CL.AddDelphiFunction(
  //  'function AnsiReverseString(const AText: AnsiString): AnsiString;');
  CL.AddDelphiFunction(
    'function StuffString(const AText: string; AStart, ALength: Cardinal; const ASubText: string): string;');
  //CL.AddDelphiFunction(
  //  'function RandomFrom(const AValues: array of string): string;');
  CL.AddDelphiFunction(
    'function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string;');
  CL.AddDelphiFunction(
    'function NaturalCompareText (const S1, S2 : string): Integer;');
  //-CL.AddDelphiFunction(
  //-  'function NaturalCompareText(const Str1, Str2: string; const ADecSeparator, AThousandSeparator: Char): Integer;');

  // VB emulations.

  CL.AddDelphiFunction(
    'function LeftStr(const AText: string; const ACount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function RightStr(const AText: string; const ACount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function MidStr(const AText: string; const AStart, ACount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function RightBStr(const AText: string; const AByteCount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function MidBStr(const AText: string; const AByteStart, AByteCount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function AnsiLeftStr(const AText: string; const ACount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function AnsiRightStr(const AText: string; const ACount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function AnsiMidStr(const AText: string; const AStart, ACount: SizeInt): string;');
  CL.AddDelphiFunction(
    'function LeftBStr(const AText: string; const AByteCount: SizeInt): string;');
  //-CL.AddDelphiFunction(
  //-  'function LeftStr(const AText: WideString; const ACount: SizeInt): WideString;');
  //-CL.AddDelphiFunction(
  //-  'function RightStr(const AText: WideString; const ACount: SizeInt): WideString;');
  //-CL.AddDelphiFunction(
  //-  'function MidStr(const AText: WideString; const AStart, ACount: SizeInt): WideString;');

  // Extended search and replace

  //CL.AddConstantN('WordDelimiters', 'set of Char').SetSet(WordDelimiters);
  CL.AddConstantN('SErrAmountStrings', 'string').SetString(SErrAmountStrings);
  //type
  //TStringSearchOption = (soDown, soMatchCase, soWholeWord);
  //TStringSearchOptions = set of TStringSearchOption;
  //TStringSeachOption = TStringSearchOption;
  //CL.AddDelphiFunction(
  //  'function SearchBuf(Buf: PChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String; Options: TStringSearchOptions): PChar;');
  //CL.AddDelphiFunction(
  //  'function SearchBuf(Buf: PChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String): PChar;');
  CL.AddDelphiFunction(
    'function PosEx(const SubStr, S: string; Offset: SizeUint): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosEx(const SubStr, S: string): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosEx(c:char; const S: string; Offset: SizeUint): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosEx(const SubStr, S: UnicodeString; Offset: SizeUint): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosEx(c: WideChar; const S: UnicodeString; Offset: SizeUint): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosEx(const SubStr, S: UnicodeString): Sizeint;');
  //CL.AddDelphiFunction(
  //  'function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;');

  //  Delphi compat

  CL.AddDelphiFunction(
    'function ReplaceStr(const AText, AFromText, AToText: string): string;');
  CL.AddDelphiFunction(
    'function ReplaceText(const AText, AFromText, AToText: string): string;');

  // Soundex Functions.

  //CL.AddTypeS('TSoundexLength', '1..MaxInt');

  //CL.AddDelphiFunction(
  //  'function Soundex(const AText: string; ALength: TSoundexLength): string;');
  //CL.AddDelphiFunction(
  //  'function Soundex(const AText: string): string;');

  //CL.AddTypeS('TSoundexIntLength', '1..8');

  //CL.AddDelphiFunction(
  //  'function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;');
  CL.AddDelphiFunction(
    'function SoundexInt(const AText: string): Integer;');
  CL.AddDelphiFunction(
    'function DecodeSoundexInt(AValue: Integer): string;');
  CL.AddDelphiFunction(
    'function SoundexWord(const AText: string): Word;');
  CL.AddDelphiFunction(
    'function DecodeSoundexWord(AValue: Word): string;');
  //CL.AddDelphiFunction(
  //  'function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;');
  CL.AddDelphiFunction(
    'function SoundexSimilar(const AText, AOther: string): Boolean;');
  //CL.AddDelphiFunction(
  //  'function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;');
  CL.AddDelphiFunction(
    'function SoundexCompare(const AText, AOther: string): Integer;');
  CL.AddDelphiFunction(
    'function SoundexProc(const AText, AOther: string): Boolean;');

  //Type
  //  TCompareTextProc = Function(const AText, AOther: string): Boolean;
  //Const
  //  AnsiResemblesProc: TCompareTextProc = @SoundexProc;

  // Other functions, based on RxStrUtils.
  CL.AddTypeS('TRomanConversionStrictness',
    '(rcsStrict, rcsRelaxed, rcsDontCare)');

  CL.AddConstantN('SInvalidRomanNumeral', 'string').SetString(
    SInvalidRomanNumeral);

  //CL.AddDelphiFunction(
  //  'function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;');
  CL.AddDelphiFunction(
    'function DelSpace(const S: string): string;');
  //CL.AddDelphiFunction(
  //  'function DelChars(const S: string; Chr: Char): string;');
  CL.AddDelphiFunction(
    'function DelSpace1(const S: string): string;');
  CL.AddDelphiFunction(
    'function Tab2Space(const S: string; Numb: Byte): string;');
  CL.AddDelphiFunction(
    'function NPos(const C: string; S: string; N: Integer): SizeInt;');
  //-CL.AddDelphiFunction(
  //-  'function RPosEX(C:char;const S : AnsiString;offs:cardinal):SizeInt;');
  //-CL.AddDelphiFunction(
  //-  'function RPosex (Const Substr : AnsiString; Const Source : AnsiString;offs:cardinal) : SizeInt;');
  // CHX overload:
  CL.AddDelphiFunction(
    'function RPosEx(const Substr, Source: string; Offset: SizeUint): SizeInt;');
  //-CL.AddDelphiFunction(
  //-  'function RPos(c:char;const S : AnsiString):SizeInt;');
  //-CL.AddDelphiFunction(
  //-  'function RPos(const Substr: AnsiString; const Source: AnsiString): SizeInt;');
  // CHX overload:
  CL.AddDelphiFunction(
    'function RPos(const Substr: string; const Source: string): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function AddChar(C: Char; const S: string; N: Integer): string;');
  //CL.AddDelphiFunction(
  //  'function AddCharR(C: Char; const S: string; N: Integer): string;');
  CL.AddDelphiFunction(
    'function PadLeft(const S: string; N: Integer): string;');
  CL.AddDelphiFunction(
    'function PadRight(const S: string; N: Integer): string;');
  CL.AddDelphiFunction(
    'function PadCenter(const S: string; Len: SizeInt): string;');
  //CL.AddDelphiFunction(
  //  'function Copy2Symb(const S: string; Symb: Char): string;');
  //CL.AddDelphiFunction(
  //  'function Copy2SymbDel(var S: string; Symb: Char): string;');
  CL.AddDelphiFunction(
    'function Copy2Space(const S: string): string;');
  CL.AddDelphiFunction(
    'function Copy2SpaceDel(var S: string): string;');
  //CL.AddDelphiFunction(
  //  'function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;');
  //CL.AddDelphiFunction(
  //  'function WordCount(const S: string; const WordDelims: TSysCharSet): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): SizeInt;');
  //CL.AddDelphiFunction(
  //  'function ExtractWord(N: Integer; const S: string;  const WordDelims: TSysCharSet): string;');
  //{$IF SIZEOF(SIZEINT)<>SIZEOF(INTEGER)}
  //CL.AddDelphiFunction(
  //  'function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; out Pos: SizeInt): string;');
  //{$ENDIF}
  //CL.AddDelphiFunction(
  //  'function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; out Pos: Integer): string;');
  //CL.AddDelphiFunction(
  //  'function ExtractDelimited(N: Integer; const S: string;  const Delims: TSysCharSet): string;');
  //{$IF SIZEOF(SIZEINT)<>SIZEOF(INTEGER)}
  //CL.AddDelphiFunction(
  //  'function ExtractSubstr(const S: string; var Pos: SizeInt;  const Delims: TSysCharSet): string;');
  //{$ENDIF}
  //CL.AddDelphiFunction(
  //  'function ExtractSubstr(const S: string; var Pos: Integer;  const Delims: TSysCharSet): string;');
  //CL.AddDelphiFunction(
  //  'function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;');
  CL.AddDelphiFunction(
    'function FindPart(const HelpWilds, InputStr: string): SizeInt;');
  CL.AddDelphiFunction(
    'function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;');
  //CL.AddDelphiFunction(
  //  'function XorString(const Key, Src: ShortString): ShortString;');
  CL.AddDelphiFunction(
    'function XorEncode(const Key, Source: string): string;');
  CL.AddDelphiFunction(
    'function XorDecode(const Key, Source: string): string;');
  //CL.AddDelphiFunction(
  //  'function GetCmdLineArg(const Switch: string; SwitchChars: TSysCharSet): string;');
  CL.AddDelphiFunction(
    'function Numb2USA(const S: string): string;');
  CL.AddDelphiFunction(
    'function Hex2Dec(const S: string): Longint;');
  CL.AddDelphiFunction(
    'function Dec2Numb(N: Longint; Len, Base: Byte): string;');
  CL.AddDelphiFunction(
    'function Numb2Dec(S: string; Base: Byte): Longint;');
  CL.AddDelphiFunction(
    'function IntToBin(Value: Longint; Digits, Spaces: Integer): string;');
  //-CL.AddDelphiFunction(
  //-  'function IntToBin(Value: Longint; Digits: Integer): string;');
  //-CL.AddDelphiFunction(
  //-  'function intToBin(Value: int64; Digits:integer): string;');
  CL.AddDelphiFunction(
    'function IntToRoman(Value: Longint): string;');
  CL.AddDelphiFunction(
    'function TryRomanToInt(S: String; out N: LongInt; Strictness: TRomanConversionStrictness): Boolean;');
  CL.AddDelphiFunction(
    'function RomanToInt(const S: string; Strictness: TRomanConversionStrictness): Longint;');
  CL.AddDelphiFunction(
    'function RomanToIntDef(const S : String; const ADefault: Longint; Strictness: TRomanConversionStrictness): Longint;');
  //CL.AddDelphiFunction(
  //  'procedure BinToHex(BinValue, HexValue: PChar; BinBufSize: Integer);');
  //CL.AddDelphiFunction(
  //  'function HexToBin(HexValue, BinValue: PChar; BinBufSize: Integer): Integer;');

  //const
  //  DigitChars = ['0'..'9'];
  //  Brackets = ['(',')','[',']','{','}'];
  //  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;
  //  StdSwitchChars = ['-','/'];

  //CL.AddDelphiFunction(
  //  'function PosSet (const c:TSysCharSet;const s : ansistring ):SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosSet (const c:string;const s : ansistring ):SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosSetEx (const c:TSysCharSet;const s : ansistring;count:Integer ):SizeInt;');
  //CL.AddDelphiFunction(
  //  'function PosSetEx (const c:string;const s : ansistring;count:Integer ):SizeInt;');

  //CL.AddDelphiFunction(
  //  'procedure Removeleadingchars(VAR S : AnsiString; Const CSet:TSysCharset);');
  //CL.AddDelphiFunction(
  //  'procedure RemoveTrailingChars(VAR S : AnsiString;Const CSet:TSysCharset);');
  //CL.AddDelphiFunction(
  //  'procedure RemovePadChars(VAR S : AnsiString;Const CSet:TSysCharset);');

  //CL.AddDelphiFunction(
  //  'function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;');
  //CL.AddDelphiFunction(
  //  'function TrimRightSet(const S: String;const CSet:TSysCharSet): String;');
  //CL.AddDelphiFunction(
  //  'function TrimSet(const S: String;const CSet:TSysCharSet): String;');


  //type
  //  SizeIntArray = array of SizeInt;

  //CL.AddDelphiFunction(
  //  'procedure FindMatchesBoyerMooreCaseSensitive(const S,OldPattern: PChar; const SSize, OldPatternSize: SizeInt; out aMatches: SizeIntArray; const aMatchAll: Boolean);');
  //CL.AddDelphiFunction(
  //  'procedure FindMatchesBoyerMooreCaseSensitive(const S,OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean);');

  //CL.AddDelphiFunction(
  //  'procedure FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: PChar; const SSize, OldPatternSize: SizeInt; out aMatches: SizeIntArray; const aMatchAll: Boolean);');
  //CL.AddDelphiFunction(
  //  'procedure FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean);');

  CL.AddTypeS('TStringReplaceAlgorithm', '(sraDefault, sraManySmall, sraBoyerMoore)');

  //CL.AddDelphiFunction(
  //  'function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags; Algorithm : TStringReplaceAlgorithm): string;');
  //CL.AddDelphiFunction(
  //  'function StringReplace(const S, OldPattern, NewPattern: unicodestring; Flags: TReplaceFlags): unicodestring;');
  //CL.AddDelphiFunction(
  //  'function StringReplace(const S, OldPattern, NewPattern: widestring; Flags: TReplaceFlags): widestring;');

end;

procedure RIRegister_FPCStrUtils_Routines(S: TPSExec);
begin
  // Case insensitive search/replace

  S.RegisterDelphiFunction(@AnsiResemblesText, 'AnsiResemblesText',
    cdRegister);
  S.RegisterDelphiFunction(@AnsiContainsText, 'AnsiContainsText', cdRegister);
  S.RegisterDelphiFunction(@AnsiStartsText, 'AnsiStartsText', cdRegister);
  S.RegisterDelphiFunction(@AnsiEndsText, 'AnsiEndsText', cdRegister);
  S.RegisterDelphiFunction(@AnsiReplaceText, 'AnsiReplaceText', cdRegister);
  S.RegisterDelphiFunction(@AnsiMatchText, 'AnsiMatchText', cdRegister);
  S.RegisterDelphiFunction(@AnsiIndexText, 'AnsiIndexText', cdRegister);

  // Case sensitive search/replace

  S.RegisterDelphiFunction(@AnsiContainsStr, 'AnsiContainsStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiStartsStr, 'AnsiStartsStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiEndsStr, 'AnsiEndsStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiReplaceStr, 'AnsiReplaceStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiMatchStr, 'AnsiMatchStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiIndexStr, 'AnsiIndexStr', cdRegister);
  S.RegisterDelphiFunction(@MatchStr, 'MatchStr', cdRegister);
  S.RegisterDelphiFunction(@IndexStr, 'IndexStr', cdRegister);

  // Miscellaneous

  S.RegisterDelphiFunction(@DupeString, 'DupeString', cdRegister);
  S.RegisterDelphiFunction(@ReverseString, 'ReverseString', cdRegister);
  S.RegisterDelphiFunction(@AnsiReverseString, 'AnsiReverseString', cdRegister);
  S.RegisterDelphiFunction(@StuffString, 'StuffString', cdRegister);
  S.RegisterDelphiFunction(@RandomFrom, 'RandomFrom', cdRegister);
  S.RegisterDelphiFunction(@IfThen, 'IfThen', cdRegister);
  S.RegisterDelphiFunction(@NaturalCompareText, 'NaturalCompareText',
    cdRegister);

  // VB emulations.

  S.RegisterDelphiFunction(@CHXLeftStr, 'LeftStr', cdRegister);
  S.RegisterDelphiFunction(@CHXRightStr, 'RightStr', cdRegister);
  S.RegisterDelphiFunction(@CHXMidStr, 'MidStr', cdRegister);
  S.RegisterDelphiFunction(@CHXRightBStr, 'RightBStr', cdRegister);
  S.RegisterDelphiFunction(@CHXMidBStr, 'MidBStr', cdRegister);
  S.RegisterDelphiFunction(@CHXAnsiLeftStr, 'AnsiLeftStr', cdRegister);
  S.RegisterDelphiFunction(@CHXAnsiRightStr, 'AnsiRightStr', cdRegister);
  S.RegisterDelphiFunction(@CHXAnsiMidStr, 'AnsiMidStr', cdRegister);
  S.RegisterDelphiFunction(@CHXLeftBStr, 'LeftBStr', cdRegister);

  // Extended search and replace

  S.RegisterDelphiFunction(@SearchBuf, 'SearchBuf', cdRegister);
  S.RegisterDelphiFunction(@PosEx, 'PosEx', cdRegister);
  S.RegisterDelphiFunction(@StringsReplace, 'StringsReplace', cdRegister);

  //  Delphi compat

  S.RegisterDelphiFunction(@ReplaceStr, 'ReplaceStr', cdRegister);
  S.RegisterDelphiFunction(@ReplaceText, 'ReplaceText', cdRegister);

  // Soundex Functions.

  S.RegisterDelphiFunction(@Soundex, 'Soundex', cdRegister);
  S.RegisterDelphiFunction(@SoundexInt, 'SoundexInt', cdRegister);
  S.RegisterDelphiFunction(@DecodeSoundexInt, 'DecodeSoundexInt', cdRegister);
  S.RegisterDelphiFunction(@SoundexWord, 'SoundexWord', cdRegister);
  S.RegisterDelphiFunction(@DecodeSoundexWord, 'DecodeSoundexWord', cdRegister);
  S.RegisterDelphiFunction(@SoundexSimilar, 'SoundexSimilar', cdRegister);
  S.RegisterDelphiFunction(@SoundexCompare, 'SoundexCompare', cdRegister);
  S.RegisterDelphiFunction(@SoundexProc, 'SoundexProc', cdRegister);

  // Other functions, based on RxStrUtils.

  S.RegisterDelphiFunction(@IsEmptyStr, 'IsEmptyStr', cdRegister);
  S.RegisterDelphiFunction(@DelSpace, 'DelSpace', cdRegister);
  S.RegisterDelphiFunction(@DelChars, 'DelChars', cdRegister);
  S.RegisterDelphiFunction(@DelSpace1, 'DelSpace1', cdRegister);
  S.RegisterDelphiFunction(@Tab2Space, 'Tab2Space', cdRegister);
  S.RegisterDelphiFunction(@NPos, 'NPos', cdRegister);
  S.RegisterDelphiFunction(@CHXRPosEX, 'RPosEX', cdRegister);
  S.RegisterDelphiFunction(@CHXRPos, 'RPos', cdRegister);
  S.RegisterDelphiFunction(@AddChar, 'AddChar', cdRegister);
  S.RegisterDelphiFunction(@AddCharR, 'AddCharR', cdRegister);
  S.RegisterDelphiFunction(@PadLeft, 'PadLeft', cdRegister);
  S.RegisterDelphiFunction(@PadRight, 'PadRight', cdRegister);
  S.RegisterDelphiFunction(@PadCenter, 'PadCenter', cdRegister);
  S.RegisterDelphiFunction(@Copy2Symb, 'Copy2Symb', cdRegister);
  S.RegisterDelphiFunction(@Copy2SymbDel, 'Copy2SymbDel', cdRegister);
  S.RegisterDelphiFunction(@Copy2Space, 'Copy2Space', cdRegister);
  S.RegisterDelphiFunction(@Copy2SpaceDel, 'Copy2SpaceDel', cdRegister);
  S.RegisterDelphiFunction(@AnsiProperCase, 'AnsiProperCase', cdRegister);
  S.RegisterDelphiFunction(@WordCount, 'WordCount', cdRegister);
  S.RegisterDelphiFunction(@WordPosition, 'WordPosition', cdRegister);
  S.RegisterDelphiFunction(@ExtractWord, 'ExtractWord', cdRegister);
  S.RegisterDelphiFunction(@ExtractWordPos, 'ExtractWordPos', cdRegister);
  S.RegisterDelphiFunction(@ExtractDelimited, 'ExtractDelimited', cdRegister);
  S.RegisterDelphiFunction(@ExtractSubstr, 'ExtractSubstr', cdRegister);
  S.RegisterDelphiFunction(@IsWordPresent, 'IsWordPresent', cdRegister);
  S.RegisterDelphiFunction(@FindPart, 'FindPart', cdRegister);
  S.RegisterDelphiFunction(@IsWild, 'IsWild', cdRegister);
  S.RegisterDelphiFunction(@XorString, 'XorString', cdRegister);
  S.RegisterDelphiFunction(@XorEncode, 'XorEncode', cdRegister);
  S.RegisterDelphiFunction(@XorDecode, 'XorDecode', cdRegister);
  S.RegisterDelphiFunction(@GetCmdLineArg, 'GetCmdLineArg', cdRegister);
  S.RegisterDelphiFunction(@Numb2USA, 'Numb2USA', cdRegister);
  S.RegisterDelphiFunction(@Hex2Dec, 'Hex2Dec', cdRegister);
  S.RegisterDelphiFunction(@Dec2Numb, 'Dec2Numb', cdRegister);
  S.RegisterDelphiFunction(@Numb2Dec, 'Numb2Dec', cdRegister);
  S.RegisterDelphiFunction(@IntToBin, 'IntToBin', cdRegister);
  S.RegisterDelphiFunction(@IntToRoman, 'IntToRoman', cdRegister);
  S.RegisterDelphiFunction(@TryRomanToInt, 'TryRomanToInt', cdRegister);
  S.RegisterDelphiFunction(@RomanToInt, 'RomanToInt', cdRegister);
  S.RegisterDelphiFunction(@RomanToIntDef, 'RomanToIntDef', cdRegister);
  S.RegisterDelphiFunction(@BinToHex, 'BinToHex', cdRegister);
  S.RegisterDelphiFunction(@HexToBin, 'HexToBin', cdRegister);

  S.RegisterDelphiFunction(@PosSet, 'PosSet', cdRegister);
  S.RegisterDelphiFunction(@PosSetEx, 'PosSetEx', cdRegister);

  S.RegisterDelphiFunction(@RemoveLeadingChars, 'RemoveLeadingChars', cdRegister);
  S.RegisterDelphiFunction(@RemoveTrailingChars, 'RemoveTrailingChars', cdRegister);
  S.RegisterDelphiFunction(@RemovePadChars, 'RemovePadChars', cdRegister);

  S.RegisterDelphiFunction(@TrimLeftSet, 'TrimLeftSet', cdRegister);
  S.RegisterDelphiFunction(@TrimRightSet, 'TrimRightSet', cdRegister);
  S.RegisterDelphiFunction(@TrimSet, 'TrimSet', cdRegister);

  S.RegisterDelphiFunction(@FindMatchesBoyerMooreCaseSensitive, 'FindMatchesBoyerMooreCaseSensitive', cdRegister);
  S.RegisterDelphiFunction(@FindMatchesBoyerMooreCaseInSensitive, 'FindMatchesBoyerMooreCaseInSensitive', cdRegister);

  S.RegisterDelphiFunction(@StringReplace, 'StringReplace', cdRegister);
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
