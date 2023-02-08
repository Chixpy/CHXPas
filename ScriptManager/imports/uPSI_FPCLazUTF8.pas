unit uPSI_FPCLazUTF8;
{< LazUTF8 for Pascal Script.

  Copyright (C) 2019-2020 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, uPSRuntime, uPSCompiler;

procedure SIRegister_FPCLazUTF8(CL: TPSPascalCompiler);

procedure RIRegister_FPCLazUTF8_Routines(S: TPSExec);

implementation

procedure SIRegister_FPCLazUTF8(CL: TPSPascalCompiler);
begin

//// AnsiToUTF8 and UTF8ToAnsi need a widestring manager under Linux, BSD, MacOSX
//// but normally these OS use UTF-8 as system encoding so the widestringmanager
//// is not needed.
//function NeedRTLAnsi: boolean;// true if system encoding is not UTF-8
//procedure SetNeedRTLAnsi(NewValue: boolean);
//
//// UTF8ToSys works like UTF8ToAnsi but more independent of widestringmanager
  CL.AddDelphiFunction('function UTF8ToSys(const s: string): string;');
//function UTF8ToSys(const AFormatSettings: TFormatSettings): TFormatSettings; overload; {$IFDEF UTF8_RTL}inline;{$ENDIF}
//
//// SysToUTF8 works like AnsiToUTF8 but more independent of widestringmanager
//function SysToUTF8(const s: string): string; overload; {$IFDEF UTF8_RTL}inline;{$ENDIF}
//function SysToUTF8(const AFormatSettings: TFormatSettings): TFormatSettings; overload;
//
//// converts OEM encoded string to UTF8 (used with some Windows specific functions)
//function ConsoleToUTF8(const s: string): string; {$IFDEF UTF8_RTL}inline;{$ENDIF}
//// converts UTF8 string to console encoding (used by Write, WriteLn)
//function UTF8ToConsole(const s: string): string; {$IFDEF UTF8_RTL}inline;{$ENDIF}
//
//// for all Windows supporting 8bit codepages (e.g. not WinCE)
//// converts string in Windows code page to UTF8 (used with some Windows specific functions)
//function WinCPToUTF8(const s: string): string; {$ifdef WinCe}inline;{$endif}
//// converts UTF8 string to Windows code page encoding (used by Write, WriteLn)
//function UTF8ToWinCP(const s: string): string; {$ifdef WinCe}inline;{$endif}
//
//function ParamStrUTF8(Param: Integer): string;
//
//{$ifdef windows}
//procedure GetFormatSettingsUTF8;
//procedure GetLocaleFormatSettingsUTF8(LCID: Integer; var aFormatSettings: TFormatSettings);
//{$endif}
//
//Function GetEnvironmentVariableCountUTF8: Integer;
//function GetEnvironmentStringUTF8(Index: Integer): string;
//function GetEnvironmentVariableUTF8(const EnvVar: string): String;
//function SysErrorMessageUTF8(ErrorCode: Integer): String;
//
//// Returns the size of one codepoint in bytes.
//function UTF8CodepointSize(p: PChar): integer; inline;
//function UTF8CharacterLength(p: PChar): integer; deprecated 'Use UTF8CodepointSize instead.';
//// Fast version of UTF8CodepointSize. Assumes the UTF-8 codepoint is valid.
//function UTF8CodepointSizeFast(p: PChar): integer; inline;
//
//function UTF8Length(const s: string): PtrInt; inline;
//function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
//// Fast versions of UTF8Length. They assume the UTF-8 data is valid.
//function UTF8LengthFast(const s: string): PtrInt; inline;
//function UTF8LengthFast(p: PChar; ByteCount: PtrInt): PtrInt;
//
//// Functions dealing with unicode number U+xxx.
//function UTF8CodepointToUnicode(p: PChar; out CodepointLen: integer): Cardinal;
//function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal; deprecated 'Use UTF8CodepointToUnicode instead.';
//function UnicodeToUTF8(CodePoint: cardinal): string; // UTF32 to UTF8
//function UnicodeToUTF8(CodePoint: cardinal; Buf: PChar): integer; // UTF32 to UTF8
//function UnicodeToUTF8SkipErrors(CodePoint: cardinal; Buf: PChar): integer; // UTF32 to UTF8
//function UnicodeToUTF8Inline(CodePoint: cardinal; Buf: PChar): integer; inline; // UTF32 to UTF8
//function UTF8ToDoubleByteString(const s: string): string;
//function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
//function UTF8FindNearestCharStart(UTF8Str: PChar; Len: SizeInt;
//                                  BytePos: SizeInt): SizeInt;
//function Utf8TryFindCodepointStart(AString: PChar; var CurPos: PChar; out CodepointLen: Integer): Boolean;
//function Utf8TryFindCodepointStart(const AString: String; var Index: Integer; out CharLen: Integer): Boolean;
//// find the n-th UTF8 codepoint, ignoring BIDI
//function UTF8CodepointStart(UTF8Str: PChar; Len, CodepointIndex: PtrInt): PChar;
//function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar; deprecated 'Use UTF8CodepointStart instead.';
//// find the byte index of the n-th UTF8 codepoint, ignoring BIDI (byte len of substr)
//function UTF8CodepointToByteIndex(UTF8Str: PChar; Len, CodepointIndex: PtrInt): PtrInt;
//function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt; deprecated 'Use UTF8CodepointToByteIndex instead.';
//procedure UTF8FixBroken(P: PChar); overload;
//procedure UTF8FixBroken(var S: string); overload;
//function UTF8CodepointStrictSize(P: PChar): integer;
//function UTF8CharacterStrictLength(P: PChar): integer; deprecated 'Use UTF8CodepointStrictSize instead.';
//function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
//

// TODO: Muchas de estas funciones deben ser usadas en uPSI_FPCStrUtils


//function UTF8Pos(const SearchForText, SearchInText: string; StartPos: SizeInt = 1): PtrInt;
//function UTF8PosP(SearchForText: PChar; SearchForTextLen: SizeInt;
//  SearchInText: PChar; SearchInTextLen: SizeInt): PChar;
//function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
//{$IFnDEF NO_CP_RTL}
//procedure UTF8Delete(var s: Utf8String; StartCharIndex, CharCount: PtrInt);
//{$ENDIF}
//procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
//{$IFnDEF NO_CP_RTL}
//procedure UTF8Insert(const source: Utf8String; var s: Utf8String; StartCharIndex: PtrInt);
//{$ENDIF}
//procedure UTF8Insert(const source: String; var s: String; StartCharIndex: PtrInt);
//function UTF8StringReplace(const S, OldPattern, NewPattern: String;
//  Flags: TReplaceFlags; ALanguage: string=''): String;
//
//function UTF8LowerCase(const AInStr: string; ALanguage: string=''): string;
//function UTF8LowerString(const s: string): string;
//function UTF8UpperCase(const AInStr: string; ALanguage: string=''): string;
//function UTF8UpperString(const s: string): string;
//function UTF8SwapCase(const AInStr: string; ALanguage: string=''): string;
//// Capitalize the first letters of every word
//function UTF8ProperCase(const AInStr: string; const WordDelims: TSysCharSet): string;
//function FindInvalidUTF8Codepoint(p: PChar; Count: PtrInt; StopOnNonUTF8: Boolean = true): PtrInt;
//function FindInvalidUTF8Character(p: PChar; Count: PtrInt; StopOnNonUTF8: Boolean = true): PtrInt; deprecated 'Use FindInvalidUTF8Codepoint instead.';
//function UTF8StringOfChar(AUtf8Char: String; N: Integer): String;
//function UTF8AddChar(AUtf8Char: String; const S: String; N: Integer): String;
//function UTF8AddCharR(AUtf8Char: String; const S: String; N: Integer): String;
//function UTF8PadLeft(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
//function UTF8PadRight(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
//function UTF8PadCenter(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
//function UTF8LeftStr(const AText: String; const ACount: Integer): String;
//function UTF8RightStr(const AText: String; const ACount: Integer): String;
//function UTF8QuotedStr(const S, Quote: string): string;
////Utf8 version of MidStr is just Utf8Copy with same parameters, so it is not implemented here
//function UTF8StartsText(const ASubText, AText: string): Boolean;
//function UTF8EndsText(const ASubText, AText: string): Boolean;
//function UTF8ReverseString(p: PChar; const ByteCount: LongInt): string;
//function UTF8ReverseString(const AText: string): string; inline;
//function UTF8RPos(const Substr, Source: string): PtrInt;
//
//function UTF8WrapText(S, BreakStr: string; BreakChars: TSysCharSet; MaxCol: integer): string; overload;
//function UTF8WrapText(S: string; MaxCol: integer): string; overload;
//
//type
//  TEscapeMode = (emPascal, emHexPascal, emHexC, emC, emAsciiControlNames);
//
//function Utf8EscapeControlChars(S: String; EscapeMode: TEscapeMode = emPascal): String;
//
//type
//  TUTF8TrimFlag = (
//    u8tKeepStart,
//    u8tKeepEnd,
//    u8tKeepTabs,
//    u8tKeepLineBreaks,
//    u8tKeepNoBreakSpaces,
//    u8tKeepControlCodes // excluding tabs and line breaks
//    );
//  TUTF8TrimFlags = set of TUTF8TrimFlag;
//function UTF8Trim(const s: string; Flags: TUTF8TrimFlags = []): string;
//
////compare functions
//
//function UTF8CompareStr(const S1, S2: string): PtrInt; inline;
//function UTF8CompareStrP(S1, S2: PChar): PtrInt;
//function UTF8CompareStr(S1: PChar; Count1: SizeInt; S2: PChar; Count2: SizeInt): PtrInt;
//function UTF8CompareText(const S1, S2: string): PtrInt;
//function UTF8CompareStrCollated(const S1, S2: string): PtrInt; {$IFnDEF ACP_RTL}inline;{$endif}
//function CompareStrListUTF8LowerCase(List: TStringList; Index1, Index2: Integer): Integer;
//
//type
//  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
//    trInvalidChar, trUnfinishedChar);
//
//  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
//    toUnfinishedCharError, toUnfinishedCharToSymbol);
//  TConvertOptions = set of TConvertOption;
//
//function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
//  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
//  out ActualWideCharCount: SizeUInt): TConvertResult;
//
//function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
//  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
//  out ActualCharCount: SizeUInt): TConvertResult;
//
//function UTF8ToUTF16(const S: AnsiString): UnicodeString; overload;
//function UTF8ToUTF16(const P: PChar; ByteCnt: SizeUInt): UnicodeString; overload;
//function UTF16ToUTF8(const S: UnicodeString): AnsiString; overload;
//function UTF16ToUTF8(const P: PWideChar; WideCnt: SizeUInt): AnsiString; overload;
//
//// locale
//procedure LazGetLanguageIDs(var Lang, FallbackLang: String);
//procedure LazGetShortLanguageID(var Lang: String);
//
//var
//  FPUpChars: array[char] of char;
//
//procedure ReplaceSubstring(var s: string; StartPos, Count: SizeInt;
//                           const Insertion: string);
end;

procedure RIRegister_FPCLazUTF8_Routines(S: TPSExec);
begin
    S.RegisterDelphiFunction(@UTF8ToSys, 'UTF8ToSys', cdRegister);

end;

end.

