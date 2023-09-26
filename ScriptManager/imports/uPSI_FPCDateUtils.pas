unit uPSI_FPCDateUtils;
{< Basic types and functions not imported by Pascal Script standard units.

  Remember to add and register uPSC_dateutils and uPSR_dateutils before this
    unit.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uPSRuntime, uPSCompiler, DateUtils;

procedure SIRegister_FPCDateUtils(CL: TPSPascalCompiler);
{< Compile-time registration functions. }

procedure RIRegister_FPCDateUtils_Routines(S: TPSExec);
{< Run-time registration functions. }

implementation


procedure SIRegister_FPCDateUtils(CL: TPSPascalCompiler);
begin
  // Const

  CL.AddConstantN('DaysPerWeek', 'integer').SetInt(DaysPerWeek);
  CL.AddConstantN('WeeksPerFortnight', 'integer').SetInt(WeeksPerFortnight);
  CL.AddConstantN('MonthsPerYear', 'integer').SetInt(MonthsPerYear);
  CL.AddConstantN('YearsPerDecade', 'integer').SetInt(YearsPerDecade);
  CL.AddConstantN('YearsPerCentury', 'integer').SetInt(YearsPerCentury);
  CL.AddConstantN('YearsPerMillennium', 'integer').SetInt(YearsPerMillennium);

  // ISO day numbers.
  CL.AddConstantN('DayMonday', 'integer').SetInt(DayMonday);
  CL.AddConstantN('DayTuesday', 'integer').SetInt(DayTuesday);
  CL.AddConstantN('DayWednesday', 'integer').SetInt(DayWednesday);
  CL.AddConstantN('DayThursday', 'integer').SetInt(DayThursday);
  CL.AddConstantN('DayFriday', 'integer').SetInt(DayFriday);
  CL.AddConstantN('DaySaturday', 'integer').SetInt(DaySaturday);
  CL.AddConstantN('DaySunday', 'integer').SetInt(DaySunday);

  CL.AddConstantN('MonthJanuary', 'integer').SetInt(MonthJanuary);
  CL.AddConstantN('MonthFebruary', 'integer').SetInt(MonthFebruary);
  CL.AddConstantN('MonthMarch', 'integer').SetInt(MonthMarch);
  CL.AddConstantN('MonthApril', 'integer').SetInt(MonthApril);
  CL.AddConstantN('MonthMay', 'integer').SetInt(MonthMay);
  CL.AddConstantN('MonthJune', 'integer').SetInt(MonthJune);
  CL.AddConstantN('MonthJuly', 'integer').SetInt(MonthJuly);
  CL.AddConstantN('MonthAugust', 'integer').SetInt(MonthAugust);
  CL.AddConstantN('MonthSeptember', 'integer').SetInt(MonthSeptember);
  CL.AddConstantN('MonthOctober', 'integer').SetInt(MonthOctober);
  CL.AddConstantN('MonthNovember', 'integer').SetInt(MonthNovember);
  CL.AddConstantN('MonthDecember', 'integer').SetInt(MonthDecember);
  (*
  // Fraction of a day
  OneHour        = TDateTime(1)/HoursPerDay;
  OneMinute      = TDateTime(1)/MinsPerDay;
  OneSecond      = TDateTime(1)/SecsPerDay;
  OneMillisecond = TDateTime(1)/MSecsPerDay;

  { This is actual days per year but you need to know if it's a leap year}
  DaysPerYear: array [Boolean] of Word = (365, 366);

  { Used in RecodeDate, RecodeTime and RecodeDateTime for those datetime }
  {  fields you want to leave alone }
  RecodeLeaveFieldAsIs = High(Word);

{ ---------------------------------------------------------------------
    Global variables used in this unit
  ---------------------------------------------------------------------}

Const

  { Average over a 4 year span. Valid for next 100 years }
  ApproxDaysPerMonth: Double = 30.4375;
  ApproxDaysPerYear: Double  = 365.25;



{ ---------------------------------------------------------------------
    Simple trimming functions.
  ---------------------------------------------------------------------}

Function DateOf(const AValue: TDateTime): TDateTime;
Function TimeOf(const AValue: TDateTime): TDateTime;

{ ---------------------------------------------------------------------
    Identification functions.
  ---------------------------------------------------------------------}

Function IsInLeapYear(const AValue: TDateTime): Boolean;
Function IsPM(const AValue: TDateTime): Boolean;
Function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
Function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
Function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
Function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
Function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
Function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean;

{ ---------------------------------------------------------------------
    Enumeration functions.
  ---------------------------------------------------------------------}

Function WeeksInYear(const AValue: TDateTime): Word;
Function WeeksInAYear(const AYear: Word): Word;
Function DaysInYear(const AValue: TDateTime): Word;
Function DaysInAYear(const AYear: Word): Word;
Function DaysInMonth(const AValue: TDateTime): Word;
Function DaysInAMonth(const AYear, AMonth: Word): Word;


{ ---------------------------------------------------------------------
    Variations on current date/time.
  ---------------------------------------------------------------------}


Function Today: TDateTime;
Function Yesterday: TDateTime;
Function Tomorrow: TDateTime;
Function IsToday(const AValue: TDateTime): Boolean;
Function IsSameDay(const AValue, ABasis: TDateTime): Boolean;
function IsSameMonth(const Avalue, ABasis: TDateTime): Boolean;
Function PreviousDayOfWeek (DayOfWeek : Word) : Word;

{ ---------------------------------------------------------------------
    Extraction functions.
  ---------------------------------------------------------------------}

Function YearOf(const AValue: TDateTime): Word;
Function MonthOf(const AValue: TDateTime): Word;
Function WeekOf(const AValue: TDateTime): Word;
Function DayOf(const AValue: TDateTime): Word;
Function HourOf(const AValue: TDateTime): Word;
Function MinuteOf(const AValue: TDateTime): Word;
Function SecondOf(const AValue: TDateTime): Word;
Function MilliSecondOf(const AValue: TDateTime): Word;

{ ---------------------------------------------------------------------
    Start/End of year functions.
  ---------------------------------------------------------------------}

Function StartOfTheYear(const AValue: TDateTime): TDateTime;
Function EndOfTheYear(const AValue: TDateTime): TDateTime;
Function StartOfAYear(const AYear: Word): TDateTime;
Function EndOfAYear(const AYear: Word): TDateTime;

{ ---------------------------------------------------------------------
    Start/End of month functions.
  ---------------------------------------------------------------------}

Function StartOfTheMonth(const AValue: TDateTime): TDateTime;
Function EndOfTheMonth(const AValue: TDateTime): TDateTime;
Function StartOfAMonth(const AYear, AMonth: Word): TDateTime;
Function EndOfAMonth(const AYear, AMonth: Word): TDateTime;

{ ---------------------------------------------------------------------
    Start/End of week functions.
  ---------------------------------------------------------------------}


Function StartOfTheWeek(const AValue: TDateTime): TDateTime;
Function EndOfTheWeek(const AValue: TDateTime): TDateTime;
Function StartOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
Function StartOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; // ADayOFWeek 1
Function EndOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
Function EndOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; // const ADayOfWeek: Word = 7


{ ---------------------------------------------------------------------
    Start/End of day functions.
  ---------------------------------------------------------------------}

Function StartOfTheDay(const AValue: TDateTime): TDateTime;
Function EndOfTheDay(const AValue: TDateTime): TDateTime;
Function StartOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload;
Function StartOfADay(const AYear, ADayOfYear: Word): TDateTime; overload;
Function EndOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload;
Function EndOfADay(const AYear, ADayOfYear: Word): TDateTime; overload;

{ ---------------------------------------------------------------------
    Part of year functions.
  ---------------------------------------------------------------------}

Function MonthOfTheYear(const AValue: TDateTime): Word;
Function WeekOfTheYear(const AValue: TDateTime): Word; overload;
Function WeekOfTheYear(const AValue: TDateTime; out AYear: Word): Word; overload;
Function DayOfTheYear(const AValue: TDateTime): Word;
Function HourOfTheYear(const AValue: TDateTime): Word;
Function MinuteOfTheYear(const AValue: TDateTime): LongWord;
Function SecondOfTheYear(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheYear(const AValue: TDateTime): Int64;

{ ---------------------------------------------------------------------
    Part of month functions.
  ---------------------------------------------------------------------}

Function WeekOfTheMonth(const AValue: TDateTime): Word; overload;
Function WeekOfTheMonth(const AValue: TDateTime; out AYear, AMonth: Word): Word; overload;
Function DayOfTheMonth(const AValue: TDateTime): Word;
Function HourOfTheMonth(const AValue: TDateTime): Word;
Function MinuteOfTheMonth(const AValue: TDateTime): Word;
Function SecondOfTheMonth(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;

{ ---------------------------------------------------------------------
    Part of week functions.
  ---------------------------------------------------------------------}

Function DayOfTheWeek(const AValue: TDateTime): Word;
Function HourOfTheWeek(const AValue: TDateTime): Word;
Function MinuteOfTheWeek(const AValue: TDateTime): Word;
Function SecondOfTheWeek(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;

{ ---------------------------------------------------------------------
    Part of day functions.
  ---------------------------------------------------------------------}

Function HourOfTheDay(const AValue: TDateTime): Word;
Function MinuteOfTheDay(const AValue: TDateTime): Word;
Function SecondOfTheDay(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;

{ ---------------------------------------------------------------------
    Part of hour functions.
  ---------------------------------------------------------------------}

Function MinuteOfTheHour(const AValue: TDateTime): Word;
Function SecondOfTheHour(const AValue: TDateTime): Word;
Function MilliSecondOfTheHour(const AValue: TDateTime): LongWord;

{ ---------------------------------------------------------------------
    Part of minute functions.
  ---------------------------------------------------------------------}


Function SecondOfTheMinute(const AValue: TDateTime): Word;
Function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;

{ ---------------------------------------------------------------------
    Part of second functions.
  ---------------------------------------------------------------------}

Function MilliSecondOfTheSecond(const AValue: TDateTime): Word;


{ ---------------------------------------------------------------------
    Range checking functions.
  ---------------------------------------------------------------------}

Function WithinPastYears(const ANow, AThen: TDateTime; const AYears: Integer): Boolean;
Function WithinPastMonths(const ANow, AThen: TDateTime; const AMonths: Integer): Boolean;
Function WithinPastWeeks(const ANow, AThen: TDateTime; const AWeeks: Integer): Boolean;
Function WithinPastDays(const ANow, AThen: TDateTime; const ADays: Integer): Boolean;
Function WithinPastHours(const ANow, AThen: TDateTime; const AHours: Int64): Boolean;
Function WithinPastMinutes(const ANow, AThen: TDateTime; const AMinutes: Int64): Boolean;
Function WithinPastSeconds(const ANow, AThen: TDateTime; const ASeconds: Int64): Boolean;
Function WithinPastMilliSeconds(const ANow, AThen: TDateTime; const AMilliSeconds: Int64): Boolean;

{ ---------------------------------------------------------------------
    Period functions.
  ---------------------------------------------------------------------}

Function YearsBetween(const ANow, AThen: TDateTime; AExact : Boolean = False): Integer;
Function MonthsBetween(const ANow, AThen: TDateTime; AExact : Boolean = False): Integer;
Function WeeksBetween(const ANow, AThen: TDateTime): Integer;
Function DaysBetween(const ANow, AThen: TDateTime): Integer;
Function HoursBetween(const ANow, AThen: TDateTime): Int64;
Function MinutesBetween(const ANow, AThen: TDateTime): Int64;
*)
  CL.AddDelphiFunction('function SecondsBetween(const ANow, AThen: TDateTime): Int64;');
(*
Function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;
Procedure PeriodBetween(const ANow, AThen: TDateTime; Out Years, months, days : Word);

{ ---------------------------------------------------------------------
    Timespan in xxx functions.
  ---------------------------------------------------------------------}

{ YearSpan and MonthSpan are approximate values }
Function YearSpan(const ANow, AThen: TDateTime): Double;
Function MonthSpan(const ANow, AThen: TDateTime): Double;
Function WeekSpan(const ANow, AThen: TDateTime): Double;
Function DaySpan(const ANow, AThen: TDateTime): Double;
Function HourSpan(const ANow, AThen: TDateTime): Double;
Function MinuteSpan(const ANow, AThen: TDateTime): Double;
Function SecondSpan(const ANow, AThen: TDateTime): Double;
Function MilliSecondSpan(const ANow, AThen: TDateTime): Double;

{ ---------------------------------------------------------------------
    Increment/decrement functions.
  ---------------------------------------------------------------------}

Function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer ): TDateTime;
Function IncYear(const AValue: TDateTime): TDateTime; // ; const ANumberOfYears: Integer = 1)
// Function IncMonth is in SysUtils
Function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;
Function IncWeek(const AValue: TDateTime): TDateTime; // ; const ANumberOfWeeks: Integer = 1)
Function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;
Function IncDay(const AValue: TDateTime): TDateTime; //; const ANumberOfDays: Integer = 1)
Function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;
Function IncHour(const AValue: TDateTime): TDateTime; //; const ANumberOfHours: Int64 = 1
Function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;
Function IncMinute(const AValue: TDateTime): TDateTime; // ; const ANumberOfMinutes: Int64 = 1
Function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
Function IncSecond(const AValue: TDateTime): TDateTime; // ; const ANumberOfSeconds: Int64 = 1
Function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;
Function IncMilliSecond(const AValue: TDateTime): TDateTime; // ; const ANumberOfMilliSeconds: Int64 = 1

{ ---------------------------------------------------------------------
    Encode/Decode of complete timestamp
  ---------------------------------------------------------------------}

Function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
Procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
Function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AValue: TDateTime): Boolean;

{ ---------------------------------------------------------------------
    Encode/decode date, specifying week of year and day of week
  ---------------------------------------------------------------------}

Function EncodeDateWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
Function EncodeDateWeek(const AYear, AWeekOfYear: Word): TDateTime; //; const ADayOfWeek: Word = 1
Procedure DecodeDateWeek(const AValue: TDateTime; out AYear, AWeekOfYear, ADayOfWeek: Word);
Function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; out AValue: TDateTime; const ADayOfWeek: Word): Boolean;
Function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; out AValue: TDateTime): Boolean; //; const ADayOfWeek: Word = 1

{ ---------------------------------------------------------------------
    Encode/decode date, specifying day of year
  ---------------------------------------------------------------------}

Function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime;
Procedure DecodeDateDay(const AValue: TDateTime; out AYear, ADayOfYear: Word);
Function TryEncodeDateDay(const AYear, ADayOfYear: Word; out AValue: TDateTime): Boolean;

{ ---------------------------------------------------------------------
    Encode/decode date, specifying week of month
  ---------------------------------------------------------------------}

Function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): TDateTime;
Procedure DecodeDateMonthWeek(const AValue: TDateTime; out AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
Function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word; out AValue: TDateTime): Boolean;

{ ---------------------------------------------------------------------
    Encode time interval, allowing hours>24
  ---------------------------------------------------------------------}

function TryEncodeTimeInterval(Hour, Min, Sec, MSec:word; Out Time : TDateTime) : boolean;
function EncodeTimeInterval(Hour, Minute, Second, MilliSecond:word): TDateTime;

{ ---------------------------------------------------------------------
    Replace given element with supplied value.
  ---------------------------------------------------------------------}

Function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
Function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
Function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
Function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
Function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
Function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
Function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;
Function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime;
Function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
Function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
Function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AResult: TDateTime): Boolean;

{ ---------------------------------------------------------------------
    Comparision of date/time
  ---------------------------------------------------------------------}

Function CompareDateTime(const A, B: TDateTime): TValueRelationship;
Function CompareDate(const A, B: TDateTime): TValueRelationship;
Function CompareTime(const A, B: TDateTime): TValueRelationship;
Function SameDateTime(const A, B: TDateTime): Boolean;
Function SameDate(const A, B: TDateTime): Boolean;
Function SameTime(const A, B: TDateTime): Boolean;
function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
function TimeInRange(ATime: TTime; AStartTime, AEndTime: TTime; AInclusive: Boolean = True): Boolean;
function DateInRange(ADate: TDate; AStartDate, AEndDate: TDate; AInclusive: Boolean = True): Boolean;


{ For a given date these Functions tell you the which day of the week of the
  month (or year).  If its a Thursday, they will tell you if its the first,
  second, etc Thursday of the month (or year).  Remember, even though its
  the first Thursday of the year it doesn't mean its the first week of the
  year.  See ISO 8601 above for more information. }

Function NthDayOfWeek(const AValue: TDateTime): Word;

Procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; out AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);

Function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word): TDateTime;
Function TryEncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word; out AValue: TDateTime): Boolean;

{ ---------------------------------------------------------------------
    Exception throwing routines
  ---------------------------------------------------------------------}

Procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; const ABaseDate: TDateTime);
Procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word); // const ABaseDate: TDateTime = 0
Procedure InvalidDateWeekError(const AYear, AWeekOfYear, ADayOfWeek: Word);
Procedure InvalidDateDayError(const AYear, ADayOfYear: Word);
Procedure InvalidDateMonthWeekError(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
Procedure InvalidDayOfWeekInMonthError(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word);

{ ---------------------------------------------------------------------
    Julian and Modified Julian Date conversion support
  ---------------------------------------------------------------------}

Function DateTimeToJulianDate(const AValue: TDateTime): Double;
Function JulianDateToDateTime(const AValue: Double): TDateTime;
Function TryJulianDateToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;

Function DateTimeToModifiedJulianDate(const AValue: TDateTime): Double;
Function ModifiedJulianDateToDateTime(const AValue: Double): TDateTime;
Function TryModifiedJulianDateToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;

{ ---------------------------------------------------------------------
    Unix timestamp support.
  ---------------------------------------------------------------------}

// Estas ya están incluidas en uPSX_dateutils
// Function DateTimeToUnix(const AValue: TDateTime; AInputIsUTC: Boolean = True): Int64;
// Function UnixToDateTime(const AValue: Int64; aReturnUTC : Boolean = true): TDateTime;
Function UnixTimeStampToMac(const AValue: Int64): Int64;

{ ---------------------------------------------------------------------
    Mac timestamp support.
  ---------------------------------------------------------------------}

Function DateTimeToMac(const AValue: TDateTime): Int64;
Function MacToDateTime(const AValue: Int64): TDateTime;
Function MacTimeStampToUnix(const AValue: Int64): Int64;

{ .....................................................................
    Dos <-> Delphi datetime support
  .....................................................................}

Function DateTimeToDosDateTime(const AValue: TDateTime): longint;
Function DosDateTimeToDateTime( AValue: longint): TDateTime;

{ UTC <-> Local time }

Function UniversalTimeToLocal(UT: TDateTime): TDateTime;
Function UniversalTimeToLocal(UT: TDateTime; TZOffset : Integer): TDateTime;
Function LocalTimeToUniversal(LT: TDateTime): TDateTime;
Function LocalTimeToUniversal(LT: TDateTime; TZOffset: Integer): TDateTime;


{ ScanDateTime is a limited inverse of formatdatetime }
function ScanDateTime(const Pattern:string;const s:string;const fmt:TFormatSettings;startpos:integer=1) : tdatetime; overload;
function ScanDateTime(const Pattern:string;const s:string;startpos:integer=1) : tdatetime; overload;

// ISO date/time
// YYYYMMDD or YYYY-MM-DD
function TryISOStrToDate(const aString: string; out outDate: TDateTime): Boolean;
// HH HH:NN HH:NN:SS HH:NN:SS.ZZZ or HHNN HHNNSS HHNNSS.ZZZ
function TryISOStrToTime(const aString: string; Out outTime: TDateTime): Boolean;
// Combination of previous
function TryISOStrToDateTime(const aString: string; out outDateTime: TDateTime): Boolean;
// Z +hh:nn -hh:nn
Function TryISOTZStrToTZOffset(const TZ : String; Out TZOffset : Integer) : boolean;

// ISO 8601 Date/Time formatting

function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean = True): string;
Function ISO8601ToDate(const DateString: string; ReturnUTC : Boolean = True): TDateTime;
Function ISO8601ToDateDef(const DateString: string; ReturnUTC : Boolean; aDefault : TDateTime ): TDateTime; deprecated;
Function ISO8601ToDateDef(const DateString: string; aDefault : TDateTime; ReturnUTC : Boolean = True ): TDateTime;
Function TryISO8601ToDate(const DateString: string; out ADateTime: TDateTime; ReturnUTC : Boolean = True) : Boolean;
 *)

end;

procedure RIRegister_FPCDateUtils_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@SecondsBetween, 'SecondsBetween', cdRegister);
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

