unit uCHXIni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8;

type

  // TODO: Merge with uMUGENIni of MUGENConf; it's different from a ini file

  { TODO : Use this from IniFiles }
  TCHXIniOption = (ociStripComments,    // Strip comments when reading file
    ociStripInvalid,
    // Strip invalid lines when reading file.
    ociEscapeLineFeeds, // Escape linefeeds when reading file.
    ociCaseSensitive,   // Use Case sensitive section/key names
    ociStripQuotes,
    // Strip quotes when reading string values.
    ociFormatSettingsActive);
  // Use format settings when writing date/float etc.
  TCHXIniOptions = set of TCHXIniOption;

  TCHXIniSectionOption = (svoIncludeComments, svoIncludeInvalid,
    svoIncludeQuotes);
  TCHXIniSectionOptions = set of TCHXIniSectionOption;

  TCHXIniValueOption = (ociNoQuotes, ociAutoQuotes, ociAlwaysQuotes);

  { cCHXIniLine class.

    This class represents a single line of an Ini file, with its Key, Value and
      inline comment.
  }

  cCHXIniLine = class(TObject)
  private
    FComment: string;
    FKey: string;
    FValue: string;
    procedure SetComment(AValue: string);
    procedure SetKey(AValue: string);
    procedure SetValue(AValue: string);

  protected

  public
    property Key: string read FKey write SetKey;
    {< Key of value. }
    property Value: string read FValue write SetValue;
    {< Value itself. }
    property Comment: string read FComment write SetComment;
    {< Inline comment. }

    constructor Create(const aKey, aValue, aComment: string);
    destructor Destroy; override;
  end;

  cCHXIniLinesList = specialize TFPGObjectList<cCHXIniLine>;
  {< List of owned lines. }

  { cCHXIniSection class.

    It represents a section in the Ini file.
  }

  cCHXIniSection = class(TObject)
  private
    FComment: string;
    FLines: cCHXIniLinesList;
    FName: string;
    procedure SetComment(AValue: string);
    procedure SetName(AValue: string);

  protected

  public
    property Name: string read FName write SetName;
    {< Name of the section. }
    property Comment: string read FComment write SetComment;
    {< Inline comment in the section line. }

    property Lines: cCHXIniLinesList read FLines;
    {< List of lines in the section. }

    function LineByKey(aKey: string): cCHXIniLine;
    {< Gets a cCHXIniLine by its key.}
    function ValueByKey(aKey, aDefault: string): string;
    {< Gets the value.}
    function AddLine(aKey, aValue, aComment: string;
      MergeKeys: boolean): cCHXIniLine;

    constructor Create(const aName, aComment: string); overload;
    destructor Destroy; override;
  end;

  { cCHXIniSectionList }
  cCHXIniSectionList = specialize TFPGObjectList<cCHXIniSection>;

  { cCHXIni }
  cCHXIni = class(TObject)
  private
    FAssignChar: string;
    FCommentBegin: string;
    FEscapeLF: boolean;
    FEscapeLFChar: string;
    FEscapeLFMaxWidth: word;
    FFileName: string;
    FMergeKeys: boolean;
    FMergeSections: boolean;
    FNewLine: string;
    FQuoteBegin: string;
    FQuoteEnd: string;
    FRemoveQuotes: boolean;
    FSectionBegin: string;
    FSectionEnd: string;
    FSectionList: cCHXIniSectionList;
    FWriteQuotes: TCHXIniValueOption;
    procedure SetAssignChar(AValue: string);
    procedure SetCommentBegin(AValue: string);
    procedure SetEscapeLF(AValue: boolean);
    procedure SetEscapeLFChar(AValue: string);
    procedure SetEscapeLFMaxWidth(AValue: word);
    procedure SetFileName(AValue: string);
    procedure SetMergeKeys(AValue: boolean);
    procedure SetMergeSections(AValue: boolean);
    procedure SetNewLine(AValue: string);
    procedure SetQuoteBegin(AValue: string);
    procedure SetQuoteEnd(AValue: string);
    procedure SetRemoveQuotes(AValue: boolean);
    procedure SetSectionBegin(AValue: string);
    procedure SetSectionEnd(AValue: string);
    procedure SetWriteQuotes(AValue: TCHXIniValueOption);

  protected
    property SectionList: cCHXIniSectionList read FSectionList;

    procedure FillSectionList(aStringList: TStrings);

  public
    property FileName: string read FFileName write SetFileName;
    {< Filename of Ini file.
    }
    property EscapeLF: boolean read FEscapeLF write SetEscapeLF;
    property MergeSections: boolean
      read FMergeSections write SetMergeSections;
    property MergeKeys: boolean read FMergeKeys write SetMergeKeys;

    property CommentBegin: string read FCommentBegin write SetCommentBegin;
    property SectionBegin: string read FSectionBegin write SetSectionBegin;
    property SectionEnd: string read FSectionEnd write SetSectionEnd;
    property NewLine: string read FNewLine write SetNewLine;
    property RemoveQuotes: boolean read FRemoveQuotes write SetRemoveQuotes;
    {< Quotes are removed when a Value is readed, internally
      the value is stored as is.
    }
    property QuoteBegin: string read FQuoteBegin write SetQuoteBegin;
    property QuoteEnd: string read FQuoteEnd write SetQuoteEnd;
    property AssignChar: string read FAssignChar write SetAssignChar;
    property EscapeLFChar: string read FEscapeLFChar write SetEscapeLFChar;
    property EscapeLFMaxWidth: word read FEscapeLFMaxWidth
      write SetEscapeLFMaxWidth;
    property WriteQuotes: TCHXIniValueOption
      read FWriteQuotes write SetWriteQuotes;

    procedure LoadFromFile(aFilename: string = '');
    {< Reads a ini file from disk.
    }
    procedure SaveToFile(aFilename: string = '');
    {< Save a ini file to disk (it doesn't merge content).

      For merging, target file must be loaded first.
    }
    function SectionNameList: TStringList;
    {< Creates a TStringlist with the name of setions.
    }
    function SectionByIndex(aIndex: integer): cCHXIniSection;
    {< Returns the section at aIndex position in the Ini file.

    Section 0 is the text encountered before any [Section] heading.

    This way you actually can access to any section in the file.
    }
    function SectionByName(aSectionName: string): cCHXIniSection;
    {< Returns the section with aSectionName name.

    If many sections have the same name, the first one is returned. So you can't
      access to all sections this way.

    The text encountered before any section is in the first "virtual" section,
      under the name '' (empty string).
    }
    function SectionCount: integer;
    {< Returns the number of sections in the file.

    It includes the text before any section ("virtual" Section 0 or ''), even
      if it's empty.
    }

    // Main reading string functions
    function ReadString(const aSectionName, aKeyName, aDefault:
      string): string;
    {< Accesses a value by section and key name. }
    function ReadString(const aSectionIndex: integer;
      const aKeyName, aDefault: string): string;
    {< Accesses a value by section index and key name. }
    function ReadString(const aSectionIndex, aKeyIndex: integer;
      const aDefault: string): string;
    {< Accesses a value by section index and line. }


    // Common ini read functions
    function ReadBoolean(const aSectionName, aKeyName: string;
      const aDefault: boolean): boolean;
    function ReadMultiStrings(const aSectionName, aKeyName, aDefault: string;
      const Separator: char = ','): TStringList;
    function ReadInteger(const aSectionName, aKeyName: string;
      const aDefault: integer): integer;
    function ReadTPoint(const aSectionName, aKeyName: string;
      const aDefault: TPoint): TPoint;

    // Common ini writing functions
    procedure WriteString(const aSectionName, aKeyName: string;
      aValue: string);
    procedure WriteBoolean(const aSectionName, aKeyName: string;
      const aValue: boolean);
    procedure WriteInteger(const aSectionName, aKeyName: string;
      const aValue: integer);

    // Other methods (may be they must be protected...)
    procedure StrRemoveQuotes(var aString: string);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cCHXIniLine }

procedure cCHXIniLine.SetComment(AValue: string);
begin
  if (Key <> '') or (Value <> '') then
    FComment := UTF8Trim(Comment) // Inline comments trimmed
  else
    FComment := AValue;
end;

procedure cCHXIniLine.SetKey(AValue: string);
begin
  FKey := UTF8Trim(AValue);
end;

procedure cCHXIniLine.SetValue(AValue: string);
begin
  if Key <> '' then
    FValue := UTF8Trim(aValue)
  else
    FValue := aValue;
end;

constructor cCHXIniLine.Create(const aKey, aValue, aComment: string);
begin
  inherited Create;

  Key := aKey;
  Value := aValue;
  Comment := aComment;
end;

destructor cCHXIniLine.Destroy;
begin
  inherited Destroy;
end;

{ cCHXIniSection }

procedure cCHXIniSection.SetComment(AValue: string);
begin
  FComment := UTF8Trim(AValue); // Inline comments trimmed
end;

procedure cCHXIniSection.SetName(AValue: string);
begin
  FName := UTF8Trim(AValue);
end;

function cCHXIniSection.LineByKey(aKey: string): cCHXIniLine;
var
  CurrLine: cCHXIniLine;
  i: integer;
begin
  Result := nil;
  aKey := UTF8Trim(aKey);
  if aKey = '' then
    Exit;

  // Getting the last line with the key
  i := Lines.Count - 1;
  while i >= 0 do
  begin
    CurrLine := Lines[i];
    if UTF8CompareText(CurrLine.Key, aKey) = 0 then
    begin
      // Found!
      Result := CurrLine;
      break;
    end;
    Dec(i);
  end;
end;

function cCHXIniSection.ValueByKey(aKey, aDefault: string): string;
var
  aLine: cCHXIniLine;
begin
  Result := aDefault;
  aLine := LineByKey(aKey);
  if aLine = nil then
    Exit;
  Result := aLine.Value;
end;

function cCHXIniSection.AddLine(aKey, aValue, aComment: string;
  MergeKeys: boolean): cCHXIniLine;
begin
  Result := nil;

  aKey := UTF8Trim(aKey);

  if MergeKeys and (aKey <> '') then
    Result := LineByKey(aKey);

  if Result = nil then
  begin
    Result := cCHXIniLine.Create(aKey, aValue, aComment);
    Lines.Add(Result);
  end
  else
  begin
    Result.Value := aValue;
    Result.Comment := Result.Comment + aComment;
  end;
end;

constructor cCHXIniSection.Create(const aName, aComment: string);
begin
  inherited Create;
  self.Name := aName;
  self.Comment := aComment;
  FLines := cCHXIniLinesList.Create(True);
end;

destructor cCHXIniSection.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

procedure cCHXIni.SetAssignChar(AValue: string);
begin
  if FAssignChar = AValue then
    Exit;
  FAssignChar := AValue;
end;

procedure cCHXIni.SetCommentBegin(AValue: string);
begin
  if FCommentBegin = AValue then
    Exit;
  FCommentBegin := AValue;
end;

procedure cCHXIni.SetEscapeLF(AValue: boolean);
begin
  if FEscapeLF = AValue then
    Exit;
  FEscapeLF := AValue;
end;

procedure cCHXIni.SetEscapeLFChar(AValue: string);
begin
  if FEscapeLFChar = AValue then
    Exit;
  FEscapeLFChar := AValue;
end;

procedure cCHXIni.SetEscapeLFMaxWidth(AValue: word);
begin
  if FEscapeLFMaxWidth = AValue then
    Exit;
  FEscapeLFMaxWidth := AValue;
end;

procedure cCHXIni.SetFileName(AValue: string);
begin
  if FFileName = AValue then
    Exit;
  FFileName := AValue;
end;

procedure cCHXIni.SetMergeKeys(AValue: boolean);
begin
  if FMergeKeys = AValue then
    Exit;
  FMergeKeys := AValue;
end;

procedure cCHXIni.SetMergeSections(AValue: boolean);
begin
  if FMergeSections = AValue then
    Exit;
  FMergeSections := AValue;
end;

procedure cCHXIni.SetNewLine(AValue: string);
begin
  if FNewLine = AValue then
    Exit;
  FNewLine := AValue;
end;

procedure cCHXIni.SetQuoteBegin(AValue: string);
begin
  if FQuoteBegin = AValue then
    Exit;
  FQuoteBegin := AValue;
end;

procedure cCHXIni.SetQuoteEnd(AValue: string);
begin
  if FQuoteEnd = AValue then
    Exit;
  FQuoteEnd := AValue;
end;

procedure cCHXIni.SetRemoveQuotes(AValue: boolean);
begin
  if FRemoveQuotes = AValue then
    Exit;
  FRemoveQuotes := AValue;
end;

procedure cCHXIni.SetSectionBegin(AValue: string);
begin
  if FSectionBegin = AValue then
    Exit;
  FSectionBegin := AValue;
end;

procedure cCHXIni.SetSectionEnd(AValue: string);
begin
  if FSectionEnd = AValue then
    Exit;
  FSectionEnd := AValue;
end;

procedure cCHXIni.SetWriteQuotes(AValue: TCHXIniValueOption);
begin
  if FWriteQuotes = AValue then
    Exit;
  FWriteQuotes := AValue;
end;

{ cCHXIni }
procedure cCHXIni.FillSectionList(aStringList: TStrings);

  procedure ExtractInlineComment(const Str: string;
  var aValue, aComment: string);
  var
    aPos: integer;
  begin
    aPos := UTF8Pos(CommentBegin, Str);
    if aPos <> 0 then
    begin
      // Has a comment
      aComment := UTF8Trim(UTF8Copy(Str, aPos + UTF8Length(CommentBegin),
        MaxInt));
      aValue := UTF8Trim(UTF8Copy(Str, 1, aPos - 1));
    end
    else
    begin
      aValue := Str;
      aComment := '';
    end;
  end;

  procedure RemoveTralingEmptyLines(aSection: cCHXIniSection);
  begin
    while (aSection.Lines.Count > 0) and
      (aSection.Lines[aSection.Lines.Count - 1].Key = '') and
      (aSection.Lines[aSection.Lines.Count - 1].Value = '') and
      (aSection.Lines[aSection.Lines.Count - 1].Comment = '') do
      aSection.Lines.Delete(aSection.Lines.Count - 1);

  end;

var
  i, aPos: integer;
  CurrLine: string;
  CurrSection: cCHXIniSection;
  aKey, aValue, aComment: string;
begin
  // Strings before any SectionByName. Internally managed as a Section
  //   with empty name at position 0 in section list.
  CurrSection := cCHXIniSection.Create('', '');
  SectionList.Add(CurrSection);

  i := 0;
  while i < aStringList.Count do
  begin
    CurrLine := UTF8Trim(aStringList[i]);
    aKey := '';
    aValue := '';
    aComment := '';

    if CurrLine = '' then
      // It's an empty line
      CurrSection.AddLine('', '', '', MergeKeys)
    else
    begin
      ExtractInlineComment(CurrLine, aValue, aComment);

      // Is it a Section?
      aPos := UTF8Pos(SectionEnd, aValue);
      if (UTF8Copy(aValue, 1, UTF8Length(SectionBegin)) =
        SectionBegin) and (aPos <> 0) then
      begin
        // Yes, it's a section

        // Removing last empty lines of the previous Section
        RemoveTralingEmptyLines(CurrSection);

        // Extracting Section Key
        aKey := UTF8Trim(UTF8Copy(aValue, UTF8Length(SectionBegin) +
          1, aPos - 1 - UTF8Length(SectionBegin)));
        // Adding text after SectionEnd to aComment;
        aComment := UTF8Trim(UTF8Copy(aValue, aPos +
          UTF8Length(SectionEnd), MaxInt)) + aComment;

        CurrSection := SectionByName(aKey);

        if (MergeSections) and (CurrSection <> nil) then
        begin
          // Merging comments if section already exists
          CurrSection.Comment := CurrSection.Comment + aComment;
        end
        else
        begin
          CurrSection := cCHXIniSection.Create(aKey, aComment);
          SectionList.Add(CurrSection);
        end;
      end
      else
      begin
        // No, it isn't a section
        // Is it a "key=value" line?
        aPos := UTF8Pos(AssignChar, aValue);
        if aPos <> 0 then
        begin
          // Yes, it's a "key=value" line
          aKey := UTF8Trim(UTF8Copy(aValue, 1, aPos - 1));
          aValue := UTF8Trim(UTF8Copy(aValue, aPos +
            UTF8Length(AssignChar), Maxint));
        end;
        CurrSection.AddLine(aKey, aValue, aComment, MergeKeys);
      end;
    end;
    Inc(i);
  end;

  // Removing last empty lines
  RemoveTralingEmptyLines(CurrSection);
end;

procedure cCHXIni.LoadFromFile(aFilename: string);
var
  StrList: TStringList;
begin
  if aFilename = '' then
    aFilename := FileName;
  StrList := TStringList.Create;
  try
    FileName := aFilename;
    StrList.LoadFromFile(UTF8ToSys(aFilename));
    { TODO -oChixpy : Join trucated lines, if EscapeLF = true}
    FillSectionList(StrList);
  finally
    FreeAndNil(StrList);
  end;
end;

procedure cCHXIni.SaveToFile(aFilename: string);
var
  StrList: TStringList;
  CurrSection: cCHXIniSection;
  CurrLine: cCHXIniLine;
  aLine: string;
  i, j: integer;
begin
  if aFilename = '' then
    aFileName := FileName;

  StrList := TStringList.Create;
  try
    aLine := ''; // Removing compiler warning...
    i := 0;
    while i < SectionList.Count do
    begin
      CurrSection := SectionList.Items[i];
      if (i <> 0) or (CurrSection.Name <> '') then
      begin
        // aLine is holding the last line
        if (aLine <> '') and (StrList.Count > 0) then
          // Adding a empty line between sections if there is not one already
          //   or it's the first line of the file...
          StrList.Add('');
        aLine := SectionBegin + CurrSection.Name + SectionEnd;
        if CurrSection.Comment <> '' then
          aLine := aline + ' ' + CommentBegin + ' ' + CurrSection.Comment;
        StrList.Add(aLine);
      end;

      j := 0;
      while j < CurrSection.Lines.Count do
      begin
        CurrLine := CurrSection.Lines[j];
        aLine := '';
        if CurrLine.key <> '' then
          aLine := aLine + CurrLine.Key + self.AssignChar;
        if CurrLine.Value <> '' then
          aLine := aLine + CurrLine.Value;
        if CurrLine.Comment <> '' then
          if aLine <> '' then
            aLine := aLine + ' ' + CommentBegin + ' ' + CurrLine.Comment
          else
            aLine := CommentBegin + ' ' + CurrLine.Comment;
        { TODO -oChixpy : Use EscapeLF to write the file }
        { Si EscapeLF, partir la línea en varias. }
        StrList.Add(UTF8ToSys(aLine));
        Inc(j);
      end;
      Inc(i);
    end;

    FileName := aFilename;
    StrList.SaveToFile(UTF8ToSys(aFilename));
  finally
    FreeAndNil(StrList);
  end;
end;

function cCHXIni.SectionNameList: TStringList;
var
  i: integer;
begin
  Result := nil;

  if SectionCount = 0 then
    Exit;

  Result := TStringList.Create;

  i := 0;
  while i < SectionList.Count do
  begin
    Result.Add(SectionByIndex(i).Name);
    Inc(i);
  end;
end;

function cCHXIni.SectionByIndex(aIndex: integer): cCHXIniSection;
begin
  Result := SectionList.Items[aIndex];
end;

function cCHXIni.SectionByName(aSectionName: string): cCHXIniSection;
var
  i: integer;
  aSection: cCHXIniSection;
begin
  Result := nil;
  i := 0;
  while i < SectionList.Count do
  begin
    aSection := SectionByIndex(i);

    if UTF8CompareText(aSection.Name, aSectionName) = 0 then
    begin
      Result := aSection;
      break;
    end;
    Inc(i);
  end;
end;

function cCHXIni.SectionCount: integer;
begin
  Result := SectionList.Count;
end;

function cCHXIni.ReadString(
  const aSectionName, aKeyName, aDefault: string): string;
var
  aSection: cCHXIniSection;
begin
  Result := aDefault;
  aSection := SectionByName(aSectionName);
  if aSection = nil then
    Exit;
  Result := aSection.ValueByKey(aKeyName, aDefault);

  if RemoveQuotes then
    StrRemoveQuotes(Result);
end;

function cCHXIni.ReadString(const aSectionIndex: integer;
  const aKeyName, aDefault: string): string;
var
  aSection: cCHXIniSection;
begin
  Result := aDefault;
  if SectionList.Count <= aSectionIndex then
    Exit;
  aSection := Self.SectionList[aSectionIndex];
  Result := aSection.ValueByKey(aKeyName, aDefault);

  if RemoveQuotes then
    StrRemoveQuotes(Result);
end;

function cCHXIni.ReadString(const aSectionIndex, aKeyIndex: integer;
  const aDefault: string): string;
var
  aSection: cCHXIniSection;
begin
  Result := aDefault;
  if SectionList.Count <= aSectionIndex then
    Exit;
  aSection := Self.SectionList[aSectionIndex];
  if aSection.Lines.Count <= aKeyIndex then
    Exit;
  Result := aSection.Lines[aKeyIndex].Value;

  if RemoveQuotes then
    StrRemoveQuotes(Result);
end;

function cCHXIni.ReadBoolean(const aSectionName, aKeyName: string;
  const aDefault: boolean): boolean;
begin
  Result := StrToBoolDef(ReadString(aSectionName, aKeyName, ''), aDefault);
end;

function cCHXIni.ReadMultiStrings(
  const aSectionName, aKeyName, aDefault: string;
  const Separator: char): TStringList;
var
  aStr: string;
  tmpRemoveQuotes: boolean;
begin
  // TStringList Trick
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := Separator;
  tmpRemoveQuotes := RemoveQuotes;

  if Result.QuoteChar + Result.QuoteChar = QuoteBegin + QuoteEnd then
    RemoveQuotes := False; // Handled by TStringList
  aStr := ReadString(aSectionName, aKeyName, '');

  RemoveQuotes := tmpRemoveQuotes;

  if aStr = '' then
    aStr := aDefault;
  Result.DelimitedText := aStr;
end;

function cCHXIni.ReadInteger(const aSectionName, aKeyName: string;
  const aDefault: integer): integer;
begin
  Result := StrToIntDef(ReadString(aSectionName, aKeyName, ''), aDefault);
end;

function cCHXIni.ReadTPoint(const aSectionName, aKeyName: string;
  const aDefault: TPoint): TPoint;
var
  aSL: TStringList;
begin
  aSL := ReadMultiStrings(aSectionName, aKeyName, IntToStr(aDefault.x) +
    ',' + IntToStr(aDefault.y));
  try
    Result.x := StrToInt(aSL[0]);
    Result.y := StrToInt(aSL[1]);
  except
    Result.x := aDefault.x;
    Result.y := aDefault.y;
  end;
  FreeAndNil(aSL);
end;

procedure cCHXIni.WriteBoolean(const aSectionName, aKeyName: string;
  const aValue: boolean);
begin
  WriteString(aSectionName, aKeyName, BoolToStr(aValue, True));
end;

procedure cCHXIni.WriteString(const aSectionName, aKeyName: string;
  aValue: string);
var
  aSection: cCHXIniSection;
  aKey: cCHXIniLine;
begin
  // Write quotes?
  // Already have quotes?
  if (UTF8Pos(QuoteBegin, aValue) <> 1) or
    (UTF8Copy(aValue, UTF8Length(aValue) - UTF8Length(QuoteEnd) +
    1, UTF8Length(QuoteEnd)) <> QuoteEnd) then
  begin
    case Self.WriteQuotes of
      ociAutoQuotes:
      begin
        { TODO : Only spaces are tested }
        if (UTF8Copy(aValue, 1, 1) = ' ') or
          (UTF8Copy(aValue, UTF8Length(aValue), 1) = ' ') then
          aValue := self.QuoteBegin + aValue + Self.QuoteEnd;
      end;
      ociAlwaysQuotes: aValue := self.QuoteBegin + aValue + Self.QuoteEnd;
    end;
  end;

  aSection := SectionByName(aSectionName);
  if aSection = nil then
  begin
    aSection := cCHXIniSection.Create(aSectionName, '');
    SectionList.add(aSection);
  end;
  aKey := aSection.LineByKey(aKeyName);
  if aKey = nil then
    aKey := aSection.AddLine(aKeyName, aValue, '', MergeKeys)
  else
    aKey.Value := aValue;
end;

procedure cCHXIni.WriteInteger(const aSectionName, aKeyName: string;
  const aValue: integer);
begin
  WriteString(aSectionName, aKeyName, IntToStr(aValue));
end;

procedure cCHXIni.StrRemoveQuotes(var aString: string);
begin
  if UTF8Pos(QuoteBegin, aString) <> 1 then
    exit;
  if UTF8Copy(aString, UTF8Length(aString) - UTF8Length(QuoteEnd) +
    1, UTF8Length(QuoteEnd)) <> QuoteEnd then
    Exit;

  // Removing quotes
  aString := UTF8Copy(aString, UTF8Length(QuoteBegin) + 1,
    UTF8Length(aString) - UTF8Length(QuoteEnd) - UTF8Length(QuoteBegin));
end;


constructor cCHXIni.Create;
begin
  inherited Create;

  // Some default values:
  self.CommentBegin := ';';
  self.SectionBegin := '[';
  self.SectionEnd := ']';
  self.NewLine := '\n';
  self.QuoteBegin := '"';
  self.QuoteEnd := '"';
  self.AssignChar := '=';
  self.EscapeLFChar := '\';
  self.EscapeLFMaxWidth := 80;
  Self.WriteQuotes := ociAutoQuotes;

  FSectionList := cCHXIniSectionList.Create(True);
end;

destructor cCHXIni.Destroy;
begin
  FreeAndNil(FSectionList);
  inherited Destroy;
end;

initialization
  // Initializating some strings for boleean conversion...

  // Weak Pascal syntax here...
  if length(TrueBoolStrs) = 0 then
  begin
    setlength(TrueBoolStrs, 7);
    TrueBoolStrs[0] := 'True';
    TrueBoolStrs[1] := 'Yes';
    TrueBoolStrs[2] := 'Si';
    TrueBoolStrs[3] := 'Sí';
    TrueBoolStrs[4] := '1';
    TrueBoolStrs[5] := '-1';
    TrueBoolStrs[6] := 'Enabled';
  end;

  if length(FalseBoolStrs) = 0 then
  begin
    setlength(FalseBoolStrs, 4);
    FalseBoolStrs[0] := 'False';
    FalseBoolStrs[1] := 'No';
    FalseBoolStrs[2] := '0';
    FalseBoolStrs[3] := 'Disabled';
  end;
end.
