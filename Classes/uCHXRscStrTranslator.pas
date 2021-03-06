{ Copyright (C) 2004-2010 V.I.Volchenko and Lazarus Developers Team

  Changes for Emuteca by Chixpy

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{ This unit is needed for using translated form strings made by Lazarus IDE.
It searches for translated .po/.mo files in some common places. If you need
to have .po/.mo files anywhere else, don't use this unit but initialize
LRSMoFile variable from LResources in your project by yourself.
If you need standard translation, just use this unit in your project and enable
i18n in project options.

Another reason for including this unit may be using translated LCL messages.
This unit localizes LCL too, if it finds lclstrconsts.xx.po/lclstrconsts.xx.mo
in directory where your program translation files are placed.

Emuteca changes:
  - Cleaning a little
  - ExtractFileName(ParamStrUTF8(0)) --> kCDTName = 'Emuteca'
}
unit uCHXRscStrTranslator;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, GetText, Controls, typinfo,
  FileUtil, LCLProc,
  Translations, Forms;

const
  kCDTName = 'Emuteca';

type
  TMOTranslator = class(TAbstractTranslator)
  private
    FMOFile: TMOFile;

  public
    constructor Create(MOFileName: string);
    destructor Destroy; override;

    procedure TranslateStringProperty(Sender: TObject;
      const Instance: TPersistent; PropInfo: PPropInfo;
      var Content: string); override;
  end;

  TPOTranslator = class(TAbstractTranslator)
  private
    FPOFile: TPOFile;

  public
    constructor Create(POFileName: string);
    destructor Destroy; override;

    procedure TranslateStringProperty(Sender: TObject;
      const Instance: TPersistent; PropInfo: PPropInfo;
      var Content: string); override;
  end;

implementation

uses
  Menus;

type
  TPersistentAccess = class(TPersistent);

function FindLocaleFileName(LCExt: string): string;

  function GetLocaleFileName(const LangID, LCExt: string): string;

    function SearchFile(const LangID, LCExt, ProgramName: string): string;
    var
      TmpStr: string;
    begin
      TmpStr := IncludeTrailingPathDelimiter(ProgramDirectory);

      Result := TmpStr + LangID + DirectorySeparator + ProgramName + LCExt;
      if FileExistsUTF8(Result) then
        Exit;

      Result := TmpStr + 'languages' + DirectorySeparator +
        LangID + DirectorySeparator + ProgramName + LCExt;
      if FileExistsUTF8(Result) then
        Exit;

      Result := TmpStr + 'locale' + DirectorySeparator + LangID +
        DirectorySeparator + ProgramName + LCExt;
      if FileExistsUTF8(Result) then
        Exit;

      Result := TmpStr + 'locale' + DirectorySeparator + LangID +
        DirectorySeparator + 'LC_MESSAGES' + DirectorySeparator +
        ProgramName + LCExt;
      if FileExistsUTF8(Result) then
        Exit;

    {$IFDEF UNIX}
      //In unix-like systems we can try to search for global locale
      Result := '/usr/share/locale/' + LangID + '/LC_MESSAGES/' +
        ProgramName + LCExt;
      if FileExistsUTF8(Result) then
        Exit;
    {$ENDIF}

      Result := '';
    end;

  var
    PrgName: string;
    PrgNameCleaned: string;
    aPos: integer;
    LangShortID: string;
  begin
    Result := '';
    if length(LangID) < 2 then
      exit;

    PrgName := ExtractFileNameWithoutExt(ExtractFileNameOnly(
      ApplicationName));
    PrgNameCleaned := PrgName;
    aPos := Pos('-', PrgName);
    if aPos <> 0 then
      PrgNameCleaned := Copy(PrgName, 1, aPos - 1);
    LangShortID := copy(LangID, 1, 2);

    Result := SearchFile(LangID, LCExt, PrgName);
    if Result <> '' then
      Exit;
    Result := SearchFile(LangID, LCExt, PrgNameCleaned);
    if Result <> '' then
      Exit;
    Result := SearchFile(LangShortID, LCExt, PrgName);
    if Result <> '' then
      Exit;
    Result := SearchFile(LangShortID, LCExt, PrgNameCleaned);
    if Result <> '' then
      Exit;

    Result := '';
  end;

var
  Lang, FallBackLang: string;
  i: integer;
begin
  // Happy compiler now
  Result := '';
  Lang := '';
  FallBackLang := '';

  for i := 1 to Paramcount - 1 do
    if (ParamStrUTF8(i) = '--LANG') or (ParamStrUTF8(i) = '-l') or
      (ParamStrUTF8(i) = '--lang') then
      Lang := ParamStrUTF8(i + 1);

  // May be the enviroment variable...
  if Lang = '' then
    Lang := GetEnvironmentVariableUTF8('LANG');

  if Lang = '' then  // Getting system language
    LCLGetLanguageIDs(Lang, FallBackLang);

  // Searching locale file...
  Result := GetLocaleFileName(Lang, LCExt);
end;

function GetIdentifierPath(Sender: TObject; const Instance: TPersistent;
  PropInfo: PPropInfo): string;
var
  Tmp: TPersistent;
  Component: TComponent;
  Reader: TReader;
begin
  Result := '';
  if (PropInfo = nil) or (SysUtils.CompareText(PropInfo^.PropType^.Name,
    'TTRANSLATESTRING') <> 0) then
    exit;

  // do not translate at design time
  // get the component
  Tmp := Instance;
  while Assigned(Tmp) and not (Tmp is TComponent) do
    Tmp := TPersistentAccess(Tmp).GetOwner;
  if not Assigned(Tmp) then
    exit;
  Component := Tmp as TComponent;
  if (csDesigning in Component.ComponentState) then
    exit;

  if not (Sender is TReader) then
    exit;
  Reader := TReader(Sender);
  if Reader.Driver is TLRSObjectReader then
    Result := TLRSObjectReader(Reader.Driver).GetStackPath
  else
    Result := Instance.ClassName + '.' + PropInfo^.Name;
  Result := UpperCase(Result);
end;

var
  lcfn: string;

{ TMOTranslator }

constructor TMOTranslator.Create(MOFileName: string);
begin
  inherited Create;
  FMOFile := TMOFile.Create(UTF8ToSys(MOFileName));
end;

destructor TMOTranslator.Destroy;
begin
  FMOFile.Free;
  //If someone will use this class incorrectly, it can be destroyed
  //before Reader destroying. It is a very bad thing, but in THIS situation
  //in this case is impossible. Maybe, in future we can overcome this difficulty
  inherited Destroy;
end;

procedure TMOTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  s: string;
begin
  if Assigned(FMOFile) then
  begin
    s := GetIdentifierPath(Sender, Instance, PropInfo);
    if s <> '' then
    begin
      s := FMoFile.Translate(s + #4 + Content);

      if s = '' then
        s := FMOFile.Translate(Content);

      if s <> '' then
        Content := s;
    end;
  end;
end;

{ TPOTranslator }

constructor TPOTranslator.Create(POFileName: string);
begin
  inherited Create;
  FPOFile := TPOFile.Create(UTF8ToSys(POFileName));
end;

destructor TPOTranslator.Destroy;
begin
  FPOFile.Free;
  //If someone will use this class incorrectly, it can be destroyed
  //before Reader destroying. It is a very bad thing, but in THIS situation
  //in this case is impossible. May be, in future we can overcome this difficulty
  inherited Destroy;
end;

procedure TPOTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  s: string;
begin
  if Assigned(FPOFile) then
  begin
    s := GetIdentifierPath(Sender, Instance, PropInfo);
    if s <> '' then
    begin
      s := FPOFile.Translate(s, Content);

      if s <> '' then
        Content := s;
    end;
  end;
end;

var
  Dot1: integer;
  LCLPath: string;
  LocalTranslator: TAbstractTranslator;

initialization
  //It is safe to place code here as no form is initialized before unit
  //initialization made

  LocalTranslator := nil;
  // search first po translation resources
  try
    lcfn := FindLocaleFileName('.po');
    if lcfn <> '' then
    begin
      Translations.TranslateResourceStrings(lcfn);
      LCLPath := ExtractFileName(lcfn);
      Dot1 := pos('.', LCLPath);
      if Dot1 > 1 then
      begin
        Delete(LCLPath, 1, Dot1 - 1);
        LCLPath := ExtractFilePath(lcfn) + 'lclstrconsts' + LCLPath;
        Translations.TranslateUnitResourceStrings('LCLStrConsts', LCLPath);
      end;
      LocalTranslator := TPOTranslator.Create(lcfn);
    end;
  except
    lcfn := '';
  end;

  if lcfn = '' then
  begin
    // try now with MO traslation resources
    try
      lcfn := FindLocaleFileName('.mo');
      if lcfn <> '' then
      begin
        GetText.TranslateResourceStrings(UTF8ToSys(lcfn));
        LCLPath := ExtractFileName(lcfn);
        Dot1 := pos('.', LCLPath);
        if Dot1 > 1 then
        begin
          Delete(LCLPath, 1, Dot1 - 1);
          LCLPath := ExtractFilePath(lcfn) + 'lclstrconsts' + LCLPath;
          if FileExistsUTF8(LCLPath) then
            GetText.TranslateResourceStrings(UTF8ToSys(LCLPath));
        end;
        LocalTranslator := TMOTranslator.Create(lcfn);
      end;
    except
      lcfn := '';
    end;
  end;

  if LocalTranslator <> nil then
    LRSTranslator := LocalTranslator;

finalization
  LocalTranslator.Free;

end.
