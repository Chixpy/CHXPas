program CTC040_3;
{< The Coding Train Challenge #040.3 - TF-IDF }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/RPMYV-eb6ll
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, FGL,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2FontTTF, uCHXSDL2Utils, uProcUtils,
  uaCHXSDL2Comp, ucCHXSDL2TextEdit, ucCHXSDL2Button;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640; { CHX: Window logical width. }
  WinH = 480; { CHX: Window logical height. }

  MaxFiles = 6;
  files : array[0..MaxFiles] of string = ('phadke.txt', 'rainbow.txt',
    'sports.txt', 'eclipse.txt', 'fish.txt', 'test.txt', 'tree.txt');
  FileIdx = 1; // Index of file to analize
  NWords = 20; // Number of words to show, intead of all.

type
  RCountData = record
    tf : Integer;
    df : Integer;
    tfidf : Double;
  end;

  cCountGenMap = specialize TFPGMap<string, RCountData>;

  { cCountMap }

  cCountMap = class(cCountGenMap)
    constructor Create;
    destructor Destroy; override;
  end;

  cCountList = specialize TFPGList<string>;

  { cCTCEng }

  cCTCEng = class(cCHXSDL2Engine)
  public
    procedure Setup; override;
    procedure Finish; override;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw; override;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override;

  public
    { CHX: Processing global variables and auxiliar functions. }
    counts : cCountMap;

    keys : cCountList;

  end;

  constructor cCountMap.Create;
  begin
    inherited Create;

    OnKeyCompare := @CompareText;
    //OnDataCompare := @CompareData;
  end;

  destructor cCountMap.Destroy;
  begin
    inherited Destroy;
  end;

  { cCTCEng }
var
  // CHX: This must be here, because Comparetfidf must to have access to
  //   CTCEng.counts for sorting.
  CTCEng : cCTCEng;

  function Comparetfidf(const Item1, Item2 : string) : Integer;
  var
    Res : Double;
  begin
    Res := CTCEng.counts[Item2].tfidf - CTCEng.counts[Item1].tfidf;
    if Res > 0 then
      Result := 1
    else if Res < 0 then
      Result := -1
    else
      Result := 0;
  end;

  procedure cCTCEng.Setup;
  var
    allwords : TStringArray;
    tokens : array of TStringArray;
    Lines : TStringList;
    i : Integer;
    aWord : string;
    aData : RCountData;
  begin
    counts := cCountMap.Create;
    counts.sorted := True; // Faster key search
    keys := cCountList.Create;

    // Reading all files and converting to strings
    SetLength(allwords, length(files));
    Lines := TStringList.Create;
    for i := 0 to MaxFiles do
    begin
      Lines.LoadFromFile('Data/' + files[i]);
      allwords[i] := Lines.Text;
    end;
    FreeAndNil(Lines);

    // CHX: Changed a little original code.

    // CHX: Spliting tokens from all files instead only the one to analize.
    SetLength(tokens, length(files));
    for i := 0 to MaxFiles do
      tokens[i] := allwords[i].Split([' ', ',', '?', '!', '.', '[',
        ']', #10, #13, ';', '"', '-', '(', ')'],
        TStringSplitOptions.ExcludeEmpty);

    // Searching keys from file to analize
    for aWord in tokens[FileIdx] do
    begin
      if counts.IndexOf(aWord) < 0 then
      begin
        keys.Add(aWord);
        aData.tf := 1;
        aData.df := 1;
        aData.tfidf := 0;
      end
      else
      begin
        aData := counts[aWord];
        aData.tf := aData.tf + 1;
      end;
      counts[aWord] := aData;
    end;

    // Counting words from all files
    for i := 0 to MaxFiles do
      if i <> FileIdx then // CHX: Skipping analized file
      begin
        for aWord in tokens[i] do
        begin
          if counts.IndexOf(aWord) >= 0 then
          begin
            aData := counts[aWord];
            aData.df := aData.df + 1;
            counts[aWord] := aData;
          end;
        end;
      end;

    // Calculating tfidf
    for aWord in keys do
    begin
      aData := counts[aWord];
      aData.tfidf := aData.tf * Ln(length(files) / aData.df);
      //aData.tfidf := aData.tf * (length(files) / aData.df);
      counts[aWord] := aData;
    end;

    // Sorting words by tfidf
    keys.Sort(@Comparetfidf);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    counts.Free;
    keys.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  begin
    if ExitProg then Exit;
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

  end;

  procedure cCTCEng.Draw;
  var
    i, aCount : Integer;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    aCount := Min(NWords, keys.Count - 1);
    for i := 0 to aCount do
      DefFont.RenderDynStr(keys[i] + ': ' + counts[keys[i]].tfidf.ToString,
        10, DefFont.LinePosY(i, 10));
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    inherited HandleEvent(aEvent, Handled, ExitProg);
    { CHX: Inherited HandleEvent can change ExitProg and Handled. }
    if ExitProg or Handled then Exit;

    { CHX: Some common events for fast reference, CTRL+MAYS+U removes comments
        while selecting the block.
      You can see full list in sdlevents.inc
      Window and general quit events are handled automatically in parent.
      Escape key is mapped to exit the program too.
      When editing text, all keys are handled too until Return is pressed (or
        other event disables it).
    }

    //case aEvent.type_ of
    //  SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
    //  begin
    //    case aEvent.key.keysym.sym of
    //      //SDLK_UP : ;
    //      //SDLK_DOWN : ;
    //      //SDLK_LEFT : ;
    //      //SDLK_RIGHT : ;
    //      //SDLK_SPACE : ;
    //      //SDLK_RETURN : ;
    //    end;
    //  end;
    //    //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
    //    //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
    //    //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
    //    //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);
    //  else
    //    ;
    //end;
  end;

  { Main program }

var
  BaseFolder : string;

  {$R *.res}

begin
  // Changing base folder to parents .exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  try
    //CTCEng := cCTCEng.Create(ApplicationName, 'SDL2.ini');
    CTCEng := cCTCEng.Create(ApplicationName, WinW, WinH, True, False);
    CTCEng.Config.DefFontSize := WinH div 30;
    // Actually, they are less than 30 lines because of LineHeight
    CTCEng.Config.DefFontColor := SDLColor(255, 255, 255, 255);
    CTCEng.Config.DefFontFile := 'FreeMonoBold.ttf';
    CTCEng.ShowFrameRate := True;
    CTCEng.Init;
    CTCEng.Run;
  finally
    FreeAndNil(CTCEng);
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
