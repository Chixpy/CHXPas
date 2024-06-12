program CTC042_1;
{< The Coding Train Challenge #042.1 - Markov Chain Name Generator }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/eGFJ8vugIWA
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, FGL,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image, sdl2_mixer,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2FontTTF, uCHXSDL2Utils, uProcUtils,
  uaCHXSDL2Comp, ucCHXSDL2TextEdit, ucCHXSDL2Button;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640; { CHX: Window logical width. }
  WinH = 480; { CHX: Window logical height. }

  txt = 'The unicorn is a legendary creature that has been described ' +
    'since antiquity as a beast with a large, pointed, spiraling horn ' +
    'projecting from its forehead. The unicorn was depicted in ancient ' +
    'seals of the Indus Valley Civilization and was mentioned by the' +
    'ancient Greeks in accounts of natural history by various writers, ' +
    'including Ctesias, Strabo, Pliny the Younger, and Aelian.[1] ' +
    'The Bible also describes an animal, the re''em, which some ' +
    'translations have erroneously rendered with the word unicorn.[1] ' +
    'In European folklore, the unicorn is often depicted as a white ' +
    'horse-like or goat-like animal with a long horn and cloven hooves ' +
    '(sometimes a goat''s beard). In the Middle Ages and Renaissance, ' +
    'it was commonly described as an extremely wild woodland creature, ' +
    'a symbol of purity and grace, which could only be captured by a ' +
    'virgin. In the encyclopedias its horn was said to have the power ' +
    'to render poisoned water potable and to heal sickness. In medieval ' +
    'and Renaissance times, the tusk of the narwhal was sometimes sold ' +
    'as unicorn horn.';
  order = 3;

type
  cCharList = specialize TFPGList<char>;

  cGenStrMap = specialize TFPGMapObject<string, cCharList>;

  { cStrMap }

  cStrMap = class(cGenStrMap)
    constructor Create;
  end;


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
    ngrams : cStrMap;

    MarkovStr : string;

    ErrStr : string; // Show error, if happens.

    procedure MarkovIt;
  end;

  { cStrmap }

  constructor cStrMap.Create;
  begin
    inherited Create(True);
    OnKeyCompare := @CompareText;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    i : Integer;
    gram : string;
  begin
    MarkovStr := '';
    ngrams := cStrMap.Create;

    i := 1; //CHX: Pascal strings begin at 1
    while i <= txt.Length - order do
    begin
      //CHX: This begins at 0!!! It uses: Copy(AStartIndex+1,ALen);
      gram := txt.Substring(i - 1, order);

      if ngrams.IndexOf(gram) < 0 then
        ngrams.Add(gram, cCharList.Create);

      // CHX: GitHub code (both P5 and Processing) didn't add the
      //   first posibility. After rewatching the video, it's added.
      ngrams[gram].Add(txt[i + order]);

      Inc(i);
    end;
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    ngrams.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  begin
    if ExitProg then Exit;
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

  end;

  procedure cCTCEng.Draw;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    DefFont.RenderDynStr('Click to generate a Markov chain.', 10, 10);
    DefFont.RenderDynStr(MarkovStr, 10, DefFont.LinePosY(2, 10), WinW - 10);

    DefFont.RenderDynStr(ErrStr, 10, WinH - DefFont.LineHeight);
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

    case aEvent.type_ of
      //SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      //begin
      //  case aEvent.key.keysym.sym of
      //    //SDLK_UP : ;
      //    //SDLK_DOWN : ;
      //    //SDLK_LEFT : ;
      //    //SDLK_RIGHT : ;
      //    //SDLK_SPACE : ;
      //    //SDLK_RETURN : ;
      //  end;
      //end;
      //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
        MarkovIt;
        //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);
      else
        ;
    end;
  end;

  procedure cCTCEng.MarkovIt;
  var
    currentGram : string;
    i : Integer;
    possibilities : cCharList;
    Next : char;
    len : Integer;
  begin
    ErrStr := '';
    currentGram := txt.substring(0, order);
    MarkovStr := currentGram;

    for i := 1 to (txt.Length - order) do
    begin
      // CHX: This can happen with ending gram, unless it already exists.
      if ngrams.IndexOf(currentGram) < 0 then
      begin
        ErrStr := '''' + currentGram + ''' not found.';
        break;
      end;
      possibilities := ngrams[currentGram];
      // CHX: This only happens with GitHub code logic.
      if possibilities.Count = 0 then
      begin
        ErrStr := '''' + currentGram + ''' without possibilities.';
        break;
      end;

      Next := possibilities[Random(possibilities.Count)];
      MarkovStr += Next;
      len := MarkovStr.Length;
      currentGram := MarkovStr.substring(len - order, order);
    end;
  end;

  { Main program }

var
  BaseFolder : string;
  CTCEng : cCTCEng;

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
