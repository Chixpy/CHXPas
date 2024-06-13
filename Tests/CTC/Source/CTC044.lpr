program CTC044;
{< The Coding Train Challenge #044 - Title }

// AFINN 11 Sentiment Analysis
// The Coding Train / Daniel Shiffman
// https://thecodingtrain.com/
// Part 1: https://youtu.be/uw3GbsY_Pbc
// Part 2: https://youtu.be/VV1JmMYceJw

// https://editor.p5js.org/codingtrain/sketches/aNeMdpy-b
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

type
  cGenStrMap = specialize TFPGMap<string, Integer>;

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
    afinn : cStrMap;

    sTextInput : string;
    scoredwords : string;

    totalScore : Integer;
    NWords : Integer; // CHX: Number of words in sTextInput
  end;

  constructor cStrMap.Create;
  begin
    inherited Create;
    OnKeyCompare := @CompareText; // Case insensitive
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    aFile, slTemp : TStringList;
    aLine : string;
  begin
    { CHX: Creating a Map (HashMap/Dictionary/etc. languaje dependeant) of
        words from txt file, instead of JSON; Is easier to parse. }
    afinn := cStrMap.Create;

    aFile := TStringList.Create;
    aFile.LoadFromFile('Data\AFINN-111.txt');

    slTemp := TStringList.Create;
    for aLine in aFile do
    begin
      slTemp.Clear;
      slTemp.AddDelimitedText(aLine, #9, True);
      if slTemp.Count = 2 then
        afinn[slTemp[0]] := StrToInt(slTemp[1]);
    end;
    slTemp.Free;
    aFile.Free;

    // Well, we use cCHXSDL2Button too
    TextInput(DefFont, sTextInput, 10, 10, WinW - 10, True);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    afinn.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    words : TStringArray;
    word : string;
  begin
    if ExitProg then Exit;
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

    words := sTextInput.Split([' ', ',', '?', '!', '.', '[',
      ']', #10, #13, ';', '"'], TStringSplitOptions.ExcludeEmpty);

    scoredwords := '';
    totalScore := 0;

    for word in words do
    begin
      if afinn.IndexOf(word) >= 0 then
      begin
        totalScore += afinn[word];
        scoredwords += word + ': ' + afinn[word].ToString + '; ';
      end;
    end;

    NWords := Length(words);
  end;

  procedure cCTCEng.Draw;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    rectangleColor(SDL2R, 8, 8, WinW - 8, DefFont.LinePosY(1, 11), $FF808080);

    DefFont.RenderDynStr('score: ' + totalScore.ToString, 10,
      DefFont.LinePosY(3, 10));
    if NWords <> 0 then // CHX: Division by 0 when empty.
    DefFont.RenderDynStr('comparative: ' + (totalScore / NWords).ToString,
      10, DefFont.LinePosY(4, 10));
    DefFont.RenderDynStr(scoredwords, 10, DefFont.LinePosY(5, 10), WinW - 10);

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
        if not IsEditingText then
          TextInput(DefFont, sTextInput, 10, 10, WinH - 10, True);
        //SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);
      else
        ;
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
