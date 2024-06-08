program CTC037;
{< The Coding Train Challenge #037 - Diastic Machine }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/u-HUtrpyi1c

// Modifications had to be made to port into Processing
// Help for getting input in Processing: http://learningprocessing.com/examples/chp18/example-18-01-userinput
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2FontTTF, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 800; { CHX: Window logical width. }
  WinH = 600; { CHX: Window logical height. }

type

  { cCTCEng }

  cCTCEng = class(cCHXSDL2Engine)
  protected
    procedure Setup; override;
    procedure Finish; override;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw; override;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override;

    function diastic(aSeed : string; words : TStringArray) : string;

  public
    { CHX: Processing global variables. }
    seed : string;
    words : TStringArray;
    phrase : string;
  end;

  { cCTCEng }
  function cCTCEng.diastic(aSeed : string; words : TStringArray) : string;
  var
    currentWord, i, j : Integer;
    c : char;
  begin
    Result := '';
    currentWord := 0;

    { CHX: Processing port of this function is wrong, Initial values
        of both "for" are swaped.

      So original P5 version values is used here. }

    { CHX: First char of a string in Pascal is at position 1.
      Position 0 had a special meaning in old pascal, returning the
      string length when strings were limited to 255 chars }
    for i := 1 to aSeed.Length do
    begin
      c := aSeed[i];

      for j := currentWord to Length(words) - 1 do
      begin
        if (words[j].length >= i + 1) and (words[j][i] = c) then
        begin
          Result += words[j] + ' ';
          currentWord := j + 1;
          Break; // CHX: Ough...
        end;
      end;
    end;
  end;

  procedure cCTCEng.Setup;
  var
    aFile : TStringList;
    srctxt : string;
  begin
    aFile := TStringList.Create;
    aFile.LoadFromFile('Data\rainbow.txt');

    { CHX: Joining lines... but with #10#13 or #10 or #13, system dependant. }
    srctxt := aFile.Text;
    srctxt := srctxt.Replace(#10, ' ', [rfReplaceAll, rfIgnoreCase]);
    srctxt := srctxt.Replace(#13, ' ', [rfReplaceAll, rfIgnoreCase]);
    // Removing duplicated spaces
    srctxt := srctxt.Replace('  ', ' ', [rfReplaceAll, rfIgnoreCase]);
    srctxt := srctxt.Replace('  ', ' ', [rfReplaceAll, rfIgnoreCase]);

    words := srctxt.Split([' ', ',', '?', '!', '.', '[', ']'],
      TStringSplitOptions.ExcludeEmpty);

    aFile.Free;

    seed := '';
    phrase := '';

    TextInput(DefFont, seed, DefFont.LinePosX('Seed: ', 25),
      DefFont.LinePosY(3, 40));
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }

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
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    SDL_RenderClear(SDLWindow.PRenderer);

    DefFont.RenderDynStr('Enter your seed and hit enter to generate!', 25, 40);
    DefFont.RenderDynStr('Hit enter again to enter another seed!',
      25, DefFont.LinePosY(1, 40));
    DefFont.RenderDynStr('Seed: ' + seed, 25, DefFont.LinePosY(3, 40));
    DefFont.RenderDynStr(phrase, 25, DefFont.LinePosY(5, 40), WinW - 25);
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
      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          //SDLK_UP : ;
          //SDLK_DOWN : ;
          //SDLK_LEFT : ;
          //SDLK_RIGHT : ;
          //SDLK_SPACE : ;
          SDLK_RETURN, SDLK_KP_ENTER :
          begin
            if phrase = '' then
              phrase := diastic(seed, words)
            else // CHX: Enter another word after generated one
            begin
              phrase := '';
              seed := '';
              if not IsEditingText then
                TextInput(DefFont, seed, DefFont.LinePosX('Seed: ', 25),
                  DefFont.LinePosY(3, 40));
            end;
            Handled := True;
          end;
        end;
      end;
        //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
        //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
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
    CTCEng := cCTCEng.Create(ApplicationName, WinW, WinH, True, False);
    CTCEng.Config.DefFontSize := WinH div 25;
    // Actually,they are less than 25 lines because of LineHeight
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
