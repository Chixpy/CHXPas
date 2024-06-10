program CTC040_2;{< Base program for NOTC and CT <- Remove this on examples

   The Coding Train Challenge #00X - Title }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/fxQ0B6BkfKo (Processing)
// Code for: https://youtu.be/unm0BLor8aE (P5.js)
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

type

  IntGenMap = specialize TFPGMap<string, Integer>;

  { IntMap }

  IntMap = class(IntGenMap)
    constructor Create;
    destructor Destroy; override;
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
    counts : IntMap;

    aTextTex : PSDL_Texture;
  end;

  { IntMap }

  constructor IntMap.Create;
  begin
    inherited Create;
    OnKeyCompare := @CompareText; //case insesitive compare
  end;

  destructor IntMap.Destroy;
  begin
    inherited Destroy;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    Lines : TStringList;
    allwords, aWord : string;
    tokens : TStringArray;
    X, Y, NTimes, i : Integer;
  begin
    // Rendering to a texture, so we can draw it every frame and then
    //   the text is not erased when window is resized or moved.
    aTextTex := SDL_CreateTexture(SDL2R, PWinPxFmt^.format,
      SDL_TEXTUREACCESS_TARGET, WinW, WinH);
    SDL_SetRenderTarget(SDL2R, aTextTex);

    counts := IntMap.Create;
    counts.sorted := True; // Faster key search

    Lines := TStringList.Create;
    Lines.LoadFromFile('Data/rainbow.txt');

    allwords := Lines.Text;

    tokens := allwords.Split([' ', ',', '?', '!', '.', '[',
      ']', #10, #13, ';', '"'], TStringSplitOptions.ExcludeEmpty);

    for aWord in tokens do
    begin
      if counts.IndexOf(aWord) < 0 then
        counts[aWord] := 1 
      else
        counts[aWord] := counts[aWord] + 1;
    end;

    // CHX: With TFPGMap, we can iterate over keys without knowing them
    for i := 0 to (counts.Count - 1) do
    begin
      aWord := counts.Keys[i];
      NTimes := counts.Data[i];

      { CHX: With cCHXSDL2FontGFX (SDL2_GFX) we can not change the size,
          and with cCHXSDL2FontTTF (SDL2_TTF) is not recommended change it
          continuously.

        But in this case we are drawing in Setup.
      }
      DefFont.ChangeFontStyle(DefFont.Color, NTimes);

      X := RandomRange(0, WinW - (NTimes * aWord.Length));
      Y := RandomRange(0, WinH - DefFont.LineHeight);

      DefFont.RenderDynStr(aWord, X, Y);
    end;

    Lines.Free;

    SDL_SetRenderTarget(SDL2R, nil);
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    counts.Free;
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
    //SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    //SDL_RenderClear(SDL2R);

    SDL_RenderCopy(SDL2R,aTextTex,nil,nil);
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
