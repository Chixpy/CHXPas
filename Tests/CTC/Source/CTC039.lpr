program CTC039;
{< The Coding Train Challenge #039 - Mad Libs Generator }

// Mad Libs!
// Daniel Shiffman
// https://thecodingtrain.com/challenges/39-madlibs-generator
// https://youtu.be/ziBO-U2_t3k
// https://editor.p5js.org/codingtrain/sketches/8gVGg0VR3
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{ CHX: We can retrieve a URL content in many ways, and the simplest
    is using TFPHTTPClient.SimpleGet in FPHTTPClient unit,
    but we will use a local cvs file.

  Original URL of the challenge:
  https://docs.google.com/spreadsheets/d/e/2PACX-1vSiJDczupcvlAJxd70RJ9hZina9cqweCiTj1EkYrH_17FhFBjdMFTEY2TOMmhwGBHGR05y7QRXLNbo6/pub?output=csv

  Its contents were copied to `bin/data/madlibs.csv`
}

{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image, RegExpr,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2FontTTF, uCHXSDL2Utils, uProcUtils,
  uaCHXSDL2Comp, ucCHXSDL2TextEdit, ucCHXSDL2Button;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640; { CHX: Window logical width. }
  WinH = 480; { CHX: Window logical height. }

  txt = '$$Exclamation$$! they said $$Adverb$$ as they jumped into their ' +
    '$$Noun$$ and flew off with their $$Adjective$$ $$PluralNoun$$.';


type

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
    bGenerate: cCHXSDL2Button;

    data: TStringList;

    ReplStr : string;

    procedure generate(const Sender : caCHXSDL2Comp);
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  begin
    data:= TStringList.Create;
    data.LoadFromFile('data/madlibs.csv');
    data.Delete(0); // CHX: Removing header

    bGenerate := cCHXSDL2Button(AddComponent(
      cCHXSDL2Button.Create(10,10, DefFont,'bGen', 'Generate')));
    bGenerate.OnClick := @generate;
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    data.Free;
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

    DefFont.RenderDynStr(ReplStr, 10, 100, WinW - 10);
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

  procedure cCTCEng.generate(const Sender : caCHXSDL2Comp);
  var
    seldata: TStringList;
  begin
    seldata := TStringList.Create;
    seldata.StrictDelimiter := True;
    seldata.CommaText := data[random(data.Count)];

    while seldata.Count < 6 do seldata.Add('Error');

    // CHX: Sorry, I'll do it in a dirty way

    // Using TRegExpr is something like
    //   re:= TRegExpr.Create('\$\$(.*?)\$\$');
    //   ReplStr := re.ReplaceEx(txt, @replacer);
    //   re.free;
    // but cCTCEng.replacer(ARegExpr : TRegExpr) : RegExprString; will be a
    //   little more complex.

    ReplStr := txt;
    ReplStr := ReplStr.Replace('$$Exclamation$$', seldata[1]);
    ReplStr := ReplStr.Replace('$$Adverb$$', seldata[2]);
    ReplStr := ReplStr.Replace('$$Noun$$', seldata[3]);
    ReplStr := ReplStr.Replace('$$Adjective$$', seldata[4]);
    ReplStr := ReplStr.Replace('$$PluralNoun$$', seldata[5]);

    seldata.free;
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
