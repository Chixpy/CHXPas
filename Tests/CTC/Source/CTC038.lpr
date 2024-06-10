program CTC038;
{< The Coding Train Challenge #038 - Word Interactor }

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/AKuW48WeNMA
// Port: (C) 2024 Chixpy https://github.com/Chixpy

{ CHX: Original Challenge makes every word from of text with a RegEx and
    write it making every word interactive with JavaScript events with span
    tags.

  Instead of it, I will make buttons with every word. }
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

    aText : string;
    teText : cCHXSDL2TextEdit;
    bSubmit : cCHXSDL2Button;

    btnPressed : string; // To draw pressed button caption.

    procedure CreateGUI;  // To easy clear all word buttons
    procedure ButtonPressed(const Sender : caCHXSDL2Comp);
    //< Callback for word buttons

    procedure NewText(const Sender : caCHXSDL2Comp);
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  begin
    aText := 'An example text, but as always, only english characters. ' +
      'En español, por ejemplo.';
    CreateGUI;
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
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    // Showing pressed button text
    DefFont.RenderDynStr('Word pressed: ' + btnPressed, 10,
      WinH - DefFont.LineHeight - 10);
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

  procedure cCTCEng.CreateGUI;
  begin
    btnPressed := '';
    CompList.Clear;

    teText := cCHXSDL2TextEdit.Create(DefFont, 10, 10, WinW - 20);
    teText.Value := aText;
    AddComponent(teText);

    bSubmit := cCHXSDL2Button.Create(10, teText.Height + 15, DefFont,
      'Submit', 'Submit');
    bSubmit.OnClick := @NewText;
    AddComponent(bSubmit);
  end;

  procedure cCTCEng.NewText(const Sender : caCHXSDL2Comp);
  var
    words : TStringList;
    i, X, Y : Integer;
    aButton : cCHXSDL2Button;
  begin
    words := TStringList.Create;
    { CHX: FPC has at least 2 RegEx engines included in the units:
        RegExpr and RegEx. (uRegExpr and OldRegExpr units are wrappers of
        RegExpr and RegEx respectively).
    }
    aText := teText.Value;

    CreateGUI;

    SplitRegExpr('(\W+)', aText, words);

    // Creating word buttons
    X := 10;
    Y := teText.Height + bSubmit.Height + 50;
    for i := 0 to (words.Count - 1) do
    begin
      if words[i] <> '' then // CHX: It can have empty strings
      begin
        aButton := cCHXSDL2Button.Create(X, Y, DefFont, i.ToString + words[i],
          words[i]);
        aButton.OnClick := @ButtonPressed;

        // Ops, right margin
        X := X + aButton.Width + 15;
        if X > WinW then
        begin
          X := 10;
          Y := Y + aButton.Height + 15;
          aButton.X := X;
          aButton.Y := Y;
          X := X + aButton.Width + 15;
        end;

        AddComponent(aButton);
      end;
    end;

    words.Free;
  end;

  procedure cCTCEng.ButtonPressed(const Sender : caCHXSDL2Comp);
  begin
    if Sender is cCHXSDL2Button then
      btnPressed := cCHXSDL2Button(Sender).Caption;
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
