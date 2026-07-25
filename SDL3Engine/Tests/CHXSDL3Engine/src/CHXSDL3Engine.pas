program CHXSDL3Engine;
{<
  A simple program with cCHXSDL3Engine.

  Shows a simple text with native SDL methods.

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implented in it's own unit.
}
{$mode ObjFPC}{$H+}
uses
  CTypes, SDL3, ucCHXSDL3Engine;

const
  // Renderer scales images to actual size of the window.
  WinW = 640; { Renderer width. }
  WinH = 480; { Renderer height. }

type

  { cSDL3Eng }

  cSDL3Eng = class(cCHXSDL3Engine)
  protected
    procedure Setup; override; { It's abstract. }
    procedure Finish; override; { It's abstract. }
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override; { It's abstract. }
    procedure Draw; override; { It's abstract. }
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override; { It's virtual. }

  public
    {
      Declaration of "global" variables and auxiliar methods.
    }
  end;

  { cSDL3Eng }
  procedure cSDL3Eng.Setup;
  begin
    {
      Code executed before enter the engine's loop.
    }
    ShowFrameRate := True;
  end;

  procedure cSDL3Eng.Finish;
  begin
    {
      Code executed after exiting the engine's loop.
    }
  end;

  procedure cSDL3Eng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  begin
    {
      Step frame logic.
        - FrameTime: Time passed after last frames.
        - ExitProg: if set to True, then Exit the program.
    }
  end;

  procedure cSDL3Eng.Draw;
  begin
    {
      Draw frame logic.
    }
    // Using SDL native functions
    SDL_SetRenderDrawColor(Window.PSDLRenderer, 80, 80, 80, 255);
    
    // Actually, SDLRenderer := Window.PSDLRenderer as a shorcut.
    // It can be changed if multiple windows are created.
    SDL_RenderClear(SDLRenderer);
    SDL_SetRenderDrawColor(SDLRenderer, 196, 196, 0, 255);
    SDL_RenderDebugText(SDLRenderer, WinW * 0.5, WinH * 0.5, 'Hello, World!');
    SDL_RenderDebugText(SDLRenderer, 10 , 10, 'Press any letter to Exit.');
  end;

  procedure cSDL3Eng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    {
      Handle events: keyboard, mouse, sensors, joysticks, system, etc.

      - aEvent: SDL event, each frame this method is called for each event in
        the queue.
      - Handled: If true, aEvent it's already handled.
      - ExitProg: If set to True, then Exit the program.

      Parent's method handles some Window and Exit/Quit events. Some keys are
        handled too by default:
      - ESC: Exits the program.
      - F11: Toggle framerate display.
      - While text editing: All characters and simbols are handled.
      It's recommended call inherited method first. But you can call it last
        if you want handle manually them.
    }

    inherited;
    if ExitProg or Handled then Exit;

    {
      You can see full list in SDL_events.inc of SDL3.
    }

    case aEvent.type_ of
      SDL_EVENT_KEY_DOWN:
      begin
        case aEvent.key.key of
          SDLK_A..SDLK_Z:
          begin
            ExitProg := True;
            Handled := True;
          end;
          otherwise
            ;
        end;
      end;
      otherwise
        ;
    end;
  end;

  { Main program }

var
  CTCEng : cSDL3Eng;

begin
  CTCEng := cSDL3Eng.Create('CHXSDL3Engine Test', WinW, WinH);
  try
    // We can change configuration, call init and then run the engine...
    //CTCEng.Config.FullScreen := True;
    //CTCEng.Init; // .. but AutoInit is set True by default.
    CTCEng.Run;
  finally
    CTCEng.Free;
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
