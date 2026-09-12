program TestComponents;
{<
  A simple program with cCHXSDL3Engine for testing visual components.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
uses
  SysUtils, CTypes, SDL3, uCHXSDL3TypeHelpers, ucCHXSDL3Engine,
  uaCHXSDL3Component, ucCHXSDL3Button;

const
  kRenderH = 200;
  kRenderW = kRenderH * 4 div 3;
  kWinScale = 900 div kRenderH;
  kFullScreen = False;
  kRDriver = '';
  kProgVersion = '1.0';

type

  { cSDL3Eng }

  cSDL3Eng = class(cCHXSDL3Engine)
  protected
    procedure Setup; override; { It's abstract. }
    procedure Finish; override; { It's abstract. }
    procedure Compute(var ExitProg : Boolean); override; { It's abstract. }
    procedure Draw; override; { It's abstract. }
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled : Boolean;
      var ExitProg : Boolean); override; { It's virtual. }

  public
    ShowHelp: Boolean;

    sButtonPushed: String;

    procedure DrawHelp;

    procedure OnButtonClick(const Sender : caCHXSDL3Component);
  end;

{ cSDL3Eng }

procedure cSDL3Eng.OnButtonClick(const Sender : caCHXSDL3Component);
begin
  if Sender is cCHXSDL3Button then
    sButtonPushed := Sender.ID
  else
    sButtonPushed := '';
end;

procedure cSDL3Eng.Setup;
var
  aButton : cCHXSDL3Button;
begin
  ShowFrameRate := True; ShowHelp := True;

  aButton := cCHXSDL3Button.Create('Button 1', 20, 20, 40, 20);
  aButton.OnClick := @OnButtonClick;
  AddComponent(aButton);
  aButton := cCHXSDL3Button.Create('Button 2', 20, 50, 40, 20);
  aButton.OnClick := @OnButtonClick;
  AddComponent(aButton);

  sButtonPushed := '';
end;

procedure cSDL3Eng.Finish;
begin
  // Components added are destroyed by cSDL3Engine.
end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
begin
  Render.Clear(0.05);

  Window.PushRenderSize(Window.WindowWidth div 2, Window.WindowHeight div 2);
  Render.PushDrawColor(1);

    if Assigned(FocusedComp) then
      Render.DebugTextF(0, 0, 'Focused: %s', [FocusedComp.ID]);

    Render.DebugTextF(0, kRenderH - (8 * 3), 'Button Pushed: %s',
      [sButtonPushed]);

  Window.PopRenderSize;
  Render.PopDrawColor;

  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
const
  NLinesHelp = 25;
var
  Factor: Integer;
begin
  Window.PushRenderSize(Window.WindowWidth div 2, Window.WindowHeight div 2);
  Render.PushDrawColor(1, 0, 1);
  Render.DebugText(0, 10, '[F1] Toggle help');
  Render.DebugText(0, 20, '[CLICK] Select / Activate component');
  Window.PopRenderSize;
  Render.PopDrawColor;
end;

procedure cSDL3Eng.HandleEvent(const aEvent : TSDL_Event;
var Handled : Boolean; var ExitProg : Boolean);
begin
  inherited;
  if ExitProg or Handled then Exit;

  case aEvent.type_ of
    SDL_EVENT_KEY_DOWN:
    begin
      Handled := True;
      case aEvent.key.key of
        // ESC, F10, F11, F12 handled by cCHXSDL3Engine

        SDLK_F1: ShowHelp := not ShowHelp;

        SDLK_Q: ExitProg := True;

      otherwise
        Handled := False;
      end;
    end;
  otherwise
    ;
  end;
end;

  { Main program }

var
  SDL3Eng : cSDL3Eng;
  ProgName: String;
begin
  ProgName := ExtractFileName(ParamStr(0));
  ChDir(ExtractFilePath(ParamStr(0)));

  // Aplication metadata
  SDL_SetAppMetadata(PAnsiChar(ProgName), kProgVersion,
    PAnsiChar('com.chixpy.' + ProgName));
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_CREATOR_STRING, 'Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_COPYRIGHT_STRING,
    '(C) 2026 Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_URL_STRING,
    'https://github.com/Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_TYPE_STRING, 'application');

  SDL3Eng := cSDL3Eng.Create(ExtractFileName(ParamStr(0)), kRenderW, kRenderH,
    kWinScale, kFullScreen, kRDriver);
  try
    SDL3Eng.Run;
  finally
    SDL3Eng.Free;
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
