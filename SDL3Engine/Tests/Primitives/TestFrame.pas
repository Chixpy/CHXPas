program TestFrame;
{<
  A simple program with cCHXSDL3Engine for testing Frame primitives.

  1. Initial version
    1. TState changed to TDrawMode and TFillMode.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kLenStep = 0.25;

  kRenderH = 50;
  kRenderW = kRenderH * 4 div 3;
  kWinScale = 900 div kRenderH;
  kFullScreen = False;
  kRDriver = '';
  kProgVersion = '1.1';

type

  TDrawMode = (dmDefault, dmSubPixel, dmFullPixel);

  TFillMode = (fmBorder, fmFilled, fmBorFill, fmAll);

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
    ShowHelp : Boolean;
    DrawMode : TDrawMode; sDrawMode : String;
    FillMode : TFillMode; sFillMode : String;
    BorderColor, FillColor : TSDL_FColor;

    FWidth, FHeight, BorderW: CFloat;
    aRect: TSDL_FRect;

    procedure ChangeDrawMode;
    procedure ChangeFillMode;
    procedure ChangeColors;
    procedure DrawHelp;
  end;

{ cSDL3Eng }

procedure cSDL3Eng.ChangeDrawMode;
begin
  if DrawMode = High(TDrawMode) then
    DrawMode := Low(TDrawMode)
  else
    Inc(DrawMode);

  case DrawMode of
    dmDefault : sDrawMode := 'Default';
    dmSubPixel : sDrawMode := 'Subpixel';
    dmFullPixel : sDrawMode := 'Full Pixel';
  otherwise
    sDrawMode := '<Undefined>';
  end;
end;

procedure cSDL3Eng.ChangeFillMode;
begin
  if FillMode = High(TFillMode) then
    FillMode := Low(TFillMode)
  else
    Inc(FillMode);

  case FillMode of
    fmBorder : sFillMode := 'Border';
    fmFilled : sFillMode := 'Filled';
    fmBorFill : sFillMode := 'Fill Only with Border';
    fmAll: sFillMode := 'All';
  otherwise
    sFillMode := '<Undefined>';
  end;
end;

procedure cSDL3Eng.ChangeColors;
begin
  BorderColor.Init(Random, Random, Random, Random);
  FillColor.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True; ShowHelp := True;
  DrawMode := High(TDrawMode); ChangeDrawMode;
  FillMode := High(TFillMode); ChangeFillMode;
  ChangeColors;

  FWidth := kRenderW * 0.75;
  FHeight := kRenderH * 0.50;
  BorderW := kRenderH * 0.10;
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin
  aRect.Init((kRenderW - FWidth) * 0.5,
      (kRenderH - FHeight) * 0.5, FWidth, FHeight);
end;

procedure cSDL3Eng.Draw;
begin
  Render.Clear(0.05);

  if (FillMode = fmBorder) then
  begin
    Render.SetDrawColor(BorderColor);
    case DrawMode of
      dmDefault : Render.FrameBorder(aRect, BorderW);
      dmSubPixel : Render.SPFrameBorder(aRect, BorderW);
      // dmFullPixel : Render.FPFrameBorder(aRectInt, BorderWInt);
    end;
  end;

  if (FillMode = fmFilled) or (FillMode = fmAll) then
  begin
    Render.SetDrawColor(FillColor);
    case DrawMode of
      dmDefault : Render.FrameFilled(aRect, BorderW);
      dmSubPixel : Render.SPFrameFilled(aRect, BorderW);
      // dmFullPixel : Render.FPFrameFilled(aRectInt, BorderWInt);
    end;
  end;

  if (FillMode = fmBorFill) or (FillMode = fmAll) then
    case DrawMode of
      dmDefault : Render.Frame(aRect, BorderW, BorderColor, FillColor);
      dmSubPixel : Render.SPFrame(aRect, BorderW, BorderColor, FillColor);
      // dmFullPixel : Render.FPFrame(aRect, BorderW, BorderColor, FillColor);
    end;

  if ShowHelp then DrawHelp;
end;

procedure cSDL3Eng.DrawHelp;
begin
  Window.PushRenderSize(Window.WindowWidth div 2, Window.WindowHeight div 2);
  Render.PushDrawColor(1, 0, 1);
  Render.DebugTextF(0, 0, '%s %s', [sDrawMode, sFillMode]);
  Render.DebugTextF(0, 10, 'W: %g H: %g Border: %g',
    [FWidth, FHeight, BorderW]);
  Render.DebugText(0, 30, '[F1] Toggle help');
  Render.DebugText(0, 40, '[F] Change mode');
  Render.DebugText(0, 50, '[C] Change color');
  Render.DebugText(0, 60, '[ARROWS] Change size');
  Render.DebugText(0, 70, '[A] [Z] Change border');
  Render.PopDrawColor;
  Window.PopRenderSize;
end;

procedure cSDL3Eng.HandleEvent(const aEvent : TSDL_Event;
var Handled : Boolean; var ExitProg : Boolean);
begin
  inherited;
  if ExitProg or Handled then Exit;

  case aEvent.type_ of
    SDL_EVENT_KEY_DOWN :
    begin
      Handled := True;
      case aEvent.key.key of
        // ESC, F10, F11, F12 handled by cCHXSDL3Engine

        SDLK_F1 : ShowHelp := not ShowHelp;

        SDLK_C : ChangeColors;

        SDLK_M : ChangeDrawMode;

        SDLK_F : ChangeFillMode;

        SDLK_UP: FHeight += kLenStep;

        SDLK_DOWN: FHeight -= kLenStep;

        SDLK_RIGHT: FWidth += kLenStep;

        SDLK_LEFT: FWidth -= kLenStep;

        SDLK_A: BorderW += kLenStep;

        SDLK_Z: BorderW -= kLenStep;

        SDLK_Q : ExitProg := True;

      otherwise
        Handled := False;
      end; // case aEvent.key.key of
    end; // SDL_EVENT_KEY_DOWN :
  end; // case aEvent.type_ of
end;

{ Main program }

var
  SDL3Eng : cSDL3Eng;
  ProgName : String;
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
