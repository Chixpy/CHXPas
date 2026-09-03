program TestLineVsRect;
{<
  A simple program with cCHXSDL3Engine that compares `SDL_RenderLine`
  and `SDL_Render[Filled]Rect[s]` with vertical and horizontal lines when
  Logical Presentation is used.

  @(In my tests Filled Rects are the fastests, then Lines and finally
    Rect Borders. **And seems to be faster draw from right to left or
    down to up than the usual way, both Lines and Rects (with negative W
    and H)**, but seems not to be worth of changing a whole array every frame.

    Additional notes:

    - Line overdraw if `Abs(Length) < 2`.
    - Rect Border overdraws always as they have 1 of width. Much more if
      `Abs(Length) < 2`.
    - Filled Rectys don't overdraw.
    - `1` must be added to `Length` when working with rects, this is how Rects
      works.
    - Line and Border Rect draw a full pixel if `Abs(Length) < 1`.
    - Fill Rects draws subpixel rects with `Abs(Length) < 1`
      (wich maybe is a desired effect) and `1` must be _substracted_ if
      `Length < 0`.
  )

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  // In actual programs use Window.Render[Width/Height]
  kRenderW = 100; // Renderer width.
  kRenderH = 100; // Renderer height.
  kWindowScale = 8; // Scale of the Window.
  kFullScreen = False;
  kUseGPU = True; // Soft or GPU renderer

  kNLines = 2000;

type

  TState = (stLine, stRectFilled, stRectBorder, stCHXLine,
    stCHXFilledRect, stCHXFilledRectsInv);
  TLineDir = (ldHor, ldVer, ldHorInv, ldVerInv);

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
    Color: TSDL_FColor;
    ShowHelp: Boolean;

    State: TState;
    LineDir: TLineDir;

    sState, sLineDir: String;

    LinesOrig: TSDLFPointDynArray; // Origin points of the lines
    LinesDst: TSDLFPointDynArray; // End points of the lines
    Rects: TSDLFRectDynArray; // Rects

    LineLength: CFloat;

    procedure InitColors;
    procedure InitLines;
    procedure InitAuxArrays;

    procedure ChangeLineDir;
    procedure ChangeState;
  end;

{ cSDL3Eng }


procedure cSDL3Eng.InitColors;
begin
  Color.Init(Random, Random, Random, Random);
end;

procedure cSDL3Eng.InitLines;
var
  i: Integer;
begin
  SetLength(LinesOrig, kNLines);
  for i := 0 to (kNLines - 1) do
    LinesOrig[i].InitRandom(LineLength, kRenderW - LineLength, LineLength,
      kRenderH - LineLength);

  InitAuxArrays;
end;

procedure cSDL3Eng.InitAuxArrays;
var
  i: Integer;
begin
  SetLength(LinesDst, Length(LinesOrig));
  SetLength(Rects, Length(LinesOrig));

  case LineDir of
  ldHor:
  begin
    for i := 0 to (Length(LinesOrig) - 1) do
    begin
      LinesDst[i].Init(LinesOrig[i].X + LineLength, LinesOrig[i].Y);
      Rects[i] := SDLFRect(LinesOrig[i].X, LinesOrig[i].Y, 1 + LineLength, 1);
    end;
  end;

  ldVer:
  begin
    for i := 0 to (Length(LinesOrig) - 1) do
    begin
      LinesDst[i].Init(LinesOrig[i].X, LinesOrig[i].Y + LineLength);
      Rects[i] := SDLFRect(LinesOrig[i].X, LinesOrig[i].Y, 1, 1 + LineLength);
    end;
  end;

  ldHorInv:
  begin
    for i := 0 to (Length(LinesOrig) - 1) do
    begin
      LinesDst[i].Init(LinesOrig[i].X - LineLength, LinesOrig[i].Y);
      Rects[i] := SDLFRect(LinesOrig[i].X, LinesOrig[i].Y, 1 - LineLength, 1);
    end;
  end;

  ldVerInv:
    for i := 0 to (Length(LinesOrig) - 1) do
    begin
      LinesDst[i].Init(LinesOrig[i].X, LinesOrig[i].Y - LineLength);
      Rects[i] := SDLFRect(LinesOrig[i].X, LinesOrig[i].Y, 1, 1 - LineLength);
    end;
    
  otherwise ;
  end;
end;

procedure cSDL3Eng.ChangeLineDir;
begin
  if LineDir = High(TLineDir) then
    LineDir := Low(TLineDir)
  else
    Inc(LineDir);

  case LineDir of
  ldHor: sLineDir := 'Horizontal';
  ldVer: sLineDir := 'Vertical';
  ldHorInv: sLineDir := 'Inverted Horizontal';
  ldVerInv: sLineDir := 'Inverted Vertical';
  otherwise ;
  end;

  InitAuxArrays;
end;

procedure cSDL3Eng.ChangeState;
begin
  if State = High(TState) then
    State := Low(TState)
  else
    Inc(State);

  case State of
  stLine: sState := 'Lines';
  stRectFilled: sState := 'Filled Rects';
  stRectBorder: sState := 'Border Rects';
  stCHXLine: sState := '(CHX) Line';
  stCHXFilledRect: sState := '(CHX) FilledRect';
  stCHXFilledRectsInv: sState := '(CHX) FilledRects Inverted';
  otherwise;
  end;

  // stCHXFilledRectsInv changes the arrays
  InitAuxArrays;
end;
procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True;

  State := High(TState);
  ChangeState; // Sets sState
  LineDir:= High(TLineDir);
  ChangeLineDir; // Sets sLineDir
  LineLength:= 2; // To check overdraw easily with < 2

  InitColors;
  InitLines; // Updates Aux arrays too

  ShowHelp := True;
end;

procedure cSDL3Eng.Finish;
begin

end;

procedure cSDL3Eng.Compute(var ExitProg : Boolean);
begin

end;

procedure cSDL3Eng.Draw;
var
  i: Integer;
begin
  // To show framerate at very small render size (2)
  Window.SetRenderSize(kRenderW, kRenderH);
  Render.SetDrawColor(1, 1, 1);
  Render.Clear(0, 0, 0);

  Render.SetDrawColor(Color);

  case State of
  stLine:
    for i := 0 to High(LinesOrig) do
      SDL_RenderLine(SDLRenderer, LinesOrig[i].X, LinesOrig[i].Y,
        LinesDst[i].X, LinesDst[i].Y);

  stRectFilled: SDL_RenderFillRects(SDLRenderer, @Rects[0], Length(Rects));

  stRectBorder: SDL_RenderRects(SDLRenderer, @Rects[0], Length(Rects));

  stCHXLine:
    for i := 0 to High(LinesOrig) do
      Render.Line(LinesOrig[i].X, LinesOrig[i].Y,
        LinesDst[i].X, LinesDst[i].Y);

  stCHXFilledRect:
    for i := 0 to High(Rects) do
      Render.RectFilled(Rects[i]);

  stCHXFilledRectsInv:
  begin
    // Inverting al Rects
    if ((LineDir = ldHor) and (LineLength > 0))
      or  ((LineDir = ldHorInv) and (LineLength < 0)) then
      for i := 0 to High(Rects) do
      begin
          Rects[i].X := LinesOrig[i].X + 1 + LineLength;
          Rects[i].Y := LinesOrig[i].Y + 1;
          Rects[i].W := -LineLength - 1;
          Rects[i].H := -1;
      end
    else if ((LineDir = ldVer) and (LineLength > 0))
      or ((LineDir = ldVerInv) and (LineLength < 0)) then
      for i := 0 to High(Rects) do
      begin
          Rects[i].X := LinesOrig[i].X + 1;
          Rects[i].Y := LinesOrig[i].Y + 1 + LineLength;
          Rects[i].W := -1;
          Rects[i].H := -LineLength - 1;
      end;

    Render.RectsFilled(Rects, 0);
  end;

  otherwise ;
  end;

  // To show framerate at very small render size (1)
  Window.SetRenderSize(400, 400);
  if ShowHelp then
  begin
    Render.SetDrawColor(1, 0, 1);
    Render.DebugTextF(0, 0, '%s %s - Size: %g', [sLineDir, sState, LineLength]);
    Render.DebugText(0, 10, '[F1] Toggle help');
    Render.DebugText(0, 20, '[C] Change color');
    Render.DebugText(0, 30, '[L] Change Lines');
    Render.DebugText(0, 40, '[M] Change Mode');
    Render.DebugText(0, 50, '[D] Change Direction');
    Render.DebugText(0, 60, '[ARROWS] Change Length');
  end;
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

        SDLK_C: InitColors;

        SDLK_L: InitLines;

        SDLK_M: ChangeState;

        SDLK_D: ChangeLineDir;

        SDLK_UP, SDLK_RIGHT:
        begin
          LineLength += 0.25;
          InitAuxArrays;
        end;

        SDLK_DOWN, SDLK_LEFT:
        begin
          LineLength -= 0.25;
          InitAuxArrays;
        end;

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
  SDL_SetAppMetadata(PAnsiChar(ProgName), '1.0',
    PAnsiChar('com.chixpy.' + ProgName));
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_CREATOR_STRING, 'Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_COPYRIGHT_STRING,
    '(C) 2026 Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_URL_STRING,
    'https://github.com/Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_TYPE_STRING, 'application');

  SDL3Eng := cSDL3Eng.Create(ExtractFileName(ParamStr(0)), kRenderW, kRenderH,
    kWindowScale, kFullScreen, kUseGPU);
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
