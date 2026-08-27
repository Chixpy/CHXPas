program TestPoints;
{<
  Program to test if drawing and array of points is clearly faster than
  drawing the points one by one and wich way is faster.

  - `[F1]`: Toggle help. Writing text is expensive.
  - `[M]`: Changes between:
    1. Draw all points of an array one by one with `SDL_RenderPoint`.
    2. Draw the array with `SDL_RenderPoints`.
    3. Draw all points adding an offset, one by one.
    4. Create an array (reseting it very frame) with displaced points, and
      draw it.
  - `[C]`: Changes color alpha of the points, to see if it is significant.
  - `[R]`: Generates a new set of random points.

  In personal tests, both 2 and 4 are a very little faster than 1 and 3, but
  not _clearly_. Other users results may vary.

  cCHXSDL3Engine descendant is declared and implemented here.
  A better practice is that it is implemented in it's own unit.

  (C) 2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}
uses
  SysUtils, CTypes, SDL3, ucCHXSDL3Engine, uCHXSDL3TypeHelpers;

const
  kProgVersion = '1.0';
  kNPoints = 10000;

type

  TPointMode = (pmPixel, pmArray, pmPixelAdd, pmArrayAdd);

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
    PointsMode: TPointMode;
    Points: TSDLFPointDynArray;
    Alpha: CFloat;

    procedure InitArray;
  end;

{ cSDL3Eng }


procedure cSDL3Eng.InitArray;
var
  i: Integer;
begin
  SetLength(Points, kNPoints);

  for i := Low(Points) to High(Points) do
    Points[i].Init(Random * Window.Width - 5, Random * Window.Height - 5);
end;

procedure cSDL3Eng.Setup;
begin
  ShowFrameRate := True;
  InitArray;
  ShowHelp := True;
  PointsMode := Low(TPointMode);
  Alpha := 1;
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
  TmpPoints: TSDLFPointDynArray;
begin
  Render.Clear(0, 0, 0);
  Render.SetDrawColor(1, 1, 1, Alpha);

  case PointsMode of

  pmArray:
  begin
    SDL_RenderPoints(SDLRenderer, @Points[0], Length(Points));
  end;

  pmPixelAdd:
  begin
    for i := Low(Points) to High(Points) do
      SDL_RenderPoint(SDLRenderer, Points[i].X + 4, Points[i].Y + 4);
  end;

  pmArrayAdd:
  begin
    SetLength(TmpPoints, 0); // Reset full array
    SetLength(TmpPoints, Length(Points));
    for i := Low(Points) to High(Points) do
    begin
      TmpPoints[i].X := Points[i].X + 4;
      TmpPoints[i].Y := Points[i].Y + 4;
    end;

    SDL_RenderPoints(SDLRenderer, @TmpPoints[0], Length(TmpPoints));
  end;

  otherwise // pmPixel1
    for i := Low(Points) to High(Points) do
      SDL_RenderPoint(SDLRenderer, Points[i].X, Points[i].Y);
  end;

  Render.SetDrawColor(1, 0, 1);
  Render.DebugText(0, 0, IntToStr(Ord(PointsMode)));

  if ShowHelp then
  begin
    Render.DebugText(0, 10, '[F1] Toggle this Help');
    Render.DebugText(0, 20, '[M] Change Mode');
    Render.DebugText(0, 30, '[R] Change Points');
    Render.DebugText(0, 40, '[C] Change color Alpha');
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

      SDLK_C:
        if Alpha = 1 then
          Alpha := 0.5
        else
          Alpha := 1;

      SDLK_R: InitArray;

      SDLK_M:
        if PointsMode = High(TPointMode) then
          PointsMode := Low(TPointMode)
        else
          Inc(PointsMode);

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
  ProgName, IniName: String;
begin
  ProgName := ExtractFileName(ParamStr(0));
  IniName := ChangeFileExt(ProgName, '.ini');
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

  SDL3Eng := cSDL3Eng.Create(ChangeFileExt(ProgName, ''), IniName);
  try
    // Create an initial config file
    if not FileExists(IniName) then
      SDL3Eng.Config.SaveToFile('', False);
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
