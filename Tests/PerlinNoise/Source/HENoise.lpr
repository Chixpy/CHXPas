program HENoise;
// Test of Hugo Elias implementation of Perlin Noise
// (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uPNoise,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640;
  WinH = 480;

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

  public
    WinOffX : integer; // Window offset X of 0 position
    WinOffY : integer; // Window offset Y of 0 position

    MouseX : Double; // X value at Mouse position
    MouseY : Double; // Y value at Mouse position

    PosX : Double; // X Position in Perlin Noise
    PosY : Double; // Y Position in Perlin Noise

    Scale : Double; // Scale in Perlin Noise

    InterpMth : TPNoiseInterp;

    DrawNoise : Boolean;
    DrawSmooth : Boolean;
    DrawInterp : Boolean;
    DrawHENoise : Boolean;
    DrawMap : Boolean;
    DrawAxis : Boolean;

    aTex : PSDL_Texture;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    aText : TStringList;
  begin
    ShowFrameRate := True;
    DefFont.ChangeFontStyle(SDLColor(255, 255, 255, 192),
      WinW div 50, -1, -1, -1);

    WinOffX := WinW div 2;
    WinOffY := WinH div 6;

    PosX := 0;
    PosY := 0;
    Scale := 0.025;

    InterpMth := PNILinear;

    DrawNoise := True;
    DrawSmooth := True;
    DrawInterp := True;
    DrawHENoise := True;
    DrawMap := False;
    DrawAxis := True;

    aText := TStringList.Create;
    aText.Add('Arrows = Move; /* = Change scale');
    aText.Add('. = Integer pos; 0 = Reset');
    aText.Add('1/2/3/4 = Toggle Noise/Smooth/Interp/Final');
    aText.Add('M = Toggle Map; I = Interpolation Mode;');
    aText.Add('L-Click = Move to this point');
    DefFont.AddStaticText('Help', aText, WinW - WinOffY * 4,
      TTF_WRAPPED_ALIGN_CENTER);
    aText.Free;

  end;

  procedure cCTCEng.Finish;
  begin

  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    Pitch : CInt;
    PPBase : PCUInt32;
    x, y, aColor : integer;
    CurrXPos, CurrYPos : Double;
  begin
    if SDLWindow.Minimized then Exit;

    // Direct pixel manupulation because of map
    aTex := SDL_CreateTexture(SDLWindow.PRenderer, PWinPxFmt^.format,
      SDL_TEXTUREACCESS_STREAMING, WinW, WinH);
    SDL_LockTexture(aTex, nil, @PPBase, @Pitch);

    for x := 0 to (WinW - 1) do
    begin
      CurrXPos := PosX + Scale * (x - WinOffX);

      // Hugo Elias 1D
      //---------------

      // HENoiseRNG1D(x)
      if DrawNoise then
      begin
        y := Round(HENoiseRNG1D(Round(CurrXPos)) * WinOffY) + WinOffY;
        PutPixel(PPBase, Pitch, x, y, 0, 0, 255, 255);
      end;

      // HESmoothNoise1D(x)
      if DrawSmooth then
      begin
        y := Round(HESmoothNoise1D(Round(CurrXPos)) * WinOffY) + WinOffY;
        PutPixel(PPBase, Pitch, x, y, 255, 0, 0, 255);
      end;

      // HEInterpNoise1D(x)
      if DrawInterp then
      begin
        y := Round(HEInterpNoise1D(CurrXPos, InterpMth) * WinOffY) + WinOffY;
        PutPixel(PPBase, Pitch, x, y, 0, 255, 0, 255);
      end;

      // HENoise1D(x)
      if DrawHENoise then
      begin
        y := Round(HENoise1D(CurrXPos, 4, 0.25, InterpMth) *
          WinOffY) + WinOffY;
        if y > 0 then // Actually can return values out of [-1..1] in Cubic
          PutPixel(PPBase, Pitch, x, y, 255, 255, 255, 255);
      end;

      // Hugo Elias 2D X Axis
      //----------------------

      // HENoiseRNG2D (x)
      if DrawNoise then
      begin
        y := Round(HENoiseRNG2D(Round(CurrXPos), Round(PosY)) * WinOffY) +
          (WinOffY * 3);
        PutPixel(PPBase, Pitch, x, y, 0, 0, 255, 255);
      end;

      // HESmoothNoise2D (x)
      if DrawSmooth then
      begin
        y := Round(HESmoothNoise2D(Round(CurrXPos), Round(PosY)) * WinOffY) +
          (WinOffY * 3);
        PutPixel(PPBase, Pitch, x, y, 255, 0, 0, 255);
      end;

      // HEInterpNoise2D (x)
      if DrawInterp then
      begin
        y := Round(HEInterpNoise2D(CurrXPos, PosY, InterpMth) * WinOffY) +
          (WinOffY * 3);
        PutPixel(PPBase, Pitch, x, y, 0, 255, 0, 255);
      end;

      // HENoise2D (x)
      if DrawHENoise then
      begin
        y := Round(HENoise2D(CurrXPos, PosY, 4, 0.25, InterpMth) * WinOffY) +
          (WinOffY * 3);
        if y > 0 then // Actually can return values out of [-1..1]
          PutPixel(PPBase, Pitch, x, y, 255, 255, 255, 255);
      end;

      // Hugo Elias 2D Map
      //-------------------

      if DrawMap and InRange(x, WinOffY * 2, WinW - WinOffY * 2) then
      begin
        // This is slow in cubic...
        for y := (WinOffY * 4) to (WinH - 1) do
        begin
          CurrYPos := PosY + Scale * (y - WinOffY * 5);
          // Well, actually all previous graph are inverted vertically
          //   with positive Y values going down.
          // So, this mapping to a color is inverted.
          aColor := Floor(map(HENoise2D(CurrXPos, CurrYPos, 4,
            0.25, InterpMth),
            -1, 1, 255, 0));
          PutPixel(PPBase, Pitch, x, y, aColor, aColor, aColor, 255);
        end;
      end;
    end;

    // Drawing 2D Y wave
    for y := WinOffY * 4 to WinH - 1 do
    begin
      CurrYPos := PosY + Scale * (y - WinOffY * 5);

      // Hugo Elias 2D Y Axis
      //----------------------

      // HENoiseRNG2D (y)
      if DrawNoise then
      begin
        x := Round(HENoiseRNG2D(Round(PosX), Round(CurrYPos)) * WinOffY) +
          WinOffY;
        PutPixel(PPBase, Pitch, x, y, 0, 0, 255, 255);
      end;

      // HESmoothNoise2D (y)
      if DrawSmooth then
      begin
        x := Round(HESmoothNoise2D(Round(PosX), Round(CurrYPos)) * WinOffY) +
          WinOffY;
        PutPixel(PPBase, Pitch, x, y, 255, 0, 0, 255);
      end;

      // HEInterpNoise2D (y)
      if DrawInterp then
      begin
        x := Round(HEInterpNoise2D(PosX, CurrYPos, InterpMth) * WinOffY) +
          WinOffY;
        PutPixel(PPBase, Pitch, x, y, 0, 255, 0, 255);
      end;

      // HENoise2D (y)
      if DrawHENoise then
      begin
        x := Round(HENoise2D(PosX, CurrYPos, 4, 0.25, InterpMth) * WinOffY) +
          WinOffY;
        if y > 0 then // Actually can return values out of [-1..1]
          PutPixel(PPBase, Pitch, x, y, 255, 255, 255, 255);
      end;
    end;

    SDL_UnlockTexture(aTex);
  end;

  procedure cCTCEng.Draw;
  var
    aText : string;
    x, y : LongInt;
  begin
    //SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    //SDL_RenderClear(SDLWindow.PRenderer);

    SDL_RenderCopy(SDLWindow.PRenderer, aTex, nil, nil);
    SDL_DestroyTexture(aTex);

    if DrawAxis then
    begin
      // Axis
      SDL_SetRenderDrawColor(SDLWindow.PRenderer, 64, 64, 64, 255);
      SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffY, WinW, WinOffY);
      SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffY * 3,
        WinW, WinOffY * 3);
      SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffY * 5,
        WinW - WinOffY * 2, WinOffY * 5);
      SDL_RenderDrawLine(SDLWindow.PRenderer, WinOffX, 0, WinOffX, WinH);
      SDL_RenderDrawLine(SDLWindow.PRenderer, WinOffY, WinOffY *
        4, WinOffY, WinH);

      // X = 0 Noise position
      x := Round(WinOffX - PosX / scale);
      if (x >= 0) and (x < WinW) then
      begin
        SDL_RenderDrawLine(SDLWindow.PRenderer, x, 0, x, WinH);
        characterColor(SDLWindow.PRenderer, x + 1, WinOffY, '0', $444444FF);
        characterColor(SDLWindow.PRenderer, x + 1, WinOffY *
        3, '0', $444444FF);
        characterColor(SDLWindow.PRenderer, x + 1, WinOffY *
        5, '0', $444444FF);
      end;

      // Borders
      SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
      SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffY * 2,
        WinW, WinOffY * 2);
      SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffY * 4,
        WinW, WinOffY * 4);
      SDL_RenderDrawLine(SDLWindow.PRenderer, WinOffY * 2,
        WinOffY * 4, WinOffY * 2, WinH);
      SDL_RenderDrawLine(SDLWindow.PRenderer, WinW - WinOffY *
        2, WinOffY * 4, WinW - WinOffY * 2, WinH);
    end;

    // Wave texts
    aText := '1D: ' + FloatToStrF(PosX, ffFixed, 4, 4) + ' = ' +
      FloatToStrF(HENoise1D(PosX, 4, 0.25, InterpMth), ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, 0);

    aText := '2D X: ' + FloatToStrF(PosX, ffFixed, 4, 4) + ' = ' +
      FloatToStrF(HENoise2D(PosX, PosY, 4, 0.25, InterpMth), ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, WinOffY * 2);

    aText := '2D Y: ' + FloatToStrF(PosY, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, WinOffY * 4);

    // Bottom right box
    x := WinW - WinOffY * 2 + 2;
    y := WinOffY * 4 + 1;

    aText := 'Scale: ' + FloatToStrF(Scale, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, x, y);

    case InterpMth of
      PNICos : aText := 'Cosine';
      PNICubic : aText := 'Cubic';
      else // PNILinear:
        aText := 'Linear';
    end;
    DefFont.RenderDynStr(aText, x, DefFont.YPosAtLine(1, y));

    aText := 'Mouse X: ' + FloatToStrF(MouseX, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, x, DefFont.YPosAtLine(2, y));

    aText := 'Mouse Y: ' + FloatToStrF(MouseY, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, x, DefFont.YPosAtLine(3, y));

    // Help
    if not DrawMap then
      DefFont.RenderStatic('help', WinOffY * 2, WinOffY * 4);
  end;

  procedure cCTCEng.HandleEvent(const aEvent : TSDL_Event;
  var Handled : Boolean; var ExitProg : Boolean);
  begin
    inherited HandleEvent(aEvent, Handled, ExitProg);
    if ExitProg then Exit;

    case aEvent.type_ of
      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        case aEvent.key.keysym.sym of
          // Changing scale
          SDLK_ASTERISK, SDLK_KP_MULTIPLY : Scale *= 2;
          SDLK_SLASH, SDLK_KP_DIVIDE : Scale /= 2;

          // Resetting
          SDLK_0, SDLK_KP_0 :
          begin
            PosX := 0;
            PosY := 0;
          end;
          SDLK_PERIOD, SDLK_KP_PERIOD :
          begin
            PosX := Round(PosX);
            PosY := Round(PosY);
          end;

          // Moving
          SDLK_LEFT : PosX -= Scale;
          SDLK_RIGHT : PosX += Scale;
          SDLK_UP : PosY -= Scale;
          SDLK_DOWN : PosY += Scale;

          // Drawings
          SDLK_1, SDLK_KP_1 : DrawNoise := not DrawNoise;
          SDLK_2, SDLK_KP_2 : DrawSmooth := not DrawSmooth;
          SDLK_3, SDLK_KP_3 : DrawInterp := not DrawInterp;
          SDLK_4, SDLK_KP_4 : DrawHENoise := not DrawHENoise;
          SDLK_m : DrawMap := not DrawMap;

          // interpolation
          SDLK_i :
            if InterpMth = high(TPNoiseInterp) then
              InterpMth := Low(TPNoiseInterp) else
              InterpMth := Succ(InterpMth);
            //SDLK_SPACE : ;
          else
            ;
        end;
      end;

      SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      begin
        // Inside Y wave keep original X
        if (aEvent.motion.y > WinOffY * 4) and
          (aEvent.motion.x < WinOffY * 2) then
          MouseX := PosX
        else
          MouseX := PosX + Scale * (aEvent.motion.x - WinOffX);

        // If out of Map / Y wave keep original Y
        if aEvent.motion.y < WinOffY * 4 then
          MouseY := PosY
        else
          MouseY := PosY + Scale * (aEvent.motion.y - WinOffY * 5);
      end;

      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        PosX := MouseX;
        PosY := MouseY;
      end
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
    CTCEng := cCTCEng.Create(ApplicationName, 'CHXSDL.ini', False);
    CTCEng.Config.WindowWidth := WinW;
    CTCEng.Config.RendererWidth := WinW;
    CTCEng.Config.WindowHeight := WinH;
    CTCEng.Config.RendererHeight := WinH;
    CTCEng.Config.RendererUseHW := True;
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
