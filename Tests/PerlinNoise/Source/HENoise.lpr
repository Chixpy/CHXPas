program HENoise;
// Test of Hugo Elias implementation of Perlin Noise
// http://web.archive.org/web/20160325134143/http://freespace.virgin.net/hugo.elias/models/m_perlin.htm

// (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPNoise,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640;
  WinH = 480;


  { Window position constants:

    @preformated(
                   0       WinOffsetX     WinWMax
                 0 +-------------------------+
                   |            ·            |
                   |·························| ZAxisPosY (1D)
                   |            ·            |
                   +-------------------------+ XFramePosY
                   |            ·            |
                   |·························| XAxisPosY
                   |            ·            |
                   +-------+---------+-------+ YFramePosY
                   |   ·   |    ·    |       |
                   |·······|·········|       | WinOffsetY
                   |   ·   |    ·    |       |
           WinHMax +-------+---------+-------+
                   YAxisPosX
                        MapPosX InfoPosX
  )
  }
  WinWMax = WinW - 1; // Actual maximum values of Window
  WinHMax = WinH - 1;

  WinOffsetX = WinW div 2; // Offset of current X position (actually center)
  ZAxisPosY = WinH div 6; // Vertical position 1D axis
  WaveAmpl = ZAxisPosY; // Wave amplitude
  XFramePosY = ZAxisPosY * 2; // First horizontal axis height
  XAxisPosY = ZAxisPosY * 3; // Vertical position 2D X axis
  YFramePosY = ZAxisPosY * 4; // Second horizontal axis height
  WinOffsetY = ZAxisPosY * 5;  // Offset of current Y position
  YAxisPosX = ZAxisPosY; // Vertical axis (Y)
  MapPosX = XFramePosY; // Vertical axis width
  InfoPosX = WinWMax - MapPosX; // Info frame X position

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

    aTex : PSDL_Texture;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    aText : TStringList;
  begin
    ShowFrameRate := True;
    DefFont.ChangeFontStyle(SDLColor(255, 255, 255, 255), WinW div 50,
      -1, -1, -1);

    PosX := 0;
    PosY := 0;
    Scale := 0.125;

    InterpMth := PNILinear;

    DrawNoise := True;
    DrawSmooth := True;
    DrawInterp := True;
    DrawHENoise := True;
    DrawMap := False;

    aText := TStringList.Create;
    aText.Add('Arrows = Move; +/-/MWheel = Change scale');
    aText.Add('. = Integer pos; 0 = Reset');
    aText.Add('1/2/3/4 = Toggle Noise/Smooth/Interp/Final');
    aText.Add('M = Toggle Map; I = Interpolation Mode;');
    aText.Add('Click = Move to this point');
    DefFont.AddStaticText('Help', aText, InfoPosX - MapPosX,
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
    X, Y, aColor : Integer;
    CurrXPos, CurrYPos : Double;
  begin
    if SDLWindow.Minimized then Exit;

    // Direct pixel manupulation because of map
    aTex := SDL_CreateTexture(SDLWindow.PRenderer, PWinPxFmt^.format,
      SDL_TEXTUREACCESS_STREAMING, WinW, WinH);
    SDL_LockTexture(aTex, nil, @PPBase, @Pitch);

    for X := 0 to WinWMax do
    begin
      CurrXPos := PosX + Scale * (X - WinOffsetX);

      // Hugo Elias 1D
      //---------------

      // HENoiseRNG1D(x)
      if DrawNoise then
      begin
        Y := ZAxisPosY - Round(HENoiseRNG1D(Round(CurrXPos)) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 0, 0, 255, 255);
      end;

      // HESmoothNoise1D(x)
      if DrawSmooth then
      begin
        Y := ZAxisPosY - Round(HESmoothNoise1D(Round(CurrXPos)) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 255, 0, 0, 255);
      end;

      // HEInterpNoise1D(x)
      if DrawInterp then
      begin
        Y := ZAxisPosY -
          Round(HEInterpNoise1D(CurrXPos, InterpMth) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 0, 255, 0, 255);
      end;

      // HENoise1D(x)
      if DrawHENoise then
      begin
        Y := ZAxisPosY -
          Round(HENoise1D(CurrXPos, 4, 0.25, InterpMth) * WaveAmpl);
        if Y > 0 then // Teorically can return values out of [-1..1] in Cubic
          PutPixel(PPBase, Pitch, X, Y, 255, 255, 255, 255);
      end;

      // Hugo Elias 2D X Axis
      //----------------------

      // HENoiseRNG2D (x)
      if DrawNoise then
      begin
        Y := XAxisPosY -
          Round(HENoiseRNG2D(Round(CurrXPos), Round(PosY)) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 0, 0, 255, 255);
      end;

      // HESmoothNoise2D (x)
      if DrawSmooth then
      begin
        Y := XAxisPosY -
          Round(HESmoothNoise2D(Round(CurrXPos), Round(PosY)) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 255, 0, 0, 255);
      end;

      // HEInterpNoise2D (x)
      if DrawInterp then
      begin
        Y := XAxisPosY -
          Round(HEInterpNoise2D(CurrXPos, PosY, InterpMth) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 0, 255, 0, 255);
      end;

      // HENoise2D (x)
      if DrawHENoise then
      begin
        Y := XAxisPosY -
          Round(HENoise2D(CurrXPos, PosY, 4, 0.25, InterpMth) * WaveAmpl);
        if Y > 0 then // Teorically can return values out of [-1..1] in Cubic
          PutPixel(PPBase, Pitch, X, Y, 255, 255, 255, 255);
      end;
    end;

    // Drawing 2D Y wave
    for Y := YFramePosY to WinHMax do
    begin
      CurrYPos := PosY + Scale * (Y - WinOffsetY);

      // Hugo Elias 2D Y Axis
      //----------------------

      // HENoiseRNG2D (y)
      if DrawNoise then
      begin
        X := YAxisPosX -
          Round(HENoiseRNG2D(Round(PosX), Round(CurrYPos)) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 0, 0, 255, 255);
      end;

      // HESmoothNoise2D (y)
      if DrawSmooth then
      begin
        X := YAxisPosX -
          Round(HESmoothNoise2D(Round(PosX), Round(CurrYPos)) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 255, 0, 0, 255);
      end;

      // HEInterpNoise2D (y)
      if DrawInterp then
      begin
        X := YAxisPosX -
          Round(HEInterpNoise2D(PosX, CurrYPos, InterpMth) * WaveAmpl);
        PutPixel(PPBase, Pitch, X, Y, 0, 255, 0, 255);
      end;

      // HENoise2D (y)
      if DrawHENoise then
      begin
        X := YAxisPosX -
          Round(HENoise2D(PosX, CurrYPos, 4, 0.25, InterpMth) * WaveAmpl);
        if Y > 0 then // Teorically can return values out of [-1..1] in Cubic
          PutPixel(PPBase, Pitch, X, Y, 255, 255, 255, 255);
      end;
    end;

    // Hugo Elias 2D Map
    //-------------------
    if DrawMap then
      for X := MapPosX to InfoPosX do
      begin
        CurrXPos := PosX + Scale * (X - WinOffsetX);

        for Y := YFramePosY to WinHMax do
        begin
          CurrYPos := PosY + Scale * (Y - WinOffsetY);
          aColor := Floor(map(HENoise2D(CurrXPos, CurrYPos, 4,
            0.25, InterpMth), -1, 1, 0, 255));
          // Teorically can return values out of [-1..1] in Cubic
          aColor := EnsureRange(aColor, 0, 255);
          PutPixel(PPBase, Pitch, X, Y, aColor, aColor, aColor, 255);
        end;
      end;

    SDL_UnlockTexture(aTex);
  end;

  procedure cCTCEng.Draw;
  var
    aText : string;
    X : Integer;
  begin
    //SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    //SDL_RenderClear(SDLWindow.PRenderer);

    SDL_RenderCopy(SDLWindow.PRenderer, aTex, nil, nil);
    SDL_DestroyTexture(aTex);

    // Axis
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 64, 64, 64, 255);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, ZAxisPosY, WinWMax, ZAxisPosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, XAxisPosY, WinWMax, XAxisPosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, YAxisPosX, YFramePosY, YAxisPosX, WinHMax);

    SDL_RenderDrawLine(SDLWindow.PRenderer, WinOffsetX, 0, WinOffsetX, WinHMax);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffsetY, InfoPosX, WinOffsetY);

    // X = 0 Noise position
    X := Round(WinOffsetX - PosX / scale);
    if InRange(X, 0, WinHMax) then
    begin
      SDL_RenderDrawLine(SDLWindow.PRenderer, X, 0, X, WinHMax);
      characterColor(SDLWindow.PRenderer, X + 1, ZAxisPosY, '0', $444444FF);
      characterColor(SDLWindow.PRenderer, X + 1, XAxisPosY, '0', $444444FF);
      characterColor(SDLWindow.PRenderer, X + 1, WinOffsetY, '0', $444444FF);
    end;

    // Borders
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, XFramePosY, WinWMax, XFramePosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, YFramePosY, WinWMax, YFramePosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, MapPosX, YFramePosY, MapPosX, WinHMax);
    SDL_RenderDrawLine(SDLWindow.PRenderer, InfoPosX, YFramePosY, InfoPosX, WinHMax);


    // Wave texts
    aText := '1D: ' + FloatToStrF(PosX, ffFixed, 4, 4) + ' = ' +
      FloatToStrF(HENoise1D(PosX, 4, 0.25, InterpMth), ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, 0);

    aText := '2D X: ' + FloatToStrF(PosX, ffFixed, 4, 4) + ' = ' +
      FloatToStrF(HENoise2D(PosX, PosY, 4, 0.25, InterpMth), ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, XFramePosY);

    aText := '2D Y: ' + FloatToStrF(PosY, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, YFramePosY);

    // Bottom right box
    aText := 'Scale: ' + FloatToStrF(Scale, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX, YFramePosY);

    case InterpMth of
      PNICos : aText := 'Cosine';
      PNICubic : aText := 'Cubic';
      else // PNILinear:
        aText := 'Linear';
    end;
    DefFont.RenderDynStr(aText, InfoPosX, DefFont.YPosAtLine(1, YFramePosY));

    aText := 'Mouse X: ' + FloatToStrF(MouseX, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX, DefFont.YPosAtLine(2, YFramePosY));

    aText := 'Mouse Y: ' + FloatToStrF(MouseY, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX, DefFont.YPosAtLine(3, YFramePosY));

    // Help
    if not DrawMap then
      DefFont.RenderStatic('help', MapPosX, YFramePosY + 1);
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
          SDLK_PLUS, SDLK_KP_PLUS : Scale *= 2;
          SDLK_MINUS, SDLK_KP_MINUS : Scale /= 2;

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
        if (aEvent.motion.Y > YFramePosY) and
          (aEvent.motion.X < MapPosX) then
          MouseX := PosX
        else
          MouseX := PosX + Scale * (aEvent.motion.X - WinOffsetX);

        // If out of Map / Y wave keep original Y
        if aEvent.motion.Y < YFramePosY then
          MouseY := PosY
        else
          MouseY := PosY + Scale * (aEvent.motion.Y - WinOffsetY);
      end;

      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        // Any button
        PosX := MouseX;
        PosY := MouseY;
      end;
      SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);
      begin
        if aEvent.wheel.Y > 0 then
          Scale *= 2
        else
          Scale /= 2;
      end;
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
