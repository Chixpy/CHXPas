program PINoise;
// Test of Perlin Improved Noise
// https://mrl.cs.nyu.edu/~perlin/noise/

// (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXPNoise,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2Font, uCHXSDL2Utils, uProcUtils;

const
  { Window size: Renderer scales images to actual size of the window. }
  WinW = 640;
  WinH = 480;

  { Window position constants:

    @preformated(
                   0       WinOffsetX     WinWMax
                 0 +-------------------------+
                   |            ·            |
                   |·························| ZAxisPosY
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

  WinOffsetX = WinW div 2; // Offset of current X/Z position (actually center)
  ZAxisPosY = WinH div 6; // Vertical position Z axis
  WaveAmpl = ZAxisPosY; // Wave amplitude
  XFramePosY = ZAxisPosY * 2; // First horizontal axis height
  XAxisPosY = ZAxisPosY * 3; // Second horizontal axis (X)
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
    MouseZ : Double; // Y value at Mouse position

    PosX : Double; // X Position in Perlin Noise
    PosY : Double; // Y Position in Perlin Noise
    PosZ : Double; // Z Position in Perlin Noise

    Scale : Double; // Scale in Perlin Noise

    DrawMap : Boolean;

    aTex : PSDL_Texture;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    aText : TStringList;
  begin
    ShowFrameRate := True;
    DefFont.ChangeFontStyle(SDLColor(255, 255, 255, 192), WinW div
      50, -1, -1, -1);

    PosX := 0;
    PosY := 0;
    PosZ := 0;
    Scale := 0.125;

    DrawMap := False;

    aText := TStringList.Create;
    aText.Add('Arrows,/,* = Move; +,- = Change scale');
    aText.Add('. = Integer pos; 0 = Reset');
    aText.Add('M = Toggle Map');
    aText.Add('Click = Move to this point');
    aText.Add('Wheel = Move Z Axis');
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
    X, Y, aColor : Integer; // Screen coords
    CurrPos, CurrPos2 : Double;
  begin
    if SDLWindow.Minimized then Exit;

    // Direct pixel manupulation because of map
    aTex := SDL_CreateTexture(SDLWindow.PRenderer, PWinPxFmt^.format,
      SDL_TEXTUREACCESS_STREAMING, WinW, WinH);

    SDL_LockTexture(aTex, nil, @PPBase, @Pitch);

    // Perlin Noise Z axis
    //---------------------
    for X := 0 to WinWMax do
    begin
      CurrPos := PosZ + Scale * (X - WinOffsetX);
      Y := ZAxisPosY - Round(PINNoise(PosX, PosY, CurrPos) * WaveAmpl);
      PutPixel(PPBase, Pitch, X, Y, 255, 255, 0, 255);
    end;

    // Perlin Noise X axis
    //---------------------
    for X := 0 to WinWMax do
    begin
      CurrPos := PosX + Scale * (X - WinOffsetX);
      Y := XAxisPosY - Round(PINNoise(CurrPos, PosY, PosZ) * WaveAmpl);
      PutPixel(PPBase, Pitch, X, Y, 255, 255, 0, 255);
    end;

    // Perlin Noise Y axis
    //---------------------
    for Y := YFramePosY to WinHMax do
    begin
      CurrPos := PosY + Scale * (Y - WinOffsetY);
      X := YAxisPosX - Round(PINNoise(PosX, CurrPos, PosZ) * WaveAmpl);
      PutPixel(PPBase, Pitch, X, Y, 255, 255, 0, 255);
    end;

    // Perlin Noise Map
    //------------------
    if DrawMap then
      for X := MapPosX to InfoPosX do
      begin
        CurrPos := PosX + Scale * (X - WinOffsetX);

        for Y := YFramePosY to (WinH - 1) do
        begin
          CurrPos2 := PosY + Scale * (Y - WinOffsetY);

          // X-Y Map
          aColor := Floor(map(PINNoise(CurrPos, CurrPos2, PosZ),
            -1, 1, 0, 255));

          PutPixel(PPBase, Pitch, X, Y, aColor, aColor, aColor, 255);
        end;
      end;

    SDL_UnlockTexture(aTex);
  end;

  procedure cCTCEng.Draw;
  var
    aText : string;
    i : Integer;
  begin
    //SDL_SetRenderDrawColor(SDLWindow.PRenderer, 0, 0, 0, 255);
    //SDL_RenderClear(SDLWindow.PRenderer);

    SDL_RenderCopy(SDLWindow.PRenderer, aTex, nil, nil);
    SDL_DestroyTexture(aTex); // ToDo: There is a way to clear a texture?

    // Axis
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 64, 64, 64, 128);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, ZAxisPosY, WinWMax, ZAxisPosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, XAxisPosY, WinWMax, XAxisPosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, WinOffsetY,
      InfoPosX, WinOffsetY);

    SDL_RenderDrawLine(SDLWindow.PRenderer, WinOffsetX, 0,
      WinOffsetX, WinHMax);
    SDL_RenderDrawLine(SDLWindow.PRenderer, YAxisPosX, YFramePosY,
      YAxisPosX, WinHMax);

    // X = 0 position
    i := Round(WinOffsetX - PosX / scale);
    if InRange(i, 0, WinWMax) then
    begin
      SDL_RenderDrawLine(SDLWindow.PRenderer, i, XFramePosY, i, WinHMax);
      characterColor(SDLWindow.PRenderer, i + 1, XAxisPosY +
        1, '0', $444444FF);
      characterColor(SDLWindow.PRenderer, i + 1, WinOffsetY +
        1, '0', $444444FF);
    end;

    // Y = 0 position
    i := Round(WinOffsetY - PosY / scale);
    if InRange(i, YFramePosY, WinHMax) then
    begin
      SDL_RenderDrawLine(SDLWindow.PRenderer, 0, i, MapPosX, i);
      characterColor(SDLWindow.PRenderer, YAxisPosX + 1, i + 1, '0', $444444FF);
    end;

    // Z = 0 position
    i := Round(WinOffsetX - PosZ / scale);
    if InRange(i, 0, WinWMax) then
    begin
      SDL_RenderDrawLine(SDLWindow.PRenderer, i, 0, i, XFramePosY);
      characterColor(SDLWindow.PRenderer, i + 1, ZAxisPosY +
        1, '0', $444444FF);
    end;

    // Borders
    SDL_SetRenderDrawColor(SDLWindow.PRenderer, 255, 255, 255, 255);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, XFramePosY, WinW, XFramePosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, 0, YFramePosY, WinW, YFramePosY);
    SDL_RenderDrawLine(SDLWindow.PRenderer, MapPosX, YFramePosY,
      MapPosX, WinHMax);
    SDL_RenderDrawLine(SDLWindow.PRenderer, InfoPosX, YFramePosY,
      InfoPosX, WinHMax);


    // Wave texts
    aText := 'Z: ' + FloatToStrF(PosZ, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, 0);

    aText := 'X: ' + FloatToStrF(PosX, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, XFramePosY);

    aText := 'Y: ' + FloatToStrF(PosY, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, 0, YFramePosY);

    // Bottom right box
    aText := 'Scale: ' + FloatToStrF(Scale, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX, YFramePosY);

    aText := 'Mouse X: ' + FloatToStrF(MouseX, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX,
      DefFont.YPosAtLine(1, YFramePosY));

    aText := 'Mouse Y: ' + FloatToStrF(MouseY, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX,
      DefFont.YPosAtLine(2, YFramePosY));

    aText := 'Mouse Z: ' + FloatToStrF(MouseZ, ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX,
      DefFont.YPosAtLine(3, YFramePosY));

    aText := 'Value: ' + FloatToStrF(PINNoise(PosX, PosY, PosZ),
      ffFixed, 4, 4);
    DefFont.RenderDynStr(aText, InfoPosX,
      DefFont.YPosAtLine(5, YFramePosY));

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
            PosZ := 0;
          end;
          SDLK_PERIOD, SDLK_KP_PERIOD :
          begin
            PosX := Round(PosX);
            PosY := Round(PosY);
            PosZ := Round(PosZ);
          end;

          // Moving
          SDLK_LEFT : PosX -= Scale;
          SDLK_RIGHT : PosX += Scale;
          SDLK_UP : PosY -= Scale;
          SDLK_DOWN : PosY += Scale;
          SDLK_SLASH, SDLK_KP_DIVIDE : PosZ -= Scale;
          SDLK_ASTERISK, SDLK_KP_MULTIPLY : PosZ += Scale;

          // Drawings
          SDLK_m : DrawMap := not DrawMap;

            //SDLK_SPACE : ;
          else
            ;
        end;
      end;

      SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
      begin
        // In Z Wave frame, X changes Z only
        if aEvent.motion.Y < XFramePosY then
        begin
          MouseX := PosX;
          MouseY := PosY;
          MouseZ := PosZ + Scale * (aEvent.motion.X - WinOffsetX);
        end
        else // Rest of the window
        begin
          MouseZ := PosZ;
          MouseX := PosX + Scale * (aEvent.motion.X - WinOffsetX);

          // Map zone
          if aEvent.motion.Y > YFramePosY then
          begin
            MouseY := PosY + Scale * (aEvent.motion.Y - WinOffsetY);

            if (aEvent.motion.X < MapPosX) then
              // Inside Y wave, keep original X
              MouseX := PosX;
          end
          else
            MouseY := PosY;
        end;
      end;

      //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
      SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
      begin
        // Any button
        PosX := MouseX;
        PosY := MouseY;
        PosZ := MouseZ;
      end;

      SDL_MOUSEWHEEL : // (wheel: TSDL_MouseWheelEvent);
      begin
        if aEvent.wheel.Y > 0 then
          PosZ += Scale
        else
          PosZ -= Scale;
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
