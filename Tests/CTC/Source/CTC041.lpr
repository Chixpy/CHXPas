program CTC041;
{< The Coding Train Challenge #041 - Clappy Bird }

// TODO: Not working!!!
//   Por el momento, lo voy a dejar apartado.
//   Lo que estoy haciendo es a ver si soy capaz de interpretar 'stream'
//     en AudioCB, pero esta funcion es llamada varias veces por frame
//     y no logro obtener algun valor significativo.

// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for: https://youtu.be/aKiyCeIuwn4
// Ported to Processing 4 by Spencer Stith<http://github.com/spencerstith>
// Port: (C) 2024 Chixpy https://github.com/Chixpy
{$mode ObjFPC}{$H+}

uses
  Classes, SysUtils, CTypes, StrUtils, FileUtil, LazFileUtils, Math, FGL,
  SDL2, SDL2_GFX, SDL2_TTF, SDL2_Image,
  uCHXStrUtils,
  ucCHXSDL2Engine, ucCHXSDL2FontTTF, uCHXSDL2Utils, uProcUtils,
  ucBird, ucPipe;

const
  { CHX: Renderer scales images to actual size of the window. }
  WinW = 640; { CHX: Window logical width. }
  WinH = 480; { CHX: Window logical height. }

type
  cPipeList = specialize TFPGObjectList<cPipe>;

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
    bird : cBird;
    pipes : cPipeList;

    // SDL audio stuff
    AudioDevice : TSDL_AudioDeviceID;
    AudioSpec : TSDL_AudioSpec;
    TextADX : Integer; // Audio driver text X position
    TextADNX : Integer; // Audio device name text X position

    wave : array[0..WinW] of Integer; // AUDIO_F32LSB or AUDIO_F32MSB
    WavePos : Integer;
  end;

  // Audio Callback
  { Actually, SDL runs audio subsystem in different thread calling a
      callback function when a device is active to feed it with audio data.

    Recording it feeds a stream to play with it.
  }
  procedure AudioCB(userdata : Pointer; stream : pcuint8; len : cint); cdecl;
  var
    CTCEng : cCTCEng;
    aStream : TMemoryStream;
    ASp : TSDL_AudioSpec; //CTCEng.AudioSpec shortcut
    Bytes4Sample : Cardinal;
    Skip, cValue : Cardinal;
    RawBVAlue: Byte;
    RawWValue : DWord;
    dValue : ^Double;
  begin
    if not assigned(userdata) or (len = 0) then Exit;

    CTCEng := cCTCEng(userdata);

    ASp := CTCEng.AudioSpec;

    Bytes4Sample := (SDL_AUDIO_BITSIZE(ASp.format) div 8);
    Skip := Bytes4Sample * (ASp.channels - 1);
    // Read 1 channel skipping others

    aStream := TMemoryStream.Create;
    aStream.WriteBuffer(stream, len);
    aStream.Seek(0, soFromBeginning);

    while aStream.Position < aStream.Size do
    begin

      // TODO: Make it for other formats
      case ASp.format of
        AUDIO_F32MSB :
        begin
          RawBValue := aStream.ReadByte;
          RawWValue := RawBValue shl 24;
          RawBValue := aStream.ReadByte;
          RawWValue := RawWValue or (RawBValue shl 16);
          RawBValue := aStream.ReadByte;
          RawWValue := RawWValue or (RawBValue shl 8);
          RawBValue := aStream.ReadByte;
          RawWValue := RawWValue or (RawBValue);

          aStream.Seek(Skip, soFromCurrent);

          dValue := @RawWValue;
          cValue := round(map(dValue^, -1, 1, 0, WinH));
        end;

        AUDIO_F32LSB :
        begin
          RawWValue := aStream.ReadDWord;
          aStream.Seek(Skip, soFromCurrent);

          dValue := @RawWValue;
          cValue := round(map(dValue^, -1, 1, 0, WinH));
        end;
        else;
      end;

      CTCEng.Wave[CTCEng.WavePos] := cValue;
      Inc(CTCEng.WavePos);
      CTCEng.WavePos := CTCEng.WavePos mod WinW;
    end;

    aStream.Free;

    //CTCEng.bird.up;
  end;

  { cCTCEng }

  procedure cCTCEng.Setup;
  var
    PAudioDeviceName : pansichar;
    AudioDeviceName : string;
    DefAudioSpec : TSDL_AudioSpec;
  begin
    bird := cBird.Create(WinW, WinH);
    pipes := cPipeList.Create(True);
    pipes.add(cPipe.Create(WinW, WinH));

    // SDL audio stuff..., using default settings
    SDL_GetDefaultAudioInfo(@PAudioDeviceName, @DefAudioSpec, 1);

    DefAudioSpec.callback := @AudioCB;
    DefAudioSpec.userdata := Self; //So, we can access to cCTCEng

    AudioDeviceName := PAudioDeviceName;
    SDL_free(PAudioDeviceName);

    AudioDevice := SDL_OpenAudioDevice(nil, 1, @DefAudioSpec, @AudioSpec,
      SDL_AUDIO_ALLOW_ANY_CHANGE);

    SDL_PauseAudioDevice(AudioDevice, 0);

    // Input device info
    TextADX := WinW - DefFont.AddStaticStr('AudioDriver',
      'Audio Driver: ' + SDL_GetCurrentAudioDriver) - 10;
    TextADNX := WinW - DefFont.AddStaticStr('AudioDeviceName',
      'Audio Device: ' + AudioDeviceName) - 10;

    WavePos := 0;
  end;

  procedure cCTCEng.Finish;
  begin
    { CHX: Free any created objects. }
    SDL_PauseAudioDevice(AudioDevice, 1);
    SDL_CloseAudioDevice(AudioDevice);

    bird.Free;
    pipes.Free;
  end;

  procedure cCTCEng.Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
  var
    aPipe : cPipe;
    i : Integer;
  begin
    if ExitProg then Exit;
    { CHX: If we want to pause when minimized. }
    // if SDLWindow.Minimized then Exit;

    for i := pipes.Count - 1 downto 0 do
    begin
      aPipe := pipes[i];
      aPipe.Update;
      aPipe.hits(bird);
      if aPipe.offscreen then
        pipes.Delete(i);
    end;

    bird.update;

    if frameCount mod 100 = 0 then
      pipes.add(cPipe.Create(WinW, WinH));
  end;

  procedure cCTCEng.Draw;
  var
    aPipe : cPipe;
    Color : CUInt32;
    X, Y : Integer;
  begin
    // Background and frame clear.
    SDL_SetRenderDrawColor(SDL2R, 0, 0, 0, 255);
    SDL_RenderClear(SDL2R);

    for aPipe in pipes do
    begin
      // cPipe.show
      if aPipe.highlight then
        Color := $FF0000FF // $AABBGGRR in windows
      else
        Color := $FFFFFFFF;
      boxColor(SDL2R, aPipe.X, 0, aPipe.X + aPipe.w, Round(aPipe.top), Color);
      boxColor(SDL2R, aPipe.X, WinH - Round(aPipe.bottom),
        aPipe.X + aPipe.w, WinH, Color);
    end;

    // cBird.show
    filledCircleColor(SDL2R, bird.X, bird.Y, 16, $FFFFFFFF);

    // Audio driver info
    DefFont.RenderStatic('AudioDriver', TextADX, 10);
    DefFont.RenderStatic('AudioDeviceName', TextADNX, DefFont.LinePosY(1, 10));

    SDL_SetRenderDrawColor(SDL2R, 255, 0, 0, 255);
    for X := 0 to Length(wave) - 1 do
    begin
      Y := wave[X];
      //if Y > 0 then
      //  DefFont.RenderDynStr(UIntToStr(wave[X]), 0,0);
      SDL_RenderDrawPoint(SDL2R, X, Y);
    end;
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

    case aEvent.type_ of
      SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
      begin
        bird.up;
        //case aEvent.key.keysym.sym of
        //  //SDLK_UP : ;
        //  //SDLK_DOWN : ;
        //  //SDLK_LEFT : ;
        //  //SDLK_RIGHT : ;
        //  //SDLK_SPACE : ;
        //  //SDLK_RETURN : ;
        //end;
      end;
        //SDL_MOUSEMOTION : // (motion: TSDL_MouseMotionEvent);
        //SDL_MOUSEBUTTONUP : // (button: TSDL_MouseButtonEvent);
        //SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
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
