program SDLTest1;

uses
  Classes,
  SysUtils,
  StrUtils,
  ctypes,
  sdl2;

  {$R *.res}

var
  SDLWin : PSDL_Window;
  SDLRend : PSDL_Renderer;
  SDLVersion : TSDL_Version;
  SDLPowerState : TSDL_PowerState;
  SDLAudioSpec : TSDL_AudioSpec;
  SDLRect : TSDL_Rect;
  SDLDisplayMode : TSDL_DisplayMode;
  aIntA, aIntB, aIntC, aIntD : CInt;
  aFloatA, aFloatB, aFloatC : cfloat;
  aMin, aMax : integer;

begin
  SDL_Init(SDL_INIT_EVERYTHING);

  // sdl_version.h
  SDL_VERSION(SDLVersion);
  WriteLn('SDL_VERSION (Compiled): ', SDLVersion.major, '.',
    SDLVersion.minor, '.', SDLVersion.patch);
  SDL_GetVersion(@SDLVersion);
  WriteLn('SDL_GetVersion (Linked): ', SDLVersion.major, '.',
    SDLVersion.minor, '.', SDLVersion.patch, ' - ', SDL_GetRevision);
  WriteLn;

  // sdl_platform.h
  WriteLn('SDL_GetPlatform: ' + SDL_GetPlatform);
  WriteLn;

  // sdl_power.h
  SDLPowerState := SDL_GetPowerInfo(@aIntA, @aIntB);
  WriteLn('SDL_GetPowerInfo (Battery): ', SDLPowerState,
    ' - Life: ', aIntB, '% (', aIntA, ' seconds)');
  WriteLn;

  // sdl_timer.h
  WriteLn('SDL_GetTicks: ', SDL_GetTicks, ' - SDL_GetTicks64: ',
    SDL_GetTicks64);
  WriteLn('SDL_GetPerformanceCounter: ', SDL_GetPerformanceCounter,
    ' - SDL_GetPerformanceFrequency: ', SDL_GetPerformanceFrequency);
  WriteLn;

  // sdl_audio.h
  aIntA := SDL_GetNumAudioDrivers;
  WriteLn('SDL_GetNumAudioDrivers: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
    WriteLn('+ SDL_GetAudioDriver(', aIntB, '): ', SDL_GetAudioDriver(aIntB));
  WriteLn('SDL_GetCurrentAudioDriver: ', SDL_GetCurrentAudioDriver);
  WriteLn;

  aIntA := SDL_GetNumAudioDevices(0);
  WriteLn('SDL_GetNumAudioDevices (Playback): ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_GetAudioDeviceName(', aIntB, '): ',
      SDL_GetAudioDeviceName(aIntB, 0));
    SDL_GetAudioDeviceSpec(aIntB, 0, @SDLAudioSpec);
    Write('  + SDL_GetAudioDeviceSpec(', aIntB, '): ');
    Write(SDLAudioSpec.freq, 'Hz - ');
    Write(SDL_AUDIO_BITSIZE(SDLAudioSpec.format), ' bits - ');
    WriteLn(SDLAudioSpec.channels, ' channels');
  end;
  aIntA := SDL_GetNumAudioDevices(1);
  WriteLn('SDL_GetNumAudioDevices (Recording): ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_GetAudioDeviceName(', aIntB, '): ',
      SDL_GetAudioDeviceName(aIntB, 1));
    SDL_GetAudioDeviceSpec(aIntB, 1, @SDLAudioSpec);
    Write('  + SDL_GetAudioDeviceSpec(', aIntB, '): ');
    Write(SDLAudioSpec.freq, 'Hz - ');
    Write(SDL_AUDIO_BITSIZE(SDLAudioSpec.format), ' bits - ');
    WriteLn(SDLAudioSpec.channels, ' channels');
  end;
  WriteLn;

  // sdl_video.h
  aIntA := SDL_GetNumVideoDrivers;
  WriteLn('SDL_GetNumVideoDrivers: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_GetVideoDriver(', aIntB, '): ', SDL_GetVideoDriver(aIntB));
  end;
  WriteLn('SDL_GetCurrentVideoDriver: ', SDL_GetCurrentVideoDriver);
  WriteLn;

  aIntA := SDL_GetNumVideoDisplays;
  WriteLn('SDL_GetNumVideoDisplays: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_GetDisplayName(', aIntB, '): ', SDL_GetDisplayName(aIntB));
    SDL_GetDisplayBounds(aIntB, @SDLRect);
    Write('  + SDL_GetDisplayBounds(', aIntB, '): ');
    WriteLn(SDLRect.x, 'x', SDLRect.y, ' Size: ', SDLRect.w, 'x', SDLRect.h);
    SDL_GetDisplayUsableBounds(aIntB, @SDLRect);
    Write('  + SDL_GetDisplayUsableBounds(', aIntB, '): ');
    WriteLn(SDLRect.x, 'x', SDLRect.y, ' Size: ', SDLRect.w, 'x', SDLRect.h);
    SDL_GetDisplayDPI(aIntB, @aFloatA, @aFloatB, @aFloatC);
    Write('  + SDL_GetDisplayDPI(', aIntB, '): ');
    WriteLn('d: ', aFloatA, ' h: ', SDLRect.w, ' v:', SDLRect.h);
    WriteLn('  + SDL_GetDisplayOrientation(', aIntB, '): ',
      SDL_GetDisplayOrientation(aIntB));
    aIntC := SDL_GetNumDisplayModes(aIntB);
    WriteLn('  + SDL_GetNumDisplayModes(', aIntB, '): ', aIntC);

    if aIntC > 0 then
    begin
      aIntD := 0;
      SDL_GetDisplayMode(aIntB, aIntD, @SDLDisplayMode);
      Write('    + SDL_GetDisplayMode(', aIntB, ',', aIntD, ') (Highest): ');
      WriteLn(SDLDisplayMode.w, 'x', SDLDisplayMode.h, '@',
        SDLDisplayMode.refresh_rate, ' - ', SDL_GetPixelFormatName(
        SDLDisplayMode.format));
    end;
    if aIntC > 1 then
    begin
      aIntD := aIntC - 1;
      SDL_GetDisplayMode(aIntB, aIntD, @SDLDisplayMode);
      Write('    + SDL_GetDisplayMode(', aIntB, ',', aIntD, ') (Lowest): ');
      WriteLn(SDLDisplayMode.w, 'x', SDLDisplayMode.h, '@',
        SDLDisplayMode.refresh_rate, ' - ', SDL_GetPixelFormatName(
        SDLDisplayMode.format));
    end;
    SDL_GetDesktopDisplayMode(aIntB, @SDLDisplayMode);
    WriteLn('  + SDL_GetDesktopDisplayMode(', aIntB, '): ');
    WriteLn(SDLDisplayMode.w, 'x', SDLDisplayMode.h, '@',
      SDLDisplayMode.refresh_rate, ' - ', SDL_GetPixelFormatName(
      SDLDisplayMode.format));
     SDL_GetCurrentDisplayMode(aIntB, @SDLDisplayMode);
    WriteLn('  + SDL_GetCurrentDisplayMode(', aIntB, '): ');
    WriteLn(SDLDisplayMode.w, 'x', SDLDisplayMode.h, '@',
      SDLDisplayMode.refresh_rate, ' - ', SDL_GetPixelFormatName(
      SDLDisplayMode.format));
  end;
  WriteLn;

  WriteLn;
  WriteLn('Close this window to exit.');
  ReadLn;
  SDL_Quit;
end.
