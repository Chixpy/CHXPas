program SDLInfo;

uses
  Classes,
  SysUtils,
  StrUtils,
  CTypes,
  SDL2;

  {$R *.res}

var
  SDLWin : PSDL_Window;
  SDLRend : PSDL_Renderer;
  SDLVersion : TSDL_Version;
  SDLPowerState : TSDL_PowerState;
  SDLAudioSpec : TSDL_AudioSpec;
  SDLRect : TSDL_Rect;
  SDLDisplayMode : TSDL_DisplayMode;
  SDL_RendererInfo : TSDL_RendererInfo;
  aIntA, aIntB, aIntC, aIntD : CInt;
  aFloatA, aFloatB, aFloatC : cfloat;
  aMin, aMax : integer;
  aGUIDStr : string;

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
    Write('  + SDL_GetDesktopDisplayMode(', aIntB, '): ');
    WriteLn(SDLDisplayMode.w, 'x', SDLDisplayMode.h, '@',
      SDLDisplayMode.refresh_rate, ' - ', SDL_GetPixelFormatName(
      SDLDisplayMode.format));
    SDL_GetCurrentDisplayMode(aIntB, @SDLDisplayMode);
    Write('  + SDL_GetCurrentDisplayMode(', aIntB, '): ');
    WriteLn(SDLDisplayMode.w, 'x', SDLDisplayMode.h, '@',
      SDLDisplayMode.refresh_rate, ' - ', SDL_GetPixelFormatName(
      SDLDisplayMode.format));
  end;

  WriteLn('SDL_IsScreenSaverEnabled: ', SDL_IsScreenSaverEnabled);
  WriteLn;

  // sdl_renderer.h
  aIntA := SDL_GetNumRenderDrivers;
  WriteLn('SDL_GetNumRenderDrivers: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    SDL_GetRenderDriverInfo(aIntB, @SDL_RendererInfo);
    WriteLn('+ (', aIntB, ') SDL_RendererInfo.name: ', SDL_RendererInfo.Name);
    WriteLn('  + SDL_RendererInfo.flags: ', SDL_RendererInfo.flags);
    WriteLn('  + SDL_RendererInfo.num_texture_formats: ',
      SDL_RendererInfo.num_texture_formats);
    //WriteLn('  + SDL_RendererInfo.texture_formats: ', SDL_RendererInfo.texture_formats);
    WriteLn('  + SDL_RendererInfo.max_texture_width: ',
      SDL_RendererInfo.max_texture_width);
    WriteLn('  + SDL_RendererInfo.max_texture_height: ',
      SDL_RendererInfo.max_texture_height);
  end;


  WriteLn;

  // sdl_joystick.h
  aIntA := SDL_NumJoysticks;
  aIntC := 0;
  WriteLn('SDL_NumJoysticks: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_JoystickNameForIndex(', aIntB, '): ',
      SDL_JoystickNameForIndex(aIntB));
    WriteLn('  + SDL_JoystickPathForIndex(', aIntB, '): ',
      SDL_JoystickPathForIndex(aIntB));
    WriteLn('  + SDL_JoystickGetDevicePlayerIndex(', aIntB, '): ',
      SDL_JoystickGetDevicePlayerIndex(aIntB));
    //SDL_GUIDToString(SDL_JoystickGetDeviceGUID(aIntB), @aGUIDStr,)
    //WriteLn('  + SDL_JoystickGetDeviceGUID(', aIntB, '): ',
    //  SDL_JoystickGetDeviceGUID(aIntB));
    WriteLn('  + SDL_JoystickGetDeviceVendor(', aIntB, '): ',
      SDL_JoystickGetDeviceVendor(aIntB));
    WriteLn('  + SDL_JoystickGetDeviceProduct(', aIntB, '): ',
      SDL_JoystickGetDeviceProduct(aIntB));
    WriteLn('  + SDL_JoystickGetDeviceProductVersion(', aIntB,
      '): ', SDL_JoystickGetDeviceProductVersion(aIntB));
    WriteLn('  + SDL_JoystickGetDeviceType(', aIntB, '): ',
      SDL_JoystickGetDeviceType(aIntB));
    WriteLn('  + SDL_JoystickGetDeviceInstanceID(', aIntB, '): ',
      SDL_JoystickGetDeviceInstanceID(aIntB));

    // sdl_gamecontroller.h
    if SDL_IsGameController(aIntB) then
      Inc(aIntC);
    WriteLn('  + SDL_GameControllerNameForIndex(', aIntB, '): ',
      SDL_GameControllerNameForIndex(aIntB));
    WriteLn('  + SDL_GameControllerPathForIndex(', aIntB, '): ',
      SDL_GameControllerPathForIndex(aIntB));
    WriteLn('  + SDL_GameControllerTypeForIndex(', aIntB, '): ',
      SDL_GameControllerTypeForIndex(aIntB));
    WriteLn('  + SDL_GameControllerMappingForDeviceIndex(', aIntB, '): ',
      SDL_GameControllerMappingForDeviceIndex(aIntB));
  end;
  WriteLn('Game Controllers: ', aIntC);
  WriteLn;

  // sdl_sensor.h
  aIntA := SDL_NumSensors;
  WriteLn('SDL_NumSensors: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_SensorGetDeviceName(', aIntB, '): ',
      SDL_SensorGetDeviceName(aIntB));
    WriteLn('  + SDL_SensorGetDeviceType(', aIntB, '): ',
      SDL_SensorGetDeviceType(aIntB));
    WriteLn('  + SDL_SensorGetDeviceNonPortableType(', aIntB, '): ',
      SDL_SensorGetDeviceNonPortableType(aIntB));
    WriteLn('  + SDL_SensorGetDeviceInstanceID(', aIntB, '): ',
      SDL_SensorGetDeviceInstanceID(aIntB));
  end;
  WriteLn;

  // sdl_haptic.h
  aIntA := SDL_NumHaptics;
  WriteLn('SDL_NumHaptics: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_HapticName(', aIntB, '): ',
      SDL_HapticName(aIntB));
  end;
  WriteLn('SDL_MouseIsHaptic: ', SDL_MouseIsHaptic);
  WriteLn;

  { TODO : sdl_hidapi.h }

  // sdl_touch.h
  aIntA := SDL_GetNumTouchDevices;
  WriteLn('SDL_GetNumTouchDevices: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
  begin
    WriteLn('+ SDL_GetTouchName(', aIntB, '): ',
      SDL_GetTouchName(aIntB));
    // TODO: SDL_GetTouchDeviceType
  end;
  WriteLn;

  // sdl_locale.h
  // TODO: SDL_GetPreferredLocales

  // sdl_cpuinfo.h
  WriteLn('SDL_GetCPUCount: ', SDL_GetCPUCount);
  WriteLn('SDL_GetCPUCacheLineSize: ', SDL_GetCPUCacheLineSize);
  WriteLn('SDL_HasRDTSC: ', SDL_HasRDTSC);
  WriteLn('SDL_HasAltiVec : ', SDL_HasAltiVec);
  WriteLn('SDL_HasMMX: ', SDL_HasMMX);
  WriteLn('SDL_Has3DNow: ', SDL_Has3DNow);
  WriteLn('SDL_HasSSE: ', SDL_HasSSE);
  WriteLn('SDL_HasSSE2: ', SDL_HasSSE2);
  WriteLn('SDL_HasSSE3: ', SDL_HasSSE3);
  WriteLn('SDL_HasSSE41: ', SDL_HasSSE41);
  WriteLn('SDL_HasSSE42: ', SDL_HasSSE42);
  WriteLn('SDL_HasAVX: ', SDL_HasAVX);
  WriteLn('SDL_HasAVX2: ', SDL_HasAVX2);
  WriteLn('SDL_HasAVX512F: ', SDL_HasAVX512F);
  WriteLn('SDL_HasARMSIMD: ', SDL_HasARMSIMD);
  WriteLn('SDL_HasNEON: ', SDL_HasNEON);
  WriteLn('SDL_GetSystemRAM: ', SDL_GetSystemRAM);
  WriteLn('SDL_SIMDGetAlignment: ', SDL_SIMDGetAlignment);
  WriteLn;

  // sdl_filesystem.h
  WriteLn('SDL_GetBasePath: ', SDL_GetBasePath);
  WriteLn('SDL_GetPrefPath(''Chixpy'', ''SDLInfo''): ', SDL_GetBasePath);

  WriteLn;
  WriteLn('Return or close this window to exit.');
  ReadLn;
  SDL_Quit;
end.
