program TestSDL3Info;
{<
  Program that tries to init SDL3 and gives info about it.

  ToDo:
  - Clean the code.
  - Use SDL_Log instead WriteLn.
  - Seems that P(Ansi)Char and Pointer Arrays don't need SDL_Free in FPC,
    althought SDL docs say to do it. SDL_GetNumAllocations returns -1.
  - Maybe make a procedure for each unit (well, header).
  - Hints are constant strings, without any iterable list.
  - Properties are used in various units.
}
{$mode objfpc}{$H+}
uses
  Classes,
  SysUtils,
  StrUtils,
  CTypes,
  SDL3;

var
  // Temp vars
  aInt1, aInt2: Integer; // Temp integers
  aCInt1, aCInt2: CInt; // Temp C integers
  aStr: String; // Temp strings
  aPAnsiChar: PAnsiChar; // String null terminated (C String)
  aPPAnsiChar: PPAnsiChar; // Pointer Array of C String
  CurrPPAnsiChar: PPAnsiChar; // To access the array of C String
  // SDL_locale.h
  LocalesList, CurrLocale: PPSDL_Locale;
  // SDL_stdinc.h-prev (unfinished)
  Environment: PSDL_Environment;
  // SDL_properties.h
  PropertiesID: TSDL_PropertiesID;
  // SDL_video
  DisplayIDList, CurrDisplayID: PSDL_DisplayID;
  aSDLRect: TSDL_Rect;
  DisplayModeList, CurrDisplayMode: PPSDL_DisplayMode;
  // SDL_audio
  AudioDeviceIDList, CurrAudioDeviceID: PSDL_AudioDeviceID;
  AudioSpec: TSDL_AudioSpec;
  // SDL_power
  PowerState: TSDL_PowerState;


procedure WriteSection(const aFileName: String);
begin
  WriteLn;
  WriteLn('=== ', aFileName, ' ===');
  WriteLn;
end;

procedure WritePriority(const Category: Integer);
begin
  Write(Category:4, ' ');
  case Category of
    SDL_LOG_CATEGORY_APPLICATION: Write('Application');
    SDL_LOG_CATEGORY_ERROR: Write('Error');
    SDL_LOG_CATEGORY_ASSERT: Write('Assert');
    SDL_LOG_CATEGORY_SYSTEM: Write('System');
    SDL_LOG_CATEGORY_AUDIO: Write('Audio');
    SDL_LOG_CATEGORY_VIDEO: Write('Video');
    SDL_LOG_CATEGORY_RENDER: Write('Render');
    SDL_LOG_CATEGORY_INPUT: Write('Input');
    SDL_LOG_CATEGORY_TEST: Write('Test');
    SDL_LOG_CATEGORY_GPU: Write('GPU');
    SDL_LOG_CATEGORY_RESERVED2..SDL_LOG_CATEGORY_RESERVED10:
      Write('Reserved');
  otherwise // SDL_LOG_CATEGORY_CUSTOM
    Write('Custom');
  end;

  Write(': ');

  case SDL_GetLogPriority(Category) of
    SDL_LOG_PRIORITY_TRACE: WriteLn('Trace');
    SDL_LOG_PRIORITY_VERBOSE: WriteLn('Verbose');
    SDL_LOG_PRIORITY_DEBUG: WriteLn('Debug');
    SDL_LOG_PRIORITY_INFO: WriteLn('Info');
    SDL_LOG_PRIORITY_WARN: WriteLn('Warn');
    SDL_LOG_PRIORITY_ERROR: WriteLn('Error');
    SDL_LOG_PRIORITY_CRITICAL: WriteLn('Critical');
    SDL_LOG_PRIORITY_COUNT: WriteLn('Count');
  otherwise // SDL_LOG_PRIORITY_INVALID = TSDL_LogPriority(0);
    WriteLn('Invalid');
  end;
end;

procedure WriteHint(const aHint: PAnsiChar);
var
  aValue: PAnsiChar;
begin
  aValue := SDL_GetHint(aHint);
  if aValue <> '' then
    WriteLn('  ', aHint, ': ', aValue);
end;

procedure WriteProperty(userdata: Pointer; props: TSDL_PropertiesID;
  name: PAnsiChar); cdecl;
{< SDL_EnumerateProperties callback. }
begin
  Write('  ', name, ' = ');
  case SDL_GetPropertyType(props, name) of
    SDL_PROPERTY_TYPE_POINTER:
      WriteLn('(Pointer) ', PtrUInt(SDL_GetPointerProperty(props, name, nil)));
    SDL_PROPERTY_TYPE_STRING:
      WriteLn('(String) ', SDL_GetStringProperty(props, name, ''));
    SDL_PROPERTY_TYPE_NUMBER:
      WriteLn('(Number) ', SDL_GetNumberProperty(props, name, -1));
    SDL_PROPERTY_TYPE_FLOAT:
      WriteLn('(Float) ', SDL_GetFloatProperty(props, name, -1));
    SDL_PROPERTY_TYPE_BOOLEAN:
      WriteLn('(Boolean) ', SDL_GetBooleanProperty(props, name, False));
  otherwise //SDL_PROPERTY_TYPE_INVALID or other
      WriteLn('Unknown type of propety.');
  end;
end;

procedure WriteOrientation(aOrientation: TSDL_DisplayOrientation);
begin
  case aOrientation of
    SDL_ORIENTATION_LANDSCAPE: WriteLn('Landscape');
    SDL_ORIENTATION_LANDSCAPE_FLIPPED: WriteLn('Landscape Flipped');
    SDL_ORIENTATION_PORTRAIT: WriteLn('Portrait');
    SDL_ORIENTATION_PORTRAIT_FLIPPED: WriteLn('Portrait Flipped');
  otherwise // SDL_ORIENTATION_UNKNOWN
    WriteLn('Unknown')
  end;
end;

begin
  // Althought SDL_init is included much later is the first to be tested
  
  // Seems to be a good practice with SDL3.
  // They must be before SDL_Init[Subsystem] call
  SDL_SetAppMetadata('SDL3 Info Test', '1.0', 'com.chixpy.TestSDL3Info');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_CREATOR_STRING, 'Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_COPYRIGHT_STRING,
    '(C) 2026 Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_URL_STRING,
    'https://github.com/Chixpy');
  SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_TYPE_STRING, 'application');

  SDL_Init(SDL_INIT_AUDIO { implies `SDL_INIT_EVENTS` }
    + SDL_INIT_VIDEO { implies `SDL_INIT_EVENTS`, req. to be in main thread }
    + SDL_INIT_JOYSTICK { implies `SDL_INIT_EVENTS` }
    + SDL_INIT_HAPTIC
    + SDL_INIT_GAMEPAD { implies `SDL_INIT_JOYSTICK` }
    + SDL_INIT_EVENTS
    + SDL_INIT_SENSOR { implies `SDL_INIT_EVENTS` }
    + SDL_INIT_CAMERA { implies `SDL_INIT_EVENTS` }
    );
  try // Call SDL_Quit on error or Exit (yes, Exit too)

    // SDL_log.h
    
    WriteSection('SDL_log.h');
    
    WriteLn('Log priorities:');
    for aInt1 := SDL_LOG_CATEGORY_APPLICATION to SDL_LOG_CATEGORY_CUSTOM do
      WritePriority(aInt1);

    // SDL_version.h

    WriteSection('SDL_version.h');
    
    aInt1 := SDL_VERSION;
    WriteLn(Format('SDL_VERSION (Compiled): %d (%d.%d.%d)',
      [aInt1, SDL_VERSIONNUM_MAJOR(aInt1), SDL_VERSIONNUM_MINOR(aInt1),
      SDL_VERSIONNUM_MICRO(aInt1)]));
    aCInt1 := SDL_GetVersion;
    WriteLn(Format('SDL_GetVersion (Linked): %d (%d.%d.%d)',
      [aCInt1, SDL_VERSIONNUM_MAJOR(aCInt1), SDL_VERSIONNUM_MINOR(aCInt1),
      SDL_VERSIONNUM_MICRO(aCInt1)]));
    Write('SDL_GetRevision: ');
    WriteLn(SDL_GetRevision);

    // SDL_revision.h

    WriteSection('SDL_revision.h');
    
    Write('SDL_REVISION: ');
    WriteLn(SDL_REVISION);

    // SDL_locale.h

    WriteSection('SDL_locale.h');
    
    LocalesList := SDL_GetPreferredLocales(@aCInt1);
    WriteLn('Number of preferred locales: ', aCInt1);

    // Recorriendo Array de Punteros a Record
    CurrLocale := LocalesList;
    while Assigned(CurrLocale^) do
    begin
      Write('  ', CurrLocale^^.language);
      if Assigned(CurrLocale^^.country) then
        Write('_', CurrLocale^^.country);
      WriteLn;
      CurrLocale += 1; //or SizeOf(PSDL_Locale); Not sure;
    end;
    SDL_Free(LocalesList);

    // SDL_guid.h
    // WriteSection('SDL_guid.h');

    // SDL_hints.h

    WriteSection('SDL_hints.h');
    
    WriteLn('Hints con valor asignado:');
    for aPAnsiChar in [SDL_HINT_ALLOW_ALT_TAB_WHILE_GRABBED,
    SDL_HINT_ANDROID_ALLOW_RECREATE_ACTIVITY, SDL_HINT_ANDROID_BLOCK_ON_PAUSE,
    SDL_HINT_ANDROID_LOW_LATENCY_AUDIO, SDL_HINT_ANDROID_TRAP_BACK_BUTTON,
    SDL_HINT_APP_ID, SDL_HINT_APP_NAME, SDL_HINT_APPLE_TV_CONTROLLER_UI_EVENTS,
    SDL_HINT_APPLE_TV_REMOTE_ALLOW_ROTATION, SDL_HINT_AUDIO_ALSA_DEFAULT_DEVICE,
    SDL_HINT_AUDIO_ALSA_DEFAULT_PLAYBACK_DEVICE,
    SDL_HINT_AUDIO_ALSA_DEFAULT_RECORDING_DEVICE,
    SDL_HINT_AUDIO_CATEGORY, SDL_HINT_AUDIO_CHANNELS,
    SDL_HINT_AUDIO_DEVICE_APP_ICON_NAME, SDL_HINT_AUDIO_DEVICE_SAMPLE_FRAMES,
    SDL_HINT_AUDIO_DEVICE_STREAM_NAME, SDL_HINT_AUDIO_DEVICE_STREAM_ROLE,
    SDL_HINT_AUDIO_DEVICE_RAW_STREAM, SDL_HINT_AUDIO_DISK_INPUT_FILE,
    SDL_HINT_AUDIO_DISK_OUTPUT_FILE, SDL_HINT_AUDIO_DISK_TIMESCALE,
    SDL_HINT_AUDIO_DRIVER, SDL_HINT_AUDIO_DUMMY_TIMESCALE,
    SDL_HINT_AUDIO_FORMAT, SDL_HINT_AUDIO_FREQUENCY,
    SDL_HINT_AUDIO_INCLUDE_MONITORS, SDL_HINT_AUTO_UPDATE_JOYSTICKS,
    SDL_HINT_AUTO_UPDATE_SENSORS, SDL_HINT_BMP_SAVE_LEGACY_FORMAT,
    SDL_HINT_CAMERA_DRIVER, SDL_HINT_CPU_FEATURE_MASK,
    SDL_HINT_JOYSTICK_DIRECTINPUT, SDL_HINT_FILE_DIALOG_DRIVER,
    SDL_HINT_DISPLAY_USABLE_BOUNDS, SDL_HINT_INVALID_PARAM_CHECKS,
    SDL_HINT_EMSCRIPTEN_ASYNCIFY, SDL_HINT_EMSCRIPTEN_CANVAS_SELECTOR,
    SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT, SDL_HINT_ENABLE_SCREEN_KEYBOARD,
    SDL_HINT_EVDEV_DEVICES, SDL_HINT_EVENT_LOGGING, SDL_HINT_FORCE_RAISEWINDOW,
    SDL_HINT_FRAMEBUFFER_ACCELERATION, SDL_HINT_GAMECONTROLLERCONFIG,
    SDL_HINT_GAMECONTROLLERCONFIG_FILE, SDL_HINT_GAMECONTROLLERTYPE,
    SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES,
    SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT,
    SDL_HINT_GAMECONTROLLER_SENSOR_FUSION, SDL_HINT_GDK_TEXTINPUT_DEFAULT_TEXT,
    SDL_HINT_GDK_TEXTINPUT_DESCRIPTION, SDL_HINT_GDK_TEXTINPUT_MAX_LENGTH,
    SDL_HINT_GDK_TEXTINPUT_SCOPE, SDL_HINT_GDK_TEXTINPUT_TITLE,
    SDL_HINT_HIDAPI_LIBUSB, SDL_HINT_HIDAPI_LIBUSB_GAMECUBE,
    SDL_HINT_HIDAPI_LIBUSB_WHITELIST, SDL_HINT_HIDAPI_UDEV, SDL_HINT_GPU_DRIVER,
    SDL_HINT_HIDAPI_ENUMERATE_ONLY_CONTROLLERS, SDL_HINT_HIDAPI_IGNORE_DEVICES,
    SDL_HINT_IME_IMPLEMENTED_UI, SDL_HINT_IOS_HIDE_HOME_INDICATOR,
    SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS,
    SDL_HINT_JOYSTICK_ARCADESTICK_DEVICES,
    SDL_HINT_JOYSTICK_ARCADESTICK_DEVICES_EXCLUDED,
    SDL_HINT_JOYSTICK_BLACKLIST_DEVICES,
    SDL_HINT_JOYSTICK_BLACKLIST_DEVICES_EXCLUDED, SDL_HINT_JOYSTICK_DEVICE,
    SDL_HINT_JOYSTICK_ENHANCED_REPORTS, SDL_HINT_JOYSTICK_FLIGHTSTICK_DEVICES,
    SDL_HINT_JOYSTICK_FLIGHTSTICK_DEVICES_EXCLUDED, SDL_HINT_JOYSTICK_GAMEINPUT,
    SDL_HINT_JOYSTICK_GAMEINPUT_RAW, SDL_HINT_JOYSTICK_GAMECUBE_DEVICES,
    SDL_HINT_JOYSTICK_GAMECUBE_DEVICES_EXCLUDED, SDL_HINT_JOYSTICK_HIDAPI,
    SDL_HINT_JOYSTICK_HIDAPI_COMBINE_JOY_CONS, SDL_HINT_JOYSTICK_HIDAPI_GAMECUBE,
    SDL_HINT_JOYSTICK_HIDAPI_GAMECUBE_RUMBLE_BRAKE, SDL_HINT_JOYSTICK_HIDAPI_JOY_CONS,
    SDL_HINT_JOYSTICK_HIDAPI_JOYCON_HOME_LED, SDL_HINT_JOYSTICK_HIDAPI_LUNA,
    SDL_HINT_JOYSTICK_HIDAPI_NINTENDO_CLASSIC, SDL_HINT_JOYSTICK_HIDAPI_PS3,
    SDL_HINT_JOYSTICK_HIDAPI_PS3_SIXAXIS_DRIVER, SDL_HINT_JOYSTICK_HIDAPI_PS4,
    SDL_HINT_JOYSTICK_HIDAPI_PS4_REPORT_INTERVAL, SDL_HINT_JOYSTICK_HIDAPI_PS5,
    SDL_HINT_JOYSTICK_HIDAPI_PS5_PLAYER_LED, SDL_HINT_JOYSTICK_HIDAPI_SHIELD,
    SDL_HINT_JOYSTICK_HIDAPI_STADIA, SDL_HINT_JOYSTICK_HIDAPI_STEAM,
    SDL_HINT_JOYSTICK_HIDAPI_STEAM_HOME_LED, SDL_HINT_JOYSTICK_HIDAPI_STEAMDECK,
    SDL_HINT_JOYSTICK_HIDAPI_STEAM_HORI, SDL_HINT_JOYSTICK_HIDAPI_LG4FF,
    SDL_HINT_JOYSTICK_HIDAPI_8BITDO, SDL_HINT_JOYSTICK_HIDAPI_SINPUT,
    SDL_HINT_JOYSTICK_HIDAPI_ZUIKI, SDL_HINT_JOYSTICK_HIDAPI_FLYDIGI,
    SDL_HINT_JOYSTICK_HIDAPI_SWITCH, SDL_HINT_JOYSTICK_HIDAPI_SWITCH_HOME_LED,
    SDL_HINT_JOYSTICK_HIDAPI_SWITCH_PLAYER_LED,
    SDL_HINT_JOYSTICK_HIDAPI_SWITCH2,
    SDL_HINT_JOYSTICK_HIDAPI_VERTICAL_JOY_CONS, SDL_HINT_JOYSTICK_HIDAPI_WII,
    SDL_HINT_JOYSTICK_HIDAPI_WII_PLAYER_LED, SDL_HINT_JOYSTICK_HIDAPI_XBOX,
    SDL_HINT_JOYSTICK_HIDAPI_XBOX_360, SDL_HINT_JOYSTICK_HIDAPI_XBOX_360_PLAYER_LED,
    SDL_HINT_JOYSTICK_HIDAPI_XBOX_360_WIRELESS,
    SDL_HINT_JOYSTICK_HIDAPI_XBOX_ONE,
    SDL_HINT_JOYSTICK_HIDAPI_XBOX_ONE_HOME_LED, SDL_HINT_JOYSTICK_HIDAPI_GIP,
    SDL_HINT_JOYSTICK_HIDAPI_GIP_RESET_FOR_METADATA, SDL_HINT_JOYSTICK_IOKIT,
    SDL_HINT_JOYSTICK_LINUX_CLASSIC, SDL_HINT_JOYSTICK_LINUX_DEADZONES,
    SDL_HINT_JOYSTICK_LINUX_DIGITAL_HATS, SDL_HINT_JOYSTICK_LINUX_HAT_DEADZONES,
    SDL_HINT_JOYSTICK_MFI, SDL_HINT_JOYSTICK_RAWINPUT,
    SDL_HINT_JOYSTICK_RAWINPUT_CORRELATE_XINPUT, SDL_HINT_JOYSTICK_ROG_CHAKRAM,
    SDL_HINT_JOYSTICK_THREAD, SDL_HINT_JOYSTICK_THROTTLE_DEVICES,
    SDL_HINT_JOYSTICK_THROTTLE_DEVICES_EXCLUDED, SDL_HINT_JOYSTICK_WGI,
    SDL_HINT_JOYSTICK_WHEEL_DEVICES, SDL_HINT_JOYSTICK_WHEEL_DEVICES_EXCLUDED,
    SDL_HINT_JOYSTICK_ZERO_CENTERED_DEVICES, SDL_HINT_JOYSTICK_HAPTIC_AXES,
    SDL_HINT_KEYCODE_OPTIONS, SDL_HINT_KMSDRM_DEVICE_INDEX,
    SDL_HINT_KMSDRM_REQUIRE_DRM_MASTER, SDL_HINT_KMSDRM_ATOMIC,
    SDL_HINT_LOGGING, SDL_HINT_MAC_BACKGROUND_APP,
    SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK,
    SDL_HINT_MAC_OPENGL_ASYNC_DISPATCH, SDL_HINT_MAC_OPTION_AS_ALT,
    SDL_HINT_MAC_SCROLL_MOMENTUM, SDL_HINT_MAC_PRESS_AND_HOLD,
    SDL_HINT_MAIN_CALLBACK_RATE, SDL_HINT_MOUSE_AUTO_CAPTURE,
    SDL_HINT_MOUSE_DOUBLE_CLICK_RADIUS, SDL_HINT_MOUSE_DOUBLE_CLICK_TIME,
    SDL_HINT_MOUSE_DEFAULT_SYSTEM_CURSOR, SDL_HINT_MOUSE_DPI_SCALE_CURSORS,
    SDL_HINT_MOUSE_EMULATE_WARP_WITH_RELATIVE,
    SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, SDL_HINT_MOUSE_NORMAL_SPEED_SCALE,
    SDL_HINT_MOUSE_RELATIVE_MODE_CENTER, SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE,
    SDL_HINT_MOUSE_RELATIVE_SYSTEM_SCALE, SDL_HINT_MOUSE_RELATIVE_WARP_MOTION,
    SDL_HINT_MOUSE_RELATIVE_CURSOR_VISIBLE, SDL_HINT_MOUSE_TOUCH_EVENTS,
    SDL_HINT_MUTE_CONSOLE_KEYBOARD, SDL_HINT_NO_SIGNAL_HANDLERS,
    SDL_HINT_OPENGL_LIBRARY, SDL_HINT_EGL_LIBRARY, SDL_HINT_OPENGL_ES_DRIVER,
    SDL_HINT_OPENGL_FORCE_SRGB_FRAMEBUFFER, SDL_HINT_OPENVR_LIBRARY,
    SDL_HINT_ORIENTATIONS, SDL_HINT_POLL_SENTINEL, SDL_HINT_PREFERRED_LOCALES,
    SDL_HINT_QUIT_ON_LAST_WINDOW_CLOSE, SDL_HINT_RENDER_DIRECT3D_THREADSAFE,
    SDL_HINT_RENDER_DIRECT3D11_DEBUG, SDL_HINT_RENDER_DIRECT3D11_WARP,
    SDL_HINT_RENDER_VULKAN_DEBUG, SDL_HINT_RENDER_GPU_DEBUG,
    SDL_HINT_RENDER_GPU_LOW_POWER, SDL_HINT_RENDER_DRIVER,
    SDL_HINT_RENDER_LINE_METHOD, SDL_HINT_RENDER_METAL_PREFER_LOW_POWER_DEVICE,
    SDL_HINT_RENDER_VSYNC, SDL_HINT_RETURN_KEY_HIDES_IME,
    SDL_HINT_ROG_GAMEPAD_MICE, SDL_HINT_ROG_GAMEPAD_MICE_EXCLUDED,
    SDL_HINT_PS2_GS_WIDTH, SDL_HINT_PS2_GS_HEIGHT, SDL_HINT_PS2_GS_PROGRESSIVE,
    SDL_HINT_PS2_GS_MODE, SDL_HINT_RPI_VIDEO_LAYER,
    SDL_HINT_SCREENSAVER_INHIBIT_ACTIVITY_NAME, SDL_HINT_SHUTDOWN_DBUS_ON_QUIT,
    SDL_HINT_STORAGE_TITLE_DRIVER, SDL_HINT_STORAGE_USER_DRIVER,
    SDL_HINT_THREAD_FORCE_REALTIME_TIME_CRITICAL,
    SDL_HINT_THREAD_PRIORITY_POLICY, SDL_HINT_TIMER_RESOLUTION,
    SDL_HINT_TOUCH_MOUSE_EVENTS, SDL_HINT_TRACKPAD_IS_TOUCH_ONLY,
    SDL_HINT_TV_REMOTE_AS_JOYSTICK, SDL_HINT_VIDEO_ALLOW_SCREENSAVER,
    SDL_HINT_VIDEO_DISPLAY_PRIORITY, SDL_HINT_VIDEO_DOUBLE_BUFFER,
    SDL_HINT_VIDEO_DRIVER, SDL_HINT_VIDEO_DUMMY_SAVE_FRAMES,
    SDL_HINT_VIDEO_EGL_ALLOW_GETDISPLAY_FALLBACK, SDL_HINT_VIDEO_FORCE_EGL,
    SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES,
    SDL_HINT_VIDEO_MAC_FULLSCREEN_MENU_VISIBILITY,
    SDL_HINT_VIDEO_METAL_AUTO_RESIZE_DRAWABLE,
    SDL_HINT_VIDEO_MATCH_EXCLUSIVE_MODE_ON_MOVE,
    SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS, SDL_HINT_VIDEO_OFFSCREEN_SAVE_FRAMES,
    SDL_HINT_VIDEO_SYNC_WINDOW_OPERATIONS,
    SDL_HINT_VIDEO_WAYLAND_ALLOW_LIBDECOR,
    SDL_HINT_VIDEO_WAYLAND_MODE_EMULATION, SDL_HINT_VIDEO_WAYLAND_MODE_SCALING,
    SDL_HINT_VIDEO_WAYLAND_PREFER_LIBDECOR,
    SDL_HINT_VIDEO_WAYLAND_SCALE_TO_DISPLAY, SDL_HINT_VIDEO_WIN_D3DCOMPILER,
    SDL_HINT_VIDEO_X11_EXTERNAL_WINDOW_INPUT, SDL_HINT_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR,
    SDL_HINT_VIDEO_X11_NET_WM_PING, SDL_HINT_VIDEO_X11_NODIRECTCOLOR,
    SDL_HINT_VIDEO_X11_SCALING_FACTOR, SDL_HINT_VIDEO_X11_VISUALID,
    SDL_HINT_VIDEO_X11_WINDOW_VISUALID, SDL_HINT_VIDEO_X11_XRANDR,
    SDL_HINT_VITA_ENABLE_BACK_TOUCH, SDL_HINT_VITA_ENABLE_FRONT_TOUCH,
    SDL_HINT_VITA_MODULE_PATH, SDL_HINT_VITA_PVR_INIT, SDL_HINT_VITA_RESOLUTION,
    SDL_HINT_VITA_PVR_OPENGL, SDL_HINT_VITA_TOUCH_MOUSE_DEVICE,
    SDL_HINT_VULKAN_DISPLAY, SDL_HINT_VULKAN_LIBRARY, SDL_HINT_WAVE_FACT_CHUNK,
    SDL_HINT_WAVE_CHUNK_LIMIT, SDL_HINT_WAVE_RIFF_CHUNK_SIZE,
    SDL_HINT_WAVE_TRUNCATION, SDL_HINT_WINDOW_ACTIVATE_WHEN_RAISED,
    SDL_HINT_WINDOW_ACTIVATE_WHEN_SHOWN, SDL_HINT_WINDOW_ALLOW_TOPMOST,
    SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN,
    SDL_HINT_WINDOWS_CLOSE_ON_ALT_F4, SDL_HINT_WINDOWS_ENABLE_MENU_MNEMONICS,
    SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP, SDL_HINT_WINDOWS_GAMEINPUT,
    SDL_HINT_WINDOWS_RAW_KEYBOARD,
    SDL_HINT_WINDOWS_RAW_KEYBOARD_EXCLUDE_HOTKEYS,
    SDL_HINT_WINDOWS_RAW_KEYBOARD_INPUTSINK,
    SDL_HINT_WINDOWS_FORCE_SEMAPHORE_KERNEL, SDL_HINT_WINDOWS_INTRESOURCE_ICON,
    SDL_HINT_WINDOWS_INTRESOURCE_ICON_SMALL, SDL_HINT_WINDOWS_USE_D3D9EX,
    SDL_HINT_WINDOWS_ERASE_BACKGROUND_MODE,
    SDL_HINT_X11_FORCE_OVERRIDE_REDIRECT, SDL_HINT_X11_WINDOW_TYPE,
    SDL_HINT_X11_XCB_LIBRARY, SDL_HINT_XINPUT_ENABLED, SDL_HINT_ASSERT,
    SDL_HINT_PEN_MOUSE_EVENTS, SDL_HINT_PEN_TOUCH_EVENTS] do
      WriteHint(aPAnsiChar);

    // SDL_misc.h
    //WriteSection('SDL_misc.h');

    // SDL_stdinc.h-prev (unfinished)

    WriteSection('SDL_stdinc.h-prev');
    
    Write('SDL_GetNumAllocations: ');
    WriteLn(SDL_GetNumAllocations);
    aInt1 := 5; // Mostramos 5 como máximo
    WriteLn('Environment variables at start: (Showing ', aInt1, ' max.)');
    // Recorriendo Array de C Strings
    Environment := SDL_GetEnvironment;
    aPPAnsiChar := SDL_GetEnvironmentVariables(Environment);
    CurrPPAnsiChar := aPPAnsiChar;
    while Assigned(CurrPPAnsiChar^) and (aInt1 > 0) do
    begin
      WriteLn('  ', CurrPPAnsiChar^);
      CurrPPAnsiChar += 1; // Not SizeOf(PAnsiChar)
      Dec(aInt1);
    end;
    SDL_Free(aPPAnsiChar);
    // NOTE: Hay otros dos conjuntos de funciones para obtener la variables
    //   del entorno, con cache o sin ella.

    // SDL_platform.h

    WriteSection('SDL_platform.h');

    WriteLn('SDL_GetPlatform: ', SDL_GetPlatform);

    // SDL_loadso.h
    //WriteSection('SDL_loadso.h');

    // SDL_rect.h
    //WriteSection('SDL_rect.h');

    // SDL_properties.h

    WriteSection('SDL_properties.h');
    
    PropertiesID := SDL_GetGlobalProperties;
    WriteLn('SDL_GetGlobalProperties: ', PropertiesID);
    SDL_EnumerateProperties(PropertiesID, @WriteProperty, nil);
    //< Ya podría ser así siempre...

    // SDL_pixels.h
    //WriteSection('SDL_pixels.h');

    // SDL_blendmode.h
    //WriteSection('SDL_blendmode.h');

    // SDL_iostream.h
    //WriteSection('SDL_iostream.h');

    // SDL_asyncio.h
    //WriteSection('SDL_asyncio.h');

    // SDL_surface.h
    //WriteSection('SDL_surface');

    // SDL_video.h

    WriteSection('SDL_video.h');
    
    aCInt1 := SDL_GetNumVideoDrivers;
    WriteLn('SDL_GetNumVideoDrivers: ', aCInt1);
    for aCInt2 := 0 to (aCInt1 - 1) do
      WriteLn('  ', SDL_GetVideoDriver(aCInt2));
    Write('SDL_GetCurrentVideoDriver: ');
    WriteLn(SDL_GetCurrentVideoDriver);
    Write('SDL_GetSystemTheme: ');
    case SDL_GetSystemTheme of
      SDL_SYSTEM_THEME_LIGHT: WriteLn('Light');
      SDL_SYSTEM_THEME_DARK: WriteLn('Dark');
    otherwise // SDL_SYSTEM_THEME_UNKNOWN:
      WriteLn('Unknown');
    end;

    DisplayIDList := SDL_GetDisplays(@aCInt1);
    WriteLn('SDL_GetDisplays: ', aCInt1);
    Write('SDL_GetPrimaryDisplay: ');
    WriteLn(SDL_GetPrimaryDisplay);
    CurrDisplayID := DisplayIDList;
    while Assigned(CurrDisplayID) and (CurrDisplayID^ <> 0) do
    begin
      PropertiesID := SDL_GetDisplayProperties(CurrDisplayID^);
      WriteLn('GetDisplayProperties(', CurrDisplayID^,'): ');
      SDL_EnumerateProperties(PropertiesID, @WriteProperty, nil);
      Write('SDL_GetDisplayName(', CurrDisplayID^, '): ');
      WriteLn(SDL_GetDisplayName(CurrDisplayID^));
      SDL_GetDisplayBounds(CurrDisplayID^, @aSDLRect);
      WriteLn(Format(
        'SDL_GetDisplayBounds(%d, <TSDLRect>): Pos(%d, %d) Size(%d, %d)',
        [CurrDisplayID^, aSDLRect.X, aSDLRect.Y, aSDLRect.W, aSDLRect.H]));
      SDL_GetDisplayUsableBounds(CurrDisplayID^, @aSDLRect);
      WriteLn(Format(
        'SDL_GetDisplayUsableBounds(%d, <TSDLRect>): Pos(%d, %d) Size(%d, %d)',
        [CurrDisplayID^, aSDLRect.X, aSDLRect.Y, aSDLRect.W, aSDLRect.H]));
      Write('SDL_GetNaturalDisplayOrientation(', CurrDisplayID^, '): ');
      WriteOrientation(SDL_GetNaturalDisplayOrientation(CurrDisplayID^));
      Write('SDL_GetCurrentDisplayOrientation(', CurrDisplayID^, '): ');
      WriteOrientation(SDL_GetCurrentDisplayOrientation(CurrDisplayID^));
      Write('SDL_GetDisplayContentScale(', CurrDisplayID^, '): ');
      WriteLn(SDL_GetDisplayContentScale(CurrDisplayID^));
      DisplayModeList := SDL_GetFullscreenDisplayModes(CurrDisplayID^, @aCInt1);
      WriteLn('Number of display modes of ', CurrDisplayID^, ': ', aCInt1);
      CurrDisplayMode := DisplayModeList;
      aInt1 := 0;
      while Assigned(CurrDisplayMode) and Assigned(CurrDisplayMode^) do
      begin
        WriteLn('  Display mode ', aInt1); // Actually they are not numered...
        Write('    Format: ');
        WriteLn(SDL_GetPixelFormatName(CurrDisplayMode^^.Format));
        Write('    Size: ');
        WriteLn(CurrDisplayMode^^.W, 'x', CurrDisplayMode^^.H);
        WriteLn('    Scale: ', CurrDisplayMode^^.Pixel_Density);
        WriteLn(Format('    Refresh: %f (%d / %d)',
          [CurrDisplayMode^^.refresh_rate,
          CurrDisplayMode^^.refresh_rate_numerator,
          CurrDisplayMode^^.refresh_rate_denominator]));
        CurrDisplayMode += 1;
        Inc(aInt1);
      end;
      SDL_Free(DisplayModeList);
      CurrDisplayID += 1;
    end;
    SDL_Free(DisplayIDList);
    Write('SDL_ScreenSaverEnabled: ');
    WriteLn(BoolToStr(SDL_ScreenSaverEnabled));

    // SDL_timer.h
    
    WriteSection('SDL_timer.h');

    WriteLn('SDL_GetTicks: ', SDL_GetTicks);
    WriteLn('SDL_GetTicksNS: ', SDL_GetTicksNS);
    WriteLn('SDL_GetPerformanceCounter: ', SDL_GetPerformanceCounter);
    WriteLn('SDL_GetPerformanceFrequency: ', SDL_GetPerformanceFrequency);

    // SDL_error.h
    //WriteSection('SDL_error.h');

    // SDL_power.h

    WriteSection('SDL_power.h');

    PowerState := SDL_GetPowerInfo(@aCInt1, @aCInt2);
    Write('SDL_GetPowerInfo: ');
    case PowerState of
      SDL_POWERSTATE_UNKNOWN: Write('Cannot determine power status');
      SDL_POWERSTATE_ON_BATTERY: 
        Write('Not plugged in, running on the battery');
      SDL_POWERSTATE_NO_BATTERY: Write('Plugged in, no battery available');
      SDL_POWERSTATE_CHARGING: Write('Plugged in, charging battery');
      SDL_POWERSTATE_CHARGED: Write('Plugged in, battery charged');
    otherwise // SDL_POWERSTATE_ERROR: 
      Write('Error determining power status');
    end;
    WriteLn(Format(' %d%% (%d seconds left)', 
      [aCInt2, aCInt1]));

    // SDL_audio.h

    WriteSection('SDL_audio.h');

    aCInt1 := SDL_GetNumAudioDrivers;
    WriteLn('SDL_GetNumAudioDrivers: ', aCInt1);
    for aCInt2 := 0 to (aCInt1 - 1) do
      WriteLn('  SDL_GetAudioDriver(', aCInt2,'): ',
        SDL_GetAudioDriver(aCInt2));
    WriteLn('SDL_GetCurrentAudioDriver: ', SDL_GetCurrentAudioDriver);
    AudioDeviceIDList := SDL_GetAudioPlaybackDevices(@aCInt1);
    WriteLn('SDL_GetAudioPlaybackDevices: ', aCInt1);
    CurrAudioDeviceID := AudioDeviceIDList;
    while Assigned(CurrAudioDeviceID) and (CurrAudioDeviceID^ <> 0) do
    begin
      WriteLn('  SDL_GetAudioDeviceName(', CurrAudioDeviceID^,'): ',
        SDL_GetAudioDeviceName(CurrAudioDeviceID^));
      SDL_GetAudioDeviceFormat(CurrAudioDeviceID^, @AudioSpec, @aCInt1);
      WriteLn('  SDL_GetAudioDeviceFormat(', CurrAudioDeviceID^,', ...): ');
      WriteLn(Format(
        '    Format: Bits: %d - Float: %s - Big Endian: %s - Signed: %s',
        [SDL_AUDIO_BITSIZE(AudioSpec.Format),
        BoolToStr(SDL_AUDIO_ISFLOAT(AudioSpec.Format) <> 0),
        BoolToStr(SDL_AUDIO_ISBIGENDIAN(AudioSpec.Format) <> 0),
        BoolToStr(SDL_AUDIO_ISSIGNED(AudioSpec.Format) <> 0)]));
      WriteLn('    Channels: ', AudioSpec.Channels);
      WriteLn('    Frequency: ', AudioSpec.Freq);
      WriteLn('    FrameSize: ', SDL_AUDIO_FRAMESIZE(AudioSpec));
      WriteLn('    BufferSize (samples): ', aCInt1);
      // SDL_GetAudioDeviceChannelMap(CurrAudioDeviceID^; count: pcint): pcint

      WriteLn('  SDL_IsAudioDevicePhysical(', CurrAudioDeviceID^,'): ',
       BoolToStr(SDL_IsAudioDevicePhysical(CurrAudioDeviceID^)));
      CurrAudioDeviceID += 1;
    end;
    SDL_Free(AudioDeviceIDList);
    
    AudioDeviceIDList := SDL_GetAudioRecordingDevices(@aCInt1);
    WriteLn('SDL_GetAudioRecordingDevices: ', aCInt1);
    CurrAudioDeviceID := AudioDeviceIDList;
    while Assigned(CurrAudioDeviceID) and (CurrAudioDeviceID^ <> 0) do
    begin
      WriteLn('  SDL_GetAudioDeviceName(', CurrAudioDeviceID^,'): ',
        SDL_GetAudioDeviceName(CurrAudioDeviceID^));
      SDL_GetAudioDeviceFormat(CurrAudioDeviceID^, @AudioSpec, @aCInt1);
      WriteLn('  SDL_GetAudioDeviceFormat(', CurrAudioDeviceID^,', ...): ');
      WriteLn(Format(
        '    Format: Bits: %d - Float: %s - Big Endian: %s - Signed: %s',
        [SDL_AUDIO_BITSIZE(AudioSpec.Format),
        BoolToStr(SDL_AUDIO_ISFLOAT(AudioSpec.Format) <> 0),
        BoolToStr(SDL_AUDIO_ISBIGENDIAN(AudioSpec.Format) <> 0),
        BoolToStr(SDL_AUDIO_ISSIGNED(AudioSpec.Format) <> 0)]));
      WriteLn('    Channels: ', AudioSpec.Channels);
      WriteLn('    Frequency: ', AudioSpec.Freq);
      WriteLn('    FrameSize: ', SDL_AUDIO_FRAMESIZE(AudioSpec));
      WriteLn('    BufferSize (samples): ', aCInt1);
      // SDL_GetAudioDeviceChannelMap(CurrAudioDeviceID^; count: pcint): pcint

      WriteLn('  SDL_IsAudioDevicePhysical(', CurrAudioDeviceID^,'): ',
       BoolToStr(SDL_IsAudioDevicePhysical(CurrAudioDeviceID^)));
      CurrAudioDeviceID += 1;
    end;
    SDL_Free(AudioDeviceIDList);


(*
    // SDL_sensor.h
    // SDL_scancode.h
    // SDL_keycode.h
    // SDL_mouse.h
    // SDL_keyboard.h
    // SDL_joystick.h
    // SDL_gamepad.h
    // SDL_haptic.h
    // SDL_touch.h
    // SDL_pen.h
    // SDL_camera.h
    // SDL_events.h
    // SDL_init.h
    // SDL_gpu.h
    // SDL_render.h
    // SDL_clipboard.h
*)

  // SDL_cpuinfo.h

  WriteSection('SDL_cpuinfo.h');

  WriteLn('SDL_GetNumLogicalCPUCores: ', SDL_GetNumLogicalCPUCores);
  WriteLn('SDL_GetCPUCacheLineSize: ', SDL_GetCPUCacheLineSize);
  WriteLn('SDL_HasAltiVec : ', SDL_HasAltiVec);
  WriteLn('SDL_HasMMX: ', SDL_HasMMX);
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
  WriteLn('SDL_HasLSX: ',  SDL_HasLSX);
  WriteLn('SDL_HasLASX: ',  SDL_HasLASX);
  WriteLn('SDL_GetSystemRAM: ', SDL_GetSystemRAM);
  WriteLn('SDL_GetSIMDAlignment: ', SDL_GetSIMDAlignment);
  WriteLn('SDL_GetSystemPageSize: ',  SDL_GetSystemPageSize);

  // In SDL2 but not in SDL3
  //WriteLn('SDL_GetCPUCount: ', SDL_GetCPUCount);
  //WriteLn('SDL_HasRDTSC: ', SDL_HasRDTSC);
  //WriteLn('SDL_Has3DNow: ', SDL_Has3DNow);

(*
    // SDL_dialog.h
    // SDL_messagebox.h
    // SDL_time.h
*)

  // SDL_filesystem.h

  WriteSection('SDL_filesystem.h');

  WriteLn('SDL_GetBasePath: ', SDL_GetBasePath);
  WriteLn('SDL_GetPrefPath(''Chixpy'', ''TestSDL3Info''): ', 
    SDL_GetPrefPath('Chixpy', 'TestSDL3Info'));
  for aInt1 := 0 to (SDL_FOLDER_COUNT - 1) do
    WriteLn('SDL_GetUserFolder(', aInt1, '): ', SDL_GetUserFolder(aInt1));
  WriteLn('SDL_GetCurrentDirectory: ',  SDL_GetCurrentDirectory);

(*
    // SDL_filesystem.h
    // SDL_atomic.h
    // SDL_hidapi.h
    // SDL_metal.h
    // SDL_vulkan.h
    // SDL_thread.h
    // SDL_process.h
    // SDL_storage.h
    // SDL_tray.h
    // SDL_mutex.h
*)

(*
  // sdl_audio.h
  aIntA := SDL_GetNumAudioDrivers;
  WriteLn('SDL_GetNumAudioDrivers: ', aIntA);
  for aIntB := 0 to (aIntA - 1) do
    WriteLn('+ SDL_GetAudioDriver(', aIntB, '): ', SDL_GetAudioDriver(aIntB));
  WriteLn('SDL_GetCurrentAudioDriver: ', SDL_GetCurrentAudioDriver);
  WriteLn;

{ 
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
}

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
  { 
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
  }

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
*)
  finally
    WriteSection('END');
    WriteLn;
    WriteLn('LAST ERROR: ', SDL_GetError);
    WriteLn('Mem not freed (must be -1): ', SDL_GetNumAllocations);
    SDL_Quit;
  end;
end.
