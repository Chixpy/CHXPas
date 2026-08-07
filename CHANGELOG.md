## 2026-08-07 19:55


## CHXSDL3Engine

- `cCHXSDL3Engine` saves automatically config file if readed one and autoinit
  is True on creation. It auto-saves config too if `Config.DefaultFileName`
  is set in any other case (of course `Config.SaveToFile` can be used manually).
- Added keys [F10] and [F12] to change frame rate.
- [F10] toggles show frame info inside the window instead change window title.
- `cCHXSDL3Renderer` added `DebugText` and `DebugTextF`.
## 2026-08-06 00:30

- Fixing `TSDL_FPointH` huge errors.
- Adding _fp.cfg_ and _Readme.md_ to SDL3Engine test programs to help on
  compile. _SDL3-for-Pascal_ must be downloaded and its _units_ folder 
  provided as parameter when compiling.
  
## 2026-08-05 21:00

- Fixing _CHXSDL3Engine.pas_ test.
- Adding `TSDL_FPointH` methods (and before ever creating _TCHXVec2[x]_...).
- Changing order, renaming and adding some methods of `TCHXVec3[x]`.
- Some comment format in _uCHXMath.pas_.

## 2026-08-02 20:31

### CHXSDL3Engine

- Added some methods and initial primitives to **cCHXSDL3Renderer**:
  `Set/GetDrawColor`, `Clear`, `Point(s)`, `Line(s)`, `Triangle`,
  `Rect(s)` (Axis Aligned Rectangles), `Quad` (Quadrilaterals), `Polygon`,
  `RegPolyCC` (Regular Polygon circumscribed in a Circle and custom rotation
   angle), `RegPolySS` (Regular Polygon with a Side Length and  rotation angle)
   and `Circle` (only Border).
   - **Note**: Some methods, specially polygons with borders, maybe don't work
     as fully expected when using Logical Presentation because SDL3 smooth
     border lines.
- **uCHXSDL3TypeHelpers.pas**:
  - Removed integer stuff intended for SDL2.
  - Adding helper methods and operators for SDL3 types as needed.
  - Adding `TCHXSDLFSegment` type. Sometimes is better to store a segment with
    it's endpoints instead using a `TSDL_FRect` and calculate `X + W` and
    `Y + H` multiple times.

## 2026-07-26 20:50

### CHXSDL3Engine

- **cCHXSDL3Window**:
  - Change default Blend Mode to Blend, to handle transparency.
- Added **cCHXSDL3FPSManager** to manage FPS while running, replacing 
  SDL_gfx one _not_ used in SDL2:
  - Added a test program.
  - Added to cCHXSDL3Engine.
- Initial basic **cCHXSDL3Renderer** to wrap `SDL_Renderer` and, in the future,
  implement primitive drawing.
  - Added a test program.
  - Added as component of `cCHXSDL3Window`, so added to `cCHXSDL3Engine` too
- More info extracted with **TestSDL3Info**.
- Removing folder structure for simple test programs

## 2026-07-25 20:09

### [SDL3Engine](SDL3Engine/SDL3Engine.pas)

- New engine in **SDL3** to deprecate **SDL2** one.
  - Too many changes were planned in SDL2Engine (mainly refactoring and remove
    *SDL_gfx* dependency with a Renderer Wrapper, Frame Manager, Primitives, 
    etc.), then I decided to update whole engine to use SDL3.
- **cCHXSDL3Window** class:
  - Removed patches from  SDL2 as I can't test if they are needed with SDL3.
  - Automatic driver and renderer is selected.
- **cCHXSDL3Engine** class: Now has a basic structure to run a simple test
  with many features from SDL2 commented out.
- Example test programs added.

###  General

- Cleaning a little:
  - Removed `SDL2/Tests/bin` directory. SDL2 Windows dlls are already in
    `SDL2/bin/<architecture>` for copying in project's bin directories.
  - Removed `SDL2/Tests/SDLTest1.*` as they are a previous version of
    `SDL2/Tests/SDLInfo.*` program.
  - Removing some _UTF8_ functions as FPC 3.0+ teorically handle it internally.
    For example, UTF8ToSys and SysToUTF8 don't do anything.
- `Abstract` folder renamed `Abstracts`.

## 2026-07-21 18:58

- Reestructuring **uCHXMatrix**, splited in utCHXMatrixS, utCHXMatrixD,
  utCHXMatrixE and utCHXMatrixR for Single, Double, Extended and Real data
  types. All using same included unit template _utCHXMatrixType.inc_.
- New **TCHXVec3** type to substitute TCHXPoint3DF, and a little test program.
  Same strategy of _uCHXMatrix_.
- New **uCHXColor.pas** with functions to handle HUE (HSL/HSI/HSV) colors in
  common ranges. _CHXFastHue_ function returns a RGB color from a HUE [0..255].
  Added a little test program.

## 2026-07-18 22:11

- Completing and Refactoring `Types/uCHXMatrix.pas` and adding a little test program.
- Adding ucWorleyNoise.pas, a class wich generate Worley Noise and test program.

## 2026-07-17 00:17

### uCHXMath.pas

- Some physics constants, not sure if separate this kind of constants and
functions.
- Adding CHANGELOG.md and LastChanges.md.
