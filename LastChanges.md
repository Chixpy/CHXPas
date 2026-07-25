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
