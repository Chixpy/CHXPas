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
