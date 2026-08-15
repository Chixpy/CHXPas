### CHXSDL3Engine

- Added `SetRenderSize` method to `cCHXSDL3Window`, to set logical size
  to draw. Actually, it's a SDL_Renderer method, but in CHXSDL3Engine is
  managed by Window.

### Miscellaneous

- `TCHXVec3[x]`:
  - Adding alias for components so it can be used as 3D coordinates,
    float solid color and access components by index.
  - Changed alias for macros `TCHXColorF` to `TCHXColorType` and
    `TCHXPoint3DF` to `TCHXPoint3DType` to avoid recursivity.
  - Adding random initialization methods, `InitRandom[x]`, inside a box.
