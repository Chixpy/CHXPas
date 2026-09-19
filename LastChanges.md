- **Huge renaming** of `cCHXSDL3Renderer` primitive methods:
  - Three drawing methods:
    - Default (no prefix) native smooth logical subpixel rendering.
    - `SP`: Pixelated draw with logical subpixel offset of coordinates.
    - `FP`: Pixelated with Full Pixel integer coordinates. (To be implemented).
  - Improve primitive tests to comparing drawing methods.
  - Tast for `LineMirror[x]`.
  - `cCHXSDL3Renderer.PointMirror[x]`: Fixed overdraw when point distance to
    axis is less than `0.5`.
  - Other little fixes and optimizations.
