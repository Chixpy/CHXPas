### CHXSDL3Engine

- Changed default `lib` directory and fixing `fpcfg.cfg` files. Adding a `lib`
  directory.
- Added `RndRect[x]` and `Frame[x]` primitives.
- Added `Rect[x]` with `(X, Y, W, H)` parameters.
- Added `TSDL_FRect.Normalize` and `TSDL_FRect.Normalized` methods 
- Added tests `TestPointVsRect.pas` and `TestLineVsRect.pas`.
- Created `CheatSheet.md`
- ¿Optimized? drawing lines and rects.
- Added some experimental `T[x]` methods. This methods will draw primitives
  using triangles and `RenderGeometryRaw`. They have full subpixel precision,
  but, for example with _Circle_ primitive, time is consumed with matematical
  functions and, actuallt it's a regular polygon of many sides.
- Many other tweaks, fixes and modifications.
