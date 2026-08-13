### `cCHXSDL3Renderer`

- Trying to optimize a little some methods and commenting out Logical
  Presentation adaptation of the algorithms.
- Added `IsValidArrayRange` to check Array subranges.
- Added "unsafe" methods drawing with an array without range checking,
  to be called internally when we are sure that range is valid.
- Changed `TriangleFillOnly` and `QuadFillOnly` methods to use
  `PolygonFillOnly`. Triangle one maybe can be optimized with its own
  algorithm.
- Changed `QuadFill` methods to use `PolygonFill`.
- Fixing "Mirror" methods and adding `PointMirrorHVFilled`.
- Fixing range checks with `X in [0..High/Length(PArr)]` if `PArr` have more
  than 255 elements.
- Added `Circle[X]` and `Ellipse[X]`.
- Some test programs for primitives of `cCHXSDL3Renderer`.
- _uCHXSDL3TypeHelpers_: Added `TCHXSDLSegment` (Pair integer points),
  `TSDLFSegmentDynArray` and `TSDLSegmentDynArray`. Changed of `Ceil`,
  `Truncate`, `Floor` and `Round` to return a `TSDL_Point` (Integer).

### Other

- Adding _fp.cfg_, _Build.bat_ and _build.sh_ to test programs.
