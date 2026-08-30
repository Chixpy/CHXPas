- _CHXVec3_: Renamed `InDistance` to `InDistance3D`; and added `InDistanceXY`,
  `InDistanceXZ` and `InDistanceZY`.
- _CHXMover_: `Update` method renamed to `ApplyForce`.
- _CHXSDL3Renderer_:
  - Added `Clear(Grey, Alpha)`.
  - `Ellipse[x]`: Changed algorithm _Integer_ variables to _CFloat_.
    They don't crash by overflow... but I suspect that can create an
    infinite loop.
- `uCHXColor`: Optimized a little `CHXFastHue`.
- `uCHXSDL3TypeHelpers`: Added `InitFastHue[x]` to `TSDL_FColor`. And added
  a CFloat version, but not backported to `uCHXColor` (This unit was created
  with SDL2 in mind...).
