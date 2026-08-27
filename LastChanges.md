### _CHXSDL3Engine_

- Added `TestPoints.pas` to compare how to draw many points.
- `CHXSDL3Renderer`:
  - Moved primitive implementation to tematic include files.
  - Added `EllipseInRect[x]` primitive wich, with integer coordinates,
    let draw ellipses and circles with odd diameter. With _floats_ it's
    a lot of easier as we can simply call `Ellipse`. _Integer algorithms_ are
    commented out.
  - All integer algorithms commented out, again, they can be confusing.
  - Fixed some overdrawn pixels in _Ellipse_ and _Circle_ because of 
    `SDL_RenderLine` changing it to `SDL_RenderRect`.

### Other
- Removing compiling notes about not inlined methods with  `{$warn 6058 OFF}`
  in some units.
