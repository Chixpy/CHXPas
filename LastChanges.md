- Some readme files edition.
- Removed `NOTC` directory, moved its units to `Classes` directory and
  update `c[x]Mover` to use `cCHXVec3[x]` instead `TCHXPoint3DF`.
- _CHXSDL3Engine_:
  - Adding support for Full Screen and choose between Software and GPU
    renderer. Actually `SDL_CreateWindowAndRenderer` created a GPU renderer...
    ¡And in my tests it is **2 times slower** than Software one! Same happened
    with SDL2. Seems that GPU is better for `RenderGeometry[Raw]` and lots
    of filled triangles.
  - Changed _CHXSDL3Renderer_ (and _CHXSDL3Window_) creation parameters and
    changing them in test programs.
