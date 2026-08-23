- _CHXSDL3Renderer_: **But it will be reworked again.**
  - Added previous commented out algorithms with subpixel adaptation
    for Logical Presentation as new methods with the suffix `LP`. So both
    versions can be used as desired.
  - Actually, all primitive methods will have `LP` or `FP` suffix, to make
    clear how the point `(0.3, 0.7)` will be drawn:
    - `[X]FP`: Full Pixel rounded to nearest integer. In the example: `(0, 1)`
    - `[X]LP`: Logical Presentation. SDL will manage it and depends on the 
      scale (Actual Window Size / Logical Render Size) of the Logical
      Presentation. For example:
      - _x2_: It's spected to be drawn at `(0.5, 1)`
      - _x3_: `(0.3, 0.6)`
      - _x4_: `(0.4, 0.8)`
