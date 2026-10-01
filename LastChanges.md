- `uCHXMath.pas`:
  - Fixed `k400Degrees` constant.
  - Added inverses to convert radians to 360º and 400º.
  - Added `FocalLength[x]` functions.
  - Added `Permutate[x]` for integer arrays.
- `cCHXSDL3Renderer`:
  - Added `LastIdleTime` and `LastComputeTime`.
  - Changed FPS info to `LastFullFrameTime (LastIdleTime) / LastComputeTime`.
- `TCHXVec3[x]`:
  - Added `Proj[x]` methods that return the projection of the point.
  - `cCHXVec3List` methods for all points.
  - Trying to change test programs to a proper ones with FPCUnit... but I
    ended making a custom TestReport... and maybe I will create a whole
    CHXTestsRunner.
- Misc formatting changes.
