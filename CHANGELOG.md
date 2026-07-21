## 2026-07-21 18:58
- Reestructuring **uCHXMatrix**, splited in utCHXMatrixS, utCHXMatrixD,
  utCHXMatrixE and utCHXMatrixR for Single, Double, Extended and Real data
  types. All using same included unit template _utCHXMatrixType.inc_.
- New **TCHXVec3** type to substitute TCHXPoint3DF, and a little test program.
  Same strategy of _uCHXMatrix_.
- New **uCHXColor.pas** with functions to handle HUE (HSL/HSI/HSV) colors in
  common ranges. _CHXFastHue_ function returns a RGB color from a HUE [0..255].
  Added a little test program.


## 2026-07-18 22:11
- Completing and Refactoring _Types/uCHXMatrix.pas_ and adding a little test program.
- Adding ucWorleyNoise.pas, a class wich generate Worley Noise and test program.


## 2026-07-17 00:17
### uCHXMath.pas
- Some physics constants, not sure if separate this kind of constants and
functions.
- Adding CHANGELOG.md and LastChanges.md.


