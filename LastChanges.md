- Reestructuring **uCHXMatrix**, splited in utCHXMatrixS, utCHXMatrixD,
  utCHXMatrixE and utCHXMatrixR for Single, Double, Extended and Real data
  types. All using same included unit template _utCHXMatrixType.inc_.
- New **TCHXVec3** type to substitute TCHXPoint3DF, and a little test program.
  Same strategy of _uCHXMatrix_.
- New **uCHXColor.pas** with functions to handle HUE (HSL/HSI/HSV) colors in
  common ranges. _CHXFastHue_ function returns a RGB color from a HUE [0..255].
  Added a little test program.
