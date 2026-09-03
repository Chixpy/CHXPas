# CHXSDL3Engine

Omitted:

- Private and protected methods, fields or properties.
- Contructor `Create`, if it doesn't have parameters.
- Destructor `Destroy`, unless it has another name or it's an alternative.
- `constructor`, `destructor`, `function`, `procedure`, `operator` keywords.
  - As well `inline`, `overload`, `override`, etc. There is not `abstract`.
  - Functions and Operators will show return type.
- `property` keyword and difference with fields. **{R}** = Read-only.
- `const` of parameters, parameters type and default values.
  - `var` and `out` are showed in cursive (when they are used as return).
- FCL / RTL inherited methods or properties. _CHXPas_ ones are added.
- Full declaration and comments. See documentation or source code.

## ucCHXSDL3Engine.pas

### cCHXSDL3Engine

- Properties and Fields:
  - `Config`: cCHXSDL3Config
  - `Window`: cCHXSDL3Window
  - `Render`: cCHXSDL3Renderer
  - `FPSMng`: cCHXSDL3FPSManager
  - `SDLRenderer`: PSDL_Renderer
  - `SDLWindow`: PSDL_Window
  - `Title`: String
  - `ShowFrameRate`: Boolean
- Methods:
  - `Create`(Title, Width, Height, Scale, FullScreen, UseGPU, AutoInit)
    - (Title, IniFile, AutoInit)
  - `Init`
  - `Run`
  - `Setup`
  - `Compute`( _ExitProg_)
  - `Draw`
  - `HandleEvent`(Event, _Handled_, _ExitProg_)
  - `Finish`

_ToDo_:

- Components.
- Text Input.

## Classes/ucCHXSDL3Config.pas

- Constants and Resource Strings:
  - `krsIniSectionSDL3Engine`, `krsIniKeyWidth`, `krsIniKeyHeight`,
    `krsIniKeyScale`, `krsIniKeyFullScreen`, `krsIniKeyUseGPU`

### cCHXSDL3Config

- Inherits:
  - `caCHXConfig`, `caCHXStorableIni`, `caCHXStorable`
  - TPersistent
- Properties and Fields:
  - `DefaultFileName`: String;
  - `Width` : CInt;
  - `Height` : CInt;
  - `Scale`: CInt;
  - `FullScreen` : Boolean;
  - `UseGPU` : Boolean;
- Methods:
  - `ResetDefaultConfig`
  - `LoadFromFile`(Filename)
  - `LoadFromIni`(MemIniFile)
  - `SaveToFile`(Filename, ClearFile)
  - `SaveToIni`(MemIniFile)

## Classes/ucCHXSDL3FPSManager.pas

### cCHXSDL3FPSManager

- Properties and Fields:
  - `FrameCount`: CUInt64 {R}
  - `FPS`: CUInt16
  - `LastFullTime`: CUInt64 {R}
  - `LastBusyTime`: CUInt64 {R}
- Methods:
  - `Create`(FPS)
  - `TimePassed`: CUInt64
  - `Delay`: CInt64

## Classes/ucCHXSDL3Renderer.pas

- Constants and Resource Strings:
  - `rsCHXSDL3RendererNilError`

### cCHXSDL3Renderer

- Properties and Fields:
  - `SDLRenderer`: PSDL_Renderer
  - `FreeRenderer`: Boolean
  - `PrevBlendMode`: TSDL_BlendMode
- Methods:
  - `Create`(SDLWindow, Drivers): cCHXSDL3Renderer
    - `Create`(SDLRenderer, FreeOnDestroy): cCHXSDL3Renderer
  - `SetDrawColor`(Color): Boolean
    - `SetDrawColor`(R, G, B, A): Boolean
    - `SetDrawColor`(Grey, A): Boolean
  - `GetDrawColor`: TSDL_FColor
    - `GetDrawColor`( _Color_): Boolean
    - `GetDrawColor`( _R_, _G_, _B_, _A_): Boolean
  - `Clear`: Boolean
    - `Clear`(Color): Boolean
    - `Clear`(R, G, B, A): Boolean
    - `Clear`(Grey, A): Boolean
  - `Point`(X, Y): Boolean
    - `Point`(Point): Boolean
  - `TPoint`(X, Y): Boolean
    - `TPoint`(Point): Boolean
  - `PointsUnsafe`(PointArr, idxFirst, Count): Boolean
  - `Points`(PointArr, idxFirst, Count): Boolean
  - `PointMirrorH`(X, Y, OffsetX): Boolean
  - `PointMirrorHFilled`(X, Y, OffsetX): Boolean
  - `PointMirrorV`(X, Y, OffsetY): Boolean
  - `PointMirrorVFilled`(X, Y, OffsetY): Boolean
  - `PointMirrorHV`(X, Y, OffsetX, OffsetY): Boolean
  - `PointMirrorHVFilled`(X, Y, FillH, FillV, OffsetX, OffsetY): Boolean
  - `Line`(P1, P2): Boolean
    - `Line`(X1, Y1, X2, Y2): Boolean
  - `TLine`(P1, P2): Boolean
    - `TLine`(X1, Y1, X2, Y2): Boolean
  - `LinesUnsafe`(PointArr, idxFirst, Count): Boolean
  - `TLinesUnsafe`(PointArr, idxFirst, Count): Boolean
  - `Lines`(PointArr, idxFirst, Count): Boolean
  - `TLines`(PointArr, idxFirst, Count): Boolean
  - `LineMirrorH`(X1, Y1, X2, Y2, OffsetX): Boolean
  - `LineMirrorV`(X1, Y1, X2, Y2, OffsetY): Boolean
  - `LineMirrorHV`(X1, Y1, X2, Y2, OffsetX, OffsetY): Boolean
  - `TriangleUnsafe`(PointArr, idxFirst, BorderC, FillC): Boolean
  - `Triangle`(PointArr, idxFirst, BorderC, FillC): Boolean
    - `Triangle`(PointArr, BorderC, FillC): Boolean
    - `Triangle`(P1, P2, P3, BorderC, FillC): Boolean
    - `Triangle`(X1, Y1, X2, Y2, X3, Y3, BorderC, FillC): Boolean
  - `TriangleBorderUnsafe`(PointArr, idxFirst): Boolean
  - `TriangleBorder`(PointArr, idxFirst): Boolean
    - `TriangleBorder`(P1, P2, P3): Boolean
    - `TriangleBorder`(X1, Y1, X2, Y2, X3, Y3): Boolean
  - `TriangleFilledUnsafe`(PointArr, idxFirst): Boolean
  - `TriangleFilled`(PointArr, idxFirst): Boolean
    - `TriangleFilled`(P1, P2, P3): Boolean
    - `TriangleFilled`(X1, Y1, X2, Y2, X3, Y3): Boolean
  - `TriangleFillOnlyUnsafe`(PointArr, idxFirst): Boolean
  - `TriangleFillOnly`(PointArr, idxFirst): Boolean
    - `TriangleFillOnly`(P1, P2, P3): Boolean
    - `TriangleFillOnly`(X1, Y1, X2, Y2, X3, Y3): Boolean
  - `Rect`(aRect, BorderC, FillC): Boolean
    - `Rect`(X, Y, W, H, BorderC, FillC): Boolean
  - `RectBorder`(aRect): Boolean
    - `RectBorder`(X, Y, W, H): Boolean
  - `TRectBorder`(aRect): Boolean
    - `TRectBorder`(X, Y, W, H): Boolean
  - `RectFilled`(aRect): Boolean
    - `RectFilled`(X, Y, W, H): Boolean
  - `TRectFilled`(aRect): Boolean
    - `TRectFilled`(X, Y, W, H): Boolean
  - `RectFillOnly`(aRect): Boolean
    - `RectFillOnly`(X, Y, W, H): Boolean
  - `RectsBorderUnsafe`(RectArr, idxFirst, Count): Boolean
  - `RectsBorder`(RectArr, idxFirst, Count): Boolean
  - `RectsFilledUnsafe`(RectArr, idxFirst, Count): Boolean
  - `RectsFilled`(RectArr, idxFirst, Count): Boolean
  - `Frame`(Rect, BWidth, BorderC, FillC): Boolean
    - `Frame`(X, Y, W, H, BWidth, BorderC, FillC): Boolean
  - `FrameBorder`(Rect, BWidth): Boolean
    - `FrameBorder`(X, Y, W, H, BWidth): Boolean
  - `FrameFilled`(Rect, BWidth): Boolean
    - `FrameFilled`(X, Y, W, H, BWidth): Boolean
  - `FrameFillOnly`(Rect, BWidth): Boolean
    - `FrameFillOnly`(X, Y, W, H, BWidth): Boolean
  - `QuadUnsafe`(PointArr, idxFirst, BorderC, FillC): Boolean
  - `Quad`(PointArr, idxFirst, BorderC, FillC): Boolean
    - `Quad`(PointArr, BorderC, FillC): Boolean
    - `Quad`(P1, P2, P3, P4, BorderC, FillC): Boolean
    - `Quad`(X1, Y1, X2, Y2, X3, Y3, X4, Y4, BorderC, FillC): Boolean
  - `QuadBorderUnsafe`(PointArr, idxFirst): Boolean
  - `QuadBorder`(PointArr, idxFirst): Boolean
    - `QuadBorder`(P1, P2, P3, P4): Boolean
    - `QuadBorder`(X1, Y1, X2, Y2, X3, Y3, X4, Y4): Boolean
  - `QuadFilledUnsafe`(PointArr, idxFirst): Boolean
  - `QuadFilled`(PointArr, idxFirst): Boolean
    - `QuadFilled`(P1, P2, P3, P4): Boolean
    - `QuadFilled`(X1, Y1, X2, Y2, X3, Y3, X4, Y4): Boolean
  - `QuadFillOnlyUnsafe`(PointArr, idxFirst): Boolean
  - `QuadFillOnly`(PointArr, idxFirst): Boolean
    - `QuadFillOnly`(P1, P2, P3, P4): Boolean
    - `QuadFillOnly`(X1, Y1, X2, Y2, X3, Y3, X4, Y4):Boolean
  - `PolygonUnsafe`(PointArr, idxFirst, Count, BorderC, FillC): Boolean
  - `Polygon`(PointArr, idxFirst, Count, BorderC, FillC) : Boolean
    - `Polygon`(PointArr, BorderC, FillC): Boolean
  - `PolygonBorderUnsafe`(PointArr, idxFirst, Count): Boolean
  - `TPolygonBorderUnsafe`(PointArr, idxFirst, Count): Boolean
  - `PolygonBorder`(PointArr, idxFirst, Count): Boolean
  - `TPolygonBorder`(PointArr, idxFirst, Count): Boolean
  - `PolygonFilledUnsafe`(PointArr, idxFirst, Count): Boolean
  - `PolygonFilled`(PointArr, idxFirst, Count): Boolean
  - `PolygonFillOnlyUnsafe`(PointArr, idxFirst, Count): Boolean
  - `PolygonFillOnly`(PointArr, idxFirst, Count): Boolean
  - `RegPolyCCVertices`(out PointArr,X, Y, R, NSides, Angle): Boolean
  - `RegPolyCC`(X, Y, R, NSides, BorderC, FillC, Angle): Boolean
  - `RegPolyCCBorder`(X, Y, R, NSides, Angle): Boolean
  - `TRegPolyCCBorder`(X, Y, R, NSides, Angle): Boolean
  - `RegPolyCCFilled`(X, Y, R, NSides, Angle): Boolean
  - `TRegPolyCCFilled`(X, Y, R, NSides, Angle): Boolean
  - `RegPolyCCFillOnly`(X, Y, R, NSides, Angle): Boolean
  - `RegPolySSVertices`(out PointArr,X, Y, SideSize, NSides, Angle): Boolean
  - `RegPolySS`(X, Y, SideSize, NSides, BorderC, FillC, Angle): Boolean
  - `RegPolySSBorder`(X, Y, SideSize,NSides, Angle): Boolean
  - `RegPolySSFilled`(X, Y, SideSize,NSides, Angle): Boolean
  - `RegPolySSFillOnly`(X, Y, SideSize,NSides, Angle): Boolean
  - `Circle`(X, Y, R, BorderC, FillC): Boolean
  - `CircleBorder`(X, Y, R): Boolean
  - `TCircleBorder`(X, Y, R, NSides): Boolean
  - `CircleFilled`(X, Y, R): Boolean
  - `CircleFilledT`(X, Y, R, NSides): Boolean
  - `CircleFillOnly`(X, Y, R): Boolean
  - `Ellipse`(X, Y, RX, RY, BorderC, FillC): Boolean
  - `EllipseBorder`(X, Y, RX, RY): Boolean
  - `EllipseFilled`(X, Y, RX, RY): Boolean
  - `EllipseFillOnly`(X, Y, RX, RY): Boolean
  - `EllipseInRect`(aRect, BorderC, FillC): Boolean
    - `EllipseInRect`(X, Y, W, H, BorderC, FillC): Boolean
  - `EllipseInRectBorder`(aRect): Boolean
    - `EllipseInRectBorder`(X, Y, W, H): Boolean
  - `EllipseInRectFilled`(aRect): Boolean
    - `EllipseInRectFilled`(X, Y, W, H): Boolean
  - `EllipseInRectFillOnly`(aRect): Boolean
    - `EllipseInRectFillOnly`(X, Y, W, H): Boolean
  - `RndRectC`(aRect, R, BorderC, FillC): Boolean
    - `RndRectC`(X, Y, W, H, R, BorderC, FillC): Boolean
  - `RndRectCBorder`(aRect, R): Boolean
    - `RndRectCBorder`(X, Y, W, H, R): Boolean
  - `RndRectCFilled`(aRect, R): Boolean
    - `RndRectCFilled`(X, Y, W, H, R): Boolean
  - `RndRectCFillOnly`(aRect, R): Boolean
    - `RndRectCFillOnly`(X, Y, W, H, R): Boolean
  - `DebugText`(X, Y, aStr: String): Boolean
  - `DebugTextF`(X, Y, aFmtStr, ArgArr): Boolean

_ToDo_:

- More primitives

## Classes/ucCHXSDL3Window.pas

### cCHXSDL3Window

- Properties and Fields:
  - `Renderer`: cCHXSDL3Renderer
  - `PSDLWindow`: PSDL_Window
  - `PSDLRenderer`: PSDL_Renderer
  - `Title`: String
  - `Width`: CInt {R}
  - `Height`: CInt {R}
  - `WindowWidth`: CInt {R}
  - `WindowHeight`: CInt {R}
  - `WindowID`: CUInt32 {R}
  - `FullScreen`: Boolean
  - `Shown`: Boolean {R}
  - `Maximized`: Boolean {R}
  - `Minimized`: Boolean {R}
  - `MouseFocus`: Boolean {R}
  - `KeyboardFocus`: Boolean {R}
- Methods:
  - `Focus`
  - `HandleEvent`(aEvent,  _Handled_)
  - `SetRenderSize`(aWidth, aHeight, Mode)
