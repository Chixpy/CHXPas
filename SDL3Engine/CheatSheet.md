# CHXSDL3Engine

Omitted:

- Private and protected methods, fields or properties.
- Contructor `Create`, if it doesn't have parameters.
- Destructor `Destroy`, unless it has another name or it has an alternative.
- `constructor`, `destructor`, `function`, `procedure`, `operator` keywords.
  - As well `inline`, `overload`, `override`, etc.
  - Functions and Operators will show return type.
- `property` keyword and difference with fields. **{R}** = Read-only.
- `const` of parameters, parameters type and default values.
  - `var` (when used as return value) and `out` are showed in cursive.
- FCL / RTL inherited methods or properties. _CHXPas_ ones are added.
- Full declaration and comments. See documentation or source code.

## ucCHXSDL3Engine.pas

### cCHXSDL3Engine

- Properties and Fields:
  - **Config** : cCHXSDL3Config
  - **Window** : cCHXSDL3Window
  - **Render** : cCHXSDL3Renderer
  - **FPSMng** : cCHXSDL3FPSManager
  - **SDLRenderer** : PSDL_Renderer
  - **SDLWindow** : PSDL_Window
  - **Title** : String
  - **ShowFrameRate** : Boolean
- Methods:
  - **Create** (Title, Width, Height, Scale, FullScreen, UseGPU, AutoInit)
    - (Title, IniFilename, AutoInit)
  - **Init**
  - **Run**
  - **Setup**
  - **Compute** (_ExitProg_)
  - **Draw**
  - **HandleEvent** (Event, _Handled_, _ExitProg_)
  - **Finish**

## Classes/ucCHXSDL3Config.pas

- Constants and Resource Strings:
  - **krsIniSectionSDL3Engine**, **krsIniKeyWidth**, **krsIniKeyHeight**,
    **krsIniKeyScale**, **krsIniKeyFullScreen**, **krsIniKeyUseGPU**

### cCHXSDL3Config

- Inherits from:
  - caCHXConfig, caCHXStorableIni, caCHXStorable
  - TPersistent
- Properties and Fields:
  - **DefaultFileName** : String
  - **Width**, **Height** : CInt
  - **Scale** : CInt
  - **FullScreen** : Boolean
  - **UseGPU** : Boolean
- Methods:
  - **ResetDefaultConfig**
  - **LoadFromFile** (Filename)
  - **LoadFromIni** (MemIniFile)
  - **SaveToFile** (Filename, ClearFile)
  - **SaveToIni** (MemIniFile)

## Classes/ucCHXSDL3FPSManager.pas

### cCHXSDL3FPSManager

- Properties and Fields:
  - **FrameCount** : CUInt64 {R}
  - **FPS** : CUInt16
  - **LastFullTime** : CUInt64 {R}
  - **LastBusyTime** : CUInt64 {R}
- Methods:
  - **Create** (FPS)
  - **TimePassed** : CUInt64
  - **Delay** : CInt64

## Classes/ucCHXSDL3Renderer.pas

- Constants and Resource Strings:
  - **rsCHXSDL3RendererNilError**

### cCHXSDL3Renderer

- Properties and Fields:
  - **SDLRenderer** : PSDL_Renderer
  - **FreeRenderer** : Boolean
  - **PrevBlendMode** : TSDL_BlendMode
- Methods:
  - Constructors:
    - **Create** (SDLWindow, Drivers)
      - (SDLRenderer, FreeOnDestroy)
  - Draw Color:
    - **SetDrawColor** (Color) : Boolean
      - (R, G, B, A) : Boolean
      - (Grey, A) : Boolean
    - **GetDrawColor** : TSDL_FColor
      - (_Color_) : Boolean
      - (_R_, _G_, _B_, _A_) : Boolean
    - **PushDrawColor** (Color) : Boolean
      - (R, G, B, A) : Boolean
      - (Grey, A) : Boolean
    - **PopDrawColor** (PopCount);
  - Clear window:
    - **Clear** : Boolean
      - (Color) : Boolean
      - (R, G, B, A) : Boolean
      - (Grey, A) : Boolean
  - Points:
    - [T]**Point** (X, Y) : Boolean
      - (SDLFPoint) : Boolean
    - [T]**Points**[Unsafe] (PointArr, idxFirst, Count) : Boolean
    - **PointMirrorH**[Filled] (X, Y, OffsetX) : Boolean
    - **PointMirrorV**[Filled] (X, Y, OffsetY) : Boolean
    - **PointMirrorHV** (X, Y, OffsetX, OffsetY) : Boolean
    - **PointMirrorHVFilled** (X, Y, FillH, FillV, OffsetX, OffsetY): Boolean
  - Lines:
    - [T]**Line** (P1, P2) : Boolean
      - (X1, Y1, X2, Y2) : Boolean
    - [T]**Lines**[Unsafe] (PointArr, idxFirst, Count) : Boolean
    - **LineMirrorH** (X1, Y1, X2, Y2, OffsetX) : Boolean
    - **LineMirrorV** (X1, Y1, X2, Y2, OffsetY) : Boolean
    - **LineMirrorHV** (X1, Y1, X2, Y2, OffsetX, OffsetY) : Boolean
  - Triangles:
    - **TriangleUnsafe** (PointArr, idxFirst, BorderC, FillC) : Boolean
    - **Triangle** (PointArr, idxFirst, BorderC, FillC) : Boolean
      - (PointArr, BorderC, FillC) : Boolean
      - (P1, P2, P3, BorderC, FillC) : Boolean
      - (X1, Y1, X2, Y2, X3, Y3, BorderC, FillC) : Boolean
    - **Triangle**[Border|Filled|FillOnly]**Unsafe** (PointArr, idxFirst) : Boolean
    - **Triangle**[Border|Filled|FillOnly] (PointArr, idxFirst) : Boolean
      - (P1, P2, P3) : Boolean
      - (X1, Y1, X2, Y2, X3, Y3) : Boolean
  - Rectangles (Axis Aligned):
    - **Rect** (aRect, BorderC, FillC) : Boolean
      - (X, Y, W, H, BorderC, FillC) : Boolean
    - [T]**Rect**[Border|Filled] (aRect) : Boolean
      - (X, Y, W, H) : Boolean
    - **RectFillOnly** (aRect) : Boolean
      - (X, Y, W, H) : Boolean
    - **Rects**[Border|Filled][Unsafe] (RectArr, idxFirst, Count) : Boolean
  - Frames (Axis Aligned):
    - **Frame** (Rect, BWidth, BorderC, FillC) : Boolean
      - (X, Y, W, H, BWidth, BorderC, FillC) : Boolean
    - **Frame**[Border|Filled|FillOnly] (Rect, BWidth) : Boolean
      - (X, Y, W, H, BWidth) : Boolean
  - Quadrilaterals:
    - **QuadUnsafe** (PointArr, idxFirst, BorderC, FillC) : Boolean
    - **Quad** (PointArr, idxFirst, BorderC, FillC) : Boolean
      - (PointArr, BorderC, FillC) : Boolean
      - (P1, P2, P3, P4, BorderC, FillC) : Boolean
      - (X1, Y1, X2, Y2, X3, Y3, X4, Y4, BorderC, FillC) : Boolean
    - **Quad**[Border|Filled|FillOnly]Unsafe (PointArr, idxFirst) : Boolean
    - **Quad**[Border|Filled|FillOnly] (PointArr, idxFirst) : Boolean
      - (P1, P2, P3, P4) : Boolean
      - (X1, Y1, X2, Y2, X3, Y3, X4, Y4) : Boolean
  - Polygons:
    - **PolygonUnsafe** (PointArr, idxFirst, Count, BorderC, FillC) : Boolean
    - **Polygon** (PointArr, idxFirst, Count, BorderC, FillC)  : Boolean
      - (PointArr, BorderC, FillC) : Boolean
    - [T]**PolygonBorder**[Unsafe] (PointArr, idxFirst, Count) : Boolean
    - **Polygon**[Filled|FillOnly][Unsafe] (PointArr, idxFirst, Count) : Boolean
  - Regular polygons (**CC** = Circ. Circle / **SS** = Side Length)
    - **RegPolyCCVertices** (_PointArr_, X, Y, R, NSides, Angle) : Boolean
    - **RegPolyCC** (X, Y, R, NSides, BorderC, FillC, Angle) : Boolean
    - [T]**RegPolyCC**[Border|Filled] (X, Y, R, NSides, Angle) : Boolean
    - **RegPolyCCFillOnly** (X, Y, R, NSides, Angle) : Boolean
    - **RegPolySSVertices** (_PointArr_, X, Y, SideSize, NSides, Angle) : Boolean
    - **RegPolySS** (X, Y, SideSize, NSides, BorderC, FillC, Angle) : Boolean
    - [T]**RegPolySS**[Border|Filled] (X, Y, SideSize,NSides, Angle) : Boolean
    - **RegPolySSFillOnly** (X, Y, SideSize, NSides, Angle) : Boolean
  - Circles / Circunferences:
    - **Circle** (X, Y, R, BorderC, FillC) : Boolean
    - **Circle**[Border|Filled|FillOnly] (X, Y, R) : Boolean
    - **TCircleVertices** (_PArr_, Y, R, OctPoints)
    - **TCircle**[Border|Filled] (X, Y, R, OctPoints) : Boolean
  - Ellipses:
    - **Ellipse** (X, Y, RX, RY, BorderC, FillC) : Boolean
    - **Ellipse**[Border|Filled|FillOnly] (X, Y, RX, RY) : Boolean
    - **TEllipseVertices** (_PArr_, X, Y, RX, RY, QuadPoints)
    - **TEllipse**[Border|Filled] (X, Y, RX, RY, QuadPoints) : Boolean
    - **EllipseInRect** (aRect, BorderC, FillC) : Boolean
      - (X, Y, W, H, BorderC, FillC) : Boolean
    - **EllipseInRect**[Border|Filled|FillOnly] (aRect) : Boolean
      - (X, Y, W, H) : Boolean
  - Rounded Rect with Circle:
    - **RndRectC** (aRect, R, BorderC, FillC) : Boolean
      - (X, Y, W, H, R, BorderC, FillC) : Boolean
    - **RndRectC**[Border|Filled|FillOnly] (aRect, R) : Boolean
      - (X, Y, W, H, R) : Boolean
  - Debug Text - Fallback Font
    - **DebugText** (X, Y, aStr: String) : Boolean
    - **DebugTextF** (X, Y, aFmtStr, ArgArr) : Boolean

## Classes/ucCHXSDL3Window.pas

### cCHXSDL3Window

- Properties and Fields:
  - **Renderer** : cCHXSDL3Renderer
  - **PSDLWindow** : PSDL_Window
  - **PSDLRenderer** : PSDL_Renderer
  - **Title** : String
  - **Width**, **Height** : CInt {R}
  - **WindowWidth**, **WindowHeight** : CInt {R}
  - **WindowID** : CUInt32 {R}
  - **FullScreen** : Boolean
  - **Shown** : Boolean {R}
  - **Maximized**, **Minimized** : Boolean {R}
  - **MouseFocus**, **KeyboardFocus** : Boolean {R}
- Methods:
  - **Focus**
  - **HandleEvent**(aEvent,  _Handled_)
  - Render size stack
    - **SetRenderSize** (aWidth, aHeight, Mode)
    - **PushRenderSize** (aWidth, aHeight, Mode)
    - **PopRenderSize** (PopCount)

## Units/uCHXSDL3TypeHelpers.pas

### Constants

- **kSigmaCFloat**, **kInv255**

### TCHXSDLFSegment

- Fields:
  - **P1**, **P2** : TSDL_FPoint
- Methods:
  - **Init** (P1, aP2)
    - (X1, Y1, X2, Y2)

### TCHXSDLSegment

- Fields:
  - **P1**, **P2** : TSDL_Point
- Methods:
  - **Init** (P1, aP2)
    - (X1, Y1, X2, Y2)

### Dynamic Array types

- **TSDLFColorDynArray** = Array of TSDL_FColor
- **TSDLFPointDynArray** = Array of TSDL_FPoint
- **TSDLPointDynArray** = Array of TSDL_Point
- **TSDLFRectDynArray** = Array of TSDL_FRect
- **TSDLRectDynArray** = Array of TSDL_Rect
- **TSDLVertexDynArray** = Array of TSDL_Vertex
- **TSDLFSegmentDynArray** = Array of TCHXSDLFSegment
- **TSDLSegmentDynArray** = Array of TCHXSDLSegment

### TSDL_FColor helper (TSDLFColorH)

- **Init** (R, G, B, A)
  - (Grey, aA)
- **InitByte** (R255, G255, B255, A255)
  - (Grey255, A255)
- **InitFastHUE** (Hue, Alpha)
- **InitFastHUEByte** (Hue255, Alpha255)
- **IsEqual** (Color)
- **ToString** (Delim)
- **ToStringFmt** (aFmtStr)
- **FromString** (String, Delim)

### TSDL_Color helper (TSDLColorH)

- **Init** (R255, G255, B255, A255)
  - (Grey255, A255)
- **InitFastHUE** (Hue255, Alpha255)
- **IsEqual** (Color)
- **ToString** (Delim)
- **ToStringFmt** (aFmtStr)
- **FromString** (String, Delim)

### TSDL_FPoint helper (TSDLFPointH)

- Initialization
  - **Init** (aX, aY)
  - **InitPolar** (aMag, aAngle)
  - **InitRandom** (MinX, MaxX, MinY, MaxY)
  - **InitRandomPolar** (aMag)
- Polar data
  - **GetAngle** : CFloat
  - **SetAngle** (aAngle)
  - **GetSqrMag** : CFloat
  - **GetMagnitude** : CFloat
  - **SetMagnitude** (aMag)
- Comparisons
  - **IsZero** (aEpsilon) : Boolean
  - **IsEqual** (P, aEpsilon) : Boolean
  - **IsOpposite** (P, aEpsilon) : Boolean
  - **IsParallel** (P, aEpsilon) : Boolean
  - **IsCoaligned** (P, aEpsilon) : Boolean
  - **IsAntiParallel** (P, aEpsilon) : Boolean
  - **IsPerpendicular** (P, aEpsilon) : Boolean
- Self operations
  - **Negate**
  - **Add** (P)
  - **Subtract** (P)
  - **Multiply** (aScale)
  - **CompScale** (P)
  - **Divide** (aScale)
  - **DivInv** (aScale)
- Common operations (Point or Vector)
  - **Normalize**
  - **GetNormalized** : TSDL_FPoint
  - **Move** (dX, dY)
  - **Scale** (sX, sY)
  - **Rotate** (aAngle)
  - **ScalProd** (P) : CFloat
  - **VectProd** (P) : CFloat
  - **SqrDistance** (P) : CFloat
  - **Distance** (P) : CFloat
  - **InDistance** (P, Distance, IncEqual) : Boolean;
  - **MidPoint** (P) : TSDL_FPoint
  - **Reflect** (aNormal) : TSDL_FPoint
  - **Refract** (aNormal, RefIdx) : TSDL_FPoint;
- Conversion to integer coords and remainder.
  - **Ceil** : TSDL_Point
  - **Truncate** : TSDL_Point
  - **Floor** : TSDL_Point
  - **Round** : TSDL_Point
  - **FracCeil** : TSDL_FPoint
  - **FracTrunc** : TSDL_FPoint
  - **FracFloor** : TSDL_FPointFPoint
- String
  - **ToString** (Delim)
  - **ToStringFmt** (aFmtStr)
  - **FromString** (String, Delim)

### TSDL_Point helper (TSDLPointH)

- Inicialization:
  - **Init** (aX, aY)
  - **InitRandom** (MinX, MaxX, MinY, MaxY)
- Polar data:
  - **GetAngle** : CFloat
  - **GetSqrMag** : CFloat
  - **GetMagnitude** : CFloat
- Comparisons:
  - **IsZero** : Boolean
  - **IsEqual** (P) : Boolean
  - **IsOpposite** (P) : Boolean
  - **IsParallel** (P) : Boolean
  - **IsCoaligned** (P) : Boolean
  - **IsAntiParallel** (P) : Boolean
  - **IsPerpendicular** (P) : Boolean
- Self operations:
  - **Negate**
  - **Add** (P)
  - **Subtract** (P)
  - **Multiply** (aScale)
  - **CompScale** (P)
  - **Divide** (aScale)
  - **DivInv** (aScale)
  - **Modulo** (aScale)
  - **ModInv** (aScale)
- Common operations (Point or Vector):
  - **Move** (dX, dY)
  - **Scale** (sX, sY)
  - **ScaleDiv** (sX, sY)
  - **Rotate90** (ClockWise)
  - **ScalProd** (P) : CFloat
  - **VectProd** (P) : CFloat
  - **SqrDistance** (P) : CFloat
  - **Distance** (P) : CFloat
  - **InDistance** (P, aDistance, IncEqual) : Boolean
  - **MidPoint** (P) : TSDL_Point
- Strings:
  - **ToString** (Delim) : String
  - **ToStringFmt** (aFmtStr) : String
  - **FromString** (aString, Delim)

### TSDL_FRect helper (TSDLFRectH)

  - **Init** (aX, aY, aW, aH)
  - **Normalize**
  - **Normalized** : TSDL_FRect
  - **Shrink** (aSize)
  - **ToString** (Delim) : String
  - **ToStringFmt** (aFmtStr) : String
  - **FromString** (aString, Delim)

### TSDL_Rect helper (TSDLRectH)

- **Init** (aX, aY, aW, aH: CFloat)
  - (PosX, PosY, R, G, B, A, PosU, PosV)
  - (Pos, aColor, TexCoord)
  - (Pos, aColor);

