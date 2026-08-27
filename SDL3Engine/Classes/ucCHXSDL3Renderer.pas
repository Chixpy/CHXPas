unit ucCHXSDL3Renderer;
(*< Unit of cCHXSDL3Renderer class.

  cCHXSDL3Renderer is an encapsulation of `SDL_Renderer` and expands
  its funcionality.

  This way, instead of using `SDL_[...](PSDL_Renderer, [...])` functions,
  they will become direct methods of the class itself.

  In the context of `cCHXSDL3Engine`, this class will be created by
  `cCHXSDL3Window` on its creation and freed by it.

  ## `SDL_Renderer`:

  SDL3 only provides the following basic functions for drawing:

  @unorderedList( @itemSpacing Compact
    @item(`SDL_Set[/Get]RenderDrawColor[Float]`)
    @item(`SDL_Set[/Get]RenderDrawColor[Float]`)
    @item(`SDL_RenderClear`)
    @item(`SDL_Set[/Get]RenderDrawBlendMode`)
    @item(`SDL_RenderPoint[s]`)
    @item(`SDL_RenderLine[s]`)
    @item(`SDL_RenderRect[s]`)
    @item(`SDL_Render[Fill]Rect[s]`)
  )

  Where:

  @definitionList( @itemSpacing Compact
    @itemLabel([Get/Set])
    @item(Ability to retrieve the current color or blend mode.)
    @itemLabel([Float])
    @item(SDL3 can handle float point RGBA colors in range [0..1]. And actually
      is the used internal format.)
    @itemLabel([s])
    @item(Draw multiple points, lines, or rectangles stored in an
      array of `TSDL_FPoint` or `TSDL_FRect`.)
    @itemLabel([Fill])
    @item(For filled (AABB) rectangles.)
  )

  All drawing functions use float parameters `CFloat`, wich is
  equivalent to `Single` in FPC.

  Internally, SDL3 _[s]_ variants are actually used for drawing. Single point,
  line or rectangle versions only create an `array of TSDL_FPoint` or
  `TSDL_FRect` with one element.

  As side note, SDL2 does the same with the addition of converting integer
  values to float.

  **For colors, SDL3 uses floats** too, while SDL2 uses Byte.

  Additionally, there are the `SDL_RenderGeometry[Raw]` functions used
  to draw filled triangles, which are more advanced as they allow gradient and
  textured rendering. They are likely faster for drawing filled polygons with
  color, although I need to test it.

  ## `SDL_gfx`:

  Initially, this unit was for `cCHXSDL2Engine` and had the purpose of remove
  the dependency of `SDL_gfx`. This unit was created before making anything
  with SDL3, so the rant will be in `ucCHXSDL2Renderer` if this algorithms
  are ported there. XD

  Anyways, `SDL_gfx` (and SDL native functions) can be used to draw as
  `PSDL_Renderer` is exposed with `SDLRenderer` property.

  ## cCHXSDL3Renderer:

  The purpose of this class is as follows:

  - Encapsulate `TSDL_Renderer` drawing functions within a dedicated class.
  - Add significant primitive drawing functions.
  - Try to adapt _integer algorithm_ methods to SDL Logical Presentation.
  - Add some Quality of Life features:
    - Overloaded variants of functions for various parameter types:
      - Point: `P: TSDL_FPoint` <=> `X, Y: CFloat`
      - Rectangle: `R: TSDL_FRect` <=> `X, Y, W, H: CFloat`
      - Color: `aColor: TSDL_FColor` <=> `R, G, B, A: CFloat` <=>
        `Grey, A: CFloat`
      - Point Array: `PArr: Array of TSDL_FPoint` (`TSDLFPointDynArray`)
      - Rect Array: `RArr: Array of TSDL_FRect` (`TSDLFRectDynArray`)
    - Evaluate whether it is more efficient for these variants: to call
          a common method or be implemented independently.
    - If a primitive method changes current draw color internally,
          it restores previous one.
  - Provide methods to draw primitives:
    - Border/Edges/Perimeter only.
    - Full filled with a color.
    - Only fill without border.
    - Border and Fill with different colors: Trying to ensure that
      edges and fill do not overlap as alpha transparencies would accumulate.
                                           
  ToDo: Move this to `uCHXSDL3TypeHelpers`:
                                           
  Separately in `uCHXSDL3TypeHelpers`, several useful types and helpers for
  SDL data structures will be defined:

  - Dynamic arrays of `TSDL_FColor`, `TSDL_FPoint`, `TSDL_FRect`, etc.:
      `TSDLFColorDynArray`, `TSDLFPointDynArray`, etc.
  - ToDo: Specialized generic lists for those types, wich can be
    inherited to add custom methods: `cSDLFPointList`, `cSDLFRectList`, etc.
    -`FGL` unit returns a not found operator overload error.
    - Try with other container generics: `Generics.Collections` use an
      actual array.
  - Type helpers for SDL types.
  - Global functions returning SDL types. Useful to be used directly
    as parameters without explicitly declaring a variable.

  More features can be added:

  - Primitive rotations.
  - Variable edge thickness for shapes.
  - Antialiasing for shapes (which goes hand in hand with thick lines).
  - Rotations and rotation pivot points.
  - Push and Pop relative coordinate systems. So we can transform points
    with Translation, Rotation and Scale...

  ## ToDo:

  - Logical Presentation quirks with _Integer Algorithms_:
    - _Points_ are drawn at subpixel position with scale size. Lines
      (and Rects) are 1 scaled point thick. Filled Rects are drawn at
      subpixel coordinates too.
    - Drawing diagonal lines with native functions apply subpixel draw,
      so they are _smooth_ and don't draw "big" points.
    - Drawing lines with length < 2 with `SDL_RenderLine` overdraws partially
      2 points (and adding alpha 2 times in intersection). For vertical and
      horizontal lines this can be fixed with `SDL_DrawFilledRect` and a
      width or height of 1.
  - Drawing lines with `SDL_RenderLines` (used by Rects, Quads and
      Polygons) apply alpha 2 times in the corner points...
      `SDL_RenderRect[s]` don't have this problem.
  - Remember: `T[F]Rect` doesn't include `X + W` row or `Y + H` column.
  - Use `SDL_SetError` and try not to halt execution, except in
      constructors that will have Exceptions.
  - ¿Do integer algorithms overloads? I don't know. Commented out until
    I decide. The simplest way to do is round the values...
  - ¿Use `procedure` instead `function`? Rarely, `Result` of the
    functions will be checked... and will simplify methods not trying
    to keep track of it.
  - Some algorithms will overflow with not so big numbers because Integer
    type.
    - If a Integer value is powered by 2: ~46344+ will crash.
    - CFloat (Single) powered by 2: ~8388607+ will create an infinite loop?

  (C) 2026 Chixpy https://github.com/Chixpy
*)
{$MODE ObjFPC}{$H+}
{$INLINE ON}{$WARN 6058 OFF}

interface

uses
  SysUtils, CTypes, Math, // FPC RTL
  SDL3, // SDL3
  uCHXSDL3TypeHelpers; // CHXSDL3engine

resourcestring
  rsCHXSDL3RendererNilError = 'cCHXSDL3Renderer.Create: %s is nil.';

type
  { Wrapper of SDL_Renderer and expanded to draw more primitives.

    It doesn't call `SDL_Init[SubSystem]` or `SDL_Quit[SubSystem]`
    as it expects at least a `SDL_Window` already created. In `cCHXSDL3Engine`
    context, this class is created by `cCHXSDL3Window`.

    Nearly all methods are functions with boolean Result as SDL_Renderer
    funtions are. They return @False on error and `SDL_GetError` can give
    information, cCHXSDL3Renderer ones included. cCHXSDL3Renderer constructors
    throw an Exception instead.
  }
  cCHXSDL3Renderer = class
  protected

  {
    Auxiliar functions
  }

    function IsValidArrayRange(const ArrLength, idxFirst: Integer;
      var Count: Integer; const LineNumber, FuncName: String): Boolean;
    (*< Check if `[idxFirst..(idxFirst+Count-1)]` is a valid range inside
      `[0..(ArrLength-1)]`.

      @param(ArrLength Length of the array. And it's supossed to begin from 0.)
      @param(idxFirst Index of the first element.)
      @param(Count Number of elements, including `idxFirst` one.)
      @param(LineNumber Use `{$I %LINE%}`.)
      @param(FuncName Use `{$I %CURRENTROUTINE%}`.)

      @returns(@False on invalid range. Sets `SDL_SetError`. And changes
        `Count` with its maximum posible value or `-1` if `idxFirst` is
        out of range.)
    *)

  public
    SDLRenderer: PSDL_Renderer;
    //< Actual SDL_Renderer pointer.
    FreeRenderer: Boolean;
    //< ¿Free SDL_Renderer on Destroy?;
    PrevBlendMode: TSDL_BlendMode;
    //< Previous blend mode when changing color.

  {
    Constructors
  }

    constructor Create(const PSDLWindow: PSDL_Window;
      const Drivers: PAnsiChar = nil); overload;
    {< Create a cCHXSDL3Renderer with a new SDL_Renderer and assign
       the SDL_Window to it.

      @param(PSDLWindow SDL_Window that will asociated be to SDL_Renderer.)
      @param(Drivers Space separated list of drivers to try to use.)
    }
    constructor Create(const PSDLRenderer: PSDL_Renderer;
      const FreeOnDestroy: Boolean); overload;
    {< Create a cCHXSDL3Renderer with an already created SDL_Renderer.

      @param(PSDLRenderer SDL_Renderer to use.)
      @param(FreeRenderer ¿Free SDL_Renderer on Destroy?)
    }

  {
    [Get|Set]DrawColor
  }

    function SetDrawColor(const aColor: TSDL_FColor): Boolean; overload;
      inline;
    function SetDrawColor(const R, G, B: CFloat; const A: CFloat = 1): Boolean;
      overload;
    function SetDrawColor(const Grey: CFloat; const A: CFloat = 1): Boolean;
      overload; inline;
    {<< Set current draw color for primitives and `Clear`.

      @param(aColor Color with components in float [0..1] range.)

      @param(R Red in float [0..1] range.)
      @param(G Green in float [0..1] range.)
      @param(B Blue in float [0..1] range.)
      @param(Alpha Opacity in float [0..1] range.)

      @param(Grey Grey in float [0..1] range.)
    }

    function GetDrawColor: TSDL_FColor; overload; inline;
    {< Get current draw color.

      Ignores SDL errors, to return the color as result.

      @returns(Color with components in float [0..1] range.)
    }
    function GetDrawColor(out aColor: TSDL_FColor): Boolean; overload; inline;
    function GetDrawColor(out R, G, B, A: CFloat): Boolean; overload; inline;
    {<< Get current draw color.

      @param(aColor Returned color with components in float [0..1] range.)

      @param(R Returned Red in float [0..1] range.)
      @param(G Returned Green in float [0..1] range.)
      @param(B Returned Blue in float [0..1] range.)
      @param(Alpha Returned Opacity in float [0..1] range.)
    }

  {
    Clear
  }

    function Clear: Boolean; overload; inline;
    {< Clear render's target (usually a SDL_Window) with current color.
    }
    function Clear(const aColor: TSDL_FColor): Boolean; overload; inline;
    function Clear(const R, G, B: CFloat; const A: CFloat = 1): Boolean;
      overload;
    {< Clear render's target (usually a SDL_Window).

      Restores previous draw color.

      @param(aColor Color to clear the render's target.)

      @param(R Red in float [0..1] range.)
      @param(G Green in float [0..1] range.)
      @param(B Blue in float [0..1] range.)
      @param(Alpha Opacity in float [0..1] range. ToDo: ¿Is Alpha used?)
    }

  {
    Point[s][FP]
  }

    function Point(const X, Y: CFloat): Boolean; overload; inline;
    function Point(const P: TSDL_FPoint): Boolean; overload; inline;
    {< Draw a point with current draw color.

      @note(Renderer Logical Presentation draw it as a rect with 1 logical
        pixel of size.)

      @param(X Horizontal coordinate.)
      @param(Y Vertical coordinate.)

      @param(P Point coordinates.)
    }

    // function PointFP(const X, Y: CFloat): Boolean; overload; inline;
    // function PointFP(const P: TSDL_FPoint): Boolean; overload; inline;
    {< Draw a point at integer coordinates with current draw color.

      @note(Renderer Logical Presentation draw it as a rect with 1 logical
        pixel of size.)

      @param(X Horizontal coordinate.)
      @param(Y Vertical coordinate.)

      @param(P Point coordinates.)
    }

    function PointsUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function Points(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw an array of points with current draw color.

      @note(Renderer Logical Presentation draw them as a rect with 1 logical
        pixel of size.)

      @param(PArr Array of points.)
      @param(idxFirst First point to draw.)
      @param(Count Number of points to draw. `0` means until the end of the
        array.)
    }

    // function PointsUnsafeFP(const PArr: TSDLFPointDynArray;
    //   const idxFirst, Count: Integer): Boolean;
    // function PointsFP(const PArr: TSDLFPointDynArray;
    //   const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw an array of points at integer coordinates with current draw color.

      @note(Renderer Logical Presentation draw them as a rect with 1 logical
        pixel of size.)

      @param(PArr Array of points.)
      @param(idxFirst First point to draw.)
      @param(Count Number of points to draw. `0` means until the end of the
        array.)
    }

  {
    PointMirror[X]
  }

    function PointMirrorH(const X, Y: CFloat; const OffsetX: CFloat = 0)
      : Boolean;
    {< Draw a point and its horizontal reflection relative to `X=0`, and then
      shifted by OffsetX.

      Do not confuse with reflection directly around OffsetX.

      Intended as an internal helper for complex primitive generation.

      @param(X Horizontal position of the point.)
      @param(Y Vertical position of the point.)
      @param(OffsetX Horizontal offset.)
    }

    function PointMirrorHFilled(const X, Y: CFloat; const OffsetX: CFloat = 0)
      : Boolean;
    {< Draw a line between the point and its horizontal reflection relative
      to `X=0`, and then shifted by OffsetX.

      Do not confuse with reflection directly around OffsetX.

      Intended as an internal helper for complex primitive generation.

      @param(X Horizontal position of the point.)
      @param(Y Vertical position of the point.)
      @param(OffsetX Horizontal offset.)
    }

    function PointMirrorV(const X, Y: CFloat; const OffsetY: CFloat = 0)
      : Boolean;
    {< Draw a point and its vertical reflection relative to `Y=0`, and then
      shifted by OffsetY.

      Do not confuse with reflection directly around OffsetY.

      Intended as an internal helper for complex primitive generation.

      @param(X Horizontal position of the point.)
      @param(Y Vertical position of the point.)
      @param(OffsetY Vertical offset.)
    }

    function PointMirrorVFilled(const X, Y: CFloat; const OffsetY: CFloat = 0)
      : Boolean;
    {< Draw a line between the point and its vertical reflection relative
      to `Y=0`, and then shifted by OffsetY.

      Do not confuse with reflection directly around OffsetY.

      Intended as an internal helper for complex primitive generation.

      @param(X Horizontal position of the point.)
      @param(Y Vertical position of the point.)
      @param(OffsetY Vertical offset.)
    }

    function PointMirrorHV(const X, Y: CFloat; const OffsetX: CFloat = 0;
      const OffsetY: CFloat = 0): Boolean;
    {< Draw a point and its horizontal and vertical reflections relative to
      `X=0` and `Y=0`, and then shifted by `OffsetX` and `OffsetY`.

      Do not confuse with reflection directly around `OffsetX` and `OffsetY`.

      Intended as an internal helper for complex primitive generation.

      @param(X Horizontal position of the point.)
      @param(Y Vertical position of the point.)
      @param(OffsetX Horizontal offset.)
      @param(OffsetY Vertical offset.)
    }

    function PointMirrorHVFilled(const X, Y: CFloat;
      const FillH: Boolean = True; const FillV: Boolean = True;
      const OffsetX: CFloat = 0; const OffsetY: CFloat = 0): Boolean;
    {< Draw lines between a point and its horizontal and vertical reflections
      relative to `X=0` and `Y=0`, and then shifted by `OffsetX` and `OffsetY`.

      Do not confuse with reflection directly around `OffsetX` and `OffsetY`.

      Intended as an internal helper for complex primitive generation.

      @warning(If `FillV` and `FillH` are both True, Vertices will acumulate
        opacity because `Lines` is used.)

      @note(If FillH and FillV are constant parameters consider change the
        call to this function with:

      @unorderedList( @itemSpacing Compact
        @item(`T`, `T`: `RectBorder`.)
        @item(`T`, `F`: `LineMirrorH`.)
        @item(`F`, `T`: `LineMirrorV`.)
        @item(`F`, `F`: `PointMirrorHV`.)
      )

      @param(X Horizontal position of the point.)
      @param(Y Vertical position of the point.)
      @param(FillH Draw lines between horizontal reflections.)
      @param(FillV Draw lines between vertical reflections.)
      @param(OffsetX Horizontal offset.)
      @param(OffsetY Vertical offset.)

      ToDo: Actually, it's not used with variable parameters anymore. But...
        if alternatives will repeat complex parameters, as 'X + FracX - 1',
        maybe its better keep as is...
    }

  {
    Line[s]
  }

    function Line(const P1, P2: TSDL_FPoint): Boolean; overload; inline;
    function Line(const X1, Y1, X2, Y2: CFloat): Boolean; overload; inline;
    {< Draw a line with current draw color.

      @note(Renderer Logical Presentation draw it _smooth_, with 1 logical
        pixel of width. If Lenght < 2, 2 logical pixels will overlap their
        opacity.)

      @param(P1 First point coordinates.)
      @param(P2 Second point coordinates.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
    }

    function LinesUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function Lines(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw an array of concatenated lines with current draw color.

      @note(Renderer Logical Presentation draw it _smooth_, with 1 logical
        pixel of width. If Lenght < 2, 2 logical pixels will overlap their
        opacity.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the first line.)
      @param(Count Number of points used to draw the lines. `0` means until
        the end of the array.)
    }

  {
    LineMirror[X]
  }

    function LineMirrorH(const X1, Y1, X2, Y2, OffsetX: CFloat): Boolean;
    function LineMirrorV(const X1, Y1, X2, Y2, OffsetY: CFloat): Boolean;
    function LineMirrorHV(const X1, Y1, X2, Y2, OffsetX, OffsetY: CFloat)
      : Boolean;

  {
    Triangle[X]
  }

    function TriangleUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
    function Triangle(const PArr: TSDLFPointDynArray; const idxFirst: Integer;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    function Triangle(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    function Triangle(const P1, P2, P3: TSDL_FPoint;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    function Triangle(const X1, Y1, X2, Y2, X3, Y3: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {<< Draw a filled triangle and its border with different colors.

      With idxFirst, three contiguous points can be used from a bigger array.

      @warning(Vertices in border will acumulate border opacity because
        `Lines` is used.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)

      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function TriangleBorderUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function TriangleBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    function TriangleBorder(const P1, P2, P3: TSDL_FPoint): Boolean; overload;
    function TriangleBorder(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
      overload;
    {<< Draw a triangle border with current draw color.

      With idxFirst, three contiguous points can be used from a bigger array.

      @warning(Vertices in border will acumulate border opacity because
        `Lines` is used.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
    }

    function TriangleFilledUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function TriangleFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    function TriangleFilled(const P1, P2, P3: TSDL_FPoint): Boolean; overload;
    function TriangleFilled(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
      overload;
    {<< Draw a filled triangle with current draw color.

      With idxFirst, three contiguous points can be used from a bigger array.

      @note(`TriangleFilled` actually uses `RenderGeometryRaw`. So,
        border vertices don't accumulate alpha.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
    }

    function TriangleFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function TriangleFillOnly(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    function TriangleFillOnly(const P1, P2, P3: TSDL_FPoint): Boolean;
      overload;
    function TriangleFillOnly(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
      overload;
    {<< Draw the fill of a triangle with current draw color.

      With idxFirst, three contiguous points can be used from a bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
    }

  {
    Rect[s][X]: Axis Aligned Rectangle.

    ToDo: ¿Overload with segment parameter? Don't do with coordinate parameters
      `X1, Y1, X2, Y2`. `X2, Y2` can be confused between size or absolute
      coordinates (and we need to normalize...).
  }

    function Rect(const aRect: TSDL_FRect; const BorderC, FillC: TSDL_FColor)
      : Boolean;
    {< Draw a filled Axis Aligned Rectangle and its border with
       different colors.

      @param(aRect Rectangle to draw.)
      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function RectBorder(const aRect: TSDL_FRect): Boolean; inline;
    {< Draw an Axis Aligned Rectangle border with current color.

      @param(aRect Rectangle to draw.)
    }

    function RectFilled(const aRect: TSDL_FRect): Boolean; inline;
    {< Draw a filled Axis Aligned Rectangle with current color.

      @param(aRect Rectangle to draw.)
    }

    function RectFillOnly(aRect: TSDL_FRect): Boolean;
    {< Draw the fill of Axis Aligned Rectangle with current color.

      @param(aRect Rectangle to draw.)
    }

    function RectsBorderUnsafe(const RArr: TSDLFRectDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function RectsBorder(const RArr: TSDLFRectDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw an array of Axis Aligned Rectangle borders with current color.

      @param(RArr Array of rectangles.)
      @param(idxFirst First rectangle to draw.)
      @param(Count Number of rectangles to draw. `0` means until the end of
        the array.)
    }

    function RectsFilledUnsafe(const RArr: TSDLFRectDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function RectsFilled(const RArr: TSDLFRectDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw an array of filled Axis Aligned Rectangles with current color.

      @param(RArr Array of rectangles.)
      @param(idxFirst First rectangle to draw.)
      @param(Count Number of rectangles to draw. `0` means until the end of
        the array.)
    }

  {
    Quad[X]: Quadrilateral.
  }

    function QuadUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
    function Quad(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
      overload;
    function Quad(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    function Quad(const P1, P2, P3, P4: TSDL_FPoint;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    function Quad(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {<< Draw a filled quadrilateral and its border with different colors.

      With idxFirst, four contiguous points can be used from a bigger array.

      @warning(Vertices in border will acumulate border opacity because
        `Lines` is used.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)
      @param(P3 Fourth point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
      @param(X4 Horizontal coordinate of the fourth point.)
      @param(Y4 Vertical coordinate of the fourth point.)

      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function QuadBorderUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function QuadBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    function QuadBorder(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
      overload;
    function QuadBorder(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat)
      : Boolean; overload;
    {<< Draw a quadrilateral border with current color.

      With idxFirst, four contiguous points can be used from a bigger array.

      @warning(Vertices in border will acumulate border opacity because
        `Lines` is used.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)
      @param(P3 Fourth point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
      @param(X4 Horizontal coordinate of the fourth point.)
      @param(Y4 Vertical coordinate of the fourth point.)
    }

    function QuadFilledUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function QuadFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    function QuadFilled(const P1, P2, P3, P4: TSDL_FPoint): Boolean; overload;
    function QuadFilled(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
       overload;
    {<< Draw a filled quadrilateral with current color.

      With idxFirst, four contiguous points can be used from a bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)
      @param(P3 Fourth point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
      @param(X4 Horizontal coordinate of the fourth point.)
      @param(Y4 Vertical coordinate of the fourth point.)
    }

    function QuadFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function QuadFillOnly(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    function QuadFillOnly(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
      overload;
    function QuadFillOnly(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat):
      Boolean; overload;
    {<< Draw the fill of a quadrilateral with current color.

      With idxFirst, four contiguous points can be used from a bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the triangle.)

      @param(P1 First point.)
      @param(P2 Second point.)
      @param(P3 Third point.)
      @param(P3 Fourth point.)

      @param(X1 Horizontal coordinate of the first point.)
      @param(Y1 Vertical coordinate of the first point.)
      @param(X2 Horizontal coordinate of the second point.)
      @param(Y2 Vertical coordinate of the second point.)
      @param(X3 Horizontal coordinate of the third point.)
      @param(Y3 Vertical coordinate of the third point.)
      @param(X4 Horizontal coordinate of the fourth point.)
      @param(Y4 Vertical coordinate of the fourth point.)
    }

  {
    Polygon[X]: Polygon
  }

    function PolygonUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer;
      const BorderC, FillC: TSDL_FColor): Boolean;
    function Polygon(const PArr: TSDLFPointDynArray; const idxFirst: Integer;
      Count: Integer; const BorderC, FillC: TSDL_FColor) : Boolean; overload;
    function Polygon(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    {<< Draw a filled polygon and its border with different colors.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the polygon.)
      @param(Count Number of points of the polygon.
        `0` means until array's end.)

      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function PolygonBorderUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function PolygonBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw a polygon border with current color.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(PArr Array of points.)
      @param(idxFirst First point of the polygon.)
      @param(Count Number of points of the polygon.
        `0` means until array's end.)
    }

    function PolygonFilledUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean;
    function PolygonFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw a filled polygon with current color.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the polygon.)
      @param(Count Number of points of the polygon.
        `0` means until array's end.)
    }

    function PolygonFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean;
    function PolygonFillOnly(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw the fill of a polygon with current color.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the polygon.)
      @param(Count Number of points of the polygon.
        `0` means until array's end.)
    }

    // function PolygonFilledUnsafeFP(const PArr: TSDLFPointDynArray;
    //   const idxFirst, Count: Integer): Boolean;
    // function PolygonFilledFP(const PArr: TSDLFPointDynArray;
    //   const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw a filled polygon with current color with rounded coordinates.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the polygon.)
      @param(Count Number of points of the polygon.
        `0` means until array's end.)
    }

    // function PolygonFillOnlyUnsafeFP(const PArr: TSDLFPointDynArray;
    //   const idxFirst, Count: Integer): Boolean;
    // function PolygonFillOnlyFP(const PArr: TSDLFPointDynArray;
    //   const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {<< Draw the fill of a polygon with current color with rounded coordinates.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @param(PArr Array of points.)
      @param(idxFirst First point of the polygon.)
      @param(Count Number of points of the polygon.
        `0` means until array's end.)
    }

  {
    RegPolyCC[X]: Regular Polygon by its Circumscribed Circle.
  }

    function RegPolyCCVertices(out PArr: TSDLFPointDynArray;
      const X, Y, R: CFloat; const NSides: Integer; Angle: CFloat = 0): Boolean;
    {< Populate `PArr` with the vertices of a Regular Polygon with `NSides`
      defined by it's circumscribed circunference and rotated an `Angle`.

      @param(PArr Array of points returned.)
      @param(X Horizontal position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(Y Vertical position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(R Radious of the circumscribed circunference of the regular
        polygon.)
      @param(NSides Number of sides of the regular polygon.)
      @param(Angle Angle of rotation of the regular polygon.)
    }

    function RegPolyCC(const X, Y, R: CFloat; const NSides: Integer;
      const BorderC, FillC: TSDL_FColor; const Angle: CFloat = 0): Boolean;
    {< Draw a filled regular polygon and its border with different colors
      described by its circumscribed circunference.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(X Horizontal position of the enter of the polygon and it's
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function RegPolyCCBorder(const X, Y, R: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Draw a regular polygon border with current draw color described by
      its circumscribed circunference.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(X Horizontal position of the enter of the polygon and it's
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
    }

    function RegPolyCCFilled(const X, Y, R: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Draw a filled regular polygon with current draw color described by
      its circumscribed circunference.

      @param(X Horizontal position of the enter of the polygon and it's
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
    }

    function RegPolyCCFillOnly(const X, Y, R: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Draw the fill of a regular polygon with current draw color described by
      its circumscribed circunference.

      @param(X Horizontal position of the enter of the polygon and it's
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
    }

  {
    RegPolySS[X]: Regular Polygon by its Side Lenght.
  }

    function RegPolySSVertices(out PArr: TSDLFPointDynArray;
      const X, Y, SideSize: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Populate `PArr` with the vertices of a Regular Polygon with `NSides`
      defined by Side lenght and rotated an `Angle`.

      @param(PArr Array of points returned.)
      @param(X Horizontal position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(Y Vertical position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(SideSize Length of the edges of the regular polygon.)
      @param(NSides Number of sides of the regular polygon.)
      @param(Angle Angle of rotation of the regular polygon.)
    }

    function RegPolySS(const X, Y, SideSize: CFloat; const NSides: Integer;
      const BorderC, FillC: TSDL_FColor; const Angle: CFloat = 0): Boolean;
      inline;
    {< Draw a filled regular polygon and its border with different colors
      described by its center and side length.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(X Horizontal position of the enter of the polygon.)
      @param(Y Vertical position of the center of the polygon.)
      @param(SideSize Length of its sides.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function RegPolySSBorder(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;
    {< Draw a regular polygon border with current color
      described by its center and side length.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(X Horizontal position of the enter of the polygon.)
      @param(Y Vertical position of the center of the polygon.)
      @param(SideSize Length of its sides.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
    }

    function RegPolySSFilled(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;
    {< Draw a filled regular polygon with current color
      described by its center and side length.

      @param(X Horizontal position of the enter of the polygon.)
      @param(Y Vertical position of the center of the polygon.)
      @param(SideSize Length of its sides.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
    }

    function RegPolySSFillOnly(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;
    {< Draw the fill of a regular polygon with current color
      described by its center and side length.

      @param(X Horizontal position of the enter of the polygon.)
      @param(Y Vertical position of the center of the polygon.)
      @param(SideSize Length of its sides.)
      @param(NSides Number of sides of the polygon.)
      @param(Angle Rotation angle of the polygon. `0` first vertex at top.)
    }

  {
    Circle[X]: Circle / Circunference.
  }

    function Circle(const X, Y, R: CFloat; const BorderC, FillC: TSDL_FColor)
      : Boolean;
    {< Draw a circle and a its circunference with different colors.

      @param(X Horizontal position of the center of the circunference.)
      @param(Y Vertical position of the center of the circunference.)
      @param(R Radius of the circunference.)
      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function CircleBorder(const X, Y: CFloat; R: CFloat): Boolean;
    {< Draw a circunference with current color.

      @param(X Horizontal position of the center of the circunference.)
      @param(Y Vertical position of the center of the circunference.)
      @param(R Radius of the circunference.)
    }

    function CircleFilled(const X, Y: CFloat; R: CFloat): Boolean;
    {< Draw a circle with current color.

      @param(X Horizontal position of the center of the circle.)
      @param(Y Vertical position of the center of the circle.)
      @param(R Radius of the circle.)
    }

    function CircleFillOnly(const X, Y: CFloat; R: CFloat): Boolean;
    {< Draw the fill of a circle with current color.

      @param(X Horizontal position of the center of the circle.)
      @param(Y Vertical position of the center of the circle.)
      @param(R Radius of the circle.)
    }

    // function CircleBorderFP(const X, Y, R: CFloat): Boolean;
    // function CircleFilledFP(const X, Y, R: CFloat): Boolean;
    // function CircleFillOnlyFP(const X, Y, R: CFloat): Boolean;

  {
    Ellipse[X]: Axis Aligned Ellipse.
  }
    function Ellipse(const X, Y, RX, RY: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean;
    {< Draw a filled Axis Aligned Ellipse and its border with different colors
      described by its center and radii.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function EllipseBorder(const X, Y: CFloat; RX, RY: CFloat): Boolean;
    {< Draw an Axis Aligned Ellipse border with current draw color
      described by its center and radii.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
    }

    function EllipseFilled(const X, Y: CFloat; RX, RY: CFloat): Boolean;
    {< Draw a filled Axis Aligned Ellipse with current draw color
      described by its center and radii.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
    }

    function EllipseFillOnly(const X, Y: CFloat; RX, RY: CFloat): Boolean;
    {< Draw the fill of an Axis Aligned Ellipse with current draw color
      described by its center and radii.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
    }

    // function EllipseBorderFP(const X, Y, RX, RY: CFloat)
    // function EllipseFilledFP(const X, Y, RX, RY: CFloat)
    // function EllipseFillOnlyFP(const X, Y, RX, RY: CFloat)

    function EllipseInRect(const aRect: TSDL_FRect;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    function EllipseInRect(const X, Y, W, H: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {<< Draw a filled Axis Aligned Ellipse and its border with different colors
      described by the rectagle where is inside.

      With this functions we can create circles and ellipses with odd diameter,
      if integer coordinates are used.

      @param(aRect Rect where the ellipse is inside)

      @param(X Horizontal position of the top-left corner of the rectangle.)
      @param(Y Vertical position of the top-left corner of the rectangle.)
      @param(W Width of the rectangle or ellipse horizontal diameter.)
      @param(H Height of the rectangle or ellipse vertical diameter.)

      @param(BorderC Color of the border.)
      @param(FillC Color for fill.)
    }

    function EllipseInRectBorder(const aRect: TSDL_FRect): Boolean;
      overload; inline;
    function EllipseInRectBorder(X, Y, W, H: CFloat): Boolean; overload;
    {<< Draw an Axis Aligned Ellipse border with current draw color
      described by the rectagle where is inside.

      With this functions we can create circles and ellipses with odd diameter,
      if integer coordinates are used.

      @param(X Horizontal position of the top-left corner of the rectangle.)
      @param(Y Vertical position of the top-left corner of the rectangle.)
      @param(W Width of the rectangle or ellipse horizontal diameter.)
      @param(H Height of the rectangle or ellipse vertical diameter.)

      @param(aRect Rect where the ellipse is inside)
    }

    function EllipseInRectFilled(const aRect: TSDL_FRect): Boolean;
      overload; inline;
    function EllipseInRectFilled(X, Y, W, H: CFloat): Boolean; overload;
    {<< Draw a filled Axis Aligned Ellipse with current draw color
      described by the rectagle where is inside.

      With this functions we can create circles and ellipses with odd diameter,
      if integer coordinates are used.

      @param(X Horizontal position of the top-left corner of the rectangle.)
      @param(Y Vertical position of the top-left corner of the rectangle.)
      @param(W Width of the rectangle or ellipse horizontal diameter.)
      @param(H Height of the rectangle or ellipse vertical diameter.)

      @param(aRect Rect where the ellipse is inside)
    }

    function EllipseInRectFillOnly(const aRect: TSDL_FRect): Boolean;
      overload; inline;
    function EllipseInRectFillOnly(X, Y, W, H: CFloat): Boolean; overload;
    {<< Draw the fill of an Axis Aligned Ellipse with current draw color
      described by the rectagle where is inside.

      With this functions we can create circles and ellipses with odd diameter,
      if integer coordinates are used.

      @param(X Horizontal position of the top-left corner of the rectangle.)
      @param(Y Vertical position of the top-left corner of the rectangle.)
      @param(W Width of the rectangle or ellipse horizontal diameter.)
      @param(H Height of the rectangle or ellipse vertical diameter.)

      @param(aRect Rect where the ellipse is inside)
    }

    // function EllipseInRectFP(const aRect: TSDL_FRect;
    //   const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    // function EllipseInRectFP(const X, Y, W, H: CFloat;
    //   const BorderC, FillC: TSDL_FColor): Boolean; overload;
    // function EllipseInRectBorderFP(const aRect: TSDL_FRect): Boolean;
    //   overload; inline;
    // function EllipseInRectBorderFP(X, Y, W, H: CFloat): Boolean; overload;
    // function EllipseInRectFilledFP(const aRect: TSDL_FRect): Boolean;
    //   overload; inline;
    // function EllipseInRectFilledFP(X, Y, W, H: CFloat): Boolean; overload;
    // function EllipseInRectFillOnlyFP(const aRect: TSDL_FRect): Boolean;
    //   overload; inline;
    // function EllipseInRectFillOnlyFP(X, Y, W, H: CFloat): Boolean; overload;


  {
    DebugText[F]
  }
    // ToDo: ¿Overload with the same name?
    function DebugText(const X, Y: CFloat; const aStr: String): Boolean;
      inline;
    function DebugTextF(const X, Y: CFloat; const aFmtStr: String;
      const Args: Array of Const): Boolean;

  {
    Destructor
  }

    destructor Destroy; override;
    {< Destructor of cCHXSDL3Renderer.

      if FreeRenderer is @True, destroys SDL_Renderer too.
    }
  end;

implementation

// Create

constructor cCHXSDL3Renderer.Create(const PSDLWindow: PSDL_Window;
  const Drivers: PAnsiChar);
begin
  if not Assigned(PSDLWindow) then
  begin
    SDL_SetError(PAnsiChar(rsCHXSDL3RendererNilError), ['PSDLWindow']);
    raise Exception.CreateFmt(rsCHXSDL3RendererNilError, ['PSDLWindow']);
  end;

  Self.Create(SDL_CreateRenderer(PSDLWindow, Drivers), True);
end;

constructor cCHXSDL3Renderer.Create(const PSDLRenderer: PSDL_Renderer;
  const FreeOnDestroy: Boolean);
begin
  if not Assigned(PSDLRenderer) then
  begin
    SDL_SetError(PAnsiChar(rsCHXSDL3RendererNilError), ['PSDLRenderer']);
    raise Exception.CreateFmt(rsCHXSDL3RendererNilError, ['PSDLRenderer']);
  end;

  inherited Create;
  SDLRenderer := PSDLRenderer;
  FreeRenderer := FreeOnDestroy;

  // Setting initial default BlendMode
  PrevBlendMode := SDL_BLENDMODE_BLEND;
  SDL_SetRenderDrawBlendMode(SDLRenderer, SDL_BLENDMODE_BLEND)
end;

{
  In an attempt to keep organized, implementations are in files to be
  included in `CHXSDL3Renderer` directory.
}
{$include 'CHXSDL3Renderer/Auxiliar.inc'}
{$include 'CHXSDL3Renderer/Color.inc'}
{$include 'CHXSDL3Renderer/Clear.inc'}
{$include 'CHXSDL3Renderer/Point.inc'}
{$include 'CHXSDL3Renderer/Line.inc'}
{$include 'CHXSDL3Renderer/Triangle.inc'}
{$include 'CHXSDL3Renderer/Rect.inc'}
{$include 'CHXSDL3Renderer/Quad.inc'}
{$include 'CHXSDL3Renderer/Polygon.inc'}
{$include 'CHXSDL3Renderer/RegPoly.inc'}
{$include 'CHXSDL3Renderer/Circle.inc'}
{$include 'CHXSDL3Renderer/Ellipse.inc'}

// DebugText[F]

function cCHXSDL3Renderer.DebugText(const X, Y: CFloat; const aStr: String)
  : Boolean;
begin
  Result := SDL_RenderDebugText(Self.SDLRenderer, X, Y, PAnsiChar(aStr));
end;

function cCHXSDL3Renderer.DebugTextF(const X, Y: CFloat;
  const aFmtStr: String; const Args: Array of Const): Boolean;
begin
  // It's not that easy :,-(
  // Result := SDL_RenderDebugTextFormat(Self.SDLRenderer, X, Y, 
  //   PAnsiChar(aStr), Args);
  Result := SDL_RenderDebugText(Self.SDLRenderer, X, Y,
  PAnsiChar(Format(aFmtStr, Args)));
end;

// Destroy

destructor cCHXSDL3Renderer.Destroy;
begin
  if FreeRenderer then
    SDL_DestroyRenderer(SDLRenderer);

  inherited;
end;

end.
{< This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
