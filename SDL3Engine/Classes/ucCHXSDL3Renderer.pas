unit ucCHXSDL3Renderer;
(*< Unit of cCHXSDL3Renderer class.

  cCHXSDL3Renderer is an encapsulation of `SDL_Renderer` and expands
  its funcionality.

  This way, instead of using `SDL_[...](PSDL_Renderer, [...])` functions,
  they will become direct methods of the class itself.

  In the context of cCHXSDL3Engine, this class will be created by
  cCHXSDL3Window on its creation and freed by it.

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
    @item(SDL3 can handle float point RGBA colors in range [0..1].)
    @itemLabel([s])
    @item(Draw multiple points, lines, or rectangles stored in an
      array of `TSDL_FPoint` or `TSDL_FRect`.)
    @itemLabel([Fill])
    @item(For filled (AABB) rectangles.)
  )

  All drawing functions use float parameters `CFloat`, wich is
  equivalent to `Single` in FPC.

  Internally, SDL3 [s] variants are actually used for drawing. Single point,
  line or rectangle versions only create an array of TSDL_FPoint or TSDL_FRect.
  As side note, SDL2 does the same with the adition of converting integer
  values to float.

  **For colors, SDL3 uses floats** too, while SDL2 uses Byte.

  Additionally, there are the `SDL_RenderGeometry[Raw]` functions used
  to draw filled triangles, which are more advanced as they allow gradient and
  textured rendering. They are likely faster for drawing filled polygons with
  color, although I need to test it.

  ## `SDL_gfx`:

  Initially, this unit was for `cCHXSDL2Engine` and had the purpose of remove
  the dependency of `SDL_gfx`. As this unit is finally being created before
  doing anything with SDL2, so the rant will be in `ucCHXSDL2Renderer` if
  implemented. XD

  Anyways, `SDL_gfx` (and SDL native functions) can be used to draw as
  `PSDL_Renderer` is exposed with `SDLRenderer` property.

  ## cCHXSDL3Renderer:

  The purpose of this class is as follows:

  @unorderedList( @itemSpacing Compact
    @item(Encapsulate `SDL_Renderer` drawing functions within a dedicated
      class.)
    @item(Add significant primitive drawing functions.)
    @item(Add some Quality of Life features:
      @unorderedList( @itemSpacing Compact
        @item(Overloaded variants of functions for various parameter types:
          @definitionList( @itemSpacing Compact
            @itemLabel(Point:)
            @item(`P: TSDL_FPoint` <=> `X, Y: CFloat`)
            @itemLabel(Rectangle:)
            @item(`R: TSDL_FRect` <=> `X, Y, W, H: CFloat`)
            @itemLabel(Color:)
            @item(`aColor: TSDL_FColor` <=> `R, G, B, A: CFloat` <=>
              `Grey, A: CFloat`)
            @itemLabel(Point Array:)
            @item(`PArr: Array of TSDL_FPoint` (`TSDLFPointDynArray`))
            @itemLabel(Rect Array:)
            @item(`RArr: Array of TSDL_FRect` (`TSDLFRectDynArray`))
          )
        )
        @item(Evaluate whether it is more efficient for these variants to call
          a common method or be implemented independently.)
        @item(If a method changes current draw color internally, it restores
          previous one.)
      )
    )
    @item(Provide methods to draw primitives:
      @unorderedList( @itemSpacing Compact
        @item(Border/Edges/Perimeter only.)
        @item(Full filled with a color.)
        @item(Only fill without border.)
        @item(Border and Fill with different colors: Trying to ensure that edges
          and fill do not overlap (wich is hard) as alpha transparencies would
          accumulate.)
      )
    )
  )

  Separately in uCHXSDL3TypeHelpers, several useful types and helpers for
  SDL data structures will be defined:

  @unorderedList( @itemSpacing Compact
    @item(Dynamic arrays of `TSDL_FColor`, `TSDL_FPoint`, `TSDL_FRect`, etc.:
      TSDLFColorDynArray, TSDLFPointDynArray, etc.)
    @item(ToDo: Specialized generic lists for those types, wich can be
      inherited to add custom methods: cSDLFPointList, cSDLFRectList, etc.
      @unorderedList( @itemSpacing Compact
        @item(`FGL` unit returns a not found operator overload error.)
        @item(Try with other container generics: Generics.Collections use an
          actual array.)
      )
    )
    @item(Type helpers for SDL types.)
    @item(Global functions returning SDL types. Useful to be used directly
      as parameters without explicitly declaring a variable)
  )

  More features can be added:

  @unorderedList( @itemSpacing Compact
    @item(Primitive rotations.)
    @item(Variable edge thickness for shapes.)
    @item(Antialiasing for shapes (which goes hand in hand with thick lines).)
    @item(Rotations and rotation pivot points.)
  )

  ## ToDo:

  @unorderedList( @itemSpacing Compact
    @item(Logical Presentation quirks: Althought initally many algorithms
      were adapted to Logical Presentation, changes were commented out, and 
      keep as "integer" ones.)
      @unorderedList( @itemSpacing Compact
        @item(Pixels are drawn at subpixel position with scale size.
          Lines (and Rects) are 1 scaled pixel thick. Filled Rects are
          drawn at subpixel coordinates too.
        @item(Drawing diagonal lines with native functions apply subpixel
          draw, so they are _smooth_ and don't draw "big" pixels.)
        @item(Drawing lines with length < 2 with `SDL_RenderLine` overdraws
          partially 2 pixel (and adding alpha 2 times in intersection).
          For vertical and horizontal lines this can be fixed with 
          `SDL_DrawFilledRect` and a width or height of 1.)
      )
    @item(Drawing lines with `SDL_RenderLines` (used by Rects, Quads and
      Polygons) apply alpha 2 times in the corner points...
      `SDL_RenderRect[s]` don't have this problem.)
    @item(Remember: `T[F]Rect` doesn't include `X + W` row or `Y + H` column.)
    @item(Use `SDL_SetError` and try not to halt execution, except in
      constructors that will have Exceptions.)
    @item(Do integer parameter overloads?)
    @item(¿Use `procedure` instead `function`? Rarely, Result will be checked.)
  )

  (C) 2026 Chixpy https://github.com/Chixpy
*)
{$mode ObjFPC}{$H+}
{$inline ON}

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
    as it expects at least a `SDL_Window` already created. In cCHXSDL3Engine
    context, this class is created by cCHXSDL3Window.

    Nearly all methods are functions with boolean Result as SDL_Renderer
    funtions are. They return @False on error and `SDL_GetError` can give
    information, cCHXSDL3Renderer ones included. cCHXSDL3Renderer constructors
    throw an Exception instead.
  }
  cCHXSDL3Renderer = class
  protected
    function IsValidArrayRange(const ArrLength, idxFirst: Integer;
      var Count: Integer; const LineNumber, FuncName: String): Boolean;
    (*< Check if `[idxFirst..(idxFirst+Count-1)]` is a valid range inside
      `[0..(ArrLength-1)]`.

      Uses `SDL_SetError` on invalid range and then changes value of `Count`
      with its maximum posible value . if `idxFirst` is out of the range,
      `Count` is set to `-1`.

      @param PArr Array to be checked.
      @param idxFirst Index of the first element.
      @param Count Number of elements, including `idxFirst`.
      @param LineNumber Use {$I %LINE%}.
      @param FuncName Use {$I %CURRENTROUTINE%}.
    *)

  {
    Unsafe methods were PArr range `[idxFirst..idxFirst+Count-1]` is not
      checked. To be called by other methods.
  }

    function PointsUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;

    function LinesUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;

    function RectsBorderUnsafe(const PArr: TSDLFRectDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function RectsFilledUnsafe(const PArr: TSDLFRectDynArray;
      const idxFirst, Count: Integer): Boolean; inline;

    function TriangleUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
    function TriangleBorderUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function TriangleFilledUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function TriangleFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;

    function QuadUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
    function QuadBorderUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function QuadFilledUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;
    function QuadFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer): Boolean; inline;

    function PolygonUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer;
      const BorderC, FillC: TSDL_FColor): Boolean;
    function PolygonBorderUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function PolygonFilledUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;
    function PolygonFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
      const idxFirst, Count: Integer): Boolean; inline;

  public
    SDLRenderer: PSDL_Renderer;
    //< Actual SDL_Renderer pointer.
    FreeRenderer: Boolean;
    //< Free SDL_Renderer on Destroy?;
    PrevBlendMode: TSDL_BlendMode;
    //< Previous blend mode when changing color.

  {
    Constructors
  }

    constructor Create(const PSDLWindow: PSDL_Window;
      const Drivers: PAnsiChar = nil); overload;
    {< Create a cCHXSDL3Renderer with a new SDL_Renderer and assign
       the SDL_Window to it.

      @param PSDLWindow SDL_Window that will asociated be to SDL_Renderer.
      @param Drivers Space separated list of drivers to try to use.
    }
    constructor Create(const PSDLRenderer: PSDL_Renderer;
      const FreeOnDestroy: Boolean); overload;
    {< Create a cCHXSDL3Renderer with an already created SDL_Renderer.

      @param PSDLRenderer SDL_Renderer to use.
      @param FreeRenderer Free SDL_Renderer on Destroy?
    }

  {
    [Set/Get]DrawColor
  }

    function SetDrawColor(const aColor: TSDL_FColor): Boolean; overload; inline;
    {< Set current draw color for primitives and clear.

      @param aColor Color with components in float [0..1] range.
    }
    function SetDrawColor(const R, G, B: CFloat; const A: CFloat = 1): Boolean;
      overload;
    {< Set current draw color for primitives and clear.

      @param R Red in float [0..1] range.
      @param G Green in float [0..1] range.
      @param B Blue in float [0..1] range.
      @param Alpha Opacity in float [0..1] range.
    }
    function SetDrawColor(const Grey: CFloat; const A: CFloat = 1): Boolean;
      overload; inline;
    {< Set current draw color for primitives and clear.

      @param Grey Grey in float [0..1] range.
    }

    {
      ToDo: Float functions that don't change blend mode.
        But I don't remenber why I wanted them, XD.
    }

    function GetDrawColor: TSDL_FColor; overload; inline;
    {< Get current draw color for primitives and clear.

      SDL errors are ignored.

      @Result Current draw color for primitives and clear.
    }
    function GetDrawColor(var aColor: TSDL_FColor): Boolean; overload; inline;
    {< Get current draw color for primitives and clear.
    }
    function GetDrawColor(var R, G, B, A: CFloat): Boolean; overload; inline;
    {< Get current draw color for primitives and clear.
    }

  {
    Clear
  }

    function Clear: Boolean; overload; inline;
    {< Clear render's target (usually a SDL_Window) with current draw color.
    }
    function Clear(const aColor: TSDL_FColor): Boolean; overload; inline;
    {< Clear render's target (usually a SDL_Window) with a color.

      Restores previous draw color after clearing.

      @param aColor Color to clear the render's target.
    }
    function Clear(const R, G, B: CFloat; const A: CFloat = 1): Boolean;
      overload; inline;
    {< Clear render's target (usually a SDL_Window) with a color.

      Restores previous draw color after clearing.

      @param R Red in float [0..1] range.
      @param G Green in float [0..1] range.
      @param B Blue in float [0..1] range.
      @param Alpha Opacity in float [0..1] range. @note(Is Alpha used?)
    }

  {
    Point[s]
  }

    function Point(const P: TSDL_FPoint): Boolean; overload; inline;
    {< Draw a point.

      @param P Point coordinates.
    }
    function Point(const X, Y: CFloat): Boolean; overload; inline;
    {< Draw a point.

      @param X Horizontal coordinate.
      @param Y Vertical coordinate.
    }

    function Points(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw an array of points.

      With idxFirst and Count a subrange of points will be drawn.

      @param PArr Array of points.
      @param idxFirst First point to draw.
      @param(Count Number of points to draw.
        `0` means until the end of the array.)
    }

  {
    Line[s]
  }

    function Line(const P1, P2: TSDL_FPoint): Boolean; overload; inline;
    {< Draw a line between two points.

      @param P1 First point coordinates.
      @param P2 Second point coordinates.
    }
    function Line(const X1, Y1, X2, Y2: CFloat): Boolean; overload; inline;
    {< Draw a line between two points.

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
    }

    function Lines(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw a polyline from an array of points.

      Draw lines concatenated, and it doesn't draw a line from last to first
      (Unless they are the same).

      With idxFirst and Count a subrange of points will be drawn.

      @warning(Alpha is acumulated in vertices because they are natively
        drawed as different segments.)

      @param PArr Array of points.
      @param idxFirst First point of the first line.
      @param(Count Number of points used to draw the lines.
        `0` means until the end of the array.)
    }

  {
    Triangle[X]
  }

    function Triangle(const PArr: TSDLFPointDynArray; const idxFirst: Integer;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled triangle with border.

      With idxFirst, three contiguous points can be used from a bigger array.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Triangle(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    {< Draw a filled triangle with border.

      Use the first 3 points of the Array.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Triangle(const P1, P2, P3: TSDL_FPoint;
     const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled triangle with border.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Triangle(const X1, Y1, X2, Y2, X3, Y3: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled triangle with border.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function TriangleBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    {< Draw only the border of a triangle.

      With idxFirst, any three contiguous points can be used from a
        bigger array.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
    }
    function TriangleBorder(const P1, P2, P3: TSDL_FPoint): Boolean; overload;
    {< Draw only the border of a triangle.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
    }
    function TriangleBorder(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
       overload;
    {< Draw only the border of a triangle.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
    }

    function TriangleFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    {< Draw a filled triangle.

      With idxFirst, any three contiguous points can be used from a
        bigger array.

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
    }
    function TriangleFilled(const P1, P2, P3: TSDL_FPoint): Boolean; overload;
    {< Draw a filled triangle.

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
    }
    function TriangleFilled(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
       overload;
    {< Draw a filled triangle.

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
    }

    function TriangleFillOnly(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload; inline;
    {< Teorically, this would draw a filled triangle without border.

      With idxFirst, any three contiguous points can be used from a
        bigger array.

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
    }
    function TriangleFillOnly(const P1, P2, P3: TSDL_FPoint): Boolean; 
      overload; inline;
    {< Teorically, this would draw a filled triangle without border.

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
    }
    function TriangleFillOnly(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
       overload; inline;
    {< Teorically, this would draw a filled triangle without border.

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
    }

  {
    Rect[X]: Axis Aligned Rectangle.

    ToDo: Overload with segment parameter? Don't do with coordinate parameters
      `X1, Y1, X2, Y2`. `X2, Y2` can be confused between or absolute
      coordinates (and we need to normalize...).
  }

    function Rect(const aRect: TSDL_FRect; const BorderC, FillC: TSDL_FColor)
      : Boolean;
    {< Draw a filled axis aligned rectangle with border.

      Previous draw color is restored.

      @param aRect Rectangle to draw.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function RectBorder(const aRect: TSDL_FRect): Boolean; inline;
    {< Draw only the border of an axis aligned rectangle.

      @param aRect Rectangle to draw.
    }

    function RectFilled(const aRect: TSDL_FRect): Boolean; inline;
    {< Draw a filled axis aligned rectangle.

      @param aRect Rectangle to draw.
    }

    function RectFillOnly(aRect: TSDL_FRect): Boolean;
    {< Draw a filled axis aligned rectangle without border.

      @param aRect Rectangle to draw.
    }

    function RectsBorder(const PArr: TSDLFRectDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw an array of rectangles, only borders with same color..

      With idxFirst and Count a subrange of rectangles will be drawn.

      @param PArr Array of rectangles.
      @param idxFirst First rectangle to draw.
      @param(Count Number of rectangles to draw.
        `0` means until the end of the array.)
    }

    function RectsFilled(const PArr: TSDLFRectDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw an array of filled rectangles, all with same color.

      With idxFirst and Count a subrange of rectangles will be drawn.

      @param PArr Array of rectangles.
      @param idxFirst First rectangle to draw.
      @param(Count Number of rectangles to draw.
        `0` means until the end of the array.)
    }

  {
    Quad[X]: Quadrilateral
  }

    function Quad(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
      overload;
    {< Draw a filled quadritateral with border.

      With idxFirst, four contiguous points can be used from a bigger array.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Quad(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    {< Draw a filled quadritateral with border.

      Use the first 4 points of the Array.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Quad(const P1, P2, P3, P4: TSDL_FPoint;
     const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled quadritateral with border.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param P4 Fourth point.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Quad(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled quadritateral with border.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
      @param X4 Horizontal coordinate of the fourth point.
      @param Y4 Vertical coordinate of the fourth point.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function QuadBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
    {< Draw only the border of a quadritateral.

      With idxFirst, any four contiguous points can be used from a
        bigger array.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
    }
    function QuadBorder(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
      overload;
    {< Draw only the border of a quadritateral.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param P3 Fourth point.
    }
    function QuadBorder(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat)
      : Boolean; overload;
    {< Draw only the border of a quadritateral.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
    }

    function QuadFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload; inline;
    {< Draw a filled quadritateral.

      With idxFirst, any four contiguous points can be used from a
        bigger array.

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
    }
    function QuadFilled(const P1, P2, P3, P4: TSDL_FPoint): Boolean; overload;
    {< Draw a filled quadritateral.

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param P4 Four point.
    }
    function QuadFilled(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
       overload;
    {< Draw a filled quadritateral.

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
      @param X4 Horizontal coordinate of the fourth point.
      @param Y4 Vertical coordinate of the fourth point.
    }

    function QuadFillOnly(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload; inline;
    {< Teorically, this would draw a filled quadritateral without border.

      With idxFirst, any four contiguous points can be used from a
        bigger array.

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
    }
    function QuadFillOnly(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
      overload; inline;
    {< Teorically, this would draw a filled quadritateral without border.

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param P4 Fourth point.
    }
    function QuadFillOnly(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat):
      Boolean; overload; inline;
    {< Teorically, this would draw a filled quadritateral without border.

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
      @param X4 Horizontal coordinate of the fourth point.
      @param Y4 Vertical coordinate of the fourth point.
    }

  {
    Polygon[X]
  }

    function Polygon(const PArr: TSDLFPointDynArray; const idxFirst: Integer;
      Count: Integer; const BorderC, FillC: TSDL_FColor) : Boolean; overload;
    {< Draw a filled polygon with border.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param idxFirst First point of the polygon.
      @param Count Number of points of the polygon. `0` means until array's end.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Polygon(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean; overload; inline;
    {< Draw a filled polygon with border.

      Use all points in the array

      Previous draw color is restored.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function PolygonBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw only the border of a polygon.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param PArr Array of points.
      @param idxFirst First point of the polygon.
      @param Count Number of points of the polygon
    }

    function PolygonFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw a Filled Polygon.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @param PArr Array of points.
      @param idxFirst First point of the polygon.
      @param Count Number of points of the polygon
    }

    function PolygonFillOnly(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean; 
    {< Draw a filled polygon without border.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @param PArr Array of points.
      @param idxFirst First point of the polygon.
      @param Count Number of points of the polygon
    }

  {
    RegPolyCC[X]: Regular Polygon with Circumscribed Circle
  }

    function RegPolyCC(const X, Y, R: CFloat; const NSides: Integer;
      const BorderC, FillC: TSDL_FColor; const Angle: CFloat = 0): Boolean;
    {< Draw a Regular Polygon with `NSides` defined by it's circumscribed
      circunference and rotated an `Angle` filled and with border.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(X Horizontal position of the enter of the polygon and it's
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(A Rotation angle of the polygon. `0` first vertex on the right.)
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function RegPolyCCBorder(const X, Y, R: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Draw the border of a Regular Polygon with `NSides` defined by it's
      circumscribed circunference and rotated an `Angle` filled and with border.

      @warning(Vertices will acumulate border opacity because `Lines` is used.)

      @param(X Horizontal position of the enter of the polygon and it's 
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's 
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(A Rotation angle of the polygon. `0` first vertex on the right.)
    }

    function RegPolyCCFilled(const X, Y, R: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Draw a filled Regular Polygon with `NSides` defined by it's
      circumscribed circunference and rotated an `Angle`.

      @param(X Horizontal position of the enter of the polygon and it's 
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's 
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(A Rotation angle of the polygon. `0` first vertex on the right.)
    }

    function RegPolyCCFillOnly(const X, Y, R: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean;
    {< Draw a filled Regular Polygon without border with `NSides` defined by
      it's circumscribed circunference and rotated an `Angle`.

      @param(X Horizontal position of the enter of the polygon and it's 
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's 
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(A Rotation angle of the polygon. `0` first vertex on the right.)
    }

  {
    RegPolySS[X]: Regular Polygon with Side Length
  }

    function RegPolySS(const X, Y, SideSize: CFloat; const NSides: Integer;
      const BorderC, FillC: TSDL_FColor; const Angle: CFloat = 0): Boolean;
      inline;
    {
      @warning(Vertices will acumulate border opacity because `Lines` is used.)
    }

    function RegPolySSBorder(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;
    {
      @warning(Vertices will acumulate border opacity because `Lines` is used.)
    }

    function RegPolySSFilled(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;

    function RegPolySSFillOnly(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;

  {
    Circle[X]
  }
    function Circle(const X, Y, R: CFloat; const BorderC, FillC: TSDL_FColor)
      : Boolean;
    {< Draw a filled circle with border.

      Previous draw color is restored.

      @param(X Horizontal position of the circunference's center.)
      @param(Y Vertical position of the circunference's center.)
      @param(R Radius of the circunference.)
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function CircleBorder(const X, Y, R: CFloat): Boolean;
    {< Draw a circunference at (`X`,`Y`) with `R` Radious.

      @param(X Horizontal position of the circunference's center.)
      @param(Y Vertical position of the circunference's center.)
      @param(R Radius of the circunference.)
    }

    function CircleFilled(const X, Y, R: CFloat): Boolean;
    {< Draw a filled circle at (`X`,`Y`) with `R` Radious.

      @param(X Horizontal position of the circle's center.)
      @param(Y Vertical position of the circle's center.)
      @param(R Radius of the circle.)
    }

    function CircleFillOnly(const X, Y, R: CFloat): Boolean;
    {< Draw a filled circle at (`X`,`Y`) with `R` Radious.

      @param(X Horizontal position of the circle's center.)
      @param(Y Vertical position of the circle's center.)
      @param(R Radius of the circle.)
    }

  {
    Ellipse[X]: Axis Aligned Ellipse.
  }
    function Ellipse(const X, Y, RX, RY: CFloat;
      const BorderC, FillC: TSDL_FColor): Boolean;
    {< Draw a filled ellipse with border.

      Previous draw color is restored.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function EllipseBorder(const X, Y, RX, RY: CFloat): Boolean;
    {< Draw a ellipse border.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
    }

    function EllipseFilled(const X, Y, RX, RY: CFloat): Boolean;
    {< Draw a filled ellipse.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
    }

    function EllipseFillOnly(const X, Y, RX, RY: CFloat): Boolean;
    {< Draw a filled ellipse.

      @param(X Horizontal position of the ellipse's center.)
      @param(Y Vertical position of the ellipse's center.)
      @param(RX Horizontal radius of the ellipse.)
      @param(RY Vertical radius of the ellipse.)
    }

  {
    EllipseInRect[X]: Axis Aligned Ellipse inside of a Rectangle.
  }

    // function EllipseInRect(const aRect: TSDL_FRect;
    //   const BorderC, FillC: TSDL_FColor) : Boolean; inline;
    // function EllipseInRect(const X, Y, W, H: CFloat;
    //   const BorderC, FillC: TSDL_FColor) : Boolean;
    {< Draw a filled ellipse with border.

      Previous draw color is restored.

      @param(X Horizontal position of the Top Left corner of the rectangle.)
      @param(Y Vertical position of the Top Left corner of the rectangle.)
      @param(W Width of the ellipse, Vertical diameter.)
      @param(H Height of the ellipse.)
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    // function EllipseInRectBorder(const aRect: TSDL_FRect): Boolean; inline;
    // function EllipseInRectBorder(const X, Y, W, H: CFloat): Boolean;
    {< Draw a ellipse border.

      @param(X Horizontal position of the Top Left corner of the rectangle.)
      @param(Y Vertical position of the Top Left corner of the rectangle.)
      @param(W Width of the ellipse, Vertical diameter.)
      @param(H Height of the ellipse.)
    }

    // function EllipseInRectFilled(const aRect: TSDL_FRect): Boolean; inline;
    // function EllipseInRectFilled(const X, Y, W, H: CFloat): Boolean;
    {< Draw a filled ellipse.

      @param(X Horizontal position of the Top Left corner of the rectangle.)
      @param(Y Vertical position of the Top Left corner of the rectangle.)
      @param(W Width of the ellipse, Vertical diameter.)
      @param(H Height of the ellipse.)
    }

    // function EllipseInRectFillOnly(const aRect: TSDL_FRect): Boolean; inline;
    // function EllipseInRectFillOnly(const X, Y, W, H: CFloat): Boolean;
    {< Draw a filled ellipse.

      @param(X Horizontal position of the Top Left corner of the rectangle.)
      @param(Y Vertical position of the Top Left corner of the rectangle.)
      @param(W Width of the ellipse, Vertical diameter.)
      @param(H Height of the ellipse.)
    }

  {
    DebugText[F]
  }
    // ToDo: ¿Overload with the same name?
    function DebugText(const X, Y: CFloat; const aStr: String): Boolean;
      inline;
    function DebugTextF(const X, Y: CFloat; const aFmtStr: String;
      const Args: Array of Const): Boolean;

  // Auxiliar methods for internal use:
  //   Implemented as needed with specific parámeters, but keep public as
  //     they can be useful.

  {
    PointMirror[X]
  }

    function PointMirrorH(const X, Y: CFloat; const OffsetX: CFloat = 0)
      : Boolean;
    {< Draw a point and its horizontal reflection relative to `X=0`, and then
      shifted by OffsetX.

      Do not confuse with reflection directly around OffsetX.

      Intended as an internal helper for complex primitive generation.

      @param X Horizontal position of the point.
      @param Y Vertical position of the point.
      @param OffsetX Horizontal offset.
    }

    function PointMirrorHFilled(const X, Y: CFloat; const OffsetX: CFloat = 0)
      : Boolean; inline;
    {< Draw a line between the point and its horizontal reflection relative
      to `X=0`, and then shifted by OffsetX.

      Do not confuse with reflection directly around OffsetX.

      Intended as an internal helper for complex primitive generation.

      @param X Horizontal position of the point.
      @param Y Vertical position of the point.
      @param OffsetX Horizontal offset.
    }

    function PointMirrorV(const X, Y: CFloat; const OffsetY: CFloat = 0)
      : Boolean;
    {< Draw a point and its vertical reflection relative to `Y=0`, and then 
      shifted by OffsetY.

      Do not confuse with reflection directly around OffsetY.

      Intended as an internal helper for complex primitive generation.

      @param X Horizontal position of the point.
      @param Y Vertical position of the point.
      @param OffsetY Vertical offset.
    }

    function PointMirrorVFilled(const X, Y: CFloat; const OffsetY: CFloat = 0)
      : Boolean; inline;
    {< Draw a line between the point and its vertical reflection relative
      to `Y=0`, and then shifted by OffsetY.

      Do not confuse with reflection directly around OffsetY.

      Intended as an internal helper for complex primitive generation.

      @param X Horizontal position of the point.
      @param Y Vertical position of the point.
      @param OffsetY Vertical offset.
    }

    function PointMirrorHV(const X, Y: CFloat; const OffsetX: CFloat = 0;
      const OffsetY: CFloat = 0): Boolean;
    {< Draw a point and its horizontal and vertical reflections relative to
      `X=0` and `Y=0`, and then shifted by `OffsetX` and `OffsetY`.

      Do not confuse with reflection directly around `OffsetX` and `OffsetY`.

      Intended as an internal helper for complex primitive generation.

      @param X Horizontal position of the point.
      @param Y Vertical position of the point.
      @param OffsetX Horizontal offset.
      @param OffsetY Vertical offset.
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

      @param X Horizontal position of the point.
      @param Y Vertical position of the point.
      @param FillH Draw lines between horizontal reflections.
      @param FillV Draw lines between vertical reflections.
      @param OffsetX Horizontal offset.
      @param OffsetY Vertical offset.
    }
    {
      ToDo: Actually, it's not used with variable parameters anymore. But...
        if alternatives will repeat complex parameters, 'X + FracX - 1', maybe
        its better keep as is...
    }

  {
    LineMirror[X]
  }

    function LineMirrorH(const X1, Y1, X2, Y2, OffsetX: CFloat): Boolean;
      inline;
    function LineMirrorV(const X1, Y1, X2, Y2, OffsetY: CFloat): Boolean;
      inline;
    function LineMirrorHV(const X1, Y1, X2, Y2, OffsetX, OffsetY: CFloat)
      : Boolean;

  {
    RegPoly[CC/SS]Vertices
  }

    function RegPolyCCVertices(out PArr: TSDLFPointDynArray;
      const X, Y, R: CFloat; const NSides: Integer; Angle: CFloat = 0): Boolean;
    {< Populate `PArr` with the vertices of a Regular Polygon with `NSides`
      defined by it's circumscribed circunference and rotated an `Angle`.

      @param PArr Array of points.
      @param(X Horizontal position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(Y Vertical position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(R Radious of the circumscribed circunference of the regular
        polygon.)
      @param NSides Number of sides of the regular polygon.
      @param Angle Angle of rotation of the regular polygon.
    }

    function RegPolySSVertices(out PArr: TSDLFPointDynArray;
      const X, Y, SideSize: CFloat; const NSides: Integer;
      const Angle: CFloat = 0): Boolean; inline;
    {< Populate `PArr` with the vertices of a Regular Polygon with `NSides`
      defined by Side lenght and rotated an `Angle`.

      @param PArr Array of points.
      @param(X Horizontal position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(Y Vertical position of the center of the circumscribed
        circunference of the regular polygon.)
      @param(SideSize Length of the edges of the regular polygon.)
      @param NSides Number of sides of the regular polygon.
      @param Angle Angle of rotation of the regular polygon.
    }

  {
    Destroy
  }

    destructor Destroy; override;
    {< Destructor of cCHXSDL3Renderer.

      if FreeRenderer is @True, destroys SDL_Renderer too.
    }
  end;

implementation

function cCHXSDL3Renderer.IsValidArrayRange(const ArrLength, idxFirst: Integer;
  var Count: Integer; const LineNumber, FuncName: String): Boolean;
var
  MaxCount: Integer;
begin
  if ArrLength <= 0 then
  begin
    if Count = 0 then
      Exit(True)
    else
    begin
      SDL_SetError('%s(%s) %s: Empty array',
        [PAnsiChar({$I %FILE%}), PAnsiChar(LineNumber), PAnsiChar(FuncName)]);
      Count := -1;
      Exit(False);
    end;
  end;

  if (idxFirst < 0) or (idxFirst >= ArrLength) or (Count < 0) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) not in [0..%d]) or (Count(%d) < 0)',
      [PAnsiChar({$I %FILE%}), PAnsiChar(LineNumber), PAnsiChar(FuncName),
      idxFirst, ArrLength - 1, Count]);
    Count := -1;
    Exit(False);
  end;

  MaxCount := ArrLength - idxFirst; // It's at least 1

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [PAnsiChar({$I %FILE%}), PAnsiChar(LineNumber), PAnsiChar(FuncName),
      idxFirst, Count, ArrLength]);
    Count := MaxCount;
    Exit(False);
  end;

  Result := True;
end;

{
  Unsafe methods were PArr range is not checked.
}

function cCHXSDL3Renderer.PointsUnsafe(const PArr: TSDLFPointDynArray; 
  const idxFirst, Count: Integer): Boolean;
begin
  Result := SDL_RenderPoints(Self.SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.LinesUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean;
begin
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.TriangleUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.TriangleFilledUnsafe(PArr, idxFirst);
      
    // Try to restore previous color anyway
    Exit(Self.SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(FillC)
      and Self.TriangleFillOnlyUnsafe(PArr, idxFirst);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.TriangleBorderUnsafe(PArr, idxFirst);
  end;

  // Try to restore previous color anyway
  Result := Self.SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.TriangleBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], 3)
    and Self.Line(PArr[idxFirst + 2], PArr[idxFirst]);
end;

function cCHXSDL3Renderer.TriangleFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean; inline;
var
  FColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(FColor)
    and SDL_RenderGeometryRaw(Self.SDLRenderer, nil, @PArr[idxFirst],
      SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0);
end;

function cCHXSDL3Renderer.TriangleFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean; inline;
begin
  // ToDo: Maybe there is an optimized method for triangles.
  Result := Self.PolygonFillOnlyUnsafe(PArr, idxFirst, 3);
end;

function cCHXSDL3Renderer.RectsBorderUnsafe(const PArr: TSDLFRectDynArray;
  const idxFirst, Count: Integer): Boolean; inline;
begin
  Result := SDL_RenderRects(Self.SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.RectsFilledUnsafe(const PArr: TSDLFRectDynArray;
  const idxFirst, Count: Integer): Boolean; inline;
begin
  Result := SDL_RenderFillRects(Self.SDLRenderer, @PArr[idxFirst], Count)
end;

function cCHXSDL3Renderer.QuadUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.QuadFilledUnsafe(PArr, idxFirst);
    // Try to restore previous color anyway
    Exit(Self.SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(FillC)
      and Self.QuadFillOnlyUnsafe(PArr, idxFirst);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.QuadBorderUnsafe(PArr, idxFirst);
  end;

  // Try to restore previous color anyway
  Result := Self.SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.QuadBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean; inline;
begin
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], 4)
    and Self.Line(PArr[idxFirst + 3], PArr[idxFirst]);
end;

function cCHXSDL3Renderer.QuadFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean; inline;
begin
  Result := Self.PolygonFilledUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean; inline;
begin
  Result := Self.PolygonFillOnlyUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.PolygonUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.PolygonFilledUnsafe(PArr, idxFirst, Count);
    // Try to restore previous color anyway
    Exit(Self.SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(FillC)
      and Self.PolygonFillOnlyUnsafe(PArr, idxFirst, Count);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.PolygonBorderUnsafe(PArr, idxFirst, Count);
  end;

  // Try to restore previous color anyway
  Result := Self.SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.PolygonBorderUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean; inline;
begin
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], Count)
    and Self.Line(PArr[idxFirst + Count - 1], PArr[idxFirst]);
end;

function cCHXSDL3Renderer.PolygonFilledUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean; inline;
{ Algorithm rationale & theoretical breakdown:

  In the absence of a precise, pre-packaged hardware algorithm, polygon filling
  is implemented using the Even-Odd Rule scanline algorithm.

  Each horizontal line (row) is scanned across the polygon's bounding box,
  finding edge intersections, sorting them by X coordinate, and filling
  scanline spans between paired intersections.

  Not sure how to manage vertical subpixels. Anyways, horizontal line corners
    will overlap polygon borders if Logical Presentation is used.

  Line Intersection Mathematics, given:

  - P1, P2: Endpoints of a polygon edge.
  - PI: Intersection point along the scanline.
  - CurrY: Current active scanline Y-coordinate.

  Interpolation formulas:

  - U = (CurrY - P1.Y) / (P2.Y - P1.Y)
  - PI.X = P1.X + U * (P2.X - P1.X)
  - PI.Y = CurrY

  Edge Cases & Constraints:

  - If (P2.Y - P1.Y) = 0: Division by zero occurs because the edge
    is completely horizontal.
  - If U NOT in [0..1]: The intersection point lies outside the physical
    segment bounds.

  Both edge cases are resolved via short-circuit logical checks prior to division:

  1. Bounds check: If both endpoints (P1.Y, P2.Y) are simultaneously above or
    below CurrY, no intersection is possible. Calculating U is skipped entirely.
  2. Zero check: If P1.Y = P2.Y, the line is horizontal; it is ignored for
    vertical intersection tracking to avoid division by zero (horizontal lines
    are naturally covered by adjacent scanlines).
  3. If both endpoint Y are less than CurrY then segment is not useful any more
    and it can be deleted from list of segments.

  ToDo: Optimize not drawing X < 0 and Y < 0, not sure if (0,0) in SDL can
    be moved...  but it will be a huge feature pushing and popping coordinate
    systems.
}
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: Integer;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_Point;
  SegList: Array of TCHXSDLSegment;
  XList: Array of Integer;
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst].Round;
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg].Round;
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    if P1.Y = P2.Y then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;
  if SegList[0].P2.Y = P2.Y then
    Delete(SegList, 0, 1)
  else
    SegList[0].P1 := P2; // Close the polygon (2)

  CurrY := MinY;
  while CurrY <= MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we will delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate X intersection point
        xIntersect := Round(
          P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y)
          );

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex];
      // Fix: "^" and "v" vertices double draw.
      if (aIndex <> 0) and (X1 = X2) then Inc(X1);
      X2 := XList[aIndex + 1];

      if X1 < X2 then
        Result := Self.Line(X1, CurrY, X2, CurrY) and Result
      else if X1 = X2 then
        Result := Self.Point(X1, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    Inc(CurrY);
  end;
end;
(* Adaptation to Logical Presentation
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: CFloat;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_FPoint;
  SegList: Array of TCHXSDLFSegment;
  XList: Array of CFloat; // Intersection values
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst];
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg];
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    // Don't add horizontal edges.
    // ToDo: use a sigma ~ 0.5?
    if SameValue(P1.Y, P2.Y) then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;

  SegList[0].P1 := P2; // Close the polygon (2)

  // ToDo: Not really sure how to handle vertical subpixel

  CurrY := MinY + 1;
  while CurrY < MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we wil delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate precise X intersection point
        xIntersect := P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y);

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex];
      X2 := XList[aIndex + 1];

      if X1 < X2 then
        Result := Self.Line(X1, CurrY, X2, CurrY) and Result
      else // X1 = X2
        Result := Self.Point(X1, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    CurrY += 1;
  end;
end;
*)

function cCHXSDL3Renderer.PolygonFillOnlyUnsafe(const PArr: TSDLFPointDynArray;
  const idxFirst, Count: Integer): Boolean; inline;
{
  See cCHXSDL3Renderer.PolygonFilledUnsafe.

  Modified to draw only interior without borders.
}
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2: Integer;
  CurrSeg, aIndex, xIntersect: Integer;
  P1, P2: TSDL_Point;
  SegList: Array of TCHXSDLSegment;
  XList: Array of Integer;
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst].Round;
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg].Round;
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    if P1.Y = P2.Y then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;
  if SegList[0].P2.Y = P2.Y then
    Delete(SegList, 0, 1)
  else
    SegList[0].P1 := P2; // Close the polygon (2)

  // Nothing to fill
  if ((MaxX - MinX) <= 1) or ((MaxY - MinY) <= 1) then
    Exit(True);

  CurrY := MinY + 1;
  while CurrY < MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we wil delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate X intersection point
        xIntersect := Round(
          P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y)
          );

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex] + 1;
      X2 := XList[aIndex + 1] - 1;

      if X1 < X2 then
        Result := Self.Line(X1, CurrY, X2, CurrY) and Result
      else if X1 = X2 then
        Result := Self.Point(X1, CurrY) and Result;
      // else draw nothing

      Inc(aIndex, 2);
    end;

    Inc(CurrY);
  end;
end;
(* Adaptation to Logical Presentation
var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: CFloat;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_FPoint;
  SegList: Array of TCHXSDLFSegment;
  XList: Array of CFloat; // Intersection values
begin
  Result := True;
  SetLength(SegList, Count);

  // Storing edges of the polygon as segments and computing bounding box.
  P2 := PArr[idxFirst];
  MinX := P2.X; MaxX := P2.X;
  MinY := P2.Y; MaxY := P2.Y;
  SegList[0].P2 := P2; // Close the polygon (1), P1 will be added later.

  aIndex := 1;
  for CurrSeg := 1 to (Count - 1) do
  begin
    P1 := P2;
    P2 := PArr[idxFirst + CurrSeg];
    if P2.X < MinX then MinX := P2.X;
    if P2.X > MaxX then MaxX := P2.X;
    if P2.Y < MinY then MinY := P2.Y;
    if P2.Y > MaxY then MaxY := P2.Y;

    // Don't add horizontal edges.
    // ToDo: use a sigma ~ 0.5?
    if SameValue(P1.Y, P2.Y) then
    begin
      // Removing last position, previous values are kept.
      SetLength(SegList, High(SegList));
      Continue;
    end;

    SegList[aIndex].P1 := P1;
    SegList[aIndex].P2 := P2;
    Inc(aIndex);
  end;

  SegList[0].P1 := P2; // Close the polygon (2)

  // Nothing to fill
  if ((MaxX - MinX) < 2) or ((MaxY - MinY) < 2) then
    Exit(True);

  // ToDo: Not really sure how to handle vertical subpixel

  CurrY := MinY + 1;
  while CurrY < MaxY do
  begin
    SetLength(XList, 0); // Removing previous intersections

    // Backwards because we wil delete not more useful segments
    for CurrSeg := High(SegList) downto 0 do
    begin
      P1 := SegList[CurrSeg].P1;
      P2 := SegList[CurrSeg].P2;

      // Remove segment not useful anymore
      if (P1.Y < CurrY) and (P2.Y < CurrY) then
      begin
        Delete(SegList, CurrSeg, 1);
        Continue;
      end;

      // Check if the scanline intersects the segment
      if ((P1.Y <= CurrY) and (P2.Y > CurrY))
        or ((P1.Y > CurrY) and (P2.Y <= CurrY)) then
      begin
        // Calculate precise X intersection point
        xIntersect := P1.X + (CurrY - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y);

        // Simple sorted insertion (maybe is slow)
        aIndex := 0;
        while (aIndex <= High(XList)) and (XList[aIndex] < xIntersect) do
          Inc(aIndex);
        Insert(xIntersect, XList, aIndex);
      end;
    end;

    Result := not Odd(Length(XList)) and Result;
    if not Result then // Parity check warning
      SDL_SetError('%s(%s) %s: Odd number of intersections in scanline %d',
            [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
            Floor(CurrY - MinY)]);

    // Draw horizontal spans between pairs of intersections
    aIndex := 0;
    while aIndex < High(XList) do
    begin
      X1 := XList[aIndex] + 1;
      X2 := XList[aIndex + 1] - 1;

      if X1 < X2 then
        Result := Self.Line(X1, CurrY, X2, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    CurrY += 1;
  end;
end;
*)

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
  Self.SDLRenderer := PSDLRenderer;
  Self.FreeRenderer := FreeOnDestroy;

  // Setting initial default BlendMode
  PrevBlendMode := SDL_BLENDMODE_BLEND;
  SDL_SetRenderDrawBlendMode(Self.SDLRenderer, SDL_BLENDMODE_BLEND)
end;

// SetDrawColor

function cCHXSDL3Renderer.SetDrawColor(const aColor: TSDL_FColor): Boolean;
begin
  Result := Self.SetDrawColor(aColor.R, aColor.G, aColor.B, aColor.A);
end;

function cCHXSDL3Renderer.SetDrawColor(const R, G, B, A: CFloat): Boolean;
var
  PrevR, PrevG, PrevB, PrevA: CFloat;
begin
  Result := SDL_GetRenderDrawColorFloat(Self.SDLRenderer,
    @PrevR, @PrevG, @PrevB, @PrevA);

  if SameValue(PrevR, R) and SameValue(PrevG, G) and SameValue(PrevB, B)
    and SameValue(PrevA, A) then
    Exit;

  if (PrevA >= 1) and (A < 1) then
    Result := Result
      and SDL_SetRenderDrawBlendMode(Self.SDLRenderer, Self.PrevBlendMode)
  else if (PrevA < 1) and (A >= 1) then
    Result := Result
      and SDL_GetRenderDrawBlendMode(Self.SDLRenderer, @Self.PrevBlendMode)
      and SDL_SetRenderDrawBlendMode(Self.SDLRenderer, SDL_BLENDMODE_NONE);

  // Try to set color anyways if something before fails
  Result := SDL_SetRenderDrawColorFloat(SDLRenderer, R, G, B, A)
    and Result;
end;

function cCHXSDL3Renderer.SetDrawColor(const Grey, A: CFloat): Boolean;
begin
  Result := Self.SetDrawColor(Grey, Grey, Grey, A);
end;

// GetDrawColor

function cCHXSDL3Renderer.GetDrawColor: TSDL_FColor;
begin
  SDL_GetRenderDrawColorFloat(Self.SDLRenderer, @Result.R, @Result.G,
    @Result.B, @Result.A);
end;

function cCHXSDL3Renderer.GetDrawColor(var aColor: TSDL_FColor): Boolean;
begin
  Result := SDL_GetRenderDrawColorFloat(Self.SDLRenderer, @aColor.R,
    @aColor.G, @aColor.B, @aColor.A);
end;

function cCHXSDL3Renderer.GetDrawColor(var R, G, B, A: CFloat): Boolean;
begin
  Result := SDL_GetRenderDrawColorFloat(Self.SDLRenderer, @R, @G, @B, @A);
end;

// Clear

function cCHXSDL3Renderer.Clear: Boolean;
begin
  Result := SDL_RenderClear(Self.SDLRenderer);
end;

function cCHXSDL3Renderer.Clear(const aColor: TSDL_FColor): Boolean;
begin
  Result := Self.Clear(aColor.R, aColor.G, aColor.B, aColor.A);
end;

function cCHXSDL3Renderer.Clear(const R, G, B, A: CFloat): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor)
    and Self.SetDrawColor(R, G, B, A)
    and SDL_RenderClear(Self.SDLRenderer);
  // Try to restore color if something failed
  Result := Self.SetDrawColor(TempColor) and Result;
end;

// Point

function cCHXSDL3Renderer.Point(const P: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderPoints(Self.SDLRenderer, @P, 1);
end;

function cCHXSDL3Renderer.Point(const X, Y: CFloat): Boolean;
begin
  Result := SDL_RenderPoint(Self.SDLRenderer, X, Y);
end;

// Points

function cCHXSDL3Renderer.Points(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  { Notes about SDL_RenderPoints:

    - It doesn't draw anything with Count <= 0. No error with negatives.
    - If Count exceeds array end, it doesn't care and draw points with
        "invalid" data (usually 0,0). No error, but this time is logical.
  }
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  // Try to draw although Result = False
  if Count > 0 then
    Exit(Self.PointsUnsafe(PArr, idxFirst, Count) and Result);
end;

// Line

function cCHXSDL3Renderer.Line(const P1, P2: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderLine(Self.SDLRenderer, P1.X, P1.Y, P2.X, P2.Y);
end;

function cCHXSDL3Renderer.Line(const X1, Y1, X2, Y2: CFloat): Boolean;
begin
  Result := SDL_RenderLine(Self.SDLRenderer, X1, Y1, X2, Y2);
end;

// Lines

function cCHXSDL3Renderer.Lines(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  { Notes about SDL_RenderLines:

    - It doesn't draw anything with Count <= 0. No error with negatives.
    - If Count exceeds array end, it doesn't care and draw lines with
        "invalid" data (usually 0,0). No error, but this time is logical.
  }
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});

  if Count <= 0 then Exit(Result);
  // Try to draw although Result = False
  if Count = 1 then
    Exit(Self.Point(PArr[idxFirst]) and Result);
  if Count > 1 then
    Exit(Self.LinesUnsafe(PArr, idxFirst, Count) and Result);
end;

// Triangle

function cCHXSDL3Renderer.Triangle(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and TriangleUnsafe(PArr, idxFirst, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Self.Triangle(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const P1, P2, P3: TSDL_FPoint;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := Self.TriangleUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const X1, Y1, X2, Y2, X3, Y3: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := Self.TriangleUnsafe(PArr, 0, BorderC, FillC);
end;

// TriangleBorder

function cCHXSDL3Renderer.TriangleBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and Self.TriangleBorderUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleBorder(const P1, P2, P3: TSDL_FPoint): Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P1;

  // Result := TriangleBorderUnsafe(PArr, idxFirst); And remove PArr[3]
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[0], 4);
end;

function cCHXSDL3Renderer.TriangleBorder(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  PArr[3] := PArr[0];

  // Result := TriangleBorderUnsafe(PArr, idxFirst); And remove PArr[3]
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[0], 4);
end;

// TriangleFilled

function cCHXSDL3Renderer.TriangleFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and Self.TriangleFilledUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFilled(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := Self.TriangleFilledUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFilled(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := Self.TriangleFilledUnsafe(PArr, 0);
end;

// TriangleFillOnly.

function cCHXSDL3Renderer.TriangleFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 3;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 3 then Exit(False);

  Result := Result and Self.TriangleFillOnlyUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := Self.TriangleFillOnlyUnsafe(PArr, 0);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  // ToDo: PArr[0].Init(X1, Y1) and remove compiler warning.
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := Self.TriangleFillOnlyUnsafe(PArr, 0);
end;

// Rectangle

function cCHXSDL3Renderer.Rect(const aRect: TSDL_FRect; const BorderC,
  FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.RectFilled(aRect);
    // Try to restore previous color anyway
    Exit(Self.SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(FillC)
      and Self.RectFillOnly(aRect);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.RectBorder(aRect);
  end;

  // Try to restore previous color anyway
  Result := Self.SetDrawColor(TempColor) and Result;
end;

// RectBorder

function cCHXSDL3Renderer.RectBorder(const aRect: TSDL_FRect): Boolean;
begin
  Result := SDL_RenderRect(Self.SDLRenderer, @aRect);
end;

// RectFilled

function cCHXSDL3Renderer.RectFilled(const aRect: TSDL_FRect): Boolean;
begin
  Result := SDL_RenderFillRect(Self.SDLRenderer, @aRect);
end;

// RectFillOnly

function cCHXSDL3Renderer.RectFillOnly(aRect: TSDL_FRect): Boolean;
begin
  if (aRect.W <= 2) or (aRect.H <= 2) then
    Exit(False); // ¿False?

  aRect.X += 1; aRect.Y += 1; aRect.W -= 2;  aRect.H -= 2;
  Result := SDL_RenderFillRect(Self.SDLRenderer, @aRect);
end;

function cCHXSDL3Renderer.RectsBorder(const PArr: TSDLFRectDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  Result := Result and RectsBorderUnsafe(PArr, idxFirst, Count);
end;

function cCHXSDL3Renderer.RectsFilled(const PArr: TSDLFRectDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  Result := Result and RectsFilledUnsafe(PArr, idxFirst, Count);
end;

// Quad(rilateral)

function cCHXSDL3Renderer.Quad(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Result and Self.QuadUnsafe(PArr, idxFirst, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Self.Quad(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const P1, P2, P3, P4: TSDL_FPoint;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := Self.QuadUnsafe(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  PArr[3].X := X4; PArr[3].Y := Y4;
  Result := Self.QuadUnsafe(PArr, 0, BorderC, FillC);
end;

// QuadBorder

function cCHXSDL3Renderer.QuadBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Result and Self.QuadBorderUnsafe(PArr, idxFirst);
end;

function cCHXSDL3Renderer.QuadBorder(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4; PArr[4] := P1;
  // Result := QuadBorderUnsafe(PArr, idxFirst); And remove PArr[4]
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[0], 5);
end;

function cCHXSDL3Renderer.QuadBorder(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  PArr[3].X := X4; PArr[3].Y := Y4;
  PArr[4] := PArr[0];
  // Result := QuadBorderUnsafe(PArr, idxFirst); And remove PArr[4]
  Result := SDL_RenderLines(Self.SDLRenderer, @PArr[0], 5);
end;

// QuadFilled

function cCHXSDL3Renderer.QuadFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Result and Self.PolygonFilledUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFilled(const P1, P2, P3, P4: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := Self.PolygonFilledUnsafe(PArr, 0, 4);
end;

function cCHXSDL3Renderer.QuadFilled(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1; PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3; PArr[3].X := X4; PArr[3].Y := Y4;
  Result := Self.PolygonFilledUnsafe(PArr, 0, 4);
end;

// QuadFillOnly.

function cCHXSDL3Renderer.QuadFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer = 0): Boolean;
var
  Count: Integer;
begin
  Count := 4;
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count < 4 then Exit(False);

  Result := Self.PolygonFillOnlyUnsafe(PArr, idxFirst, 4);
end;

function cCHXSDL3Renderer.QuadFillOnly(const P1, P2, P3, P4: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := Self.PolygonFillOnlyUnsafe(PArr, 0, 4);
end;

function cCHXSDL3Renderer.QuadFillOnly(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1; PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3; PArr[3].X := X4; PArr[3].Y := Y4;
  Result := Self.PolygonFillOnlyUnsafe(PArr, 0, 4);
end;

// Polygon.

function cCHXSDL3Renderer.Polygon(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer; const BorderC, FillC: TSDL_FColor)
  : Boolean; overload;
begin
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  Result := Result and PolygonUnsafe(PArr, idxFirst, Count, BorderC, FillC);
end;

function cCHXSDL3Renderer.Polygon(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Self.Polygon(PArr, 0, 0, BorderC, FillC);
end;

// PolygonBorder

function cCHXSDL3Renderer.PolygonBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit(Result);

  case Count of
    // 0: Exit(True); It can't happen
    1: Result := Self.Point(PArr[idxFirst]);
    2: // Result := Render.Line(PArr[idxFirst], PArr[idxFirst + 1]);
      Result := SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], 2);
  otherwise
    begin
      // Result := Lines(PArr[idxFirst], Count) and...
      Result := SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], Count)
        and Self.Line(PArr[idxFirst + Count - 1], PArr[idxFirst]);
    end;
  end;
end;

// PolygonFilled

function cCHXSDL3Renderer.PolygonFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
var
  FColor: TSDL_FColor;
begin
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  if Count <= 0 then Exit(Result); // Includes Result = False

  case Count of
    // 0: Exit(Result); // It can't happen
  1: Exit(Self.Point(PArr[idxFirst]));

  2: // Exit(Render.Line(PArr[idxFirst], PArr[idxFirst + 1]));
    Exit(SDL_RenderLines(Self.SDLRenderer, @PArr[idxFirst], 2));

  3: // Exit(Self.TriangleFilledUnsafe(PArr, idxFirst));
    Exit(Self.GetDrawColor(FColor)
      and SDL_RenderGeometryRaw(Self.SDLRenderer, nil, @PArr[idxFirst],
        SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0));

  otherwise
    Result := Self.PolygonFilledUnsafe(PArr, idxFirst, Count);
  end;
end;

// PolygonFillOnly

function cCHXSDL3Renderer.PolygonFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
begin
  Result := Self.IsValidArrayRange(Length(PArr), idxFirst, Count,
    {$I %LINE%}, {$I %CURRENTROUTINE%});
  // `Count <= 2` includes:
  //  - Result = False
  //  - 1, 2: Point or line, nothing to fill inside.
  if Count <= 2 then Exit(Result); 

  // If TriangleFillOnly has an optimized method...
  if Count = 3 then
    Exit(Self.TriangleFillOnlyUnsafe(PArr, idxFirst));

  Result := Self.PolygonFillOnlyUnsafe(PArr, idxFirst, Count);
end;

// RegPolyCC

function cCHXSDL3Renderer.RegPolyCC(const X, Y, R: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and Self.Polygon(PArr, BorderC, FillC);
end;

// RegPolyCCBorder

function cCHXSDL3Renderer.RegPolyCCBorder(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and Self.PolygonBorder(PArr);
end;

// RegPolyCCFilled

function cCHXSDL3Renderer.RegPolyCCFilled(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and Self.PolygonFilled(PArr);
end;

// RegPolyCCFillOnly

function cCHXSDL3Renderer.RegPolyCCFillOnly(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and Self.PolygonFillOnly(PArr);
end;

// RegPolySS

function cCHXSDL3Renderer.RegPolySS(const X, Y, SideSize: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and Self.Polygon(PArr, BorderC, FillC);
end;

// RegPolySSBorder

function cCHXSDL3Renderer.RegPolySSBorder(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and Self.PolygonBorder(PArr);
end;

// RegPolySSFilled

function cCHXSDL3Renderer.RegPolySSFilled(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and Self.PolygonFilled(PArr);
end;

// RegPolySSFillOnly

function cCHXSDL3Renderer.RegPolySSFillOnly(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := Self.RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle)
    and Self.PolygonFillOnly(PArr);
end;

// Circle[X]

function cCHXSDL3Renderer.Circle(const X, Y, R: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.CircleFilled(X, Y, R);
    // Try to restore previous color anyway
    Exit(Self.SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(FillC)
      and Self.CircleFillOnly(X, Y, R);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.CircleBorder(X, Y, R);
  end;

  // Try to restore previous color anyway
  Result := Self.SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.CircleBorder(const X, Y, R: CFloat): Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing pixels at cardinal/diagonal angles.
  - Minor initialization optimization. }
var
  IntX, IntY, t1, t2, CurrX, CurrY: Integer;
begin
  CurrX := Abs(Round(R));
  if CurrX < 1 then Exit(Self.Point(X, Y));
  IntX := Round(X); IntY := Round(Y); // IntR := Round(R);

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  Result := Self.PointMirrorV(IntX, CurrX, IntY)
    and PointMirrorH(CurrX, IntY, IntX);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  if t2 >= 0 then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHV(CurrX, CurrY, IntX, IntY)
      and PointMirrorHV(CurrY, CurrX, IntX, IntY)
      and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if CurrX = CurrY then
    Result := PointMirrorHV(CurrX, CurrY, IntX, IntY) and Result;
end;

(* With subpixel adaptation:
var
  t1, t2, CurrX, CurrY: Integer;
  FracR: CFloat; // Subpixel Radius Offset.
begin
  R := Abs(R);
  if R < 1 then Exit(Self.Point(X, Y));
  CurrX := Floor(R);
  FracR := R - CurrX;

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  //   - Subpixel offset.
  if IsZero(FracR) then
    Result := Self.PointMirrorV(X, R, Y) and PointMirrorH(R, Y, X)
  else
    Result := Self.PointMirrorHVFilled(FracR, R, True, False, X, Y)
      and Self.PointMirrorHVFilled(R, FracR, False, True, X, Y);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  if t2 >= 0 then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := PointMirrorHV(CurrX + FracR, CurrY + FracR, X, Y)
      and PointMirrorHV(CurrY + FracR, CurrX + FracR, X, Y)
      and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if CurrX = CurrY then
    Result := PointMirrorHV(CurrX + FracR, CurrY + FracR, X, Y) and Result;
end;
*)

function cCHXSDL3Renderer.CircleFilled(const X, Y, R: CFloat): Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing lines already drawn.
  - Minor initialization optimization.
  - Fill the circle with horizontal lines. }
var
  IntX, IntY, t1, t2, CurrX, CurrY: Integer;
begin
  CurrX := Abs(Round(R));
  if R < 1 then Exit(Self.Point(X, Y));
  IntX := Round(X); IntY := Round(Y); // IntR := Round(R);

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0° - 180°) only once.

  Result := Self.PointMirrorHFilled(CurrX, IntY, IntX);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;

  if t2 >= 0 then
  begin
    Result := Self.PointMirrorV(IntX, CurrX, IntY) and Result;
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := Self.PointMirrorHVFilled(CurrX, CurrY, True, False, IntX, IntY)
      and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
      Result := Self.PointMirrorHVFilled(CurrY - 1, CurrX + 1, True, False,
        IntX, IntY) and Result;
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if (t2 >= 0) and (CurrY = CurrX) then
    Result := Self.PointMirrorHVFilled(CurrX, CurrY, True, False, IntX, IntY)
      and Result;
end;
(* With subpixel adaptation:
var
  t1, t2, CurrX, CurrY: Integer;
  FracR: CFloat; // Subpixel Radius Offset. 
begin
  R := Abs(R);
  if R < 1 then Exit(Self.Point(X, Y));
  CurrX := Floor(R);
  FracR := R - CurrX;

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  //   - Subpixel offset.
  if IsZero(FracR) then
    Result := Self.PointMirrorHFilled(R, Y, X)
  else
    Result := Self.RectFilled(
      SDLFRect(X - R, Y - FracR, (2 * R) + 1, (2 * FracR) + 1));

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;

  if t2 >= 0 then
  begin
    t1 := t2; Dec(CurrX);
    if IsZero(FracR) then
      Result := Self.PointMirrorV(X, R, Y) and Result
    else
      Result := Self.PointMirrorHVFilled(FracR, R, True, False, X, Y)
        and Result;
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := Self.PointMirrorHVFilled(CurrX + FracR, CurrY + FracR,
        True, False, X, Y) and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    if t2 >= 0 then
    begin
      t1 := t2; Dec(CurrX);
      Result := Self.PointMirrorHVFilled(CurrY + FracR - 1, CurrX + FracR + 1,
        True, False, X, Y) and Result;
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if (t2 >= 0) and (CurrY = CurrX) then
    Result := Self.PointMirrorHVFilled(CurrX + FracR, CurrY + FracR,
      True, False, X, Y) and Result;
end;
*)

function cCHXSDL3Renderer.CircleFillOnly(const X, Y, R: CFloat)
  : Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing lines already drawn.
  - Minor initialization optimization.
  - Fill the circle with horizontal lines without border. }
var
  IntX, IntY, t1, t2, CurrX, CurrY: Integer;
  DrawLine: Boolean;
begin
  CurrX := Abs(Round(R));
  if R < 1 then Exit(True); // Nothing to fill
  IntX := Round(X); IntY := Round(Y); // IntR := Round(R);

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  Result := Self.PointMirrorHFilled(CurrX - 1, IntY, IntX);

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  DrawLine := t2 >= 0;
  if DrawLine then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := Self.PointMirrorHVFilled(CurrX - 1, CurrY, True, False,
      IntX, IntY) and Result;
    if DrawLine then
      Result := Self.PointMirrorHVFilled(CurrY - 1, CurrX, True, False,
        IntX, IntY) and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    DrawLine := t2 >= 0;
    if DrawLine then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if DrawLine and (CurrY = CurrX) then
    Result := Self.PointMirrorHVFilled(CurrX - 1, CurrY, True, False,
      IntX, IntY) and Result;
end;
(* With subpixel adaptation. 
var
  t1, t2, CurrX, CurrY: Integer;
  DrawLine: Boolean;
  FracR: CFloat; // Subpixel Radius Offset.
begin
  R := Abs(R);
  if R < 1 then Exit(True); // Nothing to fill
  CurrX := Floor(R);
  FracR := R - CurrX;

  // 1st iteration unrolled:
  //   - Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  //   - Subpixel adjust.
  if IsZero(FracR) then
    Result := Self.PointMirrorHFilled(R - 1, Y, X)
  else
    Result := Self.RectFilled(
      SDLFRect(X - R + 1, Y - FracR, 2 * R - 1, 2 * FracR + 1));

  CurrY := 1; t1 := 1 + (CurrX div 16); t2 := t1 - CurrX;
  DrawLine := t2 >= 0;
  if DrawLine then
  begin
    t1 := t2; Dec(CurrX);
  end;

  // Main Loop
  while CurrX > CurrY do
  begin
    Result := Self.PointMirrorHVFilled(CurrX + FracR - 1, CurrY + FracR,
        True, False, X, Y) and Result;
    if DrawLine then
      Result := Self.PointMirrorHVFilled(CurrY + FracR - 1, CurrX + FracR,
        True, False, X, Y) and Result;

    Inc(CurrY); Inc(t1, CurrY); t2 := t1 - CurrX;
    DrawLine := t2 >= 0;
    if DrawLine then
    begin
      t1 := t2; Dec(CurrX);
    end;
  end;

  // Drawing diagonal angles (45°, 135°, 225°, 315°) only once
  if DrawLine and (CurrY = CurrX) then
    Result := Self.PointMirrorHVFilled(CurrX + FracR - 1, CurrY + FracR,
      True, False, X, Y) and Result;
end;
*)

// Ellipse[X]

function cCHXSDL3Renderer.Ellipse(const X, Y, RX, RY: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := Self.GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.EllipseFilled(X, Y, RX, RY);
    // Try to restore previous color anyway
    Exit(Self.SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(FillC)
      and Self.EllipseFillOnly(X, Y, RX, RY);
  end;

  if BorderC.A > 0 then
  begin
    Result := Result
      and Self.SetDrawColor(BorderC)
      and Self.EllipseBorder(X, Y, RX, RY);
  end;

  // Try to restore previous color anyway
  Result := Self.SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.EllipseBorder(const X, Y, RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:
  - Special cases: `RX <= 1` or `RY <= 1`.
  - Avoid redrawing pixels at cardinal points.
  - Precalculate constant terms.
}
var
  IntX, IntY, IntRX, IntRY, CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
begin
  IntX := Round(X); IntY := Round(Y);
  IntRX := Abs(Round(RX)); IntRY := Abs(Round(RY));

  // Special cases
  if (IntRX < 1) then
    Exit(Self.Line(X, Y - IntRY, X, Y + IntRY));
  if (IntRY < 1) then
    Exit(Self.Line(X - IntRX, Y, X + IntRX, Y));

  RY2 := IntRY * IntRY;

  // Initialization
  CurrX := -IntRX; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := IntRX * IntRX; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  Result := PointMirrorH(IntRX, IntY, IntX);

  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
  end;
  if e2 <= dY then
  begin
    Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
  end;

  // Main Loop
  while CurrX < 0 do
  begin
    Result := PointMirrorHV(CurrX, CurrY, IntX, IntY) and Result;

    e2 := err * 2;
    if e2 <= dY then
    begin
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
    end;
    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;
  end;

  // Draw last points only once
  if CurrY < IntRY then
    Result := Self.LineMirrorV(IntX, CurrY, IntX, IntRY, IntY)
      and Result
  else
    Result := Self.PointMirrorV(IntX, IntRY, IntY) and Result;
end;
(* With subpixel adaptation:
var
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  FracRX, FracRY: CFloat;
begin
  RX := Abs(RX); RY := Abs(RY);
  RX2 := Floor(RX); RY2 := Floor(RY);
  FracRX := RX - RX2; FracRY := RY - RY2;

  // Special cases
  if (RX2 < 1) or (RY2 < 1) then
    // Draws both vertical and horizontal degenerate cases
    Exit(Self.RectFilled(SDLFRect(X - RX, Y - RY, 2 * RX + 1, 2 * RY + 1)));

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  if IsZero(FracRY) then
    Result := PointMirrorH(RX, Y, X)
  else
    Result := Self.LineMirrorH(RX, Y - FracRY, RX, Y + FracRY, X);

  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
  end;
  if e2 <= dY then
  begin
    Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
  end;

  // Main Loop
  while CurrX < 0 do
  begin
    Result := PointMirrorHV(CurrX - FracRX, CurrY + FracRY, X, Y) and Result;

    e2 := err * 2;
    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;
    if e2 <= dY then
    begin
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
    end;
  end;

  Result := Self.RectFilled(SDLFRect(X -FracRX, Y - RY, 2 * FracRX + 1, 1))
    and Self.RectFilled(SDLFRect(X -FracRX, Y + RY, 2 * FracRX + 1, 1))
    and Result;
  // Draw last points only once
  // Result := Self.PointMirrorHVFilled(FracRX, RY, True, False, X, Y)
  //   and Result;
end;
*)

function cCHXSDL3Renderer.EllipseFilled(const X, Y, RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases: `RX <= 1` or `RY <= 1`
  - Fails in when RY >> RX.
  - Avoid redrawing pixels at cardinal points.
  - Fill the ellipse with horizontal lines.
  - Precalculate constant terms. }
var
  IntX, IntY, IntRX, IntRY, CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
begin
  IntX := Round(X); IntY := Round(Y);
  IntRX := Abs(Round(RX)); IntRY := Abs(Round(RY));

  // Special cases
  if (IntRX < 1) then
    Exit(Self.Line(X, Y - IntRY, X, Y + IntRY));
  if (IntRY < 1) then
    Exit(Self.Line(X - IntRX, Y, X + IntRX, Y));

  RY2 := IntRY * IntRY;

  // Initialization
  CurrX := -IntRX; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := IntRX * IntRX; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  Result := PointMirrorHFilled(IntRX, IntY, IntX);

  // Main Loop
  while CurrX <= 0 do // 0 included
  begin
    e2 := err * 2;

    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;

    if e2 <= dY then
    begin
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
      // Draw line after changing CurrY only. 
      Result := Self.LineMirrorV(IntX - CurrX, CurrY, IntX + CurrX, CurrY, IntY)
        // Self.PointMirrorHVFilled(CurrX, CurrY, True, False, IntX, IntY)
        and Result;
    end;
  end;

  // Draw last points only once
  if CurrY < IntRY then
    Result := Self.LineMirrorV(IntX, CurrY + 1, IntX, IntRY, IntY)
      and Result;
end;

(* With subpixel adaptation.
var
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  FracRX, FracRY: CFloat;
begin
  RX := Abs(RX); RY := Abs(RY);
  RX2 := Floor(RX); RY2 := Floor(RY);
  FracRX := RX - RX2; FracRY := RY - RY2;

  // Special cases
  if (RX2 <= 1) or (RY2 <= 1) then
    // Draws both vertical and horizontal degenerate cases
    Exit(RectFilled(SDLFRect(X - RX, Y - RY, 2 * RX + 1, 2 * RY + 1)));

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  if IsZero(FracRY) then
    Result := PointMirrorHFilled(RX, Y, X)
  else
    Result := Self.RectFilled(
      SDLFRect(X - RX, Y - FracRY, RX * 2 + 1, FracRY * 2 + 1));

  // Main Loop
  while CurrX <= 0 do // 0 included
  begin
    e2 := err * 2;

    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;

    if e2 <= dY then
    begin
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
      // Draw line after changing CurrY AND CurrX only.
      Result := Self.PointMirrorHVFilled(CurrX - FracRX, CurrY + FracRY,
        True, False, X, Y) and Result;
    end;
  end;
end;
*)

function cCHXSDL3Renderer.EllipseFillOnly(const X, Y, RX, RY: CFloat)
  : Boolean;
{ Modification of Alois Zingl's implementation (https://zingl.github.io)
  of Bresenham's algorithm:

  - Special cases: `RX <= 1` or `RY <= 1`.
  - Avoid redrawing pixels at cardinal points.
  - Fill the ellipse with horizontal lines without border.
  - Precalculate constant terms.
}
var
  IntX, IntY, IntRX, IntRY, CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  DrawLine: Boolean;
begin
  IntX := Round(X); IntY := Round(Y);
  IntRX := Round(RX); IntRY := Round(RY);
  RX2 := Abs(IntRX); RY2 := Abs(IntRY);

  // Special cases
  if (RX2 < 1) or (RY2 < 1) then
    Exit(True); // Nothing to fill, well it's a subpixel box

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  //Result := PointMirrorHFilled(IntRX - 1, IntY, IntX);

  Result := True;

  // Skip first iteration
  e2 := err * 2;
  if e2 <= dY then
  begin
    Result := PointMirrorHFilled(IntRX - 1, IntY, IntX)
        and Result;
    Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
  end;
  if e2 >= dX then
  begin
    Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
  end;

  // Main Loop
  while CurrX < 0 do
  begin
    e2 := err * 2;
    DrawLine := e2 <= dY;
    if DrawLine then
    begin
      Result := PointMirrorHVFilled(CurrX + 1, CurrY, True, False, IntX, IntY)
        and Result;
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
    end;
    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;
  end;
end;

(* With subpixel adaptation.
var
  CurrX, CurrY, dX, dY, err, e2, RX2, RY2: Integer;
  FracRX, FracRY: CFloat;
begin
  RX := Abs(RX); RY := Abs(RY);
  RX2 := Floor(RX); RY2 := Floor(RY);
  FracRX := RX - RX2; FracRY := RY - RY2;

  // Special cases
  if (RX2 <= 1) or (RY2 <= 1) then
    Exit(True); // Nothing to fill, well it's a subpixel box

  RY2 := RY2 * RY2;

  // Initialization
  CurrX := -RX2; CurrY := 0;
  dX := (1 + CurrX * 2) * RY2; dY := RX2 * RX2; err := dX + dY;
  RX2 := dY * 2; RY2 := RY2 * 2;

  // 1st Iteration: Draw first points only once
  if IsZero(FracRY) then
    Result := PointMirrorHFilled(RX - 1, Y, X)
  else
    Result := Self.RectFilled(
      SDLFRect(X - RX + 1, Y - FracRY, RX * 2 - 1, FracRY * 2 + 1));

  // Skip first iteration
  e2 := err * 2;
  if e2 >= dX then
  begin
    Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
  end;
  if e2 <= dY then
  begin
    Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
  end;

  // Main Loop
  while CurrX < 0 do
  begin
    e2 := err * 2;

    if e2 <= dY then
    begin
      // Draw line before changing CurrY AND CurrX only.
      Result := PointMirrorHVFilled(CurrX - FracRX + 1, CurrY + FracRY, 
        True, False, X, Y) and Result;
      Inc(CurrY); Inc(dY, RX2); Inc(err, dY);
    end;

    if e2 >= dX then
    begin
      Inc(CurrX); Inc(dX, RY2); Inc(err, dX);
    end;
  end;
end;
*)

// EllipseInRect[x]

// function cCHXSDL3Renderer.EllipseInRect(const aRect: TSDL_FRect;
//   const BorderC, FillC: TSDL_FColor): Boolean;
// begin
//   Result := Self.EllipseInRect(aRect.X, aRect.Y, aRect.W, aRect.H, BorderC,
//     FillC);
// end;
// 
// function cCHXSDL3Renderer.EllipseInRect(const X, Y, W, H: CFloat;
//   const BorderC, FillC: TSDL_FColor): Boolean;
// var
//   TempColor: TSDL_FColor;
// begin
//   Result := Self.GetDrawColor(TempColor);
// 
//   // Same color for border and fill
//   if BorderC = FillC then
//   begin
//     Result := Result
//       and Self.SetDrawColor(BorderC)
//       and Self.EllipseInRectFilled(X, Y, W, H);
//     // Try to restore previous color anyway
//     Exit(Self.SetDrawColor(TempColor) and Result);
//   end;
// 
//   if FillC.A > 0 then
//   begin
//     Result := Result
//       and Self.SetDrawColor(FillC)
//       and Self.EllipseInRectFillOnly(X, Y, W, H);
//   end;
// 
//   if BorderC.A > 0 then
//   begin
//     Result := Result
//       and Self.SetDrawColor(BorderC)
//       and Self.EllipseInRectBorder(X, Y, W, H);
//   end;
// 
//   // Try to restore previous color anyway
//   Result := Self.SetDrawColor(TempColor) and Result;
// end;
// 
// function cCHXSDL3Renderer.EllipseInRectBorder(const aRect: TSDL_FRect)
//   : Boolean;
// begin
//   Result := Self.EllipseInRectBorder(aRect.X, aRect.Y, aRect.W, aRect.H);
// end;
// 
// function cCHXSDL3Renderer.EllipseInRectBorder(const X, Y, W, H: CFloat)
//   : Boolean;
// { Draw an ellipse inside a rectangle.
// 
//   Modification of Alois Zingl's implementation (https://zingl.github.io)
//   of Bresenham's algorithm:
// 
//   - Special cases.
//   - Avoid redrawing pixels some points.
//   - Precalculate constant terms.
//   - Subpixel adaptation.
// }
// var
//   X0, X1, Y0, Y1, a ,b, aa, bb, b1, dX, dY, err, e2: Integer;
//   FracX, FracY: CFloat;
// begin
// 
// end;
// 
// function cCHXSDL3Renderer.EllipseInRectFilled(const aRect: TSDL_FRect)
//   : Boolean;
// begin
//   Result := Self.EllipseInRectFilled(aRect.X, aRect.Y, aRect.W, aRect.H);
// end;
// 
// function cCHXSDL3Renderer.EllipseInRectFilled(const X, Y, W, H: CFloat)
//   : Boolean;
// begin
// 
// end;
// 
// function cCHXSDL3Renderer.EllipseInRectFillOnly(const aRect: TSDL_FRect)
//   : Boolean;
// begin
//   Result := Self.EllipseInRectFillOnly(aRect.X, aRect.Y, aRect.W, aRect.H);
// end;
// 
// function cCHXSDL3Renderer.EllipseInRectFillOnly(const X, Y, W, H: CFloat)
//   : Boolean;
// begin
// 
// end;

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

// Auxiliar methods for internal use

// PointMirror[x]

function cCHXSDL3Renderer.PointMirrorH(const X, Y, OffsetX: CFloat): Boolean;
var
  PArr: array[0..1] of TSDL_FPoint;
begin
  if IsZero(X) then
    Exit(SDL_RenderPoint(Self.SDLRenderer, OffsetX, Y));
  PArr[0].Init(OffsetX - X, Y); PArr[1].Init(OffsetX + X, Y);
  Result := SDL_RenderPoints(Self.SDLRenderer, @PArr[0], 2);
end;

function cCHXSDL3Renderer.PointMirrorHFilled(const X, Y, OffsetX: CFloat)
  : Boolean;
begin
  if IsZero(X) then
    Exit(SDL_RenderPoint(Self.SDLRenderer, OffsetX, Y));
  Result := Self.Line(OffsetX - X, Y, OffsetX + X, Y);
end;

function cCHXSDL3Renderer.PointMirrorV(const X, Y, OffsetY: CFloat): Boolean;
var
  PArr: array[0..1] of TSDL_FPoint;
begin
  if IsZero(Y) then
    Exit(SDL_RenderPoint(Self.SDLRenderer, X, OffsetY));
  PArr[0].Init(X, OffsetY - Y); PArr[1].Init(X, OffsetY + Y);
  Result := SDL_RenderPoints(Self.SDLRenderer, @PArr[0], 2);
end;

function cCHXSDL3Renderer.PointMirrorVFilled(const X, Y, OffsetY: CFloat)
  : Boolean;
begin
  if IsZero(Y) then
    Exit(SDL_RenderPoint(Self.SDLRenderer, X, OffsetY));
  Result := Self.Line(X, OffsetY - Y, X, OffsetY + Y);
end;

function cCHXSDL3Renderer.PointMirrorHV(const X, Y, OffsetX, OffsetY: CFloat)
  : Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  if IsZero(Y) then
    Exit(Self.PointMirrorH(X, OffSetY, OffSetX));
  if IsZero(X) then
    Exit(Self.PointMirrorV(OffsetX, Y, OffSetY));

  PArr[0].Init(OffsetX - X, OffsetY - Y); // '+
  PArr[1].Init(OffsetX + X, PArr[0].Y);   //  +'
  PArr[2].Init(PArr[0].X, OffsetY + Y);   // .+
  PArr[3].Init(PArr[1].X, PArr[2].Y);     //  +.
  Result := SDL_RenderPoints(Self.SDLRenderer, @PArr[0], 4);
end;

function cCHXSDL3Renderer.PointMirrorHVFilled(const X, Y: CFloat;
  const FillH, FillV: Boolean; const OffsetX, OffsetY: CFloat): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  if IsZero(Y) then
    if FillH then
      Exit(Self.PointMirrorHFilled(X, OffSetY, OffSetX))
    else
      Exit(Self.PointMirrorH(X, OffSetY, OffSetX));
  if IsZero(X) then
    if FillV then
      Exit(Self.PointMirrorVFilled(OffsetX, Y, OffSetY))
    else
      Exit(Self.PointMirrorV(OffsetX, Y, OffSetY));

  // Sorted this way is better
  PArr[0].Init(OffsetX - X, OffsetY - Y); // '+
  PArr[1].Init(OffsetX + X, PArr[0].Y);   //  +'
  PArr[2].Init(PArr[1].X, OffsetY + Y);   //  +.
  PArr[3].Init(PArr[0].X, PArr[2].Y);     // .+

  if FillV then
  begin
    PArr[4] := PArr[0];
    if FillH then // HV
      Result := SDL_RenderLines(Self.SDLRenderer, @PArr[0], 5)
    else // V
      Result := SDL_RenderLines(Self.SDLRenderer, @PArr[1], 2)
        and SDL_RenderLines(Self.SDLRenderer, @PArr[3], 2);
  end
  else
  begin
    if FillH then // H
      Result := SDL_RenderLines(Self.SDLRenderer, @PArr[0], 2)
        and SDL_RenderLines(Self.SDLRenderer, @PArr[2], 2)
    else // No Fill
      Result := SDL_RenderPoints(Self.SDLRenderer, @PArr[0], 4)
  end;
end;

// LineMirror[x]

function cCHXSDL3Renderer.LineMirrorH(const X1, Y1, X2, Y2, OffsetX: CFloat)
  : Boolean;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(Self.PointMirrorH(X1, Y2, OffsetX));

  Result := Self.Line(OffsetX - X1, Y1, OffsetX - X2, Y2)
        and Self.Line(OffsetX + X1, Y1, OffsetX + X2, Y2);
end;

function cCHXSDL3Renderer.LineMirrorV(const X1, Y1, X2, Y2, OffsetY: CFloat)
  : Boolean;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(Self.PointMirrorV(X1, Y2, OffsetY));

  Result := Self.Line(X1, OffsetY - Y1, X2, OffsetY - Y2)
      and Self.Line(X1, OffsetY + Y1, X2, OffsetY + Y2);
end;

function cCHXSDL3Renderer.LineMirrorHV(
  const X1, Y1, X2, Y2, OffsetX, OffsetY: CFloat): Boolean;
var
  P1XL, P1XR, P1YU, P1YD, P2XL, P2XR, P2YU, P2YD: CFloat;
begin
  if SameValue(X1, X2) and SameValue(Y1, Y2) then
    Exit(Self.PointMirrorHV(X1, Y2, OffsetX, OffsetY));

  P1XL := OffsetX - X1; P1YU := OffsetY - Y1;
  P1XR := OffsetX + X1; P1YD := OffsetY + Y1;
  P2XL := OffsetX - X2; P2YU := OffsetY - Y2;
  P2XR := OffsetX + X2; P2YD := OffsetY + Y2;

  Result := Self.Line(P1XL, P1YU, P2XL, P2YU) and Self.Line(P1XR, P1YU, P2XR, P2YU)
        and Self.Line(P1XL, P1YD, P2XL, P2YD) and Self.Line(P1XR, P1YD, P2XR, P2YD);
end;

// RegPolyCCVertices

function cCHXSDL3Renderer.RegPolyCCVertices(out PArr: TSDLFPointDynArray;
  const X, Y, R: CFloat; const NSides: Integer; Angle: CFloat): Boolean;
var
  i: Integer;
  dAng, aSin, aCos: CFloat;
begin
  if NSides < 1 then // `1` doesn't have sense but add a point.
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      NSides]);
    SetLength(PArr, 0); // Sorry, we don't keep previous data.
    Exit(False);
  end;

  SetLength(PArr, NSides);

  dAng := (2.0 * Pi) / NSides;
  for i := 0 to High(PArr) do
  begin
    // Actually must be SinCos(Angle, aSin, aCos) but this way the polygon
    //  has a vertex at the top for better look.
    SinCos(Angle, aCos, aSin);
    PArr[i].Init(aCos * R + X, aSin * R + Y);
    Angle += dAng;
  end;

  Result := True;
end;

// RegPolySSVertices

function cCHXSDL3Renderer.RegPolySSVertices(out PArr: TSDLFPointDynArray;
  const X, Y, SideSize: CFloat; const NSides: Integer; const Angle: CFloat)
  : Boolean;
begin
  if NSides < 1 then // `1` doesn't have sense but add a point.
  begin
    SDL_SetError('%s(%s) %s: NSides(%d) < 1',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      NSides]);
    SetLength(PArr, 0); // Sorry, we don't keep previous data.
    Exit(False);
  end;

  Result := Self.RegPolyCCVertices(PArr, X, Y,
    SideSize * 0.5 * Cosecant(Pi / NSides), NSides, Angle);
end;

// Destroy

destructor cCHXSDL3Renderer.Destroy;
begin
  if FreeRenderer then
    SDL_DestroyRenderer(Self.SDLRenderer);

  inherited;
end;

end.
