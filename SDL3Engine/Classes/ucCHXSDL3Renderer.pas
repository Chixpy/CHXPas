unit ucCHXSDL3Renderer;
(*< Unit of cCHXSDL3Renderer class.

  cCHXSDL3Renderer is an encapsulation of `SDL_Renderer` and expands
  its funcionality.

  This way, instead of using `SDL_[...](PSDL_Renderer, [...])` functions,
  they will become direct methods of the class itself.

  In the context of cCHXSDL3Engine, this class will be created by
  cCHXSDL3Window on its creation.

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
    @itemLabel([/Get])
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
  As side note, SDL2 does the same with converting integer values to float.

  **For colors, SDL3 uses floats** too, while SDL2 uses Byte.

  Additionally, there are the `SDL_RenderGeometry[Raw]` functions used
  to draw filled triangles, which are more advanced as they allow gradient and
  textured rendering. They are likely faster for drawing filled polygons with
  color, although I need to test it.

  ## `SDL_gfx`:

  Initially, this unit was for `cCHXSDL2Engine` and had the purpose of remove
  the dependency of `SDL_gfx`. As this unit is finally being created before
  doing anything with SDL3, so the rant will be in `ucCHXSDL2Renderer` if
  implemented. XD

  Anyways, `SDL_gfx` (and SDL native functions) can be used to draw as
  `PSDL_Renderer` is exposed in SDLRenderer property.

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
    @item(Use PasDoc in the comments as proper documentation.
      Actually PasDocs support some basic MarkDown with a parameter.
      So may be they will be a mix.)
    @item(Drawing lines with native functions and changed logical coordinates
      applies subpixel draw, so they are _smooth_ and don't draw big pixels.
      Maybe, We need use Scale instead LogicalPresentation.)
    @item(Use `SDL_SetError`, except in constructors wich will be Exceptions.
      `{$I %CURRENTROUTINE%}` para el nombre del método.)
  )

  (C) 2026 Chixpy https://github.com/Chixpy
*)
{$mode ObjFPC}{$H+}
{$inline ON}

interface

uses
  SysUtils, CTypes, Math, // FPC RTL
  SDL3, // SDL3
  uCHXSDL3TypeHelpers; // CHXSDL3Engine

resourcestring
  rsCHXSDL3RendererNilError = 'cCHXSDL3Renderer.Create: %s is nil.';

type
  { Wrapper of SDL_Renderer and expanded to draw more primitives.

    It doesn't call `SDL_Init[SubSystem]` or `SDL_Quit[SubSystem]`
    as it expects at least a `SDL_Window` already created. In cCHXSDL3Engine
    context, this class is created by cCHXSDL3Window.

    Near all methods are functions with boolean Result as SDL_Renderer
    funtions are. They return @False on error and `SDL_GetError` can give
    information, cCHXSDL3Renderer ones included. cCHXSDL3Renderer constructors
    throw an Exception instead.
  }
  cCHXSDL3Renderer = class
  protected
    PrevBlendMode: TSDL_BlendMode;
    //< Previous blend mode when changing color.

  public
    SDLRenderer: PSDL_Renderer;
    //< Actual SDL_Renderer.
    FreeRenderer: Boolean;
    //< Free SDL_Renderer on Destroy?;

    { === Constructors === }

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

    { === [Set/Get]DrawColor === }

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

    { === Clear === }

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

    { === Point[s] === }

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

    { === Line[s] === }

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

      @note(Alpha is acumulated in vertices as they are drawed as
        different segments.)

      @param PArr Array of points.
      @param idxFirst First point of the first line.
      @param(Count Number of points used to draw the lines.
        `0` means until the end of the array.)
    }

    { === Triangle[X] === }

    function Triangle(const PArr: TSDLFPointDynArray; const idxFirst: Integer;
      const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled triangle with border.

      With idxFirst, three contiguous points can be used from a bigger array.

      Previous draw color is restored.

      @warning(This is a "fast draw" composition algorithm, Vertices will
        acummulate border opacity. In this particular case border itself will
        acumulate their opacity with fill color too.)

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Triangle(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean;
    {< Draw a filled triangle with border.

      Use the first 3 points of the Array.

      Previous draw color is restored.

      @warning(This is a "fast draw" composition algorithm, Vertices will
        acummulate border opacity. In this particular case border itself will
        acumulate their opacity with fill color too.)

      @param PArr Array of points.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Triangle(const P1, P2, P3: TSDL_FPoint;
     const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled triangle with border.

      Previous draw color is restored.

      @warning(This is a "fast draw" composition algorithm, Vertices will
        acummulate border opacity. In this particular case border itself will
        acumulate their opacity with fill color too.)

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

      @warning(This is a "fast draw" algorithm, Vertices will acummulate
        border opacity. In this particular case border itself will acumulate
        their opacity with fill color too.)

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

      @warning(Alpha is acumulated in vertices as they are drawed as
        different segments.)

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
    }
    function TriangleBorder(const P1, P2, P3: TSDL_FPoint): Boolean; overload;
    {< Draw only the border of a triangle.

      @warning(Alpha is acumulated in vertices as they are drawed as
        different segments.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
    }
    function TriangleBorder(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
       overload;
    {< Draw only the border of a triangle.

      @warning(Alpha is acumulated in vertices as they are drawed as
        different segments.)

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

      @warning(By now, actually draws border too.)

      @param PArr Array of points.
      @param idxFirst First point of the triangle.
    }
    function TriangleFillOnly(const P1, P2, P3: TSDL_FPoint): Boolean; 
      overload; inline;
    {< Teorically, this would draw a filled triangle without border.

      @warning(By now, actually draws border too.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
    }
    function TriangleFillOnly(const X1, Y1, X2, Y2, X3, Y3: CFloat): Boolean;
       overload; inline;
    {< Teorically, this would draw a filled triangle without border.

      @warning(By now, actually draws border too.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
    }

    { === Rect[X]: Axis Aligned Rectangle === }

    function Rect(const aRect: TSDL_FRect; const BorderC,
      FillC: TSDL_FColor): Boolean;
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

    { === Quad[X]: Quadrilateral === }

    function Quad(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
      overload;
    {< Draw a filled quadritateral with border.

      With idxFirst, four contiguous points can be used from a bigger array.

      Previous draw color is restored.

      @warning(This is a "fast draw" composition algorithm, Vertices will
        acummulate border opacity. In this particular case border itself will
        acumulate their opacity with fill color too.)

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Quad(const PArr: TSDLFPointDynArray;
      const BorderC, FillC: TSDL_FColor): Boolean;
    {< Draw a filled quadritateral with border.

      Use the first 4 points of the Array.

      Previous draw color is restored.

      @warning(This is a "fast draw" composition algorithm, Vertices will
        acummulate border opacity. In this particular case border itself will
        acumulate their opacity with fill color too.)

      @param PArr Array of points.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }
    function Quad(const P1, P2, P3, P4: TSDL_FPoint;
     const BorderC, FillC: TSDL_FColor): Boolean; overload;
    {< Draw a filled quadritateral with border.

      Previous draw color is restored.

      @warning(This is a "fast draw" composition algorithm, Vertices will
        acummulate border opacity. In this particular case border itself will
        acumulate their opacity with fill color too.)

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

      @warning(This is a "fast draw" algorithm, Vertices will acummulate
        border opacity. In this particular case border itself will acumulate
        their opacity with fill color too.)

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

      @warning(Alpha is acumulated in vertices as they are drawed as
        different segments.)

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
    }
    function QuadBorder(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
      overload;
    {< Draw only the border of a quadritateral.

      @warning(Alpha is acumulated in vertices as they are drawed as
      different segments.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param P3 Fourth point.
    }
    function QuadBorder(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat)
      : Boolean; overload;
    {< Draw only the border of a quadritateral.

      @warning(Alpha is acumulated in vertices as they are drawed as
      different segments.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
    }

    function QuadFilled(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0): Boolean; overload;
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

      @warning(In this particular primitive, actually draws border too.)

      @param PArr Array of points.
      @param idxFirst First point of the quadritateral.
    }
    function QuadFillOnly(const P1, P2, P3, P4: TSDL_FPoint): Boolean; 
      overload; inline;
    {< Teorically, this would draw a filled quadritateral without border.

      @warning(In this particular primitive, actually draws border too.)

      @param P1 First point.
      @param P2 Second point.
      @param P3 Third point.
      @param P4 Fourth point.
    }
    function QuadFillOnly(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat):
      Boolean; overload; inline;
    {< Teorically, this would draw a filled quadritateral without border.

      @warning(In this particular primitive, actually draws border too.)

      @param X1 Horizontal coordinate of the first point.
      @param Y1 Vertical coordinate of the first point.
      @param X2 Horizontal coordinate of the second point.
      @param Y2 Vertical coordinate of the second point.
      @param X3 Horizontal coordinate of the third point.
      @param Y3 Vertical coordinate of the third point.
      @param X4 Horizontal coordinate of the fourth point.
      @param Y4 Vertical coordinate of the fourth point.
    }

    { === Polygon[X] === }

    function Polygon(const PArr: TSDLFPointDynArray; const idxFirst: Integer;
      Count: Integer; const BorderC, FillC: TSDL_FColor) : Boolean; overload;
    {< Draw a filled polygon with border.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      Previous draw color is restored.

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

      @param PArr Array of points.
      @param BorderC Color of the border.
      @param FillC Color for fill.
    }

    function PolygonBorder(const PArr: TSDLFPointDynArray;
      const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
    {< Draw only the border of a polygon.

      With idxFirst and Count can select wich points will be used from a
      bigger array.

      @warning(Alpha is acumulated in vertices as they are drawed as
        different segments.)

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

    { === RegPolyCC[X]: Regular Polygon with Circumscribed Circle === }

    function RegPolyCC(const X, Y, R: CFloat; const NSides: Integer;
      const BorderC, FillC: TSDL_FColor; const Angle: CFloat = 0): Boolean;
    {< Draw a Regular Polygon with `NSides` defined by it's circumscribed
      circunference and rotated an `Angle` filled and with border.

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

    { === RegPolySS[X]: Regular Polygon with Side Length === }

    function RegPolySS(const X, Y, SideSize: CFloat; const NSides: Integer;
      const BorderC, FillC: TSDL_FColor; const Angle: CFloat = 0): Boolean;
      inline;
      
    function RegPolySSBorder(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;
      
    function RegPolySSFilled(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;

    function RegPolySSFillOnly(const X, Y, SideSize: CFloat;
      const NSides: Integer; const Angle: CFloat = 0): Boolean; inline;
      

    { === Circle[X] === }


    function CircleBorder(const X, Y, R: CFloat): Boolean;
    {< Draw a filled Regular Polygon without border with `NSides` defined by
      it's circumscribed circunference and rotated an `Angle`.

      @param(X Horizontal position of the enter of the polygon and it's 
        circumscribed circunference.)
      @param(Y Vertical position of the center of the polygon and it's 
        circumscribed circunference.)
      @param(R Radius of circumscribed circunference.)
      @param(A Rotation angle of the polygon. `0` first vertex on the right.)
    }


    // Auxiliar methods for internal use:
    //   Implemented as needed with specific parámeters, but keep public as
    //     they can be useful.

    { === PointMirror[X] === }

    function PointMirrorH(const X, Y: CFloat; const OffsetX: CFloat = 0)
      : Boolean;
    {< Draw a point and its horizontal reflection relative to X=0, and then 
      shifted by OffsetX.

      Do not confuse with reflection directly around OffsetX.

      Intended as an internal helper for complex primitive generation.
    }

    function PointMirrorHFilled(const X, Y: CFloat; const OffsetX: CFloat = 0)
      : Boolean; inline;
    {< Draw line between the point and its horizontal reflection relative
      to X=0, and then shifted by OffsetX.

      Do not confuse with reflection directly around OffsetX.

      Intended as an internal helper for complex primitive generation.
    }

    function PointMirrorV(const X, Y: CFloat; const OffsetY: CFloat = 0)
      : Boolean;
    {< Draw a point and its vertical reflection relative to Y=0, and then 
      shifted by OffsetY.

      Do not confuse with reflection directly around OffsetY.

      Intended as an internal helper for complex primitive generation.
    }

    function PointMirrorVFilled(const X, Y: CFloat; const OffsetY: CFloat = 0)
      : Boolean; inline;
    {< Draw line between the point and its vertical reflection relative
      to Y=0, and then shifted by OffsetY.

      Do not confuse with reflection directly around OffsetY.

      Intended as an internal helper for complex primitive generation.
    }

    function PointMirrorHV(const X, Y: CFloat; const OffsetX: CFloat = 0;
      const OffsetY: CFloat = 0): Boolean; inline;
    {< Draw a point and its horizontal and vertical reflections relative to
      X=0 and Y=0, and then shifted by OffsetX and OffsetY.

      Do not confuse with reflection directly around OffsetX and OffsetY.

      Intended as an internal helper for complex primitive generation.
    }

    { === LineMirror[X] === }

    function LineMirrorH(const X1, Y1, X2, Y2, OffsetX: CFloat): Boolean;
      inline;
    function LineMirrorV(const X1, Y1, X2, Y2, OffsetY: CFloat): Boolean;
      inline;
    function LineMirrorHV(const X1, Y1, X2, Y2, OffsetX, OffsetY: CFloat)
      : Boolean;

    { === RegPoly[CC/SS]Vertices === }

    function RegPolyCCVertices(var PArr: TSDLFPointDynArray;
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

    function RegPolySSVertices(var PArr: TSDLFPointDynArray;
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

    { === Destroy === }

    destructor Destroy; override;
    {< Destructor of cCHXSDL3Renderer.

      if FreeRenderer is @True, destroys SDL_Renderer too.
    }
  end;

implementation

{ cCHXSDL3Renderer }

// Create

constructor cCHXSDL3Renderer.Create(const PSDLWindow: PSDL_Window;
  const Drivers: PAnsiChar);
begin
  if not assigned(PSDLWindow) then
  begin
    SDL_SetError(PChar(rsCHXSDL3RendererNilError), ['PSDLWindow']);
    raise Exception.CreateFmt(rsCHXSDL3RendererNilError, ['PSDLWindow']);
  end;

  Create(SDL_CreateRenderer(PSDLWindow, Drivers), True);
end;

constructor cCHXSDL3Renderer.Create(const PSDLRenderer: PSDL_Renderer;
  const FreeOnDestroy: Boolean);
begin
  if not assigned(PSDLRenderer) then
  begin
    SDL_SetError(PChar(rsCHXSDL3RendererNilError), ['PSDLRenderer']);
    raise Exception.CreateFmt(rsCHXSDL3RendererNilError, ['PSDLRenderer']);
  end;

  inherited Create;
  SDLRenderer := PSDLRenderer;
  FreeRenderer := FreeOnDestroy;

  // Setting initial BlendMode
  PrevBlendMode := SDL_BLENDMODE_BLEND;
  SDL_SetRenderDrawBlendMode(SDLRenderer, SDL_BLENDMODE_BLEND)
end;

// SetDrawColor

function cCHXSDL3Renderer.SetDrawColor(const aColor: TSDL_FColor): Boolean;
begin
  Result := SetDrawColor(aColor.R, aColor.G, aColor.B, aColor.A);
end;

function cCHXSDL3Renderer.SetDrawColor(const R, G, B, A: CFloat): Boolean;
var
  PrevR, PrevG, PrevB, PrevA: CFloat;
begin
  Result := SDL_GetRenderDrawColorFloat(SDLRenderer,
    @PrevR, @PrevG, @PrevB, @PrevA);

  if SameValue(PrevR, R) and SameValue(PrevG, G) and SameValue(PrevB, B)
    and SameValue(PrevA, A) then
    Exit;

  if (PrevA >= 1) and (A < 1) then
    Result :=  SDL_SetRenderDrawBlendMode(SDLRenderer, PrevBlendMode)
      and Result
  else if (PrevA < 1) and (A >= 1) then
  begin
    Result := SDL_GetRenderDrawBlendMode(SDLRenderer, @PrevBlendMode)
      and Result;
    Result := SDL_SetRenderDrawBlendMode(SDLRenderer, SDL_BLENDMODE_NONE)
      and Result;
  end;

  Result := SDL_SetRenderDrawColorFloat(SDLRenderer, R, G, B, A)
    and Result;
end;

function cCHXSDL3Renderer.SetDrawColor(const Grey, A: CFloat): Boolean;
begin
  Result := SetDrawColor(Grey, Grey, Grey, A);
end;

// GetDrawColor

function cCHXSDL3Renderer.GetDrawColor: TSDL_FColor;
begin
  SDL_GetRenderDrawColorFloat(SDLRenderer, @Result.R, @Result.G, @Result.B,
    @Result.A);
end;

function cCHXSDL3Renderer.GetDrawColor(var aColor: TSDL_FColor): Boolean;
begin
  Result := SDL_GetRenderDrawColorFloat(SDLRenderer, @aColor.R, @aColor.G,
    @aColor.B, @aColor.A);
end;

function cCHXSDL3Renderer.GetDrawColor(var R, G, B, A: CFloat): Boolean;
begin
  Result := SDL_GetRenderDrawColorFloat(SDLRenderer, @R, @G, @B, @A);
end;

// Clear

function cCHXSDL3Renderer.Clear: Boolean;
begin
  Result := SDL_RenderClear(SDLRenderer);
end;

function cCHXSDL3Renderer.Clear(const aColor: TSDL_FColor): Boolean;
begin
  Result := Self.Clear(aColor.R, aColor.G, aColor.B, aColor.A);
end;

function cCHXSDL3Renderer.Clear(const R, G, B, A: CFloat): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);
  Result := SetDrawColor(R, G, B, A) and Result;
  Result := SDL_RenderClear(SDLRenderer) and Result;
  Result := SetDrawColor(TempColor) and Result;
end;

// Point

function cCHXSDL3Renderer.Point(const P: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderPoints(SDLRenderer, @P, 1);
end;

function cCHXSDL3Renderer.Point(const X, Y: CFloat): Boolean;
begin
  Result := SDL_RenderPoint(SDLRenderer, X, Y);
end;

// Points

function cCHXSDL3Renderer.Points(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
var
  MaxCount: Integer;
begin
  { Notes about SDL_RenderPoints:

    - It doesn't draw anything with Count <= 0. No error with negatives.
    - If Count exceeds array end, it doesn't care and draw points with
        "invalid" data (usually 0,0). No error, but this time is logical.
  }
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint', 
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) or (Count < 0) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) not in [0..%d]) or (Count(%d) < 0)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr), Count]);
    Exit(False);
  end;

  MaxCount := Length(PArr) - idxFirst;
  Result := True;

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Count, Length(PArr)]);
    Count := MaxCount;
    Result := False; // Draw but return an error
  end;

  //if Count > 0 then // Always True here
  Result := SDL_RenderPoints(SDLRenderer, @PArr[idxFirst], Count)
    and Result;
end;

// Line

function cCHXSDL3Renderer.Line(const P1, P2: TSDL_FPoint): Boolean;
begin
  Result := SDL_RenderLine(SDLRenderer, P1.X, P1.Y, P2.X, P2.Y);
end;

function cCHXSDL3Renderer.Line(const X1, Y1, X2, Y2: CFloat): Boolean;
begin
  Result := SDL_RenderLine(SDLRenderer, X1, Y1, X2, Y2);
end;

// Lines

function cCHXSDL3Renderer.Lines(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
var
  MaxCount: Integer;
begin
  { Notes about SDL_RenderLines:

    - It doesn't draw anything with Count <= 0. No error with negatives.
    - If Count exceeds array end, it doesn't care and draw lines with
        "invalid" data (usually 0,0). No error, but this time is logical.
  }
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint', 
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) or (Count < 0) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) not in [0..%d]) or (Count(%d) < 0)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr), Count]);
    Exit(False);
  end;

  MaxCount := Length(PArr) - idxFirst;
  Result := True;

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Count, Length(PArr)]);
    Count := MaxCount;
    Result := False; // Draw but return an error
  end;

  if Count > 1 then
    Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], Count) and Result
  else
    Result := Point(PArr[idxFirst]) and Result;
end;

// Triangle

function cCHXSDL3Renderer.Triangle(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  if (idxFirst + 3) > Length(PArr) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + 3) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Length(PArr)]);
    Exit(False);
  end;

  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := TriangleFilled(PArr, idxFirst) and Result;
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := SetDrawColor(FillC) and Result;
    Result := TriangleFillOnly(PArr, idxFirst) and Result;
  end;

  if BorderC.A > 0 then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := TriangleBorder(PArr, idxFirst) and Result;
  end;

  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.Triangle(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Triangle(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const P1, P2, P3: TSDL_FPoint;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  Result := Triangle(PArr, BorderC, FillC);
end;

function cCHXSDL3Renderer.Triangle(const X1, Y1, X2, Y2, X3, Y3: CFloat;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  Result := Triangle(PArr, BorderC, FillC);
end;

// TriangleBorder

function cCHXSDL3Renderer.TriangleBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  if (idxFirst + 3) > Length(PArr) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + 3) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Length(PArr)]);
    Exit(False);
  end;

  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], 3);
  Result := Line(PArr[idxFirst + 2], PArr[idxFirst]) and Result;
end;

function cCHXSDL3Renderer.TriangleBorder(const P1, P2, P3: TSDL_FPoint): Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P1;
  // Result := TriangleBorder(PArr); And remove PArr[3]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 4);
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
  // Result := TriangleBorder(PArr); And remove PArr[3]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 4);
end;

// TriangleFilled

function cCHXSDL3Renderer.TriangleFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  FColor: TSDL_FColor;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  if (idxFirst + 3) > Length(PArr) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + 3) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Length(PArr)]);
    Exit(False);
  end;

  Result := GetDrawColor(FColor);
  Result := SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[idxFirst],
    SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0)
    and Result;
end;

function cCHXSDL3Renderer.TriangleFilled(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
  FColor: TSDL_FColor;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3;
  //Result := TriangleFilled(PArr);
  Result := GetDrawColor(FColor);
  Result := SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[0],
    SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0)
    and Result;
end;

function cCHXSDL3Renderer.TriangleFilled(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
var
  PArr: Array[0..2] of TSDL_FPoint;
  FColor: TSDL_FColor;
begin
  PArr[0].X := X1; PArr[0].Y := Y1;
  PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3;
  // Result := TriangleFilled(PArr);
  Result := GetDrawColor(FColor);
  Result := SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[0],
    SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0)
    and Result;
end;

// TriangleFillOnly.

function cCHXSDL3Renderer.TriangleFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer = 0): Boolean;
begin
  Result := TriangleFilled(PArr, idxFirst);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const P1, P2, P3: TSDL_FPoint)
  : Boolean;
begin
  Result := TriangleFilled(P1, P2, P3);
end;

function cCHXSDL3Renderer.TriangleFillOnly(const X1, Y1, X2, Y2, X3, Y3: CFloat)
  : Boolean;
begin
  Result := TriangleFilled(X1, Y1, X2, Y2, X3, Y3);
end;

// Rectangle

function cCHXSDL3Renderer.Rect(const aRect: TSDL_FRect; const BorderC,
  FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := RectFilled(aRect) and Result;
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := SetDrawColor(FillC) and Result;
    Result := RectFillOnly(aRect) and Result;
  end;

  if BorderC.A > 0 then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := RectBorder(aRect) and Result;
  end;

  Result := SetDrawColor(TempColor) and Result;
end;

// RectBorder

function cCHXSDL3Renderer.RectBorder(const aRect: TSDL_FRect): Boolean;
begin
  Result := SDL_RenderRect(SDLRenderer, @aRect);
end;

// RectFilled

function cCHXSDL3Renderer.RectFilled(const aRect: TSDL_FRect): Boolean;
begin
  Result := SDL_RenderFillRect(SDLRenderer, @aRect);
end;

// RectFillOnly

function cCHXSDL3Renderer.RectFillOnly(aRect: TSDL_FRect): Boolean;
begin
  if (aRect.W <= 2) or (aRect.H <= 2) then 
    Exit(False); // ¿False?

  aRect.Shrink(1);
  Result := SDL_RenderFillRect(SDLRenderer, @aRect);
end;

// Quad(rilateral)

function cCHXSDL3Renderer.Quad(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; const BorderC, FillC: TSDL_FColor): Boolean;
var
  TempColor: TSDL_FColor;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  if (idxFirst + 4) > Length(PArr) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + 4) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Length(PArr)]);
    Exit(False);
  end;

  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := QuadFilled(PArr, idxFirst) and Result;
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := SetDrawColor(FillC) and Result;
    Result := QuadFillOnly(PArr, idxFirst) and Result;
  end;

  if BorderC.A > 0 then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := QuadBorder(PArr, idxFirst) and Result;
  end;

  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.Quad(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Quad(PArr, 0, BorderC, FillC);
end;

function cCHXSDL3Renderer.Quad(const P1, P2, P3, P4: TSDL_FPoint;
  const BorderC, FillC: TSDL_FColor): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  Result := Quad(PArr, BorderC, FillC);
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
  Result := Quad(PArr, BorderC, FillC);
end;

// QuadBorder

function cCHXSDL3Renderer.QuadBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  if (idxFirst + 4) > Length(PArr) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + 4) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Length(PArr)]);
    Exit(False);
  end;

  Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], 4);
  Result := Line(PArr[idxFirst + 3], PArr[idxFirst]) and Result;
end;

function cCHXSDL3Renderer.QuadBorder(const P1, P2, P3, P4: TSDL_FPoint): Boolean;
var
  PArr: array[0..4] of TSDL_FPoint;
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4; PArr[4] := P1;
  // Result := QuadBorder(PArr); And remove PArr[4]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 5);
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
  // Result := QuadBorder(PArr); And remove PArr[4]
  Result := SDL_RenderLines(SDLRenderer, @PArr[0], 5);
end;

// QuadFilled

function cCHXSDL3Renderer.QuadFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer): Boolean;
var
  FColor: TSDL_FColor;
  Indices: array[0..5] of Integer;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  if (idxFirst + 4) > Length(PArr) then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + 4) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Length(PArr)]);
    Exit(False);
  end;

  // Índices para usar los vertices como triángulos.
  Indices[0] := idxFirst; Indices[1] := idxFirst + 1; Indices[2] := idxFirst + 2;
  Indices[3] := idxFirst; Indices[4] := idxFirst + 2; Indices[5] := idxFirst + 3;

  Result := GetDrawColor(FColor);
  Result := SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[idxFirst],
    SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 4, @Indices[0], 6,
    SizeOf(Integer));
end;

function cCHXSDL3Renderer.QuadFilled(const P1, P2, P3, P4: TSDL_FPoint)
  : Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
  FColor: TSDL_FColor;
  Indices: array[0..5] of Integer = (0, 1, 2, 0, 2, 3);
begin
  PArr[0] := P1; PArr[1] := P2; PArr[2] := P3; PArr[3] := P4;
  //Result := QuadFilled(PArr);
  Result := GetDrawColor(FColor);
  Result := SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[0],
    SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 4, @Indices[0], 6, 
    SizeOf(Integer));
end;

function cCHXSDL3Renderer.QuadFilled(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
var
  PArr: Array[0..3] of TSDL_FPoint;
  FColor: TSDL_FColor;
  Indices: array[0..5] of Integer = (0, 1, 2, 0, 2, 3);
begin
  PArr[0].X := X1; PArr[0].Y := Y1; PArr[1].X := X2; PArr[1].Y := Y2;
  PArr[2].X := X3; PArr[2].Y := Y3; PArr[3].X := X4; PArr[3].Y := Y4;
  // Result := QuadFilled(PArr);
  Result := GetDrawColor(FColor);
  Result := SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[0],
    SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 4, @Indices[0], 6, 
    SizeOf(Integer));
end;

// QuadFillOnly.

function cCHXSDL3Renderer.QuadFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer = 0): Boolean;
begin
  Result := QuadFilled(PArr, idxFirst);
end;

function cCHXSDL3Renderer.QuadFillOnly(const P1, P2, P3, P4: TSDL_FPoint)
  : Boolean;
begin
  Result := QuadFilled(P1, P2, P3, P4);
end;

function cCHXSDL3Renderer.QuadFillOnly(
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4: CFloat): Boolean;
begin
  Result := QuadFilled(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

// Polygon.

function cCHXSDL3Renderer.Polygon(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer; const BorderC, FillC: TSDL_FColor)
  : Boolean; overload;
var
  TempColor: TSDL_FColor;
  MaxCount: Integer;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  MaxCount := Length(PArr) - idxFirst;
  Result := True;

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Count, Length(PArr)]);
    Exit(False); // ToDo: Or try to draw...
  end;

  Result := GetDrawColor(TempColor);

  // Same color for border and fill
  if BorderC = FillC then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := PolygonFilled(PArr, idxFirst, Count) and Result;
    Exit(SetDrawColor(TempColor) and Result);
  end;

  if FillC.A > 0 then
  begin
    Result := SetDrawColor(FillC) and Result;
    Result := PolygonFillOnly(PArr, idxFirst, Count) and Result;
  end;

  if BorderC.A > 0 then
  begin
    Result := SetDrawColor(BorderC) and Result;
    Result := PolygonBorder(PArr, idxFirst, Count) and Result;
  end;

  Result := SetDrawColor(TempColor) and Result;
end;

function cCHXSDL3Renderer.Polygon(const PArr: TSDLFPointDynArray;
  const BorderC, FillC: TSDL_FColor): Boolean;
begin
  Result := Polygon(PArr, 0, 0, BorderC, FillC);
end;

// PolygonBorder

function cCHXSDL3Renderer.PolygonBorder(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
var
  MaxCount: Integer;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  MaxCount := Length(PArr) - idxFirst;
  // Result := True; Not needed here

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Count, Length(PArr)]);
    Count := MaxCount; // Try to draw something
    Exit(False);
  end;

  case Count of
    // Never happens here 0: Result := True; // Return False?
    1: Result := Point(PArr[idxFirst]);
    2: Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], 2); 
  otherwise
    begin
      // Result := Lines(PArr[idxFirst], Count) and...
      Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], Count)
        and Line(PArr[idxFirst + Count - 1], PArr[idxFirst]);
    end;
  end;
end;

// PolygonFilled

function cCHXSDL3Renderer.PolygonFilled(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
var
  MaxCount: Integer;
  FColor: TSDL_FColor;
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  MaxCount := Length(PArr) - idxFirst;
  // Result := True; Not needed here

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Count, Length(PArr)]);
    Exit(False); // ToDo: Or try to draw...
  end;

  case Count of
    // Never happens here 0: Result := True;
    1: Result := Point(PArr[idxFirst]);
    2: Result := SDL_RenderLines(SDLRenderer, @PArr[idxFirst], 2);
    3:
    begin
      // Result := TriangleFilled(PArr, idxFirst);
      Result := GetDrawColor(FColor)
       and SDL_RenderGeometryRaw(SDLRenderer, nil, @PArr[idxFirst],
        SizeOf(TSDL_FPoint), @FColor, 0, nil, 0, 3, nil, 0, 0);
    end;
  otherwise
    // Ideally it must have its own algorithm...
    Result := PolygonFillOnly(PArr, idxFirst, Count)
      and PolygonBorder(PArr, idxFirst, Count);
  end;

end;

// PolygonFillOnly

function cCHXSDL3Renderer.PolygonFillOnly(const PArr: TSDLFPointDynArray;
  const idxFirst: Integer; Count: Integer): Boolean;
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

  ToDo: Optimize for X < 0 and Y < 0, not sure if (0,0) in SDL can be moved... 
    but it will be a huge feature pushing and popping coordinate systems.
}

var
  MinX, MaxX, MinY, MaxY, CurrY, X1, X2, xIntersect: CFloat;
  CurrSeg, aIndex: Integer;
  P1, P2: TSDL_FPoint;
  SegList: Array of TCHXSDLFSegment;
  XList: Array of CFloat; // Intersection values
begin
  if Length(PArr) <= 0 then
  begin
    SDL_SetError('%s(%s) %s: Empty array of TSDL_FPoint',
    [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%}]);
    Exit(False);
  end;

  if (not idxFirst in [0..High(PArr)]) then
  begin
    SDL_SetError('%s(%s) %s: Index(%d) not in [0..%d]',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, High(PArr)]);
    Exit(False);
  end;

  aIndex := Length(PArr) - idxFirst; // aIndex is MaxCount in other methods

  if Count = 0 then
    Count := aIndex
  else if Count > aIndex then
  begin
    SDL_SetError('%s(%s) %s: (Index(%d) + Count(%d)) > Array Length(%d)',
      [{$I %FILE%}, {$I %LINE%}, {$I %CURRENTROUTINE%},
      idxFirst, Count, Length(PArr)]);
    Exit(False); // ToDo: Or try to draw...
  end;

  if Count <= 2 then
    Exit(True) // Point or line, nothing to fill
  else if Count = 3 then
    Exit(TriangleFillOnly(PArr, idxFirst)); // This will be more eficient

  // OK, it's time to work

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
    // ToDo: Test (Abs(P1.Y, P2.Y) < 0.5)?
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
    SetLength(XList, 0); // Removing previous segments

    // Backwards because we delete not useful segments
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

    Result := not Odd(Length(XList));
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

      if X1 <= X2 then
        Result := Line(X1, CurrY, X2, CurrY) and Result;

      Inc(aIndex, 2);
    end;

    CurrY += 1;
  end;
end;

// RegPolyCC

function cCHXSDL3Renderer.RegPolyCC(const X, Y, R: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and Polygon(PArr, BorderC, FillC);
end;

// RegPolyCCBorder

function cCHXSDL3Renderer.RegPolyCCBorder(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonBorder(PArr);
end;

// RegPolyCCFilled

function cCHXSDL3Renderer.RegPolyCCFilled(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonFilled(PArr);
end;

// RegPolyCCFillOnly

function cCHXSDL3Renderer.RegPolyCCFillOnly(const X, Y, R: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolyCCVertices(PArr, X, Y, R, NSides, Angle)
    and PolygonFillOnly(PArr);
end;

// RegPolySS

function cCHXSDL3Renderer.RegPolySS(const X, Y, SideSize: CFloat;
  const NSides: Integer; const BorderC, FillC: TSDL_FColor;
  const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle) and
            Polygon(PArr, BorderC, FillC);
end;

// RegPolySSBorder

function cCHXSDL3Renderer.RegPolySSBorder(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle) and
            PolygonBorder(PArr);
end;

// RegPolySSFilled

function cCHXSDL3Renderer.RegPolySSFilled(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle) and
            PolygonFilled(PArr);
end;

// RegPolySSFillOnly

function cCHXSDL3Renderer.RegPolySSFillOnly(const X, Y, SideSize: CFloat;
  const NSides: Integer; const Angle: CFloat): Boolean;
var
  PArr: TSDLFPointDynArray;
begin
  Result := RegPolySSVertices(PArr, X, Y, SideSize, NSides, Angle) and 
            PolygonFillOnly(PArr);
end;

// CircleBorder

function cCHXSDL3Renderer.CircleBorder(const X, Y, R: CFloat): Boolean;
{ Uses Jesko's method for circle rasterization with some modifications:
  - Avoid redrawing pixels at cardinal/diagonal angles.
  - Minor initialization optimization.
  - Subpixel adaptation. }
var
  t1, t2, CurrX, CurrY: Integer;
  FracR: CFloat; // Subpixel Radius Offset.
begin
  CurrX := Floor(R);
  FracR := R - CurrX;
  CurrX := Abs(CurrX);
  if CurrX < 1 then
    Exit(Point(X, Y));

  // 1st iteration unrolled:
  //   Draw cardinal angles (0°, 90°, 180°, and 270°) only once.
  Result := LineMirrorHV(-FracR, R, FracR, R, X, Y);

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
    Result := PointMirrorHV(CurrX, CurrY, X, Y) and Result;
end;

// Auxiliar methods for internal use

// PointMirror[x]

function cCHXSDL3Renderer.PointMirrorH(const X, Y, OffsetX: CFloat): Boolean;
var
  PArr: array[0..1] of TSDL_FPoint;
begin
  PArr[0].Init(OffsetX - X, Y); PArr[1].Init(OffsetX + X, Y);
  Result := SDL_RenderPoints(SDLRenderer, @PArr[0], 2);
end;

function cCHXSDL3Renderer.PointMirrorHFilled(const X, Y, OffsetX: CFloat)
  : Boolean;
begin
  Result := Line(OffsetX - X, Y, OffsetX + X, Y);
end;

function cCHXSDL3Renderer.PointMirrorV(const X, Y, OffsetY: CFloat): Boolean;
var
  PArr: array[0..1] of TSDL_FPoint;
begin
  PArr[0].Init(X, OffsetY - Y); PArr[1].Init(X, OffsetY + Y);
  Result := SDL_RenderPoints(SDLRenderer, @PArr[0], 2);
end;

function cCHXSDL3Renderer.PointMirrorVFilled(const X, Y, OffsetY: CFloat)
  : Boolean;
begin
  Result := Line(X, OffsetY - Y, X, OffsetY + Y);
end;

function cCHXSDL3Renderer.PointMirrorHV(const X, Y, OffsetX, OffsetY: CFloat)
  : Boolean;
var
  PArr: array[0..3] of TSDL_FPoint;
begin
  PArr[0].Init(OffsetX - X, OffsetY - Y);
  PArr[1].Init(OffsetX + X, OffsetY + Y);
  PArr[2].Init(OffsetX - Y, OffsetY + X);
  PArr[3].Init(OffsetX + Y, OffsetY - X);
  Result := SDL_RenderPoints(SDLRenderer, @PArr[0], 4);
end;

// LineMirror[x]

function cCHXSDL3Renderer.LineMirrorH(const X1, Y1, X2, Y2, OffsetX: CFloat)
  : Boolean;
begin
  Result := Line(OffsetX + X1, Y1, OffsetX + X2, Y2)
        and Line(OffsetX - X1, Y1, OffsetX - X2, Y2);
end;

function cCHXSDL3Renderer.LineMirrorV(const X1, Y1, X2, Y2, OffsetY: CFloat)
  : Boolean;
begin
  Result := Line(X1, OffsetY + Y1, X2, OffsetY + Y2)
        and Line(X1, OffsetY - Y1, X2, OffsetY - Y2);
end;

function cCHXSDL3Renderer.LineMirrorHV(
  const X1, Y1, X2, Y2, OffsetX, OffsetY: CFloat): Boolean;
begin
  Result := Line(OffsetX - X1, OffsetY - Y1, OffsetX - X2, OffsetY - Y2)
        and Line(OffsetX + X1, OffsetY + Y1, OffsetX + X2, OffsetY + Y2)
        and Line(OffsetX - Y1, OffsetY + X1, OffsetX - Y2, OffsetY + X2)
        and Line(OffsetX + Y1, OffsetY - X1, OffsetX + Y2, OffsetY - X2)
    ;
end;

// RegPolyCCVertices

function cCHXSDL3Renderer.RegPolyCCVertices(var PArr: TSDLFPointDynArray;
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

function cCHXSDL3Renderer.RegPolySSVertices(var PArr: TSDLFPointDynArray;
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

  Result := RegPolyCCVertices(PArr, X, Y, SideSize * 0.5 * Cosecant(Pi / NSides),
    NSides, Angle);
end;




// Destroy

destructor cCHXSDL3Renderer.Destroy;
begin
  if FreeRenderer then
    SDL_DestroyRenderer(SDLRenderer);

  inherited;
end;

end.
