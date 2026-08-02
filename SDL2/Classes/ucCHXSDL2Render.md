# To be implemented

This file has the hand notes I had to create a `cCHXSDL2Renderer` and integrate
it to `cCHXSDL2Engine`. But, actually, it was implemented for `SDL3Engine`.

May be some day in will backported to SDL2.

## About creating the class

Class to encapsulate SDL_Renderer and refactor unpublished CHXSDL2GFX,
which was a reimplementation and extension of the primitive drawing 
functionality from the SDL2_GFX library.

This way, instead of using `CHXSDL2[...](PSDL_Renderer, [...]);` functions, 
they will become direct methods of the class itself.

In the context of cCHXSDL2Engine, this class would be created by 
cCHXSDL2Window when returning its associated renderer.

## About SDL_Renderer

SDL2 only provides the following basic functions for drawing:

- `SDL_SetRenderDrawColor` (Get)
- `SDL_SetRenderDrawBlendMode` (Get)
- `SDL_RenderDrawPoint` (s) (F)
- `SDL_RenderDrawLine` (s) (F)
- `SDL_RenderDrawRect` (s) (F)
- `SDL_RenderFillRect` (s) (F)
- `SDL_RenderClear`

Where:

- _(Get)_: Ability to retrieve the current color or blend mode.
- _(s)_: Variant to draw multiple points, lines, or rectangles stored in an 
  array of TSDL_Point or TSDL_Rect.
- _(F)_: Variant where coordinates are floating-point values 
  (CFloat = Single in Pascal) or TSDL_FPoint / TSDL_FRect in arrays.

_(s)_ and _(F)_ can be combined, for example: `SDL_RenderDrawPoint`,
`SDL_RenderDrawPoints`, `SDL_RenderDrawPointF` and `SDL_RenderDrawPointsF`.

Internally, the _~sF_ versions are the ones ultimately responsible for performing 
the actual drawing operations.

Additionally, there are the `SDL_RenderGeometry[Raw]` functions used to draw filled 
triangles, which are more advanced as they allow gradient and textured rendering.

It is likely faster for drawing filled polygons, although I must test them.

## Drawing with SDL2_GFX:

`SDL2_GFX`, in its primitive drawing section, adds more functions and 
geometric shapes. It does this by using the basic SDL2 functions 
with SDL_Renderer.

However, it presents a series of "issues":

- The coordinate parameters in its functions use _SmallInt_, whereas 
  SDL uses _LongInt_, and ultimately _CFloat_ is used internally.
- This typically implies range checks and multiple conversions before drawing:
  _LongInt_ or  _Float_ then _SmallInt_ then _LongInt_ then _Float_ then
  _Array of Float_.
- Functions for multiple points and lines require separate arrays for each 
  coordinate (one array for _X_ and another for _Y_) instead of directly using 
  arrays of `TSDL_Point` / `TSDL_FPoint`.
  - Consequently, both arrays are merged and rewritten so that 
    SDL can consume them.
- "HTML-style" colors (essentially a hexadecimal DWORD) must have their 
  individual components extracted, as `SDL_SetRenderDrawColor` only accepts 
  individual components.
- The draw functions always alter the active color and Blend Mode.

In addition, I intend to remove the other `SDL2_GFX` dependencies used by 
`CHXSDL2Engine`, as it also has other details that I find unsatisfactory:

- FrameManager: Ended managed FPS manually {`CHXFrameManager` was created for
  SDL3)
- Basic ASCII font for text: Vanilla SDL can render debug text using the exact 
  same font as `SDL2_GFX`. 


## About CHXSDL2Renderer:

The purpose of this unit is as follows:

- Encapsulate `SDL_Renderer` drawing logic within a dedicated class.
- Add significant primitive drawing functionality from `SDL2_GFX`, with 
  key modifications:
  - Remove the _SmallInt_ range restriction and directly use _CFloat_ (Single) 
    for coordinates.
  - Directly use arrays of `TSDL_FPoint` and `TSDL_FRect` instead of separate 
    arrays for coordinates.
- Do not alter color or Blend Mode every time a primitive is drawn or 
  when the same color will be reused. This requires color changes to be 
  handled via a separate explicit function call.
- Return a _Boolean_ instead of an _Integer_ as an error code, unless the 
  returned _Integer_ carries additional semantic meaning.
- Add some Quality of Life (QoL) features:
  - Overloaded variants of functions depending on parameter types:
    - **Point**: P: `TSDL_FPoint` <=> X, Y: _CFloat_
    - **Rect**: R: `TSDL_FRect` <=> X, Y, W, H: _CFloat_ <=> Seg: TCHXSegment (maybe)
    - **Color**: aColor: `TSDL_Color` <=> R, G, B, A: _Byte_ <=> Grey, A: _Byte_
    - **Point Array**: PArr: `TSDLFPointDynArr` <=> PList: `cSDLFPointList`
    - **Rect Array**: RArr: `TSDLFRectDynArr` <=> RList: `cSDLFRectList`
  - Evaluate whether it is more efficient for these variants to call a 
    common method or be implemented independently.
- Provide methods to draw:
  - Outline/Border only.
  - Filled area (and potentially filled without border).
  - Both (ensuring border and fill do not overlap, as alpha transparencies 
    would accumulate).

Although I will try to simplify and avoid writing all variants—especially those 
drawing both Border and Filled the basic sequence would be:

1. Change to fill color.
2. Draw only fill shape.
3. Change to border color.
4. Draw border.

To use specialized generic lists as if they were dynamic arrays, use the 
List property or a pointer to the first element. Maybe is necessary to call 
the Pack method before using List.

Separately in uCHXSDL2Types is expected to have several useful types and helper
for SDL data structures will be defined, even if not all are used here:

- Dynamic arrays of `TSDL_Color`, `TSDL_FPoint`, `TSDL_FRect`, etc.
  (`TSDLColorDynArray`, `TSDLFPointDynArray`, etc.)
- Specialized generic lists for those types: (`cSDLFPointList`, 
  `cSDLFRectList`, etc.)
- Type helpers, similar to `TPoint3DF` but in 2D. For integer-based types, 
  the FPC Types unit can be referenced.
- Global constructor functions returning these types, to be used directly 
  as parameters without explicitly declaring a variable.
- Additional auxiliary types:
  - Non-continuous segment lists, which could be:
    - A list with a new `TCHXSegment` type containing 2 points.
    - Or simply a list of points, provided functions account for 
      the requirement that the element count must be even.

Features I initially will not implement, but should (and might end up doing):
  - Primitive rotations.
  - Variable border thickness for shapes.
  - Antialiasing for shapes (which goes hand in hand with thick lines).
  - Rotations and rotation pivot points.

> Rest of the notes are the declaration and implementation on many methods 
> wich were adapted and implemented in SDL3
