### CHXSDL3Engine

- Added some methods and initial primitives to **cCHXSDL3Renderer**:
  `Set/GetDrawColor`, `Clear`, `Point(s)`, `Line(s)`, `Triangle`,
  `Rect(s)` (Axis Aligned Rectangles), `Quad` (Quadrilaterals), `Polygon`,
  `RegPolyCC` (Regular Polygon circumscribed in a Circle and custom rotation
   angle), `RegPolySS` (Regular Polygon with a Side Length and  rotation angle)
   and `Circle` (only Border).
   - **Note**: Some methods, specially polygons with borders, maybe don't work
     as fully expected when using Logical Presentation because SDL3 smooth
     border lines.
- **uCHXSDL3TypeHelpers.pas**:
  - Removed integer stuff intended for SDL2.
  - Adding helper methods and operators for SDL3 types as needed.
  - Adding `TCHXSDLFSegment` type. Sometimes is better to store a segment with
    it's endpoints instead using a `TSDL_FRect` and calculate `X + W` and
    `Y + H` multiple times.
