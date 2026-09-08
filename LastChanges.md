- Changed the format of `Cheatsheet.md` and updated.
- Added `PushRenderSize` and `PopRenderSize` to _CHXSDL3Engine_ window
  to add and retrieve render sizes into a stack.
- Revisited `Ellipse[x]`, added `TEllipse[x]` methods and optimized a
  little `TCircle[x]` ones to _CHXSDL3Engine_ renderer.
- Added `PushDrawColor` and `PopDrawColor` to _CHXSDL3Engine_ renderer
  to add and retrieve current color into a stack.
- Little tweaks and fixes in primitives.
- Adding support for UI components, creating a prototype of a button.
- Changed constructor of `ucCHXSDL3Window` to have a renderer driver list
  as parameter instead `UseGPU`.
