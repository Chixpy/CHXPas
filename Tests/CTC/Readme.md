# The Coding Train Challenges

Initially this ports of [The Coding Train Challenges](http://codingtra.in) 
by Daniel Shiffman were used for testing SDL2 and creating a basic engine in
FPC/Lazarus.

Basically Processing code is ported "as is", althougth it is not very optimized
or Pascal friendly. The main changes are:

  - No Garbage Collector, so reserved dynamic memory and objects are freed
    manually.
  - Processing's Draw is splitted in cCTCEng `Compute` and `Draw` methods.
    - `cCTCEng.Compute`: To update program state.
    - `cCTCEng.Draw`: Usually to implement Show/Draw methods of created classes.

Some day improved and better "Pascalized" version of programs and classes
will be done while ucSDL2Engine will evolve if needed.

## SDLProcessing.lpi

This is the main base program to simulate Processing. To use it simply open and
`Save Proyect As...` with a new name.

Pascal don't have Garbage Collector as is, so allocated memory and objects must
be freed. In FPC, we can make reference counted interfaced objects wich
autofree, but it's easier free them manually.

`ExitProg` var is used to Finish the program in `Compute` and `HandleEvent`.

Processing's `Draw` is splited in `Compute` and `Draw`. Both are called
every frame. `Compute` has FrameTime var with millisecond passed between
frames.

The pourpose of `Draw` in the main program is for implementation of
`Show`/`Draw`/etc. methods of classes. I'm not sure wich is the best way to
implement it in this ports. (pass SDLRenderer as parameter or as property of
an common ancestor class).

Some Events are listed and commented out to have an easy reference, and window
and any quit event is handled automatically.

`Esc` key is mapped to exit the program and `F11` will show framerate in
  window title

If window is resized, SDL Renderer automatically stretch image to its actual
size. If soft renderer is used it will be scaled by integer values, because
it shows vertical black stripes ¿?.

## Done

| # | Name | Comments |
|---:|:---|:---|
| CT001 | Starfield | Testing SDL_RenderXXX and SDL_GFX functions. Manual tweak of simple coordinates tranformations |
| CT003 | The Snake Game | Keyboard event handling |
| CT004 | Purple Rain |  |
| CT005 | Space Invaders |  |
| CT006 | Mitosis Simulation | Mouse event handling |
| CT010 | Maze Generator |  |
| CT013 | Reaction Diffusion Algorithm | Direct renderer pixel access. (Redone later with direct texture pixel access) |
| CT015 | Object-Oriented Fractal Trees |  |
| CT017 | Space Colonization |  |
| CT019 | Superellipse | Interactive control |
| CT021 | Mandelbrot | Direct texture pixel access. (And redone later with direct renderer pixel access) |
| CT022 | Julia Set |  |
| CT023 | 2D Supershapes |  |
| CT027 | Fireworks (2D) |  |
| CT028 | Metaballs |  |

## Skipped

Reasons to skip:

| # | Reason | Name |
|---:|:--:|:---|
| CT002 | 3D | Menger Sponge Fractal |
| CT007 | PMM | Solar System (2D) |
| CT008 | 3D | Solar System (3D) |
| CT009 | 3D | Solar System (3D) with textures |
| CT011 | 3D | 3D Terrain Generation with Perlin Noise |
| CT012 | 3D | The Lorenz Attractor |
| CT014 | PMM | Recursive Fractal Trees |
| CT016 | PMM | Fractal Trees - L-System |
| CT018 | 3D | 3D Fractal Trees |
| CT020 | TXL | 3D Cloth with Toxiclibs |
| CT024 | PMM | Perlin Noise Flow Field |
| CT025 | 3D | Spherical Geometry |
| CT026 | 3D | 3D Supershapes |
| CT027 | 3D | Fireworks (3D) |
| CT029 | PMM / TXT | Smart Rockets in p5.js |
| CT030 | PMM | Phyllotaxis |
| CT031 | TXT | Flappy Bird |
| CT032.1 | PMM | Agar.io |
| CT032.2 | PMM / S/C | Agar.io |

  - 3D: Not done now, because needs a 3D engine; they can be done with
      OpenGL/Vulkan setup (SDL, TBGRABitmap or any other OpenGl context).
  - PMM: Not done because Processing coordinate matrix manipulations
    (pushMatrix, rotate, translate, popMatrix, etc.). They change the
    coordinate system in a stack and apply to all points to make relative
    translations and rotations. Not sure how to implement it:
    - Manually tweaked drawing of points and lines.
    - Maybe creating a stack of new rendering textures on pushMatrix. They
      can be rotated and translated and render in parent texture on popMatrix.
      But, we can't draw on negative coords.
    - Or making a wrapper of all drawing function that makes all
      transformations before actual drawing.
    - Or, Without SDL, TBGRABitmap has TBGRACanvas2D wich simulates JavaScript
      Canvas with rotate, scale, translate, save (pushMatrix) and
      restore (popMatrix). But I need to create a new engine based on
      TBGRABitmap (and maybe I can redo all SDL examples in TBGRABitmap too).
  - TXL: Use toxiclibs physics library.
  - TXT: Draw text with graphics.
    - Drawing text in SDL requires SDL_ttf, creating a surface with actual text
      rendered.
  - S/C: Use of Server / Client sockets.

## ToDo

| # | Name |
|---:|:---|
| CT033 | Poisson-disc Sampling |
| CT034 | Diffusion-Limited Aggregation |




