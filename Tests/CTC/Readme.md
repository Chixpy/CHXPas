# The Coding Train Challenges

Initially this ports of [The Coding Train Challenges](http://codingtra.in) 
by Daniel Shiffman were used for testing SDL2 and creating a basic engine in
FPC/Lazarus.

Basically Processing code is ported "as is", althougth it is not very optimized
or Pascal friendly. The main changes are:

  - No Garbage Collector, so reserved dynamic memory and objects are freed.
  - OnDraw is splitted in OnCompute and OnDraw functions.

Some day improved and better "Pascalized" version of programs and classes
will be done while ucSDL2Engine will evolve if needed.

## Done

| # | Name | Comments |
|---:|:---|:---|
| CT001 | Starfield | Testing SDL_RenderXXX and SDL_GFX functions |
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
| CT007 | PM | Solar System (2D) |
| CT008 | 3D | Solar System (3D) |
| CT009 | 3D | Solar System (3D) with textures |
| CT011 | 3D | 3D Terrain Generation with Perlin Noise |
| CT012 | 3D | The Lorenz Attractor |
| CT014 | PM | Recursive Fractal Trees |
| CT016 | PM | Fractal Trees - L-System |
| CT018 | 3D | 3D Fractal Trees |
| CT020 | TX | 3D Cloth with Toxiclibs |
| CT024 | PM | Perlin Noise Flow Field |
| CT025 | 3D | Spherical Geometry |
| CT026 | 3D | 3D Supershapes |
| CT027 | 3D | Fireworks (3D) |


  - 3D: Not done now, because needs a 3D engine; they can be done with
      SDL OpenGL/Vulkan setup.
  - PM: Not done because Processing coordinate matrix manipulations
    (pushMatrix, rotate, translate, popMatrix, etc.). They change the
    coordinate system in a stack and apply to all points to make relative
    translations and rotations. Not sure how to implement it:
    - Maybe creating a stack of new rendering textures on pushMatrix. They
      can be rotated and translated and render in parent texture on popMatrix.
      But, we can't draw on negative coords.
    - Or making a wrapper of all drawing function that makes all
      transformations before actual drawing.
    - Or, Without SDL, TBGRABitmap has TBGRACanvas2D wich simulates JavaScript
      Canvas with rotate, scale, translate, save (pushMatrix) and
      restore (popMatrix). But I need to create a new engine based on
      TBGRABitmap (and maybe I can redo all SDL examples in TBGRABitmap too).
  - TX: Use toxiclibs physics library.
 
## ToDo

| # | Name |
|---:|:---|
| CT029 | Smart Rockets in p5.js |
| CT030 | Phyllotaxis |
| CT031 | Flappy Bird |
| CT032 | Agar.io |
| CT033 | Poisson-disc Sampling |
| CT034 | Diffusion-Limited Aggregation |




