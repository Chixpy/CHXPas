# The Coding Train Challenges

Basically Processing code is ported "as is", althougt it is not very optimized
or Pascal friendly. The main changes are:

  - Show/Draw methods in classes are moved to main OnDraw call.
  - No Garbage Collector, so reserved dynamic memory and objects are freed.

Maybe some day a better "Pascalized" version of the programs and the classes 
will be done and ucSDL2Engine will evolve.

## Done / Skipped

| # | Name |
|---:|:---|
| CT001 | Starfield |
| CT003 | The Snake Game |
| CT004 | Purple Rain |
| CT005 | Space Invaders |
| CT006 | Mitosis Simulation |
| CT010 | Maze Generator |
| CT013 | Reaction Diffusion Algorithm |
| CT015 | Object-Oriented Fractal Trees |
| CT017 | Space Colonization |
| CT019 | Superellipse |
| CT021 | Mandelbrot |

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

  - 3D: Not done now, because needs a 3D engine; they can be done with
    SDL OpenGL/Vulkan setup.
  - PM: Not done because Processing coordinate Matrix manipulations
    (pushMatrix, rotate, translate, popMatrix, etc.).
    They change the coordinate system in a stack and apply to all points to
    make relative translations and rotations. Not sure how to implement it.
  - TX: Use toxiclibs physics library
 
## ToDo

| # | Name |
|---:|:---|
| CT022 | Julia Set in Processing |
| CT023 | 2D Supershapes |
| CT024 | Perlin Noise Flow Field |
| CT025 | Spherical Geometry |
| CT026 | 3D Supershapes |
| CT027 | Fireworks |
| CT028 | Metaballs |
| CT029 | Smart Rockets in p5.js |
| CT030 | Phyllotaxis |
| CT031 | Flappy Bird |
| CT032 | Agar.io |
| CT033 | Poisson-disc Sampling |
| CT034 | Diffusion-Limited Aggregation |




