# Nature of the Code / The Coding Train

This folder have some reimplementations of "Nature of the Code" book and 
challenges in "The Coding Train" Youtube channel, both by Daniel Shiffman, 
using FreePascal and SDL2 for testing.

  - **PasProc.lpi** is simple program "simulating" Processing, used as a base
    for other programs. 
  - **NOTCTest** programs inspired from *Nature of the Code*.
  - **CTTest** are programs `ported` from challenges of *The Coding Train*.
  - **source** has units and classes created from *Nature of the Code*.
  
## PasProc.lpi

To use it simply open and `Save Proyect As...` with a new name. 

Processing's `OnDraw` is splited in `OnCompute` and `OnDraw` because 
I'm not sure wich is the best way to implement it (SDLRenderer as parameter or 
as property of an common ancestor class).

So, the pourpose of `OnDraw` in the main program is for implemntation of 
`Show`/`Draw`/etc. methods of classes.

Pascal don't have Garbage Collector as is, so allocated memory and objects must 
be freed `OnFinish` (or `OnCompute`/`OnEvent`). In FPC, we can make reference
counted interfaced objects wich autofree, but it's easier free them manually.

All Events are listed and commented out to have an easy reference, and window
and any quit event is handled automatically. Escape key is mapped to exit the 
program too.

If window is resized, Renderer automatically stretch image to its actual size.

## NOTCTest

Example programs to test classes created based in *Nature of the Code* book.

They are not actually a direct *port* from Processing, but heavily inspired.

## CTTest

Programs trying to *port* challenges of *The Coding Train*.

At first, they are ports *as is* with minimal code variations. Althougt they are
not very optimized or Pascal friendly.

Maybe some of them will be *Pascalized* more, but saved as another project.

## `source` folder

Where actual *Nature of the Code* inspired units are.