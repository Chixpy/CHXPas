# Primitives

<Little description>

## Information



## Compiling

From main directory (where _build.sh_ and _build.bat_ are):

### Unix-like:

> ./build.sh

### Windows:

Dbl-Click over _build.bat_ or in command line:

> build

On Windows, Free Pascal Compiler program is suposed to be in
`PATH` enviroment variable and executable's folder must have _SDL3.dll_ (and
other .dll if needed) and be sure that they are for the compiled
architecture (32/64bits).

If Lazarus is used, maybe it's needed to add used units folders to the project.

### Parameters and executable

FPC parameters can be passed to add or override  _fp.cfg_ ones.

Parameter `-dRELEASE` generates an optimized executable.

Executable program will be created in _bin_ directory.

## Usage

  - **[F1]**: Shows help text inside of the program (if avaiable).
  - **[F11]**: Toggle FPS info.
  - **[F10]** / **[F12]**: Decrease / Increase FPS.
  - **[ESC]**: Exit the program.

## Sources and more information

