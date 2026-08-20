# CHXSDL3Renderer drawing primitives

Collection of programs to test drawing primitives of `cCHXSDL3Renderer`.

## Information

Many of the programs are simple test of random drawing and filling, or
some specific tests (compare Circle algorithm vs Ellipse one).

## Compiling

From main directory (where `build.sh` and `Build.bat`_ are):

### Unix-like:

```
./build.sh {Program}.pas
```

### Windows:

In command line (easy opened with [Mays] + [R-Click] on the folder, and then
select "Open command line here" or similar):

```
build {Program}.pas
```

On Windows, Free Pascal Compiler program is suposed to be in
`PATH` enviroment variable and executable's folder must have _SDL3.dll_ (and
other _.dll_ if needed) and be sure that they are for the compiled
architecture (32/64bits).

If Lazarus is used, maybe it's needed to add manually used units folders
to the project.

### Scripts, Parameters and Executable

Both script files simply do the following:

1. Change to script directory.
2. Create FPC output directories.
3. Run `fpc @fp.cfg {Program}.pas [OtherParameters]`

So,

- Aditional FPC parameters can be passed to scripts to add or override
  _fp.cfg_ ones.
- Parameter `-dRELEASE` generates a smart linked, stripped and optimized
  executable. By default, debug one will be created with debug info, error
  checking fallback and `heaptrc` unit for memory leaks.

Executable program will be created in `bin/` directory. As said in _Compiling_,
it need _at least_ find `SDL3.{dll|o}`. In Windows it's not common to have it
in a system folder, so it needs a copy of `SDL3.dll` in executable's folder.

Furthermore, executable will change its current directory to the directory
where it resides to search external files if needed (own _SDL3Engine_ config
file for example).

## Usage

By default some keys are assigned:

- **[F1]**: Toggle help text inside of the program.
- **[F11]**: Toggle FPS info.
- **[F10]** / **[F12]**: Decrease / Increase FPS.
- **[ESC]**: Exit the program.

Each program can have its own keys listed with **[F1]**, but in general:

- **[C]**: Change used colors.
- **[F]**: Change primitive filling between: _Full Fill_ and
  _Border + Internal Fill_.
- **[P]**: Change points.
- **[R]**: Change rects.
