unit ucCHXSDL3FPSManager;
{<
  cCHXSDL3FPSManager unit. 

  _SDL_gfx_ has some unexpected behaviours:

  1. When a frame lags sets _Frame Count_ to 0 again, so it can't be used as
    timestamp or calculate difference between frames.
  2. `SDL_FrameDelay` returns time elapsed without the delay itself. If engine
    works with deltatime we want actual total deltatime between frames.

  The first is useful to restore frame interpolation and get next FPS window.
    While in the second one it is useful to get the actual procesing time.

  This unit and class:

  - Encapsulate all functionallity inside a class, instead of using a record
    wich must be used as parameter of functions (although it's SDL general
    design).
  - Use an absolute frame counter independent of the one used for interpolation.
  - `Delay` stores actual compute time and whole frame time (with Delay) as
    properties and returns waited time (or miliseconds late)

  ToDo: May be change it to use nanoseconds.
}
{$mode ObjFPC}{$H+}
{$inline ON}
interface
uses
  CTypes, SDL3;

type
  cCHXSDL3FPSManager = class
  private
    IntFC: CUInt64;    //< Internal frame counter for interpolation.
    BaseTick: CUInt64; //< Initial tick for interpolation.
    RateTicks: CFloat; //< Ticks per frame for interpolation.
    LastTick: CUInt64; //< Last time that Delay was executed.

    FFrameCount: CUInt64;
    FFPS: CUInt16;
    FLastFrameTime: CUInt64;
    FLastCompTime: CUInt64;

  protected
    procedure SetFPS(const aFPS: CUInt16);

  public
    property FrameCount: CUInt64 read FFrameCount;
    //< Current (absolute) frame number or number of Delay calls.
    property FPS: CUInt16 read FFPS write SetFPS;
    //< FPS desired to achieve.
    property LastFrameTime: CUInt64 read FLastFrameTime;
    //< Total last frame time in miliseconds (with Delay).
    property LastCompTime: CUInt64 read FLastCompTime;
    //< Actual time between Delay calls (without Delay).

    constructor Create(const aFPS: CUInt16 = 30);

    function TimePassed: CUInt64; inline;
    {<
      Returns miliseconds passed since last call of Delay. It can be used to
        put a time limit in iterations: `while FPSMang.TimePassed < 10 do ...`
    }

    function Delay: CInt64;
    {<
      Waits for next frame window and returns time delayed, in other words 
        milisecond ahead (positive) o behind (negative) of frame window.

      Changes the properties:

      - `LastFrameTime`: Stores last computation time (without Delay).
      - `LastCompTime`: Stores last total frame time (with Delay).
    }
  end;

implementation

constructor cCHXSDL3FPSManager.Create(const aFPS: CUInt16);
begin
  FFrameCount := 0;
  FPS := aFPS; // Configure all with its setter
end;

procedure cCHXSDL3FPSManager.SetFPS(const aFPS: CUInt16);
begin
  if aFPS = FFPS then
    Exit;
  if aFPS > 0 then // Ensure FPS > 0..
    FFPS := aFPS
  else
    FFPS := 1;
  IntFC := 0;
  RateTicks := 1000 / FFPS;
  BaseTick := SDL_GetTicks;
  LastTick := BaseTick;
end;

function cCHXSDL3FPSManager.TimePassed: CUInt64;
begin
  Result := SDL_GetTicks - LastTick;
end;

function cCHXSDL3FPSManager.Delay: CInt64;
var
  TargetTick, CurrTick: CInt64; //< Yes, Signed Int64
begin
  Inc(FFrameCount); Inc(IntFC);

  TargetTick := BaseTick + Trunc(IntFC * RateTicks);
  CurrTick := SDL_GetTicks;

  FLastCompTime := CurrTick - LastTick;

  Result := TargetTick - CurrTick;

  if Result > 0 then
  begin
    SDL_Delay(Result);
    CurrTick := SDL_GetTicks;
    FLastFrameTime := CurrTick - LastTick;
  end
  else
  begin
    IntFC := 0;
    // CurrTick := SDL_GetTicks; Not needed.
    BaseTick := CurrTick;
    FLastFrameTime := FLastCompTime;
  end;

  LastTick := CurrTick;
end;

end.
