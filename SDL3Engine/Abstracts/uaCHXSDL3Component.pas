unit uaCHXSDL3Component;
{< Unit of caCHXSDL3Component abstract class.

  (C) 2024-2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}

interface

uses
  Classes, SysUtils, Generics.Collections, CTypes,
  SDL3,
  uCHXSDL3TypeHelpers, ucCHXSDL3Renderer;

type

  { caCHXSDL3Component }

  caCHXSDL3Component = class(TPersistent)
  private // Get / Set
    FFocused : Boolean;
    procedure SetFocused(const aValue : Boolean);

  protected
    FID: String;


  public
    Rect : TSDL_FRect;

    BGColor, BDColor, HLColor : TSDL_FColor;

    LogPresW, LogPresH : CInt;
    LogPresMode: TSDL_RendererLogicalPresentation;

    property ID : String read FID;
    property Focused : Boolean read FFocused write SetFocused;

    constructor Create(const aID : String; const X, Y, W, H : CFloat);
    destructor Destroy; override;

    procedure SetFocus; virtual;
    //< Childs may want to do something on Focus
    procedure UnsetFocus; virtual;
    //< Childs may want to do something on Unfocus

    procedure Setup; virtual; abstract;
    procedure Finish; virtual; abstract;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      virtual; abstract;
    procedure Draw(const Render : cCHXSDL3Renderer); virtual; abstract;
    procedure HandleEvent(const aEvent : TSDL_Event;
      var Handled, ExitProg : Boolean); virtual;
  end;

  cSDL3GenCompList = specialize TObjectList<caCHXSDL3Component>;
  cSDL3ComponentList = class(cSDL3GenCompList);

  // Event Callbacks
  TComponentCB = procedure(const Sender : caCHXSDL3Component) of object;
  TCompStringCB = procedure(const Sender : caCHXSDL3Component;
    const aStr : string) of object;

implementation

{ caCHXSDL3Component }

procedure caCHXSDL3Component.SetFocused(const aValue : Boolean);
begin
  if aValue then SetFocus else UnsetFocus;
end;

constructor caCHXSDL3Component.Create(const aID: String;
  const X, Y, W, H: CFloat);
begin
  inherited Create;

  FID := aID;
  Rect := SDLFRect(X, Y, W, H);

  // WriteLn(Format('%s: %g, %g, %g, %g',
  //   [ID, Rect.X, Rect.Y, Rect.W, Rect.H]));

  FFocused := False;

  BGColor := SDLFColor(0.25);    //< Background color
  BDColor := SDLFColor(0.5);     //< Border color if not focused
  HLColor := SDLFColor(1, 0, 1); //< Border color if focused
end;

destructor caCHXSDL3Component.Destroy;
begin

  inherited Destroy;
end;

procedure caCHXSDL3Component.SetFocus;
begin
  FFocused := True;
end;

procedure caCHXSDL3Component.UnsetFocus;
begin
  FFocused := False;
end;

procedure caCHXSDL3Component.HandleEvent(const aEvent : TSDL_Event;
  var Handled, ExitProg : Boolean);
begin
  if ExitProg then
    Exit;

  case aEvent.type_ of
  SDL_EVENT_MOUSE_BUTTON_DOWN : // (button: TSDL_MouseButtonEvent);
  begin
    if Focused then
    begin
      // Exit the component with mouse
      if (aEvent.Button.X < Rect.X)
        or (aEvent.Button.Y < Rect.Y)
        or (aEvent.Button.X > Rect.X + Rect.W)
        or (aEvent.Button.Y > Rect.Y + Rect.H) then
      begin
        UnsetFocus;
        // Handled := True;
      end;
    end
    else
    begin
      // Enter the component with mouse
      if (aEvent.Button.X > Rect.X)
        and (aEvent.Button.Y > Rect.Y)
        and (aEvent.Button.X < Rect.X + Rect.W)
        and (aEvent.Button.Y < Rect.Y + Rect.H) then
      begin
        SetFocus;
        // Handled := True;
      end;
    end;
  end;

  otherwise
    ;
  end;
end;
end.
{< This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
