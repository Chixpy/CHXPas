unit uaCHXSDL2Comp;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FGL, CTypes,
  SDL2;

type
  { caCHXSDL2Comp }

  caCHXSDL2Comp = class(TPersistent)
  private
    FFocused : Boolean;
    procedure SetFocused(const aValue : Boolean);

  public
    {property} PRenderer : PSDL_Renderer;

    {property} X : Integer;
    {property} Y : Integer;
    {property} Width: Integer;
    {property} Height: Integer;

    // Colors
    {property} BGColor : CUInt;
    //< Background color $AABBGGRR in Intel/Windows
    {property} BDColor : CUInt;
    //< Border color if not focused
    {property} HLColor : CUInt;
    //< Border color if focused

    property Focused : Boolean read FFocused write SetFocused;

    procedure SetFocus; virtual;
    //< Childs may be want to do something on Focus
    procedure UnSetFocus; virtual; //< Childs may be want to do something (2)

    procedure Setup; virtual; abstract;
    procedure Finish; virtual; abstract;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      virtual; abstract;
    procedure Draw; virtual; abstract;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled, ExitProg : Boolean); virtual;

    constructor Create;
    destructor Destroy; override;
  end;

  // We can't use specialize with forward declarations.
  cSDL2GenCompList = specialize TFPGObjectList<caCHXSDL2Comp>;

  cSDL2CompList = class(cSDL2GenCompList);

implementation

{ caCHXSDL2Comp }

procedure caCHXSDL2Comp.SetFocused(const aValue : Boolean);
begin
  if aValue then SetFocus else UnSetFocus;
end;

procedure caCHXSDL2Comp.SetFocus;
begin
  FFocused := True;
end;

procedure caCHXSDL2Comp.UnSetFocus;
begin
  FFocused := False;
end;

procedure caCHXSDL2Comp.HandleEvent(const aEvent : TSDL_Event; var Handled,
  ExitProg : Boolean);
begin
  if Handled or ExitProg then  Exit;

  case aEvent.type_ of
    SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
    begin
      if Focused then
      begin
        // Exit the component with mouse
        if (aEvent.button.X < X) or (aEvent.button.Y < Y) or
          (aEvent.button.X > X + Width) or (aEvent.button.Y > Y + Height) then
        begin
          UnsetFocus;
        end;
      end
      else
      begin
        // Enter the component with mouse
        if not ((aEvent.button.X < X) or (aEvent.button.Y < Y) or
          (aEvent.button.X > X + Width) or (aEvent.button.Y > Y + Height)) then
        begin
          SetFocus;
        end;
      end;
    end;
    else
      ;
  end;
end;

constructor caCHXSDL2Comp.Create;
begin
  inherited Create;

  FFocused := False;

  BGColor := $FF404040; //< Background color
  BDColor := $FF808080; //< Border color if not focused
  HLColor := $FF00FFFF; //< Border color if focused

end;

destructor caCHXSDL2Comp.Destroy;
begin

  inherited Destroy;
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
