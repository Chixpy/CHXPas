unit ucCHXSDL3Button;
{< Unit of cCHXSDL3Button class.

  (C) 2024-2026 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}{$INLINE ON}{$WARN 6058 OFF}

interface

uses
  Classes, SysUtils, CTypes,
  SDL3,
  uCHXSDL3TypeHelpers, ucCHXSDL3Renderer,
  uaCHXSDL3Component;

const
  MinSizeButton = 10;

type

  { cCHXSDL3Button }

  cCHXSDL3Button = class(caCHXSDL3Component)
  private // Get / Set
  (*
    FCaption : string;
    FFont : caCHXSDL2Font;
    FGlyph : PSDL_Texture;
    procedure SetCaption(const aValue : string);
    procedure SetFont(const aValue : caCHXSDL2Font);
    procedure SetGlyph(const aValue : PSDL_Texture);
  *)

  private
(*
    GlyphW : Integer;
    GlyphH : Integer;
    FontW : Integer;
    FontH : Integer;

    procedure CreateCache;
  protected
    procedure SetSize;
*)

  public
    OnClick : TComponentCB;

(*
    property Caption : string read FCaption write SetCaption;
    property Font : caCHXSDL2Font read FFont write SetFont;
    property Glyph : PSDL_Texture read FGlyph write SetGlyph;
*)

    // constructor Create(const aX, aY : Integer;
    //   const aFont : caCHXSDL2Font = nil; const aID : string = '';
    //   const aCaption : string = ''; const aGlyph : PSDL_Texture = nil);
    constructor Create(const aID: String; const X, Y, W, H: CFloat);
    destructor Destroy; override;

    procedure Setup; override;
    procedure Finish; override;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw(const Render : cCHXSDL3Renderer); override;
    procedure HandleEvent(const aEvent : TSDL_Event;
      var Handled, ExitProg : Boolean); override;
  end;

implementation

{ cCHXSDL3Button }

(*
procedure cCHXSDL3Button.SetGlyph(const aValue : PSDL_Texture);
var
  format : cuint32;
  access : cint;
begin
  FGlyph := (aValue);

  if assigned(Glyph) then
  begin
    SDL_QueryTexture(Glyph, @format, @access, @GlyphW, @GlyphH);
  end
  else
  begin
    GlyphW := 0;
    GlyphH := 0;
  end;

  SetSize;
end;

procedure cCHXSDL3Button.CreateCache;
begin
  if (not assigned(Font)) or (ID = '') then
  begin
    FontW := 0;
    FontH := 0;
  end
  else
  begin
    FontW := Font.AddStaticStr(Id, Caption);
    FontH := Font.LineHeight;
  end;

  SetSize;
end;

procedure cCHXSDL3Button.SetSize;
begin
  // 2 pixels of padding
  if FontW > GlyphW then
    Width := FontW + 4
  else
    Width := GlyphW + 4;
  if FontH > GlyphH then
    Height := FontH + 4
  else
    Width := GlyphH + 4;
end;

procedure cCHXSDL3Button.SetCaption(const aValue : string);
begin
  if Assigned(Font) and (id <> '') then
    Font.RemoveStatic(ID);

  FCaption := aValue;

  CreateCache;
end;

procedure cCHXSDL3Button.SetFont(const aValue : caCHXSDL2Font);
begin
  if Assigned(Font) and (id <> '') then
    Font.RemoveStatic(ID);

  FFont := aValue;

  CreateCache;
end;
*)


constructor cCHXSDL3Button.Create(const aID: String; const X, Y, W, H: CFloat);
// constructor cCHXSDL3Button.Create(const aX, aY : Integer;
//   const aFont : caCHXSDL2Font; const aID : string; const aCaption : string;
//   const aGlyph : PSDL_Texture);
begin
  inherited Create(aID, X, Y, W, H);

  if H < MinSizeButton then
    Rect.H := MinSizeButton; // Min size
  if W < MinSizeButton then
    Rect.W := MinSizeButton; // Min size

(*
  FFont := aFont; //FFont to call CreateCache only one time
  FCaption := aCaption; //FCaption to call CreateCache only one time
  CreateCache;

  Glyph := aGlyph;
*)
end;


destructor cCHXSDL3Button.Destroy;
begin
  inherited Destroy;
end;

procedure cCHXSDL3Button.Setup;
begin

end;

procedure cCHXSDL3Button.Finish;
begin

end;

procedure cCHXSDL3Button.Compute(const FrameTime : CUInt32;
  var ExitProg : Boolean);
begin

end;

procedure cCHXSDL3Button.Draw(const Render : cCHXSDL3Renderer);
var
  aColor : TSDL_FColor;
(*
  x1, y1 : Integer;
  TgtRect : TSDL_Rect;
*)
begin
  inherited Draw(Render);
  
  if Focused then aColor := HLColor
  else aColor := BDColor;

  Render.Rect(Rect, aColor, BGColor);

(*
  // Glyph and text padding
  x1 := X + 2;
  y1 := Y + 2;

  if Assigned(Glyph) then
  begin
    TgtRect := SDLRect(x1, y1, GlyphW, GlyphH);
    SDL_RenderCopy(PRenderer, Glyph, nil, @TgtRect);
  end;

  if Assigned(FFont) and (id <> '') then
    Font.RenderStatic(ID, x1, y1);
*)
end;

procedure cCHXSDL3Button.HandleEvent(const aEvent : TSDL_Event; var Handled,
  ExitProg : Boolean);
begin
  inherited HandleEvent(aEvent, Handled, ExitProg);
  if Handled or ExitProg or (not Focused) then  Exit;

  case aEvent.type_ of
    SDL_EVENT_MOUSE_BUTTON_DOWN : // (button: TSDL_MouseButtonEvent);
    begin
      if Assigned(OnClick) then
        OnClick(Self);
      Handled := True;
    end;

    SDL_EVENT_KEY_DOWN : // (key: TSDL_KeyboardEvent);
    begin
      case aEvent.key.key of
        SDLK_SPACE, SDLK_RETURN :
        begin
          if Assigned(OnClick) then
            OnClick(Self);
          Handled := True;
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
