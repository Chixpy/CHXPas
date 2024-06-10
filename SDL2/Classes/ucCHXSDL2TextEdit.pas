unit ucCHXSDL2TextEdit;
{< Unit of cCHXSDL2TextEdit class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes, LazUTF8,
  SDL2, SDL2_GFX,
  uaCHXSDL2Comp,
  uaCHXSDL2Font;

type

  { cCHXSDL2TextEdit

    It's a very basic EditBox component for cCHXSDL2Engine.
  }

  cCHXSDL2TextEdit = class(caCHXSDL2Comp)
  private
    FFont : caCHXSDL2Font;
    procedure SetFont(const aValue : caCHXSDL2Font);

  protected
    // Cursor properties
    {property} CursorColor : CUInt;
    {property} CursorTimer : Integer;

  public
    Value : string;
    {< String in the TextEdit. }
    OnAccept : TCompStringCB;

    property Font : caCHXSDL2Font read FFont write SetFont;

    procedure SetFocus; override;
    procedure UnSetFocus; override;

    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw; override;
    procedure Finish; override;
    procedure HandleEvent(const aEvent : TSDL_Event; var Handled,
      ExitProg : Boolean); override;
    procedure Setup; override;

    constructor Create(const aFont : caCHXSDL2Font;
      const aX, aY, aWidth : Integer);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2TextEdit }

constructor cCHXSDL2TextEdit.Create(const aFont : caCHXSDL2Font;
  const aX, aY, aWidth : Integer);
begin
  inherited Create;

  Focused := False;

  { TODO : Exception if aFont are nil }
  Font := aFont;

  X := aX;
  Y := aY;
  Width := aWidth;
  BDColor := $FF808080; //< $AABBGGRR in Intel/Windows
  HLColor := $FF00FFFF; //< $AABBGGRR in Intel/Windows

  Value := '';

  CursorColor := $FF202020; //< $AABBGGRR in Intel/Windows
  CursorTimer := 0;
end;

procedure cCHXSDL2TextEdit.SetFont(const aValue : caCHXSDL2Font);
begin
  { TODO : Exception if aValue are nil }
  FFont := aValue;
  if Assigned(Font) then
    Height := Font.LineHeight + 4
  else
    Height := 0;
end;

procedure cCHXSDL2TextEdit.SetFocus;
begin
  inherited SetFocus;

  SDL_StartTextInput;
end;

procedure cCHXSDL2TextEdit.UnSetFocus;
begin
  inherited UnSetFocus;

  SDL_StopTextInput;
end;

procedure cCHXSDL2TextEdit.Compute(const FrameTime : CUInt32;
  var ExitProg : Boolean);
begin

end;

destructor cCHXSDL2TextEdit.Destroy;
begin
  inherited Destroy;
end;

procedure cCHXSDL2TextEdit.Draw;
var
  CursorX : Integer;
  aColor : CUInt;
begin
  if Focused then aColor := HLColor else aColor := BDColor;
  rectangleColor(PRenderer, X, Y, X + Width, Y + Height, aColor);

  CursorX := Font.RenderDynStrClipped(Value, X + 2, Y + 2, Width - 4) + 1;

  if Focused and SDL_IsTextInputActive then
  begin
    Inc(CursorTimer);
    if CursorTimer > 30 then
    begin
      CursorTimer := 0;
      CursorColor := CursorColor xor $00FFFFFF; // Inverse color, keep alpha
    end;
    vlineColor(PRenderer, X + CursorX, Y + 2, Y + Height - 2, CursorColor);
  end;
end;

procedure cCHXSDL2TextEdit.Finish;
begin

end;

procedure cCHXSDL2TextEdit.HandleEvent(const aEvent : TSDL_Event; var Handled,
  ExitProg : Boolean);
begin
  inherited HandleEvent(aEvent, Handled, ExitProg);
  if Handled or ExitProg or (not Focused) then  Exit;

  case aEvent.type_ of
    SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
    begin
      case aEvent.key.keysym.sym of
        // Special keys while editing text.
        SDLK_BACKSPACE, SDLK_DELETE :
        begin
          UTF8Delete(Value, UTF8Length(Value), 1);
          Handled := True;
        end;

        SDLK_RETURN, SDLK_KP_ENTER :
        begin
          UnsetFocus;
          if Assigned(OnAccept) then
            OnAccept(Self, Value);
          Handled := True;
        end;

        { TODO : CTRL+X, CTRL+C, CTRL+V... }
        SDLK_CUT :
        begin
          SDL_SetClipboardText(PAnsiChar(Value));
          Value := '';
          Handled := True;
        end;

        SDLK_COPY :
        begin
          SDL_SetClipboardText(PAnsiChar(Value));
          Handled := True;
        end;

        SDLK_PASTE :
        begin
          if SDL_HasClipboardText then
          begin
            Value += SDL_GetClipboardText;
            Handled := True;
          end;
        end;

        // Keys
        SDLK_SPACE, SDLK_EXCLAIM, SDLK_QUOTEDBL, SDLK_HASH,
        SDLK_PERCENT, SDLK_DOLLAR, SDLK_AMPERSAND, SDLK_QUOTE,
        SDLK_LEFTPAREN, SDLK_RIGHTPAREN, SDLK_ASTERISK, SDLK_PLUS,
        SDLK_COMMA, SDLK_MINUS, SDLK_PERIOD, SDLK_SLASH, SDLK_0, SDLK_1,
        SDLK_2, SDLK_3, SDLK_4, SDLK_5, SDLK_6, SDLK_7, SDLK_8, SDLK_9,
        SDLK_COLON, SDLK_SEMICOLON, SDLK_LESS, SDLK_EQUALS, SDLK_GREATER,
        SDLK_QUESTION, SDLK_AT, SDLK_LEFTBRACKET, SDLK_BACKSLASH,
        SDLK_RIGHTBRACKET, SDLK_CARET, SDLK_UNDERSCORE, SDLK_BACKQUOTE,
        SDLK_a, SDLK_b, SDLK_c, SDLK_d, SDLK_e, SDLK_f, SDLK_g, SDLK_h,
        SDLK_i, SDLK_j, SDLK_k, SDLK_l, SDLK_m, SDLK_n, SDLK_o, SDLK_p,
        SDLK_q, SDLK_r, SDLK_s, SDLK_t, SDLK_u, SDLK_v, SDLK_w, SDLK_x,
        SDLK_y, SDLK_z, SDLK_KP_DIVIDE, SDLK_KP_MULTIPLY, SDLK_KP_MINUS,
        SDLK_KP_PLUS, SDLK_KP_1, SDLK_KP_2, SDLK_KP_3, SDLK_KP_4,
        SDLK_KP_5, SDLK_KP_6, SDLK_KP_7, SDLK_KP_8, SDLK_KP_9, SDLK_KP_0,
        SDLK_KP_PERIOD :
          Handled := True
        else
          Handled := False;
      end;
    end;

    SDL_TEXTEDITING : // (edit: TSDL_TextEditingEvent);
    begin
      // This is called when a IME window is called (Win+.)
      Value += aEvent.edit.Text;
      Handled := True;
    end;
    SDL_TEXTEDITING_EXT : // (exitExt: TSDL_TextEditingExtEvent);
    begin
      Value += aEvent.exitExt.Text;
      // Freeing as TSDL_TextEditingExtEvent documentation says.
      //   I never triggered this.
      SDL_free(aEvent.exitExt.Text);
      Handled := True;
    end;
    SDL_TEXTINPUT : // (text: TSDL_TextInputEvent);
    begin
      Value += aEvent.Text.Text;
      Handled := True;
    end;

    else
      ;
  end;
end;

procedure cCHXSDL2TextEdit.Setup;
begin

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
