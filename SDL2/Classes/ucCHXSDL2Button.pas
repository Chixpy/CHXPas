unit ucCHXSDL2Button;
{< Unit of cCHXSDL2Button class.

  (C) 2024 Chixpy https://github.com/Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CTypes, LazUTF8,
  SDL2, SDL2_GFX,
  uCHXSDL2Utils,
  uaCHXSDL2Comp, uaCHXSDL2Font;

const
  MinSizeButton = 8;

type

  { cCHXSDL2Button }

  cCHXSDL2Button = class(caCHXSDL2Comp)
  private
    FCaption : string;
    FFont : caCHXSDL2Font;
    FGlyph : PSDL_Texture;
    procedure SetCaption(const aValue : string);
    procedure SetFont(const aValue : caCHXSDL2Font);
    procedure SetGlyph(const aValue : PSDL_Texture);

  private
    ID : string;
    GlyphW : Integer;
    GlyphH : Integer;
    FontW : Integer;
    FontH : Integer;

    procedure CreateCache;

  protected
    procedure SetSize;

  public
    OnClick : TCompCB;

    property Caption : string read FCaption write SetCaption;
    property Font : caCHXSDL2Font read FFont write SetFont;
    property Glyph : PSDL_Texture read FGlyph write SetGlyph;

    procedure Setup; override;
    procedure Finish; override;
    procedure Compute(const FrameTime : CUInt32; var ExitProg : Boolean);
      override;
    procedure Draw; override;
    procedure HandleEvent(const aEvent : TSDL_Event;
      var Handled, ExitProg : Boolean); override;

    constructor Create(const aX, aY : Integer;
      const aFont : caCHXSDL2Font = nil; const aID : string = '';
      const aCaption : string = ''; const aGlyph : PSDL_Texture = nil);
    destructor Destroy; override;
  end;

implementation

{ cCHXSDL2Button }

procedure cCHXSDL2Button.SetGlyph(const aValue : PSDL_Texture);
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

procedure cCHXSDL2Button.CreateCache;
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

procedure cCHXSDL2Button.SetSize;
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

procedure cCHXSDL2Button.SetCaption(const aValue : string);
begin
  if Assigned(Font) and (id <> '') then
    Font.RemoveStatic(ID);

  FCaption := aValue;

  CreateCache;
end;

procedure cCHXSDL2Button.SetFont(const aValue : caCHXSDL2Font);
begin
  if Assigned(Font) and (id <> '') then
    Font.RemoveStatic(ID);

  FFont := aValue;

  CreateCache;
end;

procedure cCHXSDL2Button.Setup;
begin

end;

procedure cCHXSDL2Button.Finish;
begin

end;

procedure cCHXSDL2Button.Compute(const FrameTime : CUInt32;
  var ExitProg : Boolean);
begin

end;

procedure cCHXSDL2Button.Draw;
var
  aColor : cuint;
  x1, y1 : Integer;
  TgtRect : TSDL_Rect;
begin
  if Focused then aColor := HLColor else aColor := BDColor;

  rectangleColor(PRenderer, X, Y, X + Width, Y + Height, aColor);
  boxColor(PRenderer, X + 1, Y + 1, X + Width - 2, Y + Height - 2, BGColor);

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
end;

procedure cCHXSDL2Button.HandleEvent(const aEvent : TSDL_Event; var Handled,
  ExitProg : Boolean);
begin
  inherited HandleEvent(aEvent, Handled, ExitProg);
  if Handled or ExitProg or (not Focused) then  Exit;

  case aEvent.type_ of
    SDL_MOUSEBUTTONDOWN : // (button: TSDL_MouseButtonEvent);
    begin
      if Assigned(OnClick) then
        OnClick(Self);
      Handled := True;
    end;
    SDL_KEYDOWN : // (key: TSDL_KeyboardEvent);
    begin
      case aEvent.key.keysym.sym of
        SDLK_SPACE, SDLK_RETURN :
        begin
          if Assigned(OnClick) then
            OnClick(Self);
          Handled := True;
        end;
      end;
    end;
    else
      ;
  end;
end;

constructor cCHXSDL2Button.Create(const aX, aY : Integer;
  const aFont : caCHXSDL2Font; const aID : string; const aCaption : string;
  const aGlyph : PSDL_Texture);
begin
  inherited Create;

  X := aX;
  Y := aY;
  Height := MinSizeButton; // Min size
  Width := MinSizeButton; // Min size

  FFont := aFont; //FFont to call CreateCache only one time
  ID := aID;
  FCaption := aCaption; //FCaption to call CreateCache only one time
  CreateCache;

  Glyph := aGlyph;

  BGColor := $FF404040;
  BDColor := $FF808080;
  HLColor := $FF00FFFF;
end;


destructor cCHXSDL2Button.Destroy;
begin
  inherited Destroy;
end;

end.
