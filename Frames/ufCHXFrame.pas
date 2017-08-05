unit ufCHXFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  uCHXStrUtils;

type

  { TfmCHXFrame }

  TfmCHXFrame = class(TFrame)
  private
    FGUIConfigIni: string;
    FGUIIconsIni: string;

  protected
    procedure SetGUIIconsIni(AValue: string); virtual;
    procedure SetGUIConfigIni(AValue: string); virtual;

    procedure ClearFrameData; virtual; abstract;
    //< Clear components.
    procedure LoadFrameData; virtual; abstract;
    //< Update components.
  //  procedure SaveFrameData; virtual; abstract;

  public
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXFrame }

procedure TfmCHXFrame.SetGUIConfigIni(AValue: string);

  procedure SetGUIConfigChildren(const aComponent: TComponent);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).GUIConfigIni := GUIConfigIni;
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        SetGUIConfigChildren(aComponent.Components[i]);
        Inc(i);
      end;
    end;
  end;

var
  i: integer;
begin
  AValue := SetAsFile(AValue);
  if FGUIConfigIni = AValue then
    Exit;
  FGUIConfigIni := AValue;

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    SetGUIConfigChildren(Components[i]);
    Inc(i);
  end;

end;

procedure TfmCHXFrame.SetGUIIconsIni(AValue: string);

  procedure SetGUIIconsChildren(const aComponent: TComponent);
  var
    i: integer;
  begin
    if aComponent is TfmCHXFrame then
    begin
      TfmCHXFrame(aComponent).GUIIconsIni := GUIIconsIni;
      // TfmCHXFrame itself updates its children
    end
    else
    begin
      i := 0;
      while i < aComponent.ComponentCount do
      begin
        // Searching in aComponent childrens
        SetGUIIconsChildren(aComponent.Components[i]);
        Inc(i);
      end;
    end;
  end;

var
  i: integer;
begin
  AValue := SetAsFile(AValue);
  if FGUIIconsIni = AValue then
    Exit;
  FGUIIconsIni := SetAsFile(AValue);

  // Updating all TfmCHXFrame components
  i := 0;
  while i < ComponentCount do
  begin
    SetGUIIconsChildren(Components[i]);
    Inc(i);
  end;

end;

constructor TfmCHXFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Enabled := False; // Created disabled, enable it when ready for use
end;

destructor TfmCHXFrame.Destroy;
begin
  inherited Destroy;
end;

end.
