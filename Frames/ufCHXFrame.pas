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

  public
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;

    procedure ClearData; virtual; abstract;
    procedure LoadData; virtual; abstract;
    procedure SaveData; virtual; abstract;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXFrame }

procedure TfmCHXFrame.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmCHXFrame.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

constructor TfmCHXFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXFrame.Destroy;
begin
  inherited Destroy;
end;

end.

