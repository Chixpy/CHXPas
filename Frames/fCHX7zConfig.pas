unit fCHX7zConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, u7zWrapper;

type

  { TfmCHX7zConfig }

  TfmCHX7zConfig = class(TFrame)
    e7zGExePath: TFileNameEdit;
    e7zExePath: TFileNameEdit;
    l7zGExePath: TLabel;
    l7zExePath: TLabel;

  private
    { private declarations }


  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published

  end;

implementation

{$R *.lfm}

{ TfmCHX7zConfig }

constructor TfmCHX7zConfig.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  e7zExePath.Text := w7zPathTo7zexe;
  e7zGExePath.Text := w7zPathTo7zGexe;
end;

destructor TfmCHX7zConfig.Destroy;
begin
  inherited Destroy;
end;

end.

