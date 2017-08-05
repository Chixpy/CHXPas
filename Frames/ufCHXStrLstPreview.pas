unit ufCHXStrLstPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
    ufCHXListPreview;

type

  { TfmCHXStrLstPreview }

  TfmCHXStrLstPreview = class(TfmCHXListPreview)
  private
    FStrList: TStrings;
    procedure SetStrList(AValue: TStrings);

  public
    property StrList: TStrings read FStrList write SetStrList;

  end;

implementation

{$R *.lfm}

{ TfmCHXStrLstPreview }

procedure TfmCHXStrLstPreview.SetStrList(AValue: TStrings);
begin
  if FStrList = AValue then Exit;
  FStrList := AValue;

  if Assigned(StrList) then
    ItemCount := StrList.Count
  else
    ItemCount := 0;
end;

end.

