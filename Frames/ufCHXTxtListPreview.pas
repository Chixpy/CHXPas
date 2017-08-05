unit ufCHXTxtListPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, ufCHXStrLstPreview;

type

  { TfmCHXTxtListPreview }

  TfmCHXTxtListPreview = class(TfmCHXStrLstPreview)
    mText: TMemo;

  private

  protected
    procedure OnCurrItemChange; override;
  public

  end;

implementation

{$R *.lfm}

{ TfmCHXTxtListPreview }

procedure TfmCHXTxtListPreview.OnCurrItemChange;
begin
        if (CurrItem < 1) or (not Assigned(StrList)) or (StrList.Count = 0) then
    begin
      mText.Clear;
      Exit;
    end;

     mText.Lines.LoadFromFile(StrList[CurrItem - 1]);
end;

end.

