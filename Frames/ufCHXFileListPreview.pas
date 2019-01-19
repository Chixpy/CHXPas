unit ufCHXFileListPreview;
{< TfmCHXFileListPreview frame unit.

  Copyright (C) 2017-2019 Chixpy

  This source is free software; you can redistribute it and/or modify it under
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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  // CHX frames
  ufCHXListPreview;

type

  { TfmCHXFileListPreview }

  TfmCHXFileListPreview = class(TfmCHXListPreview, IFPObserver)
  private
    FFileList: TStrings;
    procedure SetFileList(AValue: TStrings);

  public
    property FileList: TStrings read FFileList write SetFileList;
    {< File list. }

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {< IFPObserver callback. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCHXFileListPreview }

procedure TfmCHXFileListPreview.SetFileList(AValue: TStrings);
begin
  if FFileList = AValue then
    Exit;

  if Assigned(FileList) then
    FileList.FPODetachObserver(self);

  FFileList := AValue;

  if Assigned(FileList) then
  begin
    FileList.FPOAttachObserver(self);
    ItemCount := FileList.Count;
  end
  else
    ItemCount := 0;
end;

procedure TfmCHXFileListPreview.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FileList then
    case Operation of
      ooFree: FileList := nil;
      else
      begin
        ItemCount := FileList.Count;
      end;
    end;
end;

constructor TfmCHXFileListPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCHXFileListPreview.Destroy;
begin
  if Assigned(FileList) then
    FileList.FPODetachObserver(self);
  inherited Destroy;
end;

end.
