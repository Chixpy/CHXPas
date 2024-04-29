unit uaCHXConfig;
{< caCHXConfig abstract class unit.

  (C) 2006-2019 Chixpy https://github.com/Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  // CHX abstracts
  uaCHXStorable;

type

  { caCHXConfig }

  caCHXConfig = class(caCHXStorableIni)
  public
    procedure ResetDefaultConfig; virtual; abstract;
    {< Sets config properties to default values. }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;
  {< Abstract config holder class. }

implementation

constructor caCHXConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ResetDefaultConfig;
end;

destructor caCHXConfig.Destroy;
begin
  inherited Destroy;
end;

end.
{
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
