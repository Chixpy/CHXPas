unit uCHXConst;

{< Copyright (C) 2011-2020 Chixpy

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

const
  kLinuxDirSeparator = '/';
  kWinDirSeparator = '\';

  kCUUTF8Delimiters: set of char =
    [#0..#127] - ['a'..'z', 'A'..'Z', '1'..'9', '0'];
  {< WordDelimiters except utf8 bit mask (Dirty way ^_^) }

  krsFmtApplicationTitle = '%0:s %1:s';
  {< Application title used in forms.

     @param(%0:s Application name).
     @param(%1:s Version).
  }

  krsFmtWindowCaption = '%0:s: %1:s';
  {< Window caption format
    %0:s = Application.Title (derived from krsFmtApplicationTitle).
    %1:s = Window caption.
  }

  krsLocaleFolder = 'locale';


type

  TCHXStrObjCB = procedure(aString: string) of object;

implementation

end.
