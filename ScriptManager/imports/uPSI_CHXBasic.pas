{ Basic function for Pascal Script.

  Copyright (C) 2011-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
unit uPSI_CHXBasic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uPSRuntime, uPSCompiler;

  { compile-time registration functions }
procedure SIRegister_CHXBasic(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_CHXBasic_Routines(S: TPSExec);

implementation

procedure SIRegister_CHXBasic(CL: TPSPascalCompiler);
begin
   // CL.AddDelphiFunction('function XXX;');

  // Basic types
  {$ifdef CPU64}
  CL.AddTypeS('SizeInt', 'Int64');
  //CL.AddTypeS('SizeUInt', 'QWord');
  CL.AddTypeS('SizeUInt', 'Int64');
  {$endif CPU64}
  {$ifdef CPU32}
  CL.AddTypeS('SizeInt','Longint');
  CL.AddTypeS('SizeUInt','DWord');
  {$endif CPU32}
  {$ifdef CPU16}
  CL.AddTypeS('SizeInt','Integer');
  CL.AddTypeS('SizeUInt','Word');
  {$endif CPU32}

end;

procedure RIRegister_CHXBasic_Routines(S: TPSExec);
begin
 // S.RegisterDelphiFunction(@XXX, 'XXX', cdRegister);

end;

end.

