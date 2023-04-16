unit uPSI_FPCSysUtils;

{< SysUtils unit for Pascal Script.

  Copyright (C) 2020-2020 Chixpy

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
  Classes, SysUtils, uPSRuntime, uPSCompiler;

{ compile-time registration functions }
procedure SIRegister_FPCSysUtils(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_FPCSysUtils_Routines(S: TPSExec);

implementation

// Helper functions
function CHXBoolToStr(B: boolean): string;
begin
  Result := BoolToStr(B, True);
end;

procedure SIRegister_FPCSysUtils(CL: TPSPascalCompiler);
begin
  // -----------
  // SYSSTR.INC
  // -----------

  // Resource Strings
  // ----------------

  // Constants
  // ---------
  //   Ejemplo
  //   CL.AddConstantN('kCHXSHA1Empty', 'TSHA1Digest').SetString(kCHXSHA1Empty);

  // Types
  // -----
  //   Ejemplo
  //   CL.AddTypeS('TItFolderObj', 'String');

  // Methods
  // -------

  CL.AddDelphiFunction('function UpperCase(const s: string): string;');
  CL.AddDelphiFunction('function LowerCase(const s: string): string;');
  CL.AddDelphiFunction(
    'function CompareStr(const S1, S2: string): Integer;');
  CL.AddDelphiFunction(
    'function CompareText(const S1, S2: string): Integer;');
  CL.AddDelphiFunction('function SameText(const s1,s2: string):Boolean;');
  CL.AddDelphiFunction('function SameStr(const s1,s2: string):Boolean;');


  CL.AddDelphiFunction('function StrToBool(const S: string): Boolean;');
  CL.AddDelphiFunction('function BoolToStr(B: Boolean): string;');
  CL.AddDelphiFunction('function StrToBoolDef(const S: string; Default: Boolean): Boolean;');
  CL.AddDelphiFunction('function TryStrToBool(const S: string; out Value: Boolean): Boolean;');
end;

procedure RIRegister_FPCSysUtils_Routines(S: TPSExec);
begin
  // -----------
  // SYSSTR.INC
  // -----------

  S.RegisterDelphiFunction(@AnsiUpperCase, 'UpperCase', cdRegister);
  S.RegisterDelphiFunction(@AnsiLowerCase, 'LowerCase', cdRegister);
  S.RegisterDelphiFunction(@AnsiCompareStr, 'CompareStr', cdRegister);
  S.RegisterDelphiFunction(@AnsiCompareText, 'CompareText', cdRegister);
  S.RegisterDelphiFunction(@AnsiSameText, 'SameText', cdRegister);
  S.RegisterDelphiFunction(@AnsiSameStr, 'SameStr', cdRegister);

  S.RegisterDelphiFunction(@StrToBool, 'StrToBool', cdRegister);
  S.RegisterDelphiFunction(@CHXBoolToStr, 'BoolToStr', cdRegister);
  S.RegisterDelphiFunction(@StrToBoolDef, 'StrToBoolDef', cdRegister);
  S.RegisterDelphiFunction(@TryStrToBool, 'TryStrToBool', cdRegister);
end;

end.
