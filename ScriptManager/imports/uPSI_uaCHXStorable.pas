unit uPSI_uaCHXStorable;

{< caCHXStorable, caCHXStorableIni and caCHXStorableTxt import for
    Pascal Script.

  Copyright (C) 2019-2023 Chixpy

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
  Classes, SysUtils, uPSComponent, uPSRuntime, uPSCompiler,
  uaCHXStorable;

type

  { TPSImport_uaCHXStorable }

  TPSImport_uaCHXStorable = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


procedure SIRegister_caCHXStorable(CL: TPSPascalCompiler);
procedure SIRegister_caCHXStorableIni(CL: TPSPascalCompiler);
procedure SIRegister_caCHXStorableTxt(CL: TPSPascalCompiler);
procedure SIRegister_uaCHXStorable(CL: TPSPascalCompiler);

procedure RIRegister_caCHXStorable(CL: TPSRuntimeClassImporter);
procedure RIRegister_caCHXStorableIni(CL: TPSRuntimeClassImporter);
procedure RIRegister_caCHXStorableTxt(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaCHXStorable(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaCHXStorable]);
end;

procedure SIRegister_caCHXStorable(CL: TPSPascalCompiler);
begin
  // with RegClassS(CL,'TComponent', 'caCHXStorable') do
  with CL.AddClassN(CL.FindClass('TComponent'), 'caCHXStorable') do
  begin
    RegisterProperty('DefaultFileName', 'string', iptrw);
  end;
end;

procedure SIRegister_caCHXStorableIni(CL: TPSPascalCompiler);
begin
  // with RegClassS(CL,'caCHXStorable', 'caCHXStorableIni') do
  with CL.AddClassN(CL.FindClass('caCHXStorable'), 'caCHXStorableIni') do
  begin

  end;
end;

procedure SIRegister_caCHXStorableTxt(CL: TPSPascalCompiler);
begin
  // with RegClassS(CL,'caCHXStorable', 'caCHXStorableTxt') do
  with CL.AddClassN(CL.FindClass('caCHXStorable'), 'caCHXStorableTxt') do
  begin
    RegisterProperty('CommaText', 'string', iptrw);
  end;
end;

procedure SIRegister_uaCHXStorable(CL: TPSPascalCompiler);
begin
  SIRegister_caCHXStorable(CL);
  SIRegister_caCHXStorableIni(CL);
  SIRegister_caCHXStorableTxt(CL);
end;

procedure caCHXStorableDefaultFileName_R(Self: caCHXStorable; var T: string);
begin
  T := Self.DefaultFileName;
end;

procedure caCHXStorableDefaultFileName_W(Self: caCHXStorable; const T: string);
begin
  Self.DefaultFileName := T;
end;

procedure RIRegister_caCHXStorable(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caCHXStorable) do
  begin
    RegisterPropertyHelper(@caCHXStorableDefaultFileName_R,
      @caCHXStorableDefaultFileName_W, 'DefaultFileName');
  end;
end;

procedure RIRegister_caCHXStorableIni(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caCHXStorableIni) do
  begin

  end;
end;

procedure caCHXStorableTxtCommaText_R(Self: caCHXStorableTxt; var T: string);
begin
  T := Self.CommaText;
end;

procedure caCHXStorableTxtCommaText_W(Self: caCHXStorableTxt; const T: string);
begin
  Self.CommaText := T;
end;

procedure RIRegister_caCHXStorableTxt(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caCHXStorableTxt) do
  begin
    RegisterPropertyHelper(@caCHXStorableTxtCommaText_R,
      @caCHXStorableTxtCommaText_W, 'CommaText');
  end;
end;

procedure RIRegister_uaCHXStorable(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caCHXStorable(CL);
  RIRegister_caCHXStorableIni(CL);
  RIRegister_caCHXStorableTxt(CL);
end;

{ TPSImport_uaCHXStorable }

procedure TPSImport_uaCHXStorable.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaCHXStorable(CompExec.comp);
end;

procedure TPSImport_uaCHXStorable.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaCHXStorable(ri);
end;

end.
