Unit uFicheroIni;
{
  AUTOR: Chixpy

  VERSIÓN: 1.0u

  USO
  ---
    -

  CARACTERISTICAS:
  ----------------
    - Desciende de cComIniFile para leer fichero de texto del tipo INI.
    - Elimina el texto que se encuentra detrás de uCOMINIFILE_SIMBOLO_COMENTARIO
    - En la fechas cambia los simbolos usados para la separacion ','; '.' y '-'
      por '/', para que las pueda leer
    - Elimina los simbolos de entrecomillado con SIMBOLO_COMILLAS. Es util
      tener entrecomillados los valores que quieren guardar espacios antes o
      despues del valor.

  POSIBLES MEJORAS:
  -----------------
    - QUITAR LA SENSIBILIDAD A LAS MAYÚSCULAS.
    - MEJORAR LA ESCRITURA... PERO COMO TODAVIA NO LO NECESITO..
    - Que reescriba el comentario original que habia en una linea de datos.
    - Escribir los valores string entre comillas, para que se guarden los
      espacios antes o despues de un valor, pero... poder también escribirlos
      sin ellas, debido a que en algunos sitios el MUGEN no lo reconoce
      con ellas. ¿¡o_O!?
    - Añadir sporte a los comentarios con inicio y final en una misma linea
      (no es necesario para el OMMUGENM, pero seria para completar la clase).

  ACTUALIZACIONES:
  ----------------
    12/04/2007 - 1.0u
      - Adaptando a Lazarus/FPC

    28/03/2006 - 1.0
      - Rescatada de la version para DelphiX, en la que la habia rechazado
        por hacer yo una unidad propia compatible con uCargador

  LICENCIA
  --------
  Copyright (C) Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
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

Interface
Uses IniFiles;
Type
  // Fichero .INI que tiene comentarios

  { cFicheroIni }

  cFicheroIni = Class(TMemIniFile)
  Private

  Protected
    function EliminarComentarios(Cadena: String): String;

  Public
    SimboloComentario: Char;
    SimboloComillas: Char;

    Function ReadString(Const Section, Ident, Default: String): String;
      Override;
    Function ReadDate(Const Section, Name: String; Default:
      TDateTime): TDateTime; Override;

    Constructor Create(Const _FileName: String);
    Destructor Destroy; Override;

  End;

Implementation
Uses SysUtils, strutils;

Function cFicheroIni.EliminarComentarios (Cadena: String): String;
Var
  Acopiar: Integer;
Begin
  Acopiar := AnsiPos(AnsiUpperCase(SimboloComentario), AnsiUpperCase(Cadena))
    - 1;
  If Acopiar <= 0 Then Acopiar := Maxint;
  Result := Trim(Copy(Cadena, 0, Acopiar));
End;

Function cFicheroIni.ReadString(Const Section, Ident, Default: String):
  String;

Begin
  Result := Inherited ReadString(Section, Ident, Default);
  Result := EliminarComentarios(Result);
  If Length(Result) > 0 then
    If (Result[1] = SimboloComillas) and
      (Result[Length(Result)] = SimboloComillas)
    then
      Result := Copy(Result, 2, (Length(Result) - 2));
End;

Function cFicheroIni.ReadDate(Const Section, Name: String; Default:
  TDateTime): TDateTime;

  Function ReemplazarSeparadoresFecha(Cadena: String): String;
  Begin
    Cadena := AnsiReplaceStr(Cadena, '-', '/');
    Cadena := AnsiReplaceStr(Cadena, ',', '/');
    Cadena := AnsiReplaceStr(Cadena, '.', '/');
    Result := Cadena;
  End;

Var
  DateStr: String;
Begin
  DateStr := ReemplazarSeparadoresFecha(ReadString(Section, Name, ''));
  Result := Default;
  If DateStr <> '' Then
  Try
    Result := StrToDate(DateStr);
  Except
    On EConvertError Do
  Else
    Raise;
  End;
End;

Constructor cFicheroIni.Create(Const _FileName: String);
Begin
  Inherited Create(_FileName);
  SimboloComentario := ';';
  SimboloComillas := '"';
End;

Destructor cFicheroIni.Destroy;
Begin
  Inherited Destroy;
End;

End.

