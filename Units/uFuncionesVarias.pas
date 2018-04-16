unit uFuncionesVarias;
// Miscelanea de funciones de uso general

interface
uses Windows, StdCtrls, Controls, SysUtils, Graphics;

procedure AnadirOpcionComboBox(ComboBox: TCustomComboBox; Cadena: string);
// A�ade una opci�n a la ComboBox especificada comprobando si est�
//   repetida

function AnadirIconoImageList(ListaImagenes: TImageList; Fichero: string):
  Integer;
// A�ade un icono desde un fichero a la lista de im�genes y devuelve la
//   posici�n de dicho icono dentro de la lista.

implementation
uses Forms, StrUtils;

function AnadirIconoImageList(ListaImagenes: TImageList; Fichero: string):
  Integer;
var
  Icono: TIcon;
begin
  Result := -1;
  if ListaImagenes = nil then
    Exit;
  if FileExists(Fichero) then
  begin
    Icono := TIcon.Create;
    try
      Icono.LoadFromFile(Fichero);
      Result := ListaImagenes.AddIcon(Icono);
    except
      Result := -1;
      FreeAndNil(Icono);
    end;
  end
end;

procedure AnadirOpcionComboBox(ComboBox: TCustomComboBox; Cadena: string);
begin
  if (ComboBox = nil) or (Cadena = '') then
    Exit;

  if ComboBox.Items.IndexOf(Cadena) = -1 then
    ComboBox.Items.Add(Cadena);
end;

end.

