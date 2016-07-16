unit uListaFiguras;
                              INTERFACE
uses
    uFigura;

const
    Nespacios = 10;

type
    tElemento = cFigura;

    tLista = class
      Private

          FUNCTION Encontrar(Modificador:telemento):LongInt;

          PROCEDURE AmpliarLista;

          PROCEDURE ReducirLista;


      Public

          Dato : Array Of tElemento;
          NElementos :longint; {Numero de elementos o posicion del primer nil}

          Constructor NuevaLista;

          FUNCTION EsVacia: Boolean;

          FUNCTION Primero: telemento;

          PROCEDURE Insertar(elemento : tElemento);

          PROCEDURE Resto;

          PROCEDURE Modificar(elemento : telemento);

          PROCEDURE Borrar(valor : LongInt);

          PROCEDURE BorrarAlt(elemento:telemento);

      end;



                              IMPLEMENTATION



PROCEDURE tLista.AmpliarLista;
begin
     SetLength(Self.Dato, High(Self.Dato) + 1 + NEspacios);
end;



PROCEDURE tLista.ReducirLista;
begin
     SetLength(Self.Dato, High(Self.Dato) + 1 - NEspacios);
end;



FUNCTION tLista.EsVacia : Boolean;
Begin
      If self.Nelementos = 0 Then
         EsVacia := True
      Else
         EsVacia := False;
End;



FUNCTION tLista.Primero: tElemento;

Begin
     iF Self.Nelementos > 0 then
        Primero := self.dato[self.Nelementos - 1]
     else
        Primero := nil;
End;



Constructor tLista.NuevaLista;
Begin
      self.Nelementos:= 0;
End;




PROCEDURE tLista.Insertar(elemento : tElemento);
Begin

     If self.Nelementos mod NEspacios = 0 then
        ampliarlista;
     self.Dato[self.Nelementos] := Elemento;
     self.Nelementos := self.Nelementos + 1;
End;




PROCEDURE tLista.Resto;
Begin
     If self.NElementos > 0 then
     begin
	self.NElementos := Self.NElementos - 1;
	self.Dato[self.Nelementos] := nil;
	If self.NElementos mod NEspacios = 0 then
		ReducirLista;
     End

End;




PROCEDURE tlista.Modificar(elemento:telemento);
Begin
      resto;
      insertar(elemento);

End;




PROCEDURE tlista.Borrar(valor:longint);
Var Contador : LongInt;

Begin
      if Valor <= Self.Nelementos then
      Begin
      For Contador := Valor-1 to self.NElementos - 2 Do
      Self.dato[contador]:= self.dato[contador +1];

      self.NElementos := Self.NElementos -1;
      Self.dato[self.NElementos]:= nil;
      If self.NElementos mod NEspacios = 0 then
         ReducirLista;
      End;
End;



PROCEDURE tlista.borraralt(elemento:telemento);
var busqueda : longint;
Begin
     busqueda := encontrar(elemento);
     if busqueda <> -1 then
        borrar(busqueda + 1);

End;



FUNCTION tlista.encontrar(modificador:telemento):longint;
var barrido, maximo: longint; Encontrado:Boolean;
Begin
      Encontrado := False;
      Maximo:= self.Nelementos;
      Barrido:= 0;
      Repeat
        If Maximo = Barrido Then
          Encontrado:= True
        else
           If (modificador.acadena = self.Dato[Barrido].acadena)  Then
               Encontrado:=True
           Else
              Barrido:= Barrido +1;
      Until Encontrado=true;
      If Maximo <> Barrido then
         encontrar:= Barrido
      Else
          Encontrar := -1;

End;

end.


