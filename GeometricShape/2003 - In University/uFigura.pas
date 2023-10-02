unit uFigura;
                               INTERFACE
uses uPunto;

CONST
     FIGURASTR = 'FABIERTA';

type

    cFigura = class
	Public

           Punto : Array of cpunto;
	      NPuntos : Integer;
           TipoFigura : String;

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cFigura creado
 DES: Crea un obj. cFigura sin ningun vertice
 ******************************************************************************
}
	      Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cFigura creado
 POS: Copia del Objeto cFigura pasado por parametro
 DES: Hace una copia un obj. cFigura en otro nuevo
 ******************************************************************************
}
	      Constructor Copiar(Figura : cFigura);


{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cFigura con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cFigura a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [nombre_figura],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
	      Constructor DeCadena (Cadena : String);


{
 ******************************************************************************
 FUNCION ACadena;
 PRE: objeto cFigura creado
 POS: String con la siguiente sintaxis:
            [nombre_figura],(x1, y1), (x2, y2), (x3, y3), ....
 DES: Devuelve una cadena con la anterior sintaxis con los puntos del objeto
 ******************************************************************************
}
	      Function ACadena : String;


{
 ******************************************************************************
 PROCEDIMIENTO Rotar;
 PRE: objeto cFigura creado y un angulo (radianes)
 POS: objeto cFigura rotado el angulo anterior
 DES: Rota la figura el angulo pasado como parametro, el centro de rotacion
      es el origen de coordenadas (0, 0). Ejemplo:

      [TRIANGULO], (2, 5), (-1, 4), (6, 7), .... rotado pi radianes quedaria:
      [TRIANGULO], (-2, -5), (1, -4), (-6, -7)

 ******************************************************************************
}
	      Procedure Rotar (Angulo: Real);


{
 ****************************************************************************
 PROCEDIMIENTO Escalar;
 PRE: Objeto cFigura creado y un factor de escala
 POS: Objeto cFigura escalado
 DES: Escala la figura el factor pasado como parametro.
      ATENCION: Si el objeto no est  centrado en el origen habr  un
                desplazamiento de la figura ya que no se calcula un origen
                'local' para comenzar la escala.

      [TRIANGULO], (2, 5), (-1, 4), (6, 7), .... escalado por 3:
      [TRIANGULO], (6, 15), (-3, 12), (18, 21)

      Una escala de -1 dar¡a como resultado la simetrica respecto al origen

 ****************************************************************************
}
	      Procedure Escalar (Factor : Real);



{
 ****************************************************************************
 PROCEDIMIENTO TrasladarHorizontal;
 PRE: Objeto cFigura creado y catidad de unidades de traslado
 POS: Objeto cFigura trasladado el numero de unidades especificado
 DES: Translada la figura HORIZONTALEMENTE un numero de unidades especificado
 ****************************************************************************
}
	      Procedure TrasladarHorizontal (Valor : Real);


{
 ****************************************************************************
 PROCEDIMIENTO TrasladarVertical
 PRE: Objeto cFigura creado y catidad de unidades de traslado
 POS: Objeto cFigura trasladado el numero de unidades especificado
 DES: Translada la figura VERTICALMENTE un numero de unidades especificado
 ****************************************************************************
}
	      Procedure Trasladarvertical (Valor : Real);
	 End;


                             IMPLEMENTATION
Constructor cFigura.Crear;
begin

	SetLength(self.Punto, 1);
	Self.NPuntos := 0;
     Self.TipoFigura := FIGURASTR;

end;


Constructor cFigura.Copiar(Figura : cFigura);
VAR
   Cont : LongInt;
begin


     {Copio cada punto del array}
     FOR Cont := 0 TO (Figura.NPuntos - 1) DO
         Self.Punto[Cont] := cPunto.crear(Figura.Punto[Cont]);

     Self.NPuntos := Figura.NPuntos;
     Self.TipoFigura := Figura.TipoFigura;
end;



Constructor cFigura.DeCadena (Cadena : String);
var
   Cont, Inicio, Fin : Integer;
Begin

    {Guardo el TipoFigura}
    Inicio := Pos('[', cadena) + 1;
    Fin := Pos(']', cadena);
    Self.Tipofigura := Copy(Cadena, Inicio, Fin - Inicio);


    {Guardamos cada punto en el array}
    Cont := 0;
    While Pos('(', cadena) <> 0 DO
    Begin

	 Self.Punto[Cont] := cPunto.deCadena(Cadena);


      {Borro el punto mas a la izq del string}
      Inicio := Pos(')', cadena) + 1;
      Fin := length(Cadena) - (Pos(')', cadena) + 1);
      Cadena := Copy(Cadena, Inicio, Fin);

      Cont := Cont + 1;

    END;

    Self.NPuntos := Cont;

end;


Function cFigura.ACadena : String;
Var cont : Integer;
    Temp, X, Y: string;

begin

     Temp:= '[' + Self.TipoFigura + ']';

     For cont := 0 to Self.NPuntos - 1 DO
     Begin
          Str(Self.Punto[cont].x:0:2, X);
          Str(Self.Punto[cont].Y:0:2, Y);
          Temp := Temp + ', (' + X + ',' + Y + ')';
     End;

     ACadena := Temp;
end;


Procedure cFigura.Rotar (Angulo: Real);
Var cont : Integer;
begin
     For cont := 0 to Self.NPuntos - 1 DO
	  Self.Punto[Cont].Rotar(Angulo);
end;


Procedure cFigura.Escalar (Factor : Real);
Var cont : Integer;
begin
     For cont := 0 to Self.NPuntos - 1 DO
	  Self.Punto[Cont].Escalar(Factor);
end;


Procedure cFigura.TrasladarHorizontal (Valor : Real);
Var cont : Integer;
begin
     For cont := 0 to Self.NPuntos - 1 DO
	  Self.Punto[Cont].TrasladarHorizontal(Valor);

end;


Procedure cFigura.TrasladarVertical (Valor : Real);
Var cont : Integer;
begin
     For cont := 0 to Self.NPuntos - 1 do
	  Self.Punto[Cont].TrasladarVertical(Valor);
end;
End.
