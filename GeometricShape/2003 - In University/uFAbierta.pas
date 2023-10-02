unit uFAbierta;
                               INTERFACE
Uses
  uFigura, uPunto;

CONST
     FIGURASTR = 'FABIERTA';

Type

    cFAbierta = Class (cFigura)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cFAbierta creado
 DES: Crea un obj. cFAbierta sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cFAbierta creado
 POS: Copia del Objeto cFAbierta pasado por parametro
 DES: Hace una copia un obj. cFAbierta en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cFAbierta);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cFAbierta con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cFAbierta a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [FABIERTA],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);


{
 ******************************************************************************
 FUNCION Longitud
 PRE: Objeto FAbierta Creado
 POS: La suma de la longitud de todos sus lados
 DES: Calcula la longitud de todos sus lados teniendo en cuenta que es un
      Poligono Abierto
 ******************************************************************************
}

                   Function Longitud : real;

    End;


                             IMPLEMENTATION


Constructor cFAbierta.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cFAbierta.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cFAbierta.Copiar(Figura: cFAbierta);
BEGIN

     Inherited Copiar(Figura);

END;



Function cFAbierta.longitud : real;
Var
   Temp : real;
   Contador: Integer;
   Punto1, punto2 : Cpunto;
Begin

     Temp := 0;

     FOR Contador := 0 TO (Self.NPuntos - 2) DO
     BEGIN
          punto1:= self.punto[Contador];
          punto2:= self.punto[Contador + 1];


          {Usamos pitagoras para obtener la distacia entre dos puntos:
           La expresion :

                sqrt(sqr(punto1.X - punto2.X) + sqr(punto1.Y - punto2.Y)

           Equivale a
                      _______________________
                     /         2            2   Que es la distancia entre
                   \/ (X1 - X2)  + (Y1 - Y2)    dos puntos
          }
          Temp := Temp + sqrt(sqr(punto1.X - punto2.X) + sqr(punto1.Y - punto2.Y));
     END;

     Longitud := Temp;
End;

End.
