unit uFCerrada;
                               INTERFACE
Uses
  uFigura;

CONST
     FIGURASTR = 'FCERRADA';

Type

    cFCerrada = Class (cFigura)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cFCerrada creado
 DES: Crea un obj. cFCerrada sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cFCerrada creado
 POS: Copia del Objeto cFCerrada pasado por parametro
 DES: Hace una copia un obj. cFCerrada en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cFCerrada);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cFCerrada con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cFCerrada a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [FCERRADA],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);


{
 ******************************************************************************
 FUNCION Perimetro
 PRE: Objeto FCerrada Creado
 POS: La suma de la longitud de todos sus lados
 DES: Calcula la longitud de todos sus lados teniendo en cuenta que es un
      Poligono Cerrado
 ******************************************************************************
}
                   Function Perimetro : real;Virtual;

    End;


                             IMPLEMENTATION


Constructor cFCerrada.Crear;
BEGIN

     Inherited.Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cFCerrada.DeCadena (Cadena : String);
Begin

     Inherited.DeCadena (Cadena);

end;



Constructor cFCerrada.Copiar(Figura: cFCerrada);
BEGIN

     Inherited.Copiar(Figura);

END;



Function cFCerrada.Perimetro : real; Virtual;
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


     {Falta un lado ...}


     punto1:= self.punto[NPuntos - 1];
     punto2:= self.punto[0];

     Temp := Temp + sqrt(sqr(punto1.X - punto2.X) + sqr(punto1.Y - punto2.Y));


     Perimetro := Temp;
End;

End.
