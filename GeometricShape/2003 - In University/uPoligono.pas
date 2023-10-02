unit uPoligono;
                               INTERFACE
Uses
  uFCerrada;

CONST
     FIGURASTR = 'POLIGONO';

Type

    cPoligono = Class (cFCerrada)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cPoligono creado
 DES: Crea un obj. cPoligono sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cPoligono creado
 POS: Copia del Objeto cPoligono pasado por parametro
 DES: Hace una copia un obj. cPoligono en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cPoligono);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cPoligono con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cPoligono a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [POLIGONO],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

    End;


                             IMPLEMENTATION


Constructor cPoligono.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cPoligono.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cPoligono.Copiar(Figura: cPoligono);
BEGIN

     Inherited Copiar(Figura);

END;

End.
