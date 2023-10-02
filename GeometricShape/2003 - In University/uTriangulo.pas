unit uTriangulo;
                               INTERFACE
Uses
  uPoligono;

CONST
     FIGURASTR = 'TRIANGULO';

Type

    cTriangulo = Class (cPoligono)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cTriangulo creado
 DES: Crea un obj. cTriangulo sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cTriangulo creado
 POS: Copia del Objeto cTriangulo pasado por parametro
 DES: Hace una copia un obj. cTriangulo en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cTriangulo);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cTriangulo con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cTriangulo a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [TRIANGULO],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

    End;


                             IMPLEMENTATION


Constructor cTriangulo.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cTriangulo.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cTriangulo.Copiar(Figura: cTriangulo);
BEGIN

     Inherited Copiar(Figura);

END;

End.
