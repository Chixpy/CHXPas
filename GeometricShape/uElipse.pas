unit uElipse;
                               INTERFACE
Uses
  uFCerrada;

CONST
     FIGURASTR = 'ELIPSE';

Type

    cElipse = Class (cFCerrada)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cElipse creado
 DES: Crea un obj. cElipse sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cElipse creado
 POS: Copia del Objeto cElipse pasado por parametro
 DES: Hace una copia un obj. cElipse en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cElipse);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cElipse con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cElipse a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [ELIPSE],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

    End;


                             IMPLEMENTATION


Constructor cElipse.Crear;
BEGIN

     Inherited.Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cElipse.DeCadena (Cadena : String);
Begin

     Inherited.DeCadena (Cadena);

end;



Constructor cElipse.Copiar(Figura: cElipse);
BEGIN

     Inherited.Copiar(Figura);

END;

End.
