unit uSegmento;
                               INTERFACE
Uses
  uFAbierta;

CONST
     FIGURASTR = 'SEGMENTO';

Type

    cSegmento = Class (cFAbierta)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cSegmento creado
 DES: Crea un obj. cSegmento sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cSegmento creado
 POS: Copia del Objeto cSegmento pasado por parametro
 DES: Hace una copia un obj. cSegmento en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cSegmento);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cSegmento con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cSegmento a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [SEGMENTO],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

    End;


                             IMPLEMENTATION


Constructor cSegmento.Crear;
BEGIN

     Inherited.Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cSegmento.DeCadena (Cadena : String);
Begin

     Inherited.DeCadena (Cadena);

end;



Constructor cSegmento.Copiar(Figura: cSegmento);
BEGIN

     Inherited.Copiar(Figura);

END;

End.
