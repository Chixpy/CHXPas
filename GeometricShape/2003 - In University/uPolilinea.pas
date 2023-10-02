unit uPolilinea;
                               INTERFACE
Uses
  uFAbierta;

CONST
     FIGURASTR = 'POLILINEA';

Type

    cPolilinea = Class (cFAbierta)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cPolilinea creado
 DES: Crea un obj. cPolilinea sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cPolilinea creado
 POS: Copia del Objeto cPolilinea pasado por parametro
 DES: Hace una copia un obj. cPolilinea en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cPolilinea);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cPolilinea con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cPolilinea a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [POLILINEA],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

    End;


                             IMPLEMENTATION


Constructor cPolilinea.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cPolilinea.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cPolilinea.Copiar(Figura: cPolilinea);
BEGIN

     Inherited Copiar(Figura);

END;

End.
