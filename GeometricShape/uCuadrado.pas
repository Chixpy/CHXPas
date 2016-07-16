unit uCuadrado;
                               INTERFACE
Uses
  uRectangulo;

CONST
     FIGURASTR = 'CUADRADO';

Type

    cCuadrado = Class (cRectangulo)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cCuadrado creado
 DES: Crea un obj. cCuadrado sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cCuadrado creado
 POS: Copia del Objeto cCuadrado pasado por parametro
 DES: Hace una copia un obj. cCuadrado en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cCuadrado);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cCuadrado con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cCuadrado a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [CUADRADO],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

{
 ******************************************************************************
 FUNCION perimetro : Real; override
 PRE: Objeto cCirculo creado.
 POS: Longitud del Perimetro del reactangulo
 DES: Redefine la operacion perimetro para calcular la longitud del
      perimetro del rectangulo.

 ******************************************************************************
}
                   FUNCTION Perimetro : Real; override

    End;


                             IMPLEMENTATION


Constructor cCuadrado.Crear;
BEGIN

     Inherited.Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cCuadrado.DeCadena (Cadena : String);
Begin

     Inherited.DeCadena (Cadena);

end;



Constructor cCuadrado.Copiar(Figura: cCuadrado);
BEGIN

     Inherited.Copiar(Figura);

END;


FUNCTION cRectangulo.Perimetro : Real; override;
VAR
   Lado1, Lado2 : Real;

BEGIN

     {Calculo la longitud del primer lado}

     Lado1 := sqr(self.punto[0].X - self.punto[1].X);
     Lado1 := Lado1 + sqr(self.punto[0].Y - self.punto[1].Y);
     Lado1 := sqrt(lado1);

     Perimetro := Lado1 * 4;
END;


End.
