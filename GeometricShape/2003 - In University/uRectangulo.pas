unit uRectangulo;
                               INTERFACE
Uses
  uCuadrilatero;

CONST
     FIGURASTR = 'RECTANGULO';

Type

    cRectangulo = Class (cCuadrilatero)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cRectangulo creado
 DES: Crea un obj. cRectangulo sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cRectangulo creado
 POS: Copia del Objeto cRectangulo pasado por parametro
 DES: Hace una copia un obj. cRectangulo en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cRectangulo);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cRectangulo con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cRectangulo a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [RECTANGULO],(x1, y1), (x2, y2), (x3, y3), ....
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
                   FUNCTION Perimetro : Real; override;



    End;


                             IMPLEMENTATION


Constructor cRectangulo.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cRectangulo.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cRectangulo.Copiar(Figura: cRectangulo);
BEGIN

     Inherited Copiar(Figura);

END;



FUNCTION cRectangulo.Perimetro : Real;
VAR
   Lado1, Lado2 : Real;

BEGIN

     {Calculo la longitud del primer lado}

     Lado1 := sqr(self.punto[0].X - self.punto[1].X);
     Lado1 := Lado1 + sqr(self.punto[0].Y - self.punto[1].Y);
     Lado1 := sqrt(lado1);

     {Calculo la longitud del segundo lado}
     Lado2 := sqr(self.punto[1].X - self.punto[2].X);
     Lado2 := Lado2 + sqr(self.punto[1].Y - self.punto[2].Y);
     Lado2 := sqrt(lado2);

     Perimetro := Lado1 * Lado2;
END;


End.
