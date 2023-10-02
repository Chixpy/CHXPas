unit uCuadrilatero;
                               INTERFACE
Uses
  uPoligono;

CONST
     FIGURASTR = 'CUADRILATERO';

Type

    cCuadrilatero = Class (cPoligono)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cCuadrilatero creado
 DES: Crea un obj. cCuadrilatero sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cCuadrilatero creado
 POS: Copia del Objeto cCuadrilatero pasado por parametro
 DES: Hace una copia un obj. cCuadrilatero en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cCuadrilatero);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cCuadrilatero con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cCuadrilatero a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [CUADRILATERO],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);

{
 ******************************************************************************
 FUNCION Diagonal
 PRE: Objeto FCaudrilatero
 POS: Longitud de la diagonal
 DES: Calcula la longitud de las diagonales del cuadrilatero

 ******************************************************************************
}
                   Function Diagonal : real;


    End;


                             IMPLEMENTATION


Constructor cCuadrilatero.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cCuadrilatero.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cCuadrilatero.Copiar(Figura: cCuadrilatero);
BEGIN

     Inherited Copiar(Figura);

END;


Function cCuadrilatero.Diagonal : real;
VAR
   Temp : Real;

BEGIN

     {Calculo la diagonal He separado la formula en varias lineas}

     Temp := sqr(self.punto[0].X - self.punto[2].X);
     Temp := Temp + sqr(self.punto[0].Y - self.punto[2].Y);
     Temp := sqrt(Temp);

     Diagonal := Temp;

END;


End.
