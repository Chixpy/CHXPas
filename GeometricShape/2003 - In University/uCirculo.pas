unit uCirculo;
                               INTERFACE
Uses
  uElipse;

CONST
     FIGURASTR = 'CIRCULO';

Type

    cCirculo = Class (cElipse)

             Public

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cCirculo creado
 DES: Crea un obj. cCirculo sin ningun vertice
 ******************************************************************************
}
                   Constructor Crear;


{
 ******************************************************************************
 constructor copiar;
 PRE: objeto cCirculo creado
 POS: Copia del Objeto cCirculo pasado por parametro
 DES: Hace una copia un obj. cCirculo en otro nuevo
 ******************************************************************************
}
                   Constructor Copiar (Figura: cCirculo);

{
 ******************************************************************************
 constructor DeCadena;
 PRE: Cadena con el tipo figura entre corchetes '[' ']' y una serie de puntos
      entre parentesis '(' ')'.
 POS: Un objeto cCirculo con los puntos que hay en la Cadena de caracteres
 DES: Crea un obj. cCirculo a Partir de una cadena de caracteres que tiene la
      siguiente sintaxis:
                [CIRCULO],(x1, y1), (x2, y2), (x3, y3), ....
 ******************************************************************************
}
                   Constructor DeCadena (Cadena : String);


{
 ******************************************************************************
 FUNCION perimetro : Real; override
 PRE: Objeto cCirculo creado.
 POS: Longitud de la circunferencia.
 DES: Redefine la operacion perimetro para calcular la longitud de
      la cicunferencia

 ******************************************************************************
}
                   FUNCTION Perimetro : Real; override;


    End;


                             IMPLEMENTATION


Constructor cCirculo.Crear;
BEGIN

     Inherited Crear;
     TipoFigura := FIGURASTR;

END;



Constructor cCirculo.DeCadena (Cadena : String);
Begin

     Inherited DeCadena (Cadena);

end;



Constructor cCirculo.Copiar(Figura: cCirculo);
BEGIN

     Inherited Copiar(Figura);

END;

FUNCTION cCirculo.Perimetro : Real;
VAR
   Radio : Real;

BEGIN

     {Calculo el radio: He separado la formula en varias lineas}

     Radio := sqr(self.punto[0].X - self.punto[1].X);
     Radio := Radio + sqr(self.punto[0].Y - self.punto[1].Y);
     Radio := sqrt(Radio);

     Perimetro := 2 * Pi * Radio
END;

End.
