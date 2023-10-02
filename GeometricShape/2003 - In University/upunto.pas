unit uPunto;
                               INTERFACE

type

    cPunto = class

        private
           Function GetModulo (X,Y : real): real;

           Function GetAngulo (X,Y : real): real;

           Function GetX (Distancia, Modulo : real): real;

           Function GetY (Distancia, Modulo : real): real;

        public

           X : Real;
           Y : Real;

{
 ******************************************************************************
 constructor crear;
 PRE: Ninguna
 POS: Objeto cPunto creado
 DES: Crea un obj. cPunto con coordenadas (0,0)
 ******************************************************************************
}
           constructor crear; overload;


{
 ******************************************************************************
 constructor crear(Punto:tPunto)
 PRE: Ninguna
 POS: Objeto cPunto creado
 DES: Crea un objeto con las coordenadas del registro
 ******************************************************************************
}
           constructor crear(Punto:cPunto); overload;


{
 ******************************************************************************
 constructor crear(distancia, angulo : real)
 PRE: Ninguna
 POS: Objeto cPunto creado
 DES: Crea un obj. cPunto con las coordenadas polares
 ******************************************************************************
}
           constructor crear(distancia, angulo : real); overload;


{
 ******************************************************************************
 Constructor DeCadena(cadena : String);
 PRE: Ninguna
 POS: Punto Modificado
 DES: LTranslada el punto a paralelo al eje vertical
 ******************************************************************************
}
           Constructor DeCadena(cadena : String);


{
 ******************************************************************************
 Procedure PonerX (X : real)
 PRE: Objeto cPunto creado
 POS: Objeto cPunto modificado
 DES: Cambia la coordenada X del objeto.
 ******************************************************************************
}
           Procedure PonerX (aX : real);


{
 ******************************************************************************
 Procedure PonerY (Y : real)
 PRE: Objeto cPunto creado
 POS: Objeto cPunto modificado
 DES: Cambia la coordenada Y del objeto.
 ******************************************************************************
}
           Procedure PonerY (aY : real);


{
 ******************************************************************************
 Procedure PonerDistacia (Distancia : real)
 PRE: Objeto cPunto creado
 POS: Objeto cPunto modificado
 DES: Asigna al obj. la distancia (coord. Polares)al origen manteniendo el
      ángulo que tenía
 ******************************************************************************
}
           Procedure PonerDistancia (Distancia : real);


{
 ******************************************************************************
 Procedure PonerAngulo (Angulo : real)
 PRE: Objeto cPunto creado
 POS: Objeto cPunto modificado
 DES: Asigna al obj. el angulo (coord. polares) al origen manteniendo el
      la distancia.
 ******************************************************************************
}
           Procedure PonerAngulo (Angulo : real);


{
 ******************************************************************************
 Function ObtenerX : real;
 PRE: Objeto cPunto creado
 POS: Nº Real con el valor de la coordenada X
 DES: Obtiene el valor X del punto.
 ******************************************************************************
}
           Function ObtenerX : real;


{
 ******************************************************************************
 Function ObtenerY : real;
 PRE: Objeto cPunto creado
 POS: Nº Real con el valor de la coordenada Y
 DES: Obtiene el valor Y del punto.
 ******************************************************************************
}
           Function ObtenerY : real;


{
 ******************************************************************************
 Function ObtenerAngulo : real;
 PRE: Objeto cPunto creado
 POS: Nº Real con el valor del angulo (Coord. Polares)
 DES: Obtiene el valor del angulo del punto.
 ******************************************************************************
}
           Function ObtenerAngulo : real;


{
 ******************************************************************************
 Function ObtenerDistancia : real;
 PRE: Objeto cPunto creado
 POS: Nº Real con el valor de la distancia (Coord. Polares)
 DES: Obtiene el valor de la distancia del punto.
 ******************************************************************************
}
           Function ObtenerDistancia : real;


{
 ******************************************************************************
 Procedure Rotar (angulo: real);
 PRE: Objeto cPunto creado
 POS: Punto Modificado
 DES: Rota el punto alrededor del origen
 ******************************************************************************
}
           Procedure Rotar (angulo: real);


{
 ******************************************************************************
 Procedure Trasladarhorizontal (X: real);
 PRE: Objeto cPunto creado
 POS: Punto Modificado
 DES: Translada el punto a paralelo al eje horizontal
 ******************************************************************************
}
           Procedure Trasladarhorizontal (aX: real);


{
 ******************************************************************************
 Procedure TrasladarVertical (Y: real);
 PRE: Objeto cPunto creado
 POS: Punto Modificado
 DES: Translada el punto a paralelo al eje vertical
 ******************************************************************************
}
           Procedure TrasladarVertical (aY: real);


{
 ******************************************************************************
 Procedure Escalar (Escalar: real);
 PRE: Objeto cPunto creado
 POS: Punto Modificado
 DES: Escala el modulo del vector (Polares), lo que llamamos distancia
 ******************************************************************************
}
           Procedure Escalar (Escalar: real);


{
 ******************************************************************************
 Function ACadena : string;
 PRE: Objeto cPunto creado
 POS: String del tipo "(X, Y) [Distancia-Angulo]"
 DES: Devuelve las coordenadas del punto en forma de string
 ******************************************************************************
}
           Function ACadena : string;


           end;


                            IMPLEMENTATION

Function cPunto.GetModulo (X,Y : real): real;
begin
     GetModulo := sqrt(sqr(X) + sqr(Y));
end;


Function cPunto.GetAngulo (X,Y : real): real;
begin

     IF x = 0 THEN
        IF y = 0 THEN

           GetAngulo := 0

        ELSE

           IF y < 0 THEN

              GetAngulo := Pi / 2

           ELSE

              GetAngulo := - Pi / 2
     ELSE

        IF x < 0 THEN

           GetAngulo := ArcTan(Y/X) + Pi

        ELSE

           GetAngulo := ArcTan(Y/X);

end;


Function cPunto.GetX (Distancia, Modulo : real): real;
begin
     GetX := abs(distancia) * cos(modulo);
end;


Function cPunto.GetY (Distancia, Modulo : real): real;
begin
     GetY := abs(distancia) * sin(modulo);
end;



constructor cPunto.crear;
begin
     Self.X := 0;
     Self.Y := 0;
end;


constructor cPunto.crear(Punto: cPunto);
begin
     Self.X := Punto.X;
     Self.Y := Punto.Y;
end;


constructor cPunto.crear(distancia, angulo: Real);
begin
     Self.X := GetX(distancia, angulo);
     Self.Y := GetY(Distancia, angulo);
end;


Constructor cPunto.DeCadena(cadena : String);
var
   Inicio, PosComa, Fin, error : Integer;

Begin

{
     HAY QUE MEJORARLO PARA QUE LEA DECIMALES :-)
}

    Inicio := Pos('(', cadena) + 1;
    Fin := Pos(')', cadena) + 1;
    PosComa := Pos(',', cadena);
    Val(Copy(Cadena, Inicio, PosComa - Inicio),Self.X, error);
    Val(Copy(Cadena, PosComa + 1, Fin - PosComa - 1),Self.Y, error);
end;


Procedure cpunto.PonerX (aX : real);
begin
     self.X := aX;
end;


Procedure cPunto.PonerY (ay : real);
begin
     self.Y := aY;
end;


Procedure cPunto.PonerDistancia (Distancia : real);
var
   angulo: real;
begin
     angulo := GetAngulo(self.X, Self.Y);
     Self.X := GetX(distancia, angulo);
     Self.Y := GetY(distancia, angulo);
end;


Procedure cPunto.PonerAngulo (Angulo : real);
var
   Distancia: real;
begin
     Distancia := GetModulo(self.X, self.Y);
     Self.X := GetX(distancia, angulo);
     Self.Y := GetY(distancia, angulo);
end;


Function cPunto.ObtenerX : real;
begin
     ObtenerX := Self.X;
end;


Function cPunto.ObtenerY : real;
begin
     ObtenerY := Self.Y;
end;


Function cPunto.ObtenerAngulo: real;
begin
     ObtenerAngulo := GetAngulo(self.X, Self.Y);
end;


Function cPunto.ObtenerDistancia: real;
Begin
     ObtenerDistancia := Getmodulo(self.X, self.Y);
End;


Procedure cPunto.Rotar (Angulo : real);
var
   distancia : real;
begin
     angulo := Angulo + GetAngulo(self.X, Self.Y);
     Distancia := GetModulo (self.X, self.Y);
     Self.X := GetX(distancia, angulo);
     Self.Y := GetY(distancia, angulo);
end;


Procedure cPunto.TrasladarHorizontal (aX :Real);
Begin
     Self.X := self.X + aX;
End;


Procedure cPunto.TrasladarVertical (aY :Real);
Begin
     Self.Y := self.Y + aY;
End;


Procedure cPunto.Escalar (Escalar :real);
begin
     Self.X := Self.X * Escalar;
     Self.Y := Self.Y * Escalar;
end;


Function cPunto.ACadena : string;
Var
   aX,aY,Dis,Ang: String;
Begin
     Str(Self.X:0:2, aX);
     Str(Self.Y:0:2, aY);

     Str(GetAngulo(self.X, Self.Y):0:2, Ang);
     Str(GetModulo(self.X, self.Y):0:2, Dis);

     ACadena := '(' + aX + '; ' + aY + ') [' + Dis + '; ' + ang +']';
End;


end.
