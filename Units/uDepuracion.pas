Unit udepuracion;
{$DEFINE Depuracion}
{$DEBUGINFO OFF}
{
  AUTOR: Chixpy

  VERSIÓN: 1

  USO
  ---
    Procedure EscribirLOG(Mensajero: TObject; Mensaje: String; Tipo: Byte = 5);
    Procedure EscribirLOG(Mensajero: String; Mensaje: String; Tipo: Byte = 5);
    Procedure EscribirLOGFmt(Mensajero: TObject; Mensaje: String;
      Const Args: Array  Of Const; Tipo: Byte = 5);
    Procedure EscribirLOGFmt(Mensajero: String; Mensaje: String;
      Const Args: Array Of Const; Tipo: Byte = 5);

    PARÁMETROS
      Mensajero: Objeto que quiere enviar el mensaje en caso de que se pase una
        String seria equivalente a una 'Seccion'
      Mensaje: Texto que se quiere escribir. En caso de los procedimientos Fmt
        es equivalente al primer parametro de 'Format'
      Tipo: Tipo de mensaje:
        uDError -> Errores
        uDAtencion -> Atenciones, Errores Ignorables, Evitables
        uDCrear -> Creacion de Objetos
        uDDestruir -> Destruccion de Objetos
        uDAccion -> Acciones Generales
        uDDetalle -> Acciones Detalladas (Por defecto)
        uDDepuracion -> Datos para la Depuracion
      Args: (solo Fmt) Parametro 'Args' de la funcion 'Format'

  NOTA: Cuidado con enviar mensajes que contengan los caracteres comillas (")
    ya que WrapText no inserta saltos de linea dentro de un par de esos
    caracteres, aunque por otra parte esta bien si no de quiere que se
    separen dos palabras...

  CONSEJO: Para desactivar la depuración desde el programa basta con poner:
    UDEPURACION_NIVELDETALLE := uDAtencion;  // Errores y Warnings
    o
    UDEPURACION_NIVELDETALLE := uDError;  // Solo Errores

  CARACTERISTICAS:
  ----------------
    - Funciones para escribir dinamicamente el mensaje que se quiera y de
      forma jerarquizada respecto a su emisor.
    - Posibilidad de cambiar dinamicamente:
      - Nivel de detalle con el que se va a escribir.
      - El ancho de la linea.
      - Tamaño de la Sangria
      - Escribir en tiempo real al fichero.
    - Escritura multilinea guardando la sangria.
    - 7 tipos de mensajes.
    - No estorba al ejecutar paso a paso el programa (usando la directiva del
      compilador DEBUGINFO)
    - Posibilidad de desactivar la depuracion (usando la directiva
      'DEFINE Depuracion')

  POSIBLES MEJORAS:
  -----------------
    - Detectar si es posible crear el fichero de depuración en caso de
      que el programa este en un CD-ROM para evitar que de errores al
      intentar escribir
    - ¿Convertir a XML? Pero tendría que reconstruir todo...
    - Hacer que no se cree el fichero si todo esta desactivado.
    - Pseudo-Monitor de Objetos (Siempre que se envien bien los mensajes
      correspondiente cuando se crean y se destruyen)
      (Simplemente dos contadores para cada mensajero)
      por supuesto con posibilidad de desactivarlo
    - No estaria mal de en vez de hacer un TAD a pelo,
      usar un objeto cDepuracion
    - Añadir la posibilidad de que los mensajes de error muestren una excepcion
      o un showmessage
    - Buscar una forma para que evitar que si oPadre tiene un EscribirLog al
      Crear/Destruir y un oHijo tambien al realizar esas operaciones se
      muestran los dos Crear/Destruir...

  ACTUALIZACIONES:
  ----------------
    12/04/07 - 1.002
      - Retoquecillo insertando un salto de línea adicional al final

    12/04/07 - 1.001
      - Desactivo salto de línea automático porque WrapText inserta un salto
        de línea de más al final de la cadena...
      
    11/04/07 - 1 (Época 2)
      - Eliminado Gestor de memoria, mejor en una unidad a parte, ya que el de
        Delphi no es del todo compatible con el de FPC y es mejor que estén
        separados.
      
    05/04/06 - 3.7u3
      m Eliminado algo de Leak a base de mostrar algo menos información al
        inicio y al final del archivo.

    26/03/06 - 3.7u2
      m Cambiados los saltos de linea '#13#10' por la variable sLineBreak
        que esta definida dependiendo del sistema operativo.
      m Corregida alguna que otra falta de ortografía en el texto de salida

    16/03/06 - 3.7u1
      m Las cadenas largas '*' se crean dinamicamente respecto al ancho
        ¿¿¿¡¡¡PERO A AUMENTADO EL LEAK!!!??? d>_<b!

    17/12/05 - 3.7
      m Convertido el parametro Tipo a un enumerado siendo del tipo
        TuDepTipoMensaje, pero el leak vueleva a ser -8 dº_ºb?

    01/12/05 - 3.6u2
      m Cambiada un poco la diferencia de memoria a -5, pero creo que es mejor,
        ahora inicio el gestor de memoria antes de abrir el fichero, y
        la comprobacion la hago despues de cerrarlo. Por tanto los programas
        de consola bien liberados ya no dan que libero mas de la que pido...
      m Cambiado el mensaje final aclarando que si es una aplicacion Windows
        es normal que de -5.

    18/11/05 - 3.6u1
      m No he añadido nada... simplemente actualizado el mensaje final del
        depurador porque misteriosamente el 'leak' ha bajado a -3
        de una forma misteriosa...  d^_^b

    23/10/05 - 3.6
      + Añadidos dos tipo nuevos de mensajes para la creacion y destruccion de
        objetos. Y por tanto cambiado tambien el orden de Tipo y el valor por
        defecto (Siguen siendo las Acciones detalladas)

    22/10/05 - 3.5
      + Añadida de desactivar la depuracion con la directiva 'DEFINE Depuracion'
      + Añadida la posibilidad de usar solo el Gestor de Memoria
        (Activando su directiva y Desactivando la de la depuracion)
      + Añadido lo necesario para que no de Warnings de varibles no usadas
        (Sin usar la directiva WARNINGS OFF), pasaba si se desactivaba
        la depuracion.
      m Reducido a -6 el desfase de no liberaciones porque cuando comenzaba el
        monitor todavia no habia asignado el fichero y sin embargo cuando hago
        la comprobacion todavia no lo he cerrado
        (Curiosamente si comienzo el monitor despues de la cabecera y hago un
        flush el desfase baja a -2, y no logro comprender que 4 asignaciones
        hago en esas lineas... ¡¡¡'ni cuales no devuelvo!!!)

    21/10/05 - 3.2
      + Añadido que el parametro byte(Tipo) en las funciones EscribirLOGXXX sea
        de Tipo 3 por defecto.

    23/09/05 - 3.1
      m Modificado el Gestor de Memoria para que se reduzca a 8 el desfase
        simplemente abriendo un ventana, por tanto si se da un desfase de 8
        posiblemente este bien liberado el programa. (La verdad no se porque
        se produce.. :,-( )
      + Añadida una directiva al compilador para que cuando se ejecute paso
        a paso no entre en esta unidad.

    23/09/05 - 3.0
      + Añadido el Gestor de Memoria.

    21/09/05 - 2.1
      + Cada vez que se escribe un error (Tipo = 0) hace un Flush.

    15/07/05 - 2.0
      + Posibilidad de cambiar dinamicamente:
        + Tipo de detalle con el que se va a escribir.
        + El ancho de la linea.
        + Tamaño de la Sangria
        + Escribir en tiempo real al fichero.
      + Escritura Jerarquizada de los mensanjes.
      + Escritura multilinea guardando la sangria.

    10/07/05 - 1.0
      + 5 tipos de mensajes:
        0 -> Errores
        1 -> Atenciones, Errores Ignorables, Evitables
        2 -> Acciones Generales
        3 -> Acciones Detalladas
        4 -> Datos para la Depuracion

  LICENCIA
  --------
  No me hago responsable del los daños ocasionados por el uso de estas
    fuentes de código.
  Puedes hacer lo que quieras con ello pero solo te pido que si las
    modificaciones no son demasiado importantes, no estaría mal un
    "Basado o Usa en las Fuentes de OMMugen por Chixpy"

  Este código fuente hace uso de JEDI-SDL pero no se distribuye con él
    * http://sourceforge.net/projects/jedi-sdl

}

Interface
Type
  TuDepTipoMensaje = (uDError, uDAtencion, uDAccion, uDDestruir, uDCrear,
    uDDetalle, uDDepuracion);
Var
  // ¿Actualizar el siempre el fichero instantaneamente?
  // NOTA: uDDError SIEMPRE lo hace
  UDEPURACION_ACTUALIZAFICHERO: Boolean = False;

  // Nivel de Detalle de los mensajes a escribir
  UDEPURACION_NIVELDETALLE: TuDepTipoMensaje = uDDetalle;

  // Ancho que no se quiere que pasen las lineas del archivo
  UDEPURACION_ANCHOLINEA: Byte = 78;

  // Tamaño del Tabulador para la sangria
  UDEPURACION_TAMANOTABULADOR: Byte = 2;

  {Este comentario es para que el DelForExp no ponga sangria en el siguiente}

// ----------------------------------------------------------------------------
//                 PROCEDIMIENTOS PARA ESCRIBIR EN EL ARCHIVO
// ----------------------------------------------------------------------------

Procedure EscribirLOG(Mensajero: TObject; Mensaje: String;
  Tipo: TuDepTipoMensaje = uDDetalle); Overload;
Procedure EscribirLOG(Mensajero: String; Mensaje: String;
  Tipo: TuDepTipoMensaje = uDDetalle); Overload;

Procedure EscribirLOGFmt(Mensajero: TObject; Mensaje: String; Const Args: Array
  Of Const; Tipo: TuDepTipoMensaje = uDDetalle); Overload;
Procedure EscribirLOGFmt(Mensajero: String; Mensaje: String; Const Args: Array
  Of Const; Tipo: TuDepTipoMensaje = uDDetalle); Overload;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
Implementation
Uses SysUtils, Strutils;

Var
{$IFDEF Depuracion}
  ListaMensajeros: Array Of String;
{$ENDIF}
  FicheroDepuracion: TextFile;

Procedure EscribeMultilinea(Sangria: Byte; Cadena: String);
{$IFDEF Depuracion}
var
  LongitudLinea: Integer;
  EspacioSangria: String;
{$ENDIF}
Begin
{$IFDEF Depuracion}
  // Aqui especifico el ancho de línea que voy a usar
  //   o El ancho menos la sangria usada
  //   o La mitad del ancho
  LongitudLinea := UDEPURACION_ANCHOLINEA - (Sangria *
    UDEPURACION_TAMANOTABULADOR);
  If LongitudLinea < (UDEPURACION_ANCHOLINEA Div 2) then
    LongitudLinea := UDEPURACION_ANCHOLINEA Div 2;
  
  // Relleno de la sangria (aka las barras verticales)
  EspacioSangria := DupeString('|' + StringOfChar(' ',
    UDEPURACION_TAMANOTABULADOR - 1),
    Sangria);

{ FALLA WrapText al poner un salto de línea de más

  // Defino el salto de linea y añado la sangria para las sublineas
  // sLineBreak -> Salto de Linea
  // EspacioSangria -> Barras Verticales
  // Lo siguiente es la sangria de las sublineas
  SaltoLinea := sLineBreak + EspacioSangria + StringOfChar(' ',
    (UDEPURACION_TAMANOTABULADOR Div 2));

  Writeln(
    FicheroDepuracion,
    EspacioSangria,
    WrapText(Cadena, SaltoLinea, ['.', ' ', #9, '-'], LongitudLinea)
    );
}

  Writeln( FicheroDepuracion, EspacioSangria, Cadena);
{$ENDIF}
End;

Procedure EscribirLOG(Mensajero: String; Mensaje: String; Tipo:
  TuDepTipoMensaje);
{$IFDEF Depuracion}
Var
  Sangria: Byte;
{$ENDIF}
Begin
{$IFDEF Depuracion}
  If Tipo > UDEPURACION_NIVELDETALLE Then Exit;

  Sangria := 0;
  // Elegimos el nivel de sangria
  While (Sangria <= High(ListaMensajeros)) Do
  Begin
    If (Mensajero <> ListaMensajeros[Sangria]) Then
      Inc(Sangria)
    Else
      Break;
  End;

  If Sangria <> High(ListaMensajeros) Then
  Begin
    If Sangria > High(ListaMensajeros) Then
    Begin
      EscribeMultilinea(Sangria, AnsiUpperCase(Mensajero));
    End;
    SetLength(ListaMensajeros, Sangria + 1);
    ListaMensajeros[Sangria] := Mensajero;
  End;

  Case Tipo Of
    uDError:
      Begin
        WriteLn(FicheroDepuracion);
        EscribeMultilinea(0, StringOfChar('*',
          (UDEPURACION_ANCHOLINEA - 7) Div 2) + ' ERROR ' + StringOfChar('*',
          (UDEPURACION_ANCHOLINEA - 7) Div 2));
        Mensaje := AnsiUpperCase(Mensajero + ': ' + Mensaje);
        EscribeMultilinea(0, Mensaje);
        EscribeMultilinea(0, StringOfChar('*',
          (UDEPURACION_ANCHOLINEA - 7) Div 2) + ' ERROR ' + StringOfChar('*',
          (UDEPURACION_ANCHOLINEA - 7) Div 2));
        WriteLn(FicheroDepuracion);
        Flush(FicheroDepuracion);
      End;

    uDAtencion:
      Begin
        EscribeMultilinea(Sangria, '*** ATENCIÓN ***');
        EscribeMultilinea(Sangria, Mensajero + ': ' + Mensaje);
        EscribeMultilinea(Sangria, '*** ATENCIÓN ***');
      End;

    uDCrear:
      Begin
        Mensaje := AnsiUpperCase(Mensaje);
        EscribeMultilinea(Sangria, '* ' + 'CREANDO OBJETO ' + Mensajero +
          ': ' + Mensaje);
      End;

    uDDestruir:
      Begin
        Mensaje := AnsiUpperCase(Mensaje);
        EscribeMultilinea(Sangria, '* ' + 'DESTRUYENDO OBJETO ' + Mensajero
          + ': ' + Mensaje);
      End;

    uDAccion:
      Begin
        Mensaje := AnsiUpperCase(Mensaje);
        EscribeMultilinea(Sangria, '> ' + Mensaje);
      End;

    uDDetalle:
      Begin
        EscribeMultilinea(Sangria, '|- ' + Mensaje);
      End;

    uDDepuracion:
      Begin
        EscribeMultilinea(Sangria, '|·· ' + Mensaje);
      End;
  End;

  If UDEPURACION_ACTUALIZAFICHERO Then
    Flush(FicheroDepuracion);
{$ENDIF}
End;

Procedure EscribirLOGFmt(Mensajero: String; Mensaje: String; Const Args: Array
  Of Const; Tipo: TuDepTipoMensaje);
Begin
{$IFDEF Depuracion}
  EscribirLOG(Mensajero, Format(Mensaje, Args), Tipo);
{$ENDIF}
End;

Procedure EscribirLOG(Mensajero: TObject; Mensaje: String; Tipo:
  TuDepTipoMensaje);
Begin
{$IFDEF Depuracion}
  EscribirLOG(Mensajero.ClassName, Mensaje, Tipo);
{$ENDIF}
End;

Procedure EscribirLOGFmt(Mensajero: TObject; Mensaje: String; Const Args: Array
  Of Const; Tipo: TuDepTipoMensaje);
Begin
{$IFDEF Depuracion}
  EscribirLOG(Mensajero.ClassName, Format(Mensaje, Args), Tipo);
{$ENDIF}
End;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
Initialization

  AssignFile(FicheroDepuracion, ChangeFileExt(ParamStr(0), '.log'));

  Rewrite(FicheroDepuracion);

  WriteLn(FicheroDepuracion, '|||||| DEBUG FILE ||||||');
  WriteLn(FicheroDepuracion);
  Flush(FicheroDepuracion);

  // ----------------------------------------------------------------------------
  // ----------------------------------------------------------------------------
Finalization

{$IFDEF Depuracion}
  Finalize(ListaMensajeros);
{$ENDIF}

  WriteLn(FicheroDepuracion);
  WriteLn(FicheroDepuracion, '|||||| END LOG ||||||');
  Flush(FicheroDepuracion);
  CloseFile(FicheroDepuracion);
End.

