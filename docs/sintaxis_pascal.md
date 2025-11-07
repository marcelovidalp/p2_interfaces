# Guía Básica de Sintaxis Pascal para Principiantes

## 1. Estructura de un Programa

```pascal
program NombrePrograma;  // Declaración del programa

uses                     // Importación de unidades
  Unit1, Unit2;

begin                    // Inicio del código
  // Tu código aquí
end.                     // Fin del programa (punto obligatorio)
```

## 2. Declaración de Unidades (Units)

```pascal
unit NombreUnidad;

interface              // Sección pública (visible externamente)
  // Declaraciones públicas

implementation         // Sección privada (implementación)
  // Código de implementación

end.                   // Fin de la unidad
```

## 3. Tipos de Datos Básicos

```pascal
var
  numero: Integer;           // Entero (-2147483648..2147483647)
  decimal: Double;           // Número con decimales
  texto: string;             // Cadena de texto
  letra: Char;               // Un solo carácter
  verdadero: Boolean;        // True o False
```

## 4. Clases y Objetos

```pascal
type
  TMiClase = class        // Declaración de clase
  private                  // Miembros privados
    FCampo: Integer;
  public                   // Miembros públicos
    constructor Create;    // Constructor
    destructor Destroy; override;  // Destructor
    procedure MiMetodo;    // Método sin retorno
    function OtroMetodo: Integer;  // Método con retorno
  end;
```

## 5. Procedimientos y Funciones

```pascal
// Procedimiento (no retorna valor)
procedure MiProcedimiento(Parametro: Integer);
begin
  // Código
end;

// Función (retorna valor)
function MiFuncion(Parametro: string): Boolean;
begin
  Result := True;  // Result es la variable de retorno
end;
```

## 6. Estructuras de Control

```pascal
// Condicional IF
if condicion then
  Instruccion
else
  OtraInstruccion;

// Bucle FOR
for i := 1 to 10 do
  Instruccion;

// Bucle WHILE
while condicion do
  Instruccion;

// CASE (similar a switch)
case variable of
  1: Instruccion1;
  2: Instruccion2;
  else
    InstruccionPorDefecto;
end;
```

## 7. Gestión de Memoria

```pascal
var
  Objeto: TMiClase;

Objeto := TMiClase.Create;  // Crear objeto
try
  // Usar objeto
finally
  Objeto.Free;              // Liberar memoria
end;
```

## 8. Manejo de Excepciones

```pascal
try
  // Código que puede fallar
except
  on E: Exception do
    ShowMessage('Error: ' + E.Message);
end;
```

## 9. Punteros a Métodos

```pascal
type
  TMetodoEvento = procedure(Sender: TObject) of object;

var
  MiEvento: TMetodoEvento;

MiEvento := @MiProcedimiento;  // Asignar método
MiEvento(Self);                 // Llamar método
```

## 10. Constantes y Tipos Propios

```pascal
const
  MAX_VALOR = 100;

type
  TDato = record
    Campo1: Integer;
    Campo2: string;
  end;

  TArreglo = array[0..9] of Integer;
```