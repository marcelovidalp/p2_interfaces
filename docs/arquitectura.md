# Arquitectura del Sistema de Monitoreo Ambiental

## Componentes Principales

### 1. Cliente Python (`cliente_ambiental.py`)
- Simula 10 estaciones ambientales
- Genera datos aleatorios cada segundo
- Envía datos vía HTTP POST en formato JSON

### 2. Servidor Lazarus (Pascal)

#### Módulo Servidor (`uServidor.pas`)
- Gestiona conexiones HTTP en puerto 8080
- Parsea JSON de entrada
- Dispara eventos para procesamiento de datos

#### Módulo Base de Datos (`uBaseDatos.pas`)
- Maneja conexión SQLite
- Crea y mantiene tabla `estaciones`
- Almacena registros históricos

#### Módulo Gráficos (`uGraficos.pas`)
- Gestiona 10 series de líneas (una por estación)
- Implementa scrolling automático (últimos 30 puntos)
- Exporta gráficos a PNG secuencial

#### Formulario Principal (`uFormPrincipal.pas`)
- Coordina todos los módulos
- Proporciona interfaz de usuario
- Selector de estaciones y botón de exportación

## Flujo de Datos

```
Cliente Python → JSON → Servidor HTTP → Parser JSON
                                              ↓
                                        Base de Datos
                                              ↓
                                        Actualizar Gráfico
                                              ↓
                                        Exportar PNG
```

## Estructura de la Base de Datos

**Tabla: estaciones**
- id: INTEGER (PRIMARY KEY)
- ide: INTEGER (ID estación 1-10)
- sFe: TEXT (Fecha)
- sHo: TEXT (Hora)
- P25: REAL (Material particulado 2.5 µm)
- P10: REAL (Material particulado 10 µm)
- nTe: REAL (Temperatura °C)
- nHr: REAL (Humedad relativa %)
- nPa: REAL (Presión atmosférica hPa)
- timestamp: DATETIME