
## 📋 Descripción General

Repositorio con dos proyectos de desarrollo de aplicaciones cliente-servidor utilizando **Lazarus Pascal** y **Python**, diseñados para demostrar habilidades en programación de interfaces, comunicación HTTP, manejo de bases de datos y procesamiento de imágenes.

---

## 🎯 Proyecto 1: Sistema de Monitoreo Ambiental

### Descripción

Sistema cliente-servidor para monitoreo en tiempo real de estaciones ambientales. El servidor (Lazarus Pascal) recibe datos mediante HTTP POST en formato JSON, los almacena en SQLite y visualiza gráficos en tiempo real con scrolling automático.

### Características Principales

- **Servidor HTTP en Lazarus Pascal** (Puerto 8080)
  - Recibe datos JSON de sensores ambientales
  - Almacena en base de datos SQLite (`clima.db`)
  - Visualiza series de líneas de 1-10 estaciones en TChart
  - Left scrolling automático (últimos 30 puntos)
  - Exportación secuencial a PNG (`chart_0001.png`, `chart_0002.png`, ...)
  - Selector de estaciones (visualización excluyente)

- **Cliente HTTP en Python**
  - Simula 10 estaciones ambientales
  - Envía datos cada 1 segundo
  - Datos: PM2.5, PM10, Temperatura, Humedad, Presión atmosférica
  - Fecha y hora de cada medición

### Estructura de Datos JSON

```json
{
  "ide": 1,
  "sFe": "2025-11-07",
  "sHo": "14:30:45",
  "P25": 45.23,
  "P10": 89.45,
  "nTe": 23.45,
  "nHr": 65.2,
  "nPa": 1013.5
}
```

### Tecnologías Utilizadas

- **Backend:** Lazarus Pascal (Free Pascal)
- **Frontend:** LCL (Lazarus Component Library)
- **Gráficos:** TAChart
- **Base de datos:** SQLite3
- **HTTP Server:** fphttpserver
- **Cliente:** Python 3.7+ con requests

### Estructura de Archivos - Proyecto 1

```
server/
├── MonitoreoAmbiental.lpr          # Programa principal
├── MonitoreoAmbiental.lpi          # Archivo de proyecto Lazarus
├── unidades/
│   ├── uMainForm.pas              # Formulario principal
│   ├── uMainForm.lfm              # Diseño del formulario
│   ├── uServidor.pas              # Servidor HTTP y parser JSON
│   ├── uDB.pas                    # Gestión de base de datos SQLite
│   └── uGraficos.pas              # Gestión de gráficos TChart
├── clima.db                       # Base de datos SQLite (auto-generada)
└── exportacion/                   # Carpeta de imágenes exportadas

cliente/
└── client.py                      # Cliente HTTP Python
```

---

## 🖼️ Proyecto 2: Servidor de Imágenes HTTP

### Descripción

Aplicación que permite visualizar y servir imágenes mediante HTTP. El cliente Python envía imágenes desde la carpeta `img` al servidor Lazarus Pascal, que las muestra en una interfaz tipo galería de 5x5 imágenes. El cliente envía imágenes cada 1 segundo, y el servidor actualiza la visualización de forma aleatoria en celdas disponibles.

### Características Principales

- **Servidor HTTP en Lazarus Pascal**
  - Interfaz tipo aplicación con grilla 5x5 (25 celdas)
  - Recibe imágenes vía HTTP POST
  - Visualización aleatoria en celdas disponibles
  - Botón "Salir" para cerrar aplicación
  - Título: "IMAGE HTTP SERVER"

- **Cliente HTTP en Python**
  - Lee imágenes de carpeta `img/`
  - Envía imágenes vía HTTP POST cada 1 segundo
  - Selección aleatoria de imágenes
  - Manejo de reconexión automática

### Interfaz del Servidor

```
┌─────────────────────────────────────┐
│      IMAGE HTTP SERVER              │
├─────────────────────────────────────┤
│ [Img] [Img] [Img] [Img] [Img]      │
│ [Img] [Img] [Img] [Img] [Img]      │
│ [Img] [Img] [Img] [Img] [Img]      │
│ [Img] [Img] [Img] [Img] [Img]      │
│ [Img] [Img] [Img] [Img] [Img]      │
│           [ Salir ]                 │
└─────────────────────────────────────┘
```

### Estructura de Archivos - Proyecto 2

```
servidor_imagenes/
├── ImagenServer.lpr               # Programa principal
├── ImagenServer.lpi               # Archivo de proyecto Lazarus
└── unidades/
    ├── uFormImagenes.pas          # Formulario con grilla 5x5
    └── uServidorImg.pas           # Servidor HTTP para imágenes

cliente_imagenes/
├── client_images.py               # Cliente HTTP Python
└── img/                          # Carpeta con imágenes a enviar
    ├── imagen1.jpg
    ├── imagen2.png
    └── ...
```

---

## 🚀 Instalación y Configuración

### Requisitos Previos

#### Para Servidor (Lazarus)
- **Lazarus IDE** 2.0+
- **Free Pascal** 3.2+
- **Sistema Operativo:** Windows 10/11, Linux, macOS

#### Para Cliente (Python)
- **Python** 3.7+
- **Librería requests:**
  ```bash
  pip install requests
  ```

### Instalación de Lazarus

**Windows:**
```powershell
# Descargar desde: https://www.lazarus-ide.org/
# Ejecutar instalador y seguir asistente
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install lazarus
```

**Instalar paquetes necesarios en Lazarus:**
1. Abrir Lazarus
2. `Package` → `Install/Uninstall Packages...`
3. Agregar: `TAChartLazarusPkg`
4. `Save and rebuild IDE`

---

## 🎮 Uso de los Proyectos

### Proyecto 1: Monitoreo Ambiental

#### 1. Compilar y ejecutar servidor

```powershell
cd server
lazbuild MonitoreoAmbiental.lpi
.\MonitoreoAmbiental.exe
```

O desde Lazarus IDE:
- Abrir `server/MonitoreoAmbiental.lpi`
- `Run` → `Build` (Shift+F9)
- `Run` → `Run` (F9)

#### 2. Ejecutar cliente Python

```powershell
cd cliente
python client.py
```

#### 3. Visualizar y exportar

- Seleccionar estación del ComboBox (1-10)
- Ver gráfico en tiempo real
- Clic en "Exportar Gráfico" para guardar PNG

### Proyecto 2: Servidor de Imágenes

#### 1. Compilar y ejecutar servidor

```powershell
cd servidor_imagenes
lazbuild ImagenServer.lpi
.\ImagenServer.exe
```

#### 2. Preparar imágenes

```powershell
cd cliente_imagenes
mkdir img
# Copiar imágenes JPG/PNG a la carpeta img/
```

#### 3. Ejecutar cliente Python

```powershell
cd cliente_imagenes
python client_images.py
```

---

## 📁 Estructura Completa del Repositorio

```
p2_interfaces/
├── README.md                      # Este archivo
├── docs/
│   ├── arquitectura.md           # Documentación de arquitectura
│   ├── sintaxis_pascal.md        # Guía de sintaxis Pascal
│   └── guia_uso.md               # Guía detallada de uso
├── server/                        # Proyecto 1: Monitoreo Ambiental
│   ├── MonitoreoAmbiental.lpr
│   ├── MonitoreoAmbiental.lpi
│   ├── unidades/
│   │   ├── uMainForm.pas
│   │   ├── uServidor.pas
│   │   ├── uDB.pas
│   │   └── uGraficos.pas
│   └── exportacion/
├── cliente/                       # Cliente Python Proyecto 1
│   └── client.py
├── servidor_imagenes/             # Proyecto 2: Servidor de Imágenes
│   ├── ImagenServer.lpr
│   └── unidades/
│       ├── uFormImagenes.pas
│       └── uServidorImg.pas
└── cliente_imagenes/              # Cliente Python Proyecto 2
    ├── client_images.py
    └── img/
```

---

## 🔧 Solución de Problemas Comunes

### Error: "No se puede cargar sqlite3.dll"

**Solución Windows:**
```powershell
# Descargar sqlite3.dll desde: https://www.sqlite.org/download.html
# Copiar a la carpeta del ejecutable
Copy-Item sqlite3.dll -Destination .\server\
```

**Solución alternativa:** Usar ruta absoluta en código

### Error: "Puerto 8080 ocupado"

```powershell
# Ver qué proceso usa el puerto
netstat -ano | findstr :8080
# Matar proceso
taskkill /PID <PID> /F
```

### Cliente no conecta

1. Verificar que el servidor esté ejecutándose
2. Comprobar firewall de Windows
3. Verificar URL en cliente: `http://127.0.0.1:8080`

---

## 📊 Características Técnicas

### Proyecto 1: Monitoreo Ambiental

- **Arquitectura:** Cliente-Servidor
- **Protocolo:** HTTP POST
- **Formato:** JSON
- **Base de datos:** SQLite3
- **Visualización:** TAChart con series de líneas
- **Frecuencia:** 1 segundo
- **Capacidad:** 10 estaciones simultáneas
- **Buffer gráfico:** 30 puntos por estación

### Proyecto 2: Servidor de Imágenes

- **Arquitectura:** Cliente-Servidor
- **Protocolo:** HTTP POST
- **Formato:** Multipart/form-data
- **Visualización:** Grilla 5x5 (25 imágenes)
- **Actualización:** Aleatoria en celdas disponibles
- **Frecuencia:** 1 imagen por segundo
- **Formatos soportados:** JPG, PNG, BMP

---

## 📚 Documentación Adicional

- **[docs/arquitectura.md](docs/arquitectura.md)** - Arquitectura detallada del sistema
- **[docs/sintaxis_pascal.md](docs/sintaxis_pascal.md)** - Guía de Pascal para principiantes
- **[docs/guia_uso.md](docs/guia_uso.md)** - Manual de usuario completo

---

## 🎓 Aprendizajes y Competencias

### Lenguajes y Tecnologías
- ✅ Pascal/Object Pascal (Lazarus)
- ✅ Python 3
- ✅ SQL (SQLite)
- ✅ JSON
- ✅ HTTP Protocol

### Conceptos Aplicados
- ✅ Arquitectura Cliente-Servidor
- ✅ Comunicación HTTP (POST, GET)
- ✅ Parseo de JSON
- ✅ Base de datos relacionales
- ✅ Visualización de datos en tiempo real
- ✅ Programación orientada a objetos
- ✅ Interfaces gráficas (GUI)
- ✅ Manejo de archivos e imágenes
- ✅ Multithreading implícito
- ✅ Manejo de excepciones

### Habilidades de Ingeniería
- ✅ Diseño modular
- ✅ Separación de responsabilidades
- ✅ Documentación técnica
- ✅ Testing y debugging
- ✅ Resolución de problemas

---

## 📝 Notas del Desarrollador

### Decisiones de Diseño

1. **Modularidad:** Cada unidad Pascal tiene una responsabilidad única
2. **Sin comentarios excesivos:** Código autodocumentado con nombres descriptivos
3. **Scrolling automático:** Mejora rendimiento y visualización
4. **Exportación secuencial:** Evita sobrescritura de archivos
5. **Base de datos persistente:** Permite análisis histórico

### Mejoras Futuras

- [ ] Autenticación y seguridad
- [ ] Dashboard web adicional
- [ ] Alertas configurables por umbrales
- [ ] Exportación a múltiples formatos (PDF, Excel)
- [ ] Configuración vía archivo INI/JSON
- [ ] Soporte para múltiples clientes simultáneos
- [ ] Compresión de imágenes
- [ ] Zoom y pan en gráficos

---

## 🏆 Entregables

- ✅ Código fuente completo y funcional
- ✅ Documentación técnica detallada
- ✅ Guías de instalación y uso
- ✅ Tutorial de sintaxis Pascal
- ✅ Ejemplos de uso
- ✅ Solución de problemas comunes

---

## 📞 Contacto y Soporte

**Desarrollador:** Marcelo Vidal  
**Empresa:** Aquí te espero gallito Ltda  
**Proyecto:** Práctica 2 - Interfaces y Programación de Aplicaciones  
**Año:** 2025

---

## 📄 Licencia

Proyecto académico desarrollado para fines educativos.

---

**¡Listo para demostrar habilidades en desarrollo de aplicaciones cliente-servidor! 🚀**
