# Guía de Uso - Sistema de Monitoreo Ambiental

## Requisitos del Sistema

### Servidor (Pascal/Lazarus)
- **Sistema Operativo:** Windows 10/11, Linux, macOS
- **Lazarus:** Versión 2.0 o superior
- **Free Pascal:** Versión 3.2 o superior
- **Paquetes requeridos:**
  - LCL (Lazarus Component Library) - incluido
  - TAChartLazarusPkg - para gráficos
  - SQLite3Conn - para base de datos

### Cliente (Python)
- **Python:** Versión 3.7 o superior
- **Librería requests:**
  ```bash
  pip install requests
  ```

## Instalación

### 1. Instalar Lazarus IDE

**Windows:**
1. Descargar desde: https://www.lazarus-ide.org/
2. Ejecutar instalador (lazarus-X.X.X-fpc-X.X.X-win64.exe)
3. Seguir asistente de instalación

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install lazarus
```

**macOS:**
```bash
brew install lazarus
```

### 2. Instalar paquetes de Lazarus

1. Abrir Lazarus
2. Menú: `Package` → `Install/Uninstall Packages...`
3. En panel izquierdo buscar y agregar:
   - `TAChartLazarusPkg`
4. Clic en `Save and rebuild IDE`
5. Esperar a que Lazarus se recompile

### 3. Instalar Python y dependencias

**Windows:**
```powershell
# Descargar Python desde python.org e instalar
# Luego en PowerShell:
pip install requests
```

**Linux/macOS:**
```bash
pip3 install requests
```

## Compilación y Ejecución

### Servidor Pascal

#### Método 1: Desde Lazarus IDE

1. **Abrir proyecto:**
   - Lazarus → `Project` → `Open Project...`
   - Navegar a: `server/MonitoreoAmbiental.lpi`
   - Clic en `Open`

2. **Compilar:**
   - Menú: `Run` → `Build` (o `Shift+F9`)
   - Esperar a que compile sin errores

3. **Ejecutar:**
   - Menú: `Run` → `Run` (o `F9`)
   - El servidor se iniciará y mostrará la ventana principal

#### Método 2: Desde línea de comandos

**Windows (PowerShell):**
```powershell
cd "c:\Users\marce\Documents\repos\p2_interfaces\server"
lazbuild MonitoreoAmbiental.lpi
.\MonitoreoAmbiental.exe
```

**Linux/macOS:**
```bash
cd ~/repos/p2_interfaces/server
lazbuild MonitoreoAmbiental.lpi
./MonitoreoAmbiental
```

### Cliente Python

**Windows (PowerShell):**
```powershell
cd "c:\Users\marce\Documents\repos\p2_interfaces\cliente"
python client.py
```

**Linux/macOS:**
```bash
cd ~/repos/p2_interfaces/cliente
python3 client.py
```

## Uso del Sistema

### 1. Iniciar el Servidor

1. Ejecutar aplicación Lazarus compilada
2. Ventana principal muestra:
   - Título: "Sistema de Monitoreo Ambiental"
   - Gráfico vacío en el centro
   - ComboBox con "Estación 1" a "Estación 10"
   - Botón "Exportar Gráfico"
   - Label inferior: "Servidor activo en puerto 8080"

### 2. Iniciar el Cliente

1. Abrir terminal/PowerShell
2. Ejecutar `python client.py`
3. Salida esperada:
   ```
   Cliente iniciado. Enviando datos a http://127.0.0.1:8080
   Simulando 10 estaciones
   ------------------------------------------------------------
   14:23:45 ✓ Estación  3 | PM2.5:  45.23 | Temp: 23.45°C
   14:23:46 ✓ Estación  7 | PM2.5:  67.89 | Temp: 28.12°C
   14:23:47 ✓ Estación  1 | PM2.5:  34.56 | Temp: 19.87°C
   ```

### 3. Visualizar Datos

#### Ver estación específica:
1. En el ComboBox, seleccionar "Estación 1" (o 2, 3, ... 10)
2. El gráfico mostrará solo los datos de esa estación
3. Eje X: Tiempo (últimos 30 puntos)
4. Eje Y: Valor PM2.5 en µg/m³

#### Características del gráfico:
- **Scrolling automático:** Solo muestra últimos 30 puntos
- **Actualización en tiempo real:** Cada segundo
- **Colores distintos:** Cada estación tiene su color
- **Leyenda:** Muestra nombre de estación activa

### 4. Exportar Gráficos

1. Seleccionar estación deseada
2. Clic en botón "Exportar Gráfico"
3. Mensaje: "Gráfico exportado exitosamente"
4. Archivo guardado en: `server/exportacion/chart_0001.png`
5. Cada exportación incrementa el número: `chart_0002.png`, etc.

### 5. Monitorear Estado

- **Label inferior** muestra última recepción:
  ```
  Última recepción: Estación 5 - 2025-11-07 14:30:45
  ```
- Si no llegan datos, verifica que el cliente esté corriendo

### 6. Detener el Sistema

**Detener cliente:**
- En terminal del cliente: `Ctrl+C`
- Salida: "Cliente detenido por el usuario"

**Detener servidor:**
- Cerrar ventana principal de Lazarus
- O clic en botón X de la ventana

## Estructura de Archivos Generados

```
p2_interfaces/
├── server/
│   ├── clima.db                    # Base de datos SQLite (auto-generada)
│   ├── exportacion/                # Carpeta de exportaciones (auto-creada)
│   │   ├── chart_0001.png
│   │   ├── chart_0002.png
│   │   └── ...
│   └── lib/                        # Archivos de compilación (auto-generados)
│       └── x86_64-win64/
```

## Verificación de Datos

### Consultar base de datos SQLite

**Windows (PowerShell):**
```powershell
cd "c:\Users\marce\Documents\repos\p2_interfaces\server"
sqlite3 clima.db "SELECT * FROM estaciones ORDER BY timestamp DESC LIMIT 10;"
```

**Linux/macOS:**
```bash
cd ~/repos/p2_interfaces/server
sqlite3 clima.db "SELECT * FROM estaciones ORDER BY timestamp DESC LIMIT 10;"
```

**Salida esperada:**
```
1|3|2025-11-07|14:23:45|45.23|89.45|23.45|65.2|1013.5|2025-11-07 14:23:45
2|7|2025-11-07|14:23:46|67.89|125.34|28.12|58.9|1015.2|2025-11-07 14:23:46
...
```

## Solución de Problemas

### Problema: "No se puede abrir puerto 8080"

**Causa:** Otro proceso usa el puerto 8080

**Solución Windows:**
```powershell
netstat -ano | findstr :8080
taskkill /PID <PID> /F
```

**Solución Linux/macOS:**
```bash
lsof -i :8080
kill -9 <PID>
```

### Problema: Cliente no conecta

**Verificar:**
1. Servidor está ejecutándose
2. Label muestra "Servidor activo en puerto 8080"
3. Firewall no bloquea puerto 8080
4. URL en cliente es correcta: `http://127.0.0.1:8080`

**Probar conexión:**
```powershell
# Windows
Test-NetConnection -ComputerName 127.0.0.1 -Port 8080

# Linux/macOS
nc -zv 127.0.0.1 8080
```

### Problema: No se exportan gráficos

**Causa 1:** Carpeta `exportacion` no existe

**Solución:**
```powershell
# Windows
New-Item -ItemType Directory -Path "exportacion" -Force

# Linux/macOS
mkdir -p exportacion
```

**Causa 2:** Sin permisos de escritura

**Solución:**
```powershell
# Windows
icacls exportacion /grant Users:F

# Linux/macOS
chmod 755 exportacion
```

### Problema: Error al compilar - TAChart no encontrado

**Solución:**
1. Lazarus → `Package` → `Install/Uninstall Packages...`
2. Buscar `TAChartLazarusPkg` en panel izquierdo
3. Clic en `→` para agregar
4. Clic en `Save and rebuild IDE`
5. Esperar reinicio de Lazarus

### Problema: Error SQLite3Conn no encontrado

**Solución:**
1. Lazarus → `Package` → `Install/Uninstall Packages...`
2. Buscar `SQLDBLaz` en panel izquierdo
3. Clic en `→` para agregar
4. Clic en `Save and rebuild IDE`

### Problema: Gráfico no se actualiza

**Verificar:**
1. ComboBox tiene estación seleccionada
2. Cliente envía datos (verificar terminal)
3. Label inferior muestra "Última recepción: ..."

**Forzar actualización:**
- Cambiar estación en ComboBox
- Volver a estación original

## Personalización

### Cambiar puerto del servidor

**Archivo:** `server/unidades/uMainForm.pas`

```pascal
procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  // ...
  FServidor := TServidorHTTP.Create(8080);  // Cambiar 8080 por otro puerto
  // ...
end;
```

**Archivo cliente:** `cliente/client.py`

```python
class ClienteAmbiental:
    def __init__(self, url_servidor='http://127.0.0.1:8080', ...):
        # Cambiar URL y puerto aquí
```

### Cambiar cantidad de puntos en gráfico

**Archivo:** `server/unidades/uGraficos.pas`

```pascal
const
  MAX_PUNTOS = 30;  // Cambiar por 50, 100, etc.
```

### Cambiar frecuencia de envío

**Archivo:** `cliente/client.py`

```python
def ejecutar(self):
    while True:
        # ...
        time.sleep(1)  # Cambiar 1 por 0.5, 2, etc. (segundos)
```

## Tips y Mejores Prácticas

1. **Iniciar primero el servidor, luego el cliente**
2. **Mantener terminal del cliente visible** para monitorear envíos
3. **Exportar gráficos antes de cerrar** si quieres conservarlos
4. **Base de datos crece constantemente:** Limpiar periódicamente
   ```sql
   DELETE FROM estaciones WHERE timestamp < datetime('now', '-7 days');
   ```
5. **Usar múltiples clientes** para simular más estaciones:
   ```python
   cliente1 = ClienteAmbiental(num_estaciones=5)
   cliente2 = ClienteAmbiental(url_servidor='http://127.0.0.1:8080', num_estaciones=5)
   ```

## Recursos Adicionales

- **Documentación Lazarus:** https://wiki.lazarus.freepascal.org/
- **TAChart Tutorial:** https://wiki.freepascal.org/TAChart
- **SQLite Documentation:** https://www.sqlite.org/docs.html
- **Python Requests:** https://requests.readthedocs.io/
