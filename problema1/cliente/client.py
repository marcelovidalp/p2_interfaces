import requests as rq
import time
import random as ra
from datetime import datetime

class ClienteAmbiental:
    def __init__(self, url_servidor='http://127.0.0.1:8080', num_estaciones=10):
        self.url_servidor = url_servidor
        self.num_estaciones = num_estaciones
    
    def generar_datos_estacion(self, id_estacion):
        return {
            'ide': id_estacion,
            'sFe': datetime.now().strftime('%Y-%m-%d'),
            'sHo': datetime.now().strftime('%H:%M:%S'),
            'P25': ra.gauss(mu=10, sigma=3),
            'P10': ra.gauss(mu=15, sigma=4),
            'nTe': ra.gauss(mu=20, sigma=4),
            'nHr': ra.gauss(mu=70, sigma=2),
            'nPa': ra.gauss(mu=1000, sigma=10)
        }
    
    def enviar_datos(self, datos):
        try:
            respuesta = rq.post(self.url_servidor, json=datos, timeout=5)
            return respuesta.status_code == 200, respuesta.text
        except rq.exceptions.RequestException as e:
            return False, str(e)
    
    def ejecutar(self):
        print(f"Cliente iniciado. Enviando datos a {self.url_servidor}")
        print(f"Simulando {self.num_estaciones} estaciones")
        print("-" * 60)
        
        while True:
            id_estacion = ra.randint(1, self.num_estaciones)
            datos = self.generar_datos_estacion(id_estacion)
            
            exito, mensaje = self.enviar_datos(datos)
            
            timestamp = datetime.now().strftime('%H:%M:%S')
            estado = "✓" if exito else "✗"
            print(f"{timestamp} {estado} Estación {id_estacion:2d} | "
                  f"PM2.5: {datos['P25']:6.2f} | Temp: {datos['nTe']:5.2f}°C")
            
            time.sleep(1)

if __name__ == '__main__':
    cliente = ClienteAmbiental()
    try:
        cliente.ejecutar()
    except KeyboardInterrupt:
        print("\n\nCliente detenido por el usuario")


