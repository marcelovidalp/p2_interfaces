'''
Referencia de codigo para la implementacion de un cliente que envia datos a un servidor RESTful
debo recibir el json con los datos enviados y una confirmacion de recepcion
'''


# By Dr.(c) Alberto Caro
# Lab Sistemas Inteligentes
import requests as req, time as ti, random as ra, datetime as dt

sURL = 'http://127.0.0.1:5000/station'

def Get_Data():
    Data = {
            'f': str(dt.datetime.now())[00:10], # Fecha
            'c': str(dt.datetime.now())[11:19], # Hora
            't': ra.randint(010,040),           # Temperatura
            'p': ra.randint(760,999),           # Presion Atmosferica
            'h': ra.randint(010,100),           # Humedad Relativa Aire
           }
    return Data

while 1:
 Data = Get_Data()
 cnx = req.post(sURL,json = Data)
 cnx.close()
 print( str(dt.datetime.now())[11:19] + ' -> ' + cnx.text )
 ti.sleep(1)


