unit uDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, SQLite3Conn, uServidor, dynlibs;

type
  TGestorBaseDatos = class
  private
    FConexion: TSQLite3Connection;
    FTransaccion: TSQLTransaction;
    FConsulta: TSQLQuery;
    procedure CargarSQLiteDLL;
  public
    constructor Create(const NombreDB: string);
    destructor Destroy; override;
    procedure InicializarTabla;
    procedure GuardarDatos(const Datos: TDatosEstacion);
  end;

implementation

constructor TGestorBaseDatos.Create(const NombreDB: string);
var
  DBPath: string;
begin

  
  FConexion := TSQLite3Connection.Create(nil);
  FTransaccion := TSQLTransaction.Create(nil);
  FConsulta := TSQLQuery.Create(nil);
  
  // Crear ruta completa para la base de datos
  DBPath := ExtractFilePath(ParamStr(0)) + NombreDB;
  FConexion.DatabaseName := DBPath;
  FTransaccion.Database := FConexion;
  FConsulta.Database := FConexion;
  FConsulta.Transaction := FTransaccion;
  
  try
    FConexion.Connected := True;
    FTransaccion.Active := True;
  except
    on E: Exception do
      raise Exception.Create('Error conectando BD: ' + E.Message);
  end;
end;

destructor TGestorBaseDatos.Destroy;
begin
  FConsulta.Free;
  FTransaccion.Free;
  FConexion.Free;
  inherited;
end;

procedure TGestorBaseDatos.InicializarTabla;
begin
  try
    FConsulta.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS estaciones (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'ide INTEGER,' +
      'sFe TEXT,' +
      'sHo TEXT,' +
      'P25 REAL,' +
      'P10 REAL,' +
      'nTe REAL,' +
      'nHr REAL,' +
      'nPa REAL,' +
      'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)';
    FConsulta.ExecSQL;
    FTransaccion.Commit;
  except
    on E: Exception do
    begin
      if FTransaccion.Active then
        FTransaccion.Rollback;
      raise Exception.Create('Error creando tabla: ' + E.Message);
    end;
  end;
end;

procedure TGestorBaseDatos.GuardarDatos(const Datos: TDatosEstacion);
begin
  try
    FConsulta.SQL.Text :=
      'INSERT INTO estaciones (ide, sFe, sHo, P25, P10, nTe, nHr, nPa) ' +
      'VALUES (:ide, :sFe, :sHo, :P25, :P10, :nTe, :nHr, :nPa)';
    
    FConsulta.ParamByName('ide').AsInteger := Datos.ide;
    FConsulta.ParamByName('sFe').AsString := Datos.sFe;
    FConsulta.ParamByName('sHo').AsString := Datos.sHo;
    FConsulta.ParamByName('P25').AsFloat := Datos.P25;
    FConsulta.ParamByName('P10').AsFloat := Datos.P10;
    FConsulta.ParamByName('nTe').AsFloat := Datos.nTe;
    FConsulta.ParamByName('nHr').AsFloat := Datos.nHr;
    FConsulta.ParamByName('nPa').AsFloat := Datos.nPa;
    
    FConsulta.ExecSQL;
    FTransaccion.Commit;
  except
    on E: Exception do
    begin
      if FTransaccion.Active then
        FTransaccion.Rollback;
      raise Exception.Create('Error guardando: ' + E.Message);
    end;
  end;
end;

end.