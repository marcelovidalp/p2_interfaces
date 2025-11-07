unit uServidor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, fpjson, jsonparser;

type
  TDatosEstacion = record
    ide: Integer;
    sFe: string;
    sHo: string;
    P25: Double;
    P10: Double;
    nTe: Double;
    nHr: Double;
    nPa: Double;
  end;

  TOnDatosRecibidos = procedure(const Datos: TDatosEstacion) of object;

  TServidorHTTP = class
  private
    FServidor: TFPHttpServer;
    FOnDatosRecibidos: TOnDatosRecibidos;
    procedure ManejarSolicitud(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ParsearJSON(const JSONData: string): TDatosEstacion;
  public
    constructor Create(Puerto: Word);
    destructor Destroy; override;
    procedure Iniciar;
    procedure Detener;
    property OnDatosRecibidos: TOnDatosRecibidos read FOnDatosRecibidos write FOnDatosRecibidos;
  end;

implementation

constructor TServidorHTTP.Create(Puerto: Word);
begin
  FServidor := TFPHttpServer.Create(nil);
  FServidor.Port := Puerto;
  FServidor.OnRequest := @ManejarSolicitud;
end;

destructor TServidorHTTP.Destroy;
begin
  Detener;
  FServidor.Free;
  inherited;
end;

procedure TServidorHTTP.Iniciar;
begin
  FServidor.Active := True;
end;

procedure TServidorHTTP.Detener;
begin
  if Assigned(FServidor) then
    FServidor.Active := False;
end;

procedure TServidorHTTP.ManejarSolicitud(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Datos: TDatosEstacion;
begin
  if ARequest.Method = 'POST' then
  begin
    try
      Datos := ParsearJSON(ARequest.Content);
      
      if Assigned(FOnDatosRecibidos) then
        FOnDatosRecibidos(Datos);
      
      AResponse.Code := 200;
      AResponse.Content := 'Datos recibidos correctamente';
    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        AResponse.Content := 'Error: ' + E.Message;
      end;
    end;
  end
  else
  begin
    AResponse.Code := 405;
    AResponse.Content := 'Método no permitido';
  end;
end;

function TServidorHTTP.ParsearJSON(const JSONData: string): TDatosEstacion;
var
  Parser: TJSONParser;
  JSONObject: TJSONObject;
begin
  Parser := TJSONParser.Create(JSONData);
  try
    JSONObject := TJSONObject(Parser.Parse);
    try
      Result.ide := JSONObject.Get('ide').AsInteger;
      Result.sFe := JSONObject.Get('sFe').AsString;
      Result.sHo := JSONObject.Get('sHo').AsString;
      Result.P25 := JSONObject.Get('P25').AsFloat;
      Result.P10 := JSONObject.Get('P10').AsFloat;
      Result.nTe := JSONObject.Get('nTe').AsFloat;
      Result.nHr := JSONObject.Get('nHr').AsFloat;
      Result.nPa := JSONObject.Get('nPa').AsFloat;
    finally
      JSONObject.Free;
    end;
  finally
    Parser.Free;
  end;
end;

end.