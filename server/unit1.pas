unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  TASeries, TAGraph, sqldb, SQLite3Conn, ExtCtrls, fphttpserver, fpjson, jsonparser, Buttons,
  LazSerial, TAGraph, Math, opensslsocket, SQLDB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    ComboBox1: TComboBox;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: TFPHttpServer;
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure InicializarBaseDatos;
    procedure InicializarChart;
    procedure ProcesarDatosJSON(const JSONData: string);
    procedure ActualizarChart(StationID: Integer; P25, P10, nTe, nHr, nPa: Double);
    procedure ExportarChartPNG;
  public
  end;

var
  Form1: TForm1;
  ContadorExportacion: Integer = 0;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Configurar combo box con las 10 estaciones
  ComboBox1.Items.Clear;
  for i := 1 to 10 do
    ComboBox1.Items.Add('Estación ' + IntToStr(i));
  ComboBox1.ItemIndex := 0;

  // Inicializar base de datos
  InicializarBaseDatos;

  // Inicializar chart
  InicializarChart;

  // Configurar servidor HTTP con TFPHttpServer
  FServer := TFPHttpServer.Create(nil);
  FServer.Port := 8080;
  FServer.OnRequest := @HandleRequest;
  FServer.Active := True;

  Caption := 'Servidor Monitoreo - Puerto: ' + IntToStr(FServer.Port);
  ShowMessage('Servidor iniciado en puerto 8080');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Free;
end;

procedure TForm1.HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  JSONData: string;
begin
  // Solo procesar POST requests
  if ARequest.Method = 'POST' then
  begin
    try
      // Leer datos JSON del cuerpo de la solicitud
      JSONData := ARequest.Content;

      // Procesar los datos JSON
      ProcesarDatosJSON(JSONData);

      AResponse.Content := 'Datos recibidos correctamente';
      AResponse.Code := 200;

    except
      on E: Exception do
      begin
        AResponse.Content := 'Error: ' + E.Message;
        AResponse.Code := 500;
      end;
    end;
  end
  else
  begin
    AResponse.Content := 'Método no permitido';
    AResponse.Code := 405;
  end;
end;

procedure TForm1.InicializarBaseDatos;
begin
  try
    // Configurar conexión SQLite
    SQLite3Connection1.DatabaseName := 'clima.db';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;

    // Crear tabla si no existe
    SQLQuery1.SQL.Text :=
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
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

  except
    on E: Exception do
      ShowMessage('Error inicializando BD: ' + E.Message);
  end;
end;

procedure TForm1.InicializarChart;
var
  i: Integer;
  Serie: TLineSeries;
begin
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add('Monitoreo de Estaciones Ambientales');

  // Crear 10 series (una por estación)
  for i := 0 to 9 do
  begin
    Serie := TLineSeries.Create(Chart1);
    Serie.Title := 'Estación ' + IntToStr(i + 1);
    // Colores distintos para cada serie
    case i of
      0: Serie.SeriesColor := clBlue;
      1: Serie.SeriesColor := clRed;
      2: Serie.SeriesColor := clGreen;
      3: Serie.SeriesColor := clPurple;
      4: Serie.SeriesColor := clMaroon;
      5: Serie.SeriesColor := clTeal;
      6: Serie.SeriesColor := clNavy;
      7: Serie.SeriesColor := clOlive;
      8: Serie.SeriesColor := clGray;
      9: Serie.SeriesColor := clFuchsia;
    end;
    Serie.ShowPoints := True;
    Serie.LinePen.Width := 2;

    // Inicialmente solo mostrar la primera serie
    if i = 0 then
      Serie.Active := True
    else
      Serie.Active := False;

    Chart1.AddSeries(Serie);
  end;

  // Configurar ejes
  Chart1.BottomAxis.Title.Caption := 'Tiempo';
  Chart1.LeftAxis.Title.Caption := 'Valor';
  Chart1.Legend.Visible := True;
end;

procedure TForm1.ProcesarDatosJSON(const JSONData: string);
var
  Parser: TJSONParser;
  JSONObject: TJSONObject;
  ide: Integer;
  sFe, sHo: string;
  P25, P10, nTe, nHr, nPa: Double;
begin
  try
    // Parsear JSON
    Parser := TJSONParser.Create(JSONData);
    try
      JSONObject := TJSONObject(Parser.Parse);

      // Extraer datos del JSON
      ide := JSONObject.Get('ide').AsInteger;
      sFe := JSONObject.Get('sFe').AsString;
      sHo := JSONObject.Get('sHo').AsString;
      P25 := JSONObject.Get('P25').AsFloat;
      P10 := JSONObject.Get('P10').AsFloat;
      nTe := JSONObject.Get('nTe').AsFloat;
      nHr := JSONObject.Get('nHr').AsFloat;
      nPa := JSONObject.Get('nPa').AsFloat;

      // Insertar en base de datos
      SQLQuery1.SQL.Text :=
        'INSERT INTO estaciones (ide, sFe, sHo, P25, P10, nTe, nHr, nPa) ' +
        'VALUES (:ide, :sFe, :sHo, :P25, :P10, :nTe, :nHr, :nPa)';

      SQLQuery1.ParamByName('ide').AsInteger := ide;
      SQLQuery1.ParamByName('sFe').AsString := sFe;
      SQLQuery1.ParamByName('sHo').AsString := sHo;
      SQLQuery1.ParamByName('P25').AsFloat := P25;
      SQLQuery1.ParamByName('P10').AsFloat := P10;
      SQLQuery1.ParamByName('nTe').AsFloat := nTe;
      SQLQuery1.ParamByName('nHr').AsFloat := nHr;
      SQLQuery1.ParamByName('nPa').AsFloat := nPa;

      SQLQuery1.ExecSQL;
      SQLTransaction1.Commit;

      // Actualizar chart
      ActualizarChart(ide, P25, P10, nTe, nHr, nPa);

      // Exportar chart a PNG
      ExportarChartPNG;

    finally
      Parser.Free;
    end;

  except
    on E: Exception do
      ShowMessage('Error procesando JSON: ' + E.Message);
  end;
end;

procedure TForm1.ActualizarChart(StationID: Integer; P25, P10, nTe, nHr, nPa: Double);
var
  Serie: TLineSeries;
  XValue: Double;
begin
  // Obtener la serie correspondiente a la estación (índice 0-based)
  if (StationID >= 1) and (StationID <= 10) then
  begin
    Serie := TLineSeries(Chart1.Series[StationID - 1]);

    // Calcular nuevo valor X (incremento secuencial)
    if Serie.Count > 0 then
      XValue := Serie.GetXValue(Serie.Count - 1) + 1
    else
      XValue := 0;

    // Agregar nuevo punto (usamos P25 como ejemplo)
    Serie.AddXY(XValue, P25);

    // Scroll izquierdo: mantener solo últimos 30 puntos
    if Serie.Count > 30 then
      Serie.Delete(0);

    // Auto-ajustar rango del eje X para scrolling
    if Serie.Count > 1 then
    begin
      Chart1.BottomAxis.Range.Min := XValue - 29;
      Chart1.BottomAxis.Range.Max := XValue + 1;
    end;
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  i: Integer;
begin
  // Mostrar solo la serie seleccionada
  for i := 0 to Chart1.SeriesCount - 1 do
  begin
    if i = ComboBox1.ItemIndex then
      TLineSeries(Chart1.Series[i]).Active := True
    else
      TLineSeries(Chart1.Series[i]).Active := False;
  end;
  Chart1.Invalidate; // Refrescar el chart
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ExportarChartPNG;
  ShowMessage('Gráfico exportado');
end;

procedure TForm1.ExportarChartPNG;
begin
  Inc(ContadorExportacion);
  Chart1.SaveToFile(TPortableNetworkGraphic,
    'chart_' + Format('%.4d', [ContadorExportacion]) + '.png');
end;

end.
