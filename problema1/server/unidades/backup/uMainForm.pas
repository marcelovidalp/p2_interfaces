unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries, uServidor, uDB, uGraficos, DateUtils;

type
  TFormPrincipal = class(TForm)
    Chart1: TChart;
    ComboBox1: TComboBox;
    Button1: TButton;
    CheckBoxRecepcion: TCheckBox;
    LabelEstado: TLabel;
    Timer1: TTimer;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBoxRecepcionChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    
  private
    FServidor: TServidorHTTP;
    FBaseDatos: TGestorBaseDatos;
    FGraficos: TGestorGraficos;
    FRecepcionActiva: Boolean;
    
    procedure OnDatosRecibidos(const Datos: TDatosEstacion);
    procedure ActualizarEstadoRecepcion;
    
  public
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.lfm}

procedure TFormPrincipal.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Caption := 'Sistema de Monitoreo Ambiental - Data Logger';
  
  // Inicializar ComboBox
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add('Todas las Estaciones');
  for i := 1 to 10 do
    ComboBox1.Items.Add('Estación ' + IntToStr(i));
  ComboBox1.ItemIndex := 0;
  
  // Configurar timer
  Timer1.Interval := 1000;
  Timer1.Enabled := True;
  
  // Inicializar estado
  FRecepcionActiva := True;
  CheckBoxRecepcion.Checked := True;
  
  // Configurar datos iniciales
  ActualizarFechaHora(nil);
  
  // Inicializar base de datos
  try
    FBaseDatos := TGestorBaseDatos.Create('clima.db');
    FBaseDatos.InicializarTabla;
  except
    on E: Exception do
      ShowMessage('Error al inicializar base de datos: ' + E.Message);
  end;
  
  // Inicializar gráficos
  FGraficos := TGestorGraficos.Create(Chart1);
  FGraficos.InicializarSeries;
  FGraficos.MostrarTodasLasSeries;
  
  // Inicializar servidor
  FServidor := TServidorHTTP.Create(8080);
  FServidor.OnDatosRecibidos := @OnDatosRecibidos;
  FServidor.Iniciar;
  
  LabelEstado.Caption := 'Servidor activo en puerto 8080';
  ActualizarEstadoRecepcion;
  
  if not DirectoryExists('exportacion') then
    CreateDir('exportacion');
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  // No necesitamos liberar Timer1 porque es un componente visual
  if Assigned(FServidor) then
    FServidor.Free;
  if Assigned(FBaseDatos) then
    FBaseDatos.Free;
  if Assigned(FGraficos) then
    FGraficos.Free;
end;

procedure TFormPrincipal.ComboBox1Change(Sender: TObject);
begin
  if Assigned(FGraficos) and (ComboBox1.ItemIndex >= 0) then
  begin
    if ComboBox1.ItemIndex = 0 then
    begin
      // Mostrar todas las estaciones
      FGraficos.MostrarTodasLasSeries;
      LabelEstado.Caption := 'Mostrando todas las estaciones';
    end
    else
    begin
      // Mostrar estación específica (índice - 1 porque el 0 es "Todas")
      FGraficos.MostrarSerie(ComboBox1.ItemIndex - 1);
      LabelEstado.Caption := Format('Mostrando datos de %s', 
        [ComboBox1.Items[ComboBox1.ItemIndex]]);
    end;
  end;
end;

procedure TFormPrincipal.Button1Click(Sender: TObject);
begin
  FGraficos.ExportarPNG;
  ShowMessage('Gráfico exportado exitosamente');
end;

procedure TFormPrincipal.OnDatosRecibidos(const Datos: TDatosEstacion);
begin
  if not FRecepcionActiva then
  begin
    LabelEstado.Caption := 'Datos pausados - No se procesan datos';
    Exit;
  end;
  
  try
    if Assigned(FBaseDatos) then
      FBaseDatos.GuardarDatos(Datos);
    
    if Assigned(FGraficos) then
      FGraficos.ActualizarSerie(Datos.ide, Datos.P25);
    
    LabelEstado.Caption := Format('✓ Estación %d - Temp: %.1f°C - PM2.5: %.1f μg/m³',
      [Datos.ide, Datos.nTe, Datos.P25]);
      
  except
    on E: Exception do
      LabelEstado.Caption := 'Error al procesar datos: ' + E.Message;
  end;
end;

procedure TFormPrincipal.Timer1Timer(Sender: TObject);
begin
  // Actualizar hora en la etiqueta del estado
  LabelEstado.Caption := LabelEstado.Caption + ' - ' + 
    FormatDateTime('hh:nn:ss', Now);
end;

procedure TFormPrincipal.CheckBoxRecepcionChange(Sender: TObject);
begin
  FRecepcionActiva := CheckBoxRecepcion.Checked;
  ActualizarEstadoRecepcion;
end;

procedure TFormPrincipal.ActualizarEstadoRecepcion;
begin
  if FRecepcionActiva then
  begin
    LabelEstado.Caption := 'Recepción ACTIVA';
    LabelEstado.Font.Color := clGreen;
    CheckBoxRecepcion.Caption := 'Recibir datos';
  end
  else
  begin
    LabelEstado.Caption := 'Recepción PAUSADA';
    LabelEstado.Font.Color := clRed;
    CheckBoxRecepcion.Caption := 'Pausado';
  end;
end;

end.
