unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  TAGraph, TASeries, uServidor, uDB, uGraficos;

type
  TFormPrincipal = class(TForm)
    Chart1: TChart;
    ComboBox1: TComboBox;
    Button1: TButton;
    LabelEstado: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FServidor: TServidorHTTP;
    FBaseDatos: TGestorBaseDatos;
    FGraficos: TGestorGraficos;
    procedure OnDatosRecibidos(const Datos: TDatosEstacion);
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
  Caption := 'Sistema de Monitoreo Ambiental';
  
  ComboBox1.Items.Clear;
  for i := 1 to 10 do
    ComboBox1.Items.Add('Estación ' + IntToStr(i));
  ComboBox1.ItemIndex := 0;
  
  FBaseDatos := TGestorBaseDatos.Create('clima.db');
  FBaseDatos.InicializarTabla;
  
  FGraficos := TGestorGraficos.Create(Chart1);
  FGraficos.InicializarSeries;
  
  FServidor := TServidorHTTP.Create(8080);
  FServidor.OnDatosRecibidos := @OnDatosRecibidos;
  FServidor.Iniciar;
  
  LabelEstado.Caption := 'Servidor activo en puerto 8080';
  
  if not DirectoryExists('exportacion') then
    CreateDir('exportacion');
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  FServidor.Free;
  FBaseDatos.Free;
  FGraficos.Free;
end;

procedure TFormPrincipal.ComboBox1Change(Sender: TObject);
begin
  FGraficos.MostrarSerie(ComboBox1.ItemIndex);
end;

procedure TFormPrincipal.Button1Click(Sender: TObject);
begin
  FGraficos.ExportarPNG;
  ShowMessage('Gráfico exportado exitosamente');
end;

procedure TFormPrincipal.OnDatosRecibidos(const Datos: TDatosEstacion);
begin
  FBaseDatos.GuardarDatos(Datos);
  FGraficos.ActualizarSerie(Datos.ide, Datos.P25);
  LabelEstado.Caption := Format('Última recepción: Estación %d - %s %s',
    [Datos.ide, Datos.sFe, Datos.sHo]);
end;

end.