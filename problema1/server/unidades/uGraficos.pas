unit uGraficos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TAGraph, TASeries;

type
  TGestorGraficos = class
  private
    FChart: TChart;
    FContadorExportacion: Integer;
    procedure ConfigurarSerie(Serie: TLineSeries; Indice: Integer);
  public
    constructor Create(Chart: TChart);
    procedure InicializarSeries;
    procedure ActualizarSerie(NumEstacion: Integer; Valor: Double);
    procedure MostrarSerie(NumEstacion: Integer);
    procedure MostrarTodasLasSeries;
    procedure ExportarPNG;
  end;

const
  MAX_PUNTOS = 30;
  COLORES_SERIES: array[0..9] of TColor = (
    clBlue, clRed, clGreen, clPurple, clMaroon,
    clTeal, clNavy, clOlive, clGray, clFuchsia
  );

implementation

constructor TGestorGraficos.Create(Chart: TChart);
begin
  FChart := Chart;
  FContadorExportacion := 0;
end;

procedure TGestorGraficos.ConfigurarSerie(Serie: TLineSeries; Indice: Integer);
begin
  Serie.Title := 'Estación ' + IntToStr(Indice + 1);
  Serie.SeriesColor := COLORES_SERIES[Indice];
  Serie.ShowPoints := True;
  Serie.LinePen.Width := 2;
  Serie.Active := True;  // Todas las series activas por defecto
end;

procedure TGestorGraficos.InicializarSeries;
var
  i: Integer;
  Serie: TLineSeries;
begin
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add('Monitoreo de Estaciones Ambientales');
  
  for i := 0 to 9 do
  begin
    Serie := TLineSeries.Create(FChart);
    ConfigurarSerie(Serie, i);
    FChart.AddSeries(Serie);
  end;
  
  FChart.BottomAxis.Title.Caption := 'Tiempo';
  FChart.LeftAxis.Title.Caption := 'Valor PM2.5 (µg/m³)';
  FChart.Legend.Visible := True;
end;

procedure TGestorGraficos.ActualizarSerie(NumEstacion: Integer; Valor: Double);
var
  Serie: TLineSeries;
  XValue: Double;
begin
  if (NumEstacion < 1) or (NumEstacion > 10) then
    Exit;
  
  Serie := TLineSeries(FChart.Series[NumEstacion - 1]);
  
  if Serie.Count > 0 then
    XValue := Serie.GetXValue(Serie.Count - 1) + 1
  else
    XValue := 0;
  
  Serie.AddXY(XValue, Valor);
  
  if Serie.Count > MAX_PUNTOS then
    Serie.Delete(0);
  
  if Serie.Count > 1 then
  begin
    FChart.BottomAxis.Range.Min := XValue - (MAX_PUNTOS - 1);
    FChart.BottomAxis.Range.Max := XValue + 1;
  end;
end;

procedure TGestorGraficos.MostrarSerie(NumEstacion: Integer);
var
  i: Integer;
begin
  // Validar que el índice esté en rango
  if (NumEstacion < 0) or (NumEstacion >= FChart.SeriesCount) then
    Exit;
  
  // Activar solo la serie seleccionada
  for i := 0 to FChart.SeriesCount - 1 do
  begin
    if TObject(FChart.Series[i]) is TLineSeries then
      TLineSeries(FChart.Series[i]).Active := (i = NumEstacion);
  end;
  
  // Actualizar el título del gráfico
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add(Format('Monitoreo Estación %d - PM2.5', [NumEstacion + 1]));
  
  // Forzar repintado
  FChart.Invalidate;
  FChart.Repaint;
end;

procedure TGestorGraficos.MostrarTodasLasSeries;
var
  i: Integer;
begin
  // Activar todas las series
  for i := 0 to FChart.SeriesCount - 1 do
  begin
    if TObject(FChart.Series[i]) is TLineSeries then
      TLineSeries(FChart.Series[i]).Active := True;
  end;
  
  // Restaurar título original
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add('Monitoreo de Estaciones Ambientales - Todas');
  
  // Forzar repintado
  FChart.Invalidate;
  FChart.Repaint;
end;

procedure TGestorGraficos.ExportarPNG;
begin
  Inc(FContadorExportacion);
  FChart.SaveToFile(TPortableNetworkGraphic,
    Format('exportacion/chart_%4.4d.png', [FContadorExportacion]));
end;

end.