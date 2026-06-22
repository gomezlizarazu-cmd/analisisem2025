# Ejemplo manual para reporte de cascada en segmentos problematicos de Muestras.
# Ejecutar desde RStudio con los objetos `dfs`, `diag_tres`,
# `diag_con_tematica`, `carpeta_salida` y `fecha_corte` ya cargados.

devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")

segmentos_problema <- c(
  "17183", "17189", "17191", "17207",
  "30303", "30305", "30324", "30328", "30439"
)

salida_segmentos <- reporte_segmentos_cascada_muestras(
  segmentos = segmentos_problema,
  dfs = dfs,
  diag_tres = diag_tres,
  diag_con_tematica = diag_con_tematica,
  carpeta_salida = carpeta_salida,
  fecha_corte = fecha_corte,
  formato_exportacion = "xlsx"
)

salida_segmentos$resumen_segmento
salida_segmentos$cascada_detallada_segmento
salida_segmentos$resumen_detalle_segmento
salida_segmentos$personas_asociadas_segmento
salida_segmentos$detalle_caidas
salida_segmentos$comparacion_con_sin_tematica
salida_segmentos$segmentos_no_encontrados
