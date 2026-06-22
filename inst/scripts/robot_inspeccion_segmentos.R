# Ejemplo manual para exportar sabana de segmentos problematicos de Muestras.
# Ejecutar desde RStudio con `dfs`, `carpeta_salida`, `fecha_corte` y,
# opcionalmente, `salida_segmentos` ya cargados.

devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")

segmentos_problema <- c(
  "25898_17183",
  "25898_17189",
  "25898_17191",
  "25898_17207",
  "25473_30303",
  "25473_30305",
  "25473_30324",
  "25473_30328",
  "25754_30439"
)

robot_seg <- robot_inspeccion_segmentos(
  dfs = dfs,
  segmentos = segmentos_problema,
  salida_segmentos = salida_segmentos,
  carpeta_salida = carpeta_salida,
  fecha_corte = fecha_corte,
  exportar_excel = TRUE,
  exportar_robots_individuales = FALSE
)

robot_seg$segmentos_solicitados
robot_seg$segmentos_encontrados
robot_seg$segmentos_no_encontrados
robot_seg$directorios_segmentos
robot_seg$archivo_excel
