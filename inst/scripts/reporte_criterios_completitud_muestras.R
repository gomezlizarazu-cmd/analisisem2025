# Ejemplo de uso manual en RStudio.
#
# Este archivo no ejecuta nada automaticamente. Active manualmente las lineas
# que necesite despues de cargar o construir:
# - dfs
# - diag_tres
# - diag_con_tematica, si aplica
# - dic_mpios, si aplica
#
# devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")
#
# salida <- generar_reporte_criterios_completitud_muestras(
#   dfs = dfs,
#   diag_tres = diag_tres,
#   diag_con_tematica = diag_con_tematica,
#   dic_mpios = dic_mpios,
#   municipios_objetivo = c("Zipacon", "Zipacón", "Soacha"),
#   incluir_recuperables = TRUE,
#   exportar = TRUE,
#   ruta_salida = "C:/Users/gomez/OneDrive/Documentos/analisisem2025/outputs/muestras_completitud"
# )
#
# salida$criterios_general
# salida$zipacon_soacha_detalle
# salida$zipacon_soacha_resumen
# salida$resumen_final_niveles
# salida$resumen_final_municipios_niveles
# salida$tabla_variables_criterios
# salida$cascada_encuestas_muestras
# salida$comparacion_cascada_con_sin_tematica
