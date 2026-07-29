# ============================================================
# 04B_Ejemplo_Funcion_Aceptacion_OSIS_K41_K42.R
#
# Ejemplo alternativo de migración del patrón histórico K41-K42
# hacia prueba_aceptacion_base_osis().
#
# No reemplaza, modifica ni reabre el script 04 aprobado. Genera
# archivos nuevos y debe ejecutarse manualmente en RStudio.
# ============================================================

options(scipen = 999)

fecha_corte <- "20260703"
ruta_paquete <-
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"

carpeta_caps <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive",
  "DANE",
  "Multiproposito",
  "Validacion",
  "Encuestas",
  "Validar",
  paste0("CAP_EM_", fecha_corte)
)

ruta_em_original <- file.path(
  carpeta_caps,
  paste0("em_completa_sin_tematica_", fecha_corte, ".rds")
)
ruta_insumos <- file.path(
  carpeta_caps,
  "diagnostico_flujo_capK_diccionario",
  paste0("insumos_imputacion_capK_", fecha_corte, ".rds")
)
ruta_base_final <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "03_diagnostico_npckp36_npckp37",
  paste0(
    "base_k_imputada_k41_k42_cierre_npckp37_",
    fecha_corte,
    ".rds"
  )
)
carpeta_salida <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "04B_ejemplo_funcion_aceptacion_osis"
)
ruta_rds_aceptacion <- file.path(
  carpeta_salida,
  paste0("aceptacion_osis_k41_k42_funcion_", fecha_corte, ".rds")
)
ruta_excel_aceptacion <- file.path(
  carpeta_salida,
  paste0("aceptacion_osis_k41_k42_funcion_", fecha_corte, ".xlsx")
)

stopifnot(
  dir.exists(ruta_paquete),
  file.exists(ruta_em_original),
  file.exists(ruta_insumos),
  file.exists(ruta_base_final)
)
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

devtools::load_all(ruta_paquete)

em_original <- readRDS(ruta_em_original)
insumos_flujo <- readRDS(ruta_insumos)
base_final_k41_k42 <- readRDS(ruta_base_final)

universo_independiente <- function(datos) {
  auditoria <- universo_independientes_k41_k44(datos)
  dplyr::if_else(
    auditoria$flujo_indeterminado,
    NA,
    auditoria$universo_k41_k44
  )
}

es_monto_valido <- function(x) {
  depurar_monto_capitulo_k(x)$monto_valido %in% TRUE
}

es_codigo_especial <- function(x) {
  suppressWarnings(as.numeric(as.character(x))) %in% c(98, 99)
}

es_monto_o_codigo <- function(x) {
  es_monto_valido(x) | es_codigo_especial(x)
}

es_mes_valido <- function(x) {
  numero <- suppressWarnings(as.numeric(as.character(x)))
  !is.na(numero) &
    is.finite(numero) &
    abs(numero - round(numero)) < 0.00000001 &
    numero >= 1 &
    numero <= 12
}

configuracion_k41_k42 <- list(
  NPCKP36 = list(
    variable = "NPCKP36",
    variable_original = "NPCKP36_original",
    descripcion = "Ingreso neto mensual como independiente",
    universo = universo_independiente,
    tipo = "numerica",
    validar_distribucion = FALSE,
    validador_dominio = es_monto_o_codigo,
    validador_observado_preservable = es_monto_valido,
    columna_flag_imputado = "NPCKP36_flag_imputado",
    columna_metodo_imputacion = "NPCKP36_metodo_imputacion"
  ),
  NPCKP36A = list(
    variable = "NPCKP36A",
    variable_original = "NPCKP36A_original",
    descripcion = "Confirmación del ingreso neto mensual",
    universo = universo_independiente,
    tipo = "numerica",
    validar_distribucion = FALSE,
    validador_dominio = es_monto_o_codigo,
    validador_observado_preservable = es_monto_valido,
    columna_flag_imputado = "NPCKP36A_flag_imputado",
    columna_metodo_imputacion = "NPCKP36A_metodo_imputacion"
  ),
  NPCKP37 = list(
    variable = "NPCKP37",
    variable_original = "NPCKP37_original",
    descripcion = "Número de meses al que corresponde el ingreso",
    universo = universo_independiente,
    valores_validos = as.character(c(1:12, 98, 99)),
    tipo = "categorica",
    validar_distribucion = TRUE,
    validador_dominio = function(x) {
      es_mes_valido(x) | es_codigo_especial(x)
    },
    validador_observado_preservable = es_mes_valido,
    columna_flag_imputado = "NPCKP37_flag_imputado",
    columna_metodo_imputacion = "NPCKP37_metodo_imputacion"
  )
)

resultado_aceptacion <- prueba_aceptacion_base_osis(
  em_original = em_original,
  base_final = base_final_k41_k42,
  insumos_flujo = insumos_flujo,
  configuracion_variables = configuracion_k41_k42,
  ruta_rds = ruta_rds_aceptacion,
  ruta_excel = ruta_excel_aceptacion,
  sobrescribir = FALSE,
  detener_si_error = TRUE
)

print(resultado_aceptacion$controles, n = Inf, width = Inf)
message(
  "Conclusión sintética del ejemplo K41-K42: ",
  resultado_aceptacion$parametros$estado_general
)
