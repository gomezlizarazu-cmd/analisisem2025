# ============================================================
# 05B_Prueba_Aceptacion_Base_OSIS_K43_K44_20260703.R
#
# Conecta la salida privada de imputación K43-K44 con la función
# general de aceptación OSIS. No imputa ni genera textos abiertos.
# Debe ejecutarse manualmente en RStudio después del script 05.
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

# Ajuste únicamente este nombre si el script privado 05 usa otro nombre
# de salida. La función acepta una tabla o una lista que contenga la tabla.
ruta_base_final_k41_k44 <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "05_imputacion_k43_k44",
  paste0("base_k_imputada_k41_k44_", fecha_corte, ".rds")
)

carpeta_salida <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "05B_prueba_aceptacion_osis_k43_k44"
)
ruta_rds_aceptacion <- file.path(
  carpeta_salida,
  paste0("aceptacion_osis_k43_k44_", fecha_corte, ".rds")
)
ruta_excel_aceptacion <- file.path(
  carpeta_salida,
  paste0("aceptacion_osis_k43_k44_", fecha_corte, ".xlsx")
)

stopifnot(
  dir.exists(ruta_paquete),
  file.exists(ruta_em_original),
  file.exists(ruta_insumos),
  file.exists(ruta_base_final_k41_k44)
)
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

devtools::load_all(ruta_paquete)

em_original <- readRDS(ruta_em_original)
insumos_flujo <- readRDS(ruta_insumos)
base_final_k41_k44 <- readRDS(ruta_base_final_k41_k44)

universo_independiente <- function(datos) {
  auditoria <- universo_independientes_k41_k44(datos)
  dplyr::if_else(
    auditoria$flujo_indeterminado,
    NA,
    auditoria$universo_k41_k44
  )
}

configuracion_k43_k44 <- list(
  NPCKP43_1 = list(
    variable = "NPCKP43_1",
    variable_original = "NPCKP43_1_original",
    descripcion = "Actividad ejercida en negocio, empresa o finca",
    universo = universo_independiente,
    valores_validos = c("1", "2"),
    tipo = "categorica",
    validar_distribucion = TRUE,
    columna_flag_imputado = "NPCKP43_1_flag_imputado",
    columna_metodo_imputacion = "NPCKP43_1_metodo_imputacion"
  ),
  NPCKP43_1A = list(
    variable = "NPCKP43_1A",
    variable_original = "NPCKP43_1A_original",
    descripcion = "Propiedad del negocio, empresa o finca",
    universo = function(datos) {
      universo_independiente(datos) &
        normalizar_categoria_distribucion_osis(
          datos$NPCKP43_1
        ) == "1"
    },
    valores_validos = c("1", "2"),
    tipo = "categorica",
    validar_distribucion = TRUE,
    variable_madre = "NPCKP43_1",
    condicion_subordinada = function(datos) {
      normalizar_categoria_distribucion_osis(
        datos$NPCKP43_1
      ) == "1"
    },
    columna_flag_imputado = "NPCKP43_1A_flag_imputado",
    columna_metodo_imputacion = "NPCKP43_1A_metodo_imputacion"
  ),
  NPCKP44_1 = list(
    variable = "NPCKP44_1",
    variable_original = "NPCKP44_1_original",
    descripcion = "Razón principal para trabajar independientemente",
    universo = universo_independiente,
    valores_validos = as.character(1:11),
    tipo = "categorica",
    validar_distribucion = TRUE,
    columna_flag_imputado = "NPCKP44_1_flag_imputado",
    columna_metodo_imputacion = "NPCKP44_1_metodo_imputacion"
  ),
  NPCKP44_1A = list(
    variable = "NPCKP44_1A",
    variable_original = "NPCKP44_1A_original",
    descripcion = "Especificación abierta de la razón 11",
    universo = function(datos) {
      universo_independiente(datos) &
        normalizar_categoria_distribucion_osis(
          datos$NPCKP44_1
        ) == "11"
    },
    tipo = "texto_abierto",
    validar_distribucion = FALSE,
    variable_madre = "NPCKP44_1",
    condicion_subordinada = function(datos) {
      normalizar_categoria_distribucion_osis(
        datos$NPCKP44_1
      ) == "11"
    },
    columna_flag_imputado = "NPCKP44_1A_flag_imputado",
    condicion_no_imputable = function(datos) {
      datos$NPCKP44_1_flag_imputado %in% TRUE &
        normalizar_categoria_distribucion_osis(
          datos$NPCKP44_1
        ) == "11"
    },
    columna_flag_no_imputable =
      "NPCKP44_1A_flag_no_imputable",
    columna_metodo_no_imputable =
      "NPCKP44_1A_metodo_imputacion",
    metodo_no_imputable_esperado =
      "texto_abierto_no_imputado"
  )
)

resultado_aceptacion <- prueba_aceptacion_base_osis(
  em_original = em_original,
  base_final = base_final_k41_k44,
  insumos_flujo = insumos_flujo,
  configuracion_variables = configuracion_k43_k44,
  ruta_rds = ruta_rds_aceptacion,
  ruta_excel = ruta_excel_aceptacion,
  sobrescribir = FALSE,
  detener_si_error = TRUE
)

print(resultado_aceptacion$controles, n = Inf, width = Inf)
message(
  "Conclusión de la aceptación K43-K44: ",
  resultado_aceptacion$parametros$estado_general
)
