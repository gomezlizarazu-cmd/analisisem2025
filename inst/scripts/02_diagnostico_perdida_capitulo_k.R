# Diagnostico operativo de perdida en Capitulo K.
# Ejecutar desde RStudio. Este script no define reglas de flujo.

ruta_paquete <- if (exists("ruta_paquete", inherits = TRUE)) {
  ruta_paquete
} else {
  "C:/Users/gomez/.codex/worktrees/733e/analisisem2025"
}

fecha_corte <- if (exists("fecha_corte", inherits = TRUE)) {
  fecha_corte
} else {
  Sys.getenv("FECHA_CORTE_EM", unset = format(Sys.Date(), "%Y%m%d"))
}

ruta_base <- if (exists("ruta_base", inherits = TRUE)) {
  ruta_base
} else {
  Sys.getenv(
    "RUTA_BASE_EM_COMPLETA",
    unset = file.path(
      "C:/Users/gomez/OneDrive/Documentos/Validaciones_EM_Basicas_2026",
      paste0("em_completa_sin_tematica_", fecha_corte, ".rds")
    )
  )
}

ruta_diccionario <- if (exists("ruta_diccionario", inherits = TRUE)) {
  ruta_diccionario
} else {
  Sys.getenv(
    "RUTA_DICCIONARIO_EM",
    unset = file.path(ruta_paquete, "inst/diccionario/Diccionario_em2025.xlsx")
  )
}

carpeta_salida <- if (exists("carpeta_salida", inherits = TRUE)) {
  carpeta_salida
} else {
  file.path(
    "C:/Users/gomez/OneDrive/Documentos/Validaciones_EM_Basicas_2026/ImputacionK",
    paste0("diagnostico_perdida_capitulo_k_", fecha_corte)
  )
}

if (!file.exists(ruta_base)) {
  stop("No existe `ruta_base`: ", ruta_base)
}

devtools::load_all(ruta_paquete)

obj_base <- readRDS(ruta_base)
dfs <- if (is.list(obj_base) && "dfs" %in% names(obj_base)) {
  obj_base$dfs
} else if (is.list(obj_base) && all(vapply(obj_base, is.data.frame, logical(1)))) {
  obj_base
} else {
  stop("`ruta_base` debe contener una lista `dfs` o una lista nombrada de data frames.")
}

diccionario <- NULL
if (file.exists(ruta_diccionario) && requireNamespace("readxl", quietly = TRUE)) {
  diccionario <- tryCatch(
    tibble::as_tibble(readxl::read_excel(ruta_diccionario)),
    error = function(e) NULL
  )
}

diag_k <- diagnostico_flujo_capitulo_k(
  dfs = dfs,
  diccionario = diccionario
)

resumen_perdida_k_con_regla <- diag_k$resumen_variables |>
  dplyr::left_join(
    diag_k$reglas_flujo |>
      dplyr::select(
        .data$variable,
        .data$orden_flujo,
        .data$condicion_debe_responder,
        .data$regla_r,
        .data$comentario
      ),
    by = "variable"
  ) |>
  dplyr::arrange(dplyr::desc(.data$pct_vacio_critico_sobre_deben))

.resumen_para_comparar_edad_k <- function(diag) {
  diag$resumen_variables |>
    dplyr::select(
      .data$variable,
      deben_responder = .data$deben_responder,
      vacios_criticos = .data$vacios_criticos,
      respuestas_fuera_flujo = .data$respuestas_fuera_flujo,
      flujos_indeterminados = .data$flujos_indeterminados
    )
}

if (exists("diag_k_antes_edad", inherits = TRUE)) {
  resumen_antes <- .resumen_para_comparar_edad_k(diag_k_antes_edad)
  escenario_referencia <- "diag_k_antes_edad"
} else {
  resumen_antes <- .resumen_para_comparar_edad_k(diag_k)
  escenario_referencia <- "sin_diag_k_antes_edad_disponible"
}

resumen_despues <- .resumen_para_comparar_edad_k(diag_k)

comparacion_edad_reglas_k <- resumen_antes |>
  dplyr::rename_with(
    ~ paste0(.x, "_antes"),
    -dplyr::all_of("variable")
  ) |>
  dplyr::full_join(
    resumen_despues |>
      dplyr::rename_with(
        ~ paste0(.x, "_despues"),
        -dplyr::all_of("variable")
      ),
    by = "variable"
  ) |>
  dplyr::mutate(
    escenario_referencia = escenario_referencia,
    dif_abs_deben_responder = abs(.data$deben_responder_despues - .data$deben_responder_antes),
    dif_abs_vacios_criticos = abs(.data$vacios_criticos_despues - .data$vacios_criticos_antes),
    dif_abs_respuestas_fuera_flujo = abs(.data$respuestas_fuera_flujo_despues - .data$respuestas_fuera_flujo_antes),
    dif_abs_flujos_indeterminados = abs(.data$flujos_indeterminados_despues - .data$flujos_indeterminados_antes)
  ) |>
  dplyr::arrange(
    dplyr::desc(.data$dif_abs_deben_responder),
    dplyr::desc(.data$dif_abs_vacios_criticos),
    .data$variable
  )

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    diag_k = diag_k,
    resumen_perdida_k_con_regla = resumen_perdida_k_con_regla,
    comparacion_edad_reglas_k = comparacion_edad_reglas_k
  ),
  file.path(carpeta_salida, paste0("diagnostico_perdida_capitulo_k_", fecha_corte, ".rds"))
)

message("Objetos disponibles: diag_k, resumen_perdida_k_con_regla, comparacion_edad_reglas_k")
message("Salida guardada fuera del repositorio en: ", carpeta_salida)
