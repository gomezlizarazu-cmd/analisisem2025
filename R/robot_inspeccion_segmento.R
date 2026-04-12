#' Robot de inspeccion de caidas por segmento a nivel persona
#'
#' Construye una base de personas del capitulo `E` para un `SEGMENTO`
#' especificado y la cruza con el reporte consolidado de
#' `diagnostico_caidas_tres_criterios()`. La salida permite identificar, para
#' cada persona del segmento, si cae o no cae y la razon principal reportada.
#'
#' @details
#' La unidad analitica de la salida es persona:
#'
#' - vivienda: `DIRECTORIO`
#' - hogar: `DIRECTORIO + SECUENCIA_P`
#' - persona: `DIRECTORIO + SECUENCIA_P + ORDEN`
#'
#' El universo se toma desde el capitulo `E`, por lo que la salida incluye
#' personas registradas en ese capitulo para el segmento consultado. El cruce
#' con `reporte_final_caidas` mantiene explicitamente ese nivel persona usando
#' la llave `DIRECTORIO + SECUENCIA_P + ORDEN`.
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param SEGMENTO Identificador del segmento a inspeccionar.
#' @param base_hogar_cap Capitulo base del universo hogar para
#'   `diagnostico_caidas_tres_criterios()`.
#' @param base_persona_cap Capitulo base del universo persona para
#'   `diagnostico_caidas_tres_criterios()`.
#' @param edad_var Variable de edad a usar en los criterios que la requieren.
#' @param diagnostico_tc Resultado previo de `diagnostico_caidas_tres_criterios()`.
#'   Si se suministra, la funcion reutiliza ese objeto y evita recalcular el
#'   diagnostico completo.
#' @param exportar_excel Si es `TRUE`, exporta el resumen y el detalle a Excel.
#' @param archivo Ruta del archivo Excel.
#'
#' @return Lista con:
#' \describe{
#'   \item{parametros}{Tabla con el segmento consultado.}
#'   \item{resumen}{Resumen general del segmento consultado a nivel persona.}
#'   \item{resumen_criterio}{Conteo de personas por criterio de reporte.}
#'   \item{personas_segmento}{Base a nivel persona con bandera de caida y
#'   razones.}
#'   \item{diagnostico_tc}{Salida completa de `diagnostico_caidas_tres_criterios()`.}
#'   \item{archivo}{Ruta del Excel exportado cuando `exportar_excel = TRUE`.}
#' }
#'
#' @export
robot_inspeccion_segmento <- function(
    dfs,
    SEGMENTO,
    base_hogar_cap = "B",
    base_persona_cap = "E",
    edad_var = "NPCEP4",
    diagnostico_tc = NULL,
    exportar_excel = FALSE,
    archivo = "robot_inspeccion_segmento.xlsx"
) {

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (missing(SEGMENTO) || length(SEGMENTO) != 1 || is.na(SEGMENTO)) {
    stop("`SEGMENTO` debe venir informado con un unico valor.")
  }

  names(dfs) <- toupper(names(dfs))

  if (!"E" %in% names(dfs) || !"A" %in% names(dfs)) {
    stop("Se requieren al menos los capitulos `A` y `E` para inspeccionar por segmento.")
  }

  A <- dfs[["A"]]
  E <- dfs[["E"]]

  if (!all(c("DIRECTORIO", "SEGMENTO") %in% names(A))) {
    stop("El capitulo `A` debe contener `DIRECTORIO` y `SEGMENTO`.")
  }

  if (!all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(E))) {
    stop("El capitulo `E` debe contener `DIRECTORIO`, `SECUENCIA_P` y `ORDEN`.")
  }

  A_seg <- A %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::select(DIRECTORIO, SEGMENTO, dplyr::any_of(c("UUID", "CLASE"))) %>%
    dplyr::mutate(
      SEGMENTO = as.character(SEGMENTO)
    ) %>%
    dplyr::distinct(DIRECTORIO, .keep_all = TRUE)

  segmento_consulta <- as.character(SEGMENTO)

  directorios_segmento <- A_seg %>%
    dplyr::filter(SEGMENTO == segmento_consulta) %>%
    dplyr::select(DIRECTORIO)

  if (nrow(directorios_segmento) == 0) {
    stop("No se encontraron directorios para el `SEGMENTO` consultado.")
  }

  if (is.null(diagnostico_tc)) {
    diagnostico_tc <- diagnostico_caidas_tres_criterios(
      dfs = dfs,
      base_hogar_cap = base_hogar_cap,
      base_persona_cap = base_persona_cap,
      edad_var = edad_var,
      exportar_excel = FALSE
    )
  }

  if (!is.list(diagnostico_tc) || !"reporte_final_caidas" %in% names(diagnostico_tc)) {
    stop(
      "`diagnostico_tc` debe ser la salida de `diagnostico_caidas_tres_criterios()` ",
      "y contener `reporte_final_caidas`."
    )
  }

  personas_universo <- E %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::left_join(A_seg, by = "DIRECTORIO") %>%
    dplyr::filter(SEGMENTO == segmento_consulta) %>%
    dplyr::select(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      dplyr::any_of(c("edad", "UUID", "SEGMENTO", "CLASE", "NPCEP2", "NPCEP3A"))
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("UUID", "SEGMENTO", "CLASE", "NPCEP2", "NPCEP3A")), as.character)
    ) %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P, ORDEN, .keep_all = TRUE)

  personas_caidas_segmento <- diagnostico_tc$reporte_final_caidas %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::semi_join(directorios_segmento, by = "DIRECTORIO") %>%
    dplyr::select(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      dplyr::any_of(c(
        "edad", "UUID", "SEGMENTO", "CLASE",
        "cae_existencia", "cae_lina", "cae_campo", "cae_duplicado",
        "n_criterios_reporte", "criterios_reporte",
        "criterio_principal_reporte", "razon_principal_caida",
        "variable_principal_caida", "valor_principal_caida",
        "observacion_final"
      ))
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "UUID", "SEGMENTO", "CLASE", "criterios_reporte",
          "criterio_principal_reporte", "razon_principal_caida",
          "variable_principal_caida", "valor_principal_caida",
          "observacion_final"
        )),
        as.character
      )
    ) %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P, ORDEN, .keep_all = TRUE)

  personas_segmento <- personas_universo %>%
    dplyr::left_join(
      personas_caidas_segmento,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
      suffix = c("_base", "")
    )

  if (!"edad_base" %in% names(personas_segmento)) {
    personas_segmento$edad_base <- if ("edad" %in% names(personas_segmento)) {
      vctrs::vec_init(personas_segmento$edad, n = nrow(personas_segmento))
    } else {
      NA_real_
    }
  }

  cols_chr_opt <- setdiff(c("UUID_base", "SEGMENTO_base", "CLASE_base"), names(personas_segmento))
  if (length(cols_chr_opt) > 0) {
    for (nm in cols_chr_opt) {
      personas_segmento[[nm]] <- NA_character_
    }
  }

  personas_segmento <- personas_segmento %>%
    dplyr::mutate(
      UUID = as.character(.data$UUID),
      UUID_base = as.character(.data$UUID_base),
      SEGMENTO = as.character(.data$SEGMENTO),
      SEGMENTO_base = as.character(.data$SEGMENTO_base),
      CLASE = as.character(.data$CLASE),
      CLASE_base = as.character(.data$CLASE_base),
      edad = dplyr::coalesce(.data$edad, .data$edad_base),
      UUID = dplyr::coalesce(.data$UUID, .data$UUID_base),
      SEGMENTO = dplyr::coalesce(.data$SEGMENTO, .data$SEGMENTO_base),
      CLASE = dplyr::coalesce(.data$CLASE, .data$CLASE_base),
      cae_existencia = dplyr::coalesce(.data$cae_existencia, FALSE),
      cae_lina = dplyr::coalesce(.data$cae_lina, FALSE),
      cae_campo = dplyr::coalesce(.data$cae_campo, FALSE),
      cae_duplicado = dplyr::coalesce(.data$cae_duplicado, FALSE),
      cae_reporte = .data$cae_existencia | .data$cae_lina | .data$cae_campo | .data$cae_duplicado,
      n_criterios_reporte = dplyr::coalesce(.data$n_criterios_reporte, 0L),
      criterios_reporte = dplyr::if_else(
        .data$cae_reporte,
        dplyr::coalesce(.data$criterios_reporte, .data$criterio_principal_reporte, "sin_detalle"),
        "no_cae"
      ),
      criterio_principal_reporte = dplyr::if_else(
        .data$cae_reporte,
        dplyr::coalesce(.data$criterio_principal_reporte, "sin_detalle"),
        "no_cae"
      ),
      razon_principal_caida = dplyr::if_else(
        .data$cae_reporte,
        dplyr::coalesce(.data$razon_principal_caida, "sin_detalle"),
        "No presenta caida en los criterios evaluados."
      ),
      variable_principal_caida = dplyr::if_else(
        .data$cae_reporte,
        dplyr::coalesce(.data$variable_principal_caida, NA_character_),
        NA_character_
      ),
      valor_principal_caida = dplyr::if_else(
        .data$cae_reporte,
        dplyr::coalesce(.data$valor_principal_caida, NA_character_),
        NA_character_
      ),
      observacion_final = dplyr::if_else(
        .data$cae_reporte,
        dplyr::coalesce(.data$observacion_final, "Persona con caida sin observacion consolidada."),
        "Persona sin caida en existencia, Lina, campo ni duplicados."
      )
    ) %>%
    dplyr::select(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      dplyr::any_of(c("edad", "UUID", "SEGMENTO", "CLASE", "NPCEP2", "NPCEP3A")),
      cae_reporte,
      cae_existencia,
      cae_lina,
      cae_campo,
      cae_duplicado,
      n_criterios_reporte,
      criterios_reporte,
      criterio_principal_reporte,
      razon_principal_caida,
      variable_principal_caida,
      valor_principal_caida,
      observacion_final
    ) %>%
    dplyr::arrange(dplyr::desc(.data$cae_reporte), .data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  parametros <- tibble::tibble(
    parametro = c("nivel_consulta", "SEGMENTO"),
    valor = c("persona", segmento_consulta)
  )

  resumen <- personas_segmento %>%
    dplyr::summarise(
      segmento = dplyr::first(.data$SEGMENTO),
      clase = dplyr::first(.data$CLASE),
      personas_totales = dplyr::n(),
      personas_con_caida = sum(.data$cae_reporte, na.rm = TRUE),
      personas_sin_caida = personas_totales - personas_con_caida,
      cae_existencia = sum(.data$cae_existencia, na.rm = TRUE),
      cae_lina = sum(.data$cae_lina, na.rm = TRUE),
      cae_campo = sum(.data$cae_campo, na.rm = TRUE),
      cae_duplicado = sum(.data$cae_duplicado, na.rm = TRUE),
      pct_caida = personas_con_caida / personas_totales
    )

  resumen_criterio <- personas_segmento %>%
    dplyr::count(.data$criterio_principal_reporte, name = "n_personas") %>%
    dplyr::mutate(
      pct_personas = .data$n_personas / sum(.data$n_personas, na.rm = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n_personas), .data$criterio_principal_reporte)

  salida <- list(
    parametros = parametros,
    resumen = resumen,
    resumen_criterio = resumen_criterio,
    personas_segmento = personas_segmento,
    diagnostico_tc = diagnostico_tc
  )

  if (isTRUE(exportar_excel)) {
    salida$archivo <- exportar_tablas_excel(
      x = list(
        parametros = parametros,
        resumen = resumen,
        resumen_criterio = resumen_criterio,
        personas_segmento = personas_segmento
      ),
      ruta = archivo
    )
  }

  salida
}
