#' Validar ENCUESTA_COMPLETA de Sistemas contra la logica sin tematica
#'
#' Compara la variable `ENCUESTA_COMPLETA` incorporada por Sistemas en las
#' tablas A, C y E contra los universos completos construidos por la logica
#' interna sin criterio tematico.
#'
#' La comparacion se realiza estrictamente por llaves y no solo por conteos:
#' A se compara por `DIRECTORIO`, C por `DIRECTORIO + SECUENCIA_P` y E por
#' `DIRECTORIO + SECUENCIA_P + ORDEN`.
#'
#' @param dfs Lista de data.frames/tibbles originales por capitulo.
#' @param dfs_completa_sin_tematica Lista de data.frames/tibbles completos sin
#'   criterio tematico. Usualmente `em_completa_sin_tematica$dfs`.
#' @param variable_sistemas Nombre de la variable de completitud incorporada por
#'   Sistemas. Por defecto `"ENCUESTA_COMPLETA"`.
#' @param exportar Logico. Si TRUE, exporta un Excel de auditoria.
#' @param ruta_exportacion Ruta completa del archivo Excel de salida. Requerida
#'   si `exportar = TRUE`.
#' @return Una lista con `resumen`, `diferencias`, `frecuencias_variable` y
#'   `ruta_exportacion`.
#' @export
validar_encuesta_completa_sistemas <- function(dfs,
                                               dfs_completa_sin_tematica,
                                               variable_sistemas = "ENCUESTA_COMPLETA",
                                               exportar = FALSE,
                                               ruta_exportacion = NULL) {
  if (!is.list(dfs)) {
    stop("`dfs` debe ser una lista de data.frames/tibbles.", call. = FALSE)
  }
  if (!is.list(dfs_completa_sin_tematica)) {
    stop("`dfs_completa_sin_tematica` debe ser una lista de data.frames/tibbles.", call. = FALSE)
  }
  if (!is.character(variable_sistemas) || length(variable_sistemas) != 1L) {
    stop("`variable_sistemas` debe ser un nombre de variable unico.", call. = FALSE)
  }
  if (isTRUE(exportar) && (is.null(ruta_exportacion) || !nzchar(ruta_exportacion))) {
    stop("`ruta_exportacion` es requerida cuando `exportar = TRUE`.", call. = FALSE)
  }

  especificacion <- list(
    A = list(nivel = "vivienda", llaves = c("DIRECTORIO")),
    C = list(nivel = "hogar", llaves = c("DIRECTORIO", "SECUENCIA_P")),
    E = list(nivel = "persona", llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
  )

  comparaciones <- purrr::imap(
    especificacion,
    function(meta, tabla) {
      .comparar_encuesta_completa_sistemas_tabla(
        dfs = dfs,
        dfs_completa_sin_tematica = dfs_completa_sin_tematica,
        tabla = tabla,
        nivel = meta$nivel,
        llaves = meta$llaves,
        variable_sistemas = variable_sistemas
      )
    }
  )

  resumen <- purrr::map_dfr(comparaciones, "resumen")
  diferencias <- purrr::map(comparaciones, "diferencias")
  frecuencias_variable <- .frecuencias_encuesta_completa_sistemas(
    dfs = dfs,
    especificacion = especificacion,
    variable_sistemas = variable_sistemas
  )

  if (isTRUE(exportar)) {
    .exportar_validacion_encuesta_completa_sistemas(
      resumen = resumen,
      diferencias = diferencias,
      frecuencias_variable = frecuencias_variable,
      ruta_exportacion = ruta_exportacion
    )
  }

  list(
    resumen = resumen,
    diferencias = diferencias,
    frecuencias_variable = frecuencias_variable,
    ruta_exportacion = if (isTRUE(exportar)) ruta_exportacion else NULL
  )
}

.normalizar_encuesta_completa_sistemas <- function(x) {
  x_chr <- stringr::str_squish(stringr::str_to_upper(as.character(x)))
  x_chr <- iconv(x_chr, from = "", to = "ASCII//TRANSLIT")

  dplyr::case_when(
    is.na(x_chr) ~ NA,
    x_chr %in% c("1", "TRUE", "T", "SI", "COMPLETA") ~ TRUE,
    x_chr %in% c("0", "FALSE", "F", "NO", "INCOMPLETA") ~ FALSE,
    TRUE ~ NA
  )
}

.frecuencias_encuesta_completa_sistemas <- function(dfs,
                                                    especificacion,
                                                    variable_sistemas) {
  purrr::imap_dfr(especificacion, function(meta, tabla) {
    .validar_tabla_sistemas(dfs, tabla, variable_sistemas)

    valor_original <- as.character(dfs[[tabla]][[variable_sistemas]])

    tibble::tibble(valor_original = valor_original) %>%
      dplyr::count(valor_original, name = "n", sort = TRUE) %>%
      dplyr::mutate(
        tabla = tabla,
        valor_normalizado = .normalizar_encuesta_completa_sistemas(valor_original),
        nota = NA_character_
      ) %>%
      dplyr::select(tabla, valor_original, valor_normalizado, n, nota)
  })
}

.comparar_encuesta_completa_sistemas_tabla <- function(dfs,
                                                       dfs_completa_sin_tematica,
                                                       tabla,
                                                       nivel,
                                                       llaves,
                                                       variable_sistemas) {
  .validar_tabla_sistemas(dfs, tabla, variable_sistemas)
  .validar_tabla_logica(dfs_completa_sin_tematica, tabla)
  .validar_llaves(dfs[[tabla]], tabla, llaves, "dfs")
  .validar_llaves(dfs_completa_sin_tematica[[tabla]], tabla, llaves, "dfs_completa_sin_tematica")

  completas_logica <- dfs_completa_sin_tematica[[tabla]] %>%
    normalize_keys(llaves) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves)))

  sistemas_norm <- dfs[[tabla]] %>%
    normalize_keys(llaves)

  sistemas_norm$encuesta_completa_sistemas <- .normalizar_encuesta_completa_sistemas(
    sistemas_norm[[variable_sistemas]]
  )

  completas_sistemas <- sistemas_norm %>%
    dplyr::filter(encuesta_completa_sistemas %in% TRUE) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves)))

  en_logica_no_en_sistemas <- dplyr::anti_join(
    completas_logica,
    completas_sistemas,
    by = llaves
  ) %>%
    dplyr::mutate(tipo_diferencia = "en_logica_no_en_sistemas")

  en_sistemas_no_en_logica <- dplyr::anti_join(
    completas_sistemas,
    completas_logica,
    by = llaves
  ) %>%
    dplyr::mutate(tipo_diferencia = "en_sistemas_no_en_logica")

  diferencias <- dplyr::bind_rows(
    en_logica_no_en_sistemas,
    en_sistemas_no_en_logica
  ) %>%
    dplyr::mutate(tabla = tabla, .before = 1)

  resumen <- tibble::tibble(
    tabla = tabla,
    nivel = nivel,
    llaves = paste(llaves, collapse = " + "),
    n_completa_logica_sin_tematica = nrow(completas_logica),
    n_completa_sistemas = nrow(completas_sistemas),
    n_en_logica_no_en_sistemas = nrow(en_logica_no_en_sistemas),
    n_en_sistemas_no_en_logica = nrow(en_sistemas_no_en_logica),
    coincide = nrow(en_logica_no_en_sistemas) == 0L &&
      nrow(en_sistemas_no_en_logica) == 0L
  )

  list(
    resumen = resumen,
    diferencias = diferencias
  )
}

.exportar_validacion_encuesta_completa_sistemas <- function(resumen,
                                                           diferencias,
                                                           frecuencias_variable,
                                                           ruta_exportacion) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "El paquete `openxlsx` es requerido para exportar la auditoria. ",
      "Instalelo o use `exportar = FALSE`.",
      call. = FALSE
    )
  }

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "00_resumen")
  openxlsx::writeData(wb, "00_resumen", resumen)

  openxlsx::addWorksheet(wb, "01_A_diferencias")
  openxlsx::writeData(wb, "01_A_diferencias", diferencias$A)

  openxlsx::addWorksheet(wb, "02_C_diferencias")
  openxlsx::writeData(wb, "02_C_diferencias", diferencias$C)

  openxlsx::addWorksheet(wb, "03_E_diferencias")
  openxlsx::writeData(wb, "03_E_diferencias", diferencias$E)

  openxlsx::addWorksheet(wb, "04_frecuencias_variable")
  openxlsx::writeData(wb, "04_frecuencias_variable", frecuencias_variable)

  openxlsx::saveWorkbook(wb, ruta_exportacion, overwrite = TRUE)
}

.validar_tabla_sistemas <- function(dfs, tabla, variable_sistemas) {
  if (!tabla %in% names(dfs)) {
    stop("No existe la tabla `", tabla, "` en `dfs`.", call. = FALSE)
  }
  if (!variable_sistemas %in% names(dfs[[tabla]])) {
    stop(
      "La tabla `", tabla, "` no contiene la variable `",
      variable_sistemas, "`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validar_tabla_logica <- function(dfs_completa_sin_tematica, tabla) {
  if (!tabla %in% names(dfs_completa_sin_tematica)) {
    stop(
      "No existe la tabla `", tabla,
      "` en `dfs_completa_sin_tematica`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validar_llaves <- function(df, tabla, llaves, objeto) {
  faltantes <- setdiff(llaves, names(df))

  if (length(faltantes) > 0L) {
    stop(
      "La tabla `", tabla, "` en `", objeto,
      "` no contiene las llaves requeridas: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
