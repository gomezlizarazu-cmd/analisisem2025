#' Exportar detalle de caidas con tematica a Excel
#'
#' Exporta a Excel un resumen general, la sabana consolidada final y el detalle
#' de caidas por nivel y por criterio a partir de la salida de
#' `diagnostico_caidas_con_tematica()`.
#'
#' La exportacion conserva explicitamente la granularidad de cada nivel:
#'
#' - vivienda: `DIRECTORIO`
#' - hogar: `DIRECTORIO + SECUENCIA_P`
#' - persona: `DIRECTORIO + SECUENCIA_P + ORDEN`
#'
#' La hoja `resumen_general` se construye con
#' `resumen_general_caidas_con_tematica()`. La hoja `reporte_final_caidas`
#' contiene la salida consolidada final del diagnostico. Las hojas de detalle se
#' construyen reutilizando `viviendas_eval`, `hogares_eval`, `personas_eval`,
#' `reporte_final_caidas` y, cuando hace falta, salidas ya calculadas de
#' existencia, Lina, tematica y duplicados. No recalcula diagnosticos.
#'
#' Para vivienda y hogar, las columnas textuales se consolidan por llave usando
#' valores unicos no vacios concatenados con `" | "`. Esta regla se aplica de
#' forma estable a `criterios_reporte`, `criterio_principal_reporte`,
#' `razon_principal_caida`, `variable_principal_caida`, `valor_principal_caida`
#' y `observacion_final`.
#'
#' Por defecto, las hojas sin registros se omiten para mantener el archivo
#' compacto. Si se desea una estructura fija, puede usarse
#' `incluir_hojas_vacias = TRUE`.
#'
#' @param diagnostico_con_tematica Objeto devuelto por
#'   `diagnostico_caidas_con_tematica()`.
#' @param ruta Ruta del archivo `.xlsx` de salida.
#' @param incluir_hojas_vacias Si es `TRUE`, crea una hoja con nota cuando una
#'   combinacion nivel-criterio no tenga registros.
#'
#' @return Lista con:
#' \describe{
#'   \item{archivo}{Ruta normalizada del Excel exportado.}
#'   \item{hojas_exportadas}{Vector con los nombres de hojas escritas.}
#'   \item{hojas_omitidas}{Vector con los nombres de hojas omitidas por no tener
#'   registros cuando `incluir_hojas_vacias = FALSE`.}
#'   \item{control_exportacion}{Tabla de validacion entre el resumen general y el
#'   numero de filas exportado por hoja.}
#' }
#'
#' @examples
#' \dontrun{
#' diag_con_tematica <- diagnostico_caidas_con_tematica(dfs)
#' exportar_detalle_caidas_con_tematica_excel(
#'   diagnostico_con_tematica = diag_con_tematica,
#'   ruta = "detalle_caidas_con_tematica.xlsx"
#' )
#' }
#'
#' @export
exportar_detalle_caidas_con_tematica_excel <- function(diagnostico_con_tematica,
                                                       ruta,
                                                       incluir_hojas_vacias = FALSE,
                                                       ajustar_anchos = FALSE,
                                                       mostrar_progreso = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  .validar_exportacion_caidas_con_tematica(diagnostico_con_tematica)

  resumen_general <- resumen_general_caidas_con_tematica(diagnostico_con_tematica)
  cache_exportacion <- .precomputar_exportacion_caidas_con_tematica(
    diagnostico_con_tematica = diagnostico_con_tematica
  )

  hojas <- list(
    resumen_general = resumen_general,
    reporte_final_caidas = diagnostico_con_tematica$reporte_final_caidas
  )

  hojas_omitidas <- character()
  detalle_hojas <- list()

  niveles <- c("vivienda", "hogar", "persona")
  criterios <- c("existencia", "lina", "campo", "duplicado", "tematica")

  for (nivel in niveles) {
    for (criterio in criterios) {
      if (isTRUE(mostrar_progreso)) {
        message("Construyendo hoja ", nivel, " / ", criterio, "...")
      }

      nombre_hoja <- .nombre_hoja_detalle_caidas_con_tematica(
        nivel = nivel,
        criterio = criterio
      )

      detalle <- .construir_detalle_caidas_con_tematica_hoja_desde_cache(
        cache_exportacion = cache_exportacion,
        nivel = nivel,
        criterio = criterio
      )

      detalle_hojas[[nombre_hoja]] <- detalle

      if (nrow(detalle) == 0) {
        if (isTRUE(incluir_hojas_vacias)) {
          hojas[[nombre_hoja]] <- .hoja_vacia_caidas_con_tematica(
            nivel = nivel,
            criterio = criterio
          )
        } else {
          hojas_omitidas <- c(hojas_omitidas, nombre_hoja)
        }
      } else {
        hojas[[nombre_hoja]] <- detalle
      }
    }
  }

  control_exportacion <- .validar_conteos_exportacion_caidas_con_tematica(
    resumen_general = resumen_general,
    detalle_hojas = detalle_hojas
  )

  if (any(!control_exportacion$coincide_con_resumen, na.rm = TRUE)) {
    filas_error <- control_exportacion %>%
      dplyr::filter(!.data$coincide_con_resumen)

    stop(
      "La exportacion no es coherente con `resumen_general_caidas_con_tematica()`. ",
      "Revise las hojas: ",
      paste(filas_error$hoja, collapse = ", ")
    )
  }

  if (isTRUE(mostrar_progreso)) {
    message("Escribiendo archivo Excel...")
  }

  archivo <- .exportar_tablas_excel_caidas_con_tematica(
    hojas = hojas,
    ruta = ruta,
    ajustar_anchos = ajustar_anchos
  )

  list(
    archivo = archivo,
    hojas_exportadas = names(hojas),
    hojas_omitidas = hojas_omitidas,
    control_exportacion = control_exportacion
  )
}

.precomputar_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica) {
  niveles <- c("vivienda", "hogar", "persona")
  criterios <- c("existencia", "lina", "campo", "duplicado", "tematica")

  base_nivel <- setNames(vector("list", length(niveles)), niveles)
  contexto_reporte <- setNames(vector("list", length(niveles)), niveles)
  detalle_criterio <- setNames(vector("list", length(niveles)), niveles)

  for (nivel in niveles) {
    base_nivel[[nivel]] <- .preparar_base_nivel_exportacion_caidas_con_tematica(
      diagnostico_con_tematica = diagnostico_con_tematica,
      nivel = nivel
    )

    contexto_reporte[[nivel]] <- .agregar_reporte_final_caidas_por_nivel(
      reporte_final_caidas = diagnostico_con_tematica$reporte_final_caidas,
      nivel = nivel
    )

    detalle_criterio[[nivel]] <- setNames(vector("list", length(criterios)), criterios)

    for (criterio in criterios) {
      detalle_criterio[[nivel]][[criterio]] <- .detalle_criterio_caidas_con_tematica(
        diagnostico_con_tematica = diagnostico_con_tematica,
        nivel = nivel,
        criterio = criterio
      )
    }
  }

  list(
    base_nivel = base_nivel,
    contexto_reporte = contexto_reporte,
    detalle_criterio = detalle_criterio
  )
}

.validar_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica) {
  if (!is.list(diagnostico_con_tematica)) {
    stop("`diagnostico_con_tematica` debe ser una lista.")
  }

  req <- c(
    "reporte_final_caidas",
    "viviendas_eval",
    "hogares_eval",
    "personas_eval",
    "diag_tres",
    "diag_tematica"
  )

  faltan <- setdiff(req, names(diagnostico_con_tematica))

  if (length(faltan) > 0) {
    stop(
      "Faltan objetos requeridos en `diagnostico_con_tematica`: ",
      paste(faltan, collapse = ", ")
    )
  }
}

.nombre_hoja_detalle_caidas_con_tematica <- function(nivel, criterio) {
  prefijo <- switch(
    nivel,
    vivienda = "viv",
    hogar = "hog",
    persona = "per",
    stop("Nivel no soportado: ", nivel)
  )

  sufijo <- switch(
    criterio,
    existencia = "existencia",
    lina = "lina",
    campo = "campo",
    duplicado = "dup",
    tematica = "tematica",
    stop("Criterio no soportado: ", criterio)
  )

  paste(prefijo, sufijo, sep = "_")
}

.hoja_vacia_caidas_con_tematica <- function(nivel, criterio) {
  tibble::tibble(
    nivel = nivel,
    criterio_caida = criterio,
    nota = "sin registros"
  )
}

.llaves_nivel_caidas_con_tematica <- function(nivel) {
  switch(
    nivel,
    vivienda = c("DIRECTORIO"),
    hogar = c("DIRECTORIO", "SECUENCIA_P"),
    persona = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    stop("Nivel no soportado: ", nivel)
  )
}

.columna_conteo_resumen_caidas_con_tematica <- function(criterio) {
  switch(
    criterio,
    existencia = "caidas_existencia",
    lina = "caidas_lina",
    campo = "caidas_campo",
    duplicado = "caidas_duplicado",
    tematica = "caidas_tematica",
    stop("Criterio no soportado: ", criterio)
  )
}

.construir_detalle_caidas_con_tematica_hoja <- function(diagnostico_con_tematica,
                                                        nivel,
                                                        criterio) {
  keys <- .llaves_nivel_caidas_con_tematica(nivel)
  flag_criterio <- paste0("cae_", criterio)

  base_nivel <- .preparar_base_nivel_exportacion_caidas_con_tematica(
    diagnostico_con_tematica = diagnostico_con_tematica,
    nivel = nivel
  ) %>%
    dplyr::filter(.data[[flag_criterio]])

  if (nrow(base_nivel) == 0) {
    return(tibble::tibble())
  }

  llaves_base <- base_nivel %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)))

  contexto_reporte <- .agregar_reporte_final_caidas_por_nivel(
    reporte_final_caidas = diagnostico_con_tematica$reporte_final_caidas,
    nivel = nivel
  )

  detalle_criterio <- .detalle_criterio_caidas_con_tematica(
    diagnostico_con_tematica = diagnostico_con_tematica,
    nivel = nivel,
    criterio = criterio
  )

  out <- base_nivel %>%
    dplyr::left_join(contexto_reporte, by = keys, suffix = c("", "_ctx")) %>%
    dplyr::left_join(detalle_criterio, by = keys, suffix = c("", "_det")) %>%
    dplyr::mutate(
      nivel = nivel,
      criterio_caida = criterio,
      variable_principal_caida = dplyr::coalesce(
        .data$variable_principal_caida_criterio,
        .data$variable_principal_caida
      ),
      valor_principal_caida = dplyr::coalesce(
        .data$valor_principal_caida_criterio,
        .data$valor_principal_caida
      ),
      razon_principal_caida = dplyr::coalesce(
        .data$razon_principal_caida_criterio,
        .data$razon_principal_caida
      ),
      observacion_final = dplyr::coalesce(
        .data$observacion_final_criterio,
        .data$observacion_final
      ),
      criterios_reporte = dplyr::coalesce(
        .data$criterios_reporte,
        .data$criterios_caida
      ),
      criterio_principal_reporte = dplyr::coalesce(
        .data$criterio_principal_reporte,
        NA_character_
      ),
      n_criterios_reporte = dplyr::coalesce(
        .data$n_criterios_reporte,
        .data$n_criterios_caida
      )
    ) %>%
    .consolidar_hoja_caidas_con_tematica_por_nivel(nivel = nivel) %>%
    dplyr::semi_join(llaves_base, by = keys) %>%
    dplyr::filter(.data[[flag_criterio]]) %>%
    dplyr::select(
      dplyr::all_of(keys),
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
      .data$nivel,
      .data$criterio_caida,
      .data$variable_principal_caida,
      .data$valor_principal_caida,
      .data$razon_principal_caida,
      .data$observacion_final,
      .data$criterios_reporte,
      .data$criterio_principal_reporte,
      .data$n_criterios_reporte,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$cae_duplicado,
      .data$cae_tematica
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))

  out
}

.construir_detalle_caidas_con_tematica_hoja_desde_cache <- function(cache_exportacion,
                                                                    nivel,
                                                                    criterio) {
  keys <- .llaves_nivel_caidas_con_tematica(nivel)
  flag_criterio <- paste0("cae_", criterio)

  base_nivel <- cache_exportacion$base_nivel[[nivel]] %>%
    dplyr::filter(.data[[flag_criterio]])

  if (nrow(base_nivel) == 0) {
    return(tibble::tibble())
  }

  llaves_base <- base_nivel %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)))

  contexto_reporte <- cache_exportacion$contexto_reporte[[nivel]]
  detalle_criterio <- cache_exportacion$detalle_criterio[[nivel]][[criterio]]

  base_nivel %>%
    dplyr::left_join(contexto_reporte, by = keys, suffix = c("", "_ctx")) %>%
    dplyr::left_join(detalle_criterio, by = keys, suffix = c("", "_det")) %>%
    dplyr::mutate(
      nivel = nivel,
      criterio_caida = criterio,
      variable_principal_caida = dplyr::coalesce(
        .data$variable_principal_caida_criterio,
        .data$variable_principal_caida
      ),
      valor_principal_caida = dplyr::coalesce(
        .data$valor_principal_caida_criterio,
        .data$valor_principal_caida
      ),
      razon_principal_caida = dplyr::coalesce(
        .data$razon_principal_caida_criterio,
        .data$razon_principal_caida
      ),
      observacion_final = dplyr::coalesce(
        .data$observacion_final_criterio,
        .data$observacion_final
      ),
      criterios_reporte = dplyr::coalesce(
        .data$criterios_reporte,
        .data$criterios_caida
      ),
      criterio_principal_reporte = dplyr::coalesce(
        .data$criterio_principal_reporte,
        NA_character_
      ),
      n_criterios_reporte = dplyr::coalesce(
        .data$n_criterios_reporte,
        .data$n_criterios_caida
      )
    ) %>%
    .consolidar_hoja_caidas_con_tematica_por_nivel(nivel = nivel) %>%
    dplyr::semi_join(llaves_base, by = keys) %>%
    dplyr::filter(.data[[flag_criterio]]) %>%
    dplyr::select(
      dplyr::all_of(keys),
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
      .data$nivel,
      .data$criterio_caida,
      .data$variable_principal_caida,
      .data$valor_principal_caida,
      .data$razon_principal_caida,
      .data$observacion_final,
      .data$criterios_reporte,
      .data$criterio_principal_reporte,
      .data$n_criterios_reporte,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$cae_duplicado,
      .data$cae_tematica
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
}

.exportar_tablas_excel_caidas_con_tematica <- function(hojas,
                                                       ruta,
                                                       ajustar_anchos = FALSE) {
  wb <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "bottom"
  )

  for (nm in names(hojas)) {
    hoja_nm <- substr(nm, 1, 31)

    df_out <- hojas[[nm]] %>%
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.factor), as.character),
        dplyr::across(tidyselect::where(is.list), ~ vapply(., toString, character(1)))
      ) %>%
      arreglar_utf8_df()

    openxlsx::addWorksheet(wb, hoja_nm)
    openxlsx::writeData(wb, hoja_nm, df_out)

    openxlsx::addStyle(
      wb,
      sheet = hoja_nm,
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(df_out)),
      gridExpand = TRUE
    )

    if (isTRUE(ajustar_anchos)) {
      openxlsx::setColWidths(
        wb,
        sheet = hoja_nm,
        cols = seq_len(ncol(df_out)),
        widths = "auto"
      )
    }
  }

  openxlsx::saveWorkbook(wb, ruta, overwrite = TRUE)

  normalizePath(ruta, winslash = "/", mustWork = FALSE)
}

.preparar_base_nivel_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                                 nivel) {
  base <- .preparar_eval_resumen_caidas_con_tematica(
    diagnostico_con_tematica = diagnostico_con_tematica,
    nivel = nivel
  )

  keys <- .llaves_nivel_caidas_con_tematica(nivel)

  base %>%
    dplyr::select(
      dplyr::all_of(keys),
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
      dplyr::any_of(c(
        "cae_existencia",
        "cae_lina",
        "cae_campo",
        "cae_duplicado",
        "cae_tematica",
        "n_criterios_caida",
        "criterios_caida"
      ))
    ) %>%
    .consolidar_hoja_caidas_con_tematica_por_nivel(nivel = nivel)
}

.agregar_reporte_final_caidas_por_nivel <- function(reporte_final_caidas, nivel) {
  keys <- .llaves_nivel_caidas_con_tematica(nivel)

  req <- c(
    "DIRECTORIO", "SECUENCIA_P", "ORDEN",
    "cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica",
    "n_criterios_reporte", "criterios_reporte", "criterio_principal_reporte",
    "razon_principal_caida", "variable_principal_caida", "valor_principal_caida",
    "observacion_final"
  )

  faltan <- setdiff(req, names(reporte_final_caidas))

  if (length(faltan) > 0) {
    stop(
      "Faltan columnas requeridas en `reporte_final_caidas`: ",
      paste(faltan, collapse = ", ")
    )
  }

  if (!"UUID" %in% names(reporte_final_caidas)) reporte_final_caidas$UUID <- NA_character_
  if (!"SEGMENTO" %in% names(reporte_final_caidas)) reporte_final_caidas$SEGMENTO <- NA_character_
  if (!"CLASE" %in% names(reporte_final_caidas)) reporte_final_caidas$CLASE <- NA_character_

  reporte_final_caidas %>%
    .completar_llaves_exportacion_caidas_con_tematica() %>%
    dplyr::select(
      dplyr::all_of(c(keys, "UUID", "SEGMENTO", "CLASE")),
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$cae_duplicado,
      .data$cae_tematica,
      .data$n_criterios_reporte,
      .data$criterios_reporte,
      .data$criterio_principal_reporte,
      .data$razon_principal_caida,
      .data$variable_principal_caida,
      .data$valor_principal_caida,
      .data$observacion_final
    ) %>%
    .consolidar_hoja_caidas_con_tematica_por_nivel(nivel = nivel)
}

.consolidar_hoja_caidas_con_tematica_por_nivel <- function(df, nivel) {
  keys <- .llaves_nivel_caidas_con_tematica(nivel)

  df <- .completar_llaves_exportacion_caidas_con_tematica(df)

  cols_bool <- intersect(
    c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica"),
    names(df)
  )

  cols_texto <- intersect(
    c(
      "UUID", "SEGMENTO", "CLASE", "nivel", "criterio_caida",
      "criterios_caida", "criterios_reporte", "criterio_principal_reporte",
      "razon_principal_caida", "variable_principal_caida", "valor_principal_caida",
      "observacion_final", "variable_principal_caida_criterio",
      "valor_principal_caida_criterio", "razon_principal_caida_criterio",
      "observacion_final_criterio"
    ),
    names(df)
  )

  cols_int <- intersect(c("n_criterios_caida", "n_criterios_reporte"), names(df))

  resumenes <- c(
    lapply(cols_bool, function(nm) {
      rlang::expr(any(!is.na(.data[[!!nm]]) & .data[[!!nm]], na.rm = TRUE))
    }),
    lapply(cols_int, function(nm) {
      rlang::expr(max(.data[[!!nm]], na.rm = TRUE))
    }),
    lapply(cols_texto, function(nm) {
      rlang::expr(.collapse_unique_nonempty(.data[[!!nm]]))
    })
  )

  names(resumenes) <- c(cols_bool, cols_int, cols_texto)

  out <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(!!!resumenes, .groups = "drop")

  if ("n_criterios_caida" %in% names(out)) {
    out$n_criterios_caida[is.infinite(out$n_criterios_caida)] <- NA_real_
  }

  if ("n_criterios_reporte" %in% names(out)) {
    out$n_criterios_reporte[is.infinite(out$n_criterios_reporte)] <- NA_real_
  }

  out
}

.detalle_criterio_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                  nivel,
                                                  criterio) {
  switch(
    criterio,
    existencia = .detalle_existencia_exportacion_caidas_con_tematica(
      diagnostico_con_tematica = diagnostico_con_tematica,
      nivel = nivel
    ),
    lina = .detalle_lina_exportacion_caidas_con_tematica(
      diagnostico_con_tematica = diagnostico_con_tematica,
      nivel = nivel
    ),
    campo = .detalle_campo_exportacion_caidas_con_tematica(
      diagnostico_con_tematica = diagnostico_con_tematica,
      nivel = nivel
    ),
    duplicado = .detalle_duplicado_exportacion_caidas_con_tematica(
      diagnostico_con_tematica = diagnostico_con_tematica,
      nivel = nivel
    ),
    tematica = .detalle_tematica_exportacion_caidas_con_tematica(
      diagnostico_con_tematica = diagnostico_con_tematica,
      nivel = nivel
    ),
    stop("Criterio no soportado: ", criterio)
  )
}

.detalle_existencia_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                                nivel) {
  detalle_base <- diagnostico_con_tematica$diag_tres$diag_existencia$resumen_caidas_regla2 %>%
    .completar_llaves_exportacion_caidas_con_tematica()

  detalle <- detalle_base %>%
    dplyr::filter(.data$nivel == nivel) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      variable_principal_caida_criterio = "capitulos_faltantes",
      valor_principal_caida_criterio = .texto_revision_valor(.data$capitulos_caida),
      razon_principal_caida_criterio = "Falta de capitulos requeridos",
      observacion_final_criterio = paste0(
        "Faltan capitulos requeridos: ",
        .texto_revision_valor(.data$capitulos_caida),
        ". Fuentes: ",
        .texto_revision_valor(.data$fuentes_caida),
        "."
      )
    )

  detalle %>%
    .recortar_detalle_caidas_con_tematica_nivel(nivel = nivel) %>%
    .consolidar_detalle_criterio_caidas_con_tematica(nivel = nivel)
}

.detalle_lina_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                          nivel) {
  diag_lina <- diagnostico_con_tematica$diag_tres$diag_lina

  detalle <- switch(
    nivel,
    vivienda = diag_lina$viviendas_eval %>%
      dplyr::transmute(
        DIRECTORIO = as.character(.data$DIRECTORIO),
        variable_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NVCAPCTRL1) ~ "capitulo_A",
          .data$NVCAPCTRL1 != 1 ~ "NVCAPCTRL1",
          .data$NVCAPCTRL2 != 1 ~ "NVCAPCTRL2",
          !.valor_no_vacio(.data$NVCBP1) ~ "NVCBP1",
          !.valor_no_vacio(.data$NVCBP16) ~ "NVCBP16",
          TRUE ~ NA_character_
        ),
        valor_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NVCAPCTRL1) ~ "sin registro",
          .data$NVCAPCTRL1 != 1 ~ .texto_revision_valor(.data$NVCAPCTRL1),
          .data$NVCAPCTRL2 != 1 ~ .texto_revision_valor(.data$NVCAPCTRL2),
          !.valor_no_vacio(.data$NVCBP1) ~ .texto_revision_valor(.data$NVCBP1),
          !.valor_no_vacio(.data$NVCBP16) ~ .texto_revision_valor(.data$NVCBP16),
          TRUE ~ NA_character_
        ),
        razon_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NVCAPCTRL1) ~ "Registro de vivienda",
          .data$NVCAPCTRL1 != 1 ~ "Control de vivienda",
          .data$NVCAPCTRL2 != 1 ~ "Presencia en vivienda",
          !.valor_no_vacio(.data$NVCBP1) ~ "Variable requerida de vivienda",
          !.valor_no_vacio(.data$NVCBP16) ~ "Variable requerida de vivienda",
          TRUE ~ NA_character_
        ),
        observacion_final_criterio = dplyr::case_when(
          is.na(.data$NVCAPCTRL1) ~ "Cae por Lina porque no hay registro en el capitulo A.",
          .data$NVCAPCTRL1 != 1 ~ paste0(
            "Cae por Lina en vivienda: NVCAPCTRL1=",
            .texto_revision_valor(.data$NVCAPCTRL1),
            "."
          ),
          .data$NVCAPCTRL2 != 1 ~ paste0(
            "Cae por Lina en vivienda: NVCAPCTRL2=",
            .texto_revision_valor(.data$NVCAPCTRL2),
            "."
          ),
          !.valor_no_vacio(.data$NVCBP1) ~ "Cae por Lina en vivienda: NVCBP1 vacia.",
          !.valor_no_vacio(.data$NVCBP16) ~ "Cae por Lina en vivienda: NVCBP16 vacia.",
          TRUE ~ NA_character_
        )
      ),
    hogar = diag_lina$hogares_eval %>%
      dplyr::transmute(
        DIRECTORIO = as.character(.data$DIRECTORIO),
        SECUENCIA_P = as.character(.data$SECUENCIA_P),
        variable_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NHCCPCTRL1) ~ "capitulo_C",
          .data$NHCCPCTRL1 != 1 ~ "NHCCPCTRL1",
          !.valor_no_vacio(.data$NHCCP1) ~ "NHCCP1",
          !.valor_no_vacio(.data$NHCMP1A) ~ "NHCMP1A",
          !.valor_no_vacio(.data$NHCMP5A) ~ "NHCMP5A",
          TRUE ~ NA_character_
        ),
        valor_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NHCCPCTRL1) ~ "sin registro",
          .data$NHCCPCTRL1 != 1 ~ .texto_revision_valor(.data$NHCCPCTRL1),
          !.valor_no_vacio(.data$NHCCP1) ~ .texto_revision_valor(.data$NHCCP1),
          !.valor_no_vacio(.data$NHCMP1A) ~ .texto_revision_valor(.data$NHCMP1A),
          !.valor_no_vacio(.data$NHCMP5A) ~ .texto_revision_valor(.data$NHCMP5A),
          TRUE ~ NA_character_
        ),
        razon_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NHCCPCTRL1) ~ "Registro de hogar",
          .data$NHCCPCTRL1 != 1 ~ "Control de hogar",
          !.valor_no_vacio(.data$NHCCP1) ~ "Variable requerida de hogar",
          !.valor_no_vacio(.data$NHCMP1A) ~ "Variable requerida de hogar",
          !.valor_no_vacio(.data$NHCMP5A) ~ "Variable requerida de hogar",
          TRUE ~ NA_character_
        ),
        observacion_final_criterio = dplyr::case_when(
          is.na(.data$NHCCPCTRL1) ~ "Cae por Lina porque no hay registro en el capitulo C.",
          .data$NHCCPCTRL1 != 1 ~ paste0(
            "Cae por Lina en hogar: NHCCPCTRL1=",
            .texto_revision_valor(.data$NHCCPCTRL1),
            "."
          ),
          !.valor_no_vacio(.data$NHCCP1) ~ "Cae por Lina en hogar: NHCCP1 vacia.",
          !.valor_no_vacio(.data$NHCMP1A) ~ "Cae por Lina en hogar: NHCMP1A vacia.",
          !.valor_no_vacio(.data$NHCMP5A) ~ "Cae por Lina en hogar: NHCMP5A vacia.",
          TRUE ~ NA_character_
        )
      ),
    persona = diag_lina$personas_eval %>%
      dplyr::transmute(
        DIRECTORIO = as.character(.data$DIRECTORIO),
        SECUENCIA_P = as.character(.data$SECUENCIA_P),
        ORDEN = as.character(.data$ORDEN),
        variable_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NPCEPCTRL1) ~ "capitulo_E",
          .data$NPCEPCTRL1 != 1 ~ "NPCEPCTRL1",
          !.valor_no_vacio(.data$NPCEP6) ~ "NPCEP6",
          TRUE ~ NA_character_
        ),
        valor_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NPCEPCTRL1) ~ "sin registro",
          .data$NPCEPCTRL1 != 1 ~ .texto_revision_valor(.data$NPCEPCTRL1),
          !.valor_no_vacio(.data$NPCEP6) ~ .texto_revision_valor(.data$NPCEP6),
          TRUE ~ NA_character_
        ),
        razon_principal_caida_criterio = dplyr::case_when(
          is.na(.data$NPCEPCTRL1) ~ "Registro de persona",
          .data$NPCEPCTRL1 != 1 ~ "Control de persona",
          !.valor_no_vacio(.data$NPCEP6) ~ "Variable requerida de persona",
          TRUE ~ NA_character_
        ),
        observacion_final_criterio = dplyr::case_when(
          is.na(.data$NPCEPCTRL1) ~ "Cae por Lina porque no hay registro en el capitulo E.",
          .data$NPCEPCTRL1 != 1 ~ paste0(
            "Cae por Lina en persona: NPCEPCTRL1=",
            .texto_revision_valor(.data$NPCEPCTRL1),
            "."
          ),
          !.valor_no_vacio(.data$NPCEP6) ~ "Cae por Lina en persona: NPCEP6 vacia.",
          TRUE ~ NA_character_
        )
      ),
    stop("Nivel no soportado: ", nivel)
  )

  detalle %>%
    dplyr::filter(!is.na(.data$razon_principal_caida_criterio)) %>%
    .recortar_detalle_caidas_con_tematica_nivel(nivel = nivel) %>%
    .consolidar_detalle_criterio_caidas_con_tematica(nivel = nivel)
}

.detalle_campo_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                           nivel) {
  base <- switch(
    nivel,
    vivienda = diagnostico_con_tematica$viviendas_eval,
    hogar = diagnostico_con_tematica$hogares_eval,
    persona = diagnostico_con_tematica$personas_eval,
    stop("Nivel no soportado: ", nivel)
  ) %>%
    .completar_llaves_exportacion_caidas_con_tematica()

  if (!"cae_campo_nhccpctrl2" %in% names(base)) base$cae_campo_nhccpctrl2 <- FALSE
  if (!"NHCCPCTRL2" %in% names(base)) base$NHCCPCTRL2 <- NA
  if (!"motivo_detallado_campo" %in% names(base)) base$motivo_detallado_campo <- NA_character_
  if (!"motivo_principal_campo" %in% names(base)) base$motivo_principal_campo <- NA_character_
  if (!"criterio_falla_campo" %in% names(base)) base$criterio_falla_campo <- NA_character_

  detalle <- base %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      variable_principal_caida_criterio = dplyr::case_when(
        .data$cae_campo_nhccpctrl2 ~ "NHCCPCTRL2",
        TRUE ~ NA_character_
      ),
      valor_principal_caida_criterio = dplyr::case_when(
        .data$cae_campo_nhccpctrl2 ~ .texto_revision_valor(.data$NHCCPCTRL2),
        TRUE ~ NA_character_
      ),
      razon_principal_caida_criterio = dplyr::coalesce(
        .data$motivo_detallado_campo,
        .data$motivo_principal_campo
      ),
      observacion_final_criterio = .data$criterio_falla_campo
    ) %>%
    dplyr::filter(!is.na(.data$razon_principal_caida_criterio))

  detalle %>%
    .recortar_detalle_caidas_con_tematica_nivel(nivel = nivel) %>%
    .consolidar_detalle_criterio_caidas_con_tematica(nivel = nivel)
}

.detalle_duplicado_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                               nivel) {
  duplicados <- diagnostico_con_tematica$diag_tres$duplicados_personas_e

  if (!is.data.frame(duplicados) || nrow(duplicados) == 0) {
    return(tibble::tibble())
  }

  detalle_per <- duplicados %>%
    .completar_llaves_exportacion_caidas_con_tematica() %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      variable_principal_caida_criterio = "nombre_fecha_documento",
      valor_principal_caida_criterio = paste0(
        .texto_revision_valor(.data$NPCEP2),
        " | ",
        .texto_revision_valor(.data$NPCEP3A),
        " | ",
        .texto_revision_valor(.data$numero_documento_unificado)
      ),
      razon_principal_caida_criterio = "Posible duplicado en personas",
      observacion_final_criterio = .data$observacion_duplicado
    ) %>%
    dplyr::distinct(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .keep_all = TRUE
    )

  detalle_per %>%
    .agregar_detalle_criterio_por_nivel_caidas_con_tematica(nivel = nivel)
}

.detalle_tematica_exportacion_caidas_con_tematica <- function(diagnostico_con_tematica,
                                                              nivel) {
  detalle <- diagnostico_con_tematica$diag_tematica$cruce_con_tres_criterios %>%
    .completar_llaves_exportacion_caidas_con_tematica() %>%
    dplyr::filter(
      .data$nivel_resultado == nivel,
      .data$falla_incompletitud_tematica
    ) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      variable_principal_caida_criterio = .data$variable_regla,
      valor_principal_caida_criterio = .data$valor_observado,
      razon_principal_caida_criterio = .data$descripcion_regla,
      observacion_final_criterio = .armar_observacion_tematica(
        descripcion = .data$descripcion_regla,
        variable = .data$variable_regla,
        valor = .data$valor_observado
      )
    )

  detalle %>%
    .agregar_detalle_criterio_por_nivel_caidas_con_tematica(nivel = nivel)
}

.agregar_detalle_criterio_por_nivel_caidas_con_tematica <- function(detalle, nivel) {
  if (!is.data.frame(detalle) || nrow(detalle) == 0) {
    return(tibble::tibble())
  }

  detalle %>%
    .recortar_detalle_caidas_con_tematica_nivel(nivel = nivel) %>%
    .consolidar_detalle_criterio_caidas_con_tematica(nivel = nivel)
}

.consolidar_detalle_criterio_caidas_con_tematica <- function(detalle, nivel) {
  keys <- .llaves_nivel_caidas_con_tematica(nivel)

  detalle %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      variable_principal_caida_criterio = .collapse_unique_nonempty(.data$variable_principal_caida_criterio),
      valor_principal_caida_criterio = .collapse_unique_nonempty(.data$valor_principal_caida_criterio),
      razon_principal_caida_criterio = .collapse_unique_nonempty(.data$razon_principal_caida_criterio),
      observacion_final_criterio = .collapse_unique_nonempty(.data$observacion_final_criterio),
      .groups = "drop"
    )
}

.recortar_detalle_caidas_con_tematica_nivel <- function(detalle, nivel) {
  keys <- .llaves_nivel_caidas_con_tematica(nivel)

  detalle %>%
    .completar_llaves_exportacion_caidas_con_tematica() %>%
    dplyr::select(
      dplyr::all_of(keys),
      dplyr::any_of(c(
        "variable_principal_caida_criterio",
        "valor_principal_caida_criterio",
        "razon_principal_caida_criterio",
        "observacion_final_criterio"
      ))
    )
}

.completar_llaves_exportacion_caidas_con_tematica <- function(df) {
  if (!"SECUENCIA_P" %in% names(df)) df$SECUENCIA_P <- NA_character_
  if (!"ORDEN" %in% names(df)) df$ORDEN <- NA_character_

  df
}

.validar_conteos_exportacion_caidas_con_tematica <- function(resumen_general,
                                                             detalle_hojas) {
  niveles <- c("vivienda", "hogar", "persona")
  criterios <- c("existencia", "lina", "campo", "duplicado", "tematica")

  control <- lapply(niveles, function(nivel) {
    lapply(criterios, function(criterio) {
      hoja <- .nombre_hoja_detalle_caidas_con_tematica(nivel, criterio)
      col_esperada <- .columna_conteo_resumen_caidas_con_tematica(criterio)

      esperado <- resumen_general %>%
        dplyr::filter(.data$nivel == .env$nivel) %>%
        dplyr::pull(!!rlang::sym(col_esperada))

      esperado <- if (length(esperado) == 0) NA_integer_ else esperado[[1]]
      actual <- if (hoja %in% names(detalle_hojas)) nrow(detalle_hojas[[hoja]]) else 0L

      tibble::tibble(
        nivel = nivel,
        criterio = criterio,
        hoja = hoja,
        filas_esperadas = esperado,
        filas_exportadas = actual,
        coincide_con_resumen = identical(as.integer(actual), as.integer(esperado))
      )
    }) %>%
      dplyr::bind_rows()
  }) %>%
    dplyr::bind_rows()

  control
}
