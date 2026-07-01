#' Reporte de cascada para segmentos problematicos de Muestras
#'
#' Construye una salida focalizada para segmentos enviados por Muestras, sin
#' recalcular diagnosticos ni modificar los criterios generales de caida. La
#' funcion toma los diagnosticos ya calculados y resume, a nivel vivienda
#' (`DIRECTORIO`), la cascada excluyente de caidas por completitud estructural,
#' campo, Lina, duplicados y tematica.
#'
#' @param segmentos Vector de segmentos a reportar.
#' @param dfs Lista de capitulos de la Encuesta Multiproposito. Debe incluir el
#'   capitulo `A` con `DIRECTORIO` y `SEGMENTO`; idealmente tambien `E`.
#' @param diag_tres Resultado de `diagnostico_caidas_tres_criterios()`.
#' @param diag_con_tematica Resultado opcional de
#'   `diagnostico_caidas_con_tematica()`. Si se suministra, se usa como fuente
#'   del escenario con reglas tematicas.
#' @param em_completa Objeto opcional que puede contener `dfs`, `diag_tres` o
#'   `diag_con_tematica`. Se usa solo como contenedor de insumos si esos
#'   argumentos no se entregan directamente.
#' @param carpeta_salida Carpeta donde se exportan los archivos si
#'   `exportar = TRUE`.
#' @param fecha_corte Fecha o etiqueta de corte para nombrar archivos. Si es
#'   `NULL`, usa la fecha del sistema.
#' @param reglas_tematica Reglas tematicas opcionales, usadas solo para
#'   documentar variables de la tabla de criterios reutilizada por el reporte.
#' @param exportar Si `TRUE`, exporta las tablas a `carpeta_salida`.
#' @param formato_exportacion Formato de exportacion: `"xlsx"` o `"csv"`.
#'
#' @return Lista con:
#' \describe{
#'   \item{resumen_segmento}{Resumen por segmento a nivel `DIRECTORIO`.}
#'   \item{cascada_detallada_segmento}{Cascada larga por segmento y etapa a nivel `DIRECTORIO`.}
#'   \item{detalle_caidas}{Detalle de filas/personas asociadas a viviendas caidas.}
#'   \item{resumen_detalle_segmento}{Resumen de filas y llaves unicas del detalle.}
#'   \item{personas_asociadas_segmento}{Conteos observados en capitulo `E` para viviendas caidas.}
#'   \item{comparacion_con_sin_tematica}{Comparacion con y sin reglas tematicas a nivel `DIRECTORIO`.}
#'   \item{segmentos_no_encontrados}{Segmentos solicitados sin registros en la base.}
#'   \item{archivos_exportados}{Rutas exportadas, o `NULL` si no se exporta.}
#' }
#'
#' @examples
#' \dontrun{
#' devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")
#'
#' segmentos_problema <- c(
#'   "17183", "17189", "17191", "17207",
#'   "30303", "30305", "30324", "30328", "30439"
#' )
#'
#' salida_segmentos <- reporte_segmentos_cascada_muestras(
#'   segmentos = segmentos_problema,
#'   dfs = dfs,
#'   diag_tres = diag_tres,
#'   diag_con_tematica = diag_con_tematica,
#'   carpeta_salida = "outputs/muestras_completitud",
#'   fecha_corte = fecha_corte
#' )
#'
#' salida_segmentos$resumen_segmento
#' salida_segmentos$cascada_detallada_segmento
#' salida_segmentos$comparacion_con_sin_tematica
#' }
#'
#' @export
reporte_segmentos_cascada_muestras <- function(segmentos,
                                               dfs = NULL,
                                               diag_tres = NULL,
                                               diag_con_tematica = NULL,
                                               em_completa = NULL,
                                               carpeta_salida = NULL,
                                               fecha_corte = NULL,
                                               reglas_tematica = NULL,
                                               exportar = !is.null(carpeta_salida),
                                               formato_exportacion = c("xlsx", "csv")) {
  formato_exportacion <- match.arg(formato_exportacion)

  insumos <- .resolver_insumos_segmentos_cascada_muestras(
    dfs = dfs,
    diag_tres = diag_tres,
    diag_con_tematica = diag_con_tematica,
    em_completa = em_completa
  )
  dfs <- insumos$dfs
  diag_tres <- insumos$diag_tres
  diag_con_tematica <- insumos$diag_con_tematica

  .validar_segmentos_cascada_muestras(
    segmentos = segmentos,
    dfs = dfs,
    diag_tres = diag_tres,
    diag_con_tematica = diag_con_tematica,
    exportar = exportar,
    carpeta_salida = carpeta_salida
  )

  segmentos_req <- .tabla_segmentos_solicitados_muestras(segmentos)
  diag_ref <- .resolver_diagnostico_reporte_criterios(
    diag_tres = diag_tres,
    diag_con_tematica = diag_con_tematica
  )
  reporte_final <- diag_ref$reporte_final_caidas

  tabla_variables_criterios <- .tabla_variables_criterios_muestras(
    diag_con_tematica = diag_con_tematica,
    reglas_tematica = reglas_tematica,
    reporte_final = reporte_final
  )

  universo <- .universo_directorio_segmentos_cascada_muestras(dfs = dfs)

  segmentos_no_encontrados <- .segmentos_no_encontrados_cascada_muestras(
    segmentos_req = segmentos_req,
    universo = universo
  )

  evaluacion_con <- .evaluar_segmentos_cascada_muestras(
    universo = universo,
    reporte_final = reporte_final,
    segmentos_req = segmentos_req
  )

  resumen_segmento <- .resumen_segmentos_cascada_muestras(
    evaluacion = evaluacion_con,
    segmentos_req = segmentos_req
  )

  cascada_detallada_segmento <- .cascada_detallada_segmentos_muestras(
    evaluacion = evaluacion_con,
    segmentos_req = segmentos_req
  )

  detalle_caidas <- .detalle_caidas_segmentos_muestras(
    evaluacion = evaluacion_con,
    reporte_final = reporte_final,
    tabla_variables_criterios = tabla_variables_criterios
  )
  resumen_detalle_segmento <- .resumen_detalle_segmentos_muestras(
    detalle_caidas = detalle_caidas,
    segmentos_req = segmentos_req
  )
  personas_asociadas_segmento <- .personas_asociadas_segmentos_muestras(
    dfs = dfs,
    evaluacion = evaluacion_con,
    segmentos_req = segmentos_req
  )

  reporte_sin_tematica <- .preparar_reporte_sin_tematica_muestras(diag_tres)
  evaluacion_sin <- .evaluar_segmentos_cascada_muestras(
    universo = universo,
    reporte_final = reporte_sin_tematica,
    segmentos_req = segmentos_req
  )

  comparacion_con_sin_tematica <- .comparacion_segmentos_tematica_muestras(
    evaluacion_con = evaluacion_con,
    evaluacion_sin = evaluacion_sin,
    segmentos_req = segmentos_req
  )

  .validar_totales_segmentos_cascada_muestras(
    resumen = resumen_segmento,
    cascada = cascada_detallada_segmento
  )

  archivos_exportados <- NULL
  if (isTRUE(exportar)) {
    archivos_exportados <- .exportar_segmentos_cascada_muestras(
      resumen_segmento = resumen_segmento,
      cascada_detallada_segmento = cascada_detallada_segmento,
      resumen_detalle_segmento = resumen_detalle_segmento,
      personas_asociadas_segmento = personas_asociadas_segmento,
      detalle_caidas = detalle_caidas,
      comparacion_con_sin_tematica = comparacion_con_sin_tematica,
      segmentos_no_encontrados = segmentos_no_encontrados,
      carpeta_salida = carpeta_salida,
      fecha_corte = fecha_corte,
      formato_exportacion = formato_exportacion
    )
  }

  list(
    resumen_segmento = resumen_segmento,
    cascada_detallada_segmento = cascada_detallada_segmento,
    detalle_caidas = detalle_caidas,
    resumen_detalle_segmento = resumen_detalle_segmento,
    personas_asociadas_segmento = personas_asociadas_segmento,
    comparacion_con_sin_tematica = comparacion_con_sin_tematica,
    segmentos_no_encontrados = segmentos_no_encontrados,
    archivos_exportados = archivos_exportados
  )
}

.resolver_insumos_segmentos_cascada_muestras <- function(dfs,
                                                         diag_tres,
                                                         diag_con_tematica,
                                                         em_completa) {
  if (is.null(dfs) && is.list(em_completa)) {
    if (is.list(em_completa$dfs_original)) {
      dfs <- em_completa$dfs_original
    } else if (is.list(em_completa$dfs)) {
      dfs <- em_completa$dfs
    }
  }

  if (is.null(diag_tres) && is.list(em_completa) && is.list(em_completa$diag_tres)) {
    diag_tres <- em_completa$diag_tres
  }

  if (is.null(diag_con_tematica) &&
      is.list(em_completa) &&
      is.list(em_completa$diag_con_tematica)) {
    diag_con_tematica <- em_completa$diag_con_tematica
  }

  list(
    dfs = dfs,
    diag_tres = diag_tres,
    diag_con_tematica = diag_con_tematica
  )
}

.validar_segmentos_cascada_muestras <- function(segmentos,
                                                dfs,
                                                diag_tres,
                                                diag_con_tematica,
                                                exportar,
                                                carpeta_salida) {
  if (is.null(segmentos)) {
    stop("`segmentos` no puede ser NULL.")
  }

  segmentos_norm <- .normalizar_segmento_cascada_muestras(segmentos)
  segmentos_norm <- segmentos_norm[!is.na(segmentos_norm) & nzchar(segmentos_norm)]
  if (length(segmentos_norm) == 0) {
    stop("`segmentos` debe contener al menos un segmento no vacio.")
  }

  if (!is.list(dfs) || !"A" %in% names(dfs) || !is.data.frame(dfs$A)) {
    stop("`dfs` debe ser una lista de capitulos e incluir `dfs$A`.")
  }
  if (!all(c("DIRECTORIO", "SEGMENTO") %in% names(dfs$A))) {
    stop("`dfs$A` debe contener `DIRECTORIO` y `SEGMENTO`.")
  }

  req_tres <- c("personas_eval", "reporte_final_caidas")
  if (!is.list(diag_tres) || length(setdiff(req_tres, names(diag_tres))) > 0) {
    stop(
      "`diag_tres` debe ser la salida de `diagnostico_caidas_tres_criterios()` ",
      "y contener `personas_eval` y `reporte_final_caidas`."
    )
  }

  if (!is.null(diag_con_tematica)) {
    req_tem <- c("personas_eval", "reporte_final_caidas")
    if (!is.list(diag_con_tematica) || length(setdiff(req_tem, names(diag_con_tematica))) > 0) {
      stop(
        "`diag_con_tematica` debe ser NULL o contener `personas_eval` y ",
        "`reporte_final_caidas`."
      )
    }
  }

  if (isTRUE(exportar) &&
      (is.null(carpeta_salida) || !is.character(carpeta_salida) || length(carpeta_salida) != 1)) {
    stop("`carpeta_salida` debe ser una ruta valida cuando `exportar = TRUE`.")
  }

  invisible(TRUE)
}

.tabla_segmentos_solicitados_muestras <- function(segmentos) {
  seg_raw <- as.character(segmentos)
  seg_norm <- .normalizar_segmento_cascada_muestras(seg_raw)
  tibble::tibble(
    orden_segmento = seq_along(seg_raw),
    SEGMENTO = stringr::str_squish(seg_raw),
    segmento_norm = seg_norm
  ) %>%
    dplyr::filter(!is.na(.data$segmento_norm), nzchar(.data$segmento_norm)) %>%
    dplyr::distinct(.data$segmento_norm, .keep_all = TRUE) %>%
    dplyr::arrange(.data$orden_segmento)
}

.normalizar_segmento_cascada_muestras <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

.mapa_segmentos_cascada_muestras <- function(dfs) {
  dfs$A %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::select(DIRECTORIO, SEGMENTO) %>%
    dplyr::mutate(
      SEGMENTO = stringr::str_squish(as.character(.data$SEGMENTO)),
      segmento_norm = .normalizar_segmento_cascada_muestras(.data$SEGMENTO)
    ) %>%
    dplyr::filter(!is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO)) %>%
    dplyr::arrange(.data$DIRECTORIO, .data$SEGMENTO) %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)
}

.universo_directorio_segmentos_cascada_muestras <- function(dfs) {
  .mapa_segmentos_cascada_muestras(dfs) %>%
    dplyr::select(DIRECTORIO, SEGMENTO, segmento_norm) %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)
}

.segmentos_no_encontrados_cascada_muestras <- function(segmentos_req, universo) {
  encontrados <- universo %>%
    dplyr::filter(!is.na(.data$segmento_norm), nzchar(.data$segmento_norm)) %>%
    dplyr::distinct(.data$segmento_norm)

  segmentos_req %>%
    dplyr::anti_join(encontrados, by = "segmento_norm") %>%
    dplyr::select(SEGMENTO)
}

.evaluar_segmentos_cascada_muestras <- function(universo,
                                                reporte_final,
                                                segmentos_req) {
  keys <- "DIRECTORIO"
  flags <- .flags_caida_nivel_muestras(reporte_final, "vivienda")
  flags_cols <- c("cae_existencia", "cae_campo", "cae_lina", "cae_duplicado", "cae_tematica")

  evaluacion <- universo %>%
    dplyr::filter(.data$segmento_norm %in% segmentos_req$segmento_norm) %>%
    dplyr::left_join(flags, by = keys)

  for (flag in flags_cols) {
    if (!flag %in% names(evaluacion)) evaluacion[[flag]] <- FALSE
    evaluacion[[flag]] <- dplyr::coalesce(evaluacion[[flag]], FALSE)
  }

  evaluacion %>%
    dplyr::mutate(
      etapa_cascada = dplyr::case_when(
        .data$cae_existencia ~ "completitud",
        .data$cae_campo ~ "campo",
        .data$cae_lina ~ "lina",
        .data$cae_duplicado ~ "duplicados",
        .data$cae_tematica ~ "tematica",
        TRUE ~ "base_final"
      )
    )
}

.etapas_segmentos_cascada_muestras <- function() {
  tibble::tibble(
    orden_etapa = 1:5,
    etapa_cascada = c("completitud", "campo", "lina", "duplicados", "tematica")
  )
}

.resumen_segmentos_cascada_muestras <- function(evaluacion, segmentos_req) {
  resumen <- evaluacion %>%
    dplyr::group_by(.data$segmento_norm) %>%
    dplyr::summarise(
      base_inicial_directorio = dplyr::n_distinct(.data$DIRECTORIO),
      cae_completitud_directorio =
        dplyr::n_distinct(.data$DIRECTORIO[.data$etapa_cascada == "completitud"]),
      cae_campo_directorio =
        dplyr::n_distinct(.data$DIRECTORIO[.data$etapa_cascada == "campo"]),
      cae_lina_directorio =
        dplyr::n_distinct(.data$DIRECTORIO[.data$etapa_cascada == "lina"]),
      cae_duplicados_directorio =
        dplyr::n_distinct(.data$DIRECTORIO[.data$etapa_cascada == "duplicados"]),
      cae_tematica_directorio =
        dplyr::n_distinct(.data$DIRECTORIO[.data$etapa_cascada == "tematica"]),
      .groups = "drop"
    )

  segmentos_req %>%
    dplyr::left_join(resumen, by = "segmento_norm") %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "base_inicial_directorio", "cae_completitud_directorio",
          "cae_campo_directorio", "cae_lina_directorio",
          "cae_duplicados_directorio", "cae_tematica_directorio"
        )),
        ~ dplyr::coalesce(as.integer(.x), 0L)
      ),
      total_caidas_directorio =
        .data$cae_completitud_directorio +
        .data$cae_campo_directorio +
        .data$cae_lina_directorio +
        .data$cae_duplicados_directorio +
        .data$cae_tematica_directorio,
      base_final_directorio = .data$base_inicial_directorio - .data$total_caidas_directorio,
      porcentaje_caida_directorio = dplyr::if_else(
        .data$base_inicial_directorio > 0,
        round(100 * .data$total_caidas_directorio / .data$base_inicial_directorio, 2),
        NA_real_
      )
    ) %>%
    dplyr::arrange(.data$orden_segmento) %>%
    dplyr::select(
      SEGMENTO,
      base_inicial_directorio,
      cae_completitud_directorio,
      cae_campo_directorio,
      cae_lina_directorio,
      cae_duplicados_directorio,
      cae_tematica_directorio,
      total_caidas_directorio,
      base_final_directorio,
      porcentaje_caida_directorio
    )
}

.cascada_detallada_segmentos_muestras <- function(evaluacion, segmentos_req) {
  etapas <- .etapas_segmentos_cascada_muestras()

  base_segmento <- evaluacion %>%
    dplyr::group_by(.data$segmento_norm) %>%
    dplyr::summarise(base_inicial_directorio = dplyr::n_distinct(.data$DIRECTORIO), .groups = "drop")

  conteos <- evaluacion %>%
    dplyr::filter(.data$etapa_cascada %in% etapas$etapa_cascada) %>%
    dplyr::group_by(.data$segmento_norm, .data$etapa_cascada) %>%
    dplyr::summarise(n_cae_etapa_directorio = dplyr::n_distinct(.data$DIRECTORIO), .groups = "drop")

  grilla <- dplyr::bind_rows(lapply(seq_len(nrow(segmentos_req)), function(i) {
    etapas %>%
      dplyr::mutate(
        SEGMENTO = segmentos_req$SEGMENTO[[i]],
        segmento_norm = segmentos_req$segmento_norm[[i]],
        orden_segmento = segmentos_req$orden_segmento[[i]]
      )
  }))

  grilla %>%
    dplyr::left_join(base_segmento, by = "segmento_norm") %>%
    dplyr::left_join(conteos, by = c("segmento_norm", "etapa_cascada")) %>%
    dplyr::mutate(
      base_inicial_directorio = dplyr::coalesce(as.integer(.data$base_inicial_directorio), 0L),
      n_cae_etapa_directorio = dplyr::coalesce(as.integer(.data$n_cae_etapa_directorio), 0L)
    ) %>%
    dplyr::arrange(.data$orden_segmento, .data$orden_etapa) %>%
    dplyr::group_by(.data$segmento_norm) %>%
    dplyr::mutate(
      n_cae_acumulado_directorio = cumsum(.data$n_cae_etapa_directorio),
      n_quedan_directorio = .data$base_inicial_directorio - .data$n_cae_acumulado_directorio,
      porcentaje_cae_etapa_directorio = dplyr::if_else(
        .data$base_inicial_directorio > 0,
        round(100 * .data$n_cae_etapa_directorio / .data$base_inicial_directorio, 2),
        NA_real_
      ),
      porcentaje_cae_acumulado_directorio = dplyr::if_else(
        .data$base_inicial_directorio > 0,
        round(100 * .data$n_cae_acumulado_directorio / .data$base_inicial_directorio, 2),
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      SEGMENTO,
      etapa_cascada,
      orden_etapa,
      n_cae_etapa_directorio,
      n_cae_acumulado_directorio,
      n_quedan_directorio,
      porcentaje_cae_etapa_directorio,
      porcentaje_cae_acumulado_directorio
    )
}

.detalle_caidas_segmentos_muestras <- function(evaluacion,
                                               reporte_final,
                                               tabla_variables_criterios) {
  keys <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  vars_detalle <- c(
    "criterio_principal_reporte", "criterios_reporte", "razon_principal_caida",
    "variable_principal_caida", "valor_principal_caida", "observacion_final",
    "capitulos_faltantes", "n_caps_faltantes", "variable_tematica",
    "razon_tematica", "observacion_tematica"
  )

  detalle_reporte <- if (is.data.frame(reporte_final) && all(keys %in% names(reporte_final))) {
    reporte_final %>%
      normalize_keys(keys) %>%
      dplyr::select(dplyr::all_of(keys), dplyr::any_of(vars_detalle)) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
  } else {
    tibble::tibble(DIRECTORIO = character(), SECUENCIA_P = character(), ORDEN = character())
  }

  detalle <- evaluacion %>%
    dplyr::filter(.data$etapa_cascada != "base_final") %>%
    dplyr::select(SEGMENTO, DIRECTORIO, etapa_cascada) %>%
    dplyr::left_join(detalle_reporte, by = "DIRECTORIO")

  for (col in setdiff(vars_detalle, "n_caps_faltantes")) {
    if (!col %in% names(detalle)) detalle[[col]] <- NA_character_
  }
  if (!"n_caps_faltantes" %in% names(detalle)) detalle$n_caps_faltantes <- NA_integer_

  detalle %>%
    dplyr::mutate(
      nivel = "persona",
      criterio_caida = dplyr::coalesce(
        as.character(.data$criterio_principal_reporte),
        as.character(.data$etapa_cascada)
      ),
      variables_regla = dplyr::coalesce(
        as.character(.data$variable_principal_caida),
        as.character(.data$variable_tematica),
        .variables_componente_detalle_segmentos(
          tabla_variables_criterios = tabla_variables_criterios,
          componente = .data$etapa_cascada
        )
      ),
      observacion = dplyr::coalesce(
        as.character(.data$observacion_final),
        as.character(.data$observacion_tematica),
        as.character(.data$razon_principal_caida),
        as.character(.data$razon_tematica)
      )
    ) %>%
    dplyr::arrange(.data$SEGMENTO, .data$etapa_cascada, .data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    dplyr::select(
      SEGMENTO,
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      nivel,
      criterio_caida,
      etapa_cascada,
      capitulos_faltantes,
      n_caps_faltantes,
      variables_regla,
      observacion
    )
}

.resumen_detalle_segmentos_muestras <- function(detalle_caidas, segmentos_req) {
  resumen <- detalle_caidas %>%
    dplyr::group_by(.data$SEGMENTO) %>%
    dplyr::summarise(
      filas_detalle = dplyr::n(),
      directorios_unicos_detalle = dplyr::n_distinct(.data$DIRECTORIO),
      hogares_unicos_detalle = .n_llaves_detalle_segmentos(
        .data$DIRECTORIO,
        .data$SECUENCIA_P
      ),
      personas_unicas_detalle = .n_llaves_detalle_segmentos(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN
      ),
      .groups = "drop"
    )

  segmentos_req %>%
    dplyr::select(SEGMENTO) %>%
    dplyr::left_join(resumen, by = "SEGMENTO") %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "filas_detalle", "directorios_unicos_detalle",
          "hogares_unicos_detalle", "personas_unicas_detalle"
        )),
        ~ dplyr::coalesce(as.integer(.x), 0L)
      )
    )
}

.n_llaves_detalle_segmentos <- function(...) {
  partes <- list(...)
  if (length(partes) == 0 || length(partes[[1]]) == 0) {
    return(0L)
  }
  df <- tibble::as_tibble(stats::setNames(partes, paste0("k", seq_along(partes))))
  for (nm in names(df)) {
    df[[nm]] <- as.character(df[[nm]])
  }
  df <- df %>%
    dplyr::filter(
      dplyr::if_all(
        dplyr::everything(),
        ~ !is.na(.x) & nzchar(as.character(.x))
      )
    ) %>%
    dplyr::distinct()
  nrow(df)
}

.personas_asociadas_segmentos_muestras <- function(dfs, evaluacion, segmentos_req) {
  caidos <- evaluacion %>%
    dplyr::filter(.data$etapa_cascada != "base_final") %>%
    dplyr::select(SEGMENTO, segmento_norm, DIRECTORIO)

  if ("E" %in% names(dfs) &&
      is.data.frame(dfs$E) &&
      all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(dfs$E))) {
    personas_e <- dfs$E %>%
      normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
      dplyr::semi_join(caidos %>% dplyr::select(DIRECTORIO), by = "DIRECTORIO") %>%
      dplyr::left_join(
        caidos %>% dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE),
        by = "DIRECTORIO"
      )
  } else {
    personas_e <- tibble::tibble(
      SEGMENTO = character(),
      segmento_norm = character(),
      DIRECTORIO = character(),
      SECUENCIA_P = character(),
      ORDEN = character()
    )
  }

  resumen_e <- personas_e %>%
    dplyr::group_by(.data$segmento_norm) %>%
    dplyr::summarise(
      hogares_observados_en_E = .n_llaves_detalle_segmentos(
        .data$DIRECTORIO,
        .data$SECUENCIA_P
      ),
      personas_observadas_en_E = .n_llaves_detalle_segmentos(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN
      ),
      .groups = "drop"
    )

  directores_caidos <- caidos %>%
    dplyr::group_by(.data$segmento_norm) %>%
    dplyr::summarise(directorios_caidos = dplyr::n_distinct(.data$DIRECTORIO), .groups = "drop")

  segmentos_req %>%
    dplyr::left_join(directores_caidos, by = "segmento_norm") %>%
    dplyr::left_join(resumen_e, by = "segmento_norm") %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "directorios_caidos", "hogares_observados_en_E",
          "personas_observadas_en_E"
        )),
        ~ dplyr::coalesce(as.integer(.x), 0L)
      )
    ) %>%
    dplyr::select(
      SEGMENTO,
      directorios_caidos,
      hogares_observados_en_E,
      personas_observadas_en_E
    )
}

.variables_componente_detalle_segmentos <- function(tabla_variables_criterios, componente) {
  vapply(componente, function(comp_i) {
    if (!is.data.frame(tabla_variables_criterios) ||
        !"componente_analisis" %in% names(tabla_variables_criterios) ||
        !"variable_encuesta" %in% names(tabla_variables_criterios)) {
      return(NA_character_)
    }
    vars <- tabla_variables_criterios %>%
      dplyr::filter(.data$componente_analisis == comp_i) %>%
      dplyr::pull(variable_encuesta)
    vars <- unique(vars[!is.na(vars) & nzchar(vars)])
    if (length(vars) == 0) NA_character_ else paste(vars, collapse = ", ")
  }, character(1))
}

.comparacion_segmentos_tematica_muestras <- function(evaluacion_con,
                                                     evaluacion_sin,
                                                     segmentos_req) {
  resumen_escenario <- function(evaluacion, escenario) {
    .resumen_segmentos_cascada_muestras(
      evaluacion = evaluacion,
      segmentos_req = segmentos_req
    ) %>%
      dplyr::transmute(
        SEGMENTO,
        escenario = escenario,
        base_inicial_directorio,
        total_caidas_directorio,
        base_final_directorio,
        porcentaje_caida_directorio
      )
  }

  dplyr::bind_rows(
    resumen_escenario(evaluacion_con, "con_reglas_tematicas"),
    resumen_escenario(evaluacion_sin, "sin_reglas_tematicas")
  ) %>%
    dplyr::arrange(.data$SEGMENTO, .data$escenario)
}

.validar_totales_segmentos_cascada_muestras <- function(resumen, cascada) {
  totales_cascada <- cascada %>%
    dplyr::group_by(.data$SEGMENTO) %>%
    dplyr::summarise(
      total_cascada_directorio = sum(.data$n_cae_etapa_directorio, na.rm = TRUE),
      .groups = "drop"
    )

  control <- resumen %>%
    dplyr::left_join(totales_cascada, by = "SEGMENTO") %>%
    dplyr::mutate(
      total_cascada_directorio = dplyr::coalesce(.data$total_cascada_directorio, 0L),
      ok_total = .data$total_caidas_directorio == .data$total_cascada_directorio,
      ok_base =
        .data$base_final_directorio ==
        .data$base_inicial_directorio - .data$total_caidas_directorio,
      ok_cota = .data$total_caidas_directorio <= .data$base_inicial_directorio
    )

  if (any(!control$ok_total | !control$ok_base | !control$ok_cota, na.rm = TRUE)) {
    stop("La validacion de totales de la cascada por segmento no fue consistente.")
  }

  invisible(TRUE)
}

.fecha_corte_segmentos_cascada_muestras <- function(fecha_corte) {
  if (is.null(fecha_corte)) {
    fecha_corte <- Sys.Date()
  }
  stringr::str_replace_all(as.character(fecha_corte)[1], "[^0-9A-Za-z_-]+", "_")
}

.exportar_segmentos_cascada_muestras <- function(resumen_segmento,
                                                 cascada_detallada_segmento,
                                                 resumen_detalle_segmento,
                                                 personas_asociadas_segmento,
                                                 detalle_caidas,
                                                 comparacion_con_sin_tematica,
                                                 segmentos_no_encontrados,
                                                 carpeta_salida,
                                                 fecha_corte,
                                                 formato_exportacion) {
  dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
  fecha_txt <- .fecha_corte_segmentos_cascada_muestras(fecha_corte)

  tablas <- list(
    `01_resumen_directorio` = resumen_segmento,
    `02_cascada_directorio` = cascada_detallada_segmento,
    `03_comparacion_directorio` = comparacion_con_sin_tematica,
    `04_resumen_detalle` = resumen_detalle_segmento,
    `05_personas_asociadas_E` = personas_asociadas_segmento,
    `06_detalle_caidas` = detalle_caidas,
    segmentos_no_encontrados = segmentos_no_encontrados
  )

  if (identical(formato_exportacion, "xlsx")) {
    ruta <- file.path(
      carpeta_salida,
      paste0("reporte_segmentos_problematicos_cascada_", fecha_txt, ".xlsx")
    )
    return(list(
      xlsx = exportar_tablas_excel(tablas, ruta = ruta)
    ))
  }

  rutas <- list(
    resumen_directorio = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_resumen_", fecha_txt, ".csv")
    ),
    cascada_directorio = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_cascada_", fecha_txt, ".csv")
    ),
    comparacion_directorio = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_comparacion_tematica_", fecha_txt, ".csv")
    ),
    resumen_detalle = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_resumen_detalle_", fecha_txt, ".csv")
    ),
    personas_asociadas_E = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_personas_asociadas_E_", fecha_txt, ".csv")
    ),
    detalle_caidas = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_detalle_caidas_", fecha_txt, ".csv")
    ),
    segmentos_no_encontrados = file.path(
      carpeta_salida,
      paste0("segmentos_problematicos_no_encontrados_", fecha_txt, ".csv")
    )
  )

  nombres_csv <- c(
    "01_resumen_directorio" = "resumen_directorio",
    "02_cascada_directorio" = "cascada_directorio",
    "03_comparacion_directorio" = "comparacion_directorio",
    "04_resumen_detalle" = "resumen_detalle",
    "05_personas_asociadas_E" = "personas_asociadas_E",
    "06_detalle_caidas" = "detalle_caidas",
    "segmentos_no_encontrados" = "segmentos_no_encontrados"
  )

  for (nm in names(tablas)) {
    utils::write.csv(tablas[[nm]], file = rutas[[nombres_csv[[nm]]]], row.names = FALSE, na = "")
  }

  lapply(rutas, normalizePath, winslash = "/", mustWork = FALSE)
}
