#' Clasifica la completitud operativa de campo
#'
#' Construye una base micro a nivel de directorio con banderas de completitud
#' de vivienda, hogar y persona usando variables de control operativo
#' presentes en los capítulos A, C y E.
#'
#' @param dfs Lista de capítulos de la encuesta.
#' @param periodo Valor del periodo a reportar. Default: 2025.
#' @param cap_viv Nombre del capítulo de vivienda. Default: "A".
#' @param cap_hog Nombre del capítulo de hogar. Default: "C".
#' @param cap_per Nombre del capítulo de personas. Default: "E".
#' @param agregar_segmento Si es TRUE agrega variables de segmento y clase.
#'
#' @return Tibble con clasificación micro por DIRECTORIO.
#' @export
clasificar_completitud_campo <- function(
    dfs,
    periodo = 2025,
    cap_viv = "A",
    cap_hog = "C",
    cap_per = "E",
    agregar_segmento = TRUE
) {

  stopifnot(is.list(dfs))
  stopifnot(cap_viv %in% names(dfs), cap_hog %in% names(dfs), cap_per %in% names(dfs))

  A <- dfs[[cap_viv]]
  C <- dfs[[cap_hog]]
  E <- dfs[[cap_per]]

  # ---------
  # Vivienda
  # ---------
  viv_micro <- A %>%
    dplyr::filter(!is.na(.data$UUID)) %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO,
      UUID,
      PERIODO = periodo,
      RES_VIV = .data$RES_VIV,
      SEGMENTO = if ("SEGMENTO" %in% names(.)) dplyr::coalesce(gsub("_", "", SEGMENTO), "SIN_SEGMENTO") else NA_character_,
      CLASE = if ("CLASE" %in% names(.)) dplyr::coalesce(CLASE, 0) else NA_real_,
      viv_efectiva = .data$NVCAPCTRL1 %in% c(1, 2),
      viv_ocupada = .data$NVCAPCTRL1 == 1,
      viv_ocupada_presente = .data$NVCAPCTRL1 == 1 & .data$NVCAPCTRL2 == 1,
      viv_resultado_completo = .data$RES_VIV == 1,
      estado_viv = dplyr::case_when(
        .data$NVCAPCTRL1 == 1 & .data$NVCAPCTRL2 == 1 ~ "ocupada_presente",
        .data$NVCAPCTRL1 == 1 & .data$NVCAPCTRL2 == 2 & .data$NVCAPCTRL2A == 1 ~ "rechazo",
        .data$NVCAPCTRL1 == 1 & .data$NVCAPCTRL2 == 2 & .data$NVCAPCTRL2A == 2 ~ "personas_ocupadas",
        .data$NVCAPCTRL1 == 1 & .data$NVCAPCTRL2 == 2 & .data$NVCAPCTRL2A == 3 ~ "nadie_en_hogar",
        .data$NVCAPCTRL1 == 1 & .data$NVCAPCTRL2 == 2 & .data$NVCAPCTRL2A == 4 ~ "ausente_temporal",
        .data$NVCAPCTRL1 == 2 ~ "vacante_desocupada",
        .data$NVCAPCTRL1 == 3 & .data$NVCAPCTRL1A == 1 ~ "direccion_inexistente",
        .data$NVCAPCTRL1 == 3 & .data$NVCAPCTRL1A == 2 ~ "uso_diferente",
        .data$NVCAPCTRL1 == 3 & .data$NVCAPCTRL1A == 3 ~ "demolicion_construccion",
        .data$NVCAPCTRL1 == 3 & .data$NVCAPCTRL1A == 4 ~ "lote",
        .data$NVCAPCTRL1 == 3 & .data$NVCAPCTRL1A == 5 ~ "otra",
        TRUE ~ "sin_clasificar"
      )
    )

  # -----
  # Hogar
  # -----
  hog_micro <- C %>%
    dplyr::transmute(
      DIRECTORIO,
      SECUENCIA_P,
      hogar_con_info = .data$NHCCPCTRL1 == 1,
      hogar_completo = .data$NHCCPCTRL1 == 1 & .data$RES_HOG == 1,
      hogar_incompleto = .data$NHCCPCTRL1 == 1 & .data$RES_HOG == 2,
      estado_hog = dplyr::case_when(
        .data$NHCCPCTRL1 == 1 & .data$RES_HOG == 1 ~ "con_info_completo",
        .data$NHCCPCTRL1 == 1 & .data$RES_HOG == 2 ~ "con_info_incompleto",
        .data$NHCCPCTRL1 == 2 & .data$NHCCPCTRL1A == 1 ~ "rechazo",
        .data$NHCCPCTRL1 == 2 & .data$NHCCPCTRL1A == 2 ~ "personas_ocupadas",
        .data$NHCCPCTRL1 == 2 & .data$NHCCPCTRL1A == 3 ~ "nadie_en_hogar",
        .data$NHCCPCTRL1 == 2 & .data$NHCCPCTRL1A == 4 ~ "ausente_temporal",
        TRUE ~ "sin_clasificar"
      )
    ) %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      n_hogares = dplyr::n(),
      hogares_completos = sum(.data$hogar_completo, na.rm = TRUE),
      hogares_incompletos = sum(.data$hogar_incompleto, na.rm = TRUE),
      todos_hogares_completos = all(.data$hogar_completo, na.rm = TRUE),
      .groups = "drop"
    )

  # -------
  # Persona
  # -------
  per_micro <- E %>%
    dplyr::transmute(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      persona_con_info = .data$NPCEPCTRL1 == 1,
      persona_completa = .data$NPCEPCTRL1 == 1 & .data$RES_PER == 1,
      persona_incompleta = .data$NPCEPCTRL1 == 1 & .data$RES_PER == 2,
      menor_10 = suppressWarnings(as.numeric(.data$NPCEP4)) < 10,
      estado_per = dplyr::case_when(
        .data$NPCEPCTRL1 == 1 & .data$RES_PER == 1 ~ "con_info_completo",
        .data$NPCEPCTRL1 == 1 & .data$RES_PER == 2 ~ "con_info_incompleto",
        .data$NPCEPCTRL1 == 2 & .data$NPCEPCTRL1A == 1 ~ "rechazo",
        .data$NPCEPCTRL1 == 2 & .data$NPCEPCTRL1A == 2 ~ "ocupado",
        .data$NPCEPCTRL1 == 2 & .data$NPCEPCTRL1A == 3 ~ "ausente_temporal",
        TRUE ~ "sin_clasificar"
      )
    ) %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      n_personas = dplyr::n(),
      personas_completas = sum(.data$persona_completa, na.rm = TRUE),
      personas_incompletas = sum(.data$persona_incompleta, na.rm = TRUE),
      menores_10 = sum(.data$menor_10, na.rm = TRUE),
      todas_personas_completas = all(.data$persona_completa, na.rm = TRUE),
      .groups = "drop"
    )

  salida <- viv_micro %>%
    dplyr::left_join(hog_micro, by = "DIRECTORIO") %>%
    dplyr::left_join(per_micro, by = "DIRECTORIO") %>%
    dplyr::mutate(
      encuesta_completa_campo =
        .data$viv_ocupada_presente &
        dplyr::coalesce(.data$viv_resultado_completo, FALSE) &
        dplyr::coalesce(.data$todos_hogares_completos, FALSE) &
        dplyr::coalesce(.data$todas_personas_completas, FALSE),
      encuesta_efectiva_campo =
        .data$viv_efectiva &
        dplyr::coalesce(.data$n_hogares, 0) > 0
    )

  if (!agregar_segmento) {
    salida <- salida %>% dplyr::select(-dplyr::any_of(c("SEGMENTO", "CLASE")))
  }

  salida
}

#' Diagnóstico de completitud operativa de campo
#'
#' Resume la completitud operativa a partir de controles de vivienda, hogar
#' y persona, devolviendo tablas generales, por segmento y el listado de
#' encuestas incompletas.
#'
#' @param dfs Lista de capítulos.
#' @param periodo Periodo de referencia.
#'
#' @return Lista con base_eval, resumen_general, resumen_segmento y encuestas_incompletas.
#' @export
diagnostico_completitud_campo <- function(dfs, periodo = 2025) {

  base_eval <- clasificar_completitud_campo(
    dfs = dfs,
    periodo = periodo
  )

  resumen_general <- base_eval %>%
    dplyr::summarise(
      encuestas_totales = dplyr::n(),
      encuestas_efectivas = sum(.data$encuesta_efectiva_campo, na.rm = TRUE),
      encuestas_completas = sum(.data$encuesta_completa_campo, na.rm = TRUE),
      encuestas_incompletas = encuestas_efectivas - encuestas_completas,
      pct_completas = encuestas_completas / encuestas_totales,
      pct_efectivas = encuestas_efectivas / encuestas_totales
    )

  resumen_segmento <- base_eval %>%
    dplyr::group_by(.data$SEGMENTO, .data$CLASE) %>%
    dplyr::summarise(
      encuestas_totales = dplyr::n(),
      encuestas_efectivas = sum(.data$encuesta_efectiva_campo, na.rm = TRUE),
      encuestas_completas = sum(.data$encuesta_completa_campo, na.rm = TRUE),
      encuestas_incompletas = encuestas_efectivas - encuestas_completas,
      cobertura = encuestas_completas / encuestas_totales,
      caida = 1 - cobertura,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$caida))

  encuestas_incompletas <- base_eval %>%
    dplyr::filter(!.data$encuesta_completa_campo) %>%
    dplyr::mutate(
      motivo_principal = dplyr::case_when(
        !.data$viv_ocupada_presente ~ paste0("vivienda_", .data$estado_viv),
        !.data$viv_resultado_completo ~ "vivienda_res_viv",
        !.data$todos_hogares_completos ~ "hogar_incompleto",
        !.data$todas_personas_completas ~ "persona_incompleta",
        TRUE ~ "otro"
      )
    ) %>%
    dplyr::arrange(.data$motivo_principal, .data$SEGMENTO, .data$DIRECTORIO)

  list(
    base_eval = base_eval,
    resumen_general = resumen_general,
    resumen_segmento = resumen_segmento,
    encuestas_incompletas = encuestas_incompletas
  )
}

#' Compara completitud estructural vs operativa de campo
#'
#' @param dfs Lista de capítulos.
#' @param caps_orden Orden de capítulos para el pipeline estructural.
#'
#' @return Lista con cruce micro y tabla resumen.
#' @export
comparar_completitud_existencia_vs_campo <- function(
    dfs,
    caps_orden = c("A","B","C","D","E","F","G","H","I","J","K","L","MA","MB")
) {

  pipe_comp <- pipeline_encuestas_completas(
    dfs = dfs,
    caps_orden = caps_orden
  )

  ids_existencia <- pipe_comp$final %>%
    dplyr::distinct(.data$DIRECTORIO) %>%
    dplyr::mutate(encuesta_completa_existencia = TRUE)

  campo <- clasificar_completitud_campo(dfs)

  comparado <- campo %>%
    dplyr::left_join(ids_existencia, by = "DIRECTORIO") %>%
    dplyr::mutate(
      encuesta_completa_existencia = dplyr::coalesce(.data$encuesta_completa_existencia, FALSE)
    )

  resumen_cruce <- comparado %>%
    dplyr::count(
      .data$encuesta_completa_existencia,
      .data$encuesta_completa_campo,
      name = "n"
    )

  list(
    base_eval = comparado,
    resumen_cruce = resumen_cruce
  )
}


