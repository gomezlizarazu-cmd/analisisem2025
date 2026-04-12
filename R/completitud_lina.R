#' Diagnostico de completitud por criterio Lina
#'
#' Evalua un criterio alternativo de completitud basado en variables de control
#' de los capitulos A, B, C, MA y E, manteniendo separadas las caidas por
#' existencia de las caidas por este nuevo criterio.
#'
#' El diagnostico se construye por nivel:
#'
#' - vivienda: `DIRECTORIO`
#' - hogar: `DIRECTORIO + SECUENCIA_P`
#' - persona: `DIRECTORIO + SECUENCIA_P + ORDEN`
#'
#' La regla especial corresponde al capitulo `B`, que se trata a nivel vivienda
#' por `DIRECTORIO`. Las validaciones de `C` y `MA` se mantienen a nivel hogar.
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param cap_viv_ctrl Capitulo con controles de vivienda. Default: `"A"`.
#' @param cap_viv_det Capitulo con variables de vivienda requeridas para el
#'   criterio Lina. Default: `"B"`.
#' @param cap_hog_ctrl Capitulo con controles de hogar. Default: `"C"`.
#' @param cap_hog_gastos Capitulo con variables de gastos requeridas para el
#'   criterio Lina. Default: `"MA"`.
#' @param cap_per Capitulo de persona. Default: `"E"`.
#' @param incluir_existencia Si es `TRUE`, calcula y cruza con
#'   `diagnostico_cruce_capitulos(dfs)`.
#' @param exportar_excel Si es `TRUE`, exporta solo las encuestas caidas.
#' @param archivo Ruta del archivo Excel a generar.
#' @param base_cap_persona Capitulo base para construir la sabana ancha.
#' @param base_hogar_cap Capitulo de referencia para universo de hogares.
#' @param caps_sabana Orden de capitulos para la sabana ancha. Si es `NULL`,
#'   usa todos los nombres de `dfs`.
#' @param edad_var Variable de edad preferida para `pegar_tablas()` y cruces de
#'   existencia. Si no existe, se intenta con `NPCEP4` y luego con `Edad`.
#'
#' @return Lista con evaluaciones por nivel, encuestas caidas y cruce contra
#'   existencia. Si `exportar_excel = TRUE`, incluye `archivo`.
#' @export
diagnostico_completitud_lina <- function(
    dfs,
    cap_viv_ctrl = "A",
    cap_viv_det = "B",
    cap_hog_ctrl = "C",
    cap_hog_gastos = "MA",
    cap_per = "E",
    incluir_existencia = TRUE,
    exportar_excel = FALSE,
    archivo = "encuestas_caidas_lina.xlsx",
    base_cap_persona = "E",
    base_hogar_cap = "B",
    caps_sabana = NULL,
    edad_var = "NPCEP4"
) {

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres de capitulos.")
  }

  names(dfs) <- toupper(names(dfs))
  cap_viv_ctrl <- toupper(cap_viv_ctrl)
  cap_viv_det <- toupper(cap_viv_det)
  cap_hog_ctrl <- toupper(cap_hog_ctrl)
  cap_hog_gastos <- toupper(cap_hog_gastos)
  cap_per <- toupper(cap_per)
  base_cap_persona <- toupper(base_cap_persona)
  base_hogar_cap <- toupper(base_hogar_cap)

  if (is.null(caps_sabana)) {
    caps_sabana <- names(dfs)
  }
  caps_sabana <- toupper(caps_sabana)

  caps_req <- c(cap_viv_ctrl, cap_viv_det, cap_hog_ctrl, cap_hog_gastos, cap_per)
  if (!all(caps_req %in% names(dfs))) {
    faltan_caps <- setdiff(caps_req, names(dfs))
    stop("Faltan capitulos requeridos en `dfs`: ", paste(faltan_caps, collapse = ", "))
  }

  .validar_variables_lina(
    dfs = dfs,
    cap_viv_ctrl = cap_viv_ctrl,
    cap_viv_det = cap_viv_det,
    cap_hog_ctrl = cap_hog_ctrl,
    cap_hog_gastos = cap_hog_gastos,
    cap_per = cap_per
  )

  if (exportar_excel && !requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  A <- normalize_keys(dfs[[cap_viv_ctrl]], c("DIRECTORIO"))
  B <- normalize_keys(dfs[[cap_viv_det]], c("DIRECTORIO"))
  C <- normalize_keys(dfs[[cap_hog_ctrl]], c("DIRECTORIO", "SECUENCIA_P"))
  MA <- normalize_keys(dfs[[cap_hog_gastos]], c("DIRECTORIO", "SECUENCIA_P"))
  E <- normalize_keys(dfs[[cap_per]], c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  edad_var_use <- col_first_existing(
    dfs[[base_cap_persona]],
    unique(c(edad_var, "NPCEP4", "Edad"))
  )

  if (is.null(edad_var_use)) {
    stop(
      "No se encontró variable de edad en `", base_cap_persona, "` entre: ",
      paste(unique(c(edad_var, "NPCEP4", "Edad")), collapse = ", ")
    )
  }

  viviendas_universo <- .build_universo_vivienda(
    dfs = dfs,
    caps_all = names(dfs)
  )

  hogares_universo <- .build_universo_hogar(
    dfs = dfs,
    base_hogar_cap = base_hogar_cap,
    base_persona_cap = base_cap_persona
  ) %>%
    dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P)

  personas_universo <- .build_universo_persona(
    dfs = dfs,
    base_persona_cap = base_cap_persona,
    edad_var = edad_var_use
  ) %>%
    dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .data$edad)

  viv_ctrl <- A %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)

  uuid_var <- col_first_existing(viv_ctrl, c("UUID"))
  seg_var <- col_first_existing(viv_ctrl, c("SEGMENTO"))
  clase_var <- col_first_existing(viv_ctrl, c("CLASE"))

  viv_ctrl$UUID <- if (!is.null(uuid_var)) as.character(viv_ctrl[[uuid_var]]) else NA_character_
  viv_ctrl$SEGMENTO <- if (!is.null(seg_var)) as.character(viv_ctrl[[seg_var]]) else NA_character_
  viv_ctrl$CLASE <- if (!is.null(clase_var)) as.character(viv_ctrl[[clase_var]]) else NA_character_

  viv_ctrl <- viv_ctrl %>%
    dplyr::transmute(
      DIRECTORIO,
      UUID = UUID,
      SEGMENTO = SEGMENTO,
      CLASE = CLASE,
      .data$NVCAPCTRL1,
      .data$NVCAPCTRL2
    )

  viv_det <- B %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO,
      NVCBP1,
      NVCBP16
    )

  viviendas_eval <- viviendas_universo %>%
    dplyr::left_join(viv_ctrl, by = "DIRECTORIO") %>%
    dplyr::left_join(viv_det, by = "DIRECTORIO") %>%
    dplyr::mutate(
      vivienda_completa_lina =
        .data$NVCAPCTRL1 == 1 &
        .data$NVCAPCTRL2 == 1 &
        .valor_no_vacio(.data$NVCBP1) &
        .valor_no_vacio(.data$NVCBP16),
      motivo_vivienda_lina = dplyr::case_when(
        is.na(.data$NVCAPCTRL1) ~ "sin registro en A",
        .data$NVCAPCTRL1 != 1 ~ "NVCAPCTRL1 != 1",
        .data$NVCAPCTRL2 != 1 ~ "NVCAPCTRL2 != 1",
        !.valor_no_vacio(.data$NVCBP1) ~ "NVCBP1 vacia",
        !.valor_no_vacio(.data$NVCBP16) ~ "NVCBP16 vacia",
        TRUE ~ "cumple"
      )
    )

  hog_ctrl <- C %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO,
      SECUENCIA_P,
      NHCCPCTRL1,
      NHCCP1
    )

  hog_gastos <- MA %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO,
      SECUENCIA_P,
      NHCMP1A,
      NHCMP5A
    )

  hogares_eval <- hogares_universo %>%
    dplyr::left_join(hog_ctrl, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(hog_gastos, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::mutate(
      hogar_completo_lina =
        .data$NHCCPCTRL1 == 1 &
        .valor_no_vacio(.data$NHCCP1) &
        .valor_no_vacio(.data$NHCMP1A) &
        .valor_no_vacio(.data$NHCMP5A),
      motivo_hogar_lina = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "sin registro en C",
        .data$NHCCPCTRL1 != 1 ~ "NHCCPCTRL1 != 1",
        !.valor_no_vacio(.data$NHCCP1) ~ "NHCCP1 vacia",
        !.valor_no_vacio(.data$NHCMP1A) ~ "NHCMP1A vacia",
        !.valor_no_vacio(.data$NHCMP5A) ~ "NHCMP5A vacia",
        TRUE ~ "cumple"
      )
    )

  per_ctrl <- E %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      NPCEPCTRL1,
      NPCEP6
    )

  personas_eval <- personas_universo %>%
    dplyr::left_join(per_ctrl, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::mutate(
      persona_completa_lina =
        .data$NPCEPCTRL1 == 1 &
        .valor_no_vacio(.data$NPCEP6),
      motivo_persona_lina = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "sin registro en E",
        .data$NPCEPCTRL1 != 1 ~ "NPCEPCTRL1 != 1",
        !.valor_no_vacio(.data$NPCEP6) ~ "NPCEP6 vacia",
        TRUE ~ "cumple"
      )
    )

  viviendas_caidas_lina <- viviendas_eval %>%
    dplyr::filter(!.data$vivienda_completa_lina)

  hogares_caidos_lina <- hogares_eval %>%
    dplyr::filter(!.data$hogar_completo_lina)

  personas_caidas_lina <- personas_eval %>%
    dplyr::filter(!.data$persona_completa_lina)

  encuestas_caidas_lina <- personas_universo %>%
    dplyr::left_join(
      viviendas_eval %>%
        dplyr::select(
          .data$DIRECTORIO,
          dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
          .data$NVCAPCTRL1,
          .data$NVCAPCTRL2,
          .data$NVCBP1,
          .data$NVCBP16,
          .data$vivienda_completa_lina,
          .data$motivo_vivienda_lina
        ),
      by = "DIRECTORIO"
    ) %>%
    dplyr::left_join(
      hogares_eval %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$SECUENCIA_P,
          .data$NHCCPCTRL1,
          .data$NHCCP1,
          .data$NHCMP1A,
          .data$NHCMP5A,
          .data$hogar_completo_lina,
          .data$motivo_hogar_lina
        ),
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::left_join(
      personas_eval %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$SECUENCIA_P,
          .data$ORDEN,
          .data$NPCEPCTRL1,
          .data$NPCEP6,
          .data$persona_completa_lina,
          .data$motivo_persona_lina
        ),
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::mutate(
      cae_por_vivienda_lina = !dplyr::coalesce(.data$vivienda_completa_lina, TRUE),
      cae_por_hogar_lina = !dplyr::coalesce(.data$hogar_completo_lina, TRUE),
      cae_por_persona_lina = !dplyr::coalesce(.data$persona_completa_lina, TRUE),
      encuesta_caida_lina =
        .data$cae_por_vivienda_lina |
        .data$cae_por_hogar_lina |
        .data$cae_por_persona_lina,
      motivo_principal_lina = dplyr::case_when(
        .data$cae_por_vivienda_lina ~ "vivienda",
        .data$cae_por_hogar_lina ~ "hogar",
        .data$cae_por_persona_lina ~ "persona",
        TRUE ~ "cumple"
      ),
      motivo_detallado_lina = dplyr::case_when(
        .data$cae_por_vivienda_lina ~ .data$motivo_vivienda_lina,
        .data$cae_por_hogar_lina ~ .data$motivo_hogar_lina,
        .data$cae_por_persona_lina ~ dplyr::coalesce(.data$motivo_persona_lina, "persona"),
        TRUE ~ "cumple"
      ),
      criterio_falla_lina = dplyr::case_when(
        .data$cae_por_vivienda_lina & .data$cae_por_hogar_lina & .data$cae_por_persona_lina ~
          paste(
            paste0("vivienda: ", .data$motivo_vivienda_lina),
            paste0("hogar: ", .data$motivo_hogar_lina),
            paste0("persona: ", dplyr::coalesce(.data$motivo_persona_lina, "NA")),
            sep = " | "
          ),
        .data$cae_por_vivienda_lina & .data$cae_por_hogar_lina ~
          paste0("vivienda: ", .data$motivo_vivienda_lina, " | hogar: ", .data$motivo_hogar_lina),
        .data$cae_por_vivienda_lina & .data$cae_por_persona_lina ~
          paste0("vivienda: ", .data$motivo_vivienda_lina, " | persona: ", dplyr::coalesce(.data$motivo_persona_lina, "NA")),
        .data$cae_por_hogar_lina & .data$cae_por_persona_lina ~
          paste0("hogar: ", .data$motivo_hogar_lina, " | persona: ", dplyr::coalesce(.data$motivo_persona_lina, "NA")),
        .data$cae_por_vivienda_lina ~ paste0("vivienda: ", .data$motivo_vivienda_lina),
        .data$cae_por_hogar_lina ~ paste0("hogar: ", .data$motivo_hogar_lina),
        .data$cae_por_persona_lina ~ paste0("persona: ", dplyr::coalesce(.data$motivo_persona_lina, "NA")),
        TRUE ~ "cumple"
      )
    ) %>%
    dplyr::filter(.data$encuesta_caida_lina) %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE", "edad")),
      .data$NVCAPCTRL1,
      .data$NVCAPCTRL2,
      .data$NVCBP1,
      .data$NVCBP16,
      .data$NHCCPCTRL1,
      .data$NHCCP1,
      .data$NHCMP1A,
      .data$NHCMP5A,
      .data$NPCEPCTRL1,
      .data$NPCEP6,
      .data$cae_por_vivienda_lina,
      .data$cae_por_hogar_lina,
      .data$cae_por_persona_lina,
      .data$encuesta_caida_lina,
      .data$motivo_principal_lina,
      .data$motivo_detallado_lina,
      .data$criterio_falla_lina
    ) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
    dplyr::arrange(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  cruce_existencia <- NULL
  existencia_persona <- NULL

  if (isTRUE(incluir_existencia)) {
    diag_existencia <- diagnostico_cruce_capitulos(
      dfs = dfs,
      base_hogar_cap = base_hogar_cap,
      base_persona_cap = base_cap_persona,
      edad_var = edad_var_use
    )

    existencia_persona <- diag_existencia$resumen_caidas_regla2 %>%
      dplyr::filter(.data$nivel == "persona") %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN,
        capitulos_caida_existencia = .data$capitulos_caida,
        fuentes_caida_existencia = .data$fuentes_caida,
        n_eventos_existencia = .data$n_eventos
      ) %>%
      dplyr::distinct()

    cruce_existencia <- dplyr::full_join(
      encuestas_caidas_lina %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$SECUENCIA_P,
          .data$ORDEN,
          dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
          .data$motivo_principal_lina,
          .data$motivo_detallado_lina,
          .data$criterio_falla_lina
        ) %>%
        dplyr::mutate(cae_lina = 1L),
      existencia_persona %>%
        dplyr::mutate(cae_existencia = 1L),
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
      dplyr::mutate(
        cae_lina = dplyr::coalesce(.data$cae_lina, 0L),
        cae_existencia = dplyr::coalesce(.data$cae_existencia, 0L),
        tipo_cruce = dplyr::case_when(
          .data$cae_lina == 1L & .data$cae_existencia == 1L ~ "misma_caida",
          .data$cae_lina == 1L & .data$cae_existencia == 0L ~ "adicional_lina",
          .data$cae_lina == 0L & .data$cae_existencia == 1L ~ "solo_existencia",
          TRUE ~ "sin_caida"
        )
      ) %>%
      dplyr::arrange(.data$tipo_cruce, .data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)
  }

  sabana_caidas_lina <- NULL
  if (length(caps_sabana) > 0) {
    caps_validos <- intersect(caps_sabana, names(dfs))

    pega_sabana <- pegar_tablas(
      dfs = dfs[caps_validos],
      base_cap = base_cap_persona,
      caps_orden = caps_validos,
      join = "left",
      edad_var = edad_var_use
    )

    sabana_caidas_lina <- pega_sabana$data %>%
      normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
      dplyr::inner_join(
        encuestas_caidas_lina %>%
          dplyr::select(
            dplyr::all_of(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")),
            dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
            dplyr::all_of(c(
              "motivo_principal_lina",
              "motivo_detallado_lina",
              "criterio_falla_lina"
            ))
          ),
        by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
      ) %>%
      dplyr::relocate(
        dplyr::all_of(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")),
        dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
        dplyr::all_of(c(
          "motivo_principal_lina",
          "motivo_detallado_lina",
          "criterio_falla_lina"
        ))
      )
  }

  salida <- list(
    variables_control = .variables_control_lina(),
    edad_var_use = edad_var_use,
    viviendas_eval = viviendas_eval,
    hogares_eval = hogares_eval,
    personas_eval = personas_eval,
    viviendas_caidas_lina = viviendas_caidas_lina,
    hogares_caidos_lina = hogares_caidos_lina,
    personas_caidas_lina = personas_caidas_lina,
    encuestas_caidas_lina = encuestas_caidas_lina,
    cruce_existencia = cruce_existencia,
    sabana_caidas_lina = sabana_caidas_lina
  )

  if (isTRUE(exportar_excel)) {
    salida$archivo <- exportar_reporte_encuestas_caidas_lina(
      res_lina = salida,
      archivo = archivo
    )
  }

  salida
}

#' Exportar reporte de encuestas caidas por criterio Lina
#'
#' Exporta a Excel solo las encuestas caidas por criterio Lina y su cruce contra
#' existencia. Incluye una sabana ancha de la encuesta completa para las caidas.
#'
#' @param res_lina Resultado de `diagnostico_completitud_lina()`.
#' @param archivo Ruta del archivo Excel.
#'
#' @return Ruta normalizada del archivo exportado.
#' @export
exportar_reporte_encuestas_caidas_lina <- function(
    res_lina,
    archivo = "encuestas_caidas_lina.xlsx"
) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar el Excel.")
  }

  if (!is.list(res_lina) || !"encuestas_caidas_lina" %in% names(res_lina)) {
    stop("`res_lina` debe ser una lista producida por `diagnostico_completitud_lina()`.")
  }

  wb <- openxlsx::createWorkbook()

  encuestas_caidas_lina <- arreglar_utf8_df(
    res_lina$encuestas_caidas_lina %>%
      dplyr::select(
        -dplyr::any_of(c(
          "n_personas_caidas_lina",
          "ordenes_persona_caida",
          "motivos_persona_lina"
        ))
      ) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
      dplyr::mutate(dplyr::across(where(is.factor), as.character))
  )

  openxlsx::addWorksheet(wb, "caidas_lina")
  openxlsx::writeData(wb, "caidas_lina", encuestas_caidas_lina)

  if (!is.null(res_lina$cruce_existencia)) {
    cruce_existencia <- arreglar_utf8_df(
      res_lina$cruce_existencia %>%
        dplyr::mutate(dplyr::across(where(is.factor), as.character))
    )
    openxlsx::addWorksheet(wb, "cruce_existencia")
    openxlsx::writeData(wb, "cruce_existencia", cruce_existencia)
  }

  if (!is.null(res_lina$sabana_caidas_lina)) {
    sabana_caidas_lina <- arreglar_utf8_df(
      res_lina$sabana_caidas_lina %>%
        dplyr::mutate(
          dplyr::across(where(is.factor), as.character),
          dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
        )
    )
    openxlsx::addWorksheet(wb, "sabana_caidas_lina")
    openxlsx::writeData(wb, "sabana_caidas_lina", sabana_caidas_lina)
  }

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)

  normalizePath(archivo, winslash = "/", mustWork = FALSE)
}

#' Diagnostico grafico de caidas del criterio Lina
#'
#' Construye un resumen de caidas del criterio Lina y un grafico de barras
#' basado en la misma unidad analitica de personas. Para vivienda y hogar, las
#' razones de caida se propagan al nivel persona para que el total y sus
#' desagregaciones sean comparables con `encuestas_caidas_lina`.
#'
#' @param res_lina Resultado de `diagnostico_completitud_lina()`. Si es `NULL`,
#'   la funcion lo calcula internamente a partir de `dfs`.
#' @param dfs Lista nombrada de data frames por capitulo. Se usa solo si
#'   `res_lina` es `NULL`.
#' @param plot Si es `TRUE`, construye el grafico interactivo con `plotly`.
#' @param main Titulo principal del grafico.
#' @param cex.names Argumento conservado por compatibilidad. No se usa en la
#'   version interactiva.
#' @param las Argumento conservado por compatibilidad. No se usa en la version
#'   interactiva.
#' @param ... Argumentos adicionales que se pasan a
#'   `diagnostico_completitud_lina()` cuando `res_lina` es `NULL`.
#'
#' @return Lista con:
#'   \itemize{
#'     \item \code{resumen_totales}: total de caidas por vivienda, hogar y persona
#'     a nivel persona.
#'     \item \code{resumen_razones}: razones de caida por nivel, tambien a nivel
#'     persona.
#'     \item \code{datos_grafico}: tabla usada para construir el grafico.
#'     \item \code{grafico}: objeto `plotly` si \code{plot = TRUE}; en otro caso
#'     `NULL`.
#'   }
#' @export
diagnostico_grafico_caidas_lina <- function(
    res_lina = NULL,
    dfs = NULL,
    plot = TRUE,
    main = "Caidas criterio Lina por nivel y razon",
    cex.names = 0.8,
    las = 2,
    ...
) {

  if (is.null(res_lina)) {
    if (is.null(dfs)) {
      stop("Debe suministrar `res_lina` o `dfs`.")
    }

    res_lina <- diagnostico_completitud_lina(
      dfs = dfs,
      ...
    )
  }

  if (!is.list(res_lina) || !all(c("viviendas_eval", "hogares_eval", "personas_eval") %in% names(res_lina))) {
    stop("`res_lina` debe ser una lista producida por `diagnostico_completitud_lina()`.")
  }

  personas_base <- res_lina$personas_eval %>%
    dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  razones_vivienda <- personas_base %>%
    dplyr::left_join(
      res_lina$viviendas_eval %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$vivienda_completa_lina,
          .data$motivo_vivienda_lina
        ),
      by = "DIRECTORIO"
    ) %>%
    dplyr::filter(!dplyr::coalesce(.data$vivienda_completa_lina, TRUE)) %>%
    dplyr::count(
      motivo = .data$motivo_vivienda_lina,
      name = "n_caidas"
    ) %>%
    dplyr::mutate(
      nivel = "vivienda",
      tipo_barra = "motivo"
    )

  razones_hogar <- personas_base %>%
    dplyr::left_join(
      res_lina$hogares_eval %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$SECUENCIA_P,
          .data$hogar_completo_lina,
          .data$motivo_hogar_lina
        ),
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::filter(!dplyr::coalesce(.data$hogar_completo_lina, TRUE)) %>%
    dplyr::count(
      motivo = .data$motivo_hogar_lina,
      name = "n_caidas"
    ) %>%
    dplyr::mutate(
      nivel = "hogar",
      tipo_barra = "motivo"
    )

  razones_persona <- res_lina$personas_eval %>%
    dplyr::filter(!dplyr::coalesce(.data$persona_completa_lina, TRUE)) %>%
    dplyr::count(
      motivo = .data$motivo_persona_lina,
      name = "n_caidas"
    ) %>%
    dplyr::mutate(
      nivel = "persona",
      tipo_barra = "motivo"
    )

  resumen_razones <- dplyr::bind_rows(
    razones_vivienda,
    razones_hogar,
    razones_persona
  ) %>%
    dplyr::mutate(
      nivel = factor(.data$nivel, levels = c("vivienda", "hogar", "persona")),
      motivo = dplyr::coalesce(.data$motivo, "sin motivo")
    ) %>%
    dplyr::arrange(.data$nivel, dplyr::desc(.data$n_caidas), .data$motivo)

  resumen_totales <- resumen_razones %>%
    dplyr::group_by(.data$nivel) %>%
    dplyr::summarise(
      n_caidas = sum(.data$n_caidas, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tipo_barra = "total",
      motivo = "Total caidas"
    )

  datos_grafico <- dplyr::bind_rows(
    resumen_totales,
    resumen_razones
  ) %>%
    dplyr::group_by(.data$nivel) %>%
    dplyr::group_modify(~ .preparar_barras_nivel_lina(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      nivel = factor(.data$nivel, levels = c("vivienda", "hogar", "persona")),
      etiqueta_barra = dplyr::if_else(
        .data$tipo_barra == "total",
        "Total",
        stringr::str_wrap(.data$motivo, width = 16)
      ),
      color_barra = dplyr::case_when(
        .data$nivel == "vivienda" & .data$tipo_barra == "total" ~ "#1b4965",
        .data$nivel == "vivienda" ~ "#62b6cb",
        .data$nivel == "hogar" & .data$tipo_barra == "total" ~ "#6a040f",
        .data$nivel == "hogar" ~ "#f28482",
        .data$nivel == "persona" & .data$tipo_barra == "total" ~ "#3a5a40",
        .data$nivel == "persona" ~ "#95d5b2",
        TRUE ~ "#6c757d"
      )
    ) %>%
    dplyr::arrange(.data$nivel, .data$orden_barra)

  grafico <- NULL

  if (isTRUE(plot)) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Se requiere el paquete `plotly` para construir el grafico interactivo.")
    }

    max_y <- if (nrow(datos_grafico) > 0) max(datos_grafico$n_caidas, na.rm = TRUE) else 0
    if (!is.finite(max_y)) {
      max_y <- 0
    }

    datos_grafico <- datos_grafico %>%
      dplyr::mutate(
        x_pos = seq_len(dplyr::n()),
        hover_text = paste0(
          "Nivel: ", tools::toTitleCase(as.character(.data$nivel)),
          "<br>Barra: ", dplyr::if_else(.data$tipo_barra == "total", "Total", "Razon"),
          "<br>Detalle: ", .data$motivo,
          "<br>Caidas: ", scales::comma(.data$n_caidas)
        )
      )

    anotaciones <- datos_grafico %>%
      dplyr::group_by(.data$nivel) %>%
      dplyr::summarise(
        x = mean(.data$x_pos),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        y = max_y * 1.08,
        label = tools::toTitleCase(as.character(.data$nivel))
      )

    grafico <- plotly::plot_ly(
      data = datos_grafico,
      x = ~x_pos,
      y = ~n_caidas,
      type = "bar",
      hovertext = ~hover_text,
      hovertemplate = "%{hovertext}<extra></extra>",
      marker = list(color = datos_grafico$color_barra)
    ) %>%
      plotly::layout(
        title = list(text = main),
        xaxis = list(
          title = "",
          tickmode = "array",
          tickvals = datos_grafico$x_pos,
          ticktext = datos_grafico$etiqueta_barra,
          tickangle = -45
        ),
        yaxis = list(
          title = "Numero de caidas (nivel persona)"
        ),
        showlegend = FALSE,
        bargap = 0.25,
        annotations = lapply(
          seq_len(nrow(anotaciones)),
          function(i) {
            list(
              x = anotaciones$x[i],
              y = anotaciones$y[i],
              text = anotaciones$label[i],
              showarrow = FALSE,
              yanchor = "bottom",
              font = list(size = 12)
            )
          }
        )
      )
  }

  list(
    resumen_totales = resumen_totales %>% dplyr::arrange(.data$nivel),
    resumen_razones = resumen_razones,
    datos_grafico = datos_grafico,
    grafico = grafico
  )
}

.variables_control_lina <- function() {
  list(
    A = c("NVCAPCTRL1", "NVCAPCTRL2"),
    B = c("NVCBP1", "NVCBP16"),
    C = c("NHCCPCTRL1", "NHCCP1"),
    MA = c("NHCMP1A", "NHCMP5A"),
    E = c("NPCEPCTRL1", "NPCEP6")
  )
}

.validar_variables_lina <- function(dfs,
                                    cap_viv_ctrl,
                                    cap_viv_det,
                                    cap_hog_ctrl,
                                    cap_hog_gastos,
                                    cap_per) {
  mapa <- .variables_control_lina()
  mapa_caps <- list(
    A = cap_viv_ctrl,
    B = cap_viv_det,
    C = cap_hog_ctrl,
    MA = cap_hog_gastos,
    E = cap_per
  )

  faltantes <- lapply(names(mapa), function(nm) {
    cap_real <- mapa_caps[[nm]]
    vars_req <- mapa[[nm]]
    vars_faltan <- setdiff(vars_req, names(dfs[[cap_real]]))

    if (length(vars_faltan) == 0) {
      return(NULL)
    }

    tibble::tibble(
      capitulo = cap_real,
      variable = vars_faltan
    )
  })

  faltantes <- dplyr::bind_rows(faltantes)

  if (nrow(faltantes) > 0) {
    stop(
      "Faltan variables de control para el criterio Lina: ",
      paste(paste0(faltantes$capitulo, "$", faltantes$variable), collapse = ", ")
    )
  }

  invisible(TRUE)
}

.valor_no_vacio <- function(x) {
  !is.na(x) & trimws(as.character(x)) != ""
}

.preparar_barras_nivel_lina <- function(df_nivel) {
  total <- df_nivel %>%
    dplyr::filter(.data$tipo_barra == "total") %>%
    dplyr::mutate(posicion_relativa = 0L)

  motivos <- df_nivel %>%
    dplyr::filter(.data$tipo_barra == "motivo") %>%
    dplyr::arrange(dplyr::desc(.data$n_caidas), .data$motivo)

  if (nrow(motivos) > 0) {
    motivos <- motivos %>%
      dplyr::mutate(
        posicion_relativa = .orden_lados_total_lina(dplyr::n())
      )
  }

  dplyr::bind_rows(total, motivos) %>%
    dplyr::arrange(.data$posicion_relativa) %>%
    dplyr::mutate(orden_barra = dplyr::row_number())
}

.orden_lados_total_lina <- function(n) {
  if (n <= 0) {
    return(integer(0))
  }

  out <- integer(n)
  for (i in seq_len(n)) {
    lado <- if (i %% 2 == 1) -1L else 1L
    magnitud <- (i + 1L) %/% 2L
    out[i] <- lado * magnitud
  }
  out
}

.espacios_barras_lina <- function(datos_grafico) {
  n <- nrow(datos_grafico)
  if (n == 0) {
    return(NULL)
  }

  espacios <- rep(0.3, n)
  cambios <- which(as.character(datos_grafico$nivel[-1]) != as.character(datos_grafico$nivel[-n])) + 1L
  espacios[cambios] <- 1.4
  espacios
}

.agregar_grupos_barras_lina <- function(mids, datos_grafico) {
  grupos <- split(seq_len(nrow(datos_grafico)), as.character(datos_grafico$nivel))

  usr <- graphics::par("usr")
  y <- usr[3] - (usr[4] - usr[3]) * 0.08

  for (nivel in names(grupos)) {
    idx <- grupos[[nivel]]
    x <- mean(mids[idx])
    graphics::text(
      x = x,
      y = y,
      labels = tools::toTitleCase(nivel),
      xpd = TRUE,
      font = 2,
      cex = 0.9
    )
  }
}
