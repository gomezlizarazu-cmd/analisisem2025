#' Diagnostico integrado de caidas por existencia, Lina y campo
#'
#' Integra los tres criterios de completitud disponibles en el paquete y
#' devuelve tablas comparables por nivel de vivienda, hogar y persona.
#'
#' El criterio de campo se calcula originalmente a nivel `DIRECTORIO`. En las
#' salidas de hogar y persona este criterio se propaga explicitamente por
#' `DIRECTORIO`, de forma intencional, para identificar que unidades quedan
#' afectadas por una misma caida operativa de campo.
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param base_hogar_cap Capitulo base del universo hogar.
#' @param base_persona_cap Capitulo base del universo persona.
#' @param edad_var Variable de edad a usar en los criterios que la requieren.
#' @param criterio_duplicados Criterio de deteccion de duplicados de personas.
#'   Puede ser `"dane_sin_placeholder"`, `"dane_completa"` o `"actual"`.
#'   Por defecto usa `"dane_sin_placeholder"`.
#' @param exportar_excel Si es `TRUE`, exporta un Excel con resumenes y detalle.
#' @param archivo Ruta del archivo Excel.
#'
#' @return Lista con:
#' \describe{
#'   \item{resumen_nivel}{Conteos de caidas por nivel y criterio.}
#'   \item{cruce_vivienda}{Cruce de combinaciones de criterios a nivel vivienda.}
#'   \item{cruce_hogar}{Cruce de combinaciones de criterios a nivel hogar.}
#'   \item{cruce_persona}{Cruce de combinaciones de criterios a nivel persona.}
#'   \item{viviendas_eval}{Base comparada a nivel vivienda.}
#'   \item{hogares_eval}{Base comparada a nivel hogar.}
#'   \item{personas_eval}{Base comparada a nivel persona.}
#'   \item{viviendas_caidas}{Viviendas caidas en al menos uno de los criterios.}
#'   \item{hogares_caidos}{Hogares caidos en al menos uno de los criterios.}
#'   \item{personas_caidas}{Personas caidas en al menos uno de los criterios.}
#'   \item{revision_campo}{Salida operativa a nivel persona con una observacion
#'   resumen, la variable de caida y el valor detectado para facilitar la
#'   revision por campo.}
#'   \item{duplicados_personas_e}{Diagnostico de posibles duplicados en el
#'   capitulo `E` usando nombre, fecha de nacimiento y numero de documento
#'   unificado segun tipo de documento.}
#'   \item{reporte_final_caidas}{Reporte consolidado a nivel persona con los
#'   criterios de existencia, Lina, campo y duplicados, incluyendo personas
#'   duplicadas aunque no caigan en los otros tres criterios.}
#'   \item{diag_existencia}{Salida original de `diagnostico_cruce_capitulos()`.}
#'   \item{diag_lina}{Salida original de `diagnostico_completitud_lina()`.}
#'   \item{diag_campo}{Salida original de `diagnostico_completitud_campo()`.}
#'   \item{archivo}{Ruta normalizada del Excel exportado cuando
#'   `exportar_excel = TRUE`.}
#' }
#' @export
diagnostico_caidas_tres_criterios <- function(
    dfs,
    base_hogar_cap = "C",
    base_persona_cap = "E",
    edad_var = "NPCEP4",
    criterio_duplicados = c("dane_sin_placeholder", "dane_completa", "actual"),
    exportar_excel = FALSE,
    archivo = "caidas_tres_criterios.xlsx"
) {

  criterio_duplicados <- match.arg(criterio_duplicados)

  if (exportar_excel && !requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  diag_existencia <- diagnostico_cruce_capitulos(
    dfs = dfs,
    base_hogar_cap = base_hogar_cap,
    base_persona_cap = base_persona_cap,
    edad_var = edad_var,
    exportar_excel = FALSE
  )

  diag_lina <- diagnostico_completitud_lina(
    dfs = dfs,
    incluir_existencia = FALSE,
    exportar_excel = FALSE,
    base_hogar_cap = base_hogar_cap,
    base_cap_persona = base_persona_cap,
    edad_var = edad_var
  )

  diag_campo <- diagnostico_completitud_campo(
    dfs = dfs
  )

  campo_hog_conteo <- .preparar_causal_conteo_personas_hogar(
    dfs = dfs,
    cap_hog = "C",
    cap_per = "E"
  )

  viv_base <- diag_lina$viviendas_eval %>%
    dplyr::select(
      .data$DIRECTORIO,
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE"))
    ) %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)

  hog_base <- diag_lina$hogares_eval %>%
    dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
    dplyr::left_join(viv_base, by = "DIRECTORIO")

  per_base <- diag_lina$personas_eval %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of("edad")
    ) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
    dplyr::left_join(viv_base, by = "DIRECTORIO")

  existencia_viv <- diag_existencia$resumen_caidas_regla2 %>%
    dplyr::filter(.data$nivel == "vivienda") %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      cae_existencia = TRUE,
      razon_existencia = .data$capitulos_caida,
      fuente_existencia = .data$fuentes_caida,
      n_eventos_existencia = .data$n_eventos
    )

  existencia_hog <- diag_existencia$resumen_caidas_regla2 %>%
    dplyr::filter(.data$nivel == "hogar") %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      cae_existencia = TRUE,
      razon_existencia = .data$capitulos_caida,
      fuente_existencia = .data$fuentes_caida,
      n_eventos_existencia = .data$n_eventos
    )

  existencia_per <- diag_existencia$resumen_caidas_regla2 %>%
    dplyr::filter(.data$nivel == "persona") %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      cae_existencia = TRUE,
      razon_existencia = .data$capitulos_caida,
      fuente_existencia = .data$fuentes_caida,
      n_eventos_existencia = .data$n_eventos
    )

  lina_viv <- diag_lina$viviendas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      cae_lina = !dplyr::coalesce(.data$vivienda_completa_lina, TRUE),
      razon_lina = dplyr::if_else(
        !dplyr::coalesce(.data$vivienda_completa_lina, TRUE),
        .data$motivo_vivienda_lina,
        NA_character_
      )
    )

  lina_hog <- diag_lina$hogares_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      cae_lina = !dplyr::coalesce(.data$hogar_completo_lina, TRUE),
      razon_lina = dplyr::if_else(
        !dplyr::coalesce(.data$hogar_completo_lina, TRUE),
        .data$motivo_hogar_lina,
        NA_character_
      )
    )

  lina_per <- diag_lina$personas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      cae_lina = !dplyr::coalesce(.data$persona_completa_lina, TRUE),
      razon_lina = dplyr::if_else(
        !dplyr::coalesce(.data$persona_completa_lina, TRUE),
        .data$motivo_persona_lina,
        NA_character_
      )
    )

  campo_viv_base <- .preparar_caidas_campo_tres_criterios(diag_campo$base_eval) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      cae_campo_base = .data$cae_campo,
      motivo_principal_campo_base = .data$motivo_principal_campo,
      motivo_detallado_campo_base = .data$motivo_detallado_campo,
      criterio_falla_campo_base = .data$criterio_falla_campo,
      nivel_fuente_campo_base = "vivienda"
    )

  campo_viv_conteo <- campo_hog_conteo %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      cae_campo_nhccpctrl2 = any(.data$cae_campo_nhccpctrl2, na.rm = TRUE),
      n_hogares_desajuste_personas = sum(.data$cae_campo_nhccpctrl2, na.rm = TRUE),
      hogares_desajuste_personas = .collapse_unique_tokens(.data$hogares_desajuste_personas),
      criterio_falla_campo_nhccpctrl2 = .collapse_unique_tokens(.data$criterio_falla_campo_nhccpctrl2),
      .groups = "drop"
    )

  campo_hog <- campo_hog_conteo

  campo_per <- campo_hog_conteo %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$NHCCPCTRL2,
      .data$n_personas_cap_e,
      .data$diferencia_personas_hogar,
      .data$cae_campo_nhccpctrl2,
      .data$motivo_detallado_campo_nhccpctrl2,
      .data$criterio_falla_campo_nhccpctrl2
    )

  viviendas_eval <- viv_base %>%
    dplyr::left_join(existencia_viv, by = "DIRECTORIO") %>%
    dplyr::left_join(lina_viv, by = "DIRECTORIO") %>%
    dplyr::left_join(campo_viv_base, by = "DIRECTORIO") %>%
    dplyr::left_join(campo_viv_conteo, by = "DIRECTORIO") %>%
    .finalizar_cruce_tres_criterios()

  hogares_eval <- hog_base %>%
    dplyr::left_join(existencia_hog, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(lina_hog, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(campo_viv_base, by = "DIRECTORIO") %>%
    dplyr::left_join(campo_hog, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    .finalizar_cruce_tres_criterios()

  personas_eval <- per_base %>%
    dplyr::left_join(existencia_per, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::left_join(lina_per, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::left_join(campo_viv_base, by = "DIRECTORIO") %>%
    dplyr::left_join(campo_per, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    .finalizar_cruce_tres_criterios()

  viviendas_caidas <- viviendas_eval %>%
    dplyr::filter(.data$n_criterios_caida > 0) %>%
    dplyr::arrange(dplyr::desc(.data$n_criterios_caida), .data$DIRECTORIO)

  hogares_caidos <- hogares_eval %>%
    dplyr::filter(.data$n_criterios_caida > 0) %>%
    dplyr::arrange(
      dplyr::desc(.data$n_criterios_caida),
      .data$DIRECTORIO,
      .data$SECUENCIA_P
    )

  personas_caidas <- personas_eval %>%
    dplyr::filter(.data$n_criterios_caida > 0) %>%
    dplyr::arrange(
      dplyr::desc(.data$n_criterios_caida),
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN
    )

  revision_campo <- .construir_revision_campo_tres_criterios(
    personas_caidas = personas_caidas,
    dfs = dfs,
    diag_existencia = diag_existencia,
    diag_lina = diag_lina
  )

  duplicados_personas_e <- diagnostico_duplicados_personas_e(
    dfs = dfs,
    cap_persona = base_persona_cap,
    criterio_duplicados = criterio_duplicados
  )

  reporte_final_caidas <- .construir_reporte_final_caidas_tres_criterios(
    personas_eval = personas_eval,
    revision_campo = revision_campo,
    duplicados_personas_e = duplicados_personas_e$personas_duplicadas
  )

  resumen_nivel <- dplyr::bind_rows(
    .resumen_tres_criterios(viviendas_eval, "vivienda"),
    .resumen_tres_criterios(hogares_eval, "hogar"),
    .resumen_tres_criterios(personas_eval, "persona")
  )

  cruce_vivienda <- .cruce_tres_criterios(viviendas_eval, "vivienda")
  cruce_hogar <- .cruce_tres_criterios(hogares_eval, "hogar")
  cruce_persona <- .cruce_tres_criterios(personas_eval, "persona")

  salida <- list(
    resumen_nivel = resumen_nivel,
    cruce_vivienda = cruce_vivienda,
    cruce_hogar = cruce_hogar,
    cruce_persona = cruce_persona,
    viviendas_eval = viviendas_eval,
    hogares_eval = hogares_eval,
    personas_eval = personas_eval,
    viviendas_caidas = viviendas_caidas,
    hogares_caidos = hogares_caidos,
    personas_caidas = personas_caidas,
    revision_campo = revision_campo,
    duplicados_personas_e = duplicados_personas_e$personas_duplicadas,
    resumen_duplicados_personas_e = duplicados_personas_e$resumen_duplicados,
    reporte_final_caidas = reporte_final_caidas,
    criterio_duplicados = criterio_duplicados,
    diag_existencia = diag_existencia,
    diag_lina = diag_lina,
    diag_campo = diag_campo
  )

  if (isTRUE(exportar_excel)) {
    salida$archivo <- .exportar_caidas_tres_criterios_excel(
      salida = salida,
      archivo = archivo
    )
  }

  salida
}

.preparar_caidas_campo_tres_criterios <- function(base_eval) {
  if (!"RES_VIV" %in% names(base_eval)) {
    base_eval$RES_VIV <- NA
  }

  base_eval %>%
    dplyr::mutate(
      cae_campo = .data$encuesta_efectiva_campo & !.data$encuesta_completa_campo,
      motivo_principal_campo = dplyr::case_when(
        .data$cae_campo & !.data$viv_ocupada_presente ~ "vivienda",
        .data$cae_campo & !dplyr::coalesce(.data$viv_resultado_completo, FALSE) ~ "vivienda",
        .data$cae_campo & !.data$todos_hogares_completos ~ "hogar",
        .data$cae_campo & !.data$todas_personas_completas ~ "persona",
        TRUE ~ NA_character_
      ),
      motivo_detallado_campo = dplyr::case_when(
        .data$cae_campo & !.data$viv_ocupada_presente & !is.na(.data$estado_viv) ~
          paste0("vivienda_", .data$estado_viv),
        .data$cae_campo & !dplyr::coalesce(.data$viv_resultado_completo, FALSE) ~
          "vivienda_res_viv_no_1",
        .data$cae_campo &
          !.data$todos_hogares_completos &
          .data$hogares_incompletos > 0 &
          .data$personas_incompletas > 0 ~
          "hogar_incompleto_y_personas_incompletas",
        .data$cae_campo &
          !.data$todos_hogares_completos &
          .data$hogares_incompletos > 0 &
          .data$personas_incompletas == 0 ~
          "hogar_incompleto_con_personas_completas",
        .data$cae_campo &
          .data$todos_hogares_completos &
          !.data$todas_personas_completas &
          .data$personas_incompletas > 0 ~
          "persona_incompleta_con_hogar_completo",
        .data$cae_campo & !.data$todos_hogares_completos ~ "hogar_incompleto",
        .data$cae_campo & !.data$todas_personas_completas ~ "persona_incompleta",
        TRUE ~ NA_character_
      ),
      criterio_falla_campo = dplyr::case_when(
        .data$cae_campo & !.data$viv_ocupada_presente ~
          paste0(
            "No cumple vivienda presente. estado_viv=",
            dplyr::coalesce(.data$estado_viv, "NA")
          ),
        .data$cae_campo & !dplyr::coalesce(.data$viv_resultado_completo, FALSE) ~
          paste0(
            "No cumple resultado de vivienda. RES_VIV=",
            dplyr::coalesce(as.character(.data$RES_VIV), "NA")
          ),
        .data$cae_campo & !.data$todos_hogares_completos & !.data$todas_personas_completas ~
          paste0(
            "Falla hogar y persona. hogares_incompletos=",
            dplyr::coalesce(as.character(.data$hogares_incompletos), "NA"),
            "; personas_incompletas=",
            dplyr::coalesce(as.character(.data$personas_incompletas), "NA")
          ),
        .data$cae_campo & !.data$todos_hogares_completos ~
          paste0(
            "Falla hogar. hogares_incompletos=",
            dplyr::coalesce(as.character(.data$hogares_incompletos), "NA"),
            "; todos_hogares_completos=",
            dplyr::coalesce(as.character(.data$todos_hogares_completos), "NA")
          ),
        .data$cae_campo & !.data$todas_personas_completas ~
          paste0(
            "Falla persona. personas_incompletas=",
            dplyr::coalesce(as.character(.data$personas_incompletas), "NA"),
            "; todas_personas_completas=",
            dplyr::coalesce(as.character(.data$todas_personas_completas), "NA")
          ),
        TRUE ~ NA_character_
      )
    )
}

.preparar_causal_conteo_personas_hogar <- function(dfs, cap_hog = "C", cap_per = "E") {
  if (!cap_hog %in% names(dfs)) {
    stop("No existe el capitulo de hogar en `dfs`: ", cap_hog)
  }

  if (!cap_per %in% names(dfs)) {
    stop("No existe el capitulo de personas en `dfs`: ", cap_per)
  }

  C <- normalize_keys(dfs[[cap_hog]], c("DIRECTORIO", "SECUENCIA_P"))
  E <- normalize_keys(dfs[[cap_per]], c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  if (!"NHCCPCTRL2" %in% names(C)) {
    stop("La variable `NHCCPCTRL2` no existe en el capitulo `", cap_hog, "`.")
  }

  hogares_control <- C %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      NHCCPCTRL2 = suppressWarnings(as.numeric(.data$NHCCPCTRL2))
    )

  personas_cap_e <- E %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    dplyr::count(.data$DIRECTORIO, .data$SECUENCIA_P, name = "n_personas_cap_e") %>%
    dplyr::mutate(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P)
    )

  hogares_control %>%
    dplyr::left_join(personas_cap_e, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::mutate(
      n_personas_cap_e = dplyr::coalesce(.data$n_personas_cap_e, 0L),
      diferencia_personas_hogar = .data$NHCCPCTRL2 - .data$n_personas_cap_e,
      cae_campo_nhccpctrl2 =
        !is.na(.data$NHCCPCTRL2) &
        .data$NHCCPCTRL2 != .data$n_personas_cap_e,
      hogares_desajuste_personas = dplyr::if_else(
        .data$cae_campo_nhccpctrl2,
        paste0(.data$SECUENCIA_P),
        NA_character_
      ),
      motivo_detallado_campo_nhccpctrl2 = dplyr::if_else(
        .data$cae_campo_nhccpctrl2,
        "desajuste_nhccpctrl2_vs_cap_e",
        NA_character_
      ),
      criterio_falla_campo_nhccpctrl2 = dplyr::if_else(
        .data$cae_campo_nhccpctrl2,
        paste0(
          "NHCCPCTRL2=",
          dplyr::coalesce(as.character(.data$NHCCPCTRL2), "NA"),
          "; registros_cap_e=",
          dplyr::coalesce(as.character(.data$n_personas_cap_e), "NA")
        ),
        NA_character_
      )
    )
}

.finalizar_cruce_tres_criterios <- function(df) {
  vars_chr <- c(
    "motivo_principal_campo_base",
    "motivo_detallado_campo_base",
    "criterio_falla_campo_base",
    "nivel_fuente_campo_base",
    "motivo_detallado_campo_nhccpctrl2",
    "criterio_falla_campo_nhccpctrl2"
  )

  for (v in vars_chr) {
    if (!v %in% names(df)) {
      df[[v]] <- NA_character_
    }
  }

  vars_log <- c(
    "cae_existencia",
    "cae_lina",
    "cae_campo_base",
    "cae_campo_nhccpctrl2"
  )

  for (v in vars_log) {
    if (!v %in% names(df)) {
      df[[v]] <- FALSE
    }
  }

  df %>%
    dplyr::mutate(
      cae_existencia = dplyr::coalesce(.data$cae_existencia, FALSE),
      cae_lina = dplyr::coalesce(.data$cae_lina, FALSE),
      cae_campo_base = dplyr::coalesce(.data$cae_campo_base, FALSE),
      cae_campo_nhccpctrl2 = dplyr::coalesce(.data$cae_campo_nhccpctrl2, FALSE),
      cae_campo = .data$cae_campo_base | .data$cae_campo_nhccpctrl2,
      motivo_principal_campo = dplyr::case_when(
        .data$cae_campo_base ~ dplyr::coalesce(.data$motivo_principal_campo_base, "campo"),
        .data$cae_campo_nhccpctrl2 ~ "hogar",
        TRUE ~ NA_character_
      ),
      n_criterios_caida =
        as.integer(.data$cae_existencia) +
        as.integer(.data$cae_lina) +
        as.integer(.data$cae_campo)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      motivo_detallado_campo = .combinar_textos_unicos(
        c(.data$motivo_detallado_campo_base, .data$motivo_detallado_campo_nhccpctrl2)
      ),
      criterio_falla_campo = .combinar_textos_unicos(
        c(.data$criterio_falla_campo_base, .data$criterio_falla_campo_nhccpctrl2)
      ),
      nivel_fuente_campo = .combinar_textos_unicos(
        c(
          if (.data$cae_campo_base) .data$nivel_fuente_campo_base else NA_character_,
          if (.data$cae_campo_nhccpctrl2) "hogar" else NA_character_
        )
      ),
      criterios_caida = if (.data$n_criterios_caida == 0) {
        "ninguno"
      } else {
        paste(
          c(
            if (.data$cae_existencia) "existencia" else NULL,
            if (.data$cae_lina) "lina" else NULL,
            if (.data$cae_campo) "campo" else NULL
          ),
          collapse = " | "
        )
      }
    ) %>%
    dplyr::ungroup()
}

.combinar_textos_unicos <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(x))]
  x <- unique(x)

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

.construir_revision_campo_tres_criterios <- function(personas_caidas,
                                                     dfs,
                                                     diag_existencia,
                                                     diag_lina) {
  if (nrow(personas_caidas) == 0) {
    return(personas_caidas %>%
      dplyr::mutate(
        criterio_revision_principal = character(),
        grupo_caida = character(),
        variable_caida = character(),
        valor_detectado = character(),
        observacion_existencia = character(),
        observacion_lina = character(),
        observacion_campo = character(),
        observacion_resumen = character()
      ))
  }

  rev_existencia <- .detalle_revision_existencia_tres_criterios(
    diag_existencia = diag_existencia
  )

  rev_lina <- .detalle_revision_lina_tres_criterios(
    diag_lina = diag_lina
  )

  rev_campo <- .detalle_revision_campo_tres_criterios(
    dfs = dfs
  )

  personas_caidas %>%
    dplyr::left_join(
      rev_existencia,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::left_join(
      rev_lina,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::left_join(
      rev_campo,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::mutate(
      criterio_revision_principal = dplyr::case_when(
        .data$cae_campo ~ "campo",
        .data$cae_lina ~ "lina",
        .data$cae_existencia ~ "existencia",
        TRUE ~ NA_character_
      ),
      grupo_caida = dplyr::case_when(
        .data$criterio_revision_principal == "campo" ~ .data$grupo_campo,
        .data$criterio_revision_principal == "lina" ~ .data$grupo_lina,
        .data$criterio_revision_principal == "existencia" ~ .data$grupo_existencia,
        TRUE ~ NA_character_
      ),
      variable_caida = dplyr::case_when(
        .data$criterio_revision_principal == "campo" ~ .data$variable_campo,
        .data$criterio_revision_principal == "lina" ~ .data$variable_lina,
        .data$criterio_revision_principal == "existencia" ~ .data$variable_existencia,
        TRUE ~ NA_character_
      ),
      valor_detectado = dplyr::case_when(
        .data$criterio_revision_principal == "campo" ~ .data$valor_campo,
        .data$criterio_revision_principal == "lina" ~ .data$valor_lina,
        .data$criterio_revision_principal == "existencia" ~ .data$valor_existencia,
        TRUE ~ NA_character_
      ),
      observacion_resumen = .armar_observacion_revision_tres_criterios(
        observacion_campo = .data$observacion_campo,
        observacion_lina = .data$observacion_lina,
        observacion_existencia = .data$observacion_existencia,
        cae_campo = .data$cae_campo,
        cae_lina = .data$cae_lina,
        cae_existencia = .data$cae_existencia
      )
    ) %>%
    dplyr::arrange(
      .data$criterio_revision_principal,
      .data$grupo_caida,
      .data$variable_caida,
      dplyr::desc(.data$n_criterios_caida),
      .data$SEGMENTO,
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN
    ) %>%
    dplyr::relocate(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of(c("edad", "UUID", "SEGMENTO", "CLASE")),
      .data$criterio_revision_principal,
      .data$grupo_caida,
      .data$variable_caida,
      .data$valor_detectado,
      .data$observacion_resumen,
      .before = .data$cae_existencia
    )
}

.detalle_revision_existencia_tres_criterios <- function(diag_existencia) {
  diag_existencia$resumen_caidas_regla2 %>%
    dplyr::filter(.data$nivel == "persona") %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      grupo_existencia = "Falta de capitulos requeridos",
      variable_existencia = "capitulos_faltantes",
      valor_existencia = .texto_revision_valor(.data$capitulos_caida),
      observacion_existencia = paste0(
        "Faltan capitulos requeridos en la encuesta: ",
        .texto_revision_valor(.data$capitulos_caida),
        "."
      )
    ) %>%
    dplyr::distinct(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .keep_all = TRUE
    )
}

.detalle_revision_lina_tres_criterios <- function(diag_lina) {
  lina_viv <- diag_lina$viviendas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      grupo_lina_viv = dplyr::case_when(
        is.na(.data$NVCAPCTRL1) ~ "Registro de vivienda",
        .data$NVCAPCTRL1 != 1 ~ "Control de vivienda",
        .data$NVCAPCTRL2 != 1 ~ "Presencia en vivienda",
        !.valor_no_vacio(.data$NVCBP1) ~ "Variable requerida de vivienda",
        !.valor_no_vacio(.data$NVCBP16) ~ "Variable requerida de vivienda",
        TRUE ~ NA_character_
      ),
      variable_lina_viv = dplyr::case_when(
        is.na(.data$NVCAPCTRL1) ~ "capitulo_A",
        .data$NVCAPCTRL1 != 1 ~ "NVCAPCTRL1",
        .data$NVCAPCTRL2 != 1 ~ "NVCAPCTRL2",
        !.valor_no_vacio(.data$NVCBP1) ~ "NVCBP1",
        !.valor_no_vacio(.data$NVCBP16) ~ "NVCBP16",
        TRUE ~ NA_character_
      ),
      valor_lina_viv = dplyr::case_when(
        is.na(.data$NVCAPCTRL1) ~ "sin registro",
        .data$NVCAPCTRL1 != 1 ~ .texto_revision_valor(.data$NVCAPCTRL1),
        .data$NVCAPCTRL2 != 1 ~ .texto_revision_valor(.data$NVCAPCTRL2),
        !.valor_no_vacio(.data$NVCBP1) ~ .texto_revision_valor(.data$NVCBP1),
        !.valor_no_vacio(.data$NVCBP16) ~ .texto_revision_valor(.data$NVCBP16),
        TRUE ~ NA_character_
      ),
      observacion_lina_viv = dplyr::case_when(
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
    )

  lina_hog <- diag_lina$hogares_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      grupo_lina_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "Registro de hogar",
        .data$NHCCPCTRL1 != 1 ~ "Control de hogar",
        !.valor_no_vacio(.data$NHCCP1) ~ "Variable requerida de hogar",
        !.valor_no_vacio(.data$NHCMP1A) ~ "Variable requerida de hogar",
        !.valor_no_vacio(.data$NHCMP5A) ~ "Variable requerida de hogar",
        TRUE ~ NA_character_
      ),
      variable_lina_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "capitulo_C",
        .data$NHCCPCTRL1 != 1 ~ "NHCCPCTRL1",
        !.valor_no_vacio(.data$NHCCP1) ~ "NHCCP1",
        !.valor_no_vacio(.data$NHCMP1A) ~ "NHCMP1A",
        !.valor_no_vacio(.data$NHCMP5A) ~ "NHCMP5A",
        TRUE ~ NA_character_
      ),
      valor_lina_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "sin registro",
        .data$NHCCPCTRL1 != 1 ~ .texto_revision_valor(.data$NHCCPCTRL1),
        !.valor_no_vacio(.data$NHCCP1) ~ .texto_revision_valor(.data$NHCCP1),
        !.valor_no_vacio(.data$NHCMP1A) ~ .texto_revision_valor(.data$NHCMP1A),
        !.valor_no_vacio(.data$NHCMP5A) ~ .texto_revision_valor(.data$NHCMP5A),
        TRUE ~ NA_character_
      ),
      observacion_lina_hog = dplyr::case_when(
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
    )

  lina_per <- diag_lina$personas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      grupo_lina_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "Registro de persona",
        .data$NPCEPCTRL1 != 1 ~ "Control de persona",
        !.valor_no_vacio(.data$NPCEP6) ~ "Variable requerida de persona",
        TRUE ~ NA_character_
      ),
      variable_lina_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "capitulo_E",
        .data$NPCEPCTRL1 != 1 ~ "NPCEPCTRL1",
        !.valor_no_vacio(.data$NPCEP6) ~ "NPCEP6",
        TRUE ~ NA_character_
      ),
      valor_lina_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "sin registro",
        .data$NPCEPCTRL1 != 1 ~ .texto_revision_valor(.data$NPCEPCTRL1),
        !.valor_no_vacio(.data$NPCEP6) ~ .texto_revision_valor(.data$NPCEP6),
        TRUE ~ NA_character_
      ),
      observacion_lina_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "Cae por Lina porque no hay registro en el capitulo E.",
        .data$NPCEPCTRL1 != 1 ~ paste0(
          "Cae por Lina en persona: NPCEPCTRL1=",
          .texto_revision_valor(.data$NPCEPCTRL1),
          "."
        ),
        !.valor_no_vacio(.data$NPCEP6) ~ "Cae por Lina en persona: NPCEP6 vacia.",
        TRUE ~ NA_character_
      )
    )

  diag_lina$personas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN)
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(lina_viv, by = "DIRECTORIO") %>%
    dplyr::left_join(lina_hog, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(lina_per, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      grupo_lina = dplyr::coalesce(
        .data$grupo_lina_viv,
        .data$grupo_lina_hog,
        .data$grupo_lina_per
      ),
      variable_lina = dplyr::coalesce(
        .data$variable_lina_viv,
        .data$variable_lina_hog,
        .data$variable_lina_per
      ),
      valor_lina = dplyr::coalesce(
        .data$valor_lina_viv,
        .data$valor_lina_hog,
        .data$valor_lina_per
      ),
      observacion_lina = .combinar_textos_unicos(
        c(
          .data$observacion_lina_viv,
          .data$observacion_lina_hog,
          .data$observacion_lina_per
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$grupo_lina,
      .data$variable_lina,
      .data$valor_lina,
      .data$observacion_lina
    )
}

.detalle_revision_campo_tres_criterios <- function(dfs,
                                                   cap_viv = "A",
                                                   cap_hog = "C",
                                                   cap_per = "E") {
  cap_viv <- toupper(cap_viv)
  cap_hog <- toupper(cap_hog)
  cap_per <- toupper(cap_per)

  A <- normalize_keys(dfs[[cap_viv]], c("DIRECTORIO"))
  C <- normalize_keys(dfs[[cap_hog]], c("DIRECTORIO", "SECUENCIA_P"))
  E <- normalize_keys(dfs[[cap_per]], c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  if (!"NVCAPCTRL2A" %in% names(A)) {
    A$NVCAPCTRL2A <- NA
  }

  if (!"NHCCPCTRL1A" %in% names(C)) {
    C$NHCCPCTRL1A <- NA
  }

  if (!"NPCEPCTRL1A" %in% names(E)) {
    E$NPCEPCTRL1A <- NA
  }

  campo_viv <- A %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      grupo_campo_viv = dplyr::case_when(
        .data$NVCAPCTRL1 != 1 ~ "Control de vivienda",
        .data$NVCAPCTRL2 != 1 ~ "Presencia en vivienda",
        .data$RES_VIV != 1 ~ "Resultado de vivienda",
        TRUE ~ NA_character_
      ),
      variable_campo_viv = dplyr::case_when(
        .data$NVCAPCTRL1 != 1 ~ "NVCAPCTRL1",
        .data$NVCAPCTRL2 != 1 ~ "NVCAPCTRL2",
        .data$RES_VIV != 1 ~ "RES_VIV",
        TRUE ~ NA_character_
      ),
      valor_campo_viv = dplyr::case_when(
        .data$NVCAPCTRL1 != 1 ~ .texto_revision_valor(.data$NVCAPCTRL1),
        .data$NVCAPCTRL2 != 1 ~ .texto_revision_valor(.data$NVCAPCTRL2),
        .data$RES_VIV != 1 ~ .texto_revision_valor(.data$RES_VIV),
        TRUE ~ NA_character_
      ),
      observacion_campo_viv = dplyr::case_when(
        .data$NVCAPCTRL1 != 1 ~ paste0(
          "Cae por campo en vivienda: NVCAPCTRL1=",
          .texto_revision_valor(.data$NVCAPCTRL1),
          "."
        ),
        .data$NVCAPCTRL2 != 1 ~ paste0(
          "Cae por campo en vivienda: NVCAPCTRL2=",
          .texto_revision_valor(.data$NVCAPCTRL2),
          dplyr::if_else(
            !is.na(.data$NVCAPCTRL2A),
            paste0(" y NVCAPCTRL2A=", .texto_revision_valor(.data$NVCAPCTRL2A)),
            ""
          ),
          "."
        ),
        .data$RES_VIV != 1 ~ paste0(
          "Cae por campo en vivienda: RES_VIV=",
          .texto_revision_valor(.data$RES_VIV),
          "."
        ),
        TRUE ~ NA_character_
      )
    )

  campo_hog <- C %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
    dplyr::mutate(
      cae_hogar_campo = !(.data$NHCCPCTRL1 == 1 & .data$RES_HOG == 1)
    ) %>%
    dplyr::filter(.data$cae_hogar_campo) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      grupo_campo_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "Registro de hogar",
        .data$NHCCPCTRL1 != 1 ~ "Control de hogar",
        .data$RES_HOG != 1 ~ "Resultado de hogar",
        TRUE ~ "Revision de hogar"
      ),
      variable_campo_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "capitulo_C",
        .data$NHCCPCTRL1 != 1 ~ "NHCCPCTRL1",
        .data$RES_HOG != 1 ~ "RES_HOG",
        TRUE ~ "hogar"
      ),
      valor_campo_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "sin registro",
        .data$NHCCPCTRL1 != 1 ~ .texto_revision_valor(.data$NHCCPCTRL1),
        .data$RES_HOG != 1 ~ .texto_revision_valor(.data$RES_HOG),
        TRUE ~ NA_character_
      ),
      observacion_campo_hog = dplyr::case_when(
        is.na(.data$NHCCPCTRL1) ~ "Cae por campo porque no hay registro del hogar en C.",
        .data$NHCCPCTRL1 != 1 ~ paste0(
          "Cae por campo en hogar: NHCCPCTRL1=",
          .texto_revision_valor(.data$NHCCPCTRL1),
          dplyr::if_else(
            !is.na(.data$NHCCPCTRL1A),
            paste0(" y NHCCPCTRL1A=", .texto_revision_valor(.data$NHCCPCTRL1A)),
            ""
          ),
          "."
        ),
        .data$RES_HOG != 1 ~ paste0(
          "Cae por campo en hogar: RES_HOG=",
          .texto_revision_valor(.data$RES_HOG),
          "."
        ),
        TRUE ~ NA_character_
      )
    )

  campo_per <- E %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
    dplyr::mutate(
      cae_persona_campo = !(.data$NPCEPCTRL1 == 1 & .data$RES_PER == 1)
    ) %>%
    dplyr::filter(.data$cae_persona_campo) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      grupo_campo_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "Registro de persona",
        .data$NPCEPCTRL1 != 1 ~ "Control de persona",
        .data$RES_PER != 1 ~ "Resultado de persona",
        TRUE ~ "Revision de persona"
      ),
      variable_campo_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "capitulo_E",
        .data$NPCEPCTRL1 != 1 ~ "NPCEPCTRL1",
        .data$RES_PER != 1 ~ "RES_PER",
        TRUE ~ "persona"
      ),
      valor_campo_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "sin registro",
        .data$NPCEPCTRL1 != 1 ~ .texto_revision_valor(.data$NPCEPCTRL1),
        .data$RES_PER != 1 ~ .texto_revision_valor(.data$RES_PER),
        TRUE ~ NA_character_
      ),
      observacion_campo_per = dplyr::case_when(
        is.na(.data$NPCEPCTRL1) ~ "Cae por campo porque no hay registro de la persona en E.",
        .data$NPCEPCTRL1 != 1 ~ paste0(
          "Cae por campo en persona: NPCEPCTRL1=",
          .texto_revision_valor(.data$NPCEPCTRL1),
          dplyr::if_else(
            !is.na(.data$NPCEPCTRL1A),
            paste0(" y NPCEPCTRL1A=", .texto_revision_valor(.data$NPCEPCTRL1A)),
            ""
          ),
          "."
        ),
        .data$RES_PER != 1 ~ paste0(
          "Cae por campo en persona: RES_PER=",
          .texto_revision_valor(.data$RES_PER),
          "."
        ),
        TRUE ~ NA_character_
      )
    )

  campo_nhccpctrl2 <- .preparar_causal_conteo_personas_hogar(
    dfs = dfs,
    cap_hog = cap_hog,
    cap_per = cap_per
  ) %>%
    dplyr::filter(.data$cae_campo_nhccpctrl2) %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      grupo_campo_nhccpctrl2 = "Conteo de personas del hogar",
      variable_campo_nhccpctrl2 = "NHCCPCTRL2",
      valor_campo_nhccpctrl2 = paste0(
        .texto_revision_valor(.data$NHCCPCTRL2),
        " vs cap_E=",
        .texto_revision_valor(.data$n_personas_cap_e)
      ),
      observacion_campo_nhccpctrl2 = paste0(
        "Cae por desajuste de conteo del hogar: NHCCPCTRL2=",
        .texto_revision_valor(.data$NHCCPCTRL2),
        " y registros en E=",
        .texto_revision_valor(.data$n_personas_cap_e),
        "."
      )
    )

  E %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN)
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(campo_viv, by = "DIRECTORIO") %>%
    dplyr::left_join(campo_hog, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(campo_per, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::left_join(campo_nhccpctrl2, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      grupo_campo = dplyr::coalesce(
        .data$grupo_campo_viv,
        .data$grupo_campo_hog,
        .data$grupo_campo_per,
        .data$grupo_campo_nhccpctrl2
      ),
      variable_campo = dplyr::coalesce(
        .data$variable_campo_viv,
        .data$variable_campo_hog,
        .data$variable_campo_per,
        .data$variable_campo_nhccpctrl2
      ),
      valor_campo = dplyr::coalesce(
        .data$valor_campo_viv,
        .data$valor_campo_hog,
        .data$valor_campo_per,
        .data$valor_campo_nhccpctrl2
      ),
      observacion_campo = .combinar_textos_unicos(
        c(
          .data$observacion_campo_viv,
          .data$observacion_campo_hog,
          .data$observacion_campo_per,
          .data$observacion_campo_nhccpctrl2
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$grupo_campo,
      .data$variable_campo,
      .data$valor_campo,
      .data$observacion_campo
    )
}

.texto_revision_valor <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[is.na(x_chr) | x_chr == ""] <- "vacia"
  x_chr
}

.armar_observacion_revision_tres_criterios <- function(observacion_campo,
                                                       observacion_lina,
                                                       observacion_existencia,
                                                       cae_campo,
                                                       cae_lina,
                                                       cae_existencia) {
  mapply(
    FUN = function(obs_campo_i, obs_lina_i, obs_exist_i, cae_campo_i, cae_lina_i, cae_exist_i) {
      partes <- character()

      if (isTRUE(cae_campo_i) && !is.na(obs_campo_i) && nzchar(obs_campo_i)) {
        partes <- c(partes, obs_campo_i)
      }

      if (isTRUE(cae_lina_i) && !is.na(obs_lina_i) && nzchar(obs_lina_i)) {
        partes <- c(partes, obs_lina_i)
      }

      if (isTRUE(cae_exist_i) && !is.na(obs_exist_i) && nzchar(obs_exist_i)) {
        partes <- c(partes, obs_exist_i)
      }

      if (length(partes) == 0) {
        return(NA_character_)
      }

      paste(partes, collapse = " Ademas, ")
    },
    obs_campo_i = observacion_campo,
    obs_lina_i = observacion_lina,
    obs_exist_i = observacion_existencia,
    cae_campo_i = cae_campo,
    cae_lina_i = cae_lina,
    cae_exist_i = cae_existencia,
    USE.NAMES = FALSE
  )
}

#' Diagnosticar posibles duplicados de personas en el capitulo E
#'
#' Identifica posibles registros repetidos en el capitulo de personas usando
#' uno de tres criterios parametrizables: la regla historica del paquete
#' (`"actual"`), la regla documental de DANE (`"dane_completa"`) y la misma
#' regla documental excluyendo placeholders de nombre y documento
#' (`"dane_sin_placeholder"`).
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param cap_persona Capitulo de personas a evaluar. Por defecto `"E"`.
#' @param criterio_duplicados Criterio de deteccion a usar. Puede ser
#'   `"dane_sin_placeholder"`, `"dane_completa"` o `"actual"`. Por defecto usa
#'   `"dane_sin_placeholder"`.
#'
#' @return Lista con:
#' \describe{
#'   \item{resumen_duplicados}{Resumen de cobertura y posibles duplicados.}
#'   \item{personas_duplicadas}{Detalle micro de personas en grupos duplicados.}
#' }
#' @export
diagnostico_duplicados_personas_e <- function(
    dfs,
    cap_persona = "E",
    criterio_duplicados = c("dane_sin_placeholder", "dane_completa", "actual")
) {
  criterio_duplicados <- match.arg(criterio_duplicados)

  .detectar_duplicados_personas_e(
    dfs = dfs,
    cap_persona = cap_persona,
    criterio_duplicados = criterio_duplicados
  )
}

.detectar_duplicados_personas_e <- function(
    dfs,
    cap_persona = "E",
    criterio_duplicados = c("dane_sin_placeholder", "dane_completa", "actual")
) {
  criterio_duplicados <- match.arg(criterio_duplicados)

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres de capitulos.")
  }

  names(dfs) <- toupper(names(dfs))
  cap_persona <- toupper(cap_persona)

  if (!cap_persona %in% names(dfs)) {
    stop("No existe el capitulo de personas en `dfs`: ", cap_persona)
  }

  E <- normalize_keys(
    dfs[[cap_persona]],
    c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  )

  vars_req <- c(
    "DIRECTORIO", "SECUENCIA_P", "ORDEN",
    "NPCEP2", "NPCEP3A", "NPCEP_5",
    "NPCEP_5A", "NPCEP_5B", "NPCEP_5C", "NPCEP_5D"
  )

  faltantes <- setdiff(vars_req, names(E))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables requeridas en `", cap_persona, "` para duplicados: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (!"UUID" %in% names(E)) {
    E$UUID <- NA_character_
  }

  meta_vars <- intersect(c("UUID", "SEGMENTO", "CLASE"), names(E))

  personas_base <- E %>%
    dplyr::distinct(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .keep_all = TRUE
    ) %>%
    dplyr::mutate(
      nombre_normalizado = .normalizar_texto_duplicado(.data$NPCEP2),
      fecha_nac_normalizada = .normalizar_texto_duplicado(.data$NPCEP3A),
      tipo_documento_normalizado = .normalizar_texto_duplicado(.data$NPCEP_5),
      numero_documento_npcep_5a = .normalizar_texto_duplicado(.data$NPCEP_5A),
      numero_documento_npcep_5b = .normalizar_texto_duplicado(.data$NPCEP_5B),
      numero_documento_npcep_5c = .normalizar_texto_duplicado(.data$NPCEP_5C),
      numero_documento_npcep_5d = .normalizar_texto_duplicado(.data$NPCEP_5D),
      numero_documento_normalizado = dplyr::case_when(
        .data$tipo_documento_normalizado == "1" ~ .data$numero_documento_npcep_5a,
        .data$tipo_documento_normalizado == "2" ~ .data$numero_documento_npcep_5b,
        .data$tipo_documento_normalizado == "3" ~ .data$numero_documento_npcep_5c,
        .data$tipo_documento_normalizado == "4" ~ .data$numero_documento_npcep_5d,
        TRUE ~ NA_character_
      ),
      numero_documento_unificado = .data$numero_documento_normalizado,
      tipo_documento = .data$tipo_documento_normalizado,
      uuid_valido = !is.na(.data$UUID) &
        nzchar(trimws(as.character(.data$UUID)))
    )

  if (identical(criterio_duplicados, "actual")) {
    personas_eval <- personas_base %>%
      dplyr::mutate(
        identificador_completo =
          !is.na(.data$nombre_normalizado) &
          !is.na(.data$fecha_nac_normalizada) &
          !is.na(.data$numero_documento_normalizado)
      )

    grupos_duplicados <- personas_eval %>%
      dplyr::filter(.data$identificador_completo) %>%
      dplyr::group_by(
        .data$nombre_normalizado,
        .data$fecha_nac_normalizada,
        .data$numero_documento_normalizado
      ) %>%
      dplyr::summarise(
        n_en_grupo_duplicado = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(.data$n_en_grupo_duplicado > 1) %>%
      dplyr::arrange(
        dplyr::desc(.data$n_en_grupo_duplicado),
        .data$nombre_normalizado,
        .data$fecha_nac_normalizada,
        .data$numero_documento_normalizado
      ) %>%
      dplyr::mutate(
        grupo_duplicado = paste0("dup_", dplyr::row_number()),
        clave_duplicado = paste(
          .data$nombre_normalizado,
          .data$fecha_nac_normalizada,
          .data$numero_documento_normalizado,
          sep = "||"
        ),
        criterio_duplicados = criterio_duplicados
      )

    personas_duplicadas <- personas_eval %>%
      dplyr::inner_join(
        grupos_duplicados,
        by = c(
          "nombre_normalizado",
          "fecha_nac_normalizada",
          "numero_documento_normalizado"
        )
      ) %>%
      dplyr::mutate(
        n_repetidos = .data$n_en_grupo_duplicado,
        observacion_duplicado = paste0(
          "Posible duplicado en ", cap_persona,
          ": mismo nombre, fecha de nacimiento y documento. ",
          "NPCEP2=", .texto_revision_valor(.data$NPCEP2),
          "; NPCEP3A=", .texto_revision_valor(.data$NPCEP3A),
          "; NPCEP_5=", .texto_revision_valor(.data$NPCEP_5),
          "; documento=", .texto_revision_valor(.data$numero_documento_normalizado),
          "; apariciones=", .data$n_en_grupo_duplicado,
          "."
        )
      ) %>%
      dplyr::arrange(
        dplyr::desc(.data$n_en_grupo_duplicado),
        .data$grupo_duplicado,
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN
      ) %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN,
        dplyr::any_of(meta_vars),
        .data$NPCEP2,
        .data$NPCEP3A,
        .data$NPCEP_5,
        .data$criterio_duplicados,
        .data$clave_duplicado,
        .data$n_en_grupo_duplicado,
        .data$nombre_normalizado,
        .data$tipo_documento_normalizado,
        .data$fecha_nac_normalizada,
        .data$numero_documento_normalizado,
        .data$numero_documento_unificado,
        .data$grupo_duplicado,
        .data$n_repetidos,
        .data$observacion_duplicado
      )

    personas_con_identificador_completo <- personas_eval %>%
      dplyr::filter(.data$identificador_completo) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
      nrow()
  } else {
    personas_eval <- personas_base %>%
      dplyr::mutate(
        identificador_completo =
          .data$uuid_valido &
          !is.na(.data$nombre_normalizado) &
          !is.na(.data$tipo_documento_normalizado)
      )

    docs_apilados <- .apilar_documentos_duplicados_personas(
      personas_base = personas_base,
      meta_vars = meta_vars
    ) %>%
      dplyr::filter(
        .data$uuid_valido,
        !is.na(.data$nombre_normalizado),
        !is.na(.data$tipo_documento_normalizado),
        !is.na(.data$numero_documento_normalizado)
      )

    if (identical(criterio_duplicados, "dane_sin_placeholder")) {
      docs_apilados <- docs_apilados %>%
        dplyr::filter(
          !.es_nombre_generico_duplicado(.data$nombre_normalizado),
          !.es_documento_placeholder_duplicado(.data$numero_documento_normalizado)
        )
    }

    docs_apilados <- docs_apilados %>%
      dplyr::mutate(
        clave_duplicado = paste(
          .data$nombre_normalizado,
          .data$tipo_documento_normalizado,
          .data$numero_documento_normalizado,
          sep = "||"
        ),
        criterio_duplicados = criterio_duplicados
      )

    grupos_duplicados <- docs_apilados %>%
      dplyr::group_by(
        .data$nombre_normalizado,
        .data$tipo_documento_normalizado,
        .data$numero_documento_normalizado,
        .data$clave_duplicado,
        .data$criterio_duplicados
      ) %>%
      dplyr::summarise(
        n_en_grupo_duplicado = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(.data$n_en_grupo_duplicado > 1) %>%
      dplyr::arrange(
        dplyr::desc(.data$n_en_grupo_duplicado),
        .data$nombre_normalizado,
        .data$tipo_documento_normalizado,
        .data$numero_documento_normalizado
      ) %>%
      dplyr::mutate(grupo_duplicado = paste0("dup_", dplyr::row_number()))

    personas_duplicadas <- docs_apilados %>%
      dplyr::inner_join(
        grupos_duplicados,
        by = c(
          "nombre_normalizado",
          "tipo_documento_normalizado",
          "numero_documento_normalizado",
          "clave_duplicado",
          "criterio_duplicados"
        )
      ) %>%
      dplyr::group_by(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN,
        dplyr::across(dplyr::any_of(meta_vars)),
        .data$NPCEP2,
        .data$NPCEP3A,
        .data$NPCEP_5,
        .data$criterio_duplicados,
        .data$nombre_normalizado,
        .data$tipo_documento_normalizado,
        .data$fecha_nac_normalizada
      ) %>%
      dplyr::summarise(
        clave_duplicado = .collapse_unique_nonempty(.data$clave_duplicado),
        n_en_grupo_duplicado = max(.data$n_en_grupo_duplicado, na.rm = TRUE),
        numero_documento_normalizado = .collapse_unique_nonempty(.data$numero_documento_normalizado),
        numero_documento_unificado = .collapse_unique_nonempty(.data$numero_documento_normalizado),
        grupo_duplicado = .collapse_unique_nonempty(.data$grupo_duplicado),
        fuentes_documento_duplicado = .collapse_unique_nonempty(.data$fuente_documento_duplicado),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        n_repetidos = .data$n_en_grupo_duplicado,
        observacion_duplicado = paste0(
          "Posible duplicado en ", cap_persona,
          ": mismo nombre, tipo de documento y documento. ",
          "NPCEP2=", .texto_revision_valor(.data$NPCEP2),
          "; NPCEP_5=", .texto_revision_valor(.data$NPCEP_5),
          "; documento=", .texto_revision_valor(.data$numero_documento_normalizado),
          "; apariciones=", .data$n_en_grupo_duplicado,
          "."
        )
      ) %>%
      dplyr::arrange(
        dplyr::desc(.data$n_en_grupo_duplicado),
        .data$grupo_duplicado,
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN
      ) %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN,
        dplyr::any_of(meta_vars),
        .data$NPCEP2,
        .data$NPCEP3A,
        .data$NPCEP_5,
        .data$criterio_duplicados,
        .data$clave_duplicado,
        .data$n_en_grupo_duplicado,
        .data$nombre_normalizado,
        .data$tipo_documento_normalizado,
        .data$fecha_nac_normalizada,
        .data$numero_documento_normalizado,
        .data$numero_documento_unificado,
        .data$grupo_duplicado,
        .data$n_repetidos,
        dplyr::any_of("fuentes_documento_duplicado"),
        .data$observacion_duplicado
      )

    personas_con_identificador_completo <- docs_apilados %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
      nrow()
  }

  personas_totales_e <- personas_base %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    nrow()

  grupos_duplicados_n <- grupos_duplicados %>% nrow()
  registros_en_grupos_duplicados <- personas_duplicadas %>% nrow()

  resumen_duplicados <- tibble::tibble(
    criterio_duplicados = criterio_duplicados,
    personas_totales_e = personas_totales_e,
    personas_con_identificador_completo = personas_con_identificador_completo,
    grupos_duplicados = grupos_duplicados_n,
    registros_en_grupos_duplicados = registros_en_grupos_duplicados,
    pct_registros_duplicados = dplyr::if_else(
      personas_totales_e > 0,
      registros_en_grupos_duplicados / personas_totales_e,
      NA_real_
    )
  )

  list(
    resumen_duplicados = resumen_duplicados,
    personas_duplicadas = personas_duplicadas
  )
}

.normalizar_texto_duplicado <- function(x) {
  x <- arreglar_utf8(x)
  x <- toupper(trimws(as.character(x)))
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x[x == "" | is.na(x)] <- NA_character_
  x
}

.catalogo_documentos_placeholder_duplicados <- function() {
  c(
    "99", "999", "9999", "99999", "999999", "9999999",
    "99999999", "999999999", "9999999999",
    "0", "00", "000", "0000", "00000", "000000",
    "0000000", "00000000", "000000000", "0000000000",
    "1111111111", "1234567890"
  )
}

.es_documento_placeholder_duplicado <- function(x) {
  x <- .normalizar_texto_duplicado(x)
  x %in% .catalogo_documentos_placeholder_duplicados() |
    stringr::str_detect(x, "^0+$") |
    stringr::str_detect(x, "^9+$")
}

.es_nombre_generico_duplicado <- function(x) {
  x <- .normalizar_texto_duplicado(x)
  x %in% c("NO INFORMA", "SIN INFORMACION") |
    stringr::str_detect(x, "^NO INFORMA")
}

.apilar_documentos_duplicados_personas <- function(personas_base, meta_vars = character()) {
  if (!is.data.frame(personas_base) || nrow(personas_base) == 0) {
    return(tibble::tibble())
  }

  docs_map <- c(
    NPCEP_5A = "numero_documento_npcep_5a",
    NPCEP_5B = "numero_documento_npcep_5b",
    NPCEP_5C = "numero_documento_npcep_5c",
    NPCEP_5D = "numero_documento_npcep_5d"
  )

  dplyr::bind_rows(lapply(names(docs_map), function(var_origen) {
    col_norm <- docs_map[[var_origen]]

    personas_base %>%
      dplyr::transmute(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$ORDEN,
        dplyr::across(dplyr::any_of(meta_vars)),
        .data$NPCEP2,
        .data$NPCEP3A,
        .data$NPCEP_5,
        .data$uuid_valido,
        .data$nombre_normalizado,
        .data$fecha_nac_normalizada,
        .data$tipo_documento_normalizado,
        numero_documento_normalizado = .data[[col_norm]],
        fuente_documento_duplicado = var_origen
      )
  }))
}

.construir_reporte_final_caidas_tres_criterios <- function(personas_eval,
                                                           revision_campo,
                                                           duplicados_personas_e) {
  ids_persona <- personas_eval %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of(c("edad", "UUID", "SEGMENTO", "CLASE"))
    ) %>%
    dplyr::distinct(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .keep_all = TRUE
    )

  revision_base <- revision_campo %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of(c("edad", "UUID", "SEGMENTO", "CLASE")),
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$n_criterios_caida,
      .data$criterios_caida,
      .data$criterio_revision_principal,
      .data$grupo_caida,
      .data$variable_caida,
      .data$valor_detectado,
      .data$observacion_resumen
    )

  duplicados_base <- duplicados_personas_e %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
      .data$NPCEP2,
      .data$NPCEP3A,
      dplyr::any_of(c(
        "criterio_duplicados",
        "clave_duplicado",
        "n_en_grupo_duplicado",
        "nombre_normalizado",
        "tipo_documento_normalizado",
        "fecha_nac_normalizada",
        "numero_documento_normalizado"
      )),
      .data$numero_documento_unificado,
      .data$observacion_duplicado
    ) %>%
    dplyr::mutate(
      criterio_duplicados = dplyr::coalesce(.data$criterio_duplicados, "actual"),
      cae_duplicado = TRUE,
      razon_duplicado = "Posible duplicado en personas",
      variable_duplicado = dplyr::case_when(
        .data$criterio_duplicados == "actual" ~ "nombre_fecha_documento",
        TRUE ~ "nombre_tipo_documento"
      ),
      valor_duplicado = dplyr::case_when(
        .data$criterio_duplicados == "actual" ~ paste0(
          .texto_revision_valor(.data$NPCEP2),
          " | ",
          .texto_revision_valor(.data$NPCEP3A),
          " | ",
          .texto_revision_valor(.data$numero_documento_unificado)
        ),
        TRUE ~ paste0(
          .texto_revision_valor(.data$NPCEP2),
          " | ",
          .texto_revision_valor(.data$tipo_documento_normalizado),
          " | ",
          .texto_revision_valor(.data$numero_documento_normalizado)
        )
      )
    ) %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      dplyr::any_of(c("UUID", "SEGMENTO", "CLASE")),
      .data$criterio_duplicados,
      dplyr::any_of(c(
        "clave_duplicado",
        "n_en_grupo_duplicado",
        "nombre_normalizado",
        "tipo_documento_normalizado",
        "fecha_nac_normalizada",
        "numero_documento_normalizado"
      )),
      .data$cae_duplicado,
      .data$razon_duplicado,
      .data$variable_duplicado,
      .data$valor_duplicado,
      .data$observacion_duplicado
    )

  ids_union <- dplyr::bind_rows(
    revision_base %>%
      dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN),
    duplicados_base %>%
      dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)
  ) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  out <- ids_union %>%
    dplyr::left_join(ids_persona, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::left_join(
      revision_base,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
      suffix = c("", "_rev")
    ) %>%
    dplyr::left_join(
      duplicados_base,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
      suffix = c("", "_dup")
    )

  cols_na_chr <- c(
    "edad_rev",
    "UUID_rev", "SEGMENTO_rev", "CLASE_rev",
    "UUID_dup", "SEGMENTO_dup", "CLASE_dup",
    "criterio_duplicados",
    "clave_duplicado",
    "nombre_normalizado",
    "tipo_documento_normalizado",
    "fecha_nac_normalizada",
    "numero_documento_normalizado",
    "razon_duplicado", "variable_duplicado", "valor_duplicado",
    "observacion_duplicado", "observacion_resumen",
    "criterios_caida", "criterio_revision_principal",
    "grupo_caida", "variable_caida", "valor_detectado"
  )

  for (v in cols_na_chr) {
    if (!v %in% names(out)) {
      out[[v]] <- NA_character_
    }
  }

  cols_na_log <- c(
    "cae_existencia", "cae_lina", "cae_campo", "cae_duplicado"
  )

  for (v in cols_na_log) {
    if (!v %in% names(out)) {
      out[[v]] <- FALSE
    }
  }

  if (!"n_en_grupo_duplicado" %in% names(out)) {
    out$n_en_grupo_duplicado <- NA_integer_
  }

  out %>%
    dplyr::mutate(
      edad = dplyr::coalesce(.data$edad, .data$edad_rev),
      UUID = dplyr::coalesce(.data$UUID, .data$UUID_rev, .data$UUID_dup),
      SEGMENTO = dplyr::coalesce(.data$SEGMENTO, .data$SEGMENTO_rev, .data$SEGMENTO_dup),
      CLASE = dplyr::coalesce(.data$CLASE, .data$CLASE_rev, .data$CLASE_dup),
      cae_existencia = dplyr::coalesce(.data$cae_existencia, FALSE),
      cae_lina = dplyr::coalesce(.data$cae_lina, FALSE),
      cae_campo = dplyr::coalesce(.data$cae_campo, FALSE),
      cae_duplicado = dplyr::coalesce(.data$cae_duplicado, FALSE),
      criterio_duplicados = dplyr::coalesce(.data$criterio_duplicados, NA_character_),
      clave_duplicado = dplyr::coalesce(.data$clave_duplicado, NA_character_),
      n_en_grupo_duplicado = dplyr::coalesce(.data$n_en_grupo_duplicado, 0L),
      nombre_normalizado = dplyr::coalesce(.data$nombre_normalizado, NA_character_),
      tipo_documento_normalizado = dplyr::coalesce(.data$tipo_documento_normalizado, NA_character_),
      fecha_nac_normalizada = dplyr::coalesce(.data$fecha_nac_normalizada, NA_character_),
      numero_documento_normalizado = dplyr::coalesce(.data$numero_documento_normalizado, NA_character_),
      n_criterios_reporte =
        as.integer(.data$cae_existencia) +
        as.integer(.data$cae_lina) +
        as.integer(.data$cae_campo) +
        as.integer(.data$cae_duplicado),
      criterios_reporte = .armar_criterios_reporte_tres_criterios(
        cae_existencia = .data$cae_existencia,
        cae_lina = .data$cae_lina,
        cae_campo = .data$cae_campo,
        cae_duplicado = .data$cae_duplicado
      ),
      criterio_principal_reporte = dplyr::case_when(
        .data$cae_campo ~ "campo",
        .data$cae_lina ~ "lina",
        .data$cae_existencia ~ "existencia",
        .data$cae_duplicado ~ "duplicados",
        TRUE ~ NA_character_
      ),
      razon_principal_caida = dplyr::case_when(
        .data$criterio_principal_reporte == "campo" ~ .data$grupo_caida,
        .data$criterio_principal_reporte == "lina" ~ .data$grupo_caida,
        .data$criterio_principal_reporte == "existencia" ~ .data$grupo_caida,
        .data$criterio_principal_reporte == "duplicados" ~ .data$razon_duplicado,
        TRUE ~ NA_character_
      ),
      variable_principal_caida = dplyr::case_when(
        .data$criterio_principal_reporte == "campo" ~ .data$variable_caida,
        .data$criterio_principal_reporte == "lina" ~ .data$variable_caida,
        .data$criterio_principal_reporte == "existencia" ~ .data$variable_caida,
        .data$criterio_principal_reporte == "duplicados" ~ .data$variable_duplicado,
        TRUE ~ NA_character_
      ),
      valor_principal_caida = dplyr::case_when(
        .data$criterio_principal_reporte == "campo" ~ .data$valor_detectado,
        .data$criterio_principal_reporte == "lina" ~ .data$valor_detectado,
        .data$criterio_principal_reporte == "existencia" ~ .data$valor_detectado,
        .data$criterio_principal_reporte == "duplicados" ~ .data$valor_duplicado,
        TRUE ~ NA_character_
      ),
      observacion_final = .armar_observacion_final_reporte_tres_criterios(
        observacion_resumen = .data$observacion_resumen,
        observacion_duplicado = .data$observacion_duplicado,
        cae_duplicado = .data$cae_duplicado
      )
    ) %>%
    dplyr::filter(.data$n_criterios_reporte > 0) %>%
    dplyr::arrange(
      dplyr::desc(.data$n_criterios_reporte),
      .data$criterio_principal_reporte,
      .data$SEGMENTO,
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN
    ) %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$edad,
      .data$UUID,
      .data$SEGMENTO,
      .data$CLASE,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$cae_duplicado,
      .data$criterio_duplicados,
      .data$clave_duplicado,
      .data$n_en_grupo_duplicado,
      .data$nombre_normalizado,
      .data$tipo_documento_normalizado,
      .data$fecha_nac_normalizada,
      .data$numero_documento_normalizado,
      .data$n_criterios_reporte,
      .data$criterios_reporte,
      .data$criterio_principal_reporte,
      .data$razon_principal_caida,
      .data$variable_principal_caida,
      .data$valor_principal_caida,
      .data$observacion_final,
      .data$observacion_resumen,
      .data$observacion_duplicado
    )
}

.armar_criterios_reporte_tres_criterios <- function(cae_existencia,
                                                    cae_lina,
                                                    cae_campo,
                                                    cae_duplicado) {
  mapply(
    FUN = function(cae_existencia_i, cae_lina_i, cae_campo_i, cae_duplicado_i) {
      criterios <- c(
        if (isTRUE(cae_existencia_i)) "existencia" else NULL,
        if (isTRUE(cae_lina_i)) "lina" else NULL,
        if (isTRUE(cae_campo_i)) "campo" else NULL,
        if (isTRUE(cae_duplicado_i)) "duplicados" else NULL
      )

      if (length(criterios) == 0) {
        return("ninguno")
      }

      paste(criterios, collapse = " | ")
    },
    cae_existencia_i = cae_existencia,
    cae_lina_i = cae_lina,
    cae_campo_i = cae_campo,
    cae_duplicado_i = cae_duplicado,
    USE.NAMES = FALSE
  )
}

.armar_observacion_final_reporte_tres_criterios <- function(observacion_resumen,
                                                            observacion_duplicado,
                                                            cae_duplicado) {
  mapply(
    FUN = function(observacion_resumen_i, observacion_duplicado_i, cae_duplicado_i) {
      partes <- character()

      if (!is.na(observacion_resumen_i) && nzchar(observacion_resumen_i)) {
        partes <- c(partes, observacion_resumen_i)
      }

      if (isTRUE(cae_duplicado_i) && !is.na(observacion_duplicado_i) && nzchar(observacion_duplicado_i)) {
        partes <- c(partes, observacion_duplicado_i)
      }

      if (length(partes) == 0) {
        return(NA_character_)
      }

      paste(unique(partes), collapse = " Ademas, ")
    },
    observacion_resumen_i = observacion_resumen,
    observacion_duplicado_i = observacion_duplicado,
    cae_duplicado_i = cae_duplicado,
    USE.NAMES = FALSE
  )
}

.resumen_tres_criterios <- function(df, nivel) {
  df %>%
    dplyr::summarise(
      nivel = nivel,
      unidades_totales = dplyr::n(),
      caidas_existencia = sum(.data$cae_existencia, na.rm = TRUE),
      caidas_lina = sum(.data$cae_lina, na.rm = TRUE),
      caidas_campo = sum(.data$cae_campo, na.rm = TRUE),
      caidas_en_al_menos_un_criterio = sum(.data$n_criterios_caida > 0, na.rm = TRUE),
      caidas_en_los_tres = sum(.data$n_criterios_caida == 3, na.rm = TRUE)
    )
}

.cruce_tres_criterios <- function(df, nivel) {
  df %>%
    dplyr::count(
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      name = "n"
    ) %>%
    dplyr::mutate(
      nivel = nivel,
      tipo_cruce = dplyr::case_when(
        .data$cae_existencia & .data$cae_lina & .data$cae_campo ~ "cae_en_los_tres",
        .data$cae_existencia & .data$cae_lina & !.data$cae_campo ~ "existencia_y_lina",
        .data$cae_existencia & !.data$cae_lina & .data$cae_campo ~ "existencia_y_campo",
        !.data$cae_existencia & .data$cae_lina & .data$cae_campo ~ "lina_y_campo",
        .data$cae_existencia & !.data$cae_lina & !.data$cae_campo ~ "solo_existencia",
        !.data$cae_existencia & .data$cae_lina & !.data$cae_campo ~ "solo_lina",
        !.data$cae_existencia & !.data$cae_lina & .data$cae_campo ~ "solo_campo",
        TRUE ~ "sin_caida"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n), .data$tipo_cruce)
}

.exportar_caidas_tres_criterios_excel <- function(salida, archivo) {
  wb <- openxlsx::createWorkbook()

  hojas <- list(
    resumen_nivel = salida$resumen_nivel,
    cruce_vivienda = salida$cruce_vivienda,
    cruce_hogar = salida$cruce_hogar,
    cruce_persona = salida$cruce_persona,
    viviendas_caidas = salida$viviendas_caidas,
    hogares_caidos = salida$hogares_caidos,
    personas_caidas = salida$personas_caidas,
    revision_campo = salida$revision_campo,
    resumen_duplicados_personas_e = salida$resumen_duplicados_personas_e,
    duplicados_personas_e = salida$duplicados_personas_e,
    reporte_final_caidas = salida$reporte_final_caidas
  )

  for (nm in names(hojas)) {
    x <- hojas[[nm]] %>%
      dplyr::mutate(
        dplyr::across(where(is.factor), as.character),
        dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
      ) %>%
      arreglar_utf8_df()

    openxlsx::addWorksheet(wb, substr(nm, 1, 31))
    openxlsx::writeData(wb, sheet = substr(nm, 1, 31), x = x)
  }

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)

  normalizePath(archivo, winslash = "/", mustWork = FALSE)
}
