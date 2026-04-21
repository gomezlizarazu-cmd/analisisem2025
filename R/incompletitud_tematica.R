#' Diagnosticar incompletitud tematica
#'
#' Evalua faltantes en variables tematicamente criticas y construye un
#' diagnostico jerarquico respetando los niveles de la encuesta:
#' vivienda (`DIRECTORIO`), hogar (`DIRECTORIO + SECUENCIA_P`) y persona
#' (`DIRECTORIO + SECUENCIA_P + ORDEN`).
#'
#' Esta funcion no reemplaza la metodologia oficial implementada en
#' `diagnostico_caidas_tres_criterios()`. En cambio, agrega una capa adicional
#' de evaluacion sobre variables tematicas faltantes, con propagacion
#' descendente y agregacion ascendente segun el nivel natural de la regla.
#'
#' Si una regla tematica es de nivel vivienda, la caida se propaga hacia hogar
#' y persona. Si es de nivel hogar, se propaga hacia persona y se agrega a
#' vivienda al resumir. Si es de nivel persona, afecta directamente a persona y
#' se agrega a hogar y vivienda al resumir.
#'
#' @param dfs Lista nombrada de capitulos.
#' @param reglas_variables Tabla de reglas tematicas. Debe contener como minimo
#'   las columnas `capitulo`, `nivel`, `variable` y `descripcion`. Si es
#'   `NULL`, usa una regla por defecto para `NVCBP11AA`.
#' @param diag_tres Resultado de `diagnostico_caidas_tres_criterios()`. Si es
#'   `NULL`, se calcula internamente.
#' @param ruta_exportacion Ruta opcional para exportar un Excel.
#' @param base_hogar_cap Capitulo base del universo hogar si `diag_tres` es
#'   `NULL`.
#' @param base_persona_cap Capitulo base del universo persona si `diag_tres` es
#'   `NULL`.
#' @param edad_var Variable de edad si `diag_tres` es `NULL`.
#'
#' @return Una lista con:
#' \describe{
#'   \item{detalle_tematica_base}{Detalle de fallas en el nivel natural de cada
#'   regla.}
#'   \item{detalle_tematica_propagado}{Detalle expandido con propagacion
#'   descendente y agregacion ascendente.}
#'   \item{resumen_tematica}{Resumen de afectacion por regla y nivel resultado.}
#'   \item{solo_tematica}{Casos que fallan por tematica pero no por los tres
#'   criterios actuales.}
#'   \item{cruce_con_tres_criterios}{Cruce del detalle tematico propagado con
#'   `diag_tres`.}
#'   \item{diag_tres}{Salida usada de `diagnostico_caidas_tres_criterios()`.}
#'   \item{archivo}{Ruta del Excel exportado, si aplica.}
#' }
#'
#' @examples
#' \dontrun{
#' diag_tematica <- diagnostico_incompletitud_tematica(dfs)
#' diag_tematica$resumen_tematica
#' }
#'
#' @export
diagnostico_incompletitud_tematica <- function(
    dfs,
    reglas_variables = NULL,
    diag_tres = NULL,
    ruta_exportacion = NULL,
    base_hogar_cap = "C",
    base_persona_cap = "E",
    edad_var = "NPCEP4"
) {
  if (is.null(dfs) || !is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de capitulos.")
  }

  if (is.null(diag_tres)) {
    diag_tres <- diagnostico_caidas_tres_criterios(
      dfs = dfs,
      base_hogar_cap = base_hogar_cap,
      base_persona_cap = base_persona_cap,
      edad_var = edad_var,
      exportar_excel = FALSE
    )
  }

  reglas <- .reglas_tematica_default(reglas_variables)

  detalle_base_list <- lapply(seq_len(nrow(reglas)), function(i) {
    .detalle_base_regla_tematica(
      regla = reglas[i, , drop = FALSE],
      dfs = dfs
    )
  })

  detalle_tematica_base <- dplyr::bind_rows(detalle_base_list)

  detalle_propagado_list <- lapply(seq_len(nrow(reglas)), function(i) {
    detalle_i <- detalle_tematica_base %>%
      dplyr::filter(
        .data$capitulo == reglas$capitulo[i],
        .data$nivel_natural == reglas$nivel[i],
        .data$variable_regla == reglas$variable[i]
      )

    .propagar_regla_tematica(
      detalle_base = detalle_i,
      regla = reglas[i, , drop = FALSE],
      diag_tres = diag_tres
    )
  })

  detalle_tematica_propagado <- dplyr::bind_rows(detalle_propagado_list) %>%
    .resumir_detalle_tematica_propagado()

  cruce_con_tres_criterios <- .cruzar_tematica_con_tres_criterios(
    detalle_tematica_propagado = detalle_tematica_propagado,
    diag_tres = diag_tres
  ) %>%
    dplyr::mutate(
      cae_solo_por_incompletitud_tematica =
        .data$falla_incompletitud_tematica & !.data$cae_tres_criterios
    )

  solo_tematica <- cruce_con_tres_criterios %>%
    dplyr::filter(.data$cae_solo_por_incompletitud_tematica)

  resumen_tematica <- cruce_con_tres_criterios %>%
    dplyr::group_by(
      .data$capitulo,
      .data$variable_regla,
      .data$descripcion_regla,
      .data$nivel_origen_regla,
      .data$nivel_resultado
    ) %>%
    dplyr::summarise(
      casos_tematica = dplyr::n(),
      solo_tematica = sum(.data$cae_solo_por_incompletitud_tematica, na.rm = TRUE),
      cae_tres_criterios = sum(.data$cae_tres_criterios, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      .data$nivel_resultado,
      .data$capitulo,
      .data$variable_regla
    )

  salida <- list(
    detalle_tematica_base = detalle_tematica_base,
    detalle_tematica_propagado = detalle_tematica_propagado,
    resumen_tematica = resumen_tematica,
    solo_tematica = solo_tematica,
    cruce_con_tres_criterios = cruce_con_tres_criterios,
    diag_tres = diag_tres
  )

  if (!is.null(ruta_exportacion)) {
    salida$archivo <- exportar_tablas_excel(
      x = list(
        detalle_tematica_base = detalle_tematica_base,
        detalle_tematica_propagado = detalle_tematica_propagado,
        resumen_tematica = resumen_tematica,
        solo_tematica = solo_tematica,
        cruce_con_tres_criterios = cruce_con_tres_criterios
      ),
      ruta = ruta_exportacion
    )
  }

  salida
}

#' Integrar incompletitud tematica con tres criterios y duplicados
#'
#' Extiende el diagnostico oficial de `diagnostico_caidas_tres_criterios()`
#' incorporando un criterio adicional de incompletitud tematica.
#'
#' Esta funcion conserva intacta la metodologia oficial existente y construye un
#' consolidado adicional con cinco criterios:
#' existencia, lina, campo, duplicados e incompletitud tematica.
#'
#' @param dfs Lista nombrada de capitulos.
#' @param reglas_variables Tabla de reglas tematicas. Si es `NULL`, usa la
#'   regla por defecto para `NVCBP11AA`.
#' @param diag_tres Resultado de `diagnostico_caidas_tres_criterios()`. Si es
#'   `NULL`, se calcula internamente.
#' @param ruta_exportacion Ruta opcional para exportar un Excel.
#' @param base_hogar_cap Capitulo base del universo hogar si `diag_tres` es
#'   `NULL`.
#' @param base_persona_cap Capitulo base del universo persona si `diag_tres` es
#'   `NULL`.
#' @param edad_var Variable de edad si `diag_tres` es `NULL`.
#'
#' @return Una lista con:
#' \describe{
#'   \item{viviendas_eval}{Base de vivienda con `cae_tematica`.}
#'   \item{hogares_eval}{Base de hogar con `cae_tematica`.}
#'   \item{personas_eval}{Base de persona con `cae_tematica`.}
#'   \item{reporte_final_caidas}{Consolidado final a nivel persona con los
#'   cinco criterios.}
#'   \item{diag_tres}{Salida original de `diagnostico_caidas_tres_criterios()`.}
#'   \item{diag_tematica}{Salida de `diagnostico_incompletitud_tematica()`.}
#'   \item{archivo}{Ruta del Excel exportado, si aplica.}
#' }
#'
#' @examples
#' \dontrun{
#' diag_ext <- diagnostico_caidas_con_tematica(dfs)
#' diag_ext$reporte_final_caidas
#' }
#'
#' @export
diagnostico_caidas_con_tematica <- function(
    dfs,
    reglas_variables = NULL,
    diag_tres = NULL,
    ruta_exportacion = NULL,
    base_hogar_cap = "C",
    base_persona_cap = "E",
    edad_var = "NPCEP4"
) {
  if (is.null(diag_tres)) {
    diag_tres <- diagnostico_caidas_tres_criterios(
      dfs = dfs,
      base_hogar_cap = base_hogar_cap,
      base_persona_cap = base_persona_cap,
      edad_var = edad_var,
      exportar_excel = FALSE
    )
  }

  diag_tematica <- diagnostico_incompletitud_tematica(
    dfs = dfs,
    reglas_variables = reglas_variables,
    diag_tres = diag_tres,
    ruta_exportacion = NULL,
    base_hogar_cap = base_hogar_cap,
    base_persona_cap = base_persona_cap,
    edad_var = edad_var
  )

  tem_viv <- .resumir_tematica_por_nivel(
    diag_tematica$cruce_con_tres_criterios,
    nivel = "vivienda"
  )
  tem_hog <- .resumir_tematica_por_nivel(
    diag_tematica$cruce_con_tres_criterios,
    nivel = "hogar"
  )
  tem_per <- .resumir_tematica_por_nivel(
    diag_tematica$cruce_con_tres_criterios,
    nivel = "persona"
  )

  viviendas_eval <- diag_tres$viviendas_eval %>%
    dplyr::left_join(tem_viv, by = "DIRECTORIO") %>%
    .extender_eval_con_tematica(nivel = "vivienda")

  hogares_eval <- diag_tres$hogares_eval %>%
    dplyr::left_join(tem_hog, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    .extender_eval_con_tematica(nivel = "hogar")

  personas_eval <- diag_tres$personas_eval %>%
    dplyr::left_join(tem_per, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    .extender_eval_con_tematica(nivel = "persona")

  reporte_final_caidas <- .construir_reporte_final_caidas_con_tematica(
    personas_eval = personas_eval,
    reporte_base = diag_tres$reporte_final_caidas,
    tematica_persona = tem_per
  )

  salida <- list(
    viviendas_eval = viviendas_eval,
    hogares_eval = hogares_eval,
    personas_eval = personas_eval,
    reporte_final_caidas = reporte_final_caidas,
    diag_tres = diag_tres,
    diag_tematica = diag_tematica
  )

  if (!is.null(ruta_exportacion)) {
    salida$archivo <- exportar_tablas_excel(
      x = list(
        viviendas_eval = viviendas_eval,
        hogares_eval = hogares_eval,
        personas_eval = personas_eval,
        reporte_final_caidas = reporte_final_caidas
      ),
      ruta = ruta_exportacion
    )
  }

  salida
}

.reglas_tematica_default <- function(reglas_variables = NULL) {
  if (is.null(reglas_variables)) {
    return(
      tibble::tribble(
        ~capitulo, ~nivel, ~variable, ~descripcion,
        "B", "vivienda", "NVCBP11AA", "Estrato faltante"
      )
    )
  }

  if (!is.data.frame(reglas_variables)) {
    stop("`reglas_variables` debe ser un data.frame o tibble.")
  }

  req <- c("capitulo", "nivel", "variable", "descripcion")
  faltantes <- setdiff(req, names(reglas_variables))

  if (length(faltantes) > 0) {
    stop(
      "Faltan columnas requeridas en `reglas_variables`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  reglas_variables %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      capitulo = toupper(trimws(as.character(.data$capitulo))),
      nivel = tolower(trimws(as.character(.data$nivel))),
      variable = trimws(as.character(.data$variable)),
      descripcion = trimws(as.character(.data$descripcion))
    )
}

.detalle_base_regla_tematica <- function(regla, dfs) {
  cap <- regla$capitulo[[1]]
  nivel <- regla$nivel[[1]]
  variable <- regla$variable[[1]]
  descripcion <- regla$descripcion[[1]]

  if (!cap %in% names(dfs)) {
    stop("No existe el capitulo `", cap, "` en `dfs`.")
  }

  df <- dfs[[cap]]

  keys <- switch(
    nivel,
    vivienda = c("DIRECTORIO"),
    hogar = c("DIRECTORIO", "SECUENCIA_P"),
    persona = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    stop("Nivel tematico no soportado: ", nivel)
  )

  faltantes_cols <- setdiff(c(keys, variable), names(df))
  if (length(faltantes_cols) > 0) {
    stop(
      "Faltan columnas requeridas en `", cap, "` para la regla tematica: ",
      paste(faltantes_cols, collapse = ", ")
    )
  }

  meta_cols <- intersect(c("UUID", "SEGMENTO", "CLASE", "edad"), names(df))

  df_eval <- df %>%
    normalize_keys(c(keys, meta_cols)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE) %>%
    dplyr::mutate(
      DIRECTORIO = dplyr::coalesce(as.character(.data$DIRECTORIO), NA_character_),
      SECUENCIA_P = if ("SECUENCIA_P" %in% names(.)) as.character(.data$SECUENCIA_P) else NA_character_,
      ORDEN = if ("ORDEN" %in% names(.)) as.character(.data$ORDEN) else NA_character_,
      nivel_natural = nivel,
      capitulo = cap,
      variable_regla = variable,
      descripcion_regla = descripcion,
      valor_observado = as.character(.data[[variable]]),
      falla_incompletitud_tematica =
        is.na(.data[[variable]]) |
        trimws(as.character(.data[[variable]])) == ""
    )

  df_eval %>%
    dplyr::filter(.data$falla_incompletitud_tematica) %>%
    dplyr::transmute(
      DIRECTORIO = .data$DIRECTORIO,
      SECUENCIA_P = dplyr::coalesce(.data$SECUENCIA_P, NA_character_),
      ORDEN = dplyr::coalesce(.data$ORDEN, NA_character_),
      dplyr::across(dplyr::any_of(meta_cols)),
      nivel_natural = .data$nivel_natural,
      capitulo = .data$capitulo,
      variable_regla = .data$variable_regla,
      descripcion_regla = .data$descripcion_regla,
      valor_observado = .data$valor_observado,
      falla_incompletitud_tematica = .data$falla_incompletitud_tematica
    )
}

.propagar_regla_tematica <- function(detalle_base, regla, diag_tres) {
  if (nrow(detalle_base) == 0) {
    return(tibble::tibble())
  }

  viv_ids <- diag_tres$viviendas_eval %>%
    dplyr::select(.data$DIRECTORIO) %>%
    dplyr::distinct()

  hog_ids <- diag_tres$hogares_eval %>%
    dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::distinct()

  per_ids <- diag_tres$personas_eval %>%
    dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    dplyr::distinct()

  nivel_origen <- regla$nivel[[1]]

  natural <- detalle_base %>%
    dplyr::transmute(
      DIRECTORIO = .data$DIRECTORIO,
      SECUENCIA_P = dplyr::coalesce(.data$SECUENCIA_P, NA_character_),
      ORDEN = dplyr::coalesce(.data$ORDEN, NA_character_),
      nivel_resultado = nivel_origen,
      nivel_origen_regla = nivel_origen,
      capitulo = .data$capitulo,
      variable_regla = .data$variable_regla,
      descripcion_regla = .data$descripcion_regla,
      valor_observado = .data$valor_observado,
      falla_incompletitud_tematica = TRUE,
      propagado_desde_nivel_superior = FALSE,
      agregado_desde_nivel_inferior = FALSE
    )

  out <- list(natural)

  if (nivel_origen == "vivienda") {
    out[[length(out) + 1]] <- detalle_base %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$capitulo,
        .data$variable_regla,
        .data$descripcion_regla,
        .data$valor_observado
      ) %>%
      dplyr::left_join(hog_ids, by = "DIRECTORIO") %>%
      dplyr::transmute(
        DIRECTORIO = .data$DIRECTORIO,
        SECUENCIA_P = .data$SECUENCIA_P,
        ORDEN = NA_character_,
        nivel_resultado = "hogar",
        nivel_origen_regla = nivel_origen,
        capitulo = .data$capitulo,
        variable_regla = .data$variable_regla,
        descripcion_regla = .data$descripcion_regla,
        valor_observado = .data$valor_observado,
        falla_incompletitud_tematica = TRUE,
        propagado_desde_nivel_superior = TRUE,
        agregado_desde_nivel_inferior = FALSE
      )

    out[[length(out) + 1]] <- detalle_base %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$capitulo,
        .data$variable_regla,
        .data$descripcion_regla,
        .data$valor_observado
      ) %>%
      dplyr::left_join(per_ids, by = "DIRECTORIO") %>%
      dplyr::transmute(
        DIRECTORIO = .data$DIRECTORIO,
        SECUENCIA_P = .data$SECUENCIA_P,
        ORDEN = .data$ORDEN,
        nivel_resultado = "persona",
        nivel_origen_regla = nivel_origen,
        capitulo = .data$capitulo,
        variable_regla = .data$variable_regla,
        descripcion_regla = .data$descripcion_regla,
        valor_observado = .data$valor_observado,
        falla_incompletitud_tematica = TRUE,
        propagado_desde_nivel_superior = TRUE,
        agregado_desde_nivel_inferior = FALSE
      )
  }

  if (nivel_origen == "hogar") {
    out[[length(out) + 1]] <- detalle_base %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$capitulo,
        .data$variable_regla,
        .data$descripcion_regla,
        .data$valor_observado
      ) %>%
      dplyr::left_join(per_ids, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
      dplyr::transmute(
        DIRECTORIO = .data$DIRECTORIO,
        SECUENCIA_P = .data$SECUENCIA_P,
        ORDEN = .data$ORDEN,
        nivel_resultado = "persona",
        nivel_origen_regla = nivel_origen,
        capitulo = .data$capitulo,
        variable_regla = .data$variable_regla,
        descripcion_regla = .data$descripcion_regla,
        valor_observado = .data$valor_observado,
        falla_incompletitud_tematica = TRUE,
        propagado_desde_nivel_superior = TRUE,
        agregado_desde_nivel_inferior = FALSE
      )

    out[[length(out) + 1]] <- detalle_base %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$capitulo,
        .data$variable_regla,
        .data$descripcion_regla,
        .data$valor_observado
      ) %>%
      dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE) %>%
      dplyr::transmute(
        DIRECTORIO = .data$DIRECTORIO,
        SECUENCIA_P = NA_character_,
        ORDEN = NA_character_,
        nivel_resultado = "vivienda",
        nivel_origen_regla = nivel_origen,
        capitulo = .data$capitulo,
        variable_regla = .data$variable_regla,
        descripcion_regla = .data$descripcion_regla,
        valor_observado = .data$valor_observado,
        falla_incompletitud_tematica = TRUE,
        propagado_desde_nivel_superior = FALSE,
        agregado_desde_nivel_inferior = TRUE
      )
  }

  if (nivel_origen == "persona") {
    out[[length(out) + 1]] <- detalle_base %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$SECUENCIA_P,
        .data$capitulo,
        .data$variable_regla,
        .data$descripcion_regla,
        .data$valor_observado
      ) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
      dplyr::transmute(
        DIRECTORIO = .data$DIRECTORIO,
        SECUENCIA_P = .data$SECUENCIA_P,
        ORDEN = NA_character_,
        nivel_resultado = "hogar",
        nivel_origen_regla = nivel_origen,
        capitulo = .data$capitulo,
        variable_regla = .data$variable_regla,
        descripcion_regla = .data$descripcion_regla,
        valor_observado = .data$valor_observado,
        falla_incompletitud_tematica = TRUE,
        propagado_desde_nivel_superior = FALSE,
        agregado_desde_nivel_inferior = TRUE
      )

    out[[length(out) + 1]] <- detalle_base %>%
      dplyr::select(
        .data$DIRECTORIO,
        .data$capitulo,
        .data$variable_regla,
        .data$descripcion_regla,
        .data$valor_observado
      ) %>%
      dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE) %>%
      dplyr::transmute(
        DIRECTORIO = .data$DIRECTORIO,
        SECUENCIA_P = NA_character_,
        ORDEN = NA_character_,
        nivel_resultado = "vivienda",
        nivel_origen_regla = nivel_origen,
        capitulo = .data$capitulo,
        variable_regla = .data$variable_regla,
        descripcion_regla = .data$descripcion_regla,
        valor_observado = .data$valor_observado,
        falla_incompletitud_tematica = TRUE,
        propagado_desde_nivel_superior = FALSE,
        agregado_desde_nivel_inferior = TRUE
      )
  }

  dplyr::bind_rows(out)
}

.resumir_detalle_tematica_propagado <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  df %>%
    dplyr::group_by(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$nivel_resultado,
      .data$nivel_origen_regla,
      .data$capitulo,
      .data$variable_regla,
      .data$descripcion_regla
    ) %>%
    dplyr::summarise(
      valor_observado = .collapse_unique_nonempty(.data$valor_observado),
      falla_incompletitud_tematica = any(.data$falla_incompletitud_tematica, na.rm = TRUE),
      propagado_desde_nivel_superior = any(.data$propagado_desde_nivel_superior, na.rm = TRUE),
      agregado_desde_nivel_inferior = any(.data$agregado_desde_nivel_inferior, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      .data$nivel_resultado,
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$variable_regla
    )
}

.cruzar_tematica_con_tres_criterios <- function(detalle_tematica_propagado, diag_tres) {
  viv_eval <- diag_tres$viviendas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      cae_tres_criterios = .data$n_criterios_caida > 0,
      n_criterios_caida = .data$n_criterios_caida,
      criterios_caida = .data$criterios_caida
    )

  hog_eval <- diag_tres$hogares_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      cae_tres_criterios = .data$n_criterios_caida > 0,
      n_criterios_caida = .data$n_criterios_caida,
      criterios_caida = .data$criterios_caida
    )

  per_eval <- diag_tres$personas_eval %>%
    dplyr::transmute(
      DIRECTORIO = as.character(.data$DIRECTORIO),
      SECUENCIA_P = as.character(.data$SECUENCIA_P),
      ORDEN = as.character(.data$ORDEN),
      cae_tres_criterios = .data$n_criterios_caida > 0,
      n_criterios_caida = .data$n_criterios_caida,
      criterios_caida = .data$criterios_caida
    )

  viv <- detalle_tematica_propagado %>%
    dplyr::filter(.data$nivel_resultado == "vivienda") %>%
    dplyr::left_join(viv_eval, by = "DIRECTORIO")

  hog <- detalle_tematica_propagado %>%
    dplyr::filter(.data$nivel_resultado == "hogar") %>%
    dplyr::left_join(hog_eval, by = c("DIRECTORIO", "SECUENCIA_P"))

  per <- detalle_tematica_propagado %>%
    dplyr::filter(.data$nivel_resultado == "persona") %>%
    dplyr::left_join(per_eval, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  dplyr::bind_rows(viv, hog, per) %>%
    dplyr::mutate(
      cae_tres_criterios = dplyr::coalesce(.data$cae_tres_criterios, FALSE),
      n_criterios_caida = dplyr::coalesce(.data$n_criterios_caida, 0L),
      criterios_caida = dplyr::coalesce(.data$criterios_caida, "ninguno")
    )
}

.resumir_tematica_por_nivel <- function(cruce_tematica, nivel) {
  keys <- switch(
    nivel,
    vivienda = c("DIRECTORIO"),
    hogar = c("DIRECTORIO", "SECUENCIA_P"),
    persona = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    stop("Nivel no soportado: ", nivel)
  )

  cruce_tematica %>%
    dplyr::filter(.data$nivel_resultado == nivel) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      cae_tematica = any(.data$falla_incompletitud_tematica, na.rm = TRUE),
      razon_tematica = .collapse_unique_nonempty(.data$descripcion_regla),
      variable_tematica = .collapse_unique_nonempty(.data$variable_regla),
      valor_tematica = .collapse_unique_nonempty(.data$valor_observado),
      observacion_tematica = .armar_observacion_tematica(
        descripcion = .data$descripcion_regla,
        variable = .data$variable_regla,
        valor = .data$valor_observado
      ),
      .groups = "drop"
    )
}

.armar_observacion_tematica <- function(descripcion, variable, valor) {
  des <- .collapse_unique_nonempty(descripcion)
  var <- .collapse_unique_nonempty(variable)
  val <- .collapse_unique_nonempty(valor)

  paste0(
    "Incompletitud tematica: ",
    dplyr::coalesce(des, "Variable tematica faltante"),
    ". Variable=",
    dplyr::coalesce(var, "NA"),
    "; valor=",
    dplyr::coalesce(val, "vacio"),
    "."
  )
}

.extender_eval_con_tematica <- function(df, nivel) {
  if (!"cae_tematica" %in% names(df)) {
    df$cae_tematica <- FALSE
  }

  if (!"razon_tematica" %in% names(df)) df$razon_tematica <- NA_character_
  if (!"variable_tematica" %in% names(df)) df$variable_tematica <- NA_character_
  if (!"valor_tematica" %in% names(df)) df$valor_tematica <- NA_character_
  if (!"observacion_tematica" %in% names(df)) df$observacion_tematica <- NA_character_

  df %>%
    dplyr::mutate(
      cae_tematica = dplyr::coalesce(.data$cae_tematica, FALSE),
      n_criterios_caida_tres = .data$n_criterios_caida,
      n_criterios_caida = .data$n_criterios_caida + as.integer(.data$cae_tematica),
      criterios_caida = .append_criterio_tematica(
        criterios = .data$criterios_caida,
        cae_tematica = .data$cae_tematica
      )
    )
}

.append_criterio_tematica <- function(criterios, cae_tematica) {
  mapply(
    FUN = function(criterio_i, cae_tematica_i) {
      criterio_i <- trimws(as.character(criterio_i))

      if (!isTRUE(cae_tematica_i)) {
        return(criterio_i)
      }

      if (is.na(criterio_i) || criterio_i == "" || criterio_i == "ninguno") {
        return("tematica")
      }

      if (grepl("(^|\\|)\\s*tematica\\s*(\\||$)", criterio_i)) {
        return(criterio_i)
      }

      paste(criterio_i, "tematica", sep = " | ")
    },
    criterio_i = criterios,
    cae_tematica_i = cae_tematica,
    USE.NAMES = FALSE
  )
}

.construir_reporte_final_caidas_con_tematica <- function(personas_eval,
                                                         reporte_base,
                                                         tematica_persona) {
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

  tematica_base <- tematica_persona %>%
    dplyr::transmute(
      DIRECTORIO = .data$DIRECTORIO,
      SECUENCIA_P = .data$SECUENCIA_P,
      ORDEN = .data$ORDEN,
      cae_tematica = TRUE,
      razon_tematica = .data$razon_tematica,
      variable_tematica = .data$variable_tematica,
      valor_tematica = .data$valor_tematica,
      observacion_tematica = .data$observacion_tematica
    )

  ids_union <- dplyr::bind_rows(
    reporte_base %>%
      dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN),
    tematica_base %>%
      dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)
  ) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  out <- ids_union %>%
    dplyr::left_join(ids_persona, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::left_join(
      reporte_base,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
      suffix = c("", "_base")
    ) %>%
    dplyr::left_join(
      tematica_base,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    )

  if (!"cae_tematica" %in% names(out)) out$cae_tematica <- FALSE
  if (!"razon_tematica" %in% names(out)) out$razon_tematica <- NA_character_
  if (!"variable_tematica" %in% names(out)) out$variable_tematica <- NA_character_
  if (!"valor_tematica" %in% names(out)) out$valor_tematica <- NA_character_
  if (!"observacion_tematica" %in% names(out)) out$observacion_tematica <- NA_character_

  out %>%
    dplyr::mutate(
      cae_existencia = dplyr::coalesce(.data$cae_existencia, FALSE),
      cae_lina = dplyr::coalesce(.data$cae_lina, FALSE),
      cae_campo = dplyr::coalesce(.data$cae_campo, FALSE),
      cae_duplicado = dplyr::coalesce(.data$cae_duplicado, FALSE),
      cae_tematica = dplyr::coalesce(.data$cae_tematica, FALSE),
      n_criterios_reporte =
        as.integer(.data$cae_existencia) +
        as.integer(.data$cae_lina) +
        as.integer(.data$cae_campo) +
        as.integer(.data$cae_duplicado) +
        as.integer(.data$cae_tematica),
      criterios_reporte = .armar_criterios_reporte_con_tematica(
        cae_existencia = .data$cae_existencia,
        cae_lina = .data$cae_lina,
        cae_campo = .data$cae_campo,
        cae_duplicado = .data$cae_duplicado,
        cae_tematica = .data$cae_tematica
      ),
      criterio_principal_reporte = dplyr::case_when(
        .data$cae_campo ~ "campo",
        .data$cae_lina ~ "lina",
        .data$cae_existencia ~ "existencia",
        .data$cae_tematica ~ "tematica",
        .data$cae_duplicado ~ "duplicados",
        TRUE ~ NA_character_
      ),
      razon_principal_caida = dplyr::case_when(
        .data$criterio_principal_reporte == "tematica" ~ .data$razon_tematica,
        TRUE ~ .data$razon_principal_caida
      ),
      variable_principal_caida = dplyr::case_when(
        .data$criterio_principal_reporte == "tematica" ~ .data$variable_tematica,
        TRUE ~ .data$variable_principal_caida
      ),
      valor_principal_caida = dplyr::case_when(
        .data$criterio_principal_reporte == "tematica" ~ .data$valor_tematica,
        TRUE ~ .data$valor_principal_caida
      ),
      observacion_final = .armar_observacion_final_con_tematica(
        observacion_final = .data$observacion_final,
        observacion_tematica = .data$observacion_tematica
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
      .data$cae_tematica,
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

.armar_criterios_reporte_con_tematica <- function(cae_existencia,
                                                  cae_lina,
                                                  cae_campo,
                                                  cae_duplicado,
                                                  cae_tematica) {
  mapply(
    FUN = function(cae_existencia_i,
                   cae_lina_i,
                   cae_campo_i,
                   cae_duplicado_i,
                   cae_tematica_i) {
      criterios <- c(
        if (isTRUE(cae_existencia_i)) "existencia" else NULL,
        if (isTRUE(cae_lina_i)) "lina" else NULL,
        if (isTRUE(cae_campo_i)) "campo" else NULL,
        if (isTRUE(cae_duplicado_i)) "duplicados" else NULL,
        if (isTRUE(cae_tematica_i)) "tematica" else NULL
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
    cae_tematica_i = cae_tematica,
    USE.NAMES = FALSE
  )
}

.armar_observacion_final_con_tematica <- function(observacion_final,
                                                  observacion_tematica) {
  mapply(
    FUN = function(obs_final_i, obs_tematica_i) {
      partes <- c(obs_final_i, obs_tematica_i)
      partes <- partes[!is.na(partes) & nzchar(trimws(partes))]

      if (length(partes) == 0) {
        return(NA_character_)
      }

      paste(unique(partes), collapse = " Ademas, ")
    },
    obs_final_i = observacion_final,
    obs_tematica_i = observacion_tematica,
    USE.NAMES = FALSE
  )
}
