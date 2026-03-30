#' Diagnostico integral de cruce entre capitulos
#'
#' Construye un diagnostico estructural por nivel de vivienda, hogar y persona
#' para identificar presencia, requerimiento y caidas entre capitulos de la
#' encuesta. La funcion resume conteos por capitulo, detecta unidades que no
#' aparecen donde deberian y puede exportar un Excel con resumenes y detalle de
#' las unidades caidas.
#'
#' @details
#' La funcion trabaja con tres universos y respeta las llaves definidas por
#' nivel en el paquete:
#'
#' - vivienda: `DIRECTORIO`
#' - hogar: `DIRECTORIO + SECUENCIA_P`
#' - persona: `DIRECTORIO + SECUENCIA_P + ORDEN`
#'
#' Para evitar cruces ambiguos, los conteos se calculan sobre llaves unicas y
#' los joins se normalizan con `normalize_keys()`.
#'
#' Reglas clave del diagnostico:
#' - vivienda: unidad `DIRECTORIO`
#' - hogar: unidad `DIRECTORIO + SECUENCIA_P`
#' - persona: unidad `DIRECTORIO + SECUENCIA_P + ORDEN`
#' - el capitulo `C` se valida a nivel vivienda como "al menos un hogar"
#' - el capitulo `F` usa todo el universo de personas del capitulo base
#' - `G`, `H`, `I`, `J` y `K` se exigen segun la edad observada en `E`
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param caps_vivienda Capitulos a evaluar a nivel vivienda. Default: `"A"`.
#' @param caps_hogar Capitulos a evaluar a nivel hogar.
#' @param caps_persona Capitulos a evaluar a nivel persona.
#' @param base_hogar_cap Capitulo base para construir el universo de hogares.
#' @param base_persona_cap Capitulo base para construir el universo de personas.
#' @param edad_var Variable de edad en el capitulo base de personas.
#' @param exportar_excel Si es `TRUE`, exporta resultados a Excel.
#' @param archivo Ruta del archivo Excel.
#' @param exportar_sabana_completa Si es `TRUE`, agrega hojas por capitulo con
#'   el detalle bruto de las unidades caidas.
#' @param max_unidades_sabana Tope de unidades por nivel para exportar detalle
#'   completo. Si se supera, la sabana de ese nivel se omite.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{resumen_capitulos}{Resumen por capitulo con conteos de viviendas,
#'   hogares, personas y promedios estructurales.}
#'   \item{resumen_vivienda}{Resumen de presencia y faltantes a nivel vivienda.}
#'   \item{resumen_hogar}{Resumen de presencia, requerimiento y faltantes a
#'   nivel hogar.}
#'   \item{resumen_persona}{Resumen de presencia, requerimiento y faltantes a
#'   nivel persona.}
#'   \item{viviendas_eval}{Base micro a nivel vivienda con indicadores de
#'   presencia por capitulo.}
#'   \item{hogares_eval}{Base micro a nivel hogar con indicadores `pres_*`,
#'   `req_*`, `ok_*` y capitulos faltantes.}
#'   \item{personas_eval}{Base micro a nivel persona con indicadores `pres_*`,
#'   `req_*`, `ok_*` y capitulos faltantes.}
#'   \item{viviendas_caidas}{Viviendas que no aparecen en todos los capitulos
#'   evaluados.}
#'   \item{hogares_caidos}{Hogares con al menos un capitulo requerido faltante.}
#'   \item{personas_caidas}{Personas con al menos un capitulo requerido faltante.}
#'   \item{detalle_capitulos}{Lista opcional con tablas brutas filtradas por
#'   unidades caidas, una por capitulo y nivel.}
#'   \item{archivo}{Ruta del Excel exportado cuando `exportar_excel = TRUE`.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- diagnostico_cruce_capitulos(dfs)
#'
#' res$resumen_capitulos
#' head(res$hogares_caidos)
#'
#' res_xlsx <- diagnostico_cruce_capitulos(
#'   dfs = dfs,
#'   exportar_excel = TRUE,
#'   exportar_sabana_completa = TRUE
#' )
#' }
#'
#' @export
diagnostico_cruce_capitulos <- function(
    dfs,
    caps_vivienda = c("A"),
    caps_hogar = c("B", "C", "D", "L", "M", "MA", "MB"),
    caps_persona = c("E", "F", "G", "H", "I", "J", "K"),
    base_hogar_cap = "B",
    base_persona_cap = "E",
    edad_var = "NPCEP4",
    exportar_excel = FALSE,
    archivo = "diagnostico_cruce_capitulos.xlsx",
    exportar_sabana_completa = FALSE,
    max_unidades_sabana = 5000L
) {

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres de capitulos.")
  }

  names(dfs) <- toupper(names(dfs))
  caps_vivienda <- intersect(toupper(caps_vivienda), names(dfs))
  caps_hogar <- intersect(toupper(caps_hogar), names(dfs))
  caps_persona <- intersect(toupper(caps_persona), names(dfs))
  base_hogar_cap <- toupper(base_hogar_cap)
  base_persona_cap <- toupper(base_persona_cap)

  if (length(c(caps_vivienda, caps_hogar, caps_persona)) == 0) {
    stop("No hay capitulos validos para evaluar en `dfs`.")
  }

  if (!base_persona_cap %in% names(dfs)) {
    stop("`base_persona_cap` no existe en `dfs`.")
  }

  if (!edad_var %in% names(dfs[[base_persona_cap]])) {
    stop("La variable de edad no existe en `base_persona_cap`: ", edad_var)
  }

  if (exportar_excel && !requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  caps_all <- unique(c(caps_vivienda, caps_hogar, caps_persona))

  resumen_capitulos <- dplyr::bind_rows(lapply(caps_all, function(cap) {
    df_cap <- dfs[[cap]]
    keys_cap <- get_join_keys(cap)
    df_cap <- normalize_keys(df_cap, keys_cap)

    hogares_cap <- intersect(c("DIRECTORIO", "SECUENCIA_P"), names(df_cap))
    personas_cap <- intersect(c("DIRECTORIO", "SECUENCIA_P", "ORDEN"), names(df_cap))

    tibble::tibble(
      capitulo = cap,
      nivel_capitulo = tipo_capitulo[[cap]],
      n_viviendas = .n_keys(df_cap, "DIRECTORIO"),
      n_hogares = .n_keys(df_cap, hogares_cap),
      n_personas = .n_keys(df_cap, personas_cap),
      prom_hogares_por_vivienda = .prom_hogares_por_vivienda(df_cap),
      prom_personas_por_hogar = .prom_personas_por_hogar(df_cap)
    )
  }))

  viviendas_universo <- .build_universo_vivienda(dfs, caps_all)
  hogares_universo <- .build_universo_hogar(dfs, base_hogar_cap, base_persona_cap)
  personas_universo <- .build_universo_persona(dfs, base_persona_cap, edad_var)

  viviendas_eval <- .build_eval_vivienda(viviendas_universo, dfs, caps_all)
  resumen_vivienda <- .resumen_vivienda(viviendas_eval, caps_all)
  viviendas_caidas <- viviendas_eval %>%
    dplyr::filter(!.data$vivienda_completa)

  hogares_eval <- .build_eval_hogar(
    hogares_universo = hogares_universo,
    personas_universo = personas_universo,
    viviendas_eval = viviendas_eval,
    dfs = dfs,
    caps_all = caps_all,
    caps_persona = caps_persona
  )
  resumen_hogar <- .resumen_req_ok(
    eval_df = hogares_eval,
    caps = caps_all,
    unidad = "hogares"
  )
  hogares_caidos <- hogares_eval %>%
    dplyr::filter(!.data$hogar_completo)

  personas_eval <- .build_eval_persona(
    personas_universo = personas_universo,
    dfs = dfs,
    caps_persona = caps_persona
  )
  resumen_persona <- .resumen_req_ok(
    eval_df = personas_eval,
    caps = caps_persona,
    unidad = "personas"
  )
  personas_caidas <- personas_eval %>%
    dplyr::filter(!.data$persona_completa)

  detalle_capitulos <- NULL
  if (isTRUE(exportar_sabana_completa)) {
    detalle_capitulos <- .build_detalle_capitulos(
      dfs = dfs,
      viviendas_caidas = viviendas_caidas,
      hogares_caidos = hogares_caidos,
      personas_caidas = personas_caidas,
      max_unidades_sabana = as.integer(max_unidades_sabana)
    )
  }

  salida <- list(
    resumen_capitulos = resumen_capitulos,
    resumen_vivienda = resumen_vivienda,
    resumen_hogar = resumen_hogar,
    resumen_persona = resumen_persona,
    viviendas_eval = viviendas_eval,
    hogares_eval = hogares_eval,
    personas_eval = personas_eval,
    viviendas_caidas = viviendas_caidas,
    hogares_caidos = hogares_caidos,
    personas_caidas = personas_caidas,
    detalle_capitulos = detalle_capitulos
  )

  if (isTRUE(exportar_excel)) {
    .exportar_diagnostico_cruce_excel(
      salida = salida,
      detalle_capitulos = detalle_capitulos,
      archivo = archivo,
      caps_vivienda = caps_vivienda,
      caps_hogar = caps_hogar,
      caps_persona = caps_persona,
      base_hogar_cap = base_hogar_cap,
      base_persona_cap = base_persona_cap,
      edad_var = edad_var,
      max_unidades_sabana = max_unidades_sabana
    )
    salida$archivo <- normalizePath(archivo, winslash = "/", mustWork = FALSE)
  }

  salida
}

.n_keys <- function(df, keys) {
  if (length(keys) == 0 || !all(keys %in% names(df))) {
    return(NA_integer_)
  }

  normalize_keys(df, keys) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) %>%
    nrow()
}

.prom_hogares_por_vivienda <- function(df) {
  if (!all(c("DIRECTORIO", "SECUENCIA_P") %in% names(df))) {
    return(NA_real_)
  }

  normalize_keys(df, c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::count(.data$DIRECTORIO, name = "n_hogares") %>%
    dplyr::summarise(valor = mean(.data$n_hogares)) %>%
    dplyr::pull(.data$valor)
}

.prom_personas_por_hogar <- function(df) {
  if (!all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(df))) {
    return(NA_real_)
  }

  normalize_keys(df, c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    dplyr::count(.data$DIRECTORIO, .data$SECUENCIA_P, name = "n_personas") %>%
    dplyr::summarise(valor = mean(.data$n_personas)) %>%
    dplyr::pull(.data$valor)
}

.build_universo_vivienda <- function(dfs, caps_all) {
  vivs <- dplyr::bind_rows(lapply(caps_all, function(cap) {
    df_cap <- dfs[[cap]]
    if (!"DIRECTORIO" %in% names(df_cap)) {
      return(NULL)
    }

    normalize_keys(df_cap, "DIRECTORIO") %>%
      dplyr::distinct(.data$DIRECTORIO)
  }))

  vivs %>%
    dplyr::distinct(.data$DIRECTORIO) %>%
    dplyr::arrange(.data$DIRECTORIO)
}

.build_universo_hogar <- function(dfs, base_hogar_cap, base_persona_cap) {
  cap_use <- if (base_hogar_cap %in% names(dfs)) base_hogar_cap else base_persona_cap

  if (!cap_use %in% names(dfs)) {
    stop("No fue posible construir el universo de hogares.")
  }

  df_cap <- dfs[[cap_use]]

  if (!all(c("DIRECTORIO", "SECUENCIA_P") %in% names(df_cap))) {
    stop("El capitulo base de hogares no contiene `DIRECTORIO` y `SECUENCIA_P`.")
  }

  normalize_keys(df_cap, c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::arrange(.data$DIRECTORIO, .data$SECUENCIA_P)
}

.build_universo_persona <- function(dfs, base_persona_cap, edad_var) {
  df_cap <- dfs[[base_persona_cap]]

  if (!all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(df_cap))) {
    stop("El capitulo base de personas no contiene las llaves completas.")
  }

  normalize_keys(df_cap, c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::mutate(edad = suppressWarnings(as.numeric(.data[[edad_var]]))) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .data$edad) %>%
    dplyr::arrange(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)
}

.build_eval_vivienda <- function(viviendas_universo, dfs, caps_all) {
  out <- viviendas_universo

  for (cap in caps_all) {
    df_cap <- dfs[[cap]]
    if (!"DIRECTORIO" %in% names(df_cap)) {
      next
    }

    pres_i <- normalize_keys(df_cap, "DIRECTORIO") %>%
      dplyr::distinct(.data$DIRECTORIO) %>%
      dplyr::mutate(!!paste0("pres_", cap) := 1L)

    out <- out %>%
      dplyr::left_join(pres_i, by = "DIRECTORIO")
  }

  out <- .replace_na_flags(out, "^pres_")
  pres_vars <- grep("^pres_", names(out), value = TRUE)
  out$n_caps_presentes <- rowSums(data.matrix(out[, pres_vars, drop = FALSE]), na.rm = TRUE)
  out$n_caps_faltantes <- length(pres_vars) - out$n_caps_presentes
  out$capitulos_presentes <- .collapse_cap_names(out, pres_vars, positive = 1L)
  out$capitulos_faltantes <- .collapse_cap_names(out, pres_vars, positive = 0L)
  out$vivienda_completa <- out$n_caps_faltantes == 0
  out
}

.build_eval_hogar <- function(hogares_universo,
                              personas_universo,
                              viviendas_eval,
                              dfs,
                              caps_all,
                              caps_persona) {
  out <- hogares_universo

  if ("pres_A" %in% names(viviendas_eval)) {
    out <- out %>%
      dplyr::left_join(
        viviendas_eval %>%
          dplyr::select(.data$DIRECTORIO, .data$pres_A),
        by = "DIRECTORIO"
      )
  }

  for (cap in setdiff(caps_all, "A")) {
    df_cap <- dfs[[cap]]
    if (!all(c("DIRECTORIO", "SECUENCIA_P") %in% names(df_cap))) {
      next
    }

    pres_i <- normalize_keys(df_cap, c("DIRECTORIO", "SECUENCIA_P")) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
      dplyr::mutate(!!paste0("pres_", cap) := 1L)

    out <- out %>%
      dplyr::left_join(pres_i, by = c("DIRECTORIO", "SECUENCIA_P"))
  }

  out <- .replace_na_flags(out, "^pres_")

  eleg_hogar <- personas_universo %>%
    dplyr::group_by(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::summarise(
      req_F = as.integer(dplyr::n() > 0),
      req_G = as.integer(any(!is.na(.data$edad) & .data$edad < 5)),
      req_H = as.integer(any(!is.na(.data$edad) & .data$edad >= 5)),
      req_I = as.integer(any(!is.na(.data$edad) & .data$edad >= 5)),
      req_J = as.integer(any(!is.na(.data$edad) & .data$edad >= 10)),
      req_K = as.integer(any(!is.na(.data$edad) & .data$edad >= 10)),
      personas_hogar = dplyr::n(),
      .groups = "drop"
    )

  out <- out %>%
    dplyr::left_join(eleg_hogar, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(
      hogares_universo %>%
        dplyr::count(.data$DIRECTORIO, name = "hogares_por_vivienda"),
      by = "DIRECTORIO"
    )

  for (cap in caps_all) {
    req_var <- paste0("req_", cap)

    if (cap == "C") {
      out[[req_var]] <- 0L
    } else if (cap %in% c("F", "G", "H", "I", "J", "K")) {
      if (!req_var %in% names(out)) {
        out[[req_var]] <- 0L
      }
    } else {
      out[[req_var]] <- 1L
    }
  }

  out <- .replace_na_flags(out, "^req_")

  for (cap in caps_all) {
    req_var <- paste0("req_", cap)
    pres_var <- paste0("pres_", cap)
    ok_var <- paste0("ok_", cap)

    if (!pres_var %in% names(out)) {
      out[[pres_var]] <- 0L
    }

    out[[ok_var]] <- ifelse(out[[req_var]] == 0L, 1L, out[[pres_var]] == 1L)
    out[[ok_var]] <- as.integer(out[[ok_var]])
  }

  req_vars <- grep("^req_", names(out), value = TRUE)
  ok_vars <- grep("^ok_", names(out), value = TRUE)
  req_mat <- data.matrix(out[, req_vars, drop = FALSE])
  ok_mat <- data.matrix(out[, ok_vars, drop = FALSE])

  out$n_caps_requeridos <- rowSums(req_mat, na.rm = TRUE)
  out$n_caps_ok_requeridos <- rowSums((req_mat == 1) & (ok_mat == 1), na.rm = TRUE)
  out$n_caps_faltantes <- out$n_caps_requeridos - out$n_caps_ok_requeridos
  out$capitulos_presentes <- .collapse_cap_names(out, grep("^pres_", names(out), value = TRUE), positive = 1L)
  out$capitulos_faltantes <- .collapse_missing_required(out, caps_all)
  out$hogar_completo <- out$n_caps_faltantes == 0

  out
}

.build_eval_persona <- function(personas_universo, dfs, caps_persona) {
  out <- personas_universo

  for (cap in caps_persona) {
    df_cap <- dfs[[cap]]
    if (!all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(df_cap))) {
      next
    }

    pres_i <- normalize_keys(df_cap, c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
      dplyr::mutate(!!paste0("pres_", cap) := 1L)

    out <- out %>%
      dplyr::left_join(pres_i, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
  }

  out <- .replace_na_flags(out, "^pres_")

  for (cap in caps_persona) {
    req_var <- paste0("req_", cap)

    if (cap == "E" || cap == "F") {
      out[[req_var]] <- 1L
    } else if (cap == "G") {
      out[[req_var]] <- as.integer(!is.na(out$edad) & out$edad < 5)
    } else if (cap %in% c("H", "I")) {
      out[[req_var]] <- as.integer(!is.na(out$edad) & out$edad >= 5)
    } else if (cap %in% c("J", "K")) {
      out[[req_var]] <- as.integer(!is.na(out$edad) & out$edad >= 10)
    } else {
      out[[req_var]] <- 0L
    }
  }

  for (cap in caps_persona) {
    req_var <- paste0("req_", cap)
    pres_var <- paste0("pres_", cap)
    ok_var <- paste0("ok_", cap)

    if (!pres_var %in% names(out)) {
      out[[pres_var]] <- 0L
    }

    out[[ok_var]] <- ifelse(out[[req_var]] == 0L, 1L, out[[pres_var]] == 1L)
    out[[ok_var]] <- as.integer(out[[ok_var]])
  }

  req_vars <- grep("^req_", names(out), value = TRUE)
  ok_vars <- grep("^ok_", names(out), value = TRUE)
  req_mat <- data.matrix(out[, req_vars, drop = FALSE])
  ok_mat <- data.matrix(out[, ok_vars, drop = FALSE])

  out$n_caps_requeridos <- rowSums(req_mat, na.rm = TRUE)
  out$n_caps_ok_requeridos <- rowSums((req_mat == 1) & (ok_mat == 1), na.rm = TRUE)
  out$n_caps_faltantes <- out$n_caps_requeridos - out$n_caps_ok_requeridos
  out$capitulos_presentes <- .collapse_cap_names(out, grep("^pres_", names(out), value = TRUE), positive = 1L)
  out$capitulos_faltantes <- .collapse_missing_required(out, caps_persona)
  out$persona_completa <- out$n_caps_faltantes == 0

  out
}

.replace_na_flags <- function(df, pattern) {
  vars <- grep(pattern, names(df), value = TRUE)
  for (v in vars) {
    df[[v]][is.na(df[[v]])] <- 0L
  }
  df
}

.collapse_cap_names <- function(df, vars, positive = 1L) {
  if (length(vars) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  caps <- sub("^[^_]+_", "", vars)
  mat <- data.matrix(df[, vars, drop = FALSE])

  apply(mat, 1, function(x) {
    idx <- which(x == positive)
    if (length(idx) == 0) {
      return(NA_character_)
    }
    paste(caps[idx], collapse = ", ")
  })
}

.collapse_missing_required <- function(df, caps) {
  out <- rep(NA_character_, nrow(df))

  for (i in seq_len(nrow(df))) {
    faltan <- character()

    for (cap in caps) {
      req_var <- paste0("req_", cap)
      ok_var <- paste0("ok_", cap)

      if (req_var %in% names(df) && ok_var %in% names(df)) {
        if (isTRUE(df[[req_var]][i] == 1L && df[[ok_var]][i] == 0L)) {
          faltan <- c(faltan, cap)
        }
      }
    }

    if (length(faltan) > 0) {
      out[i] <- paste(faltan, collapse = ", ")
    }
  }

  out
}

.resumen_vivienda <- function(viviendas_eval, caps_all) {
  dplyr::bind_rows(lapply(caps_all, function(cap) {
    pres_var <- paste0("pres_", cap)

    viviendas_presentes <- if (pres_var %in% names(viviendas_eval)) {
      sum(viviendas_eval[[pres_var]] == 1L, na.rm = TRUE)
    } else {
      0L
    }

    viviendas_universo <- nrow(viviendas_eval)
    viviendas_faltantes <- viviendas_universo - viviendas_presentes

    tibble::tibble(
      capitulo = cap,
      viviendas_universo = viviendas_universo,
      viviendas_presentes = viviendas_presentes,
      viviendas_faltantes = viviendas_faltantes,
      pct_faltantes = ifelse(viviendas_universo > 0, round(100 * viviendas_faltantes / viviendas_universo, 2), NA_real_)
    )
  }))
}

.resumen_req_ok <- function(eval_df, caps, unidad = "hogares") {
  universo_n <- nrow(eval_df)
  col_universo <- paste0(unidad, "_universo")
  col_requeridos <- paste0(unidad, "_requeridos")
  col_presentes <- paste0(unidad, "_presentes")
  col_ok <- paste0(unidad, "_ok")
  col_faltantes <- paste0(unidad, "_faltantes")

  out <- dplyr::bind_rows(lapply(caps, function(cap) {
    req_var <- paste0("req_", cap)
    pres_var <- paste0("pres_", cap)
    ok_var <- paste0("ok_", cap)

    requeridos <- if (req_var %in% names(eval_df)) sum(eval_df[[req_var]] == 1L, na.rm = TRUE) else 0L
    presentes <- if (pres_var %in% names(eval_df)) sum(eval_df[[pres_var]] == 1L, na.rm = TRUE) else 0L
    ok_n <- if (req_var %in% names(eval_df) && ok_var %in% names(eval_df)) {
      sum(eval_df[[req_var]] == 1L & eval_df[[ok_var]] == 1L, na.rm = TRUE)
    } else {
      0L
    }
    faltantes <- if (req_var %in% names(eval_df) && ok_var %in% names(eval_df)) {
      sum(eval_df[[req_var]] == 1L & eval_df[[ok_var]] == 0L, na.rm = TRUE)
    } else {
      0L
    }

    tibble::tibble(
      capitulo = cap,
      !!col_universo := universo_n,
      !!col_requeridos := requeridos,
      !!col_presentes := presentes,
      !!col_ok := ok_n,
      !!col_faltantes := faltantes,
      pct_faltantes = ifelse(requeridos > 0, round(100 * faltantes / requeridos, 2), NA_real_)
    )
  }))

  out
}

.build_detalle_capitulos <- function(dfs,
                                     viviendas_caidas,
                                     hogares_caidos,
                                     personas_caidas,
                                     max_unidades_sabana) {
  detalle <- list()

  exportar_viv <- nrow(viviendas_caidas) <= max_unidades_sabana
  exportar_hog <- nrow(hogares_caidos) <= max_unidades_sabana
  exportar_per <- nrow(personas_caidas) <= max_unidades_sabana

  if (!exportar_viv || !exportar_hog || !exportar_per) {
    detalle[["detalle_omitido"]] <- tibble::tibble(
      nivel = c("vivienda", "hogar", "persona"),
      n_unidades = c(nrow(viviendas_caidas), nrow(hogares_caidos), nrow(personas_caidas)),
      max_unidades_sabana = max_unidades_sabana,
      exportado = c(exportar_viv, exportar_hog, exportar_per)
    )
  }

  for (cap in names(dfs)) {
    df_cap <- dfs[[cap]]

    if (exportar_viv && "DIRECTORIO" %in% names(df_cap) && nrow(viviendas_caidas) > 0) {
      detalle[[paste0("det_viv_", cap)]] <- normalize_keys(df_cap, "DIRECTORIO") %>%
        dplyr::semi_join(viviendas_caidas %>% dplyr::select(.data$DIRECTORIO), by = "DIRECTORIO")
    }

    if (exportar_hog && all(c("DIRECTORIO", "SECUENCIA_P") %in% names(df_cap)) && nrow(hogares_caidos) > 0) {
      detalle[[paste0("det_hog_", cap)]] <- normalize_keys(df_cap, c("DIRECTORIO", "SECUENCIA_P")) %>%
        dplyr::semi_join(
          hogares_caidos %>% dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P),
          by = c("DIRECTORIO", "SECUENCIA_P")
        )
    }

    if (exportar_per && all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(df_cap)) && nrow(personas_caidas) > 0) {
      detalle[[paste0("det_per_", cap)]] <- normalize_keys(df_cap, c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
        dplyr::semi_join(
          personas_caidas %>% dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN),
          by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
        )
    }
  }

  detalle
}

.exportar_diagnostico_cruce_excel <- function(salida,
                                              detalle_capitulos,
                                              archivo,
                                              caps_vivienda,
                                              caps_hogar,
                                              caps_persona,
                                              base_hogar_cap,
                                              base_persona_cap,
                                              edad_var,
                                              max_unidades_sabana) {
  wb <- openxlsx::createWorkbook()

  parametros <- tibble::tibble(
    parametro = c(
      "caps_vivienda",
      "caps_hogar",
      "caps_persona",
      "base_hogar_cap",
      "base_persona_cap",
      "edad_var",
      "max_unidades_sabana"
    ),
    valor = c(
      paste(caps_vivienda, collapse = ", "),
      paste(caps_hogar, collapse = ", "),
      paste(caps_persona, collapse = ", "),
      base_hogar_cap,
      base_persona_cap,
      edad_var,
      as.character(max_unidades_sabana)
    )
  )

  hojas <- list(
    parametros = parametros,
    resumen_capitulos = salida$resumen_capitulos,
    resumen_vivienda = salida$resumen_vivienda,
    resumen_hogar = salida$resumen_hogar,
    resumen_persona = salida$resumen_persona,
    viviendas_caidas = salida$viviendas_caidas,
    hogares_caidos = salida$hogares_caidos,
    personas_caidas = salida$personas_caidas
  )

  if (!is.null(detalle_capitulos) && length(detalle_capitulos) > 0) {
    hojas <- c(hojas, detalle_capitulos)
  }

  for (nm in names(hojas)) {
    x <- hojas[[nm]]
    if (!is.data.frame(x)) {
      next
    }

    x <- x %>%
      dplyr::mutate(
        dplyr::across(where(is.factor), as.character),
        dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
      )
    x <- arreglar_utf8_df(x)

    openxlsx::addWorksheet(wb, substr(nm, 1, 31))
    openxlsx::writeData(wb, sheet = substr(nm, 1, 31), x = x)
  }

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
}
