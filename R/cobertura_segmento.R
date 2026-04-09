#' Construir base de completitud de hogares
#'
#' Función interna que construye una base a nivel hogar con:
#' universo de hogares, segmento opcional, elegibilidad por edad,
#' presencia de capítulos, requerimientos, cumplimiento por capítulo
#' y bandera de encuesta completa.
#'
#' La unidad de análisis es el hogar: \code{DIRECTORIO + SECUENCIA_P}.
#'
#' @param dfs Lista nombrada de data frames con los capítulos.
#' @param caps_fijos Vector de capítulos que se exigen siempre.
#' @param caps_edad Vector de capítulos cuya exigencia depende de la edad.
#' @param base_hogares_cap Capítulo a usar como universo de hogares.
#' @param incluir_segmento Lógico. Si es \code{TRUE} y existe el capítulo A con
#'   la variable \code{SEGMENTO}, la incorpora a la salida.
#' @param edad_var Nombre de la variable de edad en el capítulo E.
#'
#' @return Un data frame a nivel hogar con indicadores de elegibilidad,
#' presencia, requerimiento, cumplimiento y completitud.
#'
#' @details
#' Esta función asume que en el paquete existen:
#'
#' - \code{tipo_capitulo}: vector/lista que mapea cada capítulo a uno de
#'   \code{"vivienda"}, \code{"hogar"} o \code{"persona"}.
#' - \code{normalize_keys()}: función para normalizar llaves de cruce.
#'
#' Reglas de elegibilidad por edad:
#'
#' - G: requerido si existe alguna persona con edad < 5
#' - H: requerido si existe alguna persona con edad >= 5
#' - I: requerido si existe alguna persona con edad >= 5
#' - J: requerido si existe alguna persona con edad >= 10
#' - K: requerido si existe alguna persona con edad >= 10
#'
#' Un capítulo se considera cumplido si:
#'
#' - no era requerido para el hogar, o
#' - era requerido y existe registro para ese hogar.
#'
#' @keywords internal
build_base_completitud_hogar <- function(dfs,
                                         caps_fijos,
                                         caps_edad = c("G", "H", "I", "J", "K"),
                                         base_hogares_cap = "B",
                                         incluir_segmento = FALSE,
                                         edad_var = "NPCEP4") {

  # -----------------------------
  # Validaciones básicas
  # -----------------------------
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("dfs debe ser una lista nombrada de data frames")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("dfs debe ser una lista nombrada con nombres de capítulos")
  }

  names(dfs) <- toupper(names(dfs))
  base_hogares_cap <- toupper(base_hogares_cap)
  caps_fijos <- intersect(toupper(caps_fijos), names(dfs))
  caps_edad  <- intersect(toupper(caps_edad), names(dfs))

  if (length(c(caps_fijos, caps_edad)) == 0) {
    stop("No hay capítulos válidos para evaluar")
  }

  if (!"E" %in% names(dfs)) {
    stop("Se requiere el capítulo E para calcular elegibilidad por edad")
  }

  if (!(edad_var %in% names(dfs[["E"]]))) {
    stop(paste0("La variable de edad no existe en E: ", edad_var))
  }

  if (!exists("tipo_capitulo", inherits = TRUE)) {
    stop("No se encontró `tipo_capitulo` en el entorno del paquete")
  }

  if (!exists("normalize_keys", mode = "function", inherits = TRUE)) {
    stop("No se encontró la función `normalize_keys()` en el entorno del paquete")
  }

  caps_con_tipo <- c(caps_fijos, caps_edad)
  caps_sin_tipo <- caps_con_tipo[vapply(
    caps_con_tipo,
    function(x) is.null(tipo_capitulo[[x]]),
    logical(1)
  )]

  if (length(caps_sin_tipo) > 0) {
    stop(
      paste0(
        "No está definido el tipo de los capítulos: ",
        paste(caps_sin_tipo, collapse = ", ")
      )
    )
  }

  # -----------------------------
  # Universo de hogares
  # -----------------------------
  if (base_hogares_cap %in% names(dfs)) {
    tipo_base <- tipo_capitulo[[base_hogares_cap]]

    if (tipo_base %in% c("hogar", "persona")) {
      base_hogares <- dfs[[base_hogares_cap]] %>%
        dplyr::distinct(DIRECTORIO, SECUENCIA_P)
    } else {
      stop("base_hogares_cap debe ser un capítulo de hogar o persona")
    }
  } else if ("E" %in% names(dfs)) {
    base_hogares <- dfs[["E"]] %>%
      dplyr::distinct(DIRECTORIO, SECUENCIA_P)
  } else {
    stop("No se pudo construir el universo de hogares")
  }

  base_hogares <- normalize_keys(base_hogares, c("DIRECTORIO", "SECUENCIA_P"))

  # -----------------------------
  # SEGMENTO opcional desde A
  # -----------------------------
  if (isTRUE(incluir_segmento) &&
      "A" %in% names(dfs) &&
      "SEGMENTO" %in% names(dfs[["A"]])) {

    seg_a <- dfs[["A"]] %>%
      dplyr::distinct(DIRECTORIO, SEGMENTO) %>%
      normalize_keys("DIRECTORIO")

    base_hogares <- base_hogares %>%
      dplyr::left_join(seg_a, by = "DIRECTORIO")
  }

  # -----------------------------
  # Personas desde E
  # -----------------------------
  personas_e <- dfs[["E"]] %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::mutate(edad = suppressWarnings(as.numeric(.data[[edad_var]]))) %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P, ORDEN, edad)

  # -----------------------------
  # Elegibilidad por hogar
  # -----------------------------
  eleg_hogar <- personas_e %>%
    dplyr::group_by(DIRECTORIO, SECUENCIA_P) %>%
    dplyr::summarise(
      elig_G = as.integer(any(!is.na(edad) & edad < 5)),
      elig_H = as.integer(any(!is.na(edad) & edad >= 5)),
      elig_I = as.integer(any(!is.na(edad) & edad >= 5)),
      elig_J = as.integer(any(!is.na(edad) & edad >= 10)),
      elig_K = as.integer(any(!is.na(edad) & edad >= 10)),
      .groups = "drop"
    )

  base_eval <- base_hogares %>%
    dplyr::left_join(eleg_hogar, by = c("DIRECTORIO", "SECUENCIA_P"))

  for (v in c("elig_G", "elig_H", "elig_I", "elig_J", "elig_K")) {
    if (v %in% names(base_eval)) {
      base_eval[[v]][is.na(base_eval[[v]])] <- 0L
    }
  }

  # -----------------------------
  # Presencia capítulos fijos
  # -----------------------------
  for (cap in caps_fijos) {
    df_cap <- dfs[[cap]]
    tipo <- tipo_capitulo[[cap]]

    pres_i <- switch(
      tipo,
      vivienda = {
        df_cap %>%
          dplyr::distinct(DIRECTORIO) %>%
          normalize_keys("DIRECTORIO") %>%
          dplyr::inner_join(
            base_hogares %>% dplyr::distinct(DIRECTORIO, SECUENCIA_P),
            by = "DIRECTORIO"
          ) %>%
          dplyr::distinct(DIRECTORIO, SECUENCIA_P) %>%
          dplyr::mutate(!!paste0("pres_", cap) := 1L)
      },
      hogar = {
        if (cap == "C") {
          expandir_presencia_capitulo_c(base_hogares, df_cap) %>%
            dplyr::mutate(!!paste0("pres_", cap) := 1L)
        } else {
          df_cap %>%
            dplyr::distinct(DIRECTORIO, SECUENCIA_P) %>%
            normalize_keys(c("DIRECTORIO", "SECUENCIA_P")) %>%
            dplyr::mutate(!!paste0("pres_", cap) := 1L)
        }
      },
      persona = {
        df_cap %>%
          dplyr::distinct(DIRECTORIO, SECUENCIA_P) %>%
          normalize_keys(c("DIRECTORIO", "SECUENCIA_P")) %>%
          dplyr::mutate(!!paste0("pres_", cap) := 1L)
      },
      stop(paste0("Tipo de capítulo no soportado: ", tipo))
    )

    base_eval <- base_eval %>%
      dplyr::left_join(pres_i, by = c("DIRECTORIO", "SECUENCIA_P"))
  }

  # -----------------------------
  # Presencia capítulos por edad
  # -----------------------------
  for (cap in caps_edad) {
    pres_i <- dfs[[cap]] %>%
      dplyr::distinct(DIRECTORIO, SECUENCIA_P) %>%
      normalize_keys(c("DIRECTORIO", "SECUENCIA_P")) %>%
      dplyr::mutate(!!paste0("pres_", cap) := 1L)

    base_eval <- base_eval %>%
      dplyr::left_join(pres_i, by = c("DIRECTORIO", "SECUENCIA_P"))
  }

  vars_pres <- grep("^pres_", names(base_eval), value = TRUE)
  for (v in vars_pres) {
    base_eval[[v]][is.na(base_eval[[v]])] <- 0L
  }

  # -----------------------------
  # Requerimiento real por capítulo
  # -----------------------------
  for (cap in caps_fijos) {
    base_eval[[paste0("req_", cap)]] <- 1L
  }

  mapa_elig <- c(
    G = "elig_G",
    H = "elig_H",
    I = "elig_I",
    J = "elig_J",
    K = "elig_K"
  )

  for (cap in caps_edad) {
    elig_var <- mapa_elig[[cap]]
    req_var  <- paste0("req_", cap)

    if (!is.null(elig_var) && elig_var %in% names(base_eval)) {
      base_eval[[req_var]] <- base_eval[[elig_var]]
    }
  }

  # -----------------------------
  # Cumplimiento por capítulo
  # -----------------------------
  caps_eval <- c(caps_fijos, caps_edad)

  for (cap in caps_eval) {
    req_var  <- paste0("req_", cap)
    pres_var <- paste0("pres_", cap)
    ok_var   <- paste0("ok_", cap)

    if (req_var %in% names(base_eval) && pres_var %in% names(base_eval)) {
      base_eval[[ok_var]] <- ifelse(
        base_eval[[req_var]] == 0,
        1L,
        ifelse(base_eval[[pres_var]] == 1, 1L, 0L)
      )
    }
  }

  vars_req <- grep("^req_", names(base_eval), value = TRUE)
  vars_ok  <- grep("^ok_", names(base_eval), value = TRUE)

  req_mat <- data.matrix(base_eval[, vars_req, drop = FALSE])
  ok_mat  <- data.matrix(base_eval[, vars_ok,  drop = FALSE])

  base_eval$n_caps_requeridos <- rowSums(req_mat, na.rm = TRUE)
  base_eval$n_caps_ok_requeridos <- rowSums((req_mat == 1) & (ok_mat == 1), na.rm = TRUE)
  base_eval$n_caps_faltantes <- base_eval$n_caps_requeridos - base_eval$n_caps_ok_requeridos
  base_eval$encuesta_completa <- ifelse(
    base_eval$n_caps_faltantes == 0,
    1L, 0L
  )

  # -----------------------------
  # Lista de capítulos faltantes
  # -----------------------------
  caps_req_names <- sub("^req_", "", vars_req)

  base_eval$capitulos_faltantes <- apply(
    data.matrix(base_eval[, c(vars_req, vars_ok), drop = FALSE]),
    1,
    function(x) {
      req_vals <- x[seq_along(vars_req)]
      ok_vals  <- x[length(vars_req) + seq_along(vars_ok)]
      caps_faltan <- caps_req_names[req_vals == 1 & ok_vals == 0]

      if (length(caps_faltan) == 0) {
        NA_character_
      } else {
        paste(caps_faltan, collapse = ", ")
      }
    }
  )

  base_eval
}


#' Cobertura de encuesta por segmento con elegibilidad por edad
#'
#' Calcula la cobertura de encuestas completas por \code{SEGMENTO},
#' considerando capítulos estructurales y capítulos cuya exigencia depende
#' de la edad de las personas del hogar.
#'
#' La unidad de análisis es el hogar: \code{DIRECTORIO + SECUENCIA_P}.
#'
#' @param dfs Lista nombrada de data frames con los capítulos de la encuesta.
#' @param caps_fijos Vector de capítulos que siempre deben existir para un hogar.
#'   Por defecto: \code{c("A","B","C","D","E","L","M","MA","MB")}.
#' @param caps_edad Vector de capítulos cuya exigencia depende de la edad de las
#'   personas del hogar. Por defecto: \code{c("G","H","I","J","K")}.
#' @param edad_var Nombre de la variable de edad dentro del capítulo E.
#'   Por defecto: \code{"NPCEP4"}.
#' @param base_hogares_cap Capítulo a usar como universo de hogares.
#'   Por defecto: \code{"B"}. Si no existe, intenta con \code{"E"}.
#'
#' @details
#' La elegibilidad por edad se define así:
#'
#' - G: se exige si existe alguna persona con edad < 5
#' - H: se exige si existe alguna persona con edad >= 5
#' - I: se exige si existe alguna persona con edad >= 5
#' - J: se exige si existe alguna persona con edad >= 10
#' - K: se exige si existe alguna persona con edad >= 10
#'
#' Un hogar se considera completo si cumple todos los capítulos requeridos.
#'
#' @return Una lista con:
#' \describe{
#'   \item{base_eval}{Base a nivel hogar con elegibilidad, presencia, requerimiento y completitud}
#'   \item{resumen_segmento}{Resumen de cobertura por segmento}
#'   \item{mas_afectado}{Segmento(s) con mayor caída de cobertura}
#'   \item{menos_afectado}{Segmento(s) con menor caída de cobertura}
#'   \item{diagnostico_cap_segmento}{Diagnóstico por capítulo y segmento considerando solo hogares donde el capítulo era requerido}
#' }
#'
#' @examples
#' \dontrun{
#' dfs <- list(
#'   A = A, B = B, C = C, D = D, E = E,
#'   G = G, H = H, I = I, J = J, K = K,
#'   L = L, M = M, MA = MA, MB = MB
#' )
#'
#' res <- cobertura_segmento_fina(dfs)
#' head(res$resumen_segmento)
#' }
#'
#' @export
cobertura_segmento_fina <- function(dfs,
                                    caps_fijos = c("A","B","C","D","E","L","M","MA","MB"),
                                    caps_edad = c("G","H","I","J","K"),
                                    edad_var = "NPCEP4",
                                    base_hogares_cap = "B") {

  base_eval <- build_base_completitud_hogar(
    dfs = dfs,
    caps_fijos = caps_fijos,
    caps_edad = caps_edad,
    base_hogares_cap = base_hogares_cap,
    incluir_segmento = TRUE,
    edad_var = edad_var
  )

  if (!"SEGMENTO" %in% names(base_eval)) {
    stop("No fue posible incorporar SEGMENTO desde el capítulo A")
  }

  resumen_segmento <- base_eval %>%
    dplyr::group_by(SEGMENTO) %>%
    dplyr::summarise(
      hogares_totales = dplyr::n(),
      hogares_completos = sum(encuesta_completa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      hogares_incompletos = hogares_totales - hogares_completos,
      cobertura = hogares_completos / hogares_totales,
      caida = 1 - cobertura
    ) %>%
    dplyr::arrange(dplyr::desc(caida))

  mas_afectado <- resumen_segmento %>%
    dplyr::slice_max(caida, n = 1, with_ties = TRUE)

  menos_afectado <- resumen_segmento %>%
    dplyr::slice_min(caida, n = 1, with_ties = TRUE)

  caps_eval <- c(
    intersect(toupper(caps_fijos), toupper(names(dfs))),
    intersect(toupper(caps_edad), toupper(names(dfs)))
  )

  diagnostico_cap_segmento <- dplyr::bind_rows(lapply(caps_eval, function(cap) {
    req_var <- paste0("req_", cap)
    ok_var  <- paste0("ok_", cap)

    base_eval %>%
      dplyr::filter(.data[[req_var]] == 1) %>%
      dplyr::group_by(SEGMENTO) %>%
      dplyr::summarise(
        capitulo = cap,
        hogares_requeridos = dplyr::n(),
        hogares_ok = sum(.data[[ok_var]] == 1, na.rm = TRUE),
        hogares_falla = sum(.data[[ok_var]] == 0, na.rm = TRUE),
        pct_falla = 100 * hogares_falla / hogares_requeridos,
        .groups = "drop"
      )
  })) %>%
    dplyr::arrange(dplyr::desc(pct_falla), SEGMENTO, capitulo)

  list(
    base_eval = base_eval,
    resumen_segmento = resumen_segmento,
    mas_afectado = mas_afectado,
    menos_afectado = menos_afectado,
    diagnostico_cap_segmento = diagnostico_cap_segmento
  )
}


#' Diagnóstico de completitud de encuestas por capítulo con elegibilidad por edad
#'
#' Evalúa, a nivel de hogar, la presencia de capítulos en una lista de bases y
#' determina cuáles encuestas están completas y cuáles no, considerando la
#' elegibilidad por edad para los capítulos G, H, I, J y K.
#'
#' La unidad de análisis es el hogar: \code{DIRECTORIO + SECUENCIA_P}.
#'
#' @param dfs Lista nombrada de data frames con los capítulos.
#' @param caps_fijos Vector de capítulos que se exigen siempre. Si es
#'   \code{NULL}, usa \code{c("A","B","C","D","E","L","M","MA","MB")}.
#' @param caps_edad Vector de capítulos cuya exigencia depende de la edad.
#'   Por defecto \code{c("G","H","I","J","K")}.
#' @param base_hogares_cap Capítulo a usar como universo de hogares.
#'   Por defecto \code{"B"}. Si no existe, intenta con \code{"E"}.
#' @param incluir_segmento Lógico. Si es \code{TRUE} y existe el capítulo A con
#'   la variable \code{SEGMENTO}, la incorpora al resultado.
#' @param edad_var Nombre de la variable de edad en el capítulo E.
#'   Por defecto \code{"NPCEP4"}.
#'
#' @return Una lista con:
#' \describe{
#'   \item{base_eval}{Base hogar con elegibilidad, presencia, requerimiento y completitud}
#'   \item{resumen_general}{Resumen global de encuestas completas e incompletas}
#'   \item{resumen_capitulo}{Resumen por capítulo considerando solo hogares donde era requerido}
#'   \item{encuestas_incompletas}{Detalle de hogares que no cumplen todos los capítulos requeridos}
#' }
#'
#' @examples
#' \dontrun{
#' res <- diagnostico_completitud_capitulos(dfs)
#' res$resumen_general
#' head(res$encuestas_incompletas)
#' }
#'
#' @export
diagnostico_completitud_capitulos <- function(dfs,
                                              caps_fijos = NULL,
                                              caps_edad = c("G", "H", "I", "J", "K"),
                                              base_hogares_cap = "B",
                                              incluir_segmento = TRUE,
                                              edad_var = "NPCEP4") {

  if (is.null(caps_fijos)) {
    caps_fijos <- c("A", "B", "C", "D", "E", "L", "M", "MA", "MB")
  }

  base_eval <- build_base_completitud_hogar(
    dfs = dfs,
    caps_fijos = caps_fijos,
    caps_edad = caps_edad,
    base_hogares_cap = base_hogares_cap,
    incluir_segmento = incluir_segmento,
    edad_var = edad_var
  )

  caps_eval <- c(
    intersect(toupper(caps_fijos), toupper(names(dfs))),
    intersect(toupper(caps_edad), toupper(names(dfs)))
  )

  resumen_general <- base_eval %>%
    dplyr::summarise(
      encuestas_totales = dplyr::n(),
      encuestas_completas = sum(encuesta_completa, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      encuestas_incompletas = encuestas_totales - encuestas_completas,
      pct_completas = round(100 * encuestas_completas / encuestas_totales, 2),
      pct_incompletas = round(100 * encuestas_incompletas / encuestas_totales, 2)
    )

  resumen_capitulo <- dplyr::bind_rows(lapply(caps_eval, function(cap) {
    req_v <- paste0("req_", cap)
    ok_v  <- paste0("ok_", cap)

    base_tmp <- base_eval %>%
      dplyr::filter(.data[[req_v]] == 1)

    hogares_requeridos <- nrow(base_tmp)
    hogares_ok <- sum(base_tmp[[ok_v]] == 1, na.rm = TRUE)
    hogares_faltantes <- sum(base_tmp[[ok_v]] == 0, na.rm = TRUE)

    tibble::tibble(
      capitulo = cap,
      hogares_requeridos = hogares_requeridos,
      hogares_ok = hogares_ok,
      hogares_faltantes = hogares_faltantes,
      pct_ok = ifelse(hogares_requeridos > 0, round(100 * hogares_ok / hogares_requeridos, 2), NA_real_),
      pct_faltantes = ifelse(hogares_requeridos > 0, round(100 * hogares_faltantes / hogares_requeridos, 2), NA_real_)
    )
  })) %>%
    dplyr::arrange(dplyr::desc(pct_faltantes), capitulo)

  encuestas_incompletas <- base_eval %>%
    dplyr::filter(encuesta_completa == 0) %>%
    dplyr::arrange(dplyr::desc(n_caps_faltantes), dplyr::desc(n_caps_requeridos))

  list(
    base_eval = base_eval,
    resumen_general = resumen_general,
    resumen_capitulo = resumen_capitulo,
    encuestas_incompletas = encuestas_incompletas
  )
}
