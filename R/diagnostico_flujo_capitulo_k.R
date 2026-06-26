#' Diagnosticar perdida de informacion en el flujo del Capitulo K
#'
#' Construye un diagnostico persona-variable del Capitulo K, separando vacios
#' esperados por salto valido de vacios criticos donde la persona debia
#' responder segun el flujo teorico. La unidad de analisis es siempre persona:
#' `DIRECTORIO + SECUENCIA_P + ORDEN`.
#'
#' @param dfs Lista nombrada de capitulos. Debe incluir `K`; si incluye `E`,
#'   la edad se toma preferiblemente desde ese capitulo por llave persona.
#' @param diccionario Diccionario opcional del Capitulo K. Si se suministra,
#'   se usa para enriquecer reglas y para calcular un resumen agregado con
#'   [resumir_flujo_capitulo_k()].
#' @param variable_k23_final Variable final/imputada de posicion ocupacional.
#'   Por defecto `"NPCKP17"`. Se usa como verdad para reconstruir el flujo
#'   posterior y no se marca como candidata a imputacion.
#' @param vars_cap_k Vector opcional de variables de K a evaluar. Si es `NULL`,
#'   se evaluan las variables incluidas en las reglas implementadas.
#' @param detener_si_duplicados Logico. Si `TRUE`, la funcion se detiene cuando
#'   K no es unico por `DIRECTORIO + SECUENCIA_P + ORDEN`.
#' @param incluir_texto_libre Logico reservado para ampliaciones del diccionario
#'   operativo. En esta version se conservan todas las variables normadas.
#'
#' @return Lista con `diagnostico_persona_variable`, `resumen_variables`,
#'   `resumen_personas`, `resumen_bloques`,
#'   `variables_candidatas_imputacion`, `auditoria_llaves`, `reglas_flujo`,
#'   `auditoria_por_pregunta`, `variables_ausentes`, `duplicados_k` y
#'   `resumen_flujo_agregado`.
#'
#' @details
#' `NPCKP17` corresponde a K23. En la base del 22 de junio esta variable ya
#' viene imputada, por lo que se crea `NPCKP17_FINAL = NPCKP17` y se usa ese
#' valor como insumo cierto para el flujo posterior. La pregunta para variables
#' posteriores a K23 es: dada la posicion ocupacional final, esta variable
#' debia responderse?
#'
#' @examples
#' \dontrun{
#' diag_k <- diagnostico_flujo_capitulo_k(dfs = dfs, diccionario = reglas_k_total)
#' diag_k$resumen_variables
#' diag_k$variables_candidatas_imputacion
#' }
#'
#' @export
diagnostico_flujo_capitulo_k <- function(dfs,
                                         diccionario = NULL,
                                         variable_k23_final = "NPCKP17",
                                         vars_cap_k = NULL,
                                         detener_si_duplicados = TRUE,
                                         incluir_texto_libre = FALSE) {
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de capitulos.")
  }
  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe tener nombres de capitulos.")
  }

  names(dfs) <- toupper(names(dfs))
  if (!"K" %in% names(dfs) || !is.data.frame(dfs$K)) {
    stop("`dfs` debe incluir el capitulo `K` como data.frame.")
  }
  if (!identical(tipo_capitulo[["K"]], "persona")) {
    stop("El capitulo K debe estar configurado como nivel `persona`.")
  }

  llaves_persona <- get_join_keys("K")
  if (!identical(llaves_persona, c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))) {
    stop("La llave del capitulo K debe ser DIRECTORIO + SECUENCIA_P + ORDEN.")
  }

  K <- tibble::as_tibble(dfs$K) %>%
    .diag_k_normalizar_utf8_df()
  faltan_llaves <- setdiff(llaves_persona, names(K))
  if (length(faltan_llaves) > 0) {
    stop("Faltan llaves persona en `dfs$K`: ", paste(faltan_llaves, collapse = ", "))
  }

  K <- normalize_keys(K, llaves_persona)
  auditoria_dup <- .diag_k_auditar_duplicados(K, llaves_persona)
  if (auditoria_dup$n_duplicados > 0 && isTRUE(detener_si_duplicados)) {
    ejemplos <- auditoria_dup$duplicados %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(id = paste(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, sep = " + ")) %>%
      dplyr::pull(.data$id)

    stop(
      "`dfs$K` no es unico por DIRECTORIO + SECUENCIA_P + ORDEN. ",
      "Registros duplicados: ", auditoria_dup$n_duplicados, ". ",
      "Primeras llaves duplicadas: ", paste(ejemplos, collapse = "; "), "."
    )
  }
  if (auditoria_dup$n_duplicados > 0) {
    K <- K %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(llaves_persona)), .keep_all = TRUE)
  }

  data_edad <- if ("E" %in% names(dfs) && is.data.frame(dfs$E)) {
    tibble::as_tibble(dfs$E) %>%
      .diag_k_normalizar_utf8_df()
  } else {
    NULL
  }
  K_eval <- .diag_k_agregar_edad(K, data_edad, llaves_persona)
  K_eval$id_persona <- paste(K_eval$DIRECTORIO, K_eval$SECUENCIA_P, K_eval$ORDEN, sep = "::")
  K_eval$NPCKP17_FINAL <- if (variable_k23_final %in% names(K_eval)) K_eval[[variable_k23_final]] else NA

  nodos_paquete <- tryCatch(
    construir_nodos_flujo_k(K_eval, edad_var = "edad"),
    error = function(e) NULL
  )

  reglas <- .diag_k_construir_reglas(
    data = K_eval,
    nodos_paquete = nodos_paquete,
    variable_k23_final = variable_k23_final,
    incluir_texto_libre = incluir_texto_libre
  )

  if (!is.null(vars_cap_k)) {
    vars_cap_k <- unique(stringr::str_trim(.diag_k_normalizar_utf8_vector(as.character(vars_cap_k))))
    vars_cap_k <- vars_cap_k[!is.na(vars_cap_k) & nzchar(vars_cap_k)]
    reglas <- .diag_k_filtrar_o_completar_reglas(reglas, vars_cap_k, nrow(K_eval))
  }

  reglas_tbl <- .diag_k_reglas_tbl(reglas, K_eval, diccionario)
  diagnostico_persona_variable <- .diag_k_construir_largo(K_eval, reglas, variable_k23_final)
  auditoria_por_pregunta <- .diag_k_auditoria_por_pregunta(
    diagnostico_persona_variable,
    reglas_tbl
  )

  resumen_variables <- diagnostico_persona_variable %>%
    dplyr::group_by(.data$variable, .data$bloque, .data$regla_aplicada, .data$fuente_regla) %>%
    dplyr::summarise(
      personas = dplyr::n(),
      deben_responder = sum(.data$debe_responder %in% TRUE, na.rm = TRUE),
      respondieron_cuando_debian = sum(.data$estado_flujo == "Respondio cuando debia responder", na.rm = TRUE),
      vacios_criticos = sum(.data$vacio_critico, na.rm = TRUE),
      saltos_validos = sum(.data$estado_flujo == "Salto valido / no debia responder", na.rm = TRUE),
      respuestas_fuera_flujo = sum(.data$respuesta_fuera_flujo, na.rm = TRUE),
      flujos_indeterminados = sum(.data$estado_flujo == "Flujo indeterminado", na.rm = TRUE),
      variables_ausentes = sum(.data$estado_flujo == "Variable ausente en base", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_vacio_critico_sobre_deben = dplyr::if_else(
        .data$deben_responder > 0,
        .data$vacios_criticos / .data$deben_responder,
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$vacios_criticos), .data$bloque, .data$variable)

  resumen_personas <- diagnostico_persona_variable %>%
    dplyr::group_by(
      .data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .data$id_persona,
      .data$edad, .data$universo_k, .data$NPCKP17_FINAL
    ) %>%
    dplyr::summarise(
      variables_evaluadas = dplyr::n(),
      variables_debian_responder = sum(.data$debe_responder %in% TRUE, na.rm = TRUE),
      n_vacios_criticos = sum(.data$vacio_critico, na.rm = TRUE),
      n_respuestas_fuera_flujo = sum(.data$respuesta_fuera_flujo, na.rm = TRUE),
      n_flujos_indeterminados = sum(.data$estado_flujo == "Flujo indeterminado", na.rm = TRUE),
      n_variables_ausentes = sum(.data$estado_flujo == "Variable ausente en base", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(cae_flujo_k = .data$n_vacios_criticos > 0) %>%
    dplyr::arrange(dplyr::desc(.data$n_vacios_criticos), .data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  resumen_bloques <- diagnostico_persona_variable %>%
    dplyr::group_by(.data$bloque) %>%
    dplyr::summarise(
      celdas_evaluadas = dplyr::n(),
      deben_responder = sum(.data$debe_responder %in% TRUE, na.rm = TRUE),
      vacios_criticos = sum(.data$vacio_critico, na.rm = TRUE),
      respuestas_fuera_flujo = sum(.data$respuesta_fuera_flujo, na.rm = TRUE),
      flujos_indeterminados = sum(.data$estado_flujo == "Flujo indeterminado", na.rm = TRUE),
      variables_ausentes = sum(.data$estado_flujo == "Variable ausente en base", na.rm = TRUE),
      personas_con_vacio_critico = dplyr::n_distinct(.data$id_persona[.data$vacio_critico]),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_vacio_critico_sobre_deben = dplyr::if_else(
        .data$deben_responder > 0,
        .data$vacios_criticos / .data$deben_responder,
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$vacios_criticos), .data$bloque)

  variables_candidatas_imputacion <- diagnostico_persona_variable %>%
    dplyr::filter(.data$candidata_imputacion) %>%
    dplyr::group_by(.data$variable, .data$bloque, .data$regla_aplicada, .data$fuente_regla) %>%
    dplyr::summarise(
      n_personas = dplyr::n_distinct(.data$id_persona),
      n_vacios_criticos = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n_vacios_criticos), .data$variable)

  auditoria_llaves <- tibble::tibble(
    capitulo = "K",
    nivel_capitulo = tipo_capitulo[["K"]],
    llave_usada = paste(llaves_persona, collapse = " + "),
    n_filas_k_original = auditoria_dup$n_filas,
    n_personas_k = nrow(K_eval),
    n_personas_unicas_k_original = auditoria_dup$n_unicas,
    n_duplicados_k_original = auditoria_dup$n_duplicados,
    detener_si_duplicados = isTRUE(detener_si_duplicados),
    edad_objeto_k = Edad_objeto[["K"]],
    variable_k23_final = variable_k23_final,
    variable_k23_presente = variable_k23_final %in% names(K_eval),
    n_nodos_flujo_paquete = if (is.null(nodos_paquete)) 0L else length(grep("^llega_|^universo_k$", names(nodos_paquete), value = TRUE)),
    observacion = paste(
      "La unidad persona se evalua exclusivamente con DIRECTORIO + SECUENCIA_P + ORDEN.",
      "NPCKP17_FINAL se toma desde la variable final indicada y se usa como verdad para el flujo posterior."
    )
  )

  resumen_flujo_agregado <- NULL
  if (!is.null(diccionario)) {
    resumen_flujo_agregado <- tryCatch(
      resumir_flujo_capitulo_k(K_eval, diccionario, edad_var = "edad"),
      error = function(e) tibble::tibble(
        problema = "No fue posible calcular resumir_flujo_capitulo_k().",
        detalle = conditionMessage(e)
      )
    )
  }

  list(
    diagnostico_persona_variable = diagnostico_persona_variable,
    resumen_variables = resumen_variables,
    resumen_personas = resumen_personas,
    resumen_bloques = resumen_bloques,
    variables_candidatas_imputacion = variables_candidatas_imputacion,
    auditoria_llaves = auditoria_llaves,
    reglas_flujo = reglas_tbl,
    auditoria_por_pregunta = auditoria_por_pregunta,
    variables_ausentes = reglas_tbl %>%
      dplyr::filter(!.data$variable_presente) %>%
      dplyr::select(.data$orden_flujo, .data$variable, .data$bloque, .data$regla_r, .data$fuente_regla, .data$pregunta),
    duplicados_k = auditoria_dup$duplicados,
    resumen_flujo_agregado = resumen_flujo_agregado
  )
}

.diag_k_normalizar_utf8_vector <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  y <- iconv(x, from = "", to = "UTF-8", sub = "")
  idx_malo <- is.na(y) & !is.na(x)

  if (any(idx_malo)) {
    y[idx_malo] <- iconv(x[idx_malo], from = "latin1", to = "UTF-8", sub = "")
  }

  y
}

.diag_k_normalizar_utf8_df <- function(df) {
  df[] <- lapply(df, .diag_k_normalizar_utf8_vector)
  df
}

.diag_k_null <- function(x, y) {
  if (is.null(x)) y else x
}

.diag_k_fuente_visible <- function(fuente_regla, regla_r) {
  if (is.na(fuente_regla) || !nzchar(fuente_regla)) {
    return("Regla aproximada pendiente de validación")
  }

  fuente_regla <- as.character(fuente_regla)
  regla_r <- as.character(regla_r)

  if (
    identical(fuente_regla, "construir_nodos_flujo_k") ||
      grepl("calculado por construir_nodos_flujo_k", regla_r, fixed = TRUE) ||
      grepl("indeterminado", fuente_regla, ignore.case = TRUE) ||
      grepl("no codificada", regla_r, ignore.case = TRUE)
  ) {
    return("Regla aproximada pendiente de validación")
  }

  fuente_regla
}

.diag_k_extraer_variables_regla <- function(regla_r) {
  regla_r <- as.character(regla_r)
  vars <- unique(unlist(regmatches(
    regla_r,
    gregexpr("NPCK[A-Z0-9_]+|llega_K[0-9]+|edad|NPCKP17_FINAL", regla_r)
  )))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (length(vars) == 0) {
    return(NA_character_)
  }
  paste(vars, collapse = ", ")
}

.diag_k_expandir_aliases_regla <- function(regla_r, variable_k23_final) {
  regla_r <- as.character(regla_r)
  ocupado <- paste0(variable_k23_final, " %in% c(1,2,3,4,5,6,7,8)")
  reemplazos <- c(
    "no ocupado" = paste0("edad >= 10 & !(", ocupado, ")"),
    "asalariado" = paste0(variable_k23_final, " %in% c(1,2,3,7)"),
    "independiente" = paste0(variable_k23_final, " %in% c(4,5,8)"),
    "ocupado" = ocupado
  )

  for (alias in names(reemplazos)) {
    regla_r <- gsub(
      paste0("(?<![A-Za-z0-9_])", alias, "(?![A-Za-z0-9_])"),
      paste0("(", reemplazos[[alias]], ")"),
      regla_r,
      perl = TRUE
    )
  }

  regla_r
}

.diag_k_auditar_duplicados <- function(K, llaves_persona) {
  dup <- K %>%
    dplyr::count(dplyr::across(dplyr::all_of(llaves_persona)), name = "n_registros") %>%
    dplyr::filter(.data$n_registros > 1) %>%
    dplyr::arrange(dplyr::desc(.data$n_registros))

  list(
    n_filas = nrow(K),
    n_unicas = nrow(dplyr::distinct(K, dplyr::across(dplyr::all_of(llaves_persona)))),
    n_duplicados = sum(dup$n_registros - 1L),
    duplicados = dup
  )
}

.diag_k_agregar_edad <- function(K, data_edad, llaves_persona) {
  candidatos_edad <- c("edad", "NPCEP4", "Edad", "EDAD")
  edad_en_k <- col_first_existing(K, candidatos_edad)
  if (!is.null(edad_en_k)) {
    K$edad <- suppressWarnings(as.numeric(K[[edad_en_k]]))
    return(K)
  }
  if (is.null(data_edad) || !is.data.frame(data_edad)) {
    K$edad <- NA_real_
    return(K)
  }
  if (length(setdiff(llaves_persona, names(data_edad))) > 0) {
    K$edad <- NA_real_
    return(K)
  }

  edad_origen <- col_first_existing(data_edad, candidatos_edad)
  if (is.null(edad_origen)) {
    K$edad <- NA_real_
    return(K)
  }

  edad_norm <- data_edad %>%
    normalize_keys(llaves_persona) %>%
    dplyr::select(dplyr::all_of(c(llaves_persona, edad_origen))) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves_persona)), .keep_all = TRUE)
  names(edad_norm)[names(edad_norm) == edad_origen] <- "edad"
  edad_norm$edad <- suppressWarnings(as.numeric(edad_norm$edad))

  K %>% dplyr::left_join(edad_norm, by = llaves_persona)
}

.diag_k_construir_reglas <- function(data,
                                     nodos_paquete,
                                     variable_k23_final,
                                     incluir_texto_libre = FALSE) {
  n <- nrow(data)
  reglas <- list()
  orden_flujo <- 0L
  add <- function(variable, bloque, debe, regla_aplicada,
                  fuente_regla = "flujo_teorico_capitulo_k", texto_libre = FALSE,
                  condicion_debe_responder = NULL, variables_previas_usadas = NULL,
                  regla_r = NULL, universo_base = "Personas de 10 anos o mas",
                  comentario = NA_character_) {
    orden_flujo <<- orden_flujo + 1L
    regla_r <- .diag_k_null(regla_r, regla_aplicada)
    regla_r <- .diag_k_expandir_aliases_regla(regla_r, variable_k23_final)
    condicion_debe_responder <- .diag_k_null(
      condicion_debe_responder,
      paste0(variable, " debe responder si ", regla_aplicada, ".")
    )
    variables_previas_usadas <- .diag_k_null(
      variables_previas_usadas,
      .diag_k_extraer_variables_regla(regla_r)
    )
    fuente_visible <- .diag_k_fuente_visible(fuente_regla, regla_r)

    reglas[[length(reglas) + 1L]] <<- list(
      orden_flujo = orden_flujo,
      variable = variable,
      bloque = bloque,
      debe = .diag_k_logical(debe, n),
      pregunta = NA_character_,
      universo_base = universo_base,
      condicion_debe_responder = condicion_debe_responder,
      variables_previas_usadas = variables_previas_usadas,
      regla_r = regla_r,
      regla_aplicada = regla_r,
      fuente_regla = fuente_visible,
      comentario = comentario,
      texto_libre = isTRUE(texto_libre)
    )
  }
  add_many <- function(vars, bloque, debe, regla_aplicada,
                       fuente_regla = "flujo_teorico_capitulo_k") {
    for (var in vars) add(var, bloque, debe, regla_aplicada, fuente_regla)
  }

  edad <- suppressWarnings(as.numeric(data$edad))
  universo_k <- dplyr::if_else(!is.na(edad), edad >= 10, NA)
  eq <- function(var, valor) .diag_k_eq(data, var, valor)
  in_set <- function(var, valores) .diag_k_in(data, var, valores)
  lt <- function(var1, var2) .diag_k_compare(data, var1, var2, `<`)
  gt_val <- function(x, valor) dplyr::if_else(!is.na(x), x > valor, NA)
  nod <- function(nombre) {
    if (!is.null(nodos_paquete) && nombre %in% names(nodos_paquete)) {
      return(.diag_k_logical(nodos_paquete[[nombre]], n))
    }
    rep(NA, n)
  }

  debe_npckp2_1 <- .diag_k_and(universo_k, eq("NPCKP1", 1))
  debe_npckp2 <- .diag_k_and(universo_k, .diag_k_or(in_set("NPCKP1", c(2, 3, 4, 6)), eq("NPCKP2_1", 2)))
  debe_npckp3 <- .diag_k_and(universo_k, eq("NPCKP2", 2))
  debe_npckp5_1 <- .diag_k_and(universo_k, eq("NPCKP3", 1))
  debe_npckp6_1 <- .diag_k_and(universo_k, in_set("NPCKP5_1", 5:8))
  debe_npckp4 <- .diag_k_and(universo_k, .diag_k_or(eq("NPCKP3", 2), in_set("NPCKP6_1", c(2, 3))))
  debe_npckp5 <- .diag_k_and(universo_k, eq("NPCKP4", 2))
  debe_npckp6 <- .diag_k_and(universo_k, eq("NPCKP5", 1))
  debe_npckp7 <- .diag_k_and(universo_k, eq("NPCKP5", 2))
  debe_npckp8 <- .diag_k_and(universo_k, eq("NPCKP7", 1))
  debe_npckp9 <- .diag_k_and(universo_k, in_set("NPCKP8", 2:8))
  debe_npckp10 <- .diag_k_and(universo_k, eq("NPCKP9", 1))
  debe_npckp11 <- .diag_k_and(universo_k, eq("NPCKP9", 2))
  debe_npckp12 <- .diag_k_and(universo_k, .diag_k_or(eq("NPCKP10", 1), eq("NPCKP11", 1)))
  debe_npckp13 <- .diag_k_and(universo_k, in_set("NPCKP12", 1:12))

  llega_k17 <- .diag_k_and(
    universo_k,
    .diag_k_or(
      .diag_k_and(eq("NPCKP1", 1), eq("NPCKP2_1", 1)),
      .diag_k_and(.diag_k_or(in_set("NPCKP1", c(2, 3, 4, 6)), eq("NPCKP2_1", 2)), eq("NPCKP2", 1)),
      .diag_k_and(eq("NPCKP2", 2), eq("NPCKP3", 1), in_set("NPCKP5_1", 1:4)),
      .diag_k_and(eq("NPCKP2", 2), eq("NPCKP3", 1), in_set("NPCKP5_1", 5:8), eq("NPCKP6_1", 1)),
      .diag_k_and(.diag_k_or(eq("NPCKP3", 2), in_set("NPCKP6_1", c(2, 3))), eq("NPCKP4", 1))
    )
  )
  regla_llega_k17 <- paste0(
    "edad >= 10 & (",
    "(NPCKP1 == 1 & NPCKP2_1 == 1) | ",
    "((NPCKP1 %in% c(2,3,4,6) | NPCKP2_1 == 2) & NPCKP2 == 1) | ",
    "(NPCKP2 == 2 & NPCKP3 == 1 & NPCKP5_1 %in% c(1,2,3,4)) | ",
    "(NPCKP2 == 2 & NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8) & NPCKP6_1 == 1) | ",
    "((NPCKP3 == 2 | NPCKP6_1 %in% c(2,3)) & NPCKP4 == 1)",
    ")"
  )

  ocupado_k23 <- .diag_k_and(universo_k, in_set(variable_k23_final, 1:8))
  asalariado <- .diag_k_and(universo_k, in_set(variable_k23_final, c(1, 2, 3, 7)))
  independiente <- .diag_k_and(universo_k, in_set(variable_k23_final, c(4, 5, 8)))
  no_ocupado <- .diag_k_and(universo_k, .diag_k_not(ocupado_k23))

  llega_k62 <- nod("llega_K62")
  llega_k63 <- nod("llega_K63")
  llega_k66 <- nod("llega_K66")
  llega_k67 <- nod("llega_K67")
  llega_k68 <- nod("llega_K68")
  llega_k69 <- nod("llega_K69")
  llega_k70 <- nod("llega_K70")
  llega_k71 <- nod("llega_K71")
  llega_k72 <- nod("llega_K72")
  llega_k73 <- nod("llega_K73")
  llega_k77 <- nod("llega_K77")

  add("NPCKP1", "01_entrada_actividad", universo_k, "edad >= 10")
  add("NPCKP1A", "01_entrada_actividad", .diag_k_and(universo_k, eq("NPCKP1", 6)), "NPCKP1 == 6", texto_libre = TRUE)
  add("NPCKP2_1", "01_entrada_actividad", debe_npckp2_1, "NPCKP1 == 1")
  add("NPCKP2", "01_entrada_actividad", debe_npckp2, "NPCKP1 %in% c(2,3,4,6) | NPCKP2_1 == 2")
  add("NPCKP3", "01_entrada_actividad", debe_npckp3, "NPCKP2 == 2")
  add("NPCKP5_1", "01_entrada_actividad", debe_npckp5_1, "NPCKP3 == 1")
  add("NPCKP5_1A", "01_entrada_actividad", .diag_k_and(universo_k, eq("NPCKP5_1", 8)), "NPCKP5_1 == 8", texto_libre = TRUE)
  add("NPCKP6_1", "01_entrada_actividad", debe_npckp6_1, "NPCKP5_1 %in% c(5,6,7,8)")
  add("NPCKP4", "01_entrada_actividad", debe_npckp4, "NPCKP3 == 2 | NPCKP6_1 %in% c(2,3)")

  add("NPCKP5", "02_busqueda_empleo", debe_npckp5, "NPCKP4 == 2")
  add("NPCKP6", "02_busqueda_empleo", debe_npckp6, "NPCKP5 == 1")
  add("NPCKP6A", "02_busqueda_empleo", .diag_k_and(universo_k, eq("NPCKP6", 7)), "NPCKP6 == 7", texto_libre = TRUE)
  add("NPCKP7", "02_busqueda_empleo", debe_npckp7, "NPCKP5 == 2")
  add("NPCKP8", "02_busqueda_empleo", debe_npckp8, "NPCKP7 == 1")
  add("NPCKP8A", "02_busqueda_empleo", .diag_k_and(universo_k, eq("NPCKP8", 13)), "NPCKP8 == 13", texto_libre = TRUE)
  add("NPCKP9", "02_busqueda_empleo", debe_npckp9, "NPCKP8 %in% c(2,3,4,5,6,7,8)")
  add("NPCKP10", "02_busqueda_empleo", debe_npckp10, "NPCKP9 == 1")
  add("NPCKP11", "02_busqueda_empleo", debe_npckp11, "NPCKP9 == 2")
  add("NPCKP12", "02_busqueda_empleo", debe_npckp12, "NPCKP10 == 1 | NPCKP11 == 1")
  add("NPCKP13", "02_busqueda_empleo", debe_npckp13, "NPCKP12 >= 1 & NPCKP12 <= 12")

  add_many(c("NPCKP14", "NPCKP15", "NPCKP16", "NPCKP18"), "03_ocupados_pre_k23", llega_k17, "llega_K17 == TRUE")
  add("NPCKP19", "03_ocupados_pre_k23", .diag_k_and(llega_k17, eq("NPCKP18", 1)), "llega_K17 & NPCKP18 == 1")
  add("NPCKP20", "03_ocupados_pre_k23", .diag_k_and(llega_k17, eq("NPCKP19", 2)), "llega_K17 & NPCKP19 == 2")
  add("NPCKP20A", "03_ocupados_pre_k23", .diag_k_and(llega_k17, eq("NPCKP20", 2)), "llega_K17 & NPCKP20 == 2", texto_libre = TRUE)
  add(
    variable_k23_final,
    "04_k23_posicion_ocupacional",
    llega_k17,
    "llega_K17 == TRUE",
    condicion_debe_responder = paste0(variable_k23_final, " debe responder si llega_K17 == TRUE."),
    variables_previas_usadas = "llega_K17, NPCKP1, NPCKP2_1, NPCKP2, NPCKP3, NPCKP5_1, NPCKP6_1, NPCKP4",
    regla_r = regla_llega_k17,
    comentario = "K23 se audita como nodo de flujo; no se marca como candidata de imputacion porque NPCKP17_FINAL se toma como insumo cierto."
  )
  add("NPCKP17A", "04_k23_posicion_ocupacional", .diag_k_and(universo_k, eq(variable_k23_final, 8)), paste0(variable_k23_final, " == 8"), texto_libre = TRUE)

  add_many(c(
    "NPCKP22", "NPCKP25_1", "NPCKP23", "NPCKP23A", "NPCKP24", "NPCKP25",
    "NPCKP26", "NPCKP27", "NPCKP28", "NPCKP29", "NPCKP30", "NPCKP31",
    "NPCKP32", "NPCKP33", "NPCKNP33A", "NPCKP34A", "NPCKP34B",
    "NPCKP34C", "NPCKP34D", "NPCKP34E", "NPCKP35A", "NPCKP35_A",
    "NPCKP35_C", "NPCKP35_D", "NPCKP35_E"
  ), "05_rama_asalariados", asalariado, paste0(variable_k23_final, " %in% c(1,2,3,7)"))
  for (par in list(
    c("NPCKP22_1A", "NPCKP22", "6"), c("NPCKP24A", "NPCKP24", "1"), c("NPCKP24B", "NPCKP24", "1"),
    c("NPCKP25A", "NPCKP25", "1"), c("NPCKP26A", "NPCKP26", "1"), c("NPCKP27A", "NPCKP27", "1"),
    c("NPCKP28A", "NPCKP28", "1"), c("NPCKP29A", "NPCKP29", "1"), c("NPCKP30A", "NPCKP30", "1"),
    c("NPCKP31A", "NPCKP31", "1"), c("NPCKP32A", "NPCKP32", "1"), c("NPCKP33A", "NPCKP33", "1"),
    c("NPCKP33A1", "NPCKP33", "1"), c("NPCKP33AA", "NPCKNP33A", "1"), c("NPCKP33AB", "NPCKNP33A", "1"),
    c("NPCKP34AA", "NPCKP34A", "1"), c("NPCKP34BA", "NPCKP34B", "1"), c("NPCKP34CA", "NPCKP34C", "1"),
    c("NPCKP34DA", "NPCKP34D", "1"), c("NPCKP34EA", "NPCKP34E", "1"), c("NPCKP35AA", "NPCKP35A", "1")
  )) {
    add(par[1], "05_rama_asalariados", .diag_k_and(asalariado, eq(par[2], as.numeric(par[3]))), paste0("asalariado & ", par[2], " == ", par[3]))
  }

  add_many(c("NPCKP36", "NPCKP36A", "NPCKP37", "NPCKP43_1", "NPCKP44_1"),
           "06_rama_independientes", independiente, paste0(variable_k23_final, " %in% c(4,5,8)"))
  add("NPCKP43_1A", "06_rama_independientes", .diag_k_and(independiente, eq("NPCKP43_1", 1)), "independiente & NPCKP43_1 == 1", texto_libre = TRUE)
  add("NPCKP44_1A", "06_rama_independientes", .diag_k_and(independiente, eq("NPCKP44_1", 11)), "independiente & NPCKP44_1 == 11", texto_libre = TRUE)

  add_many(c(
    "NPCKP38A", "NPCKP38B", "NPCKP39", "NPCKP41", "NPCKP43", "NPCKP44",
    "NPCKP44A", "NPCKP45A", "NPCKP45B", "NPCKP45C", "NPCKP45D",
    "NPCKP45E", "NPCKP45F", "NPCKP45G", "NPCKP45H", "NPCKP45I",
    "NPCKP45J", "NPCKP45K", "NPCKP45L", "NPCKP45M", "NPCKP45N",
    "NPCKP45Q", "NPCKP45O", "NPCKP46B", "NPCKPA46", "NPCKP47", "NPCKNP48"
  ), "07_bloque_comun_ocupados", ocupado_k23, paste0(variable_k23_final, " %in% c(1,2,3,4,5,6,7,8)"))
  add("NPCKP40", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, lt("NPCKP39", 40)), "ocupado & NPCKP39 < 40")
  add("NPCKP40A", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, eq("NPCKP40", 3)), "ocupado & NPCKP40 == 3", texto_libre = TRUE)
  add("NPCKP42", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, lt("NPCKP41", "NPCKP39")), "ocupado & NPCKP41 < NPCKP39")
  add("NPCKP42A", "07_bloque_comun_ocupados", rep(NA, n), "Condicion de otra razon de NPCKP42 no codificada", "flujo_indeterminado_por_diccionario", texto_libre = TRUE)
  add("NPCKP44A1", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, eq("NPCKP44A", 11)), "ocupado & NPCKP44A == 11", texto_libre = TRUE)
  add("NPCKP46AB", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, eq("NPCKPA46", 2)), "ocupado & NPCKPA46 == 2")
  add("NPCKP46AC", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, eq("NPCKPA46", 2)), "ocupado & NPCKPA46 == 2")
  add("NPCKP47A", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, eq("NPCKP47", 1)), "ocupado & NPCKP47 == 1")
  add_many(c("NPCKNP48B", "NPCKNP48C", "NPCKNP48D"), "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, eq("NPCKNP48", 1)), "ocupado & NPCKNP48 == 1")
  add("NPCKNP48D1", "07_bloque_comun_ocupados", .diag_k_and(ocupado_k23, .diag_k_or(eq("NPCKNP48B", 1), eq("NPCKNP48C", 1), eq("NPCKNP48D", 1))), "ocupado & (NPCKNP48B == 1 | NPCKNP48C == 1 | NPCKNP48D == 1)")

  add("NPCKP47B", "08_no_ocupados_posterior", .diag_k_and(universo_k, eq("NPCKP13", 1)), "NPCKP13 == 1")
  add("NPCKP47C", "08_no_ocupados_posterior", no_ocupado, paste0("edad >= 10 & !(", variable_k23_final, " %in% c(1,2,3,4,5,6,7,8))"))
  add_many(c("NPCKP60_1", "NPCKP60_2", "NPCKP61_1"), "08_no_ocupados_posterior", .diag_k_and(no_ocupado, eq("NPCKP47C", 1)), "no ocupado & NPCKP47C == 1")
  add("NPCKP61_2", "08_no_ocupados_posterior", .diag_k_and(no_ocupado, eq("NPCKP61_1", 9)), "no ocupado & NPCKP61_1 == 9", texto_libre = TRUE)
  add("NPCKP48", "08_no_ocupados_posterior", llega_k62, "llega_K62 calculado por construir_nodos_flujo_k()", "construir_nodos_flujo_k")
  add("NPCKP48A", "08_no_ocupados_posterior", .diag_k_and(llega_k62, eq("NPCKP48", 1)), "llega_K62 & NPCKP48 == 1", "construir_nodos_flujo_k")

  add_many(c("NPCKP50_A", "NPCKP50_B"), "09_pensiones_ingresos", llega_k63, "llega_K63 calculado por construir_nodos_flujo_k()", "construir_nodos_flujo_k")
  add("NPCKP50", "09_pensiones_ingresos", .diag_k_and(universo_k, gt_val(edad, 14)), "edad > 14")
  add("NPCKP51", "09_pensiones_ingresos", .diag_k_and(universo_k, eq("NPCKP50", 1)), "NPCKP50 == 1")
  for (par in list(
    c("NPCKP52", "llega_K66", "NPCKP52A"), c("NPCKP53", "llega_K67", "NPCKP53A"),
    c("NPCKP54", "llega_K68", "NPCKP54A"), c("NPCKP55", "llega_K69", "NPCKP55A"),
    c("NPCKP56", "llega_K70", "NPCKP56A"), c("NPCKP57", "llega_K71", "NPCKP57A"),
    c("NPCKP58", "llega_K72", "NPCKP58A")
  )) {
    llega <- nod(par[2])
    add(par[1], "09_pensiones_ingresos", llega, paste0(par[2], " calculado por construir_nodos_flujo_k()"), "construir_nodos_flujo_k")
    add(par[3], "09_pensiones_ingresos", .diag_k_and(llega, eq(par[1], 1)), paste0(par[2], " & ", par[1], " == 1"), "construir_nodos_flujo_k")
  }
  add("NPCKP56B", "09_pensiones_ingresos", .diag_k_and(llega_k70, eq("NPCKP56", 1)), "llega_K70 & NPCKP56 == 1", "construir_nodos_flujo_k")

  add("NPCKP73_1", "10_emprendimiento_renta", llega_k73, "llega_K73 calculado por construir_nodos_flujo_k()", "construir_nodos_flujo_k")
  add_many(c("NPCKP73_1A", "NPCKP74_1", "NPCKP75_1"), "10_emprendimiento_renta", .diag_k_and(llega_k73, eq("NPCKP73_1", 1)), "llega_K73 & NPCKP73_1 == 1")
  add("NPCKPN62A", "10_emprendimiento_renta", .diag_k_and(universo_k, gt_val(edad, 17)), "edad >= 18")
  add("NPCKPN62B", "10_emprendimiento_renta", .diag_k_and(universo_k, eq("NPCKPN62A", 1)), "NPCKPN62A == 1", texto_libre = TRUE)

  vars_labores <- c("NPCKP59A", "NPCKP59B", "NPCKP59C", "NPCKP59D", "NPCKP59E", "NPCKP59F", "NPCKP59G", "NPCKP59H", "NPCKP59I", "NPCKP59J")
  add_many(vars_labores, "11_labores_no_remuneradas", llega_k77, "llega_K77 calculado por construir_nodos_flujo_k()", "construir_nodos_flujo_k")
  horas <- c(NPCKP59A = "NPCKP59AA", NPCKP59B = "NPCKP59BA", NPCKP59C = "NPCKP59CA", NPCKP59D = "NPCKP59DA", NPCKP59E = "NPCKP59EA", NPCKP59F = "NPCKP59FA", NPCKP59G = "NPCKP59GA", NPCKP59H = "NPCKP59HA", NPCKP59I = "NPCKP59IA", NPCKP59J = "NPCKP59JA")
  for (base in names(horas)) {
    add(horas[[base]], "11_labores_no_remuneradas", .diag_k_and(llega_k77, eq(base, 1)), paste0("llega_K77 & ", base, " == 1"), "construir_nodos_flujo_k")
  }

  add_many(c("NPCKP78_1", "NPCKP78_2"), "12_acoso_laboral", ocupado_k23, paste0(variable_k23_final, " %in% c(1,2,3,4,5,6,7,8)"))

  reglas
}

.diag_k_filtrar_o_completar_reglas <- function(reglas, vars_cap_k, n) {
  vars_reglas <- vapply(reglas, `[[`, character(1), "variable")
  reglas_filtradas <- reglas[vars_reglas %in% vars_cap_k]
  faltan_regla <- setdiff(vars_cap_k, vars_reglas)
  orden_base <- if (length(reglas_filtradas) > 0) {
    max(vapply(reglas_filtradas, `[[`, integer(1), "orden_flujo"), na.rm = TRUE)
  } else {
    0L
  }

  for (var in faltan_regla) {
    orden_base <- orden_base + 1L
    reglas_filtradas[[length(reglas_filtradas) + 1L]] <- list(
      orden_flujo = orden_base,
      variable = var,
      bloque = "sin_regla_flujo",
      debe = rep(NA, n),
      pregunta = NA_character_,
      universo_base = "No determinado",
      condicion_debe_responder = paste0(var, " no tiene regla de flujo implementada en esta version."),
      variables_previas_usadas = NA_character_,
      regla_r = "NA",
      regla_aplicada = "NA",
      fuente_regla = "Regla aproximada pendiente de validación",
      comentario = "Variable solicitada en `vars_cap_k`, pero sin regla de flujo implementada.",
      texto_libre = FALSE
    )
  }
  reglas_filtradas
}

.diag_k_reglas_tbl <- function(reglas, data, diccionario = NULL) {
  out <- tibble::tibble(
    orden_flujo = vapply(reglas, `[[`, integer(1), "orden_flujo"),
    bloque = vapply(reglas, `[[`, character(1), "bloque"),
    variable = vapply(reglas, `[[`, character(1), "variable"),
    pregunta = vapply(reglas, `[[`, character(1), "pregunta"),
    universo_base = vapply(reglas, `[[`, character(1), "universo_base"),
    condicion_debe_responder = vapply(reglas, `[[`, character(1), "condicion_debe_responder"),
    variables_previas_usadas = vapply(reglas, `[[`, character(1), "variables_previas_usadas"),
    regla_r = vapply(reglas, `[[`, character(1), "regla_r"),
    fuente_regla = vapply(reglas, `[[`, character(1), "fuente_regla"),
    comentario = vapply(reglas, `[[`, character(1), "comentario"),
    texto_libre = vapply(reglas, `[[`, logical(1), "texto_libre"),
    regla_aplicada = vapply(reglas, `[[`, character(1), "regla_aplicada")
  ) %>%
    dplyr::mutate(variable_presente = .data$variable %in% names(data))

  desc <- .diag_k_descripciones_diccionario(diccionario)
  if (nrow(desc) > 0) {
    out <- out %>% dplyr::left_join(desc, by = "variable")
  } else {
    out$descripcion_diccionario <- NA_character_
  }
  out %>%
    dplyr::mutate(
      pregunta = dplyr::coalesce(.data$descripcion_diccionario, .data$pregunta)
    ) %>%
    dplyr::select(
      .data$orden_flujo,
      .data$bloque,
      .data$variable,
      .data$pregunta,
      .data$universo_base,
      .data$condicion_debe_responder,
      .data$variables_previas_usadas,
      .data$regla_r,
      .data$fuente_regla,
      .data$comentario,
      .data$variable_presente,
      .data$texto_libre,
      .data$regla_aplicada
    ) %>%
    dplyr::distinct(.data$variable, .keep_all = TRUE) %>%
    dplyr::arrange(.data$orden_flujo)
}

.diag_k_descripciones_diccionario <- function(diccionario) {
  if (is.null(diccionario) || !is.data.frame(diccionario)) {
    return(tibble::tibble(variable = character(), descripcion_diccionario = character()))
  }
  dic <- tibble::as_tibble(diccionario) %>%
    .diag_k_normalizar_utf8_df()
  var_col <- col_first_existing(dic, c("variable", "Variable", "VARIABLE", "var", "VAR"))
  desc_col <- col_first_existing(dic, c("pregunta", "Pregunta", "PREGUNTA", "descripcion", "DESCRIPCION", "descripcion_regla"))
  if (is.null(var_col) || is.null(desc_col)) {
    return(tibble::tibble(variable = character(), descripcion_diccionario = character()))
  }
  dic %>%
    dplyr::transmute(
      variable = stringr::str_trim(as.character(.data[[var_col]])),
      descripcion_diccionario = stringr::str_squish(as.character(.data[[desc_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$variable), nzchar(.data$variable)) %>%
    dplyr::distinct(.data$variable, .keep_all = TRUE)
}

.diag_k_construir_largo <- function(data, reglas, variable_k23_final) {
  filas <- lapply(reglas, function(regla) {
    variable <- regla$variable
    presente <- variable %in% names(data)
    debe <- .diag_k_logical(regla$debe, nrow(data))
    valor <- if (presente) data[[variable]] else rep(NA, nrow(data))
    tiene_respuesta <- if (presente) .diag_k_tiene_respuesta_vector(valor) else rep(FALSE, nrow(data))
    es_k23 <- identical(variable, variable_k23_final)
    vacio_critico <- debe %in% TRUE & !tiene_respuesta & presente & !es_k23
    respuesta_fuera_flujo <- debe %in% FALSE & tiene_respuesta & presente
    estado_flujo <- dplyr::case_when(
      !presente ~ "Variable ausente en base",
      is.na(debe) ~ "Flujo indeterminado",
      debe %in% TRUE & tiene_respuesta ~ "Respondio cuando debia responder",
      debe %in% TRUE & !tiene_respuesta & es_k23 ~ "Flujo indeterminado",
      debe %in% TRUE & !tiene_respuesta ~ "Vacio critico: debia responder",
      debe %in% FALSE & tiene_respuesta ~ "Respuesta fuera de flujo",
      debe %in% FALSE & !tiene_respuesta ~ "Salto valido / no debia responder",
      TRUE ~ "Flujo indeterminado"
    )

    tibble::tibble(
      orden_flujo = regla$orden_flujo,
      DIRECTORIO = data$DIRECTORIO,
      SECUENCIA_P = data$SECUENCIA_P,
      ORDEN = data$ORDEN,
      id_persona = data$id_persona,
      edad = data$edad,
      universo_k = dplyr::if_else(!is.na(data$edad), data$edad >= 10, NA),
      NPCKP17_FINAL = data$NPCKP17_FINAL,
      variable = variable,
      bloque = regla$bloque,
      valor_original = as.character(valor),
      debe_responder = debe,
      tiene_respuesta = tiene_respuesta,
      estado_flujo = estado_flujo,
      vacio_critico = vacio_critico,
      respuesta_fuera_flujo = respuesta_fuera_flujo,
      candidata_imputacion = vacio_critico & !es_k23,
      regla_aplicada = regla$regla_aplicada,
      fuente_regla = regla$fuente_regla
    )
  })
  dplyr::bind_rows(filas)
}

.diag_k_auditoria_por_pregunta <- function(diagnostico_persona_variable, reglas_tbl) {
  ejemplos_cols <- c(
    "DIRECTORIO", "SECUENCIA_P", "ORDEN", "edad", "NPCKP17_FINAL",
    "valor_original", "estado_flujo"
  )

  filas <- diagnostico_persona_variable %>%
    dplyr::group_by(.data$orden_flujo, .data$bloque, .data$variable, .data$regla_aplicada) %>%
    dplyr::group_split()

  purrr::map_dfr(filas, function(df) {
    tibble::tibble(
      orden_flujo = df$orden_flujo[[1]],
      bloque = df$bloque[[1]],
      variable = df$variable[[1]],
      regla_aplicada = df$regla_aplicada[[1]],
      n_personas_total = nrow(df),
      n_debe_responder = sum(df$debe_responder %in% TRUE, na.rm = TRUE),
      n_no_debe_responder = sum(df$debe_responder %in% FALSE, na.rm = TRUE),
      n_flujo_indeterminado = sum(is.na(df$debe_responder), na.rm = TRUE),
      n_respondio_debia = sum(df$estado_flujo == "Respondio cuando debia responder", na.rm = TRUE),
      n_vacio_critico = sum(df$vacio_critico, na.rm = TRUE),
      n_salto_valido = sum(df$estado_flujo == "Salto valido / no debia responder", na.rm = TRUE),
      n_respuesta_fuera_flujo = sum(df$respuesta_fuera_flujo, na.rm = TRUE),
      ejemplos_vacios_criticos = list(
        df %>%
          dplyr::filter(.data$vacio_critico) %>%
          dplyr::select(dplyr::all_of(ejemplos_cols)) %>%
          dplyr::slice_head(n = 10)
      ),
      ejemplos_fuera_flujo = list(
        df %>%
          dplyr::filter(.data$respuesta_fuera_flujo) %>%
          dplyr::select(dplyr::all_of(ejemplos_cols)) %>%
          dplyr::slice_head(n = 10)
      )
    )
  }) %>%
    dplyr::mutate(
      pct_vacio_critico_sobre_debia = dplyr::if_else(
        .data$n_debe_responder > 0,
        .data$n_vacio_critico / .data$n_debe_responder,
        NA_real_
      )
    ) %>%
    dplyr::left_join(
      reglas_tbl %>%
        dplyr::select(.data$orden_flujo, .data$variable, regla_r = .data$regla_r),
      by = c("orden_flujo", "variable")
    ) %>%
    dplyr::select(
      .data$orden_flujo,
      .data$bloque,
      .data$variable,
      .data$regla_r,
      .data$n_personas_total,
      .data$n_debe_responder,
      .data$n_no_debe_responder,
      .data$n_flujo_indeterminado,
      .data$n_respondio_debia,
      .data$n_vacio_critico,
      .data$n_salto_valido,
      .data$n_respuesta_fuera_flujo,
      .data$pct_vacio_critico_sobre_debia,
      .data$ejemplos_vacios_criticos,
      .data$ejemplos_fuera_flujo
    ) %>%
    dplyr::arrange(.data$orden_flujo)
}

.diag_k_tiene_respuesta_vector <- function(x) {
  x_chr <- .diag_k_normalizar_utf8_vector(as.character(x))
  x_chr <- stringr::str_trim(x_chr)
  !is.na(x) & !is.na(x_chr) & nzchar(x_chr)
}

.diag_k_num <- function(data, var) {
  if (length(var) == 1 && is.numeric(var)) return(rep(as.numeric(var), nrow(data)))
  if (length(var) == 1 && is.character(var) && var %in% names(data)) {
    return(suppressWarnings(as.numeric(data[[var]])))
  }
  if (length(var) == nrow(data)) return(suppressWarnings(as.numeric(var)))
  rep(NA_real_, nrow(data))
}

.diag_k_eq <- function(data, var, valor) {
  x <- .diag_k_num(data, var)
  out <- x == valor
  out[is.na(x)] <- NA
  out
}

.diag_k_in <- function(data, var, valores) {
  x <- .diag_k_num(data, var)
  out <- x %in% valores
  out[is.na(x)] <- NA
  out
}

.diag_k_compare <- function(data, var1, var2, op) {
  x <- .diag_k_num(data, var1)
  y <- .diag_k_num(data, var2)
  out <- op(x, y)
  out[is.na(x) | is.na(y)] <- NA
  out
}

.diag_k_logical <- function(x, n) {
  if (length(x) == 1L) x <- rep(x, n)
  if (length(x) != n) {
    stop("Una regla de flujo produjo longitud ", length(x), " pero K tiene ", n, " filas.")
  }
  as.logical(x)
}

.diag_k_and <- function(...) {
  flags <- list(...)
  n <- max(lengths(flags))
  flags <- lapply(flags, .diag_k_logical, n = n)
  mat <- do.call(cbind, flags)
  hay_false <- rowSums(mat == FALSE, na.rm = TRUE) > 0
  todos_true <- rowSums(mat == TRUE, na.rm = TRUE) == ncol(mat)
  out <- rep(NA, nrow(mat))
  out[hay_false] <- FALSE
  out[todos_true] <- TRUE
  out
}

.diag_k_or <- function(...) {
  flags <- list(...)
  n <- max(lengths(flags))
  flags <- lapply(flags, .diag_k_logical, n = n)
  mat <- do.call(cbind, flags)
  hay_true <- rowSums(mat == TRUE, na.rm = TRUE) > 0
  todos_false <- rowSums(mat == FALSE, na.rm = TRUE) == ncol(mat)
  out <- rep(NA, nrow(mat))
  out[hay_true] <- TRUE
  out[todos_false] <- FALSE
  out
}

.diag_k_not <- function(flag) {
  flag <- as.logical(flag)
  out <- !flag
  out[is.na(flag)] <- NA
  out
}
