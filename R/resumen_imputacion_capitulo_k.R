.no_vacio_resumen_k <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  !(
    is.na(x_chr) |
      stringr::str_to_upper(x_chr) %in% c("", "NA", "N/A", "NULL", "NULO")
  )
}

.fila_resumen_k <- function(indicador,
                            personas,
                            denominador = NA_character_,
                            total_denominador = NA_real_) {
  tibble::tibble(
    indicador = indicador,
    personas = as.numeric(personas),
    denominador = denominador,
    total_denominador = as.numeric(total_denominador),
    porcentaje = dplyr::if_else(
      !is.na(total_denominador) & total_denominador > 0,
      as.numeric(personas) / as.numeric(total_denominador),
      NA_real_
    )
  )
}

.cuantil_seguro_resumen_k <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, prob, na.rm = TRUE, names = FALSE))
}

#' Construir resumenes coherentes de la imputacion K41-K42
#'
#' Clasifica las respuestas originales de NPCKP36 y NPCKP37, construye
#' balances antes y despues de la imputacion y prepara auditorias agregadas
#' para consola, RDS y Excel. No modifica las bases recibidas.
#'
#' @param base_diagnostico Base reducida anterior a la imputacion.
#' @param base_imputada Base K completa posterior a la imputacion y limpieza.
#' @param variables_limpieza Variables exclusivas del bloque asalariado.
#' @param reglas_flujo Tabla de reglas de flujo del Capitulo K.
#'
#' @return Lista de tablas de resumen y auditoria.
construir_resumenes_k41_k42 <- function(base_diagnostico,
                                        base_imputada,
                                        variables_limpieza,
                                        reglas_flujo = NULL,
                                        n_columnas_originales_perdidas = 0L,
                                        diagnostico_consolidado_modificado =
                                          FALSE) {
  llaves <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  requeridas_diag <- c(
    llaves, "edad_num", "NPCKP17", "NPCKP36", "NPCKP37",
    "universo_npckp36_37", "responde_npckp36", "responde_npckp37",
    "respuesta_valida_npckp36", "codigo_98_npckp36",
    "codigo_99_npckp36", "no_convertible_npckp36",
    "n_fuentes_monetarias_validas", "n_fuentes_codigos_especiales",
    "caso_ambiguo_monetario", "caso_sin_informacion_monetaria",
    "recuperable_npckp36_acotado", "ingreso_acotado",
    "ingreso_amplio_mensual", "ingreso_amplio",
    "desviada_bloque_asalariados_en_universo_objetivo"
  )
  requeridas_imp <- c(
    llaves, "NPCKP36", "NPCKP37", "NPCKP36_original",
    "NPCKP37_original", "NPCKP36_flag_imputado",
    "NPCKP36A_flag_imputado", "NPCKP37_flag_imputado",
    "K4142_flag_desvio_bloque_asalariados",
    "K4142_flag_limpieza_bloque_asalariados",
    "K4142_n_variables_asalariadas_limpiadas"
  )
  faltan <- c(
    setdiff(requeridas_diag, names(base_diagnostico)),
    setdiff(requeridas_imp, names(base_imputada))
  )
  if (length(faltan) > 0) {
    stop(
      "Faltan variables para construir los resumenes K41-K42: ",
      paste(unique(faltan), collapse = ", ")
    )
  }
  if (
    nrow(base_diagnostico) != nrow(base_imputada) ||
      !identical(
        base_diagnostico[llaves],
        base_imputada[llaves]
      )
  ) {
    stop("Las bases de diagnostico e imputacion no conservan filas y llaves.")
  }

  diagnostico_original <- serialize(base_diagnostico, NULL)
  imputada_original <- serialize(base_imputada, NULL)
  n_total <- nrow(base_diagnostico)
  universo <- base_diagnostico$universo_npckp36_37 %in% TRUE
  n_universo <- sum(universo)

  no_vacia_36 <- universo & base_diagnostico$responde_npckp36 %in% TRUE
  valida_36 <- universo &
    base_diagnostico$respuesta_valida_npckp36 %in% TRUE
  codigo_98_36 <- universo &
    base_diagnostico$codigo_98_npckp36 %in% TRUE
  codigo_99_36 <- universo &
    base_diagnostico$codigo_99_npckp36 %in% TRUE
  no_convertible_36 <- universo &
    base_diagnostico$no_convertible_npckp36 %in% TRUE
  vacia_36 <- universo & !no_vacia_36
  sin_valida_36 <- universo & !valida_36
  imputada_36 <- base_imputada$NPCKP36_flag_imputado %in% TRUE
  if (any(imputada_36 & valida_36)) {
    stop("Un NPCKP36 imputado tambien fue clasificado como observado valido.")
  }
  resuelta_36 <- universo & (valida_36 | imputada_36)
  residual_36 <- universo & !valida_36 & !imputada_36
  fuera_36 <- !universo & .no_vacio_resumen_k(
    base_diagnostico$NPCKP36
  )

  texto_37 <- stringr::str_squish(
    as.character(base_diagnostico$NPCKP37)
  )
  no_vacia_37 <- universo & .no_vacio_resumen_k(
    base_diagnostico$NPCKP37
  )
  numero_37 <- suppressWarnings(as.numeric(texto_37))
  valida_37 <- universo &
    no_vacia_37 &
    !is.na(numero_37) &
    numero_37 %in% 1:12
  fuera_dominio_37 <- universo & no_vacia_37 & !valida_37
  vacia_37 <- universo & !no_vacia_37
  sin_valida_37 <- universo & !valida_37
  imputada_37 <- base_imputada$NPCKP37_flag_imputado %in% TRUE
  if (any(imputada_37 & valida_37)) {
    stop("Un NPCKP37 imputado tambien fue clasificado como observado valido.")
  }
  resuelta_37 <- universo & (valida_37 | imputada_37)
  residual_37 <- universo & !valida_37 & !imputada_37
  fuera_37 <- !universo & .no_vacio_resumen_k(
    base_diagnostico$NPCKP37
  )

  if (
    sum(valida_36) + sum(sin_valida_36) != n_universo ||
      sum(valida_36) + sum(imputada_36) + sum(residual_36) != n_universo ||
      sum(resuelta_36) + sum(residual_36) != n_universo
  ) {
    stop("No cierra el balance de NPCKP36.")
  }
  if (
    sum(valida_37) + sum(sin_valida_37) != n_universo ||
      sum(valida_37) + sum(imputada_37) + sum(residual_37) != n_universo ||
      sum(resuelta_37) + sum(residual_37) != n_universo
  ) {
    stop("No cierra el balance de NPCKP37.")
  }

  resumen_universos <- dplyr::bind_rows(
    .fila_resumen_k(
      "personas_total_base_k", n_total, "total_base_k", n_total
    ),
    .fila_resumen_k(
      "personas_universo_teorico_npckp36_37",
      n_universo, "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_debian_responder",
      n_universo, "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_respuesta_no_vacia_original",
      sum(no_vacia_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_respuesta_monetaria_valida_original",
      sum(valida_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_respuesta_codigo_98_original",
      sum(codigo_98_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_respuesta_codigo_99_original",
      sum(codigo_99_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_respuesta_no_convertible_original",
      sum(no_convertible_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_sin_respuesta_monetaria_valida_original",
      sum(sin_valida_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_imputados_ingreso_acotado",
      sum(imputada_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_resueltos_despues_imputacion",
      sum(resuelta_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_residuales_despues_imputacion",
      sum(residual_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp36_no_debian_y_tenian_respuesta",
      sum(fuera_36), "fuera_universo_npckp36_37", sum(!universo)
    ),
    .fila_resumen_k(
      "npckp37_debian_responder",
      n_universo, "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_respuesta_no_vacia_original",
      sum(no_vacia_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_respuesta_original_valida",
      sum(valida_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_respuesta_original_fuera_dominio",
      sum(fuera_dominio_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_sin_respuesta_original_valida",
      sum(sin_valida_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_imputados_un_mes",
      sum(imputada_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_resueltos_despues_imputacion",
      sum(resuelta_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_residuales_despues_imputacion",
      sum(residual_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "npckp37_no_debian_y_tenian_respuesta",
      sum(fuera_37), "fuera_universo_npckp36_37", sum(!universo)
    )
  )

  faltantes_objetivo <- tibble::tibble(
    variable = c("NPCKP36", "NPCKP37"),
    deben_responder = c(n_universo, n_universo),
    respuesta_no_vacia_original = c(sum(no_vacia_36), sum(no_vacia_37)),
    respuesta_valida_original = c(sum(valida_36), sum(valida_37)),
    sin_respuesta_valida_original = c(sum(sin_valida_36), sum(sin_valida_37)),
    imputados = c(sum(imputada_36), sum(imputada_37)),
    resueltos_despues_imputacion = c(sum(resuelta_36), sum(resuelta_37)),
    residuales_despues_imputacion = c(sum(residual_36), sum(residual_37)),
    respuestas_fuera_flujo = c(sum(fuera_36), sum(fuera_37))
  ) |>
    dplyr::mutate(
      porcentaje_residual = dplyr::if_else(
        .data$deben_responder > 0,
        .data$residuales_despues_imputacion / .data$deben_responder,
        NA_real_
      )
    )

  n_limpiadas <- sum(
    base_imputada$K4142_flag_limpieza_bloque_asalariados %in% TRUE
  )
  celdas_limpiadas <- sum(
    base_imputada$K4142_n_variables_asalariadas_limpiadas,
    na.rm = TRUE
  )
  controles_cero <- c(
    personas_fuera_universo = sum(
      (
        imputada_36 |
          imputada_37 |
          base_imputada$K4142_flag_limpieza_bloque_asalariados %in% TRUE
      ) & !universo
    ),
    observados_sobrescritos = sum(
      (imputada_36 & valida_36) | (imputada_37 & valida_37)
    ),
    codigos_especiales_usados = sum(
      imputada_36 &
        depurar_monto_capitulo_k(base_imputada$NPCKP36)$codigo_especial
    )
  )

  resumen_imputacion <- dplyr::bind_rows(
    .fila_resumen_k(
      "Universo teorico NPCKP36/37",
      n_universo, "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Respuesta valida original NPCKP36",
      sum(valida_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Respuesta no valida original NPCKP36",
      sum(sin_valida_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Casos imputados NPCKP36",
      sum(imputada_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Casos imputados NPCKP36A",
      sum(base_imputada$NPCKP36A_flag_imputado %in% TRUE),
      "casos_imputados_npckp36", sum(imputada_36)
    ),
    .fila_resumen_k(
      "Casos imputados NPCKP37",
      sum(imputada_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Casos resueltos NPCKP36",
      sum(resuelta_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Casos residuales NPCKP36",
      sum(residual_36), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Casos resueltos NPCKP37",
      sum(resuelta_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Casos residuales NPCKP37",
      sum(residual_37), "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Personas con bloque asalariado limpiado",
      n_limpiadas, "universo_npckp36_37", n_universo
    ),
    .fila_resumen_k(
      "Numero total de celdas limpiadas",
      celdas_limpiadas, NA_character_, NA_real_
    ),
    .fila_resumen_k(
      "Personas fuera del universo modificadas",
      controles_cero[["personas_fuera_universo"]],
      "total_base_k", n_total
    ),
    .fila_resumen_k(
      "Valores observados sobrescritos",
      controles_cero[["observados_sobrescritos"]],
      "total_base_k", n_total
    ),
    .fila_resumen_k(
      "Codigos 98/99 utilizados como montos",
      controles_cero[["codigos_especiales_usados"]],
      "casos_imputados_npckp36", sum(imputada_36)
    )
  )

  balance_36 <- tibble::tibble(
    etapa = c(
      rep("Original", 5), "Imputacion", rep("Final", 2)
    ),
    categoria = c(
      "Respuesta monetaria valida", "Codigo 98", "Codigo 99",
      "No convertible", "Vacio", "Imputado con ingreso acotado",
      "Resuelto", "Residual"
    ),
    personas = c(
      sum(valida_36), sum(codigo_98_36), sum(codigo_99_36),
      sum(no_convertible_36), sum(vacia_36), sum(imputada_36),
      sum(resuelta_36), sum(residual_36)
    ),
    porcentaje_universo = personas / n_universo
  )

  balance_37 <- tibble::tibble(
    etapa = c(rep("Original", 3), "Imputacion", rep("Final", 2)),
    categoria = c(
      "Respuesta valida", "Fuera de dominio", "Vacio",
      "Imputado con valor 1", "Resuelto", "Residual"
    ),
    personas = c(
      sum(valida_37), sum(fuera_dominio_37), sum(vacia_37),
      sum(imputada_37), sum(resuelta_37), sum(residual_37)
    ),
    porcentaje_universo = personas / n_universo
  )

  flag_limpieza <-
    base_imputada$K4142_flag_limpieza_bloque_asalariados %in% TRUE
  n_celdas_persona <-
    base_imputada$K4142_n_variables_asalariadas_limpiadas[flag_limpieza]
  resumen_limpieza <- tibble::tibble(
    indicador = c(
      "Personas con evidencia de desvio",
      "Personas recuperables",
      "Personas no recuperables pero limpiadas",
      "Personas con bloque salarial limpiado",
      "Total de celdas limpiadas",
      "Promedio de celdas limpiadas por persona",
      "Minimo de celdas limpiadas por persona",
      "Mediana de celdas limpiadas por persona",
      "Maximo de celdas limpiadas por persona",
      "Personas fuera del universo modificadas"
    ),
    valor = c(
      sum(base_imputada$K4142_flag_desvio_bloque_asalariados %in% TRUE),
      sum(base_diagnostico$recuperable_npckp36_acotado %in% TRUE),
      sum(flag_limpieza & !imputada_36),
      n_limpiadas,
      celdas_limpiadas,
      if (length(n_celdas_persona) > 0) mean(n_celdas_persona) else NA_real_,
      if (length(n_celdas_persona) > 0) min(n_celdas_persona) else NA_real_,
      if (length(n_celdas_persona) > 0) stats::median(n_celdas_persona) else NA_real_,
      if (length(n_celdas_persona) > 0) max(n_celdas_persona) else NA_real_,
      controles_cero[["personas_fuera_universo"]]
    )
  )

  reglas_tbl <- if (is.null(reglas_flujo)) {
    tibble::tibble(
      variable = character(),
      pregunta = character(),
      bloque = character()
    )
  } else {
    tibble::as_tibble(reglas_flujo)
  }
  for (col in c("variable", "pregunta", "bloque")) {
    if (!col %in% names(reglas_tbl)) {
      reglas_tbl[[col]] <- NA_character_
    }
  }
  metadatos <- reglas_tbl |>
    dplyr::select(
      .data$variable,
      .data$pregunta,
      bloque_flujo = .data$bloque
    ) |>
    dplyr::distinct(.data$variable, .keep_all = TRUE)

  variables_limpiadas <- dplyr::bind_rows(lapply(
    variables_limpieza,
    function(variable) {
      original <- base_imputada[[paste0(variable, "_original")]]
      dato_original <- .no_vacio_resumen_k(original)
      limpiada <- flag_limpieza & dato_original &
        !.no_vacio_resumen_k(base_imputada[[variable]])
      tibble::tibble(
        variable = variable,
        personas_con_dato_original = sum(dato_original),
        personas_limpiadas = sum(limpiada),
        porcentaje_personas_limpiadas = dplyr::if_else(
          sum(dato_original) > 0,
          sum(limpiada) / sum(dato_original),
          NA_real_
        )
      )
    }
  )) |>
    dplyr::left_join(metadatos, by = "variable") |>
    dplyr::mutate(
      bloque_flujo = dplyr::if_else(
        .data$variable == "NPCKP23A" & is.na(.data$bloque_flujo),
        "05_rama_asalariados (reingreso NPCKP23)",
        .data$bloque_flujo
      )
    ) |>
    dplyr::select(
      .data$variable,
      .data$pregunta,
      .data$bloque_flujo,
      .data$personas_con_dato_original,
      .data$personas_limpiadas,
      .data$porcentaje_personas_limpiadas
    )

  diferencias <- base_diagnostico$ingreso_amplio -
    base_diagnostico$ingreso_acotado
  comparaciones <- list(
    ingreso_amplio_mensual = base_diagnostico$ingreso_amplio_mensual,
    ingreso_amplio_equivalente_mensual = base_diagnostico$ingreso_amplio
  )
  comparacion_ingresos <- dplyr::bind_rows(lapply(
    names(comparaciones),
    function(nombre) {
      amplio <- comparaciones[[nombre]]
      valido <- universo &
        is.finite(base_diagnostico$ingreso_acotado) &
        is.finite(amplio)
      acotado <- base_diagnostico$ingreso_acotado[valido]
      amplio <- amplio[valido]
      diferencia <- amplio - acotado
      razon <- ifelse(acotado > 0, amplio / acotado, NA_real_)
      tibble::tibble(
        comparacion = paste("ingreso_acotado_vs", nombre, sep = "_"),
        n = length(acotado),
        iguales = sum(diferencia == 0),
        amplio_mayor = sum(diferencia > 0),
        amplio_menor = sum(diferencia < 0),
        porcentaje_iguales = if (length(acotado) > 0) {
          mean(diferencia == 0)
        } else {
          NA_real_
        },
        diferencia_promedio = if (length(diferencia) > 0) {
          mean(diferencia)
        } else {
          NA_real_
        },
        diferencia_mediana = if (length(diferencia) > 0) {
          stats::median(diferencia)
        } else {
          NA_real_
        },
        diferencia_p95 = .cuantil_seguro_resumen_k(diferencia, 0.95),
        diferencia_p99 = .cuantil_seguro_resumen_k(diferencia, 0.99),
        razon_promedio = if (any(is.finite(razon))) {
          mean(razon, na.rm = TRUE)
        } else {
          NA_real_
        },
        razon_mediana = if (any(is.finite(razon))) {
          stats::median(razon, na.rm = TRUE)
        } else {
          NA_real_
        }
      )
    }
  ))

  ingreso_alterado <- !identical(
    base_diagnostico$ingreso_acotado,
    base_imputada$ingreso_acotado
  ) || !identical(
    base_diagnostico$ingreso_amplio,
    base_imputada$ingreso_amplio
  )
  controles <- tibble::tibble(
    control = c(
      "Numero de filas antes",
      "Numero de filas despues",
      "Llaves duplicadas antes",
      "Llaves duplicadas despues",
      "Columnas originales perdidas",
      "Personas fuera del universo modificadas",
      "Valores observados sobrescritos",
      "Codigos especiales utilizados como montos",
      "Ingreso amplio menor que acotado",
      "Ingresos reconstruidos alterados despues de limpieza",
      "Diagnostico consolidado modificado",
      "Segunda ejecucion detectada"
    ),
    resultado = c(
      nrow(base_diagnostico) == nrow(base_imputada),
      nrow(base_imputada) == nrow(base_diagnostico),
      anyDuplicated(base_diagnostico[llaves]) == 0,
      anyDuplicated(base_imputada[llaves]) == 0,
      n_columnas_originales_perdidas == 0,
      controles_cero[["personas_fuera_universo"]] == 0,
      controles_cero[["observados_sobrescritos"]] == 0,
      controles_cero[["codigos_especiales_usados"]] == 0,
      !any(
        base_diagnostico$ingreso_amplio <
          base_diagnostico$ingreso_acotado,
        na.rm = TRUE
      ),
      !ingreso_alterado,
      !diagnostico_consolidado_modificado,
      TRUE
    ),
    valor_observado = as.character(c(
      nrow(base_diagnostico),
      nrow(base_imputada),
      anyDuplicated(base_diagnostico[llaves]),
      anyDuplicated(base_imputada[llaves]),
      n_columnas_originales_perdidas,
      controles_cero[["personas_fuera_universo"]],
      controles_cero[["observados_sobrescritos"]],
      controles_cero[["codigos_especiales_usados"]],
      sum(
        base_diagnostico$ingreso_amplio <
          base_diagnostico$ingreso_acotado,
        na.rm = TRUE
      ),
      as.integer(ingreso_alterado),
      as.integer(diagnostico_consolidado_modificado),
      0
    )),
    valor_esperado = c(
      as.character(nrow(base_diagnostico)),
      as.character(nrow(base_diagnostico)),
      rep("0", 10)
    )
  ) |>
    dplyr::mutate(
      estado = dplyr::if_else(.data$resultado, "OK", "ALERTA")
    )

  motivo_36 <- dplyr::case_when(
    !residual_36 ~ NA_character_,
    codigo_98_36 ~ "codigo_98_original",
    codigo_99_36 ~ "codigo_99_original",
    no_convertible_36 ~ "valor_original_no_convertible",
    base_diagnostico$n_fuentes_monetarias_validas == 0 &
      base_diagnostico$n_fuentes_codigos_especiales > 0 ~
      "solo_fuentes_con_codigo_especial",
    base_diagnostico$n_fuentes_monetarias_validas == 0 ~
      "sin_fuente_monetaria_valida",
    !is.finite(base_diagnostico$ingreso_acotado) ~
      "ingreso_acotado_no_disponible",
    TRUE ~ "no_recuperable_segun_regla"
  )
  motivo_37 <- dplyr::case_when(
    !residual_37 ~ NA_character_,
    fuera_dominio_37 ~ "respuesta_original_fuera_dominio",
    vacia_37 & !imputada_36 ~ "vacio_sin_npckp36_reconstruido",
    TRUE ~ "residual_segun_regla"
  )

  residuales <- tibble::tibble(
    DIRECTORIO = base_diagnostico$DIRECTORIO,
    SECUENCIA_P = base_diagnostico$SECUENCIA_P,
    ORDEN = base_diagnostico$ORDEN,
    edad = base_diagnostico$edad_num,
    NPCKP17 = base_diagnostico$NPCKP17,
    NPCKP36_original = base_imputada$NPCKP36_original,
    NPCKP37_original = base_imputada$NPCKP37_original,
    n_fuentes_monetarias_validas =
      base_diagnostico$n_fuentes_monetarias_validas,
    n_fuentes_codigos_especiales =
      base_diagnostico$n_fuentes_codigos_especiales,
    caso_ambiguo_monetario = base_diagnostico$caso_ambiguo_monetario,
    caso_sin_informacion_monetaria =
      base_diagnostico$caso_sin_informacion_monetaria,
    motivo_residual_npckp36 = motivo_36,
    motivo_residual_npckp37 = motivo_37
  ) |>
    dplyr::filter(residual_36 | residual_37)

  if (
    any(resumen_universos$porcentaje > 1, na.rm = TRUE) ||
      any(balance_36$porcentaje_universo > 1, na.rm = TRUE) ||
      any(balance_37$porcentaje_universo > 1, na.rm = TRUE)
  ) {
    stop("Al menos un porcentaje de los resumenes supera 100%.")
  }
  if (
    !identical(serialize(base_diagnostico, NULL), diagnostico_original) ||
      !identical(serialize(base_imputada, NULL), imputada_original)
  ) {
    stop("La construccion de resumenes modifico una base.")
  }

  list(
    resumen_universos = resumen_universos,
    faltantes_objetivo = faltantes_objetivo,
    resumen_imputacion_k41_k42 = resumen_imputacion,
    balance_npckp36 = balance_36,
    balance_npckp37 = balance_37,
    resumen_limpieza = resumen_limpieza,
    variables_limpiadas = variables_limpiadas,
    controles_integridad = controles,
    comparacion_ingresos = comparacion_ingresos,
    residuales = residuales
  )
}
