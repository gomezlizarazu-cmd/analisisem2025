#' Cerrar deterministicamente NPCKP37
#'
#' Materializa el numero de meses final asociado a NPCKP36. Conserva los
#' valores originales validos, preserva las imputaciones previas asociadas a
#' ingresos reconstruidos, asigna un mes por moda dominante cuando existe un
#' monto original valido y traslada los codigos especiales 98 y 99 desde
#' NPCKP36. La variable `NPCKP37_original` es inmutable.
#'
#' @param data Base K posterior al cierre residual de NPCKP36.
#'
#' @return Lista con base final, resumen, balance, auditoria, controles,
#'   distribucion de donantes y parametros.
cerrar_npckp37 <- function(data) {
  llaves <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  requeridas <- c(
    llaves,
    "NPCKP17",
    "NPCKP36",
    "NPCKP36A",
    "NPCKP36_original",
    "NPCKP36A_original",
    "NPCKP36A_flag_imputado",
    "NPCKP37",
    "NPCKP37_flag_imputado",
    "NPCKP37_metodo_imputacion",
    "NPCKP36_flag_codigo_residual",
    "NPCKP36_codigo_residual",
    "NPCKP36_origen_codigo_residual",
    "universo_npckp36_37"
  )
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables para cerrar NPCKP37: ",
      paste(faltantes, collapse = ", ")
    )
  }
  if (anyDuplicated(data[llaves]) > 0) {
    stop("La llave de persona no es unica antes del cierre de NPCKP37.")
  }

  salida <- data
  if (!"NPCKP37_original" %in% names(salida)) {
    salida$NPCKP37_original <- salida$NPCKP37
  }
  if (!"NPCKP36A_metodo_imputacion" %in% names(salida)) {
    salida$NPCKP36A_metodo_imputacion <- NA_character_
  }

  nuevas_traza <- c(
    "NPCKP37_flag_codigo_residual",
    "NPCKP37_codigo_residual",
    "NPCKP37_origen_codigo_residual"
  )
  nuevas_presentes <- intersect(nuevas_traza, names(salida))
  if (
    length(nuevas_presentes) > 0 &&
      length(nuevas_presentes) < length(nuevas_traza)
  ) {
    stop(
      "La trazabilidad del cierre NPCKP37 esta incompleta: ",
      paste(nuevas_presentes, collapse = ", ")
    )
  }
  cierre_previo <- length(nuevas_presentes) == length(nuevas_traza)

  columnas_originales <- grep(
    "_original$",
    names(salida),
    value = TRUE
  )
  originales_antes <- serialize(
    salida[columnas_originales],
    connection = NULL
  )
  npckp37_original_antes <- serialize(
    salida$NPCKP37_original,
    connection = NULL
  )
  npckp36_antes <- serialize(salida$NPCKP36, NULL)
  npckp36_original_antes <- serialize(
    salida$NPCKP36_original,
    NULL
  )
  npckp36a_original_antes <- serialize(
    salida$NPCKP36A_original,
    NULL
  )
  npckp36a_recibido <- salida$NPCKP36A
  npckp37_recibido <- salida$NPCKP37
  llaves_antes <- salida[llaves]
  n_filas_antes <- nrow(salida)

  universo <- salida$universo_npckp36_37 %in% TRUE
  texto_original_37 <- stringr::str_squish(
    as.character(salida$NPCKP37_original)
  )
  numero_original_37 <- suppressWarnings(
    as.numeric(texto_original_37)
  )
  original_37_valido <- .valor_no_vacio_imputacion_k(
    salida$NPCKP37_original
  ) &
    !is.na(numero_original_37) &
    numero_original_37 %in% 1:12
  original_37_vacio <- !.valor_no_vacio_imputacion_k(
    salida$NPCKP37_original
  )
  actual_37_vacio <- !.valor_no_vacio_imputacion_k(
    salida$NPCKP37
  )
  monto_36_original <- depurar_monto_capitulo_k(
    salida$NPCKP36_original
  )
  monto_36_final <- depurar_monto_capitulo_k(salida$NPCKP36)
  imputacion_previa <- salida$NPCKP37_flag_imputado %in% TRUE

  if (!cierre_previo) {
    if (any(universo & original_37_valido & imputacion_previa)) {
      stop("Un NPCKP37 original valido estaba marcado como imputado.")
    }
    if (any(
      universo &
        imputacion_previa &
        as.character(salida$NPCKP37) != "1",
      na.rm = TRUE
    )) {
      stop("Una imputacion previa de NPCKP37 no conserva el valor un mes.")
    }

    trasladar_98 <- universo &
      original_37_vacio &
      actual_37_vacio &
      !imputacion_previa &
      monto_36_final$codigo_98
    trasladar_99 <- universo &
      original_37_vacio &
      actual_37_vacio &
      !imputacion_previa &
      monto_36_final$codigo_99
    imputar_moda <- universo &
      monto_36_original$monto_valido &
      original_37_vacio &
      actual_37_vacio &
      !imputacion_previa &
      !monto_36_final$codigo_especial

    valor_un_mes <- .formatear_monto_tipo_original_k(
      rep(1, nrow(salida)),
      salida$NPCKP37
    )
    valor_codigo_98 <- .formatear_monto_tipo_original_k(
      rep(98, nrow(salida)),
      salida$NPCKP37
    )
    valor_codigo_99 <- .formatear_monto_tipo_original_k(
      rep(99, nrow(salida)),
      salida$NPCKP37
    )

    salida$NPCKP37_flag_codigo_residual <- FALSE
    salida$NPCKP37_codigo_residual <- NA_integer_
    salida$NPCKP37_origen_codigo_residual <- NA_character_

    salida$NPCKP37_flag_imputado[
      universo & original_37_valido
    ] <- FALSE
    salida$NPCKP37_metodo_imputacion[
      universo & original_37_valido
    ] <- NA_character_
    salida$NPCKP37_metodo_imputacion[
      universo & imputacion_previa
    ] <-
      "un_mes_por_ingreso_reconstruido"

    salida$NPCKP37[imputar_moda] <- valor_un_mes[imputar_moda]
    salida$NPCKP37_flag_imputado[imputar_moda] <- TRUE
    salida$NPCKP37_metodo_imputacion[imputar_moda] <-
      "un_mes_por_moda_dominante"

    salida$NPCKP37[trasladar_98] <- valor_codigo_98[trasladar_98]
    salida$NPCKP37[trasladar_99] <- valor_codigo_99[trasladar_99]
    salida$NPCKP37_flag_codigo_residual[
      trasladar_98 | trasladar_99
    ] <- TRUE
    salida$NPCKP37_codigo_residual[trasladar_98] <- 98L
    salida$NPCKP37_codigo_residual[trasladar_99] <- 99L
    salida$NPCKP37_origen_codigo_residual[trasladar_98] <-
      "codigo_98_trasladado_desde_npckp36"
    salida$NPCKP37_origen_codigo_residual[trasladar_99] <-
      dplyr::if_else(
        salida$NPCKP36_origen_codigo_residual[trasladar_99] ==
          "99_asignado_ausencia_total",
        "codigo_99_ausencia_total",
        "codigo_99_trasladado_desde_npckp36",
        missing = "codigo_99_trasladado_desde_npckp36"
      )
    salida$NPCKP37_metodo_imputacion[trasladar_98] <-
      "codigo_98_trasladado_desde_npckp36"
    salida$NPCKP37_metodo_imputacion[trasladar_99] <-
      salida$NPCKP37_origen_codigo_residual[trasladar_99]
  }

  texto_final_37 <- stringr::str_squish(
    as.character(salida$NPCKP37)
  )
  numero_final_37 <- suppressWarnings(as.numeric(texto_final_37))
  mes_final_valido <- .valor_no_vacio_imputacion_k(
    salida$NPCKP37
  ) &
    !is.na(numero_final_37) &
    numero_final_37 %in% 1:12
  codigo_final_37 <- salida$NPCKP37_flag_codigo_residual %in% TRUE
  codigo_98_final <- codigo_final_37 &
    salida$NPCKP37_codigo_residual == 98L
  codigo_99_final <- codigo_final_37 &
    salida$NPCKP37_codigo_residual == 99L
  metodo_reconstruido <-
    salida$NPCKP37_metodo_imputacion ==
    "un_mes_por_ingreso_reconstruido"
  metodo_reconstruido[is.na(metodo_reconstruido)] <- FALSE
  metodo_moda <-
    salida$NPCKP37_metodo_imputacion ==
    "un_mes_por_moda_dominante"
  metodo_moda[is.na(metodo_moda)] <- FALSE

  casos_moda_dominante <- universo & metodo_moda
  valor_npckp36_para_36a <- .formatear_monto_tipo_original_k(
    monto_36_final$monto,
    salida$NPCKP36A
  )
  salida$NPCKP36A[casos_moda_dominante] <-
    valor_npckp36_para_36a[casos_moda_dominante]
  salida$NPCKP36A_flag_imputado[casos_moda_dominante] <- TRUE
  salida$NPCKP36A_metodo_imputacion[casos_moda_dominante] <-
    "sincronizacion_con_npckp36_por_meses_imputados_moda"

  monto_36a_final <- depurar_monto_capitulo_k(salida$NPCKP36A)
  npckp36a_coincide_npckp36 <-
    (
      monto_36_final$monto_valido &
        monto_36a_final$monto_valido &
        monto_36_final$monto == monto_36a_final$monto
    ) |
    (monto_36_final$codigo_98 & monto_36a_final$codigo_98) |
    (monto_36_final$codigo_99 & monto_36a_final$codigo_99)
  npckp36a_coincide_npckp36[
    is.na(npckp36a_coincide_npckp36)
  ] <- FALSE
  casos_moda_sincronizados <- casos_moda_dominante &
    npckp36a_coincide_npckp36 &
    salida$NPCKP36A_flag_imputado %in% TRUE &
    salida$NPCKP36A_metodo_imputacion ==
      "sincronizacion_con_npckp36_por_meses_imputados_moda"
  casos_moda_sincronizados[is.na(casos_moda_sincronizados)] <- FALSE

  categoria <- dplyr::case_when(
    universo & original_37_valido ~ "Respuesta original valida",
    universo & metodo_reconstruido ~
      "Un mes por ingreso reconstruido",
    universo & metodo_moda ~ "Un mes por moda dominante",
    universo & codigo_98_final ~ "Codigo 98 trasladado",
    universo & codigo_99_final ~ "Codigo 99 trasladado o asignado",
    universo ~ "Residual sin clasificar",
    TRUE ~ NA_character_
  )
  residual_final <- universo &
    categoria == "Residual sin clasificar"
  residual_final[is.na(residual_final)] <- FALSE
  intervencion_cierre <- metodo_moda | codigo_final_37

  balance <- tibble::tibble(
    categoria = c(
      "Respuesta original valida",
      "Un mes por ingreso reconstruido",
      "Un mes por moda dominante",
      "Codigo 98 trasladado",
      "Codigo 99 trasladado o asignado",
      "Residual sin clasificar"
    ),
    personas = c(
      sum(universo & original_37_valido),
      sum(universo & metodo_reconstruido),
      sum(universo & metodo_moda),
      sum(universo & codigo_98_final),
      sum(universo & codigo_99_final),
      sum(residual_final)
    ),
    porcentaje_universo = personas / sum(universo)
  )

  resumen <- tibble::tibble(
    indicador = c(
      "Universo NPCKP36/37",
      "Respuesta original valida NPCKP37",
      "Imputaciones previas preservadas",
      "Un mes por moda dominante",
      "Codigo 98 trasladado desde NPCKP36",
      "Codigo 99 trasladado desde NPCKP36",
      "Codigo 99 por ausencia total",
      "NPCKP36A sincronizados por moda dominante",
      "Residuales finales NPCKP37"
    ),
    personas = c(
      sum(universo),
      sum(universo & original_37_valido),
      sum(universo & metodo_reconstruido),
      sum(universo & metodo_moda),
      sum(
        universo &
          salida$NPCKP37_origen_codigo_residual ==
          "codigo_98_trasladado_desde_npckp36",
        na.rm = TRUE
      ),
      sum(
        universo &
          salida$NPCKP37_origen_codigo_residual ==
          "codigo_99_trasladado_desde_npckp36",
        na.rm = TRUE
      ),
      sum(
        universo &
          salida$NPCKP37_origen_codigo_residual ==
          "codigo_99_ausencia_total",
        na.rm = TRUE
      ),
      sum(casos_moda_sincronizados),
      sum(residual_final)
    ),
    porcentaje_universo = personas / sum(universo)
  )

  donantes <- universo & original_37_valido
  distribucion_meses <- tibble::tibble(
    meses = as.integer(numero_original_37[donantes]),
    NPCKP17 = as.character(salida$NPCKP17[donantes])
  )
  distribucion_general <- distribucion_meses |>
    dplyr::count(meses, name = "personas") |>
    dplyr::mutate(
      alcance = "Total donantes",
      NPCKP17 = NA_character_,
      porcentaje = .data$personas / sum(.data$personas)
    ) |>
    dplyr::select(
      alcance,
      NPCKP17,
      meses,
      personas,
      porcentaje
    )
  distribucion_posicion <- distribucion_meses |>
    dplyr::count(NPCKP17, meses, name = "personas") |>
    dplyr::group_by(NPCKP17) |>
    dplyr::mutate(
      alcance = "Por posicion ocupacional",
      porcentaje = .data$personas / sum(.data$personas)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      alcance,
      NPCKP17,
      meses,
      personas,
      porcentaje
    )
  distribucion_meses_donantes <- dplyr::bind_rows(
    distribucion_general,
    distribucion_posicion
  )

  originales_validos_modificados <- !identical(
    salida$NPCKP37[original_37_valido],
    npckp37_recibido[original_37_valido]
  )
  imputaciones_previas_modificadas <- !identical(
    salida$NPCKP37[imputacion_previa],
    npckp37_recibido[imputacion_previa]
  )
  fuera_universo_modificados <- !identical(
    salida$NPCKP37[!universo],
    npckp37_recibido[!universo]
  )
  originales_modificados <- !identical(
    serialize(salida$NPCKP37_original, NULL),
    npckp37_original_antes
  )
  columnas_originales_perdidas <- length(
    setdiff(columnas_originales, names(salida))
  )
  valores_asignados_invalidos <- sum(
    intervencion_cierre &
      !numero_final_37 %in% c(1, 98, 99),
    na.rm = TRUE
  )
  unos_asignados_a_codigo <- sum(
    intervencion_cierre &
      numero_final_37 == 1 &
      monto_36_final$codigo_especial,
    na.rm = TRUE
  )
  codigos_asignados_a_monto <- sum(
    codigo_final_37 & monto_36_final$monto_valido,
    na.rm = TRUE
  )
  codigos_clasificados_como_meses <- sum(
    codigo_final_37 & mes_final_valido,
    na.rm = TRUE
  )
  npckp36a_modificados_fuera_moda <- !identical(
    salida$NPCKP36A[!casos_moda_dominante],
    npckp36a_recibido[!casos_moda_dominante]
  )
  inconsistencias_npckp36_npckp36a <- sum(
    universo & !npckp36a_coincide_npckp36
  )

  controles <- tibble::tibble(
    control = c(
      "Valores originales validos de NPCKP37 modificados",
      "Imputaciones previas de NPCKP37 modificadas",
      "NPCKP37_original modificados",
      "Personas fuera del universo modificadas",
      "Valores asignados diferentes de 1, 98 o 99",
      "Valor 1 asignado cuando NPCKP36 es codigo especial",
      "Codigo 98/99 asignado cuando NPCKP36 es monto",
      "Codigos 98/99 considerados meses validos",
      "Residuales finales de NPCKP37",
      "Filas modificadas",
      "Llaves duplicadas",
      "Columnas originales perdidas",
      "NPCKP36 modificados",
      "NPCKP36_original modificados",
      "NPCKP36A_original modificados",
      "NPCKP36A modificados fuera del grupo moda",
      "Casos objetivo sincronizacion NPCKP36A",
      "Casos observados sincronizados NPCKP36A",
      "NPCKP36A distinto de NPCKP36 en universo",
      "Balance distinto del universo"
    ),
    valor_observado = c(
      as.integer(originales_validos_modificados),
      as.integer(imputaciones_previas_modificadas),
      as.integer(originales_modificados),
      as.integer(fuera_universo_modificados),
      valores_asignados_invalidos,
      unos_asignados_a_codigo,
      codigos_asignados_a_monto,
      codigos_clasificados_como_meses,
      sum(residual_final),
      abs(nrow(salida) - n_filas_antes),
      anyDuplicated(salida[llaves]),
      columnas_originales_perdidas,
      as.integer(!identical(serialize(salida$NPCKP36, NULL), npckp36_antes)),
      as.integer(
        !identical(
          serialize(salida$NPCKP36_original, NULL),
          npckp36_original_antes
        )
      ),
      as.integer(
        !identical(
          serialize(salida$NPCKP36A_original, NULL),
          npckp36a_original_antes
        )
      ),
      as.integer(npckp36a_modificados_fuera_moda),
      sum(casos_moda_dominante),
      sum(casos_moda_sincronizados),
      inconsistencias_npckp36_npckp36a,
      abs(sum(balance$personas) - sum(universo))
    ),
    valor_esperado = c(
      rep(0, 16),
      sum(casos_moda_dominante),
      sum(casos_moda_dominante),
      0,
      0
    )
  ) |>
    dplyr::mutate(
      estado = dplyr::if_else(
        .data$valor_observado == .data$valor_esperado,
        "OK",
        "ALERTA"
      )
    )

  if (
    !identical(
      serialize(salida[columnas_originales], NULL),
      originales_antes
    ) ||
      !identical(salida[llaves], llaves_antes)
  ) {
    stop("El cierre NPCKP37 altero originales o llaves.")
  }
  if (any(controles$estado != "OK")) {
    stop("Fallaron los controles de integridad del cierre NPCKP37.")
  }

  auditoria <- salida |>
    dplyr::filter(
      .data$NPCKP37_metodo_imputacion ==
        "un_mes_por_moda_dominante" |
        .data$NPCKP37_flag_codigo_residual
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        llaves,
        "NPCKP17",
        "universo_npckp36_37",
        "NPCKP36_original",
        "NPCKP36",
        "NPCKP36A_original",
        "NPCKP36A",
        "NPCKP36A_flag_imputado",
        "NPCKP36A_metodo_imputacion",
        "NPCKP36_origen_codigo_residual",
        "NPCKP37_original",
        "NPCKP37",
        "NPCKP37_flag_imputado",
        "NPCKP37_metodo_imputacion",
        "NPCKP37_flag_codigo_residual",
        "NPCKP37_codigo_residual",
        "NPCKP37_origen_codigo_residual"
      ))
    )

  list(
    base_k_imputada_k41_k42_cierre_npckp37 = salida,
    resumen_cierre_npckp37 = resumen,
    balance_final_npckp37 = balance,
    auditoria_cierre_npckp37 = auditoria,
    controles_cierre_npckp37 = controles,
    distribucion_meses_donantes = distribucion_meses_donantes,
    parametros = list(
      metodo_monto_original =
        "un_mes_por_moda_dominante",
      metodo_ingreso_reconstruido =
        "un_mes_por_ingreso_reconstruido",
      codigos_residuales = c(98L, 99L),
      variable_oficial_sistemas = "NPCKP37",
      variable_trazabilidad_original = "NPCKP37_original"
    )
  )
}
