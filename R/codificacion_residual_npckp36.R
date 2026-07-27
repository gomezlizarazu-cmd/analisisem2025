#' Codificar residuales no monetarios de NPCKP36
#'
#' Cierra casos residuales de NPCKP36 mediante codigos especiales 98 y 99.
#' El procedimiento no imputa ingresos: traslada el codigo observado en
#' `NPCKP23_original` o asigna 99 cuando una clasificacion previa confirma
#' ausencia total de informacion util. NPCKP37 y todas las copias originales
#' se conservan sin cambios.
#'
#' @param data Base K posterior a la imputacion monetaria y la limpieza.
#' @param residual_npckp36 Vector logico que identifica residuales monetarios.
#' @param sin_informacion_util Vector logico previamente construido para los
#'   casos sin informacion util en ninguna rama.
#'
#' @return Lista con base codificada, resumen, balance, auditoria y controles.
codificar_residuales_npckp36 <- function(data,
                                         residual_npckp36,
                                         sin_informacion_util) {
  llaves <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  requeridas <- c(
    llaves, "NPCKP36", "NPCKP36A", "NPCKP37",
    "NPCKP36_original", "NPCKP36A_original", "NPCKP37_original",
    "NPCKP23_original", "NPCKP36_flag_imputado",
    "universo_npckp36_37"
  )
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables para codificar residuales NPCKP36: ",
      paste(faltantes, collapse = ", ")
    )
  }
  nuevas <- c(
    "NPCKP36_flag_codigo_residual",
    "NPCKP36_codigo_residual",
    "NPCKP36_origen_codigo_residual",
    "NPCKP36A_flag_codigo_residual",
    "NPCKP36_metodo_cierre"
  )
  repetidas <- intersect(nuevas, names(data))
  if (length(repetidas) > 0) {
    stop(
      "La codificacion residual ya fue aplicada: ",
      paste(repetidas, collapse = ", ")
    )
  }
  if (
    length(residual_npckp36) != nrow(data) ||
      length(sin_informacion_util) != nrow(data)
  ) {
    stop("Las banderas residuales deben tener una observacion por persona.")
  }
  if (anyDuplicated(data[llaves]) > 0) {
    stop("La llave de persona no es unica antes de la codificacion residual.")
  }

  residual <- residual_npckp36 %in% TRUE
  sin_info <- sin_informacion_util %in% TRUE
  universo <- data$universo_npckp36_37 %in% TRUE
  depurado_23 <- depurar_monto_capitulo_k(data$NPCKP23_original)
  desde_98 <- residual & depurado_23$codigo_98
  desde_99 <- residual & depurado_23$codigo_99
  ausencia_total <- residual & !desde_98 & !desde_99 & sin_info
  origen <- dplyr::case_when(
    desde_98 ~ "98_trasladado_desde_npckp23",
    desde_99 ~ "99_trasladado_desde_npckp23",
    ausencia_total ~ "99_asignado_ausencia_total",
    TRUE ~ NA_character_
  )
  sin_clasificar_antes <- residual & is.na(origen)
  if (any(sin_clasificar_antes)) {
    stop(
      "Quedaron residuales NPCKP36 sin clasificar: ",
      sum(sin_clasificar_antes)
    )
  }
  intervenir <- residual & !is.na(origen)
  codigo <- dplyr::case_when(
    desde_98 ~ 98,
    desde_99 | ausencia_total ~ 99,
    TRUE ~ NA_real_
  )

  originales_requeridos <- c(
    "NPCKP36_original", "NPCKP36A_original",
    "NPCKP37_original", "NPCKP23_original"
  )
  originales_antes <- serialize(data[originales_requeridos], NULL)
  npckp37_antes <- serialize(data$NPCKP37, NULL)
  llaves_antes <- data[llaves]
  n_filas_antes <- nrow(data)
  monto_original <- depurar_monto_capitulo_k(data$NPCKP36_original)

  if (any(intervenir & monto_original$monto_valido)) {
    stop("Se intento sobrescribir un monto original valido de NPCKP36.")
  }
  if (any(intervenir & data$NPCKP36_flag_imputado %in% TRUE)) {
    stop("Se intento modificar una imputacion monetaria previa.")
  }
  if (any(intervenir & !universo)) {
    stop("Se intento codificar una persona fuera del universo NPCKP36.")
  }
  if (any(intervenir & !codigo %in% c(98, 99))) {
    stop("Solo se permite asignar los codigos residuales 98 o 99.")
  }

  salida <- data
  valor_36 <- .formatear_monto_tipo_original_k(codigo, salida$NPCKP36)
  valor_36a <- .formatear_monto_tipo_original_k(codigo, salida$NPCKP36A)
  salida$NPCKP36_flag_codigo_residual <- intervenir
  salida$NPCKP36_codigo_residual <- as.integer(codigo)
  salida$NPCKP36_origen_codigo_residual <- origen
  salida$NPCKP36A_flag_codigo_residual <- intervenir
  salida$NPCKP36_metodo_cierre <- NA_character_
  salida$NPCKP36_metodo_cierre[intervenir] <-
    "codificacion_residual_no_monetaria"
  salida$NPCKP36[intervenir] <- valor_36[intervenir]
  salida$NPCKP36A[intervenir] <- valor_36a[intervenir]

  depurado_36_final <- depurar_monto_capitulo_k(salida$NPCKP36)
  depurado_36a_final <- depurar_monto_capitulo_k(salida$NPCKP36A)
  if (any(
    intervenir &
      (
        depurado_36_final$monto_valido |
          !depurado_36_final$codigo_especial
      )
  )) {
    stop("Un codigo residual fue clasificado como monto monetario.")
  }
  if (any(
    intervenir &
      as.character(salida$NPCKP36) != as.character(salida$NPCKP36A)
  )) {
    stop("NPCKP36A no coincide con NPCKP36 en la codificacion residual.")
  }
  if (any(
    intervenir &
      (
        depurado_36_final$codigo_98 != depurado_36a_final$codigo_98 |
          depurado_36_final$codigo_99 != depurado_36a_final$codigo_99
      )
  )) {
    stop("NPCKP36 y NPCKP36A no conservan el mismo codigo especial.")
  }
  if (!identical(serialize(salida$NPCKP37, NULL), npckp37_antes)) {
    stop("La codificacion residual modifico NPCKP37.")
  }
  if (!identical(
    serialize(salida[originales_requeridos], NULL),
    originales_antes
  )) {
    stop("La codificacion residual modifico una copia original.")
  }
  if (
    nrow(salida) != n_filas_antes ||
      !identical(salida[llaves], llaves_antes) ||
      anyDuplicated(salida[llaves]) > 0
  ) {
    stop("La codificacion residual altero filas o llaves.")
  }

  residual_sin_clasificar <- residual &
    !salida$NPCKP36_flag_codigo_residual
  observado_valido <- universo & monto_original$monto_valido
  imputado_monetario <- universo &
    salida$NPCKP36_flag_imputado %in% TRUE
  n_universo <- sum(universo)
  balance <- tibble::tibble(
    categoria = c(
      "Respuesta monetaria valida original",
      "Imputacion monetaria con ingreso acotado",
      "Codigo 98 trasladado desde NPCKP23",
      "Codigo 99 trasladado desde NPCKP23",
      "Codigo 99 asignado por ausencia total",
      "Residual sin clasificar"
    ),
    personas = c(
      sum(observado_valido),
      sum(imputado_monetario),
      sum(desde_98),
      sum(desde_99),
      sum(ausencia_total),
      sum(residual_sin_clasificar)
    ),
    porcentaje_universo = personas / n_universo,
    es_monto_monetario_valido = c(
      TRUE, TRUE, FALSE, FALSE, FALSE, FALSE
    ),
    origen = c(
      "NPCKP36_original",
      "ingreso_acotado",
      "NPCKP23_original",
      "NPCKP23_original",
      "procesamiento_ausencia_total",
      "sin_clasificar"
    )
  )
  if (sum(balance$personas) != n_universo) {
    stop("El balance final de NPCKP36 no coincide con el universo.")
  }
  if (sum(residual_sin_clasificar) != 0) {
    stop("Quedaron residuales NPCKP36 sin clasificar despues del cierre.")
  }

  resumen <- tibble::tibble(
    indicador = c(
      "Casos residuales antes del cierre",
      "Casos codigo 98 trasladado",
      "Casos codigo 99 trasladado",
      "Casos codigo 99 asignado",
      "Casos residuales despues del cierre",
      "Total con monto monetario valido",
      "Total con codigo especial"
    ),
    personas = c(
      sum(residual),
      sum(desde_98),
      sum(desde_99),
      sum(ausencia_total),
      sum(residual_sin_clasificar),
      sum(observado_valido | imputado_monetario),
      sum(intervenir)
    ),
    porcentaje_universo = personas / n_universo
  )

  auditoria <- salida |>
    dplyr::mutate(
      grupo_codigo_residual_npckp36 =
        .data$NPCKP36_origen_codigo_residual,
      descripcion_grupo_codigo_residual = dplyr::case_when(
        .data$grupo_codigo_residual_npckp36 ==
          "98_trasladado_desde_npckp23" ~
          "Codigo 98 trasladado desde NPCKP23 original",
        .data$grupo_codigo_residual_npckp36 ==
          "99_trasladado_desde_npckp23" ~
          "Codigo 99 trasladado desde NPCKP23 original",
        .data$grupo_codigo_residual_npckp36 ==
          "99_asignado_ausencia_total" ~
          "Codigo 99 asignado por ausencia total de informacion util",
        TRUE ~ NA_character_
      ),
      residual_sin_clasificar =
        is.na(.data$grupo_codigo_residual_npckp36)
    ) |>
    dplyr::filter(.data$NPCKP36_flag_codigo_residual) |>
    dplyr::select(
      dplyr::all_of(llaves),
      dplyr::all_of(c(
        "NPCKP23_original",
        "NPCKP36_original",
        "NPCKP36",
        "NPCKP36A_original",
        "NPCKP36A",
        "NPCKP37_original",
        "NPCKP37",
        "NPCKP36_flag_imputado",
        "NPCKP36_flag_codigo_residual",
        "NPCKP36_codigo_residual",
        "NPCKP36_origen_codigo_residual",
        "grupo_codigo_residual_npckp36",
        "descripcion_grupo_codigo_residual",
        "residual_sin_clasificar",
        "NPCKP36A_flag_codigo_residual",
        "NPCKP36_metodo_cierre"
      ))
    )

  controles <- tibble::tibble(
    control = c(
      "Casos residuales antes del cierre",
      "Casos codigo 98 trasladado",
      "Casos codigo 99 trasladado",
      "Casos codigo 99 asignado",
      "Casos residuales despues del cierre",
      "Valores monetarios sobrescritos",
      "Personas fuera del universo modificadas",
      "NPCKP37 modificados",
      "Codigos especiales utilizados como montos"
    ),
    valor_observado = c(
      sum(residual),
      sum(desde_98),
      sum(desde_99),
      sum(ausencia_total),
      sum(residual_sin_clasificar),
      sum(intervenir & monto_original$monto_valido),
      sum(intervenir & !universo),
      as.integer(!identical(serialize(salida$NPCKP37, NULL), npckp37_antes)),
      sum(intervenir & depurado_36_final$monto_valido)
    ),
    valor_esperado = c(
      NA_real_, NA_real_, NA_real_, NA_real_, 0, 0, 0, 0, 0
    )
  ) |>
    dplyr::mutate(
      estado = dplyr::if_else(
        is.na(.data$valor_esperado) |
          .data$valor_observado == .data$valor_esperado,
        "OK",
        "ALERTA"
      )
    )

  list(
    base_k_imputada_k41_k42_codigos_residuales = salida,
    resumen_codificacion_residual_npckp36 = resumen,
    balance_final_npckp36 = balance,
    auditoria_codificacion_residual_npckp36 = auditoria,
    controles_codificacion_residual_npckp36 = controles
  )
}

#' Validar conteos del cierre residual de NPCKP36 por grupo
#'
#' Alinea los conteos observados y esperados mediante la clave tecnica
#' `grupo_codigo_residual_npckp36`. El orden de las filas y las etiquetas de
#' presentacion no intervienen en la comparacion.
#'
#' @param auditoria Auditoria producida por `codificar_residuales_npckp36()`.
#' @param conteos_esperados Tabla con la clave tecnica y `conteo_esperado`.
#'
#' @return Lista con la tabla de diferencias y controles agregados.
validar_conteos_cierre_npckp36 <- function(auditoria,
                                           conteos_esperados) {
  columnas_auditoria <- c(
    "grupo_codigo_residual_npckp36",
    "residual_sin_clasificar"
  )
  columnas_esperadas <- c(
    "grupo_codigo_residual_npckp36",
    "conteo_esperado"
  )
  faltan_auditoria <- setdiff(columnas_auditoria, names(auditoria))
  faltan_esperadas <- setdiff(columnas_esperadas, names(conteos_esperados))
  if (length(faltan_auditoria) > 0) {
    stop(
      "Faltan columnas en la auditoria del cierre NPCKP36: ",
      paste(faltan_auditoria, collapse = ", ")
    )
  }
  if (length(faltan_esperadas) > 0) {
    stop(
      "Faltan columnas en los conteos esperados NPCKP36: ",
      paste(faltan_esperadas, collapse = ", ")
    )
  }
  if (anyDuplicated(
    conteos_esperados["grupo_codigo_residual_npckp36"]
  ) > 0) {
    stop("Los conteos esperados contienen grupos duplicados.")
  }

  auditoria_antes <- serialize(auditoria, connection = NULL)
  esperados_antes <- serialize(conteos_esperados, connection = NULL)

  conteos_observados_cierre <- auditoria |>
    dplyr::count(
      grupo_codigo_residual_npckp36,
      name = "conteo_observado"
    )

  validacion_conteos_cierre <- conteos_esperados |>
    dplyr::select(dplyr::all_of(columnas_esperadas)) |>
    dplyr::full_join(
      conteos_observados_cierre,
      by = "grupo_codigo_residual_npckp36"
    ) |>
    dplyr::mutate(
      conteo_esperado = tidyr::replace_na(
        .data$conteo_esperado,
        0L
      ),
      conteo_observado = tidyr::replace_na(
        .data$conteo_observado,
        0L
      ),
      diferencia =
        .data$conteo_observado - .data$conteo_esperado,
      estado = dplyr::if_else(
        .data$diferencia == 0L,
        "OK",
        "ALERTA"
      )
    )

  total_observado <- sum(
    conteos_observados_cierre$conteo_observado
  )
  residuales_sin_clasificar <- sum(
    auditoria$residual_sin_clasificar,
    na.rm = TRUE
  )

  if (
    !identical(serialize(auditoria, NULL), auditoria_antes) ||
      !identical(
        serialize(conteos_esperados, NULL),
        esperados_antes
      )
  ) {
    stop("La validacion de conteos modifico una tabla de entrada.")
  }

  list(
    conteos_observados_cierre = conteos_observados_cierre,
    validacion_conteos_cierre = validacion_conteos_cierre,
    conteos_correctos =
      all(validacion_conteos_cierre$diferencia == 0L),
    total_observado = total_observado,
    residuales_sin_clasificar = residuales_sin_clasificar
  )
}
