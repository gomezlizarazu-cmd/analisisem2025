#' Depurar montos monetarios del Capitulo K
#'
#' Normaliza representaciones de montos, convierte valores numericos de forma
#' segura y separa los codigos especiales 98 y 99 de los montos validos. La
#' funcion no modifica el vector de entrada: lo conserva en `valor_original` y
#' retorna columnas auxiliares para auditoria.
#'
#' Esta funcion solo debe aplicarse a variables cuyo dominio sea monetario. No
#' debe utilizarse automaticamente sobre variables categoricas o indicadoras.
#'
#' @param x Vector con los valores originales del monto.
#'
#' @return Tibble con `valor_original`, `valor_normalizado`, `monto`,
#'   `codigo_98`, `codigo_99`, `codigo_especial`, `no_vacio`,
#'   `no_convertible` y `monto_valido`.
#'
#' @examples
#' depurar_monto_capitulo_k(c("98", "99", "1.000.000", NA))
depurar_monto_capitulo_k <- function(x) {
  valor_original <- x
  valor_normalizado <- stringr::str_squish(as.character(x))
  vacios_texto <- c("", "NA", "N/A", "NULL", "NULO")
  es_vacio <- is.na(valor_normalizado) |
    stringr::str_to_upper(valor_normalizado) %in% vacios_texto
  valor_normalizado[es_vacio] <- NA_character_

  numero_directo <- suppressWarnings(as.numeric(valor_normalizado))
  codigo_98 <- !is.na(numero_directo) & numero_directo == 98
  codigo_99 <- !is.na(numero_directo) & numero_directo == 99

  monto <- numero_directo
  requiere_limpieza <- is.na(monto) & !is.na(valor_normalizado)

  if (any(requiere_limpieza)) {
    texto_limpio <- valor_normalizado[requiere_limpieza] |>
      stringr::str_replace_all("\\$", "") |>
      stringr::str_replace_all("\\s+", "") |>
      stringr::str_replace_all("[\\.,]", "")

    monto[requiere_limpieza] <- suppressWarnings(as.numeric(texto_limpio))
  }

  codigo_especial <- codigo_98 | codigo_99
  monto[codigo_especial] <- NA_real_
  no_vacio <- !is.na(valor_normalizado)
  no_convertible <- no_vacio & is.na(monto) & !codigo_especial
  monto_valido <- !is.na(monto)

  tibble::tibble(
    valor_original = valor_original,
    valor_normalizado = valor_normalizado,
    monto = monto,
    codigo_98 = codigo_98,
    codigo_99 = codigo_99,
    codigo_especial = codigo_especial,
    no_vacio = no_vacio,
    no_convertible = no_convertible,
    monto_valido = monto_valido
  )
}

#' Auditar codigos especiales en variables monetarias del Capitulo K
#'
#' Resume la conversion de un conjunto declarado de variables monetarias. Los
#' cuantiles se calculan exclusivamente sobre montos validos; los codigos 98 y
#' 99 y los valores no convertibles quedan excluidos.
#'
#' @param data Data frame que contiene las variables monetarias.
#' @param variables Vector de nombres de variables monetarias.
#'
#' @return Tibble con conteos de vacios, montos validos, codigos especiales,
#'   valores no convertibles, ceros, positivos y cuantiles de montos validos.
#'
#' @examples
#' datos <- tibble::tibble(NPCKP23 = c("98", "1000000", "sin dato"))
#' auditar_montos_capitulo_k(datos, "NPCKP23")
auditar_montos_capitulo_k <- function(data, variables) {
  faltantes <- setdiff(variables, names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables monetarias para auditar: ",
      paste(faltantes, collapse = ", ")
    )
  }

  dplyr::bind_rows(lapply(variables, function(variable) {
    depurado <- depurar_monto_capitulo_k(data[[variable]])
    validos <- depurado$monto[depurado$monto_valido]
    positivos <- validos[validos > 0]
    cuantiles <- if (length(validos) > 0) {
      stats::quantile(
        validos,
        probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
        na.rm = TRUE,
        names = FALSE,
        type = 7
      )
    } else {
      rep(NA_real_, 7)
    }

    tibble::tibble(
      variable = variable,
      n_no_vacios = sum(depurado$no_vacio),
      n_montos_validos = sum(depurado$monto_valido),
      n_codigo_98 = sum(depurado$codigo_98),
      n_codigo_99 = sum(depurado$codigo_99),
      n_no_convertibles = sum(depurado$no_convertible),
      n_ceros = sum(validos == 0),
      n_positivos = length(positivos),
      minimo_positivo = if (length(positivos) > 0) min(positivos) else NA_real_,
      p01 = cuantiles[1],
      p05 = cuantiles[2],
      p25 = cuantiles[3],
      mediana = cuantiles[4],
      p75 = cuantiles[5],
      p95 = cuantiles[6],
      p99 = cuantiles[7]
    )
  }))
}

.componente_monto_no_incluido_k <- function(indicador, monto, incluido) {
  dplyr::if_else(
    indicador == 1L & incluido == 2L & !is.na(monto),
    monto,
    0
  )
}

.componente_monto_recibido_k <- function(indicador, monto) {
  dplyr::case_when(
    indicador == 1L & !is.na(monto) ~ monto,
    is.na(indicador) & !is.na(monto) ~ monto,
    TRUE ~ 0
  )
}

.mensualizar_monto_anual_k <- function(indicador, monto) {
  .componente_monto_recibido_k(indicador, monto) / 12
}

#' Variables exclusivas del bloque asalariado usadas en K41-K42
#'
#' Devuelve la lista de variables cuya regla consolidada pertenece a
#' `05_rama_asalariados`. `NPCKP23A` se incluye como reingreso de `NPCKP23`,
#' de acuerdo con el diccionario, aunque no figure como nodo independiente en
#' la matriz de flujo.
#'
#' @return Vector de nombres de variables.
variables_bloque_asalariados_limpieza_k41_k42 <- function() {
  c(
    "NPCKP23", "NPCKP23A", "NPCKP24", "NPCKP24A", "NPCKP24B",
    "NPCKP25", "NPCKP25A", "NPCKP26", "NPCKP26A", "NPCKP27",
    "NPCKP27A", "NPCKP28", "NPCKP28A", "NPCKP29", "NPCKP29A",
    "NPCKP29B", "NPCKP30", "NPCKP30A", "NPCKP30B", "NPCKP31",
    "NPCKP31A", "NPCKP31B", "NPCKP32", "NPCKP32A", "NPCKP32B",
    "NPCKP33", "NPCKP33A", "NPCKP33A1", "NPCKNP33A", "NPCKP33AA",
    "NPCKP33AB", "NPCKP34A", "NPCKP34AA", "NPCKP34B", "NPCKP34BA",
    "NPCKP34C", "NPCKP34CA", "NPCKP34D", "NPCKP34DA", "NPCKP34E",
    "NPCKP34EA", "NPCKP35A", "NPCKP35AA", "NPCKP35_A", "NPCKP35_C",
    "NPCKP35_D", "NPCKP35_E"
  )
}

.valor_no_vacio_imputacion_k <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  !(
    is.na(x_chr) |
      stringr::str_to_upper(x_chr) %in% c("", "NA", "N/A", "NULL", "NULO")
  )
}

.formatear_monto_tipo_original_k <- function(x, original) {
  if (is.character(original)) {
    entero <- is.na(x) | abs(x - round(x)) < sqrt(.Machine$double.eps)
    if (!all(entero)) {
      stop(
        "El ingreso acotado contiene decimales que no pueden escribirse ",
        "de forma estable en una variable monetaria character."
      )
    }
    return(format(
      round(x),
      scientific = FALSE,
      trim = TRUE,
      decimal.mark = ".",
      big.mark = ""
    ))
  }

  if (is.integer(original)) {
    entero <- is.na(x) | abs(x - round(x)) < sqrt(.Machine$double.eps)
    fuera_rango <- !is.na(x) &
      (x < -.Machine$integer.max | x > .Machine$integer.max)
    if (!all(entero) || any(fuera_rango)) {
      stop(
        "El ingreso acotado no puede conservarse de forma segura como integer."
      )
    }
    return(as.integer(round(x)))
  }

  if (is.numeric(original)) {
    return(as.numeric(x))
  }

  stop(
    "NPCKP36/NPCKP36A deben ser variables character, integer o numeric."
  )
}

#' Imputar deterministicamente NPCKP36, NPCKP36A y NPCKP37
#'
#' Imputa exclusivamente personas del universo consolidado K41-K42 con ingreso
#' acotado reconstruido y evidencia de desvio al bloque asalariado. Conserva
#' originales, registra trazabilidad y limpia despues las variables exclusivas
#' de la rama asalariada. La funcion no modifica el objeto de entrada.
#'
#' @param data Base K a nivel persona con llaves unicas y variables derivadas
#'   por el diagnostico de ingresos.
#' @param variables_limpieza Variables exclusivas del bloque asalariado.
#'
#' @return Lista con la base imputada, resumen, auditoria, variables de limpieza
#'   y parametros.
imputar_k41_k42 <- function(
    data,
    variables_limpieza =
      variables_bloque_asalariados_limpieza_k41_k42(),
    variables_exclusivas_validadas =
      variables_bloque_asalariados_limpieza_k41_k42()) {
  llaves <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  requeridas <- c(
    llaves,
    "NPCKP17", "NPCKP36", "NPCKP36A", "NPCKP37",
    "universo_npckp36_37", "vacio_critico_npckp36",
    "vacio_critico_npckp37", "recuperable_npckp36_acotado",
    "ingreso_acotado", "ingreso_amplio",
    "alguna_respuesta_bloque_asalariados",
    "desviada_bloque_asalariados_en_universo_objetivo"
  )
  faltantes <- setdiff(c(requeridas, variables_limpieza), names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables para imputar K41-K42: ",
      paste(faltantes, collapse = ", ")
    )
  }

  originales_objetivo <- paste0(
    c("NPCKP36", "NPCKP36A", "NPCKP37"),
    "_original"
  )
  originales_limpieza <- paste0(variables_limpieza, "_original")
  originales_requeridos <- unique(c(
    originales_objetivo,
    originales_limpieza
  ))
  ya_imputada <- intersect(originales_requeridos, names(data))
  if (length(ya_imputada) > 0) {
    stop(
      "La base ya contiene copias originales y no puede imputarse de nuevo: ",
      paste(ya_imputada, collapse = ", ")
    )
  }

  if (anyDuplicated(data[llaves]) > 0) {
    stop(
      "La llave DIRECTORIO + SECUENCIA_P + ORDEN no es unica."
    )
  }

  no_exclusivas <- setdiff(
    variables_limpieza,
    variables_exclusivas_validadas
  )
  if (length(no_exclusivas) > 0) {
    stop(
      "Se intento limpiar una variable no validada como exclusiva de ",
      "asalariados: ",
      paste(no_exclusivas, collapse = ", ")
    )
  }

  n_filas_original <- nrow(data)
  llaves_originales <- data[llaves]
  salida <- data

  for (variable in unique(c(
    "NPCKP36", "NPCKP36A", "NPCKP37",
    variables_limpieza
  ))) {
    salida[[paste0(variable, "_original")]] <- salida[[variable]]
  }

  posicion <- suppressWarnings(as.integer(as.character(salida$NPCKP17)))
  monto_original <- depurar_monto_capitulo_k(salida$NPCKP36_original)
  ingreso_valido <- is.finite(salida$ingreso_acotado) &
    salida$ingreso_acotado >= 0 &
    !salida$ingreso_acotado %in% c(98, 99)
  evidencia_desvio <-
    salida$alguna_respuesta_bloque_asalariados %in% TRUE &
    salida$desviada_bloque_asalariados_en_universo_objetivo %in% TRUE

  imputar_npckp36 <-
    salida$universo_npckp36_37 %in% TRUE &
    posicion %in% c(4L, 5L, 8L) &
    salida$vacio_critico_npckp36 %in% TRUE &
    salida$recuperable_npckp36_acotado %in% TRUE &
    ingreso_valido &
    evidencia_desvio &
    !monto_original$monto_valido

  valor_npckp36 <- .formatear_monto_tipo_original_k(
    salida$ingreso_acotado,
    salida$NPCKP36
  )
  salida$NPCKP36_flag_imputado <- imputar_npckp36
  salida$NPCKP36_metodo_imputacion <- NA_character_
  salida$NPCKP36_metodo_imputacion[imputar_npckp36] <-
    "reconstruccion_deterministica_ingreso_acotado_bloque_asalariados"
  salida$NPCKP36[imputar_npckp36] <-
    valor_npckp36[imputar_npckp36]

  salida$NPCKP36_ingresoamplioimputacionK4142 <- NA_real_
  salida$NPCKP36_ingresoamplioimputacionK4142[imputar_npckp36] <-
    salida$ingreso_amplio[imputar_npckp36]

  valor_npckp36a <- .formatear_monto_tipo_original_k(
    salida$ingreso_acotado,
    salida$NPCKP36A
  )
  npckp36a_cambia <- imputar_npckp36 &
    (
      is.na(salida$NPCKP36A) |
        as.character(salida$NPCKP36A) !=
          as.character(valor_npckp36a)
    )
  npckp36a_cambia[is.na(npckp36a_cambia)] <- imputar_npckp36[
    is.na(npckp36a_cambia)
  ]
  salida$NPCKP36A_flag_imputado <- npckp36a_cambia
  salida$NPCKP36A[imputar_npckp36] <-
    valor_npckp36a[imputar_npckp36]

  npckp37_original_vacio <-
    !.valor_no_vacio_imputacion_k(salida$NPCKP37_original)
  imputar_npckp37 <- imputar_npckp36 & npckp37_original_vacio
  valor_un_mes <- .formatear_monto_tipo_original_k(
    rep(1, nrow(salida)),
    salida$NPCKP37
  )
  salida$NPCKP37_flag_imputado <- imputar_npckp37
  salida$NPCKP37_metodo_imputacion <- NA_character_
  salida$NPCKP37_metodo_imputacion[imputar_npckp37] <-
    "un_mes_por_reconstruccion_desde_ingresos_mes_pasado"
  salida$NPCKP37[imputar_npckp37] <- valor_un_mes[imputar_npckp37]

  if (any(imputar_npckp36 & monto_original$monto_valido)) {
    stop("Se intento sobrescribir un NPCKP36 observado valido.")
  }
  if (any(imputar_npckp37 & !npckp37_original_vacio)) {
    stop("Se intento sobrescribir un NPCKP37 observado.")
  }
  if (any(imputar_npckp36 & !salida$universo_npckp36_37)) {
    stop("Se intento imputar NPCKP36 fuera del universo consolidado.")
  }
  if (any(imputar_npckp36 & !posicion %in% c(4L, 5L, 8L))) {
    stop("Se intento imputar una posicion ocupacional no independiente.")
  }
  if (any(imputar_npckp37 & !imputar_npckp36)) {
    stop("Se intento imputar NPCKP37 sin reconstruir NPCKP36.")
  }

  monto_imputado <- depurar_monto_capitulo_k(salida$NPCKP36)
  if (any(
    imputar_npckp36 &
      (monto_imputado$codigo_especial | !monto_imputado$monto_valido)
  )) {
    stop("La imputacion uso 98, 99 o un monto no valido.")
  }
  monto_confirmado <- depurar_monto_capitulo_k(salida$NPCKP36A)
  if (any(
    imputar_npckp36 &
      (
        !monto_confirmado$monto_valido |
          monto_imputado$monto != monto_confirmado$monto
      )
  )) {
    stop("NPCKP36 y NPCKP36A no quedaron coherentes.")
  }

  flag_limpieza <- evidencia_desvio &
    salida$universo_npckp36_37 %in% TRUE &
    posicion %in% c(4L, 5L, 8L)
  if (sum(flag_limpieza) < sum(imputar_npckp36)) {
    stop("La poblacion de limpieza es menor que la poblacion imputada.")
  }
  if (any(imputar_npckp36 & !flag_limpieza)) {
    stop("Un caso imputado quedo por fuera de la limpieza asalariada.")
  }
  salida$K4142_flag_desvio_bloque_asalariados <- evidencia_desvio
  salida$K4142_flag_limpieza_bloque_asalariados <- flag_limpieza

  n_no_vacias_originales <- rowSums(
    as.data.frame(lapply(
      salida[paste0(variables_limpieza, "_original")],
      .valor_no_vacio_imputacion_k
    )),
    na.rm = TRUE
  )
  salida$K4142_n_variables_asalariadas_limpiadas <- ifelse(
    flag_limpieza,
    n_no_vacias_originales,
    0L
  )

  ingreso_acotado_antes_limpieza <- salida$ingreso_acotado
  ingreso_amplio_antes_limpieza <- salida$ingreso_amplio
  ingreso_auxiliar_antes_limpieza <-
    salida$NPCKP36_ingresoamplioimputacionK4142

  for (variable in variables_limpieza) {
    x <- salida[[variable]]
    x[flag_limpieza] <- NA
    salida[[variable]] <- x
  }

  if (
    !identical(
      ingreso_acotado_antes_limpieza,
      salida$ingreso_acotado
    ) ||
      !identical(
        ingreso_amplio_antes_limpieza,
        salida$ingreso_amplio
      ) ||
      !identical(
        ingreso_auxiliar_antes_limpieza,
        salida$NPCKP36_ingresoamplioimputacionK4142
      )
  ) {
    stop("Los ingresos reconstruidos cambiaron durante la limpieza.")
  }

  quedan_respuestas <- rowSums(
    as.data.frame(lapply(
      salida[variables_limpieza],
      .valor_no_vacio_imputacion_k
    )),
    na.rm = TRUE
  )
  if (any(flag_limpieza & quedan_respuestas != 0L)) {
    stop("La limpieza dejo respuestas en el bloque asalariado.")
  }
  if (any(flag_limpieza & !evidencia_desvio)) {
    stop("Se modifico una persona sin evidencia de desvio.")
  }
  if (any(
    salida$universo_npckp36_37 %in% TRUE &
      salida$alguna_respuesta_bloque_asalariados %in% TRUE &
      posicion %in% c(4L, 5L, 8L) &
      !flag_limpieza
  )) {
    stop(
      "Un caso con respuestas salariales fuera de flujo quedo sin limpiar."
    )
  }
  copias_faltantes <- setdiff(
    paste0(
      unique(c(
        "NPCKP36", "NPCKP36A", "NPCKP37",
        variables_limpieza
      )),
      "_original"
    ),
    names(salida)
  )
  if (length(copias_faltantes) > 0) {
    stop(
      "Se perdieron copias originales: ",
      paste(copias_faltantes, collapse = ", ")
    )
  }
  if (
    nrow(salida) != n_filas_original ||
      !identical(salida[llaves], llaves_originales) ||
      anyDuplicated(salida[llaves]) > 0
  ) {
    stop("La imputacion altero filas, orden o unicidad de las llaves.")
  }

  modificada <- imputar_npckp36 | imputar_npckp37 | flag_limpieza
  fuera_universo_modificada <- modificada &
    !salida$universo_npckp36_37 %in% TRUE
  observados_npckp36_sobrescritos <- imputar_npckp36 &
    monto_original$monto_valido
  observados_npckp37_sobrescritos <- imputar_npckp37 &
    !npckp37_original_vacio

  resumen <- tibble::tibble(
    indicador = c(
      "Universo teorico NPCKP36/37",
      "Vacios originales NPCKP36",
      "Vacios originales NPCKP37",
      "Casos imputados NPCKP36",
      "Casos imputados NPCKP36A",
      "Casos imputados NPCKP37",
      "Casos con ingreso amplio auxiliar",
      "Casos residuales NPCKP36",
      "Casos residuales NPCKP37",
      "Personas con bloque asalariado limpiado",
      "Numero total de celdas limpiadas",
      "Personas fuera del universo modificadas",
      "Valores observados sobrescritos",
      "Codigos 98/99 utilizados como montos"
    ),
    valor = c(
      sum(salida$universo_npckp36_37 %in% TRUE),
      sum(salida$universo_npckp36_37 %in% TRUE & !monto_original$monto_valido),
      sum(salida$universo_npckp36_37 %in% TRUE & npckp37_original_vacio),
      sum(salida$NPCKP36_flag_imputado),
      sum(salida$NPCKP36A_flag_imputado),
      sum(salida$NPCKP37_flag_imputado),
      sum(!is.na(salida$NPCKP36_ingresoamplioimputacionK4142)),
      sum(
        salida$universo_npckp36_37 %in% TRUE &
          !depurar_monto_capitulo_k(salida$NPCKP36)$monto_valido
      ),
      sum(
        salida$universo_npckp36_37 %in% TRUE &
          !.valor_no_vacio_imputacion_k(salida$NPCKP37)
      ),
      sum(flag_limpieza),
      sum(salida$K4142_n_variables_asalariadas_limpiadas),
      sum(fuera_universo_modificada),
      sum(
        observados_npckp36_sobrescritos |
          observados_npckp37_sobrescritos
      ),
      sum(
        imputar_npckp36 &
          depurar_monto_capitulo_k(salida$NPCKP36)$codigo_especial
      )
    )
  )

  auditoria <- salida |>
    dplyr::transmute(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      NPCKP17,
      universo_npckp36_37,
      NPCKP36_original,
      NPCKP36,
      NPCKP36A_original,
      NPCKP36A,
      NPCKP37_original,
      NPCKP37,
      ingreso_acotado,
      NPCKP36_ingresoamplioimputacionK4142,
      NPCKP36_flag_imputado,
      NPCKP36A_flag_imputado,
      NPCKP37_flag_imputado,
      NPCKP36_metodo_imputacion,
      NPCKP37_metodo_imputacion,
      K4142_flag_desvio_bloque_asalariados,
      K4142_flag_limpieza_bloque_asalariados,
      K4142_n_variables_asalariadas_limpiadas
    ) |>
    dplyr::filter(
      .data$NPCKP36_flag_imputado |
        .data$NPCKP37_flag_imputado |
        .data$K4142_flag_limpieza_bloque_asalariados
    )

  list(
    base_k_imputada_k41_k42 = salida,
    resumen_imputacion_k41_k42 = resumen,
    auditoria_imputacion_k41_k42 = auditoria,
    variables_bloque_asalariados_limpieza = variables_limpieza,
    parametros = list(
      metodo_npckp36 =
        "reconstruccion_deterministica_ingreso_acotado_bloque_asalariados",
      metodo_npckp37 =
        "un_mes_por_reconstruccion_desde_ingresos_mes_pasado",
      npckp36a_tratada_como_reingreso_monetario = TRUE,
      posiciones_independientes = c(4L, 5L, 8L),
      limpieza_posterior_a_reconstruccion = TRUE
    )
  )
}
