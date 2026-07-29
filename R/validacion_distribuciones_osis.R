# Utilidades generales para depurar y validar bases destinadas a OSIS.
#
# Estas funciones son internas porque el paquete se carga operativamente con
# `devtools::load_all()`. No dependen de variables o capitulos particulares.

#' Normalizar una categoria para controles de distribucion OSIS
#'
#' Convierte codigos a texto, elimina espacios laterales y representa cadenas
#' vacias y marcadores comunes de ausencia como `NA`.
#'
#' @param x Vector a normalizar.
#'
#' @return Vector character.
#' @keywords internal
normalizar_categoria_distribucion_osis <- function(x) {
  salida <- stringr::str_squish(as.character(x))
  marcador_ausente <- stringr::str_to_upper(salida) %in%
    c("", "NA", "N/A", "NULL", "NULO")
  salida[is.na(salida) | marcador_ausente] <- NA_character_
  salida
}

.valor_o_osis <- function(x, valor_predeterminado) {
  if (is.null(x)) valor_predeterminado else x
}

#' Evaluar controles de aceptacion OSIS
#'
#' @param observado Vector de valores observados.
#' @param esperado Vector de valores esperados.
#' @param operador Operador de comparacion. Admite `"igual"`,
#'   `"menor_igual"`, `"mayor_igual"`, `"menor"` y `"mayor"`.
#' @param tolerancia_numerica Tolerancia absoluta para comparaciones numericas.
#'
#' @return Vector logico sin `NA`.
#' @keywords internal
evaluar_control_osis <- function(observado,
                                 esperado,
                                 operador = "igual",
                                 tolerancia_numerica = sqrt(.Machine$double.eps)) {
  tamanos <- c(length(observado), length(esperado), length(operador))
  n <- max(tamanos)

  if (n == 0L || any(!tamanos %in% c(1L, n))) {
    stop(
      "`observado`, `esperado` y `operador` deben tener longitudes compatibles.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(tolerancia_numerica) ||
      length(tolerancia_numerica) != 1L ||
      is.na(tolerancia_numerica) ||
      tolerancia_numerica < 0
  ) {
    stop(
      "`tolerancia_numerica` debe ser un numero no negativo.",
      call. = FALSE
    )
  }

  observado <- rep(observado, length.out = n)
  esperado <- rep(esperado, length.out = n)
  operador <- rep(as.character(operador), length.out = n)

  operadores_validos <- c(
    "igual",
    "menor_igual",
    "mayor_igual",
    "menor",
    "mayor"
  )
  operadores_desconocidos <- setdiff(unique(operador), operadores_validos)
  if (length(operadores_desconocidos) > 0L) {
    stop(
      "Operador de control no soportado: ",
      paste(operadores_desconocidos, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  vapply(
    seq_len(n),
    function(i) {
      obs <- observado[[i]]
      esp <- esperado[[i]]
      op <- operador[[i]]

      if (
        length(obs) != 1L ||
          length(esp) != 1L ||
          is.na(obs) ||
          is.na(esp)
      ) {
        return(FALSE)
      }

      ambos_numericos <- is.numeric(obs) && is.numeric(esp)

      if (identical(op, "igual")) {
        if (ambos_numericos) {
          return(abs(as.numeric(obs) - as.numeric(esp)) <= tolerancia_numerica)
        }
        return(identical(as.character(obs), as.character(esp)))
      }

      obs_num <- suppressWarnings(as.numeric(as.character(obs)))
      esp_num <- suppressWarnings(as.numeric(as.character(esp)))
      if (!is.finite(obs_num) || !is.finite(esp_num)) {
        return(FALSE)
      }

      switch(
        op,
        menor_igual = obs_num <= esp_num + tolerancia_numerica,
        mayor_igual = obs_num >= esp_num - tolerancia_numerica,
        menor = obs_num < esp_num - tolerancia_numerica,
        mayor = obs_num > esp_num + tolerancia_numerica,
        FALSE
      )
    },
    logical(1)
  )
}

#' Evaluar una tabla general de controles OSIS
#'
#' Las tablas historicas sin columna `operador` conservan la comparacion por
#' igualdad. Tambien se admiten controles directos que solo contienen
#' `control` y `estado`.
#'
#' @param controles Data frame con `control`, `observado` y `esperado`, o con
#'   `control` y `estado` para controles directos.
#' @param tolerancia_numerica Tolerancia absoluta para valores numericos.
#'
#' @return La tabla original con `operador`, `cumple` y `estado`.
#' @keywords internal
evaluar_controles_osis <- function(
    controles,
    tolerancia_numerica = sqrt(.Machine$double.eps)) {
  if (!is.data.frame(controles)) {
    stop("`controles` debe ser un data frame.", call. = FALSE)
  }
  if (!"control" %in% names(controles)) {
    stop("`controles` debe contener la columna `control`.", call. = FALSE)
  }

  salida <- tibble::as_tibble(controles)
  if (nrow(salida) == 0L) {
    if (!"operador" %in% names(salida)) salida$operador <- character()
    if (!"cumple" %in% names(salida)) salida$cumple <- logical()
    if (!"estado" %in% names(salida)) salida$estado <- character()
    return(salida)
  }

  tiene_operandos <- all(c("observado", "esperado") %in% names(salida))
  tiene_estado <- "estado" %in% names(salida)

  if (!tiene_operandos && !tiene_estado) {
    stop(
      "`controles` debe contener `observado` y `esperado`, o un `estado` directo.",
      call. = FALSE
    )
  }

  operador_predeterminado <- rep("igual", nrow(salida))
  if (tiene_estado) {
    sin_operandos_fila <- if (tiene_operandos) {
      is.na(salida$observado) & is.na(salida$esperado)
    } else {
      rep(TRUE, nrow(salida))
    }
    operador_predeterminado[sin_operandos_fila] <- "directo"
  }

  if (!"operador" %in% names(salida)) {
    salida$operador <- operador_predeterminado
  } else {
    salida$operador <- as.character(salida$operador)
    sin_operador <- is.na(salida$operador) | !nzchar(salida$operador)
    salida$operador[sin_operador] <-
      operador_predeterminado[sin_operador]
  }

  estado_previo <- if (tiene_estado) as.character(salida$estado) else {
    rep(NA_character_, nrow(salida))
  }
  es_directo <- salida$operador == "directo"

  if (any(es_directo) && !tiene_estado) {
    stop(
      "Los controles con operador `directo` deben contener `estado`.",
      call. = FALSE
    )
  }

  cumple <- rep(FALSE, nrow(salida))
  cumple[es_directo] <- estado_previo[es_directo] == "OK"

  if (any(!es_directo)) {
    if (!tiene_operandos) {
      stop(
        "Los controles no directos requieren `observado` y `esperado`.",
        call. = FALSE
      )
    }
    cumple[!es_directo] <- evaluar_control_osis(
      observado = salida$observado[!es_directo],
      esperado = salida$esperado[!es_directo],
      operador = salida$operador[!es_directo],
      tolerancia_numerica = tolerancia_numerica
    )
  }

  salida$cumple <- cumple
  salida$estado <- ifelse(cumple, "OK", "ERROR")
  salida
}

#' Integrar controles nuevos con controles OSIS existentes
#'
#' @param controles_existentes Tabla de controles ya utilizada por el proceso.
#' @param controles_nuevos Tabla de controles que se desea agregar.
#' @param tolerancia_numerica Tolerancia absoluta para valores numericos.
#'
#' @return Tabla consolidada y evaluada.
#' @keywords internal
integrar_controles_osis <- function(
    controles_existentes = NULL,
    controles_nuevos = NULL,
    tolerancia_numerica = sqrt(.Machine$double.eps)) {
  tablas <- list()

  if (!is.null(controles_existentes)) {
    tablas[[length(tablas) + 1L]] <- evaluar_controles_osis(
      controles_existentes,
      tolerancia_numerica = tolerancia_numerica
    )
  }
  if (!is.null(controles_nuevos)) {
    tablas[[length(tablas) + 1L]] <- evaluar_controles_osis(
      controles_nuevos,
      tolerancia_numerica = tolerancia_numerica
    )
  }
  if (length(tablas) == 0L) {
    stop(
      "Debe suministrarse al menos una tabla de controles.",
      call. = FALSE
    )
  }

  salida <- dplyr::bind_rows(tablas)
  duplicados <- unique(salida$control[duplicated(salida$control)])
  if (length(duplicados) > 0L) {
    stop(
      "Hay nombres de control duplicados: ",
      paste(duplicados, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  salida
}

.validar_llaves_base_osis <- function(datos, llaves, nombre) {
  faltantes <- setdiff(llaves, names(datos))
  if (length(faltantes) > 0L) {
    stop(
      "`", nombre, "` no contiene las llaves: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.llaves_comparables_osis <- function(datos, llaves) {
  salida <- datos[llaves]
  for (llave in llaves) {
    salida[[llave]] <- as.character(salida[[llave]])
  }
  tibble::as_tibble(salida)
}

.evaluar_universo_osis <- function(especificacion,
                                   nombre,
                                   datos,
                                   lado) {
  campo <- paste0("universo_", lado)
  definicion <- especificacion[[campo]]

  if (is.null(definicion)) {
    return(rep(TRUE, nrow(datos)))
  }
  universo <- if (is.function(definicion)) {
    definicion(datos)
  } else if (
    is.character(definicion) &&
      length(definicion) == 1L &&
      definicion %in% names(datos)
  ) {
    datos[[definicion]]
  } else {
    definicion
  }

  if (!is.logical(universo) || length(universo) != nrow(datos)) {
    stop(
      "El universo `", lado, "` de `", nombre,
      "` debe devolver un vector logico de longitud nrow(datos).",
      call. = FALSE
    )
  }
  universo
}

.normalizar_estado_registro_osis <- function(x) {
  if (is.logical(x)) {
    return(x %in% TRUE)
  }
  if (is.numeric(x)) {
    return(!is.na(x) & x == 1)
  }
  codigo <- stringr::str_to_upper(
    normalizar_categoria_distribucion_osis(x)
  )
  codigo %in% c("1", "TRUE", "T", "SI", "SÍ", "VALIDO", "VALIDA", "OK")
}

.aplicar_estado_registro_osis <- function(universo,
                                          datos,
                                          columna_estado_registro,
                                          solo_casos_validos) {
  if (!isTRUE(solo_casos_validos) || is.null(columna_estado_registro)) {
    return(universo)
  }

  estado <- if (is.function(columna_estado_registro)) {
    columna_estado_registro(datos)
  } else {
    if (
      !is.character(columna_estado_registro) ||
        length(columna_estado_registro) != 1L ||
        !columna_estado_registro %in% names(datos)
    ) {
      stop(
        "`columna_estado_registro` no existe en la base.",
        call. = FALSE
      )
    }
    datos[[columna_estado_registro]]
  }

  universo %in% TRUE & .normalizar_estado_registro_osis(estado)
}

.contar_categorias_osis <- function(valores,
                                    universo,
                                    valores_validos,
                                    excluir_na) {
  valores <- normalizar_categoria_distribucion_osis(valores)
  valores <- valores[universo %in% TRUE]

  if (!isTRUE(excluir_na)) {
    valores[is.na(valores)] <- "<NA>"
  }

  dominio_definido <- !is.null(valores_validos)
  dominio <- if (dominio_definido) {
    unique(normalizar_categoria_distribucion_osis(valores_validos))
  } else {
    unique(valores[!is.na(valores)])
  }
  dominio <- dominio[!is.na(dominio)]
  if (!isTRUE(excluir_na) && !"<NA>" %in% dominio) {
    dominio <- c(dominio, "<NA>")
  }

  es_valida <- !is.na(valores) & valores %in% dominio
  valores_validos_observados <- valores[es_valida]
  valores_invalidos_observados <- if (dominio_definido) {
    valores[!is.na(valores) & !valores %in% dominio]
  } else {
    character()
  }

  contar <- function(x) {
    if (length(x) == 0L) {
      return(tibble::tibble(categoria = character(), n = integer()))
    }
    tibble::tibble(categoria = x) |>
      dplyr::count(.data$categoria, name = "n")
  }

  list(
    dominio = dominio,
    validas = contar(valores_validos_observados),
    invalidas = contar(valores_invalidos_observados),
    total_valido = length(valores_validos_observados),
    n_faltantes = sum(is.na(valores)),
    n_universo = length(valores)
  )
}

.extraer_conteo_categoria_osis <- function(tabla, categorias) {
  salida <- integer(length(categorias))
  if (nrow(tabla) == 0L || length(categorias) == 0L) {
    return(salida)
  }
  indice <- match(categorias, tabla$categoria)
  presente <- !is.na(indice)
  salida[presente] <- tabla$n[indice[presente]]
  salida
}

.detalle_distribucion_variable_osis <- function(
    nombre,
    descripcion,
    conteo_antes,
    conteo_despues,
    limite_pp,
    tolerancia_numerica) {
  categorias_validas <- unique(c(
    conteo_antes$dominio,
    conteo_despues$dominio,
    conteo_antes$validas$categoria,
    conteo_despues$validas$categoria
  ))
  frecuencias_antes <- dplyr::bind_rows(
    conteo_antes$validas,
    conteo_antes$invalidas
  ) |>
    dplyr::group_by(.data$categoria) |>
    dplyr::summarise(n_antes = sum(.data$n), .groups = "drop")
  frecuencias_despues <- dplyr::bind_rows(
    conteo_despues$validas,
    conteo_despues$invalidas
  ) |>
    dplyr::group_by(.data$categoria) |>
    dplyr::summarise(n_despues = sum(.data$n), .groups = "drop")
  categorias_configuradas <- tibble::tibble(
    categoria = categorias_validas
  )
  cruce_categorias <- dplyr::full_join(
    frecuencias_antes,
    frecuencias_despues,
    by = "categoria"
  ) |>
    dplyr::full_join(
      categorias_configuradas,
      by = "categoria"
    ) |>
    dplyr::mutate(
      n_antes = dplyr::coalesce(.data$n_antes, 0L),
      n_despues = dplyr::coalesce(.data$n_despues, 0L)
    )
  categorias <- cruce_categorias$categoria

  if (length(categorias) == 0L) {
    return(tibble::tibble(
      variable = character(),
      descripcion_variable = character(),
      categoria = character(),
      n_antes = integer(),
      total_antes = integer(),
      proporcion_antes = double(),
      porcentaje_antes = double(),
      n_despues = integer(),
      total_despues = integer(),
      proporcion_despues = double(),
      porcentaje_despues = double(),
      diferencia_pp = double(),
      diferencia_absoluta_pp = double(),
      limite_pp = double(),
      cumple_categoria = logical(),
      categoria_valida = logical(),
      tipo_categoria = character(),
      presencia_categoria = character(),
      categoria_exclusiva_antes = logical(),
      categoria_exclusiva_despues = logical()
    ))
  }

  categoria_valida <- categorias %in% categorias_validas
  n_validos_antes <- .extraer_conteo_categoria_osis(
    conteo_antes$validas,
    categorias
  )
  n_validos_despues <- .extraer_conteo_categoria_osis(
    conteo_despues$validas,
    categorias
  )
  n_invalidos_antes <- .extraer_conteo_categoria_osis(
    conteo_antes$invalidas,
    categorias
  )
  n_invalidos_despues <- .extraer_conteo_categoria_osis(
    conteo_despues$invalidas,
    categorias
  )
  n_antes <- cruce_categorias$n_antes
  n_despues <- cruce_categorias$n_despues

  proporcion_antes <- rep(NA_real_, length(categorias))
  proporcion_despues <- rep(NA_real_, length(categorias))
  if (conteo_antes$total_valido > 0L) {
    proporcion_antes[categoria_valida] <-
      n_validos_antes[categoria_valida] / conteo_antes$total_valido
  }
  if (conteo_despues$total_valido > 0L) {
    proporcion_despues[categoria_valida] <-
      n_validos_despues[categoria_valida] / conteo_despues$total_valido
  }

  diferencia_pp <- 100 * (proporcion_despues - proporcion_antes)
  diferencia_absoluta_pp <- abs(diferencia_pp)
  cumple_categoria <- ifelse(
    categoria_valida,
    !is.na(diferencia_absoluta_pp) &
      diferencia_absoluta_pp <= limite_pp + tolerancia_numerica,
    n_invalidos_despues == 0L
  )

  presencia_categoria <- dplyr::case_when(
    n_antes > 0L & n_despues > 0L ~ "ambas",
    n_antes > 0L & n_despues == 0L ~ "solo_antes",
    n_antes == 0L & n_despues > 0L ~ "solo_despues",
    TRUE ~ "sin_frecuencia"
  )

  tibble::tibble(
    variable = nombre,
    descripcion_variable = descripcion,
    categoria = categorias,
    n_antes = n_antes,
    total_antes = conteo_antes$total_valido,
    proporcion_antes = proporcion_antes,
    porcentaje_antes = 100 * proporcion_antes,
    n_despues = n_despues,
    total_despues = conteo_despues$total_valido,
    proporcion_despues = proporcion_despues,
    porcentaje_despues = 100 * proporcion_despues,
    diferencia_pp = diferencia_pp,
    diferencia_absoluta_pp = diferencia_absoluta_pp,
    limite_pp = limite_pp,
    cumple_categoria = cumple_categoria,
    categoria_valida = categoria_valida,
    tipo_categoria = ifelse(categoria_valida, "valida", "invalida"),
    presencia_categoria = presencia_categoria,
    categoria_exclusiva_antes =
      n_antes > 0L & n_despues == 0L,
    categoria_exclusiva_despues =
      n_antes == 0L & n_despues > 0L
  )
}

.resumir_distribucion_variable_osis <- function(detalle,
                                                nombre,
                                                descripcion,
                                                total_antes,
                                                total_despues,
                                                limite_pp) {
  detalle_valido <- detalle[detalle$categoria_valida %in% TRUE, , drop = FALSE]
  diferencias <- detalle_valido$diferencia_absoluta_pp
  diferencias_finitas <- is.finite(diferencias)

  if (any(diferencias_finitas)) {
    maxima <- max(diferencias[diferencias_finitas])
    categoria_maxima <- detalle_valido$categoria[
      which(diferencias == maxima)[1L]
    ]
  } else {
    maxima <- NA_real_
    categoria_maxima <- NA_character_
  }

  incumple <- !detalle$cumple_categoria
  categorias_incumplidas <- detalle$categoria[incumple]
  categorias_incumplidas <- if (length(categorias_incumplidas) == 0L) {
    ""
  } else {
    paste(categorias_incumplidas, collapse = ", ")
  }

  cumple_variable <-
    total_antes > 0L &&
    total_despues > 0L &&
    nrow(detalle_valido) > 0L &&
    all(detalle$cumple_categoria)

  tibble::tibble(
    variable = nombre,
    descripcion_variable = descripcion,
    total_antes = total_antes,
    total_despues = total_despues,
    numero_categorias = nrow(detalle_valido),
    maxima_diferencia_absoluta_pp = maxima,
    categoria_maxima_diferencia = categoria_maxima,
    limite_pp = limite_pp,
    categorias_incumplidas = categorias_incumplidas,
    cumple_variable = cumple_variable,
    estado = ifelse(cumple_variable, "OK", "ERROR")
  )
}

#' Calcular homogeneidad y Cramer V para distribuciones OSIS
#'
#' La prueba es exclusivamente diagnostica. Cuando alguna frecuencia esperada
#' es menor que cinco se calcula un p-valor simulado con semilla reproducible.
#' Cramer V se clasifica con umbrales descriptivos: menor que 0.10,
#' `sin_efecto_apreciable`; menor que 0.30, `efecto_pequeno`; menor que 0.50,
#' `efecto_moderado`; y desde 0.50, `efecto_grande`.
#'
#' @param detalle Tabla producida por la validacion de distribuciones.
#' @param semilla Semilla para simulaciones.
#' @param simulaciones Numero de replicaciones Monte Carlo.
#'
#' @return Una fila por variable con resultados estadisticos.
#' @keywords internal
calcular_homogeneidad_distribuciones_osis <- function(
    detalle,
    semilla = 20260703L,
    simulaciones = 2000L) {
  if (!is.data.frame(detalle)) {
    stop("`detalle` debe ser un data frame.", call. = FALSE)
  }
  requeridas <- c(
    "variable",
    "categoria",
    "categoria_valida",
    "n_antes",
    "n_despues"
  )
  faltantes <- setdiff(requeridas, names(detalle))
  if (length(faltantes) > 0L) {
    stop(
      "`detalle` no contiene: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    !is.numeric(semilla) ||
      length(semilla) != 1L ||
      is.na(semilla)
  ) {
    stop("`semilla` debe ser numerica escalar.", call. = FALSE)
  }
  if (
    !is.numeric(simulaciones) ||
      length(simulaciones) != 1L ||
      is.na(simulaciones) ||
      simulaciones < 100L
  ) {
    stop("`simulaciones` debe ser al menos 100.", call. = FALSE)
  }

  variables <- unique(as.character(detalle$variable))
  filas <- lapply(seq_along(variables), function(i) {
    variable <- variables[[i]]
    datos <- detalle |>
      dplyr::filter(
        .data$variable == .env$variable,
        .data$categoria_valida,
        .data$n_antes + .data$n_despues > 0L
      )

    salida_no_calculable <- function(advertencia) {
      tibble::tibble(
        variable = variable,
        chi_cuadrado = NA_real_,
        grados_libertad = NA_real_,
        p_valor = NA_real_,
        metodo_prueba = "chi_cuadrado_homogeneidad_no_calculable",
        prueba_estadistica_calculable = FALSE,
        advertencia_prueba = advertencia,
        cramers_v = NA_real_,
        clasificacion_cramers_v = "no_calculable"
      )
    }

    if (nrow(datos) < 2L) {
      return(salida_no_calculable(
        "Se requieren al menos dos categorias con frecuencia positiva."
      ))
    }

    tabla <- rbind(
      antes = as.numeric(datos$n_antes),
      despues = as.numeric(datos$n_despues)
    )
    colnames(tabla) <- as.character(datos$categoria)
    if (any(rowSums(tabla) == 0) || sum(tabla) == 0) {
      return(salida_no_calculable(
        "La tabla contiene un total de fila igual a cero."
      ))
    }

    advertencias <- character()
    prueba_inicial <- tryCatch(
      withCallingHandlers(
        stats::chisq.test(tabla, correct = FALSE),
        warning = function(w) {
          advertencias <<- c(advertencias, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    if (inherits(prueba_inicial, "error")) {
      return(salida_no_calculable(conditionMessage(prueba_inicial)))
    }

    usar_simulacion <- any(prueba_inicial$expected < 5)
    prueba <- prueba_inicial
    metodo <- "chi_cuadrado_homogeneidad_asintotico"

    if (usar_simulacion) {
      existia_semilla <- exists(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
      if (existia_semilla) {
        semilla_anterior <- get(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )
      }
      on.exit({
        if (existia_semilla) {
          assign(
            ".Random.seed",
            semilla_anterior,
            envir = .GlobalEnv
          )
        } else if (exists(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)

      set.seed(as.integer(semilla) + i - 1L)
      advertencias <- c(
        advertencias,
        paste0(
          "Frecuencias esperadas menores que cinco; ",
          "p-valor simulado con B=",
          as.integer(simulaciones),
          "."
        )
      )
      prueba_simulada <- tryCatch(
        withCallingHandlers(
          stats::chisq.test(
            tabla,
            correct = FALSE,
            simulate.p.value = TRUE,
            B = as.integer(simulaciones)
          ),
          warning = function(w) {
            advertencias <<- c(advertencias, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) e
      )
      if (!inherits(prueba_simulada, "error")) {
        prueba <- prueba_simulada
        metodo <- "chi_cuadrado_homogeneidad_monte_carlo"
      } else {
        advertencias <- c(
          advertencias,
          paste0(
            "No fue posible simular: ",
            conditionMessage(prueba_simulada)
          )
        )
      }
    }

    estadistico <- unname(as.numeric(prueba$statistic))
    n_total <- sum(tabla)
    denominador <- n_total * min(nrow(tabla) - 1L, ncol(tabla) - 1L)
    cramers_v <- if (
      is.finite(estadistico) &&
        is.finite(denominador) &&
        denominador > 0
    ) {
      sqrt(estadistico / denominador)
    } else {
      NA_real_
    }
    clasificacion <- dplyr::case_when(
      is.na(cramers_v) ~ "no_calculable",
      cramers_v < 0.10 ~ "sin_efecto_apreciable",
      cramers_v < 0.30 ~ "efecto_pequeno",
      cramers_v < 0.50 ~ "efecto_moderado",
      TRUE ~ "efecto_grande"
    )

    tibble::tibble(
      variable = variable,
      chi_cuadrado = estadistico,
      grados_libertad = as.numeric(
        (nrow(tabla) - 1L) * (ncol(tabla) - 1L)
      ),
      p_valor = unname(as.numeric(prueba$p.value)),
      metodo_prueba = metodo,
      prueba_estadistica_calculable = TRUE,
      advertencia_prueba = paste(
        unique(advertencias[nzchar(advertencias)]),
        collapse = " | "
      ),
      cramers_v = cramers_v,
      clasificacion_cramers_v = clasificacion
    )
  })

  dplyr::bind_rows(filas)
}

.construir_controles_distribuciones_osis <- function(resumen, detalle) {
  controles_cambio <- resumen |>
    dplyr::transmute(
      control = paste0(
        "distribucion_",
        stringr::str_to_lower(.data$variable),
        "_cambio_maximo_pp"
      ),
      observado = .data$maxima_diferencia_absoluta_pp,
      esperado = .data$limite_pp,
      operador = "menor_igual"
    )

  invalidas_final <- detalle |>
    dplyr::filter(
      !.data$categoria_valida,
      .data$n_despues > 0L
    ) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      n_invalidas = sum(.data$n_despues),
      .groups = "drop"
    )

  controles_dominio <- resumen |>
    dplyr::select(.data$variable) |>
    dplyr::left_join(invalidas_final, by = "variable") |>
    dplyr::mutate(
      n_invalidas = dplyr::coalesce(.data$n_invalidas, 0L)
    ) |>
    dplyr::transmute(
      control = paste0(
        "dominio_",
        stringr::str_to_lower(.data$variable),
        "_categorias_invalidas_final"
      ),
      observado = as.numeric(.data$n_invalidas),
      esperado = 0,
      operador = "igual"
    )

  integrar_controles_osis(
    controles_existentes = controles_cambio,
    controles_nuevos = controles_dominio
  )
}

.auditar_preservacion_variable_osis <- function(
    base_antes,
    base_despues,
    llaves,
    nombre,
    variable_antes,
    variable_despues,
    universo_antes,
    universo_despues,
    valores_validos,
    llaves_unicas) {
  nombre_control <- paste0(
    "observados_validos_",
    stringr::str_to_lower(nombre),
    "_sobrescritos"
  )

  if (!isTRUE(llaves_unicas)) {
    return(list(
      detalle = tibble::tibble(),
      control = evaluar_controles_osis(tibble::tibble(
        control = nombre_control,
        observado = NA_real_,
        esperado = 0,
        operador = "igual"
      ))
    ))
  }

  valor_original <- normalizar_categoria_distribucion_osis(
    base_antes[[variable_antes]]
  )
  valor_final <- normalizar_categoria_distribucion_osis(
    base_despues[[variable_despues]]
  )
  dominio <- if (is.null(valores_validos)) {
    NULL
  } else {
    unique(normalizar_categoria_distribucion_osis(valores_validos))
  }
  observado_valido <- universo_antes %in% TRUE & !is.na(valor_original)
  if (!is.null(dominio)) {
    observado_valido <- observado_valido & valor_original %in% dominio
  }

  antes <- .llaves_comparables_osis(base_antes, llaves)
  antes$valor_original_observado <- valor_original
  antes <- antes[observado_valido, , drop = FALSE]

  despues <- .llaves_comparables_osis(base_despues, llaves)
  despues$valor_final <- valor_final
  despues$universo_final <- universo_despues %in% TRUE
  despues$llave_presente_final <- TRUE

  comparacion <- dplyr::left_join(
    antes,
    despues,
    by = llaves
  )
  sobrescrita <- is.na(comparacion$llave_presente_final) |
    (
      comparacion$universo_final %in% TRUE &
        (
          is.na(comparacion$valor_final) |
            comparacion$valor_final != comparacion$valor_original_observado
        )
    )
  detalle <- comparacion[sobrescrita, , drop = FALSE]
  if (nrow(detalle) > 0L) {
    detalle$variable <- nombre
    detalle$motivo <- ifelse(
      is.na(detalle$llave_presente_final),
      "llave_observada_ausente_en_base_final",
      "valor_observado_valido_sobrescrito"
    )
    detalle <- detalle |>
      dplyr::select(
        dplyr::all_of(llaves),
        .data$variable,
        .data$valor_original_observado,
        .data$valor_final,
        .data$universo_final,
        .data$motivo
      )
  } else {
    detalle <- tibble::tibble(
      !!!rlang::set_names(
        rep(list(character()), length(llaves)),
        llaves
      ),
      variable = character(),
      valor_original_observado = character(),
      valor_final = character(),
      universo_final = logical(),
      motivo = character()
    )
  }

  list(
    detalle = detalle,
    control = evaluar_controles_osis(tibble::tibble(
      control = nombre_control,
      observado = nrow(detalle),
      esperado = 0,
      operador = "igual"
    ))
  )
}

#' Validar distribuciones antes y despues para una base OSIS
#'
#' Compara categorias validas en los universos configurados. Conserva
#' categorias con frecuencia cero, identifica codigos invalidos y crea los
#' controles que pueden anexarse a la aceptacion general.
#'
#' Cada elemento de `configuracion_variables` puede contener:
#' `variable_antes`, `variable_despues`, `universo_antes`,
#' `universo_despues`, `valores_validos`, `excluir_na` y `descripcion`.
#'
#' @param base_antes Base con respuestas originales.
#' @param base_despues Base final, ya depurada, que se enviara a OSIS.
#' @param configuracion_variables Lista nombrada de especificaciones.
#' @param limite_pp Maximo cambio absoluto permitido, en puntos porcentuales.
#' @param llaves Llaves del nivel analitico.
#' @param columna_estado_registro Columna o funcion opcional que identifica
#'   registros validos.
#' @param solo_casos_validos Si es `TRUE`, aplica `columna_estado_registro`.
#' @param tolerancia_numerica Tolerancia para el limite.
#' @param exigir_mismas_llaves Si es `TRUE`, el control estructural exige las
#'   mismas filas, llaves y orden en ambas bases.
#' @param ejecutar_prueba_estadistica Si es `TRUE`, agrega chi-cuadrado de
#'   homogeneidad y Cramer V como diagnostico no vinculante.
#' @param semilla_prueba Semilla reproducible para Monte Carlo.
#' @param simulaciones_prueba Numero de simulaciones cuando los supuestos
#'   asintoticos no se cumplen.
#' @param imprimir Si es `TRUE`, imprime el resumen por variable.
#'
#' @return Lista con detalle, resumen, categorias invalidas, preservacion de
#'   observados, controles de distribucion, controles generales y parametros.
#' @keywords internal
validar_distribuciones_antes_despues <- function(
    base_antes,
    base_despues,
    configuracion_variables,
    limite_pp = 5,
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    columna_estado_registro = NULL,
    solo_casos_validos = TRUE,
    tolerancia_numerica = sqrt(.Machine$double.eps),
    exigir_mismas_llaves = TRUE,
    ejecutar_prueba_estadistica = TRUE,
    semilla_prueba = 20260703L,
    simulaciones_prueba = 2000L,
    imprimir = TRUE) {
  if (!is.data.frame(base_antes) || !is.data.frame(base_despues)) {
    stop(
      "`base_antes` y `base_despues` deben ser data frames.",
      call. = FALSE
    )
  }
  if (
    !is.list(configuracion_variables) ||
      length(configuracion_variables) == 0L ||
      is.null(names(configuracion_variables)) ||
      any(!nzchar(names(configuracion_variables)))
  ) {
    stop(
      "`configuracion_variables` debe ser una lista nombrada no vacia.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(limite_pp) ||
      length(limite_pp) != 1L ||
      !is.finite(limite_pp) ||
      limite_pp < 0
  ) {
    stop("`limite_pp` debe ser un numero no negativo.", call. = FALSE)
  }
  if (!is.character(llaves) || length(llaves) == 0L || any(!nzchar(llaves))) {
    stop("`llaves` debe ser un vector de nombres no vacio.", call. = FALSE)
  }

  .validar_llaves_base_osis(base_antes, llaves, "base_antes")
  .validar_llaves_base_osis(base_despues, llaves, "base_despues")

  llaves_antes <- .llaves_comparables_osis(base_antes, llaves)
  llaves_despues <- .llaves_comparables_osis(base_despues, llaves)
  duplicadas_antes <- sum(duplicated(llaves_antes))
  duplicadas_despues <- sum(duplicated(llaves_despues))
  mismas_llaves_y_orden <- identical(llaves_antes, llaves_despues)

  controles_integridad <- tibble::tibble(
    control = c(
      "filas_base_final_osis",
      "llaves_duplicadas_base_antes",
      "llaves_duplicadas_base_despues",
      "llaves_y_orden_modificados"
    ),
    observado = c(
      nrow(base_despues),
      duplicadas_antes,
      duplicadas_despues,
      as.integer(!mismas_llaves_y_orden)
    ),
    esperado = c(
      if (isTRUE(exigir_mismas_llaves)) nrow(base_antes) else nrow(base_despues),
      0,
      0,
      if (isTRUE(exigir_mismas_llaves)) {
        0
      } else {
        as.integer(!mismas_llaves_y_orden)
      }
    ),
    operador = "igual"
  )
  controles_integridad <- evaluar_controles_osis(controles_integridad)

  detalles <- vector("list", length(configuracion_variables))
  resumenes <- vector("list", length(configuracion_variables))
  auditorias_preservacion <- vector(
    "list",
    length(configuracion_variables)
  )
  controles_preservacion <- vector(
    "list",
    length(configuracion_variables)
  )
  llaves_unicas <-
    duplicadas_antes == 0L &&
    duplicadas_despues == 0L

  for (i in seq_along(configuracion_variables)) {
    nombre <- names(configuracion_variables)[[i]]
    especificacion <- configuracion_variables[[i]]
    if (!is.list(especificacion)) {
      stop(
        "La configuracion de `", nombre, "` debe ser una lista.",
        call. = FALSE
      )
    }

    variable_antes <- .valor_o_osis(
      especificacion$variable_antes,
      nombre
    )
    variable_despues <- .valor_o_osis(
      especificacion$variable_despues,
      nombre
    )
    descripcion <- .valor_o_osis(especificacion$descripcion, nombre)
    excluir_na <- .valor_o_osis(especificacion$excluir_na, TRUE)

    faltantes_antes <- setdiff(variable_antes, names(base_antes))
    faltantes_despues <- setdiff(variable_despues, names(base_despues))
    if (length(faltantes_antes) > 0L || length(faltantes_despues) > 0L) {
      stop(
        "Faltan columnas para `", nombre, "`: ",
        paste(c(faltantes_antes, faltantes_despues), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    if (!is.logical(excluir_na) || length(excluir_na) != 1L) {
      stop(
        "`excluir_na` de `", nombre, "` debe ser logico escalar.",
        call. = FALSE
      )
    }

    universo_antes <- .evaluar_universo_osis(
      especificacion,
      nombre,
      base_antes,
      "antes"
    )
    universo_despues <- .evaluar_universo_osis(
      especificacion,
      nombre,
      base_despues,
      "despues"
    )
    universo_antes <- .aplicar_estado_registro_osis(
      universo_antes,
      base_antes,
      columna_estado_registro,
      solo_casos_validos
    )
    universo_despues <- .aplicar_estado_registro_osis(
      universo_despues,
      base_despues,
      columna_estado_registro,
      solo_casos_validos
    )

    conteo_antes <- .contar_categorias_osis(
      base_antes[[variable_antes]],
      universo_antes,
      especificacion$valores_validos,
      excluir_na
    )
    conteo_despues <- .contar_categorias_osis(
      base_despues[[variable_despues]],
      universo_despues,
      especificacion$valores_validos,
      excluir_na
    )

    detalle <- .detalle_distribucion_variable_osis(
      nombre = nombre,
      descripcion = descripcion,
      conteo_antes = conteo_antes,
      conteo_despues = conteo_despues,
      limite_pp = limite_pp,
      tolerancia_numerica = tolerancia_numerica
    )
    resumen <- .resumir_distribucion_variable_osis(
      detalle = detalle,
      nombre = nombre,
      descripcion = descripcion,
      total_antes = conteo_antes$total_valido,
      total_despues = conteo_despues$total_valido,
      limite_pp = limite_pp
    )
    preservacion <- .auditar_preservacion_variable_osis(
      base_antes = base_antes,
      base_despues = base_despues,
      llaves = llaves,
      nombre = nombre,
      variable_antes = variable_antes,
      variable_despues = variable_despues,
      universo_antes = universo_antes,
      universo_despues = universo_despues,
      valores_validos = especificacion$valores_validos,
      llaves_unicas = llaves_unicas
    )
    detalles[[i]] <- detalle
    resumenes[[i]] <- resumen
    auditorias_preservacion[[i]] <- preservacion$detalle
    controles_preservacion[[i]] <- preservacion$control
  }

  detalle_distribuciones <- dplyr::bind_rows(detalles)
  resumen_distribuciones <- dplyr::bind_rows(resumenes)
  if (isTRUE(ejecutar_prueba_estadistica)) {
    estadisticas <- calcular_homogeneidad_distribuciones_osis(
      detalle_distribuciones,
      semilla = semilla_prueba,
      simulaciones = simulaciones_prueba
    )
  } else {
    estadisticas <- tibble::tibble(
      variable = resumen_distribuciones$variable,
      chi_cuadrado = NA_real_,
      grados_libertad = NA_real_,
      p_valor = NA_real_,
      metodo_prueba = "no_ejecutada",
      prueba_estadistica_calculable = FALSE,
      advertencia_prueba = "Prueba estadistica desactivada.",
      cramers_v = NA_real_,
      clasificacion_cramers_v = "no_calculable"
    )
  }
  resumen_distribuciones <- resumen_distribuciones |>
    dplyr::left_join(estadisticas, by = "variable")

  detalle_distribuciones <- detalle_distribuciones |>
    dplyr::left_join(
      resumen_distribuciones |>
        dplyr::select(
          .data$variable,
          maxima_diferencia_variable_pp =
            .data$maxima_diferencia_absoluta_pp,
          cumple_variable = .data$cumple_variable,
          estado_general_variable = .data$estado
        ),
      by = "variable"
    ) |>
    dplyr::arrange(.data$variable, .data$categoria)

  categorias_invalidas <- detalle_distribuciones |>
    dplyr::filter(
      !.data$categoria_valida,
      .data$n_antes > 0L | .data$n_despues > 0L
    ) |>
    dplyr::select(
      .data$variable,
      .data$descripcion_variable,
      .data$categoria,
      .data$n_antes,
      .data$n_despues,
      .data$presencia_categoria
    )

  controles_distribuciones <-
    .construir_controles_distribuciones_osis(
      resumen_distribuciones,
      detalle_distribuciones
    )
  observados_validos_sobrescritos <- dplyr::bind_rows(
    auditorias_preservacion
  )
  controles_preservacion <- dplyr::bind_rows(controles_preservacion)
  controles_validacion_osis <- integrar_controles_osis(
    integrar_controles_osis(
      controles_integridad,
      controles_distribuciones
    ),
    controles_preservacion
  )

  resultado <- list(
    detalle_distribuciones = detalle_distribuciones,
    resumen_distribuciones = resumen_distribuciones,
    categorias_invalidas = categorias_invalidas,
    observados_validos_sobrescritos =
      observados_validos_sobrescritos,
    controles_distribuciones = controles_distribuciones,
    controles_preservacion = controles_preservacion,
    controles_integridad = controles_integridad,
    controles_validacion_osis = controles_validacion_osis,
    parametros_validacion_distribuciones = list(
      limite_pp = limite_pp,
      unidad = "puntos_porcentuales",
      metodo = "diferencia_absoluta_por_categoria",
      base_antes = "respuestas_originales_validas",
      base_despues = "base_final_depurada_para_osis",
      llaves = llaves,
      exigir_mismas_llaves = isTRUE(exigir_mismas_llaves),
      prueba_estadistica = isTRUE(ejecutar_prueba_estadistica),
      prueba_estadistica_vinculante = FALSE,
      semilla_prueba = as.integer(semilla_prueba),
      simulaciones_prueba = as.integer(simulaciones_prueba),
      umbrales_cramers_v = c(
        sin_efecto_apreciable = 0.10,
        efecto_pequeno = 0.30,
        efecto_moderado = 0.50
      )
    )
  )

  if (isTRUE(imprimir)) {
    imprimir_resumen_distribuciones_osis(resultado)
  }
  resultado
}

#' Depurar respuestas fuera de flujo antes de construir la base OSIS
#'
#' Solo limpia casos cuyo universo sea explicitamente `FALSE`. Los `NA` de la
#' regla se conservan como flujo indeterminado para no confundirlos con casos
#' fuera del universo.
#'
#' @param base Base que se desea depurar.
#' @param configuracion_flujo Lista nombrada. Cada elemento requiere una
#'   funcion o vector `universo`; puede incluir `variable_original`,
#'   `regla_flujo` y `motivo`.
#' @param llaves Llaves del nivel analitico.
#'
#' @return Lista con base depurada, trazabilidad, resumen y controles.
#' @keywords internal
depurar_respuestas_fuera_flujo_osis <- function(
    base,
    configuracion_flujo,
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) {
  if (!is.data.frame(base)) {
    stop("`base` debe ser un data frame.", call. = FALSE)
  }
  if (
    !is.list(configuracion_flujo) ||
      length(configuracion_flujo) == 0L ||
      is.null(names(configuracion_flujo)) ||
      any(!nzchar(names(configuracion_flujo)))
  ) {
    stop(
      "`configuracion_flujo` debe ser una lista nombrada no vacia.",
      call. = FALSE
    )
  }
  .validar_llaves_base_osis(base, llaves, "base")
  llaves_originales <- base[llaves]
  if (anyDuplicated(llaves_originales) > 0L) {
    stop("Las llaves de `base` no son unicas.", call. = FALSE)
  }

  salida <- tibble::as_tibble(base)
  trazas <- vector("list", length(configuracion_flujo))
  resumenes <- vector("list", length(configuracion_flujo))

  for (i in seq_along(configuracion_flujo)) {
    variable <- names(configuracion_flujo)[[i]]
    especificacion <- configuracion_flujo[[i]]
    if (!is.list(especificacion) || is.null(especificacion$universo)) {
      stop(
        "La configuracion de `", variable,
        "` debe contener `universo`.",
        call. = FALSE
      )
    }
    if (!variable %in% names(salida)) {
      stop("No existe la variable `", variable, "`.", call. = FALSE)
    }

    universo <- .evaluar_universo_osis(
      list(universo_despues = especificacion$universo),
      variable,
      salida,
      "despues"
    )
    valor_antes <- salida[[variable]]
    tiene_respuesta <- !is.na(
      normalizar_categoria_distribucion_osis(valor_antes)
    )
    fuera_flujo <- universo %in% FALSE & tiene_respuesta
    indeterminado_con_respuesta <- is.na(universo) & tiene_respuesta

    variable_original <- especificacion$variable_original
    if (is.null(variable_original)) {
      candidata <- paste0(variable, "_original")
      variable_original <- if (candidata %in% names(salida)) {
        candidata
      } else {
        variable
      }
    }
    if (!variable_original %in% names(salida)) {
      stop(
        "No existe `", variable_original,
        "` para la trazabilidad de `", variable, "`.",
        call. = FALSE
      )
    }

    salida[[variable]][fuera_flujo] <- NA
    valor_despues <- salida[[variable]]
    regla_flujo <- .valor_o_osis(
      especificacion$regla_flujo,
      paste0("universo_configurado_", variable)
    )
    motivo <- .valor_o_osis(
      especificacion$motivo,
      "respuesta_fuera_de_flujo"
    )

    if (is.function(regla_flujo)) regla_flujo <- regla_flujo(salida)
    if (is.function(motivo)) motivo <- motivo(salida)
    regla_flujo <- rep(as.character(regla_flujo), length.out = nrow(salida))
    motivo <- rep(as.character(motivo), length.out = nrow(salida))

    traza_fuera <- salida[fuera_flujo, llaves, drop = FALSE]
    traza_fuera$variable <- variable
    traza_fuera$valor_original <- as.character(
      salida[[variable_original]][fuera_flujo]
    )
    traza_fuera$valor_antes_depuracion <- as.character(
      valor_antes[fuera_flujo]
    )
    traza_fuera$valor_despues_depuracion <- as.character(
      valor_despues[fuera_flujo]
    )
    traza_fuera$regla_flujo <- regla_flujo[fuera_flujo]
    traza_fuera$motivo <- motivo[fuera_flujo]
    traza_fuera$accion <- "convertido_a_na_por_fuera_de_flujo"
    traza_fuera$resultado <- as.character(ifelse(
      is.na(valor_despues[fuera_flujo]),
      "OK",
      "ERROR"
    ))

    traza_indeterminada <- salida[
      indeterminado_con_respuesta,
      llaves,
      drop = FALSE
    ]
    traza_indeterminada$variable <- variable
    traza_indeterminada$valor_original <- as.character(
      salida[[variable_original]][indeterminado_con_respuesta]
    )
    traza_indeterminada$valor_antes_depuracion <- as.character(
      valor_antes[indeterminado_con_respuesta]
    )
    traza_indeterminada$valor_despues_depuracion <- as.character(
      valor_despues[indeterminado_con_respuesta]
    )
    traza_indeterminada$regla_flujo <-
      regla_flujo[indeterminado_con_respuesta]
    traza_indeterminada$motivo <-
      "flujo_indeterminado_no_depurado"
    traza_indeterminada$accion <-
      "sin_cambio_flujo_indeterminado"
    traza_indeterminada$resultado <- "OK"

    trazas[[i]] <- dplyr::bind_rows(
      tibble::as_tibble(traza_fuera),
      tibble::as_tibble(traza_indeterminada)
    )

    resumenes[[i]] <- tibble::tibble(
      variable = variable,
      regla_flujo = paste(unique(regla_flujo), collapse = " | "),
      valores_borrados = sum(fuera_flujo),
      flujo_indeterminado_con_respuesta =
        sum(indeterminado_con_respuesta),
      respuestas_fuera_flujo_restantes = sum(
        universo %in% FALSE &
          !is.na(normalizar_categoria_distribucion_osis(
            salida[[variable]]
          ))
      )
    )
  }

  if (
    nrow(salida) != nrow(base) ||
      !identical(salida[llaves], llaves_originales) ||
      anyDuplicated(salida[llaves]) > 0L
  ) {
    stop(
      "La depuracion modifico filas, orden o llaves.",
      call. = FALSE
    )
  }

  trazabilidad_depuracion <- dplyr::bind_rows(trazas)
  if (nrow(trazabilidad_depuracion) == 0L) {
    trazabilidad_depuracion <- tibble::tibble(
      !!!rlang::set_names(
        rep(list(character()), length(llaves)),
        llaves
      ),
      variable = character(),
      valor_original = character(),
      valor_antes_depuracion = character(),
      valor_despues_depuracion = character(),
      regla_flujo = character(),
      motivo = character(),
      accion = character(),
      resultado = character()
    )
  }
  resumen_depuracion <- dplyr::bind_rows(resumenes)
  controles_depuracion <- tibble::tibble(
    control = c(
      "filas_depuracion_osis",
      "llaves_duplicadas_despues_depuracion",
      "respuestas_fuera_flujo_restantes"
    ),
    observado = c(
      nrow(salida),
      sum(duplicated(salida[llaves])),
      sum(resumen_depuracion$respuestas_fuera_flujo_restantes)
    ),
    esperado = c(nrow(base), 0, 0),
    operador = "igual"
  ) |>
    evaluar_controles_osis()

  list(
    base_depurada = salida,
    trazabilidad_depuracion = trazabilidad_depuracion,
    resumen_depuracion = resumen_depuracion,
    controles_depuracion = controles_depuracion,
    parametros_depuracion = list(
      llaves = llaves,
      accion = "convertido_a_na_por_fuera_de_flujo",
      limpiar_flujo_indeterminado = FALSE
    )
  )
}

.validar_resultado_distribuciones_osis <- function(resultado) {
  requeridos <- c(
    "detalle_distribuciones",
    "resumen_distribuciones",
    "controles_distribuciones",
    "parametros_validacion_distribuciones"
  )
  faltantes <- setdiff(requeridos, names(resultado))
  if (!is.list(resultado) || length(faltantes) > 0L) {
    stop(
      "`resultado` no cumple el contrato de validacion de distribuciones.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.agregar_hoja_osis <- function(wb, hoja, datos) {
  openxlsx::addWorksheet(wb, hoja)
  openxlsx::writeData(
    wb,
    hoja,
    datos,
    withFilter = TRUE,
    na.string = ""
  )
  openxlsx::freezePane(wb, hoja, firstRow = TRUE)

  if (ncol(datos) > 0L) {
    openxlsx::setColWidths(
      wb,
      hoja,
      cols = seq_len(ncol(datos)),
      widths = "auto"
    )
    estilo_encabezado <- openxlsx::createStyle(
      textDecoration = "bold",
      halign = "center",
      border = "bottom"
    )
    openxlsx::addStyle(
      wb,
      hoja,
      style = estilo_encabezado,
      rows = 1,
      cols = seq_len(ncol(datos)),
      gridExpand = TRUE
    )
  }

  if (nrow(datos) > 0L) {
    columnas_proporcion <- which(names(datos) %in% c(
      "proporcion_antes",
      "proporcion_despues"
    ))
    columnas_pp <- which(names(datos) %in% c(
      "porcentaje_antes",
      "porcentaje_despues",
      "diferencia_pp",
      "diferencia_absoluta_pp",
      "limite_pp",
      "maxima_diferencia_variable_pp",
      "maxima_diferencia_absoluta_pp"
    ))
    if (length(columnas_proporcion) > 0L) {
      openxlsx::addStyle(
        wb,
        hoja,
        style = openxlsx::createStyle(numFmt = "0.00%"),
        rows = 2:(nrow(datos) + 1L),
        cols = columnas_proporcion,
        gridExpand = TRUE
      )
    }
    if (length(columnas_pp) > 0L) {
      openxlsx::addStyle(
        wb,
        hoja,
        style = openxlsx::createStyle(numFmt = "0.00"),
        rows = 2:(nrow(datos) + 1L),
        cols = columnas_pp,
        gridExpand = TRUE
      )
    }
  }
  invisible(wb)
}

#' Agregar la validacion de distribuciones a un libro OSIS existente
#'
#' No crea un Excel separado. Recibe el `Workbook` que ya contiene las hojas
#' del proceso de aceptacion y anexa las hojas de distribucion.
#'
#' @param wb Objeto `openxlsx::Workbook`.
#' @param resultado Resultado de `validar_distribuciones_antes_despues()`.
#' @param depuracion Resultado opcional de
#'   `depurar_respuestas_fuera_flujo_osis()`.
#' @param nombres_hojas Vector nombrado con `detalle`, `resumen`,
#'   `trazabilidad` y `resumen_depuracion`.
#' @param reemplazar_hojas Si es `TRUE`, reemplaza hojas homonimas.
#'
#' @return El mismo `Workbook`, de forma invisible.
#' @keywords internal
agregar_hojas_validacion_distribuciones_osis <- function(
    wb,
    resultado,
    depuracion = NULL,
    nombres_hojas = c(
      detalle = "validacion_distribuciones",
      resumen = "resumen_distribuciones",
      trazabilidad = "trazabilidad_depuracion",
      resumen_depuracion = "resumen_depuracion"
    ),
    reemplazar_hojas = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx`.", call. = FALSE)
  }
  if (!inherits(wb, "Workbook")) {
    stop("`wb` debe ser un objeto `openxlsx::Workbook`.", call. = FALSE)
  }
  .validar_resultado_distribuciones_osis(resultado)
  nombres_requeridos <- c(
    "detalle",
    "resumen",
    "trazabilidad",
    "resumen_depuracion"
  )
  if (
    !is.character(nombres_hojas) ||
      any(!nombres_requeridos %in% names(nombres_hojas)) ||
      any(!nzchar(nombres_hojas[nombres_requeridos])) ||
      any(nchar(nombres_hojas[nombres_requeridos]) > 31L) ||
      anyDuplicated(nombres_hojas[nombres_requeridos]) > 0L
  ) {
    stop(
      "`nombres_hojas` debe definir cuatro nombres unicos de maximo 31 caracteres.",
      call. = FALSE
    )
  }

  hojas <- unname(nombres_hojas[c("detalle", "resumen")])
  if (!is.null(depuracion)) {
    requeridos_depuracion <- c(
      "trazabilidad_depuracion",
      "resumen_depuracion"
    )
    if (
      !is.list(depuracion) ||
        any(!requeridos_depuracion %in% names(depuracion))
    ) {
      stop("`depuracion` no cumple el contrato esperado.", call. = FALSE)
    }
    hojas <- c(
      hojas,
      unname(nombres_hojas[c(
        "trazabilidad",
        "resumen_depuracion"
      )])
    )
  }

  existentes <- openxlsx::sheets(wb)
  conflicto <- intersect(hojas, existentes)
  if (length(conflicto) > 0L && !isTRUE(reemplazar_hojas)) {
    stop(
      "El libro ya contiene las hojas: ",
      paste(conflicto, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (length(conflicto) > 0L) {
    for (hoja in conflicto) {
      openxlsx::removeWorksheet(wb, hoja)
    }
  }

  .agregar_hoja_osis(
    wb,
    nombres_hojas[["detalle"]],
    resultado$detalle_distribuciones
  )
  .agregar_hoja_osis(
    wb,
    nombres_hojas[["resumen"]],
    resultado$resumen_distribuciones
  )
  if (!is.null(depuracion)) {
    .agregar_hoja_osis(
      wb,
      nombres_hojas[["trazabilidad"]],
      depuracion$trazabilidad_depuracion
    )
    .agregar_hoja_osis(
      wb,
      nombres_hojas[["resumen_depuracion"]],
      depuracion$resumen_depuracion
    )
  }
  invisible(wb)
}

#' Imprimir el resumen de distribuciones OSIS
#'
#' @param resultado Resultado de `validar_distribuciones_antes_despues()`.
#'
#' @return El resumen, de forma invisible.
#' @keywords internal
imprimir_resumen_distribuciones_osis <- function(resultado) {
  .validar_resultado_distribuciones_osis(resultado)
  resumen <- resultado$resumen_distribuciones

  cat("\nVALIDACION DE DISTRIBUCIONES ANTES Y DESPUES\n")
  for (i in seq_len(nrow(resumen))) {
    cat(
      "\n", resumen$variable[[i]], ":\n",
      "Maxima diferencia: ",
      ifelse(
        is.na(resumen$maxima_diferencia_absoluta_pp[[i]]),
        "NA",
        sprintf("%.2f pp", resumen$maxima_diferencia_absoluta_pp[[i]])
      ),
      "\nCategoria: ",
      .valor_o_osis(
        resumen$categoria_maxima_diferencia[[i]],
        "NA"
      ),
      "\nResultado: ",
      resumen$estado[[i]],
      "\n",
      sep = ""
    )
  }
  invisible(resumen)
}

#' Guardar el libro de aceptacion y detener despues si existen errores
#'
#' El libro se escribe y se comprueba en disco antes de evaluar el resultado
#' general. Por defecto no reemplaza un archivo existente.
#'
#' @param wb Objeto `openxlsx::Workbook` ya completo.
#' @param ruta_excel Ruta de salida.
#' @param controles Tabla general de controles.
#' @param sobrescribir Si es `TRUE`, autoriza reemplazar la ruta.
#' @param detener_si_error Si es `TRUE`, detiene la aprobacion despues de
#'   guardar el libro.
#'
#' @return Lista con ruta, hojas, controles y estado general.
#' @keywords internal
guardar_libro_aceptacion_osis <- function(
    wb,
    ruta_excel,
    controles,
    sobrescribir = FALSE,
    detener_si_error = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx`.", call. = FALSE)
  }
  if (!inherits(wb, "Workbook")) {
    stop("`wb` debe ser un objeto `openxlsx::Workbook`.", call. = FALSE)
  }
  if (
    !is.character(ruta_excel) ||
      length(ruta_excel) != 1L ||
      !nzchar(ruta_excel)
  ) {
    stop("`ruta_excel` debe ser una ruta no vacia.", call. = FALSE)
  }
  if (file.exists(ruta_excel) && !isTRUE(sobrescribir)) {
    stop(
      "El archivo ya existe. Use una nueva version o autorice `sobrescribir = TRUE`.",
      call. = FALSE
    )
  }

  controles <- evaluar_controles_osis(controles)
  openxlsx::saveWorkbook(
    wb,
    ruta_excel,
    overwrite = isTRUE(sobrescribir)
  )
  if (!file.exists(ruta_excel)) {
    stop("El libro no fue guardado en disco.", call. = FALSE)
  }

  hojas <- openxlsx::getSheetNames(ruta_excel)
  estado_general <- if (
    nrow(controles) > 0L &&
      all(controles$estado == "OK") &&
      !anyNA(controles$estado)
  ) {
    "OK"
  } else {
    "ERROR"
  }

  salida <- list(
    ruta_excel = normalizePath(
      ruta_excel,
      winslash = "/",
      mustWork = TRUE
    ),
    hojas = hojas,
    controles = controles,
    estado_general = estado_general
  )

  if (identical(estado_general, "ERROR") && isTRUE(detener_si_error)) {
    controles_error <- controles[controles$estado != "OK", , drop = FALSE]
    print(controles_error, n = Inf, width = Inf)
    stop(
      "La base NO supera la prueba de aceptacion para OSIS. ",
      "El Excel fue guardado antes de detener el proceso: ",
      salida$ruta_excel,
      call. = FALSE
    )
  }
  salida
}
