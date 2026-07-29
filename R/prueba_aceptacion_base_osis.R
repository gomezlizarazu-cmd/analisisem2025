# Orquestacion general de la aceptacion de bases destinadas a OSIS.

.extraer_dfs_original_osis <- function(em_original) {
  if (!is.list(em_original) || length(em_original) == 0L) {
    stop("`em_original` debe ser una lista no vacia.", call. = FALSE)
  }
  dfs <- if ("dfs" %in% names(em_original)) {
    em_original$dfs
  } else {
    em_original
  }
  if (
    !is.list(dfs) ||
      is.null(names(dfs)) ||
      any(!nzchar(names(dfs)))
  ) {
    stop(
      "No fue posible extraer una lista nombrada de capitulos.",
      call. = FALSE
    )
  }
  names(dfs) <- toupper(names(dfs))
  dfs
}

.normalizar_configuracion_variables_osis <- function(configuracion_variables) {
  if (
    !is.list(configuracion_variables) ||
      length(configuracion_variables) == 0L ||
      is.null(names(configuracion_variables)) ||
      any(!nzchar(names(configuracion_variables))) ||
      anyDuplicated(names(configuracion_variables)) > 0L
  ) {
    stop(
      "`configuracion_variables` debe ser una lista nombrada, unica y no vacia.",
      call. = FALSE
    )
  }

  tipos_validos <- c("categorica", "numerica", "texto_abierto")
  salida <- lapply(seq_along(configuracion_variables), function(i) {
    nombre <- names(configuracion_variables)[[i]]
    especificacion <- configuracion_variables[[i]]
    if (!is.list(especificacion)) {
      stop(
        "La configuracion de `", nombre, "` debe ser una lista.",
        call. = FALSE
      )
    }

    variable <- .valor_o_osis(especificacion$variable, nombre)
    variable_antes <- .valor_o_osis(
      especificacion$variable_antes,
      variable
    )
    variable_despues <- .valor_o_osis(
      especificacion$variable_despues,
      variable
    )
    variable_original <- .valor_o_osis(
      especificacion$variable_original,
      paste0(variable, "_original")
    )
    descripcion <- .valor_o_osis(especificacion$descripcion, variable)
    tipo <- .valor_o_osis(especificacion$tipo, "categorica")
    validar_distribucion <- .valor_o_osis(
      especificacion$validar_distribucion,
      identical(tipo, "categorica")
    )

    escalares_texto <- list(
      variable = variable,
      variable_antes = variable_antes,
      variable_despues = variable_despues,
      variable_original = variable_original,
      descripcion = descripcion,
      tipo = tipo
    )
    if (any(vapply(
      escalares_texto,
      function(x) {
        !is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)
      },
      logical(1)
    ))) {
      stop(
        "Los nombres, descripcion y tipo de `", nombre,
        "` deben ser textos escalares no vacios.",
        call. = FALSE
      )
    }
    if (!tipo %in% tipos_validos) {
      stop(
        "Tipo no soportado para `", nombre, "`: ", tipo, ".",
        call. = FALSE
      )
    }
    if (
      !is.logical(validar_distribucion) ||
        length(validar_distribucion) != 1L ||
        is.na(validar_distribucion)
    ) {
      stop(
        "`validar_distribucion` de `", nombre,
        "` debe ser logico escalar.",
        call. = FALSE
      )
    }
    if (identical(tipo, "texto_abierto") && validar_distribucion) {
      stop(
        "Una variable de texto abierto no puede validar distribuciones: ",
        nombre,
        ".",
        call. = FALSE
      )
    }
    universo_antes <- .valor_o_osis(
      especificacion$universo_antes,
      especificacion$universo
    )
    universo_despues <- .valor_o_osis(
      especificacion$universo_despues,
      especificacion$universo
    )
    if (is.null(universo_antes) || is.null(universo_despues)) {
      stop(
        "La configuracion de `", nombre,
        "` debe definir `universo` o ambos universos antes/despues.",
        call. = FALSE
      )
    }
    if (
      validar_distribucion &&
        identical(tipo, "categorica") &&
        is.null(especificacion$valores_validos)
    ) {
      stop(
        "La variable categorica `", nombre,
        "` requiere `valores_validos`.",
        call. = FALSE
      )
    }
    for (campo in c(
      "validador_dominio",
      "validador_observado_preservable"
    )) {
      if (
        !is.null(especificacion[[campo]]) &&
          !is.function(especificacion[[campo]])
      ) {
        stop(
          "`", campo, "` de `", nombre,
          "` debe ser una funcion.",
          call. = FALSE
        )
      }
    }

    especificacion$variable <- variable
    especificacion$variable_antes <- variable_antes
    especificacion$variable_despues <- variable_despues
    especificacion$variable_original <- variable_original
    especificacion$descripcion <- descripcion
    especificacion$tipo <- tipo
    especificacion$validar_distribucion <- validar_distribucion
    especificacion$universo_antes <- universo_antes
    especificacion$universo_despues <- universo_despues
    especificacion$variable_madre <- .valor_o_osis(
      especificacion$variable_madre,
      NULL
    )
    especificacion$condicion_subordinada <- .valor_o_osis(
      especificacion$condicion_subordinada,
      NULL
    )
    especificacion
  })
  names(salida) <- names(configuracion_variables)
  salida
}

.extraer_base_final_osis_general <- function(base_final,
                                             variables,
                                             llaves) {
  if (is.data.frame(base_final)) {
    return(tibble::as_tibble(base_final))
  }
  if (!is.list(base_final)) {
    stop(
      "`base_final` debe ser una tabla o una lista que contenga una tabla.",
      call. = FALSE
    )
  }
  candidatos <- which(vapply(
    base_final,
    function(x) {
      is.data.frame(x) &&
        all(c(llaves, variables) %in% names(x))
    },
    logical(1)
  ))
  if (length(candidatos) != 1L) {
    stop(
      "No fue posible identificar de manera unica la tabla en `base_final`.",
      call. = FALSE
    )
  }
  tibble::as_tibble(base_final[[candidatos]])
}

.extraer_diccionario_flujo_osis <- function(insumos_flujo) {
  if (is.null(insumos_flujo)) {
    return(NULL)
  }
  if (is.data.frame(insumos_flujo)) {
    return(insumos_flujo)
  }
  if (!is.list(insumos_flujo)) {
    stop(
      "`insumos_flujo` debe ser NULL, un data frame o una lista.",
      call. = FALSE
    )
  }
  candidatos <- c(
    "diccionario_k",
    "diccionario",
    "reglas_k_total",
    "reglas_flujo"
  )
  candidatos <- intersect(candidatos, names(insumos_flujo))
  candidatos <- candidatos[vapply(
    insumos_flujo[candidatos],
    is.data.frame,
    logical(1)
  )]
  if (length(candidatos) == 0L) {
    return(NULL)
  }
  insumos_flujo[[candidatos[[1L]]]]
}

.diagnosticar_base_osis <- function(dfs,
                                    capitulo,
                                    variables,
                                    insumos_flujo) {
  if (!identical(toupper(capitulo), "K")) {
    stop(
      "La orquestacion de flujo disponible actualmente corresponde al capitulo K.",
      call. = FALSE
    )
  }
  argumentos <- list(
    dfs = dfs,
    diccionario = .extraer_diccionario_flujo_osis(insumos_flujo),
    vars_cap_k = variables,
    detener_si_duplicados = TRUE
  )
  if (
    is.list(insumos_flujo) &&
      "variable_k23_final" %in% names(insumos_flujo) &&
      is.character(insumos_flujo$variable_k23_final) &&
      length(insumos_flujo$variable_k23_final) == 1L
  ) {
    argumentos$variable_k23_final <- insumos_flujo$variable_k23_final
  }
  do.call(diagnostico_flujo_capitulo_k, argumentos)
}

.extraer_debe_diagnostico_osis <- function(diagnostico,
                                           base,
                                           variable,
                                           llaves) {
  largo <- diagnostico$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == .env$variable) |>
    dplyr::select(
      dplyr::all_of(llaves),
      .data$debe_responder,
      .data$regla_aplicada
    )
  if (nrow(largo) == 0L) {
    return(list(
      debe_responder = rep(NA, nrow(base)),
      regla_flujo = paste0(
        "Sin regla diagnostica implementada para ",
        variable
      )
    ))
  }
  conflictos <- largo |>
    dplyr::group_by(dplyr::across(dplyr::all_of(llaves))) |>
    dplyr::summarise(
      n_decisiones = dplyr::n_distinct(
        dplyr::coalesce(
          as.character(.data$debe_responder),
          "<NA>"
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_decisiones > 1L)
  if (nrow(conflictos) > 0L) {
    stop(
      "El diagnostico produjo decisiones contradictorias para `",
      variable,
      "`.",
      call. = FALSE
    )
  }
  largo <- largo |>
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves)), .keep_all = TRUE)

  base_llaves <- .llaves_comparables_osis(base, llaves)
  base_llaves$.orden_osis <- seq_len(nrow(base_llaves))
  largo_llaves <- .llaves_comparables_osis(largo, llaves)
  largo_llaves$debe_responder <- largo$debe_responder
  largo_llaves$regla_aplicada <- largo$regla_aplicada
  cruce <- dplyr::left_join(
    base_llaves,
    largo_llaves,
    by = llaves
  ) |>
    dplyr::arrange(.data$.orden_osis)

  list(
    debe_responder = cruce$debe_responder,
    regla_flujo = paste(
      unique(cruce$regla_aplicada[
        !is.na(cruce$regla_aplicada) &
          nzchar(cruce$regla_aplicada)
      ]),
      collapse = " | "
    )
  )
}

.configuracion_depuracion_desde_diagnostico_osis <- function(
    diagnostico,
    base,
    configuracion,
    llaves) {
  salida <- lapply(seq_along(configuracion), function(i) {
    especificacion <- configuracion[[i]]
    variable <- especificacion$variable_despues
    flujo <- .extraer_debe_diagnostico_osis(
      diagnostico,
      base,
      variable,
      llaves
    )
    lista <- list(
      universo = flujo$debe_responder,
      regla_flujo = flujo$regla_flujo,
      motivo = "respuesta_fuera_de_flujo_segun_diagnostico"
    )
    if (especificacion$variable_original %in% names(base)) {
      lista$variable_original <- especificacion$variable_original
    }
    lista
  })
  names(salida) <- vapply(
    configuracion,
    `[[`,
    character(1),
    "variable_despues"
  )
  salida
}

.trazabilidad_vacia_osis <- function(llaves) {
  tibble::tibble(
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

.resultado_depuracion_sin_cambios_osis <- function(base,
                                                   configuracion,
                                                   llaves) {
  resumen <- tibble::tibble(
    variable = vapply(configuracion, `[[`, character(1), "variable"),
    regla_flujo = "depuracion_desactivada",
    valores_borrados = 0L,
    flujo_indeterminado_con_respuesta = 0L,
    respuestas_fuera_flujo_restantes = NA_integer_
  )
  list(
    base_depurada = tibble::as_tibble(base),
    trazabilidad_depuracion = .trazabilidad_vacia_osis(llaves),
    resumen_depuracion = resumen,
    controles_depuracion = evaluar_controles_osis(tibble::tibble(
      control = c(
        "filas_depuracion_osis",
        "llaves_duplicadas_despues_depuracion"
      ),
      observado = c(nrow(base), sum(duplicated(base[llaves]))),
      esperado = c(nrow(base), 0),
      operador = "igual"
    )),
    parametros_depuracion = list(
      llaves = llaves,
      accion = "depuracion_desactivada",
      limpiar_flujo_indeterminado = FALSE
    )
  )
}

.comparar_valores_con_na_osis <- function(x, y) {
  x_chr <- as.character(x)
  y_chr <- as.character(y)
  (is.na(x_chr) & is.na(y_chr)) |
    (!is.na(x_chr) & !is.na(y_chr) & x_chr == y_chr)
}

.controles_estructura_osis <- function(base_original,
                                       base_final,
                                       configuracion,
                                       llaves) {
  columnas_originales_ausentes <- setdiff(
    names(base_original),
    names(base_final)
  )
  variables_originales <- vapply(
    configuracion,
    `[[`,
    character(1),
    "variable_original"
  )
  variables_originales_ausentes <- setdiff(
    variables_originales,
    names(base_final)
  )
  llaves_antes <- .llaves_comparables_osis(base_original, llaves)
  llaves_despues <- .llaves_comparables_osis(base_final, llaves)

  copias_modificadas <- 0L
  if (identical(llaves_antes, llaves_despues)) {
    for (especificacion in configuracion) {
      variable <- especificacion$variable_antes
      original <- especificacion$variable_original
      if (
        variable %in% names(base_original) &&
          original %in% names(base_final)
      ) {
        copias_modificadas <- copias_modificadas + sum(
          !.comparar_valores_con_na_osis(
            base_original[[variable]],
            base_final[[original]]
          )
        )
      }
    }
  } else {
    copias_modificadas <- NA_integer_
  }

  controles <- evaluar_controles_osis(tibble::tibble(
    control = c(
      "filas_base_final",
      "llaves_duplicadas",
      "orden_y_llaves_modificados",
      "columnas_originales_ausentes",
      "variables_originales_ausentes",
      "copias_originales_modificadas"
    ),
    observado = c(
      nrow(base_final),
      sum(duplicated(llaves_despues)),
      as.integer(!identical(llaves_antes, llaves_despues)),
      length(columnas_originales_ausentes),
      length(variables_originales_ausentes),
      copias_modificadas
    ),
    esperado = c(nrow(base_original), 0, 0, 0, 0, 0),
    operador = "igual"
  ))

  list(
    controles = controles,
    columnas_originales_ausentes = columnas_originales_ausentes,
    variables_originales_ausentes = variables_originales_ausentes,
    copias_originales_modificadas = copias_modificadas
  )
}

.auditar_observados_configuracion_osis <- function(base_original,
                                                   base_final,
                                                   configuracion,
                                                   llaves) {
  llaves_unicas <-
    anyDuplicated(base_original[llaves]) == 0L &&
    anyDuplicated(base_final[llaves]) == 0L
  auditorias <- vector("list", length(configuracion))

  for (i in seq_along(configuracion)) {
    especificacion <- configuracion[[i]]
    universo_antes <- .evaluar_universo_osis(
      list(universo_antes = especificacion$universo_antes),
      especificacion$variable,
      base_original,
      "antes"
    )
    universo_despues <- .evaluar_universo_osis(
      list(universo_despues = especificacion$universo_despues),
      especificacion$variable,
      base_final,
      "despues"
    )
    if (is.function(especificacion$validador_observado_preservable)) {
      preservable <- especificacion$validador_observado_preservable(
        base_original[[especificacion$variable_antes]]
      )
      if (
        !is.logical(preservable) ||
          length(preservable) != nrow(base_original)
      ) {
        stop(
          "`validador_observado_preservable` de `",
          especificacion$variable,
          "` debe devolver un vector logico.",
          call. = FALSE
        )
      }
      universo_antes <- universo_antes & preservable %in% TRUE
    }
    auditorias[[i]] <- .auditar_preservacion_variable_osis(
      base_antes = base_original,
      base_despues = base_final,
      llaves = llaves,
      nombre = especificacion$variable,
      variable_antes = especificacion$variable_antes,
      variable_despues = especificacion$variable_despues,
      universo_antes = universo_antes,
      universo_despues = universo_despues,
      valores_validos = especificacion$valores_validos,
      llaves_unicas = llaves_unicas
    )$detalle
  }
  detalle <- dplyr::bind_rows(auditorias)
  control <- evaluar_controles_osis(tibble::tibble(
    control = "valores_observados_validos_sobrescritos",
    observado = nrow(detalle),
    esperado = 0,
    operador = "igual"
  ))
  list(detalle = detalle, control = control)
}

.resumen_flujo_configurado_osis <- function(diagnostico,
                                             configuracion) {
  variables <- vapply(
    configuracion,
    `[[`,
    character(1),
    "variable_despues"
  )
  diagnostico$auditoria_por_pregunta |>
    dplyr::filter(.data$variable %in% .env$variables) |>
    dplyr::arrange(match(.data$variable, .env$variables))
}

.controles_flujo_osis <- function(diagnostico_antes,
                                  diagnostico_despues,
                                  base_despues,
                                  configuracion,
                                  llaves) {
  controles <- list()

  for (especificacion in configuracion) {
    nombre <- especificacion$variable
    variable <- especificacion$variable_despues
    flujo_antes <- .extraer_debe_diagnostico_osis(
      diagnostico_antes,
      base_despues,
      variable,
      llaves
    )$debe_responder
    flujo <- .extraer_debe_diagnostico_osis(
      diagnostico_despues,
      base_despues,
      variable,
      llaves
    )$debe_responder
    valor <- normalizar_categoria_distribucion_osis(
      base_despues[[variable]]
    )
    no_imputable <- rep(FALSE, nrow(base_despues))
    if (is.function(especificacion$condicion_no_imputable)) {
      no_imputable <- especificacion$condicion_no_imputable(base_despues)
      if (
        !is.logical(no_imputable) ||
          length(no_imputable) != nrow(base_despues)
      ) {
        stop(
          "`condicion_no_imputable` de `", variable,
          "` debe devolver un vector logico.",
          call. = FALSE
        )
      }
      no_imputable <- no_imputable %in% TRUE
    }
    vacios_criticos <- sum(
      flujo %in% TRUE &
        is.na(valor) &
        !no_imputable
    )
    fuera_flujo <- sum(flujo %in% FALSE & !is.na(valor))
    indeterminados <- sum(is.na(flujo))
    indeterminados_antes <- sum(is.na(flujo_antes))

    controles[[length(controles) + 1L]] <- tibble::tibble(
      control = c(
        paste0("vacios_criticos_", stringr::str_to_lower(nombre)),
        paste0(
          "respuestas_fuera_flujo_",
          stringr::str_to_lower(nombre)
        ),
        paste0(
          "flujos_indeterminados_",
          stringr::str_to_lower(nombre)
        )
      ),
      observado = c(
        vacios_criticos,
        fuera_flujo,
        indeterminados
      ),
      esperado = c(
        0,
        0,
        indeterminados_antes
      ),
      operador = "igual"
    )

    dominio_invalido <- if (
      is.function(especificacion$validador_dominio)
    ) {
      valido <- especificacion$validador_dominio(
        base_despues[[variable]]
      )
      if (!is.logical(valido) || length(valido) != nrow(base_despues)) {
        stop(
          "`validador_dominio` de `", nombre,
          "` debe devolver un vector logico.",
          call. = FALSE
        )
      }
      sum(
        flujo %in% TRUE &
          !is.na(valor) &
          !valido %in% TRUE
      )
    } else if (is.null(especificacion$valores_validos)) {
      0L
    } else {
      dominio <- normalizar_categoria_distribucion_osis(
        especificacion$valores_validos
      )
      sum(
        flujo %in% TRUE &
          !is.na(valor) &
          !valor %in% dominio
      )
    }
    controles[[length(controles) + 1L]] <- tibble::tibble(
      control = paste0(
        "dominio_invalido_",
        stringr::str_to_lower(nombre)
      ),
      observado = dominio_invalido,
      esperado = 0,
      operador = "igual"
    )

    es_subordinada <-
      !is.null(especificacion$variable_madre) ||
      !is.null(especificacion$condicion_subordinada)
    if (es_subordinada) {
      controles[[length(controles) + 1L]] <- tibble::tibble(
        control = c(
          paste0(
            "vacio_cuando_debe_",
            stringr::str_to_lower(nombre)
          ),
          paste0(
            "informado_fuera_salto_",
            stringr::str_to_lower(nombre)
          )
        ),
        observado = c(
          vacios_criticos,
          sum(flujo %in% FALSE & !is.na(valor))
        ),
        esperado = c(0, 0),
        operador = "igual"
      )
    }
  }
  evaluar_controles_osis(dplyr::bind_rows(controles))
}

.controles_imputacion_texto_osis <- function(base,
                                             configuracion) {
  faltantes_incorporacion <- 0L
  faltantes_trazabilidad <- 0L
  n_textos_automaticos <- 0L
  n_flags_inconsistentes <- 0L
  n_metodos_inconsistentes <- 0L
  hay_texto <- FALSE
  texto_calculable <- TRUE

  for (especificacion in configuracion) {
    variable <- especificacion$variable_despues
    flag_imputado <- especificacion$columna_flag_imputado
    metodo_imputacion <- especificacion$columna_metodo_imputacion
    if (!is.null(flag_imputado) && flag_imputado %in% names(base)) {
      imputada <- .normalizar_estado_registro_osis(base[[flag_imputado]])
      valor <- normalizar_categoria_distribucion_osis(base[[variable]])
      faltantes_incorporacion <- faltantes_incorporacion + sum(
        imputada & is.na(valor)
      )
      if (!is.null(metodo_imputacion) && metodo_imputacion %in% names(base)) {
        metodo <- normalizar_categoria_distribucion_osis(
          base[[metodo_imputacion]]
        )
        faltantes_trazabilidad <- faltantes_trazabilidad + sum(
          imputada & is.na(metodo)
        )
      }
    }

    if (!identical(especificacion$tipo, "texto_abierto")) {
      next
    }
    hay_texto <- TRUE
    detector <- especificacion$detector_texto_automatico
    if (is.function(detector)) {
      detectado <- detector(base)
      if (!is.logical(detectado) || length(detectado) != nrow(base)) {
        stop(
          "`detector_texto_automatico` debe devolver un vector logico.",
          call. = FALSE
        )
      }
      n_textos_automaticos <- n_textos_automaticos + sum(
        detectado %in% TRUE
      )
    } else if (!is.null(flag_imputado) && flag_imputado %in% names(base)) {
      n_textos_automaticos <- n_textos_automaticos + sum(
        .normalizar_estado_registro_osis(base[[flag_imputado]])
      )
    } else {
      texto_calculable <- FALSE
    }

    condicion_no_imputable <- especificacion$condicion_no_imputable
    flag_no_imputable <- especificacion$columna_flag_no_imputable
    metodo_no_imputable <- especificacion$columna_metodo_no_imputable
    metodo_esperado <- especificacion$metodo_no_imputable_esperado
    if (
      is.function(condicion_no_imputable) &&
        !is.null(flag_no_imputable) &&
        flag_no_imputable %in% names(base)
    ) {
      esperada <- condicion_no_imputable(base)
      if (!is.logical(esperada) || length(esperada) != nrow(base)) {
        stop(
          "`condicion_no_imputable` debe devolver un vector logico.",
          call. = FALSE
        )
      }
      flag <- .normalizar_estado_registro_osis(base[[flag_no_imputable]])
      n_flags_inconsistentes <- n_flags_inconsistentes + sum(
        (esperada %in% TRUE) != flag
      )
      if (
        !is.null(metodo_no_imputable) &&
          metodo_no_imputable %in% names(base) &&
          !is.null(metodo_esperado)
      ) {
        metodo <- normalizar_categoria_distribucion_osis(
          base[[metodo_no_imputable]]
        )
        n_metodos_inconsistentes <- n_metodos_inconsistentes + sum(
          esperada %in% TRUE &
            (
              is.na(metodo) |
                metodo != as.character(metodo_esperado)
            )
        )
      } else {
        texto_calculable <- FALSE
      }
    } else {
      texto_calculable <- FALSE
    }
  }

  controles <- list(tibble::tibble(
    control = c(
      "imputaciones_no_incorporadas",
      "imputaciones_sin_trazabilidad"
    ),
    observado = c(
      faltantes_incorporacion,
      faltantes_trazabilidad
    ),
    esperado = c(0, 0),
    operador = "igual"
  ))
  if (hay_texto) {
    controles[[2L]] <- tibble::tibble(
      control = c(
        "textos_abiertos_escritos_automaticamente",
        "flags_texto_no_imputable_inconsistentes",
        "metodo_texto_no_imputable_inconsistente"
      ),
      observado = if (texto_calculable) {
        c(
          n_textos_automaticos,
          n_flags_inconsistentes,
          n_metodos_inconsistentes
        )
      } else {
        c(NA_real_, NA_real_, NA_real_)
      },
      esperado = c(0, 0, 0),
      operador = "igual"
    )
  }
  evaluar_controles_osis(dplyr::bind_rows(controles))
}

.configuracion_distribuciones_desde_aceptacion_osis <- function(
    configuracion) {
  seleccion <- vapply(
    configuracion,
    function(x) isTRUE(x$validar_distribucion),
    logical(1)
  )
  salida <- lapply(configuracion[seleccion], function(especificacion) {
    list(
      variable_antes = especificacion$variable_antes,
      variable_despues = especificacion$variable_despues,
      universo_antes = especificacion$universo_antes,
      universo_despues = especificacion$universo_despues,
      valores_validos = especificacion$valores_validos,
      excluir_na = .valor_o_osis(especificacion$excluir_na, TRUE),
      descripcion = especificacion$descripcion
    )
  })
  names(salida) <- vapply(
    configuracion[seleccion],
    `[[`,
    character(1),
    "variable"
  )
  salida
}

.resultado_distribuciones_vacio_osis <- function(limite_pp,
                                                 llaves) {
  detalle <- tibble::tibble(
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
    cumple_categoria = logical()
  )
  resumen <- tibble::tibble(
    variable = character(),
    descripcion_variable = character(),
    total_antes = integer(),
    total_despues = integer(),
    numero_categorias = integer(),
    maxima_diferencia_absoluta_pp = double(),
    categoria_maxima_diferencia = character(),
    limite_pp = double(),
    categorias_incumplidas = character(),
    cumple_variable = logical(),
    estado = character(),
    chi_cuadrado = double(),
    grados_libertad = double(),
    p_valor = double(),
    metodo_prueba = character(),
    prueba_estadistica_calculable = logical(),
    advertencia_prueba = character(),
    cramers_v = double(),
    clasificacion_cramers_v = character()
  )
  list(
    detalle_distribuciones = detalle,
    resumen_distribuciones = resumen,
    categorias_invalidas = tibble::tibble(),
    observados_validos_sobrescritos = tibble::tibble(),
    controles_distribuciones = evaluar_controles_osis(tibble::tibble(
      control = character(),
      observado = double(),
      esperado = double(),
      operador = character()
    )),
    controles_preservacion = tibble::tibble(),
    controles_integridad = tibble::tibble(),
    controles_validacion_osis = tibble::tibble(),
    parametros_validacion_distribuciones = list(
      limite_pp = limite_pp,
      unidad = "puntos_porcentuales",
      metodo = "diferencia_absoluta_por_categoria",
      base_antes = "respuestas_originales_validas",
      base_despues = "base_final_depurada_para_osis",
      llaves = llaves,
      prueba_estadistica = FALSE
    )
  )
}

.integrar_lista_controles_osis <- function(tablas) {
  tablas <- tablas[!vapply(tablas, is.null, logical(1))]
  tablas <- tablas[vapply(tablas, nrow, integer(1)) > 0L]
  if (length(tablas) == 0L) {
    return(evaluar_controles_osis(tibble::tibble(
      control = character(),
      observado = double(),
      esperado = double(),
      operador = character()
    )))
  }
  salida <- evaluar_controles_osis(tablas[[1L]])
  if (length(tablas) > 1L) {
    for (i in 2:length(tablas)) {
      salida <- integrar_controles_osis(salida, tablas[[i]])
    }
  }
  salida
}

.parametros_a_tabla_osis <- function(parametros) {
  tibble::tibble(
    parametro = names(parametros),
    valor = vapply(
      parametros,
      function(x) paste(as.character(x), collapse = " | "),
      character(1)
    )
  )
}

.crear_libro_aceptacion_general_osis <- function(
    libro,
    controles,
    resumen_flujo,
    detalle_inconsistencias,
    depuracion,
    distribuciones,
    observados_sobrescritos,
    parametros) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx`.", call. = FALSE)
  }
  wb <- if (is.null(libro)) {
    openxlsx::createWorkbook()
  } else {
    if (!inherits(libro, "Workbook")) {
      stop("`libro` debe ser un Workbook de openxlsx.", call. = FALSE)
    }
    libro
  }
  hojas_obligatorias <- c(
    "00_prueba_aceptacion",
    "01_diagnostico_flujo",
    "02_inconsistencias",
    "03_trazabilidad_depuracion",
    "04_resumen_depuracion",
    "05_validacion_distribuciones",
    "06_resumen_distribuciones",
    "07_categorias_invalidas",
    "08_observados_sobrescritos",
    "09_parametros"
  )
  conflicto <- intersect(openxlsx::sheets(wb), hojas_obligatorias)
  if (length(conflicto) > 0L) {
    stop(
      "El Workbook ya contiene hojas reservadas: ",
      paste(conflicto, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  .agregar_hoja_osis(wb, hojas_obligatorias[[1L]], controles)
  .agregar_hoja_osis(wb, hojas_obligatorias[[2L]], resumen_flujo)
  .agregar_hoja_osis(
    wb,
    hojas_obligatorias[[3L]],
    detalle_inconsistencias
  )
  .agregar_hoja_osis(
    wb,
    hojas_obligatorias[[4L]],
    depuracion$trazabilidad_depuracion
  )
  .agregar_hoja_osis(
    wb,
    hojas_obligatorias[[5L]],
    depuracion$resumen_depuracion
  )
  agregar_hojas_validacion_distribuciones_osis(
    wb = wb,
    resultado = distribuciones,
    nombres_hojas = c(
      detalle = hojas_obligatorias[[6L]],
      resumen = hojas_obligatorias[[7L]],
      trazabilidad = "trazabilidad_no_usada",
      resumen_depuracion = "resumen_depuracion_no_usado"
    )
  )
  .agregar_hoja_osis(
    wb,
    hojas_obligatorias[[8L]],
    distribuciones$categorias_invalidas
  )
  .agregar_hoja_osis(
    wb,
    hojas_obligatorias[[9L]],
    observados_sobrescritos
  )
  .agregar_hoja_osis(
    wb,
    hojas_obligatorias[[10L]],
    .parametros_a_tabla_osis(parametros)
  )

  list(
    libro = wb,
    hojas_obligatorias = hojas_obligatorias
  )
}

#' Ejecutar la prueba general de aceptacion de una base para OSIS
#'
#' Orquesta integridad, diagnostico de flujo antes y despues de depurar,
#' controles semanticos, preservacion, distribuciones, prueba estadistica
#' complementaria y escritura verificable de RDS y Excel.
#'
#' @param em_original Lista de capitulos o lista con elemento `dfs`.
#' @param base_final Base imputada/corregida o lista que la contiene.
#' @param insumos_flujo Diccionario o lista de insumos del diagnostico.
#' @param configuracion_variables Lista nombrada de variables y reglas.
#' @param llaves Llaves del nivel analitico.
#' @param capitulo Capitulo que se reemplaza en la lista original.
#' @param limite_distribucion_pp Limite absoluto en puntos porcentuales.
#' @param depurar_fuera_flujo Activa la conversion a NA fuera de flujo.
#' @param ejecutar_prueba_estadistica Activa chi-cuadrado y Cramer V.
#' @param controles_adicionales Controles opcionales del usuario.
#' @param libro Workbook opcional que se debe conservar.
#' @param ruta_rds Ruta opcional para el resultado serializado.
#' @param ruta_excel Ruta opcional para el libro de auditoria.
#' @param sobrescribir Autoriza reemplazar rutas existentes.
#' @param detener_si_error Detiene despues de guardar y verificar archivos.
#'
#' @return Lista completa de objetos de aceptacion. Si se solicita detener y
#'   hay errores, los archivos se guardan antes del error.
#' @keywords internal
prueba_aceptacion_base_osis <- function(
    em_original,
    base_final,
    insumos_flujo,
    configuracion_variables,
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    capitulo = "K",
    limite_distribucion_pp = 5,
    depurar_fuera_flujo = TRUE,
    ejecutar_prueba_estadistica = TRUE,
    controles_adicionales = NULL,
    libro = NULL,
    ruta_rds = NULL,
    ruta_excel = NULL,
    sobrescribir = FALSE,
    detener_si_error = TRUE) {
  configuracion <- .normalizar_configuracion_variables_osis(
    configuracion_variables
  )
  capitulo <- toupper(capitulo)
  dfs_original <- .extraer_dfs_original_osis(em_original)
  if (!capitulo %in% names(dfs_original)) {
    stop(
      "No existe el capitulo `", capitulo, "` en `em_original`.",
      call. = FALSE
    )
  }
  base_original <- tibble::as_tibble(dfs_original[[capitulo]])
  variables <- vapply(configuracion, `[[`, character(1), "variable")
  variables_antes <- vapply(
    configuracion,
    `[[`,
    character(1),
    "variable_antes"
  )
  variables_despues <- vapply(
    configuracion,
    `[[`,
    character(1),
    "variable_despues"
  )
  base_final <- .extraer_base_final_osis_general(
    base_final,
    variables_despues,
    llaves
  )
  .validar_llaves_base_osis(base_original, llaves, "base_original")
  .validar_llaves_base_osis(base_final, llaves, "base_final")
  faltan_original <- setdiff(variables_antes, names(base_original))
  faltan_final <- setdiff(variables_despues, names(base_final))
  if (length(faltan_original) > 0L || length(faltan_final) > 0L) {
    stop(
      "Faltan variables configuradas: ",
      paste(unique(c(faltan_original, faltan_final)), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  estructura <- .controles_estructura_osis(
    base_original,
    base_final,
    configuracion,
    llaves
  )
  dfs_final <- dfs_original
  dfs_final[[capitulo]] <- base_final
  diagnostico_antes <- .diagnosticar_base_osis(
    dfs_final,
    capitulo,
    variables_despues,
    insumos_flujo
  )

  if (isTRUE(depurar_fuera_flujo)) {
    configuracion_depuracion <-
      .configuracion_depuracion_desde_diagnostico_osis(
        diagnostico_antes,
        base_final,
        configuracion,
        llaves
      )
    depuracion <- depurar_respuestas_fuera_flujo_osis(
      base_final,
      configuracion_depuracion,
      llaves
    )
  } else {
    depuracion <- .resultado_depuracion_sin_cambios_osis(
      base_final,
      configuracion,
      llaves
    )
  }
  base_osis_depurada <- depuracion$base_depurada
  dfs_depurada <- dfs_original
  dfs_depurada[[capitulo]] <- base_osis_depurada
  diagnostico_despues <- .diagnosticar_base_osis(
    dfs_depurada,
    capitulo,
    variables_despues,
    insumos_flujo
  )

  resumen_flujo <- .resumen_flujo_configurado_osis(
    diagnostico_despues,
    configuracion
  )
  detalle_inconsistencias <-
    diagnostico_despues$diagnostico_persona_variable |>
    dplyr::filter(
      .data$variable %in% .env$variables_despues,
      .data$vacio_critico |
        .data$respuesta_fuera_flujo |
        is.na(.data$debe_responder)
    )
  controles_flujo <- .controles_flujo_osis(
    diagnostico_antes,
    diagnostico_despues,
    base_osis_depurada,
    configuracion,
    llaves
  )
  preservacion <- .auditar_observados_configuracion_osis(
    base_original,
    base_osis_depurada,
    configuracion,
    llaves
  )
  controles_semanticos <- .controles_imputacion_texto_osis(
    base_osis_depurada,
    configuracion
  )

  configuracion_distribuciones <-
    .configuracion_distribuciones_desde_aceptacion_osis(
      configuracion
    )
  if (
    length(configuracion_distribuciones) > 0L
  ) {
    distribuciones <- validar_distribuciones_antes_despues(
      base_antes = base_original,
      base_despues = base_osis_depurada,
      configuracion_variables = configuracion_distribuciones,
      limite_pp = limite_distribucion_pp,
      llaves = llaves,
      ejecutar_prueba_estadistica =
        isTRUE(ejecutar_prueba_estadistica),
      imprimir = FALSE
    )
  } else {
    distribuciones <- .resultado_distribuciones_vacio_osis(
      limite_distribucion_pp,
      llaves
    )
  }

  controles_preservacion <- .integrar_lista_controles_osis(list(
    preservacion$control,
    estructura$controles |>
      dplyr::filter(.data$control == "copias_originales_modificadas")
  ))
  controles <- .integrar_lista_controles_osis(list(
    estructura$controles |>
      dplyr::filter(.data$control != "copias_originales_modificadas"),
    controles_preservacion,
    controles_flujo,
    controles_semanticos,
    depuracion$controles_depuracion,
    distribuciones$controles_distribuciones,
    controles_adicionales
  ))
  estado_general <- if (
    nrow(controles) > 0L &&
      all(controles$estado == "OK") &&
      !anyNA(controles$estado)
  ) {
    "OK"
  } else {
    "ERROR"
  }

  parametros <- list(
    capitulo = capitulo,
    llaves = llaves,
    variables = variables,
    variables_antes = variables_antes,
    variables_despues = variables_despues,
    limite_distribucion_pp = limite_distribucion_pp,
    unidad_distribucion = "puntos_porcentuales",
    criterio_vinculante =
      "maxima_diferencia_absoluta_pp_menor_igual_limite",
    prueba_estadistica_complementaria =
      isTRUE(ejecutar_prueba_estadistica),
    prueba_estadistica_vinculante = FALSE,
    depurar_fuera_flujo = isTRUE(depurar_fuera_flujo),
    diagnosticos_ejecutados = 2L,
    filas_base_original = nrow(base_original),
    filas_base_final = nrow(base_osis_depurada),
    estado_general = estado_general
  )

  libro_resultado <- .crear_libro_aceptacion_general_osis(
    libro = libro,
    controles = controles,
    resumen_flujo = resumen_flujo,
    detalle_inconsistencias = detalle_inconsistencias,
    depuracion = depuracion,
    distribuciones = distribuciones,
    observados_sobrescritos = preservacion$detalle,
    parametros = parametros
  )
  resultado_guardable <- list(
    base_osis_depurada = base_osis_depurada,
    controles = controles,
    resumen_flujo = resumen_flujo,
    detalle_inconsistencias = detalle_inconsistencias,
    trazabilidad_depuracion = depuracion$trazabilidad_depuracion,
    resumen_depuracion = depuracion$resumen_depuracion,
    detalle_distribuciones = distribuciones$detalle_distribuciones,
    resumen_distribuciones = distribuciones$resumen_distribuciones,
    categorias_invalidas = distribuciones$categorias_invalidas,
    observados_validos_sobrescritos = preservacion$detalle,
    diagnostico_antes_depuracion = diagnostico_antes,
    diagnostico_despues_depuracion = diagnostico_despues,
    parametros = parametros
  )

  ruta_rds_verificada <- NULL
  objetos_rds_verificados <- character()
  if (!is.null(ruta_rds)) {
    if (file.exists(ruta_rds) && !isTRUE(sobrescribir)) {
      stop(
        "El RDS ya existe; use una nueva ruta o autorice `sobrescribir = TRUE`.",
        call. = FALSE
      )
    }
    saveRDS(resultado_guardable, ruta_rds, compress = FALSE)
    objeto_verificado <- readRDS(ruta_rds)
    objetos_esperados <- c(
      "base_osis_depurada",
      "controles",
      "resumen_flujo",
      "detalle_inconsistencias",
      "trazabilidad_depuracion",
      "resumen_depuracion",
      "detalle_distribuciones",
      "resumen_distribuciones",
      "categorias_invalidas",
      "observados_validos_sobrescritos",
      "parametros"
    )
    if (
      !is.list(objeto_verificado) ||
        any(!objetos_esperados %in% names(objeto_verificado))
    ) {
      stop("La verificacion fisica del RDS fallo.", call. = FALSE)
    }
    ruta_rds_verificada <- normalizePath(
      ruta_rds,
      winslash = "/",
      mustWork = TRUE
    )
    objetos_rds_verificados <- objetos_esperados
  }

  ruta_excel_verificada <- NULL
  hojas_excel_verificadas <- character()
  if (!is.null(ruta_excel)) {
    guardado_excel <- guardar_libro_aceptacion_osis(
      wb = libro_resultado$libro,
      ruta_excel = ruta_excel,
      controles = controles,
      sobrescribir = sobrescribir,
      detener_si_error = FALSE
    )
    if (any(
      !libro_resultado$hojas_obligatorias %in% guardado_excel$hojas
    )) {
      stop("La verificacion fisica de hojas del Excel fallo.", call. = FALSE)
    }
    ruta_excel_verificada <- guardado_excel$ruta_excel
    hojas_excel_verificadas <- guardado_excel$hojas
  }

  salida <- c(
    resultado_guardable,
    list(
      libro = libro_resultado$libro,
      ruta_rds = ruta_rds_verificada,
      ruta_excel = ruta_excel_verificada,
      objetos_rds_verificados = objetos_rds_verificados,
      hojas_excel_verificadas = hojas_excel_verificadas
    )
  )

  if (identical(estado_general, "ERROR") && isTRUE(detener_si_error)) {
    if (is.null(ruta_rds) || is.null(ruta_excel)) {
      stop(
        "La base presenta errores. Para detener con diagnosticos persistidos, ",
        "suministre `ruta_rds` y `ruta_excel`.",
        call. = FALSE
      )
    }
    print(
      controles[controles$estado != "OK", , drop = FALSE],
      n = Inf,
      width = Inf
    )
    stop(
      "La base NO supera la prueba de aceptacion para OSIS. ",
      "El RDS y el Excel fueron guardados y verificados antes del error.",
      call. = FALSE
    )
  }
  salida
}
