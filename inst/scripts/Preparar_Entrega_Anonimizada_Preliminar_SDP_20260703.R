# Preparacion preliminar anonimizada EM 2025 para SDP
#
# Este script:
# - toma los CSV de OSIS como fuente de la entrega;
# - usa el diccionario anonimizado como lista blanca;
# - conserva solo los universos jerarquicos con ENCUESTA_COMPLETA == 1;
# - contrasta por llaves contra la salida oficial completa del corte 20260703;
# - excluye completamente el capitulo K;
# - publica la carpeta del cliente solo cuando todos los controles criticos pasan.
#
# No reconstruye completitud, no imputa y no modifica ningun archivo de entrada.
#
# Ejecucion desde una sesion limpia:
# Rscript inst/scripts/Preparar_Entrega_Anonimizada_Preliminar_SDP_20260703.R
#
# Prueba sintetica integrada (no lee archivos operativos):
# Rscript inst/scripts/Preparar_Entrega_Anonimizada_Preliminar_SDP_20260703.R --prueba-sintetica

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

configuracion_entrega_anonimizada <- function(
    fecha_corte = "20260703",
    ruta_paquete = "C:/Users/gomez/OneDrive/Documentos/analisisem2025",
    usuario = Sys.getenv("USERPROFILE")
) {
  carpeta_validar <- file.path(
    usuario,
    "OneDrive", "DANE", "Multiproposito",
    "Validacion", "Encuestas", "Validar"
  )

  carpeta_proyecto <- file.path(
    carpeta_validar,
    "EM-Anonimizada-SDP"
  )

  list(
    fecha_corte = fecha_corte,
    ruta_paquete = ruta_paquete,
    diccionario_completo = file.path(
      usuario,
      "OneDrive", "DANE", "Multiproposito",
      "Validacion", "Encuestas", "Diccionarios",
      "DICCIONARIO DE DATOS MULTIPROPOSITO 2025_VF.xlsx"
    ),
    diccionario_anonimizado = file.path(
      usuario,
      "OneDrive", "DANE", "Multiproposito",
      "Validacion", "Encuestas", "Diccionarios",
      "DICCIONARIO DE DATOS ANONIMIZADO MULTIPROPOSITO 2025_VF.xlsx"
    ),
    carpeta_osis = file.path(
      carpeta_proyecto,
      "EM-Anonimizada-SDP"
    ),
    carpeta_raiz_cap = carpeta_validar,
    carpeta_cap_oficial = file.path(
      carpeta_validar,
      paste0("CAP_EM_", fecha_corte)
    ),
    carpeta_cap_sin_tematica_opcional = file.path(
      carpeta_validar,
      paste0("CAP_EM_", fecha_corte),
      paste0("em_completa_sin_tematica_csv_", fecha_corte)
    ),
    carpeta_resultados_base = file.path(
      carpeta_proyecto,
      paste0("entrega_preliminar_sin_K_", fecha_corte)
    ),
    orden_capitulos = c(LETTERS[1:12], "MA", "MB"),
    capitulo_excluido = "K",
    variable_completitud = "ENCUESTA_COMPLETA",
    delimitador_salida = ";",
    codificacion_salida = "UTF-8 con BOM",
    representacion_na = ""
  )
}

verificar_dependencias_entrega <- function(ruta_paquete) {
  paquetes <- c(
    "devtools", "dplyr", "purrr", "readr", "readxl",
    "stringi", "stringr", "tibble", "openxlsx"
  )
  faltantes <- paquetes[
    !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(faltantes) > 0L) {
    stop(
      "Faltan paquetes requeridos: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!dir.exists(ruta_paquete)) {
    stop("No existe el repositorio del paquete: ", ruta_paquete, call. = FALSE)
  }

  devtools::load_all(ruta_paquete, quiet = TRUE)

  requeridos_paquete <- c(
    "tipo_capitulo",
    "get_join_keys",
    "cargar_capitulos_por_fecha"
  )
  ausentes <- requeridos_paquete[
    !vapply(
      requeridos_paquete,
      exists,
      logical(1),
      envir = asNamespace("analisisem2025"),
      inherits = FALSE
    )
  ]
  if (length(ausentes) > 0L) {
    stop(
      "El paquete local no contiene las interfaces requeridas: ",
      paste(ausentes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

funcion_paquete <- function(nombre) {
  getFromNamespace(nombre, "analisisem2025")
}

normalizar_texto_identificador <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_to_upper(stringr::str_squish(x))
  x <- stringr::str_replace_all(x, "[^A-Z0-9]+", "_")
  stringr::str_replace_all(x, "^_+|_+$", "")
}

normalizar_capitulo_entrega <- function(
    x,
    capitulos_validos = c(LETTERS[1:12], "MA", "MB")
) {
  x <- normalizar_texto_identificador(x)
  x <- stringr::str_replace(
    x,
    "^(EMP2025|CAPITULO|CAP)_*",
    ""
  )
  capitulos_validos <- unique(toupper(capitulos_validos))
  valido <- x %in% capitulos_validos
  x[!valido] <- NA_character_
  x
}

normalizar_variable_entrega <- function(x) {
  normalizar_texto_identificador(x)
}

es_valor_completo_oficial <- function(x) {
  x <- stringr::str_squish(as.character(x))
  !is.na(x) & stringr::str_detect(x, "^1(?:[.]0+)?$")
}

es_valor_binario_oficial <- function(x) {
  x <- stringr::str_squish(as.character(x))
  !is.na(x) & stringr::str_detect(x, "^[01](?:[.]0+)?$")
}

normalizar_llave_cruce <- function(x) {
  valor <- as.character(x)
  transformar <- !is.na(valor) & stringr::str_detect(
    valor,
    "^[0-9]+[.]0+$"
  )
  valor[transformar] <- stringr::str_replace(
    valor[transformar],
    "[.]0+$",
    ""
  )
  valor
}

normalizar_llaves_cruce_df <- function(df, llaves) {
  llaves_permitidas <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  no_permitidas <- setdiff(llaves, llaves_permitidas)
  if (length(no_permitidas) > 0L) {
    stop(
      "La normalizacion de cruces solo admite llaves canonicas: ",
      paste(llaves_permitidas, collapse = ", "),
      ". Se recibio: ", paste(no_permitidas, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  faltantes <- setdiff(llaves, names(df))
  if (length(faltantes) > 0L) {
    stop(
      "No se pueden normalizar llaves ausentes: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  salida <- tibble::as_tibble(df, .name_repair = "minimal")
  for (llave in llaves) {
    salida[[llave]] <- normalizar_llave_cruce(salida[[llave]])
  }
  salida
}

auditar_normalizacion_llaves <- function(dfs, origen) {
  if (is.null(dfs) || length(dfs) == 0L) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(dfs, function(df, capitulo) {
    llaves <- intersect(
      c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
      names(df)
    )
    purrr::map_dfr(llaves, function(llave) {
      antes <- as.character(df[[llave]])
      despues <- normalizar_llave_cruce(df[[llave]])
      cambio <- !is.na(antes) & !is.na(despues) & antes != despues
      ejemplos <- which(cambio)[seq_len(min(sum(cambio), 5L))]
      tibble::tibble(
        origen = origen,
        capitulo = capitulo,
        variable = llave,
        n_valores_revisados = length(antes),
        n_valores_normalizados = sum(cambio),
        n_valores_sin_cambio = length(antes) - sum(cambio),
        ejemplos_antes = paste(antes[ejemplos], collapse = " | "),
        ejemplos_despues = paste(
          despues[ejemplos],
          collapse = " | "
        ),
        estado = "INFORMATIVO"
      )
    })
  })
}

clasificar_problemas_lectura <- function(problemas,
                                         nombres_columnas,
                                         llaves,
                                         origen,
                                         archivo,
                                         capitulo) {
  if (is.null(problemas) || nrow(problemas) == 0L) {
    return(tibble::tibble())
  }
  problemas <- tibble::as_tibble(problemas)
  columna_indice <- suppressWarnings(as.integer(problemas$col))
  columna_nombre <- rep(NA_character_, nrow(problemas))
  indice_valido <- !is.na(columna_indice) &
    columna_indice >= 1L & columna_indice <= length(nombres_columnas)
  columna_nombre[indice_valido] <- nombres_columnas[
    columna_indice[indice_valido]
  ]
  esperado <- as.character(problemas$expected %||% NA_character_)
  encontrado <- as.character(problemas$actual %||% NA_character_)
  fila <- suppressWarnings(as.integer(problemas$row))
  afecta_estructura <- is.na(columna_indice) |
    !indice_valido |
    (!is.na(fila) & fila <= 1L) |
    columna_nombre %in% llaves |
    stringr::str_detect(
      stringr::str_to_lower(dplyr::coalesce(esperado, "")),
      "column|field|delimit|header|encabez|registro"
    )

  tibble::tibble(
    origen = origen,
    archivo = archivo,
    capitulo = capitulo,
    fila = fila,
    columna = columna_nombre,
    columna_indice = columna_indice,
    valor_esperado = esperado,
    valor_encontrado = encontrado,
    descripcion = paste0(
      "Problema de parsing reportado por readr en fila ",
      fila,
      ", columna ",
      dplyr::coalesce(columna_nombre, as.character(columna_indice)),
      "."
    ),
    clasificacion = dplyr::if_else(
      afecta_estructura,
      "puede_afectar_estructura_llaves_o_integridad",
      "sin_impacto_demostrado_en_estructura_o_llaves"
    ),
    criticidad = dplyr::if_else(
      afecta_estructura,
      "CRITICO",
      "INFORMATIVO"
    )
  )
}

detectar_encabezado_diccionario <- function(tabla_cruda,
                                            hoja,
                                            aliases_tabla,
                                            aliases_variable) {
  aliases_tabla <- normalizar_texto_identificador(aliases_tabla)
  aliases_variable <- normalizar_texto_identificador(aliases_variable)

  candidatos <- purrr::map_dfr(seq_len(nrow(tabla_cruda)), function(fila) {
    valores <- normalizar_texto_identificador(
      unlist(tabla_cruda[fila, ], use.names = FALSE)
    )
    columnas_tabla <- which(valores %in% aliases_tabla)
    columnas_variable <- which(valores %in% aliases_variable)

    if (length(columnas_tabla) == 1L &&
        length(columnas_variable) == 1L) {
      tibble::tibble(
        fila_encabezado = fila,
        columna_tabla = columnas_tabla,
        columna_variable = columnas_variable
      )
    } else {
      tibble::tibble()
    }
  })

  if (nrow(candidatos) > 1L) {
    stop(
      "La hoja `", hoja,
      "` tiene mas de un encabezado candidato para tabla y variable: ",
      paste(candidatos$fila_encabezado, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  candidatos
}

leer_diccionario_entrega <- function(
    ruta,
    nombre_diccionario,
    capitulos_validos = c(LETTERS[1:12], "MA", "MB")
) {
  if (!file.exists(ruta)) {
    stop("No existe el ", nombre_diccionario, ": ", ruta, call. = FALSE)
  }

  hojas <- readxl::excel_sheets(ruta)
  if (length(hojas) == 0L) {
    stop("El ", nombre_diccionario, " no contiene hojas.", call. = FALSE)
  }

  aliases_tabla <- c(
    "ID TABLA", "ID. TABLA", "ID DE LA TABLA",
    "IDENTIFICACION DE LA TABLA ID TABLA"
  )
  aliases_variable <- c(
    "NOMBRE DE LA VARIABLE O LA COLUMNA",
    "NOMBRE DE LA VARIABLE (O COLUMNA)",
    "NOMBRE VARIABLE", "VARIABLE", "NOMBRE_VARIABLE"
  )
  hojas_informativas <- c(
    "INSTRUCCIONES",
    "DOMINIOS",
    "DICCIONARIO_PLANTILLA"
  )

  lecturas <- purrr::map(hojas, function(hoja) {
    tabla_cruda <- readxl::read_excel(
      ruta,
      sheet = hoja,
      col_names = FALSE,
      col_types = "text",
      trim_ws = FALSE,
      .name_repair = "minimal"
    )
    tabla_cruda <- tibble::as_tibble(
      tabla_cruda,
      .name_repair = "minimal"
    )
    hoja_norm <- normalizar_texto_identificador(hoja)

    if (hoja_norm %in% hojas_informativas) {
      clasificacion <- dplyr::case_when(
        hoja_norm == "DOMINIOS" ~ "catalogo_dominios",
        hoja_norm == "INSTRUCCIONES" ~ "informativa_instrucciones",
        TRUE ~ "plantilla_informativa"
      )
      return(list(
        datos = tibble::tibble(),
        inventario = tibble::tibble(
          diccionario = nombre_diccionario,
          hoja = hoja,
          n_filas = nrow(tabla_cruda),
          n_columnas = ncol(tabla_cruda),
          clasificacion = clasificacion,
          procesable = FALSE,
          fila_encabezado = NA_integer_,
          fila_inicio_datos = NA_integer_,
          columna_capitulo = NA_character_,
          columna_variable = NA_character_,
          estado = "INFORMATIVA_NO_PROCESADA",
          observacion = paste(
            "Hoja registrada para auditoria; no integra la lista blanca."
          )
        )
      ))
    }

    encabezado <- detectar_encabezado_diccionario(
      tabla_cruda,
      hoja,
      aliases_tabla,
      aliases_variable
    )
    if (nrow(encabezado) == 0L) {
      return(list(
        datos = tibble::tibble(),
        inventario = tibble::tibble(
          diccionario = nombre_diccionario,
          hoja = hoja,
          n_filas = nrow(tabla_cruda),
          n_columnas = ncol(tabla_cruda),
          clasificacion = "no_procesable_sin_encabezado",
          procesable = FALSE,
          fila_encabezado = NA_integer_,
          fila_inicio_datos = NA_integer_,
          columna_capitulo = NA_character_,
          columna_variable = NA_character_,
          estado = "NO_PROCESADA",
          observacion = paste(
            "No se encontro un encabezado inequivoco de tabla y variable."
          )
        )
      ))
    }

    fila_encabezado <- encabezado$fila_encabezado[[1]]
    columna_tabla <- encabezado$columna_tabla[[1]]
    columna_variable <- encabezado$columna_variable[[1]]
    filas_candidatas <- seq.int(fila_encabezado + 1L, nrow(tabla_cruda))
    tiene_tabla_y_variable <- vapply(filas_candidatas, function(fila) {
      tabla_valor <- tabla_cruda[[columna_tabla]][[fila]]
      variable_valor <- tabla_cruda[[columna_variable]][[fila]]
      !is.na(tabla_valor) &&
        nzchar(stringr::str_squish(as.character(tabla_valor))) &&
        !is.na(variable_valor) &&
        nzchar(stringr::str_squish(as.character(variable_valor)))
    }, logical(1))

    if (!any(tiene_tabla_y_variable)) {
      stop(
        "La hoja `", hoja,
        "` fue identificada como tabla de variables, pero no contiene filas ",
        "con ID tabla y nombre de variable.",
        call. = FALSE
      )
    }
    fila_inicio_datos <- filas_candidatas[which(tiene_tabla_y_variable)[1]]
    filas_datos <- filas_candidatas[tiene_tabla_y_variable]
    capitulo_original <- as.character(
      tabla_cruda[[columna_tabla]][filas_datos]
    )
    variable_original <- as.character(
      tabla_cruda[[columna_variable]][filas_datos]
    )
    capitulo <- normalizar_capitulo_entrega(
      capitulo_original,
      capitulos_validos
    )

    datos <- tibble::tibble(
      diccionario = nombre_diccionario,
      hoja = hoja,
      fila_hoja = filas_datos,
      capitulo_original = capitulo_original,
      variable_original = variable_original,
      capitulo = capitulo,
      variable = normalizar_variable_entrega(variable_original),
      capitulo_vacio = is.na(capitulo),
      variable_vacia = is.na(variable_original) |
        !nzchar(stringr::str_squish(variable_original))
    )

    list(
      datos = datos,
      inventario = tibble::tibble(
        diccionario = nombre_diccionario,
        hoja = hoja,
        n_filas = nrow(tabla_cruda),
        n_columnas = ncol(tabla_cruda),
        clasificacion = "datos_variables",
        procesable = TRUE,
        fila_encabezado = fila_encabezado,
        fila_inicio_datos = fila_inicio_datos,
        columna_capitulo = paste0(
          LETTERS[[columna_tabla]],
          " / ID tabla"
        ),
        columna_variable = paste0(
          LETTERS[[columna_variable]],
          " / Nombre de la variable o la columna"
        ),
        estado = "PROCESADA",
        observacion = paste(
          "El capitulo se deriva de EMP2025_<CAP> en ID tabla."
        )
      )
    )
  })

  list(
    datos = purrr::map_dfr(lecturas, "datos"),
    inventario = purrr::map_dfr(lecturas, "inventario")
  )
}

auditar_diccionarios_entrega <- function(diccionario_completo,
                                         diccionario_anonimizado) {
  completo_valido <- diccionario_completo |>
    dplyr::filter(!.data$capitulo_vacio, !.data$variable_vacia)
  anonimizado_valido <- diccionario_anonimizado |>
    dplyr::filter(!.data$capitulo_vacio, !.data$variable_vacia)

  duplicados <- dplyr::bind_rows(
    completo_valido,
    anonimizado_valido
  ) |>
    dplyr::count(
      .data$diccionario,
      .data$capitulo,
      .data$variable,
      name = "n"
    ) |>
    dplyr::filter(.data$n > 1L)

  variables_anonimizadas_no_en_completo <- anonimizado_valido |>
    dplyr::anti_join(
      completo_valido |>
        dplyr::distinct(.data$capitulo, .data$variable),
      by = c("capitulo", "variable")
    )

  variables_permitidas <- anonimizado_valido |>
    dplyr::distinct(.data$capitulo, .data$variable, .keep_all = TRUE) |>
    dplyr::group_by(.data$capitulo) |>
    dplyr::mutate(orden_diccionario = dplyr::row_number()) |>
    dplyr::ungroup()

  variables_no_autorizadas <- completo_valido |>
    dplyr::distinct(.data$capitulo, .data$variable, .keep_all = TRUE) |>
    dplyr::anti_join(
      variables_permitidas |>
        dplyr::select("capitulo", "variable"),
      by = c("capitulo", "variable")
    )

  variables_multicapitulo <- completo_valido |>
    dplyr::filter(
      !.data$variable %in% c(
        "DIRECTORIO", "SECUENCIA_P", "ORDEN", "ENCUESTA_COMPLETA"
      )
    ) |>
    dplyr::distinct(.data$variable, .data$capitulo) |>
    dplyr::count(.data$variable, name = "n_capitulos") |>
    dplyr::filter(.data$n_capitulos > 1L) |>
    dplyr::left_join(
      completo_valido |>
        dplyr::distinct(.data$variable, .data$capitulo) |>
        dplyr::group_by(.data$variable) |>
        dplyr::summarise(
          capitulos = paste(sort(.data$capitulo), collapse = ", "),
          .groups = "drop"
        ),
      by = "variable"
    ) |>
    dplyr::mutate(
      observacion = paste(
        "Hallazgo informativo: revisar si la asignacion multicapitulo",
        "es intencional; no se invalida automaticamente."
      )
    )

  resumen <- tibble::tibble(
    metrica = c(
      "filas_diccionario_completo",
      "filas_diccionario_anonimizado",
      "capitulo_o_variable_vacia",
      "duplicados_capitulo_variable",
      "anonimizado_fuera_diccionario_completo",
      "variables_permitidas",
      "variables_no_autorizadas",
      "variables_multicapitulo_para_revision"
    ),
    valor = c(
      nrow(diccionario_completo),
      nrow(diccionario_anonimizado),
      sum(diccionario_completo$capitulo_vacio |
        diccionario_completo$variable_vacia) +
        sum(diccionario_anonimizado$capitulo_vacio |
          diccionario_anonimizado$variable_vacia),
      nrow(duplicados),
      nrow(variables_anonimizadas_no_en_completo),
      nrow(variables_permitidas),
      nrow(variables_no_autorizadas),
      nrow(variables_multicapitulo)
    )
  )

  list(
    variables_diccionario_completo = completo_valido,
    variables_diccionario_anonimizado = anonimizado_valido,
    variables_permitidas = variables_permitidas,
    variables_no_autorizadas = variables_no_autorizadas,
    variables_anonimizadas_no_en_diccionario_completo =
      variables_anonimizadas_no_en_completo,
    duplicados = duplicados,
    vacios = dplyr::bind_rows(
      diccionario_completo,
      diccionario_anonimizado
    ) |>
      dplyr::filter(.data$capitulo_vacio | .data$variable_vacia),
    variables_multicapitulo = variables_multicapitulo,
    resumen = resumen
  )
}

detectar_delimitador_csv <- function(ruta) {
  lineas <- readr::read_lines(
    ruta,
    n_max = 5L,
    progress = FALSE,
    skip_empty_rows = TRUE
  )
  lineas <- lineas[nzchar(stringr::str_squish(lineas))]
  if (length(lineas) == 0L) {
    stop("El CSV esta vacio: ", ruta, call. = FALSE)
  }

  encabezado <- lineas[[1]]
  sin_texto_entre_comillas <- stringr::str_replace_all(
    encabezado,
    '"(?:[^"]|"")*"',
    ""
  )
  n_comas <- stringr::str_count(
    sin_texto_entre_comillas,
    stringr::fixed(",")
  )
  n_punto_coma <- stringr::str_count(
    sin_texto_entre_comillas,
    stringr::fixed(";")
  )

  if (n_comas == n_punto_coma || max(n_comas, n_punto_coma) == 0L) {
    stop(
      "No fue posible determinar inequivocamente el delimitador de: ",
      ruta,
      ".",
      call. = FALSE
    )
  }
  if (n_punto_coma > n_comas) ";" else ","
}

leer_csv_osis_texto <- function(ruta, delimitador = NULL) {
  delimitador <- delimitador %||% detectar_delimitador_csv(ruta)
  tabla <- readr::read_delim(
    ruta,
    delim = delimitador,
    quote = "\"",
    escape_double = TRUE,
    col_types = readr::cols(.default = readr::col_character()),
    # Se preservan tanto cadenas vacias como el texto literal "NA".
    # Solo las ausencias que el parser represente realmente como NA quedan NA.
    na = character(),
    trim_ws = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )

  nombres_norm <- normalizar_variable_entrega(names(tabla))
  duplicados_norm <- unique(nombres_norm[duplicated(nombres_norm)])
  if (length(duplicados_norm) > 0L) {
    stop(
      "El archivo ", basename(ruta),
      " tiene columnas duplicadas despues de normalizar: ",
      paste(duplicados_norm, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  problemas <- readr::problems(tabla)
  tabla <- tibble::as_tibble(tabla, .name_repair = "minimal")
  attr(tabla, "problemas_lectura") <- problemas
  tabla
}

descubrir_y_leer_osis <- function(carpeta_osis) {
  if (!dir.exists(carpeta_osis)) {
    stop("No existe la carpeta OSIS: ", carpeta_osis, call. = FALSE)
  }

  patron <- "^VW_EMP_CAP_(.+)[.]csv$"
  archivos <- list.files(
    carpeta_osis,
    pattern = patron,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(archivos) == 0L) {
    stop(
      "No se encontraron archivos OSIS con patron ",
      patron,
      " en ",
      carpeta_osis,
      ".",
      call. = FALSE
    )
  }

  inventario_base <- tibble::tibble(
    archivo = basename(archivos),
    ruta = normalizePath(archivos, winslash = "/", mustWork = TRUE),
    capitulo = normalizar_capitulo_entrega(
      stringr::str_match(basename(archivos), stringr::regex(
        patron,
        ignore_case = TRUE
      ))[, 2]
    ),
    tamano_bytes = as.numeric(file.info(archivos)$size),
    md5 = unname(tools::md5sum(archivos))
  ) |>
    dplyr::add_count(.data$capitulo, name = "n_archivos_capitulo") |>
    dplyr::mutate(
      archivo_duplicado = .data$n_archivos_capitulo > 1L |
        is.na(.data$capitulo)
    )

  lecturas <- purrr::pmap(
    list(
      ruta = inventario_base$ruta,
      duplicado = inventario_base$archivo_duplicado,
      capitulo = inventario_base$capitulo,
      archivo = inventario_base$archivo
    ),
    function(ruta, duplicado, capitulo, archivo) {
      if (isTRUE(duplicado)) {
        return(list(
          tabla = NULL,
          delimitador = NA_character_,
          estado = "NO_LEIDO_CAPITULO_AMBIGUO",
          error = "Capitulo invalido o mas de un archivo para el capitulo.",
          problemas = tibble::tibble()
        ))
      }

      tryCatch(
        {
          delimitador <- detectar_delimitador_csv(ruta)
          tabla <- leer_csv_osis_texto(ruta, delimitador)
          llaves <- funcion_paquete("get_join_keys")(capitulo)
          problemas <- clasificar_problemas_lectura(
            attr(tabla, "problemas_lectura"),
            names(tabla),
            llaves,
            origen = "OSIS",
            archivo = archivo,
            capitulo = capitulo
          )
          list(
            tabla = tabla,
            delimitador = delimitador,
            estado = "LEIDO",
            error = NA_character_,
            problemas = problemas
          )
        },
        error = function(e) {
          list(
            tabla = NULL,
            delimitador = NA_character_,
            estado = "ERROR_LECTURA",
            error = conditionMessage(e),
            problemas = tibble::tibble(
              origen = "OSIS",
              archivo = archivo,
              capitulo = capitulo,
              fila = NA_integer_,
              columna = NA_character_,
              columna_indice = NA_integer_,
              valor_esperado = NA_character_,
              valor_encontrado = NA_character_,
              descripcion = conditionMessage(e),
              clasificacion = "lectura_incompleta_o_fallida",
              criticidad = "CRITICO"
            )
          )
        }
      )
    }
  )

  inventario <- inventario_base |>
    dplyr::mutate(
      delimitador = purrr::map_chr(lecturas, "delimitador"),
      n_filas = purrr::map_int(
        lecturas,
        ~ if (is.null(.x$tabla)) NA_integer_ else nrow(.x$tabla)
      ),
      n_columnas = purrr::map_int(
        lecturas,
        ~ if (is.null(.x$tabla)) NA_integer_ else ncol(.x$tabla)
      ),
      columnas_disponibles = purrr::map_chr(
        lecturas,
        ~ if (is.null(.x$tabla)) {
          NA_character_
        } else {
          paste(names(.x$tabla), collapse = " | ")
        }
      ),
      estado_lectura = purrr::map_chr(lecturas, "estado"),
      error_lectura = purrr::map_chr(lecturas, "error")
    )

  tablas <- purrr::map(lecturas, "tabla")
  names(tablas) <- inventario$capitulo
  tablas <- tablas[!vapply(tablas, is.null, logical(1))]

  list(
    dfs = tablas,
    inventario = inventario,
    problemas_lectura = purrr::map_dfr(lecturas, "problemas")
  )
}

cargar_referencia_cap_completa <- function(config) {
  carga <- funcion_paquete("cargar_capitulos_por_fecha")(
    fecha_corte = config$fecha_corte,
    carpeta_raiz = config$carpeta_raiz_cap,
    prefijo_carpeta = "CAP_EM_",
    orden_caps = config$orden_capitulos,
    verbose = TRUE
  )

  # La CAP oficial se conserva exactamente como la devuelve la funcion maestra
  # del paquete. No se sustituye por la salida derivada sin tematica ni se
  # implementa aqui un segundo lector para sus formatos admitidos.
  archivos_por_capitulo <- stats::setNames(
    carga$archivos,
    toupper(
      stringr::str_match(
        basename(carga$archivos),
        stringr::regex(
          paste0(
            "^CAP_([A-Z]{1,2})_",
            config$fecha_corte,
            "[.]"
          ),
          ignore_case = TRUE
        )
      )[, 2]
    )
  )

  carga$problemas_lectura <- purrr::imap_dfr(
    carga$dfs,
    function(tabla, capitulo) {
      problemas <- tryCatch(
        readr::problems(tabla),
        error = function(e) tibble::tibble()
      )
      clasificar_problemas_lectura(
        problemas,
        names(tabla),
        funcion_paquete("get_join_keys")(capitulo),
        origen = "CAP_OFICIAL",
        archivo = basename(archivos_por_capitulo[[capitulo]]),
        capitulo = capitulo
      )
    }
  )

  carga$inventario <- carga$resumen_carga |>
    dplyr::transmute(
      capitulo = .data$cap,
      n_filas = .data$n,
      n_columnas = .data$p,
      archivo = unname(archivos_por_capitulo[.data$cap]),
      carpeta = normalizePath(
        carga$carpeta_caps,
        winslash = "/",
        mustWork = TRUE
      ),
      formato = tolower(tools::file_ext(.data$archivo)),
      delimitador = NA_character_,
      estado_lectura = "CAP_OFICIAL_LEIDA"
    )
  carga
}

cargar_salida_sin_tematica_opcional <- function(config) {
  carpeta <- config$carpeta_cap_sin_tematica_opcional
  if (!dir.exists(carpeta)) {
    return(list(
      disponible = FALSE,
      dfs = NULL,
      inventario = tibble::tibble(
        capitulo = NA_character_,
        n_filas = NA_integer_,
        n_columnas = NA_integer_,
        carpeta = normalizePath(
          carpeta,
          winslash = "/",
          mustWork = FALSE
        ),
        delimitador = NA_character_,
        estado_lectura = "CONTROL_OPCIONAL_NO_DISPONIBLE"
      )
    ))
  }

  patron <- paste0(
    "^em_completa_sin_tematica_([A-Z]{1,2})_",
    config$fecha_corte,
    "[.]csv$"
  )
  archivos <- list.files(
    carpeta,
    pattern = patron,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(archivos) == 0L) {
    return(list(
      disponible = FALSE,
      dfs = NULL,
      inventario = tibble::tibble(
        capitulo = NA_character_,
        n_filas = NA_integer_,
        n_columnas = NA_integer_,
        carpeta = normalizePath(
          carpeta,
          winslash = "/",
          mustWork = TRUE
        ),
        delimitador = NA_character_,
        estado_lectura = "CONTROL_OPCIONAL_SIN_ARCHIVOS"
      )
    ))
  }

  capitulos <- normalizar_capitulo_entrega(
    stringr::str_match(
      basename(archivos),
      stringr::regex(patron, ignore_case = TRUE)
    )[, 2],
    config$orden_capitulos
  )
  if (anyNA(capitulos) || anyDuplicated(capitulos)) {
    stop(
      "Los archivos del control sin tematica tienen capitulos invalidos o ",
      "duplicados.",
      call. = FALSE
    )
  }

  delimitadores <- purrr::map_chr(archivos, detectar_delimitador_csv)
  dfs <- purrr::map2(
    archivos,
    delimitadores,
    leer_csv_osis_texto
  )
  names(dfs) <- capitulos
  dfs <- dfs[intersect(config$orden_capitulos, names(dfs))]

  list(
    disponible = TRUE,
    dfs = dfs,
    problemas_lectura = purrr::imap_dfr(
      dfs,
      function(tabla, capitulo) {
        clasificar_problemas_lectura(
          attr(tabla, "problemas_lectura"),
          names(tabla),
          funcion_paquete("get_join_keys")(capitulo),
          origen = "SALIDA_SIN_TEMATICA_OPCIONAL",
          archivo = basename(archivos[[match(capitulo, capitulos)]]),
          capitulo = capitulo
        )
      }
    ),
    inventario = tibble::tibble(
      capitulo = names(dfs),
      n_filas = vapply(dfs, nrow, integer(1)),
      n_columnas = vapply(dfs, ncol, integer(1)),
      carpeta = normalizePath(
        carpeta,
        winslash = "/",
        mustWork = TRUE
      ),
      delimitador = unname(delimitadores[match(
        names(dfs),
        capitulos
      )]),
      estado_lectura = "CONTROL_OPCIONAL_LEIDO"
    )
  )
}

tabla_llaves <- function(df, llaves, capitulo, origen) {
  faltantes <- setdiff(llaves, names(df))
  if (length(faltantes) > 0L) {
    stop(
      "El capitulo ", capitulo, " en ", origen,
      " no contiene las llaves: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  df <- normalizar_llaves_cruce_df(df, llaves)
  df |>
    dplyr::select(dplyr::all_of(llaves))
}

duplicados_por_llave <- function(df, llaves, capitulo, origen) {
  tabla_llaves(df, llaves, capitulo, origen) |>
    dplyr::count(dplyr::across(dplyr::all_of(llaves)), name = "n") |>
    dplyr::filter(.data$n > 1L) |>
    dplyr::mutate(
      capitulo = capitulo,
      origen = origen,
      .before = 1
    )
}

llaves_sin_faltantes <- function(df, llaves) {
  if (!all(llaves %in% names(df))) {
    return(FALSE)
  }
  tabla <- normalizar_llaves_cruce_df(df, llaves)
  all(vapply(
    llaves,
    function(llave) {
      valor <- tabla[[llave]]
      all(!is.na(valor) & nzchar(valor))
    },
    logical(1)
  ))
}

construir_universos_completos <- function(dfs,
                                          variable_completitud) {
  faltan_capitulos <- setdiff(c("A", "C", "E"), names(dfs))
  if (length(faltan_capitulos) > 0L) {
    stop(
      "OSIS no contiene los capitulos obligatorios A, C y E. Faltan: ",
      paste(faltan_capitulos, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  faltan_marca <- c("A", "C", "E")[
    !vapply(
      c("A", "C", "E"),
      function(cap) variable_completitud %in% names(dfs[[cap]]),
      logical(1)
    )
  ]
  if (length(faltan_marca) > 0L) {
    stop(
      "Falta ", variable_completitud, " en: ",
      paste(faltan_marca, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  llaves_a <- funcion_paquete("get_join_keys")("A")
  llaves_c <- funcion_paquete("get_join_keys")("C")
  llaves_e <- funcion_paquete("get_join_keys")("E")

  a <- normalizar_llaves_cruce_df(dfs$A, llaves_a)
  c <- normalizar_llaves_cruce_df(dfs$C, llaves_c)
  e <- normalizar_llaves_cruce_df(dfs$E, llaves_e)

  viviendas_marcadas <- a |>
    dplyr::filter(
      es_valor_completo_oficial(
        .data[[variable_completitud]]
      )
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves_a)))

  hogares_marcados <- c |>
    dplyr::filter(
      es_valor_completo_oficial(
        .data[[variable_completitud]]
      )
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves_c)))

  personas_marcadas <- e |>
    dplyr::filter(
      es_valor_completo_oficial(
        .data[[variable_completitud]]
      )
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves_e)))

  hogares_sin_vivienda <- hogares_marcados |>
    dplyr::anti_join(viviendas_marcadas, by = llaves_a) |>
    dplyr::mutate(
      contradiccion = "hogar_completo_sin_vivienda_completa"
    )

  personas_sin_hogar <- personas_marcadas |>
    dplyr::anti_join(hogares_marcados, by = llaves_c) |>
    dplyr::mutate(
      contradiccion = "persona_completa_sin_hogar_completo"
    )

  personas_sin_vivienda <- personas_marcadas |>
    dplyr::anti_join(viviendas_marcadas, by = llaves_a) |>
    dplyr::mutate(
      contradiccion = "persona_completa_sin_vivienda_completa"
    )

  hogares <- hogares_marcados |>
    dplyr::semi_join(viviendas_marcadas, by = llaves_a)
  personas <- personas_marcadas |>
    dplyr::semi_join(hogares, by = llaves_c) |>
    dplyr::semi_join(viviendas_marcadas, by = llaves_a)

  list(
    vivienda = viviendas_marcadas,
    hogar = hogares,
    persona = personas,
    marcados = list(
      vivienda = viviendas_marcadas,
      hogar = hogares_marcados,
      persona = personas_marcadas
    ),
    contradicciones = dplyr::bind_rows(
      hogares_sin_vivienda,
      personas_sin_hogar,
      personas_sin_vivienda
    ),
    resumen = tibble::tibble(
      nivel = c("vivienda", "hogar", "persona"),
      capitulo_marca = c("A", "C", "E"),
      llaves = c(
        paste(llaves_a, collapse = " + "),
        paste(llaves_c, collapse = " + "),
        paste(llaves_e, collapse = " + ")
      ),
      n_marcados_completos = c(
        nrow(viviendas_marcadas),
        nrow(hogares_marcados),
        nrow(personas_marcadas)
      ),
      n_universo_jerarquico = c(
        nrow(viviendas_marcadas),
        nrow(hogares),
        nrow(personas)
      )
    )
  )
}

universo_para_capitulo <- function(capitulo, universos) {
  nivel <- funcion_paquete("tipo_capitulo")[[capitulo]]
  if (is.null(nivel) || !nivel %in% c("vivienda", "hogar", "persona")) {
    stop(
      "No existe una tipologia valida para el capitulo ",
      capitulo,
      ".",
      call. = FALSE
    )
  }
  universos[[nivel]]
}

resolver_equivalencia_llave_b <- function(df_osis_b, df_cap_b) {
  tiene_secuencia <- "SECUENCIA_P" %in% names(df_osis_b)
  tiene_orden <- "ORDEN" %in% names(df_osis_b)
  data_validacion <- tibble::as_tibble(
    df_osis_b,
    .name_repair = "minimal"
  )
  variable_utilizada <- NA_character_
  motivo <- NA_character_
  estado_equivalencia <- "equivalencia_invalida"
  n_faltantes <- 0L
  n_duplicados <- 0L
  n_diferencias <- 0L
  n_contradicciones <- 0L
  n_llaves_comparadas <- 0L

  if (tiene_secuencia) {
    variable_utilizada <- "SECUENCIA_P"
    motivo <- "SECUENCIA_P presente; no se requiere equivalencia."
    estado_equivalencia <- "equivalencia_no_requerida"
    llaves_b <- c("DIRECTORIO", "SECUENCIA_P")
    normalizadas <- normalizar_llaves_cruce_df(
      data_validacion,
      llaves_b
    )
    n_faltantes <- sum(vapply(
      llaves_b,
      function(llave) {
        valor <- normalizadas[[llave]]
        sum(is.na(valor) | !nzchar(valor))
      },
      integer(1)
    ))
    n_duplicados <- nrow(duplicados_por_llave(
      data_validacion,
      llaves_b,
      "B",
      "OSIS_VALIDACION_B"
    ))
    n_llaves_comparadas <- nrow(dplyr::distinct(
      tabla_llaves(data_validacion, llaves_b, "B", "OSIS_VALIDACION_B")
    ))

    if (tiene_orden) {
      secuencia <- normalizar_llave_cruce(
        data_validacion$SECUENCIA_P
      )
      orden <- normalizar_llave_cruce(data_validacion$ORDEN)
      n_contradicciones <- sum(
        is.na(secuencia) != is.na(orden) |
          dplyr::coalesce(secuencia != orden, FALSE)
      )
      if (n_contradicciones > 0L) {
        estado_equivalencia <- "equivalencia_invalida"
        motivo <- paste(
          "SECUENCIA_P y ORDEN estan presentes, pero no coinciden."
        )
      }
    }
  } else if (tiene_orden) {
    variable_utilizada <- "ORDEN"
    motivo <- paste(
      "Falta SECUENCIA_P; ORDEN se evalua como equivalente interno",
      "solo para cruces."
    )
    data_validacion$SECUENCIA_P <- data_validacion$ORDEN
    llaves_b <- c("DIRECTORIO", "SECUENCIA_P")
    normalizadas <- normalizar_llaves_cruce_df(
      data_validacion,
      llaves_b
    )
    n_faltantes <- sum(vapply(
      llaves_b,
      function(llave) {
        valor <- normalizadas[[llave]]
        sum(is.na(valor) | !nzchar(valor))
      },
      integer(1)
    ))
    duplicados <- duplicados_por_llave(
      data_validacion,
      llaves_b,
      "B",
      "OSIS_ALIAS_ORDEN"
    )
    n_duplicados <- nrow(duplicados)
    llaves_osis <- tabla_llaves(
      data_validacion,
      llaves_b,
      "B",
      "OSIS_ALIAS_ORDEN"
    ) |>
      dplyr::distinct()
    llaves_cap <- tabla_llaves(
      df_cap_b,
      llaves_b,
      "B",
      "CAP_OFICIAL"
    ) |>
      dplyr::distinct()
    osis_no_cap <- dplyr::anti_join(
      llaves_osis,
      llaves_cap,
      by = llaves_b
    )
    cap_no_osis <- dplyr::anti_join(
      llaves_cap,
      llaves_osis,
      by = llaves_b
    )
    n_diferencias <- nrow(osis_no_cap) + nrow(cap_no_osis)
    n_llaves_comparadas <- nrow(llaves_osis)
    if (
      n_faltantes == 0L &&
        n_duplicados == 0L &&
        n_diferencias == 0L
    ) {
      estado_equivalencia <- "equivalencia_excepcional_activada"
    }
  } else {
    motivo <- "B no contiene SECUENCIA_P ni ORDEN."
    data_validacion$SECUENCIA_P <- NA_character_
    n_faltantes <- nrow(data_validacion)
  }

  pasa <- estado_equivalencia %in% c(
    "equivalencia_no_requerida",
    "equivalencia_excepcional_activada"
  ) && n_faltantes == 0L && n_duplicados == 0L &&
    n_diferencias == 0L && n_contradicciones == 0L

  auditoria <- tibble::tibble(
    capitulo = "B",
    variable_utilizada = variable_utilizada,
    motivo = motivo,
    n_llaves_comparadas = n_llaves_comparadas,
    n_faltantes = n_faltantes,
    n_diferencias = n_diferencias,
    n_duplicados = n_duplicados,
    n_contradicciones = n_contradicciones,
    estado_equivalencia = estado_equivalencia,
    estado_validacion = if (pasa) "PASS" else "FAIL"
  )

  list(
    data_validacion = data_validacion,
    llaves = c("DIRECTORIO", "SECUENCIA_P"),
    auditoria = auditoria,
    control = nuevo_control(
      "B_EQUIVALENCIA_LLAVE",
      capitulo = "B",
      descripcion = paste(
        "Valida SECUENCIA_P o la equivalencia excepcional interna",
        "ORDEN -> SECUENCIA_P."
      ),
      observado = paste(
        estado_equivalencia,
        "faltantes=", n_faltantes,
        "duplicados=", n_duplicados,
        "diferencias=", n_diferencias,
        "contradicciones=", n_contradicciones
      ),
      esperado = paste(
        "equivalencia_no_requerida o",
        "equivalencia_excepcional_activada sin problemas"
      ),
      n_problematicos = n_faltantes + n_duplicados +
        n_diferencias + n_contradicciones + as.integer(!pasa),
      pasa = pasa
    )
  )
}

limitar_capitulo_a_universo <- function(df,
                                        capitulo,
                                        universos,
                                        df_validacion = NULL,
                                        llaves_validacion = NULL) {
  llaves <- llaves_validacion %||%
    funcion_paquete("get_join_keys")(capitulo)
  df_validacion <- df_validacion %||% df
  df_norm <- normalizar_llaves_cruce_df(df_validacion, llaves)
  universo <- universo_para_capitulo(capitulo, universos)

  n_antes <- nrow(df_norm)
  con_indice <- df_norm |>
    dplyr::mutate(.fila_original_entrega = dplyr::row_number())
  indices_dentro <- con_indice |>
    dplyr::semi_join(universo, by = llaves) |>
    dplyr::pull(".fila_original_entrega")
  indices_fuera <- con_indice |>
    dplyr::anti_join(universo, by = llaves) |>
    dplyr::pull(".fila_original_entrega")
  salida <- tibble::as_tibble(
    df[indices_dentro, , drop = FALSE],
    .name_repair = "minimal"
  )
  fuera_universo <- tibble::as_tibble(
    df[indices_fuera, , drop = FALSE],
    .name_repair = "minimal"
  )
  n_despues <- nrow(salida)
  duplicados_antes <- duplicados_por_llave(
    df_validacion,
    llaves,
    capitulo,
    "OSIS_ANTES_FILTRO"
  )
  validacion_despues <- df_validacion[
    indices_dentro,
    ,
    drop = FALSE
  ]
  duplicados_despues <- duplicados_por_llave(
    validacion_despues,
    llaves,
    capitulo,
    "OSIS_DESPUES_FILTRO"
  )
  llaves_faltantes <- sum(vapply(
    llaves,
    function(llave) {
      valor <- df_norm[[llave]]
      sum(is.na(valor) | !nzchar(valor))
    },
    integer(1)
  ))

  list(
    data = salida,
    n_antes = n_antes,
    n_despues = n_despues,
    n_retirados = n_antes - n_despues,
    incremento_filas = n_despues > n_antes,
    fuera_universo = fuera_universo,
    duplicados_antes = duplicados_antes,
    duplicados_despues = duplicados_despues,
    n_llaves_faltantes = llaves_faltantes,
    llaves_validacion = llaves,
    data_validacion_antes = df_validacion,
    data_validacion_despues = validacion_despues
  )
}

preparar_columnas_autorizadas <- function(df,
                                          capitulo,
                                          variables_permitidas,
                                          variables_no_autorizadas,
                                          llaves_autorizacion = NULL) {
  permitidas_cap <- variables_permitidas |>
    dplyr::filter(.data$capitulo == .env$capitulo) |>
    dplyr::arrange(.data$orden_diccionario)

  if (nrow(permitidas_cap) == 0L) {
    return(list(
      data = NULL,
      permitidas_presentes = tibble::tibble(),
      permitidas_faltantes = tibble::tibble(
        capitulo = capitulo,
        variable = NA_character_,
        motivo = "Capitulo sin lista blanca en diccionario anonimizado."
      ),
      variables_retiradas = tibble::tibble(),
      columnas_exactas = FALSE,
      orden_exacto = FALSE,
      llaves_autorizadas = FALSE
    ))
  }

  nombres_fuente <- names(df)
  nombres_fuente_norm <- normalizar_variable_entrega(nombres_fuente)
  duplicados_norm <- unique(
    nombres_fuente_norm[duplicated(nombres_fuente_norm)]
  )
  if (length(duplicados_norm) > 0L) {
    stop(
      "El capitulo ", capitulo,
      " tiene columnas ambiguas despues de normalizar: ",
      paste(duplicados_norm, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  indice <- match(permitidas_cap$variable, nombres_fuente_norm)
  permitidas_cap <- permitidas_cap |>
    dplyr::mutate(
      presente_en_osis = !is.na(indice),
      nombre_real_osis = dplyr::if_else(
        .data$presente_en_osis,
        nombres_fuente[indice],
        NA_character_
      )
    )

  presentes <- permitidas_cap |>
    dplyr::filter(.data$presente_en_osis) |>
    dplyr::mutate(
      nombre_diccionario = .data$variable_original,
      nombre_normalizado = .data$variable,
      estado_correspondencia = "CORRESPONDENCIA_INEQUIVOCA",
      clasificacion = "permitida_presente"
    )
  faltantes <- permitidas_cap |>
    dplyr::filter(!.data$presente_en_osis) |>
    dplyr::mutate(
      nombre_diccionario = .data$variable_original,
      nombre_normalizado = .data$variable,
      estado_correspondencia = "FALTANTE_EN_OSIS",
      clasificacion = "permitida_faltante_en_osis"
    )

  no_autorizadas_cap <- variables_no_autorizadas |>
    dplyr::filter(.data$capitulo == .env$capitulo) |>
    dplyr::select("variable")

  retiradas <- tibble::tibble(
    capitulo = capitulo,
    nombre_real_osis = nombres_fuente,
    variable = nombres_fuente_norm
  ) |>
    dplyr::anti_join(
      permitidas_cap |>
        dplyr::select("variable"),
      by = "variable"
    ) |>
    dplyr::left_join(
      no_autorizadas_cap |>
        dplyr::mutate(en_diccionario_completo_no_anonimizado = TRUE),
      by = "variable"
    ) |>
    dplyr::mutate(
      clasificacion = dplyr::if_else(
        dplyr::coalesce(
          .data$en_diccionario_completo_no_anonimizado,
          FALSE
        ),
        "prohibida_encontrada_y_retirada",
        "presente_no_autorizada_retirada"
      )
    )

  if (nrow(faltantes) > 0L) {
    salida <- NULL
    columnas_exactas <- FALSE
    orden_exacto <- FALSE
  } else {
    salida <- df[, presentes$nombre_real_osis, drop = FALSE]
    columnas_exactas <- setequal(
      normalizar_variable_entrega(names(salida)),
      permitidas_cap$variable
    )
    orden_exacto <- identical(
      normalizar_variable_entrega(names(salida)),
      permitidas_cap$variable
    )
  }

  llaves <- llaves_autorizacion %||%
    funcion_paquete("get_join_keys")(capitulo)
  llaves_autorizadas <- all(
    normalizar_variable_entrega(llaves) %in% permitidas_cap$variable
  )

  list(
    data = salida,
    permitidas_presentes = presentes,
    permitidas_faltantes = faltantes,
    variables_retiradas = retiradas,
    columnas_exactas = columnas_exactas,
    orden_exacto = orden_exacto,
    llaves_autorizadas = llaves_autorizadas
  )
}

prefijo_tematico_predominante <- function(nombres) {
  nombres <- normalizar_variable_entrega(nombres)
  nombres <- setdiff(
    nombres,
    c(
      "DIRECTORIO",
      "SECUENCIA_P",
      "ORDEN",
      "ENCUESTA_COMPLETA"
    )
  )
  prefijos <- stringr::str_replace(nombres, "[0-9].*$", "")
  prefijos <- prefijos[
    stringr::str_detect(prefijos, "^[A-Z]{4,}$")
  ]
  if (length(prefijos) == 0L) {
    return(tibble::tibble(
      prefijo = NA_character_,
      proporcion = NA_real_
    ))
  }
  conteo <- sort(table(prefijos), decreasing = TRUE)
  tibble::tibble(
    prefijo = names(conteo)[[1]],
    proporcion = as.numeric(conteo[[1]]) / length(prefijos)
  )
}

auditar_archivos_posiblemente_equivocados <- function(
    dfs_osis,
    variables_permitidas,
    inventario_osis = NULL,
    umbral_encabezado = 0.80,
    umbral_ausencia = 0.25
) {
  capitulos <- intersect(
    names(dfs_osis),
    unique(variables_permitidas$capitulo)
  )
  inventario_osis <- inventario_osis %||% tibble::tibble()

  obtener_md5 <- function(capitulo) {
    if (
      !all(c("capitulo", "md5") %in% names(inventario_osis))
    ) {
      return(NA_character_)
    }
    candidatos <- inventario_osis |>
      dplyr::filter(.data$capitulo == .env$capitulo) |>
      dplyr::pull("md5")
    if (length(candidatos) == 0L) NA_character_ else {
      as.character(candidatos[[1]])
    }
  }

  purrr::map_dfr(capitulos, function(capitulo) {
    fuente_original <- names(dfs_osis[[capitulo]])
    fuente_normalizada <- unique(
      normalizar_variable_entrega(fuente_original)
    )
    esperadas <- variables_permitidas |>
      dplyr::filter(.data$capitulo == .env$capitulo) |>
      dplyr::pull("variable") |>
      unique()
    presentes <- intersect(esperadas, fuente_normalizada)
    ausentes <- setdiff(esperadas, fuente_normalizada)
    proporcion_ausentes <- if (length(esperadas) == 0L) {
      0
    } else {
      length(ausentes) / length(esperadas)
    }

    otros <- setdiff(capitulos, capitulo)
    comparaciones <- purrr::map_dfr(otros, function(otro) {
      encabezado_original_otro <- names(dfs_osis[[otro]])
      encabezado_otro <- unique(
        normalizar_variable_entrega(encabezado_original_otro)
      )
      denominador <- max(
        length(fuente_normalizada),
        length(encabezado_otro),
        1L
      )
      tibble::tibble(
        capitulo_comparado = otro,
        coincidencia_encabezado =
          length(intersect(fuente_normalizada, encabezado_otro)) /
          denominador,
        encabezado_original_comparado = paste(
          encabezado_original_otro,
          collapse = " | "
        ),
        encabezado_normalizado_comparado = paste(
          encabezado_otro,
          collapse = " | "
        ),
        n_columnas_comparado = length(encabezado_otro),
        md5_comparado = obtener_md5(otro),
        md5_igual = {
          md5_objetivo <- obtener_md5(capitulo)
          md5_otro <- obtener_md5(otro)
          if (is.na(md5_objetivo) || is.na(md5_otro)) {
            NA
          } else {
            identical(md5_objetivo, md5_otro)
          }
        }
      )
    })
    mejor <- if (nrow(comparaciones) == 0L) {
      tibble::tibble(
        capitulo_comparado = NA_character_,
        coincidencia_encabezado = 0,
        encabezado_original_comparado = NA_character_,
        encabezado_normalizado_comparado = NA_character_,
        n_columnas_comparado = NA_integer_,
        md5_comparado = NA_character_,
        md5_igual = NA
      )
    } else {
      comparaciones |>
        dplyr::arrange(
          dplyr::desc(dplyr::coalesce(.data$md5_igual, FALSE)),
          dplyr::desc(.data$coincidencia_encabezado),
          .data$capitulo_comparado
        ) |>
        dplyr::slice(1L)
    }

    prefijo_fuente <- prefijo_tematico_predominante(
      fuente_normalizada
    )
    prefijo_esperado <- prefijo_tematico_predominante(esperadas)
    prefijo_otro <- if (
      is.na(mejor$capitulo_comparado[[1]])
    ) {
      tibble::tibble(prefijo = NA_character_)
    } else {
      prefijo_tematico_predominante(
        variables_permitidas |>
          dplyr::filter(
            .data$capitulo == mejor$capitulo_comparado[[1]]
          ) |>
          dplyr::pull("variable")
      )
    }
    prefijo_ajeno <-
      !is.na(prefijo_fuente$prefijo[[1]]) &&
      !is.na(prefijo_esperado$prefijo[[1]]) &&
      prefijo_fuente$prefijo[[1]] != prefijo_esperado$prefijo[[1]] &&
      !is.na(prefijo_otro$prefijo[[1]]) &&
      prefijo_fuente$prefijo[[1]] == prefijo_otro$prefijo[[1]]
    encabezado_alto <-
      mejor$coincidencia_encabezado[[1]] >= umbral_encabezado
    md5_igual <- isTRUE(mejor$md5_igual[[1]])
    ausencia_significativa <-
      length(ausentes) > 0L &&
      proporcion_ausentes >= umbral_ausencia
    critico <- ausencia_significativa &&
      (encabezado_alto || md5_igual || prefijo_ajeno)

    tibble::tibble(
      capitulo = capitulo,
      archivo = if (
        "archivo" %in% names(inventario_osis)
      ) {
        archivos <- inventario_osis |>
          dplyr::filter(.data$capitulo == .env$capitulo) |>
          dplyr::pull("archivo")
        if (length(archivos) == 0L) NA_character_ else archivos[[1]]
      } else {
        NA_character_
      },
      md5 = obtener_md5(capitulo),
      capitulo_encabezado_mas_parecido =
        mejor$capitulo_comparado[[1]],
      md5_archivo_comparado = mejor$md5_comparado[[1]],
      md5_igual_archivo_comparado = mejor$md5_igual[[1]],
      coincidencia_encabezado_pct =
        100 * mejor$coincidencia_encabezado[[1]],
      encabezado_original = paste(fuente_original, collapse = " | "),
      encabezado_normalizado = paste(
        fuente_normalizada,
        collapse = " | "
      ),
      encabezado_original_archivo_comparado =
        mejor$encabezado_original_comparado[[1]],
      encabezado_normalizado_archivo_comparado =
        mejor$encabezado_normalizado_comparado[[1]],
      n_columnas = length(fuente_original),
      n_columnas_archivo_comparado =
        mejor$n_columnas_comparado[[1]],
      n_variables_esperadas = length(esperadas),
      n_variables_esperadas_presentes = length(presentes),
      n_variables_esperadas_ausentes = length(ausentes),
      proporcion_variables_esperadas_presentes =
        if (length(esperadas) == 0L) 1 else {
          length(presentes) / length(esperadas)
        },
      variables_esperadas_ausentes = paste(ausentes, collapse = ", "),
      prefijo_predominante_fuente = prefijo_fuente$prefijo[[1]],
      prefijo_predominante_esperado = prefijo_esperado$prefijo[[1]],
      prefijo_predominante_archivo_comparado =
        prefijo_otro$prefijo[[1]],
      clasificacion = dplyr::case_when(
        critico ~ "posible_archivo_de_capitulo_equivocado",
        ausencia_significativa ~
          "faltantes_significativos_sin_evidencia_conjunta_de_archivo_equivocado",
        TRUE ~ "sin_evidencia_de_archivo_equivocado"
      ),
      criticidad = if (critico) "CRITICO" else "INFORMATIVO",
      estado_validacion = if (critico) "FAIL" else "PASS"
    )
  })
}

comparar_universos_capitulo <- function(df_osis,
                                        df_osis_completo,
                                        df_cap,
                                        df_cap_completo,
                                        capitulo,
                                        universo_completo,
                                        regla_comparacion,
                                        df_osis_validacion = NULL,
                                        df_osis_completo_validacion = NULL,
                                        llaves_validacion = NULL) {
  llaves <- llaves_validacion %||%
    funcion_paquete("get_join_keys")(capitulo)
  df_osis_validacion <- df_osis_validacion %||% df_osis
  df_osis_completo_validacion <-
    df_osis_completo_validacion %||% df_osis_completo
  llaves_osis <- tabla_llaves(
    df_osis_validacion,
    llaves,
    capitulo,
    "OSIS"
  )
  llaves_osis_completo <- tabla_llaves(
    df_osis_completo_validacion,
    llaves,
    capitulo,
    "OSIS_COMPLETO"
  )
  llaves_cap_total <- tabla_llaves(
    df_cap,
    llaves,
    capitulo,
    "CAP_OFICIAL"
  )
  llaves_cap_completo <- tabla_llaves(
    df_cap_completo,
    llaves,
    capitulo,
    "CAP_OFICIAL_COMPLETA"
  )

  unicas_osis <- dplyr::distinct(llaves_osis)
  unicas_osis_completo <- dplyr::distinct(llaves_osis_completo)
  unicas_cap_total <- dplyr::distinct(llaves_cap_total)
  unicas_cap_completo <- dplyr::distinct(llaves_cap_completo)
  unicas_universo_completo <- tabla_llaves(
    universo_completo,
    llaves,
    capitulo,
    "UNIVERSO_COMPLETO_REFERENCIA"
  ) |>
    dplyr::distinct()

  osis_no_cap <- dplyr::anti_join(
    unicas_osis,
    unicas_cap_total,
    by = llaves
  ) |>
    dplyr::mutate(
      capitulo = capitulo,
      tipo_diferencia = "llave_total_osis_ausente_cap",
      .before = 1
    )
  cap_no_osis <- dplyr::anti_join(
    unicas_cap_total,
    unicas_osis,
    by = llaves
  ) |>
    dplyr::mutate(
      capitulo = capitulo,
      tipo_diferencia = "llave_cap_ausente_osis_total",
      .before = 1
    )
  osis_completo_no_cap <- dplyr::anti_join(
    unicas_osis_completo,
    unicas_cap_completo,
    by = llaves
  ) |>
    dplyr::mutate(
      capitulo = capitulo,
      tipo_diferencia = "llave_completa_osis_ausente_cap",
      criticidad = if (
        regla_comparacion == "exacta_estructural"
      ) "CRITICO" else "INFORMATIVO",
      .before = 1
    )
  cap_no_osis_completo <- dplyr::anti_join(
    unicas_cap_completo,
    unicas_osis_completo,
    by = llaves
  ) |>
    dplyr::mutate(
      capitulo = capitulo,
      tipo_diferencia = "llave_cap_ausente_osis_completo",
      criticidad = if (
        regla_comparacion == "exacta_estructural"
      ) "CRITICO" else "INFORMATIVO",
      .before = 1
    )

  osis_fuera_universo <- dplyr::anti_join(
    unicas_osis_completo,
    unicas_universo_completo,
    by = llaves
  ) |>
    dplyr::mutate(
      capitulo = capitulo,
      tipo_diferencia = "llave_osis_fuera_universo_completo",
      criticidad = "CRITICO",
      .before = 1
    )
  universo_sin_osis <- dplyr::anti_join(
    unicas_universo_completo,
    unicas_osis_completo,
    by = llaves
  ) |>
    dplyr::mutate(
      capitulo = capitulo,
      tipo_diferencia = "llave_universo_completo_sin_registro_osis",
      criticidad = "INFORMATIVO",
      .before = 1
    )

  duplicados_osis <- duplicados_por_llave(
    df_osis_validacion,
    llaves,
    capitulo,
    "OSIS"
  )
  duplicados_cap <- duplicados_por_llave(
    df_cap,
    llaves,
    capitulo,
    "CAP_OFICIAL"
  )

  coincide_total <- nrow(osis_no_cap) == 0L &&
    nrow(cap_no_osis) == 0L
  coincide_completo <- nrow(osis_completo_no_cap) == 0L &&
    nrow(cap_no_osis_completo) == 0L
  cumple_regla <- if (regla_comparacion == "exacta_estructural") {
    coincide_completo && nrow(duplicados_osis) == 0L &&
      nrow(duplicados_cap) == 0L &&
      llaves_sin_faltantes(df_osis_validacion, llaves) &&
      llaves_sin_faltantes(df_cap, llaves)
  } else {
    nrow(osis_fuera_universo) == 0L &&
      nrow(duplicados_osis) == 0L &&
      llaves_sin_faltantes(df_osis_validacion, llaves)
  }

  list(
    resumen = tibble::tibble(
      capitulo = capitulo,
      nivel = funcion_paquete("tipo_capitulo")[[capitulo]],
      regla_comparacion = regla_comparacion,
      llaves = paste(llaves, collapse = " + "),
      filas_osis = nrow(df_osis),
      filas_cap = nrow(df_cap),
      llaves_unicas_osis = nrow(unicas_osis),
      llaves_unicas_cap = nrow(unicas_cap_total),
      duplicados_osis = nrow(duplicados_osis),
      duplicados_cap = nrow(duplicados_cap),
      llaves_osis_ausentes_cap = nrow(osis_no_cap),
      llaves_cap_ausentes_osis = nrow(cap_no_osis),
      coincidencia_exacta_universo_total = coincide_total,
      filas_completas_osis = nrow(df_osis_completo),
      filas_completas_cap = nrow(df_cap_completo),
      llaves_completas_osis_ausentes_cap =
        nrow(osis_completo_no_cap),
      llaves_cap_ausentes_osis_completo =
        nrow(cap_no_osis_completo),
      coincidencia_exacta_universo_completo = coincide_completo,
      llaves_osis_fuera_universo_completo =
        nrow(osis_fuera_universo),
      llaves_universo_completo_sin_osis =
        nrow(universo_sin_osis),
      cumple_regla_comparacion = cumple_regla,
      estado_comparacion = dplyr::if_else(
        cumple_regla,
        "PASS",
        "FAIL"
      )
    ),
    diferencias = dplyr::bind_rows(
      osis_no_cap,
      cap_no_osis,
      osis_completo_no_cap,
      cap_no_osis_completo,
      osis_fuera_universo,
      universo_sin_osis
    ),
    duplicados = dplyr::bind_rows(
      duplicados_osis,
      duplicados_cap
    )
  )
}

auditar_marcas_a_c_e <- function(dfs_osis,
                                  dfs_cap,
                                  variable_completitud) {
  resumen <- list()
  diferencias <- list()

  for (capitulo in c("A", "C", "E")) {
    llaves <- funcion_paquete("get_join_keys")(capitulo)
    osis <- normalizar_llaves_cruce_df(
      dfs_osis[[capitulo]],
      llaves
    )
    cap <- normalizar_llaves_cruce_df(
      dfs_cap[[capitulo]],
      llaves
    )

    osis$marca_normalizada <- es_valor_completo_oficial(
      osis[[variable_completitud]]
    )
    valor_cap <- if (variable_completitud %in% names(cap)) {
      cap[[variable_completitud]]
    } else {
      rep("1", nrow(cap))
    }
    cap$marca_normalizada <- es_valor_completo_oficial(valor_cap)

    resumen[[capitulo]] <- dplyr::bind_rows(
      tibble::tibble(
        capitulo = capitulo,
        origen = "OSIS",
        n_registros = nrow(osis),
        n_valor_1 = sum(osis$marca_normalizada, na.rm = TRUE),
        n_faltantes = sum(
          is.na(osis[[variable_completitud]]) |
            !nzchar(stringr::str_squish(
              as.character(osis[[variable_completitud]])
            ))
        ),
        n_distintos_0_1 = sum(
          !is.na(osis[[variable_completitud]]) &
            nzchar(stringr::str_squish(
              as.character(osis[[variable_completitud]])
            )) &
            !es_valor_binario_oficial(
              osis[[variable_completitud]]
            )
        )
      ),
      tibble::tibble(
        capitulo = capitulo,
        origen = "CAP_OFICIAL_20260703",
        n_registros = nrow(cap),
        n_valor_1 = sum(cap$marca_normalizada, na.rm = TRUE),
        n_faltantes = if (variable_completitud %in% names(cap)) {
          sum(
            is.na(cap[[variable_completitud]]) |
              !nzchar(stringr::str_squish(
                as.character(cap[[variable_completitud]])
              ))
          )
        } else {
          0L
        },
        n_distintos_0_1 = if (
          variable_completitud %in% names(cap)
        ) {
          sum(
            !is.na(cap[[variable_completitud]]) &
              nzchar(stringr::str_squish(
                as.character(cap[[variable_completitud]])
              )) &
              !es_valor_binario_oficial(
                cap[[variable_completitud]]
              )
          )
        } else {
          0L
        }
      )
    )

    diferencias[[capitulo]] <- osis |>
      dplyr::select(
        dplyr::all_of(llaves),
        marca_osis = "marca_normalizada"
      ) |>
      dplyr::inner_join(
        cap |>
          dplyr::select(
            dplyr::all_of(llaves),
            marca_cap = "marca_normalizada"
          ),
        by = llaves
      ) |>
      dplyr::filter(
        is.na(.data$marca_osis) != is.na(.data$marca_cap) |
          dplyr::coalesce(
            .data$marca_osis != .data$marca_cap,
            FALSE
          )
      ) |>
      dplyr::mutate(capitulo = capitulo, .before = 1)
  }

  list(
    resumen = dplyr::bind_rows(resumen),
    diferencias = dplyr::bind_rows(diferencias)
  )
}

comparar_cap_oficial_con_salida_derivada <- function(
    bases_cap_completas,
    dfs_cap_sin_tematica
) {
  if (is.null(dfs_cap_sin_tematica)) {
    return(list(
      resumen = tibble::tibble(),
      diferencias = tibble::tibble()
    ))
  }

  capitulos <- intersect(
    names(bases_cap_completas),
    names(dfs_cap_sin_tematica)
  )
  resultados <- purrr::map(capitulos, function(capitulo) {
    llaves <- funcion_paquete("get_join_keys")(capitulo)
    oficial <- tabla_llaves(
      bases_cap_completas[[capitulo]],
      llaves,
      capitulo,
      "CAP_OFICIAL_COMPLETA"
    ) |>
      dplyr::distinct()
    derivada <- tabla_llaves(
      dfs_cap_sin_tematica[[capitulo]],
      llaves,
      capitulo,
      "SALIDA_SIN_TEMATICA"
    ) |>
      dplyr::distinct()

    oficial_no_derivada <- dplyr::anti_join(
      oficial,
      derivada,
      by = llaves
    ) |>
      dplyr::mutate(
        capitulo = capitulo,
        tipo_diferencia =
          "cap_oficial_completa_ausente_salida_sin_tematica",
        .before = 1
      )
    derivada_no_oficial <- dplyr::anti_join(
      derivada,
      oficial,
      by = llaves
    ) |>
      dplyr::mutate(
        capitulo = capitulo,
        tipo_diferencia =
          "salida_sin_tematica_ausente_cap_oficial_completa",
        .before = 1
      )

    list(
      resumen = tibble::tibble(
        capitulo = capitulo,
        n_cap_oficial_completa = nrow(oficial),
        n_salida_sin_tematica = nrow(derivada),
        n_oficial_no_derivada = nrow(oficial_no_derivada),
        n_derivada_no_oficial = nrow(derivada_no_oficial),
        coincide = nrow(oficial_no_derivada) == 0L &&
          nrow(derivada_no_oficial) == 0L
      ),
      diferencias = dplyr::bind_rows(
        oficial_no_derivada,
        derivada_no_oficial
      )
    )
  })

  list(
    resumen = purrr::map_dfr(resultados, "resumen"),
    diferencias = purrr::map_dfr(resultados, "diferencias")
  )
}

nuevo_control <- function(id,
                          capitulo = NA_character_,
                          descripcion,
                          observado,
                          esperado,
                          n_problematicos,
                          pasa,
                          criticidad = "CRITICO",
                          observacion = NA_character_) {
  tibble::tibble(
    control_id = id,
    capitulo = capitulo,
    descripcion = descripcion,
    resultado_observado = as.character(observado),
    resultado_esperado = as.character(esperado),
    n_casos_problematicos = as.integer(n_problematicos),
    estado = if (isTRUE(pasa)) "PASS" else "FAIL",
    criticidad = criticidad,
    observacion = observacion
  )
}

resumir_cobertura_capitulos <- function(origen,
                                        encontrados,
                                        esperados,
                                        permitidos) {
  encontrados <- unique(as.character(encontrados))
  esperados <- unique(as.character(esperados))
  permitidos <- unique(as.character(permitidos))
  faltantes <- setdiff(esperados, encontrados)
  inesperados <- setdiff(encontrados, permitidos)

  tibble::tibble(
    origen = origen,
    capitulos_esperados = paste(esperados, collapse = ", "),
    capitulos_encontrados = paste(encontrados, collapse = ", "),
    capitulos_faltantes = paste(faltantes, collapse = ", "),
    capitulos_inesperados = paste(inesperados, collapse = ", "),
    cantidad_esperada = length(esperados),
    cantidad_encontrada = length(encontrados),
    n_faltantes = length(faltantes),
    n_inesperados = length(inesperados),
    conjunto_esperado_completo =
      length(faltantes) == 0L &&
        length(inesperados) == 0L
  )
}

evaluar_manifest_capitulos <- function(manifest,
                                       capitulos_esperados_entrega,
                                       capitulo_excluido) {
  capitulos_exportados <- if (
    is.data.frame(manifest) &&
      "capitulo" %in% names(manifest)
  ) {
    as.character(manifest$capitulo)
  } else {
    character()
  }
  cobertura <- resumir_cobertura_capitulos(
    origen = "MANIFEST",
    encontrados = capitulos_exportados,
    esperados = capitulos_esperados_entrega,
    permitidos = capitulos_esperados_entrega
  )
  k_ausente <- !capitulo_excluido %in% capitulos_exportados
  pasa <- isTRUE(cobertura$conjunto_esperado_completo[[1]]) &&
    nrow(manifest) == length(capitulos_esperados_entrega) &&
    k_ausente

  list(
    cobertura = cobertura |>
      dplyr::mutate(
        capitulo_excluido = capitulo_excluido,
        capitulo_excluido_ausente = k_ausente
      ),
    control = nuevo_control(
      "CSV_CAPITULOS_ESPERADOS_COMPLETOS",
      descripcion = paste(
        "El manifiesto contiene exactamente todos los capitulos esperados",
        "y excluye K."
      ),
      observado = paste(
        "exportados=", cobertura$capitulos_encontrados[[1]],
        "; faltantes=", cobertura$capitulos_faltantes[[1]],
        "; inesperados=", cobertura$capitulos_inesperados[[1]],
        "; cantidad=", nrow(manifest),
        "; K_ausente=", k_ausente
      ),
      esperado = paste(
        "capitulos=", paste(
          capitulos_esperados_entrega,
          collapse = ", "
        ),
        "; cantidad=", length(capitulos_esperados_entrega),
        "; K_ausente=TRUE"
      ),
      n_problematicos = cobertura$n_faltantes[[1]] +
        cobertura$n_inesperados[[1]] +
        abs(nrow(manifest) - length(capitulos_esperados_entrega)) +
        as.integer(!k_ausente),
      pasa = pasa
    )
  )
}

procesar_entrega_en_memoria <- function(
    dfs_osis,
    dfs_cap_oficial,
    auditoria_diccionarios,
    config,
    dfs_cap_sin_tematica = NULL,
    inventario_osis = NULL,
    problemas_lectura = NULL
) {
  capitulo_excluido <- config$capitulo_excluido
  capitulos_esperados_entrega <- setdiff(
    config$orden_capitulos,
    config$capitulo_excluido
  )
  variables_permitidas <- auditoria_diccionarios$variables_permitidas
  variables_no_autorizadas <-
    auditoria_diccionarios$variables_no_autorizadas
  problemas_lectura <- problemas_lectura %||% tibble::tibble()
  auditoria_normalizacion_llaves <- dplyr::bind_rows(
    auditar_normalizacion_llaves(dfs_osis, "OSIS"),
    auditar_normalizacion_llaves(dfs_cap_oficial, "CAP_OFICIAL")
  )
  auditoria_archivos_equivocados <-
    auditar_archivos_posiblemente_equivocados(
      dfs_osis,
      variables_permitidas,
      inventario_osis
    )

  universos <- construir_universos_completos(
    dfs_osis,
    config$variable_completitud
  )
  universos_cap <- construir_universos_completos(
    dfs_cap_oficial,
    config$variable_completitud
  )
  capitulos_cap_configurados <- intersect(
    config$orden_capitulos,
    names(dfs_cap_oficial)
  )
  bases_cap_completas <- purrr::imap(
    dfs_cap_oficial[capitulos_cap_configurados],
    function(tabla, capitulo) {
      limitar_capitulo_a_universo(
        tabla,
        capitulo,
        universos_cap
      )$data
    }
  )

  controles <- list()
  agregar_control <- function(x) {
    controles[[length(controles) + 1L]] <<- x
  }

  agregar_control(nuevo_control(
    "DICC_01_VACIOS",
    descripcion = "Capitulos y variables no vacios en ambos diccionarios.",
    observado = nrow(auditoria_diccionarios$vacios),
    esperado = "0",
    n_problematicos = nrow(auditoria_diccionarios$vacios),
    pasa = nrow(auditoria_diccionarios$vacios) == 0L
  ))
  n_problemas_parsing_criticos <- if (
    nrow(problemas_lectura) == 0L ||
      !"criticidad" %in% names(problemas_lectura)
  ) {
    0L
  } else {
    sum(problemas_lectura$criticidad == "CRITICO", na.rm = TRUE)
  }
  agregar_control(nuevo_control(
    "LECTURA_01_PROBLEMAS_PARSING_CRITICOS",
    descripcion = paste(
      "Los problemas de parsing se clasifican y ninguno afecta",
      "estructura, llaves o integridad."
    ),
    observado = n_problemas_parsing_criticos,
    esperado = "0",
    n_problematicos = n_problemas_parsing_criticos,
    pasa = n_problemas_parsing_criticos == 0L
  ))

  if (nrow(auditoria_archivos_equivocados) > 0L) {
    purrr::pwalk(
      auditoria_archivos_equivocados,
      function(capitulo, criticidad, clasificacion, ...) {
        es_critico <- identical(criticidad, "CRITICO")
        agregar_control(nuevo_control(
          paste0("OSIS_", capitulo, "_ARCHIVO_CORRECTO"),
          capitulo = capitulo,
          descripcion = paste(
            "El encabezado, la lista blanca y el prefijo tematico no",
            "evidencian un archivo de otro capitulo."
          ),
          observado = clasificacion,
          esperado = "sin evidencia conjunta de archivo equivocado",
          n_problematicos = as.integer(es_critico),
          pasa = !es_critico
        ))
      }
    )
  }
  agregar_control(nuevo_control(
    "DICC_02_DUPLICADOS",
    descripcion = "Sin duplicados capitulo-variable en los diccionarios.",
    observado = nrow(auditoria_diccionarios$duplicados),
    esperado = "0",
    n_problematicos = nrow(auditoria_diccionarios$duplicados),
    pasa = nrow(auditoria_diccionarios$duplicados) == 0L
  ))

  capitulos_diccionario_anonimizado <- unique(
    variables_permitidas$capitulo
  )
  coberturas_capitulos <- dplyr::bind_rows(
    resumir_cobertura_capitulos(
      origen = "OSIS",
      encontrados = names(dfs_osis),
      esperados = capitulos_esperados_entrega,
      permitidos = c(
        capitulos_esperados_entrega,
        config$capitulo_excluido
      )
    ),
    resumir_cobertura_capitulos(
      origen = "CAP_OFICIAL",
      encontrados = names(dfs_cap_oficial),
      esperados = capitulos_esperados_entrega,
      permitidos = config$orden_capitulos
    ),
    resumir_cobertura_capitulos(
      origen = "DICCIONARIO_ANONIMIZADO",
      encontrados = capitulos_diccionario_anonimizado,
      esperados = capitulos_esperados_entrega,
      permitidos = config$orden_capitulos
    )
  )
  cobertura_osis <- coberturas_capitulos[
    coberturas_capitulos$origen == "OSIS",
    ,
    drop = FALSE
  ]
  cobertura_cap <- coberturas_capitulos[
    coberturas_capitulos$origen == "CAP_OFICIAL",
    ,
    drop = FALSE
  ]
  cobertura_diccionario <- coberturas_capitulos[
    coberturas_capitulos$origen == "DICCIONARIO_ANONIMIZADO",
    ,
    drop = FALSE
  ]

  agregar_control(nuevo_control(
    "OSIS_02_CAPITULOS_ESPERADOS",
    descripcion = "OSIS contiene todos los capitulos esperados de entrega.",
    observado = cobertura_osis$capitulos_faltantes[[1]],
    esperado = "Ninguno",
    n_problematicos = cobertura_osis$n_faltantes[[1]],
    pasa = cobertura_osis$n_faltantes[[1]] == 0L
  ))
  agregar_control(nuevo_control(
    "OSIS_03_CAPITULOS_INESPERADOS",
    descripcion = paste(
      "OSIS no contiene capitulos inesperados; K puede existir solo",
      "como fuente excluida."
    ),
    observado = cobertura_osis$capitulos_inesperados[[1]],
    esperado = "Ninguno",
    n_problematicos = cobertura_osis$n_inesperados[[1]],
    pasa = cobertura_osis$n_inesperados[[1]] == 0L
  ))
  agregar_control(nuevo_control(
    "CAPREF_02_CAPITULOS_ESPERADOS",
    descripcion = "La CAP oficial contiene todos los capitulos esperados.",
    observado = cobertura_cap$capitulos_faltantes[[1]],
    esperado = "Ninguno",
    n_problematicos = cobertura_cap$n_faltantes[[1]],
    pasa = cobertura_cap$n_faltantes[[1]] == 0L
  ))
  agregar_control(nuevo_control(
    "CAPREF_03_CAPITULOS_INESPERADOS",
    descripcion = "La CAP oficial no contiene capitulos no configurados.",
    observado = cobertura_cap$capitulos_inesperados[[1]],
    esperado = "Ninguno",
    n_problematicos = cobertura_cap$n_inesperados[[1]],
    pasa = cobertura_cap$n_inesperados[[1]] == 0L
  ))
  agregar_control(nuevo_control(
    "DICC_04_CAPITULOS_ESPERADOS",
    descripcion = paste(
      "Cada capitulo esperado tiene una lista blanca explicita en el",
      "diccionario anonimizado."
    ),
    observado = cobertura_diccionario$capitulos_faltantes[[1]],
    esperado = "Ninguno",
    n_problematicos = cobertura_diccionario$n_faltantes[[1]],
    pasa = cobertura_diccionario$n_faltantes[[1]] == 0L
  ))
  agregar_control(nuevo_control(
    "DICC_05_CAPITULOS_INESPERADOS",
    descripcion = paste(
      "El diccionario anonimizado no contiene capitulos no configurados."
    ),
    observado = cobertura_diccionario$capitulos_inesperados[[1]],
    esperado = "Ninguno",
    n_problematicos = cobertura_diccionario$n_inesperados[[1]],
    pasa = cobertura_diccionario$n_inesperados[[1]] == 0L
  ))
  agregar_control(nuevo_control(
    "DICC_03_ANON_EN_COMPLETO",
    descripcion = paste(
      "Toda variable del diccionario anonimizado existe en el",
      "diccionario completo."
    ),
    observado = nrow(
      auditoria_diccionarios$
        variables_anonimizadas_no_en_diccionario_completo
    ),
    esperado = "0",
    n_problematicos = nrow(
      auditoria_diccionarios$
        variables_anonimizadas_no_en_diccionario_completo
    ),
    pasa = nrow(
      auditoria_diccionarios$
        variables_anonimizadas_no_en_diccionario_completo
    ) == 0L
  ))
  agregar_control(nuevo_control(
    "UNIV_01_JERARQUIA_A_C_E",
    descripcion = "Coherencia jerarquica de las marcas completas A-C-E.",
    observado = nrow(universos$contradicciones),
    esperado = "0",
    n_problematicos = nrow(universos$contradicciones),
    pasa = nrow(universos$contradicciones) == 0L
  ))
  agregar_control(nuevo_control(
    "UNIV_02_JERARQUIA_CAP_A_C_E",
    descripcion = paste(
      "Coherencia jerarquica de las marcas completas en la CAP oficial."
    ),
    observado = nrow(universos_cap$contradicciones),
    esperado = "0",
    n_problematicos = nrow(universos_cap$contradicciones),
    pasa = nrow(universos_cap$contradicciones) == 0L
  ))

  caps_osis <- names(dfs_osis)
  caps_exportables <- setdiff(caps_osis, capitulo_excluido)
  caps_diccionario <- unique(variables_permitidas$capitulo)
  caps_diccionario_exportables <- setdiff(
    caps_diccionario,
    capitulo_excluido
  )

  faltan_caps_osis <- setdiff(
    caps_diccionario_exportables,
    caps_exportables
  )
  agregar_control(nuevo_control(
    "OSIS_01_CAPITULOS_LISTA_BLANCA",
    descripcion = paste(
      "Todo capitulo no K del diccionario anonimizado tiene un CSV OSIS."
    ),
    observado = paste(faltan_caps_osis, collapse = ", "),
    esperado = "Ninguno",
    n_problematicos = length(faltan_caps_osis),
    pasa = length(faltan_caps_osis) == 0L
  ))

  agregar_control(nuevo_control(
    "SALIDA_01_K_EXCLUIDO",
    capitulo = capitulo_excluido,
    descripcion = "El capitulo K queda excluido de la entrega.",
    observado = if (capitulo_excluido %in% caps_osis) {
      "Detectado en OSIS y excluido"
    } else {
      "No detectado en OSIS"
    },
    esperado = "No exportado",
    n_problematicos = 0L,
    pasa = TRUE,
    observacion = paste(
      "Exclusion metodologica temporal; no se modifica el archivo fuente."
    )
  ))

  bases_filtradas <- list()
  bases_exportables <- list()
  permitidas_presentes <- list()
  permitidas_faltantes <- list()
  variables_retiradas <- list()
  resumen_capitulos <- list()
  fuera_universo <- list()
  duplicados <- list()
  bases_validacion_antes <- list()
  bases_validacion_filtradas <- list()
  llaves_validacion_capitulo <- list()

  equivalencia_b <- NULL
  if (
    "B" %in% names(dfs_osis) &&
      "B" %in% names(dfs_cap_oficial)
  ) {
    equivalencia_b <- resolver_equivalencia_llave_b(
      dfs_osis$B,
      dfs_cap_oficial$B
    )
    agregar_control(equivalencia_b$control)
  }

  for (capitulo in caps_exportables) {
    if (!capitulo %in% names(funcion_paquete("tipo_capitulo"))) {
      agregar_control(nuevo_control(
        paste0("CAP_", capitulo, "_TIPO"),
        capitulo = capitulo,
        descripcion = "Capitulo reconocido por tipo_capitulo.",
        observado = "No reconocido",
        esperado = "Reconocido",
        n_problematicos = 1L,
        pasa = FALSE
      ))
      next
    }

    llaves <- funcion_paquete("get_join_keys")(capitulo)
    df_validacion <- dfs_osis[[capitulo]]
    llaves_autorizacion <- llaves
    if (identical(capitulo, "B") && !is.null(equivalencia_b)) {
      df_validacion <- equivalencia_b$data_validacion
      llaves <- equivalencia_b$llaves
      if (
        !"SECUENCIA_P" %in% names(dfs_osis$B) &&
          "ORDEN" %in% names(dfs_osis$B)
      ) {
        llaves_autorizacion <- c("DIRECTORIO", "ORDEN")
      }
    }
    faltan_llaves <- setdiff(llaves, names(df_validacion))
    if (length(faltan_llaves) > 0L) {
      seleccion_incompleta <- preparar_columnas_autorizadas(
        dfs_osis[[capitulo]],
        capitulo,
        variables_permitidas,
        variables_no_autorizadas,
        llaves_autorizacion = llaves_autorizacion
      )
      permitidas_presentes[[capitulo]] <-
        seleccion_incompleta$permitidas_presentes
      permitidas_faltantes[[capitulo]] <-
        seleccion_incompleta$permitidas_faltantes
      variables_retiradas[[capitulo]] <-
        seleccion_incompleta$variables_retiradas
      agregar_control(nuevo_control(
        paste0("CAP_", capitulo, "_LLAVES"),
        capitulo = capitulo,
        descripcion = "Existencia de llaves estructurales requeridas.",
        observado = paste(faltan_llaves, collapse = ", "),
        esperado = paste(llaves, collapse = " + "),
        n_problematicos = length(faltan_llaves),
        pasa = FALSE
      ))
      next
    }

    filtro <- limitar_capitulo_a_universo(
      dfs_osis[[capitulo]],
      capitulo,
      universos,
      df_validacion = df_validacion,
      llaves_validacion = llaves
    )
    bases_filtradas[[capitulo]] <- filtro$data
    fuera_universo[[capitulo]] <- filtro$fuera_universo
    bases_validacion_antes[[capitulo]] <-
      filtro$data_validacion_antes
    bases_validacion_filtradas[[capitulo]] <-
      filtro$data_validacion_despues
    llaves_validacion_capitulo[[capitulo]] <- llaves
    duplicados[[capitulo]] <- filtro$duplicados_despues

    seleccion <- preparar_columnas_autorizadas(
      filtro$data,
      capitulo,
      variables_permitidas,
      variables_no_autorizadas,
      llaves_autorizacion = llaves_autorizacion
    )
    bases_exportables[[capitulo]] <- seleccion$data
    permitidas_presentes[[capitulo]] <-
      seleccion$permitidas_presentes
    permitidas_faltantes[[capitulo]] <-
      seleccion$permitidas_faltantes
    variables_retiradas[[capitulo]] <-
      seleccion$variables_retiradas

    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_SIN_INCREMENTO"),
      capitulo = capitulo,
      descripcion = "El semi_join no incrementa filas.",
      observado = paste0(filtro$n_antes, " -> ", filtro$n_despues),
      esperado = "filas_despues <= filas_antes",
      n_problematicos = as.integer(filtro$incremento_filas),
      pasa = !filtro$incremento_filas
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_FUERA_UNIVERSO"),
      capitulo = capitulo,
      descripcion = "Sin registros fuera del universo completo.",
      observado = nrow(filtro$fuera_universo),
      esperado = "0",
      n_problematicos = nrow(filtro$fuera_universo),
      pasa = nrow(filtro$fuera_universo) == 0L
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_DUPLICADOS_ANTES"),
      capitulo = capitulo,
      descripcion = "Sin llaves duplicadas antes del filtro.",
      observado = nrow(filtro$duplicados_antes),
      esperado = "0",
      n_problematicos = nrow(filtro$duplicados_antes),
      pasa = nrow(filtro$duplicados_antes) == 0L
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_DUPLICADOS"),
      capitulo = capitulo,
      descripcion = "Sin llaves duplicadas en la base filtrada.",
      observado = nrow(duplicados[[capitulo]]),
      esperado = "0",
      n_problematicos = nrow(duplicados[[capitulo]]),
      pasa = nrow(duplicados[[capitulo]]) == 0L
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_LLAVES_FALTANTES_ANTES"),
      capitulo = capitulo,
      descripcion = "Las llaves de validacion no tienen NA ni vacios.",
      observado = filtro$n_llaves_faltantes,
      esperado = "0",
      n_problematicos = filtro$n_llaves_faltantes,
      pasa = filtro$n_llaves_faltantes == 0L
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_PERMITIDAS_PRESENTES"),
      capitulo = capitulo,
      descripcion = "Todas las variables autorizadas estan presentes.",
      observado = nrow(seleccion$permitidas_faltantes),
      esperado = "0",
      n_problematicos = nrow(seleccion$permitidas_faltantes),
      pasa = nrow(seleccion$permitidas_faltantes) == 0L
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_LLAVES_AUTORIZADAS"),
      capitulo = capitulo,
      descripcion = "Las llaves requeridas estan autorizadas.",
      observado = seleccion$llaves_autorizadas,
      esperado = "TRUE",
      n_problematicos = as.integer(!seleccion$llaves_autorizadas),
      pasa = seleccion$llaves_autorizadas
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_COLUMNAS_EXACTAS"),
      capitulo = capitulo,
      descripcion = paste(
        "Las columnas seleccionadas coinciden exactamente con la lista blanca."
      ),
      observado = seleccion$columnas_exactas,
      esperado = "TRUE",
      n_problematicos = as.integer(!seleccion$columnas_exactas),
      pasa = seleccion$columnas_exactas
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_ORDEN_COLUMNAS"),
      capitulo = capitulo,
      descripcion = "El orden de columnas sigue el diccionario anonimizado.",
      observado = seleccion$orden_exacto,
      esperado = "TRUE",
      n_problematicos = as.integer(!seleccion$orden_exacto),
      pasa = seleccion$orden_exacto
    ))
    agregar_control(nuevo_control(
      paste0("CAP_", capitulo, "_LLAVES_NO_FALTANTES"),
      capitulo = capitulo,
      descripcion = "Las llaves exportables no tienen faltantes.",
      observado = llaves_sin_faltantes(
        filtro$data_validacion_despues,
        llaves
      ),
      esperado = "TRUE",
      n_problematicos = as.integer(
        !llaves_sin_faltantes(
          filtro$data_validacion_despues,
          llaves
        )
      ),
      pasa = llaves_sin_faltantes(
        filtro$data_validacion_despues,
        llaves
      )
    ))

    resumen_capitulos[[capitulo]] <- tibble::tibble(
      capitulo = capitulo,
      nivel = funcion_paquete("tipo_capitulo")[[capitulo]],
      llaves = paste(llaves, collapse = " + "),
      filas_originales_osis = filtro$n_antes,
      filas_completas_osis = filtro$n_despues,
      filas_retiradas_fuera_universo = filtro$n_retirados,
      incremento_filas = filtro$incremento_filas,
      duplicados_antes = nrow(filtro$duplicados_antes),
      duplicados_despues = nrow(filtro$duplicados_despues),
      llaves_faltantes_antes = filtro$n_llaves_faltantes,
      variables_permitidas = sum(
        !is.na(variables_permitidas$capitulo) &
          variables_permitidas$capitulo == capitulo
      ),
      variables_exportables = if (is.null(seleccion$data)) {
        0L
      } else {
        ncol(seleccion$data)
      },
      variables_faltantes = nrow(seleccion$permitidas_faltantes),
      variables_retiradas = nrow(seleccion$variables_retiradas)
    )
  }

  bases_exportables_validas <- bases_exportables[
    !vapply(bases_exportables, is.null, logical(1))
  ]
  cobertura_salida <- resumir_cobertura_capitulos(
    origen = "SALIDA_PREEXPORTACION",
    encontrados = names(bases_exportables_validas),
    esperados = capitulos_esperados_entrega,
    permitidos = capitulos_esperados_entrega
  )
  coberturas_capitulos <- dplyr::bind_rows(
    coberturas_capitulos,
    cobertura_salida
  )
  agregar_control(nuevo_control(
    "SALIDA_02_CAPITULOS_ESPERADOS",
    descripcion = paste(
      "Las bases en memoria contienen exactamente los capitulos esperados",
      "antes de iniciar staging."
    ),
    observado = paste(
      "faltantes=", cobertura_salida$capitulos_faltantes[[1]],
      "; inesperados=", cobertura_salida$capitulos_inesperados[[1]]
    ),
    esperado = "faltantes= ; inesperados= ",
    n_problematicos = cobertura_salida$n_faltantes[[1]] +
      cobertura_salida$n_inesperados[[1]],
    pasa = setequal(
      names(bases_exportables_validas),
      capitulos_esperados_entrega
    )
  ))
  agregar_control(nuevo_control(
    "SALIDA_03_CANTIDAD_CAPITULOS",
    descripcion = paste(
      "La cantidad de bases en memoria coincide con la cantidad esperada."
    ),
    observado = length(bases_exportables_validas),
    esperado = length(capitulos_esperados_entrega),
    n_problematicos = abs(
      length(bases_exportables_validas) -
        length(capitulos_esperados_entrega)
    ),
    pasa = length(bases_exportables_validas) ==
      length(capitulos_esperados_entrega)
  ))

  variables_k <- variables_permitidas[
    !is.na(variables_permitidas$capitulo) &
      variables_permitidas$capitulo == capitulo_excluido,
    ,
    drop = FALSE
  ] |>
    dplyr::mutate(
      clasificacion = "no_exportada_por_capitulo_k"
    )

  caps_comparables <- intersect(
    names(bases_filtradas),
    names(bases_cap_completas)
  )
  faltan_caps_cap <- setdiff(
    names(bases_filtradas),
    names(bases_cap_completas)
  )
  agregar_control(nuevo_control(
    "CAPREF_01_CAPITULOS",
    descripcion = paste(
      "La CAP oficial contiene todos los capitulos exportables."
    ),
    observado = paste(faltan_caps_cap, collapse = ", "),
    esperado = "Ninguno",
    n_problematicos = length(faltan_caps_cap),
    pasa = length(faltan_caps_cap) == 0L
  ))

  comparaciones <- purrr::map(caps_comparables, function(capitulo) {
    regla_comparacion <- if (
      capitulo %in% c("A", "C", "E")
    ) {
      "exacta_estructural"
    } else {
      "subconjunto_tematico"
    }
    comparar_universos_capitulo(
      dfs_osis[[capitulo]],
      bases_filtradas[[capitulo]],
      dfs_cap_oficial[[capitulo]],
      bases_cap_completas[[capitulo]],
      capitulo,
      universo_completo = universo_para_capitulo(
        capitulo,
        universos_cap
      ),
      regla_comparacion = regla_comparacion,
      df_osis_validacion = bases_validacion_antes[[capitulo]],
      df_osis_completo_validacion =
        bases_validacion_filtradas[[capitulo]],
      llaves_validacion = llaves_validacion_capitulo[[capitulo]]
    )
  })
  names(comparaciones) <- caps_comparables

  comparacion_resumen <- purrr::map_dfr(comparaciones, "resumen")
  diferencias_llaves <- purrr::map_dfr(comparaciones, "diferencias")
  duplicados_comparacion <- purrr::map_dfr(
    comparaciones,
    "duplicados"
  )

  purrr::pwalk(
    comparacion_resumen,
    function(capitulo,
             regla_comparacion,
             cumple_regla_comparacion,
             llaves_completas_osis_ausentes_cap,
             llaves_cap_ausentes_osis_completo,
             llaves_osis_fuera_universo_completo,
             llaves_universo_completo_sin_osis,
             duplicados_osis,
             duplicados_cap,
             ...) {
      estructural <- identical(
        regla_comparacion,
        "exacta_estructural"
      )
      n_problemas <- if (estructural) {
        llaves_completas_osis_ausentes_cap +
          llaves_cap_ausentes_osis_completo
      } else {
        llaves_osis_fuera_universo_completo
      }
      agregar_control(nuevo_control(
        paste0("CAPREF_", capitulo, "_REGLA_COMPARACION"),
        capitulo = capitulo,
        descripcion = if (estructural) {
          paste(
            "A/C/E exigen coincidencia exacta del universo completo",
            "entre OSIS y CAP."
          )
        } else {
          paste(
            "El capitulo tematico debe ser subconjunto del universo",
            "completo; sus ausencias tematicas no bloquean."
          )
        },
        observado = paste(
          regla_comparacion,
          cumple_regla_comparacion
        ),
        esperado = "TRUE",
        n_problematicos = n_problemas,
        pasa = cumple_regla_comparacion
      ))
      agregar_control(nuevo_control(
        paste0("OSIS_", capitulo, "_FUERA_UNIVERSO_COMPLETO"),
        capitulo = capitulo,
        descripcion = paste(
          "Las llaves OSIS filtradas pertenecen al universo completo",
          "del nivel correspondiente."
        ),
        observado = llaves_osis_fuera_universo_completo,
        esperado = "0",
        n_problematicos = llaves_osis_fuera_universo_completo,
        pasa = llaves_osis_fuera_universo_completo == 0L
      ))
      agregar_control(nuevo_control(
        paste0("UNIVERSO_", capitulo, "_SIN_REGISTRO_OSIS"),
        capitulo = capitulo,
        descripcion = paste(
          "Llaves completas sin registro OSIS en el capitulo; en capitulos",
          "tematicos pueden corresponder al flujo particular."
        ),
        observado = llaves_universo_completo_sin_osis,
        esperado = "0",
        n_problematicos = llaves_universo_completo_sin_osis,
        pasa = llaves_universo_completo_sin_osis == 0L,
        criticidad = if (estructural) "CRITICO" else "INFORMATIVO"
      ))
      agregar_control(nuevo_control(
        paste0("OSIS_", capitulo, "_DUPLICADOS_TOTAL"),
        capitulo = capitulo,
        descripcion = "La fuente OSIS no tiene llaves duplicadas.",
        observado = duplicados_osis,
        esperado = "0",
        n_problematicos = duplicados_osis,
        pasa = duplicados_osis == 0L
      ))
      agregar_control(nuevo_control(
        paste0("CAPREF_", capitulo, "_DUPLICADOS"),
        capitulo = capitulo,
        descripcion = "La referencia CAP no tiene llaves duplicadas.",
        observado = duplicados_cap,
        esperado = "0",
        n_problematicos = duplicados_cap,
        pasa = duplicados_cap == 0L,
        criticidad = if (estructural) "CRITICO" else "INFORMATIVO"
      ))
    }
  )

  marcas <- if (
    all(c("A", "C", "E") %in% names(dfs_cap_oficial))
  ) {
    auditar_marcas_a_c_e(
      dfs_osis,
      dfs_cap_oficial,
      config$variable_completitud
    )
  } else {
    list(
      resumen = tibble::tibble(),
      diferencias = tibble::tibble()
    )
  }

  if (nrow(marcas$resumen) > 0L) {
    for (capitulo in c("A", "C", "E")) {
      resumen_marca_cap <- marcas$resumen[
        !is.na(marcas$resumen$capitulo) &
          marcas$resumen$capitulo == capitulo,
        ,
        drop = FALSE
      ]
      n_fuera_dominio <- sum(
        resumen_marca_cap$n_distintos_0_1,
        na.rm = TRUE
      )
      n_diferencias_marca <- sum(
        !is.na(marcas$diferencias$capitulo) &
          marcas$diferencias$capitulo == capitulo
      )

      agregar_control(nuevo_control(
        paste0("MARCA_", capitulo, "_DOMINIO"),
        capitulo = capitulo,
        descripcion = paste(
          "ENCUESTA_COMPLETA solo contiene valores binarios inequivocos."
        ),
        observado = n_fuera_dominio,
        esperado = "0",
        n_problematicos = n_fuera_dominio,
        pasa = n_fuera_dominio == 0L
      ))
      agregar_control(nuevo_control(
        paste0("MARCA_", capitulo, "_COINCIDENCIA_CAP"),
        capitulo = capitulo,
        descripcion = paste(
          "Las llaves comunes tienen la misma marca en OSIS y CAP."
        ),
        observado = n_diferencias_marca,
        esperado = "0",
        n_problematicos = n_diferencias_marca,
        pasa = n_diferencias_marca == 0L
      ))
    }
  }

  comparacion_cap_derivada <-
    comparar_cap_oficial_con_salida_derivada(
      bases_cap_completas,
      dfs_cap_sin_tematica
    )

  if (nrow(comparacion_cap_derivada$resumen) > 0L) {
    purrr::pwalk(
      comparacion_cap_derivada$resumen,
      function(capitulo,
               n_oficial_no_derivada,
               n_derivada_no_oficial,
               coincide,
               ...) {
        agregar_control(nuevo_control(
          paste0("CAP_DERIVADA_", capitulo),
          capitulo = capitulo,
          descripcion = paste(
            "Control secundario CAP oficial completa vs salida sin tematica."
          ),
          observado = coincide,
          esperado = "TRUE",
          n_problematicos = n_oficial_no_derivada +
            n_derivada_no_oficial,
          pasa = coincide,
          criticidad = "INFORMATIVO"
        ))
      }
    )
  }

  validacion_sistemas <- NULL
  if (
    !is.null(dfs_cap_sin_tematica) &&
      all(c("A", "C", "E") %in% names(dfs_cap_oficial)) &&
      all(c("A", "C", "E") %in% names(dfs_cap_sin_tematica)) &&
      exists(
        "validar_encuesta_completa_sistemas",
        envir = asNamespace("analisisem2025"),
        inherits = FALSE
      )
  ) {
    validacion_sistemas <- funcion_paquete(
      "validar_encuesta_completa_sistemas"
    )(
      dfs = dfs_cap_oficial,
      dfs_completa_sin_tematica = dfs_cap_sin_tematica,
      variable_sistemas = config$variable_completitud,
      exportar = FALSE
    )

    agregar_control(nuevo_control(
      "MARCA_CONTROL_PAQUETE",
      descripcion = paste(
        "Control maestro de ENCUESTA_COMPLETA frente a la salida sin tematica."
      ),
      observado = all(validacion_sistemas$resumen$coincide),
      esperado = "TRUE",
      n_problematicos = sum(
        !validacion_sistemas$resumen$coincide
      ),
      pasa = all(validacion_sistemas$resumen$coincide),
      criticidad = "INFORMATIVO",
      observacion = paste(
        "Control secundario opcional del flujo maestro. La aprobacion principal",
        "usa ENCUESTA_COMPLETA == 1 en la CAP oficial y en OSIS."
      )
    ))
  }

  permitidas_faltantes_tabla <- dplyr::bind_rows(
    permitidas_faltantes
  )
  if (nrow(permitidas_faltantes_tabla) == 0L) {
    permitidas_faltantes_tabla <- tibble::tibble(
      capitulo = character(),
      variable = character(),
      clasificacion_faltante = character()
    )
  } else {
    capitulos_archivo_equivocado <-
      auditoria_archivos_equivocados |>
      dplyr::filter(.data$criticidad == "CRITICO") |>
      dplyr::pull("capitulo")
    permitidas_faltantes_tabla <-
      permitidas_faltantes_tabla |>
      dplyr::mutate(
        clasificacion_faltante = dplyr::case_when(
          .data$variable %in% c(
            "DIRECTORIO",
            "SECUENCIA_P",
            "ORDEN"
          ) ~ "llave_estructural_ausente",
          .data$capitulo %in% capitulos_archivo_equivocado ~
            "posible_archivo_de_capitulo_equivocado",
          TRUE ~ "variable_autorizada_ausente_en_OSIS"
        ),
        diagnostico_fuente_diccionario = dplyr::if_else(
          .data$clasificacion_faltante ==
            "variable_autorizada_ausente_en_OSIS",
          "discrepancia_entre_la_fuente_y_el_diccionario",
          .data$clasificacion_faltante
        )
      )
  }
  resumen_faltantes_autorizadas <- if (
    nrow(permitidas_faltantes_tabla) == 0L
  ) {
    tibble::tibble(
      capitulo = character(),
      clasificacion_faltante = character(),
      n_variables = integer()
    )
  } else {
    permitidas_faltantes_tabla |>
      dplyr::count(
        .data$capitulo,
        .data$clasificacion_faltante,
        name = "n_variables"
      )
  }

  resumen_capitulos <- dplyr::bind_rows(resumen_capitulos) |>
    dplyr::left_join(
      comparacion_resumen |>
        dplyr::select(
          "capitulo",
          filas_completas_cap = "filas_completas_cap",
          regla_comparacion = "regla_comparacion",
          coincidencia_universos = "cumple_regla_comparacion",
          resultado = "estado_comparacion"
        ),
      by = "capitulo"
    )

  list(
    universos = universos,
    universos_cap = universos_cap,
    bases_cap_completas = bases_cap_completas,
    bases_filtradas = bases_filtradas,
    bases_exportables = bases_exportables_validas,
    capitulos_esperados_entrega = capitulos_esperados_entrega,
    cobertura_capitulos = coberturas_capitulos,
    resumen_capitulos = resumen_capitulos,
    comparacion_resumen = comparacion_resumen,
    reglas_comparacion = comparacion_resumen |>
      dplyr::select(
        "capitulo",
        "nivel",
        "regla_comparacion",
        "llaves",
        "llaves_osis_fuera_universo_completo",
        "llaves_universo_completo_sin_osis",
        "cumple_regla_comparacion",
        "estado_comparacion"
      ),
    diferencias_llaves = dplyr::bind_rows(
      diferencias_llaves,
      marcas$diferencias |>
        dplyr::mutate(
          tipo_diferencia = "marca_encuesta_completa_distinta"
        ),
      comparacion_cap_derivada$diferencias
    ),
    duplicados_llaves = dplyr::bind_rows(
      dplyr::bind_rows(duplicados),
      duplicados_comparacion
    ),
    fuera_universo = dplyr::bind_rows(
      fuera_universo,
      .id = "capitulo"
    ),
    llaves_osis_fuera_universo_completo = dplyr::bind_rows(
      fuera_universo,
      .id = "capitulo"
    ),
    llaves_completas_sin_registro_osis = diferencias_llaves |>
      dplyr::filter(
        .data$tipo_diferencia ==
          "llave_universo_completo_sin_registro_osis"
      ),
    permitidas_presentes = dplyr::bind_rows(permitidas_presentes),
    permitidas_faltantes = permitidas_faltantes_tabla,
    resumen_faltantes_autorizadas = resumen_faltantes_autorizadas,
    variables_retiradas = dplyr::bind_rows(
      dplyr::bind_rows(variables_retiradas),
      variables_k
    ),
    marcas_a_c_e = marcas,
    comparacion_cap_derivada = comparacion_cap_derivada,
    validacion_sistemas = validacion_sistemas,
    auditoria_normalizacion_llaves =
      auditoria_normalizacion_llaves,
    auditoria_archivos_equivocados =
      auditoria_archivos_equivocados,
    auditoria_equivalencia_b = if (is.null(equivalencia_b)) {
      tibble::tibble()
    } else {
      equivalencia_b$auditoria
    },
    problemas_lectura = problemas_lectura,
    controles = dplyr::bind_rows(controles)
  )
}

crear_carpeta_resultados_no_destructiva <- function(ruta_base) {
  ruta <- ruta_base
  if (dir.exists(ruta) || file.exists(ruta)) {
    sufijo <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ruta <- paste0(ruta_base, "_", sufijo)
  }
  if (dir.exists(ruta) || file.exists(ruta)) {
    i <- 1L
    repeat {
      candidata <- paste0(ruta, "_", i)
      if (!dir.exists(candidata) && !file.exists(candidata)) {
        ruta <- candidata
        break
      }
      i <- i + 1L
    }
  }
  if (!dir.create(ruta, recursive = TRUE, showWarnings = FALSE)) {
    stop("No fue posible crear la carpeta de resultados: ", ruta)
  }
  normalizePath(ruta, winslash = "/", mustWork = TRUE)
}

es_subruta_segura <- function(ruta, raiz) {
  ruta_norm <- normalizePath(ruta, winslash = "/", mustWork = FALSE)
  raiz_norm <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  startsWith(
    paste0(ruta_norm, "/"),
    paste0(raiz_norm, "/")
  )
}

eliminar_staging_seguro <- function(ruta_staging, carpeta_resultados) {
  if (
    dir.exists(ruta_staging) &&
      startsWith(basename(ruta_staging), ".staging_entrega_cliente_") &&
      es_subruta_segura(ruta_staging, carpeta_resultados)
  ) {
    unlink(ruta_staging, recursive = TRUE, force = TRUE)
  }
  invisible(!dir.exists(ruta_staging))
}

eliminar_cliente_parcial_seguro <- function(ruta_cliente,
                                            carpeta_resultados) {
  if (
    dir.exists(ruta_cliente) &&
      identical(basename(ruta_cliente), "entrega_cliente") &&
      es_subruta_segura(ruta_cliente, carpeta_resultados)
  ) {
    unlink(ruta_cliente, recursive = TRUE, force = TRUE)
  }
  invisible(!dir.exists(ruta_cliente))
}

normalizar_valor_comparacion_csv <- function(x) {
  valor <- as.character(x)
  valor[is.na(valor) | valor == ""] <- "__FALTANTE_DOCUMENTADO__"
  valor
}

comparar_muestra_valores_csv <- function(original,
                                         releida,
                                         capitulo,
                                         max_filas = 25L,
                                         max_columnas = 8L) {
  estructura_comparable <-
    nrow(original) == nrow(releida) &&
    ncol(original) == ncol(releida) &&
    identical(names(original), names(releida))

  if (!estructura_comparable || nrow(original) == 0L ||
      ncol(original) == 0L) {
    return(list(
      resumen = tibble::tibble(
        capitulo = capitulo,
        celdas_comparadas = 0L,
        cambios_detectados = if (estructura_comparable) 0L else NA_integer_,
        valores_preservados = estructura_comparable
      ),
      diferencias = tibble::tibble()
    ))
  }

  seleccionar_posiciones <- function(n, maximo) {
    unique(as.integer(round(seq(
      from = 1L,
      to = n,
      length.out = min(n, maximo)
    ))))
  }

  filas <- seleccionar_posiciones(nrow(original), max_filas)
  columnas <- seleccionar_posiciones(ncol(original), max_columnas)
  posiciones <- expand.grid(
    fila = filas,
    columna_indice = columnas,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  posiciones$columna <- names(original)[posiciones$columna_indice]
  posiciones$valor_antes <- mapply(
    function(fila, columna_indice) {
      normalizar_valor_comparacion_csv(
        original[[columna_indice]][[fila]]
      )
    },
    posiciones$fila,
    posiciones$columna_indice,
    USE.NAMES = FALSE
  )
  posiciones$valor_despues <- mapply(
    function(fila, columna_indice) {
      normalizar_valor_comparacion_csv(
        releida[[columna_indice]][[fila]]
      )
    },
    posiciones$fila,
    posiciones$columna_indice,
    USE.NAMES = FALSE
  )

  diferencias <- posiciones |>
    tibble::as_tibble() |>
    dplyr::filter(.data$valor_antes != .data$valor_despues) |>
    dplyr::transmute(
      capitulo = capitulo,
      fila = .data$fila,
      columna = .data$columna,
      valor_antes = .data$valor_antes,
      valor_despues = .data$valor_despues
    )

  list(
    resumen = tibble::tibble(
      capitulo = capitulo,
      celdas_comparadas = nrow(posiciones),
      cambios_detectados = nrow(diferencias),
      valores_preservados = nrow(diferencias) == 0L
    ),
    diferencias = diferencias
  )
}

exportar_y_releer_csv <- function(bases_exportables,
                                  carpeta_staging,
                                  config) {
  if (!dir.create(
    carpeta_staging,
    recursive = FALSE,
    showWarnings = FALSE
  )) {
    stop(
      "No fue posible crear la carpeta temporal de exportacion: ",
      carpeta_staging,
      ".",
      call. = FALSE
    )
  }

  manifest <- list()
  controles <- list()
  conservacion_valores <- list()

  for (capitulo in names(bases_exportables)) {
    base_exportada <- bases_exportables[[capitulo]]
    nombre <- paste0("VW_EMP_CAP_", capitulo, ".csv")
    ruta <- file.path(carpeta_staging, nombre)

    readr::write_excel_csv2(
      base_exportada,
      ruta,
      na = config$representacion_na
    )

    relectura <- tryCatch(
      readr::read_csv2(
        ruta,
        col_types = readr::cols(.default = readr::col_character()),
        na = character(),
        trim_ws = FALSE,
        show_col_types = FALSE,
        progress = FALSE,
        name_repair = "minimal"
      ),
      error = identity
    )

    relectura_ok <- !inherits(relectura, "error")
    filas_ok <- relectura_ok &&
      nrow(relectura) == nrow(base_exportada)
    columnas_ok <- relectura_ok &&
      ncol(relectura) == ncol(base_exportada)
    nombres_ok <- relectura_ok &&
      identical(names(relectura), names(base_exportada))
    comparacion_valores <- if (relectura_ok) {
      comparar_muestra_valores_csv(
        base_exportada,
        relectura,
        capitulo
      )
    } else {
      list(
        resumen = tibble::tibble(
          capitulo = capitulo,
          celdas_comparadas = 0L,
          cambios_detectados = NA_integer_,
          valores_preservados = FALSE
        ),
        diferencias = tibble::tibble()
      )
    }
    conservacion_valores[[capitulo]] <-
      comparacion_valores$diferencias
    valores_ok <- isTRUE(
      comparacion_valores$resumen$valores_preservados[[1]]
    )

    llaves <- if (
      identical(capitulo, "B") &&
        !"SECUENCIA_P" %in% names(base_exportada) &&
        "ORDEN" %in% names(base_exportada)
    ) {
      c("DIRECTORIO", "ORDEN")
    } else {
      funcion_paquete("get_join_keys")(capitulo)
    }
    duplicados <- duplicados_por_llave(
      base_exportada,
      llaves,
      capitulo,
      "CSV_EXPORTADO"
    )

    manifest[[capitulo]] <- tibble::tibble(
      nombre = nombre,
      capitulo = capitulo,
      filas = nrow(base_exportada),
      columnas = ncol(base_exportada),
      llaves_esperadas = paste(llaves, collapse = " + "),
      n_llaves_unicas = nrow(
        base_exportada |>
          dplyr::select(dplyr::all_of(llaves)) |>
          dplyr::distinct()
      ),
      tamano_bytes = as.numeric(file.info(ruta)$size),
      fecha_creacion = format(
        file.info(ruta)$mtime,
        "%Y-%m-%d %H:%M:%S"
      ),
      md5 = unname(tools::md5sum(ruta)),
      estado_relectura = if (relectura_ok) "PASS" else "FAIL",
      celdas_muestra_valores =
        comparacion_valores$resumen$celdas_comparadas[[1]],
      n_cambios_valores =
        comparacion_valores$resumen$cambios_detectados[[1]],
      estado_validacion = if (
        relectura_ok &&
          filas_ok &&
          columnas_ok &&
          nombres_ok &&
          valores_ok &&
          nrow(duplicados) == 0L
      ) {
        "PASS"
      } else {
        "FAIL"
      }
    )

    controles[[length(controles) + 1L]] <- dplyr::bind_rows(
      nuevo_control(
        paste0("CSV_", capitulo, "_RELECTURA"),
        capitulo = capitulo,
        descripcion = "El CSV exportado puede releerse.",
        observado = if (relectura_ok) "Relectura correcta" else {
          conditionMessage(relectura)
        },
        esperado = "Relectura correcta",
        n_problematicos = as.integer(!relectura_ok),
        pasa = relectura_ok
      ),
      nuevo_control(
        paste0("CSV_", capitulo, "_ESTRUCTURA"),
        capitulo = capitulo,
        descripcion = paste(
          "Filas, columnas y nombres se preservan despues de releer."
        ),
        observado = paste(filas_ok, columnas_ok, nombres_ok, sep = " / "),
        esperado = "TRUE / TRUE / TRUE",
        n_problematicos = sum(!c(filas_ok, columnas_ok, nombres_ok)),
        pasa = filas_ok && columnas_ok && nombres_ok
      ),
      nuevo_control(
        paste0("CSV_", capitulo, "_VALORES"),
        capitulo = capitulo,
        descripcion = paste(
          "Una muestra deterministica de celdas conserva su texto al releer;",
          "solo NA y vacio comparten la representacion faltante documentada."
        ),
        observado = if (is.na(
          comparacion_valores$resumen$cambios_detectados[[1]]
        )) {
          "No comparable"
        } else {
          comparacion_valores$resumen$cambios_detectados[[1]]
        },
        esperado = "0",
        n_problematicos = if (is.na(
          comparacion_valores$resumen$cambios_detectados[[1]]
        )) {
          1L
        } else {
          comparacion_valores$resumen$cambios_detectados[[1]]
        },
        pasa = valores_ok,
        observacion = paste(
          "La cadena literal 'NA' se conserva como valor y no se interpreta",
          "como faltante durante la relectura."
        )
      ),
      nuevo_control(
        paste0("CSV_", capitulo, "_DUPLICADOS"),
        capitulo = capitulo,
        descripcion = "El CSV final no tiene llaves duplicadas.",
        observado = nrow(duplicados),
        esperado = "0",
        n_problematicos = nrow(duplicados),
        pasa = nrow(duplicados) == 0L
      )
    )
  }

  list(
    manifest = dplyr::bind_rows(manifest),
    controles = dplyr::bind_rows(controles),
    conservacion_valores = dplyr::bind_rows(conservacion_valores)
  )
}

tabla_excel_segura <- function(x) {
  if (is.null(x)) {
    return(tibble::tibble(nota = "Sin informacion disponible."))
  }
  x <- tibble::as_tibble(x)
  if (ncol(x) == 0L) {
    return(tibble::tibble(nota = "Sin columnas disponibles."))
  }
  if (nrow(x) == 0L) {
    plantilla <- lapply(x, function(columna) {
      if (is.integer(columna)) {
        NA_integer_
      } else if (is.numeric(columna)) {
        NA_real_
      } else if (is.logical(columna)) {
        NA
      } else {
        NA_character_
      }
    })
    x <- tibble::as_tibble(plantilla)
    x$nota_sin_casos <- "Sin casos."
  }
  x
}

escribir_excel_auditoria <- function(ruta, hojas) {
  wb <- openxlsx::createWorkbook()
  purrr::iwalk(hojas, function(tabla, nombre_hoja) {
    nombre_hoja <- substr(nombre_hoja, 1L, 31L)
    openxlsx::addWorksheet(wb, nombre_hoja)
    openxlsx::writeData(
      wb,
      nombre_hoja,
      tabla_excel_segura(tabla)
    )
    openxlsx::freezePane(wb, nombre_hoja, firstRow = TRUE)
    openxlsx::setColWidths(
      wb,
      nombre_hoja,
      cols = seq_len(ncol(tabla_excel_segura(tabla))),
      widths = "auto"
    )
  })
  openxlsx::saveWorkbook(wb, ruta, overwrite = TRUE)
  invisible(ruta)
}

obtener_commit_paquete <- function(ruta_paquete) {
  salida <- tryCatch(
    system2(
      "git",
      c("-C", shQuote(ruta_paquete), "rev-parse", "--short", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character()
  )
  if (length(salida) == 0L) NA_character_ else salida[[1]]
}

estado_final_controles <- function(controles) {
  criticos <- controles |>
    dplyr::filter(.data$criticidad == "CRITICO")
  if (nrow(criticos) > 0L && all(criticos$estado == "PASS")) {
    "APROBADO_PARA_ENTREGA"
  } else {
    "NO_APROBADO_PARA_ENTREGA"
  }
}

guardar_auditoria_entrega <- function(
    carpeta_auditoria,
    config,
    estado_final,
    inventario_osis = tibble::tibble(),
    inventario_cap = tibble::tibble(),
    inventario_diccionarios = tibble::tibble(),
    auditoria_diccionarios = NULL,
    resultado = NULL,
    manifest = tibble::tibble(),
    controles = tibble::tibble(),
    log = character(),
    capitulos_exportados = character()
) {
  fecha_corte <- config$fecha_corte
  auditoria_diccionarios <- auditoria_diccionarios %||% list()
  resultado <- resultado %||% list()
  capitulos_esperados_entrega <-
    resultado$capitulos_esperados_entrega %||% character()
  cobertura_exportacion <- resumir_cobertura_capitulos(
    origen = "RESUMEN_EXPORTACION",
    encontrados = capitulos_exportados,
    esperados = capitulos_esperados_entrega,
    permitidos = capitulos_esperados_entrega
  )

  resumen_ejecucion <- tibble::tibble(
    campo = c(
      "fecha_hora",
      "fecha_corte",
      "ruta_paquete",
      "commit_paquete",
      "diccionario_completo",
      "diccionario_anonimizado",
      "carpeta_osis",
      "carpeta_cap_oficial",
      "carpeta_cap_sin_tematica_opcional",
      "capitulos_detectados_osis",
      "capitulos_esperados_entrega",
      "capitulos_exportados",
      "capitulos_faltantes_exportacion",
      "capitulos_inesperados_exportacion",
      "exclusion_capitulo_k",
      "cantidad_capitulos_esperada",
      "cantidad_capitulos_exportada",
      "n_csv_generados",
      "estado_final",
      "controles_pass",
      "controles_fail",
      "universos_osis_vs_cap",
      "columnas_vs_lista_blanca"
    ),
    valor = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      fecha_corte,
      config$ruta_paquete,
      obtener_commit_paquete(config$ruta_paquete),
      config$diccionario_completo,
      config$diccionario_anonimizado,
      config$carpeta_osis,
      config$carpeta_cap_oficial,
      config$carpeta_cap_sin_tematica_opcional,
      paste(inventario_osis$capitulo %||% character(), collapse = ", "),
      paste(capitulos_esperados_entrega, collapse = ", "),
      paste(capitulos_exportados, collapse = ", "),
      cobertura_exportacion$capitulos_faltantes[[1]],
      cobertura_exportacion$capitulos_inesperados[[1]],
      "K excluido temporalmente de toda salida de cliente",
      length(capitulos_esperados_entrega),
      length(capitulos_exportados),
      nrow(manifest),
      estado_final,
      sum(controles$estado == "PASS", na.rm = TRUE),
      sum(controles$estado == "FAIL", na.rm = TRUE),
      if (
        !is.null(resultado$comparacion_resumen) &&
          nrow(resultado$comparacion_resumen) > 0L &&
          all(
            resultado$comparacion_resumen$
              cumple_regla_comparacion
          )
      ) {
        "Reglas estructurales y tematicas satisfechas"
      } else {
        "No demostrada o con diferencias"
      },
      if (
        nrow(controles) > 0L &&
          all(
            controles$estado[
              stringr::str_detect(
                controles$control_id,
                "COLUMNAS_EXACTAS|PERMITIDAS_PRESENTES"
              )
            ] == "PASS"
          )
      ) {
        "Coincidencia exacta"
      } else {
        "No demostrada o con diferencias"
      }
    )
  )

  universos_osis_auditoria <-
    resultado$universos$resumen %||% tibble::tibble()
  universos_cap_auditoria <-
    resultado$universos_cap$resumen %||% tibble::tibble()
  if (ncol(universos_osis_auditoria) > 0L) {
    universos_osis_auditoria <- universos_osis_auditoria |>
      dplyr::mutate(fuente = "OSIS", .before = 1L)
  }
  if (ncol(universos_cap_auditoria) > 0L) {
    universos_cap_auditoria <- universos_cap_auditoria |>
      dplyr::mutate(fuente = "CAP_OFICIAL", .before = 1L)
  }

  hojas <- list(
    Resumen_ejecucion = resumen_ejecucion,
    Inventario_OSIS = inventario_osis,
    Inventario_CAP = inventario_cap,
    Inventario_diccionarios = inventario_diccionarios,
    Capitulos_comparacion = resultado$resumen_capitulos,
    Universos_A_C_E = dplyr::bind_rows(
      universos_osis_auditoria,
      universos_cap_auditoria
    ),
    Comparacion_universos = resultado$comparacion_resumen,
    Reglas_comparacion = resultado$reglas_comparacion,
    OSIS_fuera_universo =
      resultado$llaves_osis_fuera_universo_completo,
    Universo_sin_OSIS =
      resultado$llaves_completas_sin_registro_osis,
    Diferencias_llaves = resultado$diferencias_llaves,
    Marcas_A_C_E = resultado$marcas_a_c_e$resumen,
    Comparacion_CAP_derivada =
      resultado$comparacion_cap_derivada$resumen,
    Cobertura_capitulos = resultado$cobertura_capitulos,
    Resumen_diccionarios = auditoria_diccionarios$resumen,
    Permitidas_presentes = resultado$permitidas_presentes,
    Permitidas_faltantes = resultado$permitidas_faltantes,
    Resumen_faltantes = resultado$resumen_faltantes_autorizadas,
    Variables_retiradas = resultado$variables_retiradas,
    Inconsistencias_dicc = dplyr::bind_rows(
      auditoria_diccionarios$
        variables_anonimizadas_no_en_diccionario_completo,
      auditoria_diccionarios$vacios,
      auditoria_diccionarios$duplicados,
      auditoria_diccionarios$variables_multicapitulo
    ),
    Duplicados_llaves = resultado$duplicados_llaves,
    Normalizacion_llaves = resultado$auditoria_normalizacion_llaves,
    Equivalencia_llave_B = resultado$auditoria_equivalencia_b,
    Archivos_equivocados = resultado$auditoria_archivos_equivocados,
    Problemas_parsing = resultado$problemas_lectura,
    Conservacion_valores = resultado$conservacion_valores,
    Controles = controles,
    Manifest_salida = manifest,
    Sesion_R = tibble::tibble(
      linea = capture.output(utils::sessionInfo())
    )
  )

  ruta_excel <- file.path(
    carpeta_auditoria,
    paste0(
      "diagnostico_entrega_preliminar_",
      fecha_corte,
      ".xlsx"
    )
  )
  escribir_excel_auditoria(ruta_excel, hojas)

  readr::write_excel_csv2(
    tabla_excel_segura(manifest),
    file.path(
      carpeta_auditoria,
      paste0("manifest_archivos_", fecha_corte, ".csv")
    ),
    na = ""
  )
  readr::write_excel_csv2(
    tabla_excel_segura(controles),
    file.path(
      carpeta_auditoria,
      paste0("controles_entrega_", fecha_corte, ".csv")
    ),
    na = ""
  )
  writeLines(
    enc2utf8(log),
    file.path(
      carpeta_auditoria,
      paste0("log_ejecucion_", fecha_corte, ".txt")
    ),
    useBytes = TRUE
  )

  invisible(ruta_excel)
}

preparar_entrega_anonimizada_preliminar_sdp <- function(
    config = configuracion_entrega_anonimizada()
) {
  verificar_dependencias_entrega(config$ruta_paquete)

  carpeta_resultados <- crear_carpeta_resultados_no_destructiva(
    config$carpeta_resultados_base
  )
  carpeta_auditoria <- file.path(
    carpeta_resultados,
    "auditoria_interna"
  )
  if (!dir.create(
    carpeta_auditoria,
    recursive = FALSE,
    showWarnings = FALSE
  )) {
    stop(
      "No fue posible crear la carpeta de auditoria: ",
      carpeta_auditoria,
      ".",
      call. = FALSE
    )
  }

  carpeta_cliente <- file.path(carpeta_resultados, "entrega_cliente")
  carpeta_staging <- file.path(
    carpeta_resultados,
    paste0(
      ".staging_entrega_cliente_",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
  )

  log <- c(
    paste("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Corte:", config$fecha_corte),
    paste("Repositorio:", config$ruta_paquete),
    "La fuente de entrega es OSIS.",
    "La referencia principal es la CAP oficial filtrada por ENCUESTA_COMPLETA.",
    paste(
      "La salida sin tematica se usa solo como control secundario opcional."
    ),
    "El capitulo K se excluye por decision metodologica."
  )

  estado <- new.env(parent = emptyenv())
  estado$inventario_osis <- tibble::tibble()
  estado$inventario_cap <- tibble::tibble()
  estado$inventario_diccionarios <- tibble::tibble()
  estado$auditoria_diccionarios <- NULL
  estado$resultado <- NULL
  estado$manifest <- tibble::tibble()
  estado$controles <- tibble::tibble()
  estado$capitulos_exportados <- character()

  salida <- tryCatch(
    {
      lectura_diccionario_completo <- leer_diccionario_entrega(
        config$diccionario_completo,
        "diccionario_completo",
        config$orden_capitulos
      )
      lectura_diccionario_anonimizado <- leer_diccionario_entrega(
        config$diccionario_anonimizado,
        "diccionario_anonimizado",
        config$orden_capitulos
      )
      estado$inventario_diccionarios <- dplyr::bind_rows(
        lectura_diccionario_completo$inventario,
        lectura_diccionario_anonimizado$inventario
      )
      estado$auditoria_diccionarios <-
        auditar_diccionarios_entrega(
          lectura_diccionario_completo$datos,
          lectura_diccionario_anonimizado$datos
        )
      log <- c(log, "Diccionarios leidos y auditados.")

      carga_osis <- descubrir_y_leer_osis(config$carpeta_osis)
      estado$inventario_osis <- carga_osis$inventario
      errores_osis <- carga_osis$inventario[
        is.na(carga_osis$inventario$estado_lectura) |
          carga_osis$inventario$estado_lectura != "LEIDO",
        ,
        drop = FALSE
      ]
      estado$controles <- dplyr::bind_rows(
        estado$controles,
        nuevo_control(
          "OSIS_LECTURA_ARCHIVOS",
          descripcion = "Todos los CSV OSIS detectados se leen sin ambiguedad.",
          observado = nrow(errores_osis),
          esperado = "0",
          n_problematicos = nrow(errores_osis),
          pasa = nrow(errores_osis) == 0L
        )
      )
      if (nrow(errores_osis) > 0L) {
        stop(
          "Hay ", nrow(errores_osis),
          " archivos OSIS no leidos o ambiguos. Revise Inventario_OSIS.",
          call. = FALSE
        )
      }
      faltan_a_c_e <- setdiff(c("A", "C", "E"), names(carga_osis$dfs))
      faltan_marca <- c("A", "C", "E")[
        vapply(
          c("A", "C", "E"),
          function(capitulo) {
            !capitulo %in% names(carga_osis$dfs) ||
              !config$variable_completitud %in%
                names(carga_osis$dfs[[capitulo]])
          },
          logical(1)
        )
      ]
      estado$controles <- dplyr::bind_rows(
        estado$controles,
        nuevo_control(
          "OSIS_CAPITULOS_A_C_E",
          descripcion = "OSIS contiene los capitulos obligatorios A, C y E.",
          observado = paste(faltan_a_c_e, collapse = ", "),
          esperado = "Ninguno",
          n_problematicos = length(faltan_a_c_e),
          pasa = length(faltan_a_c_e) == 0L
        ),
        nuevo_control(
          "OSIS_MARCA_A_C_E",
          descripcion = "ENCUESTA_COMPLETA existe en A, C y E.",
          observado = paste(faltan_marca, collapse = ", "),
          esperado = "Ninguno",
          n_problematicos = length(faltan_marca),
          pasa = length(faltan_marca) == 0L
        )
      )
      if (
        length(faltan_a_c_e) > 0L ||
          length(faltan_marca) > 0L
      ) {
        stop(
          "Faltan capitulos A/C/E o la marca ENCUESTA_COMPLETA en OSIS.",
          call. = FALSE
        )
      }
      log <- c(
        log,
        paste(
          "CSV OSIS leidos:",
          nrow(estado$inventario_osis)
        )
      )

      carga_cap <- cargar_referencia_cap_completa(config)
      carga_sin_tematica <- cargar_salida_sin_tematica_opcional(
        config
      )
      estado$inventario_cap <- dplyr::bind_rows(
        carga_cap$inventario |>
          dplyr::mutate(fuente = "CAP_OFICIAL", .before = 1),
        carga_sin_tematica$inventario |>
          dplyr::mutate(
            fuente = "SALIDA_SIN_TEMATICA_OPCIONAL",
            .before = 1
          )
      )
      faltan_cap_referencia <- setdiff(
        c("A", "C", "E"),
        names(carga_cap$dfs)
      )
      faltan_marca_cap <- c("A", "C", "E")[
        vapply(
          c("A", "C", "E"),
          function(capitulo) {
            !capitulo %in% names(carga_cap$dfs) ||
              !config$variable_completitud %in%
                names(carga_cap$dfs[[capitulo]])
          },
          logical(1)
        )
      ]
      estado$controles <- dplyr::bind_rows(
        estado$controles,
        nuevo_control(
          "CAPREF_CAPITULOS_A_C_E",
          descripcion = "La referencia CAP contiene A, C y E.",
          observado = paste(faltan_cap_referencia, collapse = ", "),
          esperado = "Ninguno",
          n_problematicos = length(faltan_cap_referencia),
          pasa = length(faltan_cap_referencia) == 0L
        ),
        nuevo_control(
          "CAPREF_MARCA_A_C_E",
          descripcion = "ENCUESTA_COMPLETA existe en A, C y E de la CAP.",
          observado = paste(faltan_marca_cap, collapse = ", "),
          esperado = "Ninguno",
          n_problematicos = length(faltan_marca_cap),
          pasa = length(faltan_marca_cap) == 0L
        )
      )
      if (
        length(faltan_cap_referencia) > 0L ||
          length(faltan_marca_cap) > 0L
      ) {
        stop(
          "La CAP oficial no contiene A/C/E o ENCUESTA_COMPLETA.",
          call. = FALSE
        )
      }
      log <- c(
        log,
        paste(
          "Capitulos CAP oficiales leidos:",
          length(carga_cap$dfs)
        )
      )

      estado$resultado <- procesar_entrega_en_memoria(
        dfs_osis = carga_osis$dfs,
        dfs_cap_oficial = carga_cap$dfs,
        auditoria_diccionarios = estado$auditoria_diccionarios,
        config = config,
        dfs_cap_sin_tematica = carga_sin_tematica$dfs,
        inventario_osis = carga_osis$inventario,
        problemas_lectura = dplyr::bind_rows(
          carga_osis$problemas_lectura %||% tibble::tibble(),
          carga_cap$problemas_lectura %||% tibble::tibble(),
          carga_sin_tematica$problemas_lectura %||%
            tibble::tibble()
        )
      )
      estado$controles <- dplyr::bind_rows(
        estado$controles,
        estado$resultado$controles
      )
      estado_final_preexportacion <- estado_final_controles(
        estado$controles
      )

      if (estado_final_preexportacion != "APROBADO_PARA_ENTREGA") {
        log <- c(
          log,
          "La prevalidacion contiene controles criticos FAIL.",
          "No se crearon CSV de cliente."
        )
        guardar_auditoria_entrega(
          carpeta_auditoria = carpeta_auditoria,
          config = config,
          estado_final = "NO_APROBADO_PARA_ENTREGA",
          inventario_osis = estado$inventario_osis,
          inventario_cap = estado$inventario_cap,
          inventario_diccionarios =
            estado$inventario_diccionarios,
          auditoria_diccionarios =
            estado$auditoria_diccionarios,
          resultado = estado$resultado,
          manifest = estado$manifest,
          controles = estado$controles,
          log = log,
          capitulos_exportados = character()
        )
        stop(
          "Entrega no aprobada. Revise los controles criticos en: ",
          carpeta_auditoria,
          ".",
          call. = FALSE
        )
      }

      exportacion <- exportar_y_releer_csv(
        bases_exportables = estado$resultado$bases_exportables,
        carpeta_staging = carpeta_staging,
        config = config
      )
      estado$manifest <- exportacion$manifest
      estado$resultado$conservacion_valores <-
        exportacion$conservacion_valores
      evaluacion_manifest <- evaluar_manifest_capitulos(
        estado$manifest,
        estado$resultado$capitulos_esperados_entrega,
        config$capitulo_excluido
      )
      estado$resultado$cobertura_capitulos <- dplyr::bind_rows(
        estado$resultado$cobertura_capitulos,
        evaluacion_manifest$cobertura
      )
      estado$controles <- dplyr::bind_rows(
        estado$controles,
        exportacion$controles,
        evaluacion_manifest$control
      )

      archivo_k_staging <- file.path(
        carpeta_staging,
        "VW_EMP_CAP_K.csv"
      )
      estado$controles <- dplyr::bind_rows(
        estado$controles,
        nuevo_control(
          "CSV_K_AUSENTE",
          capitulo = "K",
          descripcion = "No existe CSV del capitulo K.",
          observado = file.exists(archivo_k_staging),
          esperado = "FALSE",
          n_problematicos = as.integer(file.exists(archivo_k_staging)),
          pasa = !file.exists(archivo_k_staging)
        ),
        nuevo_control(
          "CSV_UNICO_POR_CAPITULO",
          descripcion = "Existe un solo archivo final por capitulo.",
          observado = nrow(estado$manifest),
          esperado = length(unique(estado$manifest$capitulo)),
          n_problematicos = nrow(estado$manifest) -
            length(unique(estado$manifest$capitulo)),
          pasa = nrow(estado$manifest) ==
            length(unique(estado$manifest$capitulo))
        )
      )

      estado_final_postexportacion <- estado_final_controles(
        estado$controles
      )
      if (estado_final_postexportacion != "APROBADO_PARA_ENTREGA") {
        eliminar_staging_seguro(
          carpeta_staging,
          carpeta_resultados
        )
        log <- c(
          log,
          "La relectura o validacion de CSV contiene FAIL.",
          "Se elimino exclusivamente la carpeta temporal de esta ejecucion."
        )
        guardar_auditoria_entrega(
          carpeta_auditoria = carpeta_auditoria,
          config = config,
          estado_final = "NO_APROBADO_PARA_ENTREGA",
          inventario_osis = estado$inventario_osis,
          inventario_cap = estado$inventario_cap,
          inventario_diccionarios =
            estado$inventario_diccionarios,
          auditoria_diccionarios =
            estado$auditoria_diccionarios,
          resultado = estado$resultado,
          manifest = estado$manifest,
          controles = estado$controles,
          log = log,
          capitulos_exportados = character()
        )
        stop(
          "Entrega no aprobada despues de validar los CSV. Revise: ",
          carpeta_auditoria,
          ".",
          call. = FALSE
        )
      }

      if (dir.exists(carpeta_cliente) || file.exists(carpeta_cliente)) {
        eliminar_staging_seguro(
          carpeta_staging,
          carpeta_resultados
        )
        stop(
          "La carpeta de cliente ya existe y no sera sobrescrita: ",
          carpeta_cliente,
          ".",
          call. = FALSE
        )
      }
      if (!file.rename(carpeta_staging, carpeta_cliente)) {
        eliminar_staging_seguro(
          carpeta_staging,
          carpeta_resultados
        )
        stop(
          "No fue posible publicar la carpeta final del cliente.",
          call. = FALSE
        )
      }

      estado$capitulos_exportados <- estado$manifest$capitulo
      rutas_finales <- file.path(
        carpeta_cliente,
        estado$manifest$nombre
      )
      estado$manifest <- estado$manifest |>
        dplyr::mutate(
          tamano_bytes = as.numeric(file.info(rutas_finales)$size),
          md5 = unname(tools::md5sum(rutas_finales))
        )

      readr::write_excel_csv2(
        estado$manifest,
        file.path(
          carpeta_cliente,
          paste0(
            "manifest_archivos_",
            config$fecha_corte,
            ".csv"
          )
        ),
        na = ""
      )
      writeLines(
        c(
          "Entrega preliminar anonimizada EM 2025.",
          paste("Corte:", config$fecha_corte),
          "Fuente: archivos CSV de OSIS.",
          "Filtro: ENCUESTA_COMPLETA == 1 con coherencia A-C-E.",
          "Variables: lista blanca del diccionario anonimizado.",
          "Capitulo K: excluido temporalmente.",
          "Delimitador: punto y coma (;).",
          "Codificacion: UTF-8 con BOM.",
          "Valores faltantes: cadena vacia.",
          paste(
            "La cadena literal 'NA' es un valor textual y no se convierte",
            "automaticamente en faltante."
          )
        ),
        file.path(carpeta_cliente, "README_TECNICO.txt"),
        useBytes = TRUE
      )

      log <- c(
        log,
        paste(
          "CSV aprobados y publicados:",
          nrow(estado$manifest)
        ),
        paste(
          "Fin:",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
        )
      )
      guardar_auditoria_entrega(
        carpeta_auditoria = carpeta_auditoria,
        config = config,
        estado_final = "APROBADO_PARA_ENTREGA",
        inventario_osis = estado$inventario_osis,
        inventario_cap = estado$inventario_cap,
        inventario_diccionarios =
          estado$inventario_diccionarios,
        auditoria_diccionarios =
          estado$auditoria_diccionarios,
        resultado = estado$resultado,
        manifest = estado$manifest,
        controles = estado$controles,
        log = log,
        capitulos_exportados = estado$capitulos_exportados
      )

      list(
        estado_final = "APROBADO_PARA_ENTREGA",
        carpeta_resultados = carpeta_resultados,
        carpeta_auditoria = carpeta_auditoria,
        carpeta_cliente = carpeta_cliente,
        resumen_capitulos = estado$resultado$resumen_capitulos,
        controles = estado$controles,
        manifest = estado$manifest
      )
    },
    error = function(e) {
      eliminar_staging_seguro(
        carpeta_staging,
        carpeta_resultados
      )
      eliminar_cliente_parcial_seguro(
        carpeta_cliente,
        carpeta_resultados
      )

      mensaje <- conditionMessage(e)
      ya_es_bloqueo_controlado <- stringr::str_detect(
        mensaje,
        "^Entrega no aprobada"
      )
      if (!ya_es_bloqueo_controlado) {
        estado$controles <- dplyr::bind_rows(
          estado$controles,
          nuevo_control(
            "EJECUCION_ERROR",
            descripcion = "La ejecucion termino con un error bloqueante.",
            observado = mensaje,
            esperado = "Sin errores",
            n_problematicos = 1L,
            pasa = FALSE
          )
        )
        log <- c(
          log,
          paste("ERROR:", mensaje),
          paste(
            "Fin:",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
          )
        )
        try(
          guardar_auditoria_entrega(
            carpeta_auditoria = carpeta_auditoria,
            config = config,
            estado_final = "NO_APROBADO_PARA_ENTREGA",
            inventario_osis = estado$inventario_osis,
            inventario_cap = estado$inventario_cap,
            inventario_diccionarios =
              estado$inventario_diccionarios,
            auditoria_diccionarios =
              estado$auditoria_diccionarios,
            resultado = estado$resultado,
            manifest = estado$manifest,
            controles = estado$controles,
            log = log,
            capitulos_exportados = character()
          ),
          silent = TRUE
        )
      }

      stop(
        mensaje,
        "\nAuditoria interna: ",
        carpeta_auditoria,
        "\nNo se aprobo ni publico una entrega de cliente.",
        call. = FALSE
      )
    }
  )

  salida
}

construir_diccionario_sintetico_entrega <- function(tabla, nombre) {
  tibble::tibble(
    diccionario = nombre,
    hoja = "Plantilla Diccionario de Datos",
    fila_hoja = seq_len(nrow(tabla)) + 5L,
    capitulo_original = paste0("EMP2025_", tabla$capitulo),
    variable_original = tabla$variable,
    capitulo = tabla$capitulo,
    variable = tabla$variable,
    capitulo_vacio = FALSE,
    variable_vacia = FALSE
  )
}

crear_escenario_sintetico_entrega <- function(config) {
  capitulos_esperados <- setdiff(
    config$orden_capitulos,
    config$capitulo_excluido
  )
  capitulos_fuente <- c(
    capitulos_esperados,
    config$capitulo_excluido
  )

  crear_tabla <- function(capitulo) {
    llaves <- funcion_paquete("get_join_keys")(capitulo)
    valores <- stats::setNames(
      rep("1", length(llaves)),
      llaves
    )
    valores[["DIRECTORIO"]] <- "001"
    if ("SECUENCIA_P" %in% llaves) {
      valores[["SECUENCIA_P"]] <- "1"
    }
    if ("ORDEN" %in% llaves) {
      valores[["ORDEN"]] <- "1"
    }
    if (capitulo %in% c("A", "C", "E")) {
      valores[["ENCUESTA_COMPLETA"]] <- "1"
    }
    valores[[paste0("VALOR_", capitulo)]] <- paste0(
      "DATO_",
      capitulo
    )
    tibble::as_tibble(as.list(valores))
  }

  dfs_osis <- lapply(capitulos_fuente, crear_tabla)
  names(dfs_osis) <- capitulos_fuente
  dfs_osis$A$VARIABLE_PROHIBIDA <- "DATO_RESERVADO"

  dfs_cap <- lapply(capitulos_esperados, crear_tabla)
  names(dfs_cap) <- capitulos_esperados

  filas_diccionario <- lapply(names(dfs_osis), function(capitulo) {
    tibble::tibble(
      capitulo = capitulo,
      variable = names(dfs_osis[[capitulo]])
    )
  })
  variables_completas <- dplyr::bind_rows(filas_diccionario)
  variables_anonimizadas <- variables_completas[
    variables_completas$variable != "VARIABLE_PROHIBIDA",
    ,
    drop = FALSE
  ]
  diccionario_completo <- construir_diccionario_sintetico_entrega(
    variables_completas,
    "diccionario_completo"
  )
  diccionario_anonimizado <- construir_diccionario_sintetico_entrega(
    variables_anonimizadas,
    "diccionario_anonimizado"
  )

  list(
    capitulos_esperados = capitulos_esperados,
    dfs_osis = dfs_osis,
    dfs_cap = dfs_cap,
    diccionario_completo = diccionario_completo,
    diccionario_anonimizado = diccionario_anonimizado,
    auditoria_diccionarios = auditar_diccionarios_entrega(
      diccionario_completo,
      diccionario_anonimizado
    )
  )
}

estado_control_sintetico <- function(resultado, control_id) {
  indice <- which(resultado$controles$control_id == control_id)
  if (length(indice) != 1L) {
    stop(
      "El control sintetico `", control_id,
      "` no existe o no es unico.",
      call. = FALSE
    )
  }
  resultado$controles$estado[[indice]]
}

prueba_sintetica_entrega_anonimizada <- function() {
  config <- configuracion_entrega_anonimizada(
    usuario = "C:/USUARIO_SINTETICO"
  )
  escenario <- crear_escenario_sintetico_entrega(config)
  valores_llave <- c(
    "4084579", "4084579.0", "4084579.00",
    "001", "001.0", "1.5", "A01.0",
    " 4084579.0", "+4084579.0", "", NA_character_
  )
  valores_llave_esperados <- c(
    "4084579", "4084579", "4084579",
    "001", "001", "1.5", "A01.0",
    " 4084579.0", "+4084579.0", "", NA_character_
  )
  valores_llave_normalizados <- normalizar_llave_cruce(
    valores_llave
  )
  resultado <- procesar_entrega_en_memoria(
    escenario$dfs_osis,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )
  evaluacion_manifest_completo <- evaluar_manifest_capitulos(
    tibble::tibble(capitulo = escenario$capitulos_esperados),
    escenario$capitulos_esperados,
    config$capitulo_excluido
  )
  cobertura_entrada_con_k <- resumir_cobertura_capitulos(
    "OSIS_SINTETICO",
    c(escenario$capitulos_esperados, "K"),
    escenario$capitulos_esperados,
    c(escenario$capitulos_esperados, "K")
  )
  equivalencia_b_normal <- resolver_equivalencia_llave_b(
    escenario$dfs_osis$B,
    escenario$dfs_cap$B
  )
  b_alias <- escenario$dfs_osis$B |>
    dplyr::mutate(ORDEN = .data$SECUENCIA_P) |>
    dplyr::select(-"SECUENCIA_P")
  equivalencia_b_alias <- resolver_equivalencia_llave_b(
    b_alias,
    escenario$dfs_cap$B
  )
  f_con_sufijo_decimal <- escenario$dfs_osis$F |>
    dplyr::mutate(
      DIRECTORIO = paste0(.data$DIRECTORIO, ".0"),
      SECUENCIA_P = paste0(.data$SECUENCIA_P, ".0"),
      ORDEN = paste0(.data$ORDEN, ".0")
    )
  filtro_f_con_sufijo <- limitar_capitulo_a_universo(
    f_con_sufijo_decimal,
    "F",
    construir_universos_completos(
      escenario$dfs_osis,
      config$variable_completitud
    )
  )

  escenario_persona_adicional <- crear_escenario_sintetico_entrega(
    config
  )
  persona_adicional <- escenario_persona_adicional$dfs_osis$E
  persona_adicional$ORDEN <- "2"
  escenario_persona_adicional$dfs_osis$E <- dplyr::bind_rows(
    escenario_persona_adicional$dfs_osis$E,
    persona_adicional
  )
  escenario_persona_adicional$dfs_cap$E <- dplyr::bind_rows(
    escenario_persona_adicional$dfs_cap$E,
    persona_adicional
  )
  resultado_persona_tematica_ausente <- procesar_entrega_en_memoria(
    escenario_persona_adicional$dfs_osis,
    escenario_persona_adicional$dfs_cap,
    escenario_persona_adicional$auditoria_diccionarios,
    config
  )

  stopifnot(
    identical(
      escenario$capitulos_esperados,
      c(
        "A", "B", "C", "D", "E", "F", "G",
        "H", "I", "J", "L", "MA", "MB"
      )
    ),
    length(escenario$capitulos_esperados) == 13L,
    setequal(
      names(resultado$bases_exportables),
      escenario$capitulos_esperados
    ),
    length(resultado$bases_exportables) == 13L,
    !config$capitulo_excluido %in%
      names(resultado$bases_exportables),
    all(
      vapply(
        resultado$bases_exportables,
        nrow,
        integer(1)
      ) == 1L
    ),
    !"VARIABLE_PROHIBIDA" %in%
      names(resultado$bases_exportables$A),
    identical(valores_llave_normalizados, valores_llave_esperados),
    identical(
      normalizar_llave_cruce(valores_llave_normalizados),
      valores_llave_normalizados
    ),
    all(
      resultado$comparacion_resumen$regla_comparacion[
        resultado$comparacion_resumen$capitulo %in%
          c("A", "C", "E")
      ] == "exacta_estructural"
    ),
    all(
      resultado$comparacion_resumen$regla_comparacion[
        !resultado$comparacion_resumen$capitulo %in%
          c("A", "C", "E")
      ] == "subconjunto_tematico"
    ),
    all(resultado$comparacion_resumen$cumple_regla_comparacion),
    estado_final_controles(resultado$controles) ==
      "APROBADO_PARA_ENTREGA",
    cobertura_entrada_con_k$conjunto_esperado_completo[[1]],
    cobertura_entrada_con_k$n_inesperados[[1]] == 0L,
    equivalencia_b_normal$auditoria$estado_equivalencia[[1]] ==
      "equivalencia_no_requerida",
    equivalencia_b_normal$auditoria$estado_validacion[[1]] == "PASS",
    equivalencia_b_alias$auditoria$estado_equivalencia[[1]] ==
      "equivalencia_excepcional_activada",
    equivalencia_b_alias$auditoria$estado_validacion[[1]] == "PASS",
    filtro_f_con_sufijo$n_despues == 1L,
    filtro_f_con_sufijo$data$DIRECTORIO[[1]] == "001.0",
    filtro_f_con_sufijo$data$SECUENCIA_P[[1]] == "1.0",
    filtro_f_con_sufijo$data$ORDEN[[1]] == "1.0",
    estado_final_controles(
      resultado_persona_tematica_ausente$controles
    ) == "APROBADO_PARA_ENTREGA",
    any(
      resultado_persona_tematica_ausente$comparacion_resumen$
        llaves_universo_completo_sin_osis > 0L &
        resultado_persona_tematica_ausente$comparacion_resumen$
          regla_comparacion == "subconjunto_tematico"
    ),
    evaluacion_manifest_completo$control$estado[[1]] == "PASS",
    evaluacion_manifest_completo$cobertura$cantidad_encontrada[[1]] ==
      13L,
    evaluacion_manifest_completo$cobertura$
      capitulo_excluido_ausente[[1]]
  )

  carpeta_csv_sintetico <- tempfile(
    "entrega_anonimizada_csv_sintetico_"
  )
  on.exit(
    if (dir.exists(carpeta_csv_sintetico)) {
      unlink(carpeta_csv_sintetico, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )
  prueba_csv <- exportar_y_releer_csv(
    bases_exportables = list(
      A = tibble::tibble(
        DIRECTORIO = c("001", "002", "003", "004"),
        TEXTO = c("NA", "", NA_character_, " 99 ")
      )
    ),
    carpeta_staging = carpeta_csv_sintetico,
    config = config
  )
  stopifnot(
    prueba_csv$controles$estado[
      prueba_csv$controles$control_id == "CSV_A_VALORES"
    ] == "PASS",
    prueba_csv$manifest$n_cambios_valores[[1]] == 0L,
    nrow(prueba_csv$conservacion_valores) == 0L
  )

  ruta_diccionario_sintetico <- tempfile(
    "diccionario_entrega_sintetico_",
    fileext = ".xlsx"
  )
  on.exit(
    if (file.exists(ruta_diccionario_sintetico)) {
      unlink(ruta_diccionario_sintetico, force = TRUE)
    },
    add = TRUE
  )
  wb_sintetico <- openxlsx::createWorkbook()
  purrr::walk(
    c(
      "Instrucciones",
      "Plantilla Diccionario de Datos",
      "Dominios",
      "DICCIONARIO PLANTILLA"
    ),
    function(hoja) {
      openxlsx::addWorksheet(wb_sintetico, hoja)
    }
  )
  openxlsx::writeData(
    wb_sintetico,
    "Instrucciones",
    data.frame(nota = "Hoja informativa"),
    colNames = FALSE
  )
  tabla_diccionario_sintetico <- matrix(
    NA_character_,
    nrow = 7L,
    ncol = 5L
  )
  tabla_diccionario_sintetico[4L, c(1L, 5L)] <- c(
    "ID tabla",
    "Nombre de la variable o la columna"
  )
  tabla_diccionario_sintetico[6L, c(1L, 5L)] <- c(
    "EMP2025_A",
    "DIRECTORIO"
  )
  tabla_diccionario_sintetico[7L, c(1L, 5L)] <- c(
    "EMP2025_A",
    "ENCUESTA_COMPLETA"
  )
  openxlsx::writeData(
    wb_sintetico,
    "Plantilla Diccionario de Datos",
    tabla_diccionario_sintetico,
    colNames = FALSE,
    keepNA = FALSE
  )
  openxlsx::writeData(
    wb_sintetico,
    "Dominios",
    data.frame(
      DOMINIO_ID = "SINTETICO",
      VALOR = "1",
      DESCRIPCION = "Valor sintetico"
    )
  )
  openxlsx::writeData(
    wb_sintetico,
    "DICCIONARIO PLANTILLA",
    data.frame(nota = "Plantilla informativa"),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(
    wb_sintetico,
    ruta_diccionario_sintetico,
    overwrite = TRUE
  )
  lectura_diccionario_sintetico <- leer_diccionario_entrega(
    ruta_diccionario_sintetico,
    "diccionario_sintetico",
    config$orden_capitulos
  )
  stopifnot(
    nrow(lectura_diccionario_sintetico$inventario) == 4L,
    sum(lectura_diccionario_sintetico$inventario$procesable) == 1L,
    nrow(lectura_diccionario_sintetico$datos) == 2L,
    all(lectura_diccionario_sintetico$datos$capitulo == "A")
  )

  tibble::tibble(
    prueba = c(
      "conjunto exacto de 13 capitulos esperados sin M ni K",
      "normalizacion conservadora e idempotente de llaves",
      "normalizacion solo interna conserva llaves originales exportables",
      "K permitido en entrada sin alterar los 13 obligatorios",
      "todos los capitulos esperados permiten aprobar",
      "K aparece en OSIS y no se exporta",
      "filtro jerarquico A-C-E",
      "lista blanca exacta y retiro no autorizadas",
      "comparacion exacta A-C-E y subconjunto tematico",
      "uso normal de SECUENCIA_P en B",
      "equivalencia excepcional ORDEN a SECUENCIA_P en B",
      "persona completa ausente de capitulo tematico no bloquea",
      "manifest completo con 13 capitulos y sin K",
      "conservacion CSV incluido texto literal NA",
      "lector Excel distingue datos de hojas informativas"
    ),
    estado = "PASS"
  )
}

prueba_sintetica_negativa_entrega_anonimizada <- function() {
  config <- configuracion_entrega_anonimizada(
    usuario = "C:/USUARIO_SINTETICO"
  )
  escenario <- crear_escenario_sintetico_entrega(config)

  osis_sin_d <- escenario$dfs_osis
  osis_sin_d$D <- NULL
  resultado_sin_d_osis <- procesar_entrega_en_memoria(
    osis_sin_d,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )

  dic_anon_sin_d <- escenario$diccionario_anonimizado[
    escenario$diccionario_anonimizado$capitulo != "D",
    ,
    drop = FALSE
  ]
  auditoria_sin_d_ambos <- auditar_diccionarios_entrega(
    escenario$diccionario_completo,
    dic_anon_sin_d
  )
  resultado_sin_d_ambos <- procesar_entrega_en_memoria(
    osis_sin_d,
    escenario$dfs_cap,
    auditoria_sin_d_ambos,
    config
  )

  cap_sin_d <- escenario$dfs_cap
  cap_sin_d$D <- NULL
  resultado_sin_d_cap <- procesar_entrega_en_memoria(
    escenario$dfs_osis,
    cap_sin_d,
    escenario$auditoria_diccionarios,
    config
  )

  osis_con_ab <- escenario$dfs_osis
  osis_con_ab$AB <- tibble::tibble(DIRECTORIO = "001")
  resultado_con_ab <- procesar_entrega_en_memoria(
    osis_con_ab,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )

  osis_variable_faltante <- escenario$dfs_osis
  osis_variable_faltante$D$VALOR_D <- NULL
  resultado_variable_faltante <- procesar_entrega_en_memoria(
    osis_variable_faltante,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )

  osis_contradiccion <- escenario$dfs_osis
  fila_c_contradiccion <- escenario$dfs_osis$C
  fila_c_contradiccion$DIRECTORIO <- "002"
  osis_contradiccion$C <- dplyr::bind_rows(
    osis_contradiccion$C,
    fila_c_contradiccion
  )
  resultado_contradiccion <- procesar_entrega_en_memoria(
    osis_contradiccion,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )

  osis_duplicado <- escenario$dfs_osis
  osis_duplicado$B <- dplyr::bind_rows(
    osis_duplicado$B,
    osis_duplicado$B
  )
  resultado_duplicado <- procesar_entrega_en_memoria(
    osis_duplicado,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )

  osis_persona_fuera_universo <- escenario$dfs_osis
  persona_fuera_universo <- osis_persona_fuera_universo$F
  persona_fuera_universo$ORDEN <- "2"
  osis_persona_fuera_universo$F <- dplyr::bind_rows(
    osis_persona_fuera_universo$F,
    persona_fuera_universo
  )
  resultado_persona_fuera_universo <- procesar_entrega_en_memoria(
    osis_persona_fuera_universo,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config
  )

  b_alias <- escenario$dfs_osis$B |>
    dplyr::mutate(ORDEN = .data$SECUENCIA_P) |>
    dplyr::select(-"SECUENCIA_P")
  equivalencia_b_duplicada <- resolver_equivalencia_llave_b(
    dplyr::bind_rows(b_alias, b_alias),
    escenario$dfs_cap$B
  )
  b_alias_diferente <- b_alias
  b_alias_diferente$ORDEN <- "2"
  equivalencia_b_diferente <- resolver_equivalencia_llave_b(
    b_alias_diferente,
    escenario$dfs_cap$B
  )
  b_ambigua <- escenario$dfs_osis$B |>
    dplyr::mutate(ORDEN = "2")
  equivalencia_b_ambigua <- resolver_equivalencia_llave_b(
    b_ambigua,
    escenario$dfs_cap$B
  )

  variables_archivos_sinteticos <- dplyr::bind_rows(
    tibble::tibble(
      capitulo = "L",
      variable = c(
        "DIRECTORIO", "SECUENCIA_P",
        paste0("NHCLP", 1:4)
      )
    ),
    tibble::tibble(
      capitulo = "MA",
      variable = c(
        "DIRECTORIO", "SECUENCIA_P",
        paste0("NHCMP", 1:4)
      )
    )
  )
  archivo_l_sintetico <- tibble::as_tibble(
    stats::setNames(
      as.list(rep("1", 6L)),
      c(
        "DIRECTORIO", "SECUENCIA_P",
        paste0("NHCLP", 1:4)
      )
    )
  )
  auditoria_archivo_ma_equivocado <-
    auditar_archivos_posiblemente_equivocados(
      list(L = archivo_l_sintetico, MA = archivo_l_sintetico),
      variables_archivos_sinteticos
    )

  problema_critico <- clasificar_problemas_lectura(
    tibble::tibble(
      row = 2L,
      col = 1L,
      expected = "a value",
      actual = "campo desplazado"
    ),
    c("DIRECTORIO", "SECUENCIA_P", "VALOR"),
    c("DIRECTORIO", "SECUENCIA_P"),
    "OSIS",
    "VW_EMP_CAP_D.csv",
    "D"
  )
  problema_informativo <- clasificar_problemas_lectura(
    tibble::tibble(
      row = 10L,
      col = 3L,
      expected = "a double",
      actual = "texto"
    ),
    c("DIRECTORIO", "SECUENCIA_P", "VALOR"),
    c("DIRECTORIO", "SECUENCIA_P"),
    "OSIS",
    "VW_EMP_CAP_D.csv",
    "D"
  )
  resultado_parsing_critico <- procesar_entrega_en_memoria(
    escenario$dfs_osis,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config,
    problemas_lectura = problema_critico
  )
  resultado_parsing_informativo <- procesar_entrega_en_memoria(
    escenario$dfs_osis,
    escenario$dfs_cap,
    escenario$auditoria_diccionarios,
    config,
    problemas_lectura = problema_informativo
  )

  manifest_incompleto <- tibble::tibble(
    capitulo = setdiff(escenario$capitulos_esperados, "D")
  )
  evaluacion_manifest_incompleto <- evaluar_manifest_capitulos(
    manifest_incompleto,
    escenario$capitulos_esperados,
    config$capitulo_excluido
  )
  comparacion_na <- comparar_muestra_valores_csv(
    tibble::tibble(valor = c(NA_character_, "", "NA", "98")),
    tibble::tibble(valor = c("", NA_character_, "NA", "98")),
    "SINTETICO"
  )

  stopifnot(
    estado_control_sintetico(
      resultado_sin_d_osis,
      "OSIS_02_CAPITULOS_ESPERADOS"
    ) == "FAIL",
    estado_final_controles(resultado_sin_d_osis$controles) ==
      "NO_APROBADO_PARA_ENTREGA",
    estado_control_sintetico(
      resultado_sin_d_ambos,
      "OSIS_02_CAPITULOS_ESPERADOS"
    ) == "FAIL",
    estado_control_sintetico(
      resultado_sin_d_ambos,
      "DICC_04_CAPITULOS_ESPERADOS"
    ) == "FAIL",
    estado_control_sintetico(
      resultado_sin_d_cap,
      "CAPREF_02_CAPITULOS_ESPERADOS"
    ) == "FAIL",
    estado_control_sintetico(
      resultado_con_ab,
      "OSIS_03_CAPITULOS_INESPERADOS"
    ) == "FAIL",
    estado_final_controles(resultado_con_ab$controles) ==
      "NO_APROBADO_PARA_ENTREGA",
    !"K" %in% names(resultado_con_ab$bases_exportables),
    estado_control_sintetico(
      resultado_variable_faltante,
      "CAP_D_PERMITIDAS_PRESENTES"
    ) == "FAIL",
    any(
      resultado_variable_faltante$permitidas_faltantes$variable ==
        "VALOR_D"
    ),
    any(
      resultado_variable_faltante$permitidas_faltantes$
        clasificacion_faltante ==
        "variable_autorizada_ausente_en_OSIS"
    ),
    estado_control_sintetico(
      resultado_contradiccion,
      "UNIV_01_JERARQUIA_A_C_E"
    ) == "FAIL",
    estado_control_sintetico(
      resultado_duplicado,
      "CAP_B_DUPLICADOS"
    ) == "FAIL",
    estado_control_sintetico(
      resultado_persona_fuera_universo,
      "CAP_F_FUERA_UNIVERSO"
    ) == "FAIL",
    nrow(
      resultado_persona_fuera_universo$
        llaves_osis_fuera_universo_completo
    ) == 1L,
    estado_final_controles(
      resultado_persona_fuera_universo$controles
    ) == "NO_APROBADO_PARA_ENTREGA",
    equivalencia_b_duplicada$auditoria$estado_validacion[[1]] ==
      "FAIL",
    equivalencia_b_diferente$auditoria$estado_validacion[[1]] ==
      "FAIL",
    equivalencia_b_ambigua$auditoria$estado_validacion[[1]] ==
      "FAIL",
    auditoria_archivo_ma_equivocado$criticidad[
      auditoria_archivo_ma_equivocado$capitulo == "MA"
    ] == "CRITICO",
    auditoria_archivo_ma_equivocado$clasificacion[
      auditoria_archivo_ma_equivocado$capitulo == "MA"
    ] == "posible_archivo_de_capitulo_equivocado",
    problema_critico$criticidad[[1]] == "CRITICO",
    problema_informativo$criticidad[[1]] == "INFORMATIVO",
    estado_control_sintetico(
      resultado_parsing_critico,
      "LECTURA_01_PROBLEMAS_PARSING_CRITICOS"
    ) == "FAIL",
    estado_control_sintetico(
      resultado_parsing_informativo,
      "LECTURA_01_PROBLEMAS_PARSING_CRITICOS"
    ) == "PASS",
    evaluacion_manifest_incompleto$control$estado[[1]] == "FAIL",
    evaluacion_manifest_incompleto$cobertura$
      capitulos_faltantes[[1]] == "D",
    is.na(normalizar_capitulo_entrega(
      "AB",
      config$orden_capitulos
    )),
    is.na(normalizar_capitulo_entrega(
      "M",
      config$orden_capitulos
    )),
    comparacion_na$resumen$valores_preservados[[1]],
    nrow(comparacion_na$diferencias) == 0L
  )

  tibble::tibble(
    prueba = c(
      "capitulo D faltante en OSIS bloquea",
      "D faltante en OSIS y diccionario se detecta en ambos",
      "capitulo D faltante en CAP oficial bloquea",
      "capitulo inesperado AB bloquea",
      "K nunca aparece en bases exportables",
      "variable autorizada faltante bloquea",
      "contradiccion jerarquica A-C-E bloquea",
      "llave duplicada bloquea",
      "persona tematica fuera del universo completo bloquea",
      "alias B con duplicados o diferencias se rechaza",
      "B con SECUENCIA_P y ORDEN contradictorios se rechaza",
      "archivo MA con estructura y prefijo de L se detecta",
      "parsing critico bloquea y parsing informativo no bloquea",
      "manifest que omite D bloquea",
      "capitulos AB y M son rechazados",
      "NA literal no se confunde con faltante"
    ),
    estado = "PASS"
  )
}

if (identical(
  Sys.getenv("ANALISISEM2025_PRUEBA_SINTETICA_ENTREGA"),
  "true"
)) {
  testthat::test_that(
    "la preparacion anonimizada satisface los escenarios positivos",
    {
      resultado_positivo <- prueba_sintetica_entrega_anonimizada()
      testthat::expect_true(all(resultado_positivo$estado == "PASS"))
    }
  )
  testthat::test_that(
    "la preparacion anonimizada bloquea los escenarios negativos",
    {
      resultado_negativo <-
        prueba_sintetica_negativa_entrega_anonimizada()
      testthat::expect_true(all(resultado_negativo$estado == "PASS"))
    }
  )
}

if (sys.nframe() == 0L) {
  argumentos <- commandArgs(trailingOnly = TRUE)
  if ("--prueba-sintetica" %in% argumentos) {
    verificar_dependencias_entrega(
      "C:/Users/gomez/OneDrive/Documentos/analisisem2025"
    )
    print(dplyr::bind_rows(
      positiva = prueba_sintetica_entrega_anonimizada(),
      negativa = prueba_sintetica_negativa_entrega_anonimizada(),
      .id = "tipo_prueba"
    ))
  } else {
    resultado_entrega <- preparar_entrega_anonimizada_preliminar_sdp()
    print(resultado_entrega$resumen_capitulos)
    message("Estado final: ", resultado_entrega$estado_final)
    message("Auditoria: ", resultado_entrega$carpeta_auditoria)
    message("Cliente: ", resultado_entrega$carpeta_cliente)
  }
}
