#' Construir matriz auditable de flujo del Capitulo K
#'
#' Construye una matriz pregunta-variable para auditar las reglas de flujo
#' del Capitulo K de fuerza de trabajo de la EM 2025.
#'
#' @param ruta_historico Ruta opcional al script historico de diagnostico.
#' @param ruta_diccionario Ruta opcional al diccionario oficial.
#' @param ruta_formulario Ruta opcional al formulario PDF.
#' @param ruta_excel_flujo Ruta opcional al archivo Excel de flujo/formulario.
#' @param usar_reglas_actuales Si `TRUE`, incorpora reglas existentes de
#'   [diagnostico_flujo_capitulo_k()].
#'
#' @return Un tibble con la matriz auditable de flujo.
#'
#' @details
#' La funcion no evalua bases de encuesta ni escribe archivos. Las reglas
#' actuales se recuperan, cuando es posible, usando helpers internos de
#' `diagnostico_flujo_capitulo_k()` sobre una base esqueleto sin filas. Esos
#' helpers son dependencias internas del paquete y podrian cambiar.
#'
#' @export
construir_matriz_flujo_capitulo_k <- function(ruta_historico = NULL,
                                              ruta_diccionario = NULL,
                                              ruta_formulario = NULL,
                                              ruta_excel_flujo = NULL,
                                              usar_reglas_actuales = TRUE) {
  if (!exists("tipo_capitulo", inherits = TRUE) || !identical(tipo_capitulo[["K"]], "persona")) {
    stop("El capitulo K debe estar configurado como nivel `persona`.")
  }

  llaves_k <- get_join_keys("K")
  if (!identical(llaves_k, c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))) {
    stop("La llave del capitulo K debe ser DIRECTORIO + SECUENCIA_P + ORDEN.")
  }

  umbral_k <- if (exists("Edad_objeto", inherits = TRUE)) Edad_objeto[["K"]] else NA_real_
  universo_base <- if (!is.null(umbral_k) && !is.na(umbral_k)) {
    paste0("Personas de ", umbral_k, " anos o mas")
  } else {
    "Personas del universo del Capitulo K"
  }

  reglas_script <- .extraer_reglas_base_historico_k(ruta_historico)
  reglas_actuales <- if (isTRUE(usar_reglas_actuales)) {
    .reglas_actuales_flujo_k()
  } else {
    tibble::tibble(
      variable = character(),
      orden_flujo = integer(),
      bloque_funcion_actual = character(),
      pregunta_funcion_actual = character(),
      regla_funcion_actual = character(),
      comentario_funcion_actual = character(),
      variable_texto_libre = logical()
    )
  }
  diccionario_k <- .leer_diccionario_flujo_k(ruta_diccionario)
  fuente_formulario_pagina_val <- if (is.null(ruta_formulario)) {
    NA_character_
  } else if (file.exists(ruta_formulario)) {
    paste0("pendiente_verificacion_pdf:", basename(ruta_formulario))
  } else {
    "pdf_no_encontrado"
  }
  fuente_excel_flujo_val <- if (is.null(ruta_excel_flujo)) {
    NA_character_
  } else if (file.exists(ruta_excel_flujo)) {
    paste0("pendiente_verificacion_excel:", basename(ruta_excel_flujo))
  } else {
    "excel_flujo_no_encontrado"
  }

  variables <- unique(c(
    reglas_script$variable,
    reglas_actuales$variable,
    diccionario_k$variable
  ))
  variables <- variables[!is.na(variables) & nzchar(variables)]

  orden <- tibble::tibble(variable = variables) |>
    dplyr::left_join(
      reglas_script |>
        dplyr::mutate(orden_script = dplyr::row_number()) |>
        dplyr::select(.data$variable, .data$orden_script),
      by = "variable"
    ) |>
    dplyr::left_join(
      reglas_actuales |>
        dplyr::select(.data$variable, orden_actual = .data$orden_flujo),
      by = "variable"
    ) |>
    dplyr::mutate(
      orden_pregunta = dplyr::coalesce(.data$orden_script, .data$orden_actual)
    )

  faltan_orden <- is.na(orden$orden_pregunta)
  if (any(faltan_orden)) {
    orden$orden_pregunta[faltan_orden] <- max(orden$orden_pregunta, na.rm = TRUE) +
      seq_len(sum(faltan_orden))
  }

  matriz_flujo_k <- orden |>
    dplyr::select(.data$orden_pregunta, .data$variable) |>
    dplyr::left_join(reglas_script, by = "variable") |>
    dplyr::left_join(reglas_actuales, by = "variable") |>
    dplyr::left_join(diccionario_k, by = "variable") |>
    dplyr::mutate(
      bloque = dplyr::coalesce(.data$bloque_funcion_actual, .data$bloque_script, "pendiente_definir"),
      pregunta_formulario = dplyr::coalesce(.data$pregunta_diccionario, .data$pregunta_funcion_actual, "pendiente_verificar_formulario"),
      variable_texto_libre = dplyr::coalesce(.data$variable_texto_libre, FALSE),
      tipo_variable = dplyr::coalesce(
        .data$tipo_variable,
        dplyr::if_else(.data$variable_texto_libre, "texto_libre", "pendiente_verificar_diccionario")
      ),
      universo_base = universo_base,
      condicion_debe_responder_R = dplyr::coalesce(
        .data$regla_funcion_actual,
        .data$regla_script_original,
        "pendiente_definir"
      ),
      condicion_debe_responder_lenguaje = paste0(
        .data$variable,
        " debe responder si se cumple: ",
        .data$condicion_debe_responder_R
      ),
      variables_previas_usadas = vapply(
        .data$condicion_debe_responder_R,
        .extraer_vars_expr_flujo_k,
        character(1)
      ),
      salto_si_respuesta = "pendiente_verificar_formulario_excel",
      destino_si_respuesta = "pendiente_verificar_formulario_excel",
      fuente_formulario_pagina = fuente_formulario_pagina_val,
      fuente_diccionario = dplyr::coalesce(.data$fuente_diccionario, NA_character_),
      fuente_excel_flujo = fuente_excel_flujo_val,
      diferencia_script_vs_funcion = .comparar_reglas_flujo_k(
        .data$regla_script_original,
        .data$regla_funcion_actual
      ),
      estado_validacion_usuario = "pendiente_revision",
      comentario_usuario = dplyr::coalesce(.data$comentario_funcion_actual, NA_character_),
      prioridad_revision = dplyr::if_else(
        .data$condicion_debe_responder_R == "pendiente_definir" |
          .data$diferencia_script_vs_funcion %in% c(
            "diferente", "falta_en_script", "falta_en_funcion", "pendiente_comparar"
          ),
        "alta",
        "media"
      )
    ) |>
    dplyr::arrange(.data$orden_pregunta, .data$variable) |>
    dplyr::select(
      .data$orden_pregunta,
      .data$bloque,
      .data$pregunta_formulario,
      .data$variable,
      .data$variable_texto_libre,
      .data$tipo_variable,
      .data$universo_base,
      .data$condicion_debe_responder_lenguaje,
      .data$condicion_debe_responder_R,
      .data$variables_previas_usadas,
      .data$salto_si_respuesta,
      .data$destino_si_respuesta,
      .data$fuente_formulario_pagina,
      .data$fuente_diccionario,
      .data$fuente_excel_flujo,
      .data$regla_script_original,
      .data$regla_funcion_actual,
      .data$diferencia_script_vs_funcion,
      .data$estado_validacion_usuario,
      .data$comentario_usuario,
      .data$prioridad_revision
    )

  matriz_flujo_k
}

.extraer_reglas_base_historico_k <- function(ruta_historico) {
  if (is.null(ruta_historico) || !nzchar(ruta_historico)) {
    return(tibble::tibble(
      variable = character(),
      bloque_script = character(),
      regla_script_original = character()
    ))
  }
  if (!file.exists(ruta_historico)) {
    stop("No existe `ruta_historico`: ", ruta_historico)
  }

  txt <- readLines(ruta_historico, encoding = "UTF-8", warn = FALSE)
  ini <- grep("^reglas_base <- tibble::tribble\\(", txt)
  if (length(ini) == 0) {
    stop("No se encontro `reglas_base` en el script historico.")
  }

  cierres <- grep("^\\)", txt)
  fin <- cierres[cierres > ini[1]][1]
  if (is.na(fin)) {
    stop("No se pudo identificar el cierre de `reglas_base` en el script historico.")
  }

  bloque <- txt[seq.int(ini[1], fin)]
  filas <- lapply(bloque, function(linea) {
    vals <- regmatches(linea, gregexpr('"([^"]*)"', linea, perl = TRUE))[[1]]
    vals <- gsub('^"|"$', "", vals)
    if (length(vals) < 3) {
      return(NULL)
    }
    tibble::tibble(
      variable = vals[1],
      bloque_script = vals[2],
      regla_script_original = vals[3]
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1))]
  if (length(filas) == 0) {
    stop("No fue posible extraer filas de `reglas_base`.")
  }

  dplyr::bind_rows(filas) |>
    dplyr::distinct(.data$variable, .keep_all = TRUE)
}

.leer_diccionario_flujo_k <- function(ruta_diccionario) {
  if (is.null(ruta_diccionario) || !nzchar(ruta_diccionario)) {
    return(tibble::tibble(
      variable = character(),
      pregunta_diccionario = character(),
      tipo_variable = character(),
      fuente_diccionario = character()
    ))
  }
  if (!file.exists(ruta_diccionario)) {
    stop("No existe `ruta_diccionario`: ", ruta_diccionario)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Para leer `ruta_diccionario` se requiere el paquete `readxl`.")
  }

  hojas <- readxl::excel_sheets(ruta_diccionario)
  hoja <- if ("Plantilla Diccionario de Datos" %in% hojas) {
    "Plantilla Diccionario de Datos"
  } else {
    hojas[[1]]
  }

  dic <- readxl::read_excel(ruta_diccionario, sheet = hoja, skip = 3)
  dic <- tibble::as_tibble(dic)
  names(dic) <- gsub("\\n", " ", names(dic))
  names(dic) <- stringr::str_squish(names(dic))

  col_var <- .col_diccionario_flujo_k(dic, c(
    "Nombre de la variable o la columna",
    "Nombre variable",
    "VARIABLE",
    "Variable"
  ))
  col_desc <- .col_diccionario_flujo_k(dic, c(
    "Descripcion de la variable o la columna",
    "Descripción de la variable o la columna",
    "Descripcion",
    "Descripción"
  ))
  col_tipo <- .col_diccionario_flujo_k(dic, c("Tipo de dato", "Tipo"))

  if (is.na(col_var)) {
    stop("No se encontro la columna de variable en el diccionario.")
  }

  out <- tibble::tibble(
    variable = toupper(stringr::str_trim(as.character(dic[[col_var]]))),
    pregunta_diccionario = if (!is.na(col_desc)) stringr::str_squish(as.character(dic[[col_desc]])) else NA_character_,
    tipo_variable = if (!is.na(col_tipo)) stringr::str_squish(as.character(dic[[col_tipo]])) else NA_character_,
    fuente_diccionario = paste0("diccionario:", basename(ruta_diccionario), "; hoja:", hoja)
  )

  out |>
    dplyr::filter(!is.na(.data$variable), nzchar(.data$variable), grepl("^NPCK", .data$variable)) |>
    dplyr::distinct(.data$variable, .keep_all = TRUE)
}

.col_diccionario_flujo_k <- function(dic, candidatos) {
  normalizar <- function(x) {
    gsub("[^A-Z0-9]+", "", toupper(iconv(x, to = "ASCII//TRANSLIT")))
  }
  nms_norm <- normalizar(names(dic))
  cand_norm <- normalizar(candidatos)
  idx <- match(cand_norm, nms_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) {
    return(NA_character_)
  }
  names(dic)[idx[[1]]]
}

.reglas_actuales_flujo_k <- function() {
  # Dependencias internas del paquete: estos helpers pertenecen a
  # diagnostico_flujo_capitulo_k() y podrian cambiar en una version futura.
  if (
    !exists(".diag_k_construir_reglas", mode = "function", inherits = TRUE) ||
      !exists(".diag_k_reglas_tbl", mode = "function", inherits = TRUE)
  ) {
    warning("No estan disponibles los helpers internos de diagnostico_flujo_capitulo_k(); no se incorporan reglas actuales.")
    return(tibble::tibble(
      variable = character(),
      orden_flujo = integer(),
      bloque_funcion_actual = character(),
      pregunta_funcion_actual = character(),
      regla_funcion_actual = character(),
      comentario_funcion_actual = character(),
      variable_texto_libre = logical()
    ))
  }

  data_esqueleto <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    ORDEN = character(),
    edad = numeric(),
    NPCKP17 = numeric(),
    NPCKP17_FINAL = numeric()
  )

  nodos <- NULL
  if (exists("construir_nodos_flujo_k", mode = "function", inherits = TRUE)) {
    nodos <- tryCatch(
      construir_nodos_flujo_k(data_esqueleto, edad_var = "edad"),
      error = function(e) NULL
    )
  }

  reglas <- .diag_k_construir_reglas(
    data = data_esqueleto,
    nodos_paquete = nodos,
    variable_k23_final = "NPCKP17",
    incluir_texto_libre = TRUE
  )
  reglas_tbl <- .diag_k_reglas_tbl(reglas, data_esqueleto, diccionario = NULL)

  reglas_tbl |>
    dplyr::transmute(
      variable = .data$variable,
      orden_flujo = .data$orden_flujo,
      bloque_funcion_actual = .data$bloque,
      pregunta_funcion_actual = .data$pregunta,
      regla_funcion_actual = .completar_regla_universo_flujo_k(
        .data$regla_r,
        .data$universo_base
      ),
      comentario_funcion_actual = .data$comentario,
      variable_texto_libre = .data$texto_libre
    ) |>
    dplyr::distinct(.data$variable, .keep_all = TRUE)
}

.completar_regla_universo_flujo_k <- function(regla, universo_base) {
  regla <- as.character(regla)
  regla <- stringr::str_squish(regla)
  regla[is.na(regla) | !nzchar(regla)] <- NA_character_

  universo_base <- as.character(universo_base)
  es_universo_10 <- !is.na(universo_base) &
    grepl("10", universo_base, fixed = TRUE)
  tiene_edad <- !is.na(regla) &
    grepl("(^|[^A-Za-z0-9_])edad\\s*(>=|>|==|<=|<)", regla)
  agregar_universo <- !is.na(regla) & es_universo_10 & !tiene_edad

  regla[agregar_universo] <- paste0(
    "edad >= 10 & ",
    .envolver_regla_flujo_k(regla[agregar_universo])
  )
  regla
}

.envolver_regla_flujo_k <- function(regla) {
  necesita_parentesis <- grepl("\\|", regla) & !grepl("^\\s*\\(", regla)
  regla[necesita_parentesis] <- paste0("(", regla[necesita_parentesis], ")")
  regla
}

.normalizar_expr_flujo_k <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x <- gsub("[[:space:]]+", "", x)
  x <- toupper(x)
  x <- gsub("==TRUE", "", x, fixed = TRUE)
  x <- gsub("NPCKP17_FINAL", "NPCKP17", x, fixed = TRUE)
  x <- gsub("UNIVERSO_CAP_K|UNIVERSO_K", "EDAD>=10", x)
  x <- gsub("MAYOR_15", "EDAD>=15", x, fixed = TRUE)
  x <- gsub("MAYOR_18", "EDAD>=18", x, fixed = TRUE)
  x <- gsub("ASALARIADO_FINAL", "NPCKP17%IN%C(1,2,3,7)", x, fixed = TRUE)
  x <- gsub("INDEPENDIENTE_FINAL", "NPCKP17%IN%C(4,5,8)", x, fixed = TRUE)
  x <- gsub("OCUPADO_FINAL", "NPCKP17%IN%C(1,2,3,4,5,6,7,8)", x, fixed = TRUE)
  x <- gsub("FAMILIAR_SIN_PAGO_FINAL", "NPCKP17==6", x, fixed = TRUE)
  x <- gsub("NO_OCUPADO_FINAL", "!(NPCKP17%IN%C(1,2,3,4,5,6,7,8))", x, fixed = TRUE)

  regla_llega_k17 <- paste0(
    "EDAD>=10&(",
    "(NPCKP1==1&NPCKP2_1==1)|",
    "((NPCKP1%IN%C(2,3,4,6)|NPCKP2_1==2)&NPCKP2==1)|",
    "(NPCKP2==2&NPCKP3==1&NPCKP5_1%IN%C(1,2,3,4))|",
    "(NPCKP2==2&NPCKP3==1&NPCKP5_1%IN%C(5,6,7,8)&NPCKP6_1==1)|",
    "((NPCKP3==2|NPCKP6_1%IN%C(2,3))&NPCKP4==1)",
    ")"
  )
  x <- gsub(regla_llega_k17, "LLEGA_K17", x, fixed = TRUE)
  x
}

.extraer_vars_expr_flujo_k <- function(expr) {
  if (is.na(expr) || !nzchar(expr)) {
    return(NA_character_)
  }
  patron <- paste0(
    "\\b(",
    "NPCK[A-Z0-9_]+|NPCEP4|EDAD|edad|",
    "codigo_municipio|Clase|CLASE|MPIO|aplica_npckp45l_caballo|",
    "llega_K[0-9]+|universo_cap_k|universo_k|",
    "ocupado_final|asalariado_final|independiente_final|",
    "familiar_sin_pago_final|no_ocupado_final|mayor_15|mayor_18",
    ")\\b"
  )
  vars <- regmatches(expr, gregexpr(patron, expr, perl = TRUE))[[1]]
  vars <- unique(vars[nzchar(vars)])
  if (length(vars) == 0) {
    return(NA_character_)
  }
  paste(vars, collapse = ", ")
}

.comparar_reglas_flujo_k <- function(regla_script, regla_funcion) {
  mapply(
    function(script, funcion) {
      script_vacia <- is.na(script) || !nzchar(script)
      funcion_vacia <- is.na(funcion) || !nzchar(funcion)
      if (script_vacia && funcion_vacia) return("pendiente_comparar")
      if (script_vacia) return("falta_en_script")
      if (funcion_vacia) return("falta_en_funcion")
      if (identical(.normalizar_expr_flujo_k(script), .normalizar_expr_flujo_k(funcion))) {
        "igual"
      } else {
        "diferente"
      }
    },
    regla_script,
    regla_funcion,
    USE.NAMES = FALSE
  )
}
