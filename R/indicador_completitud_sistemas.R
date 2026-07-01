#' Generar indicador de encuesta completa para Sistemas
#'
#' Construye una marca binaria de completitud de encuesta para Sistemas usando
#' como insumo el reporte de caidas sin criterio tematico. La completitud se
#' determina a nivel de `DIRECTORIO` y se hereda a los niveles de vivienda,
#' hogar y persona.
#'
#' La funcion espera recibir como `reporte_final_caidas` el reporte base sin
#' reglas tematicas, por ejemplo `diag_tres_criterios$reporte_final_caidas`.
#' No reconstruye caidas ni modifica la definicion de completitud existente.
#'
#' @param dfs Lista de data.frames/tibbles originales por capitulo.
#' @param reporte_final_caidas Data frame con el reporte final de caidas base
#'   sin criterio tematico.
#' @param exportar Logico. Si TRUE, exporta un Excel con hojas por nivel.
#' @param ruta_salida Carpeta o ruta `.xlsx` donde se exporta el archivo si
#'   `exportar = TRUE`.
#' @param fecha_corte Fecha usada para nombrar el archivo cuando `ruta_salida`
#'   es una carpeta. Si es NULL, usa la fecha del sistema.
#'
#' @return Una lista con `archivo_sistemas`, `vivienda`, `hogar`, `persona`,
#'   `resumen`, `validaciones` y `ruta_exportacion`, ademas de objetos de
#'   auditoria intermedios.
#' @export
generar_archivo_indicador_completitud_sistemas <- function(dfs,
                                                           reporte_final_caidas,
                                                           exportar = FALSE,
                                                           ruta_salida = NULL,
                                                           fecha_corte = NULL) {
  .validar_insumos_indicador_sistemas(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas,
    exportar = exportar,
    ruta_salida = ruta_salida
  )
  .validar_reporte_sin_tematica_indicador_sistemas(reporte_final_caidas)

  cap_hogar <- if ("C" %in% names(dfs)) "C" else "B"

  base_completa <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas
  )

  indicador_directorio <- .construir_indicador_directorio_sistemas(
    dfs = dfs,
    dfs_completa = base_completa$dfs
  )

  resumen_caidas_base <- .resumir_caidas_base_indicador_sistemas(
    reporte_final_caidas = reporte_final_caidas
  )

  auditoria_universos <- indicador_directorio %>%
    dplyr::left_join(
      resumen_caidas_base %>%
        dplyr::select(-dplyr::any_of("cae_base_sin_tematica")),
      by = "DIRECTORIO"
    )

  vivienda <- .construir_salida_indicador_sistemas_nivel(
    df = dfs$A,
    capitulo = "A",
    tabla = "CAP_A",
    llaves = get_join_keys("A"),
    indicador_directorio = indicador_directorio
  )

  hogar <- .construir_salida_indicador_sistemas_nivel(
    df = dfs[[cap_hogar]],
    capitulo = cap_hogar,
    tabla = paste0("CAP_", cap_hogar),
    llaves = get_join_keys(cap_hogar),
    indicador_directorio = indicador_directorio
  )

  persona <- .construir_salida_indicador_sistemas_nivel(
    df = dfs$E,
    capitulo = "E",
    tabla = "CAP_E",
    llaves = get_join_keys("E"),
    indicador_directorio = indicador_directorio
  )

  archivo_sistemas <- dplyr::bind_rows(vivienda, hogar, persona)

  resumen <- .resumir_indicador_sistemas_niveles(
    vivienda = vivienda,
    hogar = hogar,
    persona = persona,
    tabla_hogar = paste0("CAP_", cap_hogar)
  )

  cascada_niveles <- resumen

  validaciones <- .validar_coherencia_indicador_sistemas(
    archivo_sistemas = archivo_sistemas,
    vivienda = vivienda,
    hogar = hogar,
    persona = persona,
    indicador_directorio = indicador_directorio,
    dfs = dfs
  )

  ruta_exportacion <- NULL
  archivos_exportados <- list()

  if (isTRUE(exportar)) {
    ruta_exportacion <- .resolver_ruta_excel_indicador_sistemas(
      ruta_salida = ruta_salida,
      fecha_corte = fecha_corte
    )

    .exportar_excel_indicador_sistemas(
      ruta_exportacion = ruta_exportacion,
      vivienda = vivienda,
      hogar = hogar,
      persona = persona
    )

    archivos_exportados <- list(
      excel_sistemas = normalizePath(
        ruta_exportacion,
        winslash = "/",
        mustWork = FALSE
      )
    )
  }

  list(
    archivo_sistemas = archivo_sistemas,
    vivienda = vivienda,
    hogar = hogar,
    persona = persona,
    resumen = resumen,
    validaciones = validaciones,
    ruta_exportacion = ruta_exportacion,
    indicador_directorio = indicador_directorio,
    auditoria_universos = auditoria_universos,
    cascada_niveles = cascada_niveles,
    resumen_caidas_base = resumen_caidas_base,
    archivos_exportados = archivos_exportados
  )
}

.validar_insumos_indicador_sistemas <- function(dfs,
                                                reporte_final_caidas,
                                                exportar,
                                                ruta_salida) {
  if (!is.list(dfs)) {
    stop("`dfs` debe ser una lista de data.frames/tibbles.", call. = FALSE)
  }
  if (!is.data.frame(reporte_final_caidas)) {
    stop("`reporte_final_caidas` debe ser un data.frame/tibble.", call. = FALSE)
  }
  if (!"A" %in% names(dfs)) {
    stop("`dfs` debe contener la tabla A para el nivel vivienda.", call. = FALSE)
  }
  if (!any(c("C", "B") %in% names(dfs))) {
    stop("`dfs` debe contener la tabla C o B para el nivel hogar.", call. = FALSE)
  }
  if (!"E" %in% names(dfs)) {
    stop("`dfs` debe contener la tabla E para el nivel persona.", call. = FALSE)
  }
  if (isTRUE(exportar) && (is.null(ruta_salida) || !nzchar(ruta_salida))) {
    stop("`ruta_salida` es requerida cuando `exportar = TRUE`.", call. = FALSE)
  }

  invisible(TRUE)
}

.validar_reporte_sin_tematica_indicador_sistemas <- function(reporte_final_caidas) {
  if ("cae_tematica" %in% names(reporte_final_caidas)) {
    cae_tematica <- .coerce_flag_indicador_sistemas(reporte_final_caidas$cae_tematica)
    if (any(cae_tematica, na.rm = TRUE)) {
      stop(
        "`reporte_final_caidas` contiene caidas tematicas. ",
        "Use el reporte base sin criterio tematico, por ejemplo ",
        "`diag_tres_criterios$reporte_final_caidas`.",
        call. = FALSE
      )
    }
  }

  vars_texto <- intersect(
    c("criterios_reporte", "criterios_caida", "criterio_principal_reporte"),
    names(reporte_final_caidas)
  )
  if (length(vars_texto) == 0L) {
    return(invisible(TRUE))
  }

  texto <- unlist(reporte_final_caidas[vars_texto], use.names = FALSE)
  texto <- iconv(as.character(texto), from = "", to = "ASCII//TRANSLIT", sub = "")
  texto <- stringr::str_to_lower(texto)
  if (any(stringr::str_detect(texto, "tematica"), na.rm = TRUE)) {
    stop(
      "`reporte_final_caidas` contiene senales tematicas en columnas de criterios. ",
      "Use el reporte base sin criterio tematico, por ejemplo ",
      "`diag_tres_criterios$reporte_final_caidas`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.construir_indicador_directorio_sistemas <- function(dfs, dfs_completa) {
  A <- normalize_keys(dfs$A, get_join_keys("A"))
  A_completa <- normalize_keys(dfs_completa$A, get_join_keys("A"))

  directorios_completos <- A_completa %>%
    dplyr::filter(!is.na(DIRECTORIO), nzchar(DIRECTORIO)) %>%
    dplyr::distinct(DIRECTORIO) %>%
    dplyr::pull(DIRECTORIO)

  indicador <- A %>%
    dplyr::distinct(DIRECTORIO, .keep_all = TRUE) %>%
    dplyr::select(dplyr::any_of(c("DIRECTORIO", "UUID", "SECUENCIA_ENCUESTA"))) %>%
    .asegurar_columnas_indicador_sistemas(c("UUID", "DIRECTORIO", "SECUENCIA_ENCUESTA")) %>%
    dplyr::mutate(
      ENCUESTA_COMPLETA = dplyr::if_else(
        DIRECTORIO %in% directorios_completos,
        1L,
        0L
      ),
      encuesta_completa = ENCUESTA_COMPLETA,
      cae_base_sin_tematica = ENCUESTA_COMPLETA == 0L
    )

  indicador %>%
    dplyr::select(
      DIRECTORIO,
      UUID,
      SECUENCIA_ENCUESTA,
      ENCUESTA_COMPLETA,
      encuesta_completa,
      cae_base_sin_tematica
    )
}

.construir_salida_indicador_sistemas_nivel <- function(df,
                                                       capitulo,
                                                       tabla,
                                                       llaves,
                                                       indicador_directorio) {
  df_norm <- normalize_keys(df, llaves)

  .validar_llaves_indicador_sistemas(df_norm, capitulo, llaves)

  salida <- df_norm %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves)), .keep_all = TRUE) %>%
    dplyr::left_join(
      indicador_directorio %>%
        dplyr::select(DIRECTORIO, ENCUESTA_COMPLETA) %>%
        dplyr::rename(ENCUESTA_COMPLETA_INDICADOR = ENCUESTA_COMPLETA),
      by = "DIRECTORIO"
    ) %>%
    dplyr::mutate(
      ENCUESTA_COMPLETA = as.integer(.data$ENCUESTA_COMPLETA_INDICADOR)
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "UUID",
      "DIRECTORIO",
      "SECUENCIA_ENCUESTA",
      "SECUENCIA_P",
      "ORDEN",
      "ENCUESTA_COMPLETA"
    ))) %>%
    .asegurar_columnas_indicador_sistemas(c(
      "UUID",
      "DIRECTORIO",
      "SECUENCIA_ENCUESTA",
      "SECUENCIA_P",
      "ORDEN",
      "ENCUESTA_COMPLETA"
    ))

  if (!"SECUENCIA_P" %in% llaves) {
    salida$SECUENCIA_P <- NA
  }
  if (!"ORDEN" %in% llaves) {
    salida$ORDEN <- NA
  }

  salida %>%
    dplyr::transmute(
      UUID,
      DIRECTORIO,
      SECUENCIA_ENCUESTA,
      SECUENCIA_P,
      ORDEN,
      VARIABLE = "ENCUESTA_COMPLETA",
      VALOR_ANTERIOR = NA_integer_,
      VALOR = as.integer(ENCUESTA_COMPLETA),
      Tabla = tabla
    )
}

.resumir_indicador_sistemas_niveles <- function(vivienda,
                                                hogar,
                                                persona,
                                                tabla_hogar) {
  niveles <- list(
    vivienda = list(df = vivienda, tabla = "CAP_A"),
    hogar = list(df = hogar, tabla = tabla_hogar),
    persona = list(df = persona, tabla = "CAP_E")
  )

  purrr::imap_dfr(niveles, function(info, nivel) {
    df <- .filtrar_marca_indicador_sistemas(info$df)

    tibble::tibble(
      nivel = nivel,
      tabla_base = info$tabla,
      n_registros_total = nrow(df),
      n_directorios_total = dplyr::n_distinct(df$DIRECTORIO),
      n_registros_completos = sum(df$VALOR == 1L, na.rm = TRUE),
      n_registros_incompletos = sum(df$VALOR == 0L, na.rm = TRUE),
      n_directorios_completos = df %>%
        dplyr::filter(VALOR == 1L) %>%
        dplyr::pull(DIRECTORIO) %>%
        dplyr::n_distinct(),
      n_directorios_incompletos = df %>%
        dplyr::filter(VALOR == 0L) %>%
        dplyr::pull(DIRECTORIO) %>%
        dplyr::n_distinct()
    )
  })
}

.validar_coherencia_indicador_sistemas <- function(archivo_sistemas,
                                                   vivienda,
                                                   hogar,
                                                   persona,
                                                   indicador_directorio,
                                                   dfs) {
  archivo_marca <- .filtrar_marca_indicador_sistemas(archivo_sistemas)
  vivienda_marca <- .filtrar_marca_indicador_sistemas(vivienda)
  hogar_marca <- .filtrar_marca_indicador_sistemas(hogar)
  persona_marca <- .filtrar_marca_indicador_sistemas(persona)

  mixtos <- archivo_marca %>%
    dplyr::filter(!is.na(VALOR)) %>%
    dplyr::group_by(DIRECTORIO) %>%
    dplyr::summarise(n_valores = dplyr::n_distinct(VALOR), .groups = "drop") %>%
    dplyr::filter(n_valores > 1L)

  sin_marca <- archivo_marca %>%
    dplyr::filter(is.na(VALOR))

  valores_no_binarios <- archivo_marca %>%
    dplyr::filter(!is.na(VALOR), !VALOR %in% c(0L, 1L))

  n_directorios_a <- dfs$A %>%
    normalize_keys(get_join_keys("A")) %>%
    dplyr::summarise(n = dplyr::n_distinct(DIRECTORIO)) %>%
    dplyr::pull(n)

  coherencia_hogar <- .comparar_directorio_nivel_indicador_sistemas(vivienda_marca, hogar_marca)
  coherencia_persona <- .comparar_directorio_nivel_indicador_sistemas(vivienda_marca, persona_marca)

  tibble::tibble(
    validacion = c(
      "sin_directorios_con_valores_mixtos",
      "sin_registros_sin_marca",
      "solo_valores_binarios",
      "vivienda_vs_hogar_coherente",
      "vivienda_vs_persona_coherente",
      "total_directorios_A_igual_indicador_directorio"
    ),
    ok = c(
      nrow(mixtos) == 0L,
      nrow(sin_marca) == 0L,
      nrow(valores_no_binarios) == 0L,
      coherencia_hogar$n_problemas == 0L,
      coherencia_persona$n_problemas == 0L,
      n_directorios_a == nrow(indicador_directorio)
    ),
    n_problemas = c(
      nrow(mixtos),
      nrow(sin_marca),
      nrow(valores_no_binarios),
      coherencia_hogar$n_problemas,
      coherencia_persona$n_problemas,
      abs(n_directorios_a - nrow(indicador_directorio))
    ),
    descripcion = c(
      "No debe existir un mismo DIRECTORIO con valores distintos de ENCUESTA_COMPLETA.",
      "No debe haber registros sin marca de completitud.",
      "La marca ENCUESTA_COMPLETA debe contener solamente valores 0 o 1.",
      "Los hogares deben heredar la misma marca del nivel vivienda para cada DIRECTORIO presente.",
      "Las personas deben heredar la misma marca del nivel vivienda para cada DIRECTORIO presente.",
      "El total de directorios en A debe coincidir con el indicador por directorio."
    )
  )
}

.comparar_directorio_nivel_indicador_sistemas <- function(vivienda, nivel) {
  vivienda_dir <- vivienda %>%
    dplyr::distinct(DIRECTORIO, valor_vivienda = VALOR)

  problemas <- nivel %>%
    dplyr::distinct(DIRECTORIO, valor_nivel = VALOR) %>%
    dplyr::left_join(vivienda_dir, by = "DIRECTORIO") %>%
    dplyr::filter(is.na(valor_vivienda) | valor_nivel != valor_vivienda)

  list(n_problemas = nrow(problemas))
}

.filtrar_marca_indicador_sistemas <- function(df) {
  if (!is.data.frame(df)) {
    return(tibble::tibble())
  }

  if ("VARIABLE" %in% names(df)) {
    df <- df %>%
      dplyr::filter(.data$VARIABLE == "ENCUESTA_COMPLETA")
  }

  if ("VALOR" %in% names(df)) {
    df$VALOR <- .coerce_valor_indicador_sistemas(df$VALOR)
  }

  df
}

.resumir_caidas_base_indicador_sistemas <- function(reporte_final_caidas) {
  reporte_norm <- normalize_keys(reporte_final_caidas, "DIRECTORIO")
  vars_caida <- intersect(
    c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado"),
    names(reporte_norm)
  )
  vars_texto <- intersect(
    c("criterios_reporte", "criterio_principal_reporte", "razon_principal_caida"),
    names(reporte_norm)
  )

  if (length(vars_caida) > 0L) {
    for (var in vars_caida) {
      reporte_norm[[var]] <- .coerce_flag_indicador_sistemas(reporte_norm[[var]])
    }
  }

  resumen <- reporte_norm %>%
    dplyr::group_by(DIRECTORIO) %>%
    dplyr::summarise(
      cae_base_sin_tematica = if (length(vars_caida) > 0L) {
        any(dplyr::if_any(dplyr::all_of(vars_caida), identity), na.rm = TRUE)
      } else {
        TRUE
      },
      .groups = "drop"
    )

  for (var in vars_texto) {
    texto_var <- reporte_norm %>%
      dplyr::group_by(DIRECTORIO) %>%
      dplyr::summarise(
        valor = paste(unique(stats::na.omit(.data[[var]])), collapse = " | "),
        .groups = "drop"
      )
    names(texto_var)[names(texto_var) == "valor"] <- var
    resumen <- dplyr::left_join(resumen, texto_var, by = "DIRECTORIO")
  }

  resumen
}

.exportar_excel_indicador_sistemas <- function(ruta_exportacion,
                                               vivienda,
                                               hogar,
                                               persona) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "El paquete `openxlsx` es requerido para exportar el Excel. ",
      "Instalelo o use `exportar = FALSE`.",
      call. = FALSE
    )
  }

  dir.create(dirname(ruta_exportacion), recursive = TRUE, showWarnings = FALSE)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Vivienda")
  openxlsx::writeData(wb, "Vivienda", vivienda)
  openxlsx::addWorksheet(wb, "Hogar")
  openxlsx::writeData(wb, "Hogar", hogar)
  openxlsx::addWorksheet(wb, "Personas")
  openxlsx::writeData(wb, "Personas", persona)
  openxlsx::saveWorkbook(wb, ruta_exportacion, overwrite = TRUE)

  invisible(ruta_exportacion)
}

.resolver_ruta_excel_indicador_sistemas <- function(ruta_salida, fecha_corte) {
  if (is.null(fecha_corte) || !nzchar(fecha_corte)) {
    fecha_corte <- format(Sys.Date(), "%Y%m%d")
  }

  extension <- tolower(tools::file_ext(ruta_salida))

  if (identical(extension, "xlsx")) {
    return(ruta_salida)
  }

  file.path(
    ruta_salida,
    paste0("indicador_encuesta_completa_sistemas_", fecha_corte, ".xlsx")
  )
}

.validar_llaves_indicador_sistemas <- function(df, capitulo, llaves) {
  faltantes <- setdiff(llaves, names(df))
  if (length(faltantes) > 0L) {
    stop(
      "La tabla ", capitulo, " no contiene las llaves requeridas: ",
      paste(faltantes, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.asegurar_columnas_indicador_sistemas <- function(df, columnas) {
  for (columna in columnas) {
    if (!columna %in% names(df)) {
      df[[columna]] <- NA
    }
  }

  df %>%
    dplyr::select(dplyr::all_of(columnas))
}

.coerce_flag_indicador_sistemas <- function(x) {
  if (is.logical(x)) {
    return(dplyr::coalesce(x, FALSE))
  }

  if (is.numeric(x) || is.integer(x)) {
    return(dplyr::coalesce(x != 0, FALSE))
  }

  x_chr <- stringr::str_squish(stringr::str_to_upper(as.character(x)))
  x_chr <- iconv(x_chr, from = "", to = "ASCII//TRANSLIT")

  dplyr::coalesce(
    x_chr %in% c("1", "TRUE", "T", "SI", "YES", "CAIDA"),
    FALSE
  )
}

.coerce_valor_indicador_sistemas <- function(x) {
  if (is.integer(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    return(as.integer(x))
  }

  x_chr <- stringr::str_squish(as.character(x))
  dplyr::case_when(
    x_chr %in% c("1", "1.0", "TRUE", "true") ~ 1L,
    x_chr %in% c("0", "0.0", "FALSE", "false") ~ 0L,
    TRUE ~ NA_integer_
  )
}
