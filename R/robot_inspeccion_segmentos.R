#' Robot de inspeccion para segmentos completos
#'
#' Exporta una sabana consolidada con todos los capitulos filtrados a los
#' `DIRECTORIO` pertenecientes a una lista de segmentos. La seleccion de
#' viviendas se hace desde el capitulo `A`, usando `SEGMENTO` y `DIRECTORIO`.
#'
#' La funcion no recalcula diagnosticos ni modifica la logica de
#' `robot_inspeccion_encuesta()`. Si se entrega una salida previa de
#' `reporte_segmentos_cascada_muestras()`, agrega sus tablas principales al
#' Excel consolidado.
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param segmentos Vector de segmentos a inspeccionar. Puede contener segmentos
#'   completos tipo `CODMPIO_SEGMENTO` o segmentos cortos; los segmentos cortos
#'   pueden ser ambiguos si `municipios = NULL`.
#' @param municipios Vector opcional de municipios/codigos para desambiguar
#'   segmentos cortos, o diccionario con columnas `cod_mpio` y `municipio`.
#' @param salida_segmentos Objeto opcional devuelto por
#'   `reporte_segmentos_cascada_muestras()`.
#' @param carpeta_salida Carpeta de salida para el Excel consolidado y, si
#'   aplica, los robots individuales.
#' @param fecha_corte Fecha o etiqueta de corte para nombrar archivos. Si es
#'   `NULL`, usa la fecha del sistema en formato `YYYYMMDD`.
#' @param exportar_excel Si `TRUE`, exporta el Excel consolidado.
#' @param exportar_robots_individuales Si `TRUE`, ejecuta
#'   `robot_inspeccion_encuesta()` para cada `DIRECTORIO` encontrado.
#' @param incluir_capitulos Vector opcional de capitulos a incluir. Si es
#'   `NULL`, incluye todos los capitulos disponibles en `dfs`.
#' @param archivo Ruta opcional del Excel consolidado. Si es `NULL`, se crea en
#'   `carpeta_salida`.
#' @param sobrescribir Si `TRUE`, permite reemplazar el Excel consolidado.
#'
#' @return Lista con segmentos solicitados/encontrados/no encontrados,
#'   directorios, sabana por capitulo y rutas exportadas.
#'
#' @examples
#' \dontrun{
#' segmentos_problema <- c(
#'   "25898_17183", "25898_17189", "25898_17191", "25898_17207",
#'   "25473_30303", "25473_30305", "25473_30324", "25473_30328",
#'   "25754_30439"
#' )
#'
#' robot_seg <- robot_inspeccion_segmentos(
#'   dfs = dfs,
#'   segmentos = segmentos_problema,
#'   salida_segmentos = salida_segmentos,
#'   carpeta_salida = carpeta_salida,
#'   fecha_corte = fecha_corte,
#'   exportar_excel = TRUE,
#'   exportar_robots_individuales = FALSE
#' )
#'
#' robot_seg$directorios_segmentos
#' robot_seg$archivo_excel
#' }
#'
#' @export
robot_inspeccion_segmentos <- function(dfs,
                                       segmentos,
                                       municipios = NULL,
                                       salida_segmentos = NULL,
                                       carpeta_salida = NULL,
                                       fecha_corte = NULL,
                                       exportar_excel = TRUE,
                                       exportar_robots_individuales = FALSE,
                                       incluir_capitulos = NULL,
                                       archivo = NULL,
                                       sobrescribir = TRUE) {
  .validar_robot_inspeccion_segmentos(
    dfs = dfs,
    segmentos = segmentos,
    carpeta_salida = carpeta_salida,
    archivo = archivo,
    exportar_excel = exportar_excel,
    exportar_robots_individuales = exportar_robots_individuales
  )

  if (isTRUE(exportar_excel) && !requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  names(dfs) <- toupper(names(dfs))
  fecha_txt <- .fecha_robot_segmentos(fecha_corte)
  if (is.null(archivo) && isTRUE(exportar_excel)) {
    archivo <- file.path(
      carpeta_salida,
      paste0("robot_sabana_segmentos_", fecha_txt, ".xlsx")
    )
  }

  segmentos_solicitados <- .segmentos_solicitados_robot_segmentos(
    segmentos = segmentos,
    municipios = municipios
  )

  if (any(segmentos_solicitados$es_segmento_corto) && is.null(municipios)) {
    warning(
      "Se entregaron segmentos cortos y `municipios = NULL`; ",
      "puede haber ambiguedad si el mismo segmento existe en varios municipios.",
      call. = FALSE
    )
  }

  mapa_segmentos <- .mapa_segmentos_robot_segmentos(
    A = dfs[["A"]],
    municipios = municipios
  )

  cruce_segmentos <- .cruzar_segmentos_robot_segmentos(
    segmentos_solicitados = segmentos_solicitados,
    mapa_segmentos = mapa_segmentos,
    municipios = municipios
  )

  directorios_segmentos <- cruce_segmentos$directorios
  segmentos_encontrados <- cruce_segmentos$segmentos_encontrados
  segmentos_no_encontrados <- cruce_segmentos$segmentos_no_encontrados

  if (nrow(directorios_segmentos) == 0) {
    warning(
      "No se encontraron DIRECTORIO para los segmentos solicitados. ",
      "La salida se devolvera con tablas vacias.",
      call. = FALSE
    )
  }

  caps <- .capitulos_robot_segmentos(dfs, incluir_capitulos)
  sabana_segmentos <- lapply(caps, function(cap) {
    .filtrar_capitulo_robot_segmentos(
      df = dfs[[cap]],
      directorios_segmentos = directorios_segmentos
    )
  })
  names(sabana_segmentos) <- caps

  archivo_excel <- NULL
  if (isTRUE(exportar_excel)) {
    archivo_excel <- .exportar_robot_inspeccion_segmentos_excel(
      segmentos_solicitados = segmentos_solicitados,
      directorios_segmentos = directorios_segmentos,
      sabana_segmentos = sabana_segmentos,
      salida_segmentos = salida_segmentos,
      archivo = archivo,
      sobrescribir = sobrescribir
    )
  }

  archivos_robot <- NULL
  if (isTRUE(exportar_robots_individuales)) {
    carpeta_robots <- file.path(
      if (!is.null(carpeta_salida)) carpeta_salida else dirname(archivo),
      paste0("robots_individuales_segmentos_", fecha_txt)
    )
    archivos_robot <- .exportar_robots_individuales_segmentos(
      dfs = dfs,
      directorios_segmentos = directorios_segmentos,
      carpeta_robots = carpeta_robots
    )
  }

  list(
    segmentos_solicitados = segmentos_solicitados,
    segmentos_encontrados = segmentos_encontrados,
    segmentos_no_encontrados = segmentos_no_encontrados,
    directorios_segmentos = directorios_segmentos,
    sabana_segmentos = sabana_segmentos,
    archivo_excel = archivo_excel,
    archivos_robot = archivos_robot
  )
}

.validar_robot_inspeccion_segmentos <- function(dfs,
                                                segmentos,
                                                carpeta_salida,
                                                archivo,
                                                exportar_excel,
                                                exportar_robots_individuales) {
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }
  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres de capitulos.")
  }

  names(dfs) <- toupper(names(dfs))
  if (!"A" %in% names(dfs) || !is.data.frame(dfs[["A"]])) {
    stop("Debe existir `dfs$A` como data frame.")
  }
  if (!"DIRECTORIO" %in% names(dfs[["A"]])) {
    stop("`dfs$A` debe contener `DIRECTORIO`.")
  }
  if (!"SEGMENTO" %in% names(dfs[["A"]])) {
    stop("`dfs$A` debe contener `SEGMENTO`.")
  }

  if (is.null(segmentos)) {
    stop("`segmentos` no puede ser NULL.")
  }
  segmentos_norm <- .normalizar_texto_robot_segmentos(segmentos)
  segmentos_norm <- segmentos_norm[!is.na(segmentos_norm) & nzchar(segmentos_norm)]
  if (length(segmentos_norm) == 0) {
    stop("`segmentos` debe contener al menos un valor no vacio.")
  }

  if ((isTRUE(exportar_excel) || isTRUE(exportar_robots_individuales)) &&
      is.null(carpeta_salida) && is.null(archivo)) {
    stop(
      "Si se exporta, debe suministrar `carpeta_salida` o `archivo` ",
      "para definir la ubicacion de salida."
    )
  }

  invisible(TRUE)
}

.normalizar_texto_robot_segmentos <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

.fecha_robot_segmentos <- function(fecha_corte) {
  if (is.null(fecha_corte)) {
    return(format(Sys.Date(), "%Y%m%d"))
  }
  out <- .normalizar_texto_robot_segmentos(fecha_corte)[1]
  stringr::str_replace_all(out, "[^0-9A-Za-z_-]+", "_")
}

.segmentos_solicitados_robot_segmentos <- function(segmentos, municipios = NULL) {
  seg_raw <- as.character(segmentos)
  seg_norm <- .normalizar_texto_robot_segmentos(seg_raw)
  codigo <- dplyr::if_else(
    !is.na(seg_norm) & stringr::str_detect(seg_norm, "^[0-9]{5}_"),
    substr(seg_norm, 1, 5),
    NA_character_
  )
  corto <- dplyr::if_else(
    !is.na(seg_norm) & stringr::str_detect(seg_norm, "^[0-9]{5}_"),
    sub("^[0-9]{5}_", "", seg_norm),
    seg_norm
  )

  tibble::tibble(
    orden_segmento = seq_along(seg_raw),
    SEGMENTO_solicitado = stringr::str_squish(seg_raw),
    segmento_norm = seg_norm,
    codigo_mpio_solicitado = codigo,
    segmento_corto_solicitado = corto,
    es_segmento_corto = !is.na(seg_norm) & !stringr::str_detect(seg_norm, "_")
  ) %>%
    dplyr::filter(!is.na(.data$segmento_norm), nzchar(.data$segmento_norm)) %>%
    dplyr::distinct(.data$segmento_norm, .keep_all = TRUE) %>%
    dplyr::arrange(.data$orden_segmento)
}

.dic_mpios_robot_segmentos <- function(municipios = NULL) {
  if (is.data.frame(municipios) && all(c("cod_mpio", "municipio") %in% names(municipios))) {
    dic <- municipios
  } else if (exists("dic_mpios", mode = "any")) {
    dic <- dic_mpios
  } else {
    dic <- NULL
  }

  if (is.null(dic) || !is.data.frame(dic) || !all(c("cod_mpio", "municipio") %in% names(dic))) {
    return(tibble::tibble(codigo_mpio = character(), NomMunicipio_dic = character()))
  }

  dic %>%
    dplyr::transmute(
      codigo_mpio = .normalizar_codigo_mpio_reporte(.data$cod_mpio),
      NomMunicipio_dic = as.character(.data$municipio)
    ) %>%
    dplyr::distinct(.data$codigo_mpio, .keep_all = TRUE)
}

.filtro_municipios_robot_segmentos <- function(municipios = NULL) {
  if (is.null(municipios) || is.data.frame(municipios)) {
    return(character())
  }
  x <- .normalizar_texto_robot_segmentos(municipios)
  x <- x[!is.na(x) & nzchar(x)]
  unique(c(.normalizar_codigo_mpio_reporte(x), .normalizar_texto_reporte_muestras(x)))
}

.mapa_segmentos_robot_segmentos <- function(A, municipios = NULL) {
  var_nom <- col_first_existing(A, c("NomMunicipio", "NOM_MPIO", "NOMBRE_MUNICIPIO", "MUNICIPIO_NOMBRE"))
  var_clase <- col_first_existing(A, c("CLASE"))
  var_localidad <- col_first_existing(A, c("LOCALIDAD"))
  dic <- .dic_mpios_robot_segmentos(municipios)

  mapa <- A %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::mutate(
      SEGMENTO = .normalizar_texto_robot_segmentos(.data$SEGMENTO),
      codigo_mpio = dplyr::if_else(
        !is.na(.data$SEGMENTO) & stringr::str_detect(.data$SEGMENTO, "^[0-9]{5}_"),
        substr(.data$SEGMENTO, 1, 5),
        NA_character_
      ),
      segmento_corto = dplyr::if_else(
        !is.na(.data$SEGMENTO) & stringr::str_detect(.data$SEGMENTO, "^[0-9]{5}_"),
        sub("^[0-9]{5}_", "", .data$SEGMENTO),
        .data$SEGMENTO
      ),
      NomMunicipio_base = if (!is.null(var_nom)) as.character(.data[[var_nom]]) else NA_character_,
      CLASE = if (!is.null(var_clase)) as.character(.data[[var_clase]]) else NA_character_,
      LOCALIDAD = if (!is.null(var_localidad)) as.character(.data[[var_localidad]]) else NA_character_
    ) %>%
    dplyr::left_join(dic, by = "codigo_mpio") %>%
    dplyr::mutate(
      NomMunicipio = dplyr::coalesce(.data$NomMunicipio_base, .data$NomMunicipio_dic),
      municipio_norm = .normalizar_texto_reporte_muestras(.data$NomMunicipio)
    ) %>%
    dplyr::filter(!is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO)) %>%
    dplyr::select(
      DIRECTORIO,
      SEGMENTO,
      codigo_mpio,
      segmento_corto,
      NomMunicipio,
      municipio_norm,
      CLASE,
      LOCALIDAD
    ) %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)

  filtro_mpio <- .filtro_municipios_robot_segmentos(municipios)
  if (length(filtro_mpio) > 0) {
    mapa <- mapa %>%
      dplyr::filter(
        .data$codigo_mpio %in% filtro_mpio |
          .data$municipio_norm %in% filtro_mpio
      )
  }

  mapa
}

.cruzar_segmentos_robot_segmentos <- function(segmentos_solicitados,
                                              mapa_segmentos,
                                              municipios = NULL) {
  directores <- dplyr::bind_rows(lapply(seq_len(nrow(segmentos_solicitados)), function(i) {
    seg_i <- segmentos_solicitados[i, , drop = FALSE]
    if (isTRUE(seg_i$es_segmento_corto[[1]])) {
      out <- mapa_segmentos %>%
        dplyr::filter(.data$segmento_corto == seg_i$segmento_corto_solicitado[[1]])
    } else {
      out <- mapa_segmentos %>%
        dplyr::filter(.data$SEGMENTO == seg_i$segmento_norm[[1]])
    }

    out %>%
      dplyr::mutate(
        SEGMENTO_solicitado = seg_i$SEGMENTO_solicitado[[1]],
        orden_segmento = seg_i$orden_segmento[[1]],
        .before = 1
      )
  }))

  if (nrow(directores) == 0) {
    directores <- tibble::tibble(
      SEGMENTO_solicitado = character(),
      orden_segmento = integer(),
      DIRECTORIO = character(),
      SEGMENTO = character(),
      codigo_mpio = character(),
      segmento_corto = character(),
      NomMunicipio = character(),
      municipio_norm = character(),
      CLASE = character(),
      LOCALIDAD = character()
    )
  }

  directores <- directores %>%
    dplyr::arrange(.data$orden_segmento, .data$DIRECTORIO) %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE) %>%
    dplyr::select(
      SEGMENTO,
      DIRECTORIO,
      NomMunicipio,
      CLASE,
      LOCALIDAD,
      codigo_mpio,
      segmento_corto,
      SEGMENTO_solicitado
    )

  encontrados <- directores %>%
    dplyr::count(.data$SEGMENTO_solicitado, .data$SEGMENTO, .data$codigo_mpio, .data$NomMunicipio, name = "n_directorios") %>%
    dplyr::arrange(.data$SEGMENTO_solicitado, .data$SEGMENTO)

  no_encontrados <- segmentos_solicitados %>%
    dplyr::anti_join(
      directores %>% dplyr::distinct(.data$SEGMENTO_solicitado),
      by = "SEGMENTO_solicitado"
    ) %>%
    dplyr::select(SEGMENTO_solicitado)

  list(
    directorios = directores,
    segmentos_encontrados = encontrados,
    segmentos_no_encontrados = no_encontrados
  )
}

.capitulos_robot_segmentos <- function(dfs, incluir_capitulos = NULL) {
  caps <- names(dfs)[vapply(dfs, is.data.frame, logical(1))]
  if (!is.null(incluir_capitulos)) {
    incluir_capitulos <- toupper(as.character(incluir_capitulos))
    caps <- intersect(incluir_capitulos, caps)
  }
  caps
}

.filtrar_capitulo_robot_segmentos <- function(df, directorios_segmentos) {
  if (!is.data.frame(df)) {
    return(tibble::tibble())
  }

  contexto <- directorios_segmentos %>%
    dplyr::select(
      DIRECTORIO,
      .ctx_SEGMENTO = SEGMENTO,
      .ctx_NomMunicipio = NomMunicipio,
      .ctx_CLASE = CLASE,
      .ctx_LOCALIDAD = LOCALIDAD
    )

  if (!"DIRECTORIO" %in% names(df)) {
    return(df[0, , drop = FALSE])
  }

  out <- df %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::semi_join(contexto, by = "DIRECTORIO") %>%
    dplyr::left_join(contexto, by = "DIRECTORIO")

  for (nm in c("SEGMENTO", "NomMunicipio", "CLASE", "LOCALIDAD")) {
    if (!nm %in% names(out)) {
      out[[nm]] <- NA_character_
    }
  }

  out$SEGMENTO <- as.character(out$.ctx_SEGMENTO)
  out$NomMunicipio <- dplyr::coalesce(as.character(out$.ctx_NomMunicipio), as.character(out$NomMunicipio))
  out$CLASE <- dplyr::coalesce(as.character(out$.ctx_CLASE), as.character(out$CLASE))
  out$LOCALIDAD <- dplyr::coalesce(as.character(out$.ctx_LOCALIDAD), as.character(out$LOCALIDAD))

  cols_inicio <- intersect(c("SEGMENTO", "DIRECTORIO", "NomMunicipio", "CLASE", "LOCALIDAD"), names(out))
  out %>%
    dplyr::select(dplyr::all_of(cols_inicio), dplyr::everything()) %>%
    dplyr::select(-dplyr::any_of(c(".ctx_SEGMENTO", ".ctx_NomMunicipio", ".ctx_CLASE", ".ctx_LOCALIDAD"))) %>%
    arreglar_utf8_df()
}

.hojas_salida_segmentos_robot <- function(salida_segmentos) {
  if (!is.list(salida_segmentos)) {
    return(list())
  }

  posibles <- list(
    `03_resumen_directorio` = salida_segmentos$resumen_segmento,
    `04_cascada_directorio` = salida_segmentos$cascada_detallada_segmento,
    `05_comparacion_directorio` = salida_segmentos$comparacion_con_sin_tematica,
    `06_resumen_detalle` = salida_segmentos$resumen_detalle_segmento,
    `07_personas_asociadas_E` = salida_segmentos$personas_asociadas_segmento,
    `08_detalle_caidas` = salida_segmentos$detalle_caidas
  )

  posibles[vapply(posibles, is.data.frame, logical(1))]
}

.exportar_robot_inspeccion_segmentos_excel <- function(segmentos_solicitados,
                                                       directorios_segmentos,
                                                       sabana_segmentos,
                                                       salida_segmentos,
                                                       archivo,
                                                       sobrescribir) {
  if (file.exists(archivo) && !isTRUE(sobrescribir)) {
    stop("El archivo ya existe y `sobrescribir = FALSE`: ", archivo)
  }

  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()

  hojas <- c(
    list(
      `01_segmentos_solicitados` = segmentos_solicitados,
      `02_directorios` = directorios_segmentos
    ),
    .hojas_salida_segmentos_robot(salida_segmentos),
    stats::setNames(sabana_segmentos, paste0("cap_", names(sabana_segmentos)))
  )

  for (nm in names(hojas)) {
    x <- hojas[[nm]]
    if (!is.data.frame(x)) {
      next
    }
    x <- x %>%
      dplyr::mutate(
        dplyr::across(where(is.factor), as.character),
        dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
      ) %>%
      arreglar_utf8_df()

    hoja <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, hoja)
    openxlsx::writeData(wb, sheet = hoja, x = x)
    if (ncol(x) > 0) {
      openxlsx::setColWidths(wb, sheet = hoja, cols = seq_len(ncol(x)), widths = "auto")
    }
  }

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
  normalizePath(archivo, winslash = "/", mustWork = FALSE)
}

.exportar_robots_individuales_segmentos <- function(dfs,
                                                    directorios_segmentos,
                                                    carpeta_robots) {
  if (nrow(directorios_segmentos) == 0) {
    return(character())
  }
  dir.create(carpeta_robots, recursive = TRUE, showWarnings = FALSE)

  rutas <- vapply(seq_len(nrow(directorios_segmentos)), function(i) {
    segmento_i <- .sanear_nombre_archivo_robot_segmentos(directorios_segmentos$SEGMENTO[[i]])
    directorio_i <- as.character(directorios_segmentos$DIRECTORIO[[i]])
    ruta_i <- file.path(
      carpeta_robots,
      paste0("robot_", segmento_i, "_DIR_", directorio_i, ".xlsx")
    )

    robot_inspeccion_encuesta(
      dfs = dfs,
      DIRECTORIO = directorio_i,
      exportar_excel = TRUE,
      archivo = ruta_i,
      solo_capitulos_con_datos = FALSE
    )

    normalizePath(ruta_i, winslash = "/", mustWork = FALSE)
  }, character(1))

  rutas
}

.sanear_nombre_archivo_robot_segmentos <- function(x) {
  x <- .normalizar_texto_robot_segmentos(x)[1]
  if (is.na(x) || !nzchar(x)) {
    x <- "segmento_sin_codigo"
  }
  stringr::str_replace_all(x, "[^0-9A-Za-z_-]+", "_")
}
