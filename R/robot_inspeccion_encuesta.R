#' Robot de inspeccion puntual de una encuesta
#'
#' Filtra todos los capitulos de la encuesta para una llave especifica y devuelve
#' un resumen por capitulo junto con el detalle completo de los registros
#' encontrados. La llave puede darse a nivel de vivienda, hogar o persona.
#'
#' @details
#' La interpretacion de la llave sigue la estructura jerarquica del paquete:
#'
#' - vivienda: `DIRECTORIO`
#' - hogar: `DIRECTORIO + SECUENCIA_P`
#' - persona: `DIRECTORIO + SECUENCIA_P + ORDEN`
#'
#' Reglas de filtrado:
#'
#' - si se consulta una vivienda, se traen todos los hogares y personas de esa
#'   vivienda;
#' - si se consulta un hogar, se trae la vivienda asociada, ese hogar y todas
#'   las personas del hogar;
#' - si se consulta una persona, se trae la vivienda asociada, el hogar
#'   correspondiente y solo esa persona.
#'
#' Todos los filtros usan `normalize_keys()` sobre las llaves presentes.
#'
#' @param dfs Lista nombrada de data frames por capitulo.
#' @param DIRECTORIO Identificador de vivienda.
#' @param SECUENCIA_P Identificador de hogar. Opcional.
#' @param ORDEN Identificador de persona. Opcional.
#' @param exportar_excel Si es `TRUE`, exporta el resumen y el detalle a Excel.
#' @param archivo Ruta del archivo Excel.
#' @param solo_capitulos_con_datos Si es `TRUE`, omite del detalle los capitulos
#'   sin registros para la llave consultada.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{parametros}{Tabla con la llave consultada y el nivel interpretado.}
#'   \item{resumen}{Resumen por capitulo con nivel, numero de filas y numero de
#'   llaves distintas encontradas.}
#'   \item{detalle}{Lista de data frames, uno por capitulo, con todas las
#'   columnas disponibles para la llave consultada.}
#'   \item{archivo}{Ruta del Excel exportado cuando `exportar_excel = TRUE`.}
#' }
#'
#' @examples
#' \dontrun{
#' caso_viv <- robot_inspeccion_encuesta(
#'   dfs = dfs,
#'   DIRECTORIO = 4002617
#' )
#'
#' caso_hog <- robot_inspeccion_encuesta(
#'   dfs = dfs,
#'   DIRECTORIO = 4002617,
#'   SECUENCIA_P = 2
#' )
#'
#' caso_per <- robot_inspeccion_encuesta(
#'   dfs = dfs,
#'   DIRECTORIO = 4002617,
#'   SECUENCIA_P = 2,
#'   ORDEN = 2,
#'   exportar_excel = TRUE
#' )
#' }
#'
#' @export
robot_inspeccion_encuesta <- function(
    dfs,
    DIRECTORIO,
    SECUENCIA_P = NULL,
    ORDEN = NULL,
    exportar_excel = FALSE,
    archivo = "robot_inspeccion_encuesta.xlsx",
    solo_capitulos_con_datos = FALSE
) {

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres de capitulos.")
  }

  if (missing(DIRECTORIO) || length(DIRECTORIO) != 1 || is.na(DIRECTORIO)) {
    stop("`DIRECTORIO` debe venir informado con un unico valor.")
  }

  if (!is.null(ORDEN) && is.null(SECUENCIA_P)) {
    stop("Si informa `ORDEN`, tambien debe informar `SECUENCIA_P`.")
  }

  if (exportar_excel && !requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  names(dfs) <- toupper(names(dfs))

  nivel_consulta <- if (!is.null(ORDEN)) {
    "persona"
  } else if (!is.null(SECUENCIA_P)) {
    "hogar"
  } else {
    "vivienda"
  }

  llave_consulta <- list(
    DIRECTORIO = as.character(DIRECTORIO),
    SECUENCIA_P = if (!is.null(SECUENCIA_P)) as.character(SECUENCIA_P) else NULL,
    ORDEN = if (!is.null(ORDEN)) as.character(ORDEN) else NULL
  )

  parametros <- tibble::tibble(
    parametro = c("nivel_consulta", "DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    valor = c(
      nivel_consulta,
      llave_consulta$DIRECTORIO,
      if (is.null(llave_consulta$SECUENCIA_P)) NA_character_ else llave_consulta$SECUENCIA_P,
      if (is.null(llave_consulta$ORDEN)) NA_character_ else llave_consulta$ORDEN
    )
  )

  detalle <- lapply(names(dfs), function(cap) {
    df_cap <- dfs[[cap]]
    if (!is.data.frame(df_cap)) {
      return(NULL)
    }

    .filtrar_capitulo_inspeccion(
      df = df_cap,
      cap = cap,
      llave_consulta = llave_consulta,
      nivel_consulta = nivel_consulta
    )
  })
  names(detalle) <- names(dfs)

  resumen <- dplyr::bind_rows(lapply(names(detalle), function(cap) {
    df_cap <- detalle[[cap]]
    if (is.null(df_cap)) {
      return(NULL)
    }

    tibble::tibble(
      capitulo = cap,
      nivel_capitulo = if (!is.null(tipo_capitulo[[cap]])) tipo_capitulo[[cap]] else NA_character_,
      nivel_consulta = nivel_consulta,
      encontro_registros = nrow(df_cap) > 0,
      n_filas = nrow(df_cap),
      n_viviendas = .n_keys(df_cap, intersect("DIRECTORIO", names(df_cap))),
      n_hogares = .n_keys(df_cap, intersect(c("DIRECTORIO", "SECUENCIA_P"), names(df_cap))),
      n_personas = .n_keys(df_cap, intersect(c("DIRECTORIO", "SECUENCIA_P", "ORDEN"), names(df_cap)))
    )
  }))

  if (isTRUE(solo_capitulos_con_datos)) {
    caps_keep <- resumen %>%
      dplyr::filter(.data$encontro_registros) %>%
      dplyr::pull(.data$capitulo)
    detalle <- detalle[caps_keep]
    resumen <- resumen %>%
      dplyr::filter(.data$capitulo %in% caps_keep)
  }

  salida <- list(
    parametros = parametros,
    resumen = resumen,
    detalle = detalle
  )

  if (isTRUE(exportar_excel)) {
    .exportar_robot_inspeccion_excel(
      parametros = parametros,
      resumen = resumen,
      detalle = detalle,
      archivo = archivo
    )
    salida$archivo <- normalizePath(archivo, winslash = "/", mustWork = FALSE)
  }

  salida
}

.filtrar_capitulo_inspeccion <- function(df, cap, llave_consulta, nivel_consulta) {
  tipo_cap <- tipo_capitulo[[cap]]
  keys_presentes <- intersect(c("DIRECTORIO", "SECUENCIA_P", "ORDEN"), names(df))

  if (length(keys_presentes) > 0) {
    df <- normalize_keys(df, keys_presentes)
  }

  valor_dir <- llave_consulta$DIRECTORIO
  valor_hog <- llave_consulta$SECUENCIA_P
  valor_per <- llave_consulta$ORDEN

  if (!"DIRECTORIO" %in% names(df)) {
    return(df[0, , drop = FALSE])
  }

  df <- df %>%
    dplyr::filter(as.character(.data$DIRECTORIO) == valor_dir)

  if (nrow(df) == 0) {
    return(df)
  }

  if (nivel_consulta == "vivienda") {
    return(df)
  }

  if (tipo_cap == "vivienda") {
    return(df)
  }

  if (!"SECUENCIA_P" %in% names(df)) {
    return(df[0, , drop = FALSE])
  }

  df <- df %>%
    dplyr::filter(as.character(.data$SECUENCIA_P) == valor_hog)

  if (nrow(df) == 0) {
    return(df)
  }

  if (nivel_consulta == "hogar") {
    return(df)
  }

  if (tipo_cap %in% c("hogar")) {
    return(df)
  }

  if (!"ORDEN" %in% names(df)) {
    return(df[0, , drop = FALSE])
  }

  df %>%
    dplyr::filter(as.character(.data$ORDEN) == valor_per)
}

.exportar_robot_inspeccion_excel <- function(parametros, resumen, detalle, archivo) {
  wb <- openxlsx::createWorkbook()

  tablas <- c(
    list(
      parametros = parametros,
      resumen = resumen
    ),
    detalle
  )

  for (nm in names(tablas)) {
    x <- tablas[[nm]]

    if (!is.data.frame(x)) {
      next
    }

    x <- x %>%
      dplyr::mutate(
        dplyr::across(where(is.factor), as.character),
        dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
      )
    x <- arreglar_utf8_df(x)

    sheet_name <- substr(
      if (nm %in% c("parametros", "resumen")) nm else paste0("cap_", nm),
      1,
      31
    )

    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, x = x)
  }

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
}
