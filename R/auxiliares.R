#' Contar combinaciones distintas de variables de forma segura
#'
#' Cuenta el número de combinaciones distintas para un conjunto de variables
#' presentes en un data frame. Si ninguna de las variables solicitadas existe
#' en la base, devuelve \code{NA_integer_}.
#'
#' @param data Data frame de entrada.
#' @param vars Vector de nombres de variables a considerar.
#'
#' @return Un entero con el número de combinaciones distintas, o
#' \code{NA_integer_} si no hay variables válidas para evaluar.
#'
#' @examples
#' df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
#' n_distinct_safe(df, c("a", "b"))
#' n_distinct_safe(df, c("a", "c"))
#'
#' @export
n_distinct_safe <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (is.null(data) || length(vars) == 0) return(NA_integer_)
  data %>% dplyr::distinct(dplyr::across(dplyr::all_of(vars))) %>% nrow()
}

#' Encontrar la primera columna existente dentro de un conjunto de candidatos
#'
#' Busca, en orden, el primer nombre de variable de \code{candidates} que
#' exista en el data frame suministrado.
#'
#' @param data Data frame de entrada.
#' @param candidates Vector de nombres candidatos.
#'
#' @return Un string con el nombre de la primera columna encontrada, o
#' \code{NULL} si ninguna existe.
#'
#' @examples
#' df <- data.frame(MPIO = 1, DEPTO = 11)
#' col_first_existing(df, c("COD_MPIO", "MPIO", "MUNICIPIO"))
#' col_first_existing(df, c("COD_UPZ", "LOCALIDAD"))
#'
#' @export
col_first_existing <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)][1]
  if (length(hit) == 0 || is.na(hit)) return(NULL)
  hit
}

#' Crear una caja de valor para interfaces Shiny
#'
#' Construye un bloque visual en HTML para mostrar un título, un valor principal
#' y, opcionalmente, un subtítulo. Está pensada para usarse en tableros Shiny.
#'
#' @param title Título de la caja.
#' @param value Valor principal que se desea mostrar.
#' @param subtitle Texto opcional de apoyo.
#' @param meta Meta opcional usada para calcular porcentaje de cumplimiento
#'   y color semaforizado.
#'
#' @return Un objeto HTML tipo \code{shiny.tag}.
#'
#' @details
#' La función usa internamente \code{div()}, \code{tags$div()} y
#' \code{comma()} para construir y formatear la salida.
#'
#' @examples
#' \dontrun{
#' make_value_box("Hogares completos", 15234, "Corte a febrero")
#' }
#'
#' @export
make_value_box <- function(title, value, subtitle = NULL, meta = NULL) {

  # Calcular cumplimiento si hay meta
  if (!is.null(meta) && meta > 0) {
    cumplimiento <- value / meta * 100

    # Semáforo
    color <- dplyr::case_when(
      cumplimiento >= 99 ~ "#198754",   # verde
      cumplimiento >= 95 ~ "#ffc107",   # amarillo
      TRUE ~ "#dc3545"                  # rojo
    )

    texto_meta <- paste0(
      "Meta: ", scales::comma(meta),
      " (", round(cumplimiento, 1), "%)"
    )

  } else {
    texto_meta <- NULL
    color <- "#000000"
  }

  htmltools::div(
    style = "background:#f8f9fa;border-radius:12px;padding:16px;
             box-shadow:0 2px 6px rgba(0,0,0,0.1);",

    htmltools::div(
      style = "font-size:14px;color:#6c757d;",
      title
    ),

    htmltools::div(
      style = paste0("font-size:28px;font-weight:bold;color:", color, ";"),
      scales::comma(value)
    ),

    if (!is.null(texto_meta)) {
      htmltools::div(
        style = paste0("font-size:13px;margin-top:4px;color:", color, ";"),
        texto_meta
      )
    },

    if (!is.null(subtitle)) {
      htmltools::div(
        style = "font-size:12px;color:#6c757d;margin-top:6px;",
        subtitle
      )
    }
  )
}
#' Convertir fechas a \code{Date} de forma segura
#'
#' Convierte un vector a clase \code{Date} intentando primero el formato
#' \code{"%d/%m/%y"} y luego, para los casos no convertidos, el formato
#' \code{"%d/%m/%Y"}.
#'
#' Si el objeto ya es de clase \code{Date}, \code{POSIXct} o \code{POSIXt},
#' simplemente se devuelve como \code{Date}.
#'
#' @param x Vector de entrada con fechas.
#'
#' @return Un vector de clase \code{Date}.
#'
#' @examples
#' safe_date(c("01/02/25", "15/03/2025"))
#'
#' @export
safe_date <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }

  x_chr <- trimws(as.character(x))

  out <- as.Date(x_chr, format = "%d/%m/%y")

  # respaldo por si algunas vienen con año de 4 dígitos
  idx <- is.na(out)
  out[idx] <- as.Date(x_chr[idx], format = "%d/%m/%Y")

  out
}

#' Normalizar texto a UTF-8 (robusto)
#'
#' Convierte un vector a UTF-8, manejando caracteres inválidos y
#' asegurando compatibilidad con exportación a Excel.
#'
#' @param x Vector de entrada.
#'
#' @return Vector de caracteres en UTF-8 limpio.
#' @export
arreglar_utf8 <- function(x) {

  # convertir a character
  x <- as.character(x)

  # intento base con iconv
  x <- iconv(x, from = "", to = "UTF-8", sub = "")

  # fallback robusto con stringi (clave 🔥)
  x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)

  # reemplazar NA
  x[is.na(x)] <- ""

  # limpiar caracteres invisibles raros (opcional pero recomendado)
  x <- stringi::stri_replace_all_regex(x, "[\\p{C}]", "")

  x
}

#' Limpiar data frame completo a UTF-8
#'
#' Aplica `arreglar_utf8()` a todas las columnas de texto de un data frame.
#'
#' @param df Data frame.
#'
#' @return Data frame limpio en UTF-8.
#' @export
arreglar_utf8_df <- function(df) {

  if (!is.data.frame(df)) {
    stop("`df` debe ser un data.frame.")
  }

  # nombres de columnas
  names(df) <- arreglar_utf8(names(df))

  # columnas
  df[] <- lapply(df, function(x) {
    if (is.character(x) || is.factor(x)) {
      arreglar_utf8(x)
    } else {
      x
    }
  })

  df
}

#' Formatear data frame como tabla gt para Quarto
#'
#' Convierte un `data.frame` en una tabla `gt` con formato limpio para
#' visualización en documentos Quarto o HTML.
#'
#' @param data Un data frame.
#' @param titulo Título de la tabla.
#' @param subtitulo Subtítulo opcional.
#' @param labels Vector nombrado opcional para renombrar columnas.
#'   Ejemplo: c(col1 = "Columna 1", col2 = "Columna 2").
#' @param decimals Número de decimales para columnas numéricas.
#' @param pct_cols Vector opcional con nombres de columnas a formatear como
#'   porcentajes.
#' @param pct_decimals Número de decimales para columnas de porcentaje.
#' @param usar_separador_miles Si `TRUE`, usa separador de miles.
#' @param centrar_numericas Si `TRUE`, centra columnas numéricas.
#' @param centrar_todas Si `TRUE`, centra todas las columnas.
#' @param row_striping Si `TRUE`, aplica franjas alternadas a las filas.
#'
#' @return Un objeto de clase `gt_tbl`.
#' @export
tabla_gt <- function(data,
                     titulo = NULL,
                     subtitulo = NULL,
                     labels = NULL,
                     decimals = 0,
                     pct_cols = NULL,
                     pct_decimals = 2,
                     usar_separador_miles = TRUE,
                     centrar_numericas = TRUE,
                     centrar_todas = FALSE,
                     row_striping = TRUE) {

  if (!is.data.frame(data)) {
    stop("`data` debe ser un data.frame.")
  }

  cols_num <- names(data)[vapply(data, is.numeric, logical(1))]

  tabla <- gt::gt(data)

  # 🔹 FORMATO NÚMEROS (excluyendo porcentajes)
  if (length(cols_num) > 0) {
    cols_num_sin_pct <- setdiff(cols_num, pct_cols)

    if (length(cols_num_sin_pct) > 0) {
      tabla <- tabla %>%
        gt::fmt_number(
          columns = dplyr::all_of(cols_num_sin_pct),
          decimals = decimals,
          use_seps = usar_separador_miles
        )
    }
  }

  # 🔹 FORMATO PORCENTAJES
  if (!is.null(pct_cols)) {
    tabla <- tabla %>%
      gt::fmt_percent(
        columns = dplyr::all_of(pct_cols),
        decimals = pct_decimals
      )
  }

  # 🔹 ALINEACIÓN
  if (isTRUE(centrar_todas)) {
    tabla <- tabla %>%
      gt::cols_align(
        align = "center",
        columns = gt::everything()
      )
  } else if (isTRUE(centrar_numericas) && length(cols_num) > 0) {
    tabla <- tabla %>%
      gt::cols_align(
        align = "center",
        columns = dplyr::all_of(cols_num)
      )
  }

  # 🔹 LABELS
  if (!is.null(labels)) {
    labels_validos <- labels[names(labels) %in% names(data)]

    if (length(labels_validos) > 0) {
      tabla <- tabla %>%
        gt::cols_label(.list = as.list(labels_validos))
    }
  }

  # 🔹 TÍTULO
  if (!is.null(titulo) || !is.null(subtitulo)) {
    tabla <- tabla %>%
      gt::tab_header(
        title = titulo,
        subtitle = subtitulo
      )
  }

  # 🔹 FILAS ALTERNADAS
  if (isTRUE(row_striping)) {
    tabla <- tabla %>%
      gt::opt_row_striping()
  }

  tabla
}

#' Exportar tablas a Excel con formato simple
#'
#' Exporta un `data.frame` o una lista nombrada de `data.frame` a un archivo
#' Excel usando `openxlsx`, con encabezados en negrilla y ajuste automatico del
#' ancho de columnas.
#'
#' @param x Objeto a exportar. Puede ser un `data.frame` o una lista nombrada de
#'   `data.frame`.
#' @param ruta Ruta del archivo `.xlsx` de salida.
#' @param hoja Nombre de la hoja cuando `x` es un solo `data.frame`.
#'
#' @return Ruta normalizada del archivo exportado.
#' @export
exportar_tablas_excel <- function(x,
                                  ruta,
                                  hoja = "Resumen") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar a Excel.")
  }

  es_df <- is.data.frame(x)
  es_lista_df <- is.list(x) &&
    length(x) > 0 &&
    all(vapply(x, is.data.frame, logical(1)))

  if (!es_df && !es_lista_df) {
    stop("`x` debe ser un data.frame o una lista nombrada de data.frames.")
  }

  if (es_lista_df) {
    if (is.null(names(x)) || any(names(x) == "")) {
      stop("Si `x` es una lista, debe tener nombres para usar como hojas.")
    }
    hojas <- x
  } else {
    hojas <- list()
    hojas[[hoja]] <- x
  }

  wb <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "bottom"
  )

  for (nm in names(hojas)) {
    hoja_nm <- substr(nm, 1, 31)

    df_out <- hojas[[nm]] %>%
      dplyr::mutate(
        dplyr::across(where(is.factor), as.character),
        dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
      ) %>%
      arreglar_utf8_df()

    openxlsx::addWorksheet(wb, hoja_nm)
    openxlsx::writeData(wb, hoja_nm, df_out)

    openxlsx::addStyle(
      wb,
      sheet = hoja_nm,
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(df_out)),
      gridExpand = TRUE
    )

    openxlsx::setColWidths(
      wb,
      sheet = hoja_nm,
      cols = seq_len(ncol(df_out)),
      widths = "auto"
    )
  }

  openxlsx::saveWorkbook(wb, ruta, overwrite = TRUE)

  normalizePath(ruta, winslash = "/", mustWork = FALSE)
}
