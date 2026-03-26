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

preparar_df_exportacion <- function(df) {
  if (!is.data.frame(df)) {
    stop("`df` debe ser un data.frame.")
  }

  df <- df %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  cols_lista <- names(df)[vapply(df, is.list, logical(1))]
  if (length(cols_lista) > 0) {
    df[cols_lista] <- lapply(df[cols_lista], function(x) vapply(x, toString, character(1)))
  }

  arreglar_utf8_df(df)
}

preparar_caps_flujo <- function(caps_orden) {
  caps_orden <- toupper(caps_orden)
  caps_excluidos <- intersect(caps_orden, names(Edad_objeto))
  caps_flujo <- setdiff(caps_orden, caps_excluidos)

  if (length(caps_excluidos) > 0) {
    message(
      "Se excluyen del flujo secuencial los capítulos condicionados por edad: ",
      paste(caps_excluidos, collapse = ", "),
      ". Evalúelos con la lógica de completitud por elegibilidad."
    )
  }

  caps_flujo
}

#' Expande la presencia del capítulo C a nivel vivienda
#'
#' Identifica y expande la presencia del capítulo C a nivel vivienda. Marca como presentes en capítulo C a todos los hogares de una vivienda
#' (`DIRECTORIO`) cuando al menos uno de sus hogares tiene registro en `df_c`.
#' Esta función sirve para corregir la lógica de cruce del capítulo C cuando
#' dicho capítulo aplica solo al primer hogar de la vivienda.
#'
#' @param base_hogares `data.frame` con los hogares base a evaluar. Debe contener
#'   las columnas `DIRECTORIO` y `SECUENCIA_P`.
#' @param df_c `data.frame` del capítulo C. Debe contener al menos la columna
#'   `DIRECTORIO`.
#'
#' @return Un `data.frame` con las columnas `DIRECTORIO` y `SECUENCIA_P`,
#' correspondiente al subconjunto de `base_hogares` cuya vivienda
#' presenta información en el capítulo C.
#'
#' @details
#' La función no agrega columnas; retorna únicamente los hogares cuya
#' vivienda tiene presencia en capítulo C.
#'
#' La lógica es a nivel vivienda, no a nivel hogar. Si cualquier hogar del mismo
#' `DIRECTORIO` aparece en `df_c`, entonces se considera que la vivienda tiene
#' presencia en capítulo C y esa presencia se expande a todos sus hogares en
#' `base_hogares`.
#'
#' Si `base_hogares` es `NULL`, no es un `data.frame` o está vacío, la función
#' retorna `base_hogares` sin modificar. Si `df_c` es `NULL`, no es un
#' `data.frame`, está vacío o no contiene `DIRECTORIO`, retorna un `data.frame`
#' vacío con las columnas `DIRECTORIO` y `SECUENCIA_P`.
#'
#' @examples
#' base_hogares <- data.frame(
#'   DIRECTORIO = c(1, 1, 2),
#'   SECUENCIA_P = c(1, 2, 1)
#' )
#'
#' df_c <- data.frame(
#'   DIRECTORIO = c(1),
#'   SECUENCIA_P = c(1)
#' )
#'
#' expandir_presencia_capitulo_c(base_hogares, df_c)
#'
#' @export
expandir_presencia_capitulo_c <- function(base_hogares, df_c) {
  if (is.null(base_hogares) || !is.data.frame(base_hogares) || nrow(base_hogares) == 0) {
    return(base_hogares)
  }

  req_base <- c("DIRECTORIO", "SECUENCIA_P")
  if (!all(req_base %in% names(base_hogares))) {
    stop("`base_hogares` debe contener DIRECTORIO y SECUENCIA_P.")
  }

  if (is.null(df_c) || !is.data.frame(df_c) || nrow(df_c) == 0 || !"DIRECTORIO" %in% names(df_c)) {
    return(base_hogares[0, req_base, drop = FALSE])
  }

  base_hogares <- normalize_keys(base_hogares, req_base) %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P)

  directorios_con_c <- df_c %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::distinct(DIRECTORIO)

  base_hogares %>%
    dplyr::inner_join(directorios_con_c, by = "DIRECTORIO") %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P)
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
