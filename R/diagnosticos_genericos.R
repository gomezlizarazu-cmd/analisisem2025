#' Diagnosticar columnas requeridas
#'
#' Construye una tabla simple que indica si cada columna requerida existe en
#' una base. Es util como chequeo previo antes de preparar llaves de cruce o
#' diagnosticos exploratorios.
#'
#' @param df Data frame que se desea revisar.
#' @param cols_requeridas Vector character con los nombres esperados.
#' @param nombre_base Etiqueta de la base revisada.
#'
#' @return Tibble con columnas \code{base}, \code{variable} y \code{existe}.
#'
#' @examples
#' diagnosticar_columnas_requeridas(
#'   data.frame(a = 1),
#'   c("a", "b"),
#'   "demo"
#' )
#'
#' @export
diagnosticar_columnas_requeridas <- function(df, cols_requeridas, nombre_base) {
  if (!is.data.frame(df)) {
    stop("`df` debe ser un data.frame.")
  }

  tibble::tibble(
    base = nombre_base,
    variable = cols_requeridas,
    existe = cols_requeridas %in% names(df)
  )
}

#' Diagnosticar una variable basica
#'
#' Resume clase, tamanio, faltantes, vacios, valores unicos y hasta tres
#' ejemplos no faltantes de una variable.
#'
#' @param df Data frame de entrada.
#' @param var Nombre de la variable a diagnosticar.
#' @param nombre_base Etiqueta de la base revisada.
#'
#' @return Tibble de una fila con metricas basicas de la variable.
#'
#' @examples
#' diagnosticar_variable_basica(
#'   data.frame(x = c("a", "", NA, "b")),
#'   "x",
#'   "demo"
#' )
#'
#' @export
diagnosticar_variable_basica <- function(df, var, nombre_base) {
  if (!is.data.frame(df)) {
    stop("`df` debe ser un data.frame.")
  }
  if (!is.character(var) || length(var) != 1 || is.na(var) || !nzchar(var)) {
    stop("`var` debe ser un nombre de variable unico.")
  }
  if (!var %in% names(df)) {
    stop("No existe `var` en `df`: ", var)
  }

  x <- df[[var]]
  idx_no_na <- which(!is.na(x))

  tibble::tibble(
    base = nombre_base,
    variable = var,
    clase = paste(class(x), collapse = " / "),
    n = length(x),
    n_na = sum(is.na(x)),
    pct_na = round(100 * mean(is.na(x)), 4),
    n_vacios = sum(stringr::str_squish(as.character(x)) == "", na.rm = TRUE),
    n_unicos = dplyr::n_distinct(x, na.rm = TRUE),
    ejemplo_1 = if (length(idx_no_na) >= 1) as.character(x[idx_no_na[1]]) else NA_character_,
    ejemplo_2 = if (length(idx_no_na) >= 2) as.character(x[idx_no_na[2]]) else NA_character_,
    ejemplo_3 = if (length(idx_no_na) >= 3) as.character(x[idx_no_na[3]]) else NA_character_
  )
}
