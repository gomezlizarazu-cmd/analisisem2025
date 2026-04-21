#' Colapsar texto limpio y sin duplicados
#'
#' Limpia un vector de texto, elimina valores faltantes o vacios y devuelve una
#' cadena unica separada por `" | "`. Opcionalmente puede separar primero los
#' tokens internos usando el separador `" | "`, lo que resulta util para
#' columnas como `criterios_reporte`.
#'
#' @param x Vector de entrada.
#' @param split_tokens Si es `TRUE`, separa previamente los valores usando el
#'   patron `\\s*\\|\\s*` antes de limpiar y volver a unir.
#'
#' @return Un `character(1)` con valores unicos y ordenados, o
#'   `NA_character_` si no hay contenido util.
#'
#' @examples
#' collapse_clean(c("campo", "lina", NA, "campo"))
#' collapse_clean(c("campo | tematica", "lina", "tematica"), split_tokens = TRUE)
#'
#' @export
collapse_clean <- function(x, split_tokens = FALSE) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]

  if (length(x) == 0) {
    return(NA_character_)
  }

  if (isTRUE(split_tokens)) {
    x <- unlist(strsplit(paste(x, collapse = " | "), "\\s*\\|\\s*"))
    x <- x[!is.na(x) & nzchar(trimws(x))]
  }

  x <- unique(trimws(x))

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(sort(x), collapse = " | ")
}
