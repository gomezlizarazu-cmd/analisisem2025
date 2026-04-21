#' Verificar si un DIRECTORIO existe en la base actual
#'
#' Recorre todos los capÃ­tulos disponibles en `dfs` y verifica si un
#' `DIRECTORIO` aparece en al menos uno de ellos. La funciÃ³n ignora los
#' capÃ­tulos que no tienen columna `DIRECTORIO` y normaliza los valores como
#' texto con `trimws()` para facilitar la comparaciÃ³n.
#'
#' Esta funciÃ³n sirve para distinguir casos reportados por Muestras que ya no
#' existen en la base actual `dfs` frente a casos que sÃ­ existen, pero no
#' fueron detectados en `reporte_final_caidas`.
#'
#' @param directorio Identificador de vivienda a verificar.
#' @param dfs Lista nombrada de capÃ­tulos de la base actual.
#'
#' @return `TRUE` si el `DIRECTORIO` existe en al menos un capÃ­tulo de `dfs`,
#'   `FALSE` en caso contrario.
#'
#' @examples
#' \dontrun{
#' existe_en_base(4081956, dfs)
#' }
#'
#' @export
existe_en_base <- function(directorio, dfs) {
  dir_df <- tibble::tibble(DIRECTORIO = directorio) %>%
    normalize_keys("DIRECTORIO")

  directorio <- dir_df$DIRECTORIO[[1]]

  if (is.na(directorio) || !nzchar(directorio) || is.null(dfs) || !is.list(dfs)) {
    return(FALSE)
  }

  any(
    purrr::map_lgl(dfs, function(df) {
      if (!is.data.frame(df) || !"DIRECTORIO" %in% names(df)) {
        return(FALSE)
      }

      df_norm <- normalize_keys(df, "DIRECTORIO")

      any(df_norm$DIRECTORIO == directorio, na.rm = TRUE)
    })
  )
}
