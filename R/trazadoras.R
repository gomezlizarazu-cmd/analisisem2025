#' Armar una base consolidada con variables trazadoras de varios capítulos
#'
#' Construye una base a partir de un capítulo de referencia e incorpora
#' variables trazadoras provenientes de otros capítulos, usando las llaves
#' de cruce compatibles según el nivel de observación de cada capítulo.
#'
#' Las variables traídas desde capítulos fuente se renombran con los alias
#' definidos en la tabla de \code{trazadoras}. La función además genera
#' un resumen del proceso de cruce, incluyendo variables faltantes y
#' registros sin correspondencia.
#'
#' @param dfs Lista nombrada de data frames con los capítulos de la encuesta.
#' @param base_cap Capítulo base sobre el cual se construirá la base final.
#' Debe existir en \code{dfs}.
#' @param trazadoras Data frame o tibble con la definición de variables
#' trazadoras. Debe contener las columnas \code{var}, \code{cap} y \code{alias}.
#' @param join Tipo de pegue a utilizar. Puede ser \code{"left"} o
#' \code{"inner"}. Por defecto es \code{"left"}.
#' @param filtrar_elegibles Lógico. Si es \code{TRUE}, aplica al final un
#' filtro por elegibilidad de edad usando \code{Edad_objeto} y la columna
#' \code{edad_var}. Por defecto es \code{FALSE}.
#' @param edad_var Nombre de la variable de edad en la base final. Solo se usa
#' si \code{filtrar_elegibles = TRUE}. Por defecto es \code{"Edad"}.
#'
#' @details
#' La función sigue este flujo:
#' \enumerate{
#'   \item toma \code{base_cap} como base de salida;
#'   \item identifica los capítulos fuente definidos en \code{trazadoras};
#'   \item para cada capítulo fuente, calcula las llaves de cruce compatibles
#'   con \code{get_join_keys()};
#'   \item selecciona únicamente las variables trazadoras presentes en la base;
#'   \item renombra esas variables usando los alias definidos;
#'   \item pega el capítulo fuente a la base acumulada;
#'   \item resume el resultado del cruce, variables faltantes y registros sin match.
#' }
#'
#' Si \code{filtrar_elegibles = TRUE}, la función aplica al final un filtro
#' basado en la edad mínima definida en \code{Edad_objeto} para el capítulo base.
#' Este filtro solo se aplica si existe un umbral definido para \code{base_cap}
#' y si la columna \code{edad_var} está presente en la base resultante.
#'
#' @return
#' Una lista con dos elementos:
#' \describe{
#'   \item{data}{Base final construida a partir de \code{base_cap} y enriquecida
#'   con las variables trazadoras de los demás capítulos.}
#'
#'   \item{resumen}{Tabla resumen del proceso de armado, con información sobre
#'   llaves usadas, tamaños de base y fuente, registros sin match, variables
#'   faltantes y notas del proceso.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- armar_base_con_trazadoras(
#'   dfs = dfs,
#'   base_cap = "E",
#'   trazadoras = trazadoras
#' )
#'
#' names(res)
#' head(res$data)
#' res$resumen
#'
#' # Ejemplo con filtro por elegibilidad
#' res2 <- armar_base_con_trazadoras(
#'   dfs = dfs,
#'   base_cap = "J",
#'   trazadoras = trazadoras,
#'   filtrar_elegibles = TRUE,
#'   edad_var = "Edad"
#' )
#' }
#'
#' @author
#' David Gómez Lizarazú
#'
#' @export
armar_base_con_trazadoras <- function(dfs,
                                      base_cap,
                                      trazadoras,
                                      join = c("left", "inner"),
                                      filtrar_elegibles = FALSE,
                                      edad_var = "Edad") {

  join <- match.arg(join)
  base_cap <- toupper(base_cap)

  if (!base_cap %in% names(dfs)) stop(paste0("No encuentro base_cap en dfs: ", base_cap))
  if (!all(c("var","cap","alias") %in% names(trazadoras))) stop("trazadoras debe tener columnas: var, cap, alias")

  # Base
  df_base <- dfs[[base_cap]]
  keys_base <- get_join_keys(base_cap)
  df_base <- normalize_keys(df_base, keys_base)

  df_out <- df_base

  # Fuentes a traer (capítulos distintos al base)
  traz2 <- trazadoras %>%
    dplyr::mutate(cap = toupper(cap)) %>%
    dplyr::filter(cap != base_cap)

  caps_fuente <- unique(traz2$cap)

  resumen <- list()

  for (cap_src in caps_fuente) {

    if (!cap_src %in% names(dfs)) {
      resumen[[length(resumen)+1]] <- tibble::tibble(
        cap_base = base_cap,
        cap_src  = cap_src,
        join_keys = NA_character_,
        n_base = nrow(df_out),
        p_base = ncol(df_out),
        n_src = NA_integer_,
        p_src = NA_integer_,
        n_result = nrow(df_out),
        p_result = ncol(df_out),
        n_sin_match = NA_integer_,
        pct_sin_match = NA_real_,
        vars_faltantes = NA_character_,
        nota = "Capítulo fuente no está cargado en dfs"
      )
      next
    }

    df_src <- dfs[[cap_src]]

    # Llaves esperadas
    keys_src <- get_join_keys(cap_src)

    # Llaves de cruce: usar llaves de la BASE pero solo las que existan en la fuente
    keys_use <- intersect(keys_base, keys_src)
    if (length(keys_use) == 0) {
      resumen[[length(resumen)+1]] <- tibble::tibble(
        cap_base = base_cap,
        cap_src  = cap_src,
        join_keys = NA_character_,
        n_base = nrow(df_out),
        p_base = ncol(df_out),
        n_src = nrow(df_src),
        p_src = ncol(df_src),
        n_result = nrow(df_out),
        p_result = ncol(df_out),
        n_sin_match = NA_integer_,
        pct_sin_match = NA_real_,
        vars_faltantes = NA_character_,
        nota = "Sin llaves comunes base-fuente"
      )
      next
    }

    # Variables a traer desde este capítulo
    vars_src <- traz2 %>%
      dplyr::filter(cap == cap_src) %>%
      dplyr::pull(var) %>%
      unique()

    # Quedarnos solo con las que existan realmente
    vars_src_presentes <- intersect(vars_src, names(df_src))
    faltantes <- setdiff(vars_src, vars_src_presentes)

    # Mapeo var -> alias
    mapeo <- traz2 %>%
      dplyr::filter(cap == cap_src, var %in% vars_src_presentes) %>%
      dplyr::distinct(var, alias)

    df_src_slim <- df_src %>%
      normalize_keys(keys_use) %>%
      dplyr::select(dplyr::any_of(c(keys_use, vars_src_presentes)))

    # Renombrar uno por uno (robusto)
    for (i in seq_len(nrow(mapeo))) {
      old <- mapeo$var[i]
      new <- mapeo$alias[i]
      if (old %in% names(df_src_slim)) {
        names(df_src_slim)[names(df_src_slim) == old] <- new
      }
    }

    df_src_slim <- df_src_slim %>% dplyr::distinct()

    n_base0 <- nrow(df_out); p_base0 <- ncol(df_out)
    n_src0  <- nrow(df_src_slim); p_src0 <- ncol(df_src_slim)

    # Marcador para aproximar "sin match"
    marca <- paste0(".__match_", cap_src)
    df_src_slim[[marca]] <- 1L

    if (join == "left") {
      df_out <- dplyr::left_join(df_out, df_src_slim, by = keys_use)
    } else {
      df_out <- dplyr::inner_join(df_out, df_src_slim, by = keys_use)
    }

    n_sin_match <- sum(is.na(df_out[[marca]]))
    df_out[[marca]] <- NULL

    resumen[[length(resumen)+1]] <- tibble::tibble(
      cap_base = base_cap,
      cap_src  = cap_src,
      join_keys = paste(keys_use, collapse = ", "),
      n_base = n_base0,
      p_base = p_base0,
      n_src = n_src0,
      p_src = p_src0,
      n_result = nrow(df_out),
      p_result = ncol(df_out),
      n_sin_match = as.integer(n_sin_match),
      pct_sin_match = round(100 * n_sin_match / nrow(df_out), 2),
      vars_faltantes = if (length(faltantes) == 0) NA_character_ else paste(faltantes, collapse = ", "),
      nota = NA_character_
    )
  }

  # ---------------------------------------
  # Filtro opcional por elegibilidad (edad)
  # ---------------------------------------
  if (isTRUE(filtrar_elegibles)) {

    umbral <- Edad_objeto[[base_cap]]

    if (is.null(umbral)) {
      warning(paste0("No hay umbral de edad definido en Edad_objeto para capítulo ", base_cap,
                     ". No se aplicó filtro."))
    } else if (!(edad_var %in% names(df_out))) {
      warning(paste0("No existe la columna ", edad_var, " en la base final. No se aplicó filtro."))
    } else {
      edad_num <- suppressWarnings(as.numeric(df_out[[edad_var]]))

      n_antes <- nrow(df_out)

      if (umbral < 5) {
        df_out <- df_out %>% dplyr::filter(!is.na(edad_num) & edad_num < 5)
        regla_txt <- "Edad < 5"
      } else {
        df_out <- df_out %>% dplyr::filter(!is.na(edad_num) & edad_num >= umbral)
        regla_txt <- paste0("Edad >= ", umbral)
      }

      n_despues <- nrow(df_out)

      # Dejar constancia en el resumen
      resumen_fil <- tibble::tibble(
        cap_base = base_cap,
        cap_src  = "__FILTRO_EDAD__",
        join_keys = NA_character_,
        n_base = n_antes,
        p_base = NA_integer_,
        n_src = NA_integer_,
        p_src = NA_integer_,
        n_result = n_despues,
        p_result = ncol(df_out),
        n_sin_match = NA_integer_,
        pct_sin_match = NA_real_,
        vars_faltantes = NA_character_,
        nota = paste0("Aplicado filtro: ", regla_txt)
      )

      resumen <- c(resumen, list(resumen_fil))
    }
  }

  list(
    data = df_out,
    resumen = dplyr::bind_rows(resumen)
  )
}
