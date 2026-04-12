#' Normalizar llaves de cruce
#'
#' Convierte a texto y elimina espacios al inicio y al final en las variables
#' llave indicadas, con el fin de estandarizar los campos usados en joins.
#'
#' @param df Data frame de entrada.
#' @param keys Vector de nombres de columnas que se desean normalizar.
#'
#' @return Un data frame con las llaves presentes convertidas a \code{character}
#' y depuradas con \code{str_trim()}.
#'
#' @details
#' Si \code{df} es \code{NULL}, no es un data frame o tiene cero filas, la
#' función devuelve el objeto sin modificar. Si ninguna de las llaves existe
#' en la base, también devuelve el objeto original.
#'
#' @examples
#' df <- data.frame(DIRECTORIO = c(" 001 ", "002"), SECUENCIA_P = c(1, 2))
#' normalize_keys(df, c("DIRECTORIO", "SECUENCIA_P"))
#'
#' @export
normalize_keys <- function(df, keys) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

  present <- intersect(keys, names(df))
  if (length(present) == 0) return(df)

  df %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(present), ~ stringr::str_trim(as.character(.x)))
    )
}

#' Pegar varios capítulos de la encuesta en una sola base
#'
#' Construye una base consolidada a partir de una lista de capítulos,
#' realizando joins secuenciales sobre un capítulo base y usando las llaves
#' esperadas según la tipología de cada capítulo.
#'
#' @param dfs Lista nombrada de data frames, por ejemplo
#' \code{list(E = dfE, H = dfH)}.
#' @param base_cap Capítulo base de la consolidación. Si es \code{NULL},
#' se usa el primer capítulo de \code{caps_orden}.
#' @param caps_orden Vector opcional que indica el orden de pegue de los
#' capítulos. Si es \code{NULL}, se usan los nombres de \code{dfs}.
#' @param join Tipo de pegue. Puede ser \code{"left"} o \code{"inner"}.
#' @param edad_var Nombre de la variable de edad en la base final.
#' Por defecto es \code{"Edad"}.
#'
#' @details
#' La función:
#' \enumerate{
#'   \item identifica el capítulo base;
#'   \item obtiene sus llaves esperadas con \code{get_join_keys()};
#'   \item pega secuencialmente los demás capítulos;
#'   \item elimina variables duplicadas del capítulo a pegar, excepto las llaves;
#'   \item produce un resumen estructural del proceso;
#'   \item calcula métricas de elegibilidad por edad con \code{calc_eligibles()}.
#' }
#'
#' El resumen incluye número de filas y columnas antes y después de cada pegue,
#' así como métricas finales de cobertura de edad y elegibilidad.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{data}{Base consolidada final.}
#'   \item{resumen}{Tibble con el detalle de cada paso del pegue y métricas
#'   finales de elegibilidad.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- pegar_tablas(
#'   dfs = list(E = E, H = H, J = J),
#'   base_cap = "E",
#'   join = "left"
#' )
#'
#' names(res)
#' res$resumen
#' }
#'
#' @export
pegar_tablas <- function(dfs,
                         base_cap = NULL,
                         caps_orden = NULL,
                         join = c("left", "inner"),
                         edad_var = "Edad") {
  join <- match.arg(join)

  if (!is.list(dfs) || length(dfs) < 1) {
    stop("dfs debe ser una lista con >=1 tabla")
  }

  if (is.null(caps_orden)) {
    caps_orden <- names(dfs)
    if (is.null(caps_orden) || any(caps_orden == "")) {
      stop("Pon nombres a la lista dfs (ej: list(E=dfE, H=dfH)) o pasa caps_orden explícito.")
    }
  }

  caps_orden <- toupper(caps_orden)

  if (is.null(base_cap)) base_cap <- caps_orden[1]
  base_cap <- toupper(base_cap)

  if (!(base_cap %in% caps_orden)) stop("base_cap no está en caps_orden")

  caps_orden <- c(base_cap, setdiff(caps_orden, base_cap))

  df_base <- dfs[[base_cap]]
  if (is.null(df_base) || !is.data.frame(df_base)) stop(paste0("No encuentro data.frame para capítulo base: ", base_cap))

  keys_base <- get_join_keys(base_cap)
  df_base <- normalize_keys(df_base, keys_base)

  resumen <- list()
  resumen[[1]] <- tibble::tibble(
    paso = 0L,
    cap_base = base_cap,
    cap_pega = NA_character_,
    join_keys = paste(keys_base, collapse = ", "),
    n_base = nrow(df_base),
    p_base = ncol(df_base),
    n_pega = NA_integer_,
    p_pega = NA_integer_,
    n_result = nrow(df_base),
    p_result = ncol(df_base)
  )

  df_out <- df_base

  if (length(caps_orden) > 1) {
    for (i in 2:length(caps_orden)) {

      cap_i <- caps_orden[i]
      df_i  <- dfs[[cap_i]]

      if (is.null(df_i) || !is.data.frame(df_i)) {
        stop(paste0("No encuentro data.frame para capítulo a pegar: ", cap_i))
      }

      keys_i <- get_join_keys(cap_i)
      keys_use <- intersect(keys_base, keys_i)

      if (length(keys_use) == 0) {
        stop(paste0(
          "No hay llaves comunes entre base(", base_cap, ") y ", cap_i,
          ". Revisa tipo_capitulo o nombres de columnas."
        ))
      }

      df_out <- normalize_keys(df_out, keys_use)
      df_i   <- normalize_keys(df_i, keys_use)

      vars_dup <- intersect(names(df_out), names(df_i))
      vars_dup <- setdiff(vars_dup, keys_use)

      if (length(vars_dup) > 0) {
        df_i <- df_i %>%
          dplyr::select(-dplyr::any_of(vars_dup))
      }

      n_base <- nrow(df_out); p_base <- ncol(df_out)
      n_pega <- nrow(df_i);   p_pega <- ncol(df_i)

      if (join == "left") {
        df_out <- dplyr::left_join(df_out, df_i, by = keys_use)
      } else {
        df_out <- dplyr::inner_join(df_out, df_i, by = keys_use)
      }

      resumen[[length(resumen) + 1]] <- tibble::tibble(
        paso = as.integer(i - 1),
        cap_base = base_cap,
        cap_pega = cap_i,
        join_keys = paste(keys_use, collapse = ", "),
        n_base = n_base,
        p_base = p_base,
        n_pega = n_pega,
        p_pega = p_pega,
        n_result = nrow(df_out),
        p_result = ncol(df_out)
      )
    }
  }

  resumen_tbl <- dplyr::bind_rows(resumen)

  elig <- calc_eligibles(df_out, cap_obj = base_cap, edad_var = edad_var)

  n_total <- nrow(df_out)

  if (edad_var %in% names(df_out)) {
    edad_num <- suppressWarnings(as.numeric(df_out[[edad_var]]))
    n_con_edad <- sum(!is.na(edad_num))
    n_sin_edad <- sum(is.na(edad_num))
  } else {
    n_con_edad <- NA_integer_
    n_sin_edad <- NA_integer_
  }

  n_eligible <- elig$n_eligible
  pct_eligible_total <- if (!is.na(n_eligible) && n_total > 0) round(100 * n_eligible / n_total, 2) else NA_real_
  pct_eligible_con_edad <- if (!is.na(n_eligible) && !is.na(n_con_edad) && n_con_edad > 0) round(100 * n_eligible / n_con_edad, 2) else NA_real_

  resumen_tbl <- resumen_tbl %>%
    dplyr::mutate(
      edad_umbral = elig$umbral,
      regla_edad = elig$regla,
      n_total_final = n_total,
      n_con_edad = n_con_edad,
      n_sin_edad = n_sin_edad,
      n_eligible = n_eligible,
      pct_eligible_total = pct_eligible_total,
      pct_eligible_con_edad = pct_eligible_con_edad
    )

  list(
    data = df_out,
    resumen = resumen_tbl
  )
}

#' Obtener llaves de cruce esperadas por capítulo
#'
#' Devuelve las variables llave esperadas para un capítulo de la encuesta,
#' según su nivel de observación definido en \code{tipo_capitulo}.
#'
#' @param cap Nombre del capítulo.
#'
#' @return Un vector de caracteres con los nombres de las llaves esperadas.
#'
#' @details
#' La correspondencia es:
#' \itemize{
#'   \item vivienda: \code{DIRECTORIO}
#'   \item hogar: \code{DIRECTORIO, SECUENCIA_P}
#'   \item persona: \code{DIRECTORIO, SECUENCIA_P, ORDEN}
#' }
#'
#' @examples
#' get_join_keys("A")
#' get_join_keys("E")
#'
#' @export
get_join_keys <- function(cap) {

  cap <- toupper(cap)

  if (!cap %in% names(tipo_capitulo)) {
    stop(paste0("Capítulo no reconocido: ", cap))
  }

  tipo <- tipo_capitulo[[cap]]

  switch(
    tipo,
    vivienda = c("DIRECTORIO"),
    hogar    = c("DIRECTORIO", "SECUENCIA_P"),
    persona  = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    stop(paste0("Tipo de capítulo no soportado: ", tipo))
  )
}

#' Calcular elegibilidad por edad para un capítulo
#'
#' Calcula cuántos registros de una base cumplen la regla de edad definida
#' para un capítulo en el objeto \code{Edad_objeto}.
#'
#' @param df Data frame de entrada.
#' @param cap_obj Capítulo objetivo sobre el cual se evaluará la elegibilidad.
#' @param edad_var Nombre de la variable de edad. Por defecto es \code{"Edad"}.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{umbral}{Umbral de edad definido para el capítulo.}
#'   \item{regla}{Texto descriptivo de la regla aplicada.}
#'   \item{n_eligible}{Número de registros elegibles.}
#' }
#'
#' @details
#' Si el capítulo no tiene regla de edad definida o si la variable de edad no
#' existe en la base, la función devuelve \code{n_eligible = NA_integer_}.
#'
#' @examples
#' \dontrun{
#' calc_eligibles(df, cap_obj = "J", edad_var = "Edad")
#' }
#'
#' @export
calc_eligibles <- function(df, cap_obj, edad_var = "Edad") {
  cap_obj <- toupper(cap_obj)

  umbral <- Edad_objeto[[cap_obj]]

  if (is.null(umbral)) {
    return(list(
      umbral = NA_real_,
      regla = "Capítulo sin regla de edad",
      n_eligible = NA_integer_
    ))
  }

  if (!(edad_var %in% names(df))) {
    return(list(
      umbral = umbral,
      regla = "Edad no encontrada en base",
      n_eligible = NA_integer_
    ))
  }

  edad <- suppressWarnings(as.numeric(df[[edad_var]]))

  if (umbral < 5) {
    n_eligible <- sum(!is.na(edad) & edad < 5)
    regla_txt <- "Edad < 5"
  } else {
    n_eligible <- sum(!is.na(edad) & edad >= umbral)
    regla_txt <- paste0("Edad >= ", umbral)
  }

  list(
    umbral = umbral,
    regla = regla_txt,
    n_eligible = as.integer(n_eligible)
  )
}
#' Leer tablas en distintos formatos de archivo
#'
#' Lee un archivo de datos en formato CSV, Excel, TXT o TSV y lo devuelve
#' como tibble.
#'
#' @param path Ruta del archivo a leer.
#'
#' @return Un tibble con la información leída.
#'
#' @details
#' Los formatos soportados son:
#' \itemize{
#'   \item \code{.csv}
#'   \item \code{.xlsx}
#'   \item \code{.xls}
#'   \item \code{.txt}
#'   \item \code{.tsv}
#' }
#'
#' Para archivos Excel, la función lee la primera hoja por defecto.
#'
#' @examples
#' \dontrun{
#' df <- leer_tabla_flexible("datos/capitulo_E.csv")
#' df2 <- leer_tabla_flexible("datos/base.xlsx")
#' }
#'
#' @export
leer_tabla_flexible <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv")) {
    return(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
  }

  if (ext %in% c("xlsx", "xls")) {
    return(
      readxl::read_excel(path) %>%
        as.data.frame() %>%
        tibble::as_tibble()
    )
  }

  if (ext %in% c("txt", "tsv")) {
    return(readr::read_delim(path, delim = "\t", show_col_types = FALSE, progress = FALSE))
  }

  stop(paste0("Extensión no soportada: ", ext, " | archivo: ", basename(path)))
}

#' Cargar capítulos de encuesta por fecha de corte
#'
#' Encapsula el flujo operativo de carga de archivos de capítulos para una fecha
#' específica, construyendo la lista \code{dfs} y un resumen inicial por capítulo.
#'
#' @param fecha_corte Fecha de corte en formato \code{"YYYYMMDD"}.
#' @param carpeta_raiz Carpeta raíz donde están las carpetas de capítulos.
#' @param prefijo_carpeta Prefijo de la carpeta de capítulos. Por defecto
#'   \code{"CAP_EM_"}.
#' @param orden_caps Orden deseado de capítulos en la salida. Por defecto
#'   \code{c(LETTERS[1:13], "MA", "MB")}.
#' @param verbose Si es \code{TRUE}, muestra mensajes de avance.
#'
#' @return Una lista con:
#' \describe{
#'   \item{dfs}{Lista nombrada de capítulos cargados y ordenados.}
#'   \item{resumen_carga}{Tibble con \code{cap}, \code{n} y \code{p}.}
#'   \item{carpeta_caps}{Ruta de la carpeta usada para cargar archivos.}
#'   \item{archivos}{Vector con rutas completas de los archivos encontrados.}
#' }
#'
#' @details
#' El patrón de búsqueda soporta archivos:
#' \code{csv}, \code{xlsx}, \code{xls}, \code{txt} y \code{tsv}, con nombres
#' del tipo \code{CAP_A_YYYYMMDD.csv}, \code{CAP_MA_YYYYMMDD.xlsx}, etc.
#'
#' @examples
#' \dontrun{
#' out <- cargar_capitulos_por_fecha(
#'   fecha_corte = "20260409",
#'   carpeta_raiz = "C:/ruta/proyecto/Validar"
#' )
#'
#' names(out$dfs)
#' out$resumen_carga
#' }
#'
#' @export
cargar_capitulos_por_fecha <- function(
    fecha_corte,
    carpeta_raiz,
    prefijo_carpeta = "CAP_EM_",
    orden_caps = c(LETTERS[1:13], "MA", "MB"),
    verbose = TRUE
) {

  if (!is.character(fecha_corte) || length(fecha_corte) != 1 || is.na(fecha_corte) || !nzchar(fecha_corte)) {
    stop("`fecha_corte` debe ser un string no vacío en formato 'YYYYMMDD'.")
  }

  if (!is.character(carpeta_raiz) || length(carpeta_raiz) != 1 || is.na(carpeta_raiz) || !nzchar(carpeta_raiz)) {
    stop("`carpeta_raiz` debe ser una ruta de carpeta válida.")
  }

  if (!is.character(prefijo_carpeta) || length(prefijo_carpeta) != 1 || is.na(prefijo_carpeta) || !nzchar(prefijo_carpeta)) {
    stop("`prefijo_carpeta` debe ser un string no vacío.")
  }

  carpeta_caps <- file.path(carpeta_raiz, paste0(prefijo_carpeta, fecha_corte))

  if (!dir.exists(carpeta_caps)) {
    stop(paste0("No existe la carpeta: ", carpeta_caps))
  }

  if (isTRUE(verbose)) {
    message("Usando carpeta: ", carpeta_caps)
  }

  pat <- paste0("^CAP_([A-Z]{1,2})_", fecha_corte, "\\.(csv|xlsx|xls|txt|tsv)$")

  archivos <- list.files(
    path = carpeta_caps,
    pattern = pat,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(archivos) == 0) {
    stop(paste0("No encontré archivos con patrón ", pat, " en: ", carpeta_caps))
  }

  dfs <- list()

  for (f in archivos) {
    cap <- stringr::str_match(
      basename(f),
      paste0("^CAP_([A-Z]{1,2})_", fecha_corte, "\\.")
    )[, 2]

    cap <- toupper(cap)

    if (isTRUE(verbose)) {
      message("Leyendo ", cap, " <- ", basename(f))
    }

    dfs[[cap]] <- leer_tabla_flexible(f)
  }

  orden_caps <- toupper(orden_caps)
  caps_presentes <- intersect(orden_caps, names(dfs))
  dfs <- dfs[caps_presentes]

  resumen_carga <- tibble::tibble(
    cap = names(dfs),
    n = vapply(dfs, nrow, integer(1)),
    p = vapply(dfs, ncol, integer(1))
  )

  list(
    dfs = dfs,
    resumen_carga = resumen_carga,
    carpeta_caps = carpeta_caps,
    archivos = archivos
  )
}
