#' Agregar nombre de municipio a uno o varios capítulos
#'
#' Pega un diccionario de municipios a los data frames de una lista de capítulos,
#' agregando una columna con el nombre del municipio a partir de un código.
#'
#' La función puede aplicarse:
#' \itemize{
#'   \item a todos los capítulos de \code{dfs}, o
#'   \item a un capítulo específico mediante \code{cap}.
#' }
#'
#' Si no se especifica \code{var_mpio}, la función intenta detectar
#' automáticamente la variable de municipio usando un conjunto de nombres
#' candidatos.
#'
#' @param dfs Lista de data frames nombrada con los capítulos de la encuesta.
#' Cada elemento de la lista debe ser un \code{data.frame}.
#' @param dic_mpios Data frame o tibble con el diccionario de municipios.
#' Debe contener las columnas \code{cod_mpio} y \code{municipio}.
#' @param cap Capítulo al que se desea aplicar el cruce. Si es \code{NULL},
#' la función se aplica a todos los capítulos de \code{dfs}. Por defecto
#' es \code{NULL}.
#' @param var_mpio Nombre de la variable que contiene el código de municipio
#' dentro del capítulo. Si es \code{NULL}, la función la detecta automáticamente.
#' @param candidatos Vector de nombres candidatos para detectar automáticamente
#' la variable de municipio. Por defecto:
#' \code{c("COD_MPIO", "Mpio", "MPIO", "Municipio", "Munic", "MUNICIPIO")}.
#' @param nombre_salida Nombre de la columna de salida que contendrá el nombre
#' del municipio. Por defecto es \code{"NomMunicipio"}.
#'
#' @details
#' La función realiza un \code{left_join()} entre cada data frame y el
#' diccionario de municipios. Si la columna de salida ya existe en el capítulo,
#' se elimina antes de volver a crearla para evitar duplicados.
#'
#' Cuando \code{cap = NULL}, se devuelve una nueva lista de data frames con el
#' cruce aplicado a todos los capítulos posibles. Cuando \code{cap} toma un
#' valor específico, solo se modifica ese capítulo dentro de \code{dfs}.
#'
#' Si en un capítulo no se encuentra ninguna variable candidata de municipio
#' y no se suministra \code{var_mpio}, ese capítulo se deja sin cambios.
#'
#' @return
#' Una lista de data frames con el nombre del municipio agregado:
#' \itemize{
#'   \item si \code{cap = NULL}, devuelve toda la lista \code{dfs} actualizada;
#'   \item si \code{cap} se especifica, devuelve \code{dfs} con ese capítulo modificado.
#' }
#'
#' @examples
#' \dontrun{
#' # Aplicar a todos los capítulos
#' dfs2 <- agregar_municipios(
#'   dfs = dfs,
#'   dic_mpios = dic_mpios
#' )
#'
#' # Aplicar solo al capítulo A
#' dfs2 <- agregar_municipios(
#'   dfs = dfs,
#'   dic_mpios = dic_mpios,
#'   cap = "A"
#' )
#'
#' # Aplicar a un capítulo indicando explícitamente la variable de municipio
#' dfs2 <- agregar_municipios(
#'   dfs = dfs,
#'   dic_mpios = dic_mpios,
#'   cap = "A",
#'   var_mpio = "MPIO",
#'   nombre_salida = "NomMunicipio"
#' )
#' }
#'
#' @author
#' David Gómez Lizarazú
#'
#' @export
####Función para pegar Municipio####
agregar_municipios <- function(dfs,
                               dic_mpios,
                               cap = NULL,
                               var_mpio = NULL,
                               candidatos = c("COD_MPIO", "Mpio", "MPIO", "Municipio", "Munic", "MUNICIPIO"),
                               nombre_salida = "NomMunicipio") {

  # Validaciones básicas
  if (!is.list(dfs)) stop("dfs debe ser una lista de data frames")
  if (!all(c("cod_mpio", "municipio") %in% names(dic_mpios))) {
    stop("dic_mpios debe tener columnas 'cod_mpio' y 'municipio'")
  }

  # Renombrar diccionario a nombre_salida
  dic_use <- dic_mpios %>%
    dplyr::rename(!!nombre_salida := municipio)

  # Función interna para un solo data frame
  pegar_en_df <- function(df, var_mpio_local = NULL) {
    if (!is.data.frame(df)) return(df)

    # Detectar variable de municipio si no viene informada
    if (is.null(var_mpio_local)) {
      hits <- intersect(candidatos, names(df))
      if (length(hits) == 0) return(df)
      var_mpio_local <- hits[1]
    }

    # Si ya existe la columna de salida, quitarla para evitar duplicados
    if (nombre_salida %in% names(df)) {
      df <- df %>% dplyr::select(-dplyr::all_of(nombre_salida))
    }

    # Join
    df %>%
      dplyr::left_join(
        dic_use,
        by = stats::setNames("cod_mpio", var_mpio_local)
      )
  }

  # Caso 1: aplicar a todos los capítulos
  if (is.null(cap)) {
    dfs_out <- lapply(dfs, pegar_en_df, var_mpio_local = var_mpio)
    return(dfs_out)
  }

  # Caso 2: aplicar solo a un capítulo
  cap <- toupper(cap)
  if (!cap %in% names(dfs)) {
    stop(paste0("No existe el capítulo '", cap, "' en dfs"))
  }

  dfs[[cap]] <- pegar_en_df(dfs[[cap]], var_mpio_local = var_mpio)
  dfs
}
