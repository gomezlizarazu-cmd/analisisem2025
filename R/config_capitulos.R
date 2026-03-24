#' Tipología de capítulos de la encuesta
#'
#' Objeto de configuración que clasifica cada capítulo de la encuesta
#' según su nivel de observación:
#'
#' \itemize{
#'   \item \code{"vivienda"}: llave \code{DIRECTORIO}
#'   \item \code{"hogar"}: llave \code{DIRECTORIO + SECUENCIA_P}
#'   \item \code{"persona"}: llave \code{DIRECTORIO + SECUENCIA_P + ORDEN}
#' }
#'
#' Este objeto se usa internamente para determinar las llaves de cruce
#' y la granularidad de los capítulos.
#'
#' @format Una lista nombrada de longitud 15.
#' @export
tipo_capitulo <- list(
  A = "vivienda",
  B = "hogar",
  C = "hogar",
  D = "hogar",
  E = "persona",
  F = "persona",
  G = "persona",
  H = "persona",
  I = "persona",
  J = "persona",
  K = "persona",
  L = "hogar",
  M = "hogar",
  MA = "hogar",
  MB = "hogar"
)

#' Umbrales de edad para capítulos condicionados
#'
#' Objeto de configuración que define la población objetivo mínima por edad
#' para algunos capítulos de la encuesta.
#'
#' La interpretación es:
#' \itemize{
#'   \item \code{G = 4.99}: población menor de 5 años
#'   \item \code{H = 5}: población de 5 años o más
#'   \item \code{I = 5}: población de 5 años o más
#'   \item \code{J = 10}: población de 10 años o más
#'   \item \code{K = 10}: población de 10 años o más
#' }
#'
#' Se usa internamente en funciones de elegibilidad y cobertura.
#'
#' @format Una lista nombrada de longitud 5.
#' @export
Edad_objeto <- list(
  G = 4.99,
  H = 5,
  I = 5,
  J = 10,
  K = 10
)

#' Variables trazadoras por capítulo
#'
#' Tabla de configuración con variables trazadoras seleccionadas de la encuesta,
#' su capítulo de origen y un alias amigable para análisis, reportes o tableros.
#'
#' Puede usarse, por ejemplo, en funciones que construyen bases consolidadas
#' con variables clave de distintos capítulos.
#'
#' @format Un tibble con 20 filas y 3 columnas:
#' \describe{
#'   \item{var}{Nombre original de la variable en la base fuente}
#'   \item{cap}{Capítulo de origen}
#'   \item{alias}{Nombre amigable o alias sugerido}
#' }
#'
#' @examples
#' trazadoras
#'
#' @export
trazadoras <- tibble::tribble(
  ~var,         ~cap, ~alias,
  "NVCAP99",     "A",  "Apertura_encuesta",
  "NOM_SUP",     "A",  "Supervisor",
  "DPTO",        "A",  "Dpto",
  "MPIO",        "A",  "Mpio",
  "SEGMENTO",    "A",  "Segmento",
  "CLASE",       "A",  "Clase",
  "MANZANA",     "A",  "Manzana",
  "P20",         "A",  "Barrio",
  "NVCAP18",     "A",  "Telefono",
  "COD_UPZ",     "A",  "UPZ",
  "FECHA_CARGA", "A",  "CargaEncuesta",
  "LOCALIDAD",   "A",  "Localidad",
  "VERSION_INICIO","A","VersionIniAplicativo",
  "VERSION_CIERRE","A","VersionFinAplicativo",
  "NVCBP10",     "B",  "TipoVivienda",
  "NHCCP18",     "C",  "PersonasHogar",
  "NPCEP2",      "E",  "Nombres_Apellidos",
  "NPCEP4",      "E",  "Edad",
  "NPCFP1",      "F",  "Afiliacion"
) %>%
  dplyr::mutate(cap = toupper(cap))

#' Diccionario de municipios de la Encuesta Multipropósito
#'
#' Tabla de correspondencia entre código de municipio y nombre del municipio.
#' Se usa para enriquecer capítulos con nombres legibles a partir de códigos.
#'
#' @format Un tibble con 22 filas y 2 columnas:
#' \describe{
#'   \item{cod_mpio}{Código del municipio}
#'   \item{municipio}{Nombre del municipio}
#' }
#'
#' @examples
#' dic_mpios
#'
#' @export
dic_mpios <- tibble::tibble(
  cod_mpio = c(
    11001,
    25099, 25126, 25175, 25214, 25260, 25269, 25286,
    25290, 25295, 25377, 25430, 25473, 25740, 25754,
    25758, 25769, 25785, 25799, 25817, 25898, 25899
  ),
  municipio = c(
    "Bogotá",
    "Bojacá",
    "Cajicá",
    "Chía",
    "Cota",
    "El Rosal",
    "Facatativá",
    "Funza",
    "Fusagasugá",
    "Gachancipá",
    "La Calera",
    "Madrid",
    "Mosquera",
    "Sibaté",
    "Soacha",
    "Sopó",
    "Subachoque",
    "Tabio",
    "Tenjo",
    "Tocancipá",
    "Zipacón",
    "Zipaquirá"
  )
)
