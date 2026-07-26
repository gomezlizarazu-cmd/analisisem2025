#' Normalizar documento para cruces entre fuentes
#'
#' Convierte un identificador documental a texto en mayusculas, elimina espacios
#' externos, remueve el sufijo decimal `.0` tipico de lecturas desde hojas de
#' calculo y conserva solo letras y digitos.
#'
#' @param x Vector con documentos originales.
#'
#' @return Vector character con documentos normalizados.
#'
#' @examples
#' normalizar_documento_cruce(c(" 1.234.0 ", "ab-45", NA))
#'
#' @export
normalizar_documento_cruce <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("\\.0$", "") |>
    stringr::str_replace_all("[^A-Z0-9]", "")
}

#' Normalizar texto para cruces entre fuentes
#'
#' Limpia campos textuales usados en identificacion, como nombres y apellidos:
#' fuerza UTF-8 con \code{arreglar_utf8()}, convierte a mayusculas, remueve
#' tildes y deja solo letras y espacios comprimidos.
#'
#' @param x Vector de texto.
#'
#' @return Vector character con texto normalizado.
#'
#' @examples
#' normalizar_texto_cruce(c(" Jose  Gomez ", "Maria-Luisa"))
#'
#' @export
normalizar_texto_cruce <- function(x) {
  x |>
    arreglar_utf8() |>
    as.character() |>
    stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^A-Z ]", " ") |>
    stringr::str_squish()
}

#' Normalizar tipo de documento para cruces
#'
#' Estandariza un tipo de documento como texto en mayusculas, sin tildes,
#' espacios ni caracteres distintos de letras o digitos.
#'
#' @param x Vector con tipos de documento.
#'
#' @return Vector character con tipos de documento normalizados.
#'
#' @examples
#' normalizar_tipo_documento_cruce(c(" c.c. ", "T.I", "cedula"))
#'
#' @export
normalizar_tipo_documento_cruce <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_trim() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^A-Z0-9]", "")
}

#' Homologar sexo para cruces entre fuentes
#'
#' Convierte codificaciones frecuentes de sexo a \code{"F"} o \code{"M"}. Los
#' valores no reconocidos se devuelven como \code{NA_character_}.
#'
#' @param x Vector con valores de sexo originales.
#'
#' @return Vector character con valores \code{"F"}, \code{"M"} o \code{NA}.
#'
#' @examples
#' normalizar_sexo_cruce(c("Mujer", "1", "HOMBRE", "x"))
#'
#' @export
normalizar_sexo_cruce <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_upper(stringr::str_squish(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")

  dplyr::case_when(
    x %in% c("F", "FEMENINO", "MUJER", "2") ~ "F",
    x %in% c("M", "MASCULINO", "HOMBRE", "1") ~ "M",
    TRUE ~ NA_character_
  )
}

#' Clasificar calidad basica de un documento para cruce
#'
#' Aplica una clasificacion operativa sobre documentos normalizados para
#' distinguir documentos vacios, genericos, muy cortos, compuestos solo por
#' ceros y potencialmente validos.
#'
#' @param x Vector con documentos originales o normalizados.
#'
#' @return Vector character con la categoria de calidad documental.
#'
#' @examples
#' clasificar_documento_cruce(c(NA, "0000", "123", "123456"))
#'
#' @export
clasificar_documento_cruce <- function(x) {
  x <- normalizar_documento_cruce(x)
  nchar_x <- nchar(x)

  dplyr::case_when(
    is.na(x) | x == "" ~ "sin_documento",
    x %in% c("0", "00", "000", "0000", "999", "9999", "999999", "999999999", "SIN", "NA") ~ "documento_generico",
    nchar_x < 4 ~ "documento_muy_corto",
    stringr::str_detect(x, "^0+$") ~ "documento_solo_ceros",
    TRUE ~ "documento_potencialmente_valido"
  )
}
