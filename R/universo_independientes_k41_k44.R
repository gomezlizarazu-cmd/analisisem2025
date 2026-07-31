#' Construir el universo auditable de independientes K41-K44
#'
#' Normaliza las variables que determinan la entrada al bloque K41-K44 y
#' devuelve las seis rutas legitimas de entrada a ocupados, la consolidacion
#' de esas rutas, la posicion ocupacional independiente y el universo final.
#'
#' La ruta que pasa por `NPCKP6_1 %in% c(2, 3)` exige expresamente haber
#' llegado a esa pregunta mediante `NPCKP3 == 1` y
#' `NPCKP5_1 %in% c(5, 6, 7, 8)`. Esto evita incorporar personas a partir de
#' respuestas residuales de `NPCKP6_1`.
#'
#' En la validacion persona a persona suministrada para el corte 20260703, la
#' regla anterior, esta regla estricta y la regla simplificada seleccionaron
#' las mismas 36.108 personas. Se observaron cero llaves diferentes y cero
#' impactos sobre imputaciones, codigos residuales o sincronizaciones de
#' K41-K42. La proteccion adicional es preventiva y no reabre esa fase.
#'
#' @param data Data frame con las variables de flujo del Capitulo K.
#' @param edad_var Nombre de la variable de edad.
#' @param posicion_var Nombre de la variable final de posicion ocupacional.
#'
#' @return Tibble con indicadores logicos auditables. Todos los indicadores
#'   de rutas y `universo_k41_k44` son logicos sin `NA`.
#'   `flujo_indeterminado` identifica los casos en los que el universo no puede
#'   decidirse por falta de antecedentes. El atributo `regla_r` conserva la
#'   expresion canonica visible.
#'
#' @keywords internal
universo_independientes_k41_k44 <- function(data,
                                            edad_var = "edad",
                                            posicion_var = "NPCKP17") {
  data <- tibble::as_tibble(data)
  n <- nrow(data)

  normalizar_numero <- function(variable) {
    if (!(variable %in% names(data))) {
      return(rep(NA_real_, n))
    }

    valor <- stringr::str_squish(as.character(data[[variable]]))
    valor[
      is.na(valor) |
        valor == "" |
        stringr::str_to_upper(valor) %in% c("NA", "N/A", "NULL", "NULO")
    ] <- NA_character_

    suppressWarnings(as.numeric(valor))
  }

  igual <- function(x, valor) {
    dplyr::if_else(!is.na(x), x == valor, NA)
  }

  pertenece <- function(x, valores) {
    dplyr::if_else(!is.na(x), x %in% valores, NA)
  }

  sin_na <- function(x) {
    dplyr::coalesce(as.logical(x), FALSE)
  }

  edad <- normalizar_numero(edad_var)
  npckp2_1 <- normalizar_numero("NPCKP2_1")
  npckp2 <- normalizar_numero("NPCKP2")
  npckp3 <- normalizar_numero("NPCKP3")
  npckp5_1 <- normalizar_numero("NPCKP5_1")
  npckp6_1 <- normalizar_numero("NPCKP6_1")
  npckp4 <- normalizar_numero("NPCKP4")
  posicion <- normalizar_numero(posicion_var)

  edad_valida_raw <- dplyr::if_else(!is.na(edad), edad >= 10, NA)
  ruta_remuneracion_raw <- igual(npckp2_1, 1)
  ruta_actividad_paga_raw <- igual(npckp2, 1)
  debe_npckp6_1_raw <-
    igual(npckp3, 1) &
    pertenece(npckp5_1, 5:8)
  debe_npckp4_raw <-
    igual(npckp3, 2) |
    (
      debe_npckp6_1_raw &
      pertenece(npckp6_1, c(2, 3))
    )
  ruta_ausencia_1_4_raw <-
    igual(npckp3, 1) &
    pertenece(npckp5_1, 1:4)
  ruta_ausencia_5_8_raw <-
    igual(npckp3, 1) &
    pertenece(npckp5_1, 5:8) &
    igual(npckp6_1, 1)
  ruta_familiar_npckp3_2_raw <-
    igual(npckp3, 2) &
    igual(npckp4, 1)
  ruta_familiar_ausencia_larga_raw <-
    igual(npckp3, 1) &
    pertenece(npckp5_1, 5:8) &
    pertenece(npckp6_1, c(2, 3)) &
    igual(npckp4, 1)

  ocupado_raw <-
    ruta_remuneracion_raw |
    ruta_actividad_paga_raw |
    ruta_ausencia_1_4_raw |
    ruta_ausencia_5_8_raw |
    ruta_familiar_npckp3_2_raw |
    ruta_familiar_ausencia_larga_raw
  posicion_independiente_raw <- pertenece(posicion, c(4, 5, 8))
  universo_raw <-
    edad_valida_raw &
    ocupado_raw &
    posicion_independiente_raw

  salida <- tibble::tibble(
    ruta_remuneracion_npckp2_1 = sin_na(ruta_remuneracion_raw),
    ruta_actividad_paga_npckp2 = sin_na(ruta_actividad_paga_raw),
    ruta_ausencia_razones_1_4 = sin_na(ruta_ausencia_1_4_raw),
    ruta_ausencia_razones_5_8_hasta_4_meses =
      sin_na(ruta_ausencia_5_8_raw),
    ruta_familiar_npckp3_2 = sin_na(ruta_familiar_npckp3_2_raw),
    ruta_familiar_despues_ausencia_larga =
      sin_na(ruta_familiar_ausencia_larga_raw),
    debe_responder_npckp6_1 = sin_na(debe_npckp6_1_raw),
    debe_responder_npckp4 = sin_na(debe_npckp4_raw),
    ocupado_consolidado = sin_na(ocupado_raw),
    posicion_independiente = sin_na(posicion_independiente_raw),
    universo_k41_k44 = sin_na(universo_raw),
    flujo_indeterminado = is.na(universo_raw),
    flujo_ocupado_indeterminado = is.na(ocupado_raw),
    flujo_npckp6_1_indeterminado = is.na(debe_npckp6_1_raw),
    flujo_npckp4_indeterminado = is.na(debe_npckp4_raw)
  )

  attr(salida, "regla_r") <- paste0(
    "edad >= 10 & (",
    "NPCKP2_1 == 1 | NPCKP2 == 1 | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(1,2,3,4)) | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8) & NPCKP6_1 == 1) | ",
    "((NPCKP3 == 2 | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8) & ",
    "NPCKP6_1 %in% c(2,3))) & NPCKP4 == 1)",
    ") & NPCKP17 %in% c(4,5,8)"
  )
  attr(salida, "regla_entrada_ocupados_r") <- paste0(
    "NPCKP2_1 == 1 | NPCKP2 == 1 | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(1,2,3,4)) | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8) & NPCKP6_1 == 1) | ",
    "((NPCKP3 == 2 | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8) & ",
    "NPCKP6_1 %in% c(2,3))) & NPCKP4 == 1)"
  )
  attr(salida, "regla_npckp6_1_r") <-
    "NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8)"
  attr(salida, "regla_npckp4_r") <- paste0(
    "NPCKP3 == 2 | ",
    "(NPCKP3 == 1 & NPCKP5_1 %in% c(5,6,7,8) & ",
    "NPCKP6_1 %in% c(2,3))"
  )

  salida
}
