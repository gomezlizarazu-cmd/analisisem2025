test_that("resumenes K41-K42 separan no vacio, valido, imputado y residual", {
  diagnostico <- tibble::tibble(
    DIRECTORIO = as.character(1:6),
    SECUENCIA_P = "1",
    ORDEN = "1",
    edad_num = c(30, 40, 50, 60, 70, 80),
    NPCKP17 = c("4", "5", "8", "4", "5", "1"),
    NPCKP36 = c("1000", "98", "99", "abc", NA, "500"),
    NPCKP37 = c("2", "13", NA, "abc", NA, "1"),
    universo_npckp36_37 = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    responde_npckp36 = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    responde_npckp37 = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    respuesta_valida_npckp36 = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
    codigo_98_npckp36 = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    codigo_99_npckp36 = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    no_convertible_npckp36 = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    n_fuentes_monetarias_validas = c(1, 1, 0, 0, 0, 1),
    n_fuentes_codigos_especiales = c(0, 0, 1, 0, 0, 0),
    caso_ambiguo_monetario = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    caso_sin_informacion_monetaria = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    recuperable_npckp36_acotado = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    ingreso_acotado = c(1000, 2000, NA, NA, NA, 500),
    ingreso_amplio_mensual = c(1000, 2100, NA, NA, NA, 500),
    ingreso_amplio = c(1100, 2200, NA, NA, NA, 500),
    desviada_bloque_asalariados_en_universo_objetivo =
      c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  imputada <- diagnostico |>
    dplyr::mutate(
      NPCKP36_original = NPCKP36,
      NPCKP37_original = NPCKP37,
      NPCKP36 = c("1000", "2000", "99", "abc", NA, "500"),
      NPCKP37 = c("2", "13", "1", "abc", NA, "1"),
      NPCKP36_flag_imputado = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
      NPCKP36A_flag_imputado = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
      NPCKP37_flag_imputado = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
      K4142_flag_desvio_bloque_asalariados =
        c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      K4142_flag_limpieza_bloque_asalariados =
        c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      K4142_n_variables_asalariadas_limpiadas = c(0L, 1L, 1L, 1L, 0L, 0L),
      NPCKP23_original = c(NA, "2000", "98", "abc", NA, NA),
      NPCKP23 = NA_character_
    )
  reglas <- tibble::tibble(
    variable = "NPCKP23",
    pregunta = "Ingreso laboral",
    bloque = "05_rama_asalariados"
  )
  diagnostico_antes <- serialize(diagnostico, NULL)
  imputada_antes <- serialize(imputada, NULL)

  out <- construir_resumenes_k41_k42(
    base_diagnostico = diagnostico,
    base_imputada = imputada,
    variables_limpieza = "NPCKP23",
    reglas_flujo = reglas
  )

  universos <- out$resumen_universos
  valor <- function(indicador) {
    universos$personas[universos$indicador == indicador]
  }
  expect_equal(valor("npckp36_respuesta_no_vacia_original"), 4)
  expect_equal(valor("npckp36_respuesta_monetaria_valida_original"), 1)
  expect_equal(valor("npckp36_respuesta_codigo_98_original"), 1)
  expect_equal(valor("npckp36_respuesta_codigo_99_original"), 1)
  expect_equal(valor("npckp36_imputados_ingreso_acotado"), 1)
  expect_equal(
    valor("npckp36_resueltos_despues_imputacion") +
      valor("npckp36_residuales_despues_imputacion"),
    5
  )
  expect_equal(
    valor("npckp37_respuesta_original_valida") +
      valor("npckp37_sin_respuesta_original_valida"),
    5
  )
  expect_equal(
    valor("npckp37_respuesta_original_valida") +
      valor("npckp37_imputados_un_mes") +
      valor("npckp37_residuales_despues_imputacion"),
    5
  )
  expect_true(all(universos$porcentaje <= 1, na.rm = TRUE))
  expect_true(all(out$controles_integridad$estado == "OK"))
  expect_equal(out$variables_limpiadas$personas_limpiadas, 3)
  expect_equal(
    sum(out$balance_npckp36$personas[out$balance_npckp36$etapa == "Final"]),
    5
  )
  expect_equal(
    sum(out$balance_npckp37$personas[out$balance_npckp37$etapa == "Final"]),
    5
  )
  expect_true(all(
    out$residuales$motivo_residual_npckp36 %in%
      c(
        NA_character_, "codigo_99_original",
        "valor_original_no_convertible",
        "sin_fuente_monetaria_valida"
      )
  ))
  expect_true(all(
    out$residuales$DIRECTORIO %in% c("2", "3", "4", "5")
  ))
  expect_false("base_k_imputada_k41_k42" %in% names(out))
  expect_true(all(c(
    "resumen_universos",
    "resumen_imputacion_k41_k42",
    "balance_npckp36",
    "balance_npckp37",
    "residuales"
  ) %in% names(out)))
  expect_identical(serialize(diagnostico, NULL), diagnostico_antes)
  expect_identical(serialize(imputada, NULL), imputada_antes)
})

test_that("resumenes K41-K42 detienen balances que no cierran", {
  expect_error(
    construir_resumenes_k41_k42(
      base_diagnostico = tibble::tibble(),
      base_imputada = tibble::tibble(),
      variables_limpieza = character()
    ),
    "Faltan variables"
  )
})
