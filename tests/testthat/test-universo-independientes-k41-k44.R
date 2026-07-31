datos_rutas_k41_k44 <- function() {
  tibble::tibble(
    caso = c(
      "remuneracion_k2_1",
      "actividad_paga_k2",
      "ausencia_razones_1_4",
      "ausencia_5_8_hasta_4_meses",
      "familiar_npckp3_2",
      "familiar_despues_ausencia_larga",
      "residual_npckp6_1_sin_antecedentes",
      "residual_npckp6_1_sin_razon_valida",
      "menor_10",
      paste0("posicion_excluida_", c(1, 2, 3, 6, 7))
    ),
    edad = c(rep(30, 8), 9, rep(30, 5)),
    NPCKP2_1 = c(1, NA, NA, NA, NA, NA, 2, 2, 1, rep(1, 5)),
    NPCKP2 = c(NA, 1, NA, NA, NA, NA, 2, 2, NA, rep(NA, 5)),
    NPCKP3 = c(NA, NA, 1, 1, 2, 1, NA, 1, NA, rep(NA, 5)),
    NPCKP5_1 = c(NA, NA, 1, 5, NA, 5, NA, 9, NA, rep(NA, 5)),
    NPCKP6_1 = c(NA, NA, NA, 1, NA, 2, 2, 2, NA, rep(NA, 5)),
    NPCKP4 = c(NA, NA, NA, NA, 1, 1, 1, 1, NA, rep(NA, 5)),
    NPCKP17 = c(4, 5, 8, 4, 5, 8, 4, 4, 4, 1, 2, 3, 6, 7)
  )
}

test_that("helper K41-K44 reconoce exclusivamente las seis rutas legitimas", {
  datos <- datos_rutas_k41_k44()
  resultado <- universo_independientes_k41_k44(datos)

  rutas <- c(
    "ruta_remuneracion_npckp2_1",
    "ruta_actividad_paga_npckp2",
    "ruta_ausencia_razones_1_4",
    "ruta_ausencia_razones_5_8_hasta_4_meses",
    "ruta_familiar_npckp3_2",
    "ruta_familiar_despues_ausencia_larga"
  )

  for (i in seq_along(rutas)) {
    expect_true(resultado[[rutas[[i]]]][[i]], info = rutas[[i]])
  }

  expect_true(all(resultado$ocupado_consolidado[1:6]))
  expect_true(all(resultado$posicion_independiente[1:9]))
  expect_true(all(resultado$universo_k41_k44[1:6]))
  expect_false(any(resultado$universo_k41_k44[7:nrow(resultado)]))
})

test_that("residuales de NPCKP6_1 no crean una ruta ocupada", {
  datos <- datos_rutas_k41_k44()
  resultado <- universo_independientes_k41_k44(datos)
  residuales <- datos$caso %in% c(
    "residual_npckp6_1_sin_antecedentes",
    "residual_npckp6_1_sin_razon_valida"
  )

  expect_false(any(resultado$ruta_familiar_despues_ausencia_larga[residuales]))
  expect_false(any(resultado$ocupado_consolidado[residuales]))
  expect_false(any(resultado$universo_k41_k44[residuales]))
})

test_that("posiciones 4 5 y 8 entran y las demas posiciones se excluyen", {
  posiciones <- tibble::tibble(
    edad = 30,
    NPCKP2_1 = 1,
    NPCKP2 = NA,
    NPCKP3 = NA,
    NPCKP5_1 = NA,
    NPCKP6_1 = NA,
    NPCKP4 = NA,
    NPCKP17 = 1:8
  )

  resultado <- universo_independientes_k41_k44(posiciones)

  expect_equal(
    which(resultado$universo_k41_k44),
    c(4L, 5L, 8L)
  )
  expect_false(any(resultado$universo_k41_k44[c(1, 2, 3, 6, 7)]))
})

test_that("NA en antecedentes no produce universo NA ni entradas espurias", {
  datos <- tibble::tibble(
    caso_na = c(
      "edad", "NPCKP2_1", "NPCKP2", "NPCKP3",
      "NPCKP5_1", "NPCKP6_1", "NPCKP4", "NPCKP17"
    ),
    edad = c(NA, rep(30, 7)),
    NPCKP2_1 = c(1, NA, 2, 2, 2, 2, 2, 1),
    NPCKP2 = c(2, 2, NA, 2, 2, 2, 2, 2),
    NPCKP3 = c(2, 2, 2, NA, 1, 1, 2, 2),
    NPCKP5_1 = c(9, 9, 9, 5, NA, 5, 9, 9),
    NPCKP6_1 = c(9, 9, 9, 2, 2, NA, 9, 9),
    NPCKP4 = c(2, 2, 2, 1, 1, 1, NA, 2),
    NPCKP17 = c(rep(4, 7), NA)
  )

  resultado <- universo_independientes_k41_k44(datos)

  expect_false(anyNA(resultado$universo_k41_k44))
  expect_false(any(resultado$universo_k41_k44))
  expect_true(all(resultado$flujo_indeterminado))
})

test_that("diagnostico comparte universo madre y metadatos correctos K41-K44", {
  datos <- datos_rutas_k41_k44()[1:9, ]
  k <- datos |>
    dplyr::mutate(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = sprintf("%02d", dplyr::row_number()),
      NPCKP36 = NA_character_,
      NPCKP36A = NA_character_,
      NPCKP37 = NA_character_,
      NPCKP43_1 = c(rep(1, 6), NA, NA, 1),
      NPCKP43_1A = NA_character_,
      NPCKP44_1 = c(rep(11, 6), NA, NA, 11),
      NPCKP44_1A = NA_character_
    ) |>
    dplyr::select(-"caso", -"edad")
  e <- datos |>
    dplyr::transmute(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = sprintf("%02d", dplyr::row_number()),
      NPCEP4 = .data$edad
    )

  variables <- c(
    "NPCKP36", "NPCKP36A", "NPCKP37", "NPCKP43_1", "NPCKP43_1A",
    "NPCKP44_1", "NPCKP44_1A"
  )
  diagnostico <- diagnostico_flujo_capitulo_k(
    dfs = list(K = k, E = e),
    vars_cap_k = variables
  )
  detalle <- diagnostico$diagnostico_persona_variable |>
    dplyr::filter(.data$variable %in% c(
      "NPCKP36", "NPCKP36A", "NPCKP37", "NPCKP43_1", "NPCKP44_1"
    )) |>
    dplyr::select("ORDEN", "variable", "debe_responder") |>
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "debe_responder"
    )

  expect_equal(detalle$NPCKP36, detalle$NPCKP37)
  expect_equal(detalle$NPCKP36, detalle$NPCKP36A)
  expect_equal(detalle$NPCKP36, detalle$NPCKP43_1)
  expect_equal(detalle$NPCKP36, detalle$NPCKP44_1)

  reglas <- diagnostico$reglas_flujo
  regla_43a <- dplyr::filter(reglas, .data$variable == "NPCKP43_1A")
  regla_44a <- dplyr::filter(reglas, .data$variable == "NPCKP44_1A")

  expect_false(regla_43a$texto_libre)
  expect_identical(regla_43a$dominio[[1]], c(1L, 2L))
  expect_true(grepl("NPCKP43_1 == 1", regla_43a$regla_r, fixed = TRUE))
  expect_true(regla_44a$texto_libre)
  expect_length(regla_44a$dominio[[1]], 0)
  expect_true(grepl("NPCKP44_1 == 11", regla_44a$regla_r, fixed = TRUE))

  detalle_43a <- diagnostico$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == "NPCKP43_1A") |>
    dplyr::arrange(.data$ORDEN)
  detalle_44a <- diagnostico$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == "NPCKP44_1A") |>
    dplyr::arrange(.data$ORDEN)

  expect_equal(detalle_43a$debe_responder, c(rep(TRUE, 6), NA, FALSE, FALSE))
  expect_equal(detalle_44a$debe_responder, c(rep(TRUE, 6), NA, FALSE, FALSE))
})
