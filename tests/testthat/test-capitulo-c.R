test_that("diagnostico de flujo con C no marca falsos positivos en hogares adicionales", {
  dfs <- list(
    B = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1")
    ),
    C = tibble::tibble(
      DIRECTORIO = "100",
      SECUENCIA_P = "1"
    ),
    D = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1")
    ),
    E = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1"),
      ORDEN = c("1", "1", "1"),
      NPCEP4 = c(30, 25, 40)
    )
  )

  flujo <- diagnostico_flujo_caps(dfs, c("B", "C", "D"))

  paso_c <- flujo$resumen %>%
    dplyr::filter(.data$cap_nuevo == "C")

  no_pegan_c <- flujo$no_pegan %>%
    dplyr::filter(.data$cap_nuevo == "C", grepl("_sin_C$", .data$tipo_no_pega))

  expect_equal(paso_c$solo_base, 1)
  expect_equal(paso_c$pegan, 2)
  expect_equal(nrow(no_pegan_c), 1)
  expect_equal(no_pegan_c$DIRECTORIO, "200")
  expect_equal(no_pegan_c$SECUENCIA_P, "1")
})

test_that("pipeline estructural conserva todos los hogares de una vivienda si alguno cruza con C", {
  dfs <- list(
    B = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1")
    ),
    C = tibble::tibble(
      DIRECTORIO = "100",
      SECUENCIA_P = "1"
    ),
    D = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1")
    )
  )

  pipe <- pipeline_encuestas_completas(dfs, c("B", "C", "D"))

  paso_c <- pipe$resumen %>%
    dplyr::filter(.data$cap_nuevo == "C")

  hogares_finales <- pipe$final %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P)

  expect_equal(paso_c$n_resultado, 2)
  expect_equal(paso_c$caida_abs, 1)
  expect_equal(nrow(hogares_finales), 2)
  expect_true(all(c("1", "2") %in% hogares_finales$SECUENCIA_P[hogares_finales$DIRECTORIO == "100"]))
})

test_that("completitud por hogar trata C como satisfecho si otro hogar de la vivienda lo tiene", {
  dfs <- list(
    B = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1")
    ),
    C = tibble::tibble(
      DIRECTORIO = "100",
      SECUENCIA_P = "1"
    ),
    D = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1")
    ),
    E = tibble::tibble(
      DIRECTORIO = c("100", "100", "200"),
      SECUENCIA_P = c("1", "2", "1"),
      ORDEN = c("1", "1", "1"),
      NPCEP4 = c(30, 25, 40)
    )
  )

  res <- diagnostico_completitud_capitulos(
    dfs = dfs,
    caps_fijos = c("B", "C", "D"),
    caps_edad = character(0),
    base_hogares_cap = "B",
    incluir_segmento = FALSE
  )

  hogar_100_2 <- res$base_eval %>%
    dplyr::filter(.data$DIRECTORIO == "100", .data$SECUENCIA_P == "2")

  hogar_200_1 <- res$base_eval %>%
    dplyr::filter(.data$DIRECTORIO == "200", .data$SECUENCIA_P == "1")

  expect_equal(hogar_100_2$pres_C, 1L)
  expect_equal(hogar_100_2$ok_C, 1L)
  expect_equal(hogar_100_2$encuesta_completa, 1L)
  expect_equal(hogar_200_1$pres_C, 0L)
  expect_equal(hogar_200_1$ok_C, 0L)
  expect_equal(hogar_200_1$encuesta_completa, 0L)
})
