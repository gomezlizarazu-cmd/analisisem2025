test_that("exportar_reporte_encuestas_caidas considera solo no pegan desde la base", {
  dfs <- list(
    A = tibble::tibble(DIRECTORIO = c("1", "2")),
    B = tibble::tibble(DIRECTORIO = c("1", "2"), SECUENCIA_P = c("1", "1")),
    C = tibble::tibble(DIRECTORIO = c("1", "2"), SECUENCIA_P = c("1", "1")),
    D = tibble::tibble(DIRECTORIO = c("1", "2"), SECUENCIA_P = c("1", "1")),
    E = tibble::tibble(
      DIRECTORIO = c("1", "1", "2"),
      SECUENCIA_P = c("1", "1", "1"),
      ORDEN = c("1", "2", "1"),
      NPCEP4 = c(30, 12, 8)
    )
  )

  diag <- diagnostico_flujo_caps(
    dfs = dfs,
    caps_orden = c("A", "B", "C", "D", "E"),
    cap_ref = "A",
    edad_var = "NPCEP4"
  )

  expect_equal(sum(diag$resumen$solo_base), 0)
  expect_equal(sum(diag$resumen$solo_nuevo), 1)

  no_pegan_base <- diag$no_pegan %>%
    dplyr::filter(.data$origen_no_pega == "base")

  expect_equal(nrow(no_pegan_base), 0)
})

test_that("diagnostico_flujo_caps aplica elegibilidad por edad en capitulos de personas", {
  dfs <- list(
    E = tibble::tibble(
      DIRECTORIO = c("1", "1", "1"),
      SECUENCIA_P = c("1", "1", "1"),
      ORDEN = c("1", "2", "3"),
      NPCEP4 = c(2, 7, 12)
    ),
    G = tibble::tibble(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = "1"
    ),
    H = tibble::tibble(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = "2"
    ),
    J = tibble::tibble(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = "3"
    )
  )

  diag <- diagnostico_flujo_caps(
    dfs = dfs,
    caps_orden = c("E", "G", "H", "J"),
    cap_ref = "E",
    edad_var = "NPCEP4"
  )

  resumen <- diag$resumen

  expect_equal(resumen$n_base[resumen$cap_nuevo == "G"], 1)
  expect_equal(resumen$n_base[resumen$cap_nuevo == "H"], 2)
  expect_equal(resumen$n_base[resumen$cap_nuevo == "J"], 1)
  expect_equal(resumen$solo_base[resumen$cap_nuevo == "G"], 0)
  expect_equal(resumen$solo_base[resumen$cap_nuevo == "H"], 1)
  expect_equal(resumen$solo_base[resumen$cap_nuevo == "J"], 0)
})
