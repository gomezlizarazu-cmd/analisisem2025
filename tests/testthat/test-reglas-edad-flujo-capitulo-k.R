test_that("diagnostico_flujo_capitulo_k aplica universo de edad general", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = c("1", "1", "1"),
      SECUENCIA_P = c("1", "1", "1"),
      ORDEN = c("1", "2", "3"),
      NPCKP6_1 = c(2, 2, 2),
      NPCKP4 = c(NA, NA, NA)
    ),
    E = tibble::tibble(
      DIRECTORIO = c("1", "1", "1"),
      SECUENCIA_P = c("1", "1", "1"),
      ORDEN = c("1", "2", "3"),
      NPCEP4 = c(9, 10, NA)
    )
  )

  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = "NPCKP4")
  out <- diag$diagnostico_persona_variable |>
    dplyr::arrange(.data$ORDEN)

  expect_false(out$debe_responder[[1]])
  expect_false(out$vacio_critico[[1]])

  expect_true(out$debe_responder[[2]])
  expect_true(out$vacio_critico[[2]])

  expect_true(is.na(out$debe_responder[[3]]))
  expect_false(out$vacio_critico[[3]])
  expect_false(out$estado_flujo[[3]] == "Salto valido / no debia responder")
  expect_equal(out$estado_flujo[[3]], "Flujo indeterminado")
})

test_that("helpers de edad preservan umbrales especificos", {
  expect_equal(
    .diag_k_normalizar_regla_visible_edad_k("NPCKP17 %in% c(1,2,3,4,5,6,7,8)"),
    "edad >= 10 & NPCKP17 %in% c(1,2,3,4,5,6,7,8)"
  )
  expect_equal(
    .diag_k_normalizar_regla_visible_edad_k("edad >= 15 & NPCKP50 == 1"),
    "edad >= 15 & NPCKP50 == 1"
  )
  expect_equal(
    .diag_k_normalizar_regla_visible_edad_k("edad >= 18 & NPCKPN62A == 1"),
    "edad >= 18 & NPCKPN62A == 1"
  )

  edad_15 <- c(14, 15)
  universo_15 <- dplyr::if_else(!is.na(edad_15), edad_15 >= 10, NA)
  debe_15 <- edad_15 >= 15
  expect_equal(
    .diag_k_aplicar_universo_edad_k(debe_15, universo_15, length(edad_15)),
    c(FALSE, TRUE)
  )

  edad_18 <- c(17, 18)
  universo_18 <- dplyr::if_else(!is.na(edad_18), edad_18 >= 10, NA)
  debe_18 <- edad_18 >= 18
  expect_equal(
    .diag_k_aplicar_universo_edad_k(debe_18, universo_18, length(edad_18)),
    c(FALSE, TRUE)
  )
})

test_that("reglas sensibles de K muestran edad explicita", {
  variables <- c(
    "NPCKP4", "NPCKP12", "NPCKP13", "NPCKP14", "NPCKP17",
    "NPCKP18", "NPCKP19", "NPCKP20", "NPCKP20A", "NPCKP45L",
    "NPCKP47C", "NPCKP50", "NPCKP51", "NPCKP60_1", "NPCKP60_2",
    "NPCKPN62A", "NPCKPN62B", "NPCKP78_1", "NPCKP78_2"
  )
  dfs <- list(
    K = tibble::as_tibble(c(
      list(DIRECTORIO = "1", SECUENCIA_P = "1", ORDEN = "1"),
      stats::setNames(rep(list(NA), length(variables)), variables)
    )),
    E = tibble::tibble(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = "1",
      NPCEP4 = 10
    )
  )

  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = variables)
  reglas <- diag$reglas_flujo |>
    dplyr::filter(.data$variable %in% variables)

  expect_setequal(reglas$variable, variables)
  expect_true(all(.diag_k_regla_tiene_edad(reglas$regla_r)))

  matriz <- construir_matriz_flujo_capitulo_k(usar_reglas_actuales = TRUE)
  regla_npckp46ab <- matriz$condicion_debe_responder_R[matriz$variable == "NPCKP46AB"]
  expect_true(length(regla_npckp46ab) == 1)
  expect_true(.diag_k_regla_tiene_edad(regla_npckp46ab))
})

test_that("construir_nodos_flujo_k no activa nodos en menores de 10", {
  nodos <- construir_nodos_flujo_k(tibble::tibble(
    edad = c(9, 10, NA),
    NPCKP17 = c(1, 1, 1),
    NPCKP44A = c(2, 2, 2)
  ))

  expect_false(nodos$llega_K45[[1]])
  expect_true(nodos$llega_K45[[2]])
  expect_true(is.na(nodos$llega_K45[[3]]))
})
