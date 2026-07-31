test_that("diagnostico_flujo_capitulo_k aplica universo de edad general", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = c("1", "1", "1"),
      SECUENCIA_P = c("1", "1", "1"),
      ORDEN = c("1", "2", "3"),
      NPCKP3 = c(1, 1, 1),
      NPCKP5_1 = c(5, 5, 5),
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
    NPCKP2_1 = c(1, 1, 1),
    NPCKP17 = c(1, 1, 1),
    NPCKP44A = c(2, 2, 2)
  ))

  expect_false(nodos$llega_K45[[1]])
  expect_true(nodos$llega_K45[[2]])
  expect_true(is.na(nodos$llega_K45[[3]]))
})

test_that("reglas de entrada y busqueda usan rutas teoricas completas", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 8),
      SECUENCIA_P = rep("1", 8),
      ORDEN = sprintf("%02d", seq_len(8)),
      NPCKP3 = c(2, 1, NA, NA, NA, NA, NA, NA),
      NPCKP5_1 = c(NA, 5, NA, NA, NA, NA, NA, NA),
      NPCKP6_1 = c(NA, 2, NA, NA, NA, NA, NA, NA),
      NPCKP8 = c(NA, NA, 8, 9, 10, 11, NA, NA),
      NPCKP10 = c(NA, NA, NA, NA, NA, NA, 1, NA),
      NPCKP11 = c(NA, NA, NA, NA, NA, NA, NA, 1),
      NPCKP5 = c(NA, NA, NA, NA, NA, NA, 1, 2),
      NPCKP7 = c(NA, NA, NA, NA, NA, NA, NA, 1),
      NPCKP4 = NA,
      NPCKP9 = NA,
      NPCKP12 = NA,
      NPCKP13 = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 8),
      SECUENCIA_P = rep("1", 8),
      ORDEN = sprintf("%02d", seq_len(8)),
      NPCEP4 = rep(30, 8)
    )
  )

  diag <- diagnostico_flujo_capitulo_k(
    dfs,
    vars_cap_k = c("NPCKP4", "NPCKP9", "NPCKP12", "NPCKP13")
  )

  esperados <- list(
    NPCKP4 = c(TRUE, TRUE, NA, NA, NA, NA, NA, NA),
    NPCKP9 = c(NA, NA, TRUE, FALSE, FALSE, FALSE, NA, NA),
    NPCKP12 = c(NA, NA, NA, NA, NA, NA, TRUE, TRUE),
    NPCKP13 = c(NA, NA, NA, NA, NA, NA, TRUE, TRUE)
  )

  for (variable in names(esperados)) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperados[[variable]]))
    testthat::expect_equal(out$debe_responder, esperados[[variable]])
  }
})

test_that("entrada ocupados abre K14-K23 y evita respuestas fuera de flujo", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 8),
      SECUENCIA_P = rep("1", 8),
      ORDEN = sprintf("%02d", seq_len(8)),
      NPCKP2_1 = c(1, NA, NA, NA, NA, NA, NA, 2),
      NPCKP2 = c(NA, 1, NA, NA, NA, NA, NA, 2),
      NPCKP3 = c(NA, NA, 1, 1, 2, 1, 1, 1),
      NPCKP5_1 = c(NA, NA, 3, 5, NA, 5, 5, 5),
      NPCKP6_1 = c(NA, NA, NA, 1, NA, 2, 2, 2),
      NPCKP4 = c(NA, NA, NA, NA, 1, 1, 1, 2),
      NPCKP18 = c(NA, NA, NA, NA, NA, NA, 1, 1),
      NPCKP19 = c(NA, NA, NA, NA, NA, NA, 2, NA),
      NPCKP20 = c(NA, NA, NA, NA, NA, NA, 2, 2),
      NPCKP17 = c(1, 1, 1, 1, 1, 1, 1, 1),
      NPCKP14 = NA,
      NPCKP15 = NA,
      NPCKP16 = NA,
      NPCKP20A = NA,
      NPCKP38A = NA,
      NPCKP36 = NA,
      NPCKP37 = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 8),
      SECUENCIA_P = rep("1", 8),
      ORDEN = sprintf("%02d", seq_len(8)),
      NPCEP4 = rep(30, 8)
    )
  )

  variables <- c("NPCKP14", "NPCKP18", "NPCKP17", "NPCKP19", "NPCKP20", "NPCKP20A", "NPCKP38A", "NPCKP36", "NPCKP37")
  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = variables)

  esperado_entrada <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

  for (variable in c("NPCKP14", "NPCKP18", "NPCKP17", "NPCKP38A")) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperado_entrada))
    testthat::expect_equal(out$debe_responder, esperado_entrada)
  }

  for (variable in c("NPCKP36", "NPCKP37")) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperado_entrada))
    testthat::expect_equal(out$debe_responder, rep(FALSE, length(esperado_entrada)))
  }

  out_19 <- diag$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == "NPCKP19") |>
    dplyr::arrange(.data$ORDEN)
  testthat::expect_equal(out_19$debe_responder, c(NA, NA, NA, NA, NA, NA, TRUE, FALSE))

  out_20 <- diag$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == "NPCKP20") |>
    dplyr::arrange(.data$ORDEN)
  testthat::expect_equal(out_20$debe_responder, c(NA, NA, NA, NA, NA, NA, TRUE, FALSE))

  out_20a <- diag$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == "NPCKP20A") |>
    dplyr::arrange(.data$ORDEN)
  testthat::expect_equal(out_20a$debe_responder, c(NA, NA, NA, NA, NA, NA, TRUE, FALSE))
})

test_that("K53 Caballo aplica para Bogota rural y municipios", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 10),
      SECUENCIA_P = rep("1", 10),
      ORDEN = sprintf("%02d", seq_len(10)),
      MPIO = c(11001, 11001, 11001, 25754, 5001, 25754, 25754, 25754, NA, 11001),
      CLASE = c(1, 2, 3, 1, 1, 1, 1, 1, 1, NA),
      NPCKP2_1 = rep(1, 10),
      NPCKP17 = rep(1, 10),
      NPCKP44A = c(7, 7, 7, 7, 7, 1, 7, 7, 7, 7),
      NPCKP45L = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 10),
      SECUENCIA_P = rep("1", 10),
      ORDEN = sprintf("%02d", seq_len(10)),
      NPCEP4 = c(30, 30, 30, 30, 30, 30, 9, NA, 30, 30)
    )
  )

  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = "NPCKP45L")
  esperado <- c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, NA, NA, NA)
  variable <- "NPCKP45L"

  out <- diag$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == .env$variable) |>
    dplyr::arrange(.data$ORDEN)

  testthat::expect_equal(nrow(out), length(esperado))
  testthat::expect_equal(out$debe_responder, esperado)

  regla <- diag$reglas_flujo |>
    dplyr::filter(.data$variable == .env$variable)

  testthat::expect_equal(nrow(regla), 1L)
  testthat::expect_true(grepl("MPIO != 11001", regla$regla_r, fixed = TRUE))
  testthat::expect_true(grepl("CLASE %in% c(2,3)", regla$regla_r, fixed = TRUE))
  testthat::expect_false(grepl("25754", regla$regla_r, fixed = TRUE))
  testthat::expect_true(grepl("NPCKP17", regla$variables_previas_usadas, fixed = TRUE))
  testthat::expect_true(grepl("MPIO", regla$variables_previas_usadas, fixed = TRUE))
  testthat::expect_true(grepl("CLASE", regla$variables_previas_usadas, fixed = TRUE))
})

test_that("K60 y K61 dependen de K59 igual a no", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 10),
      SECUENCIA_P = rep("1", 10),
      ORDEN = sprintf("%02d", seq_len(10)),
      NPCKP1 = c(5, 5, 5, 1, 5, 5, 5, 5, 5, 5),
      NPCKP7 = c(NA, NA, NA, 1, NA, NA, NA, NA, NA, NA),
      NPCKP13 = c(NA, NA, NA, 0, NA, NA, NA, NA, NA, NA),
      NPCKP47B = NA,
      NPCKP47C = c(2, 1, NA, 2, 2, 2, 2, 2, 2, 2),
      NPCKP60_1 = c(NA, NA, NA, NA, NA, NA, NA, NA, "0", "00"),
      NPCKP60_2 = c(NA, NA, NA, NA, NA, NA, NA, NA, "00", "0"),
      NPCKP61_1 = c(NA, NA, NA, NA, 9, 3, NA, NA, NA, NA),
      NPCKP61_2 = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 10),
      SECUENCIA_P = rep("1", 10),
      ORDEN = sprintf("%02d", seq_len(10)),
      NPCEP4 = c(30, 30, 30, 30, 30, 30, 30, 9, NA, 30)
    )
  )

  variables <- c("NPCKP60_1", "NPCKP60_2", "NPCKP61_1", "NPCKP61_2")
  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = variables)

  esperado_k60_k61 <- c(TRUE, FALSE, NA, FALSE, TRUE, TRUE, TRUE, FALSE, NA, TRUE)
  esperado_61_2 <- c(NA, FALSE, NA, FALSE, TRUE, FALSE, NA, FALSE, NA, NA)

  for (variable in c("NPCKP60_1", "NPCKP60_2", "NPCKP61_1")) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperado_k60_k61))
    testthat::expect_equal(out$debe_responder, esperado_k60_k61)
  }

  variable <- "NPCKP61_2"
  out_61_2 <- diag$diagnostico_persona_variable |>
    dplyr::filter(.data$variable == .env$variable) |>
    dplyr::arrange(.data$ORDEN)

  testthat::expect_equal(nrow(out_61_2), length(esperado_61_2))
  testthat::expect_equal(out_61_2$debe_responder, esperado_61_2)

  ceros_validos <- diag$diagnostico_persona_variable |>
    dplyr::filter(
      .data$variable %in% c("NPCKP60_1", "NPCKP60_2"),
      .data$ORDEN %in% c("09", "10")
    ) |>
    dplyr::arrange(.data$variable, .data$ORDEN)

  testthat::expect_true(all(ceros_validos$debe_responder %in% c(TRUE, NA)))
  testthat::expect_false(any(ceros_validos$vacio_critico %in% TRUE))

  reglas <- diag$reglas_flujo |>
    dplyr::filter(.data$variable %in% variables)

  testthat::expect_false(any(grepl("NPCKP47C tiene valor", reglas$regla_r, fixed = TRUE)))
  testthat::expect_true(all(grepl("NPCKP47C == 2", reglas$regla_r, fixed = TRUE)))
  testthat::expect_true(grepl("NPCKP61_1 == 9", reglas$regla_r[reglas$variable == "NPCKP61_2"], fixed = TRUE))
})

test_that("base k3 gobierna pensiones, renta, labores y acoso", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 6),
      SECUENCIA_P = rep("1", 6),
      ORDEN = sprintf("%02d", seq_len(6)),
      NPCKP1 = c(5, 5, NA, NA, NA, 1),
      NPCKP7 = c(NA, NA, NA, NA, NA, 1),
      NPCKP13 = c(NA, NA, 1, 2, NA, 0),
      NPCKP2_1 = c(NA, NA, NA, NA, 1, 2),
      NPCKP2 = c(NA, NA, NA, NA, NA, 2),
      NPCKP3 = c(NA, NA, NA, NA, NA, 1),
      NPCKP5_1 = c(NA, NA, NA, NA, NA, 5),
      NPCKP6_1 = c(NA, NA, NA, NA, NA, 2),
      NPCKP4 = c(NA, NA, NA, NA, NA, 2),
      NPCKP17 = c(NA, NA, NA, NA, 1, NA),
      NPCKP50 = c(NA, 1, 2, 1, NA, NA),
      NPCKPN62A = c(NA, NA, NA, 1, NA, NA),
      NPCKP52 = c(NA, NA, NA, NA, NA, 1),
      NPCKP50_A = NA,
      NPCKP51 = NA,
      NPCKPN62B = NA,
      NPCKP78_1 = NA,
      NPCKP52A = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 6),
      SECUENCIA_P = rep("1", 6),
      ORDEN = sprintf("%02d", seq_len(6)),
      NPCEP4 = c(14, 15, 17, 18, 18, 30)
    )
  )

  variables <- c("NPCKP50_A", "NPCKP50", "NPCKP51", "NPCKPN62A", "NPCKPN62B", "NPCKP78_1", "NPCKP52A")
  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = variables)

  esperados <- list(
    NPCKP50_A = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    NPCKP50 = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
    NPCKP51 = c(FALSE, TRUE, FALSE, TRUE, NA, FALSE),
    NPCKPN62A = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    NPCKPN62B = c(FALSE, FALSE, FALSE, TRUE, NA, FALSE),
    NPCKP78_1 = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    NPCKP52A = c(NA, NA, NA, NA, NA, FALSE)
  )

  for (variable in names(esperados)) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperados[[variable]]))
    testthat::expect_equal(out$debe_responder, esperados[[variable]])
  }

  reglas <- diag$reglas_flujo |>
    dplyr::filter(.data$variable %in% variables)

  testthat::expect_false(any(grepl("NPCKP47B tiene valor", reglas$regla_r, fixed = TRUE)))
  testthat::expect_true(grepl("edad >= 15", reglas$regla_r[reglas$variable == "NPCKP50"], fixed = TRUE))
  testthat::expect_true(all(grepl("edad >= 18", reglas$regla_r[reglas$variable %in% c("NPCKPN62A", "NPCKP78_1")], fixed = TRUE)))
})

test_that("base k3 descarta menores de 10 y preserva edad faltante", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 8),
      SECUENCIA_P = rep("1", 8),
      ORDEN = sprintf("%02d", seq_len(8)),
      NPCKP1 = c(5, 1, 1, 1, 5, 5, 1, 1),
      NPCKP7 = c(1, 2, 1, 1, 1, 1, 2, 1),
      NPCKP13 = c(0, 0, 1, 0, 0, 0, 0, 1),
      NPCKP2_1 = c(2, 2, 2, 1, 2, 2, 2, 2),
      NPCKP2 = c(2, 2, 2, NA, 2, 2, 2, 2),
      NPCKP3 = c(2, 2, 2, NA, 2, 2, 2, 2),
      NPCKP5_1 = c(0, 0, 0, NA, 0, 0, 0, 0),
      NPCKP6_1 = c(0, 0, 0, NA, 0, 0, 0, 0),
      NPCKP4 = c(2, 2, 2, NA, 2, 2, 2, 2),
      NPCKP17 = c(NA, NA, NA, 1, NA, NA, NA, NA),
      NPCKP50_A = NA,
      NPCKP52 = NA,
      NPCKP73_1 = NA,
      NPCKP59A = NA,
      NPCKP78_1 = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 8),
      SECUENCIA_P = rep("1", 8),
      ORDEN = sprintf("%02d", seq_len(8)),
      NPCEP4 = c(9, 9, 9, 9, 10, NA, NA, NA)
    )
  )

  variables <- c("NPCKP50_A", "NPCKP52", "NPCKP73_1", "NPCKP59A", "NPCKP78_1")
  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = variables)

  esperado_base_k3 <- c(FALSE, FALSE, FALSE, FALSE, TRUE, NA, NA, NA)
  esperados <- list(
    NPCKP50_A = esperado_base_k3,
    NPCKP52 = esperado_base_k3,
    NPCKP73_1 = esperado_base_k3,
    NPCKP59A = esperado_base_k3,
    NPCKP78_1 = c(FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA)
  )

  for (variable in names(esperados)) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperados[[variable]]))
    testthat::expect_equal(out$debe_responder, esperados[[variable]])
  }
})

test_that("K55 departamento y municipio dependen de otro municipio", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 7),
      SECUENCIA_P = rep("1", 7),
      ORDEN = as.character(seq_len(7)),
      MPIO = c(5001, 5001, 5001, 5001, 5001, 11001, 25754),
      NPCKP2_1 = rep(1, 7),
      NPCKP17 = rep(1, 7),
      NPCKP44A = c(7, 7, 1, 7, 7, 7, 7),
      NPCKPA46 = c(2, 1, 2, 2, 2, 2, 2),
      NPCKP46AB = NA,
      NPCKP46AC = NA,
      NPCKP46AD = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 7),
      SECUENCIA_P = rep("1", 7),
      ORDEN = as.character(seq_len(7)),
      NPCEP4 = c(30, 30, 30, 9, NA, 30, 30)
    )
  )

  diag <- diagnostico_flujo_capitulo_k(
    dfs,
    vars_cap_k = c("NPCKPA46", "NPCKP46AB", "NPCKP46AC", "NPCKP46AD")
  )

  esperado <- c(TRUE, FALSE, FALSE, FALSE, NA, TRUE, TRUE)

  for (variable in c("NPCKP46AB", "NPCKP46AC")) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    expect_equal(nrow(out), 7L)
    expect_equal(out$debe_responder, esperado)
  }

  reglas <- diag$reglas_flujo |>
    dplyr::filter(.data$variable %in% c("NPCKP46AB", "NPCKP46AC"))

  expect_true(all(grepl("NPCKPA46 == 2", reglas$regla_r, fixed = TRUE)))
  expect_false(any(grepl("MPIO", reglas$regla_r, fixed = TRUE)))
  expect_true(all(grepl("NPCKPA46", reglas$variables_previas_usadas, fixed = TRUE)))
})
