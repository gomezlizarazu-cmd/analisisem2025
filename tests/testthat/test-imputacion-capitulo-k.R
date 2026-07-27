test_that("depurar_monto_capitulo_k excluye 98 y 99 en sus representaciones", {
  x <- c("98", "99", 98L, 99L, 98.0, 99.0, "1000000", "1.000.000", NA)
  out <- depurar_monto_capitulo_k(x)

  expect_true(all(is.na(out$monto[1:6])))
  expect_true(all(out$codigo_98[c(1, 3, 5)]))
  expect_true(all(out$codigo_99[c(2, 4, 6)]))
  expect_equal(out$monto[7:8], c(1000000, 1000000))
  expect_true(is.na(out$monto[9]))
})

test_that("horas extras solo se suman cuando no estaban incluidas", {
  monto <- depurar_monto_capitulo_k(c(250000, 250000, 98, 99))$monto

  expect_equal(
    analisisem2025:::.componente_monto_no_incluido_k(
      indicador = c(1L, 1L, 1L, 1L),
      monto = monto,
      incluido = c(1L, 2L, 2L, 2L)
    ),
    c(0, 250000, 0, 0)
  )
})

test_that("montos anuales validos se mensualizan y codigos especiales no", {
  monto <- depurar_monto_capitulo_k(c(1200000, 98, 99))$monto

  expect_equal(
    analisisem2025:::.mensualizar_monto_anual_k(
      indicador = c(1L, 1L, 1L),
      monto = monto
    ),
    c(100000, 0, 0)
  )
})

test_that("ingreso amplio sintetico no es inferior al acotado", {
  base <- depurar_monto_capitulo_k(c(1000000, 98, 99))$monto
  horas <- analisisem2025:::.componente_monto_no_incluido_k(
    indicador = c(1L, 1L, 1L),
    monto = depurar_monto_capitulo_k(c(200000, 98, 99))$monto,
    incluido = c(2L, 2L, 2L)
  )
  anual <- analisisem2025:::.mensualizar_monto_anual_k(
    indicador = c(1L, 1L, 1L),
    monto = depurar_monto_capitulo_k(c(1200000, 98, 99))$monto
  )
  acotado <- ifelse(!is.na(base), base + horas, NA_real_)
  amplio <- ifelse(!is.na(base), acotado + anual, NA_real_)

  expect_true(all(amplio[!is.na(amplio)] >= acotado[!is.na(acotado)]))
  expect_true(all(is.na(acotado[2:3])))
})

test_that("auditoria separa codigos especiales y no convertibles", {
  datos <- tibble::tibble(
    NPCKP23 = c("", "98", "99", "0", "1000", "sin dato")
  )
  out <- auditar_montos_capitulo_k(datos, "NPCKP23")

  expect_equal(out$n_no_vacios, 5)
  expect_equal(out$n_montos_validos, 2)
  expect_equal(out$n_codigo_98, 1)
  expect_equal(out$n_codigo_99, 1)
  expect_equal(out$n_no_convertibles, 1)
  expect_equal(out$n_ceros, 1)
  expect_equal(out$n_positivos, 1)
  expect_equal(out$minimo_positivo, 1000)
})

test_that("la depuracion conserva los valores originales observados", {
  original <- c("98", "99", "1000000", NA)
  copia <- original
  out <- depurar_monto_capitulo_k(original)

  expect_identical(original, copia)
  expect_identical(out$valor_original, original)
})

test_that("imputar_k41_k42 imputa, conserva originales y limpia al final", {
  datos <- tibble::tibble(
    DIRECTORIO = as.character(1:5),
    SECUENCIA_P = rep("1", 5),
    ORDEN = rep("1", 5),
    NPCKP17 = c("4", "5", "4", "1", "8"),
    NPCKP36 = c(NA, "5000", NA, NA, "98"),
    NPCKP36A = c(NA, "5000", NA, NA, "98"),
    NPCKP37 = c(NA, "2", NA, NA, NA),
    universo_npckp36_37 = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    vacio_critico_npckp36 = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    vacio_critico_npckp37 = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    recuperable_npckp36_acotado = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    ingreso_acotado = c(1000, 5000, 2000, 3000, 98),
    ingreso_amplio = c(1200, 5500, 2200, 3300, 120),
    alguna_respuesta_bloque_asalariados =
      c(TRUE, FALSE, TRUE, TRUE, TRUE),
    desviada_bloque_asalariados_en_universo_objetivo =
      c(TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  variables_limpieza <-
    variables_bloque_asalariados_limpieza_k41_k42()
  for (variable in variables_limpieza) {
    datos[[variable]] <- rep(NA_character_, nrow(datos))
  }
  datos$NPCKP23 <- c("1000", NA, "2000", "3000", "98")
  datos$NPCKP24 <- c("2", NA, "2", "2", "2")

  out <- imputar_k41_k42(datos, variables_limpieza)
  base <- out$base_k_imputada_k41_k42

  expect_equal(base$NPCKP36, c("1000", "5000", NA, NA, "98"))
  expect_equal(base$NPCKP36A, c("1000", "5000", NA, NA, "98"))
  expect_equal(base$NPCKP37, c("1", "2", NA, NA, NA))
  expect_equal(
    base$NPCKP36_ingresoamplioimputacionK4142,
    c(1200, NA, NA, NA, NA)
  )
  expect_equal(
    which(base$NPCKP36_flag_imputado),
    1L
  )
  expect_equal(
    which(base$NPCKP37_flag_imputado),
    1L
  )
  expect_identical(base$NPCKP36_original, datos$NPCKP36)
  expect_identical(base$NPCKP36A_original, datos$NPCKP36A)
  expect_identical(base$NPCKP37_original, datos$NPCKP37)
  expect_equal(base$NPCKP23_original, datos$NPCKP23)
  expect_true(is.na(base$NPCKP23[1]))
  expect_true(is.na(base$NPCKP24[1]))
  expect_equal(base$NPCKP23[3:4], datos$NPCKP23[3:4])
  expect_true(is.na(base$NPCKP23[5]))
  expect_equal(base$NPCKP23_original[5], "98")
  expect_equal(nrow(base), nrow(datos))
  expect_identical(
    base[c("DIRECTORIO", "SECUENCIA_P", "ORDEN")],
    datos[c("DIRECTORIO", "SECUENCIA_P", "ORDEN")]
  )
  expect_error(
    imputar_k41_k42(base, variables_limpieza),
    "no puede imputarse de nuevo"
  )
})

test_that("imputar_k41_k42 rechaza variables no exclusivas", {
  variables <- variables_bloque_asalariados_limpieza_k41_k42()
  expect_false("NPCKP36" %in% variables)
  expect_false("NPCKP37" %in% variables)
  expect_true(all(c("NPCKP35A", "NPCKP35AA", "NPCKP35_E") %in% variables))
})
