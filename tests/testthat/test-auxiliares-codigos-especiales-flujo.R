test_that("es_codigo_especial_flujo reconoce 98 y 99 sin confundir faltantes", {
  es_especial <- analisisem2025:::es_codigo_especial_flujo

  expect_true(es_especial(98))
  expect_true(es_especial("98"))
  expect_true(es_especial(99))
  expect_true(es_especial(" 99 "))
  expect_false(es_especial(500000))
  expect_false(es_especial(NA))
  expect_false(es_especial(""))
  expect_true(es_especial(factor("98")))
})

test_that("es_respuesta_sustantiva conserva la semantica de tres estados", {
  es_sustantiva <- analisisem2025:::es_respuesta_sustantiva
  valores <- c(
    "500000", 98, "99", NA, "", " NA ", "texto informado"
  )

  expect_identical(
    es_sustantiva(valores),
    c(TRUE, FALSE, FALSE, NA, NA, NA, TRUE)
  )
  expect_true(es_sustantiva(0))
})

test_that("es_monto_sustantivo distingue monto, especiales, vacio y cero", {
  es_monto <- analisisem2025:::es_monto_sustantivo
  valores <- c("500000", 98, " 99 ", NA, "", "sin monto", 0, -1)

  expect_identical(
    es_monto(valores),
    c(TRUE, FALSE, FALSE, NA, NA, NA, FALSE, FALSE)
  )
  expect_false(es_monto(98))
  expect_false(es_monto("98"))
  expect_false(es_monto(99))
  expect_false(es_monto(" 99 "))
  expect_true(es_monto(0, permitir_cero = TRUE))
  expect_true(es_monto(factor("250000")))
})
