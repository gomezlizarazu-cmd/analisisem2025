test_that("normaliza campos basicos para cruces", {
  expect_equal(
    normalizar_documento_cruce(c(" 1.234.0 ", "ab-45", NA)),
    c("1234", "AB45", NA_character_)
  )

  expect_equal(
    normalizar_texto_cruce(c(" Jose  Gomez ", "Maria-Luisa")),
    c("JOSE GOMEZ", "MARIA LUISA")
  )

  expect_equal(
    normalizar_tipo_documento_cruce(c(" c.c. ", "T.I", "CE")),
    c("CC", "TI", "CE")
  )

  expect_equal(
    normalizar_sexo_cruce(c("Mujer", "1", "HOMBRE", "x")),
    c("F", "M", "M", NA_character_)
  )
})

test_that("clasifica calidad documental para cruces", {
  expect_equal(
    clasificar_documento_cruce(c(NA, "", "0000", "123", "123456")),
    c(
      "sin_documento",
      "sin_documento",
      "documento_generico",
      "documento_muy_corto",
      "documento_potencialmente_valido"
    )
  )
})

test_that("diagnosticos genericos resumen columnas y variables", {
  df <- data.frame(
    a = c("x", "", NA, "y"),
    b = c(1, 1, 2, NA)
  )

  cols <- diagnosticar_columnas_requeridas(df, c("a", "c"), "demo")
  expect_equal(cols$base, c("demo", "demo"))
  expect_equal(cols$variable, c("a", "c"))
  expect_equal(cols$existe, c(TRUE, FALSE))

  diag <- diagnosticar_variable_basica(df, "a", "demo")
  expect_equal(diag$base, "demo")
  expect_equal(diag$variable, "a")
  expect_equal(diag$n, 4L)
  expect_equal(diag$n_na, 1L)
  expect_equal(diag$n_vacios, 1L)
  expect_equal(diag$n_unicos, 3L)
  expect_equal(diag$ejemplo_1, "x")
  expect_equal(diag$ejemplo_2, "")
  expect_equal(diag$ejemplo_3, "y")
})
