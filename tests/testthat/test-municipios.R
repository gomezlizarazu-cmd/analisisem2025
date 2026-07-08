test_that("agregar_municipios une MPIO character con cod_mpio numerico", {
  dfs <- list(
    A = tibble::tibble(
      DIRECTORIO = c("1001", "1002", "1003"),
      MPIO = c("11001", "05001", NA_character_)
    )
  )
  dic_mpios <- tibble::tibble(
    cod_mpio = c(11001, 5001),
    municipio = c("Bogota", "Medellin")
  )

  out <- agregar_municipios(
    dfs = dfs,
    dic_mpios = dic_mpios,
    cap = "A"
  )

  expect_equal(out$A$MPIO, c("11001", "05001", NA_character_))
  expect_equal(out$A$NomMunicipio, c("Bogota", "Medellin", NA_character_))
})

test_that("agregar_municipios conserva compatibilidad cuando MPIO viene numerico", {
  dfs <- list(
    A = tibble::tibble(
      DIRECTORIO = c("1001", "1002"),
      MPIO = c(11001, 5001)
    )
  )
  dic_mpios <- tibble::tibble(
    cod_mpio = c("11001", "05001"),
    municipio = c("Bogota", "Medellin")
  )

  out <- agregar_municipios(
    dfs = dfs,
    dic_mpios = dic_mpios,
    cap = "A"
  )

  expect_type(out$A$MPIO, "character")
  expect_equal(out$A$MPIO, c("11001", "05001"))
  expect_equal(out$A$NomMunicipio, c("Bogota", "Medellin"))
})
