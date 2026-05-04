test_that("distribuir_variable usa etiquetas, total y universo del diccionario", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = c("1", "1", "1", "1"),
      SECUENCIA_P = c("1", "1", "1", "1"),
      ORDEN = c("1", "2", "3", "4"),
      NPCKP1 = c(1, NA, 1, 2)
    ),
    E = tibble::tibble(
      DIRECTORIO = c("1", "1", "1", "1"),
      SECUENCIA_P = c("1", "1", "1", "1"),
      ORDEN = c("1", "2", "3", "4"),
      NPCEP4 = c(9, 10, 20, 30)
    )
  )

  diccionario <- tibble::tibble(
    variable = c("NPCKP1", "NPCKP1"),
    opcion = c("1", "2"),
    etiqueta = c("Trabajando", "Buscando trabajo"),
    universo = c("edad >= 10", "edad >= 10")
  )

  out <- distribuir_variable(dfs, NPCKP1, diccionario)

  expect_equal(names(out), c("NPCKP1", "etiqueta", "n_personas", "pct_sobre_universo"))
  expect_equal(out$n_personas[out$NPCKP1 == "Total"], 3)
  expect_equal(out$pct_sobre_universo[out$NPCKP1 == "Total"], 1)
  expect_equal(out$etiqueta[out$NPCKP1 == "1"], "Trabajando")
  expect_equal(out$etiqueta[out$NPCKP1 == "2"], "Buscando trabajo")
  expect_equal(out$etiqueta[is.na(out$NPCKP1)], "Sin respuesta")
})
