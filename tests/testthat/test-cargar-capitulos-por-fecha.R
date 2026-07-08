test_that("cargar_capitulos_por_fecha lee capitulos crudos como texto", {
  raiz <- tempfile("capitulos_")
  dir.create(raiz)
  on.exit(unlink(raiz, recursive = TRUE, force = TRUE), add = TRUE)
  fecha_corte <- "20260701"
  carpeta_caps <- file.path(raiz, paste0("CAP_EM_", fecha_corte))
  dir.create(carpeta_caps)

  ruta_k <- file.path(carpeta_caps, paste0("CAP_K_", fecha_corte, ".csv"))
  writeLines(
    c(
      "DIRECTORIO,SECUENCIA_P,ORDEN,NPCKP5_1,NPCKP6_1,NPCKP1,NPCKP2_1,NPCKP17,SEGMENTO",
      "1001,1,1,1,1,1,1,1,0501",
      "1002,1,1,2,2,2,2,2,0501",
      "1003,1,1,3,3,3,,3,0502",
      "1004,1,1,4,,4,1,4,0502",
      "1005,1,1,5,1,5,2,5,0503",
      "1006,1,1,6,2,1,,6,0503",
      "1007,1,1,7,3,2,1,7,0504",
      "1008,1,1,8,,5,2,8,0504"
    ),
    ruta_k,
    useBytes = TRUE
  )

  carga <- cargar_capitulos_por_fecha(
    fecha_corte = fecha_corte,
    carpeta_raiz = raiz,
    orden_caps = "K",
    verbose = FALSE
  )

  vars_k_sensibles <- c(
    "NPCKP5_1", "NPCKP6_1", "NPCKP1", "NPCKP2_1", "NPCKP17", "SEGMENTO"
  )

  expect_true(all(vapply(carga$dfs$K[vars_k_sensibles], is.character, logical(1))))
  expect_equal(nrow(carga$diagnostico_logical), 0L)
  expect_equal(nrow(diagnosticar_variables_logicas(carga$dfs)), 0L)
  expect_equal(
    sort(stats::na.omit(unique(carga$dfs$K$NPCKP5_1))),
    as.character(1:8)
  )
  expect_equal(
    sort(stats::na.omit(unique(carga$dfs$K$NPCKP6_1))),
    as.character(1:3)
  )
})

test_that("diagnosticar_variables_logicas identifica inferencia logica accidental", {
  diagnostico <- diagnosticar_variables_logicas(
    list(
      K = tibble::tibble(
        DIRECTORIO = c("1001", "1002"),
        NPCKP5_1 = c(TRUE, NA),
        NPCKP6_1 = c("1", "2")
      )
    )
  )

  expect_equal(nrow(diagnostico), 1L)
  expect_equal(diagnostico$capitulo, "K")
  expect_equal(diagnostico$variable, "NPCKP5_1")
  expect_equal(diagnostico$clase, "logical")
})
