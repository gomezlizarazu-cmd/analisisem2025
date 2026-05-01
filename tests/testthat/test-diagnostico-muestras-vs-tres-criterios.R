test_that("4001587 no se detecta en tres criterios y si en con tematica", {
  skip_if_not_installed("openxlsx")

  archivo_muestras <- tempfile(fileext = ".xlsx")

  openxlsx::write.xlsx(
    x = list(
      estrato_na = tibble::tibble(
        DIRECTORIO = "4001587",
        UUID = NA_character_
      )
    ),
    file = archivo_muestras,
    overwrite = TRUE
  )

  dfs_mock <- list(
    A = tibble::tibble(DIRECTORIO = "4001587")
  )

  viviendas_eval_mock <- tibble::tibble(
    DIRECTORIO = "4001587",
    cae_existencia = FALSE,
    cae_lina = FALSE,
    cae_campo = FALSE,
    n_criterios_caida = 0L,
    criterios_caida = "ninguno"
  )

  hogares_eval_mock <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    cae_existencia = logical(),
    cae_lina = logical(),
    cae_campo = logical(),
    n_criterios_caida = integer(),
    criterios_caida = character()
  )

  personas_eval_mock <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    ORDEN = character(),
    cae_existencia = logical(),
    cae_lina = logical(),
    cae_campo = logical(),
    n_criterios_caida = integer(),
    criterios_caida = character()
  )

  reporte_tres_mock <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    ORDEN = character(),
    cae_existencia = logical(),
    cae_lina = logical(),
    cae_campo = logical(),
    cae_duplicado = logical(),
    n_criterios_reporte = integer(),
    criterios_reporte = character(),
    criterio_principal_reporte = character(),
    razon_principal_caida = character(),
    variable_principal_caida = character(),
    valor_principal_caida = character(),
    observacion_final = character()
  )

  diag_tres_mock <- list(
    viviendas_eval = viviendas_eval_mock,
    hogares_eval = hogares_eval_mock,
    personas_eval = personas_eval_mock,
    reporte_final_caidas = reporte_tres_mock
  )

  reporte_tematica_mock <- tibble::tibble(
    DIRECTORIO = "4001587",
    SECUENCIA_P = "1",
    ORDEN = "1",
    cae_existencia = FALSE,
    cae_lina = FALSE,
    cae_campo = FALSE,
    cae_duplicado = FALSE,
    cae_tematica = TRUE,
    n_criterios_reporte = 1L,
    criterios_reporte = "tematica",
    criterio_principal_reporte = "tematica",
    razon_principal_caida = "Estrato faltante",
    variable_principal_caida = "NVCBP11AA",
    valor_principal_caida = NA_character_,
    observacion_final = "Detectado por tematica"
  )

  diag_con_tematica_mock <- list(
    viviendas_eval = tibble::tibble(
      DIRECTORIO = "4001587",
      cae_existencia = FALSE,
      cae_lina = FALSE,
      cae_campo = FALSE,
      cae_tematica = TRUE
    ),
    hogares_eval = tibble::tibble(
      DIRECTORIO = "4001587",
      SECUENCIA_P = "1",
      cae_existencia = FALSE,
      cae_lina = FALSE,
      cae_campo = FALSE,
      cae_tematica = TRUE
    ),
    personas_eval = tibble::tibble(
      DIRECTORIO = "4001587",
      SECUENCIA_P = "1",
      ORDEN = "1",
      cae_existencia = FALSE,
      cae_lina = FALSE,
      cae_campo = FALSE,
      cae_tematica = TRUE
    ),
    reporte_final_caidas = reporte_tematica_mock,
    diag_tres = diag_tres_mock
  )

  res_tres <- diagnostico_muestras_vs_tres_criterios(
    archivo_muestras = archivo_muestras,
    diag_tres = diag_tres_mock,
    dfs = dfs_mock,
    reporte_base = "tres_criterios"
  )

  expect_true("4001587" %in% res_tres$no_detectadas$DIRECTORIO)
  expect_false("4001587" %in% res_tres$casos_detectados$DIRECTORIO)

  res_tematica <- diagnostico_muestras_vs_tres_criterios(
    archivo_muestras = archivo_muestras,
    diag_tres = diag_tres_mock,
    diag_con_tematica = diag_con_tematica_mock,
    dfs = dfs_mock,
    reporte_base = "con_tematica"
  )

  expect_true("4001587" %in% res_tematica$casos_detectados$DIRECTORIO)
  expect_false("4001587" %in% res_tematica$no_detectadas$DIRECTORIO)
})
