test_that("clasificar_completitud_campo acepta columnas de control leidas como texto", {
  dfs <- list(
    A = tibble::tibble(
      DIRECTORIO = c("1001", "1002"),
      UUID = c("u1", "u2"),
      RES_VIV = c("1", "2"),
      SEGMENTO = c("11001_001", NA_character_),
      CLASE = c("1", NA_character_),
      NVCAPCTRL1 = c("1", "1"),
      NVCAPCTRL2 = c("1", "1"),
      NVCAPCTRL1A = c(NA_character_, NA_character_),
      NVCAPCTRL2A = c(NA_character_, NA_character_)
    ),
    C = tibble::tibble(
      DIRECTORIO = c("1001", "1002"),
      SECUENCIA_P = c("1", "1"),
      NHCCPCTRL1 = c("1", "1"),
      NHCCPCTRL1A = c(NA_character_, NA_character_),
      RES_HOG = c("1", "2")
    ),
    E = tibble::tibble(
      DIRECTORIO = c("1001", "1002"),
      SECUENCIA_P = c("1", "1"),
      ORDEN = c("1", "1"),
      NPCEPCTRL1 = c("1", "1"),
      NPCEPCTRL1A = c(NA_character_, NA_character_),
      RES_PER = c("1", "2"),
      NPCEP4 = c("35", "11")
    )
  )

  base_eval <- clasificar_completitud_campo(dfs = dfs)
  diag_campo <- diagnostico_completitud_campo(dfs = dfs)

  expect_type(base_eval$CLASE, "character")
  expect_equal(base_eval$CLASE, c("1", "0"))
  expect_s3_class(diag_campo$resumen_general, "tbl_df")
  expect_equal(nrow(diag_campo$base_eval), 2L)
})
