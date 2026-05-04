make_dfs_em_completa_mock <- function() {
  list(
    A = tibble::tibble(
      DIRECTORIO = c("1001", "1002", "1003"),
      VAR_A = c("a1", "a2", "a3")
    ),
    B = tibble::tibble(
      DIRECTORIO = c("1001", "1001", "1002", "1003"),
      SECUENCIA_P = c("1", "2", "1", "1"),
      VAR_B = c("b1", "b2", "b3", "b4")
    ),
    E = tibble::tibble(
      DIRECTORIO = c("1001", "1001", "1002", "1003"),
      SECUENCIA_P = c("1", "2", "1", "1"),
      ORDEN = c("1", "1", "1", "1"),
      VAR_E = c("e1", "e2", "e3", "e4")
    )
  )
}

test_that("caso base excluye un DIRECTORIO caido", {
  dfs <- make_dfs_em_completa_mock()

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = c("1001", "1001", "1003"),
    cae_existencia = c(TRUE, FALSE, FALSE),
    cae_lina = c(FALSE, FALSE, FALSE)
  )

  res <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas
  )

  expect_equal(res$directorios_excluidos, "1001")
  expect_equal(res$n_encuestas_excluidas, 1L)
  expect_false("1001" %in% res$dfs$A$DIRECTORIO)
  expect_false("1001" %in% res$dfs$B$DIRECTORIO)
  expect_false("1001" %in% res$dfs$E$DIRECTORIO)
})

test_that("la exclusion se propaga correctamente a A, B y E", {
  dfs <- make_dfs_em_completa_mock()

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = c("1001", "1002"),
    cae_campo = c(TRUE, FALSE),
    cae_tematica = c(FALSE, TRUE)
  )

  res <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas
  )

  expect_setequal(res$directorios_excluidos, c("1001", "1002"))
  expect_setequal(res$dfs$A$DIRECTORIO, "1003")
  expect_setequal(res$dfs$B$DIRECTORIO, "1003")
  expect_setequal(res$dfs$E$DIRECTORIO, "1003")

  resumen <- res$resumen
  expect_equal(resumen$registros_antes[resumen$capitulo == "A"], 3L)
  expect_equal(resumen$registros_despues[resumen$capitulo == "A"], 1L)
  expect_equal(resumen$registros_excluidos[resumen$capitulo == "B"], 3L)
  expect_equal(resumen$registros_excluidos[resumen$capitulo == "E"], 3L)
})

test_that("error estructural si falta DIRECTORIO en algun capitulo", {
  dfs <- make_dfs_em_completa_mock()
  names(dfs$B)[names(dfs$B) == "DIRECTORIO"] <- "DIR"

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = "1001",
    cae_existencia = TRUE
  )

  expect_error(
    construir_base_em_completa(
      dfs = dfs,
      reporte_final_caidas = reporte_final_caidas
    ),
    "El capítulo `B` no contiene la variable DIRECTORIO"
  )
})

test_that("compatibilidad con una o varias variables de caida", {
  dfs <- make_dfs_em_completa_mock()

  reporte_simple <- tibble::tibble(
    DIRECTORIO = c("1001", "1002"),
    cae_existencia = c(TRUE, FALSE)
  )

  res_simple <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_simple
  )

  expect_equal(res_simple$directorios_excluidos, "1001")

  reporte_completo <- tibble::tibble(
    DIRECTORIO = c("1001", "1002", "1003"),
    cae_existencia = c(FALSE, FALSE, FALSE),
    cae_lina = c(FALSE, TRUE, FALSE),
    cae_campo = c(FALSE, FALSE, FALSE),
    cae_duplicado = c(FALSE, FALSE, FALSE),
    cae_tematica = c(FALSE, FALSE, TRUE)
  )

  res_completo <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_completo
  )

  expect_setequal(res_completo$directorios_excluidos, c("1002", "1003"))
  expect_setequal(res_completo$dfs$A$DIRECTORIO, "1001")
})

test_that("verificacion correcta conserva universos en vivienda hogar y persona", {
  dfs <- make_dfs_em_completa_mock()

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = c("1001", "1001"),
    cae_existencia = c(TRUE, FALSE)
  )

  em_completa <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas
  )

  verificacion <- verificar_universos_em_completa(
    dfs_original = dfs,
    dfs_completa = em_completa$dfs,
    directorios_excluidos = em_completa$directorios_excluidos
  )

  expect_true(verificacion$check_general)
  expect_true(all(verificacion$resumen$check_conservacion))
  expect_true(all(verificacion$resumen$check_excluidos_fuera_completa))
  expect_true(all(verificacion$resumen$check_sin_interseccion_llaves))
  expect_equal(nrow(verificacion$problemas), 0L)
})

test_that("verificacion detecta directorio excluido que permanece en base completa", {
  dfs <- make_dfs_em_completa_mock()

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = c("1001", "1001"),
    cae_existencia = c(TRUE, FALSE)
  )

  em_completa <- construir_base_em_completa(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas
  )

  dfs_completa_mal <- em_completa$dfs
  dfs_completa_mal$A <- dfs$A

  verificacion <- verificar_universos_em_completa(
    dfs_original = dfs,
    dfs_completa = dfs_completa_mal,
    directorios_excluidos = em_completa$directorios_excluidos
  )

  expect_false(verificacion$check_general)
  expect_true(any(verificacion$problemas$nivel == "vivienda"))
  expect_false(verificacion$resumen$check_excluidos_fuera_completa[verificacion$resumen$nivel == "vivienda"])
})

test_that("verificacion falla si falta DIRECTORIO", {
  dfs <- make_dfs_em_completa_mock()
  dfs_mal <- dfs
  names(dfs_mal$A)[names(dfs_mal$A) == "DIRECTORIO"] <- "DIR"

  expect_error(
    verificar_universos_em_completa(
      dfs_original = dfs_mal,
      dfs_completa = dfs,
      directorios_excluidos = "1001"
    ),
    "DIRECTORIO"
  )
})

test_that("verificacion falla si falta SECUENCIA_P", {
  dfs <- make_dfs_em_completa_mock()
  dfs_mal <- dfs
  names(dfs_mal$E)[names(dfs_mal$E) == "SECUENCIA_P"] <- "SEC"

  expect_error(
    verificar_universos_em_completa(
      dfs_original = dfs_mal,
      dfs_completa = dfs,
      directorios_excluidos = "1001"
    ),
    "SECUENCIA_P"
  )
})

test_that("verificacion falla si falta ORDEN", {
  dfs <- make_dfs_em_completa_mock()
  dfs_mal <- dfs
  names(dfs_mal$E)[names(dfs_mal$E) == "ORDEN"] <- "ORD"

  expect_error(
    verificar_universos_em_completa(
      dfs_original = dfs_mal,
      dfs_completa = dfs,
      directorios_excluidos = "1001"
    ),
    "ORDEN"
  )
})

test_that("verificacion falla si falta capitulo indicado", {
  dfs <- make_dfs_em_completa_mock()
  dfs_mal <- dfs
  dfs_mal$E <- NULL

  expect_error(
    verificar_universos_em_completa(
      dfs_original = dfs,
      dfs_completa = dfs_mal,
      directorios_excluidos = "1001"
    ),
    "El capítulo `E` no existe"
  )
})

test_that("auditoria diagnostica detecta observadas y no observadas en E", {
  dfs <- make_dfs_em_completa_mock()

  diag_con_tematica <- list(
    dfs = dfs,
    reporte_final_caidas = tibble::tibble(
      DIRECTORIO = c("1001", "1004"),
      SECUENCIA_P = c("1", "1"),
      ORDEN = c("1", "1"),
      cae_existencia = c(FALSE, TRUE),
      cae_lina = c(FALSE, TRUE),
      cae_campo = c(FALSE, FALSE),
      cae_duplicado = c(FALSE, FALSE),
      cae_tematica = c(FALSE, TRUE),
      criterio_principal_reporte = c("campo", "existencia")
    )
  )

  verif <- verificar_universos_em_completa_diag(
    diag = diag_con_tematica
  )

  expect_equal(verif$resumen$personas_diagnostico, 2L)
  expect_equal(verif$resumen$personas_observadas_en_E, 1L)
  expect_equal(verif$resumen$personas_no_observadas_en_E, 1L)
  expect_equal(nrow(verif$detalle$personas_observadas_en_E), 1L)
  expect_equal(nrow(verif$detalle$personas_no_observadas_en_E), 1L)
  expect_true("n" %in% names(verif$patrones_propagacion))
})

test_that("auditoria diagnostica da 100 por ciento observadas", {
  dfs <- make_dfs_em_completa_mock()

  diag_con_tematica <- list(
    dfs = dfs,
    reporte_final_caidas = tibble::tibble(
      DIRECTORIO = c("1001", "1002"),
      SECUENCIA_P = c("1", "1"),
      ORDEN = c("1", "1"),
      cae_existencia = c(TRUE, FALSE),
      cae_lina = c(FALSE, TRUE)
    )
  )

  verif <- verificar_universos_em_completa_diag(
    diag = diag_con_tematica
  )

  expect_equal(verif$resumen$personas_no_observadas_en_E, 0L)
  expect_equal(nrow(verif$detalle$personas_no_observadas_en_E), 0L)
  expect_equal(verif$resumen$pct_observadas_en_E, 1)
})

test_that("auditoria diagnostica da 100 por ciento propagadas", {
  dfs <- make_dfs_em_completa_mock()

  diag_con_tematica <- list(
    dfs = dfs,
    reporte_final_caidas = tibble::tibble(
      DIRECTORIO = c("9001", "9002"),
      SECUENCIA_P = c("1", "1"),
      ORDEN = c("1", "1"),
      cae_existencia = c(TRUE, TRUE),
      cae_lina = c(TRUE, TRUE),
      cae_campo = c(FALSE, FALSE),
      cae_duplicado = c(FALSE, FALSE),
      cae_tematica = c(TRUE, FALSE)
    )
  )

  verif <- verificar_universos_em_completa_diag(
    diag = diag_con_tematica
  )

  expect_equal(verif$resumen$personas_observadas_en_E, 0L)
  expect_equal(verif$resumen$personas_no_observadas_en_E, 2L)
  expect_equal(nrow(verif$detalle$personas_observadas_en_E), 0L)
  expect_equal(verif$resumen$pct_no_observadas_en_E, 1)
})

test_that("auditoria diagnostica falla si diag no tiene reporte_final_caidas", {
  diag_sin_reporte <- list(dfs = make_dfs_em_completa_mock())

  expect_error(
    verificar_universos_em_completa_diag(diag = diag_sin_reporte),
    "reporte_final_caidas"
  )
})

test_that("construye sabana de casos recuperables con tipos ORDEN y SECUENCIA_P", {
  testthat::skip_if_not_installed("openxlsx")

  dfs <- make_dfs_em_completa_mock()
  carpeta_salida <- tempfile("sabana_recuperables_")
  dir.create(carpeta_salida)

  diag_orden_fuera_E <- tibble::tibble(
    DIRECTORIO = c("1001", "1002", "1003", "1004"),
    SECUENCIA_P = c("1", "2", "1", "1"),
    ORDEN = c("2", "1", "3", "1"),
    tipo_problema = c(
      "ORDEN mayor que personas observadas en E",
      "Hogar no observado en E",
      "Hogar no observado en E",
      "DIRECTORIO no existe en E"
    )
  )

  diag_secuencia <- tibble::tibble(
    DIRECTORIO = c("1002", "1003"),
    SECUENCIA_P = c("2", "1"),
    directorio_existe_en_E = c(TRUE, FALSE)
  )

  salida <- construir_sabana_casos_recuperables(
    diag_orden_fuera_E = diag_orden_fuera_E,
    diag_secuencia = diag_secuencia,
    dfs = dfs,
    carpeta_raiz = carpeta_salida
  )

  expect_equal(nrow(salida$casos_recuperables), 2L)
  expect_setequal(salida$casos_recuperables$tipo_recuperacion, c("ORDEN", "SECUENCIA_P"))
  expect_true(file.exists(salida$ruta))
  expect_true(all(c("1001", "1002") %in% salida$resumen_directorios$DIRECTORIO))
})

test_that("falla si faltan variables requeridas para construir sabana recuperable", {
  dfs <- make_dfs_em_completa_mock()

  diag_orden_fuera_E <- tibble::tibble(
    DIRECTORIO = "1001",
    SECUENCIA_P = "1",
    tipo_problema = "ORDEN mayor que personas observadas en E"
  )

  diag_secuencia <- tibble::tibble(
    DIRECTORIO = "1001",
    SECUENCIA_P = "1",
    directorio_existe_en_E = TRUE
  )

  expect_error(
    construir_sabana_casos_recuperables(
      diag_orden_fuera_E = diag_orden_fuera_E,
      diag_secuencia = diag_secuencia,
      dfs = dfs
    ),
    "ORDEN"
  )
})

test_that("auditoria diagnostica falla si diag no tiene dfs y dfs_original es NULL", {
  diag_sin_dfs <- list(
    reporte_final_caidas = tibble::tibble(
      DIRECTORIO = "1001",
      SECUENCIA_P = "1",
      ORDEN = "1",
      cae_existencia = TRUE
    )
  )

  expect_error(
    verificar_universos_em_completa_diag(diag = diag_sin_dfs),
    "No se pudo resolver dfs_original"
  )
})

test_that("auditoria diagnostica falla si falta ORDEN", {
  dfs <- make_dfs_em_completa_mock()
  diag_mal <- list(
    dfs = dfs,
    reporte_final_caidas = tibble::tibble(
      DIRECTORIO = "1001",
      SECUENCIA_P = "1",
      cae_existencia = TRUE
    )
  )

  expect_error(
    verificar_universos_em_completa_diag(diag = diag_mal),
    "ORDEN"
  )
})
