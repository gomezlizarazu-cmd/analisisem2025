make_dfs_llaves_niveles_mock <- function() {
  list(
    A = tibble::tibble(
      DIRECTORIO = c("9001", "9002"),
      NVCAPCTRL1 = c(1, 1)
    ),
    C = tibble::tibble(
      DIRECTORIO = c("9001", "9001", "9002"),
      SECUENCIA_P = c("1", "2", "1"),
      NHCCPCTRL1 = c(1, 1, 1),
      NHCCPCTRL2 = c(1, 1, 1),
      RES_HOG = c(1, 1, 1)
    ),
    E = tibble::tibble(
      DIRECTORIO = c("9001", "9002"),
      SECUENCIA_P = c("1", "1"),
      ORDEN = c("1", "1"),
      NPCEP4 = c(35, 40),
      NPCEPCTRL1 = c(1, 1),
      RES_PER = c(1, 1),
      NPCEP6 = c(1, 1)
    ),
    F = tibble::tibble(
      DIRECTORIO = "9001",
      SECUENCIA_P = "1",
      ORDEN = "1",
      NPCFP1 = 1
    ),
    L = tibble::tibble(
      DIRECTORIO = c("9001", "9002"),
      SECUENCIA_P = c("2", "1"),
      ORDEN = c("99", "77"),
      VAR_L = c("l_hogar_2", "l_hogar_1")
    )
  )
}

test_that("L con ORDEN no entra al universo persona", {
  dfs <- make_dfs_llaves_niveles_mock()

  caps_persona_reales <- names(dfs)[vapply(
    names(dfs),
    function(cap) identical(tipo_capitulo[[cap]], "persona"),
    logical(1)
  )]

  personas_universo <- dplyr::bind_rows(lapply(caps_persona_reales, function(cap) {
    normalize_keys(dfs[[cap]], get_join_keys(cap)) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)
  })) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  ids_persona <- paste(
    personas_universo$DIRECTORIO,
    personas_universo$SECUENCIA_P,
    personas_universo$ORDEN,
    sep = "-"
  )

  expect_setequal(ids_persona, c("9001-1-1", "9002-1-1"))
  expect_false("9001-2-99" %in% ids_persona)
  expect_false("9002-1-77" %in% ids_persona)

  expect_equal(tipo_capitulo[["L"]], "hogar")
  expect_equal(get_join_keys("L"), c("DIRECTORIO", "SECUENCIA_P"))

  hogares_l <- normalize_keys(dfs$L, get_join_keys("L")) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P)

  expect_equal(nrow(hogares_l), 2L)
  expect_true(any(hogares_l$DIRECTORIO == "9001" & hogares_l$SECUENCIA_P == "2"))
})

test_that("sabana recuperable no clasifica ORDEN proveniente solo de L", {
  testthat::skip_if_not_installed("openxlsx")

  dfs <- list(
    C = tibble::tibble(
      DIRECTORIO = "9101",
      SECUENCIA_P = "1",
      NHCCPCTRL1 = 1,
      NHCCPCTRL2 = 1,
      RES_HOG = 1
    ),
    E = tibble::tibble(
      DIRECTORIO = "9101",
      SECUENCIA_P = "1",
      ORDEN = "1",
      NPCEP4 = 30,
      NPCEPCTRL1 = 1,
      RES_PER = 1,
      NPCEP6 = 1
    ),
    L = tibble::tibble(
      DIRECTORIO = "9101",
      SECUENCIA_P = "1",
      ORDEN = "99",
      VAR_L = "solo_hogar"
    )
  )

  diag_con_tematica <- list(
    reporte_final_caidas = tibble::tibble(
      DIRECTORIO = "9101",
      SECUENCIA_P = "1",
      ORDEN = "99",
      cae_existencia = TRUE,
      cae_lina = FALSE,
      cae_campo = FALSE,
      cae_duplicado = FALSE,
      cae_tematica = FALSE,
      n_criterios_reporte = 1L,
      criterios_reporte = "existencia"
    )
  )

  carpeta_salida <- tempfile("sabana_orden_l_")
  dir.create(carpeta_salida)

  salida <- construir_sabana_casos_recuperables_desde_diagnostico(
    diag_con_tematica = diag_con_tematica,
    dfs = dfs,
    carpeta_raiz = carpeta_salida
  )

  expect_equal(nrow(salida$diag_orden_fuera_E), 1L)
  expect_equal(
    salida$diag_orden_fuera_E$tipo_problema,
    "Llave sin evidencia en capitulos persona"
  )
  expect_equal(nrow(salida$casos_recuperables), 1L)
  expect_false(salida$casos_recuperables$recuperable_potencial)
  expect_equal(
    salida$casos_recuperables$estado_recuperacion,
    "auditable_no_recuperable_sin_evidencia_persona"
  )
  expect_equal(salida$casos_recuperables$nivel_evidencia_sospechosa, "hogar")
  caps_hogar <- trimws(strsplit(salida$casos_recuperables$capitulos_sospechosos_hogar, ",")[[1]])
  expect_true("L" %in% caps_hogar)
  expect_true("C" %in% caps_hogar)
  caps_persona <- salida$casos_recuperables$capitulos_sospechosos_persona
  expect_true(is.na(caps_persona) || identical(caps_persona, ""))
})

test_that("ORDEN solo en capitulos hogar no es recuperable aunque tenga ORDEN mayor a E", {
  testthat::skip_if_not_installed("openxlsx")

  cap_hog <- tibble::tibble(
    DIRECTORIO = "9201",
    SECUENCIA_P = "1",
    ORDEN = "2"
  )

  dfs <- list(
    C = cap_hog %>%
      dplyr::mutate(
        NHCCPCTRL1 = 1,
        NHCCPCTRL2 = 1,
        RES_HOG = 1
      ),
    D = cap_hog,
    L = cap_hog,
    MA = cap_hog,
    MB = cap_hog,
    E = tibble::tibble(
      DIRECTORIO = "9201",
      SECUENCIA_P = "1",
      ORDEN = "1",
      NPCEP4 = 42,
      NPCEPCTRL1 = 1,
      RES_PER = 1,
      NPCEP6 = 1
    )
  )

  diag_orden_fuera_E <- tibble::tibble(
    DIRECTORIO = "9201",
    SECUENCIA_P = "1",
    ORDEN = "2",
    tipo_problema = "ORDEN mayor que personas observadas en E"
  )

  diag_secuencia <- tibble::tibble(
    DIRECTORIO = "9201",
    SECUENCIA_P = "1",
    directorio_existe_en_E = TRUE
  )

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = "9201",
    SECUENCIA_P = "1",
    ORDEN = "2",
    cae_existencia = TRUE,
    cae_lina = FALSE,
    cae_campo = FALSE,
    cae_duplicado = FALSE,
    cae_tematica = FALSE,
    n_criterios_reporte = 1L,
    criterios_reporte = "existencia"
  )

  carpeta_salida <- tempfile("sabana_orden_solo_hogar_")
  dir.create(carpeta_salida)

  salida <- construir_sabana_casos_recuperables(
    diag_orden_fuera_E = diag_orden_fuera_E,
    diag_secuencia = diag_secuencia,
    dfs = dfs,
    carpeta_raiz = carpeta_salida,
    reporte_final_caidas = reporte_final_caidas
  )

  caso <- salida$casos_recuperables

  expect_equal(nrow(caso), 1L)
  expect_false(caso$recuperable_potencial)
  expect_equal(caso$control_hogar_sugiere_mas_personas, FALSE)
  expect_equal(caso$NHCCPCTRL2, 1)
  expect_equal(caso$max_orden_E, 1L)
  expect_equal(caso$n_capitulos_persona_sospechosos, 0L)
  expect_equal(caso$capitulos_sospechosos_hogar, "C, D, L, MA, MB")
  expect_equal(
    caso$estado_recuperacion,
    "auditable_no_recuperable_sin_evidencia_persona"
  )
})

test_that("ORDEN en capitulo persona real puede ser recuperable con control de hogar consistente", {
  testthat::skip_if_not_installed("openxlsx")

  dfs <- list(
    C = tibble::tibble(
      DIRECTORIO = "9301",
      SECUENCIA_P = "1",
      NHCCPCTRL1 = 1,
      NHCCPCTRL2 = 2,
      RES_HOG = 1
    ),
    E = tibble::tibble(
      DIRECTORIO = "9301",
      SECUENCIA_P = "1",
      ORDEN = "1",
      NPCEP4 = 35,
      NPCEPCTRL1 = 1,
      RES_PER = 1,
      NPCEP6 = 1
    ),
    F = tibble::tibble(
      DIRECTORIO = "9301",
      SECUENCIA_P = "1",
      ORDEN = "2",
      NPCFP1 = 1
    )
  )

  diag_orden_fuera_E <- tibble::tibble(
    DIRECTORIO = "9301",
    SECUENCIA_P = "1",
    ORDEN = "2",
    tipo_problema = "ORDEN mayor que personas observadas en E"
  )

  diag_secuencia <- tibble::tibble(
    DIRECTORIO = "9301",
    SECUENCIA_P = "1",
    directorio_existe_en_E = TRUE
  )

  reporte_final_caidas <- tibble::tibble(
    DIRECTORIO = "9301",
    SECUENCIA_P = "1",
    ORDEN = "2",
    cae_existencia = TRUE,
    cae_lina = FALSE,
    cae_campo = FALSE,
    cae_duplicado = FALSE,
    cae_tematica = FALSE,
    n_criterios_reporte = 1L,
    criterios_reporte = "existencia"
  )

  carpeta_salida <- tempfile("sabana_orden_persona_real_")
  dir.create(carpeta_salida)

  salida <- construir_sabana_casos_recuperables(
    diag_orden_fuera_E = diag_orden_fuera_E,
    diag_secuencia = diag_secuencia,
    dfs = dfs,
    carpeta_raiz = carpeta_salida,
    reporte_final_caidas = reporte_final_caidas
  )

  caso <- salida$casos_recuperables

  expect_equal(nrow(caso), 1L)
  expect_true(caso$recuperable_potencial)
  expect_equal(caso$control_hogar_sugiere_mas_personas, TRUE)
  expect_equal(caso$NHCCPCTRL2, 2)
  expect_equal(caso$max_orden_E, 1L)
  expect_equal(caso$capitulos_sospechosos_persona, "F")
  expect_equal(caso$nivel_evidencia_sospechosa, "persona | hogar")
})
