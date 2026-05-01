make_dfs_duplicados_mock <- function() {
  list(
    E = tibble::tibble(
      DIRECTORIO = c("1001", "1002", "2001", "2002", "3001", "3002"),
      SECUENCIA_P = c("1", "1", "1", "1", "1", "1"),
      ORDEN = c("1", "1", "1", "1", "1", "1"),
      UUID = c("u1", "u2", "u3", "u4", "u5", "u6"),
      NPCEP2 = c(
        "ALFA PERSONA",
        "ALFA PERSONA",
        "NO INFORMA",
        "NO INFORMA POR MOTIVOS DE SEGURIDAD",
        "BETA PERSONA",
        "BETA PERSONA"
      ),
      NPCEP3A = c(
        "01/01/2000",
        "02/01/2000",
        "05/05/2010",
        "06/05/2010",
        "03/03/2012",
        "03/03/2012"
      ),
      NPCEP_5 = c("1", "1", "2", "2", "1", "2"),
      NPCEP_5A = c("12345", "12345", NA, NA, "55555", NA),
      NPCEP_5B = c(NA, NA, "99", "99", NA, "55555"),
      NPCEP_5C = c(NA, NA, NA, NA, NA, NA),
      NPCEP_5D = c(NA, NA, NA, NA, NA, NA)
    )
  )
}

test_that("default usa dane_sin_placeholder", {
  dfs <- make_dfs_duplicados_mock()

  res <- diagnostico_duplicados_personas_e(dfs = dfs, cap_persona = "E")

  expect_true(all(res$personas_duplicadas$criterio_duplicados == "dane_sin_placeholder"))
  expect_equal(nrow(res$personas_duplicadas), 2)
  expect_setequal(res$personas_duplicadas$DIRECTORIO, c("1001", "1002"))
  expect_false(any(grepl("^NO INFORMA", res$personas_duplicadas$NPCEP2)))
  expect_false(any(res$personas_duplicadas$numero_documento_normalizado %in% c("99", "9999999999", "0000000000", "1111111111", "1234567890")))
})

test_that("criterio actual reproduce la logica historica", {
  dfs <- make_dfs_duplicados_mock()

  res <- diagnostico_duplicados_personas_e(
    dfs = dfs,
    cap_persona = "E",
    criterio_duplicados = "actual"
  )

  expect_true(all(res$personas_duplicadas$criterio_duplicados == "actual"))
  expect_equal(nrow(res$personas_duplicadas), 2)
  expect_setequal(res$personas_duplicadas$DIRECTORIO, c("3001", "3002"))
  expect_true(all(!is.na(res$personas_duplicadas$fecha_nac_normalizada)))
})

test_that("dane_completa detecta misma identidad documental aunque cambie fecha", {
  dfs <- make_dfs_duplicados_mock()

  res <- diagnostico_duplicados_personas_e(
    dfs = dfs,
    cap_persona = "E",
    criterio_duplicados = "dane_completa"
  )

  expect_true(all(res$personas_duplicadas$criterio_duplicados == "dane_completa"))
  expect_equal(nrow(res$personas_duplicadas), 4)
  expect_true(all(c("1001", "1002") %in% res$personas_duplicadas$DIRECTORIO))
  expect_true(any(res$personas_duplicadas$numero_documento_normalizado == "99"))
})

test_that("dane_sin_placeholder excluye placeholders de nombres y documentos", {
  dfs <- make_dfs_duplicados_mock()

  res <- diagnostico_duplicados_personas_e(
    dfs = dfs,
    cap_persona = "E",
    criterio_duplicados = "dane_sin_placeholder"
  )

  expect_false(any(res$personas_duplicadas$numero_documento_normalizado %in% c(
    "99", "0000000000", "9999999999", "1111111111", "1234567890"
  )))
  expect_false(any(res$personas_duplicadas$nombre_normalizado %in% c(
    "NO INFORMA", "SIN INFORMACION"
  )))
  expect_false(any(grepl("^NO INFORMA", res$personas_duplicadas$nombre_normalizado)))
})

test_that("diagnostico_caidas_tres_criterios propaga criterio_duplicados", {
  criterio_capturado <- NULL

  empty_existencia <- tibble::tibble(
    nivel = character(),
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    ORDEN = character(),
    capitulos_caida = character(),
    fuentes_caida = character(),
    n_eventos = integer()
  )

  testthat::local_mocked_bindings(
    diagnostico_cruce_capitulos = function(...) {
      list(resumen_caidas_regla2 = empty_existencia)
    },
    diagnostico_completitud_lina = function(...) {
      list(
        viviendas_eval = tibble::tibble(
          DIRECTORIO = "1",
          UUID = "u1",
          SEGMENTO = "s1",
          CLASE = "1",
          vivienda_completa_lina = TRUE,
          motivo_vivienda_lina = NA_character_
        ),
        hogares_eval = tibble::tibble(
          DIRECTORIO = "1",
          SECUENCIA_P = "1",
          hogar_completo_lina = TRUE,
          motivo_hogar_lina = NA_character_
        ),
        personas_eval = tibble::tibble(
          DIRECTORIO = "1",
          SECUENCIA_P = "1",
          ORDEN = "1",
          edad = 10,
          persona_completa_lina = TRUE,
          motivo_persona_lina = NA_character_
        )
      )
    },
    diagnostico_completitud_campo = function(...) {
      list(
        base_eval = tibble::tibble(
          DIRECTORIO = "1",
          encuesta_efectiva_campo = FALSE,
          encuesta_completa_campo = TRUE,
          viv_ocupada_presente = TRUE,
          viv_resultado_completo = TRUE,
          todos_hogares_completos = TRUE,
          todas_personas_completas = TRUE,
          estado_viv = NA_character_,
          hogares_incompletos = 0L,
          personas_incompletas = 0L
        )
      )
    },
    .preparar_causal_conteo_personas_hogar = function(...) {
      tibble::tibble(
        DIRECTORIO = "1",
        SECUENCIA_P = "1",
        NHCCPCTRL2 = 1,
        n_personas_cap_e = 1L,
        diferencia_personas_hogar = 0,
        cae_campo_nhccpctrl2 = FALSE,
        hogares_desajuste_personas = NA_character_,
        motivo_detallado_campo_nhccpctrl2 = NA_character_,
        criterio_falla_campo_nhccpctrl2 = NA_character_
      )
    },
    .construir_revision_campo_tres_criterios = function(...) {
      tibble::tibble(
        DIRECTORIO = character(),
        SECUENCIA_P = character(),
        ORDEN = character(),
        cae_existencia = logical(),
        cae_lina = logical(),
        cae_campo = logical(),
        n_criterios_caida = integer(),
        criterios_caida = character(),
        criterio_revision_principal = character(),
        grupo_caida = character(),
        variable_caida = character(),
        valor_detectado = character(),
        observacion_resumen = character()
      )
    },
    diagnostico_duplicados_personas_e = function(dfs, cap_persona = "E", criterio_duplicados = c("dane_sin_placeholder", "dane_completa", "actual")) {
      criterio_capturado <<- match.arg(criterio_duplicados)
      list(
        resumen_duplicados = tibble::tibble(
          criterio_duplicados = criterio_capturado,
          personas_totales_e = 1L,
          personas_con_identificador_completo = 1L,
          grupos_duplicados = 0L,
          registros_en_grupos_duplicados = 0L,
          pct_registros_duplicados = 0
        ),
        personas_duplicadas = tibble::tibble(
          DIRECTORIO = character(),
          SECUENCIA_P = character(),
          ORDEN = character(),
          criterio_duplicados = character(),
          clave_duplicado = character(),
          n_en_grupo_duplicado = integer(),
          nombre_normalizado = character(),
          tipo_documento_normalizado = character(),
          fecha_nac_normalizada = character(),
          numero_documento_normalizado = character(),
          numero_documento_unificado = character(),
          grupo_duplicado = character(),
          n_repetidos = integer(),
          observacion_duplicado = character()
        )
      )
    },
    .construir_reporte_final_caidas_tres_criterios = function(...) {
      tibble::tibble()
    }
  )

  res <- diagnostico_caidas_tres_criterios(
    dfs = list(),
    criterio_duplicados = "actual",
    exportar_excel = FALSE
  )

  expect_equal(criterio_capturado, "actual")
  expect_equal(res$criterio_duplicados, "actual")
})
