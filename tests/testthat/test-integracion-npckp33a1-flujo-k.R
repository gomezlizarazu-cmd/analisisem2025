crear_dfs_integracion_npckp33a1 <- function() {
  casos <- tibble::tibble(
    ORDEN = sprintf("%02d", seq_len(10)),
    NPCKP17 = c(1, 1, 1, 1, 1, 1, 1, 4, 1, 1),
    NPCKP33 = c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1),
    NPCKP33A = c(
      "500000", "500000", "98", "99", "98",
      NA, "500000", "500000", "sin monto", "0"
    ),
    NPCKP33A1 = c(1, NA, NA, NA, 1, NA, NA, NA, NA, NA),
    debe_esperado = c(
      TRUE, TRUE, FALSE, FALSE, FALSE,
      NA, FALSE, FALSE, NA, FALSE
    ),
    vacio_esperado = c(
      FALSE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    fuera_esperado = c(
      FALSE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    estado_esperado = c(
      "Respondio cuando debia responder",
      "Vacio critico: debia responder",
      "Salto valido / no debia responder",
      "Salto valido / no debia responder",
      "Respuesta fuera de flujo",
      "Flujo indeterminado",
      "Salto valido / no debia responder",
      "Salto valido / no debia responder",
      "Flujo indeterminado",
      "Salto valido / no debia responder"
    )
  )

  K <- tibble::tibble(
    DIRECTORIO = rep("I", nrow(casos)),
    SECUENCIA_P = rep("1", nrow(casos)),
    ORDEN = casos$ORDEN,
    edad = 30,
    NPCKP2_1 = 1,
    NPCKP2 = NA_integer_,
    NPCKP3 = NA_integer_,
    NPCKP5_1 = NA_integer_,
    NPCKP6_1 = NA_integer_,
    NPCKP4 = NA_integer_,
    NPCKP8 = c(2, 8, 9, 10, 2, 2, 2, 2, NA, 8),
    NPCKP9 = NA_integer_,
    NPCKP17 = casos$NPCKP17,
    NPCKP33 = casos$NPCKP33,
    NPCKP33A = casos$NPCKP33A,
    NPCKP33A1 = casos$NPCKP33A1,
    NPCKP36 = NA_character_,
    NPCKP37 = NA_character_,
    NPCKP39 = 40,
    NPCKP41 = 40,
    NPCKP42 = NA_integer_,
    NPCKP43_1 = NA_integer_,
    NPCKP43_1A = NA_integer_,
    NPCKP44_1 = NA_integer_,
    NPCKP44_1A = NA_character_
  )

  list(
    dfs = list(K = K),
    casos = casos
  )
}

test_that("diagnostico principal integra NPCKP33A1 con tres estados", {
  insumo <- crear_dfs_integracion_npckp33a1()
  diagnostico <- diagnostico_flujo_capitulo_k(
    insumo$dfs,
    vars_cap_k = "NPCKP33A1"
  )
  detalle <- diagnostico$diagnostico_persona_variable |>
    dplyr::arrange(.data$ORDEN)

  expect_identical(detalle$ORDEN, insumo$casos$ORDEN)
  expect_equal(detalle$debe_responder, insumo$casos$debe_esperado)
  expect_equal(detalle$vacio_critico, insumo$casos$vacio_esperado)
  expect_equal(
    detalle$respuesta_fuera_flujo,
    insumo$casos$fuera_esperado
  )
  expect_equal(detalle$estado_flujo, insumo$casos$estado_esperado)
  expect_equal(
    detalle$candidata_imputacion,
    insumo$casos$vacio_esperado
  )

  resumen <- diagnostico$resumen_variables
  expect_equal(resumen$deben_responder, 2)
  expect_equal(resumen$respondieron_cuando_debian, 1)
  expect_equal(resumen$vacios_criticos, 1)
  expect_equal(resumen$saltos_validos, 5)
  expect_equal(resumen$respuestas_fuera_flujo, 1)
  expect_equal(resumen$flujos_indeterminados, 2)

  auditoria <- diagnostico$auditoria_por_pregunta
  expect_equal(auditoria$n_debe_responder, 2)
  expect_equal(auditoria$n_no_debe_responder, 6)
  expect_equal(auditoria$n_flujo_indeterminado, 2)
  expect_equal(auditoria$n_respondio_debia, 1)
  expect_equal(auditoria$n_vacio_critico, 1)
  expect_equal(auditoria$n_salto_valido, 5)
  expect_equal(auditoria$n_respuesta_fuera_flujo, 1)

  regla <- diagnostico$reglas_flujo
  expect_identical(regla$regla_r, regla$regla_aplicada)
  expect_match(
    regla$regla_r,
    "es_monto_sustantivo\\(NPCKP33A, permitir_cero = FALSE\\)"
  )
  expect_match(regla$variables_previas_usadas, "NPCKP33A")
})

test_that("insumos de reglas se consumen por OSIS sin reconstruccion manual", {
  insumo <- crear_dfs_integracion_npckp33a1()
  diagnostico_directo <- diagnostico_flujo_capitulo_k(
    insumo$dfs,
    vars_cap_k = "NPCKP33A1"
  )
  diagnostico_osis <- analisisem2025:::.diagnosticar_base_osis(
    dfs = insumo$dfs,
    capitulo = "K",
    variables = "NPCKP33A1",
    insumos_flujo = list(
      reglas_flujo = diagnostico_directo$reglas_flujo
    )
  )

  expect_equal(
    diagnostico_osis$diagnostico_persona_variable,
    diagnostico_directo$diagnostico_persona_variable
  )
  expect_equal(
    diagnostico_osis$reglas_flujo$regla_r,
    diagnostico_directo$reglas_flujo$regla_r
  )
  expect_match(
    diagnostico_osis$reglas_flujo$variables_previas_usadas,
    "NPCKP33A"
  )

  flujo_extraido <- analisisem2025:::.extraer_debe_diagnostico_osis(
    diagnostico_osis,
    base = insumo$dfs$K,
    variable = "NPCKP33A1",
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  )
  expect_equal(
    flujo_extraido$debe_responder,
    insumo$casos$debe_esperado
  )
  expect_true(is.na(flujo_extraido$debe_responder[[6L]]))
})

test_that("depuracion OSIS conserva respuestas con flujo indeterminado", {
  insumo <- crear_dfs_integracion_npckp33a1()
  insumo$dfs$K$NPCKP33A1[[6L]] <- 1L
  diagnostico <- diagnostico_flujo_capitulo_k(
    insumo$dfs,
    vars_cap_k = "NPCKP33A1"
  )
  debe <- diagnostico$diagnostico_persona_variable |>
    dplyr::arrange(.data$ORDEN) |>
    dplyr::pull(.data$debe_responder)

  depuracion <- depurar_respuestas_fuera_flujo_osis(
    insumo$dfs$K,
    list(
      NPCKP33A1 = list(
        universo = debe,
        variable_original = "NPCKP33A1",
        regla_flujo = diagnostico$reglas_flujo$regla_r
      )
    )
  )

  expect_true(is.na(debe[[6L]]))
  expect_equal(depuracion$base_depurada$NPCKP33A1[[6L]], 1L)
  expect_true(
    "sin_cambio_flujo_indeterminado" %in%
      depuracion$trazabilidad_depuracion$accion
  )
  expect_true(is.na(depuracion$base_depurada$NPCKP33A1[[5L]]))
})

test_that("orquestadora OSIS acepta los insumos centrales de NPCKP33A1", {
  skip_if_not_installed("openxlsx")
  insumo <- crear_dfs_integracion_npckp33a1()
  indices <- c(1, 3, 4, 5, 6)
  original <- insumo$dfs$K[indices, , drop = FALSE]
  final <- original
  final$NPCKP33A1_original <- original$NPCKP33A1
  diagnostico <- diagnostico_flujo_capitulo_k(
    list(K = original),
    vars_cap_k = "NPCKP33A1"
  )
  configuracion <- list(
    NPCKP33A1 = list(
      variable = "NPCKP33A1",
      variable_original = "NPCKP33A1_original",
      descripcion = "Confirmacion de inclusion del monto",
      universo = function(datos) rep(TRUE, nrow(datos)),
      tipo = "categorica",
      valores_validos = c("1", "2"),
      validar_distribucion = FALSE,
      variable_madre = "NPCKP33A",
      condicion_subordinada = function(datos) rep(TRUE, nrow(datos))
    )
  )

  resultado <- prueba_aceptacion_base_osis(
    em_original = list(dfs = list(K = original)),
    base_final = final,
    insumos_flujo = list(
      reglas_flujo = diagnostico$reglas_flujo
    ),
    configuracion_variables = configuracion,
    ejecutar_prueba_estadistica = FALSE,
    detener_si_error = FALSE
  )

  expect_equal(resultado$parametros$diagnosticos_ejecutados, 2L)
  expect_true(
    "NPCKP33A1" %in% resultado$resumen_flujo$variable
  )
  expect_match(
    resultado$diagnostico_despues_depuracion$
      reglas_flujo$variables_previas_usadas,
    "NPCKP33A"
  )
  expect_equal(
    resultado$diagnostico_despues_depuracion$
      auditoria_por_pregunta$n_respuesta_fuera_flujo,
    0
  )
  expect_true(is.na(
    resultado$base_osis_depurada$NPCKP33A1[
      original$ORDEN == "05"
    ]
  ))
  expect_true(is.na(
    resultado$diagnostico_despues_depuracion$
      diagnostico_persona_variable$debe_responder[
        resultado$diagnostico_despues_depuracion$
          diagnostico_persona_variable$ORDEN == "06"
      ]
  ))
})

test_that("NPCKP33A1 queda aislada de las demas reglas y estructuras", {
  insumo <- crear_dfs_integracion_npckp33a1()
  variables_control <- c(
    "NPCKP33", "NPCKP33A", "NPCKP33A1", "NPCKP9",
    "NPCKP36", "NPCKP37", "NPCKP41", "NPCKP42",
    "NPCKP43_1", "NPCKP43_1A", "NPCKP44_1", "NPCKP44_1A"
  )
  variables_referencia <- setdiff(variables_control, "NPCKP33A1")
  diagnostico_completo <- diagnostico_flujo_capitulo_k(
    insumo$dfs,
    vars_cap_k = variables_control
  )
  diagnostico_referencia <- diagnostico_flujo_capitulo_k(
    insumo$dfs,
    vars_cap_k = variables_referencia
  )

  nombres_objetos <- c(
    "diagnostico_persona_variable",
    "resumen_variables",
    "resumen_personas",
    "resumen_bloques",
    "variables_candidatas_imputacion",
    "auditoria_llaves",
    "reglas_flujo",
    "auditoria_por_pregunta",
    "variables_ausentes",
    "duplicados_k",
    "resumen_flujo_agregado"
  )
  expect_identical(names(diagnostico_completo), nombres_objetos)
  expect_identical(
    names(diagnostico_referencia),
    nombres_objetos
  )
  expect_equal(
    nrow(diagnostico_completo$reglas_flujo),
    nrow(diagnostico_referencia$reglas_flujo) + 1L
  )
  expect_setequal(
    diagnostico_completo$reglas_flujo$variable,
    variables_control
  )
  expect_setequal(
    diagnostico_referencia$reglas_flujo$variable,
    variables_referencia
  )

  comparar_sin_objetivo <- function(nombre_objeto) {
    completo <- diagnostico_completo[[nombre_objeto]] |>
      dplyr::filter(.data$variable != "NPCKP33A1") |>
      dplyr::arrange(.data$variable)
    referencia <- diagnostico_referencia[[nombre_objeto]] |>
      dplyr::arrange(.data$variable)
    expect_equal(completo, referencia, info = nombre_objeto)
  }
  for (nombre in c(
    "diagnostico_persona_variable",
    "resumen_variables",
    "variables_candidatas_imputacion",
    "reglas_flujo",
    "auditoria_por_pregunta",
    "variables_ausentes"
  )) {
    comparar_sin_objetivo(nombre)
  }

  expect_equal(
    diagnostico_completo$auditoria_llaves,
    diagnostico_referencia$auditoria_llaves
  )
  expect_equal(
    diagnostico_completo$duplicados_k,
    diagnostico_referencia$duplicados_k
  )
  expect_identical(
    names(diagnostico_completo$resumen_personas),
    names(diagnostico_referencia$resumen_personas)
  )
  expect_identical(
    names(diagnostico_completo$resumen_bloques),
    names(diagnostico_referencia$resumen_bloques)
  )

  metadatos_completo <- diagnostico_completo$reglas_flujo |>
    dplyr::filter(.data$variable %in% c("NPCKP33", "NPCKP33A")) |>
    dplyr::arrange(.data$variable)
  metadatos_referencia <- diagnostico_referencia$reglas_flujo |>
    dplyr::filter(.data$variable %in% c("NPCKP33", "NPCKP33A")) |>
    dplyr::arrange(.data$variable)
  expect_equal(metadatos_completo, metadatos_referencia)
})
