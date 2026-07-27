test_that("codificacion residual traslada 98 y 99 y asigna ausencia total", {
  datos <- tibble::tibble(
    DIRECTORIO = as.character(1:5),
    SECUENCIA_P = "1",
    ORDEN = "1",
    NPCKP36 = c(NA, NA, NA, "1000", "2000"),
    NPCKP36A = c(NA, NA, NA, "1000", "2000"),
    NPCKP37 = c(NA, "4", NA, "2", "1"),
    NPCKP36_original = c(NA, NA, NA, "1000", NA),
    NPCKP36A_original = c(NA, NA, NA, "1000", NA),
    NPCKP37_original = c(NA, "4", NA, "2", NA),
    NPCKP23_original = c("98.0", 99L, NA, "500", "2000"),
    NPCKP36_flag_imputado = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    universo_npckp36_37 = TRUE
  )
  datos_antes <- serialize(datos, NULL)
  npckp37_antes <- datos$NPCKP37

  out <- codificar_residuales_npckp36(
    data = datos,
    residual_npckp36 = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    sin_informacion_util = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  base <- out$base_k_imputada_k41_k42_codigos_residuales

  expect_equal(base$NPCKP36, c("98", "99", "99", "1000", "2000"))
  expect_equal(base$NPCKP36A, c("98", "99", "99", "1000", "2000"))
  expect_equal(
    base$NPCKP36_origen_codigo_residual[1:3],
    c(
      "98_trasladado_desde_npckp23",
      "99_trasladado_desde_npckp23",
      "99_asignado_ausencia_total"
    )
  )
  expect_true(all(base$NPCKP36_flag_codigo_residual[1:3]))
  expect_false(any(base$NPCKP36_flag_imputado[1:3]))
  expect_identical(base$NPCKP37, npckp37_antes)
  expect_identical(base$NPCKP36_original, datos$NPCKP36_original)
  expect_identical(base$NPCKP36A_original, datos$NPCKP36A_original)
  expect_identical(base$NPCKP37_original, datos$NPCKP37_original)
  expect_identical(base$NPCKP23_original, datos$NPCKP23_original)
  expect_false(any(
    depurar_monto_capitulo_k(base$NPCKP36[1:3])$monto_valido
  ))
  expect_equal(
    out$resumen_codificacion_residual_npckp36$personas,
    c(3, 1, 1, 1, 0, 2, 3)
  )
  expect_equal(
    out$auditoria_codificacion_residual_npckp36$
      grupo_codigo_residual_npckp36,
    c(
      "98_trasladado_desde_npckp23",
      "99_trasladado_desde_npckp23",
      "99_asignado_ausencia_total"
    )
  )
  expect_true(all(
    nzchar(
      out$auditoria_codificacion_residual_npckp36$
        descripcion_grupo_codigo_residual
    )
  ))
  expect_equal(
    sum(
      out$auditoria_codificacion_residual_npckp36$
        residual_sin_clasificar
    ),
    0
  )
  expect_equal(sum(out$balance_final_npckp36$personas), 5)
  expect_equal(
    out$balance_final_npckp36$personas[
      out$balance_final_npckp36$categoria == "Residual sin clasificar"
    ],
    0
  )
  expect_identical(serialize(datos, NULL), datos_antes)
})

test_that("codificacion residual detiene segunda ejecucion", {
  datos <- tibble::tibble(
    DIRECTORIO = "1",
    SECUENCIA_P = "1",
    ORDEN = "1",
    NPCKP36 = NA_character_,
    NPCKP36A = NA_character_,
    NPCKP37 = NA_character_,
    NPCKP36_original = NA_character_,
    NPCKP36A_original = NA_character_,
    NPCKP37_original = NA_character_,
    NPCKP23_original = "98",
    NPCKP36_flag_imputado = FALSE,
    universo_npckp36_37 = TRUE
  )
  primera <- codificar_residuales_npckp36(
    datos,
    residual_npckp36 = TRUE,
    sin_informacion_util = FALSE
  )$base_k_imputada_k41_k42_codigos_residuales

  expect_error(
    codificar_residuales_npckp36(
      primera,
      residual_npckp36 = TRUE,
      sin_informacion_util = FALSE
    ),
    "ya fue aplicada"
  )
})

test_that("validacion alinea grupos y no depende de orden ni descripcion", {
  auditoria <- tibble::tibble(
    grupo_codigo_residual_npckp36 = rep(
      c(
        "99_asignado_ausencia_total",
        "98_trasladado_desde_npckp23",
        "99_trasladado_desde_npckp23"
      ),
      times = c(760, 316, 1441)
    ),
    descripcion_grupo_codigo_residual = rep(
      c("descripcion C", "descripcion A", "descripcion B"),
      times = c(760, 316, 1441)
    ),
    residual_sin_clasificar = FALSE,
    NPCKP37 = "1"
  ) |>
    dplyr::arrange(
      dplyr::desc(grupo_codigo_residual_npckp36)
    )
  esperados <- tibble::tibble(
    grupo_codigo_residual_npckp36 = c(
      "99_trasladado_desde_npckp23",
      "99_asignado_ausencia_total",
      "98_trasladado_desde_npckp23"
    ),
    conteo_esperado = c(1441L, 760L, 316L)
  )
  auditoria_antes <- serialize(auditoria, NULL)
  npckp37_antes <- auditoria$NPCKP37

  expect_no_warning(
    validacion <- validar_conteos_cierre_npckp36(
      auditoria,
      esperados
    )
  )

  expect_true(validacion$conteos_correctos)
  expect_true(all(
    validacion$validacion_conteos_cierre$estado == "OK"
  ))
  expect_equal(validacion$total_observado, 2517L)
  expect_equal(validacion$residuales_sin_clasificar, 0L)
  expect_identical(serialize(auditoria, NULL), auditoria_antes)
  expect_identical(auditoria$NPCKP37, npckp37_antes)

  auditoria_otra_descripcion <- auditoria |>
    dplyr::mutate(
      descripcion_grupo_codigo_residual =
        paste("Etiqueta visible", dplyr::row_number())
    )
  expect_no_warning(
    validacion_otra_descripcion <-
      validar_conteos_cierre_npckp36(
        auditoria_otra_descripcion,
        esperados
      )
  )
  expect_identical(
    validacion_otra_descripcion$validacion_conteos_cierre,
    validacion$validacion_conteos_cierre
  )
})

test_that("validacion alerta categorias ausentes e inesperadas", {
  esperados <- tibble::tibble(
    grupo_codigo_residual_npckp36 = c(
      "98_trasladado_desde_npckp23",
      "99_trasladado_desde_npckp23",
      "99_asignado_ausencia_total"
    ),
    conteo_esperado = c(316L, 1441L, 760L)
  )
  auditoria_sin_98 <- tibble::tibble(
    grupo_codigo_residual_npckp36 = rep(
      esperados$grupo_codigo_residual_npckp36[-1],
      esperados$conteo_esperado[-1]
    ),
    descripcion_grupo_codigo_residual = "Etiqueta libre",
    residual_sin_clasificar = FALSE
  )

  expect_no_warning(
    validacion_ausente <- validar_conteos_cierre_npckp36(
      auditoria_sin_98,
      esperados
    )
  )
  fila_ausente <- validacion_ausente$validacion_conteos_cierre |>
    dplyr::filter(
      grupo_codigo_residual_npckp36 ==
        "98_trasladado_desde_npckp23"
    )
  expect_equal(fila_ausente$conteo_observado, 0L)
  expect_equal(fila_ausente$diferencia, -316L)
  expect_equal(fila_ausente$estado, "ALERTA")

  auditoria_inesperada <- dplyr::bind_rows(
    auditoria_sin_98,
    tibble::tibble(
      grupo_codigo_residual_npckp36 = "grupo_no_esperado",
      descripcion_grupo_codigo_residual = "Grupo no esperado",
      residual_sin_clasificar = FALSE
    )
  )
  expect_no_warning(
    validacion_inesperada <- validar_conteos_cierre_npckp36(
      auditoria_inesperada,
      esperados
    )
  )
  fila_inesperada <-
    validacion_inesperada$validacion_conteos_cierre |>
    dplyr::filter(
      grupo_codigo_residual_npckp36 == "grupo_no_esperado"
    )
  expect_equal(fila_inesperada$conteo_esperado, 0L)
  expect_equal(fila_inesperada$conteo_observado, 1L)
  expect_equal(fila_inesperada$diferencia, 1L)
  expect_equal(fila_inesperada$estado, "ALERTA")
  expect_false(validacion_inesperada$conteos_correctos)
})

test_that("codificacion residual protege monto, imputacion y universo", {
  datos <- tibble::tibble(
    DIRECTORIO = c("1", "2", "3"),
    SECUENCIA_P = "1",
    ORDEN = "1",
    NPCKP36 = c("1000", "2000", NA),
    NPCKP36A = c("1000", "2000", NA),
    NPCKP37 = c("1", "1", NA),
    NPCKP36_original = c("1000", NA, NA),
    NPCKP36A_original = c("1000", NA, NA),
    NPCKP37_original = c("1", NA, NA),
    NPCKP23_original = c("98", "99", "98"),
    NPCKP36_flag_imputado = c(FALSE, TRUE, FALSE),
    universo_npckp36_37 = c(TRUE, TRUE, FALSE)
  )

  expect_error(
    codificar_residuales_npckp36(
      datos,
      residual_npckp36 = c(TRUE, FALSE, FALSE),
      sin_informacion_util = rep(FALSE, 3)
    ),
    "monto original valido"
  )
  expect_error(
    codificar_residuales_npckp36(
      datos,
      residual_npckp36 = c(FALSE, TRUE, FALSE),
      sin_informacion_util = rep(FALSE, 3)
    ),
    "imputacion monetaria previa"
  )
  expect_error(
    codificar_residuales_npckp36(
      datos,
      residual_npckp36 = c(FALSE, FALSE, TRUE),
      sin_informacion_util = rep(FALSE, 3)
    ),
    "fuera del universo"
  )
})
