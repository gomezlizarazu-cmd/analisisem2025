datos_sinteticos_cierre_npckp37 <- function() {
  tibble::tibble(
    DIRECTORIO = as.character(1:8),
    SECUENCIA_P = "1",
    ORDEN = "1",
    NPCKP17 = c("4", "5", "8", "4", "5", "8", "4", "5"),
    NPCKP36 = c(
      "1000", "2000", "98", "99", "99", "3000", "4000", "5000"
    ),
    NPCKP36A = c(
      "1000", "2000", "98", "99", "99", "98", "4000", "5000"
    ),
    NPCKP36_original = c(
      "1000", NA, NA, NA, NA, "3000", "4000", "5000"
    ),
    NPCKP36A_original = c(
      "1000", NA, "98", "99", "99", "98", "4000", "5000"
    ),
    NPCKP36A_flag_imputado = c(
      FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    NPCKP37 = c("2", "1", NA, NA, NA, NA, NA, "98"),
    NPCKP37_original = c("2", NA, NA, NA, NA, NA, NA, "98"),
    NPCKP37_flag_imputado = c(
      FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    NPCKP37_metodo_imputacion = c(
      NA,
      "un_mes_por_reconstruccion_desde_ingresos_mes_pasado",
      rep(NA, 6)
    ),
    NPCKP36_flag_codigo_residual = c(
      FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE
    ),
    NPCKP36_codigo_residual = c(
      NA, NA, 98L, 99L, 99L, NA, NA, NA
    ),
    NPCKP36_origen_codigo_residual = c(
      NA,
      NA,
      "98_trasladado_desde_npckp23",
      "99_trasladado_desde_npckp23",
      "99_asignado_ausencia_total",
      NA,
      NA,
      NA
    ),
    universo_npckp36_37 = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
    )
  )
}

test_that("cierre NPCKP37 aplica reglas y conserva variables protegidas", {
  datos <- datos_sinteticos_cierre_npckp37()
  datos_antes <- serialize(datos, NULL)
  original_37_antes <- serialize(datos$NPCKP37_original, NULL)
  npckp36_antes <- datos$NPCKP36
  npckp36a_original_antes <- serialize(
    datos$NPCKP36A_original,
    NULL
  )

  out <- cerrar_npckp37(datos)
  base <- out$base_k_imputada_k41_k42_cierre_npckp37

  expect_equal(
    base$NPCKP37,
    c("2", "1", "98", "99", "99", "1", NA, "98")
  )
  expect_identical(base$NPCKP36, npckp36_antes)
  expect_equal(
    base$NPCKP36A,
    c("1000", "2000", "98", "99", "99", "3000", "4000", "5000")
  )
  expect_identical(
    serialize(base$NPCKP36A_original, NULL),
    npckp36a_original_antes
  )
  expect_identical(
    serialize(base$NPCKP37_original, NULL),
    original_37_antes
  )
  expect_identical(serialize(datos, NULL), datos_antes)
  expect_false(base$NPCKP37_flag_imputado[1])
  expect_true(base$NPCKP37_flag_imputado[2])
  expect_true(base$NPCKP37_flag_imputado[6])
  expect_true(base$NPCKP36A_flag_imputado[6])
  expect_equal(
    base$NPCKP36A_metodo_imputacion[6],
    "sincronizacion_con_npckp36_por_meses_imputados_moda"
  )
  expect_equal(
    base$NPCKP37_metodo_imputacion[c(1, 2, 3, 4, 5, 6)],
    c(
      NA,
      "un_mes_por_ingreso_reconstruido",
      "codigo_98_trasladado_desde_npckp36",
      "codigo_99_trasladado_desde_npckp36",
      "codigo_99_ausencia_total",
      "un_mes_por_moda_dominante"
    )
  )
  expect_false(base$NPCKP37_flag_codigo_residual[1])
  expect_true(all(base$NPCKP37_flag_codigo_residual[3:5]))
  expect_equal(
    base$NPCKP37_codigo_residual[3:5],
    c(98L, 99L, 99L)
  )
  expect_equal(
    base$NPCKP37_origen_codigo_residual[3:5],
    c(
      "codigo_98_trasladado_desde_npckp36",
      "codigo_99_trasladado_desde_npckp36",
      "codigo_99_ausencia_total"
    )
  )
  expect_false(any(
    as.numeric(base$NPCKP37[3:5]) %in% 1:12
  ))
  expect_equal(
    out$balance_final_npckp37$personas,
    c(1, 1, 1, 1, 2, 0)
  )
  expect_equal(sum(out$balance_final_npckp37$personas), 6)
  expect_true(all(out$controles_cierre_npckp37$estado == "OK"))
  expect_equal(nrow(out$auditoria_cierre_npckp37), 4)
})

test_that("cierre NPCKP37 sincroniza NPCKP36A con codigo 99 espurio", {
  datos <- datos_sinteticos_cierre_npckp37()[6, ]
  datos$NPCKP36A <- "99"
  datos$NPCKP36A_original <- "99"
  original_36a_antes <- datos$NPCKP36A_original

  base <- cerrar_npckp37(
    datos
  )$base_k_imputada_k41_k42_cierre_npckp37

  expect_equal(base$NPCKP36A, base$NPCKP36)
  expect_equal(base$NPCKP37, "1")
  expect_identical(
    base$NPCKP36A_original,
    original_36a_antes
  )
  expect_true(base$NPCKP36A_flag_imputado)
  expect_equal(
    base$NPCKP36A_metodo_imputacion,
    "sincronizacion_con_npckp36_por_meses_imputados_moda"
  )
})

test_that("cierre NPCKP37 crea una sola vez la copia original", {
  datos <- datos_sinteticos_cierre_npckp37()[c(1, 6), ] |>
    dplyr::select(-NPCKP37_original)
  recibido <- datos$NPCKP37

  primera <- cerrar_npckp37(datos)
  base_primera <- primera$base_k_imputada_k41_k42_cierre_npckp37

  expect_identical(base_primera$NPCKP37_original, recibido)
  expect_equal(base_primera$NPCKP37, c("2", "1"))

  segunda <- cerrar_npckp37(base_primera)
  expect_identical(
    segunda$base_k_imputada_k41_k42_cierre_npckp37,
    base_primera
  )
  expect_identical(
    segunda$base_k_imputada_k41_k42_cierre_npckp37$
      NPCKP37_original,
    base_primera$NPCKP37_original
  )
})

test_that("cierre NPCKP37 no interviene fuera del universo", {
  datos <- datos_sinteticos_cierre_npckp37()
  fuera_antes <- datos$NPCKP37[!datos$universo_npckp36_37]

  base <- cerrar_npckp37(
    datos
  )$base_k_imputada_k41_k42_cierre_npckp37

  expect_identical(
    base$NPCKP37[!base$universo_npckp36_37],
    fuera_antes
  )
  expect_false(any(
    base$NPCKP37_flag_codigo_residual[
      !base$universo_npckp36_37
    ]
  ))
})
