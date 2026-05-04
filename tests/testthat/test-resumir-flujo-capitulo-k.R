test_that("resumir_flujo_capitulo_k devuelve una fila por bloque agregado", {
  dic_min <- tibble::tribble(
    ~orden, ~variable, ~pregunta, ~universo, ~opcion, ~etiqueta, ~destino,
    1, "NPCKP1", "Actividad principal", "edad >= 10", "1", "Trabajando", "K17",
    2, "NPCKP2_1", "Remuneracion", "NPCKP1 == 1", "1", "Si", "K17",
    3, "NPCKP2", "Actividad paga", "NPCKP1 != 1", "1", "Si", "K17",
    4, "NPCKP3", "Tenia trabajo", "NPCKP2 == 2", "1", "Si", "K17",
    7, "NPCKP4", "Ayudo sin pago", "NPCKP3 == 2", "1", "Si", "K17",
    8, "NPCKP5", "Busco trabajo", "NPCKP4 == 2", "1", "Si", "K16",
    10, "NPCKP7", "Desea trabajar", "NPCKP5 == 2", "1", "Si", "K8",
    13, "NPCKP10", "Despues del empleo hizo diligencias", "NPCKP9 == 1", "1", "Si", "K15",
    14, "NPCKP11", "Hizo diligencias ultimos 12 meses", "NPCKP9 == 2", "1", "Si", "K15",
    16, "NPCKP13", "Disponibilidad", "NPCKP12 >= 1", "1", "Si", "K58",
    23, "NPCKP17", "Categoria ocupacional", "llega_K17", "1", "Asalariado", "K24",
    45, "NPCKP38A", "Antiguedad anios", "llega_K45", NA, "Anios", "NPCKP38B",
    57, "NPCKNP48", "Teletrabajo", "llega_K57", "2", "No", "K63",
    63, "NPCKP50_A", "Experiencia laboral", "llega_K63", NA, "Anios", "NPCKP50_B",
    64, "NPCKP50", "Cotiza pension", "edad >= 15", "2", "No", "K66",
    65, "NPCKP51", "Fondo pension", "NPCKP50 == 1", "1", "Colpensiones", "K66",
    66, "NPCKP52", "Pension", "llega_K66", "2", "No", "K67",
    72, "NPCKP58", "Otros conceptos", "llega_K72", "2", "No", "K73",
    73, "NPCKP73_1", "Emprendimiento", "llega_K73", "2", "No", "K76",
    75, "NPCKP75_1", "Registro emprendimiento", "NPCKP73_1 == 1", "2", "No", "K76",
    76, "NPCKPN62A", "Renta", "edad >= 18", "2", "No", "K77",
    77, "NPCKP59A", "Trabajo no remunerado", "edad >= 18", "2", "No", "K78",
    78, "NPCKP78_1", "Acoso laboral", "edad >= 18", "2", "No", "FIN_K"
  )

  base_min <- tibble::tibble(
    edad = c(9, 20, 35, 42),
    NPCKP1 = c(NA, 1, 2, 2),
    NPCKP2_1 = c(NA, 1, NA, NA),
    NPCKP2 = c(NA, NA, 1, 2),
    NPCKP3 = c(NA, NA, NA, 2),
    NPCKP4 = c(NA, NA, NA, 2),
    NPCKP5 = c(NA, NA, NA, 2),
    NPCKP7 = c(NA, NA, NA, 1),
    NPCKP17 = c(NA, 1, 4, NA),
    NPCKP50_A = c(NA, 10, 2, 0),
    NPCKP52 = c(NA, 2, 2, 2),
    NPCKP58 = c(NA, 2, 2, 2),
    NPCKP73_1 = c(NA, 2, 2, 2),
    NPCKPN62A = c(NA, 2, 2, 2),
    NPCKP59A = c(NA, 2, 2, 2),
    NPCKP78_1 = c(NA, 2, 2, NA)
  )

  res <- resumir_flujo_capitulo_k(base_min, dic_min)

  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c(
      "flujo_id",
      "bloque",
      "descripcion",
      "preguntas_asociadas",
      "variables_asociadas",
      "n_personas",
      "pct_sobre_personas_10_mas",
      "observacion"
    )
  )
  expect_equal(nrow(res), 21)
  expect_setequal(
    res$flujo_id,
    c(
      "0.0", "1.0", "1.1", "1.2", "2.0", "2.1", "2.2", "2.2.1",
      "2.2.2", "2.2.3", "3.0", "3.1", "3.2", "3.3", "4.0",
      "5.1", "5.2", "5.3", "5.4", "5.5", "6.0"
    )
  )
  expect_equal(res$n_personas[res$flujo_id == "0.0"], 3L)
  expect_equal(res$n_personas[res$flujo_id == "2.1"], 2L)
  expect_false(grepl("K78:", res$preguntas_asociadas[res$flujo_id == "1.1"], fixed = TRUE))
})

test_that("resumir_flujo_capitulo_k puede tomar edad desde capitulo E", {
  dic_min <- tibble::tribble(
    ~orden, ~variable, ~pregunta, ~universo, ~opcion, ~etiqueta, ~destino,
    1, "NPCKP1", "Actividad principal", "edad >= 10", "1", "Trabajando", "K17",
    23, "NPCKP17", "Categoria ocupacional", "llega_K17", "1", "Asalariado", "K24"
  )

  base_k <- tibble::tibble(
    DIRECTORIO = c("1", "1"),
    SECUENCIA_P = c("1", "1"),
    ORDEN = c("1", "2"),
    NPCKP1 = c(1, 1),
    NPCKP17 = c(1, 1)
  )

  base_e <- tibble::tibble(
    DIRECTORIO = c("1", "1"),
    SECUENCIA_P = c("1", "1"),
    ORDEN = c("1", "2"),
    NPCEP4 = c(9, 22)
  )

  res <- resumir_flujo_capitulo_k(base_k, dic_min, data_edad = base_e)

  expect_equal(res$n_personas[res$flujo_id == "0.0"], 1L)
  expect_true(grepl("Edad tomada de `data_edad`", res$observacion[res$flujo_id == "0.0"], fixed = TRUE))
})
