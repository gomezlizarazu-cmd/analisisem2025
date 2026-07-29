base_distribucion_sintetica <- function(valores,
                                        madre = rep(1L, length(valores)),
                                        grupo = rep(1L, length(valores))) {
  n <- length(valores)
  tibble::tibble(
    DIRECTORIO = sprintf("D%03d", seq_len(n)),
    SECUENCIA_P = 1L,
    ORDEN = 1L,
    PREGUNTA = valores,
    MADRE = madre,
    GRUPO = grupo,
    REGISTRO_VALIDO = TRUE
  )
}

configuracion_pregunta_sintetica <- function(
    universo_antes = function(datos) rep(TRUE, nrow(datos)),
    universo_despues = function(datos) rep(TRUE, nrow(datos))) {
  list(
    PREGUNTA = list(
      variable_antes = "PREGUNTA",
      variable_despues = "PREGUNTA",
      universo_antes = universo_antes,
      universo_despues = universo_despues,
      valores_validos = c("1", "2", "3"),
      excluir_na = TRUE,
      descripcion = "Pregunta sintetica"
    )
  )
}

test_that("cambios inferiores, iguales y superiores a cinco pp se evaluan", {
  antes <- base_distribucion_sintetica(c(rep(1L, 50), rep(2L, 50)))

  despues_4 <- base_distribucion_sintetica(c(rep(1L, 54), rep(2L, 46)))
  resultado_4 <- validar_distribuciones_antes_despues(
    antes,
    despues_4,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  expect_equal(
    resultado_4$resumen_distribuciones$maxima_diferencia_absoluta_pp,
    4
  )
  expect_equal(resultado_4$resumen_distribuciones$estado, "OK")

  despues_5 <- base_distribucion_sintetica(c(rep(1L, 55), rep(2L, 45)))
  resultado_5 <- validar_distribuciones_antes_despues(
    antes,
    despues_5,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  expect_equal(
    resultado_5$resumen_distribuciones$maxima_diferencia_absoluta_pp,
    5
  )
  expect_equal(resultado_5$resumen_distribuciones$estado, "OK")

  despues_6 <- base_distribucion_sintetica(c(rep(1L, 56), rep(2L, 44)))
  resultado_6 <- validar_distribuciones_antes_despues(
    antes,
    despues_6,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  expect_equal(
    resultado_6$resumen_distribuciones$maxima_diferencia_absoluta_pp,
    6
  )
  expect_equal(resultado_6$resumen_distribuciones$estado, "ERROR")
})

test_that("full join conserva categorias exclusivas y de frecuencia cero", {
  antes <- base_distribucion_sintetica(c(rep(1L, 90), rep(3L, 10)))
  despues <- base_distribucion_sintetica(c(rep(1L, 90), rep(2L, 10)))

  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  detalle <- resultado$detalle_distribuciones

  expect_true(detalle$categoria_exclusiva_antes[detalle$categoria == "3"])
  expect_true(detalle$categoria_exclusiva_despues[detalle$categoria == "2"])
  expect_setequal(detalle$categoria, c("1", "2", "3"))
})

test_that("cada variable puede utilizar un universo diferente", {
  antes <- base_distribucion_sintetica(
    c(1L, 1L, 2L, 2L),
    grupo = c(1L, 1L, 2L, 2L)
  )
  despues <- antes
  antes$OTRA <- c(1L, 2L, 1L, 2L)
  despues$OTRA <- antes$OTRA

  configuracion <- list(
    PREGUNTA = list(
      valores_validos = c("1", "2"),
      universo_antes = function(datos) rep(TRUE, nrow(datos)),
      universo_despues = function(datos) rep(TRUE, nrow(datos))
    ),
    OTRA = list(
      universo_antes = function(datos) datos$GRUPO == 1L,
      universo_despues = function(datos) datos$GRUPO == 1L,
      valores_validos = c("1", "2")
    )
  )

  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion,
    imprimir = FALSE
  )
  expect_equal(
    resultado$resumen_distribuciones$total_antes,
    c(4L, 2L)
  )
})

test_that("una variable subordinada usa la respuesta de su pregunta madre", {
  antes <- base_distribucion_sintetica(
    c(1L, 2L, 2L, 1L),
    madre = c(1L, 1L, 2L, 2L)
  )
  despues <- antes

  configuracion <- configuracion_pregunta_sintetica(
    universo_antes = function(datos) datos$MADRE == 1L,
    universo_despues = function(datos) datos$MADRE == 1L
  )
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion,
    imprimir = FALSE
  )

  expect_equal(resultado$resumen_distribuciones$total_antes, 2L)
  expect_equal(resultado$resumen_distribuciones$total_despues, 2L)
  expect_equal(resultado$resumen_distribuciones$estado, "OK")
})

test_that("columnas original y final pueden tener nombres diferentes", {
  antes <- base_distribucion_sintetica(c(1L, 2L, 1L, 2L))
  names(antes)[names(antes) == "PREGUNTA"] <- "PREGUNTA_original"
  despues <- base_distribucion_sintetica(c(1L, 2L, 1L, 2L))
  configuracion <- list(
    PREGUNTA = list(
      variable_antes = "PREGUNTA_original",
      variable_despues = "PREGUNTA",
      valores_validos = c("1", "2")
    )
  )

  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion,
    imprimir = FALSE
  )

  expect_equal(resultado$resumen_distribuciones$estado, "OK")
  expect_equal(nrow(resultado$observados_validos_sobrescritos), 0L)
})

test_that("depuracion limpia solo respuestas explicitamente fuera de flujo", {
  base <- base_distribucion_sintetica(c(1L, 2L, 3L))
  base$PREGUNTA_original <- base$PREGUNTA

  depuracion <- depurar_respuestas_fuera_flujo_osis(
    base,
    list(
      PREGUNTA = list(
        universo = function(datos) c(TRUE, FALSE, NA),
        variable_original = "PREGUNTA_original",
        regla_flujo = "regla_sintetica",
        motivo = "fuera_del_universo_sintetico"
      )
    )
  )

  expect_equal(depuracion$base_depurada$PREGUNTA[[1]], 1L)
  expect_true(is.na(depuracion$base_depurada$PREGUNTA[[2]]))
  expect_equal(depuracion$base_depurada$PREGUNTA[[3]], 3L)
  expect_equal(nrow(depuracion$trazabilidad_depuracion), 2L)
  expect_true(
    "convertido_a_na_por_fuera_de_flujo" %in%
      depuracion$trazabilidad_depuracion$accion
  )
  expect_true(
    "sin_cambio_flujo_indeterminado" %in%
      depuracion$trazabilidad_depuracion$accion
  )
  expect_equal(
    depuracion$resumen_depuracion$flujo_indeterminado_con_respuesta,
    1L
  )
  expect_true(all(depuracion$controles_depuracion$estado == "OK"))
})

test_that("dominios invalidos se identifican y fallan en la base final", {
  antes <- base_distribucion_sintetica(c(1L, 1L, 2L, 2L))
  despues <- base_distribucion_sintetica(c(1L, 1L, 2L, 9L))

  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )

  expect_equal(resultado$categorias_invalidas$categoria, "9")
  expect_equal(resultado$categorias_invalidas$n_despues, 1L)
  expect_equal(resultado$resumen_distribuciones$estado, "ERROR")
  expect_equal(
    resultado$controles_distribuciones$estado[
      grepl("^dominio_", resultado$controles_distribuciones$control)
    ],
    "ERROR"
  )
})

test_that("valores observados validos sobrescritos se auditan", {
  antes <- base_distribucion_sintetica(c(1L, NA_integer_))
  despues <- base_distribucion_sintetica(c(2L, 1L))

  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )

  expect_equal(nrow(resultado$observados_validos_sobrescritos), 1L)
  expect_equal(
    resultado$observados_validos_sobrescritos$valor_original_observado,
    "1"
  )
  expect_equal(
    resultado$observados_validos_sobrescritos$valor_final,
    "2"
  )
  expect_equal(resultado$controles_preservacion$estado, "ERROR")
})

test_that("operadores generales conservan igualdad de controles antiguos", {
  controles_antiguos <- tibble::tibble(
    control = c("antiguo_ok", "antiguo_error"),
    observado = c(1, 2),
    esperado = c(1, 1)
  )
  evaluados <- evaluar_controles_osis(controles_antiguos)

  expect_equal(evaluados$operador, c("igual", "igual"))
  expect_equal(evaluados$estado, c("OK", "ERROR"))

  expect_equal(
    evaluar_control_osis(
      observado = c(5, 5, 5, 5),
      esperado = c(5, 5, 6, 4),
      operador = c("menor_igual", "mayor_igual", "menor", "mayor")
    ),
    rep(TRUE, 4)
  )

  controles_mixtos <- tibble::tibble(
    control = c("calculado", "directo"),
    observado = c(1, NA),
    esperado = c(1, NA),
    estado = c(NA_character_, "OK")
  )
  expect_equal(
    evaluar_controles_osis(controles_mixtos)$operador,
    c("igual", "directo")
  )
})

test_that("controles de distribucion se integran sin duplicar controles", {
  antes <- base_distribucion_sintetica(c(rep(1L, 50), rep(2L, 50)))
  despues <- base_distribucion_sintetica(c(rep(1L, 54), rep(2L, 46)))
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  antiguos <- tibble::tibble(
    control = "control_existente",
    observado = 10,
    esperado = 10
  )
  integrados <- integrar_controles_osis(
    antiguos,
    resultado$controles_distribuciones
  )

  expect_true("control_existente" %in% integrados$control)
  expect_true(
    "distribucion_pregunta_cambio_maximo_pp" %in% integrados$control
  )
  expect_true(all(integrados$estado == "OK"))
})

test_that("Excel anexa las hojas requeridas al libro existente", {
  skip_if_not_installed("openxlsx")

  antes <- base_distribucion_sintetica(c(rep(1L, 50), rep(2L, 50)))
  despues <- base_distribucion_sintetica(c(rep(1L, 54), rep(2L, 46)))
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "00_control_existente")
  openxlsx::writeData(wb, "00_control_existente", data.frame(estado = "OK"))

  base_depuracion <- antes
  base_depuracion$PREGUNTA_original <- base_depuracion$PREGUNTA
  depuracion <- depurar_respuestas_fuera_flujo_osis(
    base_depuracion,
    list(
      PREGUNTA = list(
        universo = function(datos) {
          c(FALSE, rep(TRUE, nrow(datos) - 1L))
        }
      )
    )
  )
  agregar_hojas_validacion_distribuciones_osis(
    wb,
    resultado,
    depuracion = depuracion
  )
  ruta <- tempfile(fileext = ".xlsx")
  on.exit(unlink(ruta), add = TRUE)
  guardado <- guardar_libro_aceptacion_osis(
    wb,
    ruta,
    resultado$controles_distribuciones
  )

  expect_true(file.exists(ruta))
  expect_true("00_control_existente" %in% guardado$hojas)
  expect_true("validacion_distribuciones" %in% guardado$hojas)
  expect_true("resumen_distribuciones" %in% guardado$hojas)
  expect_true("trazabilidad_depuracion" %in% guardado$hojas)
  expect_true("resumen_depuracion" %in% guardado$hojas)
})

test_that("Excel queda guardado antes de detener una aceptacion con ERROR", {
  skip_if_not_installed("openxlsx")

  antes <- base_distribucion_sintetica(c(rep(1L, 50), rep(2L, 50)))
  despues <- base_distribucion_sintetica(c(rep(1L, 56), rep(2L, 44)))
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "00_controles")
  openxlsx::writeData(
    wb,
    "00_controles",
    resultado$controles_distribuciones
  )
  agregar_hojas_validacion_distribuciones_osis(wb, resultado)

  ruta <- tempfile(fileext = ".xlsx")
  on.exit(unlink(ruta), add = TRUE)
  expect_error(
    guardar_libro_aceptacion_osis(
      wb,
      ruta,
      resultado$controles_distribuciones
    ),
    "Excel fue guardado"
  )

  expect_true(file.exists(ruta))
  expect_true(
    all(
      c("validacion_distribuciones", "resumen_distribuciones") %in%
        openxlsx::getSheetNames(ruta)
    )
  )
})

crear_insumos_aceptacion_osis_sinteticos <- function(
    cambio_mayor_5pp = FALSE,
    respuesta_fuera_flujo = FALSE,
    flujo_indeterminado = FALSE) {
  n <- 100L
  donantes <- seq_len(50L)
  receptores <- 51:100

  npckp43_original <- c(rep(1L, 25L), rep(2L, 25L), rep(NA_integer_, 50L))
  npckp43_final <- npckp43_original
  npckp43_final[receptores] <- if (isTRUE(cambio_mayor_5pp)) {
    c(rep(1L, 35L), rep(2L, 15L))
  } else {
    c(rep(1L, 25L), rep(2L, 25L))
  }

  npckp43a_original <- rep(NA_integer_, n)
  npckp43a_original[1:25] <- c(rep(1L, 13L), rep(2L, 12L))
  npckp43a_final <- npckp43a_original
  indices_43a <- which(npckp43_final == 1L & is.na(npckp43a_final))
  npckp43a_final[indices_43a] <- rep(
    c(1L, 2L),
    length.out = length(indices_43a)
  )

  npckp44_original <- c(rep(1L, 25L), rep(2L, 25L), rep(NA_integer_, 50L))
  npckp44_final <- npckp44_original
  npckp44_final[receptores] <- c(rep(1L, 25L), rep(2L, 25L))

  original <- tibble::tibble(
    DIRECTORIO = sprintf("K%03d", seq_len(n)),
    SECUENCIA_P = 1L,
    ORDEN = 1L,
    edad = 30,
    NPCKP2_1 = 1L,
    NPCKP2 = NA_integer_,
    NPCKP3 = NA_integer_,
    NPCKP5_1 = NA_integer_,
    NPCKP6_1 = NA_integer_,
    NPCKP4 = NA_integer_,
    NPCKP17 = 4L,
    NPCKP43_1 = npckp43_original,
    NPCKP43_1A = npckp43a_original,
    NPCKP44_1 = npckp44_original,
    NPCKP44_1A = NA_character_
  )
  final <- original
  final$NPCKP43_1 <- npckp43_final
  final$NPCKP43_1A <- npckp43a_final
  final$NPCKP44_1 <- npckp44_final
  final$NPCKP44_1A <- NA_character_

  for (variable in c(
    "NPCKP43_1",
    "NPCKP43_1A",
    "NPCKP44_1",
    "NPCKP44_1A"
  )) {
    final[[paste0(variable, "_original")]] <- original[[variable]]
  }

  imputada_43 <- is.na(original$NPCKP43_1)
  imputada_43a <- is.na(original$NPCKP43_1A) & final$NPCKP43_1 == 1L
  imputada_44 <- is.na(original$NPCKP44_1)
  final$NPCKP43_1_flag_imputado <- as.integer(imputada_43)
  final$NPCKP43_1_metodo_imputacion <- ifelse(
    imputada_43,
    "donante_sintetico",
    NA_character_
  )
  final$NPCKP43_1A_flag_imputado <- as.integer(imputada_43a)
  final$NPCKP43_1A_metodo_imputacion <- ifelse(
    imputada_43a,
    "donante_sintetico",
    NA_character_
  )
  final$NPCKP44_1_flag_imputado <- as.integer(imputada_44)
  final$NPCKP44_1_metodo_imputacion <- ifelse(
    imputada_44,
    "donante_sintetico",
    NA_character_
  )
  final$NPCKP44_1A_flag_imputado <- 0L
  final$NPCKP44_1A_flag_no_imputable <- 0L
  final$NPCKP44_1A_metodo_imputacion <- NA_character_

  if (isTRUE(respuesta_fuera_flujo)) {
    indice_fuera <- which(final$NPCKP43_1 == 2L)[[1L]]
    final$NPCKP43_1A[[indice_fuera]] <- 1L
  }
  if (isTRUE(flujo_indeterminado)) {
    indice_indeterminado <- n
    for (variable in c(
      "NPCKP2_1",
      "NPCKP2",
      "NPCKP3",
      "NPCKP5_1",
      "NPCKP6_1",
      "NPCKP4"
    )) {
      original[[variable]][[indice_indeterminado]] <- NA_integer_
      final[[variable]][[indice_indeterminado]] <- NA_integer_
    }
    original$NPCKP43_1[[indice_indeterminado]] <- 1L
    final$NPCKP43_1[[indice_indeterminado]] <- 1L
    final$NPCKP43_1_original[[indice_indeterminado]] <- 1L
  }

  universo_independiente <- function(datos) {
    edad <- suppressWarnings(as.numeric(as.character(datos$edad)))
    ruta <- suppressWarnings(
      as.numeric(as.character(datos$NPCKP2_1))
    )
    posicion <- suppressWarnings(
      as.numeric(as.character(datos$NPCKP17))
    )
    dplyr::if_else(
      is.na(edad) | is.na(ruta) | is.na(posicion),
      NA,
      edad >= 10 &
        ruta == 1 &
        posicion %in% c(4, 5, 8)
    )
  }
  configuracion <- list(
    NPCKP43_1 = list(
      variable = "NPCKP43_1",
      variable_original = "NPCKP43_1_original",
      descripcion = "Actividad ejercida en negocio, empresa o finca",
      universo = universo_independiente,
      valores_validos = c("1", "2"),
      tipo = "categorica",
      validar_distribucion = TRUE,
      columna_flag_imputado = "NPCKP43_1_flag_imputado",
      columna_metodo_imputacion = "NPCKP43_1_metodo_imputacion"
    ),
    NPCKP43_1A = list(
      variable = "NPCKP43_1A",
      variable_original = "NPCKP43_1A_original",
      descripcion = "Propiedad del negocio, empresa o finca",
      universo = function(datos) {
        universo_independiente(datos) &
          normalizar_categoria_distribucion_osis(
            datos$NPCKP43_1
          ) == "1"
      },
      valores_validos = c("1", "2"),
      tipo = "categorica",
      validar_distribucion = TRUE,
      variable_madre = "NPCKP43_1",
      condicion_subordinada = function(datos) {
        normalizar_categoria_distribucion_osis(
          datos$NPCKP43_1
        ) == "1"
      },
      columna_flag_imputado = "NPCKP43_1A_flag_imputado",
      columna_metodo_imputacion = "NPCKP43_1A_metodo_imputacion"
    ),
    NPCKP44_1 = list(
      variable = "NPCKP44_1",
      variable_original = "NPCKP44_1_original",
      descripcion = "Razón principal para trabajar independientemente",
      universo = universo_independiente,
      valores_validos = as.character(1:11),
      tipo = "categorica",
      validar_distribucion = TRUE,
      columna_flag_imputado = "NPCKP44_1_flag_imputado",
      columna_metodo_imputacion = "NPCKP44_1_metodo_imputacion"
    ),
    NPCKP44_1A = list(
      variable = "NPCKP44_1A",
      variable_original = "NPCKP44_1A_original",
      descripcion = "Especificación abierta de la razón 11",
      universo = function(datos) {
        universo_independiente(datos) &
          normalizar_categoria_distribucion_osis(
            datos$NPCKP44_1
          ) == "11"
      },
      tipo = "texto_abierto",
      validar_distribucion = FALSE,
      variable_madre = "NPCKP44_1",
      condicion_subordinada = function(datos) {
        normalizar_categoria_distribucion_osis(
          datos$NPCKP44_1
        ) == "11"
      },
      columna_flag_imputado = "NPCKP44_1A_flag_imputado",
      condicion_no_imputable = function(datos) {
        datos$NPCKP44_1_flag_imputado == 1L &
          normalizar_categoria_distribucion_osis(
            datos$NPCKP44_1
          ) == "11"
      },
      columna_flag_no_imputable =
        "NPCKP44_1A_flag_no_imputable",
      columna_metodo_no_imputable =
        "NPCKP44_1A_metodo_imputacion",
      metodo_no_imputable_esperado =
        "texto_abierto_no_imputado"
    )
  )

  list(
    em_original = list(dfs = list(K = original)),
    base_final = final,
    configuracion = configuracion
  )
}

test_that("orquestadora existe y retorna el contrato completo con OK", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  resultado <- prueba_aceptacion_base_osis(
    em_original = insumos$em_original,
    base_final = insumos$base_final,
    insumos_flujo = NULL,
    configuracion_variables = insumos$configuracion,
    detener_si_error = FALSE
  )

  expect_true(is.function(prueba_aceptacion_base_osis))
  expect_true(all(c(
    "base_osis_depurada",
    "controles",
    "resumen_flujo",
    "detalle_inconsistencias",
    "trazabilidad_depuracion",
    "resumen_depuracion",
    "detalle_distribuciones",
    "resumen_distribuciones",
    "categorias_invalidas",
    "observados_validos_sobrescritos",
    "parametros"
  ) %in% names(resultado)))
  expect_equal(resultado$parametros$diagnosticos_ejecutados, 2L)
  expect_true(all(c(
    "filas_base_final",
    "llaves_duplicadas",
    "orden_y_llaves_modificados",
    "columnas_originales_ausentes",
    "variables_originales_ausentes",
    "valores_observados_validos_sobrescritos",
    "copias_originales_modificadas",
    "vacios_criticos_npckp43_1",
    "respuestas_fuera_flujo_npckp43_1",
    "flujos_indeterminados_npckp43_1",
    "dominio_invalido_npckp43_1",
    "vacio_cuando_debe_npckp43_1a",
    "informado_fuera_salto_npckp43_1a",
    "distribucion_npckp43_1_cambio_maximo_pp",
    "textos_abiertos_escritos_automaticamente",
    "flags_texto_no_imputable_inconsistentes",
    "metodo_texto_no_imputable_inconsistente"
  ) %in% resultado$controles$control))
  expect_true(all(resultado$controles$estado == "OK"))
  expect_equal(resultado$parametros$estado_general, "OK")
})

test_that("orquestadora admite nombres distintos antes y despues", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  original <- insumos$em_original$dfs$K
  original$NPCKP44_1_ANTES <- original$NPCKP44_1
  original$NPCKP44_1 <- NULL
  insumos$em_original$dfs$K <- original
  insumos$base_final$NPCKP44_1_ANTES <-
    original$NPCKP44_1_ANTES
  insumos$configuracion$NPCKP44_1$variable_antes <-
    "NPCKP44_1_ANTES"
  universo_independiente <- insumos$configuracion$NPCKP44_1$universo
  insumos$configuracion$NPCKP44_1A$universo_antes <- function(datos) {
    universo_independiente(datos) &
      normalizar_categoria_distribucion_osis(
        datos$NPCKP44_1_ANTES
      ) == "11"
  }

  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    detener_si_error = FALSE
  )

  expect_equal(resultado$parametros$estado_general, "OK")
  expect_equal(
    resultado$parametros$variables_antes[[3L]],
    "NPCKP44_1_ANTES"
  )
  expect_true(
    "NPCKP44_1" %in% resultado$resumen_distribuciones$variable
  )
})

test_that("validadores configurables cubren dominios numericos", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  insumos$em_original$dfs$K$NPCKP44_1[[1L]] <- 9L
  insumos$base_final$NPCKP44_1[[1L]] <- 9L
  insumos$base_final$NPCKP44_1_original[[1L]] <- 9L
  insumos$configuracion$NPCKP44_1$tipo <- "numerica"
  insumos$configuracion$NPCKP44_1$validar_distribucion <- FALSE
  insumos$configuracion$NPCKP44_1$validador_dominio <- function(x) {
    suppressWarnings(as.numeric(as.character(x))) %in% c(1, 2)
  }
  insumos$configuracion$NPCKP44_1$
    validador_observado_preservable <-
    insumos$configuracion$NPCKP44_1$validador_dominio

  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    detener_si_error = FALSE
  )

  expect_equal(
    resultado$controles$observado[
      resultado$controles$control ==
        "dominio_invalido_npckp44_1"
    ],
    1
  )
  expect_equal(
    resultado$controles$observado[
      resultado$controles$control ==
        "valores_observados_validos_sobrescritos"
    ],
    0
  )
})

test_that("orquestadora depura y diagnostica nuevamente la base", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos(
    respuesta_fuera_flujo = TRUE
  )
  resultado <- prueba_aceptacion_base_osis(
    em_original = insumos$em_original,
    base_final = insumos$base_final,
    insumos_flujo = NULL,
    configuracion_variables = insumos$configuracion,
    detener_si_error = FALSE
  )

  antes <- resultado$diagnostico_antes_depuracion$
    auditoria_por_pregunta
  despues <- resultado$diagnostico_despues_depuracion$
    auditoria_por_pregunta
  expect_gt(
    antes$n_respuesta_fuera_flujo[antes$variable == "NPCKP43_1A"],
    0L
  )
  expect_equal(
    despues$n_respuesta_fuera_flujo[
      despues$variable == "NPCKP43_1A"
    ],
    0L
  )
  expect_true(
    "convertido_a_na_por_fuera_de_flujo" %in%
      resultado$trazabilidad_depuracion$accion
  )
})

test_that("texto abierto queda excluido de las distribuciones", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    detener_si_error = FALSE
  )

  expect_false(
    "NPCKP44_1A" %in% resultado$resumen_distribuciones$variable
  )
  expect_true(
    "NPCKP44_1" %in% resultado$resumen_distribuciones$variable
  )
})

test_that("descripcion metodologica correcta de NPCKP44_1 se conserva", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    detener_si_error = FALSE
  )
  descripcion <- resultado$resumen_distribuciones$descripcion_variable[
    resultado$resumen_distribuciones$variable == "NPCKP44_1"
  ]

  expect_equal(
    descripcion,
    "Razón principal para trabajar independientemente"
  )
  descripcion_incorrecta <- paste(
    "Lugar donde realiza",
    "principalmente su trabajo"
  )
  expect_false(any(
    resultado$resumen_distribuciones$descripcion_variable ==
      descripcion_incorrecta
  ))
})

test_that("chi cuadrado calculable y Cramer V quedan en el resumen", {
  antes <- base_distribucion_sintetica(c(rep(1L, 50), rep(2L, 50)))
  despues <- base_distribucion_sintetica(c(rep(1L, 55), rep(2L, 45)))
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )

  expect_true(
    resultado$resumen_distribuciones$prueba_estadistica_calculable
  )
  expect_true(is.finite(resultado$resumen_distribuciones$chi_cuadrado))
  expect_true(is.finite(resultado$resumen_distribuciones$cramers_v))
})

test_that("chi cuadrado no calculable queda documentado", {
  antes <- base_distribucion_sintetica(rep(1L, 20))
  despues <- base_distribucion_sintetica(rep(1L, 20))
  configuracion <- configuracion_pregunta_sintetica()
  configuracion$PREGUNTA$valores_validos <- "1"
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion,
    imprimir = FALSE
  )

  expect_false(
    resultado$resumen_distribuciones$prueba_estadistica_calculable
  )
  expect_match(
    resultado$resumen_distribuciones$advertencia_prueba,
    "al menos dos categorias"
  )
})

test_that("p valor significativo no rechaza si cambio no supera cinco pp", {
  n <- 20000L
  antes <- base_distribucion_sintetica(
    c(rep(1L, n / 2L), rep(2L, n / 2L))
  )
  despues <- base_distribucion_sintetica(
    c(rep(1L, n * 0.55), rep(2L, n * 0.45))
  )
  resultado <- validar_distribuciones_antes_despues(
    antes,
    despues,
    configuracion_pregunta_sintetica(),
    imprimir = FALSE
  )

  expect_lte(resultado$resumen_distribuciones$p_valor, 0.05)
  expect_equal(
    resultado$resumen_distribuciones$
      maxima_diferencia_absoluta_pp,
    5
  )
  expect_equal(resultado$resumen_distribuciones$estado, "OK")
})

test_that("flujos indeterminados se conservan y se auditan sin cambio", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos(
    flujo_indeterminado = TRUE
  )
  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    detener_si_error = FALSE
  )

  expect_true(
    "sin_cambio_flujo_indeterminado" %in%
      resultado$trazabilidad_depuracion$accion
  )
  expect_equal(
    resultado$base_osis_depurada$NPCKP43_1[[100L]],
    1L
  )
})

test_that("controles adicionales se integran al resultado general", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  adicional <- tibble::tibble(
    control = "control_usuario_sintetico",
    observado = 1,
    esperado = 1
  )
  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    controles_adicionales = adicional,
    detener_si_error = FALSE
  )

  expect_true(
    "control_usuario_sintetico" %in% resultado$controles$control
  )
  expect_equal(
    resultado$controles$estado[
      resultado$controles$control == "control_usuario_sintetico"
    ],
    "OK"
  )
})

test_that("Workbook existente conserva sus hojas", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos()
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "hoja_preexistente")
  openxlsx::writeData(wb, "hoja_preexistente", data.frame(x = 1))

  resultado <- prueba_aceptacion_base_osis(
    insumos$em_original,
    insumos$base_final,
    NULL,
    insumos$configuracion,
    libro = wb,
    detener_si_error = FALSE
  )

  expect_true("hoja_preexistente" %in% openxlsx::sheets(resultado$libro))
  expect_true(
    "00_prueba_aceptacion" %in% openxlsx::sheets(resultado$libro)
  )
})

test_that("RDS y Excel se guardan y verifican antes del error", {
  skip_if_not_installed("openxlsx")
  insumos <- crear_insumos_aceptacion_osis_sinteticos(
    cambio_mayor_5pp = TRUE
  )
  ruta_rds <- tempfile(fileext = ".rds")
  ruta_excel <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(ruta_rds, ruta_excel)), add = TRUE)

  expect_error(
    prueba_aceptacion_base_osis(
      insumos$em_original,
      insumos$base_final,
      NULL,
      insumos$configuracion,
      ruta_rds = ruta_rds,
      ruta_excel = ruta_excel,
      detener_si_error = TRUE
    ),
    "guardados y verificados"
  )

  expect_true(file.exists(ruta_rds))
  expect_true(file.exists(ruta_excel))
  objeto <- readRDS(ruta_rds)
  expect_true(all(c(
    "base_osis_depurada",
    "controles",
    "resumen_distribuciones"
  ) %in% names(objeto)))
  hojas <- openxlsx::getSheetNames(ruta_excel)
  expect_true(all(c(
    "00_prueba_aceptacion",
    "01_diagnostico_flujo",
    "02_inconsistencias",
    "03_trazabilidad_depuracion",
    "04_resumen_depuracion",
    "05_validacion_distribuciones",
    "06_resumen_distribuciones",
    "07_categorias_invalidas",
    "08_observados_sobrescritos",
    "09_parametros"
  ) %in% hojas))
})
