catalogo_k_sintetico <- function() {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = "1"
    ),
    E = tibble::tibble(
      DIRECTORIO = "1",
      SECUENCIA_P = "1",
      ORDEN = "1",
      NPCEP4 = 30
    )
  )

  diagnostico_flujo_capitulo_k(dfs)$reglas_flujo
}

test_that("catalogo de reglas K tiene cobertura estructural minima", {
  reglas <- catalogo_k_sintetico()

  testthat::expect_gt(nrow(reglas), 100)
  testthat::expect_equal(anyDuplicated(reglas$variable), 0L)
  testthat::expect_false(any(is.na(reglas$variable) | !nzchar(reglas$variable)))
  testthat::expect_false(any(is.na(reglas$bloque) | !nzchar(reglas$bloque)))
  testthat::expect_false(any(is.na(reglas$regla_r) | !nzchar(reglas$regla_r)))
  testthat::expect_false(any(is.na(reglas$variables_previas_usadas) | !nzchar(reglas$variables_previas_usadas)))

  variables_criticas <- c(
    "NPCKP4", "NPCKP9", "NPCKP12", "NPCKP13",
    "NPCKP14", "NPCKP15", "NPCKP16", "NPCKP17", "NPCKP18",
    "NPCKP19", "NPCKP20", "NPCKP20A",
    "NPCKP36", "NPCKP37", "NPCKP43_1", "NPCKP43_1A",
    "NPCKP44_1", "NPCKP44_1A",
    "NPCKP45A", "NPCKP45B", "NPCKP45L",
    "NPCKPA46", "NPCKP46AB", "NPCKP46AC", "NPCKP46B", "NPCKP46AD",
    "NPCKP47B", "NPCKP47C", "NPCKP48", "NPCKP48A",
    "NPCKP50_A", "NPCKP50_B", "NPCKP50", "NPCKP51",
    "NPCKP60_1", "NPCKP60_2", "NPCKP61_1", "NPCKP61_2",
    "NPCKP73_1", "NPCKPN62A", "NPCKPN62B",
    "NPCKP59A", "NPCKP59AA", "NPCKP78_1", "NPCKP78_2"
  )
  testthat::expect_true(all(variables_criticas %in% reglas$variable))

  reglas_norm <- stats::setNames(reglas$regla_r, reglas$variable)

  testthat::expect_true(grepl("NPCKP3 == 2", reglas_norm[["NPCKP4"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP8 %in% c(2,3,4,5,6,7,8,9,10)", reglas_norm[["NPCKP9"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP10 == 1 | NPCKP11 == 1", reglas_norm[["NPCKP12"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP5 == 1", reglas_norm[["NPCKP13"]], fixed = TRUE))
  testthat::expect_false(grepl("NPCKP12", reglas_norm[["NPCKP13"]], fixed = TRUE))

  for (variable in c("NPCKP14", "NPCKP15", "NPCKP16", "NPCKP17", "NPCKP18")) {
    testthat::expect_true(grepl("NPCKP2_1 == 1", reglas_norm[[variable]], fixed = TRUE))
    testthat::expect_true(grepl("NPCKP5_1 %in% c(5,6,7,8)", reglas_norm[[variable]], fixed = TRUE))
  }

  testthat::expect_true(grepl("NPCKP18 == 1", reglas_norm[["NPCKP19"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP18 == 1 & NPCKP19 == 2", reglas_norm[["NPCKP20"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP20 == 2", reglas_norm[["NPCKP20A"]], fixed = TRUE))

  testthat::expect_true(grepl("MPIO %in% c(11001,25754)", reglas_norm[["NPCKP45A"]], fixed = TRUE))
  testthat::expect_true(grepl("MPIO %in% c(11001,25754)", reglas_norm[["NPCKP45B"]], fixed = TRUE))
  testthat::expect_true(grepl("MPIO != 11001", reglas_norm[["NPCKP45L"]], fixed = TRUE))
  testthat::expect_true(grepl("CLASE %in% c(2,3)", reglas_norm[["NPCKP45L"]], fixed = TRUE))
  testthat::expect_false(grepl("25754", reglas_norm[["NPCKP45L"]], fixed = TRUE))

  testthat::expect_true(grepl("NPCKPA46 == 2", reglas_norm[["NPCKP46AB"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKPA46 == 2", reglas_norm[["NPCKP46AC"]], fixed = TRUE))
  testthat::expect_false(grepl("MPIO", reglas_norm[["NPCKP46AB"]], fixed = TRUE))
  testthat::expect_false(grepl("MPIO", reglas_norm[["NPCKP46AC"]], fixed = TRUE))

  testthat::expect_true(grepl("NPCKP13 %in% c(1,2)", reglas_norm[["NPCKP47C"]], fixed = TRUE))
  testthat::expect_false(grepl("NPCKP47B tiene valor", paste(reglas$regla_r, collapse = " | "), fixed = TRUE))
  testthat::expect_true(grepl("NPCKP47C == 2", reglas_norm[["NPCKP60_1"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP47C == 2", reglas_norm[["NPCKP60_2"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP47C == 2", reglas_norm[["NPCKP61_1"]], fixed = TRUE))
  testthat::expect_true(grepl("NPCKP61_1 == 9", reglas_norm[["NPCKP61_2"]], fixed = TRUE))

  testthat::expect_true(grepl("edad >= 15", reglas_norm[["NPCKP50"]], fixed = TRUE))
  testthat::expect_true(grepl("edad >= 15", reglas_norm[["NPCKP51"]], fixed = TRUE))
  for (variable in c("NPCKPN62A", "NPCKPN62B", "NPCKP78_1", "NPCKP78_2")) {
    testthat::expect_true(grepl("edad >= 18", reglas_norm[[variable]], fixed = TRUE))
  }
})

test_that("reglas visibles de K son expresiones evaluables con registro sintetico", {
  reglas <- catalogo_k_sintetico()
  expresiones <- lapply(reglas$regla_r, function(regla) parse(text = regla)[[1]])
  nombres <- unique(unlist(lapply(expresiones, all.vars), use.names = FALSE))
  nombres <- setdiff(nombres, c("c"))

  registro <- stats::setNames(as.list(rep(0, length(nombres))), nombres)
  registro$edad <- 30
  registro$MPIO <- 11001
  registro$CLASE <- 1

  for (i in seq_along(expresiones)) {
    valor <- eval(expresiones[[i]], envir = registro)
    testthat::expect_equal(length(valor), 1L, info = reglas$variable[[i]])
    testthat::expect_true(is.logical(valor), info = reglas$variable[[i]])
  }
})

test_that("universos compartidos de K producen resultados sustantivos con datos sinteticos", {
  dfs <- list(
    K = tibble::tibble(
      DIRECTORIO = rep("1", 4),
      SECUENCIA_P = rep("1", 4),
      ORDEN = sprintf("%02d", seq_len(4)),
      NPCKP1 = c(1, 1, 5, 1),
      NPCKP7 = c(1, 1, 1, 1),
      NPCKP13 = c(0, 0, 0, 0),
      NPCKP2_1 = c(1, 1, 2, 2),
      NPCKP2 = c(NA, NA, 2, 2),
      NPCKP3 = c(NA, NA, 2, 2),
      NPCKP5_1 = c(NA, NA, 0, 0),
      NPCKP6_1 = c(NA, NA, 0, 0),
      NPCKP4 = c(NA, NA, 2, 2),
      NPCKP17 = c(1, 4, NA, 1),
      NPCKP22 = NA,
      NPCKP36 = NA,
      NPCKP38A = NA,
      NPCKP47C = NA,
      NPCKP50_A = NA
    ),
    E = tibble::tibble(
      DIRECTORIO = rep("1", 4),
      SECUENCIA_P = rep("1", 4),
      ORDEN = sprintf("%02d", seq_len(4)),
      NPCEP4 = rep(30, 4)
    )
  )

  variables <- c("NPCKP17", "NPCKP22", "NPCKP36", "NPCKP38A", "NPCKP47C", "NPCKP50_A")
  diag <- diagnostico_flujo_capitulo_k(dfs, vars_cap_k = variables)

  esperados <- list(
    NPCKP17 = c(TRUE, TRUE, FALSE, FALSE),
    NPCKP22 = c(TRUE, FALSE, FALSE, FALSE),
    NPCKP36 = c(FALSE, TRUE, FALSE, FALSE),
    NPCKP38A = c(TRUE, TRUE, FALSE, FALSE),
    NPCKP47C = c(FALSE, FALSE, TRUE, FALSE),
    NPCKP50_A = c(TRUE, TRUE, TRUE, FALSE)
  )

  for (variable in names(esperados)) {
    out <- diag$diagnostico_persona_variable |>
      dplyr::filter(.data$variable == .env$variable) |>
      dplyr::arrange(.data$ORDEN)

    testthat::expect_equal(nrow(out), length(esperados[[variable]]), info = variable)
    testthat::expect_equal(out$debe_responder, esperados[[variable]], info = variable)
  }
})

test_that("dependencias declaradas en reglas K existen en catalogo o datos auxiliares", {
  reglas <- catalogo_k_sintetico()
  declaradas <- unique(unlist(strsplit(reglas$variables_previas_usadas, "\\s*,\\s*"), use.names = FALSE))
  declaradas <- declaradas[!is.na(declaradas) & nzchar(declaradas)]
  permitidas <- c(reglas$variable, "edad", "NPCKP17", "NPCKP17_FINAL", "MPIO", "CLASE")

  testthat::expect_true(all(declaradas %in% permitidas))
})

test_that("repreguntas principales de K declaran su variable madre", {
  reglas <- catalogo_k_sintetico()
  reglas_norm <- stats::setNames(reglas$regla_r, reglas$variable)

  madres <- c(
    NPCKP43_1A = "NPCKP43_1",
    NPCKP44_1A = "NPCKP44_1",
    NPCKP48A = "NPCKP48",
    NPCKP51 = "NPCKP50",
    NPCKP52A = "NPCKP52",
    NPCKP53A = "NPCKP53",
    NPCKP54A = "NPCKP54",
    NPCKP55A = "NPCKP55",
    NPCKP56A = "NPCKP56",
    NPCKP56B = "NPCKP56",
    NPCKP57A = "NPCKP57",
    NPCKP58A = "NPCKP58",
    NPCKPN62B = "NPCKPN62A",
    NPCKP59AA = "NPCKP59A",
    NPCKP59BA = "NPCKP59B",
    NPCKP59CA = "NPCKP59C",
    NPCKP59DA = "NPCKP59D",
    NPCKP59EA = "NPCKP59E",
    NPCKP59FA = "NPCKP59F",
    NPCKP59GA = "NPCKP59G",
    NPCKP59HA = "NPCKP59H",
    NPCKP59IA = "NPCKP59I",
    NPCKP59JA = "NPCKP59J"
  )

  for (variable in names(madres)) {
    testthat::expect_true(grepl(madres[[variable]], reglas_norm[[variable]], fixed = TRUE), info = variable)
  }
})
