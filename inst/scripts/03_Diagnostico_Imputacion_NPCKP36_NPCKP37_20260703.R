# ============================================================
# 03_Diagnostico_Imputacion_NPCKP36_NPCKP37_20260703.R
#
# Diagnostico liviano para iniciar la imputacion de:
#   NPCKP36: ganancia neta u honorarios netos
#   NPCKP37: numero de meses al que corresponde el ingreso
#
# Este script:
# - NO vuelve a ejecutar el diagnostico consolidado del Capitulo K;
# - NO modifica la base original;
# - carga un RDS liviano generado por el script 02;
# - identifica universos, vacios criticos y respuestas fuera de flujo;
# - reconstruye ingresos acotado y amplio para diagnostico;
# - cuantifica diferencias y recuperabilidad;
# - conserva trazabilidad completa para la futura imputacion.
# ============================================================

rm(list = ls())
gc()
options(scipen = 999)

suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(openxlsx)
})

# ============================================================
# 0. Configuracion
# ============================================================

fecha_corte <- "20260703"

ruta_paquete <-
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"

devtools::load_all(
  ruta_paquete,
  quiet = TRUE
)

carpeta_raiz <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive",
  "DANE",
  "Multiproposito",
  "Validacion",
  "Encuestas",
  "Validar"
)

carpeta_caps <- file.path(
  carpeta_raiz,
  paste0("CAP_EM_", fecha_corte)
)

carpeta_diagnostico <- file.path(
  carpeta_caps,
  "diagnostico_flujo_capK_diccionario"
)

ruta_insumos <- file.path(
  carpeta_diagnostico,
  paste0(
    "insumos_imputacion_capK_",
    fecha_corte,
    ".rds"
  )
)

carpeta_salida <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "03_diagnostico_npckp36_npckp37"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# NA = deteccion automatica.
# TRUE = NPCKP23A se trata obligatoriamente como reingreso monetario.
# FALSE = NPCKP23A se trata solo como control y el monto base es NPCKP23.
usar_npckp23a_como_monto <- NA

stopifnot(
  file.exists(ruta_insumos)
)

cat(
  "\nRuta de insumos:\n",
  ruta_insumos,
  "\n\nCarpeta de salida:\n",
  carpeta_salida,
  "\n"
)

# ============================================================
# 1. Cargar insumos ya producidos por el script 02
# ============================================================

insumos <- readRDS(
  ruta_insumos
)

objetos_requeridos <- c(
  "base_k",
  "contexto_personas",
  "reglas_flujo",
  "resumen_variables",
  "diccionario_k",
  "parametros"
)

faltan_objetos <- setdiff(
  objetos_requeridos,
  names(insumos)
)

if (length(faltan_objetos) > 0) {
  stop(
    "Faltan objetos en el RDS de insumos: ",
    paste(faltan_objetos, collapse = ", ")
  )
}

objetos_diagnostico_consolidado <- c(
  "reglas_flujo",
  "resumen_variables",
  "diccionario_k",
  "parametros"
)

diagnostico_consolidado_original <- serialize(
  insumos[objetos_diagnostico_consolidado],
  connection = NULL
)

base_k <- tibble::as_tibble(
  insumos$base_k
)

contexto_personas <- tibble::as_tibble(
  insumos$contexto_personas
)

llaves_persona <- c(
  "DIRECTORIO",
  "SECUENCIA_P",
  "ORDEN"
)

faltan_llaves <- setdiff(
  llaves_persona,
  names(base_k)
)

if (length(faltan_llaves) > 0) {
  stop(
    "Faltan llaves en base_k: ",
    paste(faltan_llaves, collapse = ", ")
  )
}

if (!"edad" %in% names(base_k)) {
  base_k <-
    base_k |>
    dplyr::left_join(
      contexto_personas |>
        dplyr::select(
          dplyr::all_of(llaves_persona),
          dplyr::any_of("edad")
        ) |>
        dplyr::distinct(
          dplyr::across(
            dplyr::all_of(llaves_persona)
          ),
          .keep_all = TRUE
        ),
      by = llaves_persona
    )
}

if (!"edad" %in% names(base_k)) {
  stop(
    "No fue posible incorporar la edad a la base K."
  )
}

# ============================================================
# 2. Variables requeridas
# ============================================================

variables_flujo <- c(
  "edad",
  "NPCKP2_1",
  "NPCKP2",
  "NPCKP3",
  "NPCKP5_1",
  "NPCKP6_1",
  "NPCKP4",
  "NPCKP17"
)

variables_objetivo <- c(
  "NPCKP36",
  "NPCKP36A",
  "NPCKP37"
)

reglas_flujo_tbl <- tibble::as_tibble(
  insumos$reglas_flujo
)

columnas_reglas_requeridas <- c(
  "variable",
  "bloque"
)

faltan_columnas_reglas <- setdiff(
  columnas_reglas_requeridas,
  names(reglas_flujo_tbl)
)

if (length(faltan_columnas_reglas) > 0) {
  stop(
    "reglas_flujo no permite validar exclusividad salarial. Faltan: ",
    paste(faltan_columnas_reglas, collapse = ", ")
  )
}

variables_npckp35_presentes <- grep(
  "^NPCKP35",
  names(base_k),
  value = TRUE
)

variables_exclusivas_reglas_asalariados <-
  reglas_flujo_tbl |>
  dplyr::filter(
    .data$bloque == "05_rama_asalariados"
  ) |>
  dplyr::pull(
    .data$variable
  ) |>
  unique()

variables_npckp35_exclusivas_asalariados <- intersect(
  variables_npckp35_presentes,
  variables_exclusivas_reglas_asalariados
)

variables_npckp35_sin_exclusividad_confirmada <- setdiff(
  variables_npckp35_presentes,
  variables_npckp35_exclusivas_asalariados
)

variables_bloque_asalariados <- c(
  "NPCKP23",
  "NPCKP23A",
  "NPCKP24",
  "NPCKP24A",
  "NPCKP24B",
  "NPCKP25",
  "NPCKP25A",
  "NPCKP26",
  "NPCKP26A",
  "NPCKP27",
  "NPCKP27A",
  "NPCKP28",
  "NPCKP28A",
  "NPCKP29",
  "NPCKP29A",
  "NPCKP29B",
  "NPCKP30",
  "NPCKP30A",
  "NPCKP30B",
  "NPCKP31",
  "NPCKP31A",
  "NPCKP31B",
  "NPCKP32",
  "NPCKP32A",
  "NPCKP32B",
  "NPCKP33",
  "NPCKP33A",
  "NPCKP33A1",
  "NPCKNP33A",
  "NPCKP33AA",
  "NPCKP33AB",
  "NPCKP34A",
  "NPCKP34AA",
  "NPCKP34B",
  "NPCKP34BA",
  "NPCKP34C",
  "NPCKP34CA",
  "NPCKP34D",
  "NPCKP34DA",
  "NPCKP34E",
  "NPCKP34EA",
  variables_npckp35_exclusivas_asalariados
)

variables_requeridas <- unique(
  c(
    llaves_persona,
    variables_flujo,
    variables_objetivo,
    variables_bloque_asalariados
  )
)

faltan_variables <- setdiff(
  variables_requeridas,
  names(base_k)
)

if (length(faltan_variables) > 0) {
  stop(
    "Faltan variables requeridas en base_k: ",
    paste(faltan_variables, collapse = ", ")
  )
}

# Trabajar solo con las columnas necesarias para este diagnostico.
base <- base_k |>
  dplyr::select(
    dplyr::all_of(variables_requeridas)
  )

valores_observados_originales <- base |>
  dplyr::select(
    dplyr::all_of(llaves_persona),
    .data$NPCKP36,
    .data$NPCKP37
  )

duplicados_llaves <- base |>
  dplyr::count(
    dplyr::across(
      dplyr::all_of(llaves_persona)
    ),
    name = "n"
  ) |>
  dplyr::filter(
    .data$n > 1L
  )

if (nrow(duplicados_llaves) > 0) {
  stop(
    "Hay llaves de persona duplicadas en base_k: ",
    nrow(duplicados_llaves),
    " combinaciones DIRECTORIO + SECUENCIA_P + ORDEN."
  )
}

# ============================================================
# 3. Funciones auxiliares
# ============================================================

normalizar_texto <- function(x) {
  y <- stringr::str_squish(
    as.character(x)
  )

  y[
    is.na(y) |
      y == "" |
      stringr::str_to_upper(y) %in% c(
        "NA",
        "N/A",
        "NULL",
        "NULO"
      )
  ] <- NA_character_

  y
}

es_vacio <- function(x) {
  is.na(
    normalizar_texto(x)
  )
}

a_codigo <- function(x) {
  y <- normalizar_texto(x)

  suppressWarnings(
    as.integer(
      readr::parse_number(
        y,
        na = c("", "NA", "N/A", "NULL", "NULO")
      )
    )
  )
}

a_monto <- function(x) {
  analisisem2025:::depurar_monto_capitulo_k(x)$monto
}

resumen_cuantiles <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0) {
    return(
      tibble::tibble(
        n = 0L,
        minimo = NA_real_,
        p01 = NA_real_,
        p05 = NA_real_,
        p25 = NA_real_,
        mediana = NA_real_,
        promedio = NA_real_,
        p75 = NA_real_,
        p95 = NA_real_,
        p99 = NA_real_,
        maximo = NA_real_
      )
    )
  }

  qs <- stats::quantile(
    x,
    probs = c(
      0,
      0.01,
      0.05,
      0.25,
      0.50,
      0.75,
      0.95,
      0.99,
      1
    ),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  tibble::tibble(
    n = length(x),
    minimo = qs[1],
    p01 = qs[2],
    p05 = qs[3],
    p25 = qs[4],
    mediana = qs[5],
    promedio = mean(x, na.rm = TRUE),
    p75 = qs[6],
    p95 = qs[7],
    p99 = qs[8],
    maximo = qs[9]
  )
}

# Componente que se agrega solo cuando:
# - la persona respondio que si recibio el concepto;
# - indico que NO estaba incluido en NPCKP23;
# - existe un monto.
componente_no_incluido <- function(indicador, monto, incluido) {
  analisisem2025:::.componente_monto_no_incluido_k(
    indicador = indicador,
    monto = monto,
    incluido = incluido
  )
}

resuelto_no_incluido <- function(indicador, monto, incluido) {
  indicador == 2L |
    (
      indicador == 1L &
        incluido == 1L
    ) |
    (
      indicador == 1L &
        incluido == 2L &
        !is.na(monto)
    )
}

# Componente que se agrega cuando fue recibido.
# Si existe monto pero falta el indicador, el monto se conserva como
# evidencia observada y el caso queda marcado para auditoria.
componente_recibido <- function(indicador, monto) {
  analisisem2025:::.componente_monto_recibido_k(
    indicador = indicador,
    monto = monto
  )
}

resuelto_recibido <- function(indicador, monto) {
  indicador == 2L |
    (
      indicador == 1L &
        !is.na(monto)
    ) |
    (
      is.na(indicador) &
        !is.na(monto)
    )
}

# ============================================================
# 4. Normalizar flujo, respuestas y montos
# ============================================================

base <- base |>
  dplyr::mutate(
    edad_num = suppressWarnings(
      as.numeric(
        as.character(.data$edad)
      )
    ),
    cod_npckp2_1 = a_codigo(.data$NPCKP2_1),
    cod_npckp2 = a_codigo(.data$NPCKP2),
    cod_npckp3 = a_codigo(.data$NPCKP3),
    cod_npckp5_1 = a_codigo(.data$NPCKP5_1),
    cod_npckp6_1 = a_codigo(.data$NPCKP6_1),
    cod_npckp4 = a_codigo(.data$NPCKP4),
    cod_npckp17 = a_codigo(.data$NPCKP17)
  )

base <- base |>
  dplyr::mutate(
    universo_posicion_independiente =
      .data$cod_npckp17 %in% c(4L, 5L, 8L),

    universo_npckp36_37 =
      dplyr::coalesce(
        .data$edad_num >= 10 &
          (
            .data$cod_npckp2_1 == 1L |
              .data$cod_npckp2 == 1L |
              (
                .data$cod_npckp3 == 1L &
                  .data$cod_npckp5_1 %in% c(1L, 2L, 3L, 4L)
              ) |
              (
                .data$cod_npckp3 == 1L &
                  .data$cod_npckp5_1 %in% c(5L, 6L, 7L, 8L) &
                  .data$cod_npckp6_1 == 1L
              ) |
              (
                (
                  .data$cod_npckp3 == 2L |
                    .data$cod_npckp6_1 %in% c(2L, 3L)
                ) &
                  .data$cod_npckp4 == 1L
              )
          ) &
          .data$cod_npckp17 %in% c(4L, 5L, 8L),
        FALSE
      ),

    responde_npckp36 = !es_vacio(.data$NPCKP36),
    responde_npckp36a = !es_vacio(.data$NPCKP36A),
    responde_npckp37 = !es_vacio(.data$NPCKP37),

    vacio_critico_npckp36 =
      .data$universo_npckp36_37 &
      !.data$responde_npckp36,

    vacio_critico_npckp37 =
      .data$universo_npckp36_37 &
      !.data$responde_npckp37,

    fuera_flujo_npckp36 =
      !.data$universo_npckp36_37 &
      .data$responde_npckp36,

    fuera_flujo_npckp37 =
      !.data$universo_npckp36_37 &
      .data$responde_npckp37
  )

universo_consolidado_npckp36_37 <- base |>
  dplyr::select(
    dplyr::all_of(llaves_persona),
    .data$universo_npckp36_37
  )

# Codigos de indicadores.
variables_indicadores <- c(
  "NPCKP24",
  "NPCKP24B",
  "NPCKP25",
  "NPCKP26",
  "NPCKP27",
  "NPCKP28",
  "NPCKP29",
  "NPCKP29B",
  "NPCKP30",
  "NPCKP30B",
  "NPCKP31",
  "NPCKP31B",
  "NPCKP32",
  "NPCKP32B",
  "NPCKP33",
  "NPCKP33A1",
  "NPCKNP33A",
  "NPCKP33AB",
  "NPCKP34A",
  "NPCKP34B",
  "NPCKP34C",
  "NPCKP34D",
  "NPCKP34E"
)

for (v in variables_indicadores) {
  base[[paste0("cod_", tolower(v))]] <- a_codigo(
    base[[v]]
  )
}

variables_montos <- c(
  "NPCKP23",
  "NPCKP23A",
  "NPCKP24A",
  "NPCKP25A",
  "NPCKP26A",
  "NPCKP27A",
  "NPCKP28A",
  "NPCKP29A",
  "NPCKP30A",
  "NPCKP31A",
  "NPCKP32A",
  "NPCKP33A",
  "NPCKP33AA",
  "NPCKP34AA",
  "NPCKP34BA",
  "NPCKP34CA",
  "NPCKP34DA",
  "NPCKP34EA",
  "NPCKP36",
  "NPCKP36A"
)

for (v in variables_montos) {
  depurado_v <- analisisem2025:::depurar_monto_capitulo_k(
    base[[v]]
  )
  sufijo_v <- tolower(v)

  base[[paste0("monto_", sufijo_v)]] <- depurado_v$monto
  base[[paste0("codigo_98_", sufijo_v)]] <- depurado_v$codigo_98
  base[[paste0("codigo_99_", sufijo_v)]] <- depurado_v$codigo_99
  base[[paste0("codigo_especial_", sufijo_v)]] <-
    depurado_v$codigo_especial
  base[[paste0("no_convertible_", sufijo_v)]] <-
    depurado_v$no_convertible
  base[[paste0("monto_valido_", sufijo_v)]] <-
    depurado_v$monto_valido
}

auditoria_codigos_especiales_montos <-
  analisisem2025:::auditar_montos_capitulo_k(
    data = base,
    variables = variables_montos
  )

variables_fuentes_reconstruccion <- setdiff(
  variables_montos,
  c("NPCKP36", "NPCKP36A")
)

columnas_montos_validos <- paste0(
  "monto_valido_",
  tolower(variables_fuentes_reconstruccion)
)

columnas_codigos_especiales <- paste0(
  "codigo_especial_",
  tolower(variables_fuentes_reconstruccion)
)

base$n_fuentes_monetarias_validas <- rowSums(
  as.data.frame(base[columnas_montos_validos]),
  na.rm = TRUE
)

base$n_fuentes_codigos_especiales <- rowSums(
  as.data.frame(base[columnas_codigos_especiales]),
  na.rm = TRUE
)

if (any(
  unlist(
    base[paste0("codigo_especial_", tolower(variables_montos))],
    use.names = FALSE
  ) &
    unlist(
      base[paste0("monto_valido_", tolower(variables_montos))],
      use.names = FALSE
    ),
  na.rm = TRUE
)) {
  stop(
    "Al menos un codigo 98 o 99 permanece clasificado como monto valido."
  )
}

base <- base |>
  dplyr::mutate(
    vacio_critico_npckp36_consolidado =
      .data$vacio_critico_npckp36,
    respuesta_valida_npckp36 =
      .data$monto_valido_npckp36,
    vacio_critico_npckp36 =
      .data$universo_npckp36_37 &
      !.data$respuesta_valida_npckp36
  )

# ============================================================
# 5. Diagnosticar NPCKP23A antes de usarlo
# ============================================================

m23 <- base$monto_npckp23
m23a <- base$monto_npckp23a

ambos_23 <- !is.na(m23) & !is.na(m23a)

n_23a_valido <- sum(
  !is.na(m23a)
)

prop_23a_mayor_100 <- dplyr::if_else(
  n_23a_valido > 0,
  mean(
    m23a[!is.na(m23a)] > 100,
    na.rm = TRUE
  ),
  NA_real_
)

prop_23a_igual_23 <- dplyr::if_else(
  sum(ambos_23) > 0,
  mean(
    m23[ambos_23] == m23a[ambos_23],
    na.rm = TRUE
  ),
  NA_real_
)

npckp23a_parece_monto_auto <-
  n_23a_valido >= 10 &
  (
    dplyr::coalesce(
      prop_23a_mayor_100 >= 0.50,
      FALSE
    ) |
      dplyr::coalesce(
        prop_23a_igual_23 >= 0.50,
        FALSE
      )
  )

usar_23a <- if (is.na(usar_npckp23a_como_monto)) {
  npckp23a_parece_monto_auto
} else {
  isTRUE(
    usar_npckp23a_como_monto
  )
}

control_npckp23a <- tibble::tibble(
  criterio = c(
    "n_npckp23_no_vacio",
    "n_npckp23a_no_vacio",
    "n_ambos_no_vacios",
    "prop_npckp23a_mayor_100",
    "prop_igualdad_npckp23_npckp23a",
    "deteccion_automatica_npckp23a_es_monto",
    "decision_final_usar_npckp23a_como_monto"
  ),
  valor = c(
    sum(!is.na(m23)),
    n_23a_valido,
    sum(ambos_23),
    prop_23a_mayor_100,
    prop_23a_igual_23,
    npckp23a_parece_monto_auto,
    usar_23a
  )
)

base <- base |>
  dplyr::mutate(
    monto_npckp23_base =
      dplyr::case_when(
        usar_23a & !is.na(.data$monto_npckp23a) ~
          .data$monto_npckp23a,
        !is.na(.data$monto_npckp23) ~
          .data$monto_npckp23,
        TRUE ~
          NA_real_
      ),

    alerta_discrepancia_npckp23 =
      usar_23a &
      !is.na(.data$monto_npckp23) &
      !is.na(.data$monto_npckp23a) &
      .data$monto_npckp23 != .data$monto_npckp23a
  )

# ============================================================
# 6. Construir componentes del ingreso
# ============================================================

base <- base |>
  dplyr::mutate(
    # Version acotada: monto base + horas extras no incluidas.
    comp_horas_extra =
      componente_no_incluido(
        .data$cod_npckp24,
        .data$monto_npckp24a,
        .data$cod_npckp24b
      ),

    resuelto_horas_extra =
      resuelto_no_incluido(
        .data$cod_npckp24,
        .data$monto_npckp24a,
        .data$cod_npckp24b
      ),

    # Pagos mensuales en especie o beneficios no incluidos en NPCKP23.
    comp_alimentos =
      componente_recibido(
        .data$cod_npckp25,
        .data$monto_npckp25a
      ),

    comp_vivienda =
      componente_recibido(
        .data$cod_npckp26,
        .data$monto_npckp26a
      ),

    comp_otros_especie =
      componente_recibido(
        .data$cod_npckp27,
        .data$monto_npckp27a
      ),

    comp_transporte_empresa =
      componente_recibido(
        .data$cod_npckp28,
        .data$monto_npckp28a
      ),

    resuelto_alimentos =
      resuelto_recibido(
        .data$cod_npckp25,
        .data$monto_npckp25a
      ),

    resuelto_vivienda =
      resuelto_recibido(
        .data$cod_npckp26,
        .data$monto_npckp26a
      ),

    resuelto_otros_especie =
      resuelto_recibido(
        .data$cod_npckp27,
        .data$monto_npckp27a
      ),

    resuelto_transporte_empresa =
      resuelto_recibido(
        .data$cod_npckp28,
        .data$monto_npckp28a
      ),

    # Subsidios y pagos mensuales que solo se adicionan si no estaban incluidos.
    comp_subsidio_alimentacion =
      componente_no_incluido(
        .data$cod_npckp29,
        .data$monto_npckp29a,
        .data$cod_npckp29b
      ),

    comp_subsidio_transporte =
      componente_no_incluido(
        .data$cod_npckp30,
        .data$monto_npckp30a,
        .data$cod_npckp30b
      ),

    comp_subsidio_familiar =
      componente_no_incluido(
        .data$cod_npckp31,
        .data$monto_npckp31a,
        .data$cod_npckp31b
      ),

    comp_subsidio_educativo =
      componente_no_incluido(
        .data$cod_npckp32,
        .data$monto_npckp32a,
        .data$cod_npckp32b
      ),

    comp_primas_mensuales =
      componente_no_incluido(
        .data$cod_npckp33,
        .data$monto_npckp33a,
        .data$cod_npckp33a1
      ),

    comp_bonificacion_mensual =
      componente_no_incluido(
        .data$cod_npcknp33a,
        .data$monto_npckp33aa,
        .data$cod_npckp33ab
      ),

    resuelto_subsidio_alimentacion =
      resuelto_no_incluido(
        .data$cod_npckp29,
        .data$monto_npckp29a,
        .data$cod_npckp29b
      ),

    resuelto_subsidio_transporte =
      resuelto_no_incluido(
        .data$cod_npckp30,
        .data$monto_npckp30a,
        .data$cod_npckp30b
      ),

    resuelto_subsidio_familiar =
      resuelto_no_incluido(
        .data$cod_npckp31,
        .data$monto_npckp31a,
        .data$cod_npckp31b
      ),

    resuelto_subsidio_educativo =
      resuelto_no_incluido(
        .data$cod_npckp32,
        .data$monto_npckp32a,
        .data$cod_npckp32b
      ),

    resuelto_primas_mensuales =
      resuelto_no_incluido(
        .data$cod_npckp33,
        .data$monto_npckp33a,
        .data$cod_npckp33a1
      ),

    resuelto_bonificacion_mensual =
      resuelto_no_incluido(
        .data$cod_npcknp33a,
        .data$monto_npckp33aa,
        .data$cod_npckp33ab
      ),

    # Pagos de los ultimos 12 meses:
    # se convierten a equivalente mensual dividiendo entre 12.
    comp_prima_servicios_mensual =
      componente_recibido(
        .data$cod_npckp34a,
        .data$monto_npckp34aa
      ) / 12,

    comp_prima_navidad_mensual =
      componente_recibido(
        .data$cod_npckp34b,
        .data$monto_npckp34ba
      ) / 12,

    comp_prima_vacaciones_mensual =
      componente_recibido(
        .data$cod_npckp34c,
        .data$monto_npckp34ca
      ) / 12,

    comp_viaticos_bonif_anual_mensual =
      componente_recibido(
        .data$cod_npckp34d,
        .data$monto_npckp34da
      ) / 12,

    comp_indemnizacion_mensual =
      componente_recibido(
        .data$cod_npckp34e,
        .data$monto_npckp34ea
      ) / 12,

    resuelto_prima_servicios =
      resuelto_recibido(
        .data$cod_npckp34a,
        .data$monto_npckp34aa
      ),

    resuelto_prima_navidad =
      resuelto_recibido(
        .data$cod_npckp34b,
        .data$monto_npckp34ba
      ),

    resuelto_prima_vacaciones =
      resuelto_recibido(
        .data$cod_npckp34c,
        .data$monto_npckp34ca
      ),

    resuelto_viaticos_bonif_anual =
      resuelto_recibido(
        .data$cod_npckp34d,
        .data$monto_npckp34da
      ),

    resuelto_indemnizacion =
      resuelto_recibido(
        .data$cod_npckp34e,
        .data$monto_npckp34ea
      )
  )

componentes_mensuales_amplios <- c(
  "comp_horas_extra",
  "comp_alimentos",
  "comp_vivienda",
  "comp_otros_especie",
  "comp_transporte_empresa",
  "comp_subsidio_alimentacion",
  "comp_subsidio_transporte",
  "comp_subsidio_familiar",
  "comp_subsidio_educativo",
  "comp_primas_mensuales",
  "comp_bonificacion_mensual"
)

componentes_anuales_prorrateados <- c(
  "comp_prima_servicios_mensual",
  "comp_prima_navidad_mensual",
  "comp_prima_vacaciones_mensual",
  "comp_viaticos_bonif_anual_mensual",
  "comp_indemnizacion_mensual"
)

resueltos_amplios <- c(
  "resuelto_horas_extra",
  "resuelto_alimentos",
  "resuelto_vivienda",
  "resuelto_otros_especie",
  "resuelto_transporte_empresa",
  "resuelto_subsidio_alimentacion",
  "resuelto_subsidio_transporte",
  "resuelto_subsidio_familiar",
  "resuelto_subsidio_educativo",
  "resuelto_primas_mensuales",
  "resuelto_bonificacion_mensual",
  "resuelto_prima_servicios",
  "resuelto_prima_navidad",
  "resuelto_prima_vacaciones",
  "resuelto_viaticos_bonif_anual",
  "resuelto_indemnizacion"
)

base$suma_componentes_mensuales <-
  rowSums(
    as.data.frame(
      base[
        componentes_mensuales_amplios
      ]
    ),
    na.rm = TRUE
  )

base$suma_componentes_anuales_mensualizados <-
  rowSums(
    as.data.frame(
      base[
        componentes_anuales_prorrateados
      ]
    ),
    na.rm = TRUE
  )

base$bloque_amplio_completo <-
  apply(
    as.data.frame(
      base[
        resueltos_amplios
      ]
    ),
    1,
    function(x) {
      all(
        x %in% TRUE
      )
    }
  )

base <- base |>
  dplyr::mutate(
    ingreso_acotado =
      dplyr::if_else(
        !is.na(.data$monto_npckp23_base),
        .data$monto_npckp23_base +
          .data$comp_horas_extra,
        NA_real_
      ),

    ingreso_acotado_disponible =
      !is.na(.data$ingreso_acotado),

    ingreso_acotado_completo =
      !is.na(.data$monto_npckp23_base) &
      .data$resuelto_horas_extra,

    ingreso_amplio_mensual =
      dplyr::if_else(
        !is.na(.data$monto_npckp23_base),
        .data$monto_npckp23_base +
          .data$suma_componentes_mensuales,
        NA_real_
      ),

    ingreso_amplio =
      dplyr::if_else(
        !is.na(.data$monto_npckp23_base),
        .data$monto_npckp23_base +
          .data$suma_componentes_mensuales +
          .data$suma_componentes_anuales_mensualizados,
        NA_real_
      ),

    ingreso_amplio_disponible =
      !is.na(.data$ingreso_amplio),

    ingreso_amplio_completo =
      !is.na(.data$monto_npckp23_base) &
      .data$bloque_amplio_completo,

    ingreso_acotado_parcial =
      .data$ingreso_acotado_disponible &
      !.data$ingreso_acotado_completo,

    ingreso_amplio_parcial =
      .data$ingreso_amplio_disponible &
      !.data$ingreso_amplio_completo,

    monto_con_codigo_especial =
      .data$n_fuentes_codigos_especiales > 0L,

    caso_sin_informacion_monetaria =
      .data$n_fuentes_monetarias_validas == 0L &
      .data$n_fuentes_codigos_especiales == 0L,

    caso_ambiguo_monetario =
      .data$monto_con_codigo_especial |
      dplyr::if_any(
        dplyr::all_of(
          paste0(
            "no_convertible_",
            tolower(variables_montos)
          )
        ),
        identity
      ),

    diferencia_amplio_acotado =
      .data$ingreso_amplio -
      .data$ingreso_acotado,

    diferencia_pct_sobre_acotado =
      dplyr::if_else(
        !is.na(.data$ingreso_acotado) &
          .data$ingreso_acotado > 0,
        .data$diferencia_amplio_acotado /
          .data$ingreso_acotado,
        NA_real_
      ),

    razon_amplio_acotado =
      dplyr::if_else(
        !is.na(.data$ingreso_acotado) &
          .data$ingreso_acotado > 0,
        .data$ingreso_amplio /
          .data$ingreso_acotado,
        NA_real_
      ),

    alguna_respuesta_bloque_asalariados =
      dplyr::if_any(
        dplyr::all_of(
          variables_bloque_asalariados
        ),
        ~ !es_vacio(.x)
      ),

    desviada_bloque_asalariados_en_rama_independiente =
      .data$universo_posicion_independiente &
      .data$alguna_respuesta_bloque_asalariados,

    desviada_bloque_asalariados_en_universo_objetivo =
      .data$universo_npckp36_37 &
      .data$alguna_respuesta_bloque_asalariados,

    recuperable_npckp36_acotado =
      .data$vacio_critico_npckp36 &
      .data$ingreso_acotado_disponible &
      .data$n_fuentes_monetarias_validas > 0L,

    recuperable_npckp36_amplio =
      .data$vacio_critico_npckp36 &
      .data$ingreso_amplio_disponible &
      .data$n_fuentes_monetarias_validas > 0L,

    recuperable_npckp37_un_mes =
      .data$vacio_critico_npckp37 &
      (
        .data$recuperable_npckp36_acotado |
          .data$recuperable_npckp36_amplio
      )
  )

if (any(
  base$recuperable_npckp36_acotado &
    base$n_fuentes_monetarias_validas == 0L,
  na.rm = TRUE
) || any(
  base$recuperable_npckp36_amplio &
    base$n_fuentes_monetarias_validas == 0L,
  na.rm = TRUE
)) {
  stop(
    "Un caso fue clasificado como recuperable sin una fuente monetaria valida."
  )
}

if (any(
  base$ingreso_amplio < base$ingreso_acotado,
  na.rm = TRUE
)) {
  stop(
    "El ingreso amplio no puede ser inferior al ingreso acotado."
  )
}

# ============================================================
# 7. Diagnostico de universos
# ============================================================

n_universo_objetivo <- sum(
  base$universo_npckp36_37,
  na.rm = TRUE
)

resumen_universos <- tibble::tibble(
  indicador = c(
    "personas_total_base_k",
    "personas_posicion_independiente_npckp17_4_5_8",
    "personas_universo_teorico_npckp36_37",
    "npckp36_debian_y_respondieron",
    "npckp36_debian_y_no_respondieron",
    "npckp36_no_debian_y_respondieron",
    "npckp37_debian_y_respondieron",
    "npckp37_debian_y_no_respondieron",
    "npckp37_no_debian_y_respondieron",
    "independientes_con_alguna_respuesta_bloque_asalariados",
    "universo_objetivo_con_alguna_respuesta_bloque_asalariados",
    "npckp36_faltante_recuperable_acotado",
    "npckp36_faltante_recuperable_amplio",
    "npckp37_faltante_recuperable_como_un_mes",
    "npckp36_faltante_sin_ingreso_acotado",
    "npckp36_observado_y_con_ingreso_reconstruible"
  ),
  personas = c(
    nrow(base),
    sum(base$universo_posicion_independiente, na.rm = TRUE),
    sum(base$universo_npckp36_37, na.rm = TRUE),
    sum(
      base$universo_npckp36_37 &
        base$responde_npckp36,
      na.rm = TRUE
    ),
    sum(base$vacio_critico_npckp36, na.rm = TRUE),
    sum(base$fuera_flujo_npckp36, na.rm = TRUE),
    sum(
      base$universo_npckp36_37 &
        base$responde_npckp37,
      na.rm = TRUE
    ),
    sum(base$vacio_critico_npckp37, na.rm = TRUE),
    sum(base$fuera_flujo_npckp37, na.rm = TRUE),
    sum(
      base$desviada_bloque_asalariados_en_rama_independiente,
      na.rm = TRUE
    ),
    sum(
      base$desviada_bloque_asalariados_en_universo_objetivo,
      na.rm = TRUE
    ),
    sum(base$recuperable_npckp36_acotado, na.rm = TRUE),
    sum(base$recuperable_npckp36_amplio, na.rm = TRUE),
    sum(base$recuperable_npckp37_un_mes, na.rm = TRUE),
    sum(
      base$vacio_critico_npckp36 &
        is.na(base$ingreso_acotado),
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$responde_npckp36 &
        !is.na(base$ingreso_acotado),
      na.rm = TRUE
    )
  )
) |>
  dplyr::mutate(
    porcentaje_sobre_total =
      .data$personas /
      nrow(base),
    porcentaje_sobre_universo_objetivo =
      if (n_universo_objetivo > 0) {
        .data$personas / n_universo_objetivo
      } else {
        NA_real_
      }
  )

faltantes_objetivo <- tibble::tibble(
  variable = c(
    "NPCKP36",
    "NPCKP37"
  ),
  deben_responder = c(
    sum(base$universo_npckp36_37, na.rm = TRUE),
    sum(base$universo_npckp36_37, na.rm = TRUE)
  ),
  respondieron = c(
    sum(
      base$universo_npckp36_37 &
        base$responde_npckp36,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$responde_npckp37,
      na.rm = TRUE
    )
  ),
  vacios_criticos = c(
    sum(base$vacio_critico_npckp36, na.rm = TRUE),
    sum(base$vacio_critico_npckp37, na.rm = TRUE)
  ),
  respuestas_fuera_flujo = c(
    sum(base$fuera_flujo_npckp36, na.rm = TRUE),
    sum(base$fuera_flujo_npckp37, na.rm = TRUE)
  )
) |>
  dplyr::mutate(
    pct_vacio_critico =
      dplyr::if_else(
        .data$deben_responder > 0,
        .data$vacios_criticos /
          .data$deben_responder,
        NA_real_
      )
  )

# Personas independientes que respondieron cada variable del bloque
# de asalariados. Estas son las respuestas que posteriormente se
# evaluaran para limpieza, despues de recuperar el ingreso.
fuera_flujo_bloque_asalariados <-
  lapply(
    variables_bloque_asalariados,
    function(v) {
      tibble::tibble(
        variable = v,
        personas_con_respuesta =
          sum(
            !es_vacio(base[[v]]),
            na.rm = TRUE
          ),
        independientes_con_respuesta =
          sum(
            base$universo_posicion_independiente &
              !es_vacio(base[[v]]),
            na.rm = TRUE
          ),
        universo_objetivo_con_respuesta =
          sum(
            base$universo_npckp36_37 &
              !es_vacio(base[[v]]),
            na.rm = TRUE
          ),
        universo_objetivo_faltante_npckp36_con_respuesta =
          sum(
            base$vacio_critico_npckp36 &
              !es_vacio(base[[v]]),
            na.rm = TRUE
          )
      )
    }
  ) |>
  dplyr::bind_rows() |>
  dplyr::arrange(
    dplyr::desc(
      .data$universo_objetivo_con_respuesta
    ),
    .data$variable
  )

# ============================================================
# 8. Diferencias entre ingreso acotado y amplio
# ============================================================

base_comparacion <- base |>
  dplyr::filter(
    .data$universo_npckp36_37,
    !is.na(.data$ingreso_acotado),
    !is.na(.data$ingreso_amplio)
  )

resumen_diferencias <- tibble::tibble(
  universo = c(
    "todos_con_ambos_ingresos",
    "faltantes_npckp36_con_ambos_ingresos",
    "casos_completos_en_ambas_construcciones"
  ),
  n = c(
    nrow(base_comparacion),
    sum(
      base_comparacion$vacio_critico_npckp36,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$ingreso_acotado_completo &
        base_comparacion$ingreso_amplio_completo,
      na.rm = TRUE
    )
  ),
  iguales = c(
    sum(
      base_comparacion$diferencia_amplio_acotado == 0,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$vacio_critico_npckp36 &
        base_comparacion$diferencia_amplio_acotado == 0,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$ingreso_acotado_completo &
        base_comparacion$ingreso_amplio_completo &
        base_comparacion$diferencia_amplio_acotado == 0,
      na.rm = TRUE
    )
  ),
  amplio_mayor = c(
    sum(
      base_comparacion$diferencia_amplio_acotado > 0,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$vacio_critico_npckp36 &
        base_comparacion$diferencia_amplio_acotado > 0,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$ingreso_acotado_completo &
        base_comparacion$ingreso_amplio_completo &
        base_comparacion$diferencia_amplio_acotado > 0,
      na.rm = TRUE
    )
  ),
  amplio_menor = c(
    sum(
      base_comparacion$diferencia_amplio_acotado < 0,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$vacio_critico_npckp36 &
        base_comparacion$diferencia_amplio_acotado < 0,
      na.rm = TRUE
    ),
    sum(
      base_comparacion$ingreso_acotado_completo &
        base_comparacion$ingreso_amplio_completo &
        base_comparacion$diferencia_amplio_acotado < 0,
      na.rm = TRUE
    )
  )
) |>
  dplyr::mutate(
    pct_iguales =
      dplyr::if_else(
        .data$n > 0,
        .data$iguales / .data$n,
        NA_real_
      ),
    pct_amplio_mayor =
      dplyr::if_else(
        .data$n > 0,
        .data$amplio_mayor / .data$n,
        NA_real_
      )
  )

cuantiles_ingresos <-
  dplyr::bind_rows(
    resumen_cuantiles(
      base_comparacion$ingreso_acotado
    ) |>
      dplyr::mutate(
        medida = "ingreso_acotado",
        .before = 1
      ),
    resumen_cuantiles(
      base_comparacion$ingreso_amplio_mensual
    ) |>
      dplyr::mutate(
        medida = "ingreso_amplio_mensual",
        .before = 1
      ),
    resumen_cuantiles(
      base_comparacion$ingreso_amplio
    ) |>
      dplyr::mutate(
        medida = "ingreso_amplio_equivalente_mensual",
        .before = 1
      ),
    resumen_cuantiles(
      base_comparacion$diferencia_amplio_acotado
    ) |>
      dplyr::mutate(
        medida = "diferencia_amplio_menos_acotado",
        .before = 1
      ),
    resumen_cuantiles(
      base_comparacion$diferencia_pct_sobre_acotado
    ) |>
      dplyr::mutate(
        medida = "diferencia_pct_sobre_acotado",
        .before = 1
      ),
    resumen_cuantiles(
      base_comparacion$razon_amplio_acotado
    ) |>
      dplyr::mutate(
        medida = "razon_amplio_sobre_acotado",
        .before = 1
      )
  )

resumen_por_posicion <- base |>
  dplyr::filter(
    .data$universo_npckp36_37
  ) |>
  dplyr::group_by(
    .data$cod_npckp17
  ) |>
  dplyr::summarise(
    personas = dplyr::n(),
    vacios_npckp36 =
      sum(
        .data$vacio_critico_npckp36,
        na.rm = TRUE
      ),
    vacios_npckp37 =
      sum(
        .data$vacio_critico_npckp37,
        na.rm = TRUE
      ),
    recuperables_acotado =
      sum(
        .data$recuperable_npckp36_acotado,
        na.rm = TRUE
      ),
    recuperables_amplio =
      sum(
        .data$recuperable_npckp36_amplio,
        na.rm = TRUE
      ),
    ingreso_acotado_mediana =
      stats::median(
        .data$ingreso_acotado,
        na.rm = TRUE
      ),
    ingreso_amplio_mediana =
      stats::median(
        .data$ingreso_amplio,
        na.rm = TRUE
      ),
    diferencia_mediana =
      stats::median(
        .data$diferencia_amplio_acotado,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

# ============================================================
# 9. Aportes y completitud de cada componente
# ============================================================

componentes_todos <- c(
  componentes_mensuales_amplios,
  componentes_anuales_prorrateados
)

resumen_componentes <-
  lapply(
    componentes_todos,
    function(v) {
      x <- base[[v]]
      x_universo <- x[
        base$universo_npckp36_37
      ]

      tibble::tibble(
        componente = v,
        personas_con_aporte_positivo =
          sum(
            x_universo > 0,
            na.rm = TRUE
          ),
        porcentaje_universo =
          mean(
            x_universo > 0,
            na.rm = TRUE
          ),
        suma_aporte =
          sum(
            x_universo,
            na.rm = TRUE
          ),
        mediana_positivos =
          ifelse(
            any(
              x_universo > 0,
              na.rm = TRUE
            ),
            stats::median(
              x_universo[
                x_universo > 0
              ],
              na.rm = TRUE
            ),
            NA_real_
          )
      )
    }
  ) |>
  dplyr::bind_rows() |>
  dplyr::arrange(
    dplyr::desc(
      .data$personas_con_aporte_positivo
    )
  )

resumen_completitud <- tibble::tibble(
  indicador = c(
    "ingreso_acotado_disponible",
    "ingreso_acotado_completo",
    "ingreso_acotado_parcial",
    "ingreso_amplio_disponible",
    "ingreso_amplio_completo",
    "ingreso_amplio_parcial",
    "monto_con_codigo_especial",
    "caso_ambiguo_monetario",
    "caso_sin_informacion_monetaria",
    "alerta_discrepancia_npckp23_npckp23a"
  ),
  personas_universo_objetivo = c(
    sum(
      base$universo_npckp36_37 &
        !is.na(base$ingreso_acotado),
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$ingreso_acotado_completo,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$ingreso_acotado_parcial,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        !is.na(base$ingreso_amplio),
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$ingreso_amplio_completo,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$ingreso_amplio_parcial,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$monto_con_codigo_especial,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$caso_ambiguo_monetario,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$caso_sin_informacion_monetaria,
      na.rm = TRUE
    ),
    sum(
      base$universo_npckp36_37 &
        base$alerta_discrepancia_npckp23,
      na.rm = TRUE
    )
  )
)

# ============================================================
# 10. Validacion frente a NPCKP36 observado
# ============================================================

validacion_con_npckp36_observado <- base |>
  dplyr::filter(
    .data$universo_npckp36_37,
    .data$responde_npckp36,
    !is.na(.data$monto_npckp36),
    !is.na(.data$ingreso_acotado)
  ) |>
  dplyr::mutate(
    diferencia_observado_acotado =
      .data$monto_npckp36 -
      .data$ingreso_acotado,
    diferencia_observado_amplio =
      .data$monto_npckp36 -
      .data$ingreso_amplio,
    razon_observado_acotado =
      dplyr::if_else(
        .data$ingreso_acotado > 0,
        .data$monto_npckp36 /
          .data$ingreso_acotado,
        NA_real_
      ),
    razon_observado_amplio =
      dplyr::if_else(
        .data$ingreso_amplio > 0,
        .data$monto_npckp36 /
          .data$ingreso_amplio,
        NA_real_
      )
  )

cuantiles_validacion_observados <-
  dplyr::bind_rows(
    resumen_cuantiles(
      validacion_con_npckp36_observado$diferencia_observado_acotado
    ) |>
      dplyr::mutate(
        medida = "npckp36_observado_menos_acotado",
        .before = 1
      ),
    resumen_cuantiles(
      validacion_con_npckp36_observado$diferencia_observado_amplio
    ) |>
      dplyr::mutate(
        medida = "npckp36_observado_menos_amplio",
        .before = 1
      ),
    resumen_cuantiles(
      validacion_con_npckp36_observado$razon_observado_acotado
    ) |>
      dplyr::mutate(
        medida = "razon_observado_sobre_acotado",
        .before = 1
      ),
    resumen_cuantiles(
      validacion_con_npckp36_observado$razon_observado_amplio
    ) |>
      dplyr::mutate(
        medida = "razon_observado_sobre_amplio",
        .before = 1
      )
  )

# ============================================================
# 11. Formatos y valores frecuentes
# ============================================================

variables_revision_formato <- unique(
  c(
    variables_objetivo,
    variables_bloque_asalariados
  )
)

resumen_formatos <-
  lapply(
    variables_revision_formato,
    function(v) {
      x <- base[[v]]
      x_txt <- normalizar_texto(x)
      x_num <- a_monto(x)

      tibble::tibble(
        variable = v,
        clase_original = paste(
          class(x),
          collapse = " | "
        ),
        no_vacios =
          sum(
            !is.na(x_txt)
          ),
        numericos_parseables =
          sum(
            !is.na(x_num)
          ),
        pct_parseable =
          dplyr::if_else(
            sum(!is.na(x_txt)) > 0,
            sum(!is.na(x_num)) /
              sum(!is.na(x_txt)),
            NA_real_
          ),
        valores_menores_100 =
          sum(
            !is.na(x_num) &
              x_num >= 0 &
              x_num < 100
          )
      )
    }
  ) |>
  dplyr::bind_rows()

valores_frecuentes <-
  lapply(
    variables_revision_formato,
    function(v) {
      tibble::tibble(
        valor = normalizar_texto(
          base[[v]]
        )
      ) |>
        dplyr::filter(
          !is.na(.data$valor)
        ) |>
        dplyr::count(
          .data$valor,
          name = "personas",
          sort = TRUE
        ) |>
        dplyr::slice_head(
          n = 15
        ) |>
        dplyr::mutate(
          variable = v,
          .before = 1
        )
    }
  ) |>
  dplyr::bind_rows()

diccionario_ingresos <- insumos$diccionario_k |>
  dplyr::filter(
    .data$variable %in%
      variables_revision_formato
  ) |>
  dplyr::arrange(
    match(
      .data$variable,
      variables_revision_formato
    )
  )

# ============================================================
# 12. Base de casos para revisar y futura imputacion
# ============================================================

columnas_trazabilidad <- c(
  llaves_persona,
  "edad_num",
  "cod_npckp17",
  "universo_posicion_independiente",
  "universo_npckp36_37",
  "responde_npckp36",
  "respuesta_valida_npckp36",
  "responde_npckp37",
  "vacio_critico_npckp36_consolidado",
  "vacio_critico_npckp36",
  "vacio_critico_npckp37",
  "fuera_flujo_npckp36",
  "fuera_flujo_npckp37",
  "alguna_respuesta_bloque_asalariados",
  "desviada_bloque_asalariados_en_rama_independiente",
  "desviada_bloque_asalariados_en_universo_objetivo",
  "monto_npckp23",
  "monto_npckp23a",
  "monto_npckp23_base",
  "alerta_discrepancia_npckp23",
  "comp_horas_extra",
  componentes_mensuales_amplios[
    componentes_mensuales_amplios !=
      "comp_horas_extra"
  ],
  componentes_anuales_prorrateados,
  "suma_componentes_mensuales",
  "suma_componentes_anuales_mensualizados",
  "ingreso_acotado",
  "ingreso_acotado_disponible",
  "ingreso_acotado_completo",
  "ingreso_acotado_parcial",
  "ingreso_amplio_mensual",
  "ingreso_amplio",
  "ingreso_amplio_disponible",
  "ingreso_amplio_completo",
  "ingreso_amplio_parcial",
  "n_fuentes_monetarias_validas",
  "n_fuentes_codigos_especiales",
  "monto_con_codigo_especial",
  "caso_ambiguo_monetario",
  "caso_sin_informacion_monetaria",
  "diferencia_amplio_acotado",
  "diferencia_pct_sobre_acotado",
  "razon_amplio_acotado",
  "recuperable_npckp36_acotado",
  "recuperable_npckp36_amplio",
  "recuperable_npckp37_un_mes",
  variables_objetivo,
  variables_bloque_asalariados
)

casos_diagnostico <- base |>
  dplyr::filter(
    .data$universo_npckp36_37 |
      .data$fuera_flujo_npckp36 |
      .data$fuera_flujo_npckp37 |
      .data$desviada_bloque_asalariados_en_rama_independiente
  ) |>
  dplyr::select(
    dplyr::all_of(
      unique(
        columnas_trazabilidad
      )
    )
  )

casos_recuperables_npckp36 <- casos_diagnostico |>
  dplyr::filter(
    .data$vacio_critico_npckp36,
    .data$recuperable_npckp36_acotado |
      .data$recuperable_npckp36_amplio
  )

casos_para_limpieza_posterior <- casos_diagnostico |>
  dplyr::filter(
    .data$desviada_bloque_asalariados_en_rama_independiente
  )

valores_observados_finales <- base |>
  dplyr::select(
    dplyr::all_of(llaves_persona),
    .data$NPCKP36,
    .data$NPCKP37
  )

if (!identical(
  valores_observados_originales,
  valores_observados_finales
)) {
  stop(
    "Se modificaron valores observados de NPCKP36 o NPCKP37."
  )
}

universo_final_npckp36_37 <- base |>
  dplyr::select(
    dplyr::all_of(llaves_persona),
    .data$universo_npckp36_37
  )

if (!identical(
  universo_consolidado_npckp36_37,
  universo_final_npckp36_37
)) {
  stop(
    "El universo NPCKP36/37 difiere de la regla consolidada preservada."
  )
}

if (!identical(
  diagnostico_consolidado_original,
  serialize(
    insumos[objetos_diagnostico_consolidado],
    connection = NULL
  )
)) {
  stop(
    "Se modificaron objetos del diagnostico consolidado."
  )
}

# ============================================================
# 13. Imputacion deterministica de NPCKP36, NPCKP36A y NPCKP37
# ============================================================

variables_bloque_asalariados_limpieza <-
  intersect(
    unique(c(
      analisisem2025:::variables_bloque_asalariados_limpieza_k41_k42(),
      variables_npckp35_exclusivas_asalariados
    )),
    names(base_k)
  )

variables_exclusivas_validadas_k41_k42 <- unique(
  c(
    intersect(
      variables_bloque_asalariados_limpieza,
      variables_exclusivas_reglas_asalariados
    ),
    intersect(
      "NPCKP23A",
      variables_bloque_asalariados_limpieza
    )
  )
)

variables_bloque_asalariados_limpieza <- intersect(
  variables_bloque_asalariados_limpieza,
  variables_exclusivas_validadas_k41_k42
)

npckp35_exclusivas_fuera_limpieza <- setdiff(
  variables_npckp35_exclusivas_asalariados,
  variables_bloque_asalariados_limpieza
)

if (length(npckp35_exclusivas_fuera_limpieza) > 0) {
  stop(
    "Variables NPCKP35* exclusivas de asalariados quedaron fuera ",
    "de la limpieza: ",
    paste(npckp35_exclusivas_fuera_limpieza, collapse = ", ")
  )
}

faltan_variables_limpieza <- setdiff(
  variables_bloque_asalariados_limpieza,
  names(base_k)
)

if (length(faltan_variables_limpieza) > 0) {
  stop(
    "Faltan variables exclusivas del bloque asalariado en base_k: ",
    paste(faltan_variables_limpieza, collapse = ", ")
  )
}

if (
  nrow(base_k) != nrow(base) ||
    !identical(
      base_k[llaves_persona],
      base[llaves_persona]
    )
) {
  stop(
    "base_k y la base diagnostica no conservan las mismas filas y llaves."
  )
}

base_k_preparada_k41_k42 <- base_k

columnas_derivadas_imputacion <- c(
  "universo_npckp36_37",
  "vacio_critico_npckp36",
  "vacio_critico_npckp37",
  "recuperable_npckp36_acotado",
  "ingreso_acotado",
  "ingreso_amplio",
  "alguna_respuesta_bloque_asalariados",
  "desviada_bloque_asalariados_en_universo_objetivo"
)

for (variable in columnas_derivadas_imputacion) {
  base_k_preparada_k41_k42[[variable]] <- base[[variable]]
}

resultado_imputacion_k41_k42 <-
  analisisem2025:::imputar_k41_k42(
    data = base_k_preparada_k41_k42,
    variables_limpieza =
      variables_bloque_asalariados_limpieza,
    variables_exclusivas_validadas =
      variables_exclusivas_validadas_k41_k42
  )

# ============================================================
# 14. Limpieza de la rama de asalariados
# ============================================================

# La funcion del paquete materializa primero los ingresos y las copias
# originales, imputa las variables objetivo y solo despues limpia las
# variables exclusivas. Los controles internos detienen cualquier alteracion
# de los ingresos reconstruidos durante la limpieza.

base_k_imputada_k41_k42 <-
  resultado_imputacion_k41_k42$base_k_imputada_k41_k42

resumen_imputacion_k41_k42 <-
  resultado_imputacion_k41_k42$resumen_imputacion_k41_k42

auditoria_imputacion_k41_k42 <-
  resultado_imputacion_k41_k42$auditoria_imputacion_k41_k42

# ============================================================
# 15. Auditoria de la imputacion y de la limpieza
# ============================================================

parametros_imputacion <- c(
  resultado_imputacion_k41_k42$parametros,
  list(
    fecha_corte = fecha_corte,
    ruta_insumos = ruta_insumos,
    variable_oficial_imputada = "ingreso_acotado",
    variable_sensibilidad =
      "NPCKP36_ingresoamplioimputacionK4142",
    reglas_exclusividad_bloque_asalariado =
      "R/diagnostico_flujo_capitulo_k.R::05_rama_asalariados"
  )
)

if (
  nrow(base_k_imputada_k41_k42) != nrow(base_k) ||
    !identical(
      base_k_imputada_k41_k42[llaves_persona],
      base_k[llaves_persona]
    ) ||
    anyDuplicated(
      base_k_imputada_k41_k42[llaves_persona]
    ) > 0
) {
  stop(
    "La base imputada altero filas, orden o unicidad de las llaves."
  )
}

columnas_originales_ausentes <- setdiff(
  names(base_k),
  names(base_k_imputada_k41_k42)
)

if (length(columnas_originales_ausentes) > 0) {
  stop(
    "La base imputada perdio columnas originales de base_k: ",
    paste(columnas_originales_ausentes, collapse = ", ")
  )
}

if (!identical(
  diagnostico_consolidado_original,
  serialize(
    insumos[objetos_diagnostico_consolidado],
    connection = NULL
  )
)) {
  stop(
    "La imputacion modifico objetos del diagnostico consolidado."
  )
}

columnas_protegidas_resumenes <- unique(c(
  llaves_persona,
  "NPCKP36",
  "NPCKP36A",
  "NPCKP37",
  variables_bloque_asalariados_limpieza
))

valores_protegidos_antes_resumenes <- serialize(
  base_k_imputada_k41_k42[columnas_protegidas_resumenes],
  connection = NULL
)

resumenes_corregidos_k41_k42 <-
  analisisem2025:::construir_resumenes_k41_k42(
    base_diagnostico = base,
    base_imputada = base_k_imputada_k41_k42,
    variables_limpieza =
      variables_bloque_asalariados_limpieza,
    reglas_flujo = reglas_flujo_tbl,
    n_columnas_originales_perdidas =
      length(columnas_originales_ausentes),
    diagnostico_consolidado_modificado = FALSE
  )

resumen_universos <-
  resumenes_corregidos_k41_k42$resumen_universos

faltantes_objetivo <-
  resumenes_corregidos_k41_k42$faltantes_objetivo

resumen_imputacion_k41_k42 <-
  resumenes_corregidos_k41_k42$resumen_imputacion_k41_k42

balance_npckp36 <-
  resumenes_corregidos_k41_k42$balance_npckp36

balance_npckp37 <-
  resumenes_corregidos_k41_k42$balance_npckp37

resumen_limpieza_k41_k42 <-
  resumenes_corregidos_k41_k42$resumen_limpieza

resumen_variables_limpiadas <-
  resumenes_corregidos_k41_k42$variables_limpiadas

controles_integridad_k41_k42 <-
  resumenes_corregidos_k41_k42$controles_integridad

comparacion_ingresos_k41_k42 <-
  resumenes_corregidos_k41_k42$comparacion_ingresos

residuales_k41_k42 <-
  resumenes_corregidos_k41_k42$residuales

if (!identical(
  valores_protegidos_antes_resumenes,
  serialize(
    base_k_imputada_k41_k42[columnas_protegidas_resumenes],
    connection = NULL
  )
)) {
  stop(
    "La construccion de resumenes modifico valores imputados o limpiados."
  )
}

indicadores_control_imputacion <- c(
  "Personas fuera del universo modificadas",
  "Valores observados sobrescritos",
  "Codigos 98/99 utilizados como montos"
)

controles_imputacion <- resumen_imputacion_k41_k42 |>
  dplyr::filter(
    .data$indicador %in% indicadores_control_imputacion
  )

if (
  nrow(controles_imputacion) !=
    length(indicadores_control_imputacion) ||
    any(controles_imputacion$personas != 0)
) {
  stop(
    "Fallaron los controles de seguridad de la imputacion K41-K42."
  )
}

ruta_base_imputada_k41_k42 <- file.path(
  carpeta_salida,
  paste0(
    "base_k_imputada_k41_k42_",
    fecha_corte,
    ".rds"
  )
)

ruta_auditoria_imputacion_k41_k42 <- file.path(
  carpeta_salida,
  paste0(
    "auditoria_imputacion_k41_k42_",
    fecha_corte,
    ".xlsx"
  )
)

saveRDS(
  list(
    base_k_imputada_k41_k42 =
      base_k_imputada_k41_k42,
    resumen_imputacion_k41_k42 =
      resumen_imputacion_k41_k42,
    auditoria_imputacion_k41_k42 =
      auditoria_imputacion_k41_k42,
    resumen_universos = resumen_universos,
    faltantes_objetivo = faltantes_objetivo,
    balance_npckp36 = balance_npckp36,
    balance_npckp37 = balance_npckp37,
    resumen_limpieza_k41_k42 =
      resumen_limpieza_k41_k42,
    resumen_variables_limpiadas =
      resumen_variables_limpiadas,
    controles_integridad_k41_k42 =
      controles_integridad_k41_k42,
    comparacion_ingresos_k41_k42 =
      comparacion_ingresos_k41_k42,
    residuales_k41_k42 = residuales_k41_k42,
    variables_bloque_asalariados_limpieza =
      variables_bloque_asalariados_limpieza,
    parametros = parametros_imputacion
  ),
  ruta_base_imputada_k41_k42,
  compress = FALSE
)

verificacion_rds_imputacion <- readRDS(
  ruta_base_imputada_k41_k42
)

if (
  !identical(
    verificacion_rds_imputacion$resumen_imputacion_k41_k42,
    resumen_imputacion_k41_k42
  ) ||
    !identical(
      verificacion_rds_imputacion$balance_npckp36,
      balance_npckp36
    ) ||
    !identical(
      verificacion_rds_imputacion$balance_npckp37,
      balance_npckp37
    )
) {
  stop(
    "Los resumenes leidos del RDS de imputacion no coinciden ",
    "con los objetos en memoria."
  )
}

rm(verificacion_rds_imputacion)

# ============================================================
# 15.1. Cierre no monetario de residuales NPCKP36
# ============================================================

# Este cierre se aplica exclusivamente a personas del universo que continuan
# sin monto valido despues de la imputacion monetaria. Los codigos 98 y 99 no
# se consideran ingresos y NPCKP37 no se modifica.
depurado_npckp36_post_imputacion <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_k_imputada_k41_k42$NPCKP36
  )

residual_npckp36_antes_cierre <-
  base$universo_npckp36_37 %in% TRUE &
  !depurado_npckp36_post_imputacion$monto_valido

sin_informacion_util_npckp36 <-
  residual_npckp36_antes_cierre &
  base$caso_sin_informacion_monetaria %in% TRUE

npckp37_antes_codificacion_residual <- serialize(
  base_k_imputada_k41_k42$NPCKP37,
  connection = NULL
)

resultado_codificacion_residual_npckp36 <-
  analisisem2025:::codificar_residuales_npckp36(
    data = base_k_imputada_k41_k42,
    residual_npckp36 = residual_npckp36_antes_cierre,
    sin_informacion_util = sin_informacion_util_npckp36
  )

base_k_imputada_k41_k42_codigos_residuales <-
  resultado_codificacion_residual_npckp36$
    base_k_imputada_k41_k42_codigos_residuales

resumen_codificacion_residual_npckp36 <-
  resultado_codificacion_residual_npckp36$
    resumen_codificacion_residual_npckp36

auditoria_codificacion_residual_npckp36 <-
  resultado_codificacion_residual_npckp36$
    auditoria_codificacion_residual_npckp36

controles_codificacion_residual_npckp36 <-
  resultado_codificacion_residual_npckp36$
    controles_codificacion_residual_npckp36

balance_npckp36_post_imputacion_monetaria <- balance_npckp36

balance_npckp36 <-
  resultado_codificacion_residual_npckp36$balance_final_npckp36

resumen_universos <- dplyr::bind_rows(
  resumen_universos,
  analisisem2025:::.fila_resumen_k(
    "npckp36_codigos_residuales_asignados",
    sum(
      base_k_imputada_k41_k42_codigos_residuales$
        NPCKP36_flag_codigo_residual
    ),
    "universo_npckp36_37",
    sum(base$universo_npckp36_37)
  ),
  analisisem2025:::.fila_resumen_k(
    "npckp36_residuales_sin_clasificar_final",
    0,
    "universo_npckp36_37",
    sum(base$universo_npckp36_37)
  ),
  analisisem2025:::.fila_resumen_k(
    "npckp36_total_documentado_final",
    sum(base$universo_npckp36_37),
    "universo_npckp36_37",
    sum(base$universo_npckp36_37)
  )
)

faltantes_objetivo <- faltantes_objetivo |>
  dplyr::mutate(
    codigos_residuales_asignados = dplyr::if_else(
      .data$variable == "NPCKP36",
      sum(
        base_k_imputada_k41_k42_codigos_residuales$
          NPCKP36_flag_codigo_residual
      ),
      0L
    ),
    residuales_sin_clasificar_final = dplyr::if_else(
      .data$variable == "NPCKP36",
      0,
      .data$residuales_despues_imputacion
    ),
    nota_residuales = dplyr::if_else(
      .data$variable == "NPCKP36",
      paste(
        "residuales_despues_imputacion corresponde al saldo monetario",
        "antes del cierre no monetario 98/99"
      ),
      "NPCKP37 no fue modificado por el cierre residual"
    )
  )

if (any(
  controles_codificacion_residual_npckp36$estado != "OK"
)) {
  stop("Fallaron los controles del cierre residual de NPCKP36.")
}

if (!identical(
  serialize(
    base_k_imputada_k41_k42_codigos_residuales$NPCKP37,
    connection = NULL
  ),
  npckp37_antes_codificacion_residual
)) {
  stop("El cierre residual modifico NPCKP37.")
}

# Validacion especifica del corte 20260703. La comparacion se alinea mediante
# la clave tecnica canonica y no depende del orden ni de las descripciones.
conteos_esperados_cierre <- tibble::tibble(
  grupo_codigo_residual_npckp36 = c(
    "98_trasladado_desde_npckp23",
    "99_trasladado_desde_npckp23",
    "99_asignado_ausencia_total"
  ),
  conteo_esperado = c(
    316L,
    1441L,
    760L
  )
)

resultado_validacion_conteos_cierre <-
  analisisem2025:::validar_conteos_cierre_npckp36(
    auditoria = auditoria_codificacion_residual_npckp36,
    conteos_esperados = conteos_esperados_cierre
  )

conteos_observados_cierre <-
  resultado_validacion_conteos_cierre$conteos_observados_cierre

validacion_conteos_cierre <-
  resultado_validacion_conteos_cierre$validacion_conteos_cierre

print(
  validacion_conteos_cierre,
  n = Inf,
  width = Inf
)

control_total_cierre <-
  sum(conteos_observados_cierre$conteo_observado) == 2517L

control_residuales_sin_clasificar <-
  sum(
    auditoria_codificacion_residual_npckp36$
      residual_sin_clasificar,
    na.rm = TRUE
  ) == 0L

if (
  !all(validacion_conteos_cierre$diferencia == 0L) ||
    !control_total_cierre ||
    !control_residuales_sin_clasificar
) {
  stop(
    paste0(
      "Los conteos del cierre residual NPCKP36 no coinciden ",
      "con la validacion esperada para el corte ",
      fecha_corte,
      ". Revise validacion_conteos_cierre."
    )
  )
}

ruta_base_k41_k42_codigos_residuales <- file.path(
  carpeta_salida,
  paste0(
    "base_k_imputada_k41_k42_codigos_residuales_",
    fecha_corte,
    ".rds"
  )
)

saveRDS(
  list(
    base_k_imputada_k41_k42_codigos_residuales =
      base_k_imputada_k41_k42_codigos_residuales,
    resumen_codificacion_residual_npckp36 =
      resumen_codificacion_residual_npckp36,
    balance_final_npckp36 = balance_npckp36,
    balance_npckp36_post_imputacion_monetaria =
      balance_npckp36_post_imputacion_monetaria,
    auditoria_codificacion_residual_npckp36 =
      auditoria_codificacion_residual_npckp36,
    controles_codificacion_residual_npckp36 =
      controles_codificacion_residual_npckp36,
    conteos_observados_cierre = conteos_observados_cierre,
    validacion_conteos_cierre = validacion_conteos_cierre,
    parametros = list(
      fecha_corte = fecha_corte,
      metodo = "codificacion_residual_no_monetaria",
      codigos_permitidos = c(98L, 99L),
      variable_fuente_traslado = "NPCKP23_original",
      variable_sin_informacion_util =
        "caso_sin_informacion_monetaria"
    )
  ),
  ruta_base_k41_k42_codigos_residuales,
  compress = FALSE
)

verificacion_rds_codigos_residuales <- readRDS(
  ruta_base_k41_k42_codigos_residuales
)

if (
  !identical(
    verificacion_rds_codigos_residuales$
      base_k_imputada_k41_k42_codigos_residuales,
    base_k_imputada_k41_k42_codigos_residuales
  ) ||
    !identical(
      verificacion_rds_codigos_residuales$
        resumen_codificacion_residual_npckp36,
      resumen_codificacion_residual_npckp36
    ) ||
    !identical(
      verificacion_rds_codigos_residuales$balance_final_npckp36,
      balance_npckp36
    ) ||
    !identical(
      verificacion_rds_codigos_residuales$
        validacion_conteos_cierre,
      validacion_conteos_cierre
    )
) {
  stop(
    "El RDS con codigos residuales no coincide con los objetos en memoria."
  )
}

rm(verificacion_rds_codigos_residuales)

# ============================================================
# 15.2. Cierre definitivo de NPCKP37
# ============================================================

npckp36_antes_cierre_npckp37 <- serialize(
  base_k_imputada_k41_k42_codigos_residuales$NPCKP36,
  connection = NULL
)
npckp36_original_antes_cierre_npckp37 <- serialize(
  base_k_imputada_k41_k42_codigos_residuales$NPCKP36_original,
  connection = NULL
)
npckp36a_original_antes_cierre_npckp37 <- serialize(
  base_k_imputada_k41_k42_codigos_residuales$NPCKP36A_original,
  connection = NULL
)
npckp36a_antes_cierre_npckp37 <-
  base_k_imputada_k41_k42_codigos_residuales$NPCKP36A
npckp37_original_antes_cierre <- serialize(
  base_k_imputada_k41_k42_codigos_residuales$NPCKP37_original,
  connection = NULL
)

resultado_cierre_npckp37 <-
  analisisem2025:::cerrar_npckp37(
    base_k_imputada_k41_k42_codigos_residuales
  )

base_k_imputada_k41_k42_cierre_npckp37 <-
  resultado_cierre_npckp37$
    base_k_imputada_k41_k42_cierre_npckp37

resumen_cierre_npckp37 <-
  resultado_cierre_npckp37$resumen_cierre_npckp37

balance_final_npckp37 <-
  resultado_cierre_npckp37$balance_final_npckp37

auditoria_cierre_npckp37 <-
  resultado_cierre_npckp37$auditoria_cierre_npckp37

controles_cierre_npckp37 <-
  resultado_cierre_npckp37$controles_cierre_npckp37

distribucion_meses_donantes <-
  resultado_cierre_npckp37$distribucion_meses_donantes

casos_moda_dominante_npckp37 <-
  base_k_imputada_k41_k42_cierre_npckp37$
    universo_npckp36_37 %in% TRUE &
  base_k_imputada_k41_k42_cierre_npckp37$
    NPCKP37_metodo_imputacion ==
    "un_mes_por_moda_dominante"
casos_moda_dominante_npckp37[
  is.na(casos_moda_dominante_npckp37)
] <- FALSE

dep_npckp36_final <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_k_imputada_k41_k42_cierre_npckp37$NPCKP36
  )
dep_npckp36a_final <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_k_imputada_k41_k42_cierre_npckp37$NPCKP36A
  )

npckp36a_coincide_npckp36 <-
  (
    dep_npckp36_final$monto_valido &
      dep_npckp36a_final$monto_valido &
      dep_npckp36_final$monto == dep_npckp36a_final$monto
  ) |
  (
    dep_npckp36_final$codigo_98 &
      dep_npckp36a_final$codigo_98
  ) |
  (
    dep_npckp36_final$codigo_99 &
      dep_npckp36a_final$codigo_99
  )
npckp36a_coincide_npckp36[
  is.na(npckp36a_coincide_npckp36)
] <- FALSE

casos_sincronizados_npckp36a <- casos_moda_dominante_npckp37 &
  npckp36a_coincide_npckp36 &
  base_k_imputada_k41_k42_cierre_npckp37$
    NPCKP36A_flag_imputado %in% TRUE &
  base_k_imputada_k41_k42_cierre_npckp37$
    NPCKP36A_metodo_imputacion ==
    "sincronizacion_con_npckp36_por_meses_imputados_moda"
casos_sincronizados_npckp36a[
  is.na(casos_sincronizados_npckp36a)
] <- FALSE

npckp36a_modificados_fuera_moda <- !identical(
  base_k_imputada_k41_k42_cierre_npckp37$NPCKP36A[
    !casos_moda_dominante_npckp37
  ],
  npckp36a_antes_cierre_npckp37[
    !casos_moda_dominante_npckp37
  ]
)

npckp36a_modificados_en_moda <- sum(
  as.character(
    base_k_imputada_k41_k42_cierre_npckp37$NPCKP36A[
      casos_moda_dominante_npckp37
    ]
  ) !=
    as.character(
      npckp36a_antes_cierre_npckp37[
        casos_moda_dominante_npckp37
      ]
    ),
  na.rm = TRUE
)

universo_cierre_npckp37 <-
  base_k_imputada_k41_k42_cierre_npckp37$
    universo_npckp36_37 %in% TRUE

controles_sincronizacion_npckp36a <- tibble::tibble(
  control = c(
    "NPCKP36 modificados",
    "NPCKP36_original modificados",
    "NPCKP36A_original modificados",
    "NPCKP37_original modificados",
    "NPCKP36A modificados fuera del grupo moda",
    "Casos objetivo sincronizacion NPCKP36A",
    "NPCKP36A modificados esperados",
    "Casos observados sincronizados NPCKP36A",
    "NPCKP36A distinto de NPCKP36 en universo",
    "NPCKP36A montos validos",
    "NPCKP36A codigos 98",
    "NPCKP36A codigos 99",
    "NPCKP36 montos validos",
    "NPCKP36 codigos 98",
    "NPCKP36 codigos 99"
  ),
  valor_observado = c(
    as.integer(!identical(
      serialize(
        base_k_imputada_k41_k42_cierre_npckp37$NPCKP36,
        NULL
      ),
      npckp36_antes_cierre_npckp37
    )),
    as.integer(!identical(
      serialize(
        base_k_imputada_k41_k42_cierre_npckp37$NPCKP36_original,
        NULL
      ),
      npckp36_original_antes_cierre_npckp37
    )),
    as.integer(!identical(
      serialize(
        base_k_imputada_k41_k42_cierre_npckp37$NPCKP36A_original,
        NULL
      ),
      npckp36a_original_antes_cierre_npckp37
    )),
    as.integer(!identical(
      serialize(
        base_k_imputada_k41_k42_cierre_npckp37$NPCKP37_original,
        NULL
      ),
      npckp37_original_antes_cierre
    )),
    as.integer(npckp36a_modificados_fuera_moda),
    sum(casos_moda_dominante_npckp37),
    npckp36a_modificados_en_moda,
    sum(casos_sincronizados_npckp36a),
    sum(
      universo_cierre_npckp37 &
        !npckp36a_coincide_npckp36
    ),
    sum(universo_cierre_npckp37 & dep_npckp36a_final$monto_valido),
    sum(universo_cierre_npckp37 & dep_npckp36a_final$codigo_98),
    sum(universo_cierre_npckp37 & dep_npckp36a_final$codigo_99),
    sum(universo_cierre_npckp37 & dep_npckp36_final$monto_valido),
    sum(universo_cierre_npckp37 & dep_npckp36_final$codigo_98),
    sum(universo_cierre_npckp37 & dep_npckp36_final$codigo_99)
  ),
  valor_esperado = c(
    0L, 0L, 0L, 0L, 0L,
    880L, 880L, 880L, 0L,
    33591L, 316L, 2201L,
    33591L, 316L, 2201L
  )
) |>
  dplyr::mutate(
    estado = dplyr::if_else(
      .data$valor_observado == .data$valor_esperado,
      "OK",
      "ALERTA"
    )
  )

print(
  controles_sincronizacion_npckp36a,
  n = Inf,
  width = Inf
)

if (any(controles_sincronizacion_npckp36a$estado != "OK")) {
  stop(
    "Fallaron los controles de sincronizacion NPCKP36A/NPCKP36."
  )
}

controles_cierre_npckp37 <- dplyr::bind_rows(
  controles_cierre_npckp37,
  controles_sincronizacion_npckp36a
)

balance_esperado_npckp37 <- tibble::tibble(
  categoria = c(
    "Respuesta original valida",
    "Un mes por ingreso reconstruido",
    "Un mes por moda dominante",
    "Codigo 98 trasladado",
    "Codigo 99 trasladado o asignado",
    "Residual sin clasificar"
  ),
  conteo_esperado = c(
    15118L,
    17593L,
    880L,
    316L,
    2201L,
    0L
  )
)

validacion_balance_npckp37 <- balance_esperado_npckp37 |>
  dplyr::full_join(
    balance_final_npckp37 |>
      dplyr::select(
        categoria,
        conteo_observado = personas
      ),
    by = "categoria"
  ) |>
  dplyr::mutate(
    conteo_esperado = tidyr::replace_na(
      .data$conteo_esperado,
      0L
    ),
    conteo_observado = tidyr::replace_na(
      .data$conteo_observado,
      0L
    ),
    diferencia =
      .data$conteo_observado - .data$conteo_esperado,
    estado = dplyr::if_else(
      .data$diferencia == 0L,
      "OK",
      "ALERTA"
    )
  )

print(
  validacion_balance_npckp37,
  n = Inf,
  width = Inf
)

origenes_esperados_npckp37 <- tibble::tibble(
  NPCKP37_origen_codigo_residual = c(
    "codigo_98_trasladado_desde_npckp36",
    "codigo_99_trasladado_desde_npckp36",
    "codigo_99_ausencia_total"
  ),
  conteo_esperado = c(316L, 1441L, 760L)
)

validacion_origenes_npckp37 <- origenes_esperados_npckp37 |>
  dplyr::full_join(
    auditoria_cierre_npckp37 |>
      dplyr::filter(.data$NPCKP37_flag_codigo_residual) |>
      dplyr::count(
        NPCKP37_origen_codigo_residual,
        name = "conteo_observado"
      ),
    by = "NPCKP37_origen_codigo_residual"
  ) |>
  dplyr::mutate(
    conteo_esperado = tidyr::replace_na(
      .data$conteo_esperado,
      0L
    ),
    conteo_observado = tidyr::replace_na(
      .data$conteo_observado,
      0L
    ),
    diferencia =
      .data$conteo_observado - .data$conteo_esperado,
    estado = dplyr::if_else(
      .data$diferencia == 0L,
      "OK",
      "ALERTA"
    )
  )

print(
  validacion_origenes_npckp37,
  n = Inf,
  width = Inf
)

n_donantes_npckp37 <- distribucion_meses_donantes |>
  dplyr::filter(.data$alcance == "Total donantes") |>
  dplyr::summarise(personas = sum(.data$personas)) |>
  dplyr::pull(personas)

n_donantes_un_mes_npckp37 <- distribucion_meses_donantes |>
  dplyr::filter(
    .data$alcance == "Total donantes",
    .data$meses == 1L
  ) |>
  dplyr::summarise(personas = sum(.data$personas)) |>
  dplyr::pull(personas)

if (
  !all(validacion_balance_npckp37$diferencia == 0L) ||
    !all(validacion_origenes_npckp37$diferencia == 0L) ||
    sum(balance_final_npckp37$personas) != 36108L ||
    nrow(auditoria_cierre_npckp37) != 3397L ||
    n_donantes_npckp37 != 15118L ||
    n_donantes_un_mes_npckp37 != 15003L ||
    any(controles_cierre_npckp37$estado != "OK")
) {
  stop(
    paste0(
      "El cierre NPCKP37 no coincide con la validacion esperada ",
      "para el corte ",
      fecha_corte,
      ". Revise validacion_balance_npckp37 y ",
      "controles_cierre_npckp37."
    )
  )
}

resumen_universos <- dplyr::bind_rows(
  resumen_universos,
  analisisem2025:::.fila_resumen_k(
    "npckp37_un_mes_por_moda_dominante",
    880L,
    "universo_npckp36_37",
    36108L
  ),
  analisisem2025:::.fila_resumen_k(
    "npckp37_codigos_residuales_asignados",
    2517L,
    "universo_npckp36_37",
    36108L
  ),
  analisisem2025:::.fila_resumen_k(
    "npckp37_residuales_sin_clasificar_final",
    0L,
    "universo_npckp36_37",
    36108L
  )
)

faltantes_objetivo <- faltantes_objetivo |>
  dplyr::mutate(
    residuales_sin_clasificar_final = dplyr::if_else(
      .data$variable == "NPCKP37",
      0,
      .data$residuales_sin_clasificar_final
    ),
    nota_residuales = dplyr::if_else(
      .data$variable == "NPCKP37",
      paste(
        "NPCKP37 fue cerrado con un mes por moda dominante",
        "o traslado de codigos 98/99 desde NPCKP36"
      ),
      .data$nota_residuales
    )
  )

ruta_base_cierre_npckp37 <- file.path(
  carpeta_salida,
  paste0(
    "base_k_imputada_k41_k42_cierre_npckp37_",
    fecha_corte,
    ".rds"
  )
)

saveRDS(
  list(
    base_k_imputada_k41_k42_cierre_npckp37 =
      base_k_imputada_k41_k42_cierre_npckp37,
    resumen_cierre_npckp37 = resumen_cierre_npckp37,
    balance_final_npckp37 = balance_final_npckp37,
    auditoria_cierre_npckp37 = auditoria_cierre_npckp37,
    controles_cierre_npckp37 = controles_cierre_npckp37,
    distribucion_meses_donantes = distribucion_meses_donantes,
    validacion_balance_npckp37 = validacion_balance_npckp37,
    validacion_origenes_npckp37 = validacion_origenes_npckp37,
    parametros = c(
      resultado_cierre_npckp37$parametros,
      list(fecha_corte = fecha_corte)
    )
  ),
  ruta_base_cierre_npckp37,
  compress = FALSE
)

verificacion_rds_cierre_npckp37 <- readRDS(
  ruta_base_cierre_npckp37
)

if (
  !identical(
    verificacion_rds_cierre_npckp37$
      base_k_imputada_k41_k42_cierre_npckp37,
    base_k_imputada_k41_k42_cierre_npckp37
  ) ||
    !identical(
      verificacion_rds_cierre_npckp37$balance_final_npckp37,
      balance_final_npckp37
    ) ||
    !identical(
      verificacion_rds_cierre_npckp37$controles_cierre_npckp37,
      controles_cierre_npckp37
    )
) {
  stop("El RDS del cierre NPCKP37 no coincide con los objetos en memoria.")
}

rm(verificacion_rds_cierre_npckp37)

wb_imputacion <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb_imputacion,
  "00_resumen_imputacion"
)

openxlsx::writeData(
  wb_imputacion,
  "00_resumen_imputacion",
  resumen_imputacion_k41_k42,
  na.string = ""
)

openxlsx::addWorksheet(
  wb_imputacion,
  "01_auditoria_personas"
)

openxlsx::writeData(
  wb_imputacion,
  "01_auditoria_personas",
  auditoria_imputacion_k41_k42,
  na.string = ""
)

openxlsx::saveWorkbook(
  wb_imputacion,
  ruta_auditoria_imputacion_k41_k42,
  overwrite = TRUE
)

# ============================================================
# 16. Guardado de resultados
# ============================================================

ruta_rds_salida <- file.path(
  carpeta_salida,
  paste0(
    "diagnostico_imputacion_npckp36_npckp37_",
    fecha_corte,
    ".rds"
  )
)

ruta_excel_salida <- file.path(
  carpeta_salida,
  paste0(
    "diagnostico_imputacion_npckp36_npckp37_",
    fecha_corte,
    ".xlsx"
  )
)

saveRDS(
  list(
    resumen_universos = resumen_universos,
    faltantes_objetivo = faltantes_objetivo,
    fuera_flujo_bloque_asalariados =
      fuera_flujo_bloque_asalariados,
    control_npckp23a = control_npckp23a,
    resumen_diferencias = resumen_diferencias,
    cuantiles_ingresos = cuantiles_ingresos,
    resumen_por_posicion = resumen_por_posicion,
    resumen_componentes = resumen_componentes,
    resumen_completitud = resumen_completitud,
    cuantiles_validacion_observados =
      cuantiles_validacion_observados,
    validacion_con_npckp36_observado =
      validacion_con_npckp36_observado,
    resumen_formatos = resumen_formatos,
    auditoria_codigos_especiales_montos =
      auditoria_codigos_especiales_montos,
    valores_frecuentes = valores_frecuentes,
    diccionario_ingresos = diccionario_ingresos,
    casos_diagnostico = casos_diagnostico,
    casos_recuperables_npckp36 =
      casos_recuperables_npckp36,
    casos_para_limpieza_posterior =
      casos_para_limpieza_posterior,
    resumen_imputacion_k41_k42 =
      resumen_imputacion_k41_k42,
    resumen_codificacion_residual_npckp36 =
      resumen_codificacion_residual_npckp36,
    balance_npckp36 = balance_npckp36,
    balance_npckp36_post_imputacion_monetaria =
      balance_npckp36_post_imputacion_monetaria,
    balance_npckp37 = balance_npckp37,
    resumen_limpieza_k41_k42 =
      resumen_limpieza_k41_k42,
    resumen_variables_limpiadas =
      resumen_variables_limpiadas,
    controles_integridad_k41_k42 =
      controles_integridad_k41_k42,
    comparacion_ingresos_k41_k42 =
      comparacion_ingresos_k41_k42,
    residuales_k41_k42 = residuales_k41_k42,
    auditoria_codificacion_residual_npckp36 =
      auditoria_codificacion_residual_npckp36,
    controles_codificacion_residual_npckp36 =
      controles_codificacion_residual_npckp36,
    conteos_observados_cierre = conteos_observados_cierre,
    validacion_conteos_cierre = validacion_conteos_cierre,
    resumen_cierre_npckp37 = resumen_cierre_npckp37,
    balance_final_npckp37 = balance_final_npckp37,
    auditoria_cierre_npckp37 = auditoria_cierre_npckp37,
    controles_cierre_npckp37 = controles_cierre_npckp37,
    distribucion_meses_donantes = distribucion_meses_donantes,
    validacion_balance_npckp37 = validacion_balance_npckp37,
    validacion_origenes_npckp37 = validacion_origenes_npckp37,
    parametros = list(
      fecha_corte = fecha_corte,
      ruta_insumos = ruta_insumos,
      usar_npckp23a_como_monto = usar_23a,
      regla_ingreso_acotado =
        "NPCKP23 confirmado + horas extras no incluidas",
      regla_ingreso_amplio =
        paste(
          "Ingreso acotado + pagos mensuales adicionales",
          "+ pagos de los ultimos 12 meses divididos entre 12"
        ),
      regla_cierre_residual_npckp36 =
        paste(
          "Trasladar 98/99 desde NPCKP23_original y asignar 99",
          "solo ante ausencia total de informacion monetaria"
        ),
      codigos_residuales_no_monetarios = c(98L, 99L),
      variable_oficial_sistemas_npckp37 = "NPCKP37",
      variable_trazabilidad_npckp37 = "NPCKP37_original"
    )
  ),
  ruta_rds_salida,
  compress = FALSE
)

verificacion_rds_diagnostico <- readRDS(
  ruta_rds_salida
)

if (
  !identical(
    verificacion_rds_diagnostico$resumen_universos,
    resumen_universos
  ) ||
    !identical(
      verificacion_rds_diagnostico$faltantes_objetivo,
      faltantes_objetivo
    ) ||
    !identical(
      verificacion_rds_diagnostico$resumen_imputacion_k41_k42,
      resumen_imputacion_k41_k42
    ) ||
    !identical(
      verificacion_rds_diagnostico$
        resumen_codificacion_residual_npckp36,
      resumen_codificacion_residual_npckp36
    ) ||
    !identical(
      verificacion_rds_diagnostico$balance_npckp36,
      balance_npckp36
    ) ||
    !identical(
      verificacion_rds_diagnostico$
        controles_codificacion_residual_npckp36,
      controles_codificacion_residual_npckp36
    ) ||
    !identical(
      verificacion_rds_diagnostico$validacion_conteos_cierre,
      validacion_conteos_cierre
    ) ||
    !identical(
      verificacion_rds_diagnostico$balance_final_npckp37,
      balance_final_npckp37
    ) ||
    !identical(
      verificacion_rds_diagnostico$controles_cierre_npckp37,
      controles_cierre_npckp37
    )
) {
  stop(
    "Los resumenes leidos del RDS diagnostico no coinciden ",
    "con los objetos en memoria."
  )
}

rm(verificacion_rds_diagnostico)

wb <- openxlsx::createWorkbook()

escribir_hoja <- function(wb, hoja, datos) {
  openxlsx::addWorksheet(
    wb,
    hoja
  )

  datos <- tibble::as_tibble(
    datos
  )

  if (nrow(datos) > 1000000L) {
    openxlsx::writeData(
      wb,
      hoja,
      tibble::tibble(
        mensaje =
          "La tabla supera el limite de Excel y queda completa en el RDS.",
        filas = nrow(datos),
        ruta_rds = ruta_rds_salida
      ),
      na.string = ""
    )

    return(
      invisible(NULL)
    )
  }

  openxlsx::writeData(
    wb,
    hoja,
    datos,
    na.string = ""
  )

  invisible(NULL)
}

hojas <- list(
  "00_universos" = resumen_universos,
  "01_faltantes_objetivo" = faltantes_objetivo,
  "02_fuera_flujo_asal" = fuera_flujo_bloque_asalariados,
  "03_control_npckp23a" = control_npckp23a,
  "04_diferencias" = resumen_diferencias,
  "05_cuantiles_ingresos" = cuantiles_ingresos,
  "06_por_posicion" = resumen_por_posicion,
  "07_componentes" = resumen_componentes,
  "08_completitud" = resumen_completitud,
  "09_validacion_observados" = cuantiles_validacion_observados,
  "10_formatos" = resumen_formatos,
  "11_auditoria_codigos" = auditoria_codigos_especiales_montos,
  "12_valores_frecuentes" = valores_frecuentes,
  "13_diccionario" = diccionario_ingresos,
  "14_recuperables" = casos_recuperables_npckp36,
  "15_limpieza_posterior" = casos_para_limpieza_posterior,
  "16_resumen_imputacion" = resumen_imputacion_k41_k42,
  "17_balance_npckp36" = balance_npckp36,
  "18_balance_npckp37" = balance_npckp37,
  "19_resumen_limpieza" = resumen_limpieza_k41_k42,
  "20_variables_limpiadas" = resumen_variables_limpiadas,
  "21_controles_integridad" = controles_integridad_k41_k42,
  "22_comparacion_ingresos" = comparacion_ingresos_k41_k42,
  "23_residuales_monetarios" = residuales_k41_k42,
  "24_resumen_cod_res" = resumen_codificacion_residual_npckp36,
  "25_auditoria_cod_res" = auditoria_codificacion_residual_npckp36,
  "26_controles_cod_res" = controles_codificacion_residual_npckp36,
  "27_validacion_conteos" = validacion_conteos_cierre,
  "28_resumen_npckp37" = resumen_cierre_npckp37,
  "29_balance_npckp37" = balance_final_npckp37,
  "30_auditoria_npckp37" = auditoria_cierre_npckp37,
  "31_controles_npckp37" = controles_cierre_npckp37,
  "32_distribucion_meses" = distribucion_meses_donantes
)

if (
  !identical(
    hojas[["16_resumen_imputacion"]],
    resumen_imputacion_k41_k42
  ) ||
    !identical(
      hojas[["17_balance_npckp36"]],
      balance_npckp36
    ) ||
    !identical(
      hojas[["18_balance_npckp37"]],
      balance_npckp37
    ) ||
    nrow(hojas[["23_residuales_monetarios"]]) !=
      nrow(residuales_k41_k42) ||
    !identical(
      hojas[["24_resumen_cod_res"]],
      resumen_codificacion_residual_npckp36
    ) ||
    !identical(
      hojas[["26_controles_cod_res"]],
      controles_codificacion_residual_npckp36
    ) ||
    !identical(
      hojas[["27_validacion_conteos"]],
      validacion_conteos_cierre
    ) ||
    !identical(
      hojas[["29_balance_npckp37"]],
      balance_final_npckp37
    ) ||
    !identical(
      hojas[["31_controles_npckp37"]],
      controles_cierre_npckp37
    )
) {
  stop(
    "Los objetos preparados para Excel no coinciden con los resumenes ",
    "guardados en el RDS."
  )
}

if (
  any(c(
    "base_k_imputada_k41_k42",
    "base_k_imputada_k41_k42_codigos_residuales",
    "base_k_imputada_k41_k42_cierre_npckp37"
  ) %in% names(hojas)) ||
    any(vapply(
      hojas,
      function(x) {
        identical(x, base_k_imputada_k41_k42) ||
          identical(
            x,
            base_k_imputada_k41_k42_codigos_residuales
          ) ||
          identical(
            x,
            base_k_imputada_k41_k42_cierre_npckp37
          )
      },
      logical(1)
    ))
) {
  stop(
    "El Excel no debe contener la base K imputada completa."
  )
}

for (hoja in names(hojas)) {
  escribir_hoja(
    wb,
    hoja,
    hojas[[hoja]]
  )
}

openxlsx::saveWorkbook(
  wb,
  ruta_excel_salida,
  overwrite = TRUE
)

readr::write_csv2(
  resumen_universos,
  file.path(
    carpeta_salida,
    paste0(
      "resumen_universos_npckp36_npckp37_",
      fecha_corte,
      ".csv"
    )
  )
)

readr::write_csv2(
  resumen_diferencias,
  file.path(
    carpeta_salida,
    paste0(
      "resumen_diferencias_ingresos_",
      fecha_corte,
      ".csv"
    )
  )
)

readr::write_csv2(
  fuera_flujo_bloque_asalariados,
  file.path(
    carpeta_salida,
    paste0(
      "fuera_flujo_bloque_asalariados_",
      fecha_corte,
      ".csv"
    )
  )
)

# ============================================================
# 17. Salida final en consola
# ============================================================

cat(
  "\n============================================================\n",
  "DIAGNOSTICO NPCKP36 Y NPCKP37 FINALIZADO\n",
  "============================================================\n",
  "\nRuta RDS: ",
  ruta_rds_salida,
  "\nRuta Excel: ",
  ruta_excel_salida,
  "\n",
  sep = ""
)

cat(
  "\nResumen de universos:\n"
)

print(
  resumen_universos,
  n = Inf,
  width = Inf
)

cat(
  "\nControl de NPCKP23A:\n"
)

print(
  control_npckp23a,
  n = Inf,
  width = Inf
)

cat(
  "\nDiferencias entre ingreso amplio y acotado:\n"
)

print(
  resumen_diferencias,
  n = Inf,
  width = Inf
)

cat(
  "\nCuantiles principales:\n"
)

print(
  cuantiles_ingresos,
  n = Inf,
  width = Inf
)

cat(
  "\nFaltantes, resueltos y residuales:\n"
)

print(
  faltantes_objetivo,
  n = Inf,
  width = Inf
)

cat(
  "\nResumen de imputacion y limpieza K41-K42:\n"
)

print(
  resumen_imputacion_k41_k42,
  n = Inf,
  width = Inf
)

cat(
  "\nControles de integridad:\n"
)

print(
  controles_integridad_k41_k42,
  n = Inf,
  width = Inf
)

cat(
  "\nResumen del cierre residual no monetario de NPCKP36:\n"
)

print(
  resumen_codificacion_residual_npckp36,
  n = Inf,
  width = Inf
)

cat(
  "\nBalance final de NPCKP36:\n"
)

print(
  balance_npckp36,
  n = Inf,
  width = Inf
)

cat(
  "\nControles del cierre residual de NPCKP36:\n"
)

print(
  controles_codificacion_residual_npckp36,
  n = Inf,
  width = Inf
)

cat(
  "\nResumen del cierre definitivo de NPCKP37:\n"
)

print(
  resumen_cierre_npckp37,
  n = Inf,
  width = Inf
)

cat(
  "\nBalance final de NPCKP37:\n"
)

print(
  balance_final_npckp37,
  n = Inf,
  width = Inf
)

cat(
  "\nControles del cierre definitivo de NPCKP37:\n"
)

print(
  controles_cierre_npckp37,
  n = Inf,
  width = Inf
)

cat(
  "\nBase imputada K41-K42:\n",
  ruta_base_imputada_k41_k42,
  "\nBase imputada K41-K42 con codigos residuales:\n",
  ruta_base_k41_k42_codigos_residuales,
  "\nBase final con cierre NPCKP37 para Sistemas:\n",
  ruta_base_cierre_npckp37,
  "\nAuditoria de imputacion K41-K42:\n",
  ruta_auditoria_imputacion_k41_k42,
  "\n",
  sep = ""
)

if (interactive()) {
  View(resumen_universos)
  View(faltantes_objetivo)
  View(fuera_flujo_bloque_asalariados)
  View(control_npckp23a)
  View(resumen_diferencias)
  View(cuantiles_ingresos)
  View(resumen_componentes)
  View(casos_recuperables_npckp36)
}
