# ============================================================
# 04_Prueba_Aceptacion_Base_OSIS_K41_K42_20260703.R
#
# Prueba de aceptación sobre la base K que se entregará a OSIS.
#
# Valida:
# - flujo teórico mediante diagnostico_flujo_capitulo_k();
# - ausencia de vacíos críticos;
# - ausencia de respuestas fuera de flujo;
# - estabilidad de los flujos indeterminados frente al diagnóstico original;
# - dominios de NPCKP36, NPCKP36A y NPCKP37;
# - sincronización NPCKP36 / NPCKP36A;
# - coherencia entre ingreso y número de meses;
# - preservación de valores monetarios originales válidos;
# - integridad de filas, columnas y llaves;
# - balances sustantivos aprobados para el corte 20260703.
#
# IMPORTANTE:
# - Este script NO corrige la base.
# - Si encuentra una inconsistencia, guarda RDS y Excel y luego se detiene.
# ============================================================

rm(list = ls())
gc()

options(scipen = 999)

suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(openxlsx)
})

# ============================================================
# 0. Configuración
# ============================================================

fecha_corte <- "20260703"

ruta_paquete <-
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"

carpeta_caps <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive",
  "DANE",
  "Multiproposito",
  "Validacion",
  "Encuestas",
  "Validar",
  paste0("CAP_EM_", fecha_corte)
)

ruta_em_original <- file.path(
  carpeta_caps,
  paste0(
    "em_completa_sin_tematica_",
    fecha_corte,
    ".rds"
  )
)

ruta_insumos <- file.path(
  carpeta_caps,
  "diagnostico_flujo_capK_diccionario",
  paste0(
    "insumos_imputacion_capK_",
    fecha_corte,
    ".rds"
  )
)

ruta_base_final <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "03_diagnostico_npckp36_npckp37",
  paste0(
    "base_k_imputada_k41_k42_cierre_npckp37_",
    fecha_corte,
    ".rds"
  )
)

carpeta_salida <- file.path(
  carpeta_caps,
  "imputacion_capK",
  "04_prueba_aceptacion_osis"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

stopifnot(
  dir.exists(ruta_paquete),
  file.exists(ruta_em_original),
  file.exists(ruta_insumos),
  file.exists(ruta_base_final)
)

devtools::load_all(
  ruta_paquete,
  quiet = TRUE
)

# ============================================================
# 1. Cargar original, insumos y base final
# ============================================================

em_original <- readRDS(
  ruta_em_original
)

insumos <- readRDS(
  ruta_insumos
)

objeto_final <- readRDS(
  ruta_base_final
)

stopifnot(
  "dfs" %in% names(em_original),
  "K" %in% names(em_original$dfs),
  is.data.frame(em_original$dfs$K),
  "diccionario_k" %in% names(insumos),
  "resumen_variables" %in% names(insumos)
)

base_k_original <- tibble::as_tibble(
  em_original$dfs$K
)

# Variables con regla de flujo propia.
variables_objetivo_flujo <- c(
  "NPCKP36",
  "NPCKP37"
)

# Variables finales que se revisan para OSIS.
variables_objetivo_final <- c(
  "NPCKP36",
  "NPCKP36A",
  "NPCKP37"
)

llaves_persona <- c(
  "DIRECTORIO",
  "SECUENCIA_P",
  "ORDEN"
)

extraer_base_final <- function(
    objeto,
    n_filas_esperado,
    variables_requeridas
) {

  if (is.data.frame(objeto)) {
    return(
      tibble::as_tibble(objeto)
    )
  }

  if (!is.list(objeto)) {
    stop(
      "El RDS final no contiene una tabla ni una lista de objetos."
    )
  }

  candidatos <- names(objeto)[
    vapply(
      objeto,
      function(x) {
        is.data.frame(x) &&
          nrow(x) == n_filas_esperado &&
          all(
            variables_requeridas %in% names(x)
          )
      },
      logical(1)
    )
  ]

  if (length(candidatos) != 1L) {
    stop(
      "No fue posible identificar de manera única la base final. ",
      "Candidatos encontrados: ",
      paste(candidatos, collapse = ", ")
    )
  }

  message(
    "Base final extraída del objeto: ",
    candidatos
  )

  tibble::as_tibble(
    objeto[[candidatos]]
  )
}

base_k_osis <- extraer_base_final(
  objeto = objeto_final,
  n_filas_esperado = nrow(base_k_original),
  variables_requeridas = c(
    llaves_persona,
    variables_objetivo_final
  )
)

# ============================================================
# 2. Controles estructurales preliminares
# ============================================================

columnas_originales_ausentes <- setdiff(
  names(base_k_original),
  names(base_k_osis)
)

variables_trazabilidad_requeridas <- c(
  "NPCKP36_original",
  "NPCKP36A_original",
  "NPCKP37_original"
)

variables_trazabilidad_ausentes <- setdiff(
  variables_trazabilidad_requeridas,
  names(base_k_osis)
)

if (length(columnas_originales_ausentes) > 0L) {
  stop(
    "La base final perdió columnas originales: ",
    paste(columnas_originales_ausentes, collapse = ", ")
  )
}

if (length(variables_trazabilidad_ausentes) > 0L) {
  stop(
    "Faltan variables originales de trazabilidad: ",
    paste(
      variables_trazabilidad_ausentes,
      collapse = ", "
    )
  )
}

copias_originales_coinciden <- c(
  NPCKP36_original =
    identical(
      base_k_osis$NPCKP36_original,
      base_k_original$NPCKP36
    ),

  NPCKP36A_original =
    identical(
      base_k_osis$NPCKP36A_original,
      base_k_original$NPCKP36A
    ),

  NPCKP37_original =
    identical(
      base_k_osis$NPCKP37_original,
      base_k_original$NPCKP37
    )
)

duplicados_llaves <- base_k_osis |>
  dplyr::count(
    dplyr::across(
      dplyr::all_of(llaves_persona)
    ),
    name = "n"
  ) |>
  dplyr::filter(
    .data$n > 1L
  )

orden_llaves_conservado <- identical(
  base_k_osis[llaves_persona],
  base_k_original[llaves_persona]
)

if (nrow(duplicados_llaves) > 0L) {
  stop(
    "La base final contiene llaves duplicadas."
  )
}

if (!orden_llaves_conservado) {
  stop(
    "La base final modificó filas, orden o llaves."
  )
}

# ============================================================
# 3. Ejecutar nuevamente el diagnóstico consolidado
# ============================================================

dfs_osis <- em_original$dfs

# Reemplazar únicamente el capítulo K.
dfs_osis$K <- base_k_osis

diccionario_diagnostico_k <-
  insumos$diccionario_k |>
  dplyr::transmute(
    variable =
      stringr::str_to_upper(
        stringr::str_squish(
          as.character(.data$variable)
        )
      ),
    pregunta =
      as.character(.data$pregunta)
  ) |>
  dplyr::distinct(
    .data$variable,
    .keep_all = TRUE
  )

diag_osis <- diagnostico_flujo_capitulo_k(
  dfs = dfs_osis,
  diccionario = diccionario_diagnostico_k,
  variable_k23_final = "NPCKP17",
  detener_si_duplicados = TRUE,
  incluir_texto_libre = FALSE
)

resumen_diagnostico_k41_k42 <-
  diag_osis$resumen_variables |>
  dplyr::filter(
    .data$variable %in%
      variables_objetivo_flujo
  ) |>
  dplyr::arrange(
    match(
      .data$variable,
      variables_objetivo_flujo
    )
  )

variables_no_evaluadas <- setdiff(
  variables_objetivo_flujo,
  resumen_diagnostico_k41_k42$variable
)

if (length(variables_no_evaluadas) > 0L) {
  stop(
    "El diagnóstico no evaluó las variables: ",
    paste(variables_no_evaluadas, collapse = ", ")
  )
}

# ============================================================
# 4. Recuperar universos teóricos por persona y variable
# ============================================================

detalle_objetivo <-
  diag_osis$diagnostico_persona_variable |>
  dplyr::filter(
    .data$variable %in%
      variables_objetivo_flujo
  )

if (
  !"flujo_indeterminado" %in%
  names(detalle_objetivo)
) {
  detalle_objetivo$flujo_indeterminado <- FALSE
}

duplicados_detalle <- detalle_objetivo |>
  dplyr::count(
    dplyr::across(
      dplyr::all_of(
        c(
          llaves_persona,
          "variable"
        )
      )
    ),
    name = "n"
  ) |>
  dplyr::filter(
    .data$n > 1L
  )

if (nrow(duplicados_detalle) > 0L) {
  stop(
    "El diagnóstico produjo más de una fila por persona-variable."
  )
}

universos_objetivo <-
  detalle_objetivo |>
  dplyr::select(
    dplyr::all_of(llaves_persona),
    "variable",
    "debe_responder",
    "flujo_indeterminado"
  ) |>
  tidyr::pivot_wider(
    names_from = "variable",
    values_from = c(
      "debe_responder",
      "flujo_indeterminado"
    ),
    names_glue = "{.value}_{variable}"
  ) |>
  dplyr::mutate(
    # Las personas que el diagnóstico deja sin debe_responder
    # pertenecen al grupo de flujo indeterminado/no evaluable.
    flujo_indeterminado_NPCKP36 =
      dplyr::coalesce(
        .data$flujo_indeterminado_NPCKP36,
        FALSE
      ) |
      is.na(
        .data$debe_responder_NPCKP36
      ),

    flujo_indeterminado_NPCKP37 =
      dplyr::coalesce(
        .data$flujo_indeterminado_NPCKP37,
        FALSE
      ) |
      is.na(
        .data$debe_responder_NPCKP37
      ),

    # NPCKP36A es confirmación de NPCKP36 y hereda su universo.
    debe_responder_NPCKP36A =
      .data$debe_responder_NPCKP36,

    flujo_indeterminado_NPCKP36A =
      .data$flujo_indeterminado_NPCKP36
  )

base_revision <-
  base_k_osis |>
  dplyr::left_join(
    universos_objetivo,
    by = llaves_persona
  )

if (nrow(base_revision) != nrow(base_k_osis)) {
  stop(
    "La incorporación de universos alteró el número de filas."
  )
}

# Flujos indeterminados esperados: se comparan contra el diagnóstico original,
# no contra cero.
faltan_columnas_flujo_original <- setdiff(
  c(
    "variable",
    "flujos_indeterminados"
  ),
  names(insumos$resumen_variables)
)

if (length(faltan_columnas_flujo_original) > 0L) {
  stop(
    "El resumen original no contiene las columnas requeridas: ",
    paste(
      faltan_columnas_flujo_original,
      collapse = ", "
    )
  )
}

flujos_indeterminados_esperados <-
  insumos$resumen_variables |>
  dplyr::filter(
    .data$variable %in%
      variables_objetivo_flujo
  ) |>
  dplyr::transmute(
    variable =
      .data$variable,

    esperado =
      as.numeric(
        .data$flujos_indeterminados
      )
  )

flujos_indeterminados_esperados <-
  dplyr::bind_rows(
    flujos_indeterminados_esperados,

    flujos_indeterminados_esperados |>
      dplyr::filter(
        .data$variable == "NPCKP36"
      ) |>
      dplyr::mutate(
        variable = "NPCKP36A"
      )
  ) |>
  dplyr::arrange(
    match(
      .data$variable,
      variables_objetivo_final
    )
  )

if (
  nrow(flujos_indeterminados_esperados) !=
  length(variables_objetivo_final)
) {
  stop(
    "No se construyeron todos los valores esperados de flujo."
  )
}

# ============================================================
# 5. Funciones de normalización y dominios
# ============================================================

normalizar_texto <- function(x) {

  y <- stringr::str_squish(
    as.character(x)
  )

  y[
    is.na(y) |
      y == "" |
      stringr::str_to_upper(y) %in%
      c(
        "NA",
        "N/A",
        "NULL",
        "NULO"
      )
  ] <- NA_character_

  y
}

a_numero <- function(x) {
  suppressWarnings(
    as.numeric(
      normalizar_texto(x)
    )
  )
}

codigo_especial <- function(x) {

  numero <- a_numero(x)

  dplyr::case_when(
    numero == 98 ~ 98L,
    numero == 99 ~ 99L,
    TRUE ~ NA_integer_
  )
}

es_entero <- function(x) {
  !is.na(x) &
    is.finite(x) &
    abs(x - round(x)) < 0.00000001
}

# ============================================================
# 6. Evaluación semántica de las variables finales
# ============================================================

dep_npckp36 <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_revision$NPCKP36
  )

dep_npckp36a <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_revision$NPCKP36A
  )

dep_npckp36_original <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_revision$NPCKP36_original
  )

dep_npckp36a_original <-
  analisisem2025:::depurar_monto_capitulo_k(
    base_revision$NPCKP36A_original
  )

txt_npckp36 <- normalizar_texto(
  base_revision$NPCKP36
)

txt_npckp36a <- normalizar_texto(
  base_revision$NPCKP36A
)

txt_npckp37 <- normalizar_texto(
  base_revision$NPCKP37
)

num_npckp37 <- a_numero(
  base_revision$NPCKP37
)

num_npckp37_original <- a_numero(
  base_revision$NPCKP37_original
)

codigo_npckp36 <- codigo_especial(
  base_revision$NPCKP36
)

codigo_npckp36a <- codigo_especial(
  base_revision$NPCKP36A
)

codigo_npckp37 <- codigo_especial(
  base_revision$NPCKP37
)

codigo_npckp36_original <- codigo_especial(
  base_revision$NPCKP36_original
)

codigo_npckp36a_original <- codigo_especial(
  base_revision$NPCKP36A_original
)

mes_npckp37_valido <-
  es_entero(num_npckp37) &
  num_npckp37 >= 1 &
  num_npckp37 <= 12

mes_npckp37_original_valido <-
  es_entero(num_npckp37_original) &
  num_npckp37_original >= 1 &
  num_npckp37_original <= 12

npckp36_npckp36a_consistentes <-
  (
    !is.na(codigo_npckp36) &
      !is.na(codigo_npckp36a) &
      codigo_npckp36 ==
      codigo_npckp36a
  ) |
  (
    dep_npckp36$monto_valido %in% TRUE &
      dep_npckp36a$monto_valido %in% TRUE &
      abs(
        dep_npckp36$monto -
          dep_npckp36a$monto
      ) < 0.00000001
  )

npckp36_npckp37_consistentes <-
  dplyr::case_when(
    !is.na(codigo_npckp36) ~
      !is.na(codigo_npckp37) &
      codigo_npckp36 ==
      codigo_npckp37,

    dep_npckp36$monto_valido %in% TRUE ~
      mes_npckp37_valido,

    TRUE ~
      FALSE
  )

# ============================================================
# 7. Banderas de inconsistencia
# ============================================================

base_revision <- base_revision |>
  dplyr::mutate(
    universo_no_determinado_npckp36 =
      is.na(.data$debe_responder_NPCKP36) &
      !(
        .data$flujo_indeterminado_NPCKP36 %in%
          TRUE
      ),

    universo_no_determinado_npckp36a =
      is.na(.data$debe_responder_NPCKP36A) &
      !(
        .data$flujo_indeterminado_NPCKP36A %in%
          TRUE
      ),

    universo_no_determinado_npckp37 =
      is.na(.data$debe_responder_NPCKP37) &
      !(
        .data$flujo_indeterminado_NPCKP37 %in%
          TRUE
      ),

    vacio_npckp36 =
      .data$debe_responder_NPCKP36 %in% TRUE &
      is.na(txt_npckp36),

    vacio_npckp36a =
      .data$debe_responder_NPCKP36A %in% TRUE &
      is.na(txt_npckp36a),

    vacio_npckp37 =
      .data$debe_responder_NPCKP37 %in% TRUE &
      is.na(txt_npckp37),

    fuera_flujo_npckp36 =
      .data$debe_responder_NPCKP36 %in% FALSE &
      !is.na(txt_npckp36),

    fuera_flujo_npckp36a =
      .data$debe_responder_NPCKP36A %in% FALSE &
      !is.na(txt_npckp36a),

    fuera_flujo_npckp37 =
      .data$debe_responder_NPCKP37 %in% FALSE &
      !is.na(txt_npckp37),

    dominio_invalido_npckp36 =
      .data$debe_responder_NPCKP36 %in% TRUE &
      !(
        dep_npckp36$monto_valido %in% TRUE |
          !is.na(codigo_npckp36)
      ),

    dominio_invalido_npckp36a =
      .data$debe_responder_NPCKP36A %in% TRUE &
      !(
        dep_npckp36a$monto_valido %in% TRUE |
          !is.na(codigo_npckp36a)
      ),

    dominio_invalido_npckp37 =
      .data$debe_responder_NPCKP37 %in% TRUE &
      !(
        mes_npckp37_valido |
          !is.na(codigo_npckp37)
      ),

    inconsistencia_npckp36_npckp36a =
      .data$debe_responder_NPCKP36 %in% TRUE &
      !npckp36_npckp36a_consistentes,

    inconsistencia_npckp36_npckp37 =
      .data$debe_responder_NPCKP36 %in% TRUE &
      !npckp36_npckp37_consistentes,

    # Solo se protegen como errores los montos monetarios originales válidos.
    # Los códigos 98/99 reclasificados se auditan aparte.
    original_npckp36_modificado =
      dep_npckp36_original$monto_valido %in% TRUE &
      !(
        dep_npckp36$monto_valido %in% TRUE &
          abs(
            dep_npckp36_original$monto -
              dep_npckp36$monto
          ) < 0.00000001
      ),

    original_npckp36a_modificado =
      dep_npckp36a_original$monto_valido %in% TRUE &
      !(
        dep_npckp36a$monto_valido %in% TRUE &
          abs(
            dep_npckp36a_original$monto -
              dep_npckp36a$monto
          ) < 0.00000001
      ),

    original_npckp37_modificado =
      mes_npckp37_original_valido &
      (
        !mes_npckp37_valido |
          num_npckp37 !=
          num_npckp37_original
      )
  )

banderas_inconsistencia <- c(
  "universo_no_determinado_npckp36",
  "universo_no_determinado_npckp36a",
  "universo_no_determinado_npckp37",
  "vacio_npckp36",
  "vacio_npckp36a",
  "vacio_npckp37",
  "fuera_flujo_npckp36",
  "fuera_flujo_npckp36a",
  "fuera_flujo_npckp37",
  "dominio_invalido_npckp36",
  "dominio_invalido_npckp36a",
  "dominio_invalido_npckp37",
  "inconsistencia_npckp36_npckp36a",
  "inconsistencia_npckp36_npckp37",
  "original_npckp36_modificado",
  "original_npckp36a_modificado",
  "original_npckp37_modificado"
)

detalle_inconsistencias <-
  base_revision |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::all_of(
        banderas_inconsistencia
      ),
      ~ .x %in% TRUE
    )
  ) |>
  dplyr::select(
    dplyr::all_of(llaves_persona),
    dplyr::any_of(
      c(
        "edad",
        "NPCKP17",
        "NPCKP36_original",
        "NPCKP36",
        "NPCKP36A_original",
        "NPCKP36A",
        "NPCKP37_original",
        "NPCKP37",
        "NPCKP36_metodo_imputacion",
        "NPCKP37_metodo_imputacion",
        "NPCKP36_origen_codigo_residual",
        "NPCKP37_origen_codigo_residual"
      )
    ),
    dplyr::all_of(
      banderas_inconsistencia
    )
  )

# Auditoría informativa. No entra como criterio automático de rechazo.
codigos_originales_reclasificados <-
  tibble::tibble(
    control = c(
      "codigo_original_npckp36_reclasificado",
      "codigo_original_npckp36a_reclasificado"
    ),

    personas = c(
      sum(
        !is.na(codigo_npckp36_original) &
          (
            is.na(codigo_npckp36) |
              codigo_npckp36_original !=
              codigo_npckp36
          ),
        na.rm = TRUE
      ),

      sum(
        !is.na(codigo_npckp36a_original) &
          (
            is.na(codigo_npckp36a) |
              codigo_npckp36a_original !=
              codigo_npckp36a
          ),
        na.rm = TRUE
      )
    )
  )

# ============================================================
# 8. Resúmenes y controles de aceptación
# ============================================================

resumen_flujo_npckp36a <-
  base_revision |>
  dplyr::summarise(
    variable =
      "NPCKP36A",

    deben_responder =
      sum(
        .data$debe_responder_NPCKP36A %in% TRUE,
        na.rm = TRUE
      ),

    respondieron_cuando_debian =
      sum(
        .data$debe_responder_NPCKP36A %in% TRUE &
          !is.na(
            normalizar_texto(
              .data$NPCKP36A
            )
          ),
        na.rm = TRUE
      ),

    vacios_criticos =
      sum(
        .data$vacio_npckp36a %in% TRUE,
        na.rm = TRUE
      ),

    saltos_validos =
      sum(
        .data$debe_responder_NPCKP36A %in% FALSE &
          is.na(
            normalizar_texto(
              .data$NPCKP36A
            )
          ),
        na.rm = TRUE
      ),

    respuestas_fuera_flujo =
      sum(
        .data$fuera_flujo_npckp36a %in% TRUE,
        na.rm = TRUE
      ),

    flujos_indeterminados =
      sum(
        .data$flujo_indeterminado_NPCKP36A %in% TRUE,
        na.rm = TRUE
      )
  )

columnas_resumen_flujo <- c(
  "variable",
  "deben_responder",
  "respondieron_cuando_debian",
  "vacios_criticos",
  "saltos_validos",
  "respuestas_fuera_flujo",
  "flujos_indeterminados"
)

resumen_flujo_variables_finales <-
  dplyr::bind_rows(
    resumen_diagnostico_k41_k42 |>
      dplyr::select(
        dplyr::all_of(
          columnas_resumen_flujo
        )
      ),

    resumen_flujo_npckp36a
  ) |>
  dplyr::arrange(
    match(
      .data$variable,
      variables_objetivo_final
    )
  )

controles_flujos_indeterminados <-
  resumen_flujo_variables_finales |>
  dplyr::transmute(
    variable =
      .data$variable,

    observado =
      .data$flujos_indeterminados
  ) |>
  dplyr::left_join(
    flujos_indeterminados_esperados,
    by = "variable"
  ) |>
  dplyr::transmute(
    control =
      paste0(
        "flujos_indeterminados_diagnostico_",
        .data$variable
      ),

    observado =
      as.numeric(.data$observado),

    esperado =
      as.numeric(.data$esperado)
  )

controles_diagnostico <-
  dplyr::bind_rows(
    resumen_flujo_variables_finales |>
      dplyr::transmute(
        control =
          paste0(
            "vacios_criticos_diagnostico_",
            .data$variable
          ),

        observado =
          as.numeric(
            .data$vacios_criticos
          ),

        esperado = 0
      ),

    resumen_flujo_variables_finales |>
      dplyr::transmute(
        control =
          paste0(
            "respuestas_fuera_flujo_diagnostico_",
            .data$variable
          ),

        observado =
          as.numeric(
            .data$respuestas_fuera_flujo
          ),

        esperado = 0
      ),

    controles_flujos_indeterminados
  )

controles_semanticos <-
  base_revision |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        banderas_inconsistencia
      ),
      ~ sum(.x %in% TRUE),
      .names = "{.col}"
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "control",
    values_to = "observado"
  ) |>
  dplyr::mutate(
    esperado = 0
  )

# ============================================================
# 8.1. Balances sustantivos esperados del corte
# ============================================================

n_universo_esperado <- 36108L

conteos_esperados_k41_k42 <-
  tibble::tibble(
    control = c(
      "universo_npckp36",
      "universo_npckp36a",
      "universo_npckp37",

      "npckp36_montos_validos",
      "npckp36_codigo_98",
      "npckp36_codigo_99",

      "npckp36a_montos_validos",
      "npckp36a_codigo_98",
      "npckp36a_codigo_99",

      "npckp37_meses_validos",
      "npckp37_un_mes",
      "npckp37_meses_2_a_12",
      "npckp37_codigo_98",
      "npckp37_codigo_99",

      "npckp36_npckp36a_consistentes"
    ),

    observado = c(
      sum(
        base_revision$debe_responder_NPCKP36 %in% TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36A %in% TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP37 %in% TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36 %in% TRUE &
          dep_npckp36$monto_valido %in% TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36 %in% TRUE &
          codigo_npckp36 == 98L,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36 %in% TRUE &
          codigo_npckp36 == 99L,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36A %in% TRUE &
          dep_npckp36a$monto_valido %in% TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36A %in% TRUE &
          codigo_npckp36a == 98L,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36A %in% TRUE &
          codigo_npckp36a == 99L,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP37 %in% TRUE &
          mes_npckp37_valido
      ),

      sum(
        base_revision$debe_responder_NPCKP37 %in% TRUE &
          num_npckp37 == 1,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP37 %in% TRUE &
          num_npckp37 %in% 2:12,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP37 %in% TRUE &
          codigo_npckp37 == 98L,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP37 %in% TRUE &
          codigo_npckp37 == 99L,
        na.rm = TRUE
      ),

      sum(
        base_revision$debe_responder_NPCKP36 %in% TRUE &
          npckp36_npckp36a_consistentes
      )
    ),

    esperado = c(
      n_universo_esperado,
      n_universo_esperado,
      n_universo_esperado,

      33591L,
      316L,
      2201L,

      33591L,
      316L,
      2201L,

      33591L,
      33476L,
      115L,
      316L,
      2201L,

      n_universo_esperado
    )
  )

controles_estructura <-
  tibble::tibble(
    control = c(
      "filas_base_final",
      "llaves_duplicadas",
      "columnas_originales_ausentes",
      "variables_trazabilidad_ausentes",
      "orden_y_llaves_modificados",
      "copia_npckp36_original_difiere",
      "copia_npckp36a_original_difiere",
      "copia_npckp37_original_difiere",
      "inconsistencias_persona_final"
    ),

    observado = c(
      nrow(base_k_osis),
      nrow(duplicados_llaves),
      length(columnas_originales_ausentes),
      length(variables_trazabilidad_ausentes),
      as.integer(!orden_llaves_conservado),

      as.integer(
        !copias_originales_coinciden[
          "NPCKP36_original"
        ]
      ),

      as.integer(
        !copias_originales_coinciden[
          "NPCKP36A_original"
        ]
      ),

      as.integer(
        !copias_originales_coinciden[
          "NPCKP37_original"
        ]
      ),

      nrow(detalle_inconsistencias)
    ),

    esperado = c(
      nrow(base_k_original),
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0
    )
  )

prueba_aceptacion_osis <-
  dplyr::bind_rows(
    controles_estructura,
    controles_diagnostico,
    controles_semanticos,
    conteos_esperados_k41_k42
  ) |>
  dplyr::mutate(
    estado =
      dplyr::if_else(
        !is.na(.data$observado) &
          !is.na(.data$esperado) &
          .data$observado ==
          .data$esperado,
        "OK",
        "ERROR"
      )
  )

# ============================================================
# 9. Guardar resultados antes de detenerse
# ============================================================

ruta_rds_prueba <- file.path(
  carpeta_salida,
  paste0(
    "prueba_aceptacion_osis_k41_k42_",
    fecha_corte,
    ".rds"
  )
)

ruta_excel_prueba <- file.path(
  carpeta_salida,
  paste0(
    "prueba_aceptacion_osis_k41_k42_",
    fecha_corte,
    ".xlsx"
  )
)

saveRDS(
  list(
    prueba_aceptacion_osis =
      prueba_aceptacion_osis,

    resumen_diagnostico_k41_k42 =
      resumen_flujo_variables_finales,

    detalle_inconsistencias =
      detalle_inconsistencias,

    balance_k41_k42 =
      conteos_esperados_k41_k42,

    codigos_originales_reclasificados =
      codigos_originales_reclasificados,

    flujos_indeterminados_esperados =
      flujos_indeterminados_esperados,

    parametros = list(
      fecha_corte = fecha_corte,
      ruta_base_final = ruta_base_final,
      variables_evaluadas =
        variables_objetivo_final,
      filas_base_final =
        nrow(base_k_osis)
    )
  ),
  ruta_rds_prueba,
  compress = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb,
  "00_prueba_aceptacion"
)

openxlsx::writeData(
  wb,
  "00_prueba_aceptacion",
  prueba_aceptacion_osis,
  na.string = ""
)

openxlsx::addWorksheet(
  wb,
  "01_diagnostico_flujo"
)

openxlsx::writeData(
  wb,
  "01_diagnostico_flujo",
  resumen_flujo_variables_finales,
  na.string = ""
)

openxlsx::addWorksheet(
  wb,
  "02_inconsistencias"
)

openxlsx::writeData(
  wb,
  "02_inconsistencias",
  detalle_inconsistencias,
  na.string = ""
)

openxlsx::addWorksheet(
  wb,
  "03_balance_k41_k42"
)

openxlsx::writeData(
  wb,
  "03_balance_k41_k42",
  conteos_esperados_k41_k42,
  na.string = ""
)

openxlsx::addWorksheet(
  wb,
  "04_codigos_reclasificados"
)

openxlsx::writeData(
  wb,
  "04_codigos_reclasificados",
  codigos_originales_reclasificados,
  na.string = ""
)

openxlsx::addWorksheet(
  wb,
  "05_flujos_esperados"
)

openxlsx::writeData(
  wb,
  "05_flujos_esperados",
  flujos_indeterminados_esperados,
  na.string = ""
)

# Guardar una sola vez, después de crear todas las hojas.
openxlsx::saveWorkbook(
  wb,
  ruta_excel_prueba,
  overwrite = TRUE
)

# ============================================================
# 10. Resultado final
# ============================================================

cat(
  "\n============================================================\n",
  "PRUEBA DE ACEPTACIÓN BASE OSIS K41-K42\n",
  "============================================================\n"
)

print(
  prueba_aceptacion_osis,
  n = Inf,
  width = Inf
)

cat(
  "\nResumen del diagnóstico de flujo:\n"
)

print(
  resumen_flujo_variables_finales,
  n = Inf,
  width = Inf
)

cat(
  "\nCódigos originales reclasificados —control informativo—:\n"
)

print(
  codigos_originales_reclasificados,
  n = Inf,
  width = Inf
)

cat(
  "\nPersonas con alguna inconsistencia: ",
  nrow(detalle_inconsistencias),
  "\nRuta RDS: ",
  ruta_rds_prueba,
  "\nRuta Excel: ",
  ruta_excel_prueba,
  "\n",
  sep = ""
)

if (
  any(
    prueba_aceptacion_osis$estado != "OK",
    na.rm = TRUE
  ) ||
  anyNA(
    prueba_aceptacion_osis$estado
  )
) {
  stop(
    paste(
      "La base NO supera la prueba de aceptación para OSIS.",
      "Revise las hojas 00_prueba_aceptacion,",
      "02_inconsistencias y 03_balance_k41_k42."
    )
  )
}

cat(
  "\nRESULTADO: BASE APROBADA PARA ENTREGA A OSIS.\n"
)