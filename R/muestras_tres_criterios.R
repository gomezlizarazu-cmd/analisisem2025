#' Cruzar archivo de Muestras con el consolidado final de tres criterios
#'
#' Lee un archivo Excel de Muestras con listados de valores faltantes en
#' variables clave, identifica el nivel analítico afectado (vivienda, hogar o
#' persona) y cruza esos casos contra la salida de
#' `diagnostico_caidas_tres_criterios()` o, si se solicita, contra
#' `diagnostico_caidas_con_tematica()`.
#'
#' La comparación oficial se realiza contra `reporte_final_caidas`, que es el
#' consolidado final de encuestas caídas de la metodología y que además incluye
#' posibles duplicados de personas (`cae_duplicado`). De manera complementaria,
#' la función también puede devolver una lectura intermedia basada en
#' `viviendas_eval`, `hogares_eval` y `personas_eval` para entender cómo venía
#' cayendo cada caso antes de la consolidación final.
#'
#' La pregunta principal que responde esta función es:
#' “De los casos reportados por Muestras, ¿cuáles sí quedaron identificados en
#' el consolidado final `reporte_final_caidas` de la metodología?”
#'
#' La detección final se evalúa en dos niveles. Primero intenta un cruce
#' estricto contra una base derivada de `reporte_final_caidas` en el nivel
#' correspondiente: `reporte_vivienda`, `reporte_hogar` o `reporte_persona`.
#' Segundo, aplica un criterio adicional de detección por `DIRECTORIO` usando
#' una base resumida por encuesta construida desde `reporte_final_caidas`.
#'
#' En consecuencia, un caso puede considerarse detectado aunque no cruce al
#' nivel exacto reportado por Muestras, siempre que su `DIRECTORIO` esté
#' incluido en `reporte_final_caidas`. En esos casos, la función anexa al
#' detalle el diagnóstico resumido disponible para ese `DIRECTORIO`.
#'
#' Adicionalmente, la función valida si el `DIRECTORIO` reportado por Muestras
#' existe en la base actual `dfs`. Esto permite distinguir entre casos que no
#' fueron detectados porque ya no existen en la base actual y casos que sí
#' existen, pero no aparecen en `reporte_final_caidas`.
#'
#' El orden de evaluaciÃ³n es explÃ­cito: primero se verifica si el
#' `DIRECTORIO` existe en la base actual `dfs`; solo despuÃ©s se evalÃºa si
#' aparece o no en `reporte_final_caidas`.
#'
#' `match_estricto_nivel` refleja únicamente el join estricto por nivel y
#' `match_directorio_reporte_final` refleja la inclusión del `DIRECTORIO` en
#' `reporte_final_caidas`. `detectada_final` se define como la unión lógica de
#' ambos y `match_en_reporte_final` se conserva como alias de esa detección
#' final para facilitar trazabilidad. Ninguno depende de los valores de
#' `cae_existencia`, `cae_lina`, `cae_campo`, `cae_duplicado` ni de
#' `n_criterios_reporte`.
#'
#' @param archivo_muestras Ruta del archivo Excel enviado por Muestras.
#' @param diag_tres Resultado de `diagnostico_caidas_tres_criterios()`. Si es
#'   `NULL`, se calcula internamente a partir de `dfs` cuando
#'   `reporte_base = "tres_criterios"` o se toma desde
#'   `diag_con_tematica$diag_tres` cuando `reporte_base = "con_tematica"`.
#' @param dfs Lista nombrada de capítulos. Se usa solo cuando `diag_tres` es
#'   `NULL` o cuando se requiere apoyar el mapeo de llaves faltantes.
#' @param base_hogar_cap Capítulo base del universo hogar cuando `diag_tres` es
#'   `NULL`.
#' @param base_persona_cap Capítulo base del universo persona cuando `diag_tres`
#'   es `NULL`.
#' @param edad_var Variable de edad para `diagnostico_caidas_tres_criterios()`
#'   cuando `diag_tres` es `NULL`.
#' @param cap_hog Capítulo de hogar usado para apoyar el mapeo de
#'   `DIRECTORIO_HOG` hacia `SECUENCIA_P`. Por defecto `"C"`.
#' @param ruta_exportacion Ruta opcional para exportar el cruce a un archivo
#'   Excel. Si es `NULL`, no exporta. Si se suministra, genera un `.xlsx` con
#'   las hojas `resumen`, `detalle`, `detectadas`, `no_detectadas` y
#'   `reporte_final_match`, todas basadas en la comparación oficial contra
#'   `reporte_final_caidas`.
#' @param incluir_diag_tres Si es `TRUE`, incluye `diag_tres` dentro de la
#'   salida para facilitar trazabilidad.
#'
#' @return Una lista con:
#' \describe{
#'   \item{problemas_muestras}{Base unificada de casos reportados por Muestras,
#'   con nivel, variable reportada y llaves analíticas normalizadas.}
#'   \item{cruce_detalle}{Cruce caso a caso entre Muestras, `reporte_final_caidas`
#'   y las bases intermedias de `diagnostico_caidas_tres_criterios()`, con
#'   columnas separadas para cruce estricto por nivel y cruce flexible por
#'   `DIRECTORIO`.}
#'   \item{cruce_resumen}{Resumen por hoja/problema con detección final,
#'   detección intermedia y conteos por criterio.}
#'   \item{casos_detectados}{Casos reportados por Muestras que sí quedaron
#'   identificados en `reporte_final_caidas`.}
#'   \item{no_detectadas}{Casos reportados por Muestras que no quedaron
#'   identificados en `reporte_final_caidas`.}
#'   \item{archivo}{Ruta normalizada del Excel exportado cuando
#'   `ruta_exportacion` no es `NULL`.}
#'   \item{diag_tres}{Salida original de `diagnostico_caidas_tres_criterios()`
#'   cuando `incluir_diag_tres = TRUE`.}
#' }
#'
#' @examples
#' \dontrun{
#' diag_tres <- diagnostico_caidas_tres_criterios(dfs)
#' diag_con_tematica <- diagnostico_caidas_con_tematica(dfs)
#'
#' cruce <- diagnostico_muestras_vs_tres_criterios(
#'   archivo_muestras = "tablas_na.xlsx",
#'   diag_tres = diag_tres,
#'   diag_con_tematica = diag_con_tematica,
#'   reporte_base = "con_tematica",
#'   dfs = dfs,
#'   ruta_exportacion = "cruce_muestras_vs_tres_criterios.xlsx"
#' )
#'
#' cruce$cruce_resumen
#' cruce$no_detectadas
#' }
#'
#' @param diag_con_tematica Resultado de
#'   `diagnostico_caidas_con_tematica()`. Es obligatorio cuando
#'   `reporte_base = "con_tematica"`.
#' @param reporte_base Universo de comparacion oficial para
#'   `reporte_final_caidas`. Puede ser `"con_tematica"` o `"tres_criterios"`.
#'   Por defecto usa `"con_tematica"`.
#' @export
diagnostico_muestras_vs_tres_criterios <- function(
    archivo_muestras,
    diag_tres = NULL,
    diag_con_tematica = NULL,
    reporte_base = c("con_tematica", "tres_criterios"),
    dfs = NULL,
    base_hogar_cap = "C",
    base_persona_cap = "E",
    edad_var = "NPCEP4",
    cap_hog = "C",
    ruta_exportacion = NULL,
    incluir_diag_tres = FALSE
) {

  if (!is.character(archivo_muestras) ||
      length(archivo_muestras) != 1 ||
      is.na(archivo_muestras) ||
      !nzchar(archivo_muestras)) {
    stop("`archivo_muestras` debe ser una ruta válida a un archivo Excel.")
  }

  if (!file.exists(archivo_muestras)) {
    stop("No existe `archivo_muestras`: ", archivo_muestras)
  }

  reporte_base <- match.arg(reporte_base)

  if (identical(reporte_base, "con_tematica")) {
    if (!is.list(diag_con_tematica) ||
        !all(c(
          "viviendas_eval",
          "hogares_eval",
          "personas_eval",
          "reporte_final_caidas",
          "diag_tres"
        ) %in% names(diag_con_tematica))) {
      stop(
        "Cuando `reporte_base = \"con_tematica\"`, debe suministrar ",
        "`diag_con_tematica` como una lista producida por ",
        "`diagnostico_caidas_con_tematica()`."
      )
    }
  }

  if (is.null(diag_tres) && identical(reporte_base, "con_tematica")) {
    diag_tres <- diag_con_tematica$diag_tres
  }

  if (is.null(diag_tres)) {
    if (is.null(dfs) || !is.list(dfs) || length(dfs) == 0) {
      stop("Debe suministrar `diag_tres` o una lista `dfs` válida.")
    }

    diag_tres <- diagnostico_caidas_tres_criterios(
      dfs = dfs,
      base_hogar_cap = base_hogar_cap,
      base_persona_cap = base_persona_cap,
      edad_var = edad_var,
      exportar_excel = FALSE
    )
  }

  if (!is.list(diag_tres) ||
      !all(c(
        "viviendas_eval",
        "hogares_eval",
        "personas_eval",
        "reporte_final_caidas"
      ) %in% names(diag_tres))) {
    stop(
      "`diag_tres` debe ser una lista producida por ",
      "`diagnostico_caidas_tres_criterios()` con `reporte_final_caidas`."
    )
  }

  reporte_referencia <- if (identical(reporte_base, "con_tematica")) {
    diag_con_tematica$reporte_final_caidas
  } else {
    diag_tres$reporte_final_caidas
  }

  hojas <- readxl::excel_sheets(archivo_muestras)
  if (length(hojas) == 0) {
    stop("El archivo de Muestras no contiene hojas legibles.")
  }

  problemas_muestras <- lapply(hojas, function(hoja_i) {
    df_i <- readxl::read_excel(archivo_muestras, sheet = hoja_i) %>%
      tibble::as_tibble()

    .normalizar_hoja_muestras(
      df = df_i,
      hoja = hoja_i,
      dfs = dfs,
      cap_hog = cap_hog
    )
  }) %>%
    dplyr::bind_rows()

  problemas_muestras <- problemas_muestras %>%
    dplyr::mutate(
      DIRECTORIO = .normalizar_id_muestras(.data$DIRECTORIO),
      SECUENCIA_P = .normalizar_id_muestras(.data$SECUENCIA_P),
      ORDEN = .normalizar_id_muestras(.data$ORDEN),
      DIRECTORIO_HOG = .normalizar_id_muestras(.data$DIRECTORIO_HOG)
    )

  if (nrow(problemas_muestras) == 0) {
    out <- list(
      problemas_muestras = problemas_muestras,
      cruce_detalle = problemas_muestras,
      cruce_resumen = tibble::tibble(),
      casos_detectados = problemas_muestras,
      no_detectadas = problemas_muestras
    )

    if (isTRUE(incluir_diag_tres)) {
      out$diag_tres <- diag_tres
    }

    return(out)
  }

  # ---- detección final oficial: reporte_final_caidas ----
  reporte_persona <- reporte_referencia %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  if (!"observacion_final" %in% names(reporte_persona)) {
    reporte_persona$observacion_final <- NA_character_
  }
  if (!"observacion_resumen" %in% names(reporte_persona)) {
    reporte_persona$observacion_resumen <- NA_character_
  }
  if (!"observacion_duplicado" %in% names(reporte_persona)) {
    reporte_persona$observacion_duplicado <- NA_character_
  }

  reporte_persona <- reporte_persona %>%
    dplyr::mutate(
      DIRECTORIO = .normalizar_id_muestras(.data$DIRECTORIO),
      SECUENCIA_P = .normalizar_id_muestras(.data$SECUENCIA_P),
      ORDEN = .normalizar_id_muestras(.data$ORDEN),
      observacion_final = as.character(.data$observacion_final),
      observacion_resumen = as.character(.data$observacion_resumen),
      observacion_duplicado = as.character(.data$observacion_duplicado),
      match_en_reporte_final = TRUE
    )

  reporte_hogar <- reporte_persona %>%
    dplyr::group_by(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::summarise(
      match_en_reporte_final = TRUE,
      cae_existencia_final = any(.data$cae_existencia, na.rm = TRUE),
      cae_lina_final = any(.data$cae_lina, na.rm = TRUE),
      cae_campo_final = any(.data$cae_campo, na.rm = TRUE),
      cae_duplicado_final = any(.data$cae_duplicado, na.rm = TRUE),
      n_criterios_reporte = max(.data$n_criterios_reporte, na.rm = TRUE),
      criterios_reporte = .collapse_unique_nonempty(.data$criterios_reporte),
      criterio_principal_reporte = .collapse_unique_nonempty(.data$criterio_principal_reporte),
      razon_principal_caida = .collapse_unique_nonempty(.data$razon_principal_caida),
      variable_principal_caida = .collapse_unique_nonempty(.data$variable_principal_caida),
      valor_principal_caida = .collapse_unique_nonempty(.data$valor_principal_caida),
      observacion_final = .collapse_unique_nonempty(.data$observacion_final),
      observacion_resumen = .collapse_unique_nonempty(.data$observacion_resumen),
      observacion_duplicado = .collapse_unique_nonempty(.data$observacion_duplicado),
      .groups = "drop"
    )

  reporte_vivienda <- reporte_persona %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      match_en_reporte_final = TRUE,
      cae_existencia_final = any(.data$cae_existencia, na.rm = TRUE),
      cae_lina_final = any(.data$cae_lina, na.rm = TRUE),
      cae_campo_final = any(.data$cae_campo, na.rm = TRUE),
      cae_duplicado_final = any(.data$cae_duplicado, na.rm = TRUE),
      n_criterios_reporte = max(.data$n_criterios_reporte, na.rm = TRUE),
      criterios_reporte = .collapse_unique_nonempty(.data$criterios_reporte),
      criterio_principal_reporte = .collapse_unique_nonempty(.data$criterio_principal_reporte),
      razon_principal_caida = .collapse_unique_nonempty(.data$razon_principal_caida),
      variable_principal_caida = .collapse_unique_nonempty(.data$variable_principal_caida),
      valor_principal_caida = .collapse_unique_nonempty(.data$valor_principal_caida),
      observacion_final = .collapse_unique_nonempty(.data$observacion_final),
      observacion_resumen = .collapse_unique_nonempty(.data$observacion_resumen),
      observacion_duplicado = .collapse_unique_nonempty(.data$observacion_duplicado),
      .groups = "drop"
    )

  reporte_directorio <- reporte_persona %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      cae_existencia_dir = any(.data$cae_existencia, na.rm = TRUE),
      cae_lina_dir = any(.data$cae_lina, na.rm = TRUE),
      cae_campo_dir = any(.data$cae_campo, na.rm = TRUE),
      cae_duplicado_dir = any(.data$cae_duplicado, na.rm = TRUE),
      n_criterios_reporte_dir = max(.data$n_criterios_reporte, na.rm = TRUE),
      criterios_reporte_dir = .collapse_unique_nonempty(.data$criterios_reporte),
      criterio_principal_reporte_dir = .collapse_unique_nonempty(.data$criterio_principal_reporte),
      razon_principal_caida_dir = .collapse_unique_nonempty(.data$razon_principal_caida),
      variable_principal_caida_dir = .collapse_unique_nonempty(.data$variable_principal_caida),
      valor_principal_caida_dir = .collapse_unique_nonempty(.data$valor_principal_caida),
      observacion_final_dir = .collapse_unique_nonempty(.data$observacion_final),
      observacion_resumen_dir = .collapse_unique_nonempty(.data$observacion_resumen),
      observacion_duplicado_dir = .collapse_unique_nonempty(.data$observacion_duplicado),
      .groups = "drop"
    )

  directorios_reporte_final <- unique(
    stats::na.omit(
      .normalizar_id_muestras(reporte_referencia$DIRECTORIO)
    )
  )
  directorios_base_actual <- .extraer_directorios_base_actual(dfs)

  # ---- lectura intermedia analítica: *_eval ----
  viviendas_eval <- diag_tres$viviendas_eval %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$n_criterios_caida,
      .data$criterios_caida
    ) %>%
    dplyr::mutate(
      detectada_intermedia = .data$n_criterios_caida > 0
    )

  hogares_eval <- diag_tres$hogares_eval %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$n_criterios_caida,
      .data$criterios_caida
    ) %>%
    dplyr::mutate(
      detectada_intermedia = .data$n_criterios_caida > 0
    )

  personas_eval <- diag_tres$personas_eval %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::select(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$n_criterios_caida,
      .data$criterios_caida
    ) %>%
    dplyr::mutate(
      detectada_intermedia = .data$n_criterios_caida > 0
    )

  cruce_viv <- problemas_muestras %>%
    dplyr::filter(.data$nivel == "vivienda") %>%
    dplyr::left_join(
      reporte_vivienda,
      by = "DIRECTORIO"
    ) %>%
    dplyr::left_join(
      viviendas_eval %>%
        dplyr::rename(
          cae_existencia_intermedia = .data$cae_existencia,
          cae_lina_intermedia = .data$cae_lina,
          cae_campo_intermedia = .data$cae_campo,
          n_criterios_caida_intermedia = .data$n_criterios_caida,
          criterios_caida_intermedia = .data$criterios_caida
        ),
      by = "DIRECTORIO"
    )

  cruce_hog <- problemas_muestras %>%
    dplyr::filter(.data$nivel == "hogar") %>%
    dplyr::left_join(
      reporte_hogar,
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::left_join(
      hogares_eval %>%
        dplyr::rename(
          cae_existencia_intermedia = .data$cae_existencia,
          cae_lina_intermedia = .data$cae_lina,
          cae_campo_intermedia = .data$cae_campo,
          n_criterios_caida_intermedia = .data$n_criterios_caida,
          criterios_caida_intermedia = .data$criterios_caida
        ),
      by = c("DIRECTORIO", "SECUENCIA_P")
    )

  cruce_per <- problemas_muestras %>%
    dplyr::filter(.data$nivel == "persona") %>%
    dplyr::left_join(
      reporte_persona %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$SECUENCIA_P,
          .data$ORDEN,
          .data$match_en_reporte_final,
          .data$cae_existencia,
          .data$cae_lina,
          .data$cae_campo,
          .data$cae_duplicado,
          .data$n_criterios_reporte,
          .data$criterios_reporte,
          .data$criterio_principal_reporte,
          .data$razon_principal_caida,
          .data$variable_principal_caida,
          .data$valor_principal_caida,
          .data$observacion_final,
          .data$observacion_resumen,
          .data$observacion_duplicado
        ),
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::left_join(
      personas_eval %>%
        dplyr::rename(
          cae_existencia_intermedia = .data$cae_existencia,
          cae_lina_intermedia = .data$cae_lina,
          cae_campo_intermedia = .data$cae_campo,
          n_criterios_caida_intermedia = .data$n_criterios_caida,
          criterios_caida_intermedia = .data$criterios_caida
        ),
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    )

  cruce_detalle <- dplyr::bind_rows(cruce_viv, cruce_hog, cruce_per) %>%
    dplyr::left_join(
      reporte_directorio,
      by = "DIRECTORIO"
    )

  if (!"observacion_final" %in% names(cruce_detalle)) {
    cruce_detalle$observacion_final <- NA_character_
  }
  if (!"observacion_resumen" %in% names(cruce_detalle)) {
    cruce_detalle$observacion_resumen <- NA_character_
  }
  if (!"observacion_duplicado" %in% names(cruce_detalle)) {
    cruce_detalle$observacion_duplicado <- NA_character_
  }

  cruce_detalle <- cruce_detalle %>%
    dplyr::mutate(
      DIRECTORIO = .normalizar_id_muestras(.data$DIRECTORIO),
      SECUENCIA_P = .normalizar_id_muestras(.data$SECUENCIA_P),
      ORDEN = .normalizar_id_muestras(.data$ORDEN),
      directorio_existe_en_base_actual = .data$DIRECTORIO %in% directorios_base_actual,
      match_directorio_reporte_final = .data$DIRECTORIO %in% directorios_reporte_final,
      nivel_cruce_usado = dplyr::case_when(
        .data$nivel == "vivienda" ~ "reporte_vivienda",
        .data$nivel == "hogar" ~ "reporte_hogar",
        .data$nivel == "persona" ~ "reporte_persona",
        TRUE ~ "sin_nivel"
      ),
      llave_cruce = dplyr::case_when(
        .data$nivel == "vivienda" ~ paste0("DIRECTORIO=", .data$DIRECTORIO),
        .data$nivel == "hogar" ~ paste0("DIRECTORIO=", .data$DIRECTORIO, " | SECUENCIA_P=", dplyr::coalesce(.data$SECUENCIA_P, "NA")),
        .data$nivel == "persona" ~ paste0(
          "DIRECTORIO=", .data$DIRECTORIO,
          " | SECUENCIA_P=", dplyr::coalesce(.data$SECUENCIA_P, "NA"),
          " | ORDEN=", dplyr::coalesce(.data$ORDEN, "NA")
        ),
        TRUE ~ NA_character_
      ),
      match_estricto_nivel = dplyr::coalesce(.data$match_en_reporte_final, FALSE),
      match_flexible_directorio = .data$match_directorio_reporte_final,
      detectada_final = dplyr::if_else(
        !.data$directorio_existe_en_base_actual,
        TRUE,
        .data$match_estricto_nivel | .data$match_directorio_reporte_final
      ),
      match_en_reporte_final = .data$detectada_final,
      aparece_reporte_final = .data$detectada_final,
      detectada_intermedia = dplyr::coalesce(.data$detectada_intermedia, FALSE),
      causal_no_deteccion = dplyr::case_when(
        .data$detectada_final ~ "detectado_en_reporte_final",
        TRUE ~ "no_cruza_con_reporte_final_caidas"
      ),
      tipo_deteccion_final = dplyr::case_when(
        !.data$directorio_existe_en_base_actual ~ "directorio_no_existe_en_base_actual",
        .data$match_estricto_nivel ~ "match_nivel_reportado",
        !.data$match_estricto_nivel & .data$match_directorio_reporte_final ~ "match_directorio_reporte_final",
        TRUE ~ "sin_match"
      ),
      observacion_deteccion_final = dplyr::case_when(
        !.data$directorio_existe_en_base_actual ~
          "El DIRECTORIO reportado por Muestras no existe en la base actual dfs. La búsqueda en la base devuelve vacío. Posible diferencia entre versiones de base.",
        .data$detectada_final ~
          "Detectado por coincidencia de DIRECTORIO en reporte_final_caidas.",
        !.data$directorio_existe_en_base_actual ~
          paste0(
            "El DIRECTORIO reportado por Muestras no existe en la base actual dfs. ",
            "La búsqueda en la base devuelve vacío. Posible diferencia entre versiones de base."
          ),
        TRUE ~
          "El DIRECTORIO existe en la base actual, pero no aparece en reporte_final_caidas."
      ),
      origen_deteccion_final = dplyr::case_when(
        .data$tipo_deteccion_final == "match_nivel_reportado" &
          .data$nivel_cruce_usado == "reporte_vivienda" ~ "match_reporte_vivienda",
        .data$tipo_deteccion_final == "match_nivel_reportado" &
          .data$nivel_cruce_usado == "reporte_hogar" ~ "match_reporte_hogar",
        .data$tipo_deteccion_final == "match_nivel_reportado" &
          .data$nivel_cruce_usado == "reporte_persona" ~ "match_reporte_persona",
        .data$tipo_deteccion_final == "match_directorio_reporte_final" ~ "match_directorio_reporte_final",
        TRUE ~ "sin_match_reporte_final"
      ),
      cae_existencia = dplyr::coalesce(.data$cae_existencia, .data$cae_existencia_final, .data$cae_existencia_dir, FALSE),
      cae_lina = dplyr::coalesce(.data$cae_lina, .data$cae_lina_final, .data$cae_lina_dir, FALSE),
      cae_campo = dplyr::coalesce(.data$cae_campo, .data$cae_campo_final, .data$cae_campo_dir, FALSE),
      cae_duplicado = dplyr::coalesce(.data$cae_duplicado, .data$cae_duplicado_final, .data$cae_duplicado_dir, FALSE),
      n_criterios_reporte = dplyr::coalesce(.data$n_criterios_reporte, .data$n_criterios_reporte_dir, 0L),
      criterios_reporte = dplyr::coalesce(.data$criterios_reporte, .data$criterios_reporte_dir, "ninguno"),
      criterio_principal_reporte = dplyr::coalesce(.data$criterio_principal_reporte, .data$criterio_principal_reporte_dir, NA_character_),
      razon_principal_caida = dplyr::coalesce(.data$razon_principal_caida, .data$razon_principal_caida_dir, NA_character_),
      variable_principal_caida = dplyr::coalesce(.data$variable_principal_caida, .data$variable_principal_caida_dir, NA_character_),
      valor_principal_caida = dplyr::coalesce(.data$valor_principal_caida, .data$valor_principal_caida_dir, NA_character_),
      observacion_final = dplyr::coalesce(.data$observacion_final, .data$observacion_final_dir, NA_character_),
      observacion_resumen = dplyr::coalesce(.data$observacion_resumen, .data$observacion_resumen_dir, NA_character_),
      observacion_duplicado = dplyr::coalesce(.data$observacion_duplicado, .data$observacion_duplicado_dir, NA_character_),
      cae_existencia_intermedia = dplyr::coalesce(.data$cae_existencia_intermedia, FALSE),
      cae_lina_intermedia = dplyr::coalesce(.data$cae_lina_intermedia, FALSE),
      cae_campo_intermedia = dplyr::coalesce(.data$cae_campo_intermedia, FALSE),
      n_criterios_caida_intermedia = dplyr::coalesce(.data$n_criterios_caida_intermedia, 0L),
      criterios_caida_intermedia = dplyr::coalesce(.data$criterios_caida_intermedia, "ninguno")
    ) %>%
    dplyr::select(
      .data$hoja,
      .data$problema,
      .data$nivel,
      .data$variable_reportada,
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$DIRECTORIO_HOG,
      .data$UUID_muestras,
      .data$directorio_existe_en_base_actual,
      .data$nivel_cruce_usado,
      .data$llave_cruce,
      .data$match_estricto_nivel,
      .data$match_directorio_reporte_final,
      .data$match_flexible_directorio,
      .data$match_en_reporte_final,
      .data$causal_no_deteccion,
      .data$origen_deteccion_final,
      .data$tipo_deteccion_final,
      .data$observacion_deteccion_final,
      .data$detectada_final,
      .data$detectada_intermedia,
      .data$aparece_reporte_final,
      .data$cae_existencia,
      .data$cae_lina,
      .data$cae_campo,
      .data$cae_duplicado,
      .data$n_criterios_reporte,
      .data$criterios_reporte,
      .data$criterio_principal_reporte,
      .data$razon_principal_caida,
      .data$variable_principal_caida,
      .data$valor_principal_caida,
      .data$observacion_final,
      .data$observacion_resumen,
      .data$observacion_duplicado,
      .data$cae_existencia_intermedia,
      .data$cae_lina_intermedia,
      .data$cae_campo_intermedia,
      .data$n_criterios_caida_intermedia,
      .data$criterios_caida_intermedia,
      dplyr::everything()
    ) %>%
    dplyr::arrange(.data$hoja, .data$nivel, .data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  cruce_resumen <- cruce_detalle %>%
    dplyr::group_by(
      .data$hoja,
      .data$problema,
      .data$nivel,
      .data$variable_reportada
    ) %>%
    dplyr::summarise(
      casos_reportados = dplyr::n(),
      directorios_unicos = n_distinct_safe(dplyr::cur_data(), "DIRECTORIO"),
      hogares_unicos = n_distinct_safe(dplyr::cur_data(), c("DIRECTORIO", "SECUENCIA_P")),
      personas_unicas = n_distinct_safe(dplyr::cur_data(), c("DIRECTORIO", "SECUENCIA_P", "ORDEN")),
      matches_estrictos_nivel = sum(.data$match_estricto_nivel, na.rm = TRUE),
      matches_flexibles_directorio = sum(.data$match_directorio_reporte_final, na.rm = TRUE),
      detectadas_final = sum(.data$detectada_final, na.rm = TRUE),
      detectadas_intermedias = sum(.data$detectada_intermedia, na.rm = TRUE),
      no_detectadas_final = sum(!.data$detectada_final, na.rm = TRUE),
      detecta_existencia = sum(.data$cae_existencia, na.rm = TRUE),
      detecta_lina = sum(.data$cae_lina, na.rm = TRUE),
      detecta_campo = sum(.data$cae_campo, na.rm = TRUE),
      detecta_duplicado = sum(.data$cae_duplicado, na.rm = TRUE),
      pct_detectadas_final = dplyr::if_else(
        casos_reportados > 0,
        detectadas_final / casos_reportados,
        NA_real_
      ),
      pct_detectadas_intermedias = dplyr::if_else(
        casos_reportados > 0,
        detectadas_intermedias / casos_reportados,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$nivel, dplyr::desc(.data$casos_reportados), .data$hoja)

  casos_detectados <- cruce_detalle %>%
    dplyr::filter(.data$detectada_final)

  no_detectadas <- cruce_detalle %>%
    dplyr::filter(!.data$detectada_final)

  salida <- list(
    problemas_muestras = problemas_muestras,
    cruce_detalle = cruce_detalle,
    cruce_resumen = cruce_resumen,
    casos_detectados = casos_detectados,
    no_detectadas = no_detectadas
  )

  if (isTRUE(incluir_diag_tres)) {
    salida$diag_tres <- diag_tres
  }

  if (!is.null(ruta_exportacion)) {
    if (!is.character(ruta_exportacion) ||
        length(ruta_exportacion) != 1 ||
        is.na(ruta_exportacion) ||
        !nzchar(ruta_exportacion)) {
      stop("`ruta_exportacion` debe ser una ruta válida a un archivo `.xlsx`.")
    }

    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        "Se requiere el paquete `openxlsx` para exportar el cruce a Excel. ",
        "Instálelo con `install.packages('openxlsx')`."
      )
    }

    reporte_final_match <- cruce_detalle %>%
      dplyr::filter(.data$aparece_reporte_final)

    hojas_exportar <- list(
      resumen = cruce_resumen,
      detalle = cruce_detalle,
      detectadas = casos_detectados,
      no_detectadas = no_detectadas,
      reporte_final_match = reporte_final_match
    )

    wb <- openxlsx::createWorkbook()

    for (nm in names(hojas_exportar)) {
      x <- hojas_exportar[[nm]] %>%
        dplyr::mutate(
          dplyr::across(where(is.factor), as.character),
          dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
        ) %>%
        arreglar_utf8_df()

      openxlsx::addWorksheet(wb, substr(nm, 1, 31))
      openxlsx::writeData(wb, sheet = substr(nm, 1, 31), x = x)
    }

    openxlsx::saveWorkbook(wb, ruta_exportacion, overwrite = TRUE)

    salida$archivo <- normalizePath(
      ruta_exportacion,
      winslash = "/",
      mustWork = FALSE
    )
  }

  salida
}

.normalizar_hoja_muestras <- function(df, hoja, dfs = NULL, cap_hog = "C") {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  df <- normalize_keys(df, intersect(c("DIRECTORIO", "DIRECTORIO_HOG", "UUID"), names(df)))
  if (!"DIRECTORIO" %in% names(df)) df$DIRECTORIO <- NA_character_
  if (!"DIRECTORIO_HOG" %in% names(df)) df$DIRECTORIO_HOG <- NA_character_
  if (!"UUID" %in% names(df)) df$UUID <- NA_character_
  hoja_lower <- tolower(trimws(as.character(hoja)))

  nivel <- .inferir_nivel_muestras(hoja = hoja_lower, df = df)
  variable_reportada <- .inferir_variable_muestras(hoja = hoja_lower, df = df)
  problema <- .inferir_problema_muestras(hoja = hoja_lower, df = df)

  out <- df %>%
    dplyr::mutate(
      hoja = hoja,
      problema = problema,
      nivel = nivel,
      variable_reportada = variable_reportada,
      DIRECTORIO = as.character(.data$DIRECTORIO),
      DIRECTORIO_HOG = as.character(.data$DIRECTORIO_HOG),
      UUID_muestras = as.character(.data$UUID),
      SECUENCIA_P = NA_character_,
      ORDEN = NA_character_
    )

  if (nivel == "hogar") {
    out <- out %>%
      dplyr::mutate(
        SECUENCIA_P = .extraer_indice_uuid(.data$UUID_muestras, "Hogares")
      )

    if (any(is.na(out$SECUENCIA_P)) &&
        !is.null(dfs) &&
        is.list(dfs) &&
        toupper(cap_hog) %in% names(dfs) &&
        "DIRECTORIO_HOG" %in% names(dfs[[toupper(cap_hog)]]) &&
        "SECUENCIA_P" %in% names(dfs[[toupper(cap_hog)]])) {

      mapa_hog <- dfs[[toupper(cap_hog)]] %>%
        dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P, .data$DIRECTORIO_HOG) %>%
        normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "DIRECTORIO_HOG")) %>%
        dplyr::distinct()

      out <- out %>%
        dplyr::left_join(
          mapa_hog %>%
            dplyr::rename(SECUENCIA_P_mapa = .data$SECUENCIA_P),
          by = c("DIRECTORIO", "DIRECTORIO_HOG")
        ) %>%
        dplyr::mutate(
          SECUENCIA_P = dplyr::coalesce(.data$SECUENCIA_P, .data$SECUENCIA_P_mapa)
        ) %>%
        dplyr::select(-dplyr::any_of("SECUENCIA_P_mapa"))
    }
  }

  if (nivel == "persona") {
    out <- out %>%
      dplyr::mutate(
        SECUENCIA_P = .extraer_indice_uuid(.data$UUID_muestras, "Hogares"),
        ORDEN = .extraer_indice_uuid(.data$UUID_muestras, "Personas")
      )
  }

  out %>%
    dplyr::relocate(
      .data$hoja,
      .data$problema,
      .data$nivel,
      .data$variable_reportada,
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .data$DIRECTORIO_HOG,
      .data$UUID_muestras
    )
}

.inferir_nivel_muestras <- function(hoja, df) {
  mapa_hoja <- c(
    "tot_hog_na" = "vivienda",
    "estrato_na" = "vivienda",
    "pers_hog_na" = "hogar",
    "sexo_edad_na" = "persona"
  )

  if (hoja %in% names(mapa_hoja)) {
    return(unname(mapa_hoja[[hoja]]))
  }

  if (grepl("sexo|edad", hoja) || "edad" %in% names(df) || "sexo" %in% names(df)) {
    return("persona")
  }

  if (grepl("pers_hog", hoja) || "PERS_HOG" %in% names(df)) {
    return("hogar")
  }

  if ("DIRECTORIO_HOG" %in% names(df) && !"sexo" %in% names(df) && !"edad" %in% names(df)) {
    return("hogar")
  }

  "vivienda"
}

.inferir_variable_muestras <- function(hoja, df) {
  mapa_hoja <- c(
    "tot_hog_na" = "TOT_HOG",
    "estrato_na" = "estrato",
    "pers_hog_na" = "PERS_HOG",
    "sexo_edad_na" = "sexo + edad"
  )

  if (hoja %in% names(mapa_hoja)) {
    return(unname(mapa_hoja[[hoja]]))
  }

  cols_prob <- setdiff(
    names(df),
    c("DIRECTORIO", "DIRECTORIO_HOG", "UUID", "SEGMENTO")
  )

  if (length(cols_prob) == 0) {
    return(NA_character_)
  }

  paste(cols_prob, collapse = " + ")
}

.inferir_problema_muestras <- function(hoja, df) {
  mapa_hoja <- c(
    "tot_hog_na" = "valor_faltante_tot_hog",
    "estrato_na" = "valor_faltante_estrato",
    "pers_hog_na" = "valor_faltante_pers_hog",
    "sexo_edad_na" = "valor_faltante_sexo_y_edad"
  )

  if (hoja %in% names(mapa_hoja)) {
    return(unname(mapa_hoja[[hoja]]))
  }

  cols_prob <- setdiff(
    names(df),
    c("DIRECTORIO", "DIRECTORIO_HOG", "UUID", "SEGMENTO")
  )

  if (length(cols_prob) == 0) {
    return("problema_muestras")
  }

  paste0("faltante_", paste(cols_prob, collapse = "_"))
}

.extraer_indice_uuid <- function(x, nodo) {
  x <- as.character(x)
  patron <- paste0(".*", nodo, "\\[([0-9]+)\\].*")
  out <- stringr::str_match(x, patron)[, 2]
  out[is.na(out) | !nzchar(trimws(out))] <- NA_character_
  out
}

.normalizar_id_muestras <- function(x) {
  df_tmp <- tibble::tibble(valor = x) %>%
    normalize_keys("valor")

  x <- df_tmp$valor
  x <- stringr::str_replace_all(x, ",", "")
  x <- stringr::str_replace(x, "\\.0+$", "")
  x[x == "" | is.na(x)] <- NA_character_
  x
}

.collapse_unique_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x)) & x != "ninguno"]
  x <- unique(x)

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

.extraer_directorios_base_actual <- function(dfs) {
  if (is.null(dfs) || !is.list(dfs) || length(dfs) == 0) {
    return(character(0))
  }

  directorios <- lapply(dfs, function(df) {
    if (!is.data.frame(df) || !"DIRECTORIO" %in% names(df)) {
      return(character(0))
    }

    df_norm <- normalize_keys(df, "DIRECTORIO")
    out <- df_norm$DIRECTORIO
    out <- out[!is.na(out) & nzchar(out)]
    unique(out)
  })

  unique(unlist(directorios, use.names = FALSE))
}
