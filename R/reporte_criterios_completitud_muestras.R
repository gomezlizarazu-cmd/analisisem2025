#' Generar reporte de criterios de completitud para Muestras
#'
#' Construye tablas de trazabilidad de criterios de completitud y perdida a
#' partir de diagnosticos ya calculados del paquete. La funcion no recalcula
#' diagnosticos pesados: `diag_tres` debe suministrarse explicitamente y
#' `diag_con_tematica` se usa solo si ya esta disponible.
#'
#' La salida documenta la logica general de perdida/completitud, genera una
#' vista larga de encuesta-regla, una vista consolidada de encuesta/persona y
#' un detalle para los municipios solicitados. Cuando `exportar = TRUE`, escribe
#' los archivos Excel solicitados; cuando `exportar = FALSE`, no escribe
#' archivos y solo devuelve objetos en memoria.
#'
#' @param dfs Lista de capitulos de la Encuesta Multiproposito.
#' @param diag_tres Resultado de `diagnostico_caidas_tres_criterios()`.
#' @param diag_con_tematica Resultado opcional de
#'   `diagnostico_caidas_con_tematica()`. Si se suministra, se usa como reporte
#'   final ampliado; si no, se usa `diag_tres`.
#' @param dic_mpios Diccionario opcional de municipios con columnas `cod_mpio`
#'   y `municipio`.
#' @param municipios_objetivo Vector de nombres de municipios a detallar.
#' @param incluir_recuperables Si `TRUE`, intenta marcar casos recuperables
#'   usando `construir_sabana_casos_recuperables_desde_diagnostico()`. Como esa
#'   funcion existente exporta una sabana, solo se evalua cuando
#'   `diag_con_tematica` esta disponible y `exportar = TRUE`.
#' @param exportar Si `TRUE`, exporta los resultados a Excel.
#' @param ruta_salida Carpeta de salida. Obligatoria si `exportar = TRUE`.
#' @param reglas_tematica Reglas tematicas opcionales con columnas `capitulo`,
#'   `nivel`, `variable` y `descripcion`; se usan solo para documentar
#'   variables de la tabla de criterios.
#'
#' @return Lista con:
#' \describe{
#'   \item{criterios_general}{Tabla general de criterios de completitud/perdida.}
#'   \item{zipacon_soacha_detalle}{Detalle municipal para los municipios objetivo.}
#'   \item{zipacon_soacha_resumen}{Resumen municipal por etapa y clasificacion.}
#'   \item{resumen_final_niveles}{Resumen final de unidades evaluadas, caidas y no caidas por nivel.}
#'   \item{resumen_final_municipios_niveles}{Resumen final municipal por nivel para los municipios objetivo.}
#'   \item{tabla_variables_criterios}{Variables puras y derivadas que soportan cada criterio de caida.}
#'   \item{cascada_encuestas_muestras}{Cascada ejecutiva excluyente de caidas por ambito y nivel.}
#'   \item{comparacion_cascada_con_sin_tematica}{Comparacion de cascada con y sin reglas tematicas.}
#'   \item{vista_larga_encuesta_regla}{Vista larga, una fila por persona-regla.}
#'   \item{vista_consolidada_encuesta}{Vista consolidada, una fila por persona.}
#'   \item{metodologia_correo}{Texto breve para comunicar la metodologia.}
#'   \item{archivos_exportados}{Rutas de archivos exportados, o `NULL` si no se exporta.}
#' }
#'
#' @examples
#' \dontrun{
#' devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")
#'
#' salida <- generar_reporte_criterios_completitud_muestras(
#'   dfs = dfs,
#'   diag_tres = diag_tres,
#'   diag_con_tematica = diag_con_tematica,
#'   dic_mpios = dic_mpios,
#'   municipios_objetivo = c("Zipacon", "Zipacón", "Soacha"),
#'   exportar = TRUE,
#'   ruta_salida = "outputs/muestras_completitud"
#' )
#'
#' salida$criterios_general
#' salida$zipacon_soacha_detalle
#' salida$zipacon_soacha_resumen
#' salida$resumen_final_niveles
#' salida$resumen_final_municipios_niveles
#' salida$tabla_variables_criterios
#' salida$cascada_encuestas_muestras
#' salida$comparacion_cascada_con_sin_tematica
#' }
#'
#' @export
generar_reporte_criterios_completitud_muestras <- function(
    dfs,
    diag_tres,
    diag_con_tematica = NULL,
    dic_mpios = NULL,
    municipios_objetivo = c("Zipacon", "Zipacón", "Soacha"),
    incluir_recuperables = TRUE,
    exportar = FALSE,
    ruta_salida = NULL,
    reglas_tematica = NULL
) {
  .validar_reporte_criterios_args(
    dfs = dfs,
    diag_tres = diag_tres,
    diag_con_tematica = diag_con_tematica,
    exportar = exportar,
    ruta_salida = ruta_salida
  )

  diag_ref <- .resolver_diagnostico_reporte_criterios(
    diag_tres = diag_tres,
    diag_con_tematica = diag_con_tematica
  )

  reporte_final <- .agregar_recuperabilidad_reporte_criterios(
    reporte_final = diag_ref$reporte_final_caidas,
    diag_con_tematica = diag_con_tematica,
    dfs = dfs,
    incluir_recuperables = incluir_recuperables,
    ruta_salida = ruta_salida,
    permitir_escritura = exportar
  )

  municipios <- .mapa_municipios_reporte_criterios(
    dfs = dfs,
    dic_mpios = dic_mpios
  )

  criterios_general <- .criterios_general_reporte_muestras(
    dfs = dfs,
    diag_tres = diag_tres,
    diag_ref = diag_ref,
    reporte_final = reporte_final,
    incluye_tematica = !is.null(diag_con_tematica)
  )

  vistas <- .vistas_reporte_final_muestras(reporte_final)

  detalle_municipal <- .detalle_municipios_reporte_muestras(
    personas_eval = diag_ref$personas_eval,
    reporte_final = reporte_final,
    municipios = municipios,
    municipios_objetivo = municipios_objetivo
  )

  resumen_municipal <- .resumen_municipios_reporte_muestras(detalle_municipal)
  resumen_final <- .resumen_final_niveles_muestras(
    dfs = dfs,
    diag_ref = diag_ref,
    reporte_final = reporte_final,
    municipios = municipios,
    municipios_objetivo = municipios_objetivo
  )
  tabla_variables_criterios <- .tabla_variables_criterios_muestras(
    diag_con_tematica = diag_con_tematica,
    reglas_tematica = reglas_tematica,
    reporte_final = reporte_final
  )
  cascada_encuestas_muestras <- .cascada_encuestas_muestras(
    dfs = dfs,
    diag_ref = diag_ref,
    reporte_final = reporte_final,
    municipios = municipios,
    tabla_variables_criterios = tabla_variables_criterios
  )
  comparacion_cascada_con_sin_tematica <- .comparacion_cascada_con_sin_tematica_muestras(
    dfs = dfs,
    diag_tres = diag_tres,
    diag_ref = diag_ref,
    reporte_final_con_tematica = reporte_final,
    municipios = municipios,
    tabla_variables_criterios = tabla_variables_criterios
  )
  metodologia_correo <- .metodologia_correo_reporte_muestras()

  archivos_exportados <- NULL
  if (isTRUE(exportar)) {
    archivos_exportados <- .exportar_reporte_criterios_muestras(
      criterios_general = criterios_general,
      detalle_municipal = detalle_municipal,
      resumen_municipal = resumen_municipal,
      resumen_final_niveles = resumen_final$resumen_final_niveles,
      resumen_final_municipios_niveles = resumen_final$resumen_final_municipios_niveles,
      tabla_variables_criterios = tabla_variables_criterios,
      cascada_encuestas_muestras = cascada_encuestas_muestras,
      comparacion_cascada_con_sin_tematica = comparacion_cascada_con_sin_tematica,
      vista_larga = vistas$vista_larga_encuesta_regla,
      vista_consolidada = vistas$vista_consolidada_encuesta,
      metodologia_correo = metodologia_correo,
      ruta_salida = ruta_salida
    )
  }

  list(
    criterios_general = criterios_general,
    zipacon_soacha_detalle = detalle_municipal,
    zipacon_soacha_resumen = resumen_municipal,
    resumen_final_niveles = resumen_final$resumen_final_niveles,
    resumen_final_municipios_niveles = resumen_final$resumen_final_municipios_niveles,
    tabla_variables_criterios = tabla_variables_criterios,
    cascada_encuestas_muestras = cascada_encuestas_muestras,
    comparacion_cascada_con_sin_tematica = comparacion_cascada_con_sin_tematica,
    vista_larga_encuesta_regla = vistas$vista_larga_encuesta_regla,
    vista_consolidada_encuesta = vistas$vista_consolidada_encuesta,
    metodologia_correo = metodologia_correo,
    archivos_exportados = archivos_exportados
  )
}

#' Validar argumentos del reporte de criterios de Muestras
#'
#' Helper interno que revisa los objetos minimos de entrada sin ejecutar
#' diagnosticos ni exportaciones.
#'
#' @return `TRUE` de forma invisible si los argumentos son validos.
.validar_reporte_criterios_args <- function(dfs,
                                            diag_tres,
                                            diag_con_tematica,
                                            exportar,
                                            ruta_salida) {
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  req_tres <- c("viviendas_eval", "hogares_eval", "personas_eval", "reporte_final_caidas")
  faltan_tres <- setdiff(req_tres, names(diag_tres))
  if (!is.list(diag_tres) || length(faltan_tres) > 0) {
    stop(
      "`diag_tres` debe ser la salida de `diagnostico_caidas_tres_criterios()` ",
      "y contener: ",
      paste(req_tres, collapse = ", "),
      "."
    )
  }

  if (!is.null(diag_con_tematica)) {
    req_tem <- c("viviendas_eval", "hogares_eval", "personas_eval", "reporte_final_caidas")
    faltan_tem <- setdiff(req_tem, names(diag_con_tematica))
    if (!is.list(diag_con_tematica) || length(faltan_tem) > 0) {
      stop(
        "`diag_con_tematica` debe ser NULL o la salida de ",
        "`diagnostico_caidas_con_tematica()`."
      )
    }
  }

  if (isTRUE(exportar) && (is.null(ruta_salida) || !nzchar(as.character(ruta_salida)))) {
    stop("`ruta_salida` es obligatoria cuando `exportar = TRUE`.")
  }

  invisible(TRUE)
}

#' Resolver diagnostico de referencia del reporte de Muestras
#'
#' Helper interno que elige entre el diagnostico con tematica y el diagnostico
#' de tres criterios sin recalcular ninguno.
#'
#' @return Lista diagnostica usada como referencia del reporte.
.resolver_diagnostico_reporte_criterios <- function(diag_tres, diag_con_tematica = NULL) {
  if (!is.null(diag_con_tematica)) {
    out <- diag_con_tematica
    if (!"diag_tres" %in% names(out)) {
      out$diag_tres <- diag_tres
    }
    return(out)
  }

  list(
    viviendas_eval = diag_tres$viviendas_eval,
    hogares_eval = diag_tres$hogares_eval,
    personas_eval = diag_tres$personas_eval,
    reporte_final_caidas = diag_tres$reporte_final_caidas,
    diag_tres = diag_tres
  )
}

#' Agregar recuperabilidad al reporte final de criterios
#'
#' Helper interno que reutiliza la sabana de recuperables existente. Si no se
#' cuenta con diagnostico con tematica, no intenta clasificar recuperabilidad.
#'
#' @return `reporte_final_caidas` con columnas de recuperabilidad cuando aplica.
.agregar_recuperabilidad_reporte_criterios <- function(reporte_final,
                                                       diag_con_tematica,
                                                       dfs,
                                                       incluir_recuperables,
                                                       ruta_salida,
                                                       permitir_escritura = FALSE) {
  reporte_final <- normalize_keys(reporte_final, c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  if (!isTRUE(incluir_recuperables) || is.null(diag_con_tematica) || !isTRUE(permitir_escritura)) {
    reporte_final$recuperable_potencial <- NA
    reporte_final$estado_recuperacion <- NA_character_
    reporte_final$motivo_estado_recuperacion <- NA_character_
    return(reporte_final)
  }

  carpeta_tmp <- if (is.null(ruta_salida) || !nzchar(as.character(ruta_salida))) {
    tempdir()
  } else {
    ruta_salida
  }

  sabana <- tryCatch(
    construir_sabana_casos_recuperables_desde_diagnostico(
      diag_con_tematica = diag_con_tematica,
      dfs = dfs,
      carpeta_raiz = carpeta_tmp,
      archivo = "sabana_auditoria_casos_recuperables.xlsx"
    ),
    error = function(e) NULL
  )

  if (is.null(sabana) || !"casos_auditables_llave" %in% names(sabana)) {
    reporte_final$recuperable_potencial <- NA
    reporte_final$estado_recuperacion <- NA_character_
    reporte_final$motivo_estado_recuperacion <- NA_character_
    return(reporte_final)
  }

  rec <- sabana$casos_auditables_llave %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::select(
      DIRECTORIO,
      SECUENCIA_P,
      ORDEN,
      dplyr::any_of(c(
        "recuperable_potencial",
        "estado_recuperacion",
        "motivo_estado_recuperacion",
        "tipo_recuperacion"
      ))
    ) %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P, ORDEN, .keep_all = TRUE)

  reporte_final %>%
    dplyr::left_join(rec, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
}

#' Construir mapa de municipios para el reporte de criterios
#'
#' Helper interno que obtiene municipio desde el capitulo A y, si se entrega,
#' cruza el diccionario oficial de municipios.
#'
#' @return Tibble con `DIRECTORIO`, `codigo_municipio` y `municipio`.
.mapa_municipios_reporte_criterios <- function(dfs, dic_mpios = NULL) {
  if (!"A" %in% names(dfs) || !is.data.frame(dfs$A)) {
    return(tibble::tibble(
      DIRECTORIO = character(),
      codigo_municipio = character(),
      municipio = character()
    ))
  }

  A <- dfs$A %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::distinct(DIRECTORIO, .keep_all = TRUE)

  var_codigo <- col_first_existing(A, c("COD_MPIO", "Mpio", "MPIO", "Municipio", "Munic", "MUNICIPIO"))
  var_nombre <- col_first_existing(A, c("NomMunicipio", "NOM_MPIO", "NOMBRE_MUNICIPIO", "MUNICIPIO_NOMBRE"))

  mapa <- A %>%
    dplyr::transmute(
      DIRECTORIO = as.character(DIRECTORIO),
      codigo_municipio = if (!is.null(var_codigo)) .normalizar_codigo_mpio_reporte(.data[[var_codigo]]) else NA_character_,
      municipio_base = if (!is.null(var_nombre)) as.character(.data[[var_nombre]]) else NA_character_
    )

  if (!is.null(dic_mpios)) {
    if (!all(c("cod_mpio", "municipio") %in% names(dic_mpios))) {
      stop("`dic_mpios` debe contener `cod_mpio` y `municipio`.")
    }

    dic <- dic_mpios %>%
      dplyr::transmute(
        codigo_municipio = .normalizar_codigo_mpio_reporte(.data$cod_mpio),
        municipio_dic = as.character(.data$municipio)
      )

    mapa <- mapa %>% dplyr::left_join(dic, by = "codigo_municipio")
  } else {
    mapa$municipio_dic <- NA_character_
  }

  mapa %>%
    dplyr::mutate(
      municipio = dplyr::coalesce(.data$municipio_base, .data$municipio_dic)
    ) %>%
    dplyr::select(DIRECTORIO, codigo_municipio, municipio)
}

#' Normalizar codigo de municipio para el reporte
#'
#' Helper interno para comparar codigos que pueden venir numericos o textuales.
#'
#' @return Vector de caracteres normalizado.
.normalizar_codigo_mpio_reporte <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, ",", "")
  x <- stringr::str_replace(x, "\\.0+$", "")
  x[x == "" | is.na(x)] <- NA_character_
  x
}

#' Normalizar texto para filtros del reporte
#'
#' Helper interno para comparar nombres de municipios sin depender de tildes o
#' mayusculas.
#'
#' @return Vector de caracteres normalizado.
.normalizar_texto_reporte_muestras <- function(x) {
  x <- arreglar_utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  stringr::str_to_lower(stringr::str_squish(x))
}

#' Obtener llaves por nivel para el reporte de criterios
#'
#' Helper interno que centraliza las llaves validas por nivel.
#'
#' @return Vector de nombres de llave.
.llaves_nivel_reporte_muestras <- function(nivel) {
  switch(
    nivel,
    vivienda = c("DIRECTORIO"),
    hogar = c("DIRECTORIO", "SECUENCIA_P"),
    persona = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    stop("Nivel no soportado: ", nivel)
  )
}

#' Contar unidades por llave para el reporte de criterios
#'
#' Helper interno para contar llaves unicas presentes sin asumir que `nrow()`
#' representa la unidad analitica.
#'
#' @return Entero con el numero de unidades distintas.
.n_unidades_reporte_muestras <- function(df, nivel) {
  keys <- .llaves_nivel_reporte_muestras(nivel)

  if (!is.data.frame(df) || !all(keys %in% names(df))) {
    return(0L)
  }

  df %>%
    normalize_keys(keys) %>%
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(keys),
        ~ !is.na(.x) & nzchar(as.character(.x))
      )
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) %>%
    nrow()
}

#' Obtener llaves distintas por nivel
#'
#' Helper interno que normaliza y conserva solo las llaves validas del nivel.
#'
#' @return Tibble con llaves unicas del nivel solicitado.
.llaves_distintas_nivel_reporte_muestras <- function(df, nivel) {
  keys <- .llaves_nivel_reporte_muestras(nivel)

  if (!is.data.frame(df) || !all(keys %in% names(df))) {
    out <- tibble::tibble(.rows = 0)
    for (key in keys) out[[key]] <- character()
    return(out)
  }

  df %>%
    normalize_keys(keys) %>%
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(keys),
        ~ !is.na(.x) & nzchar(as.character(.x))
      )
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)))
}

#' Obtener universo evaluado por nivel
#'
#' Helper interno que prioriza la fuente evaluada metodologicamente indicada
#' para cada nivel sin usar `ORDEN` en vivienda ni hogar.
#'
#' @return Tibble con llaves unicas evaluadas.
.universo_nivel_resumen_muestras <- function(dfs, diag_ref, nivel) {
  candidatos <- switch(
    nivel,
    vivienda = list(dfs$A, diag_ref$viviendas_eval, diag_ref$diag_tres$viviendas_eval),
    hogar = list(diag_ref$hogares_eval, diag_ref$diag_tres$hogares_eval, dfs$C),
    persona = list(diag_ref$personas_eval, diag_ref$diag_tres$personas_eval, dfs$E),
    stop("Nivel no soportado: ", nivel)
  )

  keys <- .llaves_nivel_reporte_muestras(nivel)
  for (df in candidatos) {
    if (is.data.frame(df) && all(keys %in% names(df))) {
      return(.llaves_distintas_nivel_reporte_muestras(df, nivel))
    }
  }

  .llaves_distintas_nivel_reporte_muestras(NULL, nivel)
}

#' Obtener caidas finales por nivel
#'
#' Helper interno que cuenta caidas finales desde `reporte_final_caidas` usando
#' exclusivamente las llaves sustantivas de cada nivel.
#'
#' @return Tibble con llaves unicas caidas.
.caidas_nivel_resumen_muestras <- function(reporte_final, nivel) {
  .llaves_distintas_nivel_reporte_muestras(reporte_final, nivel)
}

#' Construir fila de resumen final por nivel
#'
#' @return Tibble de una fila con totales, caidas y porcentajes.
.fila_resumen_final_nivel_muestras <- function(universo, caidas, nivel) {
  keys <- .llaves_nivel_reporte_muestras(nivel)
  llave_nivel <- paste(keys, collapse = " + ")
  caidas_eval <- caidas %>% dplyr::semi_join(universo, by = keys)

  unidades_totales <- nrow(universo)
  unidades_caidas <- nrow(caidas_eval)
  unidades_sin_caida <- unidades_totales - unidades_caidas

  tibble::tibble(
    nivel = nivel,
    llave_nivel = llave_nivel,
    unidades_totales = unidades_totales,
    unidades_sin_caida = unidades_sin_caida,
    unidades_caidas = unidades_caidas,
    porcentaje_caida = if (unidades_totales > 0) round(100 * unidades_caidas / unidades_totales, 2) else NA_real_,
    porcentaje_sin_caida = if (unidades_totales > 0) round(100 * unidades_sin_caida / unidades_totales, 2) else NA_real_
  )
}

#' Construir resumen final municipal por nivel
#'
#' @return Tibble con totales, caidas y porcentajes por municipio y nivel.
.resumen_final_municipal_nivel_muestras <- function(universo,
                                                    caidas,
                                                    municipios,
                                                    municipios_objetivo,
                                                    nivel) {
  keys <- .llaves_nivel_reporte_muestras(nivel)
  llave_nivel <- paste(keys, collapse = " + ")
  objetivo_norm <- .normalizar_texto_reporte_muestras(municipios_objetivo)

  if (!is.data.frame(municipios) || !"DIRECTORIO" %in% names(municipios)) {
    return(tibble::tibble(
      municipio = character(),
      codigo_municipio = character(),
      nivel = character(),
      llave_nivel = character(),
      unidades_totales = integer(),
      unidades_sin_caida = integer(),
      unidades_caidas = integer(),
      porcentaje_caida = numeric(),
      porcentaje_sin_caida = numeric()
    ))
  }

  municipios_unicos <- municipios %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)

  base <- universo %>%
    dplyr::left_join(municipios_unicos, by = "DIRECTORIO") %>%
    dplyr::filter(.normalizar_texto_reporte_muestras(.data$municipio) %in% objetivo_norm)

  if (nrow(base) == 0) {
    return(tibble::tibble(
      municipio = character(),
      codigo_municipio = character(),
      nivel = character(),
      llave_nivel = character(),
      unidades_totales = integer(),
      unidades_sin_caida = integer(),
      unidades_caidas = integer(),
      porcentaje_caida = numeric(),
      porcentaje_sin_caida = numeric()
    ))
  }

  marcador_caidas <- caidas %>%
    dplyr::semi_join(base, by = keys) %>%
    dplyr::mutate(.cae_final = TRUE)

  base %>%
    dplyr::left_join(marcador_caidas, by = keys) %>%
    dplyr::mutate(.cae_final = dplyr::coalesce(.data$.cae_final, FALSE)) %>%
    dplyr::group_by(.data$municipio, .data$codigo_municipio) %>%
    dplyr::summarise(
      unidades_totales = dplyr::n(),
      unidades_caidas = sum(.data$.cae_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      nivel = nivel,
      llave_nivel = llave_nivel,
      unidades_sin_caida = .data$unidades_totales - .data$unidades_caidas,
      porcentaje_caida = dplyr::if_else(
        .data$unidades_totales > 0,
        round(100 * .data$unidades_caidas / .data$unidades_totales, 2),
        NA_real_
      ),
      porcentaje_sin_caida = dplyr::if_else(
        .data$unidades_totales > 0,
        round(100 * .data$unidades_sin_caida / .data$unidades_totales, 2),
        NA_real_
      )
    ) %>%
    dplyr::select(
      municipio,
      codigo_municipio,
      nivel,
      llave_nivel,
      unidades_totales,
      unidades_sin_caida,
      unidades_caidas,
      porcentaje_caida,
      porcentaje_sin_caida
    ) %>%
    dplyr::arrange(.data$municipio, .data$nivel)
}

#' Resumir caidas finales por nivel y municipio
#'
#' Helper interno que usa `reporte_final_caidas` ya resuelto como fuente de
#' clasificacion final. No recalcula diagnosticos.
#'
#' @return Lista con resumen general por nivel y resumen municipal por nivel.
.resumen_final_niveles_muestras <- function(dfs,
                                            diag_ref,
                                            reporte_final,
                                            municipios,
                                            municipios_objetivo) {
  niveles <- c("vivienda", "hogar", "persona")
  universos <- stats::setNames(
    lapply(niveles, function(nivel) .universo_nivel_resumen_muestras(dfs, diag_ref, nivel)),
    niveles
  )
  caidas <- stats::setNames(
    lapply(niveles, function(nivel) .caidas_nivel_resumen_muestras(reporte_final, nivel)),
    niveles
  )

  resumen_final_niveles <- dplyr::bind_rows(lapply(niveles, function(nivel) {
    .fila_resumen_final_nivel_muestras(
      universo = universos[[nivel]],
      caidas = caidas[[nivel]],
      nivel = nivel
    )
  }))

  resumen_final_municipios_niveles <- dplyr::bind_rows(lapply(niveles, function(nivel) {
    .resumen_final_municipal_nivel_muestras(
      universo = universos[[nivel]],
      caidas = caidas[[nivel]],
      municipios = municipios,
      municipios_objetivo = municipios_objetivo,
      nivel = nivel
    )
  }))

  list(
    resumen_final_niveles = resumen_final_niveles,
    resumen_final_municipios_niveles = resumen_final_municipios_niveles
  )
}

#' Coaccionar bandera de caida para el reporte
#'
#' Helper interno que interpreta columnas logicas, numericas o textuales como
#' banderas booleanas.
#'
#' @return Vector logico.
.flag_reporte_muestras <- function(x) {
  if (is.logical(x)) {
    return(dplyr::coalesce(x, FALSE))
  }
  if (is.numeric(x) || is.integer(x)) {
    return(dplyr::coalesce(x != 0, FALSE))
  }
  x_norm <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::coalesce(x_norm %in% c("true", "t", "1", "si", "sí", "yes"), FALSE)
}

#' Resumir valores observados para una fila metodologica
#'
#' Helper interno que muestra valores observados de variables relevantes sin
#' leer diccionarios externos.
#'
#' @return Cadena de texto con valores observados.
.valores_observados_reporte_muestras <- function(df, vars, max_valores = 10L) {
  vars <- intersect(vars, names(df))

  if (!is.data.frame(df) || length(vars) == 0) {
    return("No inferido desde el objeto evaluado.")
  }

  partes <- lapply(vars, function(v) {
    x <- df[[v]]
    if (is.logical(x)) {
      return(paste0(v, ": TRUE/FALSE"))
    }

    vals <- sort(unique(stats::na.omit(as.character(x))))
    vals <- vals[nzchar(stringr::str_squish(vals))]
    if (length(vals) == 0) {
      return(paste0(v, ": sin valores observados no vacios"))
    }
    if (length(vals) > max_valores) {
      vals <- c(utils::head(vals, max_valores), paste0("... (", length(vals), " valores observados)"))
    }
    paste0(v, ": ", paste(vals, collapse = " | "))
  })

  paste(unlist(partes), collapse = "; ")
}

#' Construir una fila de criterio para el reporte general
#'
#' Helper interno para resumir una regla de caida a partir de una bandera.
#'
#' @return Tibble de una fila.
.fila_criterio_reporte_muestras <- function(df,
                                            nivel,
                                            etapa_proceso,
                                            script_funcion,
                                            variable_usada,
                                            flag,
                                            cae_si = TRUE,
                                            regla_o_validacion,
                                            observacion_tecnica,
                                            vars_valores = character()) {
  n_eval <- .n_unidades_reporte_muestras(df, nivel)

  if (!is.data.frame(df) || !flag %in% names(df)) {
    n_caen <- NA_integer_
    n_pasan <- NA_integer_
  } else {
    df_flag <- df %>% dplyr::mutate(.flag_reporte = .flag_reporte_muestras(.data[[flag]]))
    caidas <- if (isTRUE(cae_si)) {
      df_flag %>% dplyr::filter(.data$.flag_reporte)
    } else {
      df_flag %>% dplyr::filter(!.data$.flag_reporte)
    }
    n_caen <- .n_unidades_reporte_muestras(caidas, nivel)
    n_pasan <- n_eval - n_caen
  }

  tibble::tibble(
    etapa_proceso = etapa_proceso,
    script_funcion = script_funcion,
    variable_usada = variable_usada,
    valores_posibles = .valores_observados_reporte_muestras(df, unique(c(flag, vars_valores))),
    regla_o_validacion = regla_o_validacion,
    nivel_llave = paste0(nivel, ": ", paste(.llaves_nivel_reporte_muestras(nivel), collapse = " + ")),
    n_encuestas_evaluadas = n_eval,
    n_encuestas_caen = n_caen,
    n_encuestas_pasan = n_pasan,
    observacion_tecnica = observacion_tecnica
  )
}

#' Construir tabla general de criterios para Muestras
#'
#' Helper interno que arma el cuadro metodologico general usando diagnosticos
#' ya calculados.
#'
#' @return Tibble con los criterios generales.
.criterios_general_reporte_muestras <- function(dfs,
                                                diag_tres,
                                                diag_ref,
                                                reporte_final,
                                                incluye_tematica) {
  filas <- list(
    .fila_criterio_reporte_muestras(
      df = diag_tres$diag_existencia$viviendas_eval,
      nivel = "vivienda",
      etapa_proceso = "validacion de existencia entre capitulos - vivienda",
      script_funcion = "R/diagnostico_cruce_capitulos.R::diagnostico_cruce_capitulos()",
      variable_usada = "pres_*, req_*, ok_*, n_caps_faltantes, vivienda_completa",
      flag = "vivienda_completa",
      cae_si = FALSE,
      regla_o_validacion = "La vivienda cae cuando `vivienda_completa` es FALSE, equivalente a `n_caps_faltantes > 0`.",
      observacion_tecnica = "B se trata bajo su cobertura especial; no se debe exigir B a todos los hogares.",
      vars_valores = c("n_caps_faltantes", "capitulos_faltantes")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$diag_existencia$hogares_eval,
      nivel = "hogar",
      etapa_proceso = "validacion de existencia entre capitulos - hogar",
      script_funcion = "R/diagnostico_cruce_capitulos.R::diagnostico_cruce_capitulos()",
      variable_usada = "pres_*, req_*, ok_*, n_caps_faltantes, hogar_completo",
      flag = "hogar_completo",
      cae_si = FALSE,
      regla_o_validacion = "El hogar cae cuando `hogar_completo` es FALSE, equivalente a tener capitulos requeridos faltantes.",
      observacion_tecnica = "Para B, `req_B = 0` a nivel hogar por regla especial de cobertura.",
      vars_valores = c("n_caps_faltantes", "capitulos_faltantes")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$diag_existencia$personas_eval,
      nivel = "persona",
      etapa_proceso = "validacion de existencia entre capitulos - persona",
      script_funcion = "R/diagnostico_cruce_capitulos.R::diagnostico_cruce_capitulos()",
      variable_usada = "edad, pres_*, req_*, ok_*, n_caps_faltantes, persona_completa",
      flag = "persona_completa",
      cae_si = FALSE,
      regla_o_validacion = "La persona cae cuando `persona_completa` es FALSE; E y F son requeridos, y G-K dependen de edad.",
      observacion_tecnica = "Solo aqui `ORDEN` es llave sustantiva de persona.",
      vars_valores = c("edad", "n_caps_faltantes", "capitulos_faltantes")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$viviendas_eval,
      nivel = "vivienda",
      etapa_proceso = "criterio operativo de campo",
      script_funcion = "R/completitud_campo.R::clasificar_completitud_campo() / diagnostico_completitud_campo()",
      variable_usada = "encuesta_efectiva_campo, encuesta_completa_campo, cae_campo_base",
      flag = "cae_campo_base",
      cae_si = TRUE,
      regla_o_validacion = "La vivienda cae por campo base cuando es efectiva y no cumple `encuesta_completa_campo`.",
      observacion_tecnica = "La caida de campo base se consolida a nivel DIRECTORIO.",
      vars_valores = c("motivo_principal_campo_base", "motivo_detallado_campo_base", "criterio_falla_campo_base")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$hogares_eval,
      nivel = "hogar",
      etapa_proceso = "criterio de campo por conteo de personas del hogar",
      script_funcion = "R/completitud_tres_criterios.R::.preparar_causal_conteo_personas_hogar()",
      variable_usada = "NHCCPCTRL2, n_personas_cap_e, diferencia_personas_hogar, cae_campo_nhccpctrl2",
      flag = "cae_campo_nhccpctrl2",
      cae_si = TRUE,
      regla_o_validacion = "El hogar cae por campo si `NHCCPCTRL2` no coincide con personas observadas en E.",
      observacion_tecnica = "Regla adicional de tres criterios; compara C contra E usando llave de hogar.",
      vars_valores = c("NHCCPCTRL2", "n_personas_cap_e", "diferencia_personas_hogar", "criterio_falla_campo_nhccpctrl2")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$viviendas_eval,
      nivel = "vivienda",
      etapa_proceso = "criterio Lina - vivienda",
      script_funcion = "R/completitud_lina.R::diagnostico_completitud_lina()",
      variable_usada = "NVCAPCTRL1, NVCAPCTRL2, NVCBP1, NVCBP16, cae_lina",
      flag = "cae_lina",
      cae_si = TRUE,
      regla_o_validacion = "La vivienda cae por Lina si no cumple controles de A o variables requeridas de B.",
      observacion_tecnica = "B se usa a nivel DIRECTORIO; no usar ORDEN aunque exista en algun archivo.",
      vars_valores = c("razon_lina")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$hogares_eval,
      nivel = "hogar",
      etapa_proceso = "criterio Lina - hogar",
      script_funcion = "R/completitud_lina.R::diagnostico_completitud_lina()",
      variable_usada = "NHCCPCTRL1, NHCCP1, NHCMP1A, NHCMP5A, cae_lina",
      flag = "cae_lina",
      cae_si = TRUE,
      regla_o_validacion = "El hogar cae por Lina si falla control de C o variables requeridas de C/MA.",
      observacion_tecnica = "La llave valida es DIRECTORIO + SECUENCIA_P.",
      vars_valores = c("razon_lina")
    ),
    .fila_criterio_reporte_muestras(
      df = diag_tres$personas_eval,
      nivel = "persona",
      etapa_proceso = "criterio Lina - persona",
      script_funcion = "R/completitud_lina.R::diagnostico_completitud_lina()",
      variable_usada = "NPCEPCTRL1, NPCEP6, cae_lina",
      flag = "cae_lina",
      cae_si = TRUE,
      regla_o_validacion = "La persona cae por Lina si falla control de E o `NPCEP6` esta vacia.",
      observacion_tecnica = "Este criterio opera a nivel persona y justifica el uso de ORDEN.",
      vars_valores = c("razon_lina")
    )
  )

  if (isTRUE(incluye_tematica)) {
    filas <- c(filas, list(.fila_criterio_reporte_muestras(
      df = diag_ref$personas_eval,
      nivel = "persona",
      etapa_proceso = "incompletitud tematica",
      script_funcion = "R/incompletitud_tematica.R::diagnostico_caidas_con_tematica()",
      variable_usada = "cae_tematica, razon_tematica, variable_tematica, valor_tematica",
      flag = "cae_tematica",
      cae_si = TRUE,
      regla_o_validacion = "La persona cae por incompletitud tematica cuando una regla configurada marca `cae_tematica = TRUE`.",
      observacion_tecnica = "Criterio adicional; no reemplaza tres criterios.",
      vars_valores = c("razon_tematica", "variable_tematica", "valor_tematica")
    )))
  }

  filas <- c(
    filas,
    list(
      .fila_reporte_final_muestras(diag_ref$personas_eval, reporte_final),
      .fila_base_completa_muestras(dfs, reporte_final)
    )
  )

  dplyr::bind_rows(filas)
}

#' Construir fila de reporte final consolidado
#'
#' Helper interno para documentar la regla `n_criterios_reporte > 0`.
#'
#' @return Tibble de una fila.
.fila_reporte_final_muestras <- function(personas_eval, reporte_final) {
  n_eval <- .n_unidades_reporte_muestras(personas_eval, "persona")
  n_caen <- .n_unidades_reporte_muestras(reporte_final, "persona")

  tibble::tibble(
    etapa_proceso = "reporte final de caidas",
    script_funcion = "R/completitud_tres_criterios.R / R/incompletitud_tematica.R",
    variable_usada = "cae_existencia, cae_lina, cae_campo, cae_duplicado, cae_tematica, n_criterios_reporte",
    valores_posibles = .valores_observados_reporte_muestras(
      reporte_final,
      c("criterios_reporte", "criterio_principal_reporte", "razon_principal_caida", "variable_principal_caida")
    ),
    regla_o_validacion = "Una unidad entra al reporte final si `n_criterios_reporte > 0`.",
    nivel_llave = "persona: DIRECTORIO + SECUENCIA_P + ORDEN",
    n_encuestas_evaluadas = n_eval,
    n_encuestas_caen = n_caen,
    n_encuestas_pasan = n_eval - n_caen,
    observacion_tecnica = "El criterio principal usa la prioridad calculada por el paquete; este reporte no la redefine."
  )
}

#' Construir fila de base EM completa
#'
#' Helper interno para documentar la exclusion por `DIRECTORIO` en
#' `construir_base_em_completa()`.
#'
#' @return Tibble de una fila.
.fila_base_completa_muestras <- function(dfs, reporte_final) {
  directorios <- dplyr::bind_rows(lapply(dfs, function(df) {
    if (!is.data.frame(df) || !"DIRECTORIO" %in% names(df)) {
      return(tibble::tibble(DIRECTORIO = character()))
    }
    df %>%
      normalize_keys("DIRECTORIO") %>%
      dplyr::select(DIRECTORIO)
  })) %>%
    dplyr::filter(!is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO)) %>%
    dplyr::distinct(.data$DIRECTORIO)

  flags <- intersect(
    c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica"),
    names(reporte_final)
  )

  directorios_caen <- reporte_final %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(flags), .flag_reporte_muestras)) %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(flags), identity)) %>%
    dplyr::distinct(.data$DIRECTORIO)

  n_eval <- nrow(directorios)
  n_caen <- nrow(directorios_caen)

  tibble::tibble(
    etapa_proceso = "construccion de base EM completa",
    script_funcion = "R/em_completa.R::construir_base_em_completa()",
    variable_usada = paste(flags, collapse = ", "),
    valores_posibles = .valores_observados_reporte_muestras(reporte_final, c("criterios_reporte", "criterio_principal_reporte")),
    regla_o_validacion = "Un DIRECTORIO se excluye de la base completa si presenta al menos una caida en `reporte_final_caidas`.",
    nivel_llave = "vivienda: DIRECTORIO",
    n_encuestas_evaluadas = n_eval,
    n_encuestas_caen = n_caen,
    n_encuestas_pasan = n_eval - n_caen,
    observacion_tecnica = "La exclusion se resume a nivel DIRECTORIO y luego filtra todos los capitulos."
  )
}

#' Construir vistas larga y consolidada del reporte final
#'
#' Helper interno para separar conteos por regla de conteos por persona.
#'
#' @return Lista con vista larga y vista consolidada.
.vistas_reporte_final_muestras <- function(reporte_final) {
  keys <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  criterios <- c(
    existencia = "cae_existencia",
    lina = "cae_lina",
    campo = "cae_campo",
    duplicados = "cae_duplicado",
    tematica = "cae_tematica"
  )
  criterios <- criterios[criterios %in% names(reporte_final)]

  vista_larga <- dplyr::bind_rows(lapply(names(criterios), function(criterio) {
    flag <- criterios[[criterio]]
    reporte_final %>%
      normalize_keys(keys) %>%
      dplyr::filter(.flag_reporte_muestras(.data[[flag]])) %>%
      dplyr::transmute(
        DIRECTORIO,
        SECUENCIA_P,
        ORDEN,
        criterio_caida = criterio,
        flag_caida = flag,
        criterio_principal_reporte = dplyr::coalesce(.data$criterio_principal_reporte, NA_character_),
        razon_principal_caida = dplyr::coalesce(.data$razon_principal_caida, NA_character_),
        variable_principal_caida = dplyr::coalesce(.data$variable_principal_caida, NA_character_),
        valor_principal_caida = dplyr::coalesce(.data$valor_principal_caida, NA_character_),
        observacion_final = dplyr::coalesce(.data$observacion_final, NA_character_)
      )
  }))

  vista_consolidada <- reporte_final %>%
    normalize_keys(keys) %>%
    dplyr::distinct(DIRECTORIO, SECUENCIA_P, ORDEN, .keep_all = TRUE) %>%
    dplyr::mutate(
      definicion_criterio_principal =
        "Se usa `criterio_principal_reporte` calculado por el paquete; este reporte no redefine la prioridad."
    )

  list(
    vista_larga_encuesta_regla = vista_larga,
    vista_consolidada_encuesta = vista_consolidada
  )
}

#' Construir detalle municipal para municipios objetivo
#'
#' Helper interno que cruza personas evaluadas con reporte final y municipio.
#'
#' @return Tibble de detalle municipal.
.detalle_municipios_reporte_muestras <- function(personas_eval,
                                                 reporte_final,
                                                 municipios,
                                                 municipios_objetivo) {
  keys <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  objetivo_norm <- .normalizar_texto_reporte_muestras(municipios_objetivo)

  base <- personas_eval %>%
    normalize_keys(keys) %>%
    dplyr::left_join(municipios, by = "DIRECTORIO") %>%
    dplyr::filter(.normalizar_texto_reporte_muestras(.data$municipio) %in% objetivo_norm)

  reporte <- reporte_final %>%
    normalize_keys(keys) %>%
    dplyr::select(
      dplyr::all_of(keys),
      dplyr::any_of(c(
        "cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica",
        "n_criterios_reporte", "criterios_reporte", "criterio_principal_reporte",
        "razon_principal_caida", "variable_principal_caida", "valor_principal_caida",
        "observacion_final", "recuperable_potencial", "estado_recuperacion",
        "motivo_estado_recuperacion", "tipo_recuperacion"
      ))
    )

  base %>%
    dplyr::left_join(reporte, by = keys) %>%
    .completar_columnas_detalle_municipal_muestras() %>%
    dplyr::mutate(
      cae_reporte = dplyr::coalesce(.data$n_criterios_reporte, 0L) > 0,
      recuperable_potencial = dplyr::coalesce(.data$recuperable_potencial, FALSE),
      requiere_revision_manual =
        .data$recuperable_potencial |
        (.data$cae_reporte & dplyr::coalesce(.data$criterio_principal_reporte, "") == "existencia"),
      clasificacion_final = dplyr::case_when(
        .data$recuperable_potencial ~ "Recuperable",
        .data$cae_reporte & .data$requiere_revision_manual ~ "Requiere revision",
        .data$cae_reporte ~ "Perdida",
        dplyr::coalesce(.data$n_criterios_caida, 0L) > 0 ~ "Incompleta",
        TRUE ~ "Completa"
      ),
      motivo_caida = dplyr::coalesce(.data$razon_principal_caida, .data$criterios_reporte, .data$criterios_caida, "sin_caida"),
      etapa_donde_cae = dplyr::case_when(
        .data$cae_reporte ~ dplyr::coalesce(.data$criterio_principal_reporte, "reporte_final_caidas"),
        dplyr::coalesce(.data$n_criterios_caida, 0L) > 0 ~ "evaluacion_intermedia",
        TRUE ~ "no_cae"
      ),
      etapa_proceso = "detalle municipal de completitud/perdida",
      script_funcion = "R/reporte_criterios_completitud_muestras.R::generar_reporte_criterios_completitud_muestras()",
      variable_usada = dplyr::coalesce(.data$variable_principal_caida, "n_criterios_reporte"),
      valores_posibles = dplyr::coalesce(.data$valor_principal_caida, as.character(.data$n_criterios_reporte), "0"),
      regla_o_validacion = dplyr::coalesce(.data$observacion_final, "No presenta caida en el reporte final."),
      nivel_llave = "persona: DIRECTORIO + SECUENCIA_P + ORDEN",
      n_encuestas_evaluadas = dplyr::n(),
      n_encuestas_caen = sum(.data$cae_reporte, na.rm = TRUE),
      n_encuestas_pasan = sum(!.data$cae_reporte, na.rm = TRUE),
      observacion_tecnica = dplyr::case_when(
        .data$recuperable_potencial ~ dplyr::coalesce(.data$motivo_estado_recuperacion, "Caso recuperable potencial."),
        .data$requiere_revision_manual ~ "Caso requiere revision manual antes de tratarse como perdida definitiva.",
        .data$cae_reporte ~ "Caso clasificado en reporte_final_caidas.",
        TRUE ~ "Caso evaluado sin caida final."
      )
    ) %>%
    dplyr::mutate(
      directorio = .data$DIRECTORIO,
      secuencia_p = .data$SECUENCIA_P,
      orden = .data$ORDEN
    ) %>%
    dplyr::select(
      municipio,
      codigo_municipio,
      directorio,
      secuencia_p,
      orden,
      clasificacion_final,
      motivo_caida,
      etapa_donde_cae,
      requiere_revision_manual,
      etapa_proceso,
      script_funcion,
      variable_usada,
      valores_posibles,
      regla_o_validacion,
      nivel_llave,
      n_encuestas_evaluadas,
      n_encuestas_caen,
      n_encuestas_pasan,
      observacion_tecnica,
      dplyr::any_of(c(
        "cae_existencia",
        "cae_lina",
        "cae_campo",
        "cae_duplicado",
        "cae_tematica",
        "n_criterios_reporte",
        "criterios_reporte",
        "criterio_principal_reporte",
        "variable_principal_caida",
        "valor_principal_caida",
        "estado_recuperacion",
        "tipo_recuperacion"
      ))
    )
}

#' Completar columnas esperadas del detalle municipal
#'
#' Helper interno para que el detalle sea estable aunque el reporte final no
#' incluya todos los criterios opcionales.
#'
#' @return Data frame con columnas opcionales completadas.
.completar_columnas_detalle_municipal_muestras <- function(df) {
  cols_log <- c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica", "recuperable_potencial")
  for (col in cols_log) {
    if (!col %in% names(df)) df[[col]] <- FALSE
  }

  cols_chr <- c(
    "criterios_reporte", "criterio_principal_reporte", "razon_principal_caida",
    "variable_principal_caida", "valor_principal_caida", "observacion_final",
    "estado_recuperacion", "motivo_estado_recuperacion", "tipo_recuperacion",
    "criterios_caida"
  )
  for (col in cols_chr) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  if (!"n_criterios_reporte" %in% names(df)) df$n_criterios_reporte <- 0L
  if (!"n_criterios_caida" %in% names(df)) df$n_criterios_caida <- 0L

  df
}

#' Resumir detalle municipal del reporte de Muestras
#'
#' Helper interno que resume conteos por municipio, etapa y clasificacion.
#'
#' @return Tibble resumen municipal.
.resumen_municipios_reporte_muestras <- function(detalle) {
  if (!is.data.frame(detalle) || nrow(detalle) == 0) {
    return(tibble::tibble())
  }

  detalle %>%
    dplyr::group_by(.data$municipio, .data$codigo_municipio, .data$etapa_donde_cae, .data$variable_usada, .data$clasificacion_final) %>%
    dplyr::summarise(
      n_encuestas_evaluadas = dplyr::n_distinct(.data$directorio),
      n_registros_persona_evaluados = dplyr::n(),
      n_encuestas_caen = dplyr::n_distinct(.data$directorio[.data$clasificacion_final != "Completa"]),
      n_registros_caen = sum(.data$clasificacion_final != "Completa", na.rm = TRUE),
      n_requiere_revision_manual = sum(.data$requiere_revision_manual, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$municipio, dplyr::desc(.data$n_registros_caen), .data$etapa_donde_cae)
}

#' Crear tabla de variables por criterio
#'
#' Helper interno para explicar a Muestras que variables puras y derivadas
#' soportan cada componente de caida sin recalcular diagnosticos.
#'
#' @return Tibble con variables, reglas, fuente de codigo y observaciones.
.tabla_variables_criterios_muestras <- function(diag_con_tematica = NULL,
                                                reglas_tematica = NULL,
                                                reporte_final = NULL) {
  base <- tibble::tribble(
    ~componente_analisis, ~subcomponente, ~nivel, ~capitulo, ~variable_encuesta, ~variable_derivada_paquete, ~regla_o_condicion, ~valores_o_condicion_caida, ~fuente_codigo, ~observacion_tecnica,
    "completitud", "existencia_entre_capitulos", "vivienda", NA_character_, NA_character_, "pres_*, req_*, ok_*, n_caps_faltantes, capitulos_faltantes, cae_existencia", "Validacion estructural de presencia/requerimiento entre capitulos; no corresponde a una pregunta directa del formulario.", "n_caps_faltantes > 0 o capitulos requeridos no presentes", "R/diagnostico_cruce_capitulos.R::diagnostico_cruce_capitulos()", "Depende de la llave DIRECTORIO y del universo esperado por capitulo; Capitulo B tiene cobertura especial.",
    "completitud", "existencia_entre_capitulos", "hogar", NA_character_, NA_character_, "pres_*, req_*, ok_*, n_caps_faltantes, capitulos_faltantes, cae_existencia", "Validacion estructural de presencia/requerimiento entre capitulos; no corresponde a una pregunta directa del formulario.", "n_caps_faltantes > 0 o capitulos requeridos no presentes", "R/diagnostico_cruce_capitulos.R::diagnostico_cruce_capitulos()", "Depende de DIRECTORIO + SECUENCIA_P; no usar ORDEN aunque exista por arrastre.",
    "completitud", "existencia_entre_capitulos", "persona", NA_character_, NA_character_, "pres_*, req_*, ok_*, n_caps_faltantes, capitulos_faltantes, cae_existencia", "Validacion estructural de presencia/requerimiento entre capitulos; no corresponde a una pregunta directa del formulario.", "n_caps_faltantes > 0 o capitulos requeridos no presentes", "R/diagnostico_cruce_capitulos.R::diagnostico_cruce_capitulos()", "Depende de DIRECTORIO + SECUENCIA_P + ORDEN y de universos por edad para capitulos de persona.",
    "campo", "completitud_campo", "vivienda", "A", "NVCAPCTRL1", "encuesta_efectiva_campo, encuesta_completa_campo, cae_campo_base, cae_campo", "Define estado operativo de la vivienda y si la vivienda es efectiva.", "control de vivienda fuera de completo/ocupada presente segun flujo operativo", "R/completitud_campo.R::clasificar_completitud_campo()", "Criterio de campo base consolidado a DIRECTORIO.",
    "campo", "completitud_campo", "vivienda", "A", "NVCAPCTRL2", "encuesta_efectiva_campo, encuesta_completa_campo, cae_campo_base, cae_campo", "Define presencia en vivienda ocupada.", "control diferente de presencia cuando la vivienda esta ocupada", "R/completitud_campo.R::clasificar_completitud_campo()", "Criterio de campo base consolidado a DIRECTORIO.",
    "campo", "completitud_campo", "vivienda", "A", "NVCAPCTRL1A", "estado_viv, motivo_principal_campo_base", "Clasifica estados de vivienda no efectiva.", "valor asociado a vivienda no efectiva o no encuestable", "R/completitud_campo.R::clasificar_completitud_campo()", "Variable auxiliar de clasificacion operativa cuando NVCAPCTRL1 indica caso no ocupado/presente.",
    "campo", "completitud_campo", "vivienda", "A", "NVCAPCTRL2A", "estado_viv, motivo_principal_campo_base", "Clasifica motivo de no presencia en vivienda ocupada.", "rechazo, personas ocupadas, nadie en hogar o ausente temporal", "R/completitud_campo.R::clasificar_completitud_campo()", "Variable auxiliar de clasificacion operativa cuando NVCAPCTRL2 indica no presencia.",
    "campo", "completitud_campo", "vivienda", "A", "RES_VIV", "viv_resultado_completo, encuesta_completa_campo, cae_campo_base, cae_campo", "Resultado final de vivienda usado para determinar completitud de campo.", "RES_VIV diferente de 1 implica falla si la vivienda es efectiva", "R/completitud_campo.R::clasificar_completitud_campo()", "No se usa ORDEN para este nivel.",
    "campo", "completitud_campo", "hogar", "C", "NHCCPCTRL1", "hogar_completo, encuesta_completa_campo, cae_campo_base, cae_campo", "Control operativo de hogar con informacion.", "control de hogar diferente de 1 o sin informacion completa", "R/completitud_campo.R::clasificar_completitud_campo()", "Opera a nivel hogar con DIRECTORIO + SECUENCIA_P.",
    "campo", "completitud_campo", "hogar", "C", "NHCCPCTRL1A", "estado_hog, motivo_principal_campo_base", "Clasifica motivo operativo cuando el hogar no tiene informacion.", "rechazo, personas ocupadas, nadie en hogar o ausente temporal", "R/completitud_campo.R::clasificar_completitud_campo()", "Variable auxiliar de clasificacion operativa.",
    "campo", "completitud_campo", "hogar", "C", "RES_HOG", "hogar_completo, encuesta_completa_campo, cae_campo_base, cae_campo", "Resultado final de hogar usado para completitud de campo.", "RES_HOG diferente de 1 implica hogar incompleto", "R/completitud_campo.R::clasificar_completitud_campo()", "No usar ORDEN para hogares.",
    "campo", "conteo_personas_hogar", "hogar", "C", "NHCCPCTRL2", "cae_campo_nhccpctrl2, n_personas_cap_e, diferencia_personas_hogar, cae_campo", "Contrasta el numero de personas declarado en hogar contra personas observadas en capitulo E.", "NHCCPCTRL2 no coincide con registros unicos de personas en E", "R/completitud_tres_criterios.R::.preparar_causal_conteo_personas_hogar()", "Criterio de hogar; se propaga a personas del hogar en el reporte final.",
    "campo", "completitud_campo", "persona", "E", "NPCEPCTRL1", "persona_completa, encuesta_completa_campo, cae_campo_base, cae_campo", "Control operativo de persona con informacion.", "control de persona diferente de 1 o sin informacion completa", "R/completitud_campo.R::clasificar_completitud_campo()", "Opera a nivel persona con ORDEN.",
    "campo", "completitud_campo", "persona", "E", "NPCEPCTRL1A", "estado_per, motivo_principal_campo_base", "Clasifica motivo operativo cuando la persona no tiene informacion.", "rechazo, ocupado o ausente temporal", "R/completitud_campo.R::clasificar_completitud_campo()", "Variable auxiliar de clasificacion operativa.",
    "campo", "completitud_campo", "persona", "E", "RES_PER", "persona_completa, encuesta_completa_campo, cae_campo_base, cae_campo", "Resultado final de persona usado para completitud de campo.", "RES_PER diferente de 1 implica persona incompleta", "R/completitud_campo.R::clasificar_completitud_campo()", "Solo aqui ORDEN identifica la unidad persona.",
    "lina", "criterio_lina_vivienda", "vivienda", "A", "NVCAPCTRL1", "vivienda_completa_lina, cae_lina", "Control de vivienda requerido por Lina.", "NVCAPCTRL1 debe ser 1", "R/completitud_lina.R::diagnostico_completitud_lina()", "Criterio de vivienda a DIRECTORIO.",
    "lina", "criterio_lina_vivienda", "vivienda", "A", "NVCAPCTRL2", "vivienda_completa_lina, cae_lina", "Control de presencia requerido por Lina.", "NVCAPCTRL2 debe ser 1", "R/completitud_lina.R::diagnostico_completitud_lina()", "Criterio de vivienda a DIRECTORIO.",
    "lina", "criterio_lina_vivienda", "vivienda", "B", "NVCBP1", "vivienda_completa_lina, cae_lina", "Variable requerida de vivienda en Lina.", "vacio/NA implica falla", "R/completitud_lina.R::diagnostico_completitud_lina()", "Capitulo B tiene regla especial; no expandir por ORDEN.",
    "lina", "criterio_lina_vivienda", "vivienda", "B", "NVCBP16", "vivienda_completa_lina, cae_lina", "Variable requerida de vivienda en Lina.", "vacio/NA implica falla", "R/completitud_lina.R::diagnostico_completitud_lina()", "Capitulo B tiene regla especial; no expandir por ORDEN.",
    "lina", "criterio_lina_hogar", "hogar", "C", "NHCCPCTRL1", "hogar_completo_lina, cae_lina", "Control de hogar requerido por Lina.", "NHCCPCTRL1 debe ser 1", "R/completitud_lina.R::diagnostico_completitud_lina()", "Llave valida: DIRECTORIO + SECUENCIA_P.",
    "lina", "criterio_lina_hogar", "hogar", "C", "NHCCP1", "hogar_completo_lina, cae_lina", "Variable requerida de hogar en Lina.", "vacio/NA implica falla", "R/completitud_lina.R::diagnostico_completitud_lina()", "No usar ORDEN para hogar.",
    "lina", "criterio_lina_hogar", "hogar", "MA", "NHCMP1A", "hogar_completo_lina, cae_lina", "Variable requerida de gastos del hogar en Lina.", "vacio/NA implica falla", "R/completitud_lina.R::diagnostico_completitud_lina()", "MA se integra a nivel hogar.",
    "lina", "criterio_lina_hogar", "hogar", "MA", "NHCMP5A", "hogar_completo_lina, cae_lina", "Variable requerida de gastos del hogar en Lina.", "vacio/NA implica falla", "R/completitud_lina.R::diagnostico_completitud_lina()", "MA se integra a nivel hogar.",
    "lina", "criterio_lina_persona", "persona", "E", "NPCEPCTRL1", "persona_completa_lina, cae_lina", "Control de persona requerido por Lina.", "NPCEPCTRL1 debe ser 1", "R/completitud_lina.R::diagnostico_completitud_lina()", "Llave valida: DIRECTORIO + SECUENCIA_P + ORDEN.",
    "lina", "criterio_lina_persona", "persona", "E", "NPCEP6", "persona_completa_lina, cae_lina", "Variable requerida de persona en Lina.", "vacio/NA implica falla", "R/completitud_lina.R::diagnostico_completitud_lina()", "Solo los capitulos de persona justifican ORDEN.",
    "duplicados", "duplicados", "persona", "E", "NPCEP2", "cae_duplicado, criterio_duplicados, clave_duplicado", "Identificacion de posibles duplicados de personas.", "coincidencia de nombre con fecha/documento segun criterio de duplicados", "R/completitud_tres_criterios.R::.construir_reporte_final_caidas_tres_criterios()", "El reporte final materializa duplicados a nivel persona y puede propagarlos a vivienda/hogar.",
    "duplicados", "duplicados", "persona", "E", "NPCEP3A", "cae_duplicado, criterio_duplicados, clave_duplicado", "Identificacion de posibles duplicados de personas.", "coincidencia de nombre con fecha/documento segun criterio de duplicados", "R/completitud_tres_criterios.R::.construir_reporte_final_caidas_tres_criterios()", "El codigo tambien usa variables derivadas normalizadas para comparar duplicados.",
    "duplicados", "duplicados", "persona", "E", NA_character_, "numero_documento_unificado, tipo_documento_normalizado, numero_documento_normalizado, cae_duplicado", "Identificacion de posibles duplicados con datos normalizados o derivados.", "coincidencia en campos normalizados de identificacion", "R/completitud_tres_criterios.R::.construir_reporte_final_caidas_tres_criterios()", "No se identifico una variable pura unica para todos los campos normalizados en este reporte."
  )

  tematica <- .filas_tematica_variables_criterios_muestras(
    diag_con_tematica = diag_con_tematica,
    reglas_tematica = reglas_tematica,
    reporte_final = reporte_final
  )

  dplyr::bind_rows(base, tematica) %>%
    dplyr::mutate(
      llave_nivel = vapply(.data$nivel, function(nivel) {
        paste(.llaves_nivel_reporte_muestras(nivel), collapse = " + ")
      }, character(1))
    ) %>%
    dplyr::select(
      componente_analisis,
      subcomponente,
      nivel,
      llave_nivel,
      capitulo,
      variable_encuesta,
      variable_derivada_paquete,
      regla_o_condicion,
      valores_o_condicion_caida,
      fuente_codigo,
      observacion_tecnica
    ) %>%
    .agregar_descripciones_variables_criterios_muestras()
}

#' Construir filas de tematica para la tabla de variables
#'
#' @return Tibble con variables tematicas detectadas.
.filas_tematica_variables_criterios_muestras <- function(diag_con_tematica = NULL,
                                                         reglas_tematica = NULL,
                                                         reporte_final = NULL) {
  reglas <- .extraer_reglas_tematica_reporte_muestras(
    diag_con_tematica = diag_con_tematica,
    reglas_tematica = reglas_tematica
  )
  vars_reporte <- .extraer_variables_tematica_reporte_muestras(reporte_final)

  if (is.data.frame(reglas) && nrow(reglas) > 0) {
    filas <- reglas %>%
      dplyr::transmute(
        .desde_reglas_tematica = dplyr::coalesce(.data$origen_regla == "reglas_tematica_argumento", FALSE),
        componente_analisis = "tematica",
        subcomponente = "incompletitud_tematica",
        nivel = dplyr::coalesce(.data$nivel, "persona"),
        capitulo = dplyr::coalesce(.data$capitulo, NA_character_),
        variable_encuesta = .data$variable,
        variable_derivada_paquete = "cae_tematica, razon_tematica, variable_tematica, valor_tematica",
        regla_o_condicion = dplyr::coalesce(.data$descripcion, "Regla de incompletitud tematica configurada."),
        valores_o_condicion_caida = dplyr::if_else(
          dplyr::coalesce(.data$origen_regla == "reglas_tematica_argumento", FALSE),
          "NA, vacío o espacios",
          "vacio/NA o condicion tematica configurada implica falla"
        ),
        fuente_codigo = "R/incompletitud_tematica.R::diagnostico_caidas_con_tematica()",
        observacion_tecnica = dplyr::if_else(
          dplyr::coalesce(.data$origen_regla == "reglas_tematica_argumento", FALSE),
          "Variable incluida en reglas_tematica; se documenta aunque no genere caída efectiva en el reporte final.",
          "Variable recuperada desde reglas tematicas disponibles en el diagnostico."
        )
      )
  } else {
    filas <- tibble::tibble(
      componente_analisis = character(),
      subcomponente = character(),
      nivel = character(),
      capitulo = character(),
      variable_encuesta = character(),
      variable_derivada_paquete = character(),
      regla_o_condicion = character(),
      valores_o_condicion_caida = character(),
      fuente_codigo = character(),
      observacion_tecnica = character()
    )
  }

  vars_faltantes <- setdiff(vars_reporte, filas$variable_encuesta)
  if (length(vars_faltantes) > 0) {
    filas <- dplyr::bind_rows(
      filas,
      tibble::tibble(
        componente_analisis = "tematica",
        subcomponente = "incompletitud_tematica",
        nivel = vapply(vars_faltantes, .inferir_nivel_variable_reporte_muestras, character(1)),
        capitulo = vapply(vars_faltantes, .inferir_capitulo_variable_reporte_muestras, character(1)),
        variable_encuesta = vars_faltantes,
        variable_derivada_paquete = "cae_tematica, razon_tematica, variable_tematica, valor_tematica, variable_principal_caida",
        regla_o_condicion = "Variable observada en columnas tematicas del reporte final.",
        valores_o_condicion_caida = "condicion tematica configurada en el diagnostico",
        fuente_codigo = "R/incompletitud_tematica.R::diagnostico_caidas_con_tematica()",
        observacion_tecnica = "Variable recuperada desde `variable_tematica` o `variable_principal_caida`; capitulo/nivel exacto no siempre esta materializado en el reporte final."
      )
    )
  }

  if (nrow(filas) == 0) {
    filas <- tibble::tibble(
      componente_analisis = "tematica",
      subcomponente = "incompletitud_tematica",
      nivel = "vivienda",
      capitulo = "B",
      variable_encuesta = "NVCBP11AA",
      variable_derivada_paquete = "cae_tematica, razon_tematica, variable_tematica, valor_tematica",
      regla_o_condicion = "Regla tematica por defecto del paquete cuando no se suministran reglas.",
      valores_o_condicion_caida = "estrato faltante",
      fuente_codigo = "R/incompletitud_tematica.R::.reglas_tematica_default()",
      observacion_tecnica = "No se recuperaron reglas tematicas ni variables tematicas observadas; se reporta el default documentado del paquete."
    )
  }

  filas %>%
    dplyr::mutate(
      capitulo = dplyr::if_else(
        is.na(.data$capitulo) | !nzchar(.data$capitulo),
        vapply(.data$variable_encuesta, .inferir_capitulo_variable_reporte_muestras, character(1)),
        .data$capitulo
      ),
      nivel = tolower(stringr::str_squish(.data$nivel)),
      nivel_inferido = vapply(.data$variable_encuesta, .inferir_nivel_variable_reporte_muestras, character(1)),
      nivel = dplyr::if_else(
        .data$nivel %in% c("vivienda", "hogar", "persona"),
        .data$nivel,
        .data$nivel_inferido
      )
    ) %>%
    dplyr::select(-nivel_inferido) %>%
    dplyr::filter(!is.na(.data$variable_encuesta), nzchar(.data$variable_encuesta)) %>%
    dplyr::distinct(
      .data$componente_analisis,
      .data$subcomponente,
      .data$nivel,
      .data$variable_encuesta,
      .keep_all = TRUE
    )
}

#' Inferir capitulo desde nombre de variable
#'
#' @return Codigo de capitulo o `NA_character_`.
.inferir_capitulo_variable_reporte_muestras <- function(variable) {
  var <- toupper(stringr::str_squish(as.character(variable)))
  if (!nzchar(var) || is.na(var)) return(NA_character_)
  dplyr::case_when(
    grepl("^NVCAP", var) ~ "A",
    grepl("^NVCBP", var) ~ "B",
    grepl("^NHCCP", var) ~ "C",
    grepl("^NHCMP", var) ~ "MA",
    grepl("^NPCEP", var) ~ "E",
    var %in% c("SEGMENTO", "CLASE", "UUID") ~ "A",
    TRUE ~ NA_character_
  )
}

#' Inferir nivel desde nombre de variable
#'
#' @return Nivel metodologico de la variable.
.inferir_nivel_variable_reporte_muestras <- function(variable) {
  var <- toupper(stringr::str_squish(as.character(variable)))
  if (!nzchar(var) || is.na(var)) return("persona")
  dplyr::case_when(
    grepl("^NVC", var) ~ "vivienda",
    grepl("^NHC", var) ~ "hogar",
    grepl("^NPC", var) ~ "persona",
    var %in% c("SEGMENTO", "CLASE", "UUID") ~ "vivienda",
    TRUE ~ "persona"
  )
}

#' Extraer reglas tematicas desde el diagnostico
#'
#' @return Tibble con `capitulo`, `nivel`, `variable` y `descripcion`.
.extraer_reglas_tematica_reporte_muestras <- function(diag_con_tematica = NULL,
                                                      reglas_tematica = NULL) {
  candidatos <- list()
  if (is.data.frame(reglas_tematica)) {
    candidatos$reglas_tematica_argumento <- reglas_tematica
  }
  if (is.list(diag_con_tematica)) {
    nombres <- c(
      "reglas_tematica",
      "reglas_variables",
      "reglas",
      "reglas_aplicadas",
      "detalle_reglas_tematica"
    )
    candidatos <- c(candidatos, diag_con_tematica[intersect(nombres, names(diag_con_tematica))])
  }

  origenes <- names(candidatos)
  if (is.null(origenes) || length(origenes) != length(candidatos)) {
    origenes <- rep("", length(candidatos))
  }

  extraidas <- Map(function(df, origen) {
    if (!is.data.frame(df)) return(NULL)
    var_col <- col_first_existing(df, c("variable", "variable_regla", "variable_tematica", "variable_principal_caida"))
    if (is.null(var_col)) return(NULL)
    cap_col <- col_first_existing(df, c("capitulo", "capitulo_regla"))
    nivel_col <- col_first_existing(df, c("nivel", "nivel_regla"))
    desc_col <- col_first_existing(df, c("descripcion", "descripcion_regla", "razon_tematica"))

    tibble::tibble(
      capitulo = if (!is.null(cap_col)) as.character(df[[cap_col]]) else NA_character_,
      nivel = if (!is.null(nivel_col)) tolower(as.character(df[[nivel_col]])) else NA_character_,
      variable = as.character(df[[var_col]]),
      descripcion = if (!is.null(desc_col)) as.character(df[[desc_col]]) else "Regla de incompletitud tematica",
      origen_regla = origen
    )
  }, candidatos, origenes)

  out <- dplyr::bind_rows(extraidas)
  if (!is.data.frame(out) || nrow(out) == 0) {
    return(tibble::tibble(
      capitulo = character(),
      nivel = character(),
      variable = character(),
      descripcion = character(),
      origen_regla = character()
    ))
  }

  out %>%
    dplyr::mutate(
      capitulo = toupper(stringr::str_squish(.data$capitulo)),
      nivel = tolower(stringr::str_squish(.data$nivel)),
      variable = stringr::str_squish(.data$variable),
      descripcion = stringr::str_squish(.data$descripcion),
      origen_regla = stringr::str_squish(.data$origen_regla)
    ) %>%
    dplyr::filter(!is.na(.data$variable), nzchar(.data$variable))
}

#' Extraer variables tematicas desde el reporte final
#'
#' @return Vector de variables observadas.
.extraer_variables_tematica_reporte_muestras <- function(reporte_final = NULL) {
  if (!is.data.frame(reporte_final)) {
    return(character())
  }

  df <- reporte_final
  if ("cae_tematica" %in% names(df)) {
    df <- df %>% dplyr::filter(.flag_reporte_muestras(.data$cae_tematica))
  }
  if ("criterio_principal_reporte" %in% names(df)) {
    if ("cae_tematica" %in% names(df)) {
      df <- df %>%
        dplyr::filter(
          .flag_reporte_muestras(.data$cae_tematica) |
            dplyr::coalesce(.data$criterio_principal_reporte, "") == "tematica"
        )
    } else {
      df <- df %>%
        dplyr::filter(dplyr::coalesce(.data$criterio_principal_reporte, "") == "tematica")
    }
  }

  cols <- intersect(c("variable_tematica", "variable_principal_caida"), names(df))
  if (length(cols) == 0) {
    return(character())
  }

  vars <- unlist(lapply(cols, function(col) .separar_variables_reporte_muestras(df[[col]])))
  vars <- unique(stats::na.omit(vars))
  vars[nzchar(vars)]
}

#' Separar listas de variables reportadas como texto
#'
#' @return Vector de nombres de variable.
.separar_variables_reporte_muestras <- function(x) {
  x <- as.character(x)
  x <- stats::na.omit(x)
  if (length(x) == 0) return(character())
  partes <- unlist(strsplit(x, "\\s*[,;|]\\s*"))
  partes <- stringr::str_squish(partes)
  partes <- partes[!partes %in% c("", "NA", "na", "sin registro")]
  unique(partes)
}

#' Agregar descripciones de diccionario a tabla de variables
#'
#' @return Tibble con columna `descripcion_diccionario`.
.agregar_descripciones_variables_criterios_muestras <- function(tabla) {
  dic <- .diccionario_variables_reporte_muestras()

  tabla %>%
    dplyr::mutate(
      descripcion_diccionario = vapply(.data$variable_encuesta, function(var) {
        .descripcion_diccionario_variable_muestras(var, dic)
      }, character(1))
    ) %>%
    dplyr::select(
      componente_analisis,
      subcomponente,
      nivel,
      llave_nivel,
      capitulo,
      variable_encuesta,
      descripcion_diccionario,
      variable_derivada_paquete,
      regla_o_condicion,
      valores_o_condicion_caida,
      fuente_codigo,
      observacion_tecnica
    )
}

#' Leer diccionario oficial de variables
#'
#' @return Tibble con `variable` y `descripcion`.
.diccionario_variables_reporte_muestras <- function() {
  ruta <- system.file("diccionario", "Diccionario_em2025.xlsx", package = "analisisem2025")
  if (!nzchar(ruta)) {
    ruta <- file.path("inst", "diccionario", "Diccionario_em2025.xlsx")
  }
  if (!file.exists(ruta)) {
    return(tibble::tibble(variable = character(), descripcion = character()))
  }

  dic <- tryCatch(readxl::read_excel(ruta), error = function(e) NULL)
  if (!is.data.frame(dic) || nrow(dic) == 0) {
    return(tibble::tibble(variable = character(), descripcion = character()))
  }

  var_col <- col_first_existing(dic, c("variable", "Variable", "VARIABLE", "nombre_variable", "Nombre de variable", "NOMBRE_VARIABLE"))
  desc_col <- col_first_existing(dic, c("descripcion", "descripciÃ³n", "Descripción", "DESCRIPCION", "pregunta", "Pregunta", "etiqueta", "Etiqueta"))
  if (is.null(var_col) || is.null(desc_col)) {
    return(tibble::tibble(variable = character(), descripcion = character()))
  }

  dic %>%
    dplyr::transmute(
      variable = toupper(stringr::str_squish(as.character(.data[[var_col]]))),
      descripcion = stringr::str_squish(as.character(.data[[desc_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$variable), nzchar(.data$variable)) %>%
    dplyr::distinct(.data$variable, .keep_all = TRUE)
}

#' Obtener descripcion de una variable
#'
#' @return Texto de descripcion o fallback.
.descripcion_diccionario_variable_muestras <- function(variable, dic) {
  if (is.na(variable) || !nzchar(as.character(variable))) {
    return("No aplica")
  }
  variable_norm <- toupper(stringr::str_squish(as.character(variable)))
  if (!is.data.frame(dic) || nrow(dic) == 0 || !variable_norm %in% dic$variable) {
    return("No encontrada en diccionario")
  }
  desc <- dic$descripcion[dic$variable == variable_norm][1]
  if (is.na(desc) || !nzchar(desc)) "No encontrada en diccionario" else desc
}

#' Construir cascada ejecutiva de caidas para Muestras
#'
#' @return Tibble con cascada por ambito y nivel.
.cascada_encuestas_muestras <- function(dfs,
                                        diag_ref,
                                        reporte_final,
                                        municipios,
                                        tabla_variables_criterios) {
  niveles <- c("vivienda", "hogar", "persona")
  ambitos <- tibble::tibble(
    ambito = c("general", "Soacha", "Zipac\u00f3n"),
    municipio = c(NA_character_, "Soacha", "Zipac\u00f3n"),
    municipio_norm = c(NA_character_, "soacha", "zipacon")
  )

  dplyr::bind_rows(lapply(niveles, function(nivel) {
    universo <- .universo_nivel_resumen_muestras(dfs, diag_ref, nivel)
    flags <- .flags_caida_nivel_muestras(reporte_final, nivel)
    dplyr::bind_rows(lapply(seq_len(nrow(ambitos)), function(i) {
      .cascada_ambito_nivel_muestras(
        universo = universo,
        flags = flags,
        municipios = municipios,
        ambito = ambitos$ambito[[i]],
        municipio = ambitos$municipio[[i]],
        municipio_norm = ambitos$municipio_norm[[i]],
        nivel = nivel,
        tabla_variables_criterios = tabla_variables_criterios
      )
    }))
  }))
}

#' Comparar cascada con y sin reglas tematicas
#'
#' Helper interno que no recalcula diagnosticos: usa el reporte final extendido
#' para el escenario con tematica y `diag_tres$reporte_final_caidas` para el
#' escenario sin tematica.
#'
#' @return Tibble con la comparacion de cascadas por escenario, ambito y nivel.
.comparacion_cascada_con_sin_tematica_muestras <- function(dfs,
                                                           diag_tres,
                                                           diag_ref,
                                                           reporte_final_con_tematica,
                                                           municipios,
                                                           tabla_variables_criterios) {
  cascada_con <- .cascada_encuestas_muestras(
    dfs = dfs,
    diag_ref = diag_ref,
    reporte_final = reporte_final_con_tematica,
    municipios = municipios,
    tabla_variables_criterios = tabla_variables_criterios
  ) %>%
    dplyr::mutate(escenario = "Con reglas temáticas")

  reporte_sin_tematica <- .preparar_reporte_sin_tematica_muestras(diag_tres)

  cascada_sin <- .cascada_encuestas_muestras(
    dfs = dfs,
    diag_ref = diag_tres,
    reporte_final = reporte_sin_tematica,
    municipios = municipios,
    tabla_variables_criterios = tabla_variables_criterios
  ) %>%
    dplyr::mutate(
      escenario = "Sin reglas temáticas",
      criterios_incluidos = dplyr::if_else(
        .data$componente == "tematica",
        "cae_tematica (no aplicado en escenario sin reglas temáticas)",
        .data$criterios_incluidos
      ),
      variables_encuesta_usadas = dplyr::if_else(
        .data$componente == "tematica",
        "No aplica",
        .data$variables_encuesta_usadas
      )
    )

  dplyr::bind_rows(cascada_con, cascada_sin) %>%
    dplyr::select(
      escenario,
      ambito,
      municipio,
      codigo_municipio,
      nivel,
      llave_nivel,
      paso,
      componente,
      criterios_incluidos,
      variables_encuesta_usadas,
      unidades,
      unidades_caidas_acumuladas,
      unidades_restantes,
      porcentaje_sobre_base_inicial,
      porcentaje_caidas_acumulado
    ) %>%
    dplyr::arrange(.data$escenario, .data$ambito, .data$nivel, .data$paso)
}

#' Preparar reporte final sin tematica
#'
#' Helper interno que parte de `diag_tres$reporte_final_caidas`, conserva los
#' criterios base disponibles y fuerza `cae_tematica = FALSE`.
#'
#' @return Tibble minimo con llaves de persona y flags de caida.
.preparar_reporte_sin_tematica_muestras <- function(diag_tres) {
  keys <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  flags_base <- c("cae_existencia", "cae_campo", "cae_lina", "cae_duplicado")

  reporte_base <- diag_tres$reporte_final_caidas
  trae_duplicado <- is.data.frame(reporte_base) && "cae_duplicado" %in% names(reporte_base)

  if (!is.data.frame(reporte_base) || !all(keys %in% names(reporte_base))) {
    out <- tibble::tibble(DIRECTORIO = character(), SECUENCIA_P = character(), ORDEN = character())
  } else {
    out <- reporte_base %>%
      normalize_keys(keys) %>%
      dplyr::select(dplyr::all_of(keys), dplyr::any_of(flags_base)) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
  }

  for (flag in flags_base) {
    if (!flag %in% names(out)) out[[flag]] <- FALSE
  }

  if (!isTRUE(trae_duplicado) && is.data.frame(diag_tres$duplicados_personas_e) &&
      all(keys %in% names(diag_tres$duplicados_personas_e))) {
    duplicados <- diag_tres$duplicados_personas_e %>%
      normalize_keys(keys) %>%
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(keys),
          ~ !is.na(.x) & nzchar(as.character(.x))
        )
      ) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::mutate(.cae_duplicado_fuente = TRUE)

    out <- out %>%
      dplyr::full_join(duplicados, by = keys) %>%
      dplyr::mutate(
        cae_duplicado =
          dplyr::coalesce(.data$cae_duplicado, FALSE) |
          dplyr::coalesce(.data$.cae_duplicado_fuente, FALSE)
      ) %>%
      dplyr::select(-.cae_duplicado_fuente)
  }

  out %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(flags_base), .flag_reporte_muestras),
      cae_tematica = FALSE
    )
}

#' Propagar flags de caida al nivel solicitado
#'
#' @return Tibble con llaves del nivel y flags logicos.
.flags_caida_nivel_muestras <- function(reporte_final, nivel) {
  keys <- .llaves_nivel_reporte_muestras(nivel)
  flags <- c("cae_existencia", "cae_campo", "cae_lina", "cae_duplicado", "cae_tematica")

  if (!is.data.frame(reporte_final) || !all(keys %in% names(reporte_final))) {
    out <- .llaves_distintas_nivel_reporte_muestras(NULL, nivel)
    for (flag in flags) out[[flag]] <- logical()
    return(out)
  }

  df <- reporte_final %>% normalize_keys(keys)
  for (flag in setdiff(flags, names(df))) {
    df[[flag]] <- FALSE
  }

  df %>%
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(keys),
        ~ !is.na(.x) & nzchar(as.character(.x))
      )
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(flags), .flag_reporte_muestras)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(flags), ~ any(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Construir cascada para un ambito y nivel
#'
#' @return Tibble de siete pasos de cascada.
.cascada_ambito_nivel_muestras <- function(universo,
                                           flags,
                                           municipios,
                                           ambito,
                                           municipio,
                                           municipio_norm,
                                           nivel,
                                           tabla_variables_criterios) {
  nivel_actual <- nivel
  keys <- .llaves_nivel_reporte_muestras(nivel)
  llave_nivel <- paste(keys, collapse = " + ")
  flags_cols <- c("cae_existencia", "cae_campo", "cae_lina", "cae_duplicado", "cae_tematica")

  base <- universo
  codigo_municipio <- NA_character_
  if (!is.na(municipio_norm)) {
    municipios_unicos <- municipios %>%
      normalize_keys("DIRECTORIO") %>%
      dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)

    base <- base %>%
      dplyr::left_join(municipios_unicos, by = "DIRECTORIO") %>%
      dplyr::filter(.normalizar_texto_reporte_muestras(.data$municipio) == municipio_norm) %>%
      dplyr::select(dplyr::all_of(keys), dplyr::any_of(c("codigo_municipio", "municipio")))

    codigos <- unique(stats::na.omit(as.character(base$codigo_municipio)))
    codigo_municipio <- if (length(codigos) == 0) NA_character_ else paste(sort(codigos), collapse = ", ")
    base <- base %>% dplyr::select(dplyr::all_of(keys))
  }

  base <- base %>% dplyr::distinct(dplyr::across(dplyr::all_of(keys)))
  base_inicial <- nrow(base)

  eval <- base %>% dplyr::left_join(flags, by = keys)
  for (flag in flags_cols) {
    if (!flag %in% names(eval)) eval[[flag]] <- FALSE
    eval[[flag]] <- dplyr::coalesce(eval[[flag]], FALSE)
  }

  eval <- eval %>%
    dplyr::mutate(
      componente_prioritario = dplyr::case_when(
        .data$cae_existencia ~ "completitud",
        .data$cae_campo ~ "campo",
        .data$cae_lina ~ "lina",
        .data$cae_duplicado ~ "duplicados",
        .data$cae_tematica ~ "tematica",
        TRUE ~ "base_final"
      )
    )

  conteos <- stats::setNames(
    as.integer(table(factor(
      eval$componente_prioritario,
      levels = c("completitud", "campo", "lina", "duplicados", "tematica", "base_final")
    ))),
    c("completitud", "campo", "lina", "duplicados", "tematica", "base_final")
  )

  pasos <- tibble::tibble(
    paso = c(
      "0_base_inicial",
      "1_caidas_completitud",
      "2_caidas_campo",
      "3_caidas_lina",
      "4_caidas_duplicados",
      "5_caidas_tematica",
      "6_base_final"
    ),
    componente = c("base_inicial", "completitud", "campo", "lina", "duplicados", "tematica", "base_final"),
    criterios_incluidos = c(
      "universo evaluado",
      "cae_existencia",
      "cae_campo",
      "cae_lina",
      "cae_duplicado",
      "cae_tematica",
      "sin caidas en completitud, campo, Lina, duplicados ni tematica"
    ),
    unidades = c(
      base_inicial,
      conteos["completitud"],
      conteos["campo"],
      conteos["lina"],
      conteos["duplicados"],
      conteos["tematica"],
      conteos["base_final"]
    )
  )

  caidas_acum <- cumsum(c(0L, as.integer(pasos$unidades[2:6])))
  pasos$unidades_caidas_acumuladas <- c(caidas_acum, caidas_acum[length(caidas_acum)])
  pasos$unidades_restantes <- base_inicial - pasos$unidades_caidas_acumuladas
  pasos$unidades_restantes[pasos$componente == "base_inicial"] <- base_inicial
  pasos$porcentaje_sobre_base_inicial <- if (base_inicial > 0) round(100 * pasos$unidades / base_inicial, 2) else NA_real_
  pasos$porcentaje_caidas_acumulado <- if (base_inicial > 0) round(100 * pasos$unidades_caidas_acumuladas / base_inicial, 2) else NA_real_

  detalles <- lapply(pasos$componente, function(componente) {
    .detalle_variables_componente_cascada_muestras(tabla_variables_criterios, componente)
  })

  pasos %>%
    dplyr::mutate(
      ambito = ambito,
      municipio = municipio,
      codigo_municipio = codigo_municipio,
      nivel = nivel_actual,
      llave_nivel = llave_nivel,
      variables_encuesta_usadas = vapply(detalles, `[[`, character(1), "variables_encuesta_usadas"),
      descripcion_variables = vapply(detalles, `[[`, character(1), "descripcion_variables"),
      detalle_criterio_variables = vapply(detalles, `[[`, character(1), "detalle_criterio_variables"),
      criterio_prioridad = "completitud -> campo -> lina -> duplicados -> tematica",
      observacion_propagacion = .observacion_propagacion_cascada_muestras(nivel_actual)
    ) %>%
    dplyr::select(
      ambito,
      municipio,
      codigo_municipio,
      nivel,
      llave_nivel,
      paso,
      componente,
      criterios_incluidos,
      variables_encuesta_usadas,
      descripcion_variables,
      detalle_criterio_variables,
      unidades,
      unidades_caidas_acumuladas,
      unidades_restantes,
      porcentaje_sobre_base_inicial,
      porcentaje_caidas_acumulado,
      criterio_prioridad,
      observacion_propagacion
    )
}

#' Resumir variables de un componente para la cascada
#'
#' @return Lista con textos para columnas de cascada.
.detalle_variables_componente_cascada_muestras <- function(tabla, componente) {
  if (componente %in% c("base_inicial", "base_final")) {
    return(list(
      variables_encuesta_usadas = "No aplica",
      descripcion_variables = "No aplica",
      detalle_criterio_variables = if (componente == "base_inicial") {
        "Universo inicial de unidades unicas evaluadas."
      } else {
        "Unidades sin caidas en los componentes de la cascada."
      }
    ))
  }

  filas <- tabla %>%
    dplyr::filter(.data$componente_analisis == componente)
  vars <- filas$variable_encuesta
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  vars_txt <- if (length(vars) == 0) "No aplica" else paste(vars, collapse = ", ")

  desc <- filas %>%
    dplyr::filter(!is.na(.data$variable_encuesta), nzchar(.data$variable_encuesta)) %>%
    dplyr::distinct(.data$variable_encuesta, .data$descripcion_diccionario) %>%
    dplyr::mutate(txt = paste0(.data$variable_encuesta, ": ", .data$descripcion_diccionario))
  desc_txt <- if (nrow(desc) == 0) "No aplica" else paste(desc$txt, collapse = "; ")

  reglas <- unique(filas$regla_o_condicion)
  reglas <- reglas[!is.na(reglas) & nzchar(reglas)]
  detalle_txt <- if (length(reglas) == 0) "No aplica" else paste(reglas, collapse = " | ")

  list(
    variables_encuesta_usadas = vars_txt,
    descripcion_variables = desc_txt,
    detalle_criterio_variables = detalle_txt
  )
}

#' Describir propagacion de caidas para cascada
#'
#' @return Texto metodologico.
.observacion_propagacion_cascada_muestras <- function(nivel) {
  switch(
    nivel,
    vivienda = "Una caida detectada en vivienda, hogar o persona se propaga a DIRECTORIO.",
    hogar = "Una caida detectada en hogar o persona se propaga a DIRECTORIO + SECUENCIA_P; no se usa ORDEN.",
    persona = "La caida se cuenta directamente a nivel DIRECTORIO + SECUENCIA_P + ORDEN.",
    "Nivel no soportado."
  )
}

#' Texto metodologico para correo a Muestras
#'
#' Helper interno que devuelve parrafos breves y reutilizables para explicar el
#' reporte.
#'
#' @return Tibble con una columna `texto`.
.metodologia_correo_reporte_muestras <- function() {
  tibble::tibble(
    texto = c(
      "La clasificacion de perdida no se define por una sola variable sino por criterios explicitos del paquete.",
      "El flujo integrado parte de existencia entre capitulos, completitud operativa de campo, criterio Lina, duplicados y, cuando se suministra, incompletitud tematica.",
      "La regla consolidada del reporte final es n_criterios_reporte > 0 a nivel persona; la base EM completa excluye a nivel DIRECTORIO cuando algun registro del directorio presenta caida.",
      "Las llaves se interpretan por naturaleza del capitulo: vivienda DIRECTORIO, hogar DIRECTORIO + SECUENCIA_P y persona DIRECTORIO + SECUENCIA_P + ORDEN.",
      "En capitulos de vivienda u hogar no se usa ORDEN como llave sustantiva, aunque exista por estructura o arrastre.",
      "El detalle municipal conserva clasificacion final, motivo, etapa y bandera de revision manual para los municipios objetivo."
    )
  )
}

#' Exportar reporte formal de criterios de Muestras
#'
#' Helper interno que escribe los dos Excel solicitados usando la funcion
#' existente `exportar_tablas_excel()`.
#'
#' @return Lista con rutas normalizadas de archivos exportados.
.exportar_reporte_criterios_muestras <- function(criterios_general,
                                                 detalle_municipal,
                                                 resumen_municipal,
                                                 resumen_final_niveles,
                                                 resumen_final_municipios_niveles,
                                                 tabla_variables_criterios,
                                                 cascada_encuestas_muestras,
                                                 comparacion_cascada_con_sin_tematica,
                                                 vista_larga,
                                                 vista_consolidada,
                                                 metodologia_correo,
                                                 ruta_salida) {
  dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

  archivo_general <- file.path(ruta_salida, "reporte_criterios_completitud_general.xlsx")
  archivo_municipal <- file.path(ruta_salida, "reporte_criterios_completitud_zipacon_soacha.xlsx")

  ruta_general <- exportar_tablas_excel(
    x = list(
      criterios_general = criterios_general,
      tabla_variables_criterios = tabla_variables_criterios,
      cascada_encuestas_muestras = cascada_encuestas_muestras,
      comparacion_cascada_con_sin_tematica = comparacion_cascada_con_sin_tematica,
      resumen_final_niveles = resumen_final_niveles,
      vista_larga_encuesta_regla = vista_larga,
      vista_consolidada_encuesta = vista_consolidada,
      metodologia_correo = metodologia_correo
    ),
    ruta = archivo_general
  )

  ruta_municipal <- exportar_tablas_excel(
    x = list(
      detalle_zipacon_soacha = detalle_municipal,
      resumen_zipacon_soacha = resumen_municipal,
      cascada_encuestas_muestras = cascada_encuestas_muestras %>%
        dplyr::filter(.data$ambito != "general"),
      comparacion_cascada_con_sin_tematica = comparacion_cascada_con_sin_tematica %>%
        dplyr::filter(.data$ambito != "general"),
      tabla_variables_criterios = tabla_variables_criterios,
      resumen_final_municipios_niveles = resumen_final_municipios_niveles,
      metodologia_correo = metodologia_correo
    ),
    ruta = archivo_municipal
  )

  list(
    criterios_general = ruta_general,
    zipacon_soacha = ruta_municipal
  )
}
