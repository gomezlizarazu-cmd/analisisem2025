#' Construir la base definitiva de encuestas completas
#'
#' Filtra todos los capitulos de `dfs` para conservar unicamente las encuestas
#' cuyo `DIRECTORIO` no presenta caidas en los criterios integrados disponibles
#' dentro de `reporte_final_caidas`.
#'
#' @param dfs Lista de data frames con los capitulos de la encuesta.
#' @param reporte_final_caidas Data frame consolidado con las variables de
#'   caida por encuesta/persona.
#'
#' @details
#' Una encuesta completa se define como aquella cuyo `DIRECTORIO` no presenta
#' ninguna caida en los criterios integrados disponibles dentro de
#' `reporte_final_caidas`. La funcion detecta automaticamente cuales variables
#' de caida estan presentes en el reporte (`cae_existencia`, `cae_lina`,
#' `cae_campo`, `cae_duplicado`, `cae_tematica`), resume la señal a nivel de
#' encuesta (`DIRECTORIO`) y elimina ese conjunto de directorios de todos los
#' capitulos de `dfs`.
#'
#' La salida mantiene la estructura de `dfs`, pero filtrada a la base oficial
#' de encuestas completas, e incluye un resumen de registros antes y despues
#' del filtrado por capitulo.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{dfs}{Lista de capitulos filtrados a la base de encuestas completas.}
#'   \item{resumen}{Tibble con el numero de registros antes, despues y
#'   excluidos por capitulo.}
#'   \item{directorios_excluidos}{Vector de `DIRECTORIO` excluidos por tener al
#'   menos una caida.}
#'   \item{n_encuestas_excluidas}{Numero total de directorios excluidos.}
#' }
#'
#' @examples
#' \dontrun{
#' em_completa <- construir_base_em_completa(
#'   dfs = dfs,
#'   reporte_final_caidas = diag_con_tematica$reporte_final_caidas
#' )
#'
#' names(em_completa$dfs)
#' em_completa$resumen
#'
#' em_completa_tres <- construir_base_em_completa(
#'   dfs = dfs,
#'   reporte_final_caidas = diag_tres$reporte_final_caidas
#' )
#' }
#'
#' @export
construir_base_em_completa <- function(dfs, reporte_final_caidas) {
  if (!is.list(dfs)) {
    stop("`dfs` debe ser una lista de data frames.")
  }

  if (!is.data.frame(reporte_final_caidas)) {
    stop("`reporte_final_caidas` debe ser un data frame.")
  }

  nombres_dfs <- names(dfs)
  if (is.null(nombres_dfs) || any(!nzchar(nombres_dfs))) {
    nombres_dfs <- paste0("capitulo_", seq_along(dfs))
  }

  caps_sin_directorio <- nombres_dfs[
    !vapply(
      dfs,
      function(df) is.data.frame(df) && "DIRECTORIO" %in% names(df),
      logical(1)
    )
  ]

  if (length(caps_sin_directorio) > 0) {
    stop(
      "El capítulo `", caps_sin_directorio[[1]], "` no contiene la variable DIRECTORIO. ",
      "No es seguro construir la base de encuestas completas."
    )
  }

  vars_caida_disponibles <- intersect(
    c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica"),
    names(reporte_final_caidas)
  )

  if (length(vars_caida_disponibles) == 0) {
    stop("No se encontraron variables de caída en `reporte_final_caidas`.")
  }

  reporte_norm <- normalize_keys(reporte_final_caidas, "DIRECTORIO")

  directorios_excluidos <- reporte_norm %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars_caida_disponibles),
        .coerce_flag_base_em_completa
      )
    ) %>%
    dplyr::filter(!is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO)) %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      encuesta_caida = any(
        dplyr::if_any(
          dplyr::all_of(vars_caida_disponibles),
          identity
        )
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$encuesta_caida) %>%
    dplyr::pull(.data$DIRECTORIO) %>%
    unique()

  dfs_em_completa <- lapply(dfs, function(df) {
    df_norm <- normalize_keys(df, "DIRECTORIO")

    df_norm %>%
      dplyr::filter(!(.data$DIRECTORIO %in% directorios_excluidos))
  })

  resumen_em_completa <- tibble::tibble(
    capitulo = nombres_dfs,
    registros_antes = unname(vapply(dfs, nrow, integer(1))),
    registros_despues = unname(vapply(dfs_em_completa, nrow, integer(1)))
  ) %>%
    dplyr::mutate(
      registros_excluidos = .data$registros_antes - .data$registros_despues
    )

  list(
    dfs = dfs_em_completa,
    resumen = resumen_em_completa,
    directorios_excluidos = directorios_excluidos,
    n_encuestas_excluidas = length(directorios_excluidos)
  )
}

.coerce_flag_base_em_completa <- function(x) {
  if (is.logical(x)) {
    return(dplyr::coalesce(x, FALSE))
  }

  if (is.numeric(x) || is.integer(x)) {
    return(dplyr::coalesce(x != 0, FALSE))
  }

  if (is.character(x)) {
    x_norm <- stringr::str_trim(stringr::str_to_lower(x))
    return(dplyr::coalesce(x_norm %in% c("true", "t", "1", "si", "sí", "yes"), FALSE))
  }

  dplyr::coalesce(as.logical(x), FALSE)
}

#' Verificar coherencia de universos en la base de encuestas completas
#'
#' Audita que la base construida con `construir_base_em_completa()` conserve la
#' identidad de universos a nivel de vivienda, hogar y persona frente a la base
#' original.
#'
#' @param dfs_original Lista original de capitulos antes del filtrado.
#' @param dfs_completa Lista de capitulos ya filtrados a encuestas completas.
#' @param directorios_excluidos Vector de `DIRECTORIO` excluidos al construir la
#'   base completa.
#' @param cap_vivienda Capitulo que se usara para construir el universo de
#'   vivienda. Por defecto `"A"`.
#' @param cap_hogar Capitulo que se usara para construir el universo de hogar.
#'   Por defecto `"E"`.
#' @param cap_persona Capitulo que se usara para construir el universo de
#'   persona. Por defecto `"E"`.
#'
#' @details
#' Esta funcion no reconstruye las caidas. Su proposito es auditar que la base
#' completa construida desde `construir_base_em_completa()` sea coherente con
#' los universos originales a nivel de vivienda, hogar y persona.
#'
#' La validacion contrasta, para cada nivel, que:
#'
#' `universo_original = universo_completo + universo_excluido`
#'
#' ademas de verificar que ningun `DIRECTORIO` excluido permanezca en la base
#' completa y que no existan llaves compartidas simultaneamente entre el
#' universo completo y el universo excluido.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{resumen}{Tibble resumen por nivel con los conteos de conservacion.}
#'   \item{check_general}{`TRUE` si todos los niveles pasan las validaciones de
#'   conservacion e interseccion; `FALSE` en caso contrario.}
#'   \item{problemas}{Tibble con los niveles que no pasan alguna validacion.}
#'   \item{detalle}{Lista con el detalle de vivienda, hogar y persona.}
#' }
#'
#' @examples
#' \dontrun{
#' em_completa <- construir_base_em_completa(
#'   dfs = dfs,
#'   reporte_final_caidas = diag_con_tematica$reporte_final_caidas
#' )
#'
#' verificacion_em <- verificar_universos_em_completa(
#'   dfs_original = dfs,
#'   dfs_completa = em_completa$dfs,
#'   directorios_excluidos = em_completa$directorios_excluidos
#' )
#'
#' verificacion_em$resumen
#' verificacion_em$problemas
#' }
#'
#' @export
verificar_universos_em_completa <- function(
    dfs_original,
    dfs_completa,
    directorios_excluidos,
    cap_vivienda = "A",
    cap_hogar = "E",
    cap_persona = "E"
) {
  if (!is.list(dfs_original)) {
    stop("`dfs_original` debe ser una lista de data frames.")
  }

  if (!is.list(dfs_completa)) {
    stop("`dfs_completa` debe ser una lista de data frames.")
  }

  if (is.null(directorios_excluidos)) {
    stop("`directorios_excluidos` no debe ser NULL.")
  }

  .validar_capitulo_em_completa(
    dfs = dfs_original,
    capitulo = cap_vivienda,
    llaves = c("DIRECTORIO"),
    objeto = "dfs_original"
  )
  .validar_capitulo_em_completa(
    dfs = dfs_original,
    capitulo = cap_hogar,
    llaves = c("DIRECTORIO", "SECUENCIA_P"),
    objeto = "dfs_original"
  )
  .validar_capitulo_em_completa(
    dfs = dfs_original,
    capitulo = cap_persona,
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    objeto = "dfs_original"
  )

  .validar_capitulo_em_completa(
    dfs = dfs_completa,
    capitulo = cap_vivienda,
    llaves = c("DIRECTORIO"),
    objeto = "dfs_completa"
  )
  .validar_capitulo_em_completa(
    dfs = dfs_completa,
    capitulo = cap_hogar,
    llaves = c("DIRECTORIO", "SECUENCIA_P"),
    objeto = "dfs_completa"
  )
  .validar_capitulo_em_completa(
    dfs = dfs_completa,
    capitulo = cap_persona,
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    objeto = "dfs_completa"
  )

  directorios_excluidos_df <- tibble::tibble(DIRECTORIO = directorios_excluidos) %>%
    normalize_keys("DIRECTORIO") %>%
    dplyr::filter(!is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO)) %>%
    dplyr::distinct(.data$DIRECTORIO)

  viv_original <- .universo_llaves_em_completa(
    df = dfs_original[[cap_vivienda]],
    llaves = c("DIRECTORIO")
  )
  hog_original <- .universo_llaves_em_completa(
    df = dfs_original[[cap_hogar]],
    llaves = c("DIRECTORIO", "SECUENCIA_P")
  )
  per_original <- .universo_llaves_em_completa(
    df = dfs_original[[cap_persona]],
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  )

  viv_completa <- .universo_llaves_em_completa(
    df = dfs_completa[[cap_vivienda]],
    llaves = c("DIRECTORIO")
  )
  hog_completa <- .universo_llaves_em_completa(
    df = dfs_completa[[cap_hogar]],
    llaves = c("DIRECTORIO", "SECUENCIA_P")
  )
  per_completa <- .universo_llaves_em_completa(
    df = dfs_completa[[cap_persona]],
    llaves = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  )

  viv_excluido <- viv_original %>%
    dplyr::semi_join(directorios_excluidos_df, by = "DIRECTORIO")
  hog_excluido <- hog_original %>%
    dplyr::semi_join(directorios_excluidos_df, by = "DIRECTORIO")
  per_excluido <- per_original %>%
    dplyr::semi_join(directorios_excluidos_df, by = "DIRECTORIO")

  detalle_vivienda <- .verificar_nivel_em_completa(
    nivel = "vivienda",
    universo_original = viv_original,
    universo_completo = viv_completa,
    universo_excluido = viv_excluido,
    directorios_excluidos = directorios_excluidos_df
  )
  detalle_hogar <- .verificar_nivel_em_completa(
    nivel = "hogar",
    universo_original = hog_original,
    universo_completo = hog_completa,
    universo_excluido = hog_excluido,
    directorios_excluidos = directorios_excluidos_df
  )
  detalle_persona <- .verificar_nivel_em_completa(
    nivel = "persona",
    universo_original = per_original,
    universo_completo = per_completa,
    universo_excluido = per_excluido,
    directorios_excluidos = directorios_excluidos_df
  )

  resumen <- dplyr::bind_rows(
    detalle_vivienda$resumen,
    detalle_hogar$resumen,
    detalle_persona$resumen
  )

  problemas <- resumen %>%
    dplyr::filter(
      !.data$check_conservacion |
        !.data$check_excluidos_fuera_completa |
        !.data$check_sin_interseccion_llaves
    )

  list(
    resumen = resumen,
    check_general = all(
      resumen$check_conservacion &
        resumen$check_excluidos_fuera_completa &
        resumen$check_sin_interseccion_llaves
    ),
    problemas = problemas,
    detalle = list(
      vivienda = detalle_vivienda,
      hogar = detalle_hogar,
      persona = detalle_persona
    )
  )
}

.validar_capitulo_em_completa <- function(dfs, capitulo, llaves, objeto) {
  if (!capitulo %in% names(dfs)) {
    stop("El capítulo `", capitulo, "` no existe en `", objeto, "`.")
  }

  df <- dfs[[capitulo]]

  if (!is.data.frame(df)) {
    stop("El capítulo `", capitulo, "` de `", objeto, "` no es un data frame.")
  }

  faltantes <- setdiff(llaves, names(df))
  if (length(faltantes) > 0) {
    stop(
      "El capítulo `", capitulo, "` de `", objeto, "` no contiene la(s) variable(s) requerida(s): ",
      paste(faltantes, collapse = ", "), "."
    )
  }

  invisible(TRUE)
}

.universo_llaves_em_completa <- function(df, llaves) {
  normalize_keys(df, llaves) %>%
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(llaves),
        ~ !is.na(.x) & nzchar(.x)
      )
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves)))
}

.verificar_nivel_em_completa <- function(
    nivel,
    universo_original,
    universo_completo,
    universo_excluido,
    directorios_excluidos
) {
  llaves <- names(universo_original)

  llaves_duplicadas <- dplyr::inner_join(
    universo_completo,
    universo_excluido,
    by = llaves
  )

  directorios_en_completa <- universo_completo %>%
    dplyr::semi_join(directorios_excluidos, by = "DIRECTORIO")

  n_original <- nrow(universo_original)
  n_completo <- nrow(universo_completo)
  n_excluido <- nrow(universo_excluido)
  diferencia_esperada <- n_original - n_completo
  diferencia_observada <- n_excluido

  resumen <- tibble::tibble(
    nivel = nivel,
    universo_original = n_original,
    universo_completo = n_completo,
    universo_excluido = n_excluido,
    diferencia_esperada = diferencia_esperada,
    diferencia_observada = diferencia_observada,
    check_conservacion = diferencia_esperada == diferencia_observada,
    check_excluidos_fuera_completa = nrow(directorios_en_completa) == 0,
    check_sin_interseccion_llaves = nrow(llaves_duplicadas) == 0
  )

  list(
    resumen = resumen,
    universo_original = universo_original,
    universo_completo = universo_completo,
    universo_excluido = universo_excluido,
    directorios_excluidos_en_completa = directorios_en_completa,
    llaves_en_completo_y_excluido = llaves_duplicadas
  )
}

#' Auditar coherencia diagnóstica entre `reporte_final_caidas` y el capítulo E
#'
#' Contrasta el universo diagnóstico consolidado en `reporte_final_caidas`
#' contra la presencia física de registros persona en el capítulo definido por
#' `cap_persona`.
#'
#' @param diag Objeto diagnóstico de entrada. Debe ser una lista y contener
#'   `reporte_final_caidas`. Por defecto usa `diag_con_tematica` si existe en
#'   el entorno de llamada.
#' @param dfs_original Lista original de capítulos. Si es `NULL`, la función
#'   intenta usar `diag$dfs`.
#' @param cap_persona Capítulo físico de personas que se quiere auditar. Por
#'   defecto `"E"`.
#'
#' @details
#' Esta función audita la coherencia entre el universo diagnóstico consolidado
#' en `reporte_final_caidas` y la presencia física de registros persona en el
#' capítulo E, permitiendo distinguir entre caídas observadas y propagaciones
#' diagnósticas desde niveles superiores.
#'
#' La función no reconstruye caídas ni usa `construir_base_em_completa()`. Su
#' propósito es comparar directamente:
#'
#' - el universo persona del diagnóstico final;
#' - el universo persona observado físicamente en `dfs_original[[cap_persona]]`.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{resumen}{Tibble ejecutivo con el total diagnóstico, observadas en E,
#'   no observadas en E y sus porcentajes.}
#'   \item{hallazgo_principal}{Texto resumido de auditoría.}
#'   \item{patrones_propagacion}{Conteo de combinaciones de variables de caída
#'   para los casos no observados en E.}
#'   \item{criterios_fuera_E}{Conteo de `criterio_principal_reporte` para los
#'   casos no observados en E, si existe en el reporte.}
#'   \item{detalle}{Lista con `personas_diag`,
#'   `personas_observadas_en_E` y `personas_no_observadas_en_E`.}
#' }
#'
#' @examples
#' \dontrun{
#' verif_diag <- verificar_universos_em_completa_diag(
#'   diag = diag_con_tematica,
#'   dfs_original = dfs
#' )
#'
#' verif_diag$resumen
#' verif_diag$hallazgo_principal
#' verif_diag$patrones_propagacion
#' }
#'
#' @export
verificar_universos_em_completa_diag <- function(
    diag = diag_con_tematica,
    dfs_original = NULL,
    cap_persona = "E"
) {
  if (is.null(diag)) {
    rlang::abort("`diag` no puede ser NULL.")
  }

  if (!is.list(diag)) {
    rlang::abort("`diag` debe ser una lista.")
  }

  if (!"reporte_final_caidas" %in% names(diag)) {
    rlang::abort("`diag` debe contener `reporte_final_caidas`.")
  }

  if (is.null(dfs_original)) {
    if ("dfs" %in% names(diag)) {
      dfs_original <- diag$dfs
    } else {
      rlang::abort("No se pudo resolver dfs_original. Pase dfs_original = dfs.")
    }
  }

  if (!is.list(dfs_original)) {
    rlang::abort("`dfs_original` debe ser una lista de data frames.")
  }

  if (!cap_persona %in% names(dfs_original)) {
    rlang::abort(paste0("El capítulo `", cap_persona, "` no existe en `dfs_original`."))
  }

  if (!is.data.frame(diag$reporte_final_caidas)) {
    rlang::abort("`diag$reporte_final_caidas` debe ser un data frame.")
  }

  if (!is.data.frame(dfs_original[[cap_persona]])) {
    rlang::abort(paste0("El capítulo `", cap_persona, "` de `dfs_original` no es un data frame."))
  }

  req_keys <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")

  faltan_diag <- setdiff(req_keys, names(diag$reporte_final_caidas))
  if (length(faltan_diag) > 0) {
    rlang::abort(
      paste0(
        "`diag$reporte_final_caidas` no contiene la(s) variable(s) requerida(s): ",
        paste(faltan_diag, collapse = ", "),
        "."
      )
    )
  }

  faltan_cap <- setdiff(req_keys, names(dfs_original[[cap_persona]]))
  if (length(faltan_cap) > 0) {
    rlang::abort(
      paste0(
        "El capítulo `", cap_persona, "` de `dfs_original` no contiene la(s) variable(s) requerida(s): ",
        paste(faltan_cap, collapse = ", "),
        "."
      )
    )
  }

  personas_diag <- diag$reporte_final_caidas %>%
    normalize_keys(req_keys) %>%
    dplyr::filter(
      !is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO),
      !is.na(.data$SECUENCIA_P), nzchar(.data$SECUENCIA_P),
      !is.na(.data$ORDEN), nzchar(.data$ORDEN)
    ) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE)

  personas_E <- dfs_original[[cap_persona]] %>%
    normalize_keys(req_keys) %>%
    dplyr::filter(
      !is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO),
      !is.na(.data$SECUENCIA_P), nzchar(.data$SECUENCIA_P),
      !is.na(.data$ORDEN), nzchar(.data$ORDEN)
    ) %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  personas_diag_observadas_en_E <- personas_diag %>%
    dplyr::semi_join(
      personas_E,
      by = req_keys
    )

  personas_diag_no_observadas_en_E <- personas_diag %>%
    dplyr::anti_join(
      personas_E,
      by = req_keys
    )

  vars_caida_presentes <- intersect(
    c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica"),
    names(personas_diag_no_observadas_en_E)
  )

  if (length(vars_caida_presentes) > 0) {
    patrones_propagacion <- personas_diag_no_observadas_en_E %>%
      dplyr::count(
        dplyr::across(dplyr::all_of(vars_caida_presentes)),
        sort = TRUE,
        name = "n"
      )
  } else {
    patrones_propagacion <- tibble::tibble(n = integer())
  }

  if ("criterio_principal_reporte" %in% names(personas_diag_no_observadas_en_E)) {
    criterios_fuera_E <- personas_diag_no_observadas_en_E %>%
      dplyr::count(.data$criterio_principal_reporte, sort = TRUE, name = "n")
  } else {
    criterios_fuera_E <- tibble::tibble(
      criterio_principal_reporte = character(),
      n = integer()
    )
  }

  n_diag <- nrow(personas_diag)
  n_observadas <- nrow(personas_diag_observadas_en_E)
  n_no_observadas <- nrow(personas_diag_no_observadas_en_E)

  pct_observadas <- if (n_diag == 0) 0 else n_observadas / n_diag
  pct_no_observadas <- if (n_diag == 0) 0 else n_no_observadas / n_diag

  resumen <- tibble::tibble(
    personas_diagnostico = n_diag,
    personas_observadas_en_E = n_observadas,
    personas_no_observadas_en_E = n_no_observadas,
    pct_observadas_en_E = pct_observadas,
    pct_no_observadas_en_E = pct_no_observadas
  )

  hallazgo_principal <- paste0(
    "Del universo diagnóstico de personas con caída, el ",
    round(100 * pct_no_observadas, 2),
    "% no presenta observación física en el capítulo ",
    cap_persona,
    ", lo que sugiere propagación diagnóstica desde niveles vivienda/hogar o caídas estructurales de existencia."
  )

  list(
    resumen = resumen,
    hallazgo_principal = hallazgo_principal,
    patrones_propagacion = patrones_propagacion,
    criterios_fuera_E = criterios_fuera_E,
    detalle = list(
      personas_diag = personas_diag,
      personas_observadas_en_E = personas_diag_observadas_en_E,
      personas_no_observadas_en_E = personas_diag_no_observadas_en_E
    )
  )
}

#' Construir sabana de auditoria para casos recuperables de flujo persona
#'
#' Consolida en una sabana de auditoria los casos con desajustes de llave
#' detectados en la validacion de flujo persona, separando problemas de
#' `SECUENCIA_P` y de `ORDEN`, y exportando todas las filas de los capitulos
#' involucrados para los `DIRECTORIO` afectados.
#'
#' @param diag_orden_fuera_E Data frame con el detalle de personas no
#'   observadas en `E`, incluyendo `tipo_problema`.
#' @param diag_secuencia Data frame con el diagnostico de cruce por
#'   `DIRECTORIO + SECUENCIA_P`, incluyendo `directorio_existe_en_E`.
#' @param dfs Lista de data frames por capitulo.
#' @param carpeta_raiz Carpeta donde se exportara la sabana.
#' @param archivo Nombre del archivo Excel de salida.
#' @param diag_con_tematica Objeto opcional producido por
#'   `diagnostico_caidas_con_tematica()`. Si se suministra, se usa
#'   `diag_con_tematica$reporte_final_caidas` para distinguir casos
#'   recuperables de casos solo auditables por llave.
#' @param reporte_final_caidas Reporte final opcional a nivel persona. Se usa
#'   solo si `diag_con_tematica` es `NULL`.
#' @param cap_hog Capitulo de control de hogares. Por defecto `"C"`.
#' @param cap_per Capitulo de personas. Por defecto `"E"`.
#'
#' @details
#' La funcion no modifica la logica productiva del paquete. Su proposito es
#' construir una sabana manual de auditoria con los casos auditables por llave:
#'
#' - hogares no observados en `E` cuyo `DIRECTORIO` si existe en `E`;
#' - personas con `ORDEN` mayor al numero de personas observadas en `E`.
#'
#' Si se entrega `diag_con_tematica` o `reporte_final_caidas`, la funcion agrega
#' banderas de recuperabilidad. Un caso solo queda como
#' `recuperable_potencial` cuando el desajuste de llave puede explicar la caida
#' y no se observan bloqueos independientes de campo, duplicados o tematica. Por
#' ejemplo, un hogar no observado en `E` no es recuperable si en el capitulo `C`
#' tiene `NHCCPCTRL1 != 1` o `RES_HOG != 1`.
#'
#' Para cada `DIRECTORIO` auditable, la exportacion incluye todas las filas de
#' cada capitulo presente en `dfs`, junto con hojas de control:
#' `casos_recuperables`, `casos_auditables_llave`,
#' `recuperables_potenciales`, `no_recuperables` y `resumen_directorios`.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{casos_recuperables}{Tibble consolidado con los casos auditables por
#'   llave, conservado por compatibilidad historica. La columna
#'   `recuperable_potencial` identifica los casos recuperables bajo la nueva
#'   logica.}
#'   \item{casos_auditables_llave}{Misma base de casos auditables por llave.}
#'   \item{recuperables_potenciales}{Subconjunto de casos auditables sin
#'   bloqueos independientes detectados.}
#'   \item{no_recuperables}{Subconjunto de casos auditables que no pueden
#'   clasificarse como recuperables potenciales.}
#'   \item{resumen_directorios}{Resumen por `DIRECTORIO` de las llaves
#'   auditadas y tipos de recuperacion.}
#'   \item{ruta}{Ruta del archivo Excel exportado.}
#' }
#'
#' @examples
#' \dontrun{
#' salida <- construir_sabana_casos_recuperables(
#'   diag_orden_fuera_E = diag_orden_fuera_E,
#'   diag_secuencia = diag_secuencia,
#'   dfs = dfs,
#'   carpeta_raiz = carpeta_raiz
#' )
#'
#' salida$casos_recuperables
#' salida$resumen_directorios
#' salida$ruta
#' }
#'
#' @export
construir_sabana_casos_recuperables <- function(
    diag_orden_fuera_E,
    diag_secuencia,
    dfs,
    carpeta_raiz = ".",
    archivo = "sabana_auditoria_casos_recuperables.xlsx",
    diag_con_tematica = NULL,
    reporte_final_caidas = NULL,
    cap_hog = "C",
    cap_per = "E"
) {
  if (!is.data.frame(diag_orden_fuera_E)) {
    stop("`diag_orden_fuera_E` debe ser un data frame.")
  }

  if (!is.data.frame(diag_secuencia)) {
    stop("`diag_secuencia` debe ser un data frame.")
  }

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar la sabana.")
  }

  req_orden <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "tipo_problema")
  faltan_orden <- setdiff(req_orden, names(diag_orden_fuera_E))
  if (length(faltan_orden) > 0) {
    stop(
      "`diag_orden_fuera_E` no contiene la(s) variable(s) requerida(s): ",
      paste(faltan_orden, collapse = ", "),
      "."
    )
  }

  req_seq <- c("DIRECTORIO", "SECUENCIA_P", "directorio_existe_en_E")
  faltan_seq <- setdiff(req_seq, names(diag_secuencia))
  if (length(faltan_seq) > 0) {
    stop(
      "`diag_secuencia` no contiene la(s) variable(s) requerida(s): ",
      paste(faltan_seq, collapse = ", "),
      "."
    )
  }

  nombres_dfs <- names(dfs)
  if (is.null(nombres_dfs) || any(!nzchar(nombres_dfs))) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (!is.null(diag_con_tematica)) {
    if (!is.list(diag_con_tematica) ||
        !"reporte_final_caidas" %in% names(diag_con_tematica)) {
      stop("`diag_con_tematica` debe contener `reporte_final_caidas`.")
    }

    reporte_final_caidas <- diag_con_tematica$reporte_final_caidas
  }

  if (!is.null(reporte_final_caidas) && !is.data.frame(reporte_final_caidas)) {
    stop("`reporte_final_caidas` debe ser un data frame.")
  }

  keys_persona <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  keys_hogar <- c("DIRECTORIO", "SECUENCIA_P")

  diag_orden_norm <- diag_orden_fuera_E %>%
    normalize_keys(keys_persona)

  casos_orden_recuperables <- diag_orden_norm %>%
    dplyr::filter(
      .data$tipo_problema %in% c(
        "ORDEN mayor que personas observadas en E",
        "Llave sin evidencia en capitulos persona"
      )
    ) %>%
    dplyr::mutate(
      tipo_recuperacion = "ORDEN"
    )

  casos_secuencia_recuperables <- diag_orden_norm %>%
    dplyr::filter(
      .data$tipo_problema == "Hogar no observado en E"
    ) %>%
    dplyr::semi_join(
      diag_secuencia %>%
        normalize_keys(keys_hogar) %>%
        dplyr::filter(.coerce_flag_base_em_completa(.data$directorio_existe_en_E)),
      by = keys_hogar
    ) %>%
    dplyr::mutate(
      tipo_recuperacion = "SECUENCIA_P"
    )

  casos_recuperables <- dplyr::bind_rows(
    casos_orden_recuperables,
    casos_secuencia_recuperables
  ) %>%
    dplyr::distinct(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$ORDEN,
      .keep_all = TRUE
    ) %>%
    dplyr::arrange(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN)

  cols_control_recuperabilidad <- c(
    "directorio_existe_en_E",
    "hogar_existe_en_E",
    "orden_existe_en_E",
    "n_personas_E",
    "max_orden_E",
    "hogar_existe_en_C",
    "NHCCPCTRL1",
    "NHCCPCTRL1A",
    "NHCCPCTRL2",
    "RES_HOG",
    "hogar_operativo_completo",
    "NPCEPCTRL1",
    "NPCEPCTRL1A",
    "RES_PER",
    "NPCEP6",
    "persona_operativa_completa",
    "persona_lina_basica_completa",
    "tiene_reporte_final",
    "cae_existencia",
    "cae_lina",
    "cae_campo",
    "cae_duplicado",
    "cae_tematica",
    "n_criterios_reporte",
    "criterios_reporte",
    "criterio_principal_reporte",
    "razon_principal_caida",
    "variable_principal_caida",
    "valor_principal_caida",
    "observacion_final",
    "n_capitulos_persona_sospechosos",
    "n_capitulos_hogar_sospechosos",
    "n_capitulos_vivienda_sospechosos",
    "n_capitulos_sospechosos",
    "capitulos_persona_sospechosos",
    "capitulos_hogar_sospechosos",
    "capitulos_vivienda_sospechosos",
    "capitulos_sospechosos",
    "capitulos_sospechosos_persona",
    "capitulos_sospechosos_hogar",
    "capitulos_sospechosos_vivienda",
    "nivel_evidencia_sospechosa",
    "variable_control_hogar_usada",
    "control_hogar_sugiere_mas_personas"
  )

  casos_recuperables <- casos_recuperables %>%
    dplyr::select(-dplyr::any_of(cols_control_recuperabilidad))

  controles_recuperabilidad <- .construir_controles_recuperabilidad_sabana(
    dfs = dfs,
    reporte_final_caidas = reporte_final_caidas,
    cap_hog = cap_hog,
    cap_per = cap_per
  )

  fuentes_persona_llave <- .fuentes_llave_nivel_sabana(
    dfs = dfs,
    nivel = "persona",
    casos = casos_recuperables
  )
  fuentes_hogar_llave <- .fuentes_llave_nivel_sabana(
    dfs = dfs,
    nivel = "hogar",
    casos = casos_recuperables
  )
  fuentes_vivienda_llave <- .fuentes_llave_nivel_sabana(
    dfs = dfs,
    nivel = "vivienda",
    casos = casos_recuperables
  )

  casos_recuperables <- casos_recuperables %>%
    dplyr::left_join(
      controles_recuperabilidad$directorios_e,
      by = "DIRECTORIO"
    ) %>%
    dplyr::left_join(
      controles_recuperabilidad$hogares_e,
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::left_join(
      controles_recuperabilidad$personas_e,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::left_join(
      controles_recuperabilidad$resumen_hogar_e,
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::left_join(
      controles_recuperabilidad$control_hogar,
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::left_join(
      controles_recuperabilidad$reporte_final,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::left_join(
      fuentes_persona_llave,
      by = keys_persona
    ) %>%
    dplyr::left_join(
      fuentes_hogar_llave,
      by = keys_hogar
    ) %>%
    dplyr::left_join(
      fuentes_vivienda_llave,
      by = "DIRECTORIO"
    ) %>%
    dplyr::mutate(
      n_capitulos_persona_sospechosos = dplyr::coalesce(.data$n_capitulos_persona_sospechosos, 0L),
      n_capitulos_hogar_sospechosos = dplyr::coalesce(.data$n_capitulos_hogar_sospechosos, 0L),
      n_capitulos_vivienda_sospechosos = dplyr::coalesce(.data$n_capitulos_vivienda_sospechosos, 0L),
      n_capitulos_sospechosos = .data$n_capitulos_persona_sospechosos,
      capitulos_sospechosos = .data$capitulos_persona_sospechosos,
      capitulos_sospechosos_persona = .data$capitulos_persona_sospechosos,
      capitulos_sospechosos_hogar = .data$capitulos_hogar_sospechosos,
      capitulos_sospechosos_vivienda = .data$capitulos_vivienda_sospechosos,
      nivel_evidencia_sospechosa = dplyr::case_when(
        .data$n_capitulos_persona_sospechosos > 0L & .data$n_capitulos_hogar_sospechosos > 0L ~
          "persona | hogar",
        .data$n_capitulos_persona_sospechosos > 0L ~ "persona",
        .data$n_capitulos_hogar_sospechosos > 0L ~ "hogar",
        .data$n_capitulos_vivienda_sospechosos > 0L ~ "vivienda",
        TRUE ~ "sin_evidencia_capitulos"
      ),
      variable_control_hogar_usada = dplyr::if_else(
        .data$tipo_recuperacion == "ORDEN",
        "NHCCPCTRL2 > max_orden_E",
        NA_character_
      ),
      control_hogar_sugiere_mas_personas =
        !is.na(.data$NHCCPCTRL2) &
        !is.na(.data$max_orden_E) &
        .data$NHCCPCTRL2 > .data$max_orden_E
    ) %>%
    .clasificar_recuperabilidad_sabana()

  casos_auditables_llave <- casos_recuperables
  recuperables_potenciales <- casos_recuperables %>%
    dplyr::filter(.data$recuperable_potencial)
  no_recuperables <- casos_recuperables %>%
    dplyr::filter(!.data$recuperable_potencial)

  resumen_directorios <- casos_recuperables %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      n_llaves_auditadas = dplyr::n(),
      n_recuperables_potenciales = sum(.data$recuperable_potencial, na.rm = TRUE),
      n_no_recuperables = sum(!.data$recuperable_potencial, na.rm = TRUE),
      n_indeterminados = sum(grepl("indeterminado", .data$estado_recuperacion), na.rm = TRUE),
      secuencias_afectadas = .collapse_tokens_em_completa(.data$SECUENCIA_P),
      ordenes_afectados = .collapse_tokens_em_completa(.data$ORDEN),
      tipos_recuperacion = .collapse_tokens_em_completa(.data$tipo_recuperacion),
      estados_recuperacion = .collapse_tokens_em_completa(.data$estado_recuperacion),
      motivos_estado_recuperacion = .collapse_tokens_em_completa(.data$motivo_estado_recuperacion),
      capitulos_sospechosos = .collapse_tokens_em_completa(
        unlist(strsplit(
          .data$capitulos_sospechosos[!is.na(.data$capitulos_sospechosos)],
          "\\s*,\\s*"
        ))
      ),
      capitulos_persona_sospechosos = .collapse_tokens_em_completa(
        unlist(strsplit(
          .data$capitulos_persona_sospechosos[!is.na(.data$capitulos_persona_sospechosos)],
          "\\s*,\\s*"
        ))
      ),
      capitulos_hogar_sospechosos = .collapse_tokens_em_completa(
        unlist(strsplit(
          .data$capitulos_hogar_sospechosos[!is.na(.data$capitulos_hogar_sospechosos)],
          "\\s*,\\s*"
        ))
      ),
      capitulos_vivienda_sospechosos = .collapse_tokens_em_completa(
        unlist(strsplit(
          .data$capitulos_vivienda_sospechosos[!is.na(.data$capitulos_vivienda_sospechosos)],
          "\\s*,\\s*"
        ))
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$DIRECTORIO)

  directorios_recuperables <- casos_recuperables %>%
    dplyr::select(.data$DIRECTORIO) %>%
    dplyr::filter(!is.na(.data$DIRECTORIO), nzchar(.data$DIRECTORIO)) %>%
    dplyr::distinct()

  sabanas_capitulos <- lapply(names(dfs), function(cap) {
    df_cap <- dfs[[cap]]

    if (!is.data.frame(df_cap)) {
      return(NULL)
    }

    if (!"DIRECTORIO" %in% names(df_cap)) {
      return(NULL)
    }

    normalize_keys(df_cap, "DIRECTORIO") %>%
      dplyr::semi_join(directorios_recuperables, by = "DIRECTORIO") %>%
      dplyr::mutate(capitulo = cap, .before = 1)
  })
  names(sabanas_capitulos) <- names(dfs)

  ruta_sabana <- file.path(carpeta_raiz, archivo)
  wb <- openxlsx::createWorkbook()

  hojas_exportar <- unique(c(
    names(dfs),
    "casos_recuperables",
    "casos_auditables_llave",
    "recuperables_potenciales",
    "no_recuperables",
    "resumen_directorios"
  ))

  for (hoja in hojas_exportar) {
    openxlsx::addWorksheet(wb, hoja)

    if (hoja == "casos_recuperables") {
      datos_hoja <- casos_recuperables
    } else if (hoja == "casos_auditables_llave") {
      datos_hoja <- casos_auditables_llave
    } else if (hoja == "recuperables_potenciales") {
      datos_hoja <- recuperables_potenciales
    } else if (hoja == "no_recuperables") {
      datos_hoja <- no_recuperables
    } else if (hoja == "resumen_directorios") {
      datos_hoja <- resumen_directorios
    } else {
      datos_hoja <- sabanas_capitulos[[hoja]]
      if (is.null(datos_hoja)) {
        datos_hoja <- tibble::tibble()
      }
    }

    if (is.data.frame(datos_hoja)) {
      datos_hoja <- arreglar_utf8_df(datos_hoja)
    }

    openxlsx::writeData(wb, sheet = hoja, x = datos_hoja)
  }

  openxlsx::saveWorkbook(wb, file = ruta_sabana, overwrite = TRUE)

  list(
    casos_recuperables = casos_recuperables,
    casos_auditables_llave = casos_auditables_llave,
    recuperables_potenciales = recuperables_potenciales,
    no_recuperables = no_recuperables,
    resumen_directorios = resumen_directorios,
    ruta = ruta_sabana
  )
}

#' Construir sabana de casos recuperables desde el diagnostico completo
#'
#' Prepara automaticamente los insumos de auditoria de llaves a partir del
#' objeto devuelto por `diagnostico_caidas_con_tematica()` y construye la
#' sabana de casos auditables o recuperables.
#'
#' @param diag_con_tematica Objeto producido por
#'   `diagnostico_caidas_con_tematica()`.
#' @param dfs Lista nombrada de data frames por capitulo. Si es `NULL`, se
#'   intenta usar `diag_con_tematica$dfs`.
#' @param carpeta_raiz Carpeta donde se exportara la sabana.
#' @param archivo Nombre del archivo Excel de salida.
#' @param cap_hog Capitulo de control de hogares. Por defecto `"C"`.
#' @param cap_per Capitulo de personas. Por defecto `"E"`.
#'
#' @return
#' La salida de `construir_sabana_casos_recuperables()`, agregando los insumos
#' calculados internamente: `diag_orden_fuera_E`, `diag_secuencia` y
#' `verificacion_diag`.
#'
#' @details
#' Esta funcion es el punto de entrada recomendado cuando ya existe
#' `diag_con_tematica`. Internamente usa
#' `verificar_universos_em_completa_diag()` para identificar personas del
#' reporte final que no tienen registro fisico en el capitulo `E`, clasifica el
#' tipo de problema de llave y luego delega la clasificacion de recuperabilidad
#' a `construir_sabana_casos_recuperables()`.
#'
#' @examples
#' \dontrun{
#' sabana <- construir_sabana_casos_recuperables_desde_diagnostico(
#'   diag_con_tematica = diag_con_tematica,
#'   dfs = dfs,
#'   carpeta_raiz = carpeta_caps
#' )
#' }
#'
#' @export
construir_sabana_casos_recuperables_desde_diagnostico <- function(
    diag_con_tematica,
    dfs = NULL,
    carpeta_raiz = ".",
    archivo = "sabana_auditoria_casos_recuperables.xlsx",
    cap_hog = "C",
    cap_per = "E"
) {
  if (!is.list(diag_con_tematica)) {
    stop("`diag_con_tematica` debe ser una lista.")
  }

  if (is.null(dfs)) {
    if ("dfs" %in% names(diag_con_tematica) && is.list(diag_con_tematica$dfs)) {
      dfs <- diag_con_tematica$dfs
    } else {
      stop("Debe suministrar `dfs` o incluir `diag_con_tematica$dfs`.")
    }
  }

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  dir.create(carpeta_raiz, showWarnings = FALSE, recursive = TRUE)

  insumos <- .preparar_insumos_sabana_recuperables_desde_diagnostico(
    diag_con_tematica = diag_con_tematica,
    dfs = dfs,
    cap_per = cap_per
  )

  salida <- construir_sabana_casos_recuperables(
    diag_orden_fuera_E = insumos$diag_orden_fuera_E,
    diag_secuencia = insumos$diag_secuencia,
    dfs = dfs,
    carpeta_raiz = carpeta_raiz,
    archivo = archivo,
    diag_con_tematica = diag_con_tematica,
    cap_hog = cap_hog,
    cap_per = cap_per
  )

  salida$diag_orden_fuera_E <- insumos$diag_orden_fuera_E
  salida$diag_secuencia <- insumos$diag_secuencia
  salida$verificacion_diag <- insumos$verificacion_diag
  salida
}

.preparar_insumos_sabana_recuperables_desde_diagnostico <- function(diag_con_tematica,
                                                                    dfs,
                                                                    cap_per = "E") {
  cap_per <- toupper(cap_per)
  keys_persona <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")

  if (!cap_per %in% names(dfs) || !is.data.frame(dfs[[cap_per]])) {
    stop("No existe el capitulo de personas en `dfs`: ", cap_per, ".")
  }

  verificacion_diag <- verificar_universos_em_completa_diag(
    diag = diag_con_tematica,
    dfs_original = dfs,
    cap_persona = cap_per
  )

  personas_no_observadas <- verificacion_diag$detalle$personas_no_observadas_en_E

  for (v in keys_persona) {
    if (!v %in% names(personas_no_observadas)) {
      personas_no_observadas[[v]] <- NA_character_
    }
  }

  controles_e <- .control_persona_recuperabilidad_sabana(dfs[[cap_per]])
  fuentes_persona <- .fuentes_llave_nivel_sabana(
    dfs = dfs,
    nivel = "persona",
    casos = personas_no_observadas
  )

  diag_orden_fuera_E <- personas_no_observadas %>%
    normalize_keys(keys_persona) %>%
    dplyr::select(
      -dplyr::any_of(c(
        "directorio_existe_en_E",
        "hogar_existe_en_E",
        "orden_existe_en_E",
        "n_personas_E",
        "max_orden_E",
        "ORDEN_num",
        "tipo_problema"
      ))
    ) %>%
    dplyr::mutate(
      ORDEN_num = suppressWarnings(as.integer(.data$ORDEN))
    ) %>%
    dplyr::left_join(controles_e$directorios_e, by = "DIRECTORIO") %>%
    dplyr::left_join(controles_e$hogares_e, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(controles_e$resumen_hogar_e, by = c("DIRECTORIO", "SECUENCIA_P")) %>%
    dplyr::left_join(fuentes_persona, by = keys_persona) %>%
    dplyr::mutate(
      directorio_existe_en_E = dplyr::coalesce(.data$directorio_existe_en_E, FALSE),
      hogar_existe_en_E = dplyr::coalesce(.data$hogar_existe_en_E, FALSE),
      n_capitulos_persona_sospechosos = dplyr::coalesce(.data$n_capitulos_persona_sospechosos, 0L),
      tiene_evidencia_persona = .data$n_capitulos_persona_sospechosos > 0L,
      tipo_problema = dplyr::case_when(
        !.data$directorio_existe_en_E ~ "DIRECTORIO no existe en E",
        !.data$hogar_existe_en_E ~ "Hogar no observado en E",
        .data$tiene_evidencia_persona &
          !is.na(.data$max_orden_E) &
          .data$ORDEN_num > .data$max_orden_E ~
          "ORDEN mayor que personas observadas en E",
        .data$tiene_evidencia_persona ~
          "Persona no observada en E por otra razon",
        TRUE ~ "Llave sin evidencia en capitulos persona"
      )
    )

  diag_secuencia <- diag_orden_fuera_E %>%
    dplyr::distinct(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      .data$directorio_existe_en_E,
      .data$hogar_existe_en_E
    )

  list(
    diag_orden_fuera_E = diag_orden_fuera_E,
    diag_secuencia = diag_secuencia,
    verificacion_diag = verificacion_diag
  )
}

.llaves_nivel_sabana_recuperables <- function(nivel) {
  switch(
    nivel,
    vivienda = c("DIRECTORIO"),
    hogar = c("DIRECTORIO", "SECUENCIA_P"),
    persona = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
    stop("Nivel no soportado: ", nivel)
  )
}

.fuentes_llave_nivel_sabana <- function(dfs, nivel, casos) {
  keys <- .llaves_nivel_sabana_recuperables(nivel)
  n_col <- paste0("n_capitulos_", nivel, "_sospechosos")
  caps_col <- paste0("capitulos_", nivel, "_sospechosos")

  empty <- tibble::as_tibble(stats::setNames(
    rep(list(character()), length(keys)),
    keys
  ))
  empty[[n_col]] <- integer()
  empty[[caps_col]] <- character()

  if (!is.list(dfs) || !is.data.frame(casos) || nrow(casos) == 0) {
    return(empty)
  }

  casos_keys <- casos %>%
    normalize_keys(keys) %>%
    dplyr::select(dplyr::all_of(keys)) %>%
    dplyr::distinct()

  for (key in keys) {
    casos_keys <- casos_keys %>%
      dplyr::filter(!is.na(.data[[key]]), nzchar(.data[[key]]))
  }

  if (nrow(casos_keys) == 0) {
    return(empty)
  }

  caps_nivel <- names(dfs)[vapply(
    names(dfs),
    function(cap) {
      tipo_ok <- !is.null(tipo_capitulo[[cap]]) && identical(tipo_capitulo[[cap]], nivel)
      keys_cap <- tryCatch(get_join_keys(cap), error = function(e) character())
      cols_ok <- is.data.frame(dfs[[cap]]) && all(keys_cap %in% names(dfs[[cap]]))
      tipo_ok && cols_ok
    },
    logical(1)
  )]

  if (length(caps_nivel) == 0) {
    return(empty)
  }

  fuentes <- dplyr::bind_rows(lapply(caps_nivel, function(cap) {
    dfs[[cap]] %>%
      normalize_keys(keys) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::semi_join(casos_keys, by = keys) %>%
      dplyr::mutate(capitulo_sospechoso = cap)
  }))

  if (nrow(fuentes) == 0) {
    return(empty)
  }

  fuentes %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      !!n_col := dplyr::n_distinct(.data$capitulo_sospechoso),
      !!caps_col := .collapse_tokens_em_completa(.data$capitulo_sospechoso),
      .groups = "drop"
    )
}

.construir_controles_recuperabilidad_sabana <- function(dfs,
                                                        reporte_final_caidas = NULL,
                                                        cap_hog = "C",
                                                        cap_per = "E") {
  cap_hog <- toupper(cap_hog)
  cap_per <- toupper(cap_per)

  C <- if (cap_hog %in% names(dfs) && is.data.frame(dfs[[cap_hog]])) {
    dfs[[cap_hog]]
  } else {
    tibble::tibble()
  }

  E <- if (cap_per %in% names(dfs) && is.data.frame(dfs[[cap_per]])) {
    dfs[[cap_per]]
  } else {
    tibble::tibble()
  }

  control_hogar <- .control_hogar_recuperabilidad_sabana(C)
  controles_e <- .control_persona_recuperabilidad_sabana(E)
  reporte_final <- .reporte_final_recuperabilidad_sabana(reporte_final_caidas)

  list(
    control_hogar = control_hogar,
    directorios_e = controles_e$directorios_e,
    hogares_e = controles_e$hogares_e,
    personas_e = controles_e$personas_e,
    resumen_hogar_e = controles_e$resumen_hogar_e,
    reporte_final = reporte_final
  )
}

.control_hogar_recuperabilidad_sabana <- function(C) {
  out_empty <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    hogar_existe_en_C = logical(),
    NHCCPCTRL1 = numeric(),
    NHCCPCTRL1A = character(),
    NHCCPCTRL2 = numeric(),
    RES_HOG = numeric(),
    hogar_operativo_completo = logical()
  )

  if (!is.data.frame(C) ||
      nrow(C) == 0 ||
      !all(c("DIRECTORIO", "SECUENCIA_P") %in% names(C))) {
    return(out_empty)
  }

  C <- normalize_keys(C, c("DIRECTORIO", "SECUENCIA_P"))

  for (v in c("NHCCPCTRL1", "NHCCPCTRL1A", "NHCCPCTRL2", "RES_HOG")) {
    if (!v %in% names(C)) {
      C[[v]] <- NA
    }
  }

  C %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO = .data$DIRECTORIO,
      SECUENCIA_P = .data$SECUENCIA_P,
      hogar_existe_en_C = TRUE,
      NHCCPCTRL1 = suppressWarnings(as.numeric(.data$NHCCPCTRL1)),
      NHCCPCTRL1A = as.character(.data$NHCCPCTRL1A),
      NHCCPCTRL2 = suppressWarnings(as.numeric(.data$NHCCPCTRL2)),
      RES_HOG = suppressWarnings(as.numeric(.data$RES_HOG)),
      hogar_operativo_completo = .data$NHCCPCTRL1 == 1 & .data$RES_HOG == 1
    )
}

.control_persona_recuperabilidad_sabana <- function(E) {
  empty_directorios <- tibble::tibble(
    DIRECTORIO = character(),
    directorio_existe_en_E = logical()
  )
  empty_hogares <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    hogar_existe_en_E = logical()
  )
  empty_personas <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    ORDEN = character(),
    orden_existe_en_E = logical(),
    NPCEPCTRL1 = numeric(),
    NPCEPCTRL1A = character(),
    RES_PER = numeric(),
    NPCEP6 = character(),
    persona_operativa_completa = logical(),
    persona_lina_basica_completa = logical()
  )
  empty_resumen <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    n_personas_E = integer(),
    max_orden_E = integer()
  )

  if (!is.data.frame(E) ||
      nrow(E) == 0 ||
      !all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(E))) {
    return(list(
      directorios_e = empty_directorios,
      hogares_e = empty_hogares,
      personas_e = empty_personas,
      resumen_hogar_e = empty_resumen
    ))
  }

  E <- normalize_keys(E, c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  for (v in c("NPCEPCTRL1", "NPCEPCTRL1A", "RES_PER", "NPCEP6")) {
    if (!v %in% names(E)) {
      E[[v]] <- NA
    }
  }

  personas_e <- E %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO = .data$DIRECTORIO,
      SECUENCIA_P = .data$SECUENCIA_P,
      ORDEN = .data$ORDEN,
      orden_existe_en_E = TRUE,
      NPCEPCTRL1 = suppressWarnings(as.numeric(.data$NPCEPCTRL1)),
      NPCEPCTRL1A = as.character(.data$NPCEPCTRL1A),
      RES_PER = suppressWarnings(as.numeric(.data$RES_PER)),
      NPCEP6 = as.character(.data$NPCEP6),
      persona_operativa_completa = .data$NPCEPCTRL1 == 1 & .data$RES_PER == 1,
      persona_lina_basica_completa = .data$NPCEPCTRL1 == 1 & .valor_no_vacio(.data$NPCEP6)
    )

  resumen_hogar_e <- E %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    dplyr::mutate(ORDEN_num = suppressWarnings(as.integer(.data$ORDEN))) %>%
    dplyr::group_by(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
    dplyr::summarise(
      n_personas_E = dplyr::n_distinct(.data$ORDEN),
      max_orden_E = suppressWarnings(max(.data$ORDEN_num, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      max_orden_E = dplyr::if_else(is.infinite(.data$max_orden_E), NA_integer_, as.integer(.data$max_orden_E))
    )

  list(
    directorios_e = E %>%
      dplyr::distinct(.data$DIRECTORIO) %>%
      dplyr::mutate(directorio_existe_en_E = TRUE),
    hogares_e = E %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P) %>%
      dplyr::mutate(hogar_existe_en_E = TRUE),
    personas_e = personas_e,
    resumen_hogar_e = resumen_hogar_e
  )
}

.reporte_final_recuperabilidad_sabana <- function(reporte_final_caidas) {
  empty <- tibble::tibble(
    DIRECTORIO = character(),
    SECUENCIA_P = character(),
    ORDEN = character(),
    tiene_reporte_final = logical(),
    cae_existencia = logical(),
    cae_lina = logical(),
    cae_campo = logical(),
    cae_duplicado = logical(),
    cae_tematica = logical(),
    n_criterios_reporte = integer(),
    criterios_reporte = character(),
    criterio_principal_reporte = character(),
    razon_principal_caida = character(),
    variable_principal_caida = character(),
    valor_principal_caida = character(),
    observacion_final = character()
  )

  if (is.null(reporte_final_caidas) ||
      !is.data.frame(reporte_final_caidas) ||
      nrow(reporte_final_caidas) == 0 ||
      !all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(reporte_final_caidas))) {
    return(empty)
  }

  reporte <- normalize_keys(reporte_final_caidas, c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))

  for (v in c("cae_existencia", "cae_lina", "cae_campo", "cae_duplicado", "cae_tematica")) {
    if (!v %in% names(reporte)) {
      reporte[[v]] <- FALSE
    }
  }

  for (v in c(
    "criterios_reporte", "criterio_principal_reporte", "razon_principal_caida",
    "variable_principal_caida", "valor_principal_caida", "observacion_final"
  )) {
    if (!v %in% names(reporte)) {
      reporte[[v]] <- NA_character_
    }
  }

  if (!"n_criterios_reporte" %in% names(reporte)) {
    reporte$n_criterios_reporte <- NA_integer_
  }

  reporte %>%
    dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN, .keep_all = TRUE) %>%
    dplyr::transmute(
      DIRECTORIO = .data$DIRECTORIO,
      SECUENCIA_P = .data$SECUENCIA_P,
      ORDEN = .data$ORDEN,
      tiene_reporte_final = TRUE,
      cae_existencia = .coerce_flag_base_em_completa(.data$cae_existencia),
      cae_lina = .coerce_flag_base_em_completa(.data$cae_lina),
      cae_campo = .coerce_flag_base_em_completa(.data$cae_campo),
      cae_duplicado = .coerce_flag_base_em_completa(.data$cae_duplicado),
      cae_tematica = .coerce_flag_base_em_completa(.data$cae_tematica),
      n_criterios_reporte = as.integer(.data$n_criterios_reporte),
      criterios_reporte = as.character(.data$criterios_reporte),
      criterio_principal_reporte = as.character(.data$criterio_principal_reporte),
      razon_principal_caida = as.character(.data$razon_principal_caida),
      variable_principal_caida = as.character(.data$variable_principal_caida),
      valor_principal_caida = as.character(.data$valor_principal_caida),
      observacion_final = as.character(.data$observacion_final)
    )
}

.clasificar_recuperabilidad_sabana <- function(df) {
  if (nrow(df) == 0) {
    return(df %>%
      dplyr::mutate(
        auditable_llave = logical(),
        recuperable_potencial = logical(),
        estado_recuperacion = character(),
        motivo_estado_recuperacion = character()
      ))
  }

  df %>%
    dplyr::mutate(
      auditable_llave = TRUE,
      directorio_existe_en_E = dplyr::coalesce(.data$directorio_existe_en_E, FALSE),
      hogar_existe_en_E = dplyr::coalesce(.data$hogar_existe_en_E, FALSE),
      orden_existe_en_E = dplyr::coalesce(.data$orden_existe_en_E, FALSE),
      hogar_existe_en_C = dplyr::coalesce(.data$hogar_existe_en_C, FALSE),
      tiene_reporte_final = dplyr::coalesce(.data$tiene_reporte_final, FALSE),
      cae_existencia = dplyr::coalesce(.data$cae_existencia, FALSE),
      cae_lina = dplyr::coalesce(.data$cae_lina, FALSE),
      cae_campo = dplyr::coalesce(.data$cae_campo, FALSE),
      cae_duplicado = dplyr::coalesce(.data$cae_duplicado, FALSE),
      cae_tematica = dplyr::coalesce(.data$cae_tematica, FALSE),
      n_capitulos_persona_sospechosos = dplyr::coalesce(.data$n_capitulos_persona_sospechosos, 0L),
      n_capitulos_hogar_sospechosos = dplyr::coalesce(.data$n_capitulos_hogar_sospechosos, 0L),
      control_hogar_sugiere_mas_personas = dplyr::coalesce(.data$control_hogar_sugiere_mas_personas, FALSE),
      bloqueo_campo_hogar =
        .data$hogar_existe_en_C &
        !dplyr::coalesce(.data$hogar_operativo_completo, FALSE),
      bloqueo_campo_persona =
        .data$orden_existe_en_E &
        !dplyr::coalesce(.data$persona_operativa_completa, TRUE),
      bloqueo_lina_persona =
        .data$orden_existe_en_E &
        !dplyr::coalesce(.data$persona_lina_basica_completa, TRUE),
      bloqueo_duplicado = .data$cae_duplicado,
      bloqueo_tematica = .data$cae_tematica,
      tiene_bloqueos_independientes =
        .data$bloqueo_campo_hogar |
        .data$bloqueo_campo_persona |
        .data$bloqueo_lina_persona |
        .data$bloqueo_duplicado |
        .data$bloqueo_tematica,
      precondicion_llave_ok = dplyr::case_when(
        .data$tipo_recuperacion == "SECUENCIA_P" ~
          .data$directorio_existe_en_E &
          .data$hogar_existe_en_C &
          dplyr::coalesce(.data$hogar_operativo_completo, FALSE),
        .data$tipo_recuperacion == "ORDEN" ~
          .data$hogar_existe_en_E &
          .data$n_capitulos_persona_sospechosos > 0L &
          .data$control_hogar_sugiere_mas_personas &
          (
            !.data$hogar_existe_en_C |
              dplyr::coalesce(.data$hogar_operativo_completo, FALSE)
          ),
        TRUE ~ FALSE
      ),
      recuperable_potencial =
        .data$auditable_llave &
        .data$tiene_reporte_final &
        .data$precondicion_llave_ok &
        !.data$tiene_bloqueos_independientes,
      estado_recuperacion = dplyr::case_when(
        .data$recuperable_potencial ~ "recuperable_potencial",
        .data$tipo_recuperacion == "ORDEN" & .data$n_capitulos_persona_sospechosos == 0L ~
          "auditable_no_recuperable_sin_evidencia_persona",
        .data$tipo_recuperacion == "ORDEN" & !.data$control_hogar_sugiere_mas_personas ~
          "auditable_no_recuperable_control_hogar_no_sugiere_persona",
        .data$bloqueo_campo_hogar ~ "auditable_no_recuperable_campo_hogar",
        .data$bloqueo_campo_persona ~ "auditable_no_recuperable_campo_persona",
        .data$bloqueo_lina_persona ~ "auditable_no_recuperable_lina_persona",
        .data$bloqueo_duplicado ~ "auditable_no_recuperable_duplicado",
        .data$bloqueo_tematica ~ "auditable_no_recuperable_tematica",
        !.data$tiene_reporte_final ~ "auditable_indeterminado_sin_diagnostico_completo",
        .data$tipo_recuperacion == "SECUENCIA_P" & !.data$directorio_existe_en_E ~
          "no_auditable_directorio_no_existe_E",
        .data$tipo_recuperacion == "SECUENCIA_P" & !.data$hogar_existe_en_C ~
          "auditable_indeterminado_sin_control_hogar",
        .data$tipo_recuperacion == "ORDEN" & !.data$hogar_existe_en_E ~
          "auditable_indeterminado_hogar_no_observado_E",
        TRUE ~ "auditable_indeterminado"
      ),
      motivo_estado_recuperacion = dplyr::case_when(
        .data$recuperable_potencial ~
          "El desajuste de llave es compatible con las caidas observadas y no se detectan bloqueos independientes.",
        .data$tipo_recuperacion == "ORDEN" & .data$n_capitulos_persona_sospechosos == 0L ~
          "El ORDEN sospechoso no aparece en capitulos persona reales; la evidencia de capitulos hogar no crea personas recuperables.",
        .data$tipo_recuperacion == "ORDEN" & !.data$control_hogar_sugiere_mas_personas ~ paste0(
          "El control de hogar no sugiere personas adicionales: NHCCPCTRL2=",
          .texto_valor_sabana_recuperables(.data$NHCCPCTRL2),
          " y max_orden_E=",
          .texto_valor_sabana_recuperables(.data$max_orden_E),
          "."
        ),
        .data$bloqueo_campo_hogar ~ paste0(
          "Hogar no recuperable por campo: NHCCPCTRL1=",
          .texto_valor_sabana_recuperables(.data$NHCCPCTRL1),
          "; RES_HOG=",
          .texto_valor_sabana_recuperables(.data$RES_HOG),
          "."
        ),
        .data$bloqueo_campo_persona ~ paste0(
          "Persona no recuperable por campo: NPCEPCTRL1=",
          .texto_valor_sabana_recuperables(.data$NPCEPCTRL1),
          "; RES_PER=",
          .texto_valor_sabana_recuperables(.data$RES_PER),
          "."
        ),
        .data$bloqueo_lina_persona ~ paste0(
          "Persona no recuperable por Lina: NPCEP6=",
          .texto_valor_sabana_recuperables(.data$NPCEP6),
          "."
        ),
        .data$bloqueo_duplicado ~ "Caso no recuperable por desajuste de llave porque tambien cae por duplicado.",
        .data$bloqueo_tematica ~ "Caso no recuperable por desajuste de llave porque tambien cae por tematica.",
        !.data$tiene_reporte_final ~ "No se suministro diagnostico completo para separar caidas inducidas por llave de caidas independientes.",
        .data$tipo_recuperacion == "SECUENCIA_P" & !.data$directorio_existe_en_E ~
          "El DIRECTORIO no existe en E; no es una recuperacion simple por SECUENCIA_P.",
        .data$tipo_recuperacion == "SECUENCIA_P" & !.data$hogar_existe_en_C ~
          "El hogar no tiene registro de control en C para confirmar recuperabilidad.",
        .data$tipo_recuperacion == "ORDEN" & !.data$hogar_existe_en_E ~
          "El hogar del ORDEN sospechoso no existe en E; debe revisarse como problema de SECUENCIA_P.",
        TRUE ~ "No hay informacion suficiente para confirmar recuperabilidad."
      )
    )
}

.texto_valor_sabana_recuperables <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[is.na(x_chr) | x_chr == "" | x_chr == "NA"] <- "vacia"
  x_chr
}

.collapse_tokens_em_completa <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(sort(unique(x)), collapse = ", ")
}
