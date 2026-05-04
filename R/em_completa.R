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
#' Consolida en una sabana de auditoria los casos recuperables detectados en la
#' validacion de flujo persona, separando problemas de `SECUENCIA_P` y de
#' `ORDEN`, y exportando todas las filas de los capitulos involucrados para los
#' `DIRECTORIO` afectados.
#'
#' @param diag_orden_fuera_E Data frame con el detalle de personas no
#'   observadas en `E`, incluyendo `tipo_problema`.
#' @param diag_secuencia Data frame con el diagnostico de cruce por
#'   `DIRECTORIO + SECUENCIA_P`, incluyendo `directorio_existe_en_E`.
#' @param dfs Lista de data frames por capitulo.
#' @param carpeta_raiz Carpeta donde se exportara la sabana.
#' @param archivo Nombre del archivo Excel de salida.
#'
#' @details
#' La funcion no modifica la logica productiva del paquete. Su proposito es
#' construir una sabana manual de auditoria con los casos potencialmente
#' recuperables:
#'
#' - hogares no observados en `E` cuyo `DIRECTORIO` si existe en `E`;
#' - personas con `ORDEN` mayor al numero de personas observadas en `E`.
#'
#' Para cada `DIRECTORIO` recuperable, la exportacion incluye todas las filas de
#' cada capitulo presente en `dfs`, junto con dos hojas de control:
#' `casos_recuperables` y `resumen_directorios`.
#'
#' @return
#' Una lista con:
#' \describe{
#'   \item{casos_recuperables}{Tibble consolidado con los casos recuperables.}
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
    archivo = "sabana_auditoria_casos_recuperables.xlsx"
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

  casos_orden_recuperables <- diag_orden_fuera_E %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::filter(
      .data$tipo_problema == "ORDEN mayor que personas observadas en E"
    ) %>%
    dplyr::mutate(
      tipo_recuperacion = "ORDEN"
    )

  casos_secuencia_recuperables <- diag_orden_fuera_E %>%
    normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
    dplyr::filter(
      .data$tipo_problema == "Hogar no observado en E"
    ) %>%
    dplyr::semi_join(
      diag_secuencia %>%
        normalize_keys(c("DIRECTORIO", "SECUENCIA_P")) %>%
        dplyr::filter(.coerce_flag_base_em_completa(.data$directorio_existe_en_E)),
      by = c("DIRECTORIO", "SECUENCIA_P")
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

  caps_con_llave_persona <- names(dfs)[vapply(
    dfs,
    function(x) is.data.frame(x) && all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN") %in% names(x)),
    logical(1)
  )]

  fuentes_llave <- dplyr::bind_rows(lapply(caps_con_llave_persona, function(cap) {
    dfs[[cap]] %>%
      normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
      dplyr::mutate(capitulo_sospechoso = cap)
  })) %>%
    dplyr::semi_join(
      casos_recuperables %>%
        normalize_keys(c("DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>%
        dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN),
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    ) %>%
    dplyr::group_by(.data$DIRECTORIO, .data$SECUENCIA_P, .data$ORDEN) %>%
    dplyr::summarise(
      n_capitulos_sospechosos = dplyr::n_distinct(.data$capitulo_sospechoso),
      capitulos_sospechosos = .collapse_tokens_em_completa(.data$capitulo_sospechoso),
      .groups = "drop"
    )

  casos_recuperables <- casos_recuperables %>%
    dplyr::left_join(
      fuentes_llave,
      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    )

  resumen_directorios <- casos_recuperables %>%
    dplyr::group_by(.data$DIRECTORIO) %>%
    dplyr::summarise(
      n_llaves_auditadas = dplyr::n(),
      secuencias_afectadas = .collapse_tokens_em_completa(.data$SECUENCIA_P),
      ordenes_afectados = .collapse_tokens_em_completa(.data$ORDEN),
      tipos_recuperacion = .collapse_tokens_em_completa(.data$tipo_recuperacion),
      capitulos_sospechosos = .collapse_tokens_em_completa(
        unlist(strsplit(
          .data$capitulos_sospechosos[!is.na(.data$capitulos_sospechosos)],
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
    "resumen_directorios"
  ))

  for (hoja in hojas_exportar) {
    openxlsx::addWorksheet(wb, hoja)

    if (hoja == "casos_recuperables") {
      datos_hoja <- casos_recuperables
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
    resumen_directorios = resumen_directorios,
    ruta = ruta_sabana
  )
}

.collapse_tokens_em_completa <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(sort(unique(x)), collapse = ", ")
}
