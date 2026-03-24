#' Diagnosticar el pegue entre dos capítulos de la encuesta
#'
#' Evalúa la consistencia de llaves entre dos capítulos de una lista de bases,
#' identificando cuántos registros pegan entre sí y cuáles aparecen solo en uno
#' de los dos capítulos.
#'
#' La función:
#' \itemize{
#'   \item determina automáticamente las llaves esperadas para cada capítulo,
#'   \item usa la intersección de esas llaves como base de comparación,
#'   \item realiza un pegue tipo \code{left} desde \code{cap1} hacia \code{cap2},
#'   \item resume cuántas llaves pegan y cuántas quedan solo en uno u otro capítulo,
#'   \item devuelve el detalle de las llaves que no pegan.
#' }
#'
#' @param dfs Lista nombrada de data frames con los capítulos de la encuesta.
#' @param cap1 Capítulo base para el diagnóstico. Se usa como referencia del pegue.
#' @param cap2 Capítulo a contrastar contra \code{cap1}.
#'
#' @details
#' Las llaves usadas para el diagnóstico se obtienen con \code{get_join_keys()}
#' según la tipología del capítulo:
#' \itemize{
#'   \item vivienda: \code{DIRECTORIO}
#'   \item hogar: \code{DIRECTORIO + SECUENCIA_P}
#'   \item persona: \code{DIRECTORIO + SECUENCIA_P + ORDEN}
#' }
#'
#' Para comparar los capítulos, la función toma la intersección de las llaves
#' esperadas en \code{cap1} y \code{cap2}. Luego:
#' \itemize{
#'   \item identifica llaves presentes en ambos capítulos,
#'   \item llaves presentes solo en \code{cap1},
#'   \item llaves presentes solo en \code{cap2}.
#' }
#'
#' Además, ejecuta internamente \code{pegar_tablas()} para devolver el resultado
#' del pegue y su resumen estructural.
#'
#' @return
#' Una lista con cuatro elementos:
#' \describe{
#'   \item{data}{Base resultante del pegue entre \code{cap1} y \code{cap2}
#'   usando \code{pegar_tablas()}.}
#'
#'   \item{resumen_pega}{Resumen estructural del pegue generado por
#'   \code{pegar_tablas()}.}
#'
#'   \item{resumen_llaves}{Tabla resumen con el número de llaves que:
#'   pegan en ambos capítulos, aparecen solo en \code{cap1}, aparecen solo en
#'   \code{cap2} y el total de llaves comparadas.}
#'
#'   \item{no_pegan}{Detalle de las llaves que no pegan, con una variable
#'   \code{tipo_no_pega} que indica si el registro está en \code{cap1} pero no
#'   en \code{cap2}, o viceversa.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- diagnostico_pega_caps(dfs, "E", "F")
#'
#' res$resumen_llaves
#' head(res$no_pegan)
#' }
#'
#' @author
#' David Gómez Lizarazú
#'
#' @export
diagnostico_pega_caps <- function(dfs, cap1, cap2) {

  cap1 <- toupper(cap1)
  cap2 <- toupper(cap2)

  keys_1 <- get_join_keys(cap1)
  keys_2 <- get_join_keys(cap2)
  keys_use <- intersect(keys_1, keys_2)

  pega <- pegar_tablas(
    dfs = dfs[c(cap1, cap2)],
    base_cap = cap1,
    join = "left"
  )

  if (!all(c(cap1, cap2) %in% names(dfs))) {
    stop("Ambos capítulos deben existir en la lista dfs")
  }

  df1_keys <- dfs[[cap1]] %>%
    normalize_keys(keys_use) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys_use))) %>%
    dplyr::mutate(en_1 = 1L)

  df2_keys <- dfs[[cap2]] %>%
    normalize_keys(keys_use) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys_use))) %>%
    dplyr::mutate(en_2 = 1L)

  comp <- dplyr::full_join(df1_keys, df2_keys, by = keys_use)

  resumen_llaves <- comp %>%
    dplyr::summarise(
      pegan  = sum(!is.na(en_1) & !is.na(en_2)),
      solo_1 = sum(!is.na(en_1) &  is.na(en_2)),
      solo_2 = sum( is.na(en_1) & !is.na(en_2)),
      total = dplyr::n()
    )

  no_pegan <- dplyr::bind_rows(
    comp %>% dplyr::filter(!is.na(en_1) & is.na(en_2)) %>% dplyr::mutate(tipo_no_pega = paste0(cap1, "_sin_", cap2)),
    comp %>% dplyr::filter(is.na(en_1) & !is.na(en_2)) %>% dplyr::mutate(tipo_no_pega = paste0(cap2, "_sin_", cap1))
  )

  list(
    data = pega$data,
    resumen_pega = pega$resumen,
    resumen_llaves = resumen_llaves,
    no_pegan = no_pegan
  )
}


#' Diagnóstico secuencial del flujo de capítulos
#'
#' Realiza un diagnóstico secuencial del cruce entre capítulos de la encuesta.
#' En cada paso compara la base acumulada contra un nuevo capítulo, identifica
#' qué llaves pegan y cuáles quedan solo en uno u otro lado, y luego actualiza
#' el acumulado mediante un pegue secuencial.
#'
#' @param dfs Lista nombrada de data frames con los capítulos de la encuesta.
#' @param caps_orden Vector de nombres de capítulos en el orden del flujo.
#'   Ejemplo: c("A", "B", "C", "D").
#' @param join Tipo de unión para construir el acumulado. Puede ser
#'   "left", "full" o "inner". Por defecto "left".
#'
#' @details
#' La función sigue la lógica del paquete:
#' \itemize{
#'   \item usa \code{get_join_keys()} para obtener las llaves esperadas de cada capítulo,
#'   \item usa \code{normalize_keys()} antes de comparar las llaves,
#'   \item en cada paso toma la intersección entre las llaves del acumulado y las del
#'   capítulo nuevo,
#'   \item identifica registros que pegan, que están solo en la base acumulada o que están
#'   solo en el capítulo nuevo,
#'   \item luego actualiza el acumulado con un pegue secuencial.
#' }
#'
#' @return Una lista con cuatro elementos:
#' \describe{
#'   \item{resumen}{Tabla resumen por paso con el número de llaves que pegan,
#'   quedan solo en la base o solo en el capítulo nuevo.}
#'   \item{no_pegan}{Detalle de las llaves que no cruzan en cada paso, con la
#'   variable \code{tipo_no_pega}.}
#'   \item{acumulados}{Lista de bases acumuladas intermedias.}
#'   \item{acumulado_final}{Base acumulada final.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- diagnostico_flujo_caps(dfs, c("A", "B", "C", "D"))
#'
#' res$resumen
#' head(res$no_pegan)
#' names(res$acumulados)
#' }
#'
#' @author
#' David Gómez Lizarazú
#'
#' @export
diagnostico_flujo_caps <- function(dfs, caps_orden, join = "left") {

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres.")
  }

  if (!is.character(caps_orden) || length(caps_orden) < 2) {
    stop("`caps_orden` debe ser un vector de nombres de capítulos con longitud >= 2.")
  }

  if (!join %in% c("left", "full", "inner")) {
    stop("`join` debe ser 'left', 'full' o 'inner'.")
  }

  names(dfs) <- toupper(names(dfs))
  caps_orden <- toupper(caps_orden)

  if (!all(caps_orden %in% names(dfs))) {
    faltan <- setdiff(caps_orden, names(dfs))
    stop("Estos capítulos no están en `dfs`: ", paste(faltan, collapse = ", "))
  }

  acumular_join <- function(x, y, by, join = "left") {
    if (join == "left") {
      dplyr::left_join(x, y, by = by)
    } else if (join == "full") {
      dplyr::full_join(x, y, by = by)
    } else {
      dplyr::inner_join(x, y, by = by)
    }
  }

  cap_base <- caps_orden[1]
  acumulado <- dfs[[cap_base]]
  nombre_acumulado <- cap_base

  resumen_lista <- list()
  no_pegan_lista <- list()
  acumulados_lista <- list()
  acumulados_lista[[nombre_acumulado]] <- acumulado

  for (i in 2:length(caps_orden)) {

    cap_nuevo <- caps_orden[i]
    df_nuevo <- dfs[[cap_nuevo]]

    keys_acum <- intersect(c("DIRECTORIO", "SECUENCIA_P", "ORDEN"), names(acumulado))
    keys_nuevo <- get_join_keys(cap_nuevo)
    keys_use <- intersect(keys_acum, keys_nuevo)

    if (length(keys_use) == 0) {
      stop(
        "No hay llaves comunes entre la base acumulada (", nombre_acumulado,
        ") y el capítulo ", cap_nuevo, "."
      )
    }

    df1_keys <- acumulado %>%
      normalize_keys(keys_use) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys_use))) %>%
      dplyr::mutate(en_base = 1L)

    df2_keys <- df_nuevo %>%
      normalize_keys(keys_use) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys_use))) %>%
      dplyr::mutate(en_nuevo = 1L)

    comp <- dplyr::full_join(df1_keys, df2_keys, by = keys_use)

    resumen_paso <- comp %>%
      dplyr::summarise(
        pegan = sum(!is.na(en_base) & !is.na(en_nuevo)),
        solo_base = sum(!is.na(en_base) & is.na(en_nuevo)),
        solo_nuevo = sum(is.na(en_base) & !is.na(en_nuevo)),
        total = dplyr::n()
      ) %>%
      dplyr::mutate(
        paso = i - 1,
        base_acumulada = nombre_acumulado,
        cap_nuevo = cap_nuevo,
        join_keys = paste(keys_use, collapse = ", "),
        n_base = nrow(df1_keys),
        n_nuevo = nrow(df2_keys),
        .before = 1
      )

    no_pegan_paso <- dplyr::bind_rows(
      comp %>%
        dplyr::filter(!is.na(en_base) & is.na(en_nuevo)) %>%
        dplyr::mutate(tipo_no_pega = paste0(nombre_acumulado, "_sin_", cap_nuevo)),
      comp %>%
        dplyr::filter(is.na(en_base) & !is.na(en_nuevo)) %>%
        dplyr::mutate(tipo_no_pega = paste0(cap_nuevo, "_sin_", nombre_acumulado))
    ) %>%
      dplyr::mutate(
        paso = i - 1,
        base_acumulada = nombre_acumulado,
        cap_nuevo = cap_nuevo,
        .before = 1
      )

    resumen_lista[[i - 1]] <- resumen_paso
    no_pegan_lista[[i - 1]] <- no_pegan_paso

    acumulado <- acumular_join(
      x = acumulado,
      y = df_nuevo,
      by = keys_use,
      join = join
    )

    nombre_acumulado <- paste0(nombre_acumulado, "_", cap_nuevo)
    acumulados_lista[[nombre_acumulado]] <- acumulado
  }

  list(
    resumen = dplyr::bind_rows(resumen_lista),
    no_pegan = dplyr::bind_rows(no_pegan_lista),
    acumulados = acumulados_lista,
    acumulado_final = acumulado
  )
}


#' Pipeline de encuestas completas por flujo secuencial
#'
#' Realiza un pegue secuencial entre capítulos usando `inner_join`,
#' de forma que en cada paso solo se conservan los registros que
#' cruzan entre la base acumulada y el nuevo capítulo.
#'
#' @param dfs Lista nombrada de data frames con los capítulos.
#' @param caps_orden Vector de nombres de capítulos en orden secuencial.
#'
#' @return Una lista con:
#' \describe{
#'   \item{resumen}{Tabla con el tamaño de la base en cada paso y la retención.}
#'   \item{acumulados}{Lista de bases acumuladas intermedias.}
#'   \item{final}{Base final luego del último cruce.}
#' }
#' @export
pipeline_encuestas_completas <- function(dfs, caps_orden) {

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres.")
  }

  if (!is.character(caps_orden) || length(caps_orden) < 2) {
    stop("`caps_orden` debe ser un vector de nombres de capítulos con longitud >= 2.")
  }

  names(dfs) <- toupper(names(dfs))
  caps_orden <- toupper(caps_orden)

  if (!all(caps_orden %in% names(dfs))) {
    faltan <- setdiff(caps_orden, names(dfs))
    stop("Estos capítulos no están en `dfs`: ", paste(faltan, collapse = ", "))
  }

  llaves_std <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")

  acumulado <- dfs[[caps_orden[1]]]
  nombre_acumulado <- caps_orden[1]

  acumulados <- list()
  acumulados[[nombre_acumulado]] <- acumulado

  resumen <- list()

  # fila inicial
  keys_ini <- intersect(llaves_std, names(acumulado))
  n_ini <- acumulado %>%
    normalize_keys(keys_ini) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys_ini))) %>%
    nrow()

  resumen[[1]] <- tibble::tibble(
    paso = 0,
    base_acumulada = caps_orden[1],
    cap_nuevo = NA_character_,
    join_keys = paste(keys_ini, collapse = ", "),
    n_base = n_ini,
    n_nuevo = NA_integer_,
    n_resultado = n_ini,
    caida_abs = 0,
    retencion_pct = 100
  )

  for (i in 2:length(caps_orden)) {
    cap_nuevo <- caps_orden[i]
    df_nuevo <- dfs[[cap_nuevo]]

    keys_acum <- intersect(llaves_std, names(acumulado))
    keys_nuevo <- get_join_keys(cap_nuevo)
    keys_use <- intersect(keys_acum, keys_nuevo)

    if (length(keys_use) == 0) {
      stop(
        "No hay llaves comunes entre la base acumulada (", nombre_acumulado,
        ") y el capítulo ", cap_nuevo, "."
      )
    }

    base_keys <- acumulado %>%
      normalize_keys(keys_use) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys_use)))

    nuevo_keys <- df_nuevo %>%
      normalize_keys(keys_use) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(keys_use)))

    n_base <- nrow(base_keys)
    n_nuevo <- nrow(nuevo_keys)

    # inner sobre llaves para medir supervivencia real
    result_keys <- dplyr::inner_join(base_keys, nuevo_keys, by = keys_use)
    n_resultado <- nrow(result_keys)

    acumulado <- dplyr::inner_join(acumulado, df_nuevo, by = keys_use)
    nombre_acumulado <- paste0(nombre_acumulado, "_", cap_nuevo)
    acumulados[[nombre_acumulado]] <- acumulado

    resumen[[i]] <- tibble::tibble(
      paso = i - 1,
      base_acumulada = paste(caps_orden[1:(i - 1)], collapse = "_"),
      cap_nuevo = cap_nuevo,
      join_keys = paste(keys_use, collapse = ", "),
      n_base = n_base,
      n_nuevo = n_nuevo,
      n_resultado = n_resultado,
      caida_abs = n_base - n_resultado,
      retencion_pct = round(100 * n_resultado / n_base, 2)
    )
  }

  list(
    resumen = dplyr::bind_rows(resumen),
    acumulados = acumulados,
    final = acumulado
  )
}


#' Exportar reporte de encuestas que se quedan en el flujo
#'
#' Genera un archivo Excel con el detalle de las encuestas que no logran
#' cruzar en algún paso del flujo secuencial de capítulos. El reporte incluye
#' un resumen del flujo, el listado único de encuestas caídas y una base a
#' nivel de personas con todas las variables disponibles en una base ancha.
#'
#' @param dfs Lista nombrada de data frames con los capítulos.
#' @param caps_orden Vector de nombres de capítulos en orden secuencial.
#' @param base_cap_persona Capítulo base para construir la base ancha a nivel
#'   persona. Por defecto `"F"`.
#' @param caps_persona_orden Vector opcional con el orden de capítulos para
#'   construir la base ancha de personas. Si es `NULL`, se usa `caps_orden`.
#' @param archivo Ruta del archivo Excel de salida.
#' @param join_flujo Tipo de unión para el diagnóstico secuencial.
#'   Puede ser `"left"` o `"inner"`. Por defecto `"left"`.
#' @param quedarse_con Qué paso de caída conservar por encuesta.
#'   Puede ser `"primero"` o `"ultimo"`.
#' @param edad_var Nombre de la variable de edad para `pegar_tablas()`.
#'
#' @return Una lista con:
#' \describe{
#'   \item{resumen_flujo}{Resumen del diagnóstico secuencial.}
#'   \item{encuestas_caidas}{Encuestas únicas que no cruzan en algún paso.}
#'   \item{personas_caidas}{Base a nivel de personas de las encuestas caídas.}
#'   \item{archivo}{Ruta del Excel generado.}
#' }
#'
#' @export
exportar_reporte_encuestas_caidas <- function(
    dfs,
    caps_orden,
    base_cap_persona = "F",
    caps_persona_orden = NULL,
    archivo = "encuestas_caidas_flujo.xlsx",
    join_flujo = c("left", "inner"),
    quedarse_con = c("primero", "ultimo"),
    edad_var = "Edad"
) {

  join_flujo <- match.arg(join_flujo)
  quedarse_con <- match.arg(quedarse_con)

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Se requiere el paquete `openxlsx` para exportar el Excel.")
  }

  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe ser una lista con nombres.")
  }

  names(dfs) <- toupper(names(dfs))
  caps_orden <- toupper(caps_orden)
  base_cap_persona <- toupper(base_cap_persona)

  if (is.null(caps_persona_orden)) {
    caps_persona_orden <- caps_orden
  } else {
    caps_persona_orden <- toupper(caps_persona_orden)
  }

  if (!all(caps_orden %in% names(dfs))) {
    faltan <- setdiff(caps_orden, names(dfs))
    stop("Estos capítulos no están en `dfs`: ", paste(faltan, collapse = ", "))
  }

  if (!base_cap_persona %in% names(dfs)) {
    stop("`base_cap_persona` no está en `dfs`.")
  }

  if (!all(caps_persona_orden %in% names(dfs))) {
    faltan <- setdiff(caps_persona_orden, names(dfs))
    stop(
      "Estos capítulos de `caps_persona_orden` no están en `dfs`: ",
      paste(faltan, collapse = ", ")
    )
  }

  diag_flujo <- diagnostico_flujo_caps(
    dfs = dfs,
    caps_orden = caps_orden,
    join = join_flujo
  )

  no_pegan <- diag_flujo$no_pegan

  req_cols <- c("DIRECTORIO", "SECUENCIA_P", "paso", "base_acumulada", "cap_nuevo", "tipo_no_pega")
  if (nrow(no_pegan) > 0) {
    faltan_cols <- setdiff(req_cols, names(no_pegan))
    if (length(faltan_cols) > 0) {
      stop(
        "`diag_flujo$no_pegan` no contiene estas columnas requeridas: ",
        paste(faltan_cols, collapse = ", ")
      )
    }
  }

  if (nrow(no_pegan) == 0) {
    resumen_limpio <- diag_flujo$resumen %>%
      dplyr::mutate(dplyr::across(where(is.factor), as.character))
    resumen_limpio <- arreglar_utf8_df(resumen_limpio)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "resumen_flujo")
    openxlsx::writeData(wb, "resumen_flujo", resumen_limpio)
    openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)

    return(list(
      resumen_flujo = resumen_limpio,
      encuestas_caidas = data.frame(),
      personas_caidas = data.frame(),
      archivo = normalizePath(archivo, winslash = "/", mustWork = FALSE)
    ))
  }

  if (quedarse_con == "primero") {
    encuestas_caidas <- no_pegan %>%
      dplyr::arrange(.data$DIRECTORIO, .data$SECUENCIA_P, .data$paso) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE)
  } else {
    encuestas_caidas <- no_pegan %>%
      dplyr::arrange(.data$DIRECTORIO, .data$SECUENCIA_P, dplyr::desc(.data$paso)) %>%
      dplyr::distinct(.data$DIRECTORIO, .data$SECUENCIA_P, .keep_all = TRUE)
  }

  encuestas_caidas <- encuestas_caidas %>%
    dplyr::rename(
      paso_caida = .data$paso,
      origen_caida = .data$base_acumulada,
      capitulo_falla = .data$cap_nuevo
    )

  pega_personas <- pegar_tablas(
    dfs = dfs[caps_persona_orden],
    base_cap = base_cap_persona,
    caps_orden = caps_persona_orden,
    join = "left",
    edad_var = edad_var
  )

  if (!is.list(pega_personas) || !"data" %in% names(pega_personas)) {
    stop("`pegar_tablas()` no devolvió el componente `data` esperado.")
  }

  base_persona <- pega_personas$data

  req_persona <- c("DIRECTORIO", "SECUENCIA_P")
  faltan_persona <- setdiff(req_persona, names(base_persona))
  if (length(faltan_persona) > 0) {
    stop(
      "La base ancha de personas no contiene las columnas requeridas: ",
      paste(faltan_persona, collapse = ", ")
    )
  }

  personas_caidas <- base_persona %>%
    dplyr::semi_join(
      encuestas_caidas %>% dplyr::select(.data$DIRECTORIO, .data$SECUENCIA_P),
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::left_join(
      encuestas_caidas %>%
        dplyr::select(
          .data$DIRECTORIO,
          .data$SECUENCIA_P,
          .data$paso_caida,
          .data$origen_caida,
          .data$capitulo_falla,
          .data$tipo_no_pega
        ),
      by = c("DIRECTORIO", "SECUENCIA_P")
    ) %>%
    dplyr::relocate(
      .data$DIRECTORIO,
      .data$SECUENCIA_P,
      dplyr::any_of("ORDEN"),
      .data$paso_caida,
      .data$origen_caida,
      .data$capitulo_falla,
      .data$tipo_no_pega
    )

  resumen_falla <- encuestas_caidas %>%
    dplyr::count(.data$capitulo_falla, .data$tipo_no_pega, name = "n") %>%
    dplyr::arrange(dplyr::desc(.data$n))

  resumen_encuestas <- encuestas_caidas %>%
    dplyr::count(.data$capitulo_falla, name = "n_encuestas") %>%
    dplyr::arrange(dplyr::desc(.data$n_encuestas))

  # Forzar tipos seguros
  diag_flujo$resumen <- diag_flujo$resumen %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  resumen_falla <- resumen_falla %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  resumen_encuestas <- resumen_encuestas %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  encuestas_caidas <- encuestas_caidas %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  personas_caidas <- personas_caidas %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  # Limpiar UTF-8
  diag_flujo$resumen <- arreglar_utf8_df(diag_flujo$resumen)
  resumen_falla <- arreglar_utf8_df(resumen_falla)
  resumen_encuestas <- arreglar_utf8_df(resumen_encuestas)
  encuestas_caidas <- arreglar_utf8_df(encuestas_caidas)
  personas_caidas <- arreglar_utf8_df(personas_caidas)

  # Exportar a Excel
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "resumen_flujo")
  openxlsx::writeData(wb, "resumen_flujo", diag_flujo$resumen)

  openxlsx::addWorksheet(wb, "resumen_falla")
  openxlsx::writeData(wb, "resumen_falla", resumen_falla)

  openxlsx::addWorksheet(wb, "resumen_encuestas")
  openxlsx::writeData(wb, "resumen_encuestas", resumen_encuestas)

  openxlsx::addWorksheet(wb, "encuestas_caidas")
  openxlsx::writeData(wb, "encuestas_caidas", encuestas_caidas)

  openxlsx::addWorksheet(wb, "personas_caidas")
  openxlsx::writeData(wb, "personas_caidas", personas_caidas)

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)

  list(
    resumen_flujo = diag_flujo$resumen,
    encuestas_caidas = encuestas_caidas,
    personas_caidas = personas_caidas,
    archivo = normalizePath(archivo, winslash = "/", mustWork = FALSE)
  )
}

#' Exporta reporte de encuestas caídas por flujo y agrega hojas de campo
#'
#' Usa la función existente `exportar_reporte_encuestas_caidas()` para generar
#' el archivo base de caídas por flujo, y luego agrega hojas con las encuestas
#' que no cumplen criterio operativo de campo.
#'
#' @param dfs Lista de data frames por capítulo.
#' @param caps_orden Orden de capítulos para el flujo.
#' @param base_cap_persona Capítulo base de personas.
#' @param caps_persona_orden Orden de capítulos para la sábana.
#' @param archivo Nombre del archivo Excel a exportar.
#' @param join_flujo Tipo de join para flujo.
#' @param quedarse_con Estrategia de deduplicación.
#' @param edad_var Nombre de la variable edad.
#' @param res_campo Resultado de `diagnostico_completitud_campo(dfs)`.
#'   Si es NULL, se calcula internamente.
#' @param exportar_sabana_campo Si TRUE, agrega sábana completa para caídas de campo.
#' @param join_sabana_campo Tipo de join para sábana de campo.
#' @param quedarse_con_campo Estrategia de deduplicación para sábana de campo.
#'
#' @return Lista invisible con el archivo exportado y objetos creados.
#' @export
exportar_reporte_encuestas_caidas_campo <- function(
    dfs,
    caps_orden,
    base_cap_persona = "E",
    caps_persona_orden = caps_orden,
    archivo = "encuestas_caidas_flujo_y_campo.xlsx",
    join_flujo = "left",
    quedarse_con = "primero",
    edad_var = "Edad",
    res_campo = NULL,
    exportar_sabana_campo = TRUE,
    join_sabana_campo = "left",
    quedarse_con_campo = "primero"
) {

  if (!is.list(dfs)) {
    stop("`dfs` debe ser una lista.")
  }

  if (is.null(res_campo)) {
    res_campo <- diagnostico_completitud_campo(dfs)
  }

  limpiar_id <- function(x) {
    x %>%
      as.character() %>%
      stringr::str_trim()
  }

  limpiar_seg <- function(x) {
    x %>%
      as.character() %>%
      trimws()
  }

  # =========================================================
  # 1) Genera primero el archivo base con la función actual
  # =========================================================
  rep_flujo_obj <- exportar_reporte_encuestas_caidas(
    dfs = dfs,
    caps_orden = caps_orden,
    base_cap_persona = base_cap_persona,
    caps_persona_orden = caps_persona_orden,
    archivo = archivo,
    join_flujo = join_flujo,
    quedarse_con = quedarse_con,
    edad_var = edad_var
  )

  # =========================================================
  # 2) Construye objetos de campo
  # =========================================================
  encuestas_caidas_campo <- res_campo$base_eval %>%
    dplyr::filter(.data$encuesta_efectiva_campo & !.data$encuesta_completa_campo) %>%
    dplyr::mutate(
      motivo_principal = dplyr::case_when(
        !.data$viv_ocupada_presente ~ "vivienda",
        !.data$todos_hogares_completos ~ "hogar",
        !.data$todas_personas_completas ~ "persona",
        TRUE ~ "otro"
      ),
      motivo_detallado = dplyr::case_when(
        !.data$viv_ocupada_presente & !is.na(.data$estado_viv) ~
          paste0("vivienda_", .data$estado_viv),

        !.data$todos_hogares_completos &
          .data$hogares_incompletos > 0 &
          .data$personas_incompletas > 0 ~
          "hogar_incompleto_y_personas_incompletas",

        !.data$todos_hogares_completos &
          .data$hogares_incompletos > 0 &
          .data$personas_incompletas == 0 ~
          "hogar_incompleto_con_personas_completas",

        .data$todos_hogares_completos &
          !.data$todas_personas_completas &
          .data$personas_incompletas > 0 ~
          "persona_incompleta_con_hogar_completo",

        !.data$todos_hogares_completos ~
          "hogar_incompleto",

        !.data$todas_personas_completas ~
          "persona_incompleta",

        TRUE ~ "otro"
      ),
      criterio_falla = dplyr::case_when(
        !.data$viv_ocupada_presente ~
          paste0(
            "No cumple vivienda presente. estado_viv=",
            dplyr::coalesce(.data$estado_viv, "NA")
          ),

        !.data$todos_hogares_completos & !.data$todas_personas_completas ~
          paste0(
            "Falla hogar y persona. hogares_incompletos=",
            dplyr::coalesce(as.character(.data$hogares_incompletos), "NA"),
            "; personas_incompletas=",
            dplyr::coalesce(as.character(.data$personas_incompletas), "NA")
          ),

        !.data$todos_hogares_completos ~
          paste0(
            "Falla hogar. hogares_incompletos=",
            dplyr::coalesce(as.character(.data$hogares_incompletos), "NA"),
            "; todos_hogares_completos=",
            dplyr::coalesce(as.character(.data$todos_hogares_completos), "NA")
          ),

        !.data$todas_personas_completas ~
          paste0(
            "Falla persona. personas_incompletas=",
            dplyr::coalesce(as.character(.data$personas_incompletas), "NA"),
            "; todas_personas_completas=",
            dplyr::coalesce(as.character(.data$todas_personas_completas), "NA")
          ),

        TRUE ~ "Sin detalle"
      )
    )

  motivos_campo <- encuestas_caidas_campo %>%
    dplyr::count(.data$motivo_principal, .data$motivo_detallado, name = "n") %>%
    dplyr::group_by(.data$motivo_principal) %>%
    dplyr::mutate(pct_dentro_principal = .data$n / sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pct_total = .data$n / sum(.data$n)) %>%
    dplyr::arrange(dplyr::desc(.data$n))

  diag_hogar_persona <- encuestas_caidas_campo %>%
    dplyr::count(
      .data$todos_hogares_completos,
      .data$todas_personas_completas,
      name = "n"
    ) %>%
    dplyr::mutate(
      pct = .data$n / sum(.data$n)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n))

  # =========================================================
  # 3) Sábana completa de caídas de campo
  # =========================================================
  sabana_caidas_campo <- NULL

  if (isTRUE(exportar_sabana_campo)) {
    ids_caidas_campo <- encuestas_caidas_campo %>%
      dplyr::mutate(
        DIRECTORIO = limpiar_id(.data$DIRECTORIO)
      ) %>%
      dplyr::distinct(.data$DIRECTORIO) %>%
      dplyr::mutate(flag_caida_campo = TRUE)

    sabana_base <- pegar_tablas(
      dfs = dfs,
      caps_orden = caps_persona_orden,
      base_cap = base_cap_persona,
      join = join_sabana_campo,
      edad_var = edad_var
    )

    sabana_base <- sabana_base$data %>%
      dplyr::mutate(
        DIRECTORIO = limpiar_id(.data$DIRECTORIO)
      )

    meta_caidas_campo <- encuestas_caidas_campo %>%
      dplyr::mutate(
        DIRECTORIO = limpiar_id(.data$DIRECTORIO)
      ) %>%
      dplyr::select(
        dplyr::all_of(c(
          "DIRECTORIO",
          "UUID",
          "SEGMENTO",
          "CLASE",
          "encuesta_efectiva_campo",
          "encuesta_completa_campo",
          "viv_efectiva",
          "viv_ocupada_presente",
          "estado_viv",
          "n_hogares",
          "hogares_completos",
          "hogares_incompletos",
          "todos_hogares_completos",
          "n_personas",
          "personas_completas",
          "personas_incompletas",
          "todas_personas_completas",
          "motivo_principal",
          "motivo_detallado",
          "criterio_falla"
        ))
      ) %>%
      dplyr::distinct(.data$DIRECTORIO, .keep_all = TRUE)

    sabana_caidas_campo <- sabana_base %>%
      dplyr::inner_join(ids_caidas_campo, by = "DIRECTORIO") %>%
      dplyr::select(-.data$flag_caida_campo) %>%
      dplyr::left_join(meta_caidas_campo, by = "DIRECTORIO") %>%

      # 👇 AQUÍ agregas la bandera
      dplyr::mutate(
        flag_caida_campo = .data$encuesta_efectiva_campo & !.data$encuesta_completa_campo
      ) %>%

      # 👇 y luego ordenas columnas
      dplyr::relocate(
        dplyr::any_of(c(
          "DIRECTORIO",
          "UUID",
          "SEGMENTO",
          "CLASE",
          "flag_caida_campo",   # 👈 la dejas visible arriba
          "encuesta_efectiva_campo",
          "encuesta_completa_campo",
          "motivo_principal",
          "motivo_detallado",
          "criterio_falla",
          "viv_efectiva",
          "viv_ocupada_presente",
          "estado_viv",
          "n_hogares",
          "hogares_completos",
          "hogares_incompletos",
          "todos_hogares_completos",
          "n_personas",
          "personas_completas",
          "personas_incompletas",
          "todas_personas_completas"
        ))
      )
  }

  # =========================================================
  # 4) Limpiar tipos y UTF-8
  # =========================================================
  resumen_general_campo <- res_campo$resumen_general %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  resumen_segmento_campo <- res_campo$resumen_segmento %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  encuestas_caidas_campo <- encuestas_caidas_campo %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  motivos_campo <- motivos_campo %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  diag_hogar_persona <- diag_hogar_persona %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  if (!is.null(sabana_caidas_campo)) {
    sabana_caidas_campo <- sabana_caidas_campo %>%
      dplyr::mutate(
        dplyr::across(where(is.factor), as.character),
        dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
      )
  }

  resumen_general_campo <- arreglar_utf8_df(resumen_general_campo)
  resumen_segmento_campo <- arreglar_utf8_df(resumen_segmento_campo)
  encuestas_caidas_campo <- arreglar_utf8_df(encuestas_caidas_campo)
  motivos_campo <- arreglar_utf8_df(motivos_campo)
  diag_hogar_persona <- arreglar_utf8_df(diag_hogar_persona)

  if (!is.null(sabana_caidas_campo)) {
    sabana_caidas_campo <- arreglar_utf8_df(sabana_caidas_campo)
  }

  # =========================================================
  # 5) Cruce flujo vs campo por segmento (todos los segmentos)
  # =========================================================

  if (!("A" %in% names(dfs)) ||
      !all(c("DIRECTORIO", "SEGMENTO", "CLASE") %in% names(dfs[["A"]]))) {
    stop("No se encontró en A la información de DIRECTORIO, SEGMENTO y CLASE para construir el cruce por segmento.")
  }

  # Universo base por directorio desde A
  seg_a <- dfs[["A"]] %>%
    dplyr::distinct(DIRECTORIO, SEGMENTO, CLASE) %>%
    dplyr::mutate(
      DIRECTORIO = limpiar_id(.data$DIRECTORIO),
      SEGMENTO = limpiar_seg(.data$SEGMENTO),
      CLASE = limpiar_seg(.data$CLASE)
    )

  # IDs que caen por campo
  ids_campo <- encuestas_caidas_campo %>%
    dplyr::transmute(
      DIRECTORIO = limpiar_id(.data$DIRECTORIO),
      cae_campo = 1L
    ) %>%
    dplyr::distinct()

  # IDs que caen por flujo
  if (is.list(rep_flujo_obj) && "encuestas_caidas" %in% names(rep_flujo_obj)) {
    ids_flujo <- rep_flujo_obj$encuestas_caidas %>%
      dplyr::transmute(
        DIRECTORIO = limpiar_id(.data$DIRECTORIO),
        cae_flujo = 1L
      ) %>%
      dplyr::distinct()
  } else {
    ids_flujo <- tibble::tibble(
      DIRECTORIO = character(),
      cae_flujo = integer()
    )
  }

  # Cruce a nivel DIRECTORIO
  cruce_ids <- seg_a %>%
    dplyr::left_join(ids_flujo, by = "DIRECTORIO") %>%
    dplyr::left_join(ids_campo, by = "DIRECTORIO") %>%
    dplyr::mutate(
      cae_flujo = dplyr::coalesce(.data$cae_flujo, 0L),
      cae_campo = dplyr::coalesce(.data$cae_campo, 0L),
      solo_flujo = as.integer(.data$cae_flujo == 1L & .data$cae_campo == 0L),
      solo_campo = as.integer(.data$cae_flujo == 0L & .data$cae_campo == 1L),
      caidas_ambas = as.integer(.data$cae_flujo == 1L & .data$cae_campo == 1L),
      sin_caida = as.integer(.data$cae_flujo == 0L & .data$cae_campo == 0L)
    )

  # Resumen de caídas por segmento
  resumen_caidas_segmento <- cruce_ids %>%
    dplyr::group_by(.data$SEGMENTO, .data$CLASE) %>%
    dplyr::summarise(
      caidas_flujo = sum(.data$cae_flujo, na.rm = TRUE),
      caidas_campo = sum(.data$cae_campo, na.rm = TRUE),
      solo_flujo = sum(.data$solo_flujo, na.rm = TRUE),
      solo_campo = sum(.data$solo_campo, na.rm = TRUE),
      caidas_ambas = sum(.data$caidas_ambas, na.rm = TRUE),
      sin_caida = sum(.data$sin_caida, na.rm = TRUE),
      .groups = "drop"
    )

  # Totales por segmento desde A
  totales_segmento <- seg_a %>%
    dplyr::group_by(.data$SEGMENTO, .data$CLASE) %>%
    dplyr::summarise(
      encuestas_totales = dplyr::n_distinct(.data$DIRECTORIO),
      .groups = "drop"
    )

  # Efectivas y completas desde base_eval
  base_eval_segmento <- res_campo$base_eval %>%
    dplyr::mutate(
      SEGMENTO = limpiar_seg(.data$SEGMENTO),
      CLASE = limpiar_seg(.data$CLASE),
      DIRECTORIO = limpiar_id(.data$DIRECTORIO)
    ) %>%
    dplyr::group_by(.data$SEGMENTO, .data$CLASE) %>%
    dplyr::summarise(
      encuestas_efectivas = sum(.data$encuesta_efectiva_campo, na.rm = TRUE),
      encuestas_completas = sum(.data$encuesta_completa_campo, na.rm = TRUE),
      encuestas_incompletas = sum(.data$encuesta_efectiva_campo & !.data$encuesta_completa_campo, na.rm = TRUE),
      .groups = "drop"
    )

  cruce_flujo_vs_campo_segmento <- totales_segmento %>%
    dplyr::left_join(base_eval_segmento, by = c("SEGMENTO", "CLASE")) %>%
    dplyr::left_join(resumen_caidas_segmento, by = c("SEGMENTO", "CLASE")) %>%
    dplyr::mutate(
      encuestas_efectivas = dplyr::coalesce(.data$encuestas_efectivas, 0L),
      encuestas_completas = dplyr::coalesce(.data$encuestas_completas, 0L),
      encuestas_incompletas = dplyr::coalesce(.data$encuestas_incompletas, 0L),
      caidas_flujo = dplyr::coalesce(.data$caidas_flujo, 0L),
      caidas_campo = dplyr::coalesce(.data$caidas_campo, 0L),
      solo_flujo = dplyr::coalesce(.data$solo_flujo, 0L),
      solo_campo = dplyr::coalesce(.data$solo_campo, 0L),
      caidas_ambas = dplyr::coalesce(.data$caidas_ambas, 0L),
      sin_caida = dplyr::coalesce(.data$sin_caida, 0L),
      cobertura = dplyr::if_else(
        .data$encuestas_totales > 0,
        .data$encuestas_completas / .data$encuestas_totales,
        NA_real_
      ),
      caida = dplyr::if_else(
        .data$encuestas_totales > 0,
        1 - .data$cobertura,
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$caida), .data$SEGMENTO, .data$CLASE)

  cruce_flujo_vs_campo_segmento <- cruce_flujo_vs_campo_segmento %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  cruce_flujo_vs_campo_segmento <- arreglar_utf8_df(cruce_flujo_vs_campo_segmento)

  # =========================================================
  # 6) Resumen ejecutivo
  # =========================================================
  n_total_base <- nrow(res_campo$base_eval)

  n_caidas_campo <- nrow(encuestas_caidas_campo)

  n_caidas_flujo <- if (is.list(rep_flujo_obj) && "encuestas_caidas" %in% names(rep_flujo_obj)) {
    rep_flujo_obj$encuestas_caidas %>%
      dplyr::distinct(.data$DIRECTORIO) %>%
      nrow()
  } else {
    0L
  }

  n_solo_flujo <- cruce_ids %>%
    dplyr::filter(.data$cae_flujo == 1L & .data$cae_campo == 0L) %>%
    nrow()

  n_solo_campo <- cruce_ids %>%
    dplyr::filter(.data$cae_flujo == 0L & .data$cae_campo == 1L) %>%
    nrow()

  n_ambas <- cruce_ids %>%
    dplyr::filter(.data$cae_flujo == 1L & .data$cae_campo == 1L) %>%
    nrow()

  resumen_ejecutivo <- tibble::tibble(
    indicador = c(
      "Universo base evaluado",
      "Caídas por flujo",
      "Caídas por campo",
      "Solo flujo",
      "Solo campo",
      "Caídas en ambas",
      "Encuestas completas en campo"
    ),
    n = c(
      n_total_base,
      n_caidas_flujo,
      n_caidas_campo,
      n_solo_flujo,
      n_solo_campo,
      n_ambas,
      sum(res_campo$base_eval$encuesta_completa_campo, na.rm = TRUE)
    )
  ) %>%
    dplyr::mutate(
      pct_sobre_base = dplyr::if_else(
        .data$indicador == "Universo base evaluado",
        1,
        .data$n / n_total_base
      )
    )

  resumen_ejecutivo <- resumen_ejecutivo %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  resumen_ejecutivo <- arreglar_utf8_df(resumen_ejecutivo)

  # =========================================================
  # 7) Armar hojas y escribir Excel
  # =========================================================
  caidas_campo_74 <- encuestas_caidas_campo %>%
    dplyr::select(
      dplyr::any_of(c(
        "DIRECTORIO",
        "UUID",
        "SEGMENTO",
        "CLASE",
        "motivo_principal",
        "motivo_detallado",
        "criterio_falla",
        "encuesta_efectiva_campo",
        "encuesta_completa_campo",
        "viv_efectiva",
        "viv_ocupada_presente",
        "estado_viv",
        "n_hogares",
        "hogares_completos",
        "hogares_incompletos",
        "todos_hogares_completos",
        "n_personas",
        "personas_completas",
        "personas_incompletas",
        "todas_personas_completas"
      ))
    ) %>%
    dplyr::arrange(.data$motivo_principal, .data$SEGMENTO, .data$DIRECTORIO)

  caidas_campo_74 <- caidas_campo_74 %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.list), ~ vapply(., toString, character(1)))
    )

  caidas_campo_74 <- arreglar_utf8_df(caidas_campo_74)


  hojas_campo <- list(
    resumen_ejecutivo = resumen_ejecutivo,
    resumen_campo = resumen_general_campo,
    segmento_campo = resumen_segmento_campo,
    encuestas_caidas_campo = encuestas_caidas_campo,
    caidas_campo_74 = caidas_campo_74,
    motivos_campo = motivos_campo,
    diag_hogar_persona = diag_hogar_persona,
    cruce_flujo_vs_campo = cruce_flujo_vs_campo_segmento
  )

  if (!is.null(sabana_caidas_campo)) {
    hojas_campo[["sabana_caidas_campo"]] <- sabana_caidas_campo
  }

  wb <- openxlsx::loadWorkbook(archivo)

  hojas_existentes <- names(wb)

  for (nm in names(hojas_campo)) {
    if (nm %in% hojas_existentes) {
      openxlsx::removeWorksheet(wb, nm)
    }
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, sheet = nm, x = hojas_campo[[nm]])
  }

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)

  invisible(list(
    archivo = archivo,
    encuestas_caidas_campo = encuestas_caidas_campo,
    motivos_campo = motivos_campo,
    diag_hogar_persona = diag_hogar_persona,
    cruce_flujo_vs_campo = cruce_flujo_vs_campo_segmento,
    sabana_caidas_campo = sabana_caidas_campo
  ))
}
