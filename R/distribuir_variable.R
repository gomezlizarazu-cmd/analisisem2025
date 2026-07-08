#' Distribuir una variable segun su universo de diccionario
#'
#' Calcula la distribucion de frecuencias de una variable, agrega etiquetas de
#' respuesta desde el diccionario y anade una fila final de total. Cuando el
#' diccionario incluye la columna \code{universo}, la funcion evalua el universo
#' definido para la variable antes de calcular conteos y porcentajes.
#'
#' @param data Data frame de entrada, usualmente el capitulo K con edad pegada.
#'   Tambien puede ser una lista \code{dfs}; en ese caso se toma el capitulo
#'   indicado en \code{capitulo}.
#' @param variable Variable a distribuir. Se pasa sin comillas.
#' @param diccionario Data frame opcional con columnas \code{variable},
#'   \code{opcion}, \code{etiqueta} y, opcionalmente, \code{universo}.
#' @param dfs Lista opcional de capitulos. Si \code{data} no trae edad y
#'   \code{dfs$E} existe, la edad se pega automaticamente desde E.
#' @param capitulo Capitulo base usado cuando \code{data} es una lista
#'   \code{dfs}. Por defecto \code{"K"}.
#' @param data_edad Base opcional a nivel persona con la edad, usualmente
#'   \code{dfs$E}. Si se suministra, se une por llave de persona explicita.
#' @param edad_var Nombre de la variable de edad a usar en los universos.
#'   Por defecto \code{"edad"}.
#' @param usar_universo_diccionario Logico. Si \code{TRUE}, aplica el universo
#'   leido del diccionario para la variable. Por defecto \code{TRUE}.
#'
#' @return Un tibble con la variable distribuida como texto, la etiqueta, el
#'   numero de personas o registros, y el porcentaje sobre el universo aplicado.
#'
#' @examples
#' \dontrun{
#' distribuir_variable(K_con_edad, NPCKP1, reglas_k_total)
#' distribuir_variable(dfs, NPCKP1, reglas_k_total)
#' distribuir_variable(dfs$K, NPCKP1, reglas_k_total, dfs = dfs)
#' }
#'
#' @export
distribuir_variable <- function(data,
                                variable,
                                diccionario = NULL,
                                dfs = NULL,
                                capitulo = "K",
                                data_edad = NULL,
                                edad_var = "edad",
                                usar_universo_diccionario = TRUE) {
  var <- rlang::ensym(variable)
  var_name <- rlang::as_name(var)
  capitulo <- toupper(capitulo)

  if (is.list(data) && !is.data.frame(data)) {
    dfs <- data
    names(dfs) <- toupper(names(dfs))

    if (!(capitulo %in% names(dfs))) {
      stop("No se encontro el capitulo `", capitulo, "` en `data`.")
    }

    data <- dfs[[capitulo]]
  }

  if (!is.data.frame(data)) {
    stop("`data` debe ser un data.frame o una lista `dfs`.")
  }

  if (!is.null(dfs)) {
    names(dfs) <- toupper(names(dfs))
    if (is.null(data_edad) && "E" %in% names(dfs)) {
      data_edad <- dfs[["E"]]
    }
  }

  data_eval <- tibble::as_tibble(data)

  if (!(var_name %in% names(data_eval))) {
    stop("La variable `", var_name, "` no existe en `data`.")
  }

  data_eval <- .agregar_edad_desde_e(
    data = data_eval,
    data_edad = data_edad,
    capitulo = capitulo,
    edad_var = edad_var
  )

  etiquetas <- .etiquetas_diccionario_variable(
    diccionario = diccionario,
    var_name = var_name
  )

  universo_aplicado <- NA_character_

  if (isTRUE(usar_universo_diccionario) && !is.null(diccionario)) {
    universo_aplicado <- .universo_diccionario_variable(
      diccionario = diccionario,
      var_name = var_name
    )

    if (!is.na(universo_aplicado)) {
      data_eval <- .filtrar_universo_diccionario(
        data = data_eval,
        universo = universo_aplicado,
        edad_var = edad_var
      )
    }
  }

  denominador <- nrow(data_eval)

  distribucion <- data_eval %>%
    dplyr::count(!!var, name = "n_personas") %>%
    dplyr::mutate(
      valor = .limpiar_utf8(as.character(!!var)),
      pct_sobre_universo = if (denominador > 0) {
        .data$n_personas / denominador
      } else {
        NA_real_
      }
    ) %>%
    dplyr::left_join(etiquetas, by = "valor") %>%
    dplyr::mutate(
      etiqueta = .limpiar_utf8(.data$etiqueta),
      etiqueta = dplyr::case_when(
        is.na(.data$valor) ~ "Sin respuesta",
        is.na(.data$etiqueta) | trimws(.data$etiqueta) == "" ~ .data$valor,
        TRUE ~ .data$etiqueta
      )
    ) %>%
    dplyr::transmute(
      !!var_name := .data$valor,
      .data$etiqueta,
      .data$n_personas,
      .data$pct_sobre_universo
    )

  total <- tibble::tibble(
    !!var_name := "Total",
    etiqueta = "Total",
    n_personas = denominador,
    pct_sobre_universo = if (denominador > 0) 1 else NA_real_
  )

  out <- dplyr::bind_rows(distribucion, total) %>%
    dplyr::arrange(
      dplyr::if_else(.data[[var_name]] == "Total", 1L, 0L, missing = 0L),
      suppressWarnings(as.numeric(.data[[var_name]])),
      .data[[var_name]]
    )

  attr(out, "universo") <- universo_aplicado
  out
}

.agregar_edad_desde_e <- function(data,
                                  data_edad = NULL,
                                  capitulo = "K",
                                  edad_var = "edad") {
  candidatos_edad <- unique(c(edad_var, "edad", "NPCEP4", "Edad"))
  edad_en_data <- col_first_existing(data, candidatos_edad)

  if (!is.null(edad_en_data)) {
    data[[edad_var]] <- suppressWarnings(as.numeric(data[[edad_en_data]]))
    return(data)
  }

  if (is.null(data_edad)) {
    return(data)
  }

  if (!is.data.frame(data_edad)) {
    stop("`data_edad` debe ser un data.frame si se suministra.")
  }

  llaves <- if (exists("get_join_keys", mode = "function")) {
    get_join_keys(capitulo)
  } else {
    c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
  }

  faltan_data <- setdiff(llaves, names(data))
  faltan_edad <- setdiff(llaves, names(data_edad))

  if (length(faltan_data) > 0 || length(faltan_edad) > 0) {
    stop(
      "No fue posible pegar edad desde E. Faltan llaves en data: ",
      paste(faltan_data, collapse = ", "),
      "; faltan llaves en data_edad: ",
      paste(faltan_edad, collapse = ", ")
    )
  }

  edad_origen <- col_first_existing(data_edad, candidatos_edad)
  if (is.null(edad_origen)) {
    stop("No se encontro variable de edad en `data_edad`.")
  }

  data_norm <- normalize_keys(data, llaves)
  edad_norm <- data_edad %>%
    normalize_keys(llaves) %>%
    dplyr::select(dplyr::all_of(c(llaves, edad_origen))) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(llaves)), .keep_all = TRUE)

  names(edad_norm)[names(edad_norm) == edad_origen] <- edad_var
  edad_norm[[edad_var]] <- suppressWarnings(as.numeric(edad_norm[[edad_var]]))

  data_norm %>%
    dplyr::left_join(edad_norm, by = llaves)
}

.etiquetas_diccionario_variable <- function(diccionario, var_name) {
  etiquetas <- tibble::tibble(
    valor = character(),
    etiqueta = character()
  )

  if (is.null(diccionario)) {
    return(etiquetas)
  }

  cols_dic <- c("variable", "opcion", "etiqueta")
  faltan_cols <- setdiff(cols_dic, names(diccionario))

  if (length(faltan_cols) > 0) {
    stop(
      "`diccionario` debe tener las columnas: ",
      paste(cols_dic, collapse = ", ")
    )
  }

  diccionario %>%
    dplyr::filter(.data$variable == var_name) %>%
    dplyr::transmute(
      valor = .limpiar_utf8(as.character(.data$opcion)),
      etiqueta = .limpiar_utf8(as.character(.data$etiqueta))
    ) %>%
    dplyr::distinct()
}

.universo_diccionario_variable <- function(diccionario, var_name) {
  if (!"universo" %in% names(diccionario)) {
    return(NA_character_)
  }

  universos <- diccionario %>%
    dplyr::filter(.data$variable == var_name) %>%
    dplyr::pull(.data$universo) %>%
    as.character()

  universos <- universos[!is.na(universos)]
  universos <- unique(trimws(universos[trimws(universos) != ""]))

  if (length(universos) == 0) {
    return(NA_character_)
  }

  if (length(universos) > 1) {
    stop(
      "La variable `", var_name,
      "` tiene mas de un universo en `diccionario`: ",
      paste(universos, collapse = " | ")
    )
  }

  universos
}

construir_nodos_flujo_k <- function(data, edad_var = "edad") {
  data <- tibble::as_tibble(data)
  n <- nrow(data)

  num_col <- function(var) {
    if (!(var %in% names(data))) {
      return(rep(NA_real_, n))
    }

    suppressWarnings(as.numeric(data[[var]]))
  }

  tiene_valor <- function(var) {
    if (!(var %in% names(data))) {
      return(rep(FALSE, n))
    }

    x <- .limpiar_utf8(as.character(data[[var]]))
    !is.na(data[[var]]) & !is.na(x) & trimws(x) != ""
  }

  algun_valor <- function(vars) {
    vars <- intersect(vars, names(data))
    if (length(vars) == 0) {
      return(rep(FALSE, n))
    }

    rowSums(do.call(cbind, lapply(vars, tiene_valor)), na.rm = TRUE) > 0
  }

  edad_origen <- col_first_existing(data, unique(c(edad_var, "edad", "NPCEP4", "Edad")))
  edad <- if (!is.null(edad_origen)) {
    suppressWarnings(as.numeric(data[[edad_origen]]))
  } else {
    rep(NA_real_, n)
  }

  universo_k <- !is.na(edad) & edad >= 10

  llega_K17 <- universo_k & (
    num_col("NPCKP18") %in% 2 |
      num_col("NPCKP19") %in% 1 |
      num_col("NPCKP20") %in% c(1, 2, 9)
  )

  ocupado_k23 <- universo_k & num_col("NPCKP17") %in% 1:8
  no_ocupado <- universo_k & (
    num_col("NPCKP1") %in% 5 |
      num_col("NPCKP7") %in% 2 |
      num_col("NPCKP13") %in% 2
  )
  ocupados_o_no_ocupados <- universo_k & (ocupado_k23 | no_ocupado)
  mayor_18_ocupados_o_no_ocupados <- !is.na(edad) & edad >= 18 & (ocupado_k23 | no_ocupado)

  llega_K45 <- ocupado_k23

  llega_K50 <- llega_K45 &
    tiene_valor("NPCKP41") &
    (
      suppressWarnings(num_col("NPCKP41") >= num_col("NPCKP39")) |
        tiene_valor("NPCKP42") |
        tiene_valor("NPCKP42A")
    )

  llega_K53 <- llega_K45 & !(num_col("NPCKP44A") %in% 1)

  vars_medios <- c(
    "NPCKP45A", "NPCKP45B", "NPCKP45C", "NPCKP45D",
    "NPCKP45E", "NPCKP45F", "NPCKP45G", "NPCKP45H",
    "NPCKP45I", "NPCKP45J", "NPCKP45K", "NPCKP45L",
    "NPCKP45M", "NPCKP45N", "NPCKP45O"
  )

  usa_algun_medio_transporte <- rowSums(
    do.call(cbind, lapply(vars_medios, function(var) num_col(var) %in% 1)),
    na.rm = TRUE
  ) > 0

  no_se_desplaza <- num_col("NPCKP45Q") %in% 1
  llega_K54 <- llega_K53 & usa_algun_medio_transporte & !no_se_desplaza
  llega_K55 <- llega_K54 | (llega_K53 & no_se_desplaza)

  llega_K56 <- num_col("NPCKP44A") %in% 1 |
    (
      llega_K55 &
        (
          num_col("NPCKPA46") %in% c(1, 2) |
            tiene_valor("NPCKP46AD")
        )
    )

  llega_K57 <- llega_K56 &
    (
      num_col("NPCKP47") %in% 2 |
        (num_col("NPCKP47") %in% 1 & tiene_valor("NPCKP47A"))
    )

  llega_K58 <- universo_k & num_col("NPCKP13") %in% 1

  llega_K59 <- no_ocupado

  llega_K62 <- no_ocupado

  llega_K63 <- ocupados_o_no_ocupados

  llega_K66 <- ocupados_o_no_ocupados

  llega_K67 <- ocupados_o_no_ocupados

  llega_K68 <- ocupados_o_no_ocupados

  llega_K69 <- ocupados_o_no_ocupados

  llega_K70 <- ocupados_o_no_ocupados

  llega_K71 <- ocupados_o_no_ocupados

  llega_K72 <- ocupados_o_no_ocupados

  llega_K73 <- ocupados_o_no_ocupados

  llega_K76 <- universo_k &
    !is.na(edad) &
    edad >= 18 &
    (
      num_col("NPCKP73_1") %in% 2 |
        num_col("NPCKP75_1") %in% c(1, 2, 3)
    )

  llega_K77 <- ocupados_o_no_ocupados

  llega_K78 <- mayor_18_ocupados_o_no_ocupados

  data$edad <- edad
  data$universo_k <- universo_k
  data$llega_K17 <- llega_K17
  data$llega_K45 <- llega_K45
  data$llega_K50 <- llega_K50
  data$llega_K53 <- llega_K53
  data$usa_algun_medio_transporte <- usa_algun_medio_transporte
  data$no_se_desplaza <- no_se_desplaza
  data$llega_K54 <- llega_K54
  data$llega_K55 <- llega_K55
  data$llega_K56 <- llega_K56
  data$llega_K57 <- llega_K57
  data$llega_K58 <- llega_K58
  data$llega_K59 <- llega_K59
  data$llega_K62 <- llega_K62
  data$llega_K63 <- llega_K63
  data$llega_K66 <- llega_K66
  data$llega_K67 <- llega_K67
  data$llega_K68 <- llega_K68
  data$llega_K69 <- llega_K69
  data$llega_K70 <- llega_K70
  data$llega_K71 <- llega_K71
  data$llega_K72 <- llega_K72
  data$llega_K73 <- llega_K73
  data$llega_K76 <- llega_K76
  data$llega_K77 <- llega_K77
  data$llega_K78 <- llega_K78

  data
}

.filtrar_universo_diccionario <- function(data, universo, edad_var = "edad") {
  data_eval <- if (exists("construir_nodos_flujo_k", mode = "function")) {
    construir_nodos_flujo_k(data, edad_var = edad_var)
  } else {
    data
  }

  expr <- tryCatch(parse(text = universo)[[1]], error = function(e) e)
  if (inherits(expr, "error")) {
    stop(
      "No fue posible interpretar el universo `", universo, "`: ",
      conditionMessage(expr)
    )
  }

  vars_universo <- all.vars(expr)
  faltantes <- setdiff(vars_universo, names(data_eval))

  if (length(faltantes) > 0) {
    stop(
      "No fue posible evaluar el universo `", universo,
      "` porque faltan variables en `data`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  env <- list2env(as.list(data_eval), parent = baseenv())
  filtro <- tryCatch(eval(expr, envir = env), error = function(e) e)

  if (inherits(filtro, "error")) {
    stop(
      "No fue posible evaluar el universo `", universo, "`: ",
      conditionMessage(filtro)
    )
  }

  if (!is.logical(filtro)) {
    stop("El universo `", universo, "` no devuelve un vector logico.")
  }

  if (length(filtro) == 1) {
    filtro <- rep(filtro, nrow(data_eval))
  }

  if (length(filtro) != nrow(data_eval)) {
    stop(
      "El universo `", universo,
      "` devuelve un vector de longitud ", length(filtro),
      ", pero `data` tiene ", nrow(data_eval), " filas."
    )
  }

  data_eval[!is.na(filtro) & filtro, , drop = FALSE]
}

.limpiar_utf8 <- function(x) {
  iconv(x, from = "", to = "UTF-8", sub = "")
}
