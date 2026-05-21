#' Diagnosticar no respuesta en una variable candidata a imputacion
#'
#' Construye una base diagnostica para una variable objetivo, separando las
#' unidades que debian responder con informacion valida de aquellas que debian
#' responder pero no tienen un codigo valido. Luego compara ambos grupos frente
#' a variables auxiliares o trazadoras para evaluar si la no respuesta parece
#' aleatoria o condicionada por perfiles observables.
#'
#' @param dfs Lista nombrada de data frames con los capitulos de la encuesta.
#' @param base_cap Capitulo base sobre el cual se arma el diagnostico.
#' @param variable_objetivo Nombre de la variable objetivo a diagnosticar.
#' @param codigos_validos Vector con los codigos considerados informacion
#'   valida para \code{variable_objetivo}.
#' @param expresion_universo Expresion logica que define quien debia responder
#'   la variable objetivo. Se evalua sobre la base armada con trazadoras.
#' @param trazadoras Tibble o data frame con columnas \code{var}, \code{cap} y
#'   \code{alias}, compatible con \code{armar_base_con_trazadoras()}.
#' @param variables_analisis Vector de variables auxiliares a tabular. Puede ser
#'   un vector simple, por ejemplo \code{c("sexo", "grupo_edad")}, o un vector
#'   nombrado donde el nombre es el alias de salida y el valor es la variable
#'   origen, por ejemplo \code{c(sexo = "sexo_codigo")}.
#' @param etiquetas Lista nombrada opcional. Cada elemento debe corresponder a
#'   una variable auxiliar y puede ser un vector nombrado codigo -> etiqueta. Los
#'   codigos no mapeados quedan como \code{"Otro código"} y los valores vacios o
#'   \code{NA} quedan como \code{"No informado"}. Tambien se admite una funcion
#'   que reciba el vector origen y devuelva etiquetas, util para agrupar edades.
#' @param excluir_no_informado Logico. Si es \code{TRUE}, excluye la categoria
#'   \code{"No informado"} de las tablas comparativas, del indice de
#'   disimilitud y de las graficas. La base diagnostica conserva los datos
#'   originales.
#' @param generar_graficos Logico. Si es \code{TRUE}, genera objetos
#'   interactivos \code{plotly} para cada variable auxiliar.
#'
#' @details
#' La funcion usa \code{armar_base_con_trazadoras()} para integrar variables de
#' otros capitulos con las llaves definidas por el paquete. La clasificacion de
#' \code{grupo_imputacion} se define asi:
#' \itemize{
#'   \item \code{"Con información"}: esta dentro del universo esperado y
#'   \code{variable_objetivo} tiene un codigo valido.
#'   \item \code{"Sin información"}: esta dentro del universo esperado y
#'   \code{variable_objetivo} no tiene un codigo valido.
#'   \item \code{"Fuera del universo"}: no cumple la expresion de universo.
#' }
#'
#' Para cada variable auxiliar se calculan conteos, porcentajes dentro de cada
#' grupo, tabla ancha de distribuciones entre \code{"Con información"} y
#' \code{"Sin información"}, diferencias en puntos porcentuales y tasa de
#' ausencia de la variable objetivo por categoria.
#'
#' El indice de disimilitud compara las distribuciones porcentuales de los
#' grupos \code{"Con información"} y \code{"Sin información"}:
#'
#' \deqn{D = \frac{1}{2} \sum_i |p_{i,sin} - p_{i,con}|}
#'
#' donde \eqn{p_{i,sin}} y \eqn{p_{i,con}} son las proporciones de la categoria
#' \eqn{i} dentro de cada grupo. Valores cercanos a 0 indican perfiles
#' similares; valores mas altos sugieren que la ausencia de informacion esta
#' asociada con la variable auxiliar.
#'
#' @return Una lista con:
#' \describe{
#'   \item{data}{Base diagnostica con \code{debe_responder},
#'   \code{tiene_info}, \code{tiene_codigo_valido} y
#'   \code{grupo_imputacion}.}
#'   \item{resumen_variable_objetivo}{Tabla resumen por grupo de imputacion.}
#'   \item{resumen_armado}{Resumen devuelto por
#'   \code{armar_base_con_trazadoras()}.}
#'   \item{diagnosticos}{Lista nombrada por variable auxiliar con perfil,
#'   tabla ancha, tabla para graficar, indice de disimilitud, deltas, tasas y
#'   graficas.}
#' }
#'
#' @examples
#' \dontrun{
#' trazadoras_k23 <- tibble::tribble(
#'   ~var,      ~cap, ~alias,
#'   "NPCEP4",  "E",  "edad",
#'   "NPCEP5",  "E",  "sexo_codigo",
#'   "CLASE",   "A",  "clase",
#'   "NPCFP2",  "F",  "regimen_salud",
#'   "NPCHP4",  "H",  "nivel_educativo"
#' )
#'
#' etiquetas_k23 <- list(
#'   sexo_codigo = c("1" = "Hombre", "2" = "Mujer"),
#'   clase = c(
#'     "1" = "Urbano",
#'     "2" = "Centro poblado",
#'     "3" = "Rural disperso"
#'   ),
#'   regimen_salud = c(
#'     "1" = "Contributivo",
#'     "3" = "Subsidiado",
#'     "2" = "Especial / excepcion",
#'     "9" = "No sabe / no informa"
#'   ),
#'   nivel_educativo = c(
#'     "1" = "Ninguno",
#'     "2" = "Preescolar",
#'     "3" = "Basica primaria",
#'     "4" = "Basica secundaria",
#'     "5" = "Media",
#'     "6" = "Tecnico",
#'     "7" = "Tecnologico",
#'     "8" = "Universitaria incompleta",
#'     "9" = "Universitaria completa",
#'     "10" = "Especializacion incompleta",
#'     "11" = "Especializacion completa",
#'     "12" = "Maestria incompleta",
#'     "13" = "Maestria completa",
#'     "14" = "Doctorado incompleto",
#'     "15" = "Doctorado completo"
#'   ),
#'   grupo_edad = function(edad) {
#'     edad <- suppressWarnings(as.numeric(edad))
#'     factor(
#'       dplyr::case_when(
#'         edad >= 10 & edad <= 17 ~ "10-17",
#'         edad >= 18 & edad <= 24 ~ "18-24",
#'         edad >= 25 & edad <= 34 ~ "25-34",
#'         edad >= 35 & edad <= 44 ~ "35-44",
#'         edad >= 45 & edad <= 54 ~ "45-54",
#'         edad >= 55 & edad <= 64 ~ "55-64",
#'         edad >= 65 ~ "65+",
#'         TRUE ~ "No informado"
#'       ),
#'       levels = c("10-17", "18-24", "25-34", "35-44",
#'                  "45-54", "55-64", "65+", "No informado")
#'     )
#'   }
#' )
#'
#' res_k23 <- diagnostico_imputacion_variable(
#'   dfs = dfs,
#'   base_cap = "K",
#'   variable_objetivo = "NPCKP17",
#'   codigos_validos = as.character(1:8),
#'   expresion_universo =
#'     edad >= 10 &
#'     (
#'       as.character(NPCKP1) == "1" & as.character(NPCKP2_1) == "1" |
#'         as.character(NPCKP2) == "1" |
#'         as.character(NPCKP3) == "1" &
#'           as.character(NPCKP5_1) %in% c("1", "2", "3", "4") |
#'         as.character(NPCKP3) == "1" &
#'           as.character(NPCKP5_1) %in% c("5", "6", "7", "8") &
#'           as.character(NPCKP6_1) == "1" |
#'         as.character(NPCKP4) == "1"
#'     ),
#'   trazadoras = trazadoras_k23,
#'   variables_analisis = c(
#'     grupo_edad = "edad",
#'     sexo = "sexo_codigo",
#'     zona = "clase",
#'     regimen_salud_lbl = "regimen_salud",
#'     nivel_educativo_lbl = "nivel_educativo"
#'   ),
#'   etiquetas = etiquetas_k23,
#'   excluir_no_informado = TRUE,
#'   generar_graficos = TRUE
#' )
#'
#' res_k23$resumen_variable_objetivo
#' res_k23$diagnosticos$grupo_edad$disimilitud
#' res_k23$diagnosticos$nivel_educativo_lbl$delta
#' }
#'
#' @export
diagnostico_imputacion_variable <- function(dfs,
                                            base_cap,
                                            variable_objetivo,
                                            codigos_validos,
                                            expresion_universo,
                                            trazadoras,
                                            variables_analisis,
                                            etiquetas = NULL,
                                            excluir_no_informado = TRUE,
                                            generar_graficos = TRUE) {

  universo_quo <- rlang::enquo(expresion_universo)

  .validar_entradas_diagnostico_imputacion(
    dfs = dfs,
    base_cap = base_cap,
    variable_objetivo = variable_objetivo,
    codigos_validos = codigos_validos,
    trazadoras = trazadoras,
    variables_analisis = variables_analisis,
    etiquetas = etiquetas,
    universo_quo = universo_quo
  )

  if (isTRUE(generar_graficos) && !requireNamespace("plotly", quietly = TRUE)) {
    stop("Se requiere el paquete `plotly` para construir las graficas.")
  }

  names(dfs) <- toupper(names(dfs))
  base_cap <- toupper(base_cap)
  variable_objetivo <- as.character(variable_objetivo)
  codigos_validos <- stringr::str_trim(as.character(codigos_validos))
  variables_tbl <- .normalizar_variables_analisis(variables_analisis)

  armado <- armar_base_con_trazadoras(
    dfs = dfs,
    base_cap = base_cap,
    trazadoras = trazadoras,
    join = "left"
  )

  base_diagnostico <- tibble::as_tibble(armado$data)

  if (!(variable_objetivo %in% names(base_diagnostico))) {
    stop(
      "La variable objetivo `", variable_objetivo,
      "` no existe en la base armada desde el capitulo `", base_cap, "`."
    )
  }

  faltan_aux <- setdiff(variables_tbl$variable_origen, names(base_diagnostico))
  if (length(faltan_aux) > 0) {
    stop(
      "Estas variables auxiliares no existen en la base armada: ",
      paste(faltan_aux, collapse = ", "),
      ". Revise `trazadoras` y los alias solicitados en `variables_analisis`."
    )
  }

  debe_responder <- .evaluar_expresion_logica(
    data = base_diagnostico,
    expr_quo = universo_quo,
    arg_name = "expresion_universo"
  )

  valor_objetivo <- stringr::str_trim(as.character(base_diagnostico[[variable_objetivo]]))
  tiene_info <- !is.na(base_diagnostico[[variable_objetivo]]) &
    !is.na(valor_objetivo) &
    valor_objetivo != ""

  tiene_codigo_valido <- tiene_info & valor_objetivo %in% codigos_validos
  niveles_grupo <- .niveles_grupo_imputacion()

  base_diagnostico$debe_responder <- debe_responder
  base_diagnostico$tiene_info <- tiene_info
  base_diagnostico$tiene_codigo_valido <- tiene_codigo_valido
  base_diagnostico$grupo_imputacion <- dplyr::case_when(
    !base_diagnostico$debe_responder ~ "Fuera del universo",
    base_diagnostico$tiene_codigo_valido ~ "Con información",
    TRUE ~ "Sin información"
  )
  base_diagnostico$grupo_imputacion <- factor(
    base_diagnostico$grupo_imputacion,
    levels = niveles_grupo
  )

  keys_base <- get_join_keys(base_cap)
  base_diagnostico <- base_diagnostico %>%
    dplyr::relocate(
      dplyr::any_of(c(
        keys_base,
        variable_objetivo,
        "debe_responder",
        "tiene_info",
        "tiene_codigo_valido",
        "grupo_imputacion"
      ))
    )

  resumen_objetivo <- .resumir_variable_objetivo(
    data = base_diagnostico,
    variable_objetivo = variable_objetivo
  )

  diagnosticos <- purrr::map(
    seq_len(nrow(variables_tbl)),
    function(i) {
      variable_auxiliar <- variables_tbl$variable[i]
      variable_origen <- variables_tbl$variable_origen[i]
      etiquetas_var <- .obtener_etiquetas_variable(
        etiquetas = etiquetas,
        variable_auxiliar = variable_auxiliar,
        variable_origen = variable_origen
      )

      .preparar_tabla_variable_auxiliar(
        data = base_diagnostico,
        variable_auxiliar = variable_auxiliar,
        variable_origen = variable_origen,
        etiquetas = etiquetas_var,
        excluir_no_informado = excluir_no_informado,
        generar_graficos = generar_graficos
      )
    }
  )

  names(diagnosticos) <- variables_tbl$variable

  list(
    data = base_diagnostico,
    resumen_variable_objetivo = resumen_objetivo,
    resumen_armado = armado$resumen,
    diagnosticos = diagnosticos
  )
}

.validar_entradas_diagnostico_imputacion <- function(dfs,
                                                     base_cap,
                                                     variable_objetivo,
                                                     codigos_validos,
                                                     trazadoras,
                                                     variables_analisis,
                                                     etiquetas,
                                                     universo_quo) {
  if (!is.list(dfs) || length(dfs) == 0) {
    stop("`dfs` debe ser una lista nombrada de data frames.")
  }

  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("`dfs` debe tener nombres de capitulos.")
  }

  if (!is.character(base_cap) || length(base_cap) != 1 || !nzchar(base_cap)) {
    stop("`base_cap` debe ser un string de longitud 1.")
  }

  if (!is.character(variable_objetivo) || length(variable_objetivo) != 1 || !nzchar(variable_objetivo)) {
    stop("`variable_objetivo` debe ser un string de longitud 1.")
  }

  if (missing(codigos_validos) || length(codigos_validos) == 0) {
    stop("`codigos_validos` debe contener al menos un codigo valido.")
  }

  if (rlang::quo_is_missing(universo_quo)) {
    stop("Debe suministrar `expresion_universo`.")
  }

  if (!is.data.frame(trazadoras)) {
    stop("`trazadoras` debe ser un data.frame o tibble.")
  }

  if (!all(c("var", "cap", "alias") %in% names(trazadoras))) {
    stop("`trazadoras` debe tener columnas: var, cap, alias.")
  }

  .normalizar_variables_analisis(variables_analisis)
  .validar_etiquetas_diagnostico(etiquetas)

  invisible(TRUE)
}

.normalizar_variables_analisis <- function(variables_analisis) {
  if (is.null(variables_analisis) || length(variables_analisis) == 0) {
    stop("`variables_analisis` debe contener al menos una variable.")
  }

  if (is.list(variables_analisis) && !is.data.frame(variables_analisis)) {
    variables_analisis <- unlist(variables_analisis, use.names = TRUE)
  }

  if (!is.character(variables_analisis)) {
    stop("`variables_analisis` debe ser un vector o lista de nombres de variables.")
  }

  variables_analisis <- variables_analisis[nzchar(variables_analisis)]
  if (length(variables_analisis) == 0) {
    stop("`variables_analisis` no contiene nombres validos.")
  }

  nombres <- names(variables_analisis)
  if (is.null(nombres)) {
    nombres <- rep("", length(variables_analisis))
  }

  variable <- dplyr::if_else(
    !is.na(nombres) & nombres != "",
    nombres,
    as.character(variables_analisis)
  )

  tibble::tibble(
    variable = as.character(variable),
    variable_origen = as.character(variables_analisis)
  ) %>%
    dplyr::distinct(.data$variable, .keep_all = TRUE)
}

.validar_etiquetas_diagnostico <- function(etiquetas) {
  if (is.null(etiquetas)) {
    return(invisible(TRUE))
  }

  if (!is.list(etiquetas)) {
    stop("`etiquetas` debe ser una lista nombrada o NULL.")
  }

  if (is.null(names(etiquetas)) || any(names(etiquetas) == "")) {
    stop("`etiquetas` debe ser una lista nombrada.")
  }

  for (nombre in names(etiquetas)) {
    mapa <- etiquetas[[nombre]]

    if (is.function(mapa)) {
      next
    }

    if (!is.atomic(mapa) || is.null(names(mapa)) || any(names(mapa) == "")) {
      stop(
        "Cada elemento de `etiquetas` debe ser un vector nombrado codigo -> etiqueta ",
        "o una funcion. Revise `", nombre, "`."
      )
    }
  }

  invisible(TRUE)
}

.evaluar_expresion_logica <- function(data, expr_quo, arg_name) {
  expr <- rlang::get_expr(expr_quo)
  env <- rlang::get_env(expr_quo)

  resultado <- tryCatch(
    rlang::eval_tidy(expr_quo, data = data),
    error = function(e) e
  )

  if (inherits(resultado, "error")) {
    stop(
      "No fue posible evaluar `", arg_name, "` sobre la base diagnostica: ",
      conditionMessage(resultado)
    )
  }

  if (is.character(resultado) && length(resultado) == 1) {
    expr_parseada <- tryCatch(rlang::parse_expr(resultado), error = function(e) e)
    if (inherits(expr_parseada, "error")) {
      stop(
        "No fue posible interpretar `", arg_name, "` como expresion logica: ",
        conditionMessage(expr_parseada)
      )
    }
    resultado <- rlang::eval_tidy(rlang::new_quosure(expr_parseada, env), data = data)
  } else if (rlang::is_formula(resultado)) {
    resultado <- rlang::eval_tidy(
      rlang::new_quosure(rlang::f_rhs(resultado), rlang::f_env(resultado)),
      data = data
    )
  } else if (is.expression(resultado)) {
    resultado <- rlang::eval_tidy(rlang::new_quosure(resultado[[1]], env), data = data)
  } else if (is.call(resultado) || is.name(resultado)) {
    resultado <- rlang::eval_tidy(rlang::new_quosure(resultado, env), data = data)
  } else if (is.character(expr) && length(expr) == 1) {
    expr_parseada <- rlang::parse_expr(expr)
    resultado <- rlang::eval_tidy(rlang::new_quosure(expr_parseada, env), data = data)
  }

  if (!is.logical(resultado)) {
    stop("`", arg_name, "` debe devolver un vector logico.")
  }

  if (length(resultado) == 1) {
    resultado <- rep(resultado, nrow(data))
  }

  if (length(resultado) != nrow(data)) {
    stop(
      "`", arg_name, "` devuelve longitud ", length(resultado),
      ", pero la base diagnostica tiene ", nrow(data), " filas."
    )
  }

  !is.na(resultado) & resultado
}

.niveles_grupo_imputacion <- function() {
  c("Con información", "Sin información", "Fuera del universo")
}

.resumir_variable_objetivo <- function(data, variable_objetivo) {
  niveles_grupo <- .niveles_grupo_imputacion()
  n_total <- nrow(data)
  n_universo <- sum(data$debe_responder, na.rm = TRUE)

  data %>%
    dplyr::count(.data$grupo_imputacion, name = "n") %>%
    tidyr::complete(
      grupo_imputacion = factor(niveles_grupo, levels = niveles_grupo),
      fill = list(n = 0L)
    ) %>%
    dplyr::mutate(
      variable_objetivo = variable_objetivo,
      pct_total = if (n_total > 0) .data$n / n_total else NA_real_,
      pct_universo_esperado = dplyr::if_else(
        as.character(.data$grupo_imputacion) == "Fuera del universo" | n_universo == 0,
        NA_real_,
        .data$n / n_universo
      )
    ) %>%
    dplyr::select(
      .data$variable_objetivo,
      .data$grupo_imputacion,
      .data$n,
      .data$pct_total,
      .data$pct_universo_esperado
    )
}

.obtener_etiquetas_variable <- function(etiquetas, variable_auxiliar, variable_origen) {
  if (is.null(etiquetas)) {
    return(NULL)
  }

  if (variable_auxiliar %in% names(etiquetas)) {
    return(etiquetas[[variable_auxiliar]])
  }

  if (variable_origen %in% names(etiquetas)) {
    return(etiquetas[[variable_origen]])
  }

  NULL
}

.aplicar_etiquetas <- function(x, etiquetas = NULL) {
  x_chr <- stringr::str_trim(as.character(x))
  faltante <- is.na(x) | is.na(x_chr) | x_chr == ""

  if (is.null(etiquetas)) {
    salida <- x_chr
    salida[faltante] <- "No informado"
    niveles <- unique(salida)
    niveles <- c(setdiff(niveles, "No informado"), intersect("No informado", niveles))
    return(factor(salida, levels = niveles))
  }

  if (is.function(etiquetas)) {
    salida_raw <- etiquetas(x)
    niveles_funcion <- if (is.factor(salida_raw)) {
      levels(salida_raw)
    } else {
      unique(as.character(salida_raw))
    }

    salida <- stringr::str_trim(as.character(salida_raw))
    faltante_salida <- is.na(salida_raw) | is.na(salida) | salida == ""
    salida[faltante | faltante_salida] <- "No informado"

    niveles <- c(
      setdiff(niveles_funcion, c(NA_character_, "", "No informado")),
      "No informado"
    )
    niveles <- unique(c(niveles, setdiff(unique(salida), niveles)))
    return(factor(salida, levels = niveles))
  }

  codigos <- stringr::str_trim(as.character(names(etiquetas)))
  etiquetas_chr <- as.character(unname(etiquetas))
  names(etiquetas_chr) <- codigos

  salida <- unname(etiquetas_chr[x_chr])
  salida[faltante] <- "No informado"
  salida[!faltante & is.na(salida)] <- "Otro código"

  niveles <- unique(c(etiquetas_chr, "Otro código", "No informado"))
  factor(salida, levels = niveles)
}

.preparar_tabla_variable_auxiliar <- function(data,
                                             variable_auxiliar,
                                             variable_origen,
                                             etiquetas = NULL,
                                             excluir_no_informado = TRUE,
                                             generar_graficos = TRUE) {
  niveles_grupo <- .niveles_grupo_imputacion()

  categoria <- .aplicar_etiquetas(
    x = data[[variable_origen]],
    etiquetas = etiquetas
  )

  niveles_categoria <- levels(categoria)
  if (is.null(niveles_categoria)) {
    niveles_categoria <- unique(as.character(categoria))
  }

  if (isTRUE(excluir_no_informado)) {
    niveles_categoria <- setdiff(niveles_categoria, "No informado")
  }

  data_aux <- tibble::tibble(
    variable = variable_auxiliar,
    grupo_imputacion = factor(as.character(data$grupo_imputacion), levels = niveles_grupo),
    categoria = factor(as.character(categoria), levels = niveles_categoria)
  ) %>%
    dplyr::filter(!is.na(.data$categoria))

  if (length(niveles_categoria) == 0 || nrow(data_aux) == 0) {
    perfil_vacio <- tibble::tibble(
      variable = character(),
      grupo_imputacion = factor(levels = niveles_grupo),
      categoria = character(),
      n = integer(),
      total_grupo = integer(),
      pct = numeric()
    )

    tabla_vacia <- tibble::tibble(
      variable = character(),
      categoria = character(),
      n_con_info = integer(),
      n_sin_info = integer(),
      pct_con_info = numeric(),
      pct_sin_info = numeric(),
      delta_pp = numeric(),
      delta_abs_pp = numeric(),
      tasa_faltante = numeric()
    )

    tabla_ancha_vacia <- tibble::tibble(
      variable = character(),
      categoria = character(),
      `Con información` = numeric(),
      `Sin información` = numeric(),
      delta_pp = numeric(),
      delta_abs_pp = numeric()
    )

    delta_vacia <- tibble::tibble(
      variable = character(),
      categoria = character(),
      pct_con_info = numeric(),
      pct_sin_info = numeric(),
      delta_pp = numeric(),
      delta_abs_pp = numeric()
    )

    tasa_vacia <- tibble::tibble(
      variable = character(),
      categoria = character(),
      n_con_info = integer(),
      n_sin_info = integer(),
      tasa_faltante = numeric()
    )

    return(list(
      perfil = perfil_vacio,
      tabla_ancha = tabla_ancha_vacia,
      tabla_plot = tabla_vacia,
      disimilitud = tibble::tibble(variable = variable_auxiliar, disimilitud = NA_real_),
      delta = delta_vacia,
      tasa_faltante = tasa_vacia,
      grafico_comparabilidad = NULL,
      grafico_tasa_faltante = NULL
    ))
  }

  perfil <- data_aux %>%
    dplyr::count(.data$variable, .data$grupo_imputacion, .data$categoria, name = "n") %>%
    tidyr::complete(
      variable = variable_auxiliar,
      grupo_imputacion = factor(niveles_grupo, levels = niveles_grupo),
      categoria = factor(niveles_categoria, levels = niveles_categoria),
      fill = list(n = 0L)
    ) %>%
    dplyr::group_by(.data$variable, .data$grupo_imputacion) %>%
    dplyr::mutate(
      total_grupo = sum(.data$n, na.rm = TRUE),
      pct = dplyr::if_else(.data$total_grupo > 0, .data$n / .data$total_grupo, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$grupo_imputacion, .data$categoria)

  tabla_plot <- data_aux %>%
    dplyr::filter(as.character(.data$grupo_imputacion) %in% c("Con información", "Sin información")) %>%
    dplyr::mutate(
      grupo_simple = dplyr::case_when(
        as.character(.data$grupo_imputacion) == "Con información" ~ "con_info",
        as.character(.data$grupo_imputacion) == "Sin información" ~ "sin_info",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::count(.data$variable, .data$categoria, .data$grupo_simple, name = "n") %>%
    tidyr::complete(
      variable = variable_auxiliar,
      categoria = factor(niveles_categoria, levels = niveles_categoria),
      grupo_simple = c("con_info", "sin_info"),
      fill = list(n = 0L)
    ) %>%
    tidyr::pivot_wider(
      names_from = "grupo_simple",
      values_from = "n",
      names_prefix = "n_",
      values_fill = list(n = 0L)
    )

  if (!"n_con_info" %in% names(tabla_plot)) {
    tabla_plot$n_con_info <- 0L
  }

  if (!"n_sin_info" %in% names(tabla_plot)) {
    tabla_plot$n_sin_info <- 0L
  }

  n_con_total <- sum(tabla_plot$n_con_info, na.rm = TRUE)
  n_sin_total <- sum(tabla_plot$n_sin_info, na.rm = TRUE)

  tabla_plot <- tabla_plot %>%
    dplyr::mutate(
      pct_con_info = if (n_con_total > 0) .data$n_con_info / n_con_total else NA_real_,
      pct_sin_info = if (n_sin_total > 0) .data$n_sin_info / n_sin_total else NA_real_,
      delta_pp = (.data$pct_sin_info - .data$pct_con_info) * 100,
      delta_abs_pp = abs(.data$delta_pp),
      tasa_faltante = dplyr::if_else(
        .data$n_con_info + .data$n_sin_info > 0,
        .data$n_sin_info / (.data$n_con_info + .data$n_sin_info),
        NA_real_
      )
    ) %>%
    dplyr::filter(.data$n_con_info + .data$n_sin_info > 0) %>%
    dplyr::arrange(.data$categoria)

  disimilitud_val <- .calcular_indice_disimilitud(
    pct_con_info = tabla_plot$pct_con_info,
    pct_sin_info = tabla_plot$pct_sin_info
  )

  tabla_ancha <- tabla_plot %>%
    dplyr::transmute(
      .data$variable,
      .data$categoria,
      `Con información` = .data$pct_con_info,
      `Sin información` = .data$pct_sin_info,
      .data$delta_pp,
      .data$delta_abs_pp
    )

  delta <- tabla_plot %>%
    dplyr::select(
      .data$variable,
      .data$categoria,
      .data$pct_con_info,
      .data$pct_sin_info,
      .data$delta_pp,
      .data$delta_abs_pp
    )

  tasa_faltante <- tabla_plot %>%
    dplyr::select(
      .data$variable,
      .data$categoria,
      .data$n_con_info,
      .data$n_sin_info,
      .data$tasa_faltante
    )

  grafico_comparabilidad <- NULL
  grafico_tasa_faltante <- NULL

  if (isTRUE(generar_graficos)) {
    grafico_comparabilidad <- .crear_grafico_comparabilidad(
      tabla_plot = tabla_plot,
      variable_auxiliar = variable_auxiliar
    )
    grafico_tasa_faltante <- .crear_grafico_tasa_faltante(
      tabla_plot = tabla_plot,
      variable_auxiliar = variable_auxiliar
    )
  }

  list(
    perfil = perfil,
    tabla_ancha = tabla_ancha,
    tabla_plot = tabla_plot,
    disimilitud = tibble::tibble(
      variable = variable_auxiliar,
      disimilitud = disimilitud_val
    ),
    delta = delta,
    tasa_faltante = tasa_faltante,
    grafico_comparabilidad = grafico_comparabilidad,
    grafico_tasa_faltante = grafico_tasa_faltante
  )
}

.calcular_indice_disimilitud <- function(pct_con_info, pct_sin_info) {
  if (length(pct_con_info) == 0 || length(pct_sin_info) == 0) {
    return(NA_real_)
  }

  if (any(!is.finite(pct_con_info)) || any(!is.finite(pct_sin_info))) {
    return(NA_real_)
  }

  sum(abs(pct_con_info - pct_sin_info), na.rm = TRUE) / 2
}

.crear_grafico_comparabilidad <- function(tabla_plot, variable_auxiliar) {
  if (nrow(tabla_plot) == 0) {
    return(NULL)
  }

  datos_grafico <- tabla_plot %>%
    dplyr::mutate(
      categoria_chr = as.character(.data$categoria),
      categoria_plot = factor(.data$categoria_chr, levels = rev(unique(.data$categoria_chr))),
      pct_con_plot = .data$pct_con_info * 100,
      pct_sin_plot = .data$pct_sin_info * 100,
      hover_con = paste0(
        "Variable: ", .data$variable,
        "<br>Categoria: ", .data$categoria_chr,
        "<br>Grupo: Con información",
        "<br>Porcentaje: ", round(.data$pct_con_plot, 2), "%",
        "<br>n: ", scales::comma(.data$n_con_info)
      ),
      hover_sin = paste0(
        "Variable: ", .data$variable,
        "<br>Categoria: ", .data$categoria_chr,
        "<br>Grupo: Sin información",
        "<br>Porcentaje: ", round(.data$pct_sin_plot, 2), "%",
        "<br>n: ", scales::comma(.data$n_sin_info),
        "<br>Delta pp: ", round(.data$delta_pp, 2)
      )
    )

  plotly::plot_ly(data = datos_grafico) %>%
    plotly::add_segments(
      x = ~pct_con_plot,
      xend = ~pct_sin_plot,
      y = ~categoria_plot,
      yend = ~categoria_plot,
      line = list(color = "#9aa0a6", width = 2),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    plotly::add_markers(
      x = ~pct_con_plot,
      y = ~categoria_plot,
      name = "Con información",
      marker = list(color = "#2878b5", size = 9),
      hovertext = ~hover_con,
      hovertemplate = "%{hovertext}<extra></extra>"
    ) %>%
    plotly::add_markers(
      x = ~pct_sin_plot,
      y = ~categoria_plot,
      name = "Sin información",
      marker = list(color = "#c75146", size = 9),
      hovertext = ~hover_sin,
      hovertemplate = "%{hovertext}<extra></extra>"
    ) %>%
    plotly::layout(
      title = list(text = paste0("Comparabilidad: ", variable_auxiliar)),
      xaxis = list(title = "Distribucion dentro del grupo (%)", rangemode = "tozero"),
      yaxis = list(title = ""),
      legend = list(orientation = "h", x = 0, y = -0.15),
      margin = list(l = 120, r = 30, b = 80, t = 60)
    )
}

.crear_grafico_tasa_faltante <- function(tabla_plot, variable_auxiliar) {
  if (nrow(tabla_plot) == 0) {
    return(NULL)
  }

  datos_grafico <- tabla_plot %>%
    dplyr::mutate(
      categoria_chr = as.character(.data$categoria),
      tasa_pct = .data$tasa_faltante * 100,
      hover_text = paste0(
        "Variable: ", .data$variable,
        "<br>Categoria: ", .data$categoria_chr,
        "<br>Tasa de ausencia: ", round(.data$tasa_pct, 2), "%",
        "<br>Con información: ", scales::comma(.data$n_con_info),
        "<br>Sin información: ", scales::comma(.data$n_sin_info)
      )
    )

  plotly::plot_ly(
    data = datos_grafico,
    x = ~categoria_chr,
    y = ~tasa_pct,
    type = "bar",
    marker = list(color = "#3a7d44"),
    hovertext = ~hover_text,
    hovertemplate = "%{hovertext}<extra></extra>"
  ) %>%
    plotly::layout(
      title = list(text = paste0("Tasa de ausencia: ", variable_auxiliar)),
      xaxis = list(title = "", categoryorder = "array", categoryarray = datos_grafico$categoria_chr),
      yaxis = list(title = "Sin información / universo esperado (%)", rangemode = "tozero"),
      showlegend = FALSE,
      margin = list(l = 70, r = 30, b = 120, t = 60)
    )
}
