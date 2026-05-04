#' Resumir el flujo agregado del Capitulo K
#'
#' @description
#' Construye una lectura agregada del flujo operativo del Capitulo K de la
#' Encuesta Multiproposito. La funcion no valida respuesta esperada contra
#' respuesta efectiva por variable; resume cuantas personas caen en las rutas
#' principales del diagrama operativo usando la base de personas y el
#' diccionario de reglas del capitulo.
#'
#' @param data Base de personas del Capitulo K.
#' @param diccionario Diccionario del Capitulo K con las columnas `orden`,
#'   `variable`, `pregunta`, `universo`, `opcion`, `etiqueta` y `destino`.
#' @param edad_var Nombre de la variable de edad en `data`. Por defecto
#'   `"edad"`.
#' @param data_edad Base opcional a nivel persona que contiene la edad,
#'   usualmente `dfs$E`. Si `data` no contiene `edad_var` ni `NPCEP4`, la
#'   funcion une esta base por `DIRECTORIO`, `SECUENCIA_P` y `ORDEN`.
#'
#' @return Un tibble con las columnas `flujo_id`, `bloque`, `descripcion`,
#'   `preguntas_asociadas`, `variables_asociadas`, `n_personas`,
#'   `pct_sobre_personas_10_mas` y `observacion`.
#'
#' @examples
#' dic_min <- tibble::tribble(
#'   ~orden, ~variable, ~pregunta, ~universo, ~opcion, ~etiqueta, ~destino,
#'   1, "NPCKP1", "Actividad principal", "edad >= 10", "1", "Trabajando", "K17",
#'   3, "NPCKP2", "Actividad paga", "NPCKP1 != 1", "1", "Si", "K17",
#'   4, "NPCKP3", "Tenia trabajo", "NPCKP2 == 2", "1", "Si", "K17",
#'   7, "NPCKP4", "Ayudo sin pago", "NPCKP3 == 2", "1", "Si", "K17",
#'   8, "NPCKP5", "Busco trabajo", "NPCKP4 == 2", "1", "Si", "K16",
#'   10, "NPCKP7", "Desea trabajar", "NPCKP5 == 2", "1", "Si", "K8",
#'   16, "NPCKP13", "Disponibilidad", "NPCKP12 >= 1", "1", "Si", "K58",
#'   23, "NPCKP17", "Categoria ocupacional", "llega_K17", "1", "Asalariado", "K24",
#'   63, "NPCKP50_A", "Experiencia laboral", "llega_K63", NA, "Anios", "NPCKP50_B",
#'   66, "NPCKP52", "Pension", "llega_K66", "2", "No", "K67",
#'   73, "NPCKP73_1", "Emprendimiento", "llega_K73", "2", "No", "K76",
#'   76, "NPCKPN62A", "Renta", "edad >= 18", "2", "No", "K77",
#'   77, "NPCKP59A", "Trabajo no remunerado", "edad >= 18", "2", "No", "K78",
#'   78, "NPCKP78_1", "Acoso laboral", "edad >= 18", "2", "No", "FIN_K"
#' )
#'
#' base_min <- tibble::tibble(
#'   edad = c(9, 20, 35),
#'   NPCKP1 = c(NA, 1, 2),
#'   NPCKP2 = c(NA, NA, 2),
#'   NPCKP3 = c(NA, NA, 2),
#'   NPCKP4 = c(NA, NA, 2),
#'   NPCKP5 = c(NA, NA, 2),
#'   NPCKP7 = c(NA, NA, 1),
#'   NPCKP17 = c(NA, 1, NA),
#'   NPCKP50_A = c(NA, 10, 2),
#'   NPCKP52 = c(NA, 2, 2),
#'   NPCKP73_1 = c(NA, 2, 2),
#'   NPCKPN62A = c(NA, 2, 2),
#'   NPCKP59A = c(NA, 2, 2),
#'   NPCKP78_1 = c(NA, 2, NA)
#' )
#'
#' resumir_flujo_capitulo_k(base_min, dic_min)
#'
#' @export
resumir_flujo_capitulo_k <- function(data,
                                     diccionario,
                                     edad_var = "edad",
                                     data_edad = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` debe ser un data.frame.")
  }

  if (!is.data.frame(diccionario)) {
    stop("`diccionario` debe ser un data.frame.")
  }

  columnas_requeridas <- c(
    "orden", "variable", "pregunta", "universo",
    "opcion", "etiqueta", "destino"
  )

  faltan_dic <- setdiff(columnas_requeridas, names(diccionario))
  if (length(faltan_dic) > 0) {
    stop(
      "Faltan columnas en `diccionario`: ",
      paste(faltan_dic, collapse = ", ")
    )
  }

  data <- tibble::as_tibble(data)
  diccionario <- tibble::as_tibble(diccionario)

  obs_edad_join <- NULL

  if (
    !(edad_var %in% names(data)) &&
      !("NPCEP4" %in% names(data)) &&
      !is.null(data_edad)
  ) {
    if (!is.data.frame(data_edad)) {
      stop("`data_edad` debe ser un data.frame si se suministra.")
    }

    data_edad <- tibble::as_tibble(data_edad)
    llaves_persona <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
    faltan_data <- setdiff(llaves_persona, names(data))
    faltan_edad <- setdiff(llaves_persona, names(data_edad))
    edad_origen <- dplyr::case_when(
      edad_var %in% names(data_edad) ~ edad_var,
      "NPCEP4" %in% names(data_edad) ~ "NPCEP4",
      TRUE ~ NA_character_
    )

    if (length(faltan_data) == 0 && length(faltan_edad) == 0 && !is.na(edad_origen)) {
      edad_para_unir <- data_edad |>
        dplyr::select(dplyr::all_of(c(llaves_persona, edad_origen))) |>
        dplyr::distinct(dplyr::across(dplyr::all_of(llaves_persona)), .keep_all = TRUE)

      if (edad_origen != edad_var) {
        names(edad_para_unir)[names(edad_para_unir) == edad_origen] <- edad_var
      }

      data <- data |>
        dplyr::left_join(edad_para_unir, by = llaves_persona)

      obs_edad_join <- paste0(
        "Edad tomada de `data_edad` usando join explicito por ",
        paste(llaves_persona, collapse = " + "),
        "."
      )
    } else {
      obs_edad_join <- paste0(
        "No fue posible unir edad desde `data_edad`; faltan llaves en data: ",
        paste(faltan_data, collapse = ", "),
        "; faltan llaves en data_edad: ",
        paste(faltan_edad, collapse = ", "),
        "; variable de edad disponible: ",
        ifelse(is.na(edad_origen), "ninguna", edad_origen),
        "."
      )
    }
  }

  n <- nrow(data)

  vars_orden <- function(diccionario, orden) {
    orden_chr <- as.character(orden)
    vars <- diccionario$variable[as.character(diccionario$orden) %in% orden_chr]
    unique(stats::na.omit(as.character(vars)))
  }

  primera_var <- function(diccionario, orden) {
    vars <- vars_orden(diccionario, orden)
    if (length(vars) == 0) {
      return(NA_character_)
    }
    vars[[1]]
  }

  primera_var_patron <- function(diccionario, orden, patron) {
    idx <- as.character(diccionario$orden) %in% as.character(orden)
    texto <- paste(diccionario$variable, diccionario$pregunta)
    vars <- unique(stats::na.omit(as.character(diccionario$variable[
      idx & grepl(patron, texto, ignore.case = TRUE)
    ])))

    if (length(vars) > 0) {
      return(vars[[1]])
    }

    primera_var(diccionario, orden)
  }

  contar_condicion <- function(data, condicion) {
    condicion <- as.logical(condicion)
    if (length(condicion) != nrow(data)) {
      return(NA_integer_)
    }
    if (all(is.na(condicion))) {
      return(NA_integer_)
    }
    as.integer(sum(condicion, na.rm = TRUE))
  }

  pct_seguro <- function(n, denominador) {
    if (is.na(n) || is.na(denominador) || denominador <= 0) {
      return(NA_real_)
    }
    n / denominador
  }

  num_var <- function(var) {
    if (is.na(var) || !(var %in% names(data))) {
      return(rep(NA_real_, n))
    }
    suppressWarnings(as.numeric(data[[var]]))
  }

  tiene_respuesta_var <- function(var) {
    if (is.na(var) || !(var %in% names(data))) {
      return(rep(NA, n))
    }
    x <- as.character(data[[var]])
    !is.na(x) & trimws(x) != ""
  }

  tiene_respuesta_vars <- function(vars) {
    vars <- intersect(unique(stats::na.omit(as.character(vars))), names(data))

    if (length(vars) == 0) {
      return(rep(NA, n))
    }

    respuestas <- lapply(vars, function(var) tiene_respuesta_var(var))
    Reduce(`|`, respuestas)
  }

  flag_or <- function(...) {
    flags <- list(...)
    flags <- flags[lengths(flags) > 0]

    if (length(flags) == 0) {
      return(rep(NA, n))
    }

    mat <- do.call(cbind, lapply(flags, as.logical))
    hay_true <- rowSums(mat == TRUE, na.rm = TRUE) > 0
    hay_na <- rowSums(is.na(mat)) > 0
    todos_conocidos <- rowSums(!is.na(mat)) == ncol(mat)
    out <- hay_true
    out[!hay_true & !todos_conocidos & hay_na] <- NA
    out
  }

  flag_or_evidencia <- function(...) {
    flags <- list(...)
    flags <- flags[lengths(flags) > 0]

    if (length(flags) == 0) {
      return(rep(NA, n))
    }

    mat <- do.call(cbind, lapply(flags, as.logical))
    hay_info <- rowSums(!is.na(mat)) > 0
    out <- rowSums(mat == TRUE, na.rm = TRUE) > 0
    out[!hay_info] <- NA
    out
  }

  flag_and <- function(...) {
    flags <- list(...)
    flags <- flags[lengths(flags) > 0]

    if (length(flags) == 0) {
      return(rep(NA, n))
    }

    mat <- do.call(cbind, lapply(flags, as.logical))
    hay_false <- rowSums(mat == FALSE, na.rm = TRUE) > 0
    todos_conocidos <- rowSums(!is.na(mat)) == ncol(mat)
    todos_true <- rowSums(mat == TRUE, na.rm = TRUE) == ncol(mat)
    out <- todos_true
    out[hay_false] <- FALSE
    out[!hay_false & !todos_conocidos] <- NA
    out
  }

  flag_not <- function(flag) {
    flag <- as.logical(flag)
    out <- !flag
    out[is.na(flag)] <- NA
    out
  }

  preguntas_orden <- function(orden) {
    orden_ref <- as.character(orden)

    preguntas <- diccionario |>
      dplyr::filter(as.character(.data$orden) %in% orden_ref) |>
      dplyr::distinct(.data$orden, .data$pregunta) |>
      dplyr::mutate(txt = paste0("K", .data$orden, ": ", .data$pregunta)) |>
      dplyr::pull(.data$txt)

    preguntas <- unique(stats::na.omit(preguntas))
    if (length(preguntas) == 0) {
      return(NA_character_)
    }
    paste(preguntas, collapse = " | ")
  }

  variables_orden <- function(orden) {
    vars <- vars_orden(diccionario, orden)
    if (length(vars) == 0) {
      return(NA_character_)
    }
    paste(vars, collapse = ", ")
  }

  observacion_vars <- function(vars, extra = NULL) {
    vars_dic <- unique(stats::na.omit(as.character(vars)))
    faltan_base <- setdiff(vars_dic, names(data))
    partes <- character()

    if (length(vars_dic) == 0) {
      partes <- c(partes, "El diccionario no contiene variables asociadas para este bloque.")
    }

    if (length(faltan_base) > 0) {
      partes <- c(
        partes,
        paste0("Variables no encontradas en data: ", paste(faltan_base, collapse = ", "), ".")
      )
    }

    if (!is.null(extra) && !is.na(extra) && nzchar(extra)) {
      partes <- c(partes, extra)
    }

    if (length(partes) == 0) {
      return("OK")
    }

    paste(partes, collapse = " ")
  }

  fila_resumen <- function(flujo_id,
                           bloque,
                           descripcion,
                           ordenes,
                           flag,
                           observacion_extra = NULL) {
    vars <- vars_orden(diccionario, ordenes)
    n_personas <- contar_condicion(data, flag)

    tibble::tibble(
      flujo_id = flujo_id,
      bloque = bloque,
      descripcion = descripcion,
      preguntas_asociadas = preguntas_orden(ordenes),
      variables_asociadas = variables_orden(ordenes),
      n_personas = n_personas,
      pct_sobre_personas_10_mas = pct_seguro(n_personas, denominador_10_mas),
      observacion = observacion_vars(vars, observacion_extra)
    )
  }

  edad <- if (edad_var %in% names(data)) {
    suppressWarnings(as.numeric(data[[edad_var]]))
  } else if ("NPCEP4" %in% names(data)) {
    suppressWarnings(as.numeric(data[["NPCEP4"]]))
  } else {
    rep(NA_real_, n)
  }

  var_k1 <- primera_var(diccionario, 1)
  var_k2_1 <- primera_var(diccionario, 2)
  var_k2 <- primera_var(diccionario, 3)
  var_k3 <- primera_var(diccionario, 4)
  var_k4 <- primera_var_patron(diccionario, 7, "sin pago|ayudo")
  var_diligencias <- primera_var(diccionario, 8)
  var_desea <- primera_var(diccionario, 10)
  var_post_empleo <- primera_var(diccionario, 13)
  var_ultimos_12 <- primera_var(diccionario, 14)
  var_disponibilidad <- primera_var(diccionario, 16)
  var_categoria <- primera_var(diccionario, 23)

  k1 <- num_var(var_k1)
  k2_1 <- num_var(var_k2_1)
  k2 <- num_var(var_k2)
  k3 <- num_var(var_k3)
  k4 <- num_var(var_k4)
  diligencias <- num_var(var_diligencias)
  desea <- num_var(var_desea)
  post_empleo <- num_var(var_post_empleo)
  ultimos_12 <- num_var(var_ultimos_12)
  disponibilidad <- num_var(var_disponibilidad)
  categoria <- num_var(var_categoria)

  universo_k <- !is.na(edad) & edad >= 10
  denominador_10_mas <- contar_condicion(data, universo_k)

  k1_trabajando <- flag_and(universo_k, k1 == 1)
  k1_no_trabajando <- flag_and(universo_k, !is.na(k1) & k1 != 1)

  filtros_ocupacion_respondidos <- tiene_respuesta_vars(c(var_k2_1, var_k2, var_k3, var_k4))
  rescate_ocupacion <- flag_and(k1_no_trabajando, filtros_ocupacion_respondidos)

  ocupado <- flag_and(
    universo_k,
    flag_or_evidencia(
      k1 == 1,
      k2_1 == 1,
      k2 == 1,
      k3 == 1,
      k4 == 1
    )
  )

  ruta_busqueda <- flag_and(universo_k, flag_not(ocupado))

  desocupado <- flag_and(
    ruta_busqueda,
    flag_or_evidencia(
      diligencias == 1,
      post_empleo == 1,
      ultimos_12 == 1,
      disponibilidad == 1
    )
  )

  inactivo <- flag_and(ruta_busqueda, flag_not(desocupado))
  inactivo_disponible <- flag_and(inactivo, desea == 1)

  llega_k23 <- flag_and(ocupado, tiene_respuesta_var(var_categoria))
  asalariado <- flag_and(ocupado, categoria %in% c(1, 2, 3, 7, 8))
  independiente <- flag_and(ocupado, categoria %in% c(4, 5))
  sin_remuneracion <- flag_and(ocupado, categoria == 6)

  calidad_empleo <- ocupado

  pensiones_experiencia <- flag_and(
    universo_k,
    !is.na(edad) & edad >= 15,
    tiene_respuesta_vars(vars_orden(diccionario, 63:65))
  )

  otros_ingresos <- flag_and(
    universo_k,
    tiene_respuesta_vars(vars_orden(diccionario, 66:72))
  )

  emprendimiento <- flag_and(
    universo_k,
    tiene_respuesta_vars(vars_orden(diccionario, 73:75))
  )

  tributacion <- flag_and(
    universo_k,
    !is.na(edad) & edad >= 18,
    tiene_respuesta_vars(vars_orden(diccionario, 76))
  )

  trabajo_no_remunerado <- flag_and(
    universo_k,
    !is.na(edad) & edad >= 18,
    tiene_respuesta_vars(vars_orden(diccionario, 77))
  )

  experiencia_laboral <- flag_or_evidencia(
    ocupado,
    tiene_respuesta_vars(vars_orden(diccionario, 59:65))
  )

  violencia_laboral <- flag_and(
    universo_k,
    !is.na(edad) & edad >= 18,
    experiencia_laboral,
    tiene_respuesta_vars(vars_orden(diccionario, 78))
  )

  obs_universo <- if (!(edad_var %in% names(data)) && !("NPCEP4" %in% names(data))) {
    paste0(
      "No se encontro la variable de edad `", edad_var,
      "` ni la alternativa `NPCEP4`; el universo queda indeterminado."
    )
  } else if (!is.null(obs_edad_join)) {
    obs_edad_join
  } else {
    NULL
  }

  dplyr::bind_rows(
    fila_resumen(
      "0.0",
      "Universo de analisis",
      "Personas de 10 anos y mas.",
      1,
      universo_k,
      obs_universo
    ),
    fila_resumen(
      "1.0",
      "Identificacion inicial de actividad",
      "Personas que llegan a K1.",
      1,
      universo_k
    ),
    fila_resumen(
      "1.1",
      "Trabajando",
      "Personas que en K1 reportan estar trabajando.",
      1,
      k1_trabajando
    ),
    fila_resumen(
      "1.2",
      "No trabajando",
      "Personas que en K1 reportan una actividad diferente a trabajar.",
      1,
      k1_no_trabajando
    ),
    fila_resumen(
      "2.0",
      "Filtros de rescate de ocupacion",
      "Personas no clasificadas como trabajando en K1 y evaluadas en K2-K4.",
      c(2, 3, 4, 7),
      rescate_ocupacion
    ),
    fila_resumen(
      "2.1",
      "Ruta ocupados",
      "Personas clasificadas como ocupadas por K1 o por respuesta positiva en filtros K2-K4.",
      c(1, 2, 3, 4, 7),
      ocupado
    ),
    fila_resumen(
      "2.2",
      "Ruta busqueda",
      "Personas no clasificadas como ocupadas.",
      8:16,
      ruta_busqueda
    ),
    fila_resumen(
      "2.2.1",
      "Desocupado",
      "Personas no ocupadas con evidencia de busqueda o disponibilidad en K8-K16.",
      8:16,
      desocupado
    ),
    fila_resumen(
      "2.2.2",
      "Inactivo",
      "Personas no ocupadas que no cumplen condicion agregada de desocupacion.",
      8:16,
      inactivo
    ),
    fila_resumen(
      "2.2.3",
      "Inactivo disponible",
      "Personas inactivas que desean trabajar, pero no quedan clasificadas como desocupadas.",
      10,
      inactivo_disponible
    ),
    fila_resumen(
      "3.0",
      "Caracterizacion del empleo",
      "Personas ocupadas que llegan a K23.",
      23,
      llega_k23
    ),
    fila_resumen(
      "3.1",
      "Asalariados",
      "Ocupados con categoria ocupacional K23 en categorias 1, 2, 3, 7 u 8.",
      23,
      asalariado
    ),
    fila_resumen(
      "3.2",
      "Independientes",
      "Ocupados con categoria ocupacional K23 en categorias 4 o 5.",
      23,
      independiente
    ),
    fila_resumen(
      "3.3",
      "Sin remuneracion",
      "Ocupados con categoria ocupacional K23 igual a 6.",
      23,
      sin_remuneracion
    ),
    fila_resumen(
      "4.0",
      "Calidad del empleo y entorno laboral",
      "Ocupados que deberian pasar por las preguntas K45-K57.",
      45:57,
      calidad_empleo,
      "Conteo esperado por ruta agregada; no valida respuesta efectiva variable por variable."
    ),
    fila_resumen(
      "5.1",
      "Pensiones y experiencia",
      "Personas de 15 anos y mas que llegan al bloque K63-K65.",
      63:65,
      pensiones_experiencia
    ),
    fila_resumen(
      "5.2",
      "Otros ingresos",
      "Personas del universo general que llegan al bloque K66-K72.",
      66:72,
      otros_ingresos
    ),
    fila_resumen(
      "5.3",
      "Emprendimiento",
      "Personas del universo general que llegan al bloque K73-K75.",
      73:75,
      emprendimiento
    ),
    fila_resumen(
      "5.4",
      "Tributacion",
      "Personas de 18 anos y mas que llegan a K76.",
      76,
      tributacion
    ),
    fila_resumen(
      "5.5",
      "Trabajo no remunerado",
      "Personas de 18 anos y mas que llegan a K77.",
      77,
      trabajo_no_remunerado
    ),
    fila_resumen(
      "6.0",
      "Experiencias de violencia laboral",
      "Personas de 18 anos y mas ocupadas o con experiencia laboral que llegan a K78.",
      78,
      violencia_laboral
    )
  )
}
