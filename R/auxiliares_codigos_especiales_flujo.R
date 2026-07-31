# Auxiliares internos para distinguir respuestas observadas de antecedentes
# sustantivos en reglas de flujo. Los codigos especiales pueden seguir siendo
# respuestas validas de la variable que los contiene, pero no habilitan por si
# solos una pregunta subordinada que requiere informacion efectiva.

.normalizar_valor_flujo <- function(x) {
  valor <- trimws(as.character(x))
  vacios_texto <- c("", "NA", "N/A", "NULL", "NULO")
  vacio <- is.na(x) |
    is.na(valor) |
    toupper(valor) %in% vacios_texto
  valor[vacio] <- NA_character_
  valor
}

es_codigo_especial_flujo <- function(x, codigos = c(98, 99)) {
  valor <- .normalizar_valor_flujo(x)
  numero <- suppressWarnings(as.numeric(valor))
  codigos_num <- suppressWarnings(as.numeric(codigos))
  codigos_num <- codigos_num[!is.na(codigos_num)]

  out <- !is.na(numero) & numero %in% codigos_num
  out[is.na(out)] <- FALSE
  out
}

es_respuesta_sustantiva <- function(x, codigos_especiales = c(98, 99)) {
  valor <- .normalizar_valor_flujo(x)
  especial <- es_codigo_especial_flujo(
    valor,
    codigos = codigos_especiales
  )

  out <- rep(NA, length(valor))
  observado <- !is.na(valor)
  out[observado] <- !especial[observado]
  out
}

es_monto_sustantivo <- function(x,
                                codigos_especiales = c(98, 99),
                                permitir_cero = FALSE) {
  valor <- .normalizar_valor_flujo(x)
  numero <- suppressWarnings(as.numeric(valor))
  requiere_limpieza <- is.na(numero) & !is.na(valor)

  if (any(requiere_limpieza)) {
    texto_limpio <- valor[requiere_limpieza]
    texto_limpio <- gsub("\\$", "", texto_limpio)
    texto_limpio <- gsub("[[:space:]]+", "", texto_limpio)
    texto_limpio <- gsub("[\\.,]", "", texto_limpio)
    numero[requiere_limpieza] <- suppressWarnings(
      as.numeric(texto_limpio)
    )
  }

  especial <- es_codigo_especial_flujo(
    valor,
    codigos = codigos_especiales
  )
  out <- rep(NA, length(valor))
  out[especial] <- FALSE

  evaluable <- !is.na(valor) & !especial & !is.na(numero)
  out[evaluable & numero < 0] <- FALSE
  out[evaluable & numero > 0] <- TRUE
  out[evaluable & numero == 0] <- isTRUE(permitir_cero)
  out
}
