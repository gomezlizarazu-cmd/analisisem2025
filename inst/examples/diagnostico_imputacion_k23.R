# Ejemplo de diagnostico de no respuesta para K23 / NPCKP17.
# Requiere que `dfs` ya exista como lista nombrada de capitulos EM 2025.

trazadoras_k23 <- tibble::tribble(
  ~var,      ~cap, ~alias,
  "NPCEP4",  "E",  "edad",
  "NPCEP5",  "E",  "sexo_codigo",
  "CLASE",   "A",  "clase",
  "NPCFP2",  "F",  "regimen_salud",
  "NPCHP4",  "H",  "nivel_educativo"
)

etiquetas_k23 <- list(
  sexo_codigo = c(
    "1" = "Hombre",
    "2" = "Mujer"
  ),
  clase = c(
    "1" = "Urbano",
    "2" = "Centro poblado",
    "3" = "Rural disperso"
  ),
  regimen_salud = c(
    "1" = "Contributivo",
    "3" = "Subsidiado",
    "2" = "Especial / excepción",
    "9" = "No sabe / no informa"
  ),
  nivel_educativo = c(
    "1" = "Ninguno",
    "2" = "Preescolar",
    "3" = "Básica primaria",
    "4" = "Básica secundaria",
    "5" = "Media",
    "6" = "Técnico",
    "7" = "Tecnológico",
    "8" = "Universitaria incompleta",
    "9" = "Universitaria completa",
    "10" = "Especialización incompleta",
    "11" = "Especialización completa",
    "12" = "Maestría incompleta",
    "13" = "Maestría completa",
    "14" = "Doctorado incompleto",
    "15" = "Doctorado completo"
  ),
  grupo_edad = function(edad) {
    edad <- suppressWarnings(as.numeric(edad))
    factor(
      dplyr::case_when(
        edad >= 10 & edad <= 17 ~ "10-17",
        edad >= 18 & edad <= 24 ~ "18-24",
        edad >= 25 & edad <= 34 ~ "25-34",
        edad >= 35 & edad <= 44 ~ "35-44",
        edad >= 45 & edad <= 54 ~ "45-54",
        edad >= 55 & edad <= 64 ~ "55-64",
        edad >= 65 ~ "65+",
        TRUE ~ "No informado"
      ),
      levels = c(
        "10-17", "18-24", "25-34", "35-44",
        "45-54", "55-64", "65+", "No informado"
      )
    )
  }
)

res_k23 <- diagnostico_imputacion_variable(
  dfs = dfs,
  base_cap = "K",
  variable_objetivo = "NPCKP17",
  codigos_validos = as.character(1:8),
  expresion_universo =
    edad >= 10 &
    (
      as.character(NPCKP1) == "1" & as.character(NPCKP2_1) == "1" |
        as.character(NPCKP2) == "1" |
        as.character(NPCKP3) == "1" &
          as.character(NPCKP5_1) %in% c("1", "2", "3", "4") |
        as.character(NPCKP3) == "1" &
          as.character(NPCKP5_1) %in% c("5", "6", "7", "8") &
          as.character(NPCKP6_1) == "1" |
        as.character(NPCKP4) == "1"
    ),
  trazadoras = trazadoras_k23,
  variables_analisis = c(
    grupo_edad = "edad",
    sexo = "sexo_codigo",
    zona = "clase",
    regimen_salud_lbl = "regimen_salud",
    nivel_educativo_lbl = "nivel_educativo"
  ),
  etiquetas = etiquetas_k23,
  excluir_no_informado = TRUE,
  generar_graficos = requireNamespace("plotly", quietly = TRUE)
)

res_k23$resumen_variable_objetivo
res_k23$diagnosticos$grupo_edad$disimilitud
res_k23$diagnosticos$nivel_educativo_lbl$delta
