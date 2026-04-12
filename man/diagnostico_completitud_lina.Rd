\name{diagnostico_completitud_lina}
\alias{diagnostico_completitud_lina}
\title{Diagnostico de completitud por criterio Lina}
\usage{
diagnostico_completitud_lina(
  dfs,
  cap_viv_ctrl = "A",
  cap_viv_det = "B",
  cap_hog_ctrl = "C",
  cap_hog_gastos = "MA",
  cap_per = "E",
  incluir_existencia = TRUE,
  exportar_excel = FALSE,
  archivo = "encuestas_caidas_lina.xlsx",
  base_cap_persona = "E",
  base_hogar_cap = "B",
  caps_sabana = NULL,
  edad_var = "NPCEP4"
)
}
\arguments{
\item{dfs}{Lista nombrada de data frames por capitulo.}

\item{cap_viv_ctrl}{Capitulo con controles de vivienda.}

\item{cap_viv_det}{Capitulo con variables de vivienda requeridas para el
criterio Lina.}

\item{cap_hog_ctrl}{Capitulo con controles de hogar.}

\item{cap_hog_gastos}{Capitulo con variables de gastos requeridas para el
criterio Lina.}

\item{cap_per}{Capitulo de persona.}

\item{incluir_existencia}{Si es \code{TRUE}, calcula y cruza con
\code{diagnostico_cruce_capitulos(dfs)}.}

\item{exportar_excel}{Si es \code{TRUE}, exporta solo las encuestas caidas.}

\item{archivo}{Ruta del archivo Excel a generar.}

\item{base_cap_persona}{Capitulo base para construir la sabana ancha.}

\item{base_hogar_cap}{Capitulo de referencia para universo de hogares.}

\item{caps_sabana}{Orden de capitulos para la sabana ancha. Si es \code{NULL},
usa todos los nombres de \code{dfs}.}

\item{edad_var}{Variable de edad preferida para \code{pegar_tablas()} y cruces de
existencia. Si no existe, se intenta con \code{NPCEP4} y luego con \code{Edad}.}
}
\value{
Lista con evaluaciones por nivel, encuestas caidas y cruce contra
existencia. Si \code{exportar_excel = TRUE}, incluye \code{archivo}.
}
\description{
Evalua un criterio alternativo de completitud basado en variables de control
de los capitulos A, B, C, MA y E, manteniendo separadas las caidas por existencia
de las caidas por este nuevo criterio.
}
\details{
El diagnostico se construye por nivel:

\itemize{
\item vivienda: \code{DIRECTORIO}
\item hogar: \code{DIRECTORIO + SECUENCIA_P}
\item persona: \code{DIRECTORIO + SECUENCIA_P + ORDEN}
}

La regla especial corresponde al capitulo \code{B}, que se trata a nivel
vivienda por \code{DIRECTORIO}. Las validaciones de \code{C} y \code{MA} se
mantienen a nivel hogar.

Luego consolida las caidas a nivel de encuesta (hogar), propagando:

\itemize{
\item caida de vivienda a todos los hogares de la vivienda
\item caida de hogar a ese hogar
\item caida de persona al hogar de la persona
}

Finalmente cruza las encuestas caidas por criterio Lina contra
\code{resumen_caidas_regla2} del diagnostico estructural para identificar si la
caida es la misma o si es adicional frente al criterio de existencia.
}
