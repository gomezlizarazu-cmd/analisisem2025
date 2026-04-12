\name{exportar_reporte_encuestas_caidas_lina}
\alias{exportar_reporte_encuestas_caidas_lina}
\title{Exportar reporte de encuestas caidas por criterio Lina}
\usage{
exportar_reporte_encuestas_caidas_lina(
  res_lina,
  archivo = "encuestas_caidas_lina.xlsx"
)
}
\arguments{
\item{res_lina}{Resultado de \code{diagnostico_completitud_lina()}.}

\item{archivo}{Ruta del archivo Excel.}
}
\value{
Ruta normalizada del archivo exportado.
}
\description{
Exporta a Excel solo las encuestas caidas por criterio Lina y su cruce contra
existencia. Incluye una sabana ancha de la encuesta completa para las caidas.
}
