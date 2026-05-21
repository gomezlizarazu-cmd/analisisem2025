\name{construir_sabana_casos_recuperables_desde_diagnostico}
\alias{construir_sabana_casos_recuperables_desde_diagnostico}
\title{Construir sabana de casos recuperables desde el diagnostico completo}
\usage{
construir_sabana_casos_recuperables_desde_diagnostico(
  diag_con_tematica,
  dfs = NULL,
  carpeta_raiz = ".",
  archivo = "sabana_auditoria_casos_recuperables.xlsx",
  cap_hog = "C",
  cap_per = "E"
)
}
\arguments{
\item{diag_con_tematica}{Objeto producido por
\code{diagnostico_caidas_con_tematica()}.}

\item{dfs}{Lista nombrada de data frames por capitulo. Si es \code{NULL}, se
intenta usar \code{diag_con_tematica$dfs}.}

\item{carpeta_raiz}{Carpeta donde se exportara la sabana.}

\item{archivo}{Nombre del archivo Excel de salida.}

\item{cap_hog}{Capitulo de control de hogares. Por defecto \code{"C"}.}

\item{cap_per}{Capitulo de personas. Por defecto \code{"E"}.}
}
\value{
La salida de \code{construir_sabana_casos_recuperables()}, agregando los insumos
calculados internamente: \code{diag_orden_fuera_E}, \code{diag_secuencia} y
\code{verificacion_diag}.
}
\description{
Prepara automaticamente los insumos de auditoria de llaves a partir del
objeto devuelto por \code{diagnostico_caidas_con_tematica()} y construye la
sabana de casos auditables o recuperables.
}
\details{
Esta funcion es el punto de entrada recomendado cuando ya existe
\code{diag_con_tematica}. Internamente usa
\code{verificar_universos_em_completa_diag()} para identificar personas del
reporte final que no tienen registro fisico en el capitulo \code{E}, clasifica el
tipo de problema de llave y luego delega la clasificacion de recuperabilidad
a \code{construir_sabana_casos_recuperables()}.
}
\examples{
\dontrun{
sabana <- construir_sabana_casos_recuperables_desde_diagnostico(
  diag_con_tematica = diag_con_tematica,
  dfs = dfs,
  carpeta_raiz = carpeta_caps
)
}
}
