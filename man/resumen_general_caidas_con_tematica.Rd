\name{resumen_general_caidas_con_tematica}
\alias{resumen_general_caidas_con_tematica}
\title{Resumen general de caidas con tematica por nivel}
\usage{
resumen_general_caidas_con_tematica(diagnostico_con_tematica)
}
\arguments{
\item{diagnostico_con_tematica}{Objeto devuelto por
\code{diagnostico_caidas_con_tematica()}.}
}
\value{
Un \code{tibble} con una fila por nivel y las columnas:
\code{nivel}, \code{unidades_totales}, \code{caidas_existencia},
\code{caidas_lina}, \code{caidas_campo}, \code{caidas_duplicado},
\code{caidas_tematica}, \code{caidas_en_al_menos_un_criterio},
\code{caidas_en_todos_los_criterios}, \code{pct_existencia},
\code{pct_logica}, \code{pct_campo}, \code{pct_duplicado},
\code{pct_tematica}, \code{pct_al_menos_1} y \code{pct_todos}.
}
\description{
Resume, por nivel de analisis, el diagnostico integrado devuelto por
\code{diagnostico_caidas_con_tematica()}, sin recalcular criterios ni modificar
sus salidas originales.
}
\details{
La funcion trabaja directamente sobre \code{viviendas_eval},
\code{hogares_eval} y \code{personas_eval}, manteniendo la granularidad propia
de cada nivel:

\itemize{
\item vivienda: \code{DIRECTORIO}
\item hogar: \code{DIRECTORIO + SECUENCIA_P}
\item persona: \code{DIRECTORIO + SECUENCIA_P + ORDEN}
}

Cuando \code{cae_duplicado} no esta materializado en \code{viviendas_eval} o
\code{hogares_eval}, la funcion lo deriva desde \code{reporte_final_caidas} y
lo agrega explicitamente por llave, respetando que el criterio de duplicados
se detecta originalmente a nivel persona.
}
\examples{
\dontrun{
diag_con_tematica <- diagnostico_caidas_con_tematica(dfs)
resumen_general_caidas_con_tematica(diag_con_tematica)
}
}
