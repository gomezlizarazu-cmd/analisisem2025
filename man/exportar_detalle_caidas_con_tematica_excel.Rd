\name{exportar_detalle_caidas_con_tematica_excel}
\alias{exportar_detalle_caidas_con_tematica_excel}
\title{Exportar detalle de caidas con tematica a Excel}
\usage{
exportar_detalle_caidas_con_tematica_excel(
  diagnostico_con_tematica,
  ruta,
  incluir_hojas_vacias = FALSE
)
}
\arguments{
\item{diagnostico_con_tematica}{Objeto devuelto por
\code{diagnostico_caidas_con_tematica()}.}

\item{ruta}{Ruta del archivo \code{.xlsx} de salida.}

\item{incluir_hojas_vacias}{Si es \code{TRUE}, crea una hoja con nota cuando una
combinacion nivel-criterio no tenga registros.}
}
\value{
Lista con:
\describe{
\item{archivo}{Ruta normalizada del Excel exportado.}
\item{hojas_exportadas}{Vector con los nombres de hojas escritas.}
\item{hojas_omitidas}{Vector con los nombres de hojas omitidas por no tener
registros cuando \code{incluir_hojas_vacias = FALSE}.}
\item{control_exportacion}{Tabla de validacion entre el resumen general y el
numero de filas exportado por hoja.}
}
}
\description{
Exporta a Excel un resumen general, la sabana consolidada final y el detalle de
caidas por nivel y por criterio a partir de la salida de
\code{diagnostico_caidas_con_tematica()}.
}
\details{
La exportacion conserva explicitamente la granularidad de cada nivel:

\itemize{
\item vivienda: \code{DIRECTORIO}
\item hogar: \code{DIRECTORIO + SECUENCIA_P}
\item persona: \code{DIRECTORIO + SECUENCIA_P + ORDEN}
}

La hoja \code{resumen_general} se construye con
\code{resumen_general_caidas_con_tematica()}. La hoja
\code{reporte_final_caidas} contiene la salida consolidada final del
diagnostico. Las hojas de detalle se construyen reutilizando
\code{viviendas_eval}, \code{hogares_eval}, \code{personas_eval},
\code{reporte_final_caidas} y, cuando hace falta, salidas ya calculadas de
existencia, Lina, tematica y duplicados. No recalcula diagnosticos.

Para vivienda y hogar, las columnas textuales se consolidan por llave usando
valores unicos no vacios concatenados con \code{" | "}. Esta regla se aplica
de forma estable a \code{criterios_reporte},
\code{criterio_principal_reporte}, \code{razon_principal_caida},
\code{variable_principal_caida}, \code{valor_principal_caida} y
\code{observacion_final}.

Por defecto, las hojas sin registros se omiten para mantener el archivo
compacto. Si se desea una estructura fija, puede usarse
\code{incluir_hojas_vacias = TRUE}.
}
\examples{
\dontrun{
diag_con_tematica <- diagnostico_caidas_con_tematica(dfs)
exportar_detalle_caidas_con_tematica_excel(
  diagnostico_con_tematica = diag_con_tematica,
  ruta = "detalle_caidas_con_tematica.xlsx"
)
}
}
