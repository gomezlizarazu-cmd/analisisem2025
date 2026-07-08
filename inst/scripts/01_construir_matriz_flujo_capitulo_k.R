devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")

matriz_flujo_k <- construir_matriz_flujo_capitulo_k(
  ruta_historico = "C:/Users/gomez/OneDrive/Documentos/Validaciones_EM_Basicas_2026/ImputacionK/01_Diagnostico_Perdida_CapK_.R",
  ruta_diccionario = "C:/Users/gomez/OneDrive/DANE/Multiproposito/Validacion/Encuestas/DICCIONARIO DE DATOS MULTIPROPOSITO 2025 V2.xlsx",
  ruta_formulario = "C:/Users/gomez/OneDrive/DANE/Multiproposito/Validacion/Encuestas/CNT-EM-IDR-001_FORMULARIO MULTIPROPOSITO 2025_VF.pdf",
  ruta_excel_flujo = "C:/Users/gomez/OneDrive/DANE/Multiproposito/Validacion/Encuestas/DANE_Formulario Encuesta Multipropósito 2025_Mayo_Versión 8_Definitivo (1).xlsx"
)

dir.create(
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025/inst/extdata/flujo_capitulo_k",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  matriz_flujo_k,
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025/inst/extdata/flujo_capitulo_k/matriz_flujo_k_pendiente_revision.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

View(matriz_flujo_k)
