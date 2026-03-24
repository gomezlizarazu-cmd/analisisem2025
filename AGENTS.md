# AGENTS.md

## Contexto del proyecto

Este paquete (`analisisem2025`) contiene utilidades para validación y análisis operativo de la Encuesta Multipropósito (EM) del DANE.

El enfoque principal es:
- Validación de consistencia entre capítulos
- Diagnóstico de cobertura y completitud
- Análisis de flujo y caídas de información
- Construcción de bases derivadas para análisis

---

## Estructura de datos clave

### Niveles de análisis

- Vivienda → `DIRECTORIO`
- Hogar → `DIRECTORIO`, `SECUENCIA_P`
- Persona → `DIRECTORIO`, `SECUENCIA_P`, `ORDEN`

---

## Llaves de joins (CRÍTICO)

- Vivienda: `DIRECTORIO`
- Hogar: `DIRECTORIO + SECUENCIA_P`
- Persona: `DIRECTORIO + SECUENCIA_P + ORDEN`

⚠️ Regla fundamental:
Nunca hacer joins sin especificar explícitamente estas llaves.

---

## Convenciones de variables

Variables comunes:
- `DIRECTORIO`: identificador de vivienda
- `SECUENCIA_P`: identificador de hogar
- `ORDEN`: identificador de persona
- `SEGMENTO`: unidad operativa
- `CLASE`: urbano/rural
- `UUID`: identificador de encuesta

Variables de estado:
- `encuesta_completa`
- `encuestas_completas`
- `encuestas_efectivas`
- `encuestas_totales`

Variables de diagnóstico:
- `caida`, `cae_campo`, `cae_flujo`
- `pct_falla`, `pct_faltantes`
- `n_caps_faltantes`, `n_caps_requeridos`

---

## Estilo de código

- Uso de tidyverse (principalmente `dplyr`)
- Uso de NSE (variables sin comillas)
- Variables se usan directamente (ej: `DIRECTORIO`, no `.data$DIRECTORIO`)
- Uso de `utils::globalVariables()` para evitar warnings

---

## Reglas para modificar código

### 1. NO cambiar lógica sin justificación explícita
- Mantener outputs actuales
- Evitar cambios silenciosos en resultados

### 2. NO romper joins existentes
- Validar cardinalidad antes y después
- Evitar duplicación de registros

### 3. Mantener consistencia de nombres
- No renombrar variables existentes sin necesidad

### 4. Validar cambios con:
```r
devtools::load_all()
devtools::check()
