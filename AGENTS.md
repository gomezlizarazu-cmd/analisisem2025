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

## Cobertura diferencial por capítulo

No todos los capítulos aplican al mismo universo ni al mismo nivel operativo.

⚠️ Regla:
Antes de interpretar faltantes, diferencias de conteo o aparentes caídas, el agente debe verificar si el capítulo:

- aplica a vivienda, hogar o persona;
- aplica a todos los registros del nivel;
- o tiene una regla especial de cobertura.

Ejemplo crítico:
- Capítulo B: se usa un solo hogar por vivienda; por tanto, no se espera información de B para todos los hogares.

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

### 4. Dejar instrucciones de validación manual

El agente no debe ejecutar automáticamente pruebas pesadas.  
Después de modificar código, debe dejar instrucciones para que el usuario valide manualmente en RStudio, comenzando por:

```r
devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")
```

`devtools::check()` solo debe sugerirse cuando sea estrictamente necesario y no debe ejecutarse sin autorización explícita del usuario.

### 5. Utilizar funciones existentes
Las funciones existentes ya resolvieron muchos de los problemas emergentes. Usar siempre las funciones del paquete. Solo cuando no existan y sea estrictamente necesario crear nuevas.

---

## Niveles de granularidad (CRÍTICO)

La encuesta tiene estructura jerárquica:

- Vivienda: DIRECTORIO
- Hogar: DIRECTORIO + SECUENCIA_P
- Persona: DIRECTORIO + SECUENCIA_P + ORDEN

### Regla fundamental

Un join puede:

1. Mantener nivel
2. Subir nivel (agregación)
3. Bajar nivel (expansión)

### Bajar nivel NO es error si es intencional

Ejemplo:
- Base: hogar
- Se une capítulo de personas

Resultado:
- Cada hogar se repite por número de personas

✔️ Esto es correcto

### PERO debe cumplirse:

- El cambio de nivel debe ser explícito
- No debe ser implícito ni accidental

---

## Validación de joins

Antes de cualquier join, el agente debe identificar:

- nivel de la base
- nivel del capítulo a unir

Después del join:

- validar si el cambio de filas es esperado
- advertir si hay expansión no intencional

---

## Regla especial de integración: Capítulo B (CRÍTICO)

El Capítulo B tiene una lógica especial de unión y cobertura.

### Naturaleza del capítulo

El Capítulo B es un capítulo dirigido al hogar, pero conceptualmente recoge condiciones de la vivienda.  
Por diseño operativo, dentro de una misma vivienda solo se utiliza la información de **un único hogar** para representar este capítulo.

### Regla de join

Cuando se integre el Capítulo B con otras bases:

- **No debe esperarse cobertura para todos los hogares de la vivienda**
- La unión de B debe hacerse considerando **solo un hogar por vivienda**
- La ausencia de información de B en otros hogares de la misma vivienda **no constituye error**
- Tampoco debe interpretarse automáticamente como caída, incompletitud o inconsistencia estructural

### Implicación analítica

Si una vivienda tiene varios hogares:

- puede existir información de B para un solo hogar,
- y los demás hogares de esa misma vivienda pueden quedar sin valores de B,
- sin que ello implique problema de calidad ni falla de recolección.

### Regla para el agente

El agente debe tratar el Capítulo B como un caso especial:

- advertir que su cobertura esperada no es a nivel de todos los hogares;
- no marcar como caída la ausencia de B en hogares distintos al hogar seleccionado;
- no proponer “correcciones” para expandir artificialmente B a todos los hogares;
- documentar explícitamente cuando una comparación entre capítulos esté afectada por esta regla.

### Validación esperada

Al revisar conteos o diferencias entre capítulos, el agente debe recordar que:

- `nrow()` en B no debe compararse mecánicamente contra todos los hogares de la vivienda;
- la comparación debe hacerse bajo la lógica de “un hogar representativo por vivienda”;
- cualquier diagnóstico de cobertura de B debe aclarar esta restricción antes de concluir existencia de caídas.

---

## Interpretación de conteos

Las métricas pueden basarse en:

- llaves únicas (ej: hogares)
- filas reales (ej: personas)

⚠️ Nunca asumir que nrow() representa la unidad analítica

---

## Acciones esperadas del agente ante riesgos

Cuando el agente detecte:

### Posible duplicación por join
Debe:
- reportar el cambio en nrow()
- identificar el nivel antes y después
- NO corregir automáticamente

### Inconsistencia en conteos
Debe:
- comparar llaves únicas vs filas
- señalar posibles causas (join, duplicados, agregación)

### Uso de llaves incompletas
Debe:
- advertir explícitamente
- sugerir llaves correctas según nivel

⚠️ El agente nunca debe modificar la lógica sin instrucción explícita del usuario


## Diccionario de variables

El diccionario oficial de la encuesta se encuentra en:

```text
inst/diccionario/Diccionario_em2025.xlsx
```
Las descripciones de variables provienen de ese mismo diccionario oficial.

Contiene:
- Nombre de variable
- Descripción
- Dominio
- Tipo

Regla:
- Toda variable usada en validaciones debe poder mapearse al diccionario
- Usar nombres en mayúscula
- Normalizar espacios y encoding

Las descripciones de variables provienen del diccionario oficial:

inst/diccionario/diccionario_em2025.xlsx

Convenciones:
- Variables en errores: error_<VAR>
- Para buscar descripción: limpiar prefijos y usar nombre base
- Usar get_desc_fina() cuando esté disponible

---

## Definición operativa de encuesta caída (CRÍTICO)

Una "encuesta caída" no se define por una única condición, sino por la ocurrencia de al menos una falla en los criterios de validación implementados en el paquete.

### Principio general

Una encuesta se considera caída cuando, en el nivel de análisis correspondiente (vivienda, hogar o persona), se incumple al menos uno de los criterios definidos de:

- existencia en el flujo de capítulos
- completitud estructural entre capítulos
- completitud de campo
- reglas de control (Lina)
- detección de duplicados

### Regla general

Una unidad se clasifica como caída si:

`n_criterios_caida > 0`


donde `n_criterios_caida` corresponde al número de reglas de validación que fallan para esa unidad.

### Consideraciones importantes

- No toda diferencia entre capítulos implica caída
- No todo valor faltante implica caída
- La clasificación depende del:
  - nivel de análisis (vivienda, hogar, persona)
  - universo aplicable del capítulo
  - reglas de cobertura diferencial (ej: Capítulo B)

### Ejemplo crítico

La ausencia de información del Capítulo B en un hogar distinto al seleccionado dentro de la vivienda:

- NO constituye caída
- NO debe clasificarse como inconsistencia
- es consistente con el diseño operativo de la encuesta

### Regla para interpretación de resultados

Antes de concluir que existe una caída, el agente debe verificar:

1. si el capítulo aplica al nivel de análisis;
2. si existe cobertura diferencial;
3. si la diferencia proviene de un join o expansión de nivel;
4. si la regla corresponde a detección o solo a resumen/propagación.

⚠️ Conclusión:
Una caída es el resultado de la aplicación explícita de reglas de validación, no de diferencias descriptivas entre capítulos.

## Protocolo de trabajo del agente: edición sí, pruebas pesadas no (CRÍTICO)

El agente debe actuar como asistente de edición y revisión del repositorio, no como ejecutor autónomo de pruebas pesadas ni de pipelines completos.

### Regla principal

El agente puede:

- leer la estructura del paquete;
- inspeccionar funciones y dependencias;
- proponer planes de cambio;
- modificar archivos de código, documentación o tests cuando el usuario lo autorice;
- dejar instrucciones claras para validación manual.

El agente NO debe ejecutar automáticamente:

- pipelines completos de la encuesta;
- procesos sobre bases grandes;
- `devtools::check()`;
- renderizados de Quarto;
- scripts de análisis completos;
- exportaciones masivas;
- comandos que puedan tardar mucho o modificar grandes volúmenes de archivos;
- `git commit`, `git push`, `git merge` o cambios de rama sin autorización explícita.

### Pruebas y validación

La validación sustantiva será realizada por el usuario en RStudio.

Cuando el agente modifique código, debe entregar al final:

1. archivos modificados;
2. resumen técnico de los cambios;
3. explicación de la lógica ajustada;
4. supuestos aplicados;
5. riesgos o posibles efectos colaterales;
6. instrucciones exactas para que el usuario pruebe manualmente;
7. ejemplo mínimo de validación usando `devtools::load_all()`.

Ejemplo esperado de instrucciones de prueba:

```r
devtools::load_all("C:/Users/gomez/OneDrive/Documentos/analisisem2025")
```

Luego ejecutar manualmente la función modificada con objetos ya cargados por el usuario en RStudio.

### Ejecución de comandos

El agente solo puede ejecutar comandos livianos de inspección, por ejemplo:

```text
git status
git diff
ls
grep
find
Get-Content
Get-ChildItem
Select-String
rg
```

No debe ejecutar comandos que cambien el estado del repositorio o del entorno sin autorización.
### Antes de modificar archivos

Antes de editar, el agente debe:

- identificar las funciones relevantes;
- identificar los archivos que tocaría;
- explicar el cambio mínimo propuesto;
- esperar aprobación del usuario si el cambio puede afectar resultados existentes.

### Después de modificar archivos

Después de editar, el agente debe reportar:

Archivos modificados:
- R/archivo_1.R
- man/archivo_1.Rd, si aplica
- NAMESPACE, si aplica

Cambios realizados:
- ...

Supuestos:
- ...

Riesgos:
- ...

Cómo probar manualmente:
- ...

### Regla de seguridad

Si una tarea requiere comprobar resultados sobre bases grandes, el agente debe preparar el código o las instrucciones, pero no ejecutar la prueba. El usuario será quien corra la validación final en RStudio.
