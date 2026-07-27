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

## Naturaleza del capítulo y uso de llaves (CRÍTICO)

Las llaves de cruce deben definirse según la naturaleza del capítulo y el nivel operativo al que pertenece la información, no únicamente según las columnas que aparezcan disponibles en la tabla.

Regla por nivel:

- Capítulos de vivienda: usar `DIRECTORIO`
- Capítulos de hogar: usar `DIRECTORIO + SECUENCIA_P`
- Capítulos de persona: usar `DIRECTORIO + SECUENCIA_P + ORDEN`

### Regla crítica sobre `ORDEN`

`ORDEN` solo tiene interpretación sustantiva en capítulos de persona.

En capítulos de vivienda o de hogar, `ORDEN` no debe interpretarse como identificador de persona, aunque la variable exista en la base por razones de estructura, arrastre, exportación, formato o integración operativa.

Por tanto:

- No usar `ORDEN` para hacer joins de capítulos de vivienda.
- No usar `ORDEN` para hacer joins de capítulos de hogar.
- No usar `ORDEN` para diagnosticar faltantes o caídas en capítulos que no son de persona.
- No usar `ORDEN` para recuperar casos cuando la fuente de información proviene de capítulos de vivienda u hogar.
- No concluir que una persona está presente, ausente o recuperable a partir de `ORDEN` en capítulos no personales.

### Riesgo metodológico

Usar `ORDEN` fuera de capítulos de persona puede generar:

- falsos diagnósticos de pérdida;
- falsas inconsistencias entre capítulos;
- duplicaciones por joins indebidos;
- recuperaciones incorrectas de personas;
- clasificación errónea de encuestas como completas, incompletas o recuperables.

### Protocolo obligatorio para el agente

Antes de cualquier cruce, validación o diagnóstico entre capítulos, el agente debe identificar explícitamente:

1. cuál es el capítulo fuente;
2. cuál es la naturaleza del capítulo: vivienda, hogar o persona;
3. cuál es la llave válida para ese capítulo;
4. si el uso de `ORDEN` está justificado por tratarse de un capítulo de persona.

Si el agente detecta código que utiliza `ORDEN` en capítulos de vivienda u hogar, debe:

- detenerse;
- reportar el archivo, función y línea donde ocurre;
- explicar por qué puede ser riesgoso;
- no modificar la lógica automáticamente sin aprobación explícita del usuario.

Esta regla es especialmente importante en diagnósticos de completitud, clasificación de encuestas caídas, identificación de casos recuperables y cruces entre capítulos.

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

El agente puede crear, modificar y ejecutar pruebas unitarias específicas cuando utilicen exclusivamente datos sintéticos construidos dentro del propio archivo de prueba.

El usuario mantiene el control exclusivo sobre:

- la carga y el procesamiento de bases reales;
- la ejecución de scripts operativos;
- los diagnósticos completos;
- las imputaciones;
- la generación de salidas;
- la validación sustantiva en RStudio.

Después de modificar código, el agente debe preparar instrucciones exactas para que el usuario valide manualmente los resultados sobre datos reales, comenzando por:

```r
devtools::load_all(
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"
)
```

`devtools::check()` no debe ejecutarse ni sugerirse como validación rutinaria. Solo puede proponerse cuando sea estrictamente necesario, explicando previamente su alcance y solicitando autorización.

### 5. Utilizar funciones existentes

Las funciones existentes ya resolvieron muchos de los problemas emergentes. Usar siempre las funciones del paquete. Solo crear funciones nuevas cuando no exista una alternativa y sea estrictamente necesario.

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

Contiene:

- Nombre de variable
- Descripción
- Dominio
- Tipo

Reglas:

- Toda variable usada en validaciones debe poder mapearse al diccionario.
- Usar nombres de variables en mayúscula.
- Normalizar espacios y codificación de texto.

Convenciones:

- Variables en errores: `error_<VAR>`.
- Para buscar la descripción, limpiar prefijos y usar el nombre base.
- Usar `get_desc_fina()` cuando esté disponible.

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


## Protocolo de trabajo del agente: edición local sin ejecución de datos (CRÍTICO)

El agente actúa exclusivamente como asistente de inspección estática, edición y revisión del paquete.

La ejecución sobre datos reales, los scripts operativos, el procesamiento de información y la validación sustantiva corresponden exclusivamente al usuario en su sesión local de RStudio.

El agente puede ejecutar únicamente pruebas unitarias específicas con datos completamente sintéticos, bajo las condiciones definidas en este documento.

### Repositorio operativo único

El repositorio operativo y canónico es:

```text
C:/Users/gomez/OneDrive/Documentos/analisisem2025
```

Todos los cambios del paquete deben realizarse directamente en esa carpeta.

No crear, utilizar ni modificar:

```text
C:/Users/gomez/.codex/worktrees/...
```

ni otros worktrees, clones, copias temporales o repositorios alternativos, salvo autorización expresa del usuario.

Si una rama necesaria está activa en otro worktree, el agente debe detenerse, informar al usuario y esperar instrucciones. No debe continuar el desarrollo en ese worktree, eliminarlo ni descartar cambios.

El usuario recupera inmediatamente los cambios mediante:

```r
devtools::load_all(
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"
)
```

Una modificación no se considera entregada hasta que el archivo actualizado se encuentre físicamente en este repositorio local.

### Uso de Git y GitHub

Git y GitHub se utilizan para:

- administrar ramas;
- revisar diferencias;
- versionar y respaldar cambios;
- integrar posteriormente los cambios a `main`.

El agente no debe trabajar directamente sobre `main`.

Antes de modificar archivos, debe verificar la rama activa y el estado del repositorio mediante:

```powershell
git -C "C:\Users\gomez\OneDrive\Documentos\analisisem2025" status -sb
```

No debe cambiar de rama ni modificar el estado de Git sin autorización expresa.

### Acciones permitidas

El agente puede:

- inspeccionar archivos de código del paquete;
- inspeccionar pruebas existentes;
- buscar funciones, objetos y dependencias;
- revisar diferencias de Git;
- modificar funciones, documentación o pruebas cuando la tarea lo autorice;
- crear, modificar y ejecutar pruebas unitarias específicas con datos completamente sintéticos;
- cargar el paquete local mediante `devtools::load_all()` únicamente para ejecutar esas pruebas sintéticas;
- ejecutar `testthat::test_file()` únicamente sobre archivos de prueba específicos relacionados con la tarea;
- preparar scripts o bloques de validación para que el usuario los ejecute sobre datos reales;
- ejecutar comandos livianos de inspección estática.

Comandos permitidos, entre otros:

```text
git status
git diff
git diff --stat
git branch --show-current
ls
dir
find
grep
rg
Get-Content
Get-ChildItem
Select-String
```

### Prohibición de acceso y ejecución sobre datos

El agente no debe cargar, abrir, inspeccionar, resumir ni procesar bases de datos reales de la encuesta.

Sin autorización expresa del usuario, no debe acceder a archivos operativos ubicados fuera del repositorio del paquete, incluyendo carpetas como:

```text
C:/Users/gomez/OneDrive/DANE/Multiproposito/...
C:/Users/gomez/OneDrive/Documentos/Validaciones_EM_Basicas_2026/...
```

Solo puede modificar un script operativo externo cuando el usuario autorice explícitamente su ruta exacta. Esa autorización de edición no implica autorización para ejecutarlo ni para acceder a los datos que utiliza.

El agente no debe ejecutar comandos o funciones que carguen datos reales, scripts operativos o conjuntos amplios de pruebas, incluyendo:

```text
readRDS()
read.csv()
read_csv()
read_csv2()
read_excel()
arrow::read_parquet()
load()
source()
devtools::test()
devtools::check()
devtools::document()
testthat::test_dir()
quarto::quarto_render()
```

`devtools::load_all()` y `testthat::test_file()` están permitidos exclusivamente para cargar el paquete local y ejecutar un archivo de pruebas unitarias sintéticas específico. No pueden utilizarse para acceder a bases reales, diccionarios operativos, scripts externos ni resultados de la encuesta.

Tampoco debe ejecutar:

- funciones del paquete;
- diagnósticos completos;
- pipelines de la encuesta;
- scripts operativos;
- conteos o frecuencias sobre registros reales;
- exportaciones RDS, Excel, CSV o Parquet;
- renderizados de Quarto;
- procesos de imputación;
- validaciones que requieran cargar objetos de datos.

Esta prohibición aplica aunque la ejecución parezca rápida o liviana.

### Pruebas unitarias sintéticas

El agente puede crear, modificar y ejecutar pruebas unitarias cuando utilicen exclusivamente datos sintéticos construidos dentro del propio archivo de prueba.

Se consideran pruebas sintéticas aquellas que:

- crean todos sus datos mediante `tibble()`, `data.frame()`, vectores, listas u otros objetos definidos dentro del test;
- no leen archivos externos;
- no utilizan registros reales de la encuesta;
- no acceden a rutas operativas fuera del repositorio;
- no dependen de objetos previamente cargados por el usuario;
- no generan exportaciones con información real;
- tienen alcance limitado a las funciones o reglas modificadas.

El agente puede ejecutar:

```r
ruta_paquete <-
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"

devtools::load_all(
  ruta_paquete
)

testthat::test_file(
  file.path(
    ruta_paquete,
    "tests/testthat/archivo_de_prueba.R"
  )
)
```

También puede ejecutar un archivo de prueba específico relacionado directamente con la tarea.

No debe ejecutar automáticamente:

```r
devtools::test()
testthat::test_dir()
devtools::check()
```

Estos comandos pueden activar conjuntos amplios de pruebas o procesos no relacionados y requieren autorización expresa del usuario.

Incluso dentro de una prueba, el agente no debe ejecutar funciones que accedan a información real o archivos operativos, como:

```r
readRDS()
read.csv()
readr::read_csv()
readr::read_csv2()
readxl::read_excel()
arrow::read_parquet()
load()
source()
```

Después de ejecutar pruebas sintéticas, el agente debe informar:

1. archivo de prueba ejecutado;
2. pruebas creadas o modificadas;
3. resultado `PASS`, `FAIL`, `WARN` y `SKIP`;
4. causa de cualquier falla;
5. si la falla corresponde al código del paquete o al diseño del test;
6. ajustes realizados para corregirla.

Una tarea no debe presentarse como validada cuando las pruebas sintéticas relacionadas todavía reporten fallas.

La ejecución de pruebas sintéticas no reemplaza la validación final del usuario sobre los datos reales.

### Scripts operativos

Los scripts operativos pueden encontrarse fuera del paquete, especialmente en:

```text
C:/Users/gomez/OneDrive/Documentos/Validaciones_EM_Basicas_2026
```

El agente no debe ejecutarlos.

Solo puede modificarlos cuando el usuario autorice explícitamente el archivo exacto.

Cuando una función del paquete cambie, el agente debe indicar qué script o sección debe volver a ejecutar el usuario, pero no debe realizar la ejecución.

Los scripts operativos deben cargar siempre el paquete desde:

```r
devtools::load_all(
  "C:/Users/gomez/OneDrive/Documentos/analisisem2025"
)
```

Los scripts operativos no deben duplicar reglas, funciones o lógica ya implementadas en el paquete.

La separación esperada es:

- el paquete contiene funciones, reglas y validaciones reutilizables;
- los scripts operativos cargan bases, llaman las funciones y generan salidas;
- las bases y resultados se almacenan fuera del repositorio del paquete.

### Antes de modificar archivos

Antes de editar, el agente debe:

1. confirmar la ruta del repositorio operativo;
2. verificar la rama activa;
3. ejecutar `git status -sb`;
4. identificar los archivos relacionados con la tarea;
5. detectar cambios previos ajenos;
6. explicar el cambio mínimo propuesto;
7. conservar sin alteración los cambios ajenos.

Si el cambio puede afectar resultados existentes y la instrucción del usuario no lo autoriza explícitamente, debe explicar el impacto y esperar aprobación.

### Después de modificar archivos

Después de editar, el agente debe reportar:

1. rama activa;
2. archivos modificados;
3. funciones o reglas modificadas;
4. lógica anterior y lógica nueva;
5. pruebas sintéticas creadas o actualizadas;
6. archivo de prueba ejecutado y resultado `PASS`, `FAIL`, `WARN` y `SKIP`;
7. supuestos aplicados;
8. riesgos o posibles efectos colaterales;
9. `git diff --stat`;
10. `git status -sb`;
11. comandos exactos para que el usuario cargue el paquete;
12. comandos exactos para que el usuario repita las pruebas, cuando resulte útil;
13. comandos exactos para que el usuario regenere las salidas afectadas con datos reales.

### Restricciones de Git

No ejecutar sin autorización expresa:

```text
git add
git commit
git push
git pull
git merge
git switch
git checkout
git restore
git reset
git clean
```

Al preparar un commit autorizado, agregar únicamente los archivos relacionados con la tarea mediante rutas explícitas. No usar `git add -A`.

No eliminar worktrees ni descartar cambios sin autorización.

### Documentación del paquete

No ejecutar:

```r
devtools::document()
```

salvo autorización expresa y revisión previa de su alcance.

No modificar manualmente `NAMESPACE` ni archivos de `man/` salvo autorización expresa y específica.

### Regla final de seguridad

Lionarx modifica el código del paquete local y puede ejecutar pruebas unitarias específicas con datos completamente sintéticos.

El usuario procesa las bases reales, ejecuta los scripts operativos, genera las salidas y realiza la validación sustantiva en su propio entorno de RStudio.

El agente no debe cargar, inspeccionar ni comprobar resultados utilizando información real de la encuesta.
