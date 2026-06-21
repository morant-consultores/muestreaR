# Fase 5 — Preprocesamiento + asignación por estrato

**Fecha:** 2026-06-20
**Repo:** `morant-consultores/muestreaR`
**Branch:** `mejoras/fase5-preprocesamiento` (sobre `mejoras/fase4-api`)
**Autor del diseño:** auditoría asistida (Claude) + Emilio Morones

---

## 1. Contexto

Dos necesidades surgidas al construir la muestra real de Edomex 2026:

1. **Asignación de manzanas por estrato.** La prueba sobre el marco estatal real
   mostró que 7 municipios chicos no tienen 30 secciones (las que requieren 300
   entrevistas a 2 manzanas/sección). Necesitan **más manzanas por sección**, pero
   hoy `manzanas_por_seccion` es un escalar global.

2. **Preprocesamiento como funciones.** Todo lo que se hace *antes* de diseñar la
   muestra (cargar shapefiles + lista nominal del INE, corregir la lista nominal,
   construir la población) hoy son ~130 líneas imperativas que se copian entre
   scripts. Deben ser **funciones con un objetivo claro**, para que cada paso sea
   explícito, reutilizable y testeable.

Ambas partes son **aditivas** (no rompen nada existente).

**Cambio metodológico adicional:** Morant **ya no usa cuotas ni sustituciones**
(ante no respuesta, rellenar cuotas o sustituir reemplaza al difícil-de-contactar
por el fácil → incrementa el sesgo; la no respuesta se maneja por sobremuestreo
vía `tasa_rechazo`). Por tanto `disenar_muestra_ine()` pasa a `calcular_cuotas =
FALSE` por defecto (las cuotas quedan como legado opcional). Esto vuelve casi
irrelevante el hardcodeo de cuotas a municipio.

---

## 2. Parte A — `manzanas_por_seccion` por estrato

Igual que `tasa_rechazo`: puede ser un **escalar global** o una **columna en la
tabla `estratos`**.

- `calcular_asignacion()`: si `estratos` trae columna `manzanas_por_seccion`, se
  usa por fila; si no, el argumento escalar. Se valida `>= 1`.
- `validar_estratos()` y `disenar_muestra_ine()`: pasan la columna a través.
- Resuelve el caso Edomex: a los 7 municipios chicos se les pone más manzanas/
  sección (p. ej. Xonacatlán: 16 secciones × 4 manzanas × 5 = 320 ≥ 300).

**Criterio:** `calcular_asignacion(estratos con columna manzanas_por_seccion)`
produce las secciones/levantamiento correctos por estrato; el ejemplo de
referencia escalar sigue igual.

---

## 3. Parte B — Funciones de preprocesamiento del marco INE

Convertir el preámbulo de los scripts municipales en funciones nombradas, cada
una con su objetivo. Replican la lógica probada en producción (sin cambiarla),
solo la encapsulan y documentan.

### 3.1 `leer_cartografia_ine(carpeta, distrito = c("ninguno", "local", "federal"))`
**Objetivo:** cargar solo los shapefiles del INE **necesarios** y devolverlos como
una lista de objetos `sf` en CRS 4326. Moderniza `rgdal::readOGR` (retirado en
2023) a `sf::st_read`.
- **Siempre:** `MANZANA`, `LOCALIDAD`, `MUNICIPIO`, `SECCION`. El municipio se
  necesita siempre (ubicación en campo, aunque los estratos sean por distrito); la
  sección siempre es necesaria.
- **Distrito es uno u otro:** `distrito = "local"` añade `DISTRITO_LOCAL`;
  `distrito = "federal"` añade `DISTRITO_FEDERAL`; `"ninguno"` (default) ninguno.
- **Nunca** carga `MANCHA_URBANA` (no se usa; se elimina del flujo).
- **Devuelve:** `list(mza, loc, mun, sec[, distrito])`.

> **Nota de hardcodeo (importante):** el paquete arma las **cuotas siempre por
> municipio** (`cuotas_ine` usa `NOMBRE_MUN`), lo cual es intencional (campo se
> ubica por municipio). El **nivel de estratificación** sí es parametrizable vía
> `variable_estrato` en `disenar_muestra_ine`. Esta fase **no** modifica ese
> hardcodeo; las funciones de preprocesamiento no tocan el muestreo ni las cuotas.

### 3.2 `leer_lista_nominal_ine(ruta, entidad = 15)`
**Objetivo:** leer el xlsx de lista nominal por rango de edad y sexo, normalizar
los nombres de columna y filtrar a la entidad.
- **Devuelve:** `tibble` de lista nominal (`ln`).

### 3.3 `corregir_lista_nominal(ln, base_ine)`
**Objetivo:** reconciliar la lista nominal por edad/sexo (por sección) con la base
del INE por localidad, reescalando los conteos por sexo (la lógica `auxi` →
`ln_sexo`/`ln_edad_sexo` → `ln2` de los scripts). Es el paso que hoy más fácil se
copia mal.
- **Devuelve:** `tibble` de lista nominal corregida (`ln2`).

### 3.4 `construir_poblacion_ine(ln, electoral, cartografia)`
**Objetivo:** envoltura declarativa de `PoblacionINE$new()` que arma la población
a partir de la lista nominal corregida, la base electoral y la lista de
cartografías, dejando claro qué insumo va en cada lugar.
- **Devuelve:** objeto `PoblacionINE`.

Con esto, el preámbulo de un script de muestra pasa de ~130 líneas a:
```r
cart <- leer_cartografia_ine("~/.../INE/SHP/2023/15 MEXICO")
ln   <- leer_lista_nominal_ine("~/.../ln_re_sexo.xlsx", entidad = 15)
ln2  <- corregir_lista_nominal(ln, base_ine)
pob  <- construir_poblacion_ine(ln2, electoral, cart)
```

### Fuera de alcance
- La app/skill (se apoyan en esta API).
- Herencia `*INE`, sustituciones sin IO.

---

## 4. Plan de commits atómicos

1. `feat: manzanas_por_seccion por estrato en calcular_asignacion/validar/disenar`
2. `test: asignación de manzanas por estrato (caso municipios chicos)`
3. `feat: leer_cartografia_ine() — cargar shapefiles INE (sf, CRS 4326)`
4. `feat: leer_lista_nominal_ine() + corregir_lista_nominal() — lista nominal del INE`
5. `feat: construir_poblacion_ine() — envoltura declarativa de PoblacionINE`
6. `test: preprocesamiento sobre insumos sintéticos`
7. `docs: documentar el preprocesamiento (vignette/README) + actualizar el script Edomex`

Cada commit deja el paquete cargable y la suite verde.

---

## 5. Criterios de aceptación

- [ ] `manzanas_por_seccion` acepta escalar **y** columna por estrato; resuelve los
  7 municipios chicos de Edomex (validación pasa con manzanas mayores).
- [ ] Las 4 funciones de preprocesamiento replican el resultado del preámbulo
  actual (se valida `corregir_lista_nominal` contra una muestra de su salida).
- [ ] `leer_cartografia_ine` usa `sf::st_read` (no `rgdal`).
- [ ] API aditiva: suite existente verde; `R CMD check` sin nuevos ERRORS.
- [ ] El script `crear_muestra_edomex_2026.R` se simplifica usando las funciones.

---

## 6. Riesgos
- **`corregir_lista_nominal`** es la lógica más delicada (pivots/joins). Mitigación:
  copiarla verbatim de los scripts probados, encapsular sin reescribir, y validar
  con datos sintéticos que reproduzcan la estructura.
- **Dependencias de lectura** (`readxl`, `sf`): añadir a `Imports`/`Suggests` según
  corresponda. `rgdal`/`sp` quedan fuera (retirados).
- No tengo los archivos del Drive: el preprocesamiento se valida con fixtures
  sintéticos que imitan el formato del INE; la prueba final con datos reales la
  haces tú.
