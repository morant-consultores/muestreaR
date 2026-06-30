# Fase 4 — API declarativa y paramétrica (lista para skill/Shiny) de `muestreaR`

**Fecha:** 2026-06-20
**Repo:** `morant-consultores/muestreaR`
**Branch:** `mejoras/fase4-api` (sobre `mejoras/fase3-documentacion`)
**Autor del diseño:** auditoría asistida (Claude) + Emilio Morones

---

## 1. Contexto y objetivo

La API actual de `muestreaR` es **imperativa, con estado y orden implícito frágil**
(`agregar_nivel ×N → plan_muestra(por nivel) → fpc(2,3,0) → extraer → cuotas`,
sin validación). Además, los casos reales no son de asignación uniforme: la
directora razona en términos de **entrevistas objetivo por estrato** y de un
**modelo operativo** concreto, y hoy lo resuelve con `criterio = "manual"` pasando
un **vector posicional de secciones** que tunea a mano.

**Objetivo final:** que un cliente/objetivo de encuesta se traduzca en un diseño
sin escribir código de bajo nivel. La forma (skill que recibe un brief en lenguaje
natural, o app de Shiny) se decide después; ambas se apoyan en la **misma API
declarativa y paramétrica** que construye esta fase. La Fase 4 **no** construye la
skill ni la app.

### Principio: aditivo (no romper producción)
Toda la producción usa la API imperativa actual. La Fase 4 **añade** funciones de
alto nivel **sobre** los métodos existentes; éstos no cambian.

---

## 2. El modelo operativo (parámetros, no números mágicos)

Ver `[[modelo-operativo-muestreo]]`. Valores por defecto, todos configurables:

- **`n_0 = 5`** entrevistas por manzana.
- **`manzanas_por_seccion = 2`** (base) → 10 entrevistas efectivas por sección.
- **secciones por estrato = `entrevistas_objetivo / (n_0 × manzanas_por_seccion)`**.

### Asignación desproporcionada por estrato
Cada estrato puede tener distinto objetivo (sobremuestreo). Se declara como
**tabla nombrada** `estrato → entrevistas` (no un vector posicional). Ej. Edomex:
30 municipios a 300 + "Resto" a 900.

### Ajuste por rechazo (refusal), sin sustitución
`factor = 1 / (1 − tasa_rechazo)` (50% → ×2). Dos **modos** configurables:
- **`modo_rechazo = "manzanas"` (default):** se inflan las manzanas/sección
  (`manzanas_por_seccion × factor`), las secciones quedan fijas → la muestra no se
  dispersa.
- **`modo_rechazo = "secciones"`:** se inflan las secciones, las manzanas/sección
  quedan fijas → útil en municipios pequeños.

La **`tasa_rechazo` puede ser escalar (global) o por estrato** (columna en la
tabla de asignación).

---

## 3. Diseño de la API

### 3.1 Función principal
```r
disenar_muestra_ine(
  poblacion,                    # objeto PoblacionINE ya construido
  estratos,                     # tabla declarativa (ver 3.2)
  variable_estrato = "region",  # columna del marco que define el estrato
  variable_cluster = "SECCION", # columna del conglomerado (último nivel)
  n_0 = 5,
  manzanas_por_seccion = 2,
  tasa_rechazo = 0,             # escalar; o por estrato vía columna en `estratos`
  modo_rechazo = c("manzanas", "secciones"),
  semilla = NULL,
  calcular_cuotas = TRUE,
  ajustar_cuotas = TRUE,
  validar = TRUE
)
```
Devuelve el objeto `DiseñoINE` completo (muestra extraída + cuotas), tras correr
internamente todo el pipeline en el orden correcto, y con un atributo/`resumen`
adjunto (ver 3.4).

### 3.2 `estratos` como datos planos (clave para skill/Shiny)
`data.frame` con una fila por estrato:

| columna | tipo | descripción |
|---|---|---|
| `estrato` | chr | valor de `variable_estrato` (p. ej. `"Toluca"`, `"Resto"`) |
| `entrevistas` | num | entrevistas **efectivas** objetivo en ese estrato |
| `tasa_rechazo` | num | *(opcional)* rechazo por estrato; si falta, usa el escalar global |

Un brief de cliente ("30 municipios a 300, 900 al resto, 50% rechazo") se traduce
directamente a esta tabla; eso es lo que generaría una skill o una UI.

### 3.3 Orquestación interna (deriva y ejecuta en el orden correcto)
A partir de `estratos` y los parámetros, la función:
1. Calcula por estrato: `secciones`, `manzanas_por_seccion` efectivas y
   `entrevistas_a_levantar` según `modo_rechazo` y `tasa_rechazo`.
2. Crea `DiseñoINE$new(..., n = total a levantar, n_0, semilla)`.
3. `agregar_nivel(variable_estrato, "strata")` y `agregar_nivel(variable_cluster, "cluster")`.
4. `plan_muestra(nivel = 1, criterio = "manual", manual = <vector de secciones por estrato derivado de la tabla, en el orden interno correcto>)`.
5. `plan_muestra(nivel = último)`; ajusta las manzanas/sección al valor inflado.
6. Calcula el **orden de `fpc` automáticamente** (clusters ascendente, luego nivel 0).
7. `extraer_muestra` niveles 1..último.
8. Si `calcular_cuotas`, `calcular_cuotas(ajustar_cuotas)`.

El mapeo nombre→posición del vector `manual` lo resuelve la función (cierra el
punto frágil del vector posicional actual).

### 3.4 Inspección y validación (para skill/UI y para cerrar el ciclo de tuneo)
- **`resumen_diseno(diseno)`** → `tibble` por estrato: objetivo efectivo,
  secciones, manzanas/sección, entrevistas a levantar y **realizadas**
  (objetivo vs realizado), más totales.
- **`validar_estratos(poblacion, estratos, ...)`** → valida y **acumula** errores
  (estrato inexistente en el marco, entrevistas/tasa no válidas, secciones que
  exceden lo disponible) y los devuelve como mensaje accionable, para mostrarse
  **antes** de correr el muestreo.

### Fuera de alcance (fases posteriores)
- La **skill** y/o **app de Shiny** (se apoyan en esta API).
- Estimación municipal/estatal (corresponde a `encuestar`: dominios + pesos).
- Variante censal (`disenar_muestra` INEGI) con el mismo patrón.
- Sustituciones sin IO de archivos.
- Herencia `*INE`.

---

## 4. Plan de commits atómicos

1. `feat: calcular_asignacion() — deriva secciones/manzanas/levantamiento desde el modelo operativo (n_0, manzanas, rechazo, modo)`
2. `feat: validar_estratos() — validación declarativa de la tabla de estratos`
3. `feat: disenar_muestra_ine() — receta declarativa que orquesta el pipeline`
4. `feat: resumen_diseno() — resumen objetivo vs realizado por estrato`
5. `test: equivalencia con el flujo imperativo + casos de rechazo (manzanas/secciones) y sobremuestreo`
6. `docs: documentar la API declarativa + sección en vignette/README`

Cada commit deja el paquete cargable y la suite verde.

---

## 5. Criterios de aceptación

- [ ] `calcular_asignacion()` reproduce el ejemplo de referencia: 300 efectivas,
  `n_0=5`, `manzanas=2` → 30 secciones; con `tasa_rechazo=0.5` y
  `modo="manzanas"` → 4 manzanas/sección y 600 a levantar (secciones fijas en 30);
  con `modo="secciones"` → 60 secciones y 2 manzanas/sección.
- [ ] `disenar_muestra_ine()` con asignación uniforme produce **la misma muestra y
  cuotas** que el flujo imperativo equivalente (misma semilla) — test de equivalencia.
- [ ] Soporta **asignación desproporcionada** (vector/tabla por estrato) y
  `tasa_rechazo` escalar **y** por estrato.
- [ ] `validar_estratos()` detecta y acumula errores comunes.
- [ ] API **aditiva**: métodos imperativos sin cambios; demo y tests existentes verdes.
- [ ] `R CMD check` sin nuevos ERRORS/WARNINGS; funciones nuevas documentadas.

---

## 6. Riesgos
- **Orden interno del vector `manual`** debe mapear correctamente nombre→posición
  de estrato. Mitigación: test de equivalencia y de orden sobre el fixture.
- **Regla de `fpc`** derivada de la estructura. Mitigación: test contra casos 2,0 y 2,3,0.
- **`sobremuestra()` a medio implementar**: la receta no depende de él para el caso
  estándar (usa `criterio="manual"`); consolidarlo queda como mejora interna acotada.
- R disponible localmente: **todo se valida ejecutando**.
