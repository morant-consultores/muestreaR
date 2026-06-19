# Fase 1 — Red de seguridad de `muestreaR` (tests de regresión + reproducibilidad)

**Fecha:** 2026-06-17
**Repo:** `morant-consultores/muestreaR`
**Branch:** `mejoras/fase1-red-de-seguridad`
**Autor del diseño:** auditoría asistida (Claude) + Emilio Morones

---

## 1. Contexto y motivación

La auditoría de `muestreaR` / `encuestar` concluyó que **la matemática de muestreo es correcta**, pero que el paquete carece de tres cosas que lo hacen riesgoso de mantener:

1. **Cero pruebas automatizadas** (no existe `tests/`). Ningún cálculo de muestra está protegido contra regresiones.
2. **Sorteo no reproducible**: `extraer_muestra()` (`slice_sample`) y `calcular_cuotas()` (ajuste ±1) no fijan semilla. Dos corridas del mismo diseño dan muestras distintas; una muestra histórica no se puede regenerar ni auditar. Esto se relaciona directamente con la nota de migración *"no se reproducen los resultados de la paquetería anterior"*.
3. **Sin fixture ni ejemplo ejecutable** independiente de datos propietarios del INE.

Esta Fase 1 es la **primera de cuatro** (seguridad → limpieza → docs → API). Su objetivo es crear la red de seguridad que permita hacer las fases siguientes sin romper la matemática en silencio. **No** modifica la lógica de muestreo; solo añade reproducibilidad (opt-in, retrocompatible), tests y un ejemplo.

### Fuera de alcance (fases posteriores)
- Limpieza de código muerto (~300 líneas comentadas en `diseno.R`, `nivel()` rota).
- Herencia de las clases `*INE` (hoy copy-paste).
- Documentación (DESCRIPTION real, roxygen, README).
- Rediseño declarativo de la API.

---

## 2. Decisiones de diseño (acordadas)

| Tema | Decisión |
|---|---|
| Tipo de tests | Invariantes (núcleo) **+** snapshots (complemento) |
| Fixture | Sintético, **incluye** `crear_mm` con `sf` mínimo; flujo **INE** |
| Reproducibilidad | Semilla **guardada en `DiseñoINE$new`** (y `Diseño$new`), derivada por etapa |
| Script de prueba | Paso a paso sobre el **fixture sintético** (corre sin datos reales) |
| Proceso | Branch nueva, **commits atómicos**, todo documentado |

---

## 3. Componentes

### 3.1 Reproducibilidad — semilla opt-in y retrocompatible

**Cambio en `R/clases.R` y `R/clases_ine.R`:**

- Nuevo parámetro `semilla = NULL` en `Diseño$new` y `DiseñoINE$new`, almacenado en el campo público `self$semilla`.
- La semilla se aplica **dentro del método R6** que envuelve cada sorteo, derivando una **sub-semilla por etapa** para que las etapas no compartan estado del generador:

  | Método | Sub-semilla |
  |---|---|
  | `extraer_muestra(nivel)` | `set.seed(self$semilla + nivel)` antes de `muestrear()` |
  | `calcular_cuotas()` | `set.seed(self$semilla + 1000)` antes del ajuste ±1 |
  | `sustituir_muestra()` | `set.seed(self$semilla + 2000 + self$n_sustitucion)` |

- **Retrocompatibilidad (requisito duro):** si `semilla = NULL`, **no** se llama a `set.seed` y el flujo de números aleatorios es idéntico al actual. Ningún script de producción existente cambia de comportamiento.
- **Las funciones puras NO se tocan:** `muestrear()` y `cuotas()` permanecen sin `set.seed` interno; la semilla se fija en el método R6 llamador. Esto mantiene las funciones testeables de forma aislada.

**Interfaz:** `DiseñoINE$new(poblacion, n, n_0, variable_poblacional, unidad_muestreo, id_unidad_muestreo, llave_muestreo, semilla = NULL)`. Depende de: nada nuevo (usa `base::set.seed`).

### 3.2 Fixture sintético — `tests/testthat/helper-fixture.R`

Función determinista `generar_fixture_ine()` que devuelve una lista con los insumos del flujo INE, **sin depender de shapefiles reales**:

- Geografía mínima en grilla de polígonos cuadrados (para que `sf::st_join`, `st_is_valid`, `STATUS == 1` funcionen): 1 entidad, 2 municipios, ~4 secciones, ~12 manzanas, anidados correctamente (manzana ⊂ sección ⊂ municipio).
- Objetos producidos:
  - `shp_mza`: `sf` con `ENTIDAD, DISTRITO_F, DISTRITO_L, MUNICIPIO, SECCION, MANZANA, STATUS, geometry`.
  - `shp_loc`, `shp_mun` (con `NOMBRE`), `sec_shp`, `df_shp`, `dl_shp` para `CartografiaINE`.
  - `ln`: tabla de lista nominal con `SECCION`, columnas `LISTA_*` (rango de edad × sexo) y `LISTA NOMINAL` total.
- Cualquier aleatoriedad interna del generador se fija con `set.seed` propio para que el fixture sea siempre idéntico.
- Sirve de insumo tanto a `crear_mm_ine`/`PoblacionINE` como al pipeline de muestreo y al script de prueba.

Depende de: `sf`, `dplyr`, `tibble`.

### 3.3 Suite de tests — `tests/testthat/`

**Invariantes** (sin baseline grabado; verifican corrección, no solo estabilidad):
- `test-repartir_cociente.R`: suma exactamente `n`; resultados enteros; conserva longitud; `stop` en vector inválido.
- `test-calcular_fpc.R`: `fpc ∈ (0,1]`; longitud coincide con nº de unidades; sin `NA` inesperados.
- `test-asignar.R`: `asignar_m`/`asignar_n` — la asignación proporcional suma el total previsto y topa en la disponibilidad (`m ≤ unidades disponibles`).
- `test-cuotas.R`: `cuotas_ine` — las cuotas suman las entrevistas por cluster; no negativas; cuadre exacto ("Exacto").

**Snapshots** (baseline grabado por el equipo en la 1ª corrida):
- `test-crear_mm_ine.R`: `crear_mm_ine(fixture)` → snapshot de columnas y nº de filas esperadas.
- `test-pipeline_ine.R`: pipeline `DiseñoINE` completo con `semilla` fija → snapshot del resumen de la muestra (conteos por nivel) y de las cuotas.

Cada test usa `generar_fixture_ine()`. Depende de: `testthat (>= 3.0.0)`.

### 3.4 Script de prueba paso a paso — `inst/ejemplos/disenar_muestra_demo.R`

Réplica comentada del flujo de producción (`data-raw/muestra_ine_ejemplo.R`) pero **sobre el fixture sintético**, ejecutable de principio a fin:

1. Generar fixture → `PoblacionINE$new`.
2. `DiseñoINE$new(..., semilla = 123)`.
3. `CartografiaINE$new`.
4. `agregar_nivel` (region/strata, municipio/cluster, sección/cluster).
5. `plan_muestra` por nivel (peso, uniforme, último).
6. `fpc` (con nota explícita del orden requerido).
7. `extraer_muestra` por nivel.
8. `calcular_cuotas`, `revisar_muestra`.
9. (opcional) `exportar` a carpeta temporal.

Cada paso lleva comentarios que (a) explican qué hace, (b) marcan **dónde irían los insumos reales del INE** en producción. Sirve como prueba manual, documentación viva y plantilla.

### 3.5 Andamiaje
- `tests/testthat.R` (runner) y `Config/testthat/edition: 3` en DESCRIPTION.
- Añadir `testthat (>= 3.0.0)` a `Suggests`.
- *(No se reescribe el resto del DESCRIPTION en esta fase; eso es Fase 3.)*

---

## 4. Cómo se ejecuta (dado que aquí no hay R)

Todo el código se entrega escrito. El equipo, en una máquina con R:

```r
devtools::load_all(".")
devtools::test()        # corre invariantes; 1ª vez graba snapshots en tests/testthat/_snaps/
testthat::snapshot_review()   # revisar y aceptar los snapshots iniciales
source("inst/ejemplos/disenar_muestra_demo.R")   # prueba manual paso a paso
```

Los snapshots aceptados (`_snaps/`) se versionan en git. A partir de ahí, cualquier cambio futuro que altere un resultado hará fallar el test.

---

## 5. Plan de commits atómicos

1. `test: andamiaje de testthat (runner, edition 3, Suggests)`
2. `feat: semilla opcional reproducible en Diseño y DiseñoINE`
3. `test: fixture sintético INE (generar_fixture_ine)`
4. `test: invariantes de repartir_cociente, calcular_fpc, asignar`
5. `test: invariantes de cuotas_ine`
6. `test: snapshots de crear_mm_ine y pipeline DiseñoINE`
7. `docs: script de prueba paso a paso disenar_muestra_demo.R`

Cada commit es autocontenido y deja el paquete cargable.

---

## 6. Criterios de aceptación

- [ ] `semilla` es opt-in; con `NULL` el comportamiento es bit-idéntico al actual.
- [ ] Con `semilla` fija, dos corridas del pipeline producen la **misma** muestra y las **mismas** cuotas.
- [ ] Los tests de invariantes pasan sin baseline previo.
- [ ] `generar_fixture_ine()` produce insumos que `crear_mm_ine` procesa sin error.
- [ ] `inst/ejemplos/disenar_muestra_demo.R` corre de principio a fin sobre el fixture.
- [ ] Ningún cambio en la matemática de muestreo (solo se añade reproducibilidad y andamiaje).

---

## 7. Riesgos

- **Fixture sf que satisfaga `crear_mm_ine`**: el flujo INE tiene varios `st_join`/`separate`/`pivot` sensibles a nombres de columna; el fixture puede requerir iteración hasta que `crear_mm_ine` lo acepte. Mitigación: empezar el fixture por las columnas exactas que consume `crear_mm_ine` (`R/crear_mm.R:181`).
- **No puedo ejecutar R aquí**: los snapshots y el ajuste fino del fixture los valida el equipo. El código se entrega con esa expectativa explícita.
