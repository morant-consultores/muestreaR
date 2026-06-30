# Fase 2 — Limpieza interna de `muestreaR`

**Fecha:** 2026-06-19
**Repo:** `morant-consultores/muestreaR`
**Branch:** `mejoras/fase2-limpieza` (sobre `mejoras/fase1-red-de-seguridad`)
**Autor del diseño:** auditoría asistida (Claude) + Emilio Morones

---

## 1. Contexto

La Fase 1 dejó una red de seguridad (tests + reproducibilidad + CI) que ahora **protege contra regresiones**. La Fase 2 aprovecha esa red para limpiar la deuda técnica de bajo riesgo, dejando el paquete instalable de forma limpia y con un `R CMD check` notablemente más sano.

Esta fase es de **bajo riesgo por diseño**: no toca la matemática de muestreo ni la estructura de las clases. Cada cambio se valida con `devtools::test()` (suite verde de la Fase 1) y con el demo.

### Decisiones acordadas
- **DESCRIPTION:** licencia **MIT**, autor único **Emilio Morones** (`emorones@morant.com.mx`, cre/aut).
- **Herencia `*INE`:** **se difiere** a una fase posterior dedicada (es el único refactor de alto riesgo; se aísla en su propia rama/PR con pruebas específicas).

---

## 2. Alcance

### 2.1 Imports del NAMESPACE (bajo riesgo, alto valor)
**Problema:** el paquete usa `purrr`, `tidyr`, `sf`, `rlang`, `glue` (y otros) **sin prefijo y sin importarlos** en el NAMESPACE (que solo importa `dplyr`, `ggplot2`, `leaflet`, `tibble`). Por eso varias funciones solo corren si el usuario tiene esos paquetes adjuntados — la fragilidad que aparició en la Fase 1 con `crear_mm_ine`.

**Solución:** crear un archivo de documentación a nivel de paquete `R/muestreaR-package.R` con directivas roxygen `@importFrom` (o `@import`) para los paquetes faltantes, y **regenerar el NAMESPACE con `devtools::document()`**.

**Mitigación de riesgo (crítica):** tras regenerar, **diff del NAMESPACE** para confirmar que **los 28 `export(...)` actuales se conservan** intactos (solo deben añadirse líneas `importFrom`/`import`). Si roxygen elimina algún export, se restaura el `@export` correspondiente antes de continuar. Validar con `devtools::load_all()` + `devtools::test()`.

Paquetes a declarar (usados sin prefijo, vistos en el código): `purrr`, `tidyr`, `sf`, `rlang`, `glue`, `sampling`, `survey`, `scales`, `readr`, `stringr`, `cowplot`, `ineq`, `stats`. (`dplyr`, `ggplot2`, `leaflet`, `tibble` ya están.)

### 2.2 Código muerto (bajo riesgo)
- `R/diseno.R`: ~300 líneas comentadas (`criterio_N`, `criterio_m`, `criterio_n`, `empaquetar`, `calcular_varianza_estratificada`, `calcular_varianza_mas`) y la función **`nivel()` viva pero rota** (llama a `criterio_N`/`calcular_fpc` con firmas que ya no existen; no se invoca desde ningún sitio). Eliminar.
- Bloques comentados sueltos en `R/crear_mm.R` (lecturas `readOGR`/`st_read` antiguas) y otros restos.
- El historial de git preserva todo; no hace falta conservarlo comentado.

**Validación:** `devtools::test()` verde + demo corre, antes y después.

### 2.3 DESCRIPTION real (bajo riesgo)
Reemplazar la plantilla de `usethis`:
- `Title`: "Diseño de Muestras Polietápicas para Encuestas" (o similar, en Title Case).
- `Description`: un párrafo real sobre el muestreo polietápico (estratos + clusters PPT) para encuestas del INE/INEGI.
- `Authors@R`: `person("Emilio", "Morones", "emorones@morant.com.mx", role = c("aut","cre"))`.
- `License: MIT + file LICENSE` y crear el archivo `LICENSE` (plantilla MIT con titular).
- Conservar `Imports`/`Suggests` actuales (la Fase 1 ya añadió `testthat` + `Config/testthat/edition`).

### Fuera de alcance (fases posteriores)
- **Herencia `*INE`** (refactor de alto riesgo) → fase dedicada.
- **Documentación roxygen con contenido** (los `#' Title` vacíos) y README → Fase 3.
- **Rediseño de API** → Fase 4.

---

## 3. Plan de commits atómicos

1. `refactor: declarar imports faltantes en NAMESPACE (purrr, tidyr, sf, rlang, glue, ...)`
2. `refactor: eliminar código muerto en diseno.R (nivel() + bloques comentados)`
3. `refactor: eliminar bloques comentados sueltos (crear_mm.R, otros)`
4. `docs: DESCRIPTION real + LICENSE (MIT, Emilio Morones)`

Cada commit deja el paquete cargable y la suite verde.

---

## 4. Criterios de aceptación

- [ ] El NAMESPACE conserva los **28 exports** actuales y añade los `importFrom`/`import` faltantes.
- [ ] `devtools::test()` sigue **verde** (33+ asserts) tras cada commit.
- [ ] El demo `inst/ejemplos/disenar_muestra_demo.R` corre de principio a fin.
- [ ] `R CMD check` mejora respecto a la Fase 1: desaparecen el ERROR de licencia inválida y los NOTE de "no visible global function" de los paquetes ahora importados.
- [ ] No se modifica la matemática de muestreo ni la estructura de las clases.

---

## 5. Riesgos

- **Regeneración del NAMESPACE con roxygen** podría alterar exports si algún `@export` no está donde roxygen espera. Mitigación: diff obligatorio + validación con tests; restaurar manualmente cualquier export perdido.
- **Eliminar código comentado** podría borrar por error una referencia viva. Mitigación: confirmar con `grep` que cada función eliminada no se invoca, y correr tests + demo después.
- R está disponible localmente: **toda la Fase 2 se valida ejecutando** (no a ciegas).
